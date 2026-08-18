#pragma once

#include <atomic>

#include "Scene.h"

#include "smdl/Support/SpectralRenderImage.h"

/// Atomic add for `std::atomic<float>`, which gains a native `fetch_add`
/// only in C++20.
inline void atomicAdd(std::atomic<float> &atomicValue, float value) noexcept {
  float expected{atomicValue.load(std::memory_order_relaxed)};
  while (!atomicValue.compare_exchange_weak(expected, expected + value,
                                            std::memory_order_relaxed)) {
  }
}

/// Map a unit direction to the unit square with the equal-area world-space
/// cylindrical parameterization, so uniform density on the square is
/// uniform density over the sphere: `dω = 4π du dv`.
[[nodiscard]] inline float2 directionToSquare(const float3 &w) noexcept {
  float u{(std::atan2(w.y, w.x) + PI) / (2.0f * PI)};
  float v{(w.z + 1.0f) / 2.0f};
  u = std::fmin(std::fmax(u, 0.0f), 0.99999994f);
  v = std::fmin(std::fmax(v, 0.0f), 0.99999994f);
  return {u, v};
}

/// The inverse of `directionToSquare`.
[[nodiscard]] inline float3 squareToDirection(const float2 &uv) noexcept {
  float cosTheta{2.0f * uv.y - 1.0f};
  float sinTheta{std::sqrt(std::max(0.0f, 1.0f - cosTheta * cosTheta))};
  float phi{2.0f * PI * uv.x - PI};
  return {sinTheta * std::cos(phi), sinTheta * std::sin(phi), cosTheta};
}

/// An adaptive quadtree over the unit square, holding the directional
/// radiance distribution of one spatial cell of the SD-tree (Müller,
/// Gross & Novák, "Practical Path Guiding," EGSR 2017).
///
/// Each node stores the flux of its four quadrant subtrees; a quadrant
/// with no child index is a leaf cell. Recording adds flux at every level
/// of the descent, so every node's quadrant flux is the total of its
/// subtree without a separate propagation pass. The structure is fixed
/// while a pass renders (recording only touches the atomic flux) and is
/// rebuilt between passes on a single thread.
class DTree final {
public:
  struct Node final {
    Node() = default;

    Node(const Node &other) noexcept : child(other.child) {
      for (int q = 0; q < 4; q++)
        flux[q].store(other.flux[q].load(std::memory_order_relaxed),
                      std::memory_order_relaxed);
    }

    Node &operator=(const Node &other) noexcept {
      child = other.child;
      for (int q = 0; q < 4; q++)
        flux[q].store(other.flux[q].load(std::memory_order_relaxed),
                      std::memory_order_relaxed);
      return *this;
    }

    /// The flux of each quadrant subtree.
    std::array<std::atomic<float>, 4> flux{};

    /// The node index of each quadrant subtree, or 0 if the quadrant is a
    /// leaf cell. (Node 0 is the root and is never anyone's child.)
    std::array<uint32_t, 4> child{};
  };

  DTree() : mNodes(1) {}

  DTree(const DTree &other) : mNodes(other.mNodes) {
    statisticalWeight.store(
        other.statisticalWeight.load(std::memory_order_relaxed),
        std::memory_order_relaxed);
    momentBSDF.store(other.momentBSDF.load(std::memory_order_relaxed),
                     std::memory_order_relaxed);
    momentGuide.store(other.momentGuide.load(std::memory_order_relaxed),
                      std::memory_order_relaxed);
    mixtureAlpha = other.mixtureAlpha;
  }

  DTree &operator=(const DTree &other) {
    mNodes = other.mNodes;
    statisticalWeight.store(
        other.statisticalWeight.load(std::memory_order_relaxed),
        std::memory_order_relaxed);
    momentBSDF.store(other.momentBSDF.load(std::memory_order_relaxed),
                     std::memory_order_relaxed);
    momentGuide.store(other.momentGuide.load(std::memory_order_relaxed),
                      std::memory_order_relaxed);
    mixtureAlpha = other.mixtureAlpha;
    return *this;
  }

  /// The total flux at the root.
  [[nodiscard]] float totalFlux() const noexcept {
    float total{};
    for (int q = 0; q < 4; q++)
      total += mNodes[0].flux[q].load(std::memory_order_relaxed);
    return total;
  }

  /// The mean incident radiance implied by the recorded flux: the total
  /// over the record count and the sphere area.
  [[nodiscard]] float meanRadiance() const noexcept {
    float w{statisticalWeight.load(std::memory_order_relaxed)};
    return w > 0 ? totalFlux() / (4.0f * PI * w) : 0.0f;
  }

  /// Record `value` at the square point `uv`, adding flux at every level
  /// of the descent.
  void record(float2 uv, float value) noexcept;

  /// The side length in square space of the leaf cell containing `uv`,
  /// for filtered (jittered) splatting.
  [[nodiscard]] float leafSize(float2 uv) const noexcept;

  /// The solid-angle PDF of `sampleDirection` producing the direction
  /// `w`. Returns the uniform `1/4π` if the tree has no flux.
  [[nodiscard]] float pdf(const float3 &w) const noexcept;

  /// Sample a direction proportional to the recorded flux, hierarchically
  /// warping through the quadtree. Falls back to a uniform sphere sample
  /// when the tree has no flux.
  [[nodiscard]] float3 sampleDirection(Sampler &sampler,
                                       float &pdf) const noexcept;

  /// Rebuild this tree as the refined version of `prev`: quadrants whose
  /// flux exceeds `rho` of the total are subdivided (newly created
  /// children split their parent's flux equally), quadrants at or below
  /// the threshold collapse to leaves. Flux and statistical weight carry
  /// over, so the result is ready to sample from.
  void rebuildFrom(const DTree &prev, float rho, int maxDepth);

  /// Reset to the structure of `structure` with all flux and statistical
  /// weight zeroed, ready to record the next pass.
  void resetToStructureOf(const DTree &structure);

  /// The number of records, for converting flux to mean radiance.
  std::atomic<float> statisticalWeight{};

  /// Estimated second moments of the pure-BSDF and pure-tree one-sample
  /// estimators over the training records: `Σ (f L)² / (p_s p_mix)`
  /// estimates the second moment strategy `s` would have alone.
  /// Accumulated on the building side of a pass; they yield
  /// `mixtureAlpha` at refinement.
  std::atomic<float> momentBSDF{};
  std::atomic<float> momentGuide{};

  /// The learned probability of sampling the BSDF rather than the tree
  /// at vertices in this cell: the inverse-second-moment share
  /// `momentGuide / (momentBSDF + momentGuide)`, clamped defensively so
  /// lobes the tree cannot represent (a near-mirror BSDF) push toward
  /// pure BSDF sampling. Read from the frozen sampling tree during a
  /// pass; recomputed between passes.
  float mixtureAlpha{0.5f};

private:
  std::vector<Node> mNodes;
};

/// The spatial half of the SD-tree: a binary tree over the (cubified)
/// scene bounds with midpoint splits on alternating axes. Each leaf holds
/// a pair of directional quadtrees, `sampling` frozen and read during a
/// pass and `building` accumulating the pass's records, swapped and
/// refined between passes.
class STree final {
public:
  struct Node final {
    Node() = default;

    Node(const Node &other) noexcept
        : child(other.child), axis(other.axis), sampling(other.sampling),
          building(other.building) {
      recordCount.store(other.recordCount.load(std::memory_order_relaxed),
                        std::memory_order_relaxed);
    }

    /// The child node indices, or 0 if this is a leaf.
    std::array<uint32_t, 2> child{};

    /// The split axis, which is the depth modulo 3.
    uint8_t axis{};

    /// The number of vertices recorded here since the last refinement.
    std::atomic<uint32_t> recordCount{};

    DTree sampling{};

    DTree building{};
  };

  STree(const float3 &boundsMin, const float3 &boundsMax);

  /// The frozen sampling quadtree at `position`.
  [[nodiscard]] const DTree &samplingAt(const float3 &position) const noexcept {
    return mNodes[leafIndex(position)].sampling;
  }

  /// Record an incident-radiance estimate: `value` arrived at `position`
  /// from `direction`, already divided by the density that sampled the
  /// direction. Splats with the stochastic box filter of the 2019 "Path
  /// Guiding in Production" course: the position is jittered within the
  /// spatial leaf and the direction within the directional leaf.
  /// `momentBSDF` and `momentGuide` are the bounce's contributions to the
  /// cell's per-strategy second moments, deposited unjittered on the leaf
  /// containing `position`.
  void record(Sampler &sampler, const float3 &position, const float3 &direction,
              float value, float momentBSDF, float momentGuide) noexcept;

  /// Refine between passes: split spatial leaves whose record count
  /// exceeds `splitThreshold` (children copy their parent's quadtrees and
  /// halve its count), then rebuild every leaf's sampling quadtree from
  /// the flux its building quadtree collected and zero the building side.
  void refine(uint32_t splitThreshold, float rho, int maxDepth);

  /// The number of spatial leaves, for progress diagnostics.
  [[nodiscard]] size_t leafCount() const noexcept {
    size_t count{};
    for (const auto &node : mNodes)
      if (node.child[0] == 0) count++;
    return count;
  }

  /// The minimum and mean learned mixture weight over the spatial
  /// leaves, for progress diagnostics.
  void alphaStats(float &minAlpha, float &meanAlpha) const noexcept {
    minAlpha = 1.0f;
    meanAlpha = 0.0f;
    size_t count{};
    for (const auto &node : mNodes) {
      if (node.child[0] != 0) continue;
      minAlpha = std::fmin(minAlpha, node.sampling.mixtureAlpha);
      meanAlpha += node.sampling.mixtureAlpha;
      count++;
    }
    if (count > 0) meanAlpha /= float(count);
  }

private:
  [[nodiscard]] uint32_t leafIndex(const float3 &position) const noexcept;

  /// The leaf containing `position` along with the size of its box, for
  /// the record jitter.
  [[nodiscard]] uint32_t leafIndex(const float3 &position,
                                   float3 &leafBoxSize) const noexcept;

  std::vector<Node> mNodes;

  float3 mBoundMin{};

  float3 mBoundExtent{};
};

/// The per-pixel guiding context handed to `tracePath`. A null `tree`
/// disables guiding entirely and the walk behaves as before.
struct Guiding final {
  /// The SD-tree, read-only during the walk.
  const STree *tree{};

  /// The pixel's value estimate from the passes so far (spectral mean),
  /// or 0 if there is none yet. Drives adjoint-driven Russian roulette
  /// (Vorba & Křivánek, SIGGRAPH 2016, roulette only): survival is
  /// proportional to the expected pixel contribution of continuing.
  float pixelEstimate{};

  /// The probability of drawing the bounce direction from the BSDF
  /// rather than the SD-tree (the one-sample-MIS mixture weight, alpha
  /// in the paper), used when `bsdfFractionFixed` or when the cell has
  /// not learned its own weight yet.
  float bsdfFraction{0.5f};

  /// Ignore the per-cell learned mixture weights and use `bsdfFraction`
  /// everywhere, for experiments.
  bool bsdfFractionFixed{};
};

/// One vertex of training data retained by `tracePath()` for
/// `trainGuiding()`: what the SD-tree needs to reconstruct the radiance
/// estimate along the sampled continuation once the path has terminated,
/// which is a small fraction of what the walk knows at the vertex.
struct GuideRecord final {
  /// The vertex position.
  float3 point{};

  /// The sampled continuation direction.
  float3 wNext{};

  /// The path throughput arriving at the vertex, roulette-compensated, so
  /// consecutive records recover the bounce weight by ratio.
  Color beta{};

  /// The NEE estimate gathered at the vertex, throughput excluded: the
  /// light-sampling half gathered on the spot, plus the MIS-weighted
  /// emission or escape radiance the vertex's own continuation segment
  /// landed on, which `tracePath` folds back in when it gets there.
  Color direct{};

  /// The MIS-weighted emission or escape radiance the continuation
  /// segment landed on, by itself: the same quantity folded into
  /// `direct` but without the bounce weight, so the trainer can aim the
  /// tree at it along `wNext`. This is the residual that light sampling
  /// leaves to the continuation.
  Color continuationEmission{};

  /// The solid-angle density that sampled `wNext`, the full mixture
  /// density when path guiding participated.
  float pdfWNext{};

  /// The BSDF and SD-tree densities of `wNext` separately, valid only
  /// when the tree participated at the vertex (`pdfGuide >= 0`), and the
  /// spectral average of the BSDF value: what the trainer needs to
  /// estimate each strategy's stand-alone second moment for the cell's
  /// learned mixture weight.
  float pdfBSDF{};
  float pdfGuide{-1.0f};
  float fAvg{};

  /// Was the continuation a Dirac lobe? The tree cannot learn delta
  /// directions, so the trainer skips them.
  bool isDeltaBounce{};

  /// Did the walk escape to the environment here? Escape radiance is not
  /// part of the reflected field the tree trains on.
  bool isInfiniteLight{};
};

/// Train the SD-tree from one completed path's records, in path order as
/// `tracePath` appended them.
void trainGuiding(STree &tree, Sampler &sampler, const GuideRecord *records,
                  uint64_t numRecords);

/// Combination of a guided render's passes, each folded in at plain
/// sample-count weight (deliberately; see `foldPass()`). Also maintains
/// the box-blurred pixel value estimates that drive adjoint-driven
/// Russian roulette between passes.
class PassCombiner final {
public:
  PassCombiner(size_t nX, size_t nY)
      : mNumPixelsX(nX), mNumPixelsY(nY), mNumBands(renderNumBands()),
        mHalfImageA(nX * nY * renderNumBands()),
        mHalfImageB(nX * nY * renderNumBands()), //
        mHalfSquaresA(nX * nY), mHalfSquaresB(nX * nY),
        mComboNumer(nX * nY * renderNumBands(), 0.0),
        mComboDenom(nX * nY, 0.0) {}

  /// Seed the combination with a resumed prior session's accumulation,
  /// folded in as if it were an already-rendered pass of
  /// `samplesPerPixel` samples. Because every pass folds in at
  /// sample-count weight, `resolve()` then reproduces the exact merged
  /// accumulation of both sessions, and `rebuildPixelEstimates()` can
  /// inform the first pass's ADRRS from the resumed image.
  void seed(const smdl::SpectralRenderImage &image, size_t samplesPerPixel);

  /// One pixel's contribution to the pass being rendered, split into
  /// two half images by alternating samples so the combination can
  /// cross-weight each half by the other's variance estimate.
  struct PixelHalves final {
    /// The radiance sums of the even (A) and odd (B) samples.
    Color halfA{}, halfB{};

    /// The sums of squared per-sample spectral means, from which
    /// `foldPass()` estimates each half's per-sample variance.
    float squaresA{}, squaresB{};
  };

  /// Deposit one pixel's half images for the current pass, overwriting
  /// whatever the previous pass left there. Called from the parallel
  /// render loop, which is safe because each pixel owns its own slots.
  void deposit(size_t pixelIndex, const PixelHalves &halves) noexcept;

  /// Fold the deposited pass of `passSamples` samples per pixel into
  /// the combination at sample-count weight, so the result is the plain
  /// mean of every sample rendered.
  void foldPass(size_t passSamples);

  /// Rebuild the ADRRS pixel estimates from the combination so far:
  /// the spectral mean of the combined image, box-blurred so
  /// single-pixel noise does not drive the roulette.
  void rebuildPixelEstimates();

  /// The pixel's value estimate, or 0 before the first
  /// `rebuildPixelEstimates()`.
  [[nodiscard]] float pixelEstimate(size_t pixelIndex) const noexcept {
    return mImageEstimate.empty() ? 0.0f : mImageEstimate[pixelIndex];
  }

  /// Resolve the combination into the image every downstream output
  /// reads from, replacing its contents. The folded sample count
  /// becomes the image's per-pixel count, so the sums-plus-counts
  /// invariant holds and the means read back as the combined image.
  void resolve(smdl::SpectralRenderImage &image) const;

private:
  // The image dimensions in pixels.
  size_t mNumPixelsX{};
  size_t mNumPixelsY{};

  // The band count, frozen at construction.
  size_t mNumBands{};

  // The total samples per pixel of the folded passes.
  size_t mKeptSPP{};

  // The deposited half images of the current pass, per pixel per band.
  std::vector<float> mHalfImageA;
  std::vector<float> mHalfImageB;

  // The deposited sums of squared spectral means, per pixel.
  std::vector<float> mHalfSquaresA;
  std::vector<float> mHalfSquaresB;

  // The weighted radiance sums of the folded passes, per pixel per
  // band, and the total weights, per pixel.
  std::vector<double> mComboNumer;
  std::vector<double> mComboDenom;

  // The box-blurred ADRRS estimates, empty until the first
  // `rebuildPixelEstimates()`.
  std::vector<float> mImageEstimate;
};
