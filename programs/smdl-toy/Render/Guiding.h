#pragma once

#include <algorithm>
#include <string>
#include <string_view>

#include "Color.h"
#include "Render/Sampler.h"

#include "smdl/RenderUtil/SpectralFilm.h"

/// The extension that conventionally marks a saved SD-tree (see
/// `STree::writeFile()`). Advisory, as everywhere: the magic bytes
/// decide.
constexpr std::string_view GUIDE_TREE_EXTENSION = ".sdtree";

/// The magic that begins a saved SD-tree.
constexpr std::string_view GUIDE_TREE_MAGIC = "SMDLSDTR";

/// Map a unit direction to the unit square with the equal-area world-space
/// cylindrical parameterization, so uniform density on the square is
/// uniform density over the sphere: `dω = 4π du dv`.
[[nodiscard]] inline float2 directionToSquare(const float3 &w) noexcept {
  float u{(std::atan2(w.y, w.x) + PI) / TWO_PI};
  float v{(w.z + 1.0f) / 2.0f};
  u = std::clamp(u, 0.0f, ONE_MINUS_EPS);
  v = std::clamp(v, 0.0f, ONE_MINUS_EPS);
  return {u, v};
}

/// The inverse of `directionToSquare`.
[[nodiscard]] inline float3 squareToDirection(const float2 &uv) noexcept {
  float cosTheta{2.0f * uv.y - 1.0f};
  float sinTheta{std::sqrt(std::max(0.0f, 1.0f - cosTheta * cosTheta))};
  float phi{TWO_PI * uv.x - PI};
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
/// while a pass renders and is rebuilt between passes on a single
/// thread; the counters do not move during a pass either, because
/// recording goes through the per-thread mirrors of `GuideAccumulator`
/// and lands here only when they are absorbed between passes.
class DTree final {
public:
  struct Node final {
    /// The flux of each quadrant subtree, in fixed-point units of
    /// `fluxUnit`.
    ///
    /// Integral on purpose: integer addition commutes, so the absorbed
    /// totals do not depend on which thread recorded what, and a guided
    /// render is bit-wise reproducible run to run. Real-valued
    /// accumulation goes through the fixed-point units of `fluxUnit`
    /// instead of float sums, whose totals take whatever order the
    /// threads happen to record in; a one-ulp wobble in the trained
    /// tree amplifies through the sampling feedback into visibly
    /// different renders.
    std::array<uint64_t, 4> flux{};

    /// The node index of each quadrant subtree, or 0 if the quadrant is a
    /// leaf cell. (Node 0 is the root and is never anyone's child.)
    std::array<uint32_t, 4> child{};
  };

  DTree() : mNodes(1) {}

  /// The total flux at the root, in units of `fluxUnit`.
  [[nodiscard]] uint64_t totalFluxUnits() const noexcept {
    uint64_t total{};
    for (int q = 0; q < 4; q++) total += mNodes[0].flux[q];
    return total;
  }

  /// The total flux at the root, as a value.
  [[nodiscard]] float totalFlux() const noexcept {
    return float(totalFluxUnits()) * fluxUnit;
  }

  /// The mean incident radiance implied by the recorded flux: the total
  /// over the record count and the sphere area.
  [[nodiscard]] float meanRadiance() const noexcept {
    const auto w{uint64_t(statisticalWeight)};
    return w > 0 ? totalFlux() / (2.0f * TWO_PI * float(w)) : 0.0f;
  }

  /// Record `units` of flux at the square point `uv` into the mirror
  /// counters `flux`, which hold four counters per node in node order,
  /// adding at every level of the descent.
  void record(float2 uv, uint64_t units, uint64_t *flux) const noexcept;

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
  uint64_t statisticalWeight{};

  /// Estimated second moments of the pure-BSDF and pure-tree one-sample
  /// estimators over the training records: `Σ (f L)² / (p_s p_mix)`
  /// estimates the second moment strategy `s` would have alone, in
  /// fixed-point units of `momentUnit`. Accumulated on the building side
  /// of a pass; they yield `mixtureAlpha` at refinement.
  uint64_t momentBSDF{};
  uint64_t momentGuide{};

  /// The value of one fixed-point unit of `flux`, and of the moments.
  ///
  /// The scale is chosen per pass from the total the counter reached in
  /// the previous one (see `STree::refine()`), floored for a fresh
  /// tree, so 64 bits leave billionfold growth headroom above the
  /// expected total. A record's sub-unit fraction rounds stochastically
  /// with the pixel's sampler, so no scale can quantize the training
  /// signal to zero, and its magnitude caps at 2^40 units, three
  /// decades above the previous pass's whole total, where only a
  /// genuinely absurd firefly clamps. Both are harmless where exact
  /// float accumulation would not be: the tree only steers sampling,
  /// and every density the estimator weighs with is read back from the
  /// tree that resulted.
  float fluxUnit{0x1p-20f};
  float momentUnit{0x1p-20f};

  /// The learned probability of sampling the BSDF rather than the tree
  /// at vertices in this cell: the inverse-second-moment share
  /// `momentGuide / (momentBSDF + momentGuide)`, clamped defensively so
  /// lobes the tree cannot represent (a near-mirror BSDF) push toward
  /// pure BSDF sampling. Read from the frozen sampling tree during a
  /// pass; recomputed between passes.
  float mixtureAlpha{0.5f};

private:
  /// For `STree::writeFile()` and `STree::readFile()`, which move the
  /// nodes to and from disk directly.
  friend class STree;

  std::vector<Node> mNodes;
};

/// The spatial half of the SD-tree: a binary tree over the (cubified)
/// scene bounds with midpoint splits on alternating axes. Each leaf holds
/// a pair of directional quadtrees, `sampling` frozen and read during a
/// pass and `building` collecting the pass's records, swapped and
/// refined between passes.
///
/// The whole tree is immutable while a pass renders: the threads record
/// into the per-thread counter mirrors of a `GuideAccumulator`, which
/// `absorb()` sums into the building side between passes. Nothing here
/// is atomic, and the render threads never write a shared line.
class STree final {
public:
  struct Node final {
    /// The child node indices, or 0 if this is a leaf.
    std::array<uint32_t, 2> child{};

    /// The split axis, which is the depth modulo 3.
    uint8_t axis{};

    /// The number of vertices recorded here since the last refinement.
    uint32_t recordCount{};

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
  ///
  /// The deposit lands in `counters`, one thread's mirror of the
  /// building-side counters (see `GuideAccumulator`); the sampler draws
  /// and fixed-point scales read only the frozen structure, so what a
  /// record deposits never depends on what the mirrors have collected.
  void record(Sampler &sampler, const float3 &position, const float3 &direction,
              float value, float momentBSDF, float momentGuide,
              uint64_t *counters) const noexcept;

  /// The number of `uint64_t` counters a mirror of the building side
  /// holds: per leaf, the record count, statistical weight, and two
  /// moments, then four flux counters per building quadtree node.
  /// Fixed until `refine()` rebuilds the structure.
  [[nodiscard]] size_t counterCount() const noexcept { return mCounterCount; }

  /// Sum per-thread counter mirrors into the building side, in parallel
  /// over the spatial nodes. Every counter is integral, so the result
  /// is the same bits no matter how the records were spread across the
  /// mirrors.
  void absorb(const std::vector<const uint64_t *> &mirrors);

  /// Refine between passes: split spatial leaves whose record count
  /// exceeds `splitThreshold` (children copy their parent's quadtrees and
  /// halve its count), then rebuild every leaf's sampling quadtree from
  /// the flux its building quadtree collected and zero the building side.
  void refine(uint32_t splitThreshold, float rho, int maxDepth);

  /// Write the tree to `fileName`, with `samplesPerPixel` recording how
  /// many samples per pixel trained it, so a session resuming from the
  /// paired accumulation can tell a current tree from a stale one.
  ///
  /// Meant for the moment `refine()` leaves behind: the sampling side
  /// frozen and worth keeping, the building side zeroed, so only the
  /// sampling quadtrees and the units the next pass needs are written.
  /// The layout is little-endian:
  ///
  /// ```
  /// offset  size  field
  ///      0     8  magic "SMDLSDTR"
  ///      8     2  u16 version, currently 1
  ///     10     2  u16 reserved, 0 in v1
  ///     12     4  u32 spatial node count
  ///     16     8  u64 samples per pixel the tree was trained by
  ///     24    12  f32 x3 bound minimum
  ///     36    12  f32 x3 bound extent
  ///     48        spatial nodes, each:
  ///            8  u32 x2 child node indices, 0 for a leaf
  ///            4  u32 split axis
  ///   and, for a leaf only:
  ///            8  u64 record count (statistical weight)
  ///            4  f32 learned mixture weight
  ///            4  f32 flux unit of the sampling quadtree
  ///            4  f32 moment unit for the next pass
  ///            4  u32 quadtree node count, then that many:
  ///           32  u64 x4 quadrant flux, in flux units
  ///           16  u32 x4 quadrant child node indices, 0 for a leaf
  /// ```
  ///
  /// \throws smdl::Error  If the file cannot be written.
  ///
  void writeFile(const std::string &fileName, uint64_t samplesPerPixel) const;

  /// Read a tree written by `writeFile()`, filling `samplesPerPixel`
  /// from its header.
  ///
  /// The result continues training exactly as the saving session's next
  /// pass would have: the bounds come from the file (they are baked
  /// into every leaf lookup, so the scene never re-derives them), each
  /// leaf's building side is rebuilt from its sampling structure, and
  /// the saved moment unit is restored, it being the one scale
  /// `refine()` derives from totals it consumes.
  ///
  /// \throws smdl::Error  If the file cannot be read, the magic or
  ///                      version is wrong, or the structure does not
  ///                      add up. Fail-loud so the caller can decide to
  ///                      retrain from scratch, which is always safe:
  ///                      the tree only steers sampling.
  ///
  [[nodiscard]] static STree readFile(const std::string &fileName,
                                      uint64_t &samplesPerPixel);

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
      minAlpha = std::min(minAlpha, node.sampling.mixtureAlpha);
      meanAlpha += node.sampling.mixtureAlpha;
      count++;
    }
    if (count > 0) meanAlpha /= float(count);
  }

private:
  /// For `readFile()`, which fills everything in from the file.
  STree() = default;

  [[nodiscard]] uint32_t leafIndex(const float3 &position) const noexcept;

  /// The leaf containing `position` along with the size of its box, for
  /// the record jitter.
  [[nodiscard]] uint32_t leafIndex(const float3 &position,
                                   float3 &leafBoxSize) const noexcept;

  /// Assign every leaf its offset into the counter mirrors; called
  /// whenever the structure changes.
  void buildCounterLayout();

  std::vector<Node> mNodes;

  /// Each leaf's offset into a counter mirror; see `counterCount()`.
  std::vector<uint64_t> mCounterOffset;

  size_t mCounterCount{};

  float3 mBoundMin{};

  float3 mBoundExtent{};
};

/// The per-thread accumulation of one training pass: each render thread
/// records into its own flat mirror of the SD-tree's building-side
/// counters, and the mirrors are summed into the tree between passes.
/// This is what lets a pass run with no shared writes at all, which is
/// where a shared tree loses its scaling: every record descends to the
/// root of some quadtree, and under a small render window most of them
/// descend to the same one.
///
/// A mirror costs 8 bytes per counter and is allocated (and zeroed) the
/// first time its thread records, so threads that never train a pass
/// cost nothing. The observed heaviest tree to date (a long full-frame
/// waves training run) costs about 16 MB per mirror, roughly 2 GB
/// across a 128-thread pool.
class GuideAccumulator final {
public:
  explicit GuideAccumulator(const STree &tree);

  /// The calling thread's counter mirror, allocated on first use.
  [[nodiscard]] uint64_t *local();

  /// Sum the mirrors into the tree's building side; single-threaded
  /// caller, between passes. The tree must be the one this accumulator
  /// was built for, with the same structure.
  void absorbInto(STree &tree) const;

private:
  const STree &mTree;

  /// One mirror per thread that may record: the pool plus the calling
  /// thread. Indexed by a process-wide per-thread slot, so a thread
  /// keeps its slot across passes and the vector is never resized.
  std::vector<std::vector<uint64_t>> mMirrors;
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

/// The one-sample-MIS mixture density of a finite continuation
/// direction: the SD-tree's density and the BSDF's own, mixed by the
/// probability of having drawn the direction from the BSDF.
[[nodiscard]] inline float guidedMixturePdf(float guidePdf, float bsdfPdf,
                                            float bsdfFraction) noexcept {
  return smdl::lerp(guidePdf, bsdfPdf, bsdfFraction);
}

/// The density the continuation sampler assigns to `w` at a vertex: the
/// BSDF's own `bsdfPdf`, or the guided mixture when the SD-tree
/// participates there. Every MIS weight that competes against the
/// walk's continuation must use this density, not the raw BSDF's: the
/// sampler draws from the mixture, and weighing against anything else
/// leaves the pair of estimators summing away from one.
[[nodiscard]] inline float guidedContinuationPdf(const DTree *dtree,
                                                 float bsdfFraction,
                                                 const float3 &w,
                                                 float bsdfPdf) noexcept {
  return dtree ? guidedMixturePdf(dtree->pdf(w), bsdfPdf, bsdfFraction)
               : bsdfPdf;
}

/// The tree cell participating at a surface vertex: the cell at `point`
/// when guiding is on and the vertex's material has finite lobes for
/// the mixture to draw, else null and the BSDF samples alone.
[[nodiscard]] inline const DTree *guidingCellAt(const Guiding *guiding,
                                                const float3 &point,
                                                bool anyFiniteLobes) noexcept {
  return guiding && guiding->tree && anyFiniteLobes
             ? &guiding->tree->samplingAt(point)
             : nullptr;
}

/// The probability the continuation sampler draws from the BSDF at a
/// vertex whose cell is `dtree`: the cell's learned mixture weight
/// unless pinned for experiments or the cell is absent, 1 with no
/// guiding at all.
[[nodiscard]] inline float bsdfFractionAt(const Guiding *guiding,
                                          const DTree *dtree) noexcept {
  return !dtree || guiding->bsdfFractionFixed
             ? guiding ? guiding->bsdfFraction : 1.0f
             : dtree->mixtureAlpha;
}

/// The discrete chance the continuation at a vertex entered a Dirac
/// lobe: only the BSDF branch of the one-sample MIS can produce a Dirac
/// direction, so this is the vertex's mixture weight where a cell
/// participates, else 1. Every density that competes against a
/// continuation through a Dirac crossing must carry this factor to
/// match what the sampler actually pays there.
[[nodiscard]] inline float diracBranchChance(const Guiding *guiding,
                                             const float3 &point,
                                             bool anyFiniteLobes) noexcept {
  const DTree *dtree{guidingCellAt(guiding, point, anyFiniteLobes)};
  return dtree ? bsdfFractionAt(guiding, dtree) : 1.0f;
}

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
  float wNextPdf{};

  /// The BSDF and SD-tree densities of `wNext` separately, valid only
  /// when the tree participated at the vertex (`pdfGuide >= 0`), and the
  /// spectral average of the BSDF value: what the trainer needs to
  /// estimate each strategy's stand-alone second moment for the cell's
  /// learned mixture weight.
  float wNextBsdfPdf{};
  float wNextGuidePdf{-1.0f};
  float fAvg{};

  /// Was the continuation a Dirac lobe? The tree cannot learn Dirac
  /// directions, so the trainer skips them.
  bool isDiracBounce{};

  /// Did the walk escape to the environment here? Escape radiance is not
  /// part of the reflected field the tree trains on.
  bool isInfiniteLight{};
};

/// Train the SD-tree from one completed path's records, in path order as
/// `tracePath` appended them. The deposits land in the calling thread's
/// mirror of `accumulator`; the tree itself is only read.
void trainGuiding(const STree &tree, GuideAccumulator &accumulator,
                  Sampler &sampler, const GuideRecord *records,
                  uint64_t numRecords);

/// Combination of a guided render's passes, each folded in at plain
/// sample-count weight (deliberately; see `foldPass()`). Also maintains
/// the box-blurred pixel value estimates that drive adjoint-driven
/// Russian roulette between passes.
class PassCombiner final {
public:
  /// Construct for a frame of `nX` by `nY` pixels, of which `window` is
  /// the rectangle actually being rendered.
  PassCombiner(size_t nX, size_t nY, int4 window)
      : mNumPixelsX(nX), mNumPixelsY(nY), mWindow(window),
        mNumBands(renderGrid().numBands),
        mHalfImageA(nX * nY * renderGrid().numBands),
        mHalfImageB(nX * nY * renderGrid().numBands), //
        mHalfSquaresA(nX * nY), mHalfSquaresB(nX * nY),
        mComboNumer(nX * nY * renderGrid().numBands, 0.0),
        mComboDenom(nX * nY, 0.0) {}

  /// Seed the combination with a resumed prior session's accumulation,
  /// folded in as if it were an already-rendered pass of the film's
  /// own sample count. Because every pass folds in at sample-count
  /// weight, `resolve()` then reproduces the exact merged accumulation
  /// of both sessions, and `rebuildPixelEstimates()` can inform the
  /// first pass's ADRRS from the resumed image.
  void seed(const smdl::SpectralFilm &film);

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
  /// single-pixel noise does not drive the roulette. Both the estimates
  /// and their blur stay inside the render window, where the samples
  /// are.
  void rebuildPixelEstimates();

  /// The pixel's value estimate, or 0 before the first
  /// `rebuildPixelEstimates()`.
  [[nodiscard]] float pixelEstimate(size_t pixelIndex) const noexcept {
    return mImageEstimate.empty() ? 0.0f : mImageEstimate[pixelIndex];
  }

  /// Resolve the combination into the film every downstream output
  /// reads from, replacing its contents. The folded sample count
  /// becomes the film's sample count, so the sums-plus-count
  /// invariant holds and the means read back as the combined image.
  void resolve(smdl::SpectralFilm &film) const;

private:
  // The image dimensions in pixels.
  size_t mNumPixelsX{};
  size_t mNumPixelsY{};

  // The rendered pixel rectangle, the whole image unless -crop-window
  // narrows it. Every buffer below stays frame-sized and indexed by the
  // frame pixel index; only the estimates restrict to this.
  int4 mWindow{};

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
