#pragma once

#include "raytracing.h"

#include <atomic>

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
/// while a pass renders — recording only touches the atomic flux — and is
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

  DTree() : nodes(1) {}

  DTree(const DTree &other) : nodes(other.nodes) {
    statisticalWeight.store(
        other.statisticalWeight.load(std::memory_order_relaxed),
        std::memory_order_relaxed);
  }

  DTree &operator=(const DTree &other) {
    nodes = other.nodes;
    statisticalWeight.store(
        other.statisticalWeight.load(std::memory_order_relaxed),
        std::memory_order_relaxed);
    return *this;
  }

  /// The total flux at the root.
  [[nodiscard]] float totalFlux() const noexcept {
    float total{};
    for (int q = 0; q < 4; q++)
      total += nodes[0].flux[q].load(std::memory_order_relaxed);
    return total;
  }

  /// The mean incident radiance implied by the recorded flux: each record
  /// is an unbiased estimate of the flux `∫ L dω = 4π L̄`, so dividing the
  /// total by the record count and the sphere area recovers `L̄`.
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

private:
  std::vector<Node> nodes;
};

/// The spatial half of the SD-tree: a binary tree over the (cubified)
/// scene bounds with midpoint splits on alternating axes. Each leaf holds
/// a pair of directional quadtrees — `sampling` is frozen and read during
/// a pass, `building` accumulates the pass's records — swapped and
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
    return nodes[leafIndex(position)].sampling;
  }

  /// Record an incident-radiance estimate: `value` arrived at `position`
  /// from `direction`, already divided by the density that sampled the
  /// direction. Splats with the stochastic box filter of the 2019 "Path
  /// Guiding in Production" course: the position is jittered within the
  /// spatial leaf and the direction within the directional leaf.
  void record(Sampler &sampler, const float3 &position, const float3 &direction,
              float value) noexcept;

  /// Refine between passes: split spatial leaves whose record count
  /// exceeds `splitThreshold` (children copy their parent's quadtrees and
  /// halve its count), then rebuild every leaf's sampling quadtree from
  /// the flux its building quadtree collected and zero the building side.
  void refine(uint32_t splitThreshold, float rho, int maxDepth);

  /// The number of spatial leaves, for progress diagnostics.
  [[nodiscard]] size_t leafCount() const noexcept {
    size_t count{};
    for (const auto &node : nodes)
      if (node.child[0] == 0) count++;
    return count;
  }

private:
  [[nodiscard]] uint32_t leafIndex(const float3 &position) const noexcept;

  /// The leaf containing `position` along with the size of its box, for
  /// the record jitter.
  [[nodiscard]] uint32_t leafIndex(const float3 &position,
                                   float3 &leafBoxSize) const noexcept;

  std::vector<Node> nodes;

  float3 boundMin{};

  float3 boundExtent{};
};

/// The per-pixel guiding context handed to `random_walk`. A null `tree`
/// disables guiding entirely and the walk behaves as before.
struct Guiding final {
  /// The SD-tree, read-only during the walk.
  const STree *tree{};

  /// The pixel's value estimate from the passes so far (spectral mean),
  /// or 0 if there is none yet. Drives adjoint-driven Russian roulette
  /// (Vorba & Křivánek, SIGGRAPH 2016, roulette only): survival is
  /// proportional to the expected pixel contribution of continuing.
  float pixelEstimate{};
};
