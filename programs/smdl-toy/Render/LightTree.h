#pragma once

#include <vector>

#include "Common.h"

#include "smdl/RenderUtil/MonteCarlo.h"
#include "smdl/Support/Span.h"

/// What light selection keeps per light for the `LightTree`: the
/// world-space box of the emitting geometry and the selection weight,
/// the radiant power in the units every kind of light weighs by.
class LightBounds final {
public:
  BoundBox3 box{};
  float phi{};
};

/// A bounding volume hierarchy over the lights, for drawing a light
/// with a probability that depends on where the receiver is (after
/// Conty Estevez and Kulla, "Importance Sampling of Many Lights with
/// Adaptive Tree Splitting", 2018, without the orientation terms and
/// without splitting). A cluster's importance to a receiver is its
/// power over the mean squared distance to its lights, and the
/// traversal picks a child in proportion to the two children's
/// importances, so a receiver next to one lamp of a thousand draws that
/// lamp most of the time and a receiver far from all of them draws by
/// power alone.
///
/// Deliberately blind to normals and emission cones: a light facing
/// away from the receiver keeps a probability proportional to its
/// power, which the manifold gathers rely on (their connection arrives
/// at the light from elsewhere than the receiver), and so recomputing
/// a probability at an arrival needs the receiver's position and
/// nothing else.
///
/// Every light with a weight is a leaf of its own, so the probability of
/// any light for any receiver is recomputable by walking its path from
/// the root with the same arithmetic `sample()` accumulates, which is
/// what MIS at an arrival needs. The path is a bit trail of at most
/// `MAX_DEPTH` bits; a build that reaches the cap closes the remaining
/// lights into one leaf drawn by weight.
///
/// The build is sequential and stable, so one scene builds one tree
/// whatever the thread count, which keeps renders reproducible.
class LightTree final {
public:
  /// The depth cap, what a 32-bit trail holds with bit 31 to spare.
  static constexpr int MAX_DEPTH = 31;

  LightTree() = default;

  /// Build over the lights in index order. A light with no weight or no
  /// box is left out: never drawn, and `pmf()` reports zero for it.
  /// `maxDepth` below `MAX_DEPTH` makes fatter leaves sooner.
  explicit LightTree(smdl::Span<const LightBounds> lights,
                     int maxDepth = MAX_DEPTH);

  /// Is there nothing to draw?
  [[nodiscard]] bool empty() const noexcept { return mNodes.empty(); }

  /// Draw a light for the receiver at `point` on the uniform `xi`,
  /// returning its index and filling `pmf` with its probability.
  [[nodiscard]] int sample(const float3 &point, float xi,
                           float &pmf) const noexcept;

  /// The probability that `sample()` draws `lightIndex` for the
  /// receiver at `point`.
  [[nodiscard]] float pmf(int lightIndex, const float3 &point) const noexcept;

  [[nodiscard]] size_t nodeCount() const noexcept { return mNodes.size(); }

  /// The depth of the deepest leaf, the root at zero.
  [[nodiscard]] int depth() const noexcept { return mDepth; }

private:
  class Node final {
  public:
    BoundBox3 box{};
    float phi{};
    float3 center{};
    float radiusSq{};

    /// Interior: the index of the right child, the left child being the
    /// next node. Leaf: the first index into the leaf light list.
    uint32_t link{};

    /// The number of lights in a leaf, zero for an interior node.
    uint32_t lightCount{};
  };

  [[nodiscard]] float importance(const Node &node,
                                 const float3 &point) const noexcept;

  /// The probability of stepping into the left child of the interior
  /// node at `nodeIndex` from `point`, the one function both `sample()`
  /// and `pmf()` read.
  [[nodiscard]] float leftProbability(uint32_t nodeIndex,
                                      const float3 &point) const noexcept;

  uint32_t build(std::vector<uint32_t> &order,
                 smdl::Span<const LightBounds> lights, size_t begin, size_t end,
                 int depth, uint32_t trail, int maxDepth);

  std::vector<Node> mNodes{};

  /// The light indices of every leaf, in leaf order.
  std::vector<uint32_t> mLeafLights{};

  /// Per input light, the path from the root to its leaf, bit `d` the
  /// choice at depth `d` (set for the right child), or `INVALID_INDEX`
  /// for a light the tree left out. Bit 31 is never a path bit, so the
  /// sentinel cannot collide with a trail.
  std::vector<uint32_t> mTrails{};

  /// Per input light, its weight, which a depth-capped leaf draws by.
  std::vector<float> mPhis{};

  int mDepth{};
};
