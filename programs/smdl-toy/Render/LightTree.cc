#include "Render/LightTree.h"

#include "smdl/Support/Macros.h"

[[nodiscard]] static float halfDiagonalSq(const BoundBox3 &box) noexcept {
  return box.isEmpty() ? 0.0f : lengthSquared(0.5f * box.extent());
}

LightTree::LightTree(smdl::Span<const LightBounds> lights, int maxDepth) {
  mTrails.assign(lights.size(), INVALID_INDEX);
  mPhis.assign(lights.size(), 0.0f);
  auto order{std::vector<uint32_t>()};
  for (uint32_t i = 0; i < lights.size(); i++) {
    if (lights[i].phi > 0.0f && !lights[i].box.isEmpty()) {
      order.push_back(i);
      mPhis[i] = lights[i].phi;
    }
  }
  if (order.empty()) return;
  mNodes.reserve(2 * order.size());
  mLeafLights.reserve(order.size());
  (void)build(order, lights, 0, order.size(), 0, 0,
              std::clamp(maxDepth, 0, MAX_DEPTH));
}

uint32_t LightTree::build(std::vector<uint32_t> &order,
                          smdl::Span<const LightBounds> lights, size_t begin,
                          size_t end, int depth, uint32_t trail, int maxDepth) {
  const auto nodeIndex{uint32_t(mNodes.size())};
  {
    auto node{Node()};
    for (size_t i = begin; i < end; i++) {
      node.box.extend(lights[order[i]].box);
      node.phi += lights[order[i]].phi;
    }
    node.center = node.box.center();
    node.radiusSq = halfDiagonalSq(node.box);
    mNodes.push_back(node);
  }
  mDepth = std::max(mDepth, depth);
  const size_t count{end - begin};
  if (count == 1 || depth >= maxDepth) {
    mNodes[nodeIndex].link = uint32_t(mLeafLights.size());
    mNodes[nodeIndex].lightCount = uint32_t(count);
    for (size_t i = begin; i < end; i++) {
      mLeafLights.push_back(order[i]);
      mTrails[order[i]] = trail;
    }
    return nodeIndex;
  }
  // The split: bucket the lights by box center along each axis and
  // take the cheapest of the bucket boundaries, the cost of a side
  // being its power times its squared half diagonal. That is the
  // radius the importance clamps by, so the cost measures exactly what
  // a fat cluster costs the traversal; the box surface area of the
  // usual heuristic is zero for a row of lamps along a road, where it
  // would split one lamp off at a time.
  constexpr int NUM_BUCKETS = 12;
  const auto &box{mNodes[nodeIndex].box};
  const float3 extent{box.extent()};
  auto bucketOf{[&](uint32_t light, int axis) {
    const float t{(lights[light].box.center()[axis] - box.lower[axis]) /
                  extent[axis]};
    return std::clamp(int(t * float(NUM_BUCKETS)), 0, NUM_BUCKETS - 1);
  }};
  float bestCost{INF};
  int bestAxis{-1};
  int bestBucket{-1};
  for (int axis = 0; axis < 3; axis++) {
    if (!(extent[axis] > 0.0f)) continue;
    BoundBox3 bucketBoxes[NUM_BUCKETS]{};
    float bucketPhis[NUM_BUCKETS]{};
    int bucketCounts[NUM_BUCKETS]{};
    for (size_t i = begin; i < end; i++) {
      const int b{bucketOf(order[i], axis)};
      bucketBoxes[b].extend(lights[order[i]].box);
      bucketPhis[b] += lights[order[i]].phi;
      bucketCounts[b]++;
    }
    // Suffix sides first, then sweep the prefix.
    BoundBox3 rightBoxes[NUM_BUCKETS]{};
    float rightPhis[NUM_BUCKETS]{};
    int rightCounts[NUM_BUCKETS]{};
    for (int b = NUM_BUCKETS - 1; b >= 0; b--) {
      rightBoxes[b] = b + 1 < NUM_BUCKETS ? rightBoxes[b + 1] : BoundBox3();
      rightBoxes[b].extend(bucketBoxes[b]);
      rightPhis[b] =
          (b + 1 < NUM_BUCKETS ? rightPhis[b + 1] : 0.0f) + bucketPhis[b];
      rightCounts[b] =
          (b + 1 < NUM_BUCKETS ? rightCounts[b + 1] : 0) + bucketCounts[b];
    }
    BoundBox3 leftBox{};
    float leftPhi{};
    int leftCount{};
    for (int b = 0; b + 1 < NUM_BUCKETS; b++) {
      leftBox.extend(bucketBoxes[b]);
      leftPhi += bucketPhis[b];
      leftCount += bucketCounts[b];
      if (leftCount == 0 || rightCounts[b + 1] == 0) continue;
      const float cost{leftPhi * halfDiagonalSq(leftBox) +
                       rightPhis[b + 1] * halfDiagonalSq(rightBoxes[b + 1])};
      if (cost < bestCost) {
        bestCost = cost;
        bestAxis = axis;
        bestBucket = b;
      }
    }
  }
  size_t mid{};
  if (bestAxis >= 0) {
    mid = size_t(
        std::stable_partition(order.begin() + begin, order.begin() + end,
                              [&](uint32_t light) {
                                return bucketOf(light, bestAxis) <= bestBucket;
                              }) -
        order.begin());
  } else {
    // Every center in one bucket on every axis: halve by center along
    // the longest axis, ties by index, which is what keeps the build
    // stable.
    int axis{0};
    for (int a = 1; a < 3; a++)
      if (extent[a] > extent[axis]) axis = a;
    std::stable_sort(order.begin() + begin, order.begin() + end,
                     [&](uint32_t a, uint32_t b) {
                       const float ca{lights[a].box.center()[axis]};
                       const float cb{lights[b].box.center()[axis]};
                       return ca != cb ? ca < cb : a < b;
                     });
    mid = begin + count / 2;
  }
  SMDL_SANITY_CHECK(mid > begin && mid < end);
  (void)build(order, lights, begin, mid, depth + 1, trail, maxDepth);
  const uint32_t right{build(order, lights, mid, end, depth + 1,
                             trail | (1u << depth), maxDepth)};
  mNodes[nodeIndex].link = right;
  return nodeIndex;
}

float LightTree::importance(const Node &node,
                            const float3 &point) const noexcept {
  // The power over the mean squared distance to the cluster's lights,
  // modeled as spread uniformly over the box: by the parallel axis
  // theorem that mean is the squared distance to the center plus a
  // third of the squared half diagonal. Clamping the distance to the
  // radius instead, the usual guard against the singularity inside a
  // cluster, is blind there: a receiver inside both children's spheres
  // then descends by power alone, and a receiver under a field of
  // lamps reaches its own lamp no more often than any other. The
  // self-intersection offset is as close as a receiver ever stands to
  // a point light, which keeps a leaf finite.
  const float distSq{lengthSquared(point - node.center)};
  return node.phi / std::max(distSq + node.radiusSq * (1.0f / 3.0f), EPS * EPS);
}

float LightTree::leftProbability(uint32_t nodeIndex,
                                 const float3 &point) const noexcept {
  const float left{importance(mNodes[nodeIndex + 1], point)};
  const float right{importance(mNodes[mNodes[nodeIndex].link], point)};
  const float sum{left + right};
  return sum > 0.0f ? left / sum : 0.5f;
}

int LightTree::sample(const float3 &point, float xi,
                      float &pmf) const noexcept {
  pmf = 0.0f;
  if (mNodes.empty()) return 0;
  uint32_t nodeIndex{0};
  float p{1.0f};
  for (;;) {
    const auto &node{mNodes[nodeIndex]};
    if (node.lightCount == 1) {
      pmf = p;
      return int(mLeafLights[node.link]);
    }
    if (node.lightCount > 1) {
      // A depth-capped leaf draws by weight, the running sum in the
      // order the node's own weight was summed in.
      const float target{xi * node.phi};
      float cmf{};
      for (uint32_t i = 0; i < node.lightCount; i++) {
        const uint32_t light{mLeafLights[node.link + i]};
        cmf += mPhis[light];
        if (target < cmf || i + 1 == node.lightCount) {
          pmf = p * mPhis[light] / node.phi;
          return int(light);
        }
      }
    }
    const float pLeft{leftProbability(nodeIndex, point)};
    if (xi < pLeft) {
      p *= pLeft;
      xi = clampUnit(xi / pLeft);
      nodeIndex++;
    } else {
      p *= 1.0f - pLeft;
      xi = clampUnit((xi - pLeft) / (1.0f - pLeft));
      nodeIndex = node.link;
    }
  }
}

float LightTree::pmf(int lightIndex, const float3 &point) const noexcept {
  if (lightIndex < 0 || size_t(lightIndex) >= mTrails.size()) return 0.0f;
  uint32_t trail{mTrails[lightIndex]};
  if (trail == INVALID_INDEX) return 0.0f;
  uint32_t nodeIndex{0};
  float p{1.0f};
  for (;;) {
    const auto &node{mNodes[nodeIndex]};
    if (node.lightCount > 0)
      return node.lightCount == 1 ? p : p * mPhis[lightIndex] / node.phi;
    const float pLeft{leftProbability(nodeIndex, point)};
    if (trail & 1u) {
      p *= 1.0f - pLeft;
      nodeIndex = node.link;
    } else {
      p *= pLeft;
      nodeIndex++;
    }
    trail >>= 1;
  }
}
