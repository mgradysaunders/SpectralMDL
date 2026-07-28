#include "guiding.h"

/// The quadrant of `uv`, rescaling `uv` to the quadrant's own unit
/// square. Bit 0 is the upper half in `u`, bit 1 the upper half in `v`.
[[nodiscard]] static int descendQuadrant(float2 &uv) noexcept {
  int q{};
  if (uv.x >= 0.5f)
    q |= 1, uv.x = 2.0f * uv.x - 1.0f;
  else
    uv.x = 2.0f * uv.x;
  if (uv.y >= 0.5f)
    q |= 2, uv.y = 2.0f * uv.y - 1.0f;
  else
    uv.y = 2.0f * uv.y;
  return q;
}

void DTree::record(float2 uv, float value) noexcept {
  uint32_t n{0};
  while (true) {
    int q{descendQuadrant(uv)};
    atomicAdd(nodes[n].flux[q], value);
    uint32_t c{nodes[n].child[q]};
    if (c == 0) break;
    n = c;
  }
}

float DTree::leafSize(float2 uv) const noexcept {
  float size{1.0f};
  uint32_t n{0};
  while (true) {
    int q{descendQuadrant(uv)};
    uint32_t c{nodes[n].child[q]};
    if (c == 0) return size;
    size *= 0.5f;
    n = c;
  }
}

float DTree::pdf(const float3 &w) const noexcept {
  constexpr float uniformPDF{1.0f / (4.0f * PI)};
  if (!(totalFlux() > 0)) return uniformPDF;
  float2 uv{directionToSquare(w)};
  float pdf{uniformPDF};
  uint32_t n{0};
  while (true) {
    int q{descendQuadrant(uv)};
    float total{};
    for (int i = 0; i < 4; i++)
      total += nodes[n].flux[i].load(std::memory_order_relaxed);
    float flux{nodes[n].flux[q].load(std::memory_order_relaxed)};
    if (!(total > 0) || !(flux > 0)) return 0.0f;
    pdf *= 4.0f * flux / total;
    uint32_t c{nodes[n].child[q]};
    if (c == 0) return pdf;
    n = c;
  }
}

float3 DTree::sampleDirection(Sampler &sampler, float &pdf) const noexcept {
  constexpr float uniformPDF{1.0f / (4.0f * PI)};
  if (!(totalFlux() > 0)) {
    pdf = uniformPDF;
    return squareToDirection(float2(sampler));
  }
  pdf = uniformPDF;
  float2 corner{};
  float size{1.0f};
  uint32_t n{0};
  while (true) {
    float flux[4];
    float total{};
    for (int i = 0; i < 4; i++)
      total += flux[i] = nodes[n].flux[i].load(std::memory_order_relaxed);
    if (!(total > 0)) break; // Degenerate: sample uniformly within the box.
    // Choose a quadrant proportional to flux.
    float xi{float(sampler) * total};
    int q{0};
    for (; q < 3; q++) {
      if (xi < flux[q]) break;
      xi -= flux[q];
    }
    pdf *= 4.0f * flux[q] / total;
    size *= 0.5f;
    corner.x += (q & 1) ? size : 0.0f;
    corner.y += (q & 2) ? size : 0.0f;
    uint32_t c{nodes[n].child[q]};
    if (c == 0) break;
    n = c;
  }
  float2 xi{sampler};
  return squareToDirection({corner.x + size * xi.x, corner.y + size * xi.y});
}

void DTree::rebuildFrom(const DTree &prev, float rho, int maxDepth) {
  nodes.assign(1, Node{});
  statisticalWeight.store(
      prev.statisticalWeight.load(std::memory_order_relaxed),
      std::memory_order_relaxed);
  const float total{prev.totalFlux()};
  if (!(total > 0)) return;
  const float threshold{rho * total};
  // Walk the previous structure, subdividing above-threshold quadrants —
  // synthesizing equal-split children where the previous tree had none —
  // and collapsing the rest.
  struct Item final {
    uint32_t dst{};    ///< The node being filled in.
    uint32_t prev{};   ///< The corresponding previous node.
    bool prevValid{};  ///< Is there a corresponding previous node?
    float synthFlux{}; ///< The flux to split equally if not.
    int depth{};
  };
  std::vector<Item> stack{Item{0, 0, true, 0.0f, 1}};
  while (!stack.empty()) {
    Item item{stack.back()};
    stack.pop_back();
    for (int q = 0; q < 4; q++) {
      float flux{item.prevValid ? prev.nodes[item.prev].flux[q].load(
                                      std::memory_order_relaxed)
                                : item.synthFlux / 4.0f};
      nodes[item.dst].flux[q].store(flux, std::memory_order_relaxed);
      if (flux > threshold && item.depth < maxDepth) {
        uint32_t c{uint32_t(nodes.size())};
        nodes.emplace_back();
        nodes[item.dst].child[q] = c;
        uint32_t prevChild{item.prevValid ? prev.nodes[item.prev].child[q] : 0};
        stack.push_back(Item{c, prevChild, item.prevValid && prevChild != 0,
                             flux, item.depth + 1});
      }
    }
  }
}

void DTree::resetToStructureOf(const DTree &structure) {
  nodes = structure.nodes;
  for (auto &node : nodes)
    for (int q = 0; q < 4; q++)
      node.flux[q].store(0.0f, std::memory_order_relaxed);
  statisticalWeight.store(0.0f, std::memory_order_relaxed);
}

STree::STree(const float3 &boundsMin, const float3 &boundsMax) : nodes(1) {
  // Cubify so midpoint splits keep cells roughly isotropic.
  auto extent{boundsMax - boundsMin};
  float maxExtent{
      std::max(std::max(extent.x, extent.y), std::max(extent.z, 1e-4f))};
  boundMin = boundsMin;
  boundExtent = float3(maxExtent);
}

uint32_t STree::leafIndex(const float3 &position) const noexcept {
  float3 unused{};
  return leafIndex(position, unused);
}

uint32_t STree::leafIndex(const float3 &position,
                          float3 &leafBoxSize) const noexcept {
  float3 lo{boundMin};
  float3 size{boundExtent};
  uint32_t n{0};
  while (nodes[n].child[0] != 0) {
    int axis{nodes[n].axis};
    float half{size[axis] * 0.5f};
    if (position[axis] < lo[axis] + half) {
      n = nodes[n].child[0];
    } else {
      n = nodes[n].child[1];
      lo[axis] += half;
    }
    size[axis] = half;
  }
  leafBoxSize = size;
  return n;
}

void STree::record(Sampler &sampler, const float3 &position,
                   const float3 &direction, float value) noexcept {
  // Stochastic box filter: jitter the position within the containing
  // leaf's box, then record wherever the jittered position lands.
  float3 boxSize{};
  (void)leafIndex(position, boxSize);
  float3 xi{sampler};
  float3 jittered{position + float3(boxSize.x * (xi.x - 0.5f),
                                    boxSize.y * (xi.y - 0.5f),
                                    boxSize.z * (xi.z - 0.5f))};
  jittered.x =
      std::fmin(std::fmax(jittered.x, boundMin.x), boundMin.x + boundExtent.x);
  jittered.y =
      std::fmin(std::fmax(jittered.y, boundMin.y), boundMin.y + boundExtent.y);
  jittered.z =
      std::fmin(std::fmax(jittered.z, boundMin.z), boundMin.z + boundExtent.z);
  auto &node{nodes[leafIndex(jittered)]};
  node.recordCount.fetch_add(1, std::memory_order_relaxed);
  atomicAdd(node.building.statisticalWeight, 1.0f);
  // And likewise for the direction, within its leaf cell of the building
  // quadtree. The `u` axis is azimuth, so it wraps; `v` clamps.
  float2 uv{directionToSquare(direction)};
  float cell{node.building.leafSize(uv)};
  float2 uvXi{sampler};
  uv = float2(uv.x + cell * (uvXi.x - 0.5f), uv.y + cell * (uvXi.y - 0.5f));
  uv.x -= std::floor(uv.x);
  uv.y = std::fmin(std::fmax(uv.y, 0.0f), 0.99999994f);
  node.building.record(uv, value);
}

void STree::refine(uint32_t splitThreshold, float rho, int maxDepth) {
  // Split overfull leaves. Children are appended, so this loop reaches
  // them and keeps splitting while their halved counts still exceed the
  // threshold. The cap is a guard against degenerate scenes.
  constexpr size_t MAX_NODES{1u << 16};
  for (size_t n = 0; n < nodes.size(); n++) {
    if (nodes[n].child[0] != 0) continue;
    uint32_t count{nodes[n].recordCount.load(std::memory_order_relaxed)};
    if (count <= splitThreshold || nodes.size() + 2 > MAX_NODES) continue;
    // Copy the parent BEFORE appending: emplacing an element of the same
    // vector is undefined behavior when the append reallocates.
    Node parentCopy{nodes[n]};
    uint32_t c0{uint32_t(nodes.size())};
    nodes.push_back(parentCopy);
    nodes.push_back(parentCopy);
    auto &parent{nodes[n]};
    parent.child = {c0, c0 + 1};
    for (uint32_t c : {c0, c0 + 1}) {
      auto &childNode{nodes[c]};
      childNode.child = {0, 0};
      childNode.axis = uint8_t((parent.axis + 1) % 3);
      childNode.recordCount.store(count / 2, std::memory_order_relaxed);
    }
    parent.sampling = DTree();
    parent.building = DTree();
  }
  // Rebuild every leaf's sampling quadtree from what the pass recorded
  // and zero the building side for the next pass; likewise freeze the
  // pass's suffix statistics for reading and clear the write side.
  for (auto &node : nodes) {
    if (node.child[0] != 0) continue;
    node.sampling.rebuildFrom(node.building, rho, maxDepth);
    node.building.resetToStructureOf(node.sampling);
    node.recordCount.store(0, std::memory_order_relaxed);
  }
}
