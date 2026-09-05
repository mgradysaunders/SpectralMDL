#include "Render/Guiding.h"
#include "IO/BinaryFile.h"

#include "smdl/Support/Logger.h"
#include "smdl/Support/Parallel.h"

#include <algorithm>
#include <atomic>
#include <fstream>

// The fixed-point unit for a counter's next pass, from the total it
// reached in the previous one: 2^30 units across the previous total is
// resolution to spare, leaves 2^33 growth headroom in 64 bits, and puts
// the per-record cap three decades above the whole previous total,
// where only a genuinely absurd firefly clamps. The floor covers a
// fresh tree with no total yet. See `DTree::fluxUnit`.
[[nodiscard]] static float nextUnit(float total) noexcept {
  return std::max(total * 0x1p-30f, 0x1p-20f);
}

// One record's value as fixed-point units: the fraction rounds
// stochastically so the expected value survives any scale, in double so
// the fraction is meaningful across the whole capped range.
[[nodiscard]] static uint64_t unitsOf(Sampler &sampler, float value,
                                      float unit) noexcept {
  if (!(value > 0.0f)) return 0;
  const double u{std::min(double(value) / double(unit), 0x1p40)};
  const auto lo{uint64_t(u)};
  return lo + (float(sampler) < float(u - double(lo)) ? 1 : 0);
}

// The quadrant of `uv`, rescaling `uv` to the quadrant's own unit
// square. Bit 0 is the upper half in `u`, bit 1 the upper half in `v`.
[[nodiscard]] static int descendQuadrant(float2 &uv) noexcept {
  int q{};
  if (uv.x >= 0.5f) {
    q |= 1;
    uv.x = 2.0f * uv.x - 1.0f;
  } else {
    uv.x = 2.0f * uv.x;
  }
  if (uv.y >= 0.5f) {
    q |= 2;
    uv.y = 2.0f * uv.y - 1.0f;
  } else {
    uv.y = 2.0f * uv.y;
  }
  return q;
}

void DTree::record(float2 uv, uint64_t units, uint64_t *flux) const noexcept {
  uint32_t n{0};
  while (true) {
    int q{descendQuadrant(uv)};
    flux[4 * n + q] += units;
    uint32_t c{mNodes[n].child[q]};
    if (c == 0) break;
    n = c;
  }
}

float DTree::leafSize(float2 uv) const noexcept {
  float size{1.0f};
  uint32_t n{0};
  while (true) {
    int q{descendQuadrant(uv)};
    uint32_t c{mNodes[n].child[q]};
    if (c == 0) return size;
    size *= 0.5f;
    n = c;
  }
}

float DTree::pdf(const float3 &w) const noexcept {
  const float uniformPdf{smdl::uniformSpherePDF()};
  if (totalFluxUnits() == 0) return uniformPdf;
  float2 uv{directionToSquare(w)};
  float pdf{uniformPdf};
  uint32_t n{0};
  while (true) {
    int q{descendQuadrant(uv)};
    uint64_t total{};
    for (int i = 0; i < 4; i++) total += mNodes[n].flux[i];
    uint64_t flux{mNodes[n].flux[q]};
    if (total == 0 || flux == 0) return 0.0f;
    pdf *= 4.0f * (float(flux) / float(total));
    uint32_t c{mNodes[n].child[q]};
    if (c == 0) return pdf;
    n = c;
  }
}

float3 DTree::sampleDirection(Sampler &sampler, float &pdf) const noexcept {
  const float uniformPdf{smdl::uniformSpherePDF()};
  if (totalFluxUnits() == 0) {
    pdf = uniformPdf;
    return squareToDirection(float2(sampler));
  }
  pdf = uniformPdf;
  float2 corner{};
  float size{1.0f};
  uint32_t n{0};
  while (true) {
    float flux[4];
    float total{};
    for (int i = 0; i < 4; i++)
      total += flux[i] = float(uint64_t(mNodes[n].flux[i]));
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
    uint32_t c{mNodes[n].child[q]};
    if (c == 0) break;
    n = c;
  }
  float2 xi{sampler};
  return squareToDirection({corner.x + size * xi.x, corner.y + size * xi.y});
}

void DTree::rebuildFrom(const DTree &prev, float rho, int maxDepth) {
  mNodes.assign(1, Node{});
  statisticalWeight = uint64_t(prev.statisticalWeight);
  fluxUnit = prev.fluxUnit;
  momentUnit = prev.momentUnit;
  const uint64_t total{prev.totalFluxUnits()};
  if (total == 0) return;
  const auto threshold{uint64_t(double(rho) * double(total))};
  // Walk the previous structure, subdividing above-threshold quadrants
  // (synthesizing equal-split children where the previous tree had none)
  // and collapsing the rest.
  struct Item final {
    uint32_t dst{};       //< The node being filled in.
    uint32_t prev{};      //< The corresponding previous node.
    bool prevValid{};     //< Is there a corresponding previous node?
    uint64_t synthFlux{}; //< The flux to split equally if not.
    int depth{};
  };
  std::vector<Item> stack{Item{0, 0, true, 0, 1}};
  while (!stack.empty()) {
    Item item{stack.back()};
    stack.pop_back();
    for (int q = 0; q < 4; q++) {
      uint64_t flux{item.prevValid ? uint64_t(prev.mNodes[item.prev].flux[q])
                                   : item.synthFlux / 4};
      mNodes[item.dst].flux[q] = flux;
      if (flux > threshold && item.depth < maxDepth) {
        uint32_t c{uint32_t(mNodes.size())};
        mNodes.emplace_back();
        mNodes[item.dst].child[q] = c;
        uint32_t prevChild{item.prevValid ? prev.mNodes[item.prev].child[q]
                                          : 0};
        stack.push_back(Item{c, prevChild, item.prevValid && prevChild != 0,
                             flux, item.depth + 1});
      }
    }
  }
}

void DTree::resetToStructureOf(const DTree &structure) {
  mNodes = structure.mNodes;
  for (auto &node : mNodes)
    for (int q = 0; q < 4; q++) node.flux[q] = 0;
  statisticalWeight = 0;
  momentBSDF = 0;
  momentGuide = 0;
  // The next pass's scale, from the total the structure just collected;
  // the caller sets `momentUnit`, knowing the moment totals it consumed.
  fluxUnit = nextUnit(structure.totalFlux());
}

STree::STree(const float3 &boundsMin, const float3 &boundsMax) : mNodes(1) {
  // Cubify so midpoint splits keep cells roughly isotropic.
  auto extent{boundsMax - boundsMin};
  float maxExtent{std::max({extent.x, extent.y, extent.z, 1e-4f})};
  mBoundMin = boundsMin;
  mBoundExtent = float3(maxExtent);
  buildCounterLayout();
}

void STree::buildCounterLayout() {
  mCounterOffset.assign(mNodes.size(), 0);
  uint64_t offset{};
  for (size_t n = 0; n < mNodes.size(); n++) {
    if (mNodes[n].child[0] != 0) continue;
    mCounterOffset[n] = offset;
    offset += 4 + 4 * mNodes[n].building.mNodes.size();
  }
  mCounterCount = offset;
}

void STree::absorb(const std::vector<const uint64_t *> &mirrors) {
  smdl::parallelFor(0, mNodes.size(), [&](size_t n) {
    auto &node{mNodes[n]};
    if (node.child[0] != 0) return;
    const uint64_t offset{mCounterOffset[n]};
    for (const uint64_t *mirror : mirrors) {
      const uint64_t *c{mirror + offset};
      node.recordCount += uint32_t(c[0]);
      node.building.statisticalWeight += c[1];
      node.building.momentBSDF += c[2];
      node.building.momentGuide += c[3];
      const uint64_t *flux{c + 4};
      for (size_t q = 0; q < node.building.mNodes.size(); q++)
        for (int i = 0; i < 4; i++)
          node.building.mNodes[q].flux[i] += flux[4 * q + i];
    }
  });
}

uint32_t STree::leafIndex(const float3 &position) const noexcept {
  float3 unused{};
  return leafIndex(position, unused);
}

uint32_t STree::leafIndex(const float3 &position,
                          float3 &leafBoxSize) const noexcept {
  float3 lo{mBoundMin};
  float3 size{mBoundExtent};
  uint32_t n{0};
  while (mNodes[n].child[0] != 0) {
    int axis{mNodes[n].axis};
    float half{size[axis] * 0.5f};
    if (position[axis] < lo[axis] + half) {
      n = mNodes[n].child[0];
    } else {
      n = mNodes[n].child[1];
      lo[axis] += half;
    }
    size[axis] = half;
  }
  leafBoxSize = size;
  return n;
}

void STree::record(Sampler &sampler, const float3 &position,
                   const float3 &direction, float value, float momentBSDF,
                   float momentGuide, uint64_t *counters) const noexcept {
  // One descent at `position` serves both the moment deposit and the
  // jitter box below; only the jittered position needs its own.
  float3 boxSize{};
  const uint32_t at{leafIndex(position, boxSize)};
  // The second-moment statistics deposit unjittered: they estimate the
  // cell's own estimator quality, not a field worth splat-filtering.
  {
    const auto &node{mNodes[at]};
    uint64_t *c{counters + mCounterOffset[at]};
    const float unit{node.building.momentUnit};
    c[2] += unitsOf(sampler, momentBSDF, unit);
    c[3] += unitsOf(sampler, momentGuide, unit);
  }
  // Stochastic box filter: jitter the position within the containing
  // leaf's box, then record wherever the jittered position lands.
  float3 xi{sampler};
  float3 jittered{position + float3(boxSize.x * (xi.x - 0.5f),
                                    boxSize.y * (xi.y - 0.5f),
                                    boxSize.z * (xi.z - 0.5f))};
  jittered.x =
      std::clamp(jittered.x, mBoundMin.x, mBoundMin.x + mBoundExtent.x);
  jittered.y =
      std::clamp(jittered.y, mBoundMin.y, mBoundMin.y + mBoundExtent.y);
  jittered.z =
      std::clamp(jittered.z, mBoundMin.z, mBoundMin.z + mBoundExtent.z);
  const uint32_t jat{leafIndex(jittered)};
  const auto &node{mNodes[jat]};
  uint64_t *c{counters + mCounterOffset[jat]};
  c[0] += 1;
  c[1] += 1;
  // And likewise for the direction, within its leaf cell of the building
  // quadtree. The `u` axis is azimuth, so it wraps; `v` clamps.
  float2 uv{directionToSquare(direction)};
  float cell{node.building.leafSize(uv)};
  float2 uvXi{sampler};
  uv = float2(uv.x + cell * (uvXi.x - 0.5f), uv.y + cell * (uvXi.y - 0.5f));
  uv.x -= std::floor(uv.x);
  uv.y = std::clamp(uv.y, 0.0f, ONE_MINUS_EPS);
  if (const uint64_t units{unitsOf(sampler, value, node.building.fluxUnit)})
    node.building.record(uv, units, c + 4);
}

void STree::refine(uint32_t splitThreshold, float rho, int maxDepth) {
  // Split overfull leaves. Children are appended, so this loop reaches
  // them and keeps splitting while their halved counts still exceed the
  // threshold. The cap is a guard against degenerate scenes.
  constexpr size_t MAX_NODES{1u << 16};
  for (size_t n = 0; n < mNodes.size(); n++) {
    if (mNodes[n].child[0] != 0) continue;
    uint32_t count{mNodes[n].recordCount};
    if (count <= splitThreshold || mNodes.size() + 2 > MAX_NODES) continue;
    // Copy the parent BEFORE appending: emplacing an element of the same
    // vector is undefined behavior when the append reallocates.
    Node parentCopy{mNodes[n]};
    uint32_t c0{uint32_t(mNodes.size())};
    mNodes.push_back(parentCopy);
    mNodes.push_back(parentCopy);
    auto &parent{mNodes[n]};
    parent.child = {c0, c0 + 1};
    for (uint32_t c : {c0, c0 + 1}) {
      auto &childNode{mNodes[c]};
      childNode.child = {0, 0};
      childNode.axis = uint8_t((parent.axis + 1) % 3);
      childNode.recordCount = count / 2;
    }
    parent.sampling = DTree();
    parent.building = DTree();
  }
  // Rebuild every leaf's sampling quadtree from what the pass recorded
  // and zero the building side for the next pass; likewise freeze the
  // pass's suffix statistics for reading and clear the write side.
  for (auto &node : mNodes) {
    if (node.child[0] != 0) continue;
    const float prevAlpha{node.sampling.mixtureAlpha};
    node.sampling.rebuildFrom(node.building, rho, maxDepth);
    // The learned mixture weight: each strategy in proportion to its
    // inverse estimated stand-alone second moment, clamped so neither
    // strategy is ever shut out, and blended with the previous weight so
    // one noisy pass cannot swing a cell to either extreme. Cells with
    // no moment statistics keep their weight. The units cancel in the
    // ratio; the totals still scale the next pass's `momentUnit`.
    const float unit{node.building.momentUnit};
    const float mf{float(uint64_t(node.building.momentBSDF)) * unit};
    const float mg{float(uint64_t(node.building.momentGuide)) * unit};
    node.sampling.mixtureAlpha =
        mf + mg > 0
            ? 0.5f * prevAlpha + 0.5f * std::clamp(mg / (mf + mg), 0.25f, 0.98f)
            : prevAlpha;
    node.building.resetToStructureOf(node.sampling);
    node.building.momentUnit = nextUnit(mf + mg);
    node.recordCount = 0;
  }
  buildCounterLayout();
}

// The saved-tree reader and writer: explicit-width little-endian I/O
// over the layout documented at `STree::writeFile()`. The in-memory
// nodes hold atomics and would drag their padding into the file, so
// every record goes through one of these mirror structs instead.
namespace {

class TreeFileHeader final {
public:
  char magic[8]{};
  uint16_t version{};
  uint16_t reserved{};
  uint32_t nodeCount{};
  uint64_t samplesPerPixel{};
  float boundMin[3]{};
  float boundExtent[3]{};
};

class TreeFileSpatialNode final {
public:
  uint32_t child[2]{};
  uint32_t axis{};
};

class TreeFileLeaf final {
public:
  uint64_t statisticalWeight{};
  float mixtureAlpha{};
  float fluxUnit{};
  float momentUnit{};
  uint32_t quadNodeCount{};
};

class TreeFileQuadNode final {
public:
  uint64_t flux[4]{};
  uint32_t child[4]{};
};

static_assert(sizeof(TreeFileHeader) == 48, "the header is 48 bytes");
static_assert(sizeof(TreeFileSpatialNode) == 12, "a spatial node is 12 bytes");
static_assert(sizeof(TreeFileLeaf) == 24, "a leaf payload is 24 bytes");
static_assert(sizeof(TreeFileQuadNode) == 48, "a quadtree node is 48 bytes");

} // namespace

void STree::writeFile(const std::string &fileName,
                      uint64_t samplesPerPixel) const {
  requireLittleEndianHost("guide tree");
  auto stream{std::ofstream(fileName, std::ios::binary)};
  if (!stream)
    throw smdl::Error(
        smdl::concat("cannot write guide tree ", smdl::QuotedPath(fileName)));
  auto header{TreeFileHeader()};
  setMagic(header.magic, GUIDE_TREE_MAGIC);
  header.version = 1;
  header.nodeCount = uint32_t(mNodes.size());
  header.samplesPerPixel = samplesPerPixel;
  for (int i = 0; i < 3; i++) {
    header.boundMin[i] = mBoundMin[i];
    header.boundExtent[i] = mBoundExtent[i];
  }
  putRecord(stream, header);
  for (const auto &node : mNodes) {
    auto spatial{TreeFileSpatialNode()};
    spatial.child[0] = node.child[0];
    spatial.child[1] = node.child[1];
    spatial.axis = node.axis;
    putRecord(stream, spatial);
    if (node.child[0] != 0) continue;
    const auto &sampling{node.sampling};
    auto leaf{TreeFileLeaf()};
    leaf.statisticalWeight = uint64_t(sampling.statisticalWeight);
    leaf.mixtureAlpha = sampling.mixtureAlpha;
    leaf.fluxUnit = sampling.fluxUnit;
    leaf.momentUnit = node.building.momentUnit;
    leaf.quadNodeCount = uint32_t(sampling.mNodes.size());
    putRecord(stream, leaf);
    for (const auto &quad : sampling.mNodes) {
      auto quadFile{TreeFileQuadNode()};
      for (int q = 0; q < 4; q++) {
        quadFile.flux[q] = uint64_t(quad.flux[q]);
        quadFile.child[q] = quad.child[q];
      }
      putRecord(stream, quadFile);
    }
  }
  if (!stream)
    throw smdl::Error(
        smdl::concat("cannot write guide tree ", smdl::QuotedPath(fileName)));
}

STree STree::readFile(const std::string &fileName, uint64_t &samplesPerPixel) {
  requireLittleEndianHost("guide tree");
  auto stream{std::ifstream(fileName, std::ios::binary)};
  if (!stream)
    throw smdl::Error(
        smdl::concat("cannot open guide tree ", smdl::QuotedPath(fileName)));
  const auto corrupt{[&](const char *what) {
    return smdl::Error(smdl::concat("cannot read guide tree ",
                                    smdl::QuotedPath(fileName), ": ", what));
  }};
  auto header{TreeFileHeader()};
  getRecord(stream, header);
  if (!stream || !hasMagic(header.magic, GUIDE_TREE_MAGIC))
    throw corrupt("bad magic; expected it to begin with \"SMDLSDTR\"");
  if (header.version != 1)
    throw smdl::Error(smdl::concat(
        "cannot read guide tree ", smdl::QuotedPath(fileName), ": version ",
        header.version, " (this build reads version 1)"));
  if (header.reserved != 0) throw corrupt("the reserved field is not 0");
  // Caps against a corrupt count allocating the world. The spatial cap
  // is `refine()`'s own; the quadtree cap is far past what its depth
  // limit and flux threshold can produce.
  if (header.nodeCount == 0 || header.nodeCount > (1u << 16))
    throw corrupt("implausible spatial node count");
  for (int i = 0; i < 3; i++)
    if (!(header.boundExtent[i] > 0.0f) ||
        !std::isfinite(header.boundExtent[i]) ||
        !std::isfinite(header.boundMin[i]))
      throw corrupt("degenerate bounds");
  auto tree{STree()};
  samplesPerPixel = header.samplesPerPixel;
  tree.mBoundMin = {header.boundMin[0], header.boundMin[1], header.boundMin[2]};
  tree.mBoundExtent = {header.boundExtent[0], header.boundExtent[1],
                       header.boundExtent[2]};
  tree.mNodes.resize(header.nodeCount);
  for (auto &node : tree.mNodes) {
    auto spatial{TreeFileSpatialNode()};
    getRecord(stream, spatial);
    if (!stream) throw corrupt("truncated");
    if ((spatial.child[0] == 0) != (spatial.child[1] == 0) ||
        spatial.child[0] >= header.nodeCount ||
        spatial.child[1] >= header.nodeCount || spatial.axis >= 3)
      throw corrupt("spatial node does not add up");
    node.child = {spatial.child[0], spatial.child[1]};
    node.axis = uint8_t(spatial.axis);
    if (node.child[0] != 0) continue;
    auto leaf{TreeFileLeaf()};
    getRecord(stream, leaf);
    if (!stream) throw corrupt("truncated");
    if (!(leaf.fluxUnit > 0.0f) || !std::isfinite(leaf.fluxUnit) ||
        !(leaf.momentUnit > 0.0f) || !std::isfinite(leaf.momentUnit) ||
        !std::isfinite(leaf.mixtureAlpha) || leaf.quadNodeCount == 0 ||
        leaf.quadNodeCount > (1u << 24))
      throw corrupt("leaf payload does not add up");
    auto &sampling{node.sampling};
    sampling.mNodes.resize(leaf.quadNodeCount);
    for (auto &quad : sampling.mNodes) {
      auto quadFile{TreeFileQuadNode()};
      getRecord(stream, quadFile);
      if (!stream) throw corrupt("truncated");
      for (int q = 0; q < 4; q++) {
        if (quadFile.child[q] >= leaf.quadNodeCount)
          throw corrupt("quadtree node does not add up");
        quad.flux[q] = quadFile.flux[q];
        quad.child[q] = quadFile.child[q];
      }
    }
    sampling.statisticalWeight = leaf.statisticalWeight;
    sampling.mixtureAlpha = leaf.mixtureAlpha;
    sampling.fluxUnit = leaf.fluxUnit;
    sampling.momentUnit = leaf.momentUnit;
    // The building side exactly as `refine()` left it in the saving
    // session: structure and flux unit rederived from the sampling side,
    // the moment unit restored from the file.
    node.building.resetToStructureOf(sampling);
    node.building.momentUnit = leaf.momentUnit;
  }
  tree.buildCounterLayout();
  return tree;
}

// The calling thread's mirror slot, claimed once per thread for the
// life of the process. Only the render pool and the main thread ever
// record, so the slots stay within `getThreadCount() + 1`.
[[nodiscard]] static unsigned threadMirrorSlot() noexcept {
  static std::atomic<unsigned> nextSlot{};
  static thread_local unsigned slot{
      nextSlot.fetch_add(1, std::memory_order_relaxed)};
  return slot;
}

GuideAccumulator::GuideAccumulator(const STree &tree)
    : mTree(tree), mMirrors(smdl::getThreadCount() + 1) {}

uint64_t *GuideAccumulator::local() {
  const unsigned slot{threadMirrorSlot()};
  SMDL_SANITY_CHECK(slot < mMirrors.size());
  auto &mirror{mMirrors[slot]};
  if (mirror.empty()) mirror.resize(mTree.counterCount());
  return mirror.data();
}

void GuideAccumulator::absorbInto(STree &tree) const {
  SMDL_SANITY_CHECK(&tree == &mTree);
  auto mirrors{std::vector<const uint64_t *>()};
  for (const auto &mirror : mMirrors)
    if (!mirror.empty()) mirrors.push_back(mirror.data());
  tree.absorb(mirrors);
}

void trainGuiding(const STree &tree, GuideAccumulator &accumulator,
                  Sampler &sampler, const GuideRecord *records,
                  uint64_t numRecords) {
  uint64_t *counters{accumulator.local()};
  // Walk the path backward, reconstructing the radiance estimate along
  // every sampled continuation direction, and splat it in. `R` carries
  // the reflected-radiance estimate leaving the next vertex; the
  // RECORDED target is `R` plus the MIS-weighted first-hit emission or
  // escape radiance the continuation landed on: exactly the field the
  // continuation half of the estimator is responsible for. Total
  // incident radiance would aim continuations at emitters light sampling
  // already covers; the reflected field alone would aim them away from
  // the MIS-compensated sky residual only the continuation can reach.
  // Zero-valued records still count, so refinement reflects every
  // trainable continuation rather than only the lucky ones.
  Color R{};
  for (uint64_t i = numRecords; i-- > 0;) {
    const GuideRecord &record{records[i]};
    if (record.isInfiniteLight) {
      R = Color();
      continue;
    }
    if (i + 1 < numRecords) {
      // The continuation resolved: on an escape the sentinel has already
      // reset `R` to zero and `continuationEmission` holds the
      // MIS-weighted escape radiance.
      if (!record.isDiracBounce && record.wNextPdf > 0) {
        const float Lhat{(R + record.continuationEmission).average()};
        const float value{Lhat / record.wNextPdf};
        if (std::isfinite(value) && value >= 0) {
          // The bounce's contributions to the cell's per-strategy second
          // moments: `(f L)^2 / (p_s p_mix)` estimates the second moment
          // strategy `s` would have alone. The density floor keeps
          // near-zero tree densities a large but finite penalty instead
          // of an absorbing infinity.
          float momentBsdf{};
          float momentGuide{};
          if (record.wNextGuidePdf >= 0) {
            const float PDF_FLOOR{0.01f * smdl::uniformSpherePDF()};
            const float fL2{(record.fAvg * Lhat) * (record.fAvg * Lhat)};
            momentBsdf = fL2 / (std::max(record.wNextBsdfPdf, PDF_FLOOR) *
                                record.wNextPdf);
            momentGuide = fL2 / (std::max(record.wNextGuidePdf, PDF_FLOOR) *
                                 record.wNextPdf);
            if (!std::isfinite(momentBsdf) || !std::isfinite(momentGuide))
              momentBsdf = momentGuide = 0.0f;
          }
          tree.record(sampler, record.point, record.wNext, value, momentBsdf,
                      momentGuide, counters);
        }
      }
      if (!records[i + 1].isInfiniteLight) {
        // R(i) = D_i + w_i * R(i+1), where the bounce weight w_i is
        // recovered from the stored throughputs.
        Color w{};
        for (size_t b = 0; b < record.beta.size(); b++)
          w[b] = record.beta[b] > 0 ? records[i + 1].beta[b] / record.beta[b]
                                    : 0.0f;
        R = record.direct + w * R;
      } else {
        R = record.direct;
      }
    } else {
      // The walk ended at this vertex, so its own continuation never
      // resolved and there is nothing to record along it.
      R = record.direct;
    }
    if (R.isAnyNonFinite()) R = Color();
  }
}

// In-place 3x3 box blur over a single-channel image, confined to the
// window: outside it there are no samples and no estimate, so averaging
// that in would only drag the border pixels' estimates toward black.
static void boxBlur3(std::vector<float> &image, size_t numPixelsX,
                     int4 window) {
  auto source{image};
  for (int y = window[1]; y < window[3]; y++) {
    for (int x = window[0]; x < window[2]; x++) {
      float sum{};
      int count{};
      for (int dy = -1; dy <= 1; dy++) {
        for (int dx = -1; dx <= 1; dx++) {
          int xx = x + dx, yy = y + dy;
          if (xx < window[0] || yy < window[1] || //
              xx >= window[2] || yy >= window[3])
            continue;
          sum += source[size_t(xx) + numPixelsX * size_t(yy)];
          count++;
        }
      }
      if (count == 0) count = 1; // Should never happen
      image[size_t(x) + numPixelsX * size_t(y)] = sum / float(count);
    }
  }
}

void PassCombiner::seed(const smdl::SpectralFilm &film) {
  const auto samplesPerPixel{film.getNumSamples()};
  for (size_t p = 0; p < mNumPixelsX * mNumPixelsY; p++) {
    const auto totals{film.totals(p % mNumPixelsX, p / mNumPixelsX)};
    mComboDenom[p] += double(samplesPerPixel);
    for (size_t b = 0; b < mNumBands; b++)
      mComboNumer[p * mNumBands + b] += totals[b];
  }
  mKeptSPP += samplesPerPixel;
}

void PassCombiner::deposit(size_t pixelIndex,
                           const PixelHalves &halves) noexcept {
  for (size_t b = 0; b < mNumBands; b++) {
    mHalfImageA[pixelIndex * mNumBands + b] = halves.halfA[b];
    mHalfImageB[pixelIndex * mNumBands + b] = halves.halfB[b];
  }
  mHalfSquaresA[pixelIndex] = halves.squaresA;
  mHalfSquaresB[pixelIndex] = halves.squaresB;
}

void PassCombiner::foldPass(size_t passSamples) {
  const size_t numPixels{mNumPixelsX * mNumPixelsY};
  const size_t countA{(passSamples + 1) / 2};
  const size_t countB{passSamples / 2};
  // Every pass folds in at plain sample-count weight, deliberately NOT
  // inverse-variance weight: the per-sample distribution is heavy-tailed
  // (a near-specular material under a sun), so a small pass's image-mean
  // variance estimate almost never catches the tail, and cross-half IVW
  // hands the early low-spp passes orders of magnitude too much weight.
  // Count weights concede a few percent of dilution from the badly
  // guided early passes and are never wrong by more than that. If early
  // passes ever need fading, weight by a ROBUST noise statistic (e.g.
  // the median over pixels of the squared half-image difference); the
  // halves are retained for that. The per-half variance sums below
  // survive purely as a debug diagnostic of the tail.
  const double weightA{double(countA)};
  const double weightB{double(countB)};
  if (countB >= 2) {
    double varSumA{};
    double varSumB{};
    for (size_t p = 0; p < numPixels; p++) {
      double meanA{};
      double meanB{};
      for (size_t b = 0; b < mNumBands; b++) {
        meanA += double(mHalfImageA[p * mNumBands + b]);
        meanB += double(mHalfImageB[p * mNumBands + b]);
      }
      meanA /= double(mNumBands * countA);
      meanB /= double(mNumBands * countB);
      // Per-sample variance within each half.
      double ex2A{double(mHalfSquaresA[p]) / double(countA)};
      double ex2B{double(mHalfSquaresB[p]) / double(countB)};
      varSumA += std::max(0.0, ex2A - meanA * meanA) *
                 (double(countA) / double(countA - 1));
      varSumB += std::max(0.0, ex2B - meanB * meanB) *
                 (double(countB) / double(countB - 1));
    }
    SMDL_LOG_DEBUG("foldPass: spp ", passSamples,          //
                   ", varA ", varSumA / double(numPixels), //
                   ", varB ", varSumB / double(numPixels));
  }
  for (size_t p = 0; p < numPixels; p++) {
    mComboDenom[p] += weightA + weightB;
    for (size_t b = 0; b < mNumBands; b++) {
      double value{weightA * double(mHalfImageA[p * mNumBands + b]) /
                   double(countA)};
      if (countB > 0)
        value +=
            weightB * double(mHalfImageB[p * mNumBands + b]) / double(countB);
      mComboNumer[p * mNumBands + b] += value;
    }
  }
  mKeptSPP += passSamples;
}

void PassCombiner::rebuildPixelEstimates() {
  mImageEstimate.assign(mNumPixelsX * mNumPixelsY, 0.0f);
  for (int y = mWindow[1]; y < mWindow[3]; y++) {
    for (int x = mWindow[0]; x < mWindow[2]; x++) {
      const size_t p{size_t(x) + mNumPixelsX * size_t(y)};
      if (mComboDenom[p] > 0) {
        double sum{};
        for (size_t b = 0; b < mNumBands; b++)
          sum += mComboNumer[p * mNumBands + b];
        mImageEstimate[p] = float(sum / (double(mNumBands) * mComboDenom[p]));
      }
    }
  }
  boxBlur3(mImageEstimate, mNumPixelsX, mWindow);
}

void PassCombiner::resolve(smdl::SpectralFilm &film) const {
  film.resize(mNumBands, mNumPixelsX, mNumPixelsY);
  film.addSamples(mKeptSPP);
  auto combined{std::vector<double>(mNumBands)};
  for (size_t p = 0; p < mNumPixelsX * mNumPixelsY; p++) {
    if (!(mComboDenom[p] > 0)) continue;
    for (size_t b = 0; b < mNumBands; b++)
      combined[b] =
          mComboNumer[p * mNumBands + b] / mComboDenom[p] * double(mKeptSPP);
    film.addTotals(p % mNumPixelsX, p / mNumPixelsX, combined.data());
  }
}
