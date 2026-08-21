#include "Guiding.h"

#include "smdl/Support/Logger.h"

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

void DTree::record(float2 uv, float value) noexcept {
  uint32_t n{0};
  while (true) {
    int q{descendQuadrant(uv)};
    mNodes[n].flux[q].add(value);
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
  const float uniformPDF{smdl::uniformSpherePDF()};
  if (!(totalFlux() > 0)) return uniformPDF;
  float2 uv{directionToSquare(w)};
  float pdf{uniformPDF};
  uint32_t n{0};
  while (true) {
    int q{descendQuadrant(uv)};
    float total{};
    for (int i = 0; i < 4; i++) total += mNodes[n].flux[i];
    float flux{mNodes[n].flux[q]};
    if (!(total > 0) || !(flux > 0)) return 0.0f;
    pdf *= 4.0f * flux / total;
    uint32_t c{mNodes[n].child[q]};
    if (c == 0) return pdf;
    n = c;
  }
}

float3 DTree::sampleDirection(Sampler &sampler, float &pdf) const noexcept {
  const float uniformPDF{smdl::uniformSpherePDF()};
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
    for (int i = 0; i < 4; i++) total += flux[i] = mNodes[n].flux[i];
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
  statisticalWeight = prev.statisticalWeight;
  const float total{prev.totalFlux()};
  if (!(total > 0)) return;
  const float threshold{rho * total};
  // Walk the previous structure, subdividing above-threshold quadrants
  // (synthesizing equal-split children where the previous tree had none)
  // and collapsing the rest.
  struct Item final {
    uint32_t dst{};    //< The node being filled in.
    uint32_t prev{};   //< The corresponding previous node.
    bool prevValid{};  //< Is there a corresponding previous node?
    float synthFlux{}; //< The flux to split equally if not.
    int depth{};
  };
  std::vector<Item> stack{Item{0, 0, true, 0.0f, 1}};
  while (!stack.empty()) {
    Item item{stack.back()};
    stack.pop_back();
    for (int q = 0; q < 4; q++) {
      float flux{item.prevValid ? float(prev.mNodes[item.prev].flux[q])
                                : item.synthFlux / 4.0f};
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
    for (int q = 0; q < 4; q++) node.flux[q] = 0.0f;
  statisticalWeight = 0.0f;
  momentBSDF = 0.0f;
  momentGuide = 0.0f;
}

STree::STree(const float3 &boundsMin, const float3 &boundsMax) : mNodes(1) {
  // Cubify so midpoint splits keep cells roughly isotropic.
  auto extent{boundsMax - boundsMin};
  float maxExtent{
      std::max(std::max(extent.x, extent.y), std::max(extent.z, 1e-4f))};
  mBoundMin = boundsMin;
  mBoundExtent = float3(maxExtent);
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
                   float momentGuide) noexcept {
  // The second-moment statistics deposit unjittered: they estimate the
  // cell's own estimator quality, not a field worth splat-filtering.
  {
    auto &node{mNodes[leafIndex(position)]};
    node.building.momentBSDF.add(momentBSDF);
    node.building.momentGuide.add(momentGuide);
  }
  // Stochastic box filter: jitter the position within the containing
  // leaf's box, then record wherever the jittered position lands.
  float3 boxSize{};
  (void)leafIndex(position, boxSize);
  float3 xi{sampler};
  float3 jittered{position + float3(boxSize.x * (xi.x - 0.5f),
                                    boxSize.y * (xi.y - 0.5f),
                                    boxSize.z * (xi.z - 0.5f))};
  jittered.x = std::fmin(std::fmax(jittered.x, mBoundMin.x),
                         mBoundMin.x + mBoundExtent.x);
  jittered.y = std::fmin(std::fmax(jittered.y, mBoundMin.y),
                         mBoundMin.y + mBoundExtent.y);
  jittered.z = std::fmin(std::fmax(jittered.z, mBoundMin.z),
                         mBoundMin.z + mBoundExtent.z);
  auto &node{mNodes[leafIndex(jittered)]};
  node.recordCount.add(1);
  node.building.statisticalWeight.add(1.0f);
  // And likewise for the direction, within its leaf cell of the building
  // quadtree. The `u` axis is azimuth, so it wraps; `v` clamps.
  float2 uv{directionToSquare(direction)};
  float cell{node.building.leafSize(uv)};
  float2 uvXi{sampler};
  uv = float2(uv.x + cell * (uvXi.x - 0.5f), uv.y + cell * (uvXi.y - 0.5f));
  uv.x -= std::floor(uv.x);
  uv.y = std::fmin(std::fmax(uv.y, 0.0f), ONE_MINUS_EPS);
  node.building.record(uv, value);
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
    // no moment statistics keep their weight.
    const float mf{node.building.momentBSDF};
    const float mg{node.building.momentGuide};
    node.sampling.mixtureAlpha =
        mf + mg > 0
            ? 0.5f * prevAlpha +
                  0.5f * std::fmin(std::fmax(mg / (mf + mg), 0.25f), 0.98f)
            : prevAlpha;
    node.building.resetToStructureOf(node.sampling);
    node.recordCount = 0;
  }
}

void trainGuiding(STree &tree, Sampler &sampler, const GuideRecord *records,
                  uint64_t numRecords) {
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
      if (!record.isDeltaBounce && record.pdfWNext > 0) {
        const float Lhat{(R + record.continuationEmission).average()};
        const float value{Lhat / record.pdfWNext};
        if (std::isfinite(value) && value >= 0) {
          // The bounce's contributions to the cell's per-strategy second
          // moments: `(f L)^2 / (p_s p_mix)` estimates the second moment
          // strategy `s` would have alone. The density floor keeps
          // near-zero tree densities a large but finite penalty instead
          // of an absorbing infinity.
          float momentBSDF{};
          float momentGuide{};
          if (record.pdfGuide >= 0) {
            const float PDF_FLOOR{0.01f * smdl::uniformSpherePDF()};
            const float fL2{(record.fAvg * Lhat) * (record.fAvg * Lhat)};
            momentBSDF =
                fL2 / (std::fmax(record.pdfBSDF, PDF_FLOOR) * record.pdfWNext);
            momentGuide =
                fL2 / (std::fmax(record.pdfGuide, PDF_FLOOR) * record.pdfWNext);
            if (!std::isfinite(momentBSDF) || !std::isfinite(momentGuide))
              momentBSDF = momentGuide = 0.0f;
          }
          tree.record(sampler, record.point, record.wNext, value, momentBSDF,
                      momentGuide);
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

// In-place 3x3 box blur over a single-channel image.
static void boxBlur3(std::vector<float> &image, size_t numPixelsX,
                     size_t numPixelsY) {
  auto source{image};
  for (size_t y = 0; y < numPixelsY; y++) {
    for (size_t x = 0; x < numPixelsX; x++) {
      float sum{};
      int count{};
      for (int dy = -1; dy <= 1; dy++) {
        for (int dx = -1; dx <= 1; dx++) {
          int xx = int(x) + dx, yy = int(y) + dy;
          if (xx < 0 || yy < 0 ||      //
              xx >= int(numPixelsX) || //
              yy >= int(numPixelsY))
            continue;
          sum += source[xx + numPixelsX * yy];
          count++;
        }
      }
      if (count == 0) count = 1; // Should never happen
      image[x + numPixelsX * y] = sum / float(count);
    }
  }
}

void PassCombiner::seed(const smdl::SpectralRenderImage &image,
                        size_t samplesPerPixel) {
  // The given count is used for the weight rather than the image's own
  // per-pixel counts, matching the uniform-count assumption resolve()
  // already makes.
  for (size_t p = 0; p < mNumPixelsX * mNumPixelsY; p++) {
    const auto pixel{image(p % mNumPixelsX, p / mNumPixelsX)};
    mComboDenom[p] += double(samplesPerPixel);
    for (size_t b = 0; b < mNumBands; b++)
      mComboNumer[p * mNumBands + b] += double(pixel.totals[b]);
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
  const double weightA(countA);
  const double weightB(countB);
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
  for (size_t p = 0; p < mNumPixelsX * mNumPixelsY; p++) {
    if (mComboDenom[p] > 0) {
      double sum{};
      for (size_t b = 0; b < mNumBands; b++)
        sum += mComboNumer[p * mNumBands + b];
      mImageEstimate[p] = float(sum / (mNumBands * mComboDenom[p]));
    }
  }
  boxBlur3(mImageEstimate, mNumPixelsX, mNumPixelsY);
}

void PassCombiner::resolve(smdl::SpectralRenderImage &image) const {
  image.resize(mNumBands, mNumPixelsX, mNumPixelsY);
  auto combined{std::vector<double>(mNumBands)};
  for (size_t p = 0; p < mNumPixelsX * mNumPixelsY; p++) {
    if (!(mComboDenom[p] > 0)) continue;
    for (size_t b = 0; b < mNumBands; b++)
      combined[b] =
          mComboNumer[p * mNumBands + b] / mComboDenom[p] * double(mKeptSPP);
    image(p % mNumPixelsX, p / mNumPixelsX)
        .addSamples(mKeptSPP, combined.data());
  }
}
