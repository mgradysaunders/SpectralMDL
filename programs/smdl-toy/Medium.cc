#include "Medium.h"

#include <mutex>
#include <set>

#include "smdl/Resource/VoxelGrid.h"
#include "smdl/Support/Logger.h"

// The cap on tentative collisions per segment, a guard against
// marching forever through unbounded or leaky geometry with a positive
// majorant. A segment that exhausts it is treated as fully absorbed;
// reaching the cap honestly would mean an optical depth in the tens of
// thousands.
static constexpr int MAX_TENTATIVE_COLLISIONS{65536};

// Warn once per material about a heterogeneous volume that fails to
// declare a majorant for a coefficient it uses, and therefore falls
// back to the homogeneous treatment.
static void warnMissingMajorantOnce(const smdl::JIT::Material *material) {
  static std::mutex mutex{};
  static std::set<const smdl::JIT::Material *> warned{};
  const auto lock{std::lock_guard(mutex)};
  if (warned.insert(material).second)
    SMDL_LOG_WARN(
        "material ", smdl::Quoted(material->materialName),
        " has a heterogeneous volume but no majorant for every "
        "coefficient it uses (see "
        "'material_volume.max_absorption_coefficient' and "
        "'max_scattering_coefficient'); treating the volume as "
        "homogeneous with the coefficients evaluated at the surface hit");
}

// Warn once per material about a spatially varying volume inside an
// instance that shears or scales non-uniformly. The queries run in the
// instance's rigid frame, which is the only frame in which the direction
// stays unit length and distances stay in scene units, so the boundary is
// deformed while the interior is not.
static void warnDeformedVolumeOnce(const smdl::JIT::Material *material) {
  static std::mutex mutex{};
  static std::set<const smdl::JIT::Material *> warned{};
  const auto lock{std::lock_guard(mutex)};
  if (warned.insert(material).second)
    SMDL_LOG_WARN("material ", smdl::Quoted(material->materialName),
                  " has a spatially varying volume inside an instance that "
                  "shears or scales non-uniformly; the surface is deformed "
                  "but the volume it encloses is not");
}

// One majorant span of a segment: over `[t0, t1)` in scene units the
// tracking majorant is the declared majorant scaled by `scale`, and the
// declared majorant scaled by `scaleMin` is a lower bound of the
// extinction over the span, which residual ratio tracking uses as an
// analytic control. Without the density hint the bounds are the
// trivial 1 and 0.
struct MajorantSpan final {
  float t0{};
  float t1{};
  float scale{};
  float scaleMin{};
};

// Iterates the majorant spans of one segment. Without a density hint
// there is a single span at the global scale of 1. With the hint, an
// Amanatides-Woo walk over the grid bricks inside the hint box yields
// one span per brick at the brick's dilated-maximum-over-global-maximum
// scale (empty bricks yield zero-scale spans the tracking loops skip),
// and the portions of the segment outside the hint box are conservative
// global-scale spans, since clamp-wrapped coefficients out there hold
// edge values the per-brick maxima do not bound.
class MajorantSpanIterator final {
public:
  MajorantSpanIterator(const smdl::VoxelGrid *grid, const float3 &brickOrg,
                       const float3 &brickDir, float invMaxValue,
                       float tEnd) noexcept
      : mGrid(grid), mBrickOrg(brickOrg), mBrickDir(brickDir),
        mInvMaxValue(invMaxValue), mTEnd(tEnd) {
    if (!mGrid) return;
    // Clip the segment against the brick-space box with the slab test.
    // The box upper corner is the fractional brick extent, so partial
    // bricks at the high boundary are covered exactly.
    const auto extent{mGrid->getExtent()};
    const float3 boxMax{extent.x / 16.0f, extent.y / 16.0f, extent.z / 16.0f};
    float tEnter{0.0f};
    float tExit{tEnd};
    for (int axis = 0; axis < 3 && tEnter <= tExit; axis++) {
      const float o{mBrickOrg[axis]};
      const float v{mBrickDir[axis]};
      if (v != 0.0f) {
        const float tA{(0.0f - o) / v};
        const float tB{(boxMax[axis] - o) / v};
        tEnter = std::max(tEnter, std::min(tA, tB));
        tExit = std::min(tExit, std::max(tA, tB));
      } else if (o < 0.0f || o > boxMax[axis]) {
        tEnter = tEnd;
        tExit = 0.0f;
      }
    }
    if (!(tEnter < tExit)) {
      // The segment never enters the hint box: one global span.
      mGrid = nullptr;
      return;
    }
    mTEnter = tEnter;
    mTExit = tExit;
    // Set up the brick walk at the entry point.
    const auto brickCount{mGrid->getBrickCount()};
    const float3 pEnter{mBrickOrg + tEnter * mBrickDir};
    for (int axis = 0; axis < 3; axis++) {
      mCell[axis] =
          std::clamp(int(std::floor(pEnter[axis])), 0, brickCount[axis] - 1);
      if (mBrickDir[axis] > 0.0f) {
        mStep[axis] = 1;
        mTNext[axis] =
            (float(mCell[axis] + 1) - mBrickOrg[axis]) / mBrickDir[axis];
        mTDelta[axis] = 1.0f / mBrickDir[axis];
      } else if (mBrickDir[axis] < 0.0f) {
        mStep[axis] = -1;
        mTNext[axis] = (float(mCell[axis]) - mBrickOrg[axis]) / mBrickDir[axis];
        mTDelta[axis] = -1.0f / mBrickDir[axis];
      } else {
        mStep[axis] = 0;
        mTNext[axis] = INF;
        mTDelta[axis] = INF;
      }
    }
  }

  // The next span, or false when the segment is exhausted.
  [[nodiscard]] bool next(MajorantSpan &span) noexcept {
    if (!mGrid) {
      // No hint (or the segment misses the hint box): one global span.
      if (mDone) return false;
      mDone = true;
      span = {0.0f, mTEnd, 1.0f};
      return mTEnd > 0.0f;
    }
    if (mPhase == 0) {
      // The stretch before the hint box, at the global scale.
      mPhase = 1;
      mTCur = mTEnter;
      if (mTEnter > 0.0f) {
        span = {0.0f, mTEnter, 1.0f};
        return true;
      }
    }
    if (mPhase == 1) {
      // One span per brick.
      if (mTCur < mTExit) {
        const int axis{mTNext[0] < mTNext[1] ? (mTNext[0] < mTNext[2] ? 0 : 2)
                                             : (mTNext[1] < mTNext[2] ? 1 : 2)};
        const float tCellEnd{std::min(mTNext[axis], mTExit)};
        const float scale{
            std::min(mGrid->getBrickMaxValue(mCell[0], mCell[1], mCell[2]) *
                         mInvMaxValue,
                     1.0f)};
        // The dilated brick minimum lower-bounds every trilinear value
        // in the brick the same way the dilated maximum upper-bounds
        // it. Clamped into `[0, scale]` defensively: a grid holding
        // negative values must not produce a negative control.
        const float scaleMin{
            std::clamp(mGrid->getBrickMinValue(mCell[0], mCell[1], mCell[2]) *
                           mInvMaxValue,
                       0.0f, scale)};
        span = {mTCur, tCellEnd, scale, scaleMin};
        mTCur = tCellEnd;
        mCell[axis] += mStep[axis];
        mTNext[axis] += mTDelta[axis];
        return true;
      }
      mPhase = 2;
      // The stretch after the hint box, at the global scale.
      if (mTExit < mTEnd) {
        span = {mTExit, mTEnd, 1.0f};
        return true;
      }
    }
    return false;
  }

private:
  const smdl::VoxelGrid *mGrid{};
  float3 mBrickOrg{};
  float3 mBrickDir{};
  float mInvMaxValue{};
  float mTEnd{};
  float mTEnter{};
  float mTExit{};
  float mTCur{};
  int mCell[3]{};
  int mStep[3]{};
  float mTNext[3]{};
  float mTDelta[3]{};
  int mPhase{};
  bool mDone{};
};

Medium::Medium(const MediumStack *stack, const Color &wavelengths,
               const float3 &org, const float3 &dir) noexcept {
  if (!stack || (!stack->materialInstance.hasMedium() &&
                 stack->materialInstance.getVolumeEmissionIntensity().empty()))
    return;
  mHasMedium = true;
  const auto &materialInstance{stack->materialInstance};
  mMaterial = materialInstance.material;
  mState = makeRenderState(wavelengths);
  // Coefficients are in inverse meters per the MDL specification;
  // distances here are in scene units. smdl-toy renders with the
  // default meters-per-scene-unit of 1, so this is the identity, but
  // the conversion is where a unit-aware scene flag would land.
  const float unitScale{mState.meters_per_scene_unit};
  mSigmaA = Color(materialInstance.getAbsorptionCoefficient()) * unitScale;
  mSigmaS = Color(materialInstance.getScatteringCoefficient()) * unitScale;
  mHasEmission = !materialInstance.getVolumeEmissionIntensity().empty();
  mEmission = Color(materialInstance.getVolumeEmissionIntensity()) * unitScale;
  if (mMaterial->hasHomogeneousVolume()) return;
  // Heterogeneous (or unproven, which must be treated the same): the
  // per-point queries need majorants to track against, covering every
  // coefficient the material actually has.
  const bool missingMajorantA{
      !materialInstance.getAbsorptionCoefficient().empty() &&
      materialInstance.getMaxAbsorptionCoefficient().empty()};
  const bool missingMajorantS{
      !materialInstance.getScatteringCoefficient().empty() &&
      materialInstance.getMaxScatteringCoefficient().empty()};
  if (missingMajorantA || missingMajorantS) {
    warnMissingMajorantOnce(mMaterial);
    return;
  }
  mHeterogeneous = true;
  mMaxSigmaA =
      Color(materialInstance.getMaxAbsorptionCoefficient()) * unitScale;
  mMaxSigmaS =
      Color(materialInstance.getMaxScatteringCoefficient()) * unitScale;
  mMajorant = (mMaxSigmaA + mMaxSigmaS).maxComponent();
  // The queries evaluate in the rigid frame of the instance whose
  // boundary entered the medium, paired with the rigid transform so
  // world reassembly inside the material is exact. The rigid transform
  // has no scale, so the direction stays unit length and distances
  // stay in scene units. A medium with no geometry queries in world
  // space directly.
  if (const auto *meshInstance{stack->meshInstance}) {
    if (meshInstance->isDeformed) warnDeformedVolumeOnce(mMaterial);
    mOrgR = float3(meshInstance->worldToRigid * float4(org, 1.0f));
    mDirR = float3(meshInstance->worldToRigid * float4(dir, 0.0f));
    mState.object_to_world_matrix = meshInstance->rigidToWorld;
  } else {
    mOrgR = org;
    mDirR = dir;
  }
  mState.direction = mDirR;
  // The density acceleration hint, active only when the material
  // declares all three fields and they are usable. The segment is
  // mapped into the grid's brick space up front: the hint box spans
  // texture space [0,1]^3, which spans the voxel extent, and bricks
  // are 16 voxels per axis.
  const auto *densityGrid{materialInstance.getVolumeDensityGrid()};
  const auto *boundMin{materialInstance.getVolumeDensityBoundMin()};
  const auto *boundMax{materialInstance.getVolumeDensityBoundMax()};
  if (densityGrid && boundMin && boundMax && densityGrid->isValid() &&
      densityGrid->getMaxValue() > 0.0f && //
      boundMax->x > boundMin->x &&         //
      boundMax->y > boundMin->y &&         //
      boundMax->z > boundMin->z) {
    mDensityGrid = densityGrid;
    const auto extent{densityGrid->getExtent()};
    const float3 scale{float(extent.x) / (16.0f * (boundMax->x - boundMin->x)),
                       float(extent.y) / (16.0f * (boundMax->y - boundMin->y)),
                       float(extent.z) / (16.0f * (boundMax->z - boundMin->z))};
    for (int axis = 0; axis < 3; axis++) {
      mBrickOrg[axis] = (mOrgR[axis] - (*boundMin)[axis]) * scale[axis];
      mBrickDir[axis] = mDirR[axis] * scale[axis];
    }
    mInvMaxValue = 1.0f / densityGrid->getMaxValue();
  }
}

void Medium::evaluateCoefficients(float t, float majorantScale, Color &sigmaA,
                                  Color &sigmaS, Color &emission) const {
  mState.position = mOrgR + t * mDirR;
  mMaterial->volumeEvaluate(mState, sigmaA.data(), sigmaS.data(),
                            emission.data());
  // Convert to inverse scene units and clamp to the declared majorants
  // at the local scale, so a lying majorant or density hint renders a
  // clamped medium instead of accumulating negative-weight bias. The
  // emission coefficient has no majorant and only clamps nonnegative:
  // it never gates sampling, so no bound is needed for unbiasedness.
  const float unitScale{mState.meters_per_scene_unit};
  for (size_t i = 0; i < sigmaA.size(); i++) {
    sigmaA[i] = std::min(std::max(sigmaA[i] * unitScale, 0.0f),
                         mMaxSigmaA[i] * majorantScale);
    sigmaS[i] = std::min(std::max(sigmaS[i] * unitScale, 0.0f),
                         mMaxSigmaS[i] * majorantScale);
    emission[i] = std::max(emission[i] * unitScale, 0.0f);
  }
}

bool Medium::sampleDistance(Sampler &sampler, float tEnd, float &t, Color &beta,
                            Color &emitted) const {
  if (!mHasMedium) return false;
  if (!mHeterogeneous) {
    // The emitted radiance along the segment is deterministic for a
    // homogeneous medium: the integral of transmittance times the
    // emission coefficient to the segment end, per bin, regardless of
    // where scattering is sampled below. The extinction-free limit is
    // linear in distance, so an unbounded segment through an emissive
    // vacuum is clamped rather than infinite.
    if (mHasEmission) {
      const Color mu{mSigmaA + mSigmaS};
      const float tEmit{std::min(tEnd, 1e8f)};
      for (size_t i = 0; i < mu.size(); i++)
        emitted[i] +=
            mEmission[i] * (mu[i] > 1e-12f
                                ? (1.0f - std::exp(-mu[i] * tEmit)) / mu[i]
                                : tEmit);
    }
    // The closed-form homogeneous estimator: sample the free-flight
    // distance against one uniformly drawn hero wavelength and weight
    // by the single-sample MIS balance heuristic over all bins. The
    // caller clamps nothing: wavelengths with zero extinction keep
    // transmittance 1 through the min against FLT_MAX.
    const Color mu{mSigmaA + mSigmaS};
    const float xi{float(sampler)};
    const int hero{sampler.index(int(mu.size()))};
    const float tScatter{-std::log1p(-xi) / mu[hero]};
    Color Tr{};
    for (size_t i = 0; i < mu.size(); i++)
      Tr[i] = std::exp(-mu[i] * std::min({tScatter, tEnd,
                                          std::numeric_limits<float>::max()}));
    if (tScatter < tEnd) {
      beta *= mSigmaS * Tr / (mu * Tr).average();
      t = tScatter;
      return true;
    }
    beta *= Tr / Tr.average();
    return false;
  }
  // Delta tracking against the scalar majorant, generalizing the same
  // spectral strategy: the hero wavelength drives the real-or-null
  // classification, and the per-bin products of null factors carry the
  // balance-heuristic weight through the chain. 'P' is meaningful only
  // up to a common scale, which cancels in every weight, so it is
  // renormalized as it goes to keep the products from underflowing.
  //
  // The tracking loop draws from a plain generator seeded by the
  // sampler rather than from the sampler itself, so that this call
  // consumes a FIXED number of low-discrepancy dimensions: burning a
  // variable number here would push every draw after the medium onto
  // different dimensions from sample to sample, destroying the
  // stratification of the rest of the path.
  if (!(mMajorant > 0.0f)) return false;
  const int hero{sampler.index(int(mSigmaA.size()))};
  auto rng{
      smdl::RNG((uint64_t(sampler.nextBits()) << 32) | sampler.nextBits())};
  Color P{1.0f};
  int iter{0};
  auto spans{MajorantSpanIterator(mDensityGrid, mBrickOrg, mBrickDir,
                                  mInvMaxValue, tEnd)};
  MajorantSpan span{};
  while (spans.next(span)) {
    // The local tracking majorant of this span. A zero-scale span is
    // an empty brick: skipped outright, at no cost of any kind. The
    // exponential restart at each span boundary is unbiased by
    // memorylessness.
    const float m{mMajorant * span.scale};
    if (!(m > 0.0f)) continue;
    float tCur{span.t0};
    while (true) {
      tCur += -std::log1p(-rng.generateFloat()) / m;
      if (!(tCur < span.t1)) break;
      if (++iter > MAX_TENTATIVE_COLLISIONS) {
        beta = Color();
        return false;
      }
      Color sigmaA{}, sigmaS{};
      Color emission{};
      evaluateCoefficients(tCur, span.scale, sigmaA, sigmaS, emission);
      // Accumulate the medium's own emission at every tentative
      // collision, before classification: the chain-survival density
      // times the per-bin null-product weight integrates, in
      // expectation, transmittance times the emission coefficient over
      // the whole segment, at no extra 'volumeEvaluate' cost. The same
      // balance-heuristic normalization as the terminal weights
      // applies, and the common renormalization of 'P' cancels.
      if (mHasEmission) {
        const float pdfEmit{m * P.average()};
        if (pdfEmit > 0.0f) emitted += emission * P / pdfEmit;
      }
      const Color muT{sigmaA + sigmaS};
      if (rng.generateFloat() * m < muT[hero]) {
        // A real collision. Weight by the scattering coefficient over
        // the mixture density of all heroes having produced this chain;
        // absorption is folded into the weight rather than terminating,
        // exactly like the homogeneous path.
        const float pdf{(muT * P).average()};
        if (!(pdf > 0.0f)) {
          beta = Color();
          return false;
        }
        beta *= sigmaS * P / pdf;
        t = tCur;
        return true;
      }
      // A null collision against the local majorant, which the local
      // clamp in 'evaluateCoefficients' keeps non-negative per bin.
      P *= m - muT;
      const float renormalize{P.maxComponent()};
      if (!(renormalize > 0.0f)) {
        // Every bin hit the majorant: the chain carries no throughput
        // in any wavelength, so the path is dead.
        beta = Color();
        return false;
      }
      P *= 1.0f / renormalize;
    }
  }
  beta *= P / P.average();
  return false;
}

void Medium::attenuate(Sampler &sampler, float tEnd, Color &beta) const {
  if (!mHasMedium) return;
  if (!mHeterogeneous) {
    // Closed-form Beer-Lambert. The caller clamps 'tEnd' finite, so
    // wavelengths with zero extinction keep transmittance 1 instead of
    // producing 0 times infinity.
    const Color mu{mSigmaA + mSigmaS};
    for (size_t i = 0; i < mu.size(); i++) beta[i] *= std::exp(-mu[i] * tEnd);
    return;
  }
  // Residual ratio tracking (Novak et al. 2014): per span, the
  // extinction splits into a piecewise-constant control (the declared
  // majorant scaled by the span's lower bound) plus a residual. The
  // control transmittance is analytic, accumulated as an optical depth
  // and exponentiated once at the end; only the residual is ratio
  // tracked, at rate proportional to the majorant-minus-control gap. A
  // brick whose bounds coincide costs no collisions at all; without a
  // density hint the control is zero and this reduces to plain ratio
  // tracking. The per-bin expectation is exactly the transmittance for
  // residuals of either sign, so no hero selection or MIS weighting is
  // involved. The loop draws from a seeded generator for the same
  // fixed-dimension-count reason as in 'sampleDistance'.
  if (!(mMajorant > 0.0f)) return;
  auto rng{
      smdl::RNG((uint64_t(sampler.nextBits()) << 32) | sampler.nextBits())};
  int iter{0};
  auto spans{MajorantSpanIterator(mDensityGrid, mBrickOrg, mBrickDir,
                                  mInvMaxValue, tEnd)};
  MajorantSpan span{};
  Color controlDepth{};
  const Color majorantColor{mMaxSigmaA + mMaxSigmaS};
  while (spans.next(span)) {
    if (span.scaleMin > 0.0f)
      controlDepth += majorantColor * (span.scaleMin * (span.t1 - span.t0));
    const float m{mMajorant * (span.scale - span.scaleMin)};
    if (!(m > 0.0f)) continue;
    float tCur{span.t0};
    while (true) {
      tCur += -std::log1p(-rng.generateFloat()) / m;
      if (!(tCur < span.t1)) break;
      if (++iter > MAX_TENTATIVE_COLLISIONS) {
        beta = Color();
        return;
      }
      Color sigmaA{}, sigmaS{};
      Color emissionUnused{};
      evaluateCoefficients(tCur, span.scale, sigmaA, sigmaS, emissionUnused);
      // The residual factor per bin. The local clamp keeps the
      // residual at most `m`, so the factor is nonnegative; where the
      // extinction dips below the control the factor exceeds 1, which
      // the estimator identity covers for residuals of either sign.
      beta *= (m - ((sigmaA + sigmaS) - majorantColor * span.scaleMin)) / m;
      if (!(beta.maxComponent() > 0.0f)) {
        beta = Color();
        return;
      }
    }
  }
  // The analytic control transmittance, one exponential per segment.
  for (size_t i = 0; i < beta.size(); i++)
    beta[i] *= std::exp(-controlDepth[i]);
}
