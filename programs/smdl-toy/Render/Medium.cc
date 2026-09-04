#include "Render/Medium.h"

#include <algorithm>
#include <cstdint>
#include <mutex>
#include <set>

#include "smdl/RenderUtil/FastMath.h"
#include "smdl/Resource/VoxelGrid.h"
#include "smdl/Support/Logger.h"

// The Beer-Lambert transmittance of one band, exp(-d): exactly 1 for a
// negative depth, exactly 0 past 87, where the true value is under 1e-38,
// and 0 for a depth that is not a number. Inline because the per-band
// loops call it once per band and libm's expf neither inlines nor
// vectorizes, which left it at 7 percent of a scene rendered through a
// homogeneous medium.
[[nodiscard]] static inline float transmittance(float opticalDepth) noexcept {
  const float d{opticalDepth > 0.0f ? opticalDepth : 0.0f};
  return opticalDepth < 87.0f ? smdl::fastExp(-d) : 0.0f;
}

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

// Is the density acceleration hint of the given instance usable? The
// material must declare all three fields and they must be coherent.
[[nodiscard]] static bool
hasUsableDensityGrid(const smdl::JIT::MaterialInstance &mat) {
  const auto *densityGrid{mat.getVolumeDensityGrid()};
  const auto *boundMin{mat.getVolumeDensityBoundMin()};
  const auto *boundMax{mat.getVolumeDensityBoundMax()};
  return densityGrid && boundMin && boundMax && densityGrid->isValid() &&
         densityGrid->getMaxValue() > 0.0f && //
         boundMax->x > boundMin->x &&         //
         boundMax->y > boundMin->y &&         //
         boundMax->z > boundMin->z;
}

void Medium::reset(const MediumStack *stack, const Color &wavelengths,
                   float time, const float3 &org, const float3 &dir) noexcept {
  if (!mResolved || stack != mStack || time != mTime)
    resolve(stack, wavelengths, time);
  setSegment(org, dir);
}

void Medium::resolve(const MediumStack *stack, const Color &wavelengths,
                     float time) noexcept {
  mStack = stack;
  mTime = time;
  mResolved = true;
  mHasMedium = false;
  mHeterogeneous = false;
  mIsHaze = false;
  mMaterial = nullptr;
  mHasEmission = false;
  mMajorant = 0.0f;
  mMajorantBase = 0.0f;
  mMajorantGrid = 0.0f;
  mMeshInstance = nullptr;
  mState = std::nullopt;
  mDensityGrid = nullptr;
  mGridComponent = -1;
  mInvMaxValue = 0.0f;
  mUnitScale = 1.0f;
  mComponents.clear();
  mScatterInstance = nullptr;
  // The coefficient spectra are left holding whatever the last
  // resolution put there: every read of them is guarded by
  // `mHasMedium`/`mHeterogeneous`, under which the branches below
  // assign them.

  // The exterior haze stands in for the empty stack, that being where
  // the atmosphere is: a walk inside an object is inside whatever the
  // object encloses instead. Nothing else here applies to it, since it
  // is neither a material nor tracked against a majorant.
  if (!stack && mHaze) {
    mHasMedium = true;
    mIsHaze = true;
    return;
  }

  // Collect the active media: the run of additive entries from the top
  // of the stack plus the first non-additive entry, which replaces
  // everything below it. Entries that carry no coefficients and no
  // emission (e.g., clear glass interiors) contribute nothing but
  // still terminate the walk when non-additive.
  const MediumStack *primary{};
  size_t count{0};
  for (const MediumStack *entry{stack}; entry; entry = entry->prev) {
    const auto &mat{entry->mat};
    if (mat.hasMedium() || !mat.getVolumeEmissionIntensity().empty()) {
      if (!primary) primary = entry;
      ++count;
    }
    if (!mat.hasAdditiveVolume()) break;
  }
  if (count == 0) return;
  mHasMedium = true;
  const auto renderState{makeRenderState(wavelengths, nullptr, time)};
  // Coefficients are in inverse meters per the MDL specification;
  // distances here are in scene units. smdl-toy renders with the
  // default meters-per-scene-unit of 1, so this is the identity, but
  // the conversion is where a unit-aware scene flag would land.
  const float unitScale{renderState.meters_per_scene_unit};
  mUnitScale = unitScale;
  if (count == 1) {
    // The single-medium segment, which is the overwhelmingly common
    // case: everything lives in the flat members and the component
    // vector stays empty.
    const auto &mat{primary->mat};
    mScatterInstance = &mat;
    mMaterial = mat.material;
    mSigmaA = Color(mat.getAbsorptionCoefficient()) * unitScale;
    mSigmaS = Color(mat.getScatteringCoefficient()) * unitScale;
    mHasEmission = !mat.getVolumeEmissionIntensity().empty();
    mEmission = Color(mat.getVolumeEmissionIntensity()) * unitScale;
    if (mMaterial->hasHomogeneousVolume()) return;
    // Heterogeneous (or unproven, which must be treated the same): the
    // per-point queries need majorants to track against, covering every
    // coefficient the material actually has.
    const bool missingMajorantA{!mat.getAbsorptionCoefficient().empty() &&
                                mat.getMaxAbsorptionCoefficient().empty()};
    const bool missingMajorantS{!mat.getScatteringCoefficient().empty() &&
                                mat.getMaxScatteringCoefficient().empty()};
    if (missingMajorantA || missingMajorantS) {
      warnMissingMajorantOnce(mMaterial);
      return;
    }
    mHeterogeneous = true;
    mState = renderState;
    mMaxSigmaA = Color(mat.getMaxAbsorptionCoefficient()) * unitScale;
    mMaxSigmaS = Color(mat.getMaxScatteringCoefficient()) * unitScale;
    mMajorant = (mMaxSigmaA + mMaxSigmaS).maxComponent();
    mMajorantGrid = mMajorant;
    mGridMaxSigma = mMaxSigmaA + mMaxSigmaS;
    // The queries evaluate in the rigid frame of the instance whose
    // boundary entered the medium, paired with the rigid transform so
    // world reassembly inside the material is exact. The rigid transform
    // has no scale, so the direction stays unit length and distances
    // stay in scene units. A medium with no geometry queries in world
    // space directly.
    mMeshInstance = primary->meshInstance;
    if (mMeshInstance) {
      if (mMeshInstance->isDeformed) warnDeformedVolumeOnce(mMaterial);
      mState->object_to_world_matrix = mMeshInstance->rigidToWorld;
    }
    // The density acceleration hint, active only when the material
    // declares all three fields and they are usable. The hint box spans
    // texture space [0,1]^3, which spans the voxel extent, and bricks
    // are 16 voxels per axis, which is the map `setSegment()` takes the
    // segment into brick space with.
    if (hasUsableDensityGrid(mat)) {
      const auto *densityGrid{mat.getVolumeDensityGrid()};
      const auto *boundMin{mat.getVolumeDensityBoundMin()};
      const auto *boundMax{mat.getVolumeDensityBoundMax()};
      mDensityGrid = densityGrid;
      const auto extent{densityGrid->getExtent()};
      mBrickBoundMin = *boundMin;
      mBrickScale =
          float3(float(extent.x) / (16.0f * (boundMax->x - boundMin->x)),
                 float(extent.y) / (16.0f * (boundMax->y - boundMin->y)),
                 float(extent.z) / (16.0f * (boundMax->z - boundMin->z)));
      mInvMaxValue = 1.0f / densityGrid->getMaxValue();
    }
    return;
  }
  // Additive overlap: two or more media are active at once. Set up one
  // component per entry, mirroring the single-medium setup, and
  // aggregate the sums the sampling loops run against.
  mComponents.reserve(count);
  int gridComponent{-1};
  int gridCandidates{0};
  for (const MediumStack *entry{stack}; entry; entry = entry->prev) {
    const auto &mat{entry->mat};
    const bool contributes{mat.hasMedium() ||
                           !mat.getVolumeEmissionIntensity().empty()};
    if (contributes) {
      auto &component{mComponents.emplace_back()};
      component.mat = &mat;
      component.sigmaA = Color(mat.getAbsorptionCoefficient()) * unitScale;
      component.sigmaS = Color(mat.getScatteringCoefficient()) * unitScale;
      component.emission = Color(mat.getVolumeEmissionIntensity()) * unitScale;
      mHasEmission |= !mat.getVolumeEmissionIntensity().empty();
      if (!mat.material->hasHomogeneousVolume()) {
        const bool missingMajorantA{!mat.getAbsorptionCoefficient().empty() &&
                                    mat.getMaxAbsorptionCoefficient().empty()};
        const bool missingMajorantS{!mat.getScatteringCoefficient().empty() &&
                                    mat.getMaxScatteringCoefficient().empty()};
        if (missingMajorantA || missingMajorantS) {
          warnMissingMajorantOnce(mat.material);
        } else {
          component.heterogeneous = true;
          component.maxSigmaA =
              Color(mat.getMaxAbsorptionCoefficient()) * unitScale;
          component.maxSigmaS =
              Color(mat.getMaxScatteringCoefficient()) * unitScale;
          component.state = renderState;
          component.meshInstance = entry->meshInstance;
          if (component.meshInstance) {
            if (component.meshInstance->isDeformed)
              warnDeformedVolumeOnce(mat.material);
            component.state.object_to_world_matrix =
                component.meshInstance->rigidToWorld;
          }
          if (hasUsableDensityGrid(mat)) {
            ++gridCandidates;
            gridComponent = int(mComponents.size()) - 1;
          }
        }
      }
    }
    if (!mat.hasAdditiveVolume()) break;
  }
  // The aggregates. The homogeneous closed form runs on the summed
  // snapshots when every component is homogeneous; otherwise the
  // tracking loops run against the summed majorants, a homogeneous
  // component contributing its exact spectrum as its own bound.
  mSigmaA = Color();
  mSigmaS = Color();
  mEmission = Color();
  for (const auto &component : mComponents) {
    mSigmaA += component.sigmaA;
    mSigmaS += component.sigmaS;
    mEmission += component.emission;
    mHeterogeneous |= component.heterogeneous;
  }
  if (!mHeterogeneous) return;
  mMaxSigmaA = Color();
  mMaxSigmaS = Color();
  for (const auto &component : mComponents) {
    mMaxSigmaA +=
        component.heterogeneous ? component.maxSigmaA : component.sigmaA;
    mMaxSigmaS +=
        component.heterogeneous ? component.maxSigmaS : component.sigmaS;
  }
  mMajorant = (mMaxSigmaA + mMaxSigmaS).maxComponent();
  // The density-hint spans can drive the walk only when exactly one
  // component has a usable grid: its contribution scales per span, and
  // everything else is the constant base. With competing grids (or
  // none) the whole majorant is the constant global span.
  if (gridCandidates == 1) {
    auto &component{mComponents[size_t(gridComponent)]};
    component.scaledByGrid = true;
    mGridComponent = gridComponent;
    mGridMaxSigma = component.maxSigmaA + component.maxSigmaS;
    mMajorantGrid = mGridMaxSigma.maxComponent();
    mMajorantBase = std::max(
        (mMaxSigmaA + mMaxSigmaS - mGridMaxSigma).maxComponent(), 0.0f);
    const auto &mat{*component.mat};
    const auto *densityGrid{mat.getVolumeDensityGrid()};
    const auto *boundMin{mat.getVolumeDensityBoundMin()};
    const auto *boundMax{mat.getVolumeDensityBoundMax()};
    mDensityGrid = densityGrid;
    const auto extent{densityGrid->getExtent()};
    mBrickBoundMin = *boundMin;
    mBrickScale =
        float3(float(extent.x) / (16.0f * (boundMax->x - boundMin->x)),
               float(extent.y) / (16.0f * (boundMax->y - boundMin->y)),
               float(extent.z) / (16.0f * (boundMax->z - boundMin->z)));
    mInvMaxValue = 1.0f / densityGrid->getMaxValue();
  } else {
    mGridMaxSigma = Color();
    mMajorantGrid = mMajorant;
    mMajorantBase = 0.0f;
  }
}

void Medium::setSegment(const float3 &org, const float3 &dir) noexcept {
  // The haze varies with world height alone, so the segment reduces to
  // the extinction where it starts and the rate the height changes at.
  if (mIsHaze) {
    mHaze->extinctionAt(
        org.z, smdl::Span<float>(mHazeSigmaC.data(), mHazeSigmaC.size()));
    mHazeK = mHaze->shapeExponent(dir.z);
    return;
  }
  // A homogeneous medium has the same coefficients everywhere, so it
  // never queries and has no segment to place.
  if (!mHeterogeneous) return;
  if (mComponents.empty()) {
    if (mMeshInstance) {
      mOrgR = float3(mMeshInstance->worldToRigid * float4(org, 1.0f));
      mDirR = float3(mMeshInstance->worldToRigid * float4(dir, 0.0f));
    } else {
      mOrgR = org;
      mDirR = dir;
    }
    mState->direction = mDirR;
  } else {
    for (auto &component : mComponents) {
      if (!component.heterogeneous) continue;
      if (component.meshInstance) {
        component.orgR =
            float3(component.meshInstance->worldToRigid * float4(org, 1.0f));
        component.dirR =
            float3(component.meshInstance->worldToRigid * float4(dir, 0.0f));
      } else {
        component.orgR = org;
        component.dirR = dir;
      }
      component.state.direction = component.dirR;
    }
  }
  if (mDensityGrid) {
    // The grid's own component drives the spans, which is the single
    // medium itself where there is no overlap.
    const float3 &orgR{
        mGridComponent < 0 ? mOrgR : mComponents[size_t(mGridComponent)].orgR};
    const float3 &dirR{
        mGridComponent < 0 ? mDirR : mComponents[size_t(mGridComponent)].dirR};
    for (int axis = 0; axis < 3; axis++) {
      mBrickOrg[axis] = (orgR[axis] - mBrickBoundMin[axis]) * mBrickScale[axis];
      mBrickDir[axis] = dirR[axis] * mBrickScale[axis];
    }
  }
}

void Medium::evaluateCoefficients(float t, float majorantScale, Color &sigmaA,
                                  Color &sigmaS, Color &emission) const {
  if (mComponents.empty()) {
    mState->position = mOrgR + t * mDirR;
    mMaterial->volumeEvaluate(*mState, sigmaA.data(), sigmaS.data(),
                              emission.data());
    // Convert to inverse scene units and clamp to the declared majorants
    // at the local scale, so a lying majorant or density hint renders a
    // clamped medium instead of accumulating negative-weight bias. The
    // emission coefficient has no majorant and only clamps nonnegative:
    // it never gates sampling, so no bound is needed for unbiasedness.
    // Not `std::clamp`: the majorant is what the material declared, and a
    // misdeclared negative one must not invert the bounds.
    for (size_t i = 0; i < sigmaA.size(); i++) {
      sigmaA[i] = std::min(std::max(sigmaA[i] * mUnitScale, 0.0f),
                           mMaxSigmaA[i] * majorantScale);
      sigmaS[i] = std::min(std::max(sigmaS[i] * mUnitScale, 0.0f),
                           mMaxSigmaS[i] * majorantScale);
      emission[i] = std::max(emission[i] * mUnitScale, 0.0f);
    }
    return;
  }
  // Additive overlap: sum the per-component queries, each clamped to
  // its own majorants, with the density-hint scale applying only to
  // the grid component. Homogeneous components contribute their exact
  // snapshots. The clamped scattering coefficient is stashed per
  // heterogeneous component for the phase pick at a real collision.
  sigmaA = Color();
  sigmaS = Color();
  emission = Color();
  // One component's query, reused down the components: `volumeEvaluate`
  // overwrites every band of all three.
  Color a{}, s{}, e{};
  for (const auto &component : mComponents) {
    if (!component.heterogeneous) {
      sigmaA += component.sigmaA;
      sigmaS += component.sigmaS;
      emission += component.emission;
      continue;
    }
    component.state.position = component.orgR + t * component.dirR;
    component.mat->material->volumeEvaluate(component.state, a.data(), s.data(),
                                            e.data());
    const float scale{component.scaledByGrid ? majorantScale : 1.0f};
    for (size_t i = 0; i < a.size(); i++) {
      a[i] = std::min(std::max(a[i] * mUnitScale, 0.0f),
                      component.maxSigmaA[i] * scale);
      s[i] = std::min(std::max(s[i] * mUnitScale, 0.0f),
                      component.maxSigmaS[i] * scale);
      e[i] = std::max(e[i] * mUnitScale, 0.0f);
    }
    sigmaA += a;
    sigmaS += s;
    emission += e;
    component.lastSigmaS = s;
  }
}

void Medium::pickScatterComponent(float xi, const Color &sigmaS,
                                  Color &beta) const {
  // Selection probability proportional to the component's share of the
  // scattering coefficient at the collision, averaged over bins. Zero
  // total scattering (a pure-absorption collision) leaves `beta` all
  // zero already; default to the first component so the caller always
  // has a phase function.
  mScatterInstance = mComponents.front().mat;
  float totalAverage{};
  for (const auto &component : mComponents)
    totalAverage += componentSigmaS(component).average();
  if (!(totalAverage > 0.0f)) return;
  const Component *picked{};
  float pickedProbability{};
  float cdf{};
  for (const auto &component : mComponents) {
    const float average{componentSigmaS(component).average()};
    if (!(average > 0.0f)) continue;
    picked = &component;
    pickedProbability = average / totalAverage;
    cdf += pickedProbability;
    if (xi < cdf) break;
  }
  mScatterInstance = picked->mat;
  // The per-bin spectral share over the scalar pick probability, so
  // the expectation per bin is the sigma_s-weighted phase mixture. A
  // bin with zero total scattering has zero throughput already; keep
  // it zero rather than forming 0/0.
  const auto &pickedSigmaS{componentSigmaS(*picked)};
  for (size_t i = 0; i < beta.size(); i++)
    beta[i] *= sigmaS[i] > 0.0f
                   ? pickedSigmaS[i] / sigmaS[i] / pickedProbability
                   : 0.0f;
}

bool Medium::sampleDistance(Sampler &sampler, float tEnd, float &t, Color &beta,
                            Color &emitted) const {
  if (!mHasMedium) return false;
  if (mIsHaze) {
    // The analytic exponential-height medium. The optical depth is the
    // extinction at the segment origin times one distance shape shared
    // by every band, so the free-flight distance inverts in closed form
    // against the hero band and the other bands' transmittance follows
    // from the same shape. Nothing is tracked, nothing is clamped, and
    // the spectral weighting is the homogeneous estimator's with
    // `shape(t)` in place of `t`. The haze does not emit.
    const float xi{float(sampler)};
    const int hero{sampler.index(int(mHazeSigmaC.size()))};
    // Both the collision and the segment end are placed by their shape
    // rather than their distance. The shape is monotone, so the two
    // orders agree, and settling it here means only a collision pays
    // the inversion, while the depth it carries is the shape that was
    // sampled instead of a round trip back out through the logarithm.
    // The end is clamped because an unbounded segment that never turns
    // upward has infinite depth, and infinity times a band whose
    // extinction has underflowed to zero is not a number.
    const float sScatter{-std::log1p(-xi) / mHazeSigmaC[size_t(hero)]};
    const float sEnd{std::min(smdl::Haze::shape(mHazeK, tEnd),
                              std::numeric_limits<float>::max())};
    const bool scattered{sScatter < sEnd};
    const float depth{scattered ? sScatter : sEnd};
    Color Tr{};
    for (size_t i = 0; i < Tr.size(); i++)
      Tr[i] = transmittance(mHazeSigmaC[i] * depth);
    if (scattered) {
      // The extinction at the collision is the origin spectrum times a
      // factor common to every band, which cancels between the
      // scattering weight and the balance heuristic that normalizes it.
      beta *= mHazeSigmaC * mHazeAlbedo * Tr / (mHazeSigmaC * Tr).average();
      t = smdl::Haze::shapeInverse(mHazeK, sScatter);
      return true;
    }
    beta *= Tr / Tr.average();
    return false;
  }
  if (!mHeterogeneous) {
    // The emitted radiance along the segment is deterministic for a
    // homogeneous medium: the integral of transmittance times the
    // emission coefficient to the segment end, per bin, regardless of
    // where scattering is sampled below. The extinction-free limit is
    // linear in distance, so an unbounded segment through an emissive
    // vacuum is clamped rather than infinite.
    //
    // The one place that keeps `std::exp`: at small optical depth the
    // difference against one cancels down to the error of whatever
    // computed it, and the two ulp `transmittance()` carries are enough
    // to turn this integral negative there.
    const Color mu{mSigmaA + mSigmaS};
    if (mHasEmission) {
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
    const float xi{float(sampler)};
    const int hero{sampler.index(int(mu.size()))};
    const float tScatter{-std::log1p(-xi) / mu[hero]};
    Color Tr{};
    const float tTravel{
        std::min({tScatter, tEnd, std::numeric_limits<float>::max()})};
    for (size_t i = 0; i < mu.size(); i++)
      Tr[i] = transmittance(mu[i] * tTravel);
    if (tScatter < tEnd) {
      beta *= mSigmaS * Tr / (mu * Tr).average();
      if (!mComponents.empty())
        pickScatterComponent(float(sampler), mSigmaS, beta);
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
  // The coefficients of one tentative collision, held across the loop
  // rather than sized anew at each: `evaluateCoefficients()` overwrites
  // every band of all three.
  Color sigmaA{}, sigmaS{}, emission{};
  auto spans{MajorantSpanIterator(mDensityGrid, mBrickOrg, mBrickDir,
                                  mInvMaxValue, tEnd)};
  MajorantSpan span{};
  while (spans.next(span)) {
    // The local tracking majorant of this span: the constant base plus
    // the grid part at the span's scale (a single medium is all grid
    // part, so this is plain scaling). A zero span is an empty brick
    // with no base: skipped outright, at no cost of any kind. The
    // exponential restart at each span boundary is unbiased by
    // memorylessness.
    const float m{mMajorantBase + mMajorantGrid * span.scale};
    if (!(m > 0.0f)) continue;
    float tCur{span.t0};
    while (true) {
      tCur += -std::log1p(-rng.generateFloat()) / m;
      if (!(tCur < span.t1)) break;
      if (++iter > MAX_TENTATIVE_COLLISIONS) {
        beta = Color();
        return false;
      }
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
        if (!mComponents.empty())
          pickScatterComponent(rng.generateFloat(), sigmaS, beta);
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

void Medium::attenuate(Sampler &sampler, float tEnd, Color &beta,
                       bool unbounded) const {
  if (!mHasMedium) return;
  if (mIsHaze) {
    // Closed-form Beer-Lambert against the analytic optical depth: a
    // shadow ray through the haze is exact and draws nothing, which is
    // the whole reason not to track it.
    const float depth{
        std::min(smdl::Haze::shape(mHazeK, unbounded ? INF : tEnd),
                 std::numeric_limits<float>::max())};
    for (size_t i = 0; i < beta.size(); i++)
      beta[i] *= transmittance(mHazeSigmaC[i] * depth);
    return;
  }
  if (!mHeterogeneous) {
    // Closed-form Beer-Lambert. The caller clamps 'tEnd' finite, so
    // wavelengths with zero extinction keep transmittance 1 instead of
    // producing 0 times infinity.
    const Color mu{mSigmaA + mSigmaS};
    for (size_t i = 0; i < mu.size(); i++)
      beta[i] *= transmittance(mu[i] * tEnd);
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
  // See `sampleDistance()`; the emission this one asks for goes unread.
  Color sigmaA{}, sigmaS{}, emissionUnused{};
  auto spans{MajorantSpanIterator(mDensityGrid, mBrickOrg, mBrickDir,
                                  mInvMaxValue, tEnd)};
  MajorantSpan span{};
  Color controlDepth{};
  // The control that `span.scaleMin` lower bounds is the grid
  // component's majorant alone; with overlap the base contribution of
  // the other components stays in the tracked rate at full strength.
  const Color majorantColor{mGridMaxSigma};
  while (spans.next(span)) {
    if (span.scaleMin > 0.0f)
      controlDepth += majorantColor * (span.scaleMin * (span.t1 - span.t0));
    const float m{mMajorantGrid * (span.scale - span.scaleMin) + mMajorantBase};
    if (!(m > 0.0f)) continue;
    float tCur{span.t0};
    while (true) {
      tCur += -std::log1p(-rng.generateFloat()) / m;
      if (!(tCur < span.t1)) break;
      if (++iter > MAX_TENTATIVE_COLLISIONS) {
        beta = Color();
        return;
      }
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
    beta[i] *= transmittance(controlDepth[i]);
}
