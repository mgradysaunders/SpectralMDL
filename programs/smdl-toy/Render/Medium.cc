#include "Render/Medium.h"

#include <algorithm>
#include <mutex>
#include <unordered_set>

#include "smdl/RenderUtil/FastMath.h"
#include "smdl/RenderUtil/MajorantWalk.h"
#include "smdl/Support/Logger.h"

namespace {

//--{ Band kernels
// The Beer-Lambert transmittance of one band, exp(-d): exactly 1 for a
// negative depth, exactly 0 past 87, and 0 for a depth that is not a
// number. Inline because the per-band loops call it once per band and
// libm's expf neither inlines nor vectorizes.
//
// The cutoff at 87 is a termination threshold, not a domain guard, and
// is deliberately below `fastExp`'s own floor of -87.33: over the sliver
// between them the true value is a normal float around 1.5e-38, which
// flush-to-zero does NOT flush, so dropping the cutoff would leave every
// fully absorbed segment carrying 1e-38 of throughput past the
// `maxComponent() > 0` checks that are supposed to retire it.
[[nodiscard]]
SMDL_ALWAYS_INLINE float transmittance(float opticalDepth) noexcept {
  return opticalDepth < 87.0f ? smdl::fastExp(-std::max(opticalDepth, 0.0f))
                              : 0.0f;
}

// The per-band kernels of the estimators below.
//
// A color at or under `INLINE_CAPACITY` bands is inline, and an inline
// buffer always holds `INLINE_CAPACITY` initialized floats, so a kernel
// runs the fixed lane count whenever its output is inline: a fixed trip
// count compiles to whole vectors, and the lanes past the size hold junk
// the container's own operators tolerate the same way. Every kernel
// updates its output lane by lane in place, which is vectorizable
// however the operands alias, and the loop pragmas say so rather than
// leave the vectorizer to prove it: the band pointers are storage
// selects, and the full unroller would sink that select into every lane
// and hand the vectorizer a gather. The reductions take the tree form
// only at the full inline width, where every lane is live.

constexpr size_t LANES = smdl::SpectralColor::INLINE_CAPACITY;
static_assert(LANES == 16, "the reductions below split the lanes 16-8-4");

// body(i) over the bands of a color of `n` bands.
template <typename Body>
SMDL_ALWAYS_INLINE void forBands(size_t n, Body body) noexcept {
  if (SMDL_LIKELY(n <= LANES)) {
#pragma clang loop vectorize(assume_safety) unroll(disable)
    for (size_t i = 0; i < LANES; i++) body(i);
  } else {
    // The heap path, scalar: a render past the inline width is rare and
    // the vector form would double every kernel's code for it.
#pragma clang loop vectorize(disable) unroll(disable)
    for (size_t i = 0; i < n; i++) body(i);
  }
}

// The sum of lane(i) over `n` bands: a pairwise tree at the inline width.
template <typename Lane>
[[nodiscard]] SMDL_ALWAYS_INLINE float sumBands(size_t n, Lane lane) noexcept {
  if (SMDL_LIKELY(n == LANES)) {
    float s8[8];
#pragma clang loop vectorize(assume_safety) unroll(disable)
    for (size_t i = 0; i < 8; i++) s8[i] = lane(i) + lane(i + 8);
    float s4[4];
    for (size_t i = 0; i < 4; i++) s4[i] = s8[i] + s8[i + 4];
    return (s4[0] + s4[2]) + (s4[1] + s4[3]);
  }
  float sum{};
#pragma clang loop vectorize(disable) unroll(disable)
  for (size_t i = 0; i < n; i++) sum += lane(i);
  return sum;
}

// The maximum of lane(i) over `n` bands, likewise.
template <typename Lane>
[[nodiscard]] SMDL_ALWAYS_INLINE float maxBands(size_t n, Lane lane) noexcept {
  if (SMDL_LIKELY(n == LANES)) {
    float s8[8];
#pragma clang loop vectorize(assume_safety) unroll(disable)
    for (size_t i = 0; i < 8; i++) s8[i] = std::max(lane(i), lane(i + 8));
    float s4[4];
    for (size_t i = 0; i < 4; i++) s4[i] = std::max(s8[i], s8[i + 4]);
    return std::max(std::max(s4[0], s4[2]), std::max(s4[1], s4[3]));
  }
  float result{lane(0)};
#pragma clang loop vectorize(disable) unroll(disable)
  for (size_t i = 1; i < n; i++) result = std::max(result, lane(i));
  return result;
}

// The average of the per-band products of `a` and `b`: the balance
// heuristic normalizers below.
[[nodiscard]] SMDL_ALWAYS_INLINE float
averageOfProducts(const smdl::SpectralColor &a,
                  const smdl::SpectralColor &b) noexcept {
  const size_t n{a.size()};
  const float *pA{a.data()};
  const float *pB{b.data()};
  return sumBands(n, [=](size_t i) { return pA[i] * pB[i]; }) / float(n);
}

// The average of the per-band products of the extinction `a + s` with `b`.
[[nodiscard]] SMDL_ALWAYS_INLINE float
averageOfExtinctionProducts(const smdl::SpectralColor &a,
                            const smdl::SpectralColor &s,
                            const smdl::SpectralColor &b) noexcept {
  const size_t n{a.size()};
  const float *pA{a.data()};
  const float *pS{s.data()};
  const float *pB{b.data()};
  return sumBands(n, [=](size_t i) { return (pA[i] + pS[i]) * pB[i]; }) /
         float(n);
}

// Tr = exp(-sigma * depth), the per-band transmittance of one segment.
SMDL_ALWAYS_INLINE void fillTransmittance(smdl::SpectralColor &Tr,
                                          const smdl::SpectralColor &sigma,
                                          float depth) noexcept {
  float *pTr{Tr.data()};
  const float *pSigma{sigma.data()};
  forBands(Tr.size(),
           [=](size_t i) { pTr[i] = transmittance(pSigma[i] * depth); });
}

// beta *= exp(-sigma * depth), the closed-form Beer-Lambert attenuation.
SMDL_ALWAYS_INLINE void attenuateBy(smdl::SpectralColor &beta,
                                    const smdl::SpectralColor &sigma,
                                    float depth) noexcept {
  float *pB{beta.data()};
  const float *pSigma{sigma.data()};
  forBands(beta.size(),
           [=](size_t i) { pB[i] *= transmittance(pSigma[i] * depth); });
}

// beta *= weight * invNorm, the normalized weight of a segment that
// survived to its end.
SMDL_ALWAYS_INLINE void applySurvivalWeight(smdl::SpectralColor &beta,
                                            const smdl::SpectralColor &weight,
                                            float invNorm) noexcept {
  float *pB{beta.data()};
  const float *pW{weight.data()};
  forBands(beta.size(), [=](size_t i) { pB[i] *= pW[i] * invNorm; });
}

// beta *= sigmaS * weight * invNorm, the normalized weight of a real
// collision, `weight` being the transmittance of the closed forms or the
// null-collision product of the tracking chain.
SMDL_ALWAYS_INLINE void applyScatterWeight(smdl::SpectralColor &beta,
                                           const smdl::SpectralColor &sigmaS,
                                           const smdl::SpectralColor &weight,
                                           float invNorm) noexcept {
  float *pB{beta.data()};
  const float *pS{sigmaS.data()};
  const float *pW{weight.data()};
  forBands(beta.size(), [=](size_t i) { pB[i] *= pS[i] * pW[i] * invNorm; });
}

// beta *= sigma * albedo * Tr * invNorm, the haze's real-collision weight,
// whose scattering coefficient is the extinction times the albedo.
SMDL_ALWAYS_INLINE void
applyHazeScatterWeight(smdl::SpectralColor &beta,
                       const smdl::SpectralColor &sigma,
                       const smdl::SpectralColor &albedo,
                       const smdl::SpectralColor &Tr, float invNorm) noexcept {
  float *pB{beta.data()};
  const float *pSigma{sigma.data()};
  const float *pAlbedo{albedo.data()};
  const float *pTr{Tr.data()};
  forBands(beta.size(), [=](size_t i) {
    pB[i] *= pSigma[i] * pAlbedo[i] * pTr[i] * invNorm;
  });
}

// emitted += emission times the integral of transmittance over
// `[0, tEmit]`, whose extinction-free limit is linear in the distance.
// `std::exp` rather than `fastExp`: at small optical depth the difference
// against one cancels down to the error of whatever computed it.
SMDL_ALWAYS_INLINE void accumulateHomogeneousEmission(
    smdl::SpectralColor &emitted, const smdl::SpectralColor &emission,
    const smdl::SpectralColor &mu, float tEmit) noexcept {
  float *pEmitted{emitted.data()};
  const float *pE{emission.data()};
  const float *pMu{mu.data()};
  forBands(emitted.size(), [=](size_t i) {
    pEmitted[i] +=
        pE[i] *
        (pMu[i] > 1e-12f ? (1.0f - std::exp(-pMu[i] * tEmit)) / pMu[i] : tEmit);
  });
}

// emitted += emission * P * invPdf at one tentative collision of the chain.
SMDL_ALWAYS_INLINE void
accumulateTrackedEmission(smdl::SpectralColor &emitted,
                          const smdl::SpectralColor &emission,
                          const smdl::SpectralColor &P, float invPdf) noexcept {
  float *pEmitted{emitted.data()};
  const float *pE{emission.data()};
  const float *pP{P.data()};
  forBands(emitted.size(),
           [=](size_t i) { pEmitted[i] += pE[i] * pP[i] * invPdf; });
}

// P *= m - (a + s), the per-band factor of a null collision against the
// local majorant, which the local clamp keeps non-negative; returns the
// maximum over the bands, the chain's renormalizer.
[[nodiscard]] SMDL_ALWAYS_INLINE float
applyNullWeight(smdl::SpectralColor &P, const smdl::SpectralColor &a,
                const smdl::SpectralColor &s, float m) noexcept {
  float *pP{P.data()};
  const float *pA{a.data()};
  const float *pS{s.data()};
  forBands(P.size(), [=](size_t i) { pP[i] *= m - (pA[i] + pS[i]); });
  return maxBands(P.size(), [=](size_t i) { return pP[i]; });
}

// controlDepth += majorant * depth, the analytic control of one span.
SMDL_ALWAYS_INLINE void
accumulateControlDepth(smdl::SpectralColor &controlDepth,
                       const smdl::SpectralColor &majorant,
                       float depth) noexcept {
  float *pC{controlDepth.data()};
  const float *pM{majorant.data()};
  forBands(controlDepth.size(), [=](size_t i) { pC[i] += pM[i] * depth; });
}

// beta *= (m - residual) / m at one tentative collision of residual ratio
// tracking, the residual being the extinction less the span's control.
// The local clamp keeps the residual at most `m`, so the factor is
// nonnegative; where the extinction dips below the control the factor
// exceeds 1, which the estimator identity covers for residuals of either
// sign.
SMDL_ALWAYS_INLINE void applyResidualWeight(smdl::SpectralColor &beta,
                                            const smdl::SpectralColor &a,
                                            const smdl::SpectralColor &s,
                                            const smdl::SpectralColor &majorant,
                                            float scaleMin, float m,
                                            float invM) noexcept {
  float *pB{beta.data()};
  const float *pA{a.data()};
  const float *pS{s.data()};
  const float *pM{majorant.data()};
  forBands(beta.size(), [=](size_t i) {
    const float residual{(pA[i] + pS[i]) - pM[i] * scaleMin};
    pB[i] *= (m - residual) * invM;
  });
}

// beta *= the picked component's per-band share of the scattering
// coefficient over the scalar probability it was picked with. A band with
// no scattering carries no throughput already; keep it zero rather than
// forming 0/0.
SMDL_ALWAYS_INLINE void
applySpectralShare(smdl::SpectralColor &beta, const smdl::SpectralColor &sigmaS,
                   const smdl::SpectralColor &pickedSigmaS,
                   float invProbability) noexcept {
  float *pB{beta.data()};
  const float *pS{sigmaS.data()};
  const float *pPicked{pickedSigmaS.data()};
  forBands(beta.size(), [=](size_t i) {
    pB[i] = pS[i] > 0.0f ? pB[i] * (pPicked[i] / pS[i] * invProbability) : 0.0f;
  });
}

// Clamp one component's query to the declared majorants at the local
// scale, and the emission nonnegative. The majorants are nonnegative
// (the resolution clamps them), so the ranges cannot invert.
SMDL_ALWAYS_INLINE void clampCoefficients(smdl::SpectralColor &sigmaA,
                                          smdl::SpectralColor &sigmaS,
                                          smdl::SpectralColor &emission,
                                          const smdl::SpectralColor &maxSigmaA,
                                          const smdl::SpectralColor &maxSigmaS,
                                          float scale) noexcept {
  float *pA{sigmaA.data()};
  float *pS{sigmaS.data()};
  float *pE{emission.data()};
  const float *pMaxA{maxSigmaA.data()};
  const float *pMaxS{maxSigmaS.data()};
  forBands(sigmaA.size(), [=](size_t i) {
    pA[i] = std::clamp(pA[i], 0.0f, pMaxA[i] * scale);
    pS[i] = std::clamp(pS[i], 0.0f, pMaxS[i] * scale);
    pE[i] = std::max(pE[i], 0.0f);
  });
}
//--}

//--{ Resolution helpers
// The cap on tentative collisions per segment, a guard against
// marching forever through unbounded or leaky geometry with a positive
// majorant. A segment that exhausts it is treated as fully absorbed;
// reaching the cap honestly would mean an optical depth in the tens of
// thousands.
constexpr int MAX_TENTATIVE_COLLISIONS{65536};

// Warn once per material about a heterogeneous volume that fails to
// declare a majorant for a coefficient it uses, and therefore falls
// back to the homogeneous treatment.
void warnMissingMajorantOnce(const smdl::JIT::MaterialDef *materialDef) {
  static std::mutex mutex{};
  static std::unordered_set<const smdl::JIT::MaterialDef *> warned{};
  const std::scoped_lock lock{mutex};
  if (warned.insert(materialDef).second)
    SMDL_LOG_WARN(
        "material ", smdl::Quoted(materialDef->materialName),
        " has a isHeterogeneous volume but no majorant for every "
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
void warnDeformedVolumeOnce(const smdl::JIT::MaterialDef *materialDef) {
  static std::mutex mutex{};
  static std::unordered_set<const smdl::JIT::MaterialDef *> warned{};
  const std::scoped_lock lock{mutex};
  if (warned.insert(materialDef).second)
    SMDL_LOG_WARN("material ", smdl::Quoted(materialDef->materialName),
                  " has a spatially varying volume inside an instance that "
                  "shears or scales non-uniformly; the surface is deformed "
                  "but the volume it encloses is not");
}

// Is the density acceleration hint of the given instance usable? The
// material must declare all three fields and they must be coherent.
[[nodiscard]] bool hasUsableDensityGrid(const smdl::JIT::Material &material) {
  const smdl::VoxelGrid *densityGrid{material.getVolumeDensityGrid()};
  const float3 *boundMin{material.getVolumeDensityBoundMin()};
  const float3 *boundMax{material.getVolumeDensityBoundMax()};
  return densityGrid && boundMin && boundMax && densityGrid->isValid() &&
         densityGrid->getMaxValue() > 0.0f && //
         boundMax->x > boundMin->x &&         //
         boundMax->y > boundMin->y &&         //
         boundMax->z > boundMin->z;
}
//--}

} // namespace

//--{ Resolution
void Medium::setHaze(const smdl::Haze *haze) noexcept {
  if (haze == mHaze.haze) return;
  mHaze.haze = haze;
  if (haze)
    haze->albedo(smdl::Span<float>(mHaze.albedo.data(), mHaze.albedo.size()));
  mKey.isResolved = false;
}

void Medium::reset(const MediumStack *stack, const Color &wavelengths,
                   PathTime time, const float3 &org,
                   const float3 &dir) noexcept {
  if (!mKey.isKnown || stack != mKey.stack || time.seconds != mKey.time)
    resolve(stack, wavelengths, time);
  setSegment(org, dir, time.fraction);
}

void Medium::resolve(const MediumStack *stack, const Color &wavelengths,
                     PathTime time) noexcept {
  mKey.stack = stack;
  mKey.isKnown = true;
  if (mKey.isResolved && rebind(stack, time)) return;
  rebuild(stack, wavelengths, time);
}

bool Medium::rebind(const MediumStack *stack, PathTime time) noexcept {
  // The haze stands in for the empty stack and has no components; see
  // `rebuild()`.
  if (mIsHaze != (!stack && mHaze.haze != nullptr)) return false;
  if (!mIsHaze) {
    size_t count{0};
    for (const MediumStack *entry{stack}; entry; entry = entry->prev) {
      const smdl::JIT::Material &material{*entry->material};
      if (material.hasMedium() ||
          !material.getVolumeEmissionIntensity().empty()) {
        if (count == mComponents.size() || !matches(mComponents[count], *entry))
          return false;
        mComponents[count++].material = &material;
      }
      if (!material.hasAdditiveVolume()) break;
    }
    if (count != mComponents.size()) return false;
    if (count > 0) mScatterInstance = mComponents.front().material;
  }
  if (time.seconds != mKey.time) {
    mKey.time = time.seconds;
    if (mIsMoving) {
      std::optional<InstanceFrame> scratch{};
      for (auto &comp : mComponents)
        if (comp.isHeterogeneous && comp.meshInstance)
          comp.state->objectToWorld =
              comp.meshInstance->frameAt(time.fraction, scratch).rigidToWorld;
    }
  }
  return true;
}

bool Medium::matches(const Component &comp,
                     const MediumStack &entry) const noexcept {
  // The cheap fields first, the spectra last: a mismatch is usually a
  // different material, and the compare runs once per path.
  const smdl::JIT::Material &material{*entry.material};
  if (comp.materialDef != material.def) return false;
  if (comp.presence != presenceOf(material)) return false;
  if (!comp.isHeterogeneous)
    return valuesMatch(material.getAbsorptionCoefficient(), comp.sigmaA) &&
           valuesMatch(material.getScatteringCoefficient(), comp.sigmaS) &&
           valuesMatch(material.getVolumeEmissionIntensity(), comp.emission);
  if (comp.meshInstance != entry.meshInstance) return false;
  const smdl::VoxelGrid *grid{hasUsableDensityGrid(material)
                                  ? material.getVolumeDensityGrid()
                                  : nullptr};
  if (comp.grid != grid) return false;
  if (grid &&
      (!smdl::isAllTrue(comp.boundMin ==
                        *material.getVolumeDensityBoundMin()) ||
       !smdl::isAllTrue(comp.boundMax == *material.getVolumeDensityBoundMax())))
    return false;
  return valuesMatch(material.getMaxAbsorptionCoefficient(), comp.maxSigmaA) &&
         valuesMatch(material.getMaxScatteringCoefficient(), comp.maxSigmaS);
}

void Medium::rebuild(const MediumStack *stack, const Color &wavelengths,
                     PathTime time) noexcept {
  mKey.time = time.seconds;
  mKey.isResolved = true;
  mHasMedium = false;
  mIsHeterogeneous = false;
  mIsMoving = false;
  mIsHaze = false;
  mHasEmission = false;
  mHasOverlap = false;
  mMajorant = 0.0f;
  mMajorantBase = 0.0f;
  mMajorantGrid = 0.0f;
  mHint = Hint{};
  mComponents.clear();
  mScatterInstance = nullptr;
  // The coefficient spectra are left holding whatever the last
  // resolution put there: every read of them is behind `mHasMedium` or
  // `mIsHeterogeneous`, under which the branches below assign them.

  // The exterior haze stands in for the empty stack, that being where
  // the atmosphere is: a walk inside an object is inside whatever the
  // object encloses instead. Nothing else here applies to it, since it
  // is neither a material nor tracked against a majorant.
  if (!stack && mHaze.haze) {
    mHasMedium = true;
    mIsHaze = true;
    return;
  }

  const smdl::State renderState{
      makeRenderState(wavelengths, nullptr, time.seconds)};
  // Coefficients are in inverse meters per the MDL specification, and
  // the toy's scene unit is the meter, so they are in inverse scene
  // units as they come.
  SMDL_SANITY_CHECK(renderState.metersPerSceneUnit == 1.0f);
  // Collect the active media: the run of additive entries from the top
  // of the stack plus the first non-additive entry, which replaces
  // everything below it. Entries that carry no coefficients and no
  // emission (e.g., clear glass interiors) contribute nothing but
  // still terminate the walk when non-additive.
  int hintCandidates{0};
  for (const MediumStack *entry{stack}; entry; entry = entry->prev) {
    const smdl::JIT::Material &material{*entry->material};
    if (material.hasMedium() ||
        !material.getVolumeEmissionIntensity().empty()) {
      Component &comp{mComponents.emplace_back()};
      comp.material = &material;
      comp.materialDef = material.def;
      comp.presence = presenceOf(material);
      comp.sigmaA = Color(material.getAbsorptionCoefficient());
      comp.sigmaS = Color(material.getScatteringCoefficient());
      comp.emission = Color(material.getVolumeEmissionIntensity());
      mHasEmission |= !material.getVolumeEmissionIntensity().empty();
      // Heterogeneous (or unproven, which must be treated the same): the
      // per-point queries need majorants to track against, covering every
      // coefficient the material actually has.
      if (!material.def->hasHomogeneousVolume()) {
        if (!hasUsableMajorants(material)) {
          warnMissingMajorantOnce(material.def);
        } else {
          comp.isHeterogeneous = true;
          // Clamped nonnegative, so that a misdeclared negative majorant
          // bounds the coefficient at zero instead of inverting the
          // per-collision clamp.
          comp.maxSigmaA = Color(material.getMaxAbsorptionCoefficient());
          comp.maxSigmaA.setNonPositiveToZero();
          comp.maxSigmaS = Color(material.getMaxScatteringCoefficient());
          comp.maxSigmaS.setNonPositiveToZero();
          comp.state = renderState;
          // The queries evaluate in the rigid frame of the instance whose
          // boundary entered the medium, paired with the rigid transform
          // so world reassembly inside the material is exact. The rigid
          // transform has no scale, so the direction stays unit length
          // and distances stay in scene units. A medium with no geometry
          // queries in world space directly.
          comp.meshInstance = entry->meshInstance;
          if (comp.meshInstance) {
            mIsMoving |= comp.meshInstance->isMoving;
            if (comp.meshInstance->frame.isDeformed)
              warnDeformedVolumeOnce(material.def);
            std::optional<InstanceFrame> scratch{};
            comp.state->objectToWorld =
                comp.meshInstance->frameAt(time.fraction, scratch).rigidToWorld;
          }
          if (hasUsableDensityGrid(material)) {
            comp.grid = material.getVolumeDensityGrid();
            comp.boundMin = *material.getVolumeDensityBoundMin();
            comp.boundMax = *material.getVolumeDensityBoundMax();
            ++hintCandidates;
            mHint.component = int(mComponents.size()) - 1;
          }
        }
      }
    }
    if (!material.hasAdditiveVolume()) break;
  }
  if (mComponents.empty()) return;
  mHasMedium = true;
  mHasOverlap = mComponents.size() > 1;
  mScatterInstance = mComponents.front().material;
  // The aggregates. The homogeneous closed form runs on the summed
  // snapshots when every component is homogeneous; otherwise the
  // tracking loops run against the summed majorants, a homogeneous
  // component contributing its exact spectrum as its own bound. Assigned
  // rather than filled: the members are sized here, being empty until a
  // medium resolves.
  mSigmaA = Color();
  mSigmaS = Color();
  mEmission = Color();
  for (const auto &comp : mComponents) {
    mSigmaA += comp.sigmaA;
    mSigmaS += comp.sigmaS;
    mEmission += comp.emission;
    mIsHeterogeneous |= comp.isHeterogeneous;
  }
  mSigmaT = mSigmaA + mSigmaS;
  if (!mIsHeterogeneous) return;
  mMaxSigmaA = Color();
  mMaxSigmaS = Color();
  for (const auto &comp : mComponents) {
    mMaxSigmaA += comp.isHeterogeneous ? comp.maxSigmaA : comp.sigmaA;
    mMaxSigmaS += comp.isHeterogeneous ? comp.maxSigmaS : comp.sigmaS;
  }
  mMajorant = (mMaxSigmaA + mMaxSigmaS).maxComponent();
  // The density hint can drive the walk only when exactly one component
  // has a usable grid: its contribution scales per span, and everything
  // else is the constant base. With competing grids (or none) the whole
  // majorant is the constant global span, whose lower bound the walk
  // reports as zero, so the control goes unread.
  if (hintCandidates == 1) {
    Component &comp{mComponents[size_t(mHint.component)]};
    comp.isScaledByGrid = true;
    mHint.majorant = comp.maxSigmaA + comp.maxSigmaS;
    mMajorantGrid = mHint.majorant.maxComponent();
    mMajorantBase = std::max(
        (mMaxSigmaA + mMaxSigmaS - mHint.majorant).maxComponent(), 0.0f);
    setHint(comp);
  } else {
    mHint.component = -1;
    mHint.majorant = Color();
    mMajorantGrid = mMajorant;
    mMajorantBase = 0.0f;
  }
}

void Medium::setHint(const Component &comp) noexcept {
  const float3 &boundMin{comp.boundMin};
  const float3 &boundMax{comp.boundMax};
  const int3 extent{comp.grid->getExtent()};
  const float cellExtent{float(comp.grid->getMajorantExtent())};
  mHint.grid = comp.grid;
  mHint.boundMin = boundMin;
  mHint.cellScale =
      float3(float(extent.x) / (cellExtent * (boundMax.x - boundMin.x)),
             float(extent.y) / (cellExtent * (boundMax.y - boundMin.y)),
             float(extent.z) / (cellExtent * (boundMax.z - boundMin.z)));
  mHint.invMaxValue = 1.0f / comp.grid->getMaxValue();
}
//--}

//--{ Segment
void Medium::setSegment(const float3 &org, const float3 &dir,
                        float time) noexcept {
  if (mIsHaze) {
    mHaze.haze->extinctionAt(
        org.z, smdl::Span<float>(mHaze.sigmaC.data(), mHaze.sigmaC.size()));
    mHaze.k = mHaze.haze->shapeExponent(dir.z);
    return;
  }
  // A homogeneous medium has the same coefficients everywhere, so it
  // never queries and has no segment to place.
  if (!mIsHeterogeneous) return;
  if (SMDL_UNLIKELY(mIsMoving)) {
    projectSegmentMoving(org, dir, time);
  } else {
    projectSegment(org, dir);
  }
  if (mHint.grid) {
    const Component &comp{mComponents[size_t(mHint.component)]};
    for (int axis = 0; axis < 3; axis++) {
      mHint.cellOrg[axis] =
          (comp.orgR[axis] - mHint.boundMin[axis]) * mHint.cellScale[axis];
      mHint.cellDir[axis] = comp.dirR[axis] * mHint.cellScale[axis];
    }
  }
}

void Medium::projectSegment(const float3 &org, const float3 &dir) noexcept {
  projectSegmentWith(org, dir,
                     [](const MeshInstance &instance) -> const float4x4 & {
                       return instance.frame.worldToRigid;
                     });
}

void Medium::projectSegmentMoving(const float3 &org, const float3 &dir,
                                  float time) noexcept {
  // The scratch outlives every reference the projection takes out of
  // it, each of which is consumed before the next component asks.
  std::optional<InstanceFrame> scratch{};
  projectSegmentWith(org, dir,
                     [&](const MeshInstance &instance) -> const float4x4 & {
                       return instance.frameAt(time, scratch).worldToRigid;
                     });
}
//--}

//--{ Queries
SMDL_ALWAYS_INLINE void Medium::query(const Component &comp, float t,
                                      float majorantScale, bool shouldStash,
                                      Color &sigmaA, Color &sigmaS,
                                      Color &emission) const {
  if (!comp.isHeterogeneous) {
    sigmaA = comp.sigmaA;
    sigmaS = comp.sigmaS;
    emission = comp.emission;
    return;
  }
  comp.state->position = comp.orgR + t * comp.dirR;
  comp.material->def->volumeEvaluate(*comp.state, sigmaA.data(), sigmaS.data(),
                                     emission.data());
  // Clamp so a lying majorant or density hint renders a clamped medium
  // instead of accumulating negative-weight bias. The emission
  // coefficient never gates sampling, so it needs no bound for
  // unbiasedness and only clamps nonnegative.
  clampCoefficients(sigmaA, sigmaS, emission, comp.maxSigmaA, comp.maxSigmaS,
                    comp.isScaledByGrid ? majorantScale : 1.0f);
  if (shouldStash) comp.lastSigmaS = sigmaS;
}

SMDL_ALWAYS_INLINE void
Medium::evaluateCoefficients(float t, float majorantScale, Color &sigmaA,
                             Color &sigmaS, Color &emission) const {
  if (SMDL_LIKELY(!mHasOverlap)) {
    query(mComponents.front(), t, majorantScale, false, sigmaA, sigmaS,
          emission);
    return;
  }
  evaluateOverlap(t, majorantScale, sigmaA, sigmaS, emission);
}

SMDL_NO_INLINE void Medium::evaluateOverlap(float t, float majorantScale,
                                            Color &sigmaA, Color &sigmaS,
                                            Color &emission) const {
  // The first component writes the sums and the rest add into them, one
  // component's query reused down the rest: `query()` overwrites every
  // band of all three.
  query(mComponents.front(), t, majorantScale, true, sigmaA, sigmaS, emission);
  Color a{}, s{}, e{};
  for (size_t i = 1; i < mComponents.size(); i++) {
    query(mComponents[i], t, majorantScale, true, a, s, e);
    sigmaA += a;
    sigmaS += s;
    emission += e;
  }
}

void Medium::pickScatterComponent(float xi, const Color &sigmaS,
                                  Color &beta) const {
  // Selection probability proportional to the component's share of the
  // scattering coefficient at the collision, averaged over bins. Zero
  // total scattering (a pure-absorption collision) leaves `beta` all
  // zero already; default to the first component so the caller always
  // has a phase function.
  mScatterInstance = mComponents.front().material;
  float totalAverage{};
  for (const auto &comp : mComponents)
    totalAverage += componentSigmaS(comp).average();
  if (!(totalAverage > 0.0f)) return;
  const Component *picked{};
  float pickedChance{};
  float cdf{};
  for (const auto &comp : mComponents) {
    const float average{componentSigmaS(comp).average()};
    if (!(average > 0.0f)) continue;
    picked = &comp;
    pickedChance = average / totalAverage;
    cdf += pickedChance;
    if (xi < cdf) break;
  }
  mScatterInstance = picked->material;
  applySpectralShare(beta, sigmaS, componentSigmaS(*picked),
                     1.0f / pickedChance);
}
//--}

//--{ Tracking
template <typename MajorantOf, typename Collide>
Medium::Outcome Medium::track(smdl::RNG &rng, float tEnd, MajorantOf majorantOf,
                              Collide collide) const {
  smdl::MajorantSpanWalk spans{mHint.grid,    mHint.cellOrg,
                               mHint.cellDir, mHint.invMaxValue,
                               tEnd,          !(mMajorantBase > 0.0f)};
  smdl::MajorantSpan span{};
  int iter{0};
  float tau{-smdl::fastLog(rng.generateFloat())};
  while (spans.next(span)) {
    // A zero majorant only reaches here when a component that does not
    // scale with the grid kept the walk from skipping the cell itself.
    const float m{majorantOf(span)};
    if (!(m > 0.0f)) continue;
    float tCur{span.t0};
    while (true) {
      if (const float dTau{m * (span.t1 - tCur)}; tau >= dTau) {
        tau -= dTau;
        break;
      }
      if (SMDL_UNLIKELY(++iter > MAX_TENTATIVE_COLLISIONS))
        return Outcome::DEAD;
      tCur += tau / m;
      if (const Outcome outcome{collide(tCur, span, m, tau)};
          outcome != Outcome::CONTINUE)
        return outcome;
    }
  }
  return Outcome::SURVIVED;
}
//--}

//--{ Distance sampling
bool Medium::sampleDistance(Sampler &sampler, float tEnd, float &t, Color &beta,
                            Color &emitted) const {
  if (SMDL_UNLIKELY(!mHasMedium)) return false;
  if (SMDL_UNLIKELY(mIsHaze)) return sampleDistanceHaze(sampler, tEnd, t, beta);
  if (!mIsHeterogeneous)
    return sampleDistanceHomogeneous(sampler, tEnd, t, beta, emitted);
  return sampleDistanceTracked(sampler, tEnd, t, beta, emitted);
}

SMDL_NO_INLINE bool Medium::sampleDistanceHaze(Sampler &sampler, float tEnd,
                                               float &t, Color &beta) const {
  // The analytic exponential-height medium. The optical depth is the
  // extinction at the segment origin times one distance shape shared
  // by every band, so the free-flight distance inverts in closed form
  // against the hero band and the other bands' transmittance follows
  // from the same shape: the homogeneous estimator with `shape(t)` in
  // place of `t`. The haze does not emit.
  //
  // One pair for the free-flight draw and the hero band together, so the
  // segment costs the sampler one pair rather than two.
  const float2 u{float2(sampler)};
  const float xi{u.x};
  const int hero{Sampler::indexOf(u.y, int(mHaze.sigmaC.size()))};
  // Both the collision and the segment end are placed by their shape
  // rather than their distance. The shape is monotone, so the two
  // orders agree, and only a collision pays the inversion. The end is
  // clamped because an unbounded segment that never turns upward has
  // infinite depth, and infinity times a band whose extinction has
  // underflowed to zero is not a number.
  const float sScatter{-smdl::fastLog(1.0f - xi) / mHaze.sigmaC[size_t(hero)]};
  const float sEnd{std::min(smdl::Haze::shape(mHaze.k, tEnd), FLOAT_MAX)};
  const bool hasScattered{sScatter < sEnd};
  const float depth{hasScattered ? sScatter : sEnd};
  Color Tr{};
  fillTransmittance(Tr, mHaze.sigmaC, depth);
  if (hasScattered) {
    // The extinction at the collision is the origin spectrum times a
    // factor common to every band, which cancels between the
    // scattering weight and the balance heuristic that normalizes it.
    const float invNorm{1.0f / averageOfProducts(mHaze.sigmaC, Tr)};
    applyHazeScatterWeight(beta, mHaze.sigmaC, mHaze.albedo, Tr, invNorm);
    t = smdl::Haze::shapeInverse(mHaze.k, sScatter);
    return true;
  }
  applySurvivalWeight(beta, Tr, 1.0f / Tr.average());
  return false;
}

SMDL_NO_INLINE bool Medium::sampleDistanceHomogeneous(Sampler &sampler,
                                                      float tEnd, float &t,
                                                      Color &beta,
                                                      Color &emitted) const {
  // The emitted radiance along the segment is deterministic for a
  // homogeneous medium: the integral of transmittance times the
  // emission coefficient to the segment end, per bin, regardless of
  // where scattering is sampled below. The extinction-free limit is
  // linear in distance, so an unbounded segment through an emissive
  // vacuum is clamped rather than infinite.
  const smdl::SpectralColor &mu{mSigmaT};
  if (SMDL_UNLIKELY(mHasEmission))
    accumulateHomogeneousEmission(emitted, mEmission, mu, std::min(tEnd, 1e8f));
  // The closed-form estimator: the free-flight distance against one
  // uniformly drawn hero wavelength, weighted by the single-sample MIS
  // balance heuristic over all bins. Wavelengths with zero extinction
  // keep transmittance 1 through the min against FLT_MAX. One pair for
  // the draw and the hero band together, and the free flight off the
  // complement of the canonical draw, which is in (0, 1): the same
  // exponential, without the libm call.
  const float2 u{float2(sampler)};
  const float xi{u.x};
  const int hero{Sampler::indexOf(u.y, int(mu.size()))};
  const float tScatter{-smdl::fastLog(1.0f - xi) / mu[size_t(hero)]};
  Color Tr{};
  fillTransmittance(Tr, mu, std::min({tScatter, tEnd, FLOAT_MAX}));
  if (tScatter < tEnd) {
    applyScatterWeight(beta, mSigmaS, Tr, 1.0f / averageOfProducts(mu, Tr));
    // Only an overlap has a phase function to choose; a single medium
    // must not draw here, or every render with a medium moves onto a
    // different sampler dimension.
    if (SMDL_UNLIKELY(mHasOverlap))
      pickScatterComponent(float(sampler), mSigmaS, beta);
    t = tScatter;
    return true;
  }
  applySurvivalWeight(beta, Tr, 1.0f / Tr.average());
  return false;
}

SMDL_NO_INLINE bool Medium::sampleDistanceTracked(Sampler &sampler, float tEnd,
                                                  float &t, Color &beta,
                                                  Color &emitted) const {
  // Delta tracking against the scalar majorant, generalizing the same
  // spectral strategy: the hero wavelength drives the real-or-null
  // classification, and the per-bin products of null factors carry the
  // balance-heuristic weight through the chain. `P` is meaningful only
  // up to a common scale, which cancels in every weight, so it is
  // renormalized as it goes to keep the products from underflowing,
  // which flush-to-zero makes more necessary, not less: a product that
  // underflows reaches exactly zero at the smallest normal float.
  //
  // The chain draws from a plain generator seeded by the sampler rather
  // than from the sampler itself, so that this call consumes a FIXED
  // number of low-discrepancy dimensions: a variable number here would
  // push every draw after the medium onto different dimensions from
  // sample to sample, destroying the stratification of the rest of the
  // path.
  if (SMDL_UNLIKELY(!(mMajorant > 0.0f))) return false;
  const int hero{sampler.index(int(mSigmaA.size()))};
  smdl::RNG rng{nextSeed64(sampler)};
  Color P{1.0f};
  // The coefficients of one tentative collision, held across the chain:
  // `evaluateCoefficients()` overwrites every band of all three.
  Color sigmaA{}, sigmaS{}, emission{};
  const auto majorantOf{[&](const smdl::MajorantSpan &span) {
    // The constant base plus the grid part at the span's scale; a single
    // medium is all grid part, so this is plain scaling.
    return mMajorantBase + mMajorantGrid * span.scale;
  }};
  const auto collide{
      [&](float tCur, const smdl::MajorantSpan &span, float m, float &tau) {
        evaluateCoefficients(tCur, span.scale, sigmaA, sigmaS, emission);
        // The medium's own emission accumulates at every tentative
        // collision, before classification: the chain-survival density
        // times the per-bin null-product weight integrates, in expectation,
        // transmittance times the emission coefficient over the whole
        // segment, at no extra query. The same balance-heuristic
        // normalization as the terminal weights applies, and the common
        // renormalization of `P` cancels.
        if (SMDL_UNLIKELY(mHasEmission)) {
          if (const float pdfEmit{m * P.average()}; pdfEmit > 0.0f)
            accumulateTrackedEmission(emitted, emission, P, 1.0f / pdfEmit);
        }
        const float muTHero{sigmaA[size_t(hero)] + sigmaS[size_t(hero)]};
        if (rng.generateFloat() * m < muTHero) {
          // A real collision: weight by the scattering coefficient over the
          // mixture density of all heroes having produced this chain, the
          // absorption folded into the weight rather than terminating, as
          // in the homogeneous path.
          const float pdf{averageOfExtinctionProducts(sigmaA, sigmaS, P)};
          if (SMDL_UNLIKELY(!(pdf > 0.0f))) return Outcome::DEAD;
          applyScatterWeight(beta, sigmaS, P, 1.0f / pdf);
          if (SMDL_UNLIKELY(mHasOverlap))
            pickScatterComponent(rng.generateFloat(), sigmaS, beta);
          t = tCur;
          return Outcome::SCATTERED;
        }
        // A null collision, and the next flight drawn before the chain is
        // renormalized, so the logarithm overlaps the reduction. Every bin
        // at the majorant means the chain carries no throughput in any
        // wavelength: the path is dead.
        const float renormalize{applyNullWeight(P, sigmaA, sigmaS, m)};
        tau = -smdl::fastLog(rng.generateFloat());
        if (SMDL_UNLIKELY(!(renormalize > 0.0f))) return Outcome::DEAD;
        P *= 1.0f / renormalize;
        return Outcome::CONTINUE;
      }};
  switch (track(rng, tEnd, majorantOf, collide)) {
  case Outcome::SCATTERED:
    return true;
  case Outcome::DEAD:
    beta.fill(0.0f);
    return false;
  default:
    applySurvivalWeight(beta, P, 1.0f / P.average());
    return false;
  }
}
//--}

//--{ Transmittance
void Medium::attenuate(Sampler &sampler, float tEnd, Color &beta,
                       bool isUnbounded) const {
  if (SMDL_UNLIKELY(!mHasMedium)) return;
  if (SMDL_UNLIKELY(mIsHaze)) {
    attenuateHaze(tEnd, beta, isUnbounded);
  } else if (!mIsHeterogeneous) {
    // Closed-form Beer-Lambert. The caller clamps `tEnd` finite, so
    // wavelengths with zero extinction keep transmittance 1 instead of
    // producing 0 times infinity.
    attenuateBy(beta, mSigmaT, tEnd);
  } else {
    attenuateTracked(sampler, tEnd, beta);
  }
}

SMDL_NO_INLINE void Medium::attenuateHaze(float tEnd, Color &beta,
                                          bool isUnbounded) const {
  // Closed-form Beer-Lambert against the analytic optical depth: a
  // shadow ray through the haze is exact and draws nothing, which is
  // the whole reason not to track it.
  attenuateBy(beta, mHaze.sigmaC,
              std::min(smdl::Haze::shape(mHaze.k, isUnbounded ? INF : tEnd),
                       FLOAT_MAX));
}

SMDL_NO_INLINE void Medium::attenuateTracked(Sampler &sampler, float tEnd,
                                             Color &beta) const {
  // Residual ratio tracking (Novak et al. 2014): per span, the
  // extinction splits into a piecewise-constant control (the declared
  // majorant scaled by the span's lower bound) plus a residual. The
  // control transmittance is analytic, accumulated as an optical depth
  // and exponentiated once at the end; only the residual is ratio
  // tracked, at rate proportional to the majorant-minus-control gap. A
  // cell whose bounds coincide costs no collisions at all; without a
  // density hint the control is zero and this reduces to plain ratio
  // tracking. The per-bin expectation is exactly the transmittance for
  // residuals of either sign, so no hero selection or MIS weighting is
  // involved. The chain draws from a seeded generator for the same
  // fixed-dimension-count reason as `sampleDistanceTracked()`.
  if (SMDL_UNLIKELY(!(mMajorant > 0.0f))) return;
  smdl::RNG rng{nextSeed64(sampler)};
  Color sigmaA{}, sigmaS{}, emissionUnused{};
  Color controlDepth{};
  // The control that `span.scaleMin` lower bounds is the hinted
  // component's majorant alone; with overlap the base contribution of
  // the other components stays in the tracked rate at full strength.
  const Color control{mHint.majorant};
  float invM{};
  const auto majorantOf{[&](const smdl::MajorantSpan &span) {
    if (span.scaleMin > 0.0f)
      accumulateControlDepth(controlDepth, control,
                             span.scaleMin * (span.t1 - span.t0));
    const float m{mMajorantGrid * (span.scale - span.scaleMin) + mMajorantBase};
    invM = 1.0f / m;
    return m;
  }};
  const auto collide{[&](float tCur, const smdl::MajorantSpan &span, float m,
                         float &tau) {
    evaluateCoefficients(tCur, span.scale, sigmaA, sigmaS, emissionUnused);
    applyResidualWeight(beta, sigmaA, sigmaS, control, span.scaleMin, m, invM);
    tau = -smdl::fastLog(rng.generateFloat());
    return SMDL_UNLIKELY(!(beta.maxComponent() > 0.0f)) ? Outcome::DEAD
                                                        : Outcome::CONTINUE;
  }};
  if (track(rng, tEnd, majorantOf, collide) == Outcome::DEAD) {
    beta.fill(0.0f);
    return;
  }
  // The analytic control transmittance, one exponential per segment.
  attenuateBy(beta, controlDepth, 1.0f);
}
//--}
