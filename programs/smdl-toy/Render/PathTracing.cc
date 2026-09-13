#include "Render/PathTracing.h"
#include "Render/Camera.h"
#include "Render/Guiding.h"
#include "Render/Light.h"
#include "Render/MNEE.h"
#include "Render/Manifold.h"
#include "Render/PathStats.h"

#include <algorithm>
#include <cstring>

namespace {

// The depth after which the walk is terminated by Russian roulette rather
// than continued unconditionally.
constexpr uint64_t ROULETTE_MIN_DEPTH{4};

// The largest survival probability Russian roulette will use, so that every
// path terminates eventually no matter how bright its throughput is.
constexpr float ROULETTE_MAX_SURVIVAL{0.95f};

// The survival probability under which Russian roulette is worth running at
// a volume vertex. A phase sample carries weight exactly 1 and a scattering
// albedo is often near it, so throughput decays far more slowly in a medium
// than across a surface: rouletting a vertex whose survival is still near 1
// retires a sliver of the paths to save a sliver of the work, and the
// variance of that trade compounds over the many vertices a dense medium
// produces. The gate leaves a bounded medium to terminate the walk by its
// own far side and still bounds one in an unbounded medium, which has none.
constexpr float ROULETTE_VOLUME_GATE{0.25f};

// The ray cone spread growth in radians added by a non-Dirac bounce whose
// material has no diffuse component. Crude heuristic: the JIT instance
// exposes only the DF_* lobe word, not per-lobe roughness.
// Tunable.
constexpr float ANGLE_GROWTH_GLOSSY{0.05f};

// The ray cone spread growth in radians added by a bounce whose material
// has a diffuse component, or by a volume scattering event.
constexpr float ANGLE_GROWTH_DIFFUSE{0.3f};

// The cap on the ray cone spread angle, keeping the cone width growth
// well-conditioned on long paths.
constexpr float ANGLE_MAX{1.0f};

// Fold a MIS-weighted arrival at a light, an environment escape or an
// emitter hit, into the record of the vertex whose continuation reached
// it, so the training target keeps the full estimator's expectation.
// `record` is that vertex's record or null, `beta` the throughput at
// the arrival, and `Larrival` the arrival radiance. The copy without
// the bounce weight trains the tree along the continuation direction
// itself.
void foldArrivalIntoRecord(GuideRecord *record, const Color &beta,
                           const Color &Larrival) {
  if (!record) return;
  for (size_t b = 0; b < record->beta.size(); b++) {
    if (record->beta[b] > 0)
      record->direct[b] += beta[b] / record->beta[b] * Larrival[b];
    record->continuationEmission[b] = Larrival[b];
  }
}

// The bounce that produced the segment in flight, as an arrival at the
// far end of that segment is weighed against. Meaningless on the camera
// segment, which no light sampling competes with.
struct PrevBounce final {
  // The density that sampled the direction.
  float pdf{};

  // Whether it came from a Dirac lobe, whose hits carry weight 1 since
  // light sampling can never produce them.
  bool isDirac{};

  // The vertex it left from, which anchors the solid-angle conversion of
  // the competing light-sample density; cutout hops re-base `ray.org`,
  // so the ray origin cannot serve.
  float3 point{};

  // Whether the gather at that vertex drew its light sample by area, so
  // that the arrival's MIS density is the one it drew with.
  bool isAreaSampled{};

  // The share of the segment's throughput the manifold estimators claim,
  // per wavelength: what an arrival at a light along it must drop, since
  // a gather behind produces it. Zero on a segment nobody claims.
  Color claimedShare{};

  // Whether that share is the reflective gather's, which the light marks
  // may restrict to the caustic targets, so that it applies only to
  // arrivals at one of those; a glossy chain's share is the refractive
  // gather's and applies to any.
  bool shouldShareCausticOnly{};

  // The share of the segment's bounce the vertex's receiving lobes
  // carry, read against the light (see `ReceiverShare`), which its
  // reflective gather's claim at the next vertex is scaled by. Zero on
  // the camera segment and after a Dirac bounce, which no gather claims
  // through.
  ReceiverShare receiverShare{};

  // When `claimedShare` is a reflection's, the receiver whose gather
  // claims it, the vertex before the bounce, with its point: what the
  // arrival reads `claimedShare` against, since the light it lands on
  // decides that receiver's share.
  ReceiverShare claimedReceiver{};
  float3 claimedReceiverPoint{};

  // Begin a path: no bounce behind the camera segment, and no share of
  // it claimed, which is what the arrival sites read on that segment.
  void reset() noexcept {
    pdf = 0.0f;
    isDirac = false;
    point = float3(0.0f);
    isAreaSampled = false;
    claimedShare.fill(0.0f);
    shouldShareCausticOnly = false;
    receiverShare = ReceiverShare{};
    claimedReceiver = ReceiverShare{};
    claimedReceiverPoint = float3(0.0f);
  }
};

} // namespace

// The walk of one camera path at a time: everything it carries from one
// vertex to the next, and the bookkeeping its arrivals are weighed by.
// It lives for a block of paths and `trace()` begins each afresh; the
// states and stacks a path holds are built in the path's allocator,
// which the caller resets between samples.
class PathWalk final {
public:
  PathWalk(const RenderContext &render, PathContext &path)
      : mRender(render), mPath(path), mGatherState(path.gatherState),
        mState(path.walkState) {}

  // Trace the path the camera sample starts and return its radiance
  // estimate; see `tracePath()`.
  [[nodiscard]] Color trace(const CameraSample &camera);

private:
  // The clamp scale of a contribution with the given number of bounces:
  // 1 outside the contribution bound's reach, else what scales the
  // largest band down to the bound. Applied to what a contribution adds
  // to the estimate and to what the guide record retains of it, so the
  // tree trains toward the clamped field it steers.
  [[nodiscard]] float clampScale(const Color &contribution,
                                 uint64_t bounces) const noexcept {
    const PathOptions &bounds{mRender.pathOptions};
    if (!(bounds.maxContribution > 0.0f) ||
        bounces < uint64_t(bounds.maxContributionBounces))
      return 1.0f;
    const float maxValue{contribution.maxComponent()};
    return maxValue > bounds.maxContribution ? bounds.maxContribution / maxValue
                                             : 1.0f;
  }

  // Has the walk scattered as often as it may? Asked at every vertex
  // once its arrival is in: the vertex's own gather would be one bounce
  // deeper than the bound allows.
  [[nodiscard]] bool isAtMaxBounces() const noexcept {
    return mDepth - 1 > mRender.pathOptions.maxBounces;
  }

  // The stack entry of the exterior medium for the path in flight, or
  // null for vacuum: the instance the block already holds when the path's
  // wavelengths and time are the ones it was evaluated with, else a new
  // evaluation.
  [[nodiscard]] const MediumStack *exteriorMedium();

  // Terminate by Russian roulette instead of by a fixed depth limit, so
  // that high-albedo transport keeps the energy it is entitled to.
  // Returns whether the walk continues, scaling the throughput by the
  // reciprocal survival when it does. Asked at every scattering vertex,
  // volume as well as surface: an unbounded medium of high albedo goes
  // on scattering indefinitely otherwise, and the deep vertices cost a
  // gather apiece to carry a throughput roulette would have retired.
  [[nodiscard]] bool rouletteSurvives(const DTree *dtree,
                                      float gate = 1.0f) noexcept {
    if (!mRender.pathOptions.useRoulette || mDepth <= ROULETTE_MIN_DEPTH)
      return true;
    float survival{};
    const float meanRadiance{
        dtree && mPath.guiding->pixelEstimate > 0 ? dtree->meanRadiance() : 0};
    if (meanRadiance > 0) {
      // Adjoint-driven Russian roulette (Vorba & Krivanek, SIGGRAPH
      // 2016; roulette only, no splitting): survive in proportion to the
      // expected pixel contribution of continuing the walk, which is the
      // throughput times the SD-tree's cached mean incident radiance,
      // relative to the pixel's estimate from the previous pass.
      survival = std::clamp(mBeta.average() * meanRadiance /
                                mPath.guiding->pixelEstimate,
                            0.05f, 1.0f);
    } else {
      survival = std::min(ROULETTE_MAX_SURVIVAL, mBeta.maxComponent());
    }
    if (!(survival < gate)) return true;
    if (!(float(mPath.sampler) < survival)) return false;
    mBeta *= 1.0f / survival;
    return true;
  }

  // Weigh an arrival at a light, an environment escape or an emitter
  // hit, and fold it into the estimate and the guide record. Through a
  // Dirac chain of claimed refractive interfaces the arrival competes
  // with the manifold gather at the chain's receiver by re-walk MIS (the
  // chain always follows a Dirac transmission, so the weight it replaces
  // is the `PrevBounce::isDirac` 1), and under the biased claimed mode
  // it is dropped outright instead, the gather's multi-seed estimate
  // having claimed every drawable target exclusively; otherwise the
  // ordinary MIS `weight` applies to the share of the segment's
  // throughput nobody claims, a claimed share toward a target its
  // gather reaches is the gather's outright, and a Dirac chain the
  // caster refractive gather owns is dropped. `makeCoverTarget` fills the
  // target the re-walk aims at and returns the light-sampling density it
  // competes with, negative when there is no target to re-walk, which
  // keeps the ordinary weight.
  template <typename MakeCoverTarget, typename AngularRadiusFrom>
  void addArrival(const Color &Li, float weight, uint64_t bounces,
                  bool isCausticTarget, GuideRecord *record,
                  const MakeCoverTarget &makeCoverTarget,
                  const AngularRadiusFrom &angularRadiusFrom) {
    // The factor on the throughput times the radiance, per band: one
    // weight across the bands for a covered Dirac chain, and otherwise
    // the ordinary weight on the share of each band nobody claims.
    float uniform{weight};
    bool isCoverWeighed{false};
    const Color *share{nullptr};
    if (mCoverage.coversDirac(mRender.mneeOptions)) {
      ManifoldTarget target{};
      const float lightPdf{makeCoverTarget(target)};
      if (lightPdf >= 0.0f) {
        // Whether the chain starts on a caster the caster refractive
        // gather samples for the Dirac kind, by the membership the
        // gather itself goes by; the re-walk then settles which family
        // the arrival belongs to.
        const MNEECaster *caster{lightPdf > 0.0f && mRender.mneeOptions.casters
                                     ? mRender.mneeOptions.casters->casterOf(
                                           mCoverage.firstInstIndex())
                                     : nullptr};
        const bool isCasterChain{
            caster && (caster->refractLobes & smdl::DF_DIRAC_BTDF) != 0};
        uniform = mCoverage.coverWeight(mRender, mPath, target, lightPdf,
                                        isCasterChain);
        isCoverWeighed = true;
      }
    } else if (!(mPrev.shouldShareCausticOnly && !isCausticTarget)) {
      share = &mPrev.claimedShare;
    }
    // Every claim is scaled by the share of the receiver's bounce its
    // receiving lobes carry, read against this light from the receiver
    // (`angularRadiusFrom`): the armed receiver's for a chain, the
    // previous receiver's for a claimed reflection. The rest of the
    // arrival keeps the ordinary weight; see `ReceiverShare`.
    Color receiverShare{1.0f};
    if (isCoverWeighed || (share && !mPrev.shouldShareCausticOnly))
      receiverShare = mCoverage.receiverShare().at(
          angularRadiusFrom(mCoverage.receiver().point));
    else if (share)
      receiverShare = mPrev.claimedReceiver.at(
          angularRadiusFrom(mPrev.claimedReceiverPoint));
    const auto factorAt{[&](size_t b) {
      if (share) return (1.0f - (*share)[b] * receiverShare[b]) * weight;
      if (isCoverWeighed)
        return uniform + (1.0f - receiverShare[b]) * (weight - uniform);
      return uniform;
    }};
    Color contribution{};
    for (size_t b = 0; b < contribution.size(); b++)
      contribution[b] = mBeta[b] * Li[b] * factorAt(b);
    if (contribution.isAnyNonFinite()) return;
    if (mPath.stats) mPath.stats->recordContribution(bounces, contribution);
    const float scale{clampScale(contribution, bounces)};
    if (scale < 1.0f) {
      if (mPath.stats) mPath.stats->recordClamp(bounces, contribution, scale);
      contribution *= scale;
    }
    mL += contribution;
    if (record) {
      Color Larrival{};
      for (size_t b = 0; b < Larrival.size(); b++)
        Larrival[b] = Li[b] * factorAt(b) * scale;
      foldArrivalIntoRecord(record, mBeta, Larrival);
    }
  }

  // Fold a vertex's gathered direct lighting into the estimate under the
  // contribution bound, and into its guide record. The throughput
  // product is formed once: the bound reads it, and it is the
  // contribution itself whenever the bound leaves the gather alone.
  void addGathered(Color &direct, GuideRecord *record) {
    Color contribution{mBeta * direct};
    const uint64_t bounces{mDepth - 1};
    if (mPath.stats) mPath.stats->recordContribution(bounces, contribution);
    if (const float scale{clampScale(contribution, bounces)}; scale < 1.0f) {
      if (mPath.stats) mPath.stats->recordClamp(bounces, contribution, scale);
      direct *= scale;
      for (size_t b = 0; b < contribution.size(); b++)
        contribution[b] = mBeta[b] * direct[b];
    }
    mL += contribution;
    if (record) record->direct = direct;
  }

  const RenderContext &mRender;
  PathContext &mPath;

  // The pristine gather-side state, see `gatherDirect()`.
  const smdl::State &mGatherState;

  // The vertex shading state: the fields that never change were set up
  // with the block's, and the geometric ones at every vertex by
  // `Hit::applyGeometryToState()`.
  smdl::State &mState;

  // The estimate so far, and the throughput the next contribution is
  // weighed by.
  Color mL{};
  Color mBeta{};

  // The nested-medium stack the walk is currently inside, which starts
  // at the scene-wide exterior and evolves across every transmitting
  // boundary the walk crosses.
  const MediumStack *mMediumStack{};

  // The exterior medium's instance, evaluated at the render fields of
  // the path in flight and kept for every following path of the block
  // that shares them; see `exteriorMedium()`. Its own allocator, since
  // the path's is reset between samples.
  smdl::BumpPtrAllocator mExteriorAllocator{};
  const MediumStack *mExteriorMedium{};
  Color mExteriorWavelengths{};
  float mExteriorTime{};
  float mExteriorWavelengthHero{};

  // What the manifold estimators have claimed since the last receiver;
  // see `MNEECoverage`.
  MNEECoverage mCoverage{};

  // The number of path vertices so far, counting the camera as the
  // first.
  uint64_t mDepth{1};

  PrevBounce mPrev{};

  // The path LOD context, which an all-zero camera cone leaves all-zero,
  // and that is "LOD off" per the `State` conventions.
  int mOrder{};
  float mTravel{};
  float mSpread{};
  float mWidth{};
};

const MediumStack *PathWalk::exteriorMedium() {
  if (!mRender.exteriorMediumDef) return nullptr;
  // An instance is exact at the wavelengths, time and hero wavelength it
  // was evaluated with and at no others, which is the contract of the
  // homogeneity proof (see `MaterialDef::hasHomogeneousCoefficients()`),
  // so the exterior is evaluated again whenever a path's differ from the
  // last evaluation's: every path under -wavelength-jitter, an open
  // shutter or a lens that disperses, and once per block otherwise. The
  // instance has no geometry and no side, so the render state's default
  // frame is already the finalized one, and the evaluation takes no
  // sampler draw.
  const Color &wavelengths{mPath.wavelengths};
  const float time{mPath.time.seconds};
  const float wavelengthHero{mPath.wavelengthHero};
  if (!mExteriorMedium || mExteriorTime != time ||
      mExteriorWavelengthHero != wavelengthHero ||
      std::memcmp(mExteriorWavelengths.data(), wavelengths.data(),
                  wavelengths.size() * sizeof(float)) != 0) {
    mExteriorAllocator.reset();
    smdl::State state{makeRenderState(wavelengths, &mExteriorAllocator, time,
                                      wavelengthHero)};
    mExteriorMedium = new (mExteriorAllocator)
        MediumStack{nullptr,
                    mExteriorAllocator.allocate<smdl::JIT::Material>(
                        state, mRender.exteriorMediumDef),
                    nullptr};
    mExteriorWavelengths = wavelengths;
    mExteriorTime = time;
    mExteriorWavelengthHero = wavelengthHero;
  }
  return mExteriorMedium;
}

Color PathWalk::trace(const CameraSample &camera) {
  // Begin the path: nothing below reads what the last one left, other
  // than through these.
  mPath.numRecords = 0;
  mPath.medium.beginPath();
  mL.fill(0.0f);
  mBeta.fill(camera.weight);
  mMediumStack = exteriorMedium();
  mCoverage.disarm();
  mDepth = 1;
  mPrev.reset();
  mOrder = 0;
  mTravel = 0.0f;
  // The camera's own per-pixel cone spread seeds the LOD context.
  mSpread = camera.coneAngle;
  mWidth = 0.0f;
  const EnvLight *envLight{mRender.lights.env()};

  // The camera segment. The walk re-bases this at every vertex and at
  // every cutout hop, so it is the path's own copy.
  Ray ray{camera.ray};

  Color f{};
  float wpdf{};
  // The JIT ABI reports a reverse PDF alongside every forward PDF, which a
  // forward path tracer never consumes; every call shares this sink.
  float wpdfRevUnused{};
  // The walk ends by escape, absorption, roulette, or the bounce bound,
  // never by this loop's own condition.
  // The hit the casts fill, one record for the whole walk: `intersect()`
  // writes every field where it finds a surface and the walk ends where
  // it does not, so nothing reads what the last vertex left.
  Hit hit{};
  // Why the walk ended, for the tally.
  PathEnd end{};
  while (true) {
    bool hasHitSurface{mRender.scene.intersect(ray, hit)};
    // The stack being empty is the exterior segment, and with no haze
    // it is vacuum, the common case: the view is left alone rather than
    // resolved to nothing, which would still walk the stack.
    if (mMediumStack || mPath.medium.hasHaze()) {
      mPath.medium.reset(mMediumStack, mPath.wavelengths, mPath.time,
                         mPath.wavelengthHero, ray.org, ray.dir);
      if (mPath.medium.hasMedium()) {
        // Sample a free-flight distance over the cast, which
        // `Scene::intersect` bounded at the hit parameter (or left
        // unbounded on a miss). The medium weighs `mBeta` itself: the
        // scattering weight on an event, the transmittance weight on
        // surviving to the surface or escape. The medium's own emission
        // along the segment accumulates separately, weighted by the
        // throughput from before the segment, and lands at weight 1:
        // light sampling never competes with it, the same as an
        // unregistered emitter. No guide record retains it, since the
        // trainer learns the reflected field.
        float t{};
        Color emitted{};
        bool hasScattered{};
        if (SMDL_UNLIKELY(mPath.medium.hasEmission())) {
          const Color betaStart{mBeta};
          hasScattered = mPath.medium.sampleDistance(mPath.sampler, ray.tmax, t,
                                                     mBeta, emitted);
          const Color Lemit{betaStart * emitted};
          if (!Lemit.isAnyNonFinite()) {
            mL += Lemit;
            if (mPath.stats) mPath.stats->recordMediumEmission(Lemit);
          }
        } else {
          hasScattered = mPath.medium.sampleDistance(mPath.sampler, ray.tmax, t,
                                                     mBeta, emitted);
        }
        if (hasScattered) {
          // A volume scattering event.
          ++mDepth;
          ++mOrder;
          mTravel += t;
          mWidth += mSpread * t;
          const float3 point{ray(t)};
          const float3 wo{-ray.dir};
          GuideRecord *record{mPath.records ? &mPath.records[mPath.numRecords++]
                                            : nullptr};
          if (record) {
            *record = GuideRecord{};
            record->point = point;
            record->beta = mBeta;
          }
          if (isAtMaxBounces()) {
            end = PathEnd::BOUND;
            break;
          }
          // The phase function of the vertex: the haze's own, or the
          // medium's (with additive overlap, the component the collision
          // picked), which is the instance's VDF when the definition
          // proves it point-independent and otherwise the VDF evaluated
          // at the collision into the path's allocator. Whatever it
          // names outlives the view, so the gather below is free to
          // retarget the view.
          const Scatterer phase{mPath.medium.scatterer(mPath.allocator)};
          bool ranManifold{false};
          {
            PathVertex vertex{phase};
            vertex.kind = VertexKind::VOLUME;
            vertex.point = point;
            vertex.wo = wo;
            vertex.mediumStack = mMediumStack;
            // A phase function receives every light, as a smooth lobe.
            vertex.finiteLobes = smdl::DF_SMOOTH;
            // The SD-tree never participates at volume vertices, so the
            // vertex keeps its null cell and the continuation density
            // the gather weighs against is the phase function alone.
            Color direct{gatherDirect(mRender, mPath, mGatherState, vertex)};
            addGathered(direct, record);
            ranManifold = vertex.ranManifold;
          }
          // Sample the vertex's phase function. It returns the phase
          // value, which is also the solid-angle PDF of having sampled
          // it, so the throughput weight is exactly 1 and `mBeta` is
          // unchanged.
          float3 wNext{};
          float phaseValue{
              phase.volumeScatterSample(float4(mPath.sampler), wo, wNext)};
          if (!(phaseValue > 0)) {
            end = PathEnd::ABSORBED;
            break;
          }
          if (record) {
            record->wNext = wNext;
            record->wNextPdf = phaseValue;
          }
          mPrev.pdf = phaseValue;
          mPrev.isDirac = false;
          // A phase function has no lobe that would not receive.
          mPrev.receiverShare = ReceiverShare::whole();
          mPrev.point = point;
          mPrev.isAreaSampled = ranManifold;
          // A volume vertex is a manifold-NEE receiver like any other.
          mCoverage.arm(mRender.mneeOptions.isEnabled(),
                        MNEEReceiver{point, wo, mMediumStack}, phaseValue,
                        ReceiverShare::whole());
          // Phase functions scatter wide, so grow the cone like a
          // diffuse bounce.
          mSpread = std::min(mSpread + ANGLE_GROWTH_DIFFUSE, ANGLE_MAX);
          // No SD-tree steers a volume vertex, so the throughput is the
          // only thing the roulette can weigh here.
          if (!rouletteSurvives(/*dtree=*/nullptr, ROULETTE_VOLUME_GATE)) {
            end = PathEnd::ROULETTE;
            break;
          }
          ray = Ray{point, wNext, EPS, INF, mPath.time.fraction};
          continue;
        }
      }
    }
    if (!hasHitSurface) {
      // The walk escaped the scene: the segment is the BSDF-sampling half
      // of the MIS pair, so add the environment weighted against what the
      // light-sampling gather at the previous vertex would have produced.
      // No pdf gate on the radiance: with MIS compensation the environment
      // sampling density is zero below the mean radiance, but the radiance
      // is not; those directions are exactly the ones this half alone must
      // cover, at weight 1, which is what the power heuristic degrades to.
      if (envLight) {
        float Lipdf{};
        Color Li{envLight->Li(mRender.compiler, mGatherState, mPath.skyBasis,
                              ray.dir, Lipdf)};
        float weight{
            mDepth == 1 || mPrev.isDirac
                ? 1.0f
                : smdl::powerHeuristic(
                      mPrev.pdf,
                      mRender.lights.envSelectionPMF(mPrev.point) * Lipdf)};
        // The re-walk's `coverWeight` returns 1 whenever the gather
        // cannot produce this transport, which covers the dim sky the
        // compensated environment sampler never draws (`Lipdf` zero),
        // fold solutions the walk does not find, and failed walks. A
        // sun-gated sky arrival reports no target at all, so it keeps
        // its ordinary weight without spending a re-walk; the gather
        // side stands down by the same predicate.
        addArrival(
            Li, weight, mDepth - 1, mRender.lights.isCausticEnv(),
            mPath.records && mPath.numRecords > 0
                ? &mPath.records[mPath.numRecords - 1]
                : nullptr,
            [&](ManifoldTarget &target) {
              if (!mRender.mneeOptions.isEnvTarget(ray.dir)) return -1.0f;
              target.wl = ray.dir;
              return mRender.lights.envSelectionPMF(
                         mCoverage.receiver().point) *
                     Lipdf;
            },
            [&](const float3 &) {
              return mRender.lights.angularRadiusOfEnv(ray.dir);
            });
      }
      if (mPath.records) {
        mPath.records[mPath.numRecords] = GuideRecord{};
        mPath.records[mPath.numRecords].isInfiniteLight = true;
        ++mPath.numRecords;
      }
      end = PathEnd::ESCAPED;
      break;
    }

    // The distance traveled by this cast, which the cone widens over. A
    // cutout passthrough below is not a scattering event, so it commits
    // the distance and width but not the order.
    const float castDistance{ray.tmax};
    // A hair vertex: a curve hit whose material binds `material.hair`,
    // which routes scattering through the hair entry points. A curve hit
    // whose material has no hair keeps the ordinary surface path, and a
    // hair material on non-curve geometry shades as its (typically
    // default) surface. Hair fibers are also not medium boundaries:
    // transmission through the fiber is part of the BSDF, so the
    // null-interface hop and the medium-stack bookkeeping stand down.
    const bool isHair{hit.instance->isCurves() && hit.materialDef->hasHair()};
    // A null interface left through its exterior side needs no state
    // and no instance: the entry to drop is found by the instance and
    // the material, and the side is the record's geometry normal, the
    // normal the instance's own side test reads. The seed draw every
    // hit makes is still made, so the sample sequence stands.
    if (hit.materialDef->isNullInterface() && !isHair &&
        dot(hit.Ng, ray.dir) > 0.0f) {
      (void)nextSeed64(mPath.sampler);
      MediumStack::Leave(mMediumStack, mPath.allocator, hit.materialDef,
                         hit.instance);
      mTravel += castDistance;
      mWidth += mSpread * castDistance;
      ray = Ray{hit.point, ray.dir, EPS, INF, mPath.time.fraction};
      continue;
    }
    hit.applyGeometryToState(mState, ray.dir);
    mState.scatteringOrder = mOrder + 1;
    mState.travelDistance = mTravel + castDistance;
    mState.coneAngle = mSpread;
    mState.coneWidth = mWidth + mSpread * castDistance;
    // Reseed the stochastic-evaluation generator at every vertex, so
    // stochastically evaluated BSDFs decorrelate across bounces, samples,
    // and pixels while staying deterministic for a given sampler state.
    mState.rng = smdl::RNG(nextSeed64(mPath.sampler), uint64_t(mOrder));
    // The vertex's instance lives in the path's allocator, beside the
    // coefficients the evaluate keeps there, so the stack entry a
    // crossing pushes and the vertex record share it by address.
    smdl::JIT::Material &material{
        *mPath.allocator.allocate<smdl::JIT::Material>(mState,
                                                       hit.materialDef)};
    material.setExteriorIOR(ExteriorIOR(mMediumStack, material, -ray.dir));
    // A null interface, a boundary that scatters nothing itself but
    // encloses a participating medium (e.g., a smoke container), and a
    // cutout the opacity draw passes both hop straight through:
    // committing the distance and width but not the order, with only the
    // medium-stack bookkeeping.
    if (const bool hopsThrough{[&] {
          if (hit.materialDef->isNullInterface() && !isHair) return true;
          const float opacity{material.getCutoutOpacity()};
          return opacity < 1 &&
                 (opacity == 0 || float(mPath.sampler) > opacity);
        }()}) {
      MediumStack::Update(mMediumStack, mPath.allocator, &material,
                          hit.instance, -ray.dir, ray.dir);
      mTravel += castDistance;
      mWidth += mSpread * castDistance;
      ray = Ray{hit.point, ray.dir, EPS, INF, mPath.time.fraction};
      continue;
    }

    ++mDepth;
    ++mOrder;
    mTravel += castDistance;
    mWidth += mSpread * castDistance;
    const float3 wo{-ray.dir};
    // The side of the interface the path arrived on, which selects the
    // material's `backface` scattering tree where it declares one. This
    // is the side the scattering functions derive for themselves, so the
    // lobes read from it describe the tree they will actually run. A hair
    // hit has no such side: `wo` behind the fiber normal is an ordinary
    // configuration rather than a backface.
    const bool isBackface{!isHair && material.isInterior(wo)};
    GuideRecord *record{mPath.records ? &mPath.records[mPath.numRecords++]
                                      : nullptr};
    if (record) {
      *record = GuideRecord{};
      record->point = hit.point;
      record->beta = mBeta;
    }

    // A directly visible emitter: the segment that found it is the
    // BSDF-sampling half of the MIS pair, so weigh the emission against
    // what the light-sampling gather at the previous vertex would have
    // produced. The camera hit (`mDepth` counts the camera, so that is
    // depth 2) has no competing strategy, and neither does a Dirac bounce,
    // whose direction light sampling can never generate; both add at
    // weight 1. An unregistered emitter (one light selection never picks)
    // reports a zero density and lands at weight 1 the same way.
    if (material.hasEmission()) {
      Color Le{};
      if (mRender.lights.emittedRadiance(material, hit.instIndex, wo, Le)) {
        float weight{mDepth == 2 || mPrev.isDirac
                         ? 1.0f
                         : smdl::powerHeuristic(
                               mPrev.pdf, mRender.lights.solidAnglePDF(
                                              hit.instIndex, hit.faceIndex,
                                              hit.point, hit.Ng, mPrev.point,
                                              mPrev.isAreaSampled, hit.time))};
        addArrival(
            Le, weight, mDepth - 2,
            mRender.lights.isCausticLight(hit.instIndex),
            mPath.records && mPath.numRecords > 1
                ? &mPath.records[mPath.numRecords - 2]
                : nullptr,
            [&](ManifoldTarget &target) {
              const float3 toLight{hit.point - mCoverage.receiver().point};
              const float distStraight{length(toLight)};
              if (!(distStraight > 0.0f)) return -1.0f;
              target.wl = toLight / distStraight;
              target.point = hit.point;
              target.isInfinite = false;
              target.normal = hit.Ng;
              return mRender.lights.solidAnglePDF(
                  hit.instIndex, hit.faceIndex, hit.point, hit.Ng,
                  mCoverage.receiver().point,
                  /*isAreaSampled=*/true, hit.time);
            },
            [&](const float3 &point) {
              return mRender.lights.angularRadiusOfInstance(hit.instIndex,
                                                            point);
            });
      }
    }
    if (isAtMaxBounces()) {
      end = PathEnd::BOUND;
      break;
    }
    // With guiding active, non-Dirac surface bounces one-sample-MIS the
    // SD-tree against the BSDF. Materials whose scattering is purely a
    // Dirac delta bypass guiding entirely: the tree cannot produce their
    // directions and evaluating the BSDF at a guided direction would
    // always be zero. Looked up before the gather, whose MIS weight has
    // to match the continuation density. Hair vertices bypass guiding
    // outright: the guided halves would evaluate the surface BSDF rather
    // than the hair BSDF. A claimed vertex needs no bypass: the share of
    // its continuation the manifold estimators claim is decided by the
    // value the direction carries, not by what sampled it.
    const bool wasArmed{mCoverage.isArmed()};
    const DTree *dtree{guidingCellAt(
        mPath.guiding, hit.point,
        !isHair && (material.getLobes(isBackface) & smdl::DF_FINITE) != 0)};
    // The one-sample-MIS mixture weight at this vertex: the cell's
    // learned weight unless pinned for experiments. Meaningful only when
    // `dtree` is non-null, and shared by the gather below, whose MIS
    // weight has to match the continuation density.
    const float bsdfFraction{bsdfFractionAt(mPath.guiding, dtree)};
    // What the manifold estimators claim at this vertex: the instance's
    // claim, narrowed to what the gathers behind can actually reach from
    // here. This vertex gathers the rest, and the claimed share of its
    // continuation is dropped at the light.
    const ManifoldClaim claim{
        mRender.mneeOptions.isEnabled() && !isHair
            ? manifoldClaim(material, isBackface, hit.instance->isCausticCaster)
            : ManifoldClaim()};
    const ManifoldClaim reachable{
        mCoverage.reach(claim, mRender.mneeOptions, mPrev.isDirac)};
    // What this vertex can receive with: its finite lobes, and the
    // narrowest width of its glossy ones, which `gatherDirect()` judges
    // per light against the light's angular radius. A vertex with a
    // finite lobe arms for the claims behind, whose shares are read
    // against their light the same way; see `manifoldReceiverLobes()`.
    const int finiteLobes{mRender.mneeOptions.isEnabled() && !isHair
                              ? material.getLobes(isBackface) & smdl::DF_FINITE
                              : 0};
    const bool isReceiver{finiteLobes != 0};
    const float glossyWidth{
        isReceiver ? manifoldGlossyWidth(material, isBackface,
                                         [&] { return float4(mPath.sampler); })
                   : INFINITY};
    // The receiver behind this vertex, before the bounce below replaces
    // it: whose reflective gather a reflection claimed here belongs to.
    const ReceiverShare shareBehind{mPrev.receiverShare};
    const float3 pointBehind{mPrev.point};
    bool ranManifold{false};
    // Gather direct lighting at this vertex, before the bounce, so the
    // cone the gather rays inherit is the arrival cone.
    {
      PathVertex vertex{material};
      vertex.kind = isHair ? VertexKind::HAIR : VertexKind::SURFACE;
      vertex.point = hit.point;
      vertex.wo = wo;
      vertex.instance = hit.instance;
      vertex.mediumStack = mMediumStack;
      vertex.dtree = dtree;
      vertex.bsdfFraction = bsdfFraction;
      vertex.reachableClaim = reachable;
      vertex.isArmedBehind = wasArmed;
      vertex.isReceiver = isReceiver;
      vertex.finiteLobes = finiteLobes;
      vertex.glossyWidth = glossyWidth;
      vertex.reflectShareBehind = shareBehind;
      vertex.reflectPointBehind = pointBehind;
      vertex.refractShareBehind = mCoverage.receiverShare();
      vertex.refractPointBehind = mCoverage.receiver().point;
      Color direct{gatherDirect(mRender, mPath, mGatherState, vertex)};
      addGathered(direct, record);
      ranManifold = vertex.ranManifold;
    }

    float3 wNext{};
    int sampledLobe{};
    bool isDiracBounce{};
    if (isHair) {
      // There are no Dirac hair distributions, so every accepted sample
      // is a finite-density direction.
      if (!material.hairScatterSample(float4(mPath.sampler), wo, wNext, wpdf,
                                      wpdfRevUnused, f)) {
        end = PathEnd::ABSORBED;
        break;
      }
    } else if (dtree) {
      // One-sample MIS between the BSDF and the SD-tree: either half's
      // sample is weighed by the mixture density.
      float bsdfPdf{};
      float guidePdf{};
      if (float(mPath.sampler) < bsdfFraction) {
        if (!material.scatterSample(float4(mPath.sampler), wo, wNext, bsdfPdf,
                                    wpdfRevUnused, f, sampledLobe)) {
          end = PathEnd::ABSORBED;
          break;
        }
        if (isDiracBounce = (sampledLobe & smdl::DF_DIRAC) != 0; !isDiracBounce)
          guidePdf = dtree->pdf(wNext);
      } else {
        if (wNext = dtree->sampleDirection(mPath.sampler, guidePdf);
            !(guidePdf > 0) ||
            !material.scatterEvaluate(wo, wNext, bsdfPdf, wpdfRevUnused, f)) {
          end = PathEnd::ABSORBED;
          break;
        }
      }
      if (isDiracBounce) {
        // The Dirac lobe folds its density into `f` (unit PDF by
        // convention), and the tree cannot compete with it, so the only
        // density left is the discrete chance of having chosen BSDF
        // sampling at all.
        wpdf = bsdfFraction;
      } else {
        wpdf = guidedMixturePdf(guidePdf, bsdfPdf, bsdfFraction);
        if (record) {
          record->wNextBsdfPdf = bsdfPdf;
          record->wNextGuidePdf = guidePdf;
          record->fAvg = f.average();
        }
      }
    } else if (material.scatterSample(float4(mPath.sampler), wo, wNext, wpdf,
                                      wpdfRevUnused, f, sampledLobe)) {
      isDiracBounce = (sampledLobe & smdl::DF_DIRAC) != 0;
    } else {
      end = PathEnd::ABSORBED;
      break;
    }
    // Grow the ray cone for the bounce. Dirac bounces leave the spread
    // unchanged; otherwise the growth is a crude heuristic since the
    // instance exposes only the DF_* lobe word, and a material whose
    // sampled lobe was specular still gets its diffuse growth, which errs
    // toward more prefiltering deeper in the path.
    if (!isDiracBounce) {
      mSpread = std::min(
          mSpread + ((material.getLobes(isBackface) & smdl::DF_SMOOTH) != 0
                         ? ANGLE_GROWTH_DIFFUSE
                         : ANGLE_GROWTH_GLOSSY),
          ANGLE_MAX);
    }
    if (record) {
      record->wNext = wNext;
      record->wNextPdf = wpdf;
      record->isDiracBounce = isDiracBounce;
    }
    mPrev.pdf = wpdf;
    mPrev.isDirac = isDiracBounce;
    mPrev.point = hit.point;
    mPrev.isAreaSampled = ranManifold;
    const bool transmits{!isHair && material.isTransmitting(wo, wNext)};
    // A reflection claimed here is the previous receiver's gather's, to
    // the share of that receiver's bounce its receiving lobes carry
    // against the light the arrival lands on (`PrevBounce::claimedReceiver`);
    // a claimed transmission extends the armed receiver's chain, read the
    // same way at the arrival.
    const Color claimedShare{claimedShareOf(material, reachable, wo, wNext, f,
                                            isDiracBounce, transmits,
                                            sampledLobe)};
    const ReceiverShare receiverShare{
        receiverShareOf(material, finiteLobes, glossyWidth, wo, wNext, f,
                        isDiracBounce, isReceiver)};
    // Advance the MNEE coverage: a non-Dirac, non-hair
    // vertex is a fresh receiver whose gather may attempt a connection,
    // if it is a receiver at all (a narrow glossy vertex is not, and
    // disarms); a claimed transmission extends the chain from the
    // receiver behind, Dirac or glossy; anything else (a hair vertex, a
    // reflection, an unclaimed or index-matched transmission) breaks it. A
    // glossy transmission that extends a chain is a finite-density bounce that
    // would otherwise have armed a new receiver, and it must not: the
    // gather at the receiver behind it is what claims this chain.
    bool hasExtendedChain{false};
    if (!isHair && mCoverage.isArmed() && transmits &&
        (claim.refractLobes &
         (isDiracBounce ? smdl::DF_DIRAC_BTDF : smdl::DF_GLOSSY_BTDF)) != 0) {
      hasExtendedChain = true;
      mCoverage.extend(hit, !isDiracBounce, claimedShare);
    } else if (!isHair && !isDiracBounce) {
      mCoverage.arm(
          isReceiver,
          MNEEReceiver{hit.point, wo, mMediumStack, &material, hit.instance},
          wpdf, receiverShare);
    } else {
      mCoverage.disarm();
    }
    // What the next arrival drops: the chain's claimed share if this bounce
    // extended a glossy chain the gather can reach (a Dirac chain is
    // weighed instead, and an overlong or mixed one is nobody's), else the
    // share of this one reflection.
    mPrev.claimedShare = hasExtendedChain
                             ? (mCoverage.coversGlossy(mRender.mneeOptions)
                                    ? mCoverage.chainShare()
                                    : Color(0.0f))
                             : claimedShare;
    mPrev.shouldShareCausticOnly = !hasExtendedChain;
    mPrev.receiverShare = receiverShare;
    mPrev.claimedReceiver = shareBehind;
    mPrev.claimedReceiverPoint = pointBehind;
    for (size_t b = 0; b < mBeta.size(); b++) mBeta[b] *= f[b] / wpdf;
    if (mBeta.isAnyNonFinite()) {
      end = PathEnd::FAILED;
      break;
    }
    if (!rouletteSurvives(dtree)) {
      end = PathEnd::ROULETTE;
      break;
    }
    if (!isHair)
      MediumStack::Update(mMediumStack, mPath.allocator, &material,
                          hit.instance, wo, wNext);
    ray = Ray{hit.point, wNext, EPS, INF, mPath.time.fraction};
  }
  // The bounce count of the deepest contribution the path could have
  // made: at the bound the walk folded one more vertex's arrival in
  // and stopped short of its gather.
  if (mPath.stats)
    mPath.stats->recordPath(end == PathEnd::BOUND ? mDepth - 2 : mDepth - 1,
                            end);
  return mL;
}

void PathWalkDeleter::operator()(PathWalk *walk) const noexcept { delete walk; }

std::unique_ptr<PathWalk, PathWalkDeleter>
makePathWalk(const RenderContext &render, PathContext &path) {
  return std::unique_ptr<PathWalk, PathWalkDeleter>(new PathWalk(render, path));
}

Color tracePath(PathWalk &walk, const CameraSample &camera) {
  return walk.trace(camera);
}
