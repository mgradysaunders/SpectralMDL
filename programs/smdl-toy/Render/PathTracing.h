#pragma once

#include <optional>

#include "Render/Manifold.h"
#include "Render/Medium.h"
#include "Render/Sampler.h"

class LightSampler;
class MNEECasterSet;

struct Guiding;

/// Which manifold estimators run for a render, and how they discover
/// their connections.
class MNEEOptions final {
public:
  /// The most refractive interfaces a connection may cross, 0 when
  /// manifold next-event estimation is off.
  int depth{};

  /// The marked casters a reflective gather searches in place of the
  /// straight shadow segment, built once per render; null or empty when
  /// the layout marks none. What each marked instance claims, by domain,
  /// is `manifoldClaim()`.
  const MNEECasterSet *casters{};

  /// How many fresh starts a reciprocal estimate may draw before dropping
  /// the sample; see `MANIFOLD_MAX_TRIALS`.
  int maxTrials{MANIFOLD_MAX_TRIALS};

  /// When positive, the biased claimed mode. The randomly seeded
  /// estimators replace the reciprocal estimate: run exactly this many
  /// walks per estimate, cluster the converged solutions by
  /// `isSameManifoldSolution()`, and sum each distinct one once, with
  /// no reciprocal weight, trading the reciprocal estimate's
  /// heavy-tailed weights for a hard per-sample cost bound. The Dirac
  /// refraction gather runs the same multi-seed clustered estimate
  /// (first walk from the straight seed, the rest jittered) and claims
  /// its transport exclusively: an arrival through a covered Dirac
  /// chain at a target the sampler can draw is dropped outright instead
  /// of weighed by re-walk MIS, which deletes the weight-1 firefly tail
  /// along with its re-walks. Either way the estimate darkens by
  /// whatever the walks miss and approaches the unbiased answer from
  /// below as the count grows (the reference's figure 15).
  int biasedTrials{};

  /// When positive, hand glossy lobes wider than this squared roughness
  /// to ordinary sampling instead of claiming them, identically on the
  /// gather and arrival sides; see `manifoldClaim()`. The stopgap for a
  /// light-extent-aware partition: wide lobes under small lights are
  /// where the claimed estimator loses to ordinary sampling, and until
  /// the split reads the light's angular size this is the one knob.
  float maxRoughness{};

  /// The squared roughness a glossy-only vertex needs to be a receiver
  /// the gathers run from and claim for; see `isManifoldReceiver()`.
  float minReceiverAlpha{0.005f};

  /// Restrict the Dirac-chain machinery to the environment's sun cone:
  /// the deterministic refractive gather runs for an environment sample
  /// only when it aims inside the sun disk, and an environment arrival
  /// outside it keeps its ordinary weight instead of re-walk MIS. The
  /// same predicate on both sides keeps the MIS pair summing to one, so
  /// this is a cost partition, not a bias: refracted sky is smooth
  /// transport BSDF sampling resolves at weight 1 anyway. Glossy chains
  /// stay ungated, since their claimed share is dropped at arrivals
  /// toward every light regardless of target.
  bool sunOnly{};

  /// With `sunOnly`, the unit direction toward the sun-disk center and
  /// the cosine of its angular radius.
  float3 sunDirection{};
  float cosSunRadius{1.0f};

  /// Does the Dirac-chain machinery treat this environment target
  /// direction as one of its own?
  [[nodiscard]] bool envTarget(const float3 &wi) const noexcept {
    return !sunOnly || dot(wi, sunDirection) >= cosSunRadius;
  }

  /// Does the manifold estimator run at all? The MNEE coverage only
  /// arms when it does.
  [[nodiscard]] bool any() const noexcept { return depth > 0; }
};

/// The bounds on the walk: how many scattering events a path may
/// undergo, and how much any single contribution may add to the
/// estimate.
///
/// A bounce is a scattering event the contribution passed through: an
/// emitter or the environment seen straight from the camera has 0,
/// light sampling at the first vertex and an emitter found after one
/// scattering both have 1, and so on. A manifold gather's chain
/// contribution counts as the receiver's bounce, however many
/// interfaces the chain crosses.
class PathOptions final {
public:
  /// The most bounces a path may undergo. After this many the walk
  /// casts one more segment for the emission it lands on and stops
  /// before gathering, so the estimate holds every contribution of at
  /// most this many bounces and none deeper.
  uint64_t maxBounces{63};

  /// Terminate by Russian roulette past the first few bounces, which
  /// leaves `maxBounces` as a backstop roulette reaches only in
  /// high-albedo transport. Off, every path runs to `maxBounces` and the
  /// estimate is the fixed-depth truncation.
  bool useRoulette{true};

  /// The largest value any band of one contribution may add, 0 when
  /// unbounded. The standard biased firefly control: a contribution
  /// whose largest band exceeds this is scaled down to it uniformly
  /// across bands, so the spectrum keeps its shape. The bias is
  /// deliberate: the rare-event tail is traded for a bounded per-sample
  /// brightness, darkening the estimate by whatever the tail carried.
  float maxContribution{};

  /// The least bounces a contribution must have for `maxContribution`
  /// to apply, at least 1: directly visible lights and environment
  /// escapes stay exact, and 2 also exempts single-bounce glints and
  /// direct lighting on the first vertex.
  int maxContributionBounces{1};
};

struct GuideRecord;

/// The scratch one path and every visibility walk it spawns share, so
/// that neither the medium a segment travels in nor the state a hit
/// shades in is built from nothing at every use.
///
/// Its lifetime is one path, because the medium view resolves stacks by
/// address and the allocator that owns them is reset between samples.
struct PathScratch final {
  /// The medium of the segment in flight; see `Medium::reset()`.
  Medium medium{};

  /// The state `shadeHit()` shades in, empty until the first hit that
  /// needs it, so that a path which shades none, which is every path in
  /// a scene with `Scene::opaqueShadows`, builds none.
  std::optional<smdl::State> hitState{};

  /// The shading state of `hit`, reached along the direction of
  /// propagation `wState`, which is the shared state with this hit's
  /// geometry applied over the last one's; see
  /// `Hit::applyGeometryToState()`.
  ///
  /// Nothing here writes the state's level-of-detail fields or `rng`,
  /// which is what lets opacity evaluate at full fidelity, the
  /// conservative choice for a shadow ray. The state keeps the
  /// wavelengths, allocator and time of the first call, so the callers
  /// sharing it must agree on those, which they do: they are the
  /// path's.
  [[nodiscard]] smdl::State &shadeHit(const Hit &hit, const float3 &wState,
                                      const Color &wavelengths,
                                      smdl::BumpPtrAllocator &allocator,
                                      float time) {
    if (!hitState)
      hitState.emplace(makeRenderState(wavelengths, &allocator, time));
    hit.applyGeometryToState(*hitState, wState);
    return *hitState;
  }
};

// TODO Remove `mNeedBlocker` -> instead update method signature to
// `bool nextBlocker(Hit *hit = {});` so that `nextBlocker()` or 
// `nextBlocker(nullptr)` behaves as if `mNeedBlocker=false` and
// `nextBlocker(&hit)` behaves as if `mNeedBlocker=true`.

/// A visibility segment walk from `point0` toward `point1`: attenuates
/// medium transmittance into `beta` over the spans it covers, passes
/// through cutout hits and null interfaces with the nested-medium stack
/// kept current, and stops on the first surface that blocks under
/// cutout semantics, leaving what to make of that surface to the
/// caller. Plain shadow rays treat it as the occluder.
class VisibilityWalk final {
public:
  /// `needBlocker` promises the caller reads the blocker `nextBlocker()`
  /// returns, which is what the manifold refraction gather does to
  /// discover chains; it keeps the walk on the closest-hit path in
  /// scenes whose `Scene::opaqueShadows` would otherwise answer the walk
  /// as a boolean occlusion query and return no blocker at all.
  /// `scratch` belongs to the path the walk hangs off, so that a segment
  /// inside the medium the path is already in resolves nothing; see
  /// `PathScratch`. The walk overwrites both of its members, so a caller
  /// must not expect either to survive the walk.
  VisibilityWalk(smdl::BumpPtrAllocator &allocator, const Scene &scene,
                 Sampler &sampler, const Color &wavelengths, PathTime time,
                 const MediumStack *medium, PathScratch &scratch,
                 const float3 &point0, const float3 &point1, Color &beta,
                 bool needBlocker = false, bool infiniteTarget = false);

  /// Advance to the next blocking surface. Returns true with `hit`
  /// filled in; returns false when the walk finished without one,
  /// either because the segment reached `point1` or because `beta` was
  /// fully absorbed along the way (the caller distinguishes by looking
  /// at `beta`).
  [[nodiscard]] bool nextBlocker(Hit &hit);

  /// The nested-medium stack as of the walk's current position, e.g.,
  /// at the blocker just returned.
  [[nodiscard]] const MediumStack *medium() const noexcept { return mMedium; }

  /// Pass through the blocker `nextBlocker()` just returned: update the
  /// nested-medium stack across it with the given instance and continue
  /// the walk on the far side, exactly as the walk passes its own
  /// cutout hits.
  void passThrough(const smdl::JIT::MaterialInstance &mat, const Hit &hit);

  // TODO Is there any way to get MNEE to work with cutouts? Or at least 
  //      to get MNEE to work with deterministic cutouts, i.e., leaves where
  //      the mask is mostly exactly 0 or exactly 1 so we can know the
  //      re-walk will be deterministic?

  /// Did the walk pass through a cutout so far? The manifold gather
  /// declines such segments, so that its coverage stays the exact
  /// complement of the deterministic re-walk the arrival-side MIS
  /// runs.
  [[nodiscard]] bool passedCutout() const noexcept { return mPassedCutout; }

private:
  smdl::BumpPtrAllocator &mAllocator;
  const Scene &mScene;
  Sampler &mSampler;
  const Color &mWavelengths;
  PathTime mTime;

  /// The nested-medium stack as of the walk's current position, a
  /// walk-local view that evolves across the boundaries it passes
  /// through without touching the caller's stack.
  const MediumStack *mMedium{};

  /// The path's scratch, whose medium view the walk retargets at every
  /// segment and whose state it shades every surface it passes through
  /// in; see the constructor.
  PathScratch &mScratch;

  Color &mBeta;

  /// The world-space segment length.
  float mDist{};

  /// The normalized segment direction, or zero when the endpoints
  /// coincide, honoring the zero-means-off `State` convention.
  float3 mShadowDir{};

  /// The self-intersection offset in the segment's unit
  /// parameterization. Offsets are parametric, so for segments longer
  /// than one scene unit they are rescaled to stay near `EPS` in WORLD
  /// units: a sun shadow ray spans the whole scene, and an offset
  /// scaled by that length is wide enough to skip real geometry, and a
  /// boundary crossing inside the skipped sliver desyncs the medium
  /// stack for the entire segment.
  float mParamEps{};

  /// The current cast over the segment's unit parameterization.
  Ray mRay{};

  /// The parameter up to which the medium has been integrated, tracked
  /// separately from `mRay.tmin` deliberately: integrating only
  /// `[tmin, tmax]` of each cast would skip a scene-scaled sliver of
  /// medium at every pass-through restart, and where such a gap crosses
  /// dense medium the skipped optical depth reads as a bright seam in
  /// the shadow.
  float mTCovered{};

  /// See `passedCutout()`.
  bool mPassedCutout{};

  /// See the constructor.
  bool mNeedBlocker{};

  /// Does the segment end where it does only because a light infinitely
  /// far away needs a finite point to aim at? See `Medium::attenuate()`.
  bool mInfiniteTarget{};
};

/// Trace a camera path and return its radiance estimate.
///
/// The path starts on `ray`, whose direction must be normalized, carrying
/// `cameraWeight` as the initial throughput and `cameraConeAngle` as the
/// per-pixel ray cone spread (zero switches the LOD cone off end to end).
/// Direct lighting is gathered at every scattering vertex as the walk
/// reaches it, so nothing is retained per vertex.
///
/// `time` is the whole path's: its seconds reach every material, light,
/// and medium evaluation along it as `State::animation_time`, and its
/// shutter fraction every ray the path and its gathers trace.
///
/// Each vertex pairs light sampling with the walk's own continuation as
/// the BSDF-sampling half of the MIS estimate: an emitter hit or an
/// environment escape contributes MIS-weighted against what light
/// sampling at the previous vertex would have produced, and the camera
/// segment, which no light sampling competes with, contributes at
/// weight 1.
///
/// The `haze` is the scene-wide exterior atmosphere, or null. It is the
/// medium of every segment the walk spends outside all geometry, so it
/// is mutually exclusive with an `exteriorMedium`, which occupies the
/// same place with a material behind it.
///
/// The walk starts inside `exteriorMedium`, which may be null for
/// vacuum: this is the bottom of the nested-medium stack, typically a
/// scene-wide fog or atmosphere named by the composition's `medium`
/// directive, whose `MediumStack` entry the caller owns for the whole
/// render.
///
/// `mneeOptions` decides which manifold estimators run; see
/// `MNEEOptions`. With `depth > 0`, a light gather whose straight
/// shadow segment is blocked by up to that many smooth refractive
/// interfaces connects through them by manifold next-event estimation
/// instead of reading as occluded: toward the sun and sky, toward
/// punctual lights (whose through-interface transport no other
/// estimator can reach at all), and toward area lights. The walk's own
/// arrivals at lights through such chains, environment escapes and
/// emitter hits alike, are weighed against the gather by re-walk MIS:
/// the arrival keeps its full weight exactly where the gather cannot
/// produce the transport (a chain family or fold solution the walk
/// does not reach, a failed walk, a light the sampler never draws), so
/// the combined estimator is unbiased rather than exclusive.
///
/// `pathOptions` bounds the walk's length and what any single
/// contribution may add to the estimate; see `PathOptions`. The medium's
/// own emission is never bounded: it is bounded transport with no
/// rare-event tail.
///
/// The `guiding` may be null or have a null tree, in which case direction
/// sampling and Russian roulette behave as plain path tracing; with a
/// tree, non-Dirac surface bounces one-sample-MIS the SD-tree against the
/// BSDF and roulette becomes adjoint-driven.
///
/// If `records` is non-null it must hold `pathOptions.maxBounces + 1`
/// entries: the walk appends one `GuideRecord` per vertex, returns the
/// count in `numRecords`, and the completed buffer feeds
/// `trainGuiding()`. A null `records` retains nothing.
[[nodiscard]]
Color tracePath(smdl::Compiler &compiler, smdl::BumpPtrAllocator &allocator,
                const Scene &scene, Sampler &sampler, const Color &wavelengths,
                Ray ray, PathTime time, float cameraWeight,
                float cameraConeAngle, const MediumStack *exteriorMedium,
                const smdl::Haze *haze, const LightSampler &lightSampler,
                const MNEEOptions &mneeOptions, const PathOptions &pathOptions,
                const Guiding *guiding, GuideRecord *records,
                uint64_t &numRecords);
