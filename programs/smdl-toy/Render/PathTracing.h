#pragma once

#include <optional>

#include "Render/Manifold.h"
#include "Render/Medium.h"
#include "Render/Sampler.h"

class LightSampler;
class MNEECasterSet;

struct CameraSample;
struct GuideRecord;
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
  [[nodiscard]] bool isEnvTarget(const float3 &wi) const noexcept {
    return !sunOnly || dot(wi, sunDirection) >= cosSunRadius;
  }

  /// Does the manifold estimator run at all? The MNEE coverage only
  /// arms when it does.
  [[nodiscard]] bool isEnabled() const noexcept { return depth > 0; }
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

/// Everything a render fixes for every path it traces: the scene the
/// walk casts against, the lights it gathers from, the exterior it
/// starts in, and the bounds and estimators the command line chose.
///
/// Built once per render and shared by every worker thread, so nothing
/// reachable through it may be written while paths are in flight.
struct RenderContext final {
  smdl::Compiler &compiler;

  const Scene &scene;

  /// The lights direct lighting is gathered from, and what the walk's
  /// own arrivals at emitters and the environment are weighed against.
  const LightSampler &lights;

  /// Which manifold estimators run and how they discover their
  /// connections; see `MNEEOptions`.
  const MNEEOptions &mneeOptions;

  /// How long a walk may run and what one contribution may add to the
  /// estimate; see `PathOptions`.
  const PathOptions &pathOptions;

  /// The scene-wide exterior atmosphere, or null. It is the medium of
  /// every segment a walk spends outside all geometry, so it is
  /// mutually exclusive with `exteriorMedium`, which occupies the same
  /// place with a material behind it.
  const smdl::Haze *haze{};

  /// The bottom of the nested-medium stack every walk starts inside,
  /// null for vacuum: typically a scene-wide fog or atmosphere named by
  /// the composition's `medium` directive, whose `MediumStack` entry
  /// the caller owns for the whole render.
  const MediumStack *exteriorMedium{};
};

/// What one camera path is traced with, what it works in, and what it
/// leaves behind.
///
/// Its lifetime is one path: the states and medium stacks it holds are
/// built in `allocator`, which the caller resets between samples, and
/// the medium view below resolves those stacks by address.
struct PathContext final {
  /// Where the path builds its shading states and nested-medium stacks.
  smdl::BumpPtrAllocator &allocator;

  /// The sampler, positioned at the pixel and sample index this path is
  /// for.
  Sampler &sampler;

  /// The medium of the segment in flight, shared by the path and every
  /// visibility walk it spawns so that a segment inside the medium the
  /// path is already in resolves nothing; see `Medium::reset()`. Every
  /// walk overwrites it, so no caller may expect it to survive one.
  ///
  /// Borrowed rather than owned because it holds the component storage a
  /// resolution fills, which is worth buying once for a block of pixels
  /// instead of once per sample. Reuse across paths is safe because
  /// `PathWalk` sets the haze at the head of every path, which
  /// invalidates whatever the last one resolved; nothing may rely on the
  /// resolution surviving, since the stacks it keyed on are gone.
  Medium &medium;

  /// The wavelengths the path estimates at, which is this sample's own
  /// grid where the render jitters them.
  const Color &wavelengths;

  /// The path's time: its seconds reach every material, light and
  /// medium evaluation along it as `State::animation_time`, and its
  /// shutter fraction every ray the path and its gathers trace.
  PathTime time;

  /// The SD-tree the walk steers by and the pixel estimate that drives
  /// its Russian roulette, or null for plain path tracing; a null
  /// `Guiding::tree` behaves the same way.
  const Guiding *guiding{};

  /// Where to retain the walk's training data for `trainGuiding()`, or
  /// null to retain none. It must hold `PathOptions::maxBounces + 1`
  /// entries: the walk appends one record per vertex and reports how
  /// many it filled in `numRecords`.
  GuideRecord *records{};

  /// How many of `records` the walk filled in.
  uint64_t numRecords{};

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
  /// conservative choice for a shadow ray. Every caller shades in the
  /// one state, so they all see the wavelengths, allocator and time it
  /// was first built with, which are the path's.
  [[nodiscard]] smdl::State &shadeHit(const Hit &hit, const float3 &wState) {
    if (!hitState)
      hitState.emplace(makeRenderState(wavelengths, &allocator, time.seconds));
    hit.applyGeometryToState(*hitState, wState);
    return *hitState;
  }
};

/// Trace the camera path `camera` starts and return its radiance
/// estimate.
///
/// Direct lighting is gathered at every scattering vertex as the walk
/// reaches it, so nothing is retained per vertex. Each vertex pairs
/// light sampling with the walk's own continuation as the BSDF-sampling
/// half of the MIS estimate: an emitter hit or an environment escape
/// contributes MIS-weighted against what light sampling at the previous
/// vertex would have produced, and the camera segment, which no light
/// sampling competes with, contributes at weight 1.
///
/// With `RenderContext::mneeOptions` enabled, a light gather whose straight
/// shadow segment is blocked by up to `MNEEOptions::depth` smooth
/// refractive interfaces connects through them by manifold next-event
/// estimation instead of reading as occluded: toward the sun and sky,
/// toward punctual lights (whose through-interface transport no other
/// estimator can reach at all), and toward area lights. The walk's own
/// arrivals at lights through such chains, environment escapes and
/// emitter hits alike, are weighed against the gather by re-walk MIS:
/// the arrival keeps its full weight exactly where the gather cannot
/// produce the transport (a chain family or fold solution the walk does
/// not reach, a failed walk, a light the sampler never draws), so the
/// combined estimator is unbiased rather than exclusive.
///
/// With `Guiding::tree`, non-Dirac surface bounces one-sample-MIS the
/// SD-tree against the BSDF and Russian roulette becomes adjoint-driven;
/// without one, direction sampling and roulette are plain path tracing's.
[[nodiscard]]
Color tracePath(const RenderContext &render, PathContext &path,
                const CameraSample &camera);
