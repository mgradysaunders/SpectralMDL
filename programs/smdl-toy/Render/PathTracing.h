#pragma once

#include <memory>

#include "Render/Manifold.h"
#include "Render/Medium.h"
#include "Render/Sampler.h"

namespace smdl {
class SkyBasis;
} // namespace smdl

class LightSampler;
class MNEECasterSet;
class PathStats;

struct LightSample;

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

  /// The marked casters the searched gathers sample, for a reflection
  /// off one and for a refraction through one, in place of the straight
  /// shadow segment the straight-line gather is handed; built once per
  /// render, null or empty when the layout marks none. What each marked
  /// instance claims, by domain, is `manifoldClaim()`.
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
  bool isSunOnly{};

  /// With `isSunOnly`, the unit direction toward the sun-disk center and
  /// the cosine of its angular radius.
  float3 sunDirection{};
  float cosSunRadius{1.0f};

  /// Does the Dirac-chain machinery treat this environment target
  /// direction as one of its own?
  [[nodiscard]] bool isEnvTarget(const float3 &wi) const noexcept {
    return !isSunOnly || dot(wi, sunDirection) >= cosSunRadius;
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
  /// mutually exclusive with `exteriorMediumDef`, which occupies the
  /// same place with a material behind it.
  const smdl::Haze *haze{};

  /// The material of the medium every walk starts inside, null for
  /// vacuum: typically a scene-wide fog named by the composition's
  /// `medium` directive. A walk evaluates it at its head, at its own
  /// wavelengths and time, into the path allocator, so that a
  /// coefficient resampled onto the wavelength grid is exact under
  /// `-wavelength-jitter`; see `MaterialDef::hasHomogeneousCoefficients()`.
  const smdl::JIT::MaterialDef *exteriorMediumDef{};
};

/// What the camera paths of a block of pixels are traced with, what
/// they work in, and what each leaves behind.
///
/// Its lifetime is a block: everything here but `time` and `numRecords`
/// is the block's, and `makePathWalk()` binds a walker to it for the
/// block. The states and medium stacks a path builds live in
/// `allocator`, which the caller resets between samples, and the medium
/// view below resolves those stacks by address.
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
  /// instead of once per sample, and because the resolution itself is
  /// worth keeping from one path to the next when both cross the same
  /// medium. `PathWalk` calls `Medium::beginPath()` at the head of every
  /// path, so the last path's stacks, gone with its allocator, are never
  /// taken for this one's.
  Medium &medium;

  /// The sun-sky resolved onto `wavelengths`, which every environment
  /// evaluation this path makes reads. Borrowed rather than owned for
  /// the reason `medium` is: it is worth resolving once per sample
  /// instead of once per evaluation, and a render that does not jitter
  /// resolves it once for the whole frame.
  const smdl::SkyBasis &skyBasis;

  /// The four shading states the path works in, borrowed for the reason
  /// `medium` is: everything in one but the animation time is a property
  /// of the block rather than the path, and every field a vertex varies
  /// is overwritten at the vertex, so building them per path is half a
  /// kilobyte of copy each for two fields' worth of difference. The
  /// caller sets `animationTime` on all four at the head of the path.
  ///
  /// \{

  /// The pristine gather-side state, which nothing writes geometry into.
  smdl::State &gatherState;

  /// The walk's own vertex state, which carries the level-of-detail
  /// fields the walk tracks along the path.
  smdl::State &walkState;

  /// The state `shadeHit()` shades in, which deliberately carries no
  /// level-of-detail so that opacity evaluates at full fidelity.
  smdl::State &shadeState;

  /// The state a light sample's emitter is evaluated in, which likewise
  /// carries no level-of-detail: `LightSampler::sample()` applies the
  /// light point's geometry to it over whatever the last sample left.
  smdl::State &lightState;

  /// The light sample and the blocker a gather works in, borrowed for
  /// the reason the states above are: between them they are six hundred
  /// bytes of default member initializers, and a gather runs at every
  /// vertex. Neither carries anything into a gather.
  /// `LightSampler::sample()` establishes the whole sample it returns
  /// true for, and `VisibilityWalk::nextBlocker()` resets the blocker
  /// before it looks at one.
  ///
  /// \{

  LightSample &gatherSample;

  Hit &gatherBlocker;

  /// \}

  /// \}

  /// The wavelengths the path estimates at, which is this sample's own
  /// grid where the render jitters them.
  const Color &wavelengths;

  /// The path's time, which the caller sets at the head of each path:
  /// its seconds reach every material, light and medium evaluation
  /// along it as `State::animationTime`, and its shutter fraction
  /// every ray the path and its gathers trace.
  PathTime time;

  /// The path's hero wavelength in nanometers, which the caller sets at
  /// the head of each path beside the time: the wavelength a lens whose
  /// glasses disperse was traced at, which reaches every evaluation along
  /// the path as `State::wavelengthHero` so that a material refracts at
  /// the index the picture is being formed at. The d line where nothing
  /// drew one.
  float wavelengthHero;

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

  /// Where to tally what the walk adds and where it ends, or null to
  /// tally nothing; see `PathStats`.
  PathStats *stats{};

  /// The shading state of `hit`, reached along the direction of
  /// propagation `wState`, which is the shared state with this hit's
  /// geometry applied over the last one's; see
  /// `Hit::applyGeometryToState()`.
  ///
  /// Nothing here writes the state's level-of-detail fields or `rng`,
  /// which is what lets opacity evaluate at full fidelity, the
  /// conservative choice for a shadow ray. Every caller shades in the
  /// one state, so they all see the wavelengths, allocator and time it
  /// carries, which are the path's.
  [[nodiscard]] smdl::State &shadeHit(const Hit &hit, const float3 &wState) {
    hit.applyGeometryToState(shadeState, wState);
    return shadeState;
  }
};

/// The walker of a block's paths: what `tracePath()` carries along a
/// path, opaque to the caller. Built once per block by `makePathWalk()`
/// rather than per path, because building it is over a kilobyte of
/// default member initializers, most of them the manifold coverage's
/// chain of hits, and no path reads what the last one left.
class PathWalk;

struct PathWalkDeleter final {
  void operator()(PathWalk *walk) const noexcept;
};

/// Make the walker of `path`'s block, bound to `render` and `path` for
/// as long as it lives.
[[nodiscard]] std::unique_ptr<PathWalk, PathWalkDeleter>
makePathWalk(const RenderContext &render, PathContext &path);

/// Trace the camera path `camera` starts with `walk` and return its
/// radiance estimate.
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
[[nodiscard]] Color tracePath(PathWalk &walk, const CameraSample &camera);
