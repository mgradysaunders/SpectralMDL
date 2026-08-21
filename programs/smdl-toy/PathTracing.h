#pragma once

#include "Manifold.h"
#include "Medium.h"

class LightSampler;

struct Guiding;

/// Which manifold estimators run for a render, and how they discover
/// their connections.
class ManifoldOptions final {
public:
  /// The most refractive interfaces a connection may cross, 0 when
  /// manifold next-event estimation is off.
  int depth{};

  /// Solve through glossy transmissive interfaces as well as Dirac ones,
  /// by drawing a half vector from each interface's normal distribution
  /// and constraining the crossing to it.
  bool glossy{};

  /// Does the manifold estimator run at all? The cancelation state only
  /// arms when it does.
  [[nodiscard]] bool any() const noexcept { return depth > 0; }
};

struct GuideRecord;

/// A visibility segment walk from `point0` toward `point1`: attenuates
/// medium transmittance into `beta` over the spans it covers, passes
/// through cutout hits and null interfaces with the nested-medium stack
/// kept current, and stops on the first surface that blocks under
/// cutout semantics, leaving what to make of that surface to the
/// caller. Plain shadow rays treat it as the occluder.
class SegmentWalk final {
public:
  SegmentWalk(const Scene &scene, Sampler &sampler, const Color &wavelengths,
              smdl::BumpPtrAllocator &allocator, const MediumStack *medium,
              const float3 &point0, const float3 &point1, Color &beta);

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

  /// Did the walk pass through a cutout so far? The manifold gather
  /// declines such segments, so that its coverage stays the exact
  /// complement of the deterministic re-walk the arrival-side MIS
  /// runs.
  [[nodiscard]] bool passedCutout() const noexcept { return mPassedCutout; }

private:
  const Scene &mScene;
  Sampler &mSampler;
  const Color &mWavelengths;
  smdl::BumpPtrAllocator &mAllocator;

  /// The nested-medium stack as of the walk's current position, a
  /// walk-local view that evolves across the boundaries it passes
  /// through without touching the caller's stack.
  const MediumStack *mMedium{};

  Color &mBeta;

  /// The world-space segment length.
  float mDistance{};

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
};

/// Trace a camera path and return its radiance estimate.
///
/// The path starts on `ray`, whose direction must be normalized, carrying
/// `cameraWeight` as the initial throughput and `cameraConeAngle` as the
/// per-pixel ray cone spread (zero switches the LOD cone off end to end).
/// Direct lighting is gathered at every scattering vertex as the walk
/// reaches it, so nothing is retained per vertex; `maxDepth` bounds only
/// the walk itself, and Russian roulette terminates paths long before it.
///
/// Each vertex pairs light sampling with the walk's own continuation as
/// the BSDF-sampling half of the MIS estimate: an emitter hit or an
/// environment escape contributes MIS-weighted against what light
/// sampling at the previous vertex would have produced, and the camera
/// segment, which no light sampling competes with, contributes at
/// weight 1.
///
/// The walk starts inside `exteriorMedium`, which may be null for
/// vacuum: this is the bottom of the nested-medium stack, typically a
/// scene-wide fog or atmosphere named by the composition's `medium`
/// directive, whose `MediumStack` entry the caller owns for the whole
/// render.
///
/// The `guiding` may be null or have a null tree, in which case direction
/// sampling and Russian roulette behave as plain path tracing; with a
/// tree, non-delta surface bounces one-sample-MIS the SD-tree against the
/// BSDF and roulette becomes adjoint-driven.
///
/// `manifold` decides which manifold estimators run; see
/// `ManifoldOptions`. With `depth > 0`, a light gather whose straight
/// shadow segment is blocked by up to that many smooth refractive
/// interfaces
/// connects through them by manifold next-event estimation instead of
/// reading as occluded: toward the sun and sky, toward punctual lights
/// (whose through-interface transport no other estimator can reach at
/// all), and toward area lights. The walk's own arrivals at lights
/// through such chains, environment escapes and emitter hits alike,
/// are weighed against the gather by re-walk MIS: the arrival keeps
/// its full weight exactly where the gather cannot produce the
/// transport (a chain family or fold solution the walk does not
/// reach, a failed walk, a light the sampler never draws), so the
/// combined estimator is unbiased rather than exclusive.
///
/// If `records` is non-null it must hold `maxDepth` entries: the walk
/// appends one `GuideRecord` per vertex, returns the count in
/// `numRecords`, and the completed buffer feeds `trainGuiding()`. A null
/// `records` retains nothing.
[[nodiscard]] Color tracePath(smdl::Compiler &compiler, const Scene &scene,
                              Sampler &sampler, const Color &wavelengths,
                              smdl::BumpPtrAllocator &allocator, Ray ray,
                              float cameraWeight, float cameraConeAngle,
                              const MediumStack *exteriorMedium,
                              uint64_t maxDepth, const LightSampler &lights,
                              const Guiding *guiding,
                              const ManifoldOptions &manifold,
                              GuideRecord *records, uint64_t &numRecords);
