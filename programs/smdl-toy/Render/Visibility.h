/// \file
/// The visibility walk: the one implementation of what a shadow-type
/// segment passes through and what blocks it. Plain shadow rays, the
/// straight-line discovery both halves of the manifold estimator run,
/// the caster seed trace, and the sub-segments of a converged
/// connection all walk through here, so a null interface, a cutout, and
/// the nested-medium bookkeeping across them mean one thing everywhere.
#pragma once

#include "Render/Context.h"

/// A visibility segment walk from `point0` toward `point1`: attenuates
/// medium transmittance into `beta` over the spans it covers, passes
/// through cutout hits and null interfaces with the nested-medium stack
/// kept current, and stops on the first surface that blocks under cutout
/// semantics, leaving what to make of that surface to the caller. Plain
/// shadow rays treat it as the occluder.
class VisibilityWalk final {
public:
  /// `mediumStack` is the nested medium the segment starts in. The walk
  /// works in the path's own scratch, so that a segment inside the medium
  /// the path is already in resolves nothing: it overwrites `path.medium`
  /// and shades every surface it passes through in `path.shadeHit()`, so
  /// a caller must not expect either to survive the walk.
  ///
  /// `beta` is where the transmittance goes, or null for a walk that
  /// wants the hits alone: no medium is resolved and nothing is drawn
  /// for one, which is what the arrival-side re-walk and the seed traces
  /// want, since a MIS weight and a start owe nothing to attenuation.
  ///
  /// `isInfiniteTarget` says `point1` is the finite point an infinitely
  /// far light needs to aim at rather than where the segment ends: the
  /// casts run to infinity along the line, so a surface past the aim
  /// point blocks as it blocks the walk's own escapes, while the
  /// medium is integrated to the aim point, the bounded stand-in for
  /// the far end that `Medium::attenuate()` documents.
  VisibilityWalk(const RenderContext &render, PathContext &path,
                 const MediumStack *mediumStack, const float3 &point0,
                 const float3 &point1, Color *beta,
                 bool isInfiniteTarget = false);

  /// Advance to the next blocking surface. Returns true, with `hit`
  /// filled in when the caller asked for one; returns false when the walk
  /// finished without a blocker, either because the segment reached
  /// `point1` or because `beta` was fully absorbed along the way (the
  /// caller distinguishes by looking at `beta`).
  ///
  /// Passing somewhere to put the blocker promises the caller reads it,
  /// which is what the chain discoveries do; it keeps the walk on the
  /// closest-hit path in scenes whose `Scene::useOpaqueShadows` would
  /// otherwise answer the walk as a boolean occlusion query and return
  /// no blocker at all.
  [[nodiscard]] bool nextBlocker(Hit *hit = {});

  /// The nested-medium stack as of the walk's current position, e.g., at
  /// the blocker just returned.
  [[nodiscard]] const MediumStack *mediumStack() const noexcept {
    return mMediumStack;
  }

  /// Pass through the blocker `nextBlocker()` just returned: update the
  /// nested-medium stack across it with the given instance and continue
  /// the walk on the far side, exactly as the walk passes its own cutout
  /// hits.
  void passThrough(const smdl::JIT::Material *material, const Hit &hit);

  /// Did the walk pass through a cutout on a draw of its own, i.e. through
  /// a hit whose opacity is strictly between 0 and 1? The straight-line
  /// discovery declines such segments, so that the gather's coverage
  /// stays the exact complement of the deterministic re-walk the
  /// arrival-side MIS runs. A hit whose opacity is exactly 0 or exactly 1
  /// does not count: there is no coin to replay, so the re-walk resolves
  /// it the same way every time, which is what keeps a silhouette mask
  /// (a leaf, a fence) from disabling the estimators wherever it covers
  /// the light.
  [[nodiscard]] bool hasPassedStochasticCutout() const noexcept {
    return mHasPassedStochasticCutout;
  }

  /// The surface hits resolved along the segment so far: cutout and
  /// null-interface hops, and the blockers the caller passed through.
  /// The straight-line discovery declines a segment that needs more of
  /// them than its budget; see `MNEE_STRAIGHT_MAX_HOPS`.
  [[nodiscard]] int hopCount() const noexcept { return mHopCount; }

private:
  /// Continue the walk on the far side of the hit it is at.
  void stepPast();

  const RenderContext &mRender;
  PathContext &mPath;

  /// The nested-medium stack as of the walk's current position, a
  /// walk-local view that evolves across the boundaries it passes through
  /// without touching the caller's stack.
  const MediumStack *mMediumStack{};

  /// Where the transmittance goes, or null for none; see the constructor.
  Color *mBeta{};

  /// The world-space distance from `point0` to `point1`.
  float mDist{};

  /// The normalized segment direction, or zero when the endpoints
  /// coincide, honoring the zero-means-off `State` convention.
  float3 mShadowDir{};

  /// The self-intersection offset in the segment's unit parameterization.
  /// Offsets are parametric, so for segments longer than one scene unit
  /// they are rescaled to stay near `EPS` in WORLD units: a sun shadow ray
  /// spans the whole scene, and an offset scaled by that length is wide
  /// enough to skip real geometry, and a boundary crossing inside the
  /// skipped sliver desyncs the medium stack for the entire segment.
  float mParamEps{};

  /// The current cast over the segment's unit parameterization, in which
  /// `point1` is parameter 1 and an infinite target's casts run to
  /// infinity.
  Ray mRay{};

  /// The parameter up to which the medium has been integrated, tracked
  /// separately from `mRay.tmin` deliberately: integrating only
  /// `[tmin, tmax]` of each cast would skip a scene-scaled sliver of
  /// medium at every pass-through restart, and where such a gap crosses
  /// dense medium the skipped optical depth reads as a bright seam in the
  /// shadow.
  float mTCovered{};

  /// See `hopCount()`.
  int mHopCount{};

  /// See `hasPassedStochasticCutout()`.
  bool mHasPassedStochasticCutout{};

  /// See the constructor.
  bool mIsInfiniteTarget{};
};

/// Is the segment from `point0` to `point1` clear, and what does it
/// transmit? A plain shadow ray: the walk above with its blocker taken
/// for the occluder, true when nothing blocks and `beta` survives.
[[nodiscard]] bool testVisibility(const RenderContext &render,
                                  PathContext &path,
                                  const MediumStack *mediumStack,
                                  const float3 &point0, const float3 &point1,
                                  Color &beta, bool isInfiniteTarget = false);
