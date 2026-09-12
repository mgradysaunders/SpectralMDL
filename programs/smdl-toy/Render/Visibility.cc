#include "Render/Visibility.h"

#include <algorithm>
#include <optional>

VisibilityWalk::VisibilityWalk(const RenderContext &render, PathContext &path,
                               const MediumStack *mediumStack,
                               const float3 &point0, const float3 &point1,
                               Color *beta, bool isInfiniteTarget)
    : mRender(render), mPath(path), mMediumStack(mediumStack), mBeta(beta),
      mIsInfiniteTarget(isInfiniteTarget) {
  mDist = length(point1 - point0);
  mShadowDir = mDist > 0 ? (point1 - point0) / mDist : float3{};
  mParamEps = mDist > 1.0f ? EPS / mDist : EPS;
  mRay = Ray{point0, point1 - point0, mParamEps,
             isInfiniteTarget ? INF : 1.0f - mParamEps, path.time.fraction};
}

bool VisibilityWalk::nextBlocker(Hit *hit) {
  // The medium is integrated to the aim point at most, which is the
  // segment's end for a finite target and the bounded stand-in for
  // infinity otherwise; see the constructor.
  const bool hasMedium{mBeta && (mMediumStack || mPath.medium.hasHaze())};
  // Where every material blocks a shadow ray at its first hit (see
  // `Scene::useOpaqueShadows`), a walk whose caller wants no blocker is a
  // pure boolean, which Embree answers cheaper than a closest hit:
  // occlusion early-outs on any hit and skips the hit reconstruction.
  // The medium stack cannot change across such a walk (nothing passes
  // through), so a clear segment attenuates over its whole span in the
  // starting medium, and a blocked one carries nothing: every caller
  // discards `beta` on a blocked outcome, so only the two sampler draws
  // heterogeneous tracking would have made are consumed in its place,
  // keeping the deterministic sample sequence unchanged. The chain
  // discoveries, which read blockers, ask for one and keep the
  // closest-hit path.
  if (mRender.scene.useOpaqueShadows && !hit) {
    const bool isOccluded{mRender.scene.isOccluded(mRay)};
    if (hasMedium) {
      mPath.medium.reset(mMediumStack, mPath.wavelengths, mPath.time,
                         mPath.wavelengthHero, mRay(mTCovered), mShadowDir);
      if (!isOccluded) {
        mPath.medium.attenuate(mPath.sampler,
                               (std::min(mRay.tmax, 1.0f) - mTCovered) * mDist,
                               *mBeta, mIsInfiniteTarget);
      } else if (mPath.medium.attenuationDraws()) {
        (void)mPath.sampler.nextBits();
        (void)mPath.sampler.nextBits();
      }
    }
    return isOccluded;
  }
  // The blocker the walk works in: the caller's where it wants one, its
  // own where it does not. Built only in the second case, because the
  // gather supplies one at every vertex and a plain local would be a
  // hundred and ninety-two bytes of default member initializers that
  // nothing there ever reads. Its own rather than shared, because a
  // caller's blocker outlives the call and the walks it goes on to spawn
  // ask for one of these themselves.
  std::optional<Hit> ownBlocker;
  Hit &found{hit ? *hit : ownBlocker.emplace()};
  RawHit raw;
  while (mRay.tmin < mRay.tmax) {
    // The cast reports what it hit and nothing more; the record is
    // built below only where something reads it. Nothing clears the
    // record first: `Scene::makeHit()` assigns the whole of it, so what
    // the last iteration left standing is never seen.
    bool hasHitSurface{mRender.scene.intersect(mRay, raw)};
    // Attenuate over the span actually traveled, hit or miss
    // (`Scene::intersect` narrows `tmax` to the hit parameter on a
    // hit), and never past the aim point. The parametrization spans
    // `[0, 1]` over the segment, so the world-space span is rescaled and
    // the medium sees a unit direction with distances in scene units.
    // The epsilon slivers the casts exclude are attributed to whichever
    // side of the boundary this iteration integrates. An empty stack
    // with no haze skips the medium view outright, shadow segments in
    // vacuum being the common case.
    if (hasMedium) {
      const float tEnd{std::min(mRay.tmax, 1.0f)};
      mPath.medium.reset(mMediumStack, mPath.wavelengths, mPath.time,
                         mPath.wavelengthHero, mRay(mTCovered), mShadowDir);
      mPath.medium.attenuate(mPath.sampler, (tEnd - mTCovered) * mDist, *mBeta,
                             mIsInfiniteTarget && !hasHitSurface);
      mTCovered = tEnd;
      if (!(mBeta->maxComponent() > 0.0f)) {
        return false; // Fully absorbed already.
      }
    }
    if (!hasHitSurface) {
      return false;
    }
    ++mHopCount;
    const Scene &scene{mRender.scene};
    const MeshInstance &instance{scene.meshInstances[raw.instIndex]};
    const smdl::JIT::MaterialDef *materialDef{
        scene.materialDefs[scene.materialIndexOf(instance)]};
    // A null interface passes shadow rays straight through: no opacity
    // and no blocking, only the medium-stack bookkeeping. Leaving needs
    // neither the record nor an instance: the side is the face's
    // geometry normal, which is the normal the instance's own side test
    // reads, and the entry is found by the instance and the material.
    // Entering builds both, since the stack entry carries what the
    // medium view reads. A curve or primitive boundary owes its normal
    // to the record builder and keeps the full path either way.
    if (materialDef->isNullInterface()) {
      if (!instance.isCurves() && !instance.isPrimitive()) {
        const float side{dot(scene.hitNg(raw, mRay.time), mRay.dir)};
        if (side >= 0.0f) {
          if (side > 0.0f)
            MediumStack::Leave(mMediumStack, mPath.allocator, materialDef,
                               &instance);
          stepPast();
          continue;
        }
      }
      scene.makeHit(raw, mRay, found);
      smdl::State &state{mPath.shadeHit(found, mShadowDir)};
      passThrough(mPath.allocator.allocate<smdl::JIT::Material>(
                      state, found.materialDef),
                  found);
      continue;
    }
    // A statically opaque material blocks without any material work,
    // and without the record unless the caller asked for the blocker.
    if (materialDef->isAlwaysOpaque()) {
      if (hit) scene.makeHit(raw, mRay, found);
      return true;
    }
    scene.makeHit(raw, mRay, found);
    // Only the ray direction is populated; the LOD fields stay zero so
    // opacity evaluates at full fidelity, the conservative choice for
    // shadow rays.
    smdl::State &state{mPath.shadeHit(found, mShadowDir)};
    // An exactly transparent hit is the segment's own geometry: it costs
    // no draw and passes like a null interface, as it does in the walk
    // (see `PathWalk::trace()`). Anything else is decided here, and a
    // pass the draw decided is a coin the arrival side cannot replay.
    if (const float opacity{found.materialDef->opacityEvaluate(state)};
        opacity > 0) {
      if (opacity == 1 || float(mPath.sampler) < opacity) {
        return true; // Blocks visibility!
      }
      mHasPassedStochasticCutout = true;
    }
    // Only an actual pass-through needs the full instance, to keep the
    // medium stack current across the cutout.
    passThrough(
        mPath.allocator.allocate<smdl::JIT::Material>(state, found.materialDef),
        found);
  }
  return false;
}

void VisibilityWalk::passThrough(const smdl::JIT::Material *material,
                                 const Hit &hit) {
  MediumStack::Update(mMediumStack, mPath.allocator, material, hit.instance,
                      -mRay.dir, mRay.dir);
  stepPast();
}

void VisibilityWalk::stepPast() {
  mRay.tmin = smdl::incrementFloat(mRay.tmax + mParamEps);
  mRay.tmax = mIsInfiniteTarget ? INF : 1.0f - mParamEps;
}

bool testVisibility(const RenderContext &render, PathContext &path,
                    const MediumStack *mediumStack, const float3 &point0,
                    const float3 &point1, Color &beta, bool isInfiniteTarget) {
  VisibilityWalk walk{render, path,  mediumStack,     point0,
                      point1, &beta, isInfiniteTarget};
  return walk.nextBlocker() ? false : beta.maxComponent() > 0.0f;
}
