#include "Render/PathTracing.h"
#include "Render/Camera.h"
#include "Render/Guiding.h"
#include "Render/Light.h"
#include "Render/Manifold.h"
#include "Render/PathStats.h"

#include <algorithm>
#include <cstring>
#include <optional>

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

// The surface hits one straight receiver-to-light segment may resolve and
// still carry transport the manifold estimators claim. Both halves of the
// pair walk that line, the gather to discover its chain and the arrival
// side to prove the chain covered, so a budget one half gave up at alone
// would leave the other claiming transport nobody cancels. Exceeding it
// is not an error: the gather stands down and the arrival keeps the
// ordinary weight, the way every other uncovered arrival does.
constexpr int MNEE_STRAIGHT_MAX_HOPS{64};

// The scattering role of a path vertex: a surface BSDF, a volume phase
// function, or the hair BSDF at a curve hit whose material binds
// `material.hair`.
enum class VertexKind { SURFACE, VOLUME, HAIR };

// A visibility segment walk from `point0` toward `point1`: attenuates
// medium transmittance into `beta` over the spans it covers, passes
// through cutout hits and null interfaces with the nested-medium stack
// kept current, and stops on the first surface that blocks under cutout
// semantics, leaving what to make of that surface to the caller. Plain
// shadow rays treat it as the occluder.
class VisibilityWalk final {
public:
  // `mediumStack` is the nested medium the segment starts in. The walk
  // works in the path's own scratch, so that a segment inside the medium
  // the path is already in resolves nothing: it overwrites `path.medium`
  // and shades every surface it passes through in `path.shadeHit()`, so
  // a caller must not expect either to survive the walk.
  VisibilityWalk(const RenderContext &render, PathContext &path,
                 const MediumStack *mediumStack, const float3 &point0,
                 const float3 &point1, Color &beta,
                 bool isInfiniteTarget = false);

  // Advance to the next blocking surface. Returns true, with `hit`
  // filled in when the caller asked for one; returns false when the walk
  // finished without a blocker, either because the segment reached
  // `point1` or because `beta` was fully absorbed along the way (the
  // caller distinguishes by looking at `beta`).
  //
  // Passing somewhere to put the blocker promises the caller reads it,
  // which is what the manifold refraction gather does to discover
  // chains; it keeps the walk on the closest-hit path in scenes whose
  // `Scene::useOpaqueShadows` would otherwise answer the walk as a boolean
  // occlusion query and return no blocker at all.
  [[nodiscard]] bool nextBlocker(Hit *hit = {});

  // The nested-medium stack as of the walk's current position, e.g., at
  // the blocker just returned.
  [[nodiscard]] const MediumStack *mediumStack() const noexcept {
    return mMediumStack;
  }

  // Pass through the blocker `nextBlocker()` just returned: update the
  // nested-medium stack across it with the given instance and continue
  // the walk on the far side, exactly as the walk passes its own cutout
  // hits.
  void passThrough(const smdl::JIT::Material *material, const Hit &hit);

  // Did the walk pass through a cutout on a draw of its own, i.e. through
  // a hit whose opacity is strictly between 0 and 1? The manifold gather
  // declines such segments, so that its coverage stays the exact
  // complement of the deterministic re-walk the arrival-side MIS runs. A
  // hit whose opacity is exactly 0 or exactly 1 does not count: there is
  // no coin to replay, so the re-walk resolves it the same way every
  // time, which is what keeps a silhouette mask (a leaf, a fence) from
  // disabling the estimators wherever it covers the light.
  [[nodiscard]] bool hasPassedStochasticCutout() const noexcept {
    return mHasPassedStochasticCutout;
  }

  // The surface hits resolved along the segment so far: cutout and
  // null-interface hops, and the blockers the caller passed through. The
  // gather declines a segment that needs more of them than
  // `MNEE_STRAIGHT_MAX_HOPS`, the budget the re-walk of the same line
  // gives up at; see `MNEECoverage::coverWeight()`.
  [[nodiscard]] int hopCount() const noexcept { return mHopCount; }

private:
  // Continue the walk on the far side of the hit it is at.
  void stepPast();

  const RenderContext &mRender;
  PathContext &mPath;

  // The nested-medium stack as of the walk's current position, a
  // walk-local view that evolves across the boundaries it passes through
  // without touching the caller's stack.
  const MediumStack *mMediumStack{};

  Color &mBeta;

  // The world-space segment length.
  float mDist{};

  // The normalized segment direction, or zero when the endpoints
  // coincide, honoring the zero-means-off `State` convention.
  float3 mShadowDir{};

  // The self-intersection offset in the segment's unit parameterization.
  // Offsets are parametric, so for segments longer than one scene unit
  // they are rescaled to stay near `EPS` in WORLD units: a sun shadow ray
  // spans the whole scene, and an offset scaled by that length is wide
  // enough to skip real geometry, and a boundary crossing inside the
  // skipped sliver desyncs the medium stack for the entire segment.
  float mParamEps{};

  // The current cast over the segment's unit parameterization.
  Ray mRay{};

  // The parameter up to which the medium has been integrated, tracked
  // separately from `mRay.tmin` deliberately: integrating only
  // `[tmin, tmax]` of each cast would skip a scene-scaled sliver of
  // medium at every pass-through restart, and where such a gap crosses
  // dense medium the skipped optical depth reads as a bright seam in the
  // shadow.
  float mTCovered{};

  // See `hopCount()`.
  int mHopCount{};

  // See `hasPassedStochasticCutout()`.
  bool mHasPassedStochasticCutout{};

  // Does the segment end where it does only because a light infinitely
  // far away needs a finite point to aim at? See `Medium::attenuate()`.
  bool mIsInfiniteTarget{};
};

VisibilityWalk::VisibilityWalk(const RenderContext &render, PathContext &path,
                               const MediumStack *mediumStack,
                               const float3 &point0, const float3 &point1,
                               Color &beta, bool isInfiniteTarget)
    : mRender(render), mPath(path), mMediumStack(mediumStack), mBeta(beta),
      mIsInfiniteTarget(isInfiniteTarget) {
  mDist = length(point1 - point0);
  mShadowDir = mDist > 0 ? (point1 - point0) / mDist : float3{};
  mParamEps = mDist > 1.0f ? EPS / mDist : EPS;
  mRay = Ray{point0, point1 - point0, mParamEps, 1.0f - mParamEps,
             path.time.fraction};
}

bool VisibilityWalk::nextBlocker(Hit *hit) {
  // Where every material blocks a shadow ray at its first hit (see
  // `Scene::useOpaqueShadows`), a walk whose caller wants no blocker is a
  // pure boolean, which Embree answers cheaper than a closest hit:
  // occlusion early-outs on any hit and skips the hit reconstruction.
  // The medium stack cannot change across such a walk (nothing passes
  // through), so a clear segment attenuates over its whole span in the
  // starting medium, and a blocked one carries nothing: every caller
  // discards `mBeta` on a blocked outcome, so only the two sampler draws
  // heterogeneous tracking would have made are consumed in its place,
  // keeping the deterministic sequence unchanged. The refraction
  // gather's walk, which reads blockers to discover chains, asks for one
  // and keeps the closest-hit path.
  if (mRender.scene.useOpaqueShadows && !hit) {
    const bool isOccluded{mRender.scene.isOccluded(mRay)};
    if (mMediumStack || mPath.medium.hasHaze()) {
      mPath.medium.reset(mMediumStack, mPath.wavelengths, mPath.time,
                         mRay(mTCovered), mShadowDir);
      if (!isOccluded) {
        mPath.medium.attenuate(mPath.sampler, (mRay.tmax - mTCovered) * mDist,
                               mBeta, mIsInfiniteTarget);
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
    // hit). The parametrization spans `[0, 1]` over the segment, so the
    // world-space span is rescaled and the medium sees a unit direction
    // with distances in scene units. The epsilon slivers the casts
    // exclude are attributed to whichever side of the boundary this
    // iteration integrates. An empty stack with no haze skips the medium
    // view outright, shadow segments in vacuum being the common case.
    if (mMediumStack || mPath.medium.hasHaze()) {
      mPath.medium.reset(mMediumStack, mPath.wavelengths, mPath.time,
                         mRay(mTCovered), mShadowDir);
      mPath.medium.attenuate(mPath.sampler, (mRay.tmax - mTCovered) * mDist,
                             mBeta, mIsInfiniteTarget && !hasHitSurface);
    }
    mTCovered = mRay.tmax;
    if (!(mBeta.maxComponent() > 0.0f)) {
      return false; // Fully absorbed already.
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
  mRay.tmax = 1.0f - mParamEps;
}

[[nodiscard]]
bool testVisibility(const RenderContext &render, PathContext &path,
                    const MediumStack *mediumStack, const float3 &point0,
                    const float3 &point1, Color &beta,
                    bool isInfiniteTarget = false) {
  VisibilityWalk walk{render, path, mediumStack,     point0,
                      point1, beta, isInfiniteTarget};
  return walk.nextBlocker() ? false : beta.maxComponent() > 0.0f;
}

[[nodiscard]]
bool scatterEvaluate(Scatterer scatterer, VertexKind kind, const float3 &wo,
                     const float3 &wi, float &pdf, Color &f,
                     int lobeMask = smdl::DF_ALL) {
  // The JIT ABI reports a reverse PDF alongside every forward PDF,
  // which a forward path tracer never consumes.
  float pdfFwdUnused{};
  float pdfRevUnused{};
  if (kind == VertexKind::VOLUME) {
    float phase{scatterer.volumeScatterEvaluate(wo, wi)};
    pdf = phase;
    f = Color(phase);
    return phase > 0;
  }
  // Everything but a volume vertex is a material's; a volume vertex
  // scatters with a VDF or the haze and has no material behind it.
  const smdl::JIT::Material &material{scatterer.material()};
  if (kind == VertexKind::HAIR) {
    return material.hairScatterEvaluate(wo, wi, pdf, pdfRevUnused, f);
  } else {
    if (!material.scatterEvaluate(wo, wi, pdf, pdfRevUnused, f)) return false;
    if (lobeMask == smdl::DF_ALL) return true;
    // The masked value over the UNMASKED density: the mask restricts what
    // is estimated, not the continuation sampler it competes with.
    return material.scatterEvaluate(wo, wi, pdfFwdUnused, pdfRevUnused, f,
                                    lobeMask);
  }
}

// Ask the interface for the Dirac branch `diracMask` names, toward the
// far side of a crossing: sampled rather than evaluated, so a material
// mixing several Dirac lobes renormalizes over the mix, reporting the
// weight of the branch and the chance the ordinary continuation takes
// it. False when the material does not scatter that way. Both halves of
// the estimator ask this the same way, the gather of its converged
// crossings and the arrival-side cancelation of the crossings the path
// took, which is part of what keeps the two MIS weights summing to one.
[[nodiscard]]
bool sampleDiracCrossing(const smdl::JIT::Material &material, Sampler &sampler,
                         const float3 &wPrev, int diracMask, float3 &wi,
                         Color &f, float &chance) {
  float pdfFwd{}, pdfRev{};
  int lobe{};
  chance = 1.0f;
  return material.scatterSample(float4(sampler), wPrev, wi, pdfFwd, pdfRev, f,
                                lobe, diracMask, &chance) &&
         (lobe & smdl::DF_DIRAC) != 0;
}

// One vertex of the walk, as the gathers and the manifold estimators
// see it: where it is, what arrived there, what scatters there, and
// what the estimators may claim of it.
struct PathVertex final {
  // What scatters here: the material instance at a surface or hair
  // vertex, the phase function of the collision at a volume one. It has
  // no default, so a vertex is always built around one.
  Scatterer scatterer;

  VertexKind kind{};

  float3 point{};

  // The direction back along the segment that arrived here.
  float3 wo{};

  // The nested-medium stack the vertex sits in, which every gather ray
  // leaving it starts in.
  const MediumStack *mediumStack{};

  // The SD-tree cell participating here, or null where the continuation
  // samples the BSDF alone; see `guidingCellAt()`.
  const DTree *dtree{};

  // The probability the continuation draws from the BSDF rather than
  // the cell, meaningful only with a `dtree`; see `bsdfFractionAt()`.
  float bsdfFraction{1.0f};

  // What the manifold estimators claim at this vertex, already narrowed
  // to what the gathers behind it can reach from here; see
  // `MNEECoverage::reach()`.
  ManifoldClaim reachableClaim{};

  // Whether a receiver behind this vertex ran a gather, so that the
  // claimed lobes are that gather's to estimate and light sampling here
  // covers the rest.
  bool isArmedBehind{};

  // Whether this vertex is one the gathers run from at all; see
  // `isManifoldReceiver()`.
  bool isReceiver{true};
};

// Everything one manifold connection is weighed against that does not
// change from one connection to the next, which is every input the
// gather has except the connection itself.
class MNEEGather final {
public:
  // Manifold next-event estimation by reflection off a caster; see the
  // definition.
  [[nodiscard]] Color gatherReflection() const;

  // Manifold next-event estimation through the refractive interfaces
  // blocking the straight shadow segment; see the definition.
  [[nodiscard]] Color gatherRefraction(VisibilityWalk &walk, Hit blocker,
                                       int maxDepth, int receiverMask) const;

  // What one converged connection is worth; see the definition.
  // `claimed` sends a Dirac chain down the claimed-exclusive branch,
  // for the biased claimed mode.
  [[nodiscard]] Color contribution(const ManifoldChain &chain,
                                   const ManifoldConnection &connection,
                                   float inverseProbability, int receiverMask,
                                   bool isClaimed = false) const;

public:
  const RenderContext &render;
  PathContext &path;

  // The pristine gather-side state; see `gatherDirect()`.
  const smdl::State &gatherState;

  const PathVertex &vertex;
  const LightSample &lightSample;

private:
  // Draw the offset a glossy crossing is solved for; see the definition.
  [[nodiscard]] bool drawOffset(const ManifoldSurfaces &surfaces,
                                const Hit &hit, const MediumStack *medium,
                                const float3 &wState, const float3 &wo,
                                int lobeMask,
                                ManifoldVertexSeed &vertexSeed) const;

  // The light side of a connection, as the solver's target: a sample with
  // an orientation carries it for the offset Jacobian, and a punctual or
  // distant one has none; see `LightSample::normal`.
  [[nodiscard]]
  static ManifoldTarget makeManifoldTarget(const LightSample &sample) {
    ManifoldTarget target{};
    target.wl = sample.wi;
    target.point = sample.target;
    target.isInfinite = sample.isInfinite;
    target.normal = sample.normal;
    return target;
  }

  // The residual a randomly started walk must reach: the reciprocal
  // estimate's own, and for a glossy chain a fraction of the narrowest
  // lobe it crosses besides, so that the distribution the estimate
  // evaluates at the converged half vector agrees with the density of the
  // drawn one.
  [[nodiscard]] static float
  reciprocalResidualTolerance(const ManifoldChain &chain) {
    float tol{MANIFOLD_RECIPROCAL_RESIDUAL};
    for (int i = 0; i < chain.count; i++)
      if (chain[i].isGlossy && chain[i].alpha > 0.0f)
        tol = std::min(tol, MANIFOLD_GLOSSY_RESIDUAL_FRACTION * chain[i].alpha);
    return tol;
  }

  // The shared reciprocal loop of the randomly seeded estimators; see the
  // definition.
  template <typename Reseed>
  [[nodiscard]]
  Color reciprocalEstimate(const ManifoldTarget &target, ManifoldChain &chain,
                           MNEEStats::Kind statKind, int receiverMask,
                           float scale, const Reseed &reseed) const;
};

// Manifold next-event estimation by reflection off a caster for a light
// sample at a receiver, whether or not the straight segment to it is clear.
//
// The structure is the refractive glossy one: fix a half vector, solve from
// a start, and estimate the reciprocal of the chance of having reached that
// solution by drawing fresh starts until one lands on it again. What
// differs is where a start comes from. A mirror is nowhere near the line
// from the receiver to the light, so there is no crossing to seed from and
// the caster surface is sampled instead: one caster per estimate, chosen
// with a probability the estimate divides out, and every start of the
// estimate drawn on it.
Color MNEEGather::gatherReflection() const {
  const ManifoldTarget target{makeManifoldTarget(lightSample)};
  // One caster for the whole estimate, so that every start is drawn from
  // the same surface with the same material and the same frame, and can
  // re-find what the first walk found. Its selection probability is the
  // one start density the estimate divides out.
  float casterPdf{};
  const MNEECaster *caster{
      render.mneeOptions.casters->sampleCaster(path.sampler, casterPdf)};
  if (!caster) return {};
  const SceneManifoldSurfaces surfaces{render.scene, path.time};
  Color result{};
  // One estimate per claimed kind, each with its own constraint (the
  // exact reflection, or a drawn microfacet normal), its own throughput
  // (the masked material query of that kind), and its own reciprocal
  // count. A material mixing both kinds weighs them inside the masked
  // query, so nothing else multiplies.
  for (const int kindLobe : {smdl::DF_DIRAC_BRDF, smdl::DF_GLOSSY_BRDF}) {
    if ((caster->reflectLobes & kindLobe) == 0) continue;
    // A reflection changes no medium and has no index contrast to
    // resolve, so the two sides weigh the same and `H` is the reflection
    // half vector.
    ManifoldChain chain{};
    chain.count = 1;
    ManifoldVertexSeed &seed{chain[0]};
    seed.etaPrev = seed.etaNext = 1.0f;
    seed.isReflect = true;
    seed.isGlossy = kindLobe == smdl::DF_GLOSSY_BRDF;
    Hit startHit{};
    if (!render.mneeOptions.casters->samplePoint(
            render.scene, path.sampler, *caster, path.time.fraction, startHit))
      continue;
    seed.vertex = vertexOf(startHit);
    seed.frameSeed = manifoldFrameSeed(surfaces, seed.vertex);
    // The half vector is drawn once and held, as on the refractive path:
    // with it fixed the constraint has isolated solutions to recognize. It
    // is expressed in the frame the walk builds from `frameSeed`, which
    // every start of the estimate shares, so it names one world normal
    // throughout.
    if (seed.isGlossy && !drawOffset(surfaces, startHit, /*medium=*/nullptr,
                                     startHit.point - vertex.point,
                                     normalize(vertex.point - startHit.point),
                                     smdl::DF_GLOSSY_BRDF, seed))
      continue;
    chain.residualTolerance = reciprocalResidualTolerance(chain);
    const MNEEStats::Kind statKind{seed.isGlossy ? MNEEStats::GLOSSY_REFLECT
                                                 : MNEEStats::DIRAC_REFLECT};
    // The first walk starts at the hit the offset was drawn at, which is a
    // start like any other: the offset's density cancels pointwise
    // whichever start it was drawn at, so nothing is gained by discarding
    // that one.
    result += reciprocalEstimate( //
        target, chain, statKind, smdl::DF_ALL, 1.0f / casterPdf,
        [&](ManifoldChain &reseeded) {
          Hit reseededHit{};
          if (render.mneeOptions.casters->samplePoint(
                  render.scene, path.sampler, *caster, path.time.fraction,
                  reseededHit)) {
            reseeded[0].vertex = vertexOf(reseededHit);
            return true;
          } else {
            return false;
          }
        });
  }
  return result;
}

// Manifold next-event estimation for an environment sample whose
// straight shadow segment is blocked by smooth refractive interfaces
// (Hanika et al. 2015): discover the seed chain by continuing the walk
// through up to `maxDepth` eligible interfaces, solve the refracted
// connection, then assemble the receiver BSDF at the bent direction,
// the per-crossing Fresnel transmission and radiance compression, the
// transfer Jacobian, and the attenuation and visibility of every
// sub-segment. The result is MIS-weighted against the walk's own
// escapes through the same chain, whose density per unit light solid
// angle is the receiver's continuation density times the host Fresnel
// transmissions times the transfer Jacobian; `tracePath()` applies the
// complementary weight to those escapes with the same formula, so the
// pair sums to one.
Color MNEEGather::gatherRefraction(VisibilityWalk &walk, Hit blocker,
                                   int maxDepth, int receiverMask) const {
  // Discover the seed chain: every blocker along the straight segment
  // must be an eligible interface the walk can differentiate, with its
  // per-side refractive indices resolved against the medium stack as of
  // the crossing, and the segment must clear past the last one.
  ManifoldChain chain{};
  // The medium on the receiver side of each crossing, kept so that the
  // offset draw below can resolve the same exterior index the transport
  // does, and the discovery hit itself, which the offset draw evaluates
  // the interface material at. The arrival side keeps the same things
  // for the same reasons.
  std::array<const MediumStack *, MANIFOLD_MAX_DEPTH> seedMedium{};
  std::array<Hit, MANIFOLD_MAX_DEPTH> seedHits{};
  // The sun gate: toward a gated environment sample the Dirac estimate
  // stands down, and its arrivals keep their ordinary weights by the
  // same predicate, so only a chain with a glossy claim is worth
  // discovering.
  const bool isEnvGated{lightSample.isInfinite &&
                        !render.mneeOptions.isEnvTarget(lightSample.wi)};
  int lobes{smdl::DF_DIRAC_BTDF | smdl::DF_GLOSSY_BTDF};
  // One state for every crossing the discovery walks in turn; see
  // `Hit::applyGeometryToState()`.
  smdl::State state{makeRenderState(path.wavelengths, &path.allocator,
                                    gatherState.animationTime)};
  while (true) {
    if (chain.count == std::min(maxDepth, MANIFOLD_MAX_DEPTH)) return {};
    if (blocker.instance->isCurves()) return {};
    blocker.applyGeometryToState(state, lightSample.wi);
    smdl::JIT::Material &interfaceMaterial{
        *path.allocator.allocate<smdl::JIT::Material>(state,
                                                      blocker.materialDef)};
    ManifoldVertexSeed &seed{chain[chain.count]};
    seedMedium[chain.count] = walk.mediumStack();
    seedHits[chain.count] = blocker;
    if (!makeManifoldSeed(walk.mediumStack(), interfaceMaterial, blocker,
                          lightSample.wi, render.mneeOptions.maxRoughness,
                          seed))
      return {};
    // One estimate per lobe the WHOLE chain claims: the measure now
    // handles a mixed chain, but estimating one would mean claiming it,
    // and the arrival side treats a mixed chain as nobody's (see
    // `MNEECoverage`), so the two policies must move together.
    lobes &= seed.claimedLobes;
    if (lobes == 0) return {};
    if (isEnvGated && (lobes & smdl::DF_GLOSSY_BTDF) == 0) return {};
    chain.count++;
    walk.passThrough(&interfaceMaterial, blocker);
    if (!walk.nextBlocker(&blocker)) break;
  }
  // Decline a segment the arrival side cannot reproduce: a cutout pass
  // this walk's own draw decided, or more hits than the re-walk's budget
  // resolves. The escape-side cancelation probes coverage with a
  // deterministic cast, so the two must agree on what is covered.
  if (walk.hasPassedStochasticCutout() ||
      walk.hopCount() >= MNEE_STRAIGHT_MAX_HOPS)
    return {};
  const ManifoldTarget target{makeManifoldTarget(lightSample)};
  const SceneManifoldSurfaces surfaces{render.scene, path.time};
  MNEEStats *const stats{path.stats ? &path.stats->mnee() : nullptr};
  Color result{};
  // One estimate per kind the whole chain claims: the Dirac chain,
  // deterministic and weighed against the path tracer by re-walk MIS, and
  // the glossy chain below, claimed outright.
  if (!isEnvGated && (lobes & smdl::DF_DIRAC_BTDF) != 0) {
    for (int i = 0; i < chain.count; i++) chain[i].isGlossy = false;
    if (render.mneeOptions.biasedTrials > 0) {
      // The biased claimed mode: exactly `biasedTrials` walks, the
      // first from the straight seed and the rest jittered, the
      // converged solutions clustered and each distinct one summed
      // once at full weight. The arrival side drops every covered
      // arrival at a drawable target, so nothing is weighed twice, and
      // whatever the walks miss is the mode's knowing darkening.
      ManifoldSolutionSet solutions{};
      for (int trial = 0; trial < render.mneeOptions.biasedTrials; trial++) {
        if (trial > 0)
          for (int i = 0; i < chain.count; i++)
            chain[i].seedJitter = MANIFOLD_SEED_JITTER *
                                  smdl::uniformDiskSample(float2(path.sampler));
        ManifoldConnection connection;
        ManifoldWalkReport report{};
        const bool hasConverged{solveManifoldConnection(
            surfaces, vertex.point, target, chain, connection, &report)};
        if (stats) stats->recordWalk(report);
        if (trial == 0 && stats)
          stats->recordEstimate(MNEEStats::DIRAC_REFRACT, hasConverged);
        if (hasConverged)
          solutions.consider(
              vertex.point, connection,
              [&](const ManifoldConnection &other) {
                return contribution(chain, other, 1.0f, receiverMask,
                                    /*isClaimed=*/true);
              },
              stats);
      }
      if (stats)
        stats->recordTrials(MNEEStats::DIRAC_REFRACT,
                            render.mneeOptions.biasedTrials, false);
      result += solutions.sum();
    } else {
      ManifoldConnection connection;
      ManifoldWalkReport report{};
      const bool hasConverged{solveManifoldConnection(
          surfaces, vertex.point, target, chain, connection, &report)};
      if (stats) stats->recordWalk(report);
      if (stats) stats->recordEstimate(MNEEStats::DIRAC_REFRACT, hasConverged);
      if (hasConverged) {
        const Color value{contribution(chain, connection, 1.0f, receiverMask)};
        if (stats) stats->recordContribution(!value.isAllZero());
        result += value;
      }
    }
  }
  if ((lobes & smdl::DF_GLOSSY_BTDF) == 0) return result;
  for (int i = 0; i < chain.count; i++) chain[i].isGlossy = true;
  // A roughened connection, by the estimator of Zeltner, Georgiev and Jakob.
  //
  // Draw one half vector per crossing and hold it FIXED. With the offsets
  // fixed the constraint has isolated solutions, so "which one did the walk
  // reach" is a question with a probability rather than a certainty, and the
  // reciprocal of that probability is what an unbiased estimate of the sum
  // over them needs. Redrawing the offsets per trial would move the solutions
  // and leave nothing to recognize.
  for (int i = 0; i < chain.count; i++)
    if (!drawOffset(surfaces, seedHits[i], seedMedium[i], lightSample.wi,
                    -lightSample.wi, smdl::DF_GLOSSY_BTDF, chain[i]))
      return result;
  chain.residualTolerance = reciprocalResidualTolerance(chain);
  auto jitter{[&](ManifoldChain &reseeded) {
    for (int i = 0; i < reseeded.count; i++)
      reseeded[i].seedJitter =
          MANIFOLD_SEED_JITTER * smdl::uniformDiskSample(float2(path.sampler));
    return true;
  }};
  // Unlike the caster seeder, the straight-line crossings are one fixed
  // start, so the first walk is jittered like every trial or the
  // deterministic start would be over-counted.
  (void)jitter(chain);
  result += reciprocalEstimate(target, chain, MNEEStats::GLOSSY_REFRACT,
                               receiverMask, 1.0f, jitter);
  return result;
}

// What one converged connection is worth: the transport along it, the
// throughput of the Dirac lobe at every crossing, and the re-walk MIS
// weight against the arrival that reaches the same light the same way.
// Every discovery funnels through here, so that solving the constraint a
// different way, or for a different lobe, changes nothing about what a
// solution is worth.
Color MNEEGather::contribution(const ManifoldChain &chain,
                               const ManifoldConnection &connection,
                               float inverseProbability, int receiverMask,
                               bool isClaimed) const {
  // A finite light illuminates the last crossing, not the receiver, so
  // whatever the light does with direction has to be asked again along
  // the segment that actually arrives; see `LightSampler::reevaluateLi()`
  // for what moves and what stays. A zero here is not a failure to
  // report: the path tracer reads the same radiance off an emitter it
  // reaches through the chain, so both halves of the estimator agree the
  // transport carries nothing.
  const float3 &lastPoint{
      connection.vertices[connection.count - 1].geometry.point};
  Color Li{lightSample.Li};
  if (!lightSample.isInfinite) {
    if (Li = render.lights.reevaluateLi(lightSample, path.lightState,
                                        vertex.point, lastPoint,
                                        path.time.fraction);
        Li.isAllZero())
      return {};
  }
  // The receiver BSDF toward the bent direction: the masked value over the
  // unmasked density, which is what the MIS below competes against.
  // `receiverMask` is everything for the reflective gather; the chain
  // gather narrows it to everything but the transmission lobes a gather
  // behind this receiver already claims through it; see `gatherDirect()`.
  float fPdf{};
  Color f{};
  if (!scatterEvaluate(vertex.scatterer, vertex.kind, vertex.wo, connection.wr,
                       fPdf, f, receiverMask))
    return {};
  // The per-crossing throughput and the continuation's chance of taking
  // this chain, both accumulated below from the interface material
  // itself. The material is the only thing that knows them: a tinted or
  // conducting interface has a weight no Fresnel term computed here
  // would carry, and a layered one has a selection chance to match. The
  // weight includes the radiance compression the specular BSDF applies
  // to refracted radiance, which telescopes to first-over-last across a
  // chain, so an air-glass-air chain compresses nothing; the transfer
  // Jacobian carries the purely geometric part.
  Color beta{1.0f};
  float chainChance{1.0f};
  // Visibility and attenuation of every sub-segment, crossing the
  // nested-medium stack at each converged vertex, and finally toward
  // the light: the actual sample point for a finite light, or the far
  // environment target translated to the last vertex.
  Color Tr{1.0f};
  const MediumStack *segMedium{vertex.mediumStack};
  float3 segStart{vertex.point};
  // One state for every converged crossing in turn; see
  // `Hit::applyGeometryToState()`.
  smdl::State crossState{makeRenderState(path.wavelengths, &path.allocator,
                                         gatherState.animationTime)};
  for (int i = 0; i < connection.count; i++) {
    const ManifoldConnectionVertex &crossing{connection.vertices[i]};
    VisibilityWalk segWalk{
        render, path, segMedium, segStart, crossing.geometry.point, Tr};
    if (segWalk.nextBlocker() || !(Tr.maxComponent() > 0.0f)) return {};
    // The solver's vertex is an address, not a hit record; rebuild the
    // hit to evaluate the interface material at the converged crossing.
    Hit crossHit{};
    hitOf(render.scene, crossing.vertex, path.time.fraction, crossHit);
    if (!crossHit.instance) return {};
    crossHit.applyGeometryToState(crossState, -crossing.wPrev);
    smdl::JIT::Material &crossMaterial{
        *path.allocator.allocate<smdl::JIT::Material>(crossState,
                                                      crossHit.materialDef)};
    segMedium = segWalk.mediumStack();
    crossMaterial.setExteriorIOR(
        ExteriorIOR(segMedium, crossMaterial, crossing.wPrev));
    // Ask the interface for the kind this crossing was solved for, per
    // vertex: naming one Dirac LOBE forces that branch on the sampling
    // path however the material layers it, and reports both the weight
    // of the branch and the chance the ordinary continuation would have
    // taken it; a glossy crossing has a density instead, so it is
    // evaluated rather than sampled, under the mask of its own kind.
    //
    // The Dirac sample is drawn rather than fixed. One live Dirac lobe
    // leaves nothing to choose and the draw goes unread, but a material
    // mixing several of them renormalizes over the mix, and a fixed
    // sample would take the first every time while weighting it as
    // though it had been chosen at random. Several Dirac transmissions
    // mixed at one interface remain the one case this cannot settle:
    // they are indistinguishable by direction, so each side reports the
    // chance of the branch its own draw took rather than the sum over
    // every branch producing the direction, and the two MIS weights stop
    // summing to exactly one.
    const ManifoldVertexSeed &vertexSeed{chain[i]};
    const int diracMask{vertexSeed.isReflect ? smdl::DF_DIRAC_BRDF
                                             : smdl::DF_DIRAC_BTDF};
    const int glossyMask{vertexSeed.isReflect ? smdl::DF_GLOSSY_BRDF
                                              : smdl::DF_GLOSSY_BTDF};
    if (vertexSeed.isGlossy) {
      // A glossy crossing has a density, so it is evaluated at the
      // directions the solve produced rather than sampled, masked to the
      // one kind this estimate is for: whatever else the material does in
      // that direction is the ordinary estimators' transport.
      float crossPdfFwd{}, crossPdfRev{};
      Color fCross{};
      if (!crossMaterial.scatterEvaluate(crossing.wPrev, crossing.wNext,
                                         crossPdfFwd, crossPdfRev, fCross,
                                         glossyMask))
        return {};
      beta *= fCross;
      if (beta.isAllZero()) return {};
    } else {
      float vertexChance{};
      float3 wiDirac{};
      Color fDirac{};
      if (!sampleDiracCrossing(crossMaterial, path.sampler, crossing.wPrev,
                               diracMask, wiDirac, fDirac, vertexChance))
        return {};
      // The constraint was solved for this crossing, so the material has
      // to agree that it scatters that way. A disagreement means the
      // interface is not the one the solve differentiated.
      if (!(dot(wiDirac, crossing.wNext) > 1.0f - 1e-3f)) return {};
      beta *= fDirac;
      // Only the BSDF branch of the one-sample MIS can produce a Dirac
      // direction, so where a guiding cell participates at the interface
      // the continuation's chance of this chain carries that branch's
      // discrete weight besides the material's own selection.
      chainChance *=
          vertexChance *
          diracBranchChance(path.guiding, crossing.geometry.point,
                            (crossMaterial.getLobes(
                                 crossMaterial.isInterior(crossing.wPrev)) &
                             smdl::DF_FINITE) != 0);
      if (!(chainChance > 0.0f) || beta.isAllZero()) return {};
    }
    // A reflection stays on the side it arrived from, so it crosses no
    // boundary and the nested medium is the one it was already in.
    if (!chain[i].isReflect)
      MediumStack::Update(segMedium, path.allocator, &crossMaterial,
                          crossHit.instance, crossing.wPrev, crossing.wNext);
    segStart = crossing.geometry.point;
  }
  const float3 lightPoint{lightSample.isInfinite
                              ? segStart + (lightSample.target - vertex.point)
                              : lightSample.target};
  VisibilityWalk lightWalk{render, path, segMedium, segStart, lightPoint, Tr};
  if (lightWalk.nextBlocker() || !(Tr.maxComponent() > 0.0f)) return {};
  // The connection's one measure (see `ManifoldConnection::measure()`),
  // and the density of the drawn offsets, which is 1 where every
  // crossing is Dirac and has no draw.
  const float measure{connection.measure(chain)};
  float offsetDensity{1.0f};
  bool anyGlossy{false};
  for (int i = 0; i < connection.count; i++) {
    if (chain[i].isGlossy) {
      offsetDensity *= chain[i].offsetDensity;
      anyGlossy = true;
    }
  }
  if (!(measure > 0.0f) || !(offsetDensity > 0.0f)) return {};
  // Band by band, the products in the order the color operators would
  // take them, without their four temporaries.
  const float transfer{measure / lightSample.pdf};
  Color direct{};
  for (size_t b = 0; b < direct.size(); b++)
    direct[b] = f[b] * Tr[b] * Li[b] * beta[b] * transfer;
  if (isClaimed || chain[0].isReflect || anyGlossy) {
    // A searched-for connection is claimed exclusively: a reflection was
    // never handed a straight crossing, and a chain with any drawn
    // offset has isolated solutions the walk reaches with a probability
    // it cannot report, so in either case there is no density to weigh
    // against the path tracer's. The reciprocal estimate stands in for
    // the sum over solutions, and the path tracer is barred from this
    // transport rather than sharing it, as in the reference
    // implementation; applying a heuristic here as well would lose
    // whatever share it assigns to a strategy that is no longer running.
    // The biased claimed mode sends the Dirac chain down this branch
    // too (`claimed`): its arrivals are dropped rather than weighed.
    direct *= inverseProbability / offsetDensity;
  } else {
    // Re-walk MIS: the competing density is the receiver's continuation
    // density toward the bent direction, carried through the chain by the
    // discrete Fresnel transmissions and the transfer Jacobian. A light
    // the continuation cannot reach has MIS weight 1, matching the plain
    // branch; see `LightSample::isReachable`.
    if (lightSample.isReachable) {
      float escapePdf{guidedContinuationPdf(vertex.dtree, vertex.bsdfFraction,
                                            connection.wr, fPdf) *
                      chainChance * measure};
      direct *= smdl::powerHeuristic(lightSample.pdf, escapePdf);
    }
  }
  return direct.isAnyNonFinite() ? Color() : direct;
}

// Draw the offset a glossy crossing is solved for: a microfacet normal
// from the lobe the mask names, at the interface material built at `hit`
// (the straight segment's blocker for a refractive chain, a sampled
// caster point for a reflective one; `medium`, when given, first
// resolves the exterior index the way the transport does), converted
// into the walk's frame at that hit and into the measure the constraint
// lives in. The density reported is that of this draw, which is all the
// estimator divides by; where the draw is made matters only for
// variance. `wState` is the arrival direction the shading state is
// built with.
//
// The frame is the one the walk builds from the seed's `frameSeed`, fixed
// here if the seed has none yet, so that the drawn offset names the same
// world normal in every walk of the estimate.
bool MNEEGather::drawOffset(const ManifoldSurfaces &surfaces, const Hit &hit,
                            const MediumStack *medium, const float3 &wState,
                            const float3 &wo, int lobeMask,
                            ManifoldVertexSeed &vertexSeed) const {
  smdl::State &state{path.shadeHit(hit, wState)};
  smdl::JIT::Material offsetMaterial{state, hit.materialDef};
  if (medium)
    offsetMaterial.setExteriorIOR(ExteriorIOR(medium, offsetMaterial, wo));
  if (!(lengthSquared(vertexSeed.frameSeed) > 0.0f))
    vertexSeed.frameSeed = manifoldFrameSeed(surfaces, vertexSeed.vertex);
  float3 normal{}, t1{}, t2{};
  if (!buildManifoldSeedFrame(surfaces, vertexSeed.vertex, vertexSeed.frameSeed,
                              normal, t1, t2))
    return false;
  // The internal frame's z axis is the geometric normal, so the side of
  // the query is the geometric side `wo` is on.
  const bool isBackface{dot(wo, hit.Ng) < 0.0f};
  float3 wm{};
  float pdf{};
  float2 alpha{};
  if (!offsetMaterial.scatterNormalSample(float4(path.sampler), isBackface, wm,
                                          pdf, alpha, lobeMask) ||
      !(pdf > 0.0f))
    return false;
  // The walk orients its half vector onto the shading normal, so the
  // offset has to name the representative on that side.
  if (dot(wm, normal) < 0.0f) wm = -wm;
  const float cosWm{dot(wm, normal)};
  if (!(cosWm > 1e-4f)) return false;
  vertexSeed.offset = float2(dot(wm, t1), dot(wm, t2));
  vertexSeed.offsetDensity = pdf / cosWm;
  vertexSeed.alpha = std::min(alpha.x, alpha.y);
  return true;
}

// The shared reciprocal loop of the randomly seeded estimators (Zeltner,
// Georgiev & Jakob): run the first walk on the chain exactly as handed
// over, weigh its solution, then draw fresh starts of the same estimate
// until one re-finds that solution, and scale by the count, whose
// expectation is the reciprocal of the chance of reaching the solution.
// `reseed` is the seeder that turns the chain into a fresh start of the
// same chain family, returning false when no start could be drawn: the
// straight-line seeder re-jitters the discovered crossings about the
// segment, the caster seeder draws a fresh point on the estimate's one
// caster. The start density never enters the estimator (see
// `MNEECasterSet`); `scale` multiplies into the solution's value,
// which is the one place a seeder's own selection probability (the
// caster's) is divided out.
template <typename Reseed>
Color MNEEGather::reciprocalEstimate(const ManifoldTarget &target,
                                     ManifoldChain &chain,
                                     MNEEStats::Kind statKind, int receiverMask,
                                     float scale, const Reseed &reseed) const {
  const SceneManifoldSurfaces surfaces{render.scene, path.time};
  MNEEStats *const stats{path.stats ? &path.stats->mnee() : nullptr};
  auto solve{[&](ManifoldConnection &connection) {
    ManifoldWalkReport report{};
    const bool hasSolution{solveManifoldConnection(
        surfaces, vertex.point, target, chain, connection, &report)};
    if (stats) stats->recordWalk(report);
    return hasSolution;
  }};
  ManifoldConnection connection;
  const bool hasFirstConverged{solve(connection)};
  if (stats) stats->recordEstimate(statKind, hasFirstConverged);
  if (render.mneeOptions.biasedTrials > 0) {
    // The biased variant: exactly `biasedTrials` walks, the first on
    // the chain as handed over and the rest reseeded, clustering the
    // converged solutions and summing each distinct one once; see
    // `MNEEOptions::biasedTrials` and `ManifoldSolutionSet`.
    ManifoldSolutionSet solutions{};
    const auto value{[&](const ManifoldConnection &other) {
      return contribution(chain, other, scale, receiverMask);
    }};
    if (hasFirstConverged)
      solutions.consider(vertex.point, connection, value, stats);
    for (int trial = 1; trial < render.mneeOptions.biasedTrials; trial++) {
      ManifoldConnection other;
      if (reseed(chain) && solve(other))
        solutions.consider(vertex.point, other, value, stats);
    }
    if (stats)
      stats->recordTrials(statKind, render.mneeOptions.biasedTrials, false);
    return solutions.sum();
  }
  if (!hasFirstConverged) return {};
  // Weigh the solution before the trials, so a worthless one (its light
  // segment blocked, its transport zero) costs none: the estimate is
  // linear in the trial count, so the expectation does not care, and a
  // wall whose reflection of the light is blocked by another interface
  // converges on every walk and would otherwise spend the whole trial
  // budget on nothing.
  Color value{contribution(chain, connection, scale, receiverMask)};
  if (stats) stats->recordContribution(!value.isAllZero());
  if (value.isAllZero()) return {};
  int trials{};
  float inverseProbability{};
  if (manifoldReciprocal(vertex.point, connection, render.mneeOptions.maxTrials,
                         trials, inverseProbability,
                         [&](ManifoldConnection &other) {
                           return reseed(chain) && solve(other);
                         })) {
    if (stats) stats->recordTrials(statKind, trials, false);
    value *= inverseProbability;
    return value;
  }
  if (stats) stats->recordTrials(statKind, render.mneeOptions.maxTrials, true);
  return {};
}

// The MNEE coverage the camera walk carries: armed at every vertex
// whose gather could attempt a manifold connection, along with that
// vertex and the identity of every claimed transmission the walk has
// taken unbroken since. An arrival at a light through a Dirac
// chain is weighed against the gather at the receiver by `coverWeight()`;
// one through a glossy chain is claimed outright, to the share of the
// throughput the chain's claimed lobes carry; any other bounce breaks the
// chain and restores the ordinary weights.
class MNEECoverage final {
public:
  // What the chain since the receiver is made of. A chain of one kind is
  // what the gathers estimate; a mixed one nobody claims.
  enum class ChainKind { NONE, DIRAC, GLOSSY, MIXED };

  // Begin a fresh receiver, the vertex a gather could connect from.
  // `isEnabled` is false when manifold NEE is off, which leaves the state
  // permanently disarmed.
  void arm(bool isEnabled, const float3 &point, float pdf,
           const MediumStack *medium) noexcept {
    mIsArmed = isEnabled;
    mChainLength = 0;
    mChainKind = ChainKind::NONE;
    mChainShare = Color(1.0f);
    mReceiver = point;
    mReceiverPdf = pdf;
    mReceiverMedium = medium;
  }

  // Disarm, which is all a fresh path needs: `arm()` sets everything
  // else before any reader consults it, and the chain arrays are read
  // only below the length it resets.
  void disarm() noexcept { mIsArmed = false; }

  [[nodiscard]] bool isArmed() const noexcept { return mIsArmed; }

  // Extend the chain across a claimed transmission: Dirac, or glossy with
  // the share of the crossing's throughput its claimed lobe carries. The
  // length keeps counting past `MANIFOLD_MAX_DEPTH` so that an overlong chain
  // reads as uncovered rather than as a shorter one.
  void extend(const Hit &hit, bool isGlossy,
              const Color &claimedShare) noexcept {
    if (mChainLength < MANIFOLD_MAX_DEPTH) {
      mChainInstances[mChainLength] = hit.instance;
      mChainPieces[mChainLength] = hit.faceIndex;
      mChainHits[mChainLength] = hit;
    }
    mChainLength++;
    const ChainKind kind{isGlossy ? ChainKind::GLOSSY : ChainKind::DIRAC};
    mChainKind = mChainKind == ChainKind::NONE || mChainKind == kind
                     ? kind
                     : ChainKind::MIXED;
    if (isGlossy) mChainShare *= claimedShare;
  }

  // Is there a Dirac chain of connectable length for the gather to
  // compete with, so that `coverWeight()` replaces the ordinary weight?
  [[nodiscard]]
  bool coversDirac(const MNEEOptions &mneeOptions) const noexcept {
    return covers(ChainKind::DIRAC, mneeOptions);
  }

  // Is there a glossy chain of connectable length, which the gather at
  // the receiver claims to the share `chainShare()`?
  [[nodiscard]]
  bool coversGlossy(const MNEEOptions &mneeOptions) const noexcept {
    return covers(ChainKind::GLOSSY, mneeOptions);
  }

  [[nodiscard]] const Color &chainShare() const noexcept { return mChainShare; }

  [[nodiscard]] ChainKind chainKind() const noexcept { return mChainKind; }

  [[nodiscard]] int chainLength() const noexcept { return mChainLength; }

  // What the gathers behind this vertex can reach of the vertex's own
  // claim, which is what the vertex's gather leaves to them and what the
  // next arrival drops. The reflection kinds are the previous vertex's
  // reflective gather's, which ran there and weighed its connections
  // with the finite lobes the path then bounced through, so a Dirac
  // bounce there is outside it; the transmission kinds are the chain
  // receiver's refractive gather's, which reaches this vertex only as
  // the next crossing of a chain of one kind within its depth. Nothing
  // is reachable while disarmed.
  [[nodiscard]] ManifoldClaim reach(const ManifoldClaim &claim,
                                    const MNEEOptions &mneeOptions,
                                    bool isPrevDirac) const noexcept {
    ManifoldClaim reachable{};
    if (!mIsArmed) return reachable;
    if (!isPrevDirac) reachable.reflectLobes = claim.reflectLobes;
    if (mChainLength < mneeOptions.depth) {
      switch (mChainKind) {
      case ChainKind::NONE:
        reachable.refractLobes = claim.refractLobes;
        break;
      case ChainKind::DIRAC:
        reachable.refractLobes = claim.refractLobes & smdl::DF_DIRAC_BTDF;
        break;
      case ChainKind::GLOSSY:
        reachable.refractLobes = claim.refractLobes & smdl::DF_GLOSSY_BTDF;
        break;
      case ChainKind::MIXED:
        break;
      }
    }
    return reachable;
  }

  [[nodiscard]] const float3 &isReceiver() const noexcept { return mReceiver; }

  // The MIS weight of a BSDF-side arrival at `target` through the
  // chain, by re-walk MIS; see the definition.
  [[nodiscard]] float coverWeight(const RenderContext &render,
                                  PathContext &path,
                                  const ManifoldTarget &target,
                                  float lightPdf) const;

private:
  [[nodiscard]] bool covers(ChainKind kind,
                            const MNEEOptions &mneeOptions) const noexcept {
    return mIsArmed && mChainKind == kind && mChainLength >= 1 &&
           mChainLength <= mneeOptions.depth;
  }

  bool mIsArmed{};
  int mChainLength{};
  ChainKind mChainKind{ChainKind::NONE};
  Color mChainShare{1.0f};
  float3 mReceiver{};
  float mReceiverPdf{};
  const MediumStack *mReceiverMedium{};
  std::array<const MeshInstance *, MANIFOLD_MAX_DEPTH> mChainInstances{};
  std::array<uint32_t, MANIFOLD_MAX_DEPTH> mChainPieces{};
  std::array<Hit, MANIFOLD_MAX_DEPTH> mChainHits{};
};

// The MIS weight of a BSDF-side arrival at a light, an environment
// escape or an emitter hit, through a Dirac chain of eligible refractive
// interfaces, by re-walk MIS (Hanika et al. 2015, section 5): re-run
// the deterministic manifold walk the gather at `receiver` runs for
// this target, and only when it converges to the same crossings the
// path actually took does the gather compete; otherwise (a different
// chain family, a different fold solution, a failed walk, a partial
// cutout in the way, a line too crowded for the hop budget, or a light
// the sampler cannot draw) the arrival keeps weight 1 instead of
// silently losing its transport. The competing densities are per unit
// solid angle of the straight line toward the light: the gather's is the
// light sampling density, the arrival's is the receiver's recorded
// continuation density times the interfaces' own selection chances times
// the transfer Jacobian; the gather applies the complementary weight with
// the same formula, so the pair sums to one.
//
// That last factor is taken from the walk run here rather than
// re-evaluated on the crossings the path actually took. The two agree
// only to the convergence tolerance, and the pair sums to one exactly
// when both sides weigh the same number, so the number to weigh by is
// the one the gather would compute: this walk IS the gather's walk, for
// this target.
float MNEECoverage::coverWeight(const RenderContext &render, PathContext &path,
                                const ManifoldTarget &target,
                                float lightPdf) const {
  const float3 &isReceiver{mReceiver};
  const MediumStack *receiverMedium{mReceiverMedium};
  const float receiverPdf{mReceiverPdf};
  const std::array<Hit, MANIFOLD_MAX_DEPTH> &chainHits{mChainHits};
  const std::array<const MeshInstance *, MANIFOLD_MAX_DEPTH> &chainInstances{
      mChainInstances};
  const std::array<uint32_t, MANIFOLD_MAX_DEPTH> &chainPieces{mChainPieces};
  const int chainLength{mChainLength};
  // Whether the re-walk reproduced the crossings the path took; every
  // other exit keeps the arrival at weight 1, so the matched fraction
  // is the share of covered arrivals the gather can ever claim.
  bool isMatched{false};
  SMDL_DEFER([&] {
    if (path.stats) path.stats->mnee().recordCover(isMatched);
  });
  // A light the sampler cannot draw is covered by this arrival alone.
  if (!(lightPdf > 0.0f)) return 1.0f;
  // Build the seed chain along the straight cast, mirroring the
  // gather's discovery: the same interfaces in the same order and
  // nothing else, with per-side indices resolved against the
  // receiver's medium stack as it evolves across the crossings. A
  // finite target bounds each cast short of the light point itself.
  ManifoldChain chain{};
  // The receiver-side medium at each crossing, kept so that the chance
  // below can be asked of the interface material with the same exterior
  // index the gather resolves.
  std::array<const MediumStack *, MANIFOLD_MAX_DEPTH> crossingMedium{};
  const MediumStack *medium{receiverMedium};
  const float3 wl{target.wl};
  const float3 woStraight{-wl};
  float3 origin{isReceiver};
  // One state for every interface the cast crosses in turn, and for the
  // crossings re-asked below; see `Hit::applyGeometryToState()`.
  smdl::State state{
      makeRenderState(path.wavelengths, &path.allocator, path.time.seconds)};
  bool hasReached{false};
  for (int hops = 0; hops < MNEE_STRAIGHT_MAX_HOPS; hops++) {
    float tmax{INF};
    if (!target.isInfinite) {
      tmax = length(target.point - origin) - EPS;
      if (!(tmax > EPS)) {
        hasReached = true;
        break;
      }
    }
    Ray ray{origin, wl, EPS, tmax, path.time.fraction};
    Hit hit{};
    if (!render.scene.intersect(ray, hit)) {
      hasReached = true;
      break;
    }
    hit.applyGeometryToState(state, wl);
    const bool isNullInterface{hit.materialDef->isNullInterface()};
    // A hole the straight line pierces, which hops like a null interface.
    // An exactly transparent cutout is the only one that may: anything
    // else the gather's walk resolved with a draw of its own (see
    // `VisibilityWalk::hasPassedStochasticCutout()`), and a hit it took
    // for a blocker is one this cast has to take for a crossing, or the
    // two stop agreeing on what is covered. Asked before the material is
    // built, and through the same entry point the gather's walk asks,
    // so that a stochastically evaluated cutout answers both the same.
    const bool isHole{!isNullInterface && !hit.materialDef->isAlwaysOpaque() &&
                      hit.materialDef->opacityEvaluate(state) == 0};
    smdl::JIT::Material &interfaceInst{
        *path.allocator.allocate<smdl::JIT::Material>(state, hit.materialDef)};
    if (isNullInterface || isHole) {
      MediumStack::Update(medium, path.allocator, &interfaceInst, hit.instance,
                          woStraight, wl);
      origin = hit.point;
      continue;
    }
    if (chain.count == chainLength) return 1.0f;
    if (hit.instance != chainInstances[chain.count]) return 1.0f;
    if (hit.instance->isCurves()) return 1.0f;
    if (hit.instance->isPrimitive() &&
        hit.faceIndex != chainPieces[chain.count])
      return 1.0f;
    // The gather runs its Dirac chain only where every crossing claims
    // the Dirac transmission, so this must ask the same or the pair stops
    // summing to one.
    if (!makeManifoldSeed(medium, interfaceInst, hit, wl,
                          render.mneeOptions.maxRoughness,
                          chain[chain.count]) ||
        (chain[chain.count].claimedLobes & smdl::DF_DIRAC_BTDF) == 0)
      return 1.0f;
    crossingMedium[chain.count] = medium;
    chain.count++;
    MediumStack::Update(medium, path.allocator, &interfaceInst, hit.instance,
                        woStraight, wl);
    origin = hit.point;
  }
  if (!hasReached || chain.count != chainLength) return 1.0f;
  ManifoldConnection connection;
  ManifoldWalkReport report{};
  const SceneManifoldSurfaces surfaces{render.scene, path.time};
  const bool hasConverged{solveManifoldConnection(surfaces, isReceiver, target,
                                                  chain, connection, &report)};
  if (path.stats) path.stats->mnee().recordRewalk(report);
  if (!hasConverged) return 1.0f;
  for (int i = 0; i < chainLength; i++) {
    const float scale{std::max(1e-3f, length(chainHits[i].point - isReceiver))};
    if (!(length(connection.vertices[i].vertex.point - chainHits[i].point) <
          MANIFOLD_IDENTITY_FRACTION * scale))
      return 1.0f;
  }
  isMatched = true;
  const float transfer{connection.measure(chain)};
  // The chance the continuation takes this chain, asked of each
  // interface rather than recomputed here, so that a layered or tinted
  // interface reports the selection it actually makes. The gather
  // accumulates the same quantity the same way, drawn from its own
  // sampler, which is what keeps the pair summing to one.
  float Q{1.0f};
  float3 prev{isReceiver};
  for (int i = 0; i < chainLength; i++) {
    const float3 toHit{chainHits[i].point - prev};
    const float d{length(toHit)};
    if (!(d > 0.0f)) return 1.0f;
    const float3 travel{toHit / d};
    chainHits[i].applyGeometryToState(state, travel);
    smdl::JIT::Material crossMaterial{state, chainHits[i].materialDef};
    crossMaterial.setExteriorIOR(
        ExteriorIOR(crossingMedium[i], crossMaterial, -travel));
    float3 wiDirac{};
    float vertexChance{};
    Color fDirac{};
    if (!sampleDiracCrossing(crossMaterial, path.sampler, -travel,
                             smdl::DF_DIRAC_BTDF, wiDirac, fDirac,
                             vertexChance))
      return 1.0f;
    // The gather folds the same guiding branch chance into its
    // `chainChance` at its converged crossing, so the pair keeps
    // weighing the same number.
    Q *= vertexChance *
         diracBranchChance(
             path.guiding, chainHits[i].point,
             (crossMaterial.getLobes(crossMaterial.isInterior(-travel)) &
              smdl::DF_FINITE) != 0);
    prev = chainHits[i].point;
  }
  const float arrivalPdf{receiverPdf * Q * transfer};
  if (!(arrivalPdf > 0.0f) || !std::isfinite(arrivalPdf)) return 1.0f;
  return smdl::powerHeuristic(arrivalPdf, lightPdf);
}

// Does the gather at a vertex of `kind` run the manifold estimators,
// whose light samples are drawn by area (`LightSampler::sample()`'s
// `shouldKeepDark`)? The arrival sites ask this of the previous vertex to
// recompute the density its gather drew with.
[[nodiscard]] bool gatherRunsManifold(const MNEEOptions &mneeOptions,
                                      VertexKind kind,
                                      bool isReceiver) noexcept {
  return kind != VertexKind::HAIR && mneeOptions.depth > 0 && isReceiver;
}

// Gather direct lighting at one path vertex by light sampling: sample a
// light, evaluate the BSDF, and test visibility against the sampled
// point. The BSDF-sampling half of the MIS pair is the walk's own
// continuation segment, whose emitter hits and environment escapes
// `tracePath` weighs against the density this gather would have
// produced. Returns the estimate WITHOUT the path throughput, which is
// exactly what the guiding trainer records. `gatherState` is a pristine
// state carrying only the render-wide fields: light sampling applies the
// light hit's own geometry, and the LOD fields stay zero so emission
// evaluates at full fidelity.
//
// With the manifold estimators enabled, a light sample whose straight
// segment is blocked by claimed refractive interfaces routes through
// `MNEEGather::gatherRefraction()` instead of reading as occluded, and
// the reflective gather searches the marked casters besides. Hair
// vertices keep plain gathering: the manifold estimator's MIS is not
// wired through the hair BSDF.
//
// Where the vertex both claims lobes and stands behind an armed
// receiver, those lobes are the receiver's gather's to estimate, so
// light sampling here covers the other lobes only, weighed against the
// unmasked continuation density exactly as the continuation's arrivals
// are weighed for those lobes (`tracePath()` keeps their share of each
// arrival and drops the claimed share).
[[nodiscard]]
Color gatherDirect(const RenderContext &render, PathContext &path,
                   const smdl::State &gatherState, const PathVertex &vertex) {
  const int mneeDepth{
      vertex.kind == VertexKind::HAIR ? 0 : render.mneeOptions.depth};
  const bool shouldRunManifold{
      gatherRunsManifold(render.mneeOptions, vertex.kind, vertex.isReceiver)};
  Color direct{};
  if (render.lights.empty()) return direct;
  LightSample &lightSample{path.gatherSample};
  // A manifold gather keeps the samples that radiate nothing toward the
  // receiver: its connection arrives at the light from elsewhere and reads
  // the radiance from there. The plain estimate of such a sample is zero
  // and is skipped below.
  if (render.lights.sample(path.lightState, path.skyBasis, path.sampler,
                           vertex.point, path.time.fraction, lightSample,
                           shouldRunManifold)) {
    const MNEEGather mneeGather{render, path, gatherState, vertex, lightSample};
    // The reflect claims belong to the reflective gather, which the
    // layout's light marks may restrict to the caustic targets: toward
    // any other light the claimed reflections are ordinary transport
    // again, here and at the arrivals. The transmit claims are the
    // refractive chains', which run for every light.
    ManifoldClaim lightClaim{vertex.reachableClaim};
    if (!lightSample.isCaustic) lightClaim.reflectLobes = 0;
    const bool shouldSplit{vertex.isArmedBehind && !lightClaim.empty()};
    const int neeMask{shouldSplit ? (smdl::DF_ALL & ~lightClaim.lobes())
                                  : smdl::DF_ALL};
    // The plain estimator of a sample whose straight segment is clear,
    // with `vis` the segment's attenuation. The competing density in the
    // MIS weight must be the density the continuation sampler actually
    // assigns to this direction: the BSDF alone, or the guided mixture
    // when the SD-tree participates at this vertex. Weighing against the
    // raw BSDF density while the continuation samples the mixture makes
    // the two halves sum past 1 and reads several percent bright.
    const auto gatherPlain{[&](const Color &Tr) {
      float fPdf{};
      Color f{};
      if (neeMask == 0 || lightSample.Li.isAllZero() ||
          !scatterEvaluate(vertex.scatterer, vertex.kind, vertex.wo,
                           lightSample.wi, fPdf, f, neeMask))
        return;
      const float continuationPdf{guidedContinuationPdf(
          vertex.dtree, vertex.bsdfFraction, lightSample.wi, fPdf)};
      Color D{};
      for (size_t b = 0; b < D.size(); b++)
        D[b] = f[b] * Tr[b] * lightSample.Li[b] / lightSample.pdf;
      if (D.isAnyNonFinite()) return;
      // A light the continuation cannot reach has MIS weight 1; see
      // `LightSample::isReachable`.
      if (lightSample.isReachable)
        D *= smdl::powerHeuristic(lightSample.pdf, continuationPdf);
      direct += D;
    }};
    if (!shouldRunManifold) {
      if (Color Tr{1.0f};
          neeMask != 0 &&
          testVisibility(render, path, vertex.mediumStack, vertex.point,
                         lightSample.target, Tr, lightSample.isInfinite)) {
        gatherPlain(Tr);
      }
    } else {
      // Visibility first: whether the straight segment is clear decides
      // which estimator runs, and the manifold connection does not care
      // whether the straight direction can scatter at the receiver.
      Color Tr{1.0f};
      Hit &blocker{path.gatherBlocker};
      VisibilityWalk walk{render,
                          path,
                          vertex.mediumStack,
                          vertex.point,
                          lightSample.target,
                          Tr,
                          lightSample.isInfinite};
      if (!walk.nextBlocker(&blocker)) {
        if (Tr.maxComponent() > 0.0f) gatherPlain(Tr);
      } else {
        // The chain gather of the receiver behind this vertex already
        // claims every chain that starts with this vertex's claimed
        // transmission kinds, so this gather weighs its connections with
        // the rest of the BSDF only.
        direct += mneeGather.gatherRefraction(
            walk, blocker, mneeDepth,
            smdl::DF_ALL & ~vertex.reachableClaim.refractLobes);
      }
      // The reflective gather is additive rather than an alternative: a
      // mirror is nowhere near the line to the light, so whether that line
      // is clear says nothing about whether there is a reflection to find.
      // It searches toward the caustic targets only; see
      // `LightSample::caustic`.
      if (render.mneeOptions.casters && !render.mneeOptions.casters->empty() &&
          lightSample.isCaustic)
        direct += mneeGather.gatherReflection();
    }
  }
  return direct;
}

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

// The share of one bounce's throughput the manifold estimators claim,
// per wavelength, given what the gathers behind this vertex can reach:
// all of a Dirac reflection of a claimed kind, and of a finite bounce
// the part of its value the claimed lobes carry, which is the value
// without them over the value with them. A Dirac transmission is not a
// share but a chain, weighed against by re-walk MIS, and reports zero
// here.
[[nodiscard]]
Color claimedShareOf(const smdl::JIT::Material &material,
                     const ManifoldClaim &reachable, const float3 &wo,
                     const float3 &wNext, const Color &f, bool isDiracBounce,
                     bool transmits, int sampledLobe) {
  Color claimedShare{};
  if (reachable.empty()) return claimedShare;
  if (isDiracBounce) {
    if (!transmits &&
        (sampledLobe & reachable.reflectLobes & smdl::DF_DIRAC_BRDF) != 0)
      claimedShare = Color(1.0f);
    return claimedShare;
  }
  if ((transmits ? reachable.refractLobes & smdl::DF_GLOSSY_BTDF
                 : reachable.reflectLobes & smdl::DF_GLOSSY_BRDF) == 0)
    return claimedShare;
  float pdfUnclaimed{}, pdfRevUnused{};
  Color fUnclaimed{};
  if (material.scatterEvaluate(wo, wNext, pdfUnclaimed, pdfRevUnused,
                               fUnclaimed, smdl::DF_ALL & ~reachable.lobes()))
    for (size_t b = 0; b < claimedShare.size(); b++)
      claimedShare[b] =
          f[b] > 0.0f ? std::clamp(1.0f - fUnclaimed[b] / f[b], 0.0f, 1.0f)
                      : 0.0f;
  else
    claimedShare = Color(1.0f);
  return claimedShare;
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

  // Begin a path: no bounce behind the camera segment, and no share of
  // it claimed, which is what the arrival sites read on that segment.
  void reset() noexcept {
    pdf = 0.0f;
    isDirac = false;
    point = float3(0.0f);
    isAreaSampled = false;
    claimedShare.fill(0.0f);
    shouldShareCausticOnly = false;
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
  // throughput nobody claims, and a claimed share toward a target its
  // gather reaches is the gather's outright. `makeCoverTarget` fills the
  // target the re-walk aims at and returns the light-sampling density it
  // competes with, negative when there is no target to re-walk, which
  // keeps the ordinary weight.
  template <typename MakeCoverTarget>
  void addArrival(const Color &Li, float weight, uint64_t bounces,
                  bool isCausticTarget, GuideRecord *record,
                  const MakeCoverTarget &makeCoverTarget) {
    // The factor on the throughput times the radiance, per band: one
    // weight across the bands for a covered Dirac chain, and otherwise
    // the ordinary weight on the share of each band nobody claims.
    float uniform{weight};
    const Color *share{nullptr};
    if (mCoverage.coversDirac(mRender.mneeOptions)) {
      ManifoldTarget target{};
      const float lightPdf{makeCoverTarget(target)};
      if (lightPdf > 0.0f && mRender.mneeOptions.biasedTrials > 0) return;
      if (lightPdf >= 0.0f)
        uniform = mCoverage.coverWeight(mRender, mPath, target, lightPdf);
    } else if (!(mPrev.shouldShareCausticOnly && !isCausticTarget)) {
      share = &mPrev.claimedShare;
    }
    const auto factorAt{[&](size_t b) {
      return share ? (1.0f - (*share)[b]) * weight : uniform;
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
  // An instance is exact at the wavelengths and time it was evaluated
  // with and at no others, which is the contract of the homogeneity proof
  // (see `MaterialDef::hasHomogeneousCoefficients()`), so the exterior is
  // evaluated again whenever a path's differ from the last evaluation's:
  // every path under -wavelength-jitter or an open shutter, and once per
  // block otherwise. The instance has no geometry and no side, so the
  // render state's default frame is already the finalized one, and the
  // evaluation takes no sampler draw.
  const Color &wavelengths{mPath.wavelengths};
  const float time{mPath.time.seconds};
  if (!mExteriorMedium || mExteriorTime != time ||
      std::memcmp(mExteriorWavelengths.data(), wavelengths.data(),
                  wavelengths.size() * sizeof(float)) != 0) {
    mExteriorAllocator.reset();
    smdl::State state{makeRenderState(wavelengths, &mExteriorAllocator, time)};
    mExteriorMedium = new (mExteriorAllocator)
        MediumStack{nullptr,
                    mExteriorAllocator.allocate<smdl::JIT::Material>(
                        state, mRender.exteriorMediumDef),
                    nullptr};
    mExteriorWavelengths = wavelengths;
    mExteriorTime = time;
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
      mPath.medium.reset(mMediumStack, mPath.wavelengths, mPath.time, ray.org,
                         ray.dir);
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
          {
            PathVertex vertex{phase};
            vertex.kind = VertexKind::VOLUME;
            vertex.point = point;
            vertex.wo = wo;
            vertex.mediumStack = mMediumStack;
            // The SD-tree never participates at volume vertices, so the
            // vertex keeps its null cell and the continuation density
            // the gather weighs against is the phase function alone.
            Color direct{gatherDirect(mRender, mPath, mGatherState, vertex)};
            addGathered(direct, record);
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
          mPrev.point = point;
          mPrev.isAreaSampled =
              gatherRunsManifold(mRender.mneeOptions, VertexKind::VOLUME, true);
          // A volume vertex is a manifold-NEE receiver like any other.
          mCoverage.arm(mRender.mneeOptions.isEnabled(), point, phaseValue,
                        mMediumStack);
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
        addArrival(Li, weight, mDepth - 1, mRender.lights.isCausticEnv(),
                   mPath.records && mPath.numRecords > 0
                       ? &mPath.records[mPath.numRecords - 1]
                       : nullptr,
                   [&](ManifoldTarget &target) {
                     if (!mRender.mneeOptions.isEnvTarget(ray.dir))
                       return -1.0f;
                     target.wl = ray.dir;
                     return mRender.lights.envSelectionPMF(
                                mCoverage.isReceiver()) *
                            Lipdf;
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
        addArrival(Le, weight, mDepth - 2,
                   mRender.lights.isCausticLight(hit.instIndex),
                   mPath.records && mPath.numRecords > 1
                       ? &mPath.records[mPath.numRecords - 2]
                       : nullptr,
                   [&](ManifoldTarget &target) {
                     const float3 toLight{hit.point - mCoverage.isReceiver()};
                     const float distStraight{length(toLight)};
                     if (!(distStraight > 0.0f)) return -1.0f;
                     target.wl = toLight / distStraight;
                     target.point = hit.point;
                     target.isInfinite = false;
                     target.normal = hit.Ng;
                     return mRender.lights.solidAnglePDF(
                         hit.instIndex, hit.faceIndex, hit.point, hit.Ng,
                         mCoverage.isReceiver(), /*isAreaSampled=*/true,
                         hit.time);
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
            ? manifoldClaim(material, isBackface, hit.instance->isCausticCaster,
                            mRender.mneeOptions.maxRoughness)
            : ManifoldClaim()};
    const ManifoldClaim reachable{
        mCoverage.reach(claim, mRender.mneeOptions, mPrev.isDirac)};
    // Whether this vertex is a manifold receiver: the gathers run from it
    // and it arms for the claims behind, or neither.
    const bool isReceiver{mRender.mneeOptions.isEnabled() && !isHair &&
                          isManifoldReceiver(
                              material, isBackface,
                              [&] { return float4(mPath.sampler); },
                              mRender.mneeOptions.minReceiverAlpha)};
    // Gather direct lighting at this vertex, before the bounce, so the
    // cone the gather rays inherit is the arrival cone.
    {
      PathVertex vertex{material};
      vertex.kind = isHair ? VertexKind::HAIR : VertexKind::SURFACE;
      vertex.point = hit.point;
      vertex.wo = wo;
      vertex.mediumStack = mMediumStack;
      vertex.dtree = dtree;
      vertex.bsdfFraction = bsdfFraction;
      vertex.reachableClaim = reachable;
      vertex.isArmedBehind = wasArmed;
      vertex.isReceiver = isReceiver;
      Color direct{gatherDirect(mRender, mPath, mGatherState, vertex)};
      addGathered(direct, record);
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
    mPrev.isAreaSampled = gatherRunsManifold(
        mRender.mneeOptions, isHair ? VertexKind::HAIR : VertexKind::SURFACE,
        isReceiver);
    const bool transmits{!isHair && material.isTransmitting(wo, wNext)};
    const Color claimedShare{claimedShareOf(material, reachable, wo, wNext, f,
                                            isDiracBounce, transmits,
                                            sampledLobe)};
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
      mCoverage.arm(isReceiver, hit.point, wpdf, mMediumStack);
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
