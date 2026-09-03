#include "PathTracing.h"
#include "Guiding.h"
#include "Light.h"
#include "Manifold.h"

#include <algorithm>

VisibilityWalk::VisibilityWalk(smdl::BumpPtrAllocator &allocator,
                               const Scene &scene, Sampler &sampler,
                               const Color &wavelengths, float time,
                               const MediumStack *medium, PathScratch &scratch,
                               const float3 &point0, const float3 &point1,
                               Color &beta, bool needBlocker,
                               bool infiniteTarget)
    : mAllocator(allocator), mScene(scene), mSampler(sampler),
      mWavelengths(wavelengths), mTime(time), mMedium(medium),
      mScratch(scratch), mBeta(beta), mNeedBlocker(needBlocker),
      mInfiniteTarget(infiniteTarget) {
  mDistance = length(point1 - point0);
  mShadowDir = mDistance > 0 ? (point1 - point0) / mDistance : float3{};
  mParamEps = mDistance > 1.0f ? EPS / mDistance : EPS;
  mRay = Ray{point0, point1 - point0, mParamEps, 1.0f - mParamEps};
}

bool VisibilityWalk::nextBlocker(Hit &hit) {
  // Where every material blocks a shadow ray at its first hit (see
  // `Scene::opaqueShadows`), a walk whose caller ignores the blocker is
  // a pure boolean, which Embree answers cheaper than a closest hit:
  // occlusion early-outs on any hit and skips the hit reconstruction.
  // The medium stack cannot change across such a walk (nothing passes
  // through), so a clear segment attenuates over its whole span in the
  // starting medium, and a blocked one carries nothing: every caller
  // discards `mBeta` on a blocked outcome, so only the two sampler
  // draws heterogeneous tracking would have made are consumed in its
  // place, keeping the deterministic sequence unchanged. `hit` is left
  // untouched; the refraction gather's walk, which reads blockers to
  // discover chains, says so at construction and keeps the closest-hit
  // path.
  if (mScene.opaqueShadows && !mNeedBlocker) {
    const bool occluded{mScene.isOccluded(mRay)};
    if (mMedium || mScratch.medium.hasHaze()) {
      mScratch.medium.reset(mMedium, mWavelengths, mTime, mRay(mTCovered),
                            mShadowDir);
      if (!occluded) {
        mScratch.medium.attenuate(mSampler, (mRay.tmax - mTCovered) * mDistance,
                                  mBeta, mInfiniteTarget);
      } else if (mScratch.medium.attenuationDraws()) {
        (void)mSampler.nextBits();
        (void)mSampler.nextBits();
      }
    }
    return occluded;
  }
  while (mRay.tmin < mRay.tmax) {
    hit = Hit{};
    bool hitSurface{mScene.intersect(mRay, hit)};
    // Attenuate over the span actually traveled, hit or miss
    // (`Scene::intersect` narrows `tmax` to the hit parameter on a
    // hit). The parametrization spans `[0, 1]` over the segment, so the
    // world-space span is rescaled and the medium sees a unit direction
    // with distances in scene units. The epsilon slivers the casts
    // exclude are attributed to whichever side of the boundary this
    // iteration integrates. An empty stack with no haze skips the medium
    // view outright, shadow segments in vacuum being the common case.
    if (mMedium || mScratch.medium.hasHaze()) {
      mScratch.medium.reset(mMedium, mWavelengths, mTime, mRay(mTCovered),
                            mShadowDir);
      mScratch.medium.attenuate(mSampler, (mRay.tmax - mTCovered) * mDistance,
                                mBeta, mInfiniteTarget && !hitSurface);
    }
    mTCovered = mRay.tmax;
    if (!(mBeta.maxComponent() > 0.0f)) {
      return false; // Fully absorbed already.
    }
    if (!hitSurface) {
      return false;
    }
    // A null interface passes shadow rays straight through: no opacity
    // and no blocking, only the medium-stack bookkeeping, which needs
    // the full instance.
    if (hit.material->isNullInterface()) {
      auto &state{
          mScratch.shadeHit(hit, mShadowDir, mWavelengths, mAllocator, mTime)};
      passThrough(smdl::JIT::MaterialInstance{state, hit.material}, hit);
      continue;
    }
    // A statically opaque material blocks without any material work.
    if (hit.material->isAlwaysOpaque()) return true;
    // Only the ray direction is populated; the LOD fields stay zero so
    // opacity evaluates at full fidelity, the conservative choice for
    // shadow rays.
    auto &state{
        mScratch.shadeHit(hit, mShadowDir, mWavelengths, mAllocator, mTime)};
    if (float opacity{hit.material->evaluateOpacity(state)};
        opacity == 1 || float(mSampler) < opacity) {
      return true; // Blocks visibility!
    }
    // Only an actual pass-through needs the full instance, to keep the
    // medium stack current across the cutout.
    mPassedCutout = true;
    passThrough(smdl::JIT::MaterialInstance{state, hit.material}, hit);
  }
  return false;
}

void VisibilityWalk::passThrough(const smdl::JIT::MaterialInstance &mat,
                                 const Hit &hit) {
  MediumStack::Update(mMedium, mAllocator, mat, hit.instance, -mRay.dir,
                      mRay.dir);
  mRay.tmin = smdl::incrementFloat(mRay.tmax + mParamEps);
  mRay.tmax = 1.0f - mParamEps;
}

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

// The scattering role of a path vertex: a surface BSDF, a volume phase
// function, or the hair BSDF at a curve hit whose material binds
// `material.hair`.
enum class VertexKind { SURFACE, VOLUME, HAIR };

[[nodiscard]]
bool testVisibility(smdl::BumpPtrAllocator &allocator, const Scene &scene,
                    Sampler &sampler, const Color &wavelengths, float time,
                    const MediumStack *medium, PathScratch &scratch,
                    const float3 &point0, const float3 &point1, Color &beta,
                    bool infiniteTarget = false) {
  Hit hit{};
  VisibilityWalk walk{allocator, scene,  sampler, wavelengths,
                      time,      medium, scratch, point0,
                      point1,    beta,   false,   infiniteTarget};
  return walk.nextBlocker(hit) ? false : beta.maxComponent() > 0.0f;
}

[[nodiscard]]
bool scatterEvaluate(Scatterer scatterer, VertexKind kind, const float3 &wo,
                     const float3 &wi, float &pdf, Color &f,
                     int lobeMask = smdl::JIT::DF_ALL) {
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
  // Everything but a volume vertex is a material's, the haze being the
  // one scatterer with no material behind it.
  const auto &mat{scatterer.mat()};
  if (kind == VertexKind::HAIR) {
    return mat.hairScatterEvaluate(wo, wi, pdf, pdfRevUnused, f);
  } else {
    if (!mat.scatterEvaluate(wo, wi, pdf, pdfRevUnused, f)) return false;
    if (lobeMask == smdl::JIT::DF_ALL) return true;
    // The masked value over the UNMASKED density: the mask restricts what
    // is estimated, not the continuation sampler it competes with.
    return mat.scatterEvaluate(wo, wi, pdfFwdUnused, pdfRevUnused, f, lobeMask);
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
bool sampleDiracCrossing(const smdl::JIT::MaterialInstance &mat,
                         Sampler &sampler, const float3 &wPrev, int diracMask,
                         float3 &wi, Color &f, float &chance) {
  float pdfFwd{}, pdfRev{};
  int sampledLobe{};
  chance = 1.0f;
  return mat.scatterSample(float4(sampler), wPrev, wi, pdfFwd, pdfRev, f,
                           sampledLobe, diracMask, &chance) &&
         (sampledLobe & smdl::JIT::DF_DIRAC) != 0;
}

// Everything one manifold connection is weighed against that does not
// change from one connection to the next, which is every input the
// gather has except the connection itself.
class MNEEGather final {
public:
  // Manifold next-event estimation by reflection off a caster; see the
  // definition.
  [[nodiscard]] Color gatherReflection(const MNEEOptions &mneeOptions) const;

  // Manifold next-event estimation through the refractive interfaces
  // blocking the straight shadow segment; see the definition.
  [[nodiscard]] Color gatherRefraction(const MNEEOptions &mneeOptions,
                                       VisibilityWalk &walk, Hit blocker,
                                       int maxDepth, int receiverMask) const;

  // What one converged connection is worth; see the definition.
  // `claimed` sends a Dirac chain down the claimed-exclusive branch,
  // for the biased claimed mode.
  [[nodiscard]] Color contribution(const ManifoldChain &chain,
                                   const ManifoldConnection &connection,
                                   float inverseProbability, int receiverMask,
                                   bool claimed = false) const;

public:
  const Scene &scene;
  Sampler &sampler;
  const Color &wavelengths;
  smdl::BumpPtrAllocator &allocator;
  const LightSampler &lights;
  const smdl::State &gatherState;
  Scatterer scatterer;
  VertexKind kind{};
  const DTree *dtree{};
  float bsdfFraction{};
  const Guiding *guiding{};
  const MediumStack *medium{};

  // The path's scratch, which every walk this gather spawns works in;
  // see `PathScratch`.
  PathScratch &scratch;

  float3 point{};
  float3 wo{};
  const LightSample &lightSample;

private:
  // Draw the offset a glossy crossing is solved for; see the definition.
  [[nodiscard]] bool drawOffset(const ManifoldSurfaces &surfaces,
                                const Hit &hit, const MediumStack *medium,
                                const float3 &wState, const float3 &wo,
                                int lobeMask,
                                ManifoldVertexSeed &vertexSeed) const;

  // The light side of a connection, as the solver's target: an area sample
  // carries the emitter's orientation, which the offset Jacobian needs and
  // a punctual or distant one does not have.
  [[nodiscard]]
  static ManifoldTarget makeManifoldTarget(const LightSample &sample) {
    ManifoldTarget target{};
    target.wl = sample.wi;
    target.point = sample.target;
    target.infinite = sample.isInfinite;
    if (!sample.isDirac && !sample.isInfinite) target.normal = sample.hit.Ng;
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
                           ManifoldStats::Kind statKind,
                           const MNEEOptions &mneeOptions, int receiverMask,
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
Color MNEEGather::gatherReflection(const MNEEOptions &mneeOptions) const {
  const ManifoldTarget target{makeManifoldTarget(lightSample)};
  // One caster for the whole estimate, so that every start is drawn from
  // the same surface with the same material and the same frame, and can
  // re-find what the first walk found. Its selection probability is the
  // one start density the estimate divides out.
  float casterPdf{};
  const auto *caster{mneeOptions.casters->sampleCaster(sampler, casterPdf)};
  if (!caster) return {};
  const SceneManifoldSurfaces surfaces{scene};
  Color result{};
  // One estimate per claimed kind, each with its own constraint (the
  // exact reflection, or a drawn microfacet normal), its own throughput
  // (the masked material query of that kind), and its own reciprocal
  // count. A material mixing both kinds weighs them inside the masked
  // query, so nothing else multiplies.
  for (const int kindLobe :
       {smdl::JIT::DF_DIRAC_BRDF, smdl::JIT::DF_GLOSSY_BRDF}) {
    if ((caster->reflectLobes & kindLobe) == 0) continue;
    // A reflection changes no medium and has no index contrast to
    // resolve, so the two sides weigh the same and `H` is the reflection
    // half vector.
    ManifoldChain chain{};
    chain.count = 1;
    auto &seed{chain[0]};
    seed.etaPrev = seed.etaNext = 1.0f;
    seed.isReflect = true;
    seed.isGlossy = kindLobe == smdl::JIT::DF_GLOSSY_BRDF;
    Hit startHit{};
    if (!mneeOptions.casters->samplePoint(scene, sampler, *caster, startHit))
      continue;
    seed.vertex = vertexOf(startHit);
    seed.frameSeed = manifoldFrameSeed(surfaces, seed.vertex);
    // The half vector is drawn once and held, as on the refractive path:
    // with it fixed the constraint has isolated solutions to recognize. It
    // is expressed in the frame the walk builds from `frameSeed`, which
    // every start of the estimate shares, so it names one world normal
    // throughout.
    if (seed.isGlossy &&
        !drawOffset(surfaces, startHit, /*medium=*/nullptr,
                    startHit.point - point, normalize(point - startHit.point),
                    smdl::JIT::DF_GLOSSY_BRDF, seed))
      continue;
    chain.residualTolerance = reciprocalResidualTolerance(chain);
    const auto statKind{seed.isGlossy ? ManifoldStats::GLOSSY_REFLECT
                                      : ManifoldStats::DIRAC_REFLECT};
    // The first walk starts at the hit the offset was drawn at, which is a
    // start like any other: the offset's density cancels pointwise
    // whichever start it was drawn at, so nothing is gained by discarding
    // that one.
    result += reciprocalEstimate( //
        target, chain, statKind, mneeOptions, smdl::JIT::DF_ALL,
        1.0f / casterPdf, [&](ManifoldChain &reseeded) {
          Hit reseededHit{};
          if (mneeOptions.casters->samplePoint(scene, sampler, *caster,
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
Color MNEEGather::gatherRefraction(const MNEEOptions &mneeOptions,
                                   VisibilityWalk &walk, Hit blocker,
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
  const bool envGated{lightSample.isInfinite &&
                      !mneeOptions.envTarget(lightSample.wi)};
  int lobes{smdl::JIT::DF_DIRAC_BTDF | smdl::JIT::DF_GLOSSY_BTDF};
  // One state for every crossing the discovery walks in turn; see
  // `Hit::applyGeometryToState()`.
  auto state{
      makeRenderState(wavelengths, &allocator, gatherState.animation_time)};
  while (true) {
    if (chain.count == std::min(maxDepth, MANIFOLD_MAX_DEPTH)) return {};
    if (blocker.instance->isCurves()) return {};
    blocker.applyGeometryToState(state, lightSample.wi);
    smdl::JIT::MaterialInstance interfaceMat{state, blocker.material};
    auto &seed{chain[chain.count]};
    seedMedium[chain.count] = walk.medium();
    seedHits[chain.count] = blocker;
    if (!makeManifoldSeed(walk.medium(), interfaceMat, blocker, lightSample.wi,
                          mneeOptions.maxRoughness, seed))
      return {};
    // One estimate per lobe the WHOLE chain claims: the measure now
    // handles a mixed chain, but estimating one would mean claiming it,
    // and the arrival side treats a mixed chain as nobody's (see
    // `MNEECoverage`), so the two policies must move together.
    lobes &= seed.claimedLobes;
    if (lobes == 0) return {};
    if (envGated && (lobes & smdl::JIT::DF_GLOSSY_BTDF) == 0) return {};
    chain.count++;
    walk.passThrough(interfaceMat, blocker);
    if (!walk.nextBlocker(blocker)) break;
  }
  // Decline segments that passed a cutout: the pass is stochastic, and
  // the escape-side cancelation probes coverage with a deterministic
  // cast, so the two must agree on what is covered.
  if (walk.passedCutout()) return {};
  const ManifoldTarget target{makeManifoldTarget(lightSample)};
  const SceneManifoldSurfaces surfaces{scene};
  auto &stats{ManifoldStats::global()};
  Color result{};
  // One estimate per kind the whole chain claims: the Dirac chain,
  // deterministic and weighed against the path tracer by re-walk MIS, and
  // the glossy chain below, claimed outright.
  if (!envGated && (lobes & smdl::JIT::DF_DIRAC_BTDF) != 0) {
    for (int i = 0; i < chain.count; i++) chain[i].isGlossy = false;
    if (mneeOptions.biasedTrials > 0) {
      // The biased claimed mode: exactly `biasedTrials` walks, the
      // first from the straight seed and the rest jittered, the
      // converged solutions clustered and each distinct one summed
      // once at full weight. The arrival side drops every covered
      // arrival at a drawable target, so nothing is weighed twice, and
      // whatever the walks miss is the mode's knowing darkening.
      constexpr int MAX_SOLUTIONS{32};
      std::array<ManifoldConnection, MAX_SOLUTIONS> solutions{};
      int numSolutions{0};
      const auto consider{[&](const ManifoldConnection &other) {
        for (int i = 0; i < numSolutions; i++)
          if (isSameManifoldSolution(point, solutions[i], other)) return;
        if (numSolutions == MAX_SOLUTIONS) return;
        solutions[numSolutions++] = other;
        const Color value{
            contribution(chain, other, 1.0f, receiverMask, /*claimed=*/true)};
        stats.recordContribution(!value.isAllZero());
        result += value;
      }};
      for (int trial = 0; trial < mneeOptions.biasedTrials; trial++) {
        if (trial > 0)
          for (int i = 0; i < chain.count; i++)
            chain[i].seedJitter =
                MANIFOLD_SEED_JITTER * smdl::uniformDiskSample(float2(sampler));
        ManifoldConnection connection{};
        ManifoldWalkReport report{};
        const bool converged{solveManifoldConnection(
            surfaces, point, target, chain, connection, &report)};
        stats.recordWalk(report);
        if (trial == 0)
          stats.recordEstimate(ManifoldStats::DIRAC_REFRACT, converged);
        if (converged) consider(connection);
      }
      stats.recordTrials(ManifoldStats::DIRAC_REFRACT, mneeOptions.biasedTrials,
                         false);
    } else {
      ManifoldConnection connection{};
      ManifoldWalkReport report{};
      const bool converged{solveManifoldConnection(surfaces, point, target,
                                                   chain, connection, &report)};
      stats.recordWalk(report);
      stats.recordEstimate(ManifoldStats::DIRAC_REFRACT, converged);
      if (converged) {
        const Color value{contribution(chain, connection, 1.0f, receiverMask)};
        stats.recordContribution(!value.isAllZero());
        result += value;
      }
    }
  }
  if ((lobes & smdl::JIT::DF_GLOSSY_BTDF) == 0) return result;
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
                    -lightSample.wi, smdl::JIT::DF_GLOSSY_BTDF, chain[i]))
      return result;
  chain.residualTolerance = reciprocalResidualTolerance(chain);
  auto jitter{[&](ManifoldChain &reseeded) {
    for (int i = 0; i < reseeded.count; i++)
      reseeded[i].seedJitter =
          MANIFOLD_SEED_JITTER * smdl::uniformDiskSample(float2(sampler));
    return true;
  }};
  // Unlike the caster seeder, the straight-line crossings are one fixed
  // start, so the first walk is jittered like every trial or the
  // deterministic start would be over-counted.
  (void)jitter(chain);
  result += reciprocalEstimate(target, chain, ManifoldStats::GLOSSY_REFRACT,
                               mneeOptions, receiverMask, 1.0f, jitter);
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
                               bool claimed) const {
  // A finite light illuminates the last crossing, not the receiver, so
  // whatever the light does with direction has to be asked again along
  // the segment that actually arrives: a punctual light's spot cone or
  // IES profile, an area light's EDF and which of its sides faces that
  // way. The distance falloff and the pdf are not that; they carry the
  // straight-line solid-angle measure the estimator is built in, which
  // the transfer Jacobian converts out of, so they stay as they are.
  //
  // A zero here is not a failure to report: the path tracer reads the
  // same radiance off an emitter it reaches through the chain, so both
  // halves of the estimator agree the transport carries nothing.
  const float3 &lastPoint{
      connection.vertices[connection.count - 1].geometry.point};
  Color Li{lightSample.Li};
  if (lightSample.isDirac) {
    if (Li = lights.reevaluatePunctualLi(lightSample, point, lastPoint,
                                         gatherState.meters_per_scene_unit);
        Li.isAllZero())
      return {};
  } else if (!lightSample.isInfinite) {
    if (Li = lights.reevaluateAreaLi(lightSample, gatherState, lastPoint);
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
  if (!scatterEvaluate(scatterer, kind, wo, connection.wr, fPdf, f,
                       receiverMask))
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
  Hit blockerUnused{};
  const MediumStack *segMedium{medium};
  float3 segStart{point};
  // One state for every converged crossing in turn; see
  // `Hit::applyGeometryToState()`.
  auto crossState{
      makeRenderState(wavelengths, &allocator, gatherState.animation_time)};
  for (int i = 0; i < connection.count; i++) {
    const auto &vertex{connection.vertices[i]};
    VisibilityWalk segWalk{allocator,
                           scene,
                           sampler,
                           wavelengths,
                           gatherState.animation_time,
                           segMedium,
                           scratch,
                           segStart,
                           vertex.geometry.point,
                           Tr};
    if (segWalk.nextBlocker(blockerUnused) || !(Tr.maxComponent() > 0.0f))
      return {};
    // The solver's vertex is an address, not a hit record; rebuild the
    // hit to evaluate the interface material at the converged crossing.
    const Hit crossHit{hitOf(scene, vertex.vertex)};
    if (!crossHit.instance) return {};
    crossHit.applyGeometryToState(crossState, -vertex.wPrev);
    smdl::JIT::MaterialInstance crossMat{crossState, crossHit.material};
    segMedium = segWalk.medium();
    crossMat.setExteriorIOR(ExteriorIOR(segMedium, crossMat, vertex.wPrev));
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
    const auto &vertexSeed{chain[i]};
    const int diracMask{vertexSeed.isReflect ? smdl::JIT::DF_DIRAC_BRDF
                                             : smdl::JIT::DF_DIRAC_BTDF};
    const int glossyMask{vertexSeed.isReflect ? smdl::JIT::DF_GLOSSY_BRDF
                                              : smdl::JIT::DF_GLOSSY_BTDF};
    if (vertexSeed.isGlossy) {
      // A glossy crossing has a density, so it is evaluated at the
      // directions the solve produced rather than sampled, masked to the
      // one kind this estimate is for: whatever else the material does in
      // that direction is the ordinary estimators' transport.
      float crossPdfFwd{}, crossPdfRev{};
      Color fCross{};
      if (!crossMat.scatterEvaluate(vertex.wPrev, vertex.wNext, crossPdfFwd,
                                    crossPdfRev, fCross, glossyMask))
        return {};
      beta *= fCross;
      if (beta.isAllZero()) return {};
    } else {
      float3 wiDirac{};
      float vertexChance{};
      Color fDirac{};
      if (!sampleDiracCrossing(crossMat, sampler, vertex.wPrev, diracMask,
                               wiDirac, fDirac, vertexChance))
        return {};
      // The constraint was solved for this crossing, so the material has
      // to agree that it scatters that way. A disagreement means the
      // interface is not the one the solve differentiated.
      if (!(dot(wiDirac, vertex.wNext) > 1.0f - 1e-3f)) return {};
      beta *= fDirac;
      // Only the BSDF branch of the one-sample MIS can produce a Dirac
      // direction, so where a guiding cell participates at the interface
      // the continuation's chance of this chain carries that branch's
      // discrete weight besides the material's own selection.
      chainChance *=
          vertexChance *
          diracBranchChance(guiding, vertex.geometry.point,
                            (dfLobesOf(crossMat) & smdl::JIT::DF_FINITE) != 0);
      if (!(chainChance > 0.0f) || beta.isAllZero()) return {};
    }
    // A reflection stays on the side it arrived from, so it crosses no
    // boundary and the nested medium is the one it was already in.
    if (!chain[i].isReflect)
      MediumStack::Update(segMedium, allocator, crossMat, crossHit.instance,
                          vertex.wPrev, vertex.wNext);
    segStart = vertex.geometry.point;
  }
  const float3 lightPoint{lightSample.isInfinite
                              ? segStart + (lightSample.target - point)
                              : lightSample.target};
  VisibilityWalk lightWalk{
      allocator, scene,   sampler,  wavelengths, gatherState.animation_time,
      segMedium, scratch, segStart, lightPoint,  Tr};
  if (lightWalk.nextBlocker(blockerUnused) || !(Tr.maxComponent() > 0.0f))
    return {};
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
  Color direct{f * Tr * Li * beta * (measure / lightSample.pdf)};
  if (claimed || chain[0].isReflect || anyGlossy) {
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
    // discrete Fresnel transmissions and the transfer Jacobian. A Dirac
    // light is unreachable by the continuation, so its MIS weight is 1,
    // matching the plain branch.
    if (!lightSample.isDirac) {
      float escapePdf{
          guidedContinuationPdf(dtree, bsdfFraction, connection.wr, fPdf) *
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
  auto &state{scratch.shadeHit(hit, wState, wavelengths, allocator,
                               gatherState.animation_time)};
  smdl::JIT::MaterialInstance offsetMat{state, hit.material};
  if (medium) offsetMat.setExteriorIOR(ExteriorIOR(medium, offsetMat, wo));
  if (!(dot(vertexSeed.frameSeed, vertexSeed.frameSeed) > 0.0f))
    vertexSeed.frameSeed = manifoldFrameSeed(surfaces, vertexSeed.vertex);
  float3 normal{}, t1{}, t2{};
  if (!manifoldSeedFrame(surfaces, vertexSeed.vertex, vertexSeed.frameSeed,
                         normal, t1, t2))
    return false;
  // The internal frame's z axis is the geometric normal, so the side of
  // the query is the geometric side `wo` is on.
  const bool backface{dot(wo, hit.Ng) < 0.0f};
  float3 wm{};
  float pdf{};
  float2 alpha{};
  if (!offsetMat.scatterNormalSample(float4(sampler), backface, wm, pdf, alpha,
                                     lobeMask) ||
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
                                     ManifoldStats::Kind statKind,
                                     const MNEEOptions &mneeOptions,
                                     int receiverMask, float scale,
                                     const Reseed &reseed) const {
  const SceneManifoldSurfaces surfaces{scene};
  auto &stats{ManifoldStats::global()};
  auto solve{[&](ManifoldConnection &connection) {
    ManifoldWalkReport report{};
    const bool ok{solveManifoldConnection(surfaces, point, target, chain,
                                          connection, &report)};
    stats.recordWalk(report);
    return ok;
  }};
  ManifoldConnection connection{};
  const bool firstConverged{solve(connection)};
  stats.recordEstimate(statKind, firstConverged);
  if (mneeOptions.biasedTrials > 0) {
    // The biased variant: exactly `biasedTrials` walks, the first on
    // the chain as handed over and the rest reseeded, clustering the
    // converged solutions and summing each distinct one once; see
    // `MNEEOptions::biasedTrials`. Distinct solutions past the
    // cluster cap are dropped rather than summed unclustered, so a
    // re-find can never double-count; a surface with more solutions in
    // reach than the cap needs the walk count raised far past it anyway.
    constexpr int MAX_SOLUTIONS{32};
    std::array<ManifoldConnection, MAX_SOLUTIONS> solutions{};
    int numSolutions{0};
    Color sum{};
    const auto consider{[&](const ManifoldConnection &other) {
      for (int i = 0; i < numSolutions; i++)
        if (isSameManifoldSolution(point, solutions[i], other)) return;
      if (numSolutions == MAX_SOLUTIONS) return;
      solutions[numSolutions++] = other;
      const Color value{contribution(chain, other, scale, receiverMask)};
      stats.recordContribution(!value.isAllZero());
      sum += value;
    }};
    if (firstConverged) consider(connection);
    for (int trial = 1; trial < mneeOptions.biasedTrials; trial++) {
      ManifoldConnection other{};
      if (reseed(chain) && solve(other)) consider(other);
    }
    stats.recordTrials(statKind, mneeOptions.biasedTrials, false);
    return sum;
  }
  if (!firstConverged) return {};
  // Weigh the solution before the trials, so a worthless one (its light
  // segment blocked, its transport zero) costs none: the estimate is
  // linear in the trial count, so the expectation does not care, and a
  // wall whose reflection of the light is blocked by another interface
  // converges on every walk and would otherwise spend the whole trial
  // budget on nothing.
  const Color value{contribution(chain, connection, scale, receiverMask)};
  stats.recordContribution(!value.isAllZero());
  if (value.isAllZero()) return {};
  int trials{};
  float inverseProbability{};
  if (manifoldReciprocal(point, connection, mneeOptions.maxTrials, trials,
                         inverseProbability, [&](ManifoldConnection &other) {
                           return reseed(chain) && solve(other);
                         })) {
    stats.recordTrials(statKind, trials, false);
    return inverseProbability * value;
  }
  stats.recordTrials(statKind, mneeOptions.maxTrials, true);
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
  // `enabled` is false when manifold NEE is off, which leaves the state
  // permanently disarmed.
  void arm(bool enabled, const float3 &point, float pdf,
           const MediumStack *medium) noexcept {
    mArmed = enabled;
    mChainLength = 0;
    mChainKind = ChainKind::NONE;
    mChainShare = Color(1.0f);
    mReceiver = point;
    mReceiverPdf = pdf;
    mReceiverMedium = medium;
  }

  void disarm() noexcept { mArmed = false; }

  [[nodiscard]] bool isArmed() const noexcept { return mArmed; }

  // Extend the chain across a claimed transmission: Dirac, or glossy with
  // the share of the crossing's throughput its claimed lobe carries. The
  // length keeps counting past `MANIFOLD_MAX_DEPTH` so that an overlong chain
  // reads as uncovered rather than as a shorter one.
  void extend(const Hit &hit, bool glossy, const Color &claimedShare) noexcept {
    if (mChainLength < MANIFOLD_MAX_DEPTH) {
      mChainInstances[mChainLength] = hit.instance;
      mChainPieces[mChainLength] = hit.faceIndex;
      mChainHits[mChainLength] = hit;
    }
    mChainLength++;
    const auto kind{glossy ? ChainKind::GLOSSY : ChainKind::DIRAC};
    mChainKind = mChainKind == ChainKind::NONE || mChainKind == kind
                     ? kind
                     : ChainKind::MIXED;
    if (glossy) mChainShare *= claimedShare;
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
                                    bool prevDirac) const noexcept {
    ManifoldClaim reachable{};
    if (!mArmed) return reachable;
    if (!prevDirac) reachable.reflectLobes = claim.reflectLobes;
    if (mChainLength < mneeOptions.depth) {
      switch (mChainKind) {
      case ChainKind::NONE:
        reachable.refractLobes = claim.refractLobes;
        break;
      case ChainKind::DIRAC:
        reachable.refractLobes = claim.refractLobes & smdl::JIT::DF_DIRAC_BTDF;
        break;
      case ChainKind::GLOSSY:
        reachable.refractLobes = claim.refractLobes & smdl::JIT::DF_GLOSSY_BTDF;
        break;
      case ChainKind::MIXED:
        break;
      }
    }
    return reachable;
  }

  [[nodiscard]] const float3 &receiver() const noexcept { return mReceiver; }

  // The MIS weight of a BSDF-side arrival at `target` through the
  // chain, by re-walk MIS; see the definition.
  [[nodiscard]] float coverWeight(const Scene &scene, Sampler &sampler,
                                  const Color &wavelengths, float time,
                                  smdl::BumpPtrAllocator &allocator,
                                  const ManifoldTarget &target, float lightPdf,
                                  const MNEEOptions &mneeOptions,
                                  const Guiding *guiding) const;

private:
  [[nodiscard]] bool covers(ChainKind kind,
                            const MNEEOptions &mneeOptions) const noexcept {
    return mArmed && mChainKind == kind && mChainLength >= 1 &&
           mChainLength <= mneeOptions.depth;
  }

  bool mArmed{};
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
// chain family, a different fold solution, a failed walk, a cutout in
// the way, or a light the sampler cannot draw) the arrival keeps
// weight 1 instead of silently losing its transport. The competing
// densities are per unit solid angle of the straight line toward the
// light: the gather's is the light sampling density, the arrival's is
// the receiver's recorded continuation density times the interfaces'
// own selection chances times the transfer Jacobian; the gather applies
// the complementary weight with the same formula, so the pair sums to
// one.
//
// That last factor is taken from the walk run here rather than
// re-evaluated on the crossings the path actually took. The two agree
// only to the convergence tolerance, and the pair sums to one exactly
// when both sides weigh the same number, so the number to weigh by is
// the one the gather would compute: this walk IS the gather's walk, for
// this target.
float MNEECoverage::coverWeight(const Scene &scene, Sampler &sampler,
                                const Color &wavelengths, float time,
                                smdl::BumpPtrAllocator &allocator,
                                const ManifoldTarget &target, float lightPdf,
                                const MNEEOptions &mneeOptions,
                                const Guiding *guiding) const {
  const float3 &receiver{mReceiver};
  const MediumStack *receiverMedium{mReceiverMedium};
  const float receiverPdf{mReceiverPdf};
  const auto &chainHits{mChainHits};
  const auto &chainInstances{mChainInstances};
  const auto &chainPieces{mChainPieces};
  const int chainLength{mChainLength};
  // Whether the re-walk reproduced the crossings the path took; every
  // other exit keeps the arrival at weight 1, so the matched fraction
  // is the share of covered arrivals the gather can ever claim.
  bool matched{false};
  SMDL_DEFER([&] { ManifoldStats::global().recordCover(matched); });
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
  float3 origin{receiver};
  // One state for every interface the cast crosses in turn, and for the
  // crossings re-asked below; see `Hit::applyGeometryToState()`.
  auto state{makeRenderState(wavelengths, &allocator, time)};
  bool reached{false};
  for (int skip = 0; skip < 64; skip++) {
    float tmax{INF};
    if (!target.infinite) {
      tmax = length(target.point - origin) - EPS;
      if (!(tmax > EPS)) {
        reached = true;
        break;
      }
    }
    Ray ray{origin, wl, EPS, tmax};
    Hit hit{};
    if (!scene.intersect(ray, hit)) {
      reached = true;
      break;
    }
    hit.applyGeometryToState(state, wl);
    smdl::JIT::MaterialInstance interfaceInst{state, hit.material};
    if (hit.material->isNullInterface()) {
      MediumStack::Update(medium, allocator, interfaceInst, hit.instance,
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
                          mneeOptions.maxRoughness, chain[chain.count]) ||
        (chain[chain.count].claimedLobes & smdl::JIT::DF_DIRAC_BTDF) == 0)
      return 1.0f;
    crossingMedium[chain.count] = medium;
    chain.count++;
    MediumStack::Update(medium, allocator, interfaceInst, hit.instance,
                        woStraight, wl);
    origin = hit.point;
  }
  if (!reached || chain.count != chainLength) return 1.0f;
  ManifoldConnection connection{};
  ManifoldWalkReport report{};
  const SceneManifoldSurfaces surfaces{scene};
  const bool converged{solveManifoldConnection(surfaces, receiver, target,
                                               chain, connection, &report)};
  ManifoldStats::global().recordRewalk(report);
  if (!converged) return 1.0f;
  for (int i = 0; i < chainLength; i++) {
    const float scale{std::max(1e-3f, length(chainHits[i].point - receiver))};
    if (!(length(connection.vertices[i].vertex.point - chainHits[i].point) <
          MANIFOLD_IDENTITY_FRACTION * scale))
      return 1.0f;
  }
  matched = true;
  const float transfer{connection.measure(chain)};
  // The chance the continuation takes this chain, asked of each
  // interface rather than recomputed here, so that a layered or tinted
  // interface reports the selection it actually makes. The gather
  // accumulates the same quantity the same way, drawn from its own
  // sampler, which is what keeps the pair summing to one.
  float Q{1.0f};
  float3 prev{receiver};
  for (int i = 0; i < chainLength; i++) {
    const float3 toHit{chainHits[i].point - prev};
    const float d{length(toHit)};
    if (!(d > 0.0f)) return 1.0f;
    const float3 travel{toHit / d};
    chainHits[i].applyGeometryToState(state, travel);
    smdl::JIT::MaterialInstance crossMat{state, chainHits[i].material};
    crossMat.setExteriorIOR(ExteriorIOR(crossingMedium[i], crossMat, -travel));
    float3 wiDirac{};
    float vertexChance{};
    Color fDirac{};
    if (!sampleDiracCrossing(crossMat, sampler, -travel,
                             smdl::JIT::DF_DIRAC_BTDF, wiDirac, fDirac,
                             vertexChance))
      return 1.0f;
    // The gather folds the same guiding branch chance into its
    // `chainChance` at its converged crossing, so the pair keeps
    // weighing the same number.
    Q *= vertexChance *
         diracBranchChance(guiding, chainHits[i].point,
                           (dfLobesOf(crossMat) & smdl::JIT::DF_FINITE) != 0);
    prev = chainHits[i].point;
  }
  const float arrivalPdf{receiverPdf * Q * transfer};
  if (!(arrivalPdf > 0.0f) || !std::isfinite(arrivalPdf)) return 1.0f;
  return smdl::powerHeuristic(arrivalPdf, lightPdf);
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
// With `mneeOptions.depth > 0`, a light sample whose straight segment is
// blocked by claimed refractive interfaces routes through
// `MNEEGather::gatherRefraction()` instead of reading as
// occluded, and the reflective gather searches the marked casters besides. Hair
// vertices keep plain gathering: the manifold estimator's MIS is not wired
// through the hair BSDF.
//
// `receiver` is whether this vertex is one the gathers run from at all
// (`isManifoldReceiver()`); the caller arms the MNEE coverage by the
// same answer, so what is gathered here is exactly what is claimed behind.
// `claim` is what the manifold estimators claim at THIS vertex and
// `armedBehind` whether a receiver behind it ran a gather. Where both
// hold, the claimed lobes are the receiver's gather's to estimate, so
// light sampling here covers the other lobes only, weighed against the
// unmasked continuation density exactly as the continuation's arrivals
// are weighed for those lobes (`tracePath()` keeps their share of each
// arrival and drops the claimed share).
[[nodiscard]]
Color gatherDirect(const Scene &scene, Sampler &sampler,
                   const Color &wavelengths, smdl::BumpPtrAllocator &allocator,
                   const LightSampler &lights, const smdl::State &gatherState,
                   Scatterer scatterer, VertexKind kind, const DTree *dtree,
                   float bsdfFraction, const Guiding *guiding,
                   const MediumStack *medium, PathScratch &scratch,
                   const float3 &point, const float3 &wo,
                   const MNEEOptions &mneeOptions,
                   const ManifoldClaim &manifoldClaim = {},
                   bool armedBehind = false, bool receiver = true) {
  const auto mneeDepth{kind == VertexKind::HAIR ? 0 : mneeOptions.depth};
  const bool runManifold{mneeDepth > 0 && receiver};
  Color direct{};
  if (lights.empty()) return direct;
  LightSample lightSample{};
  // A manifold gather keeps the samples that radiate nothing toward the
  // receiver: its connection arrives at the light from elsewhere and reads
  // the radiance from there. The plain estimate of such a sample is zero
  // and is skipped below.
  if (lights.sample(gatherState, sampler, point, lightSample, runManifold)) {
    const MNEEGather mneeGather{
        scene,     sampler, wavelengths, allocator,    lights,  gatherState,
        scatterer, kind,    dtree,       bsdfFraction, guiding, medium,
        scratch,   point,   wo,          lightSample};
    // The reflect claims belong to the reflective gather, which the
    // layout's light marks may restrict to the caustic targets: toward
    // any other light the claimed reflections are ordinary transport
    // again, here and at the arrivals. The transmit claims are the
    // refractive chains', which run for every light.
    ManifoldClaim lightClaim{manifoldClaim};
    if (!lightSample.caustic) lightClaim.reflectLobes = 0;
    const bool split{armedBehind && !lightClaim.empty()};
    const int neeMask{split ? (smdl::JIT::DF_ALL & ~lightClaim.lobes())
                            : smdl::JIT::DF_ALL};
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
          !scatterEvaluate(scatterer, kind, wo, lightSample.wi, fPdf, f,
                           neeMask))
        return;
      const float continuationPdf{
          guidedContinuationPdf(dtree, bsdfFraction, lightSample.wi, fPdf)};
      Color D{f * Tr * lightSample.Li / lightSample.pdf};
      if (D.isAnyNonFinite()) return;
      // A Dirac light is unreachable by the continuation, so its MIS
      // weight is 1.
      if (!lightSample.isDirac)
        D *= smdl::powerHeuristic(lightSample.pdf, continuationPdf);
      direct += D;
    }};
    if (!runManifold) {
      if (Color Tr{Color(1.0f)};
          neeMask != 0 &&
          testVisibility(allocator, scene, sampler, wavelengths,
                         gatherState.animation_time, medium, scratch, point,
                         lightSample.target, Tr, lightSample.isInfinite)) {
        gatherPlain(Tr);
      }
    } else {
      // Visibility first: whether the straight segment is clear decides
      // which estimator runs, and the manifold connection does not care
      // whether the straight direction can scatter at the receiver.
      Color Tr{1.0f};
      Hit blocker{};
      VisibilityWalk walk{allocator,
                          scene,
                          sampler,
                          wavelengths,
                          gatherState.animation_time,
                          medium,
                          scratch,
                          point,
                          lightSample.target,
                          Tr,
                          /*needBlocker=*/true,
                          lightSample.isInfinite};
      if (!walk.nextBlocker(blocker)) {
        if (Tr.maxComponent() > 0.0f) gatherPlain(Tr);
      } else {
        // The chain gather of the receiver behind this vertex already
        // claims every chain that starts with this vertex's claimed
        // transmission kinds, so this gather weighs its connections with
        // the rest of the BSDF only.
        direct += mneeGather.gatherRefraction(
            mneeOptions, walk, blocker, mneeDepth,
            smdl::JIT::DF_ALL & ~manifoldClaim.refractLobes);
      }
      // The reflective gather is additive rather than an alternative: a
      // mirror is nowhere near the line to the light, so whether that line
      // is clear says nothing about whether there is a reflection to find.
      // It searches toward the caustic targets only; see
      // `LightSample::caustic`.
      if (mneeOptions.casters && !mneeOptions.casters->empty() &&
          lightSample.caustic)
        direct += mneeGather.gatherReflection(mneeOptions);
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

// What an arrival at a light is worth, per wavelength, as a factor on the
// throughput times the radiance: the ordinary MIS `weight` on the share
// nobody claims, and nothing on the claimed share, which is left to the
// gather that claimed it. The Dirac chain arrival is claimed whole and
// weighed by re-walk MIS instead; see `addArrival` in `tracePath()`.
[[nodiscard]] Color arrivalFactor(float weight, const Color &claimedShare) {
  return (Color(1.0f) - claimedShare) * weight;
}

// The share of one bounce's throughput the manifold estimators claim,
// per wavelength, given what the gathers behind this vertex can reach:
// all of a Dirac reflection of a claimed kind, and of a finite bounce
// the part of its value the claimed lobes carry, which is the value
// without them over the value with them. A Dirac transmission is not a
// share but a chain, weighed against by re-walk MIS, and reports zero
// here.
[[nodiscard]]
Color claimedShareOf(const smdl::JIT::MaterialInstance &mat,
                     const ManifoldClaim &reachable, const float3 &wo,
                     const float3 &wNext, const Color &f, bool isDiracBounce,
                     bool transmits, int sampledLobe) {
  Color claimedShare{};
  if (reachable.empty()) return claimedShare;
  if (isDiracBounce) {
    if (!transmits &&
        (sampledLobe & reachable.reflectLobes & smdl::JIT::DF_DIRAC_BRDF) != 0)
      claimedShare = Color(1.0f);
    return claimedShare;
  }
  if ((transmits ? reachable.refractLobes & smdl::JIT::DF_GLOSSY_BTDF
                 : reachable.reflectLobes & smdl::JIT::DF_GLOSSY_BRDF) == 0)
    return claimedShare;
  float pdfUnclaimed{}, pdfRevUnused{};
  Color fUnclaimed{};
  if (mat.scatterEvaluate(wo, wNext, pdfUnclaimed, pdfRevUnused, fUnclaimed,
                          smdl::JIT::DF_ALL & ~reachable.lobes()))
    for (size_t b = 0; b < claimedShare.size(); b++)
      claimedShare[b] =
          f[b] > 0.0f ? std::clamp(1.0f - fUnclaimed[b] / f[b], 0.0f, 1.0f)
                      : 0.0f;
  else
    claimedShare = Color(1.0f);
  return claimedShare;
}

} // namespace

Color tracePath(smdl::Compiler &compiler, smdl::BumpPtrAllocator &allocator,
                const Scene &scene, Sampler &sampler, const Color &wavelengths,
                Ray ray, float time, float cameraWeight, float cameraConeAngle,
                const MediumStack *exteriorMedium, const smdl::Haze *haze,
                const LightSampler &lightSampler,
                const MNEEOptions &mneeOptions, const PathOptions &pathOptions,
                const Guiding *guiding, GuideRecord *records,
                uint64_t &numRecords) {
  numRecords = 0;
  Color L{};
  const EnvLight *envLight{lightSampler.env()};
  const STree *stree{guiding ? guiding->tree : nullptr};

  // The walk starts on the exterior of all scene geometry, inside the
  // scene-wide exterior medium if the composition names one (the bottom
  // of the nested-medium stack, owned by the caller), else in vacuum.
  const MediumStack *medium{exteriorMedium};

  // The scratch this path and every shadow ray it spawns work in,
  // retargeted rather than rebuilt, so that a path scattering
  // repeatedly inside one medium resolves it once and the surfaces its
  // shadow rays pass through shade in one state; see `PathScratch`.
  PathScratch scratch{};
  scratch.medium.setHaze(haze);

  // The pristine gather-side state, see `gatherDirect()`.
  const auto gatherState{makeRenderState(wavelengths, &allocator, time)};
  // Set up the state variables that never change; the geometric ones are
  // updated at every vertex by `Hit::apply_geometry_to_state()`.
  auto state{makeRenderState(wavelengths, &allocator, time)};

  Color beta{Color(cameraWeight)};
  Color f{};
  float wpdf{};
  // The JIT ABI reports a reverse PDF alongside every forward PDF, which a
  // forward path tracer never consumes; every call shares this sink.
  float wpdfRevUnused{};
  // The path LOD context. The camera carries the per-pixel cone spread,
  // and an all-zero context stays all-zero, which is "LOD off" per the
  // `State` conventions.
  int order{};
  float travel{};
  float spread{cameraConeAngle};
  float width{};
  // The bounce that produced the current segment, for the MIS weight when
  // the segment lands on an emitter or escapes: the density that sampled
  // the direction, whether it came from a Dirac lobe (whose hits carry
  // weight 1, since light sampling can never produce them), and the vertex
  // it left from (which anchors the solid-angle conversion of the
  // competing light-sample density; cutout hops re-base `ray.org`, so the
  // ray origin cannot serve). Meaningless on the camera segment, which no
  // light sampling competes with.
  float wpdfPrev{};
  bool prevDirac{};
  // The share of the current segment's throughput the manifold estimators
  // claim, per wavelength: what an arrival at a light along it must drop,
  // since a gather behind produces it. Zero on a segment nobody claims.
  // A reflect share is the reflective gather's, which the light marks
  // may restrict to the caustic targets, so it applies only to arrivals
  // at one of those; a glossy chain share is the refractive gather's
  // and applies to any.
  Color prevClaimedShare{};
  bool prevShareCausticOnly{};
  float3 prevPoint{};
  MNEECoverage mneeCoverage{};
  // The clamp scale of a contribution with the given number of bounces:
  // 1 outside the contribution bound's reach, else what scales the
  // largest band down to the bound. Applied to what a contribution adds
  // to the estimate and to what the guide record retains of it, so the
  // tree trains toward the clamped field it steers.
  const auto clampScale{
      [&](const Color &contribution, uint64_t bounces) noexcept -> float {
        if (!(pathOptions.maxContribution > 0.0f) ||
            bounces < uint64_t(pathOptions.maxContributionBounces))
          return 1.0f;
        const float maxValue{contribution.maxComponent()};
        return maxValue > pathOptions.maxContribution
                   ? pathOptions.maxContribution / maxValue
                   : 1.0f;
      }};
  // Has the walk scattered as often as it may? Asked at every vertex
  // once its arrival is in: the vertex's own gather would be one bounce
  // deeper than the bound allows.
  const auto atMaxBounces{[&](uint64_t depth) noexcept {
    return depth - 1 > pathOptions.maxBounces;
  }};
  // Terminate by Russian roulette instead of by a fixed depth limit, so
  // that high-albedo transport keeps the energy it is entitled to.
  // Returns whether the walk continues, scaling the throughput by the
  // reciprocal survival when it does. Asked at every scattering vertex,
  // volume as well as surface: an unbounded medium of high albedo goes
  // on scattering indefinitely otherwise, and the deep vertices cost a
  // gather apiece to carry a throughput roulette would have retired.
  const auto rouletteSurvives{[&](uint64_t depth, const DTree *dtree,
                                  float gate = 1.0f) noexcept -> bool {
    if (!pathOptions.roulette || depth <= ROULETTE_MIN_DEPTH) return true;
    float survival{};
    const float meanRadiance{
        dtree && guiding->pixelEstimate > 0 ? dtree->meanRadiance() : 0};
    if (meanRadiance > 0) {
      // Adjoint-driven Russian roulette (Vorba & Krivanek, SIGGRAPH
      // 2016; roulette only, no splitting): survive in proportion to the
      // expected pixel contribution of continuing the walk, which is the
      // throughput times the SD-tree's cached mean incident radiance,
      // relative to the pixel's estimate from the previous pass.
      survival = std::clamp(
          beta.average() * meanRadiance / guiding->pixelEstimate, 0.05f, 1.0f);
    } else {
      survival = std::min(ROULETTE_MAX_SURVIVAL, beta.maxComponent());
    }
    if (!(survival < gate)) return true;
    if (!(float(sampler) < survival)) return false;
    beta *= 1.0f / survival;
    return true;
  }};
  // Weigh an arrival at a light, an environment escape or an emitter hit,
  // and fold it into the estimate and the guide record. Through a Dirac
  // chain of claimed refractive interfaces the arrival competes with the
  // manifold gather at the chain's receiver by re-walk MIS (the chain
  // always follows a Dirac transmission, so the weight it replaces is the
  // `prevDirac` 1), and under the biased claimed mode it is dropped
  // outright instead, the gather's multi-seed estimate having claimed
  // every drawable target exclusively; otherwise the ordinary MIS
  // `weight` applies to the share of the segment's throughput nobody
  // claims, and a claimed share toward a target its gather reaches is
  // the gather's outright; see `arrivalFactor()`. `makeCoverTarget`
  // fills the target the re-walk aims at and returns the light-sampling
  // density it competes with, negative when there is no target to
  // re-walk, which keeps the ordinary weight.
  const auto addArrival{[&](const Color &Li, float weight, uint64_t bounces,
                            bool causticTarget, GuideRecord *record,
                            const auto &makeCoverTarget) {
    Color factor{};
    if (mneeCoverage.coversDirac(mneeOptions)) {
      ManifoldTarget target{};
      const float lightPdf{makeCoverTarget(target)};
      if (lightPdf > 0.0f && mneeOptions.biasedTrials > 0) return;
      factor = Color(lightPdf >= 0.0f
                         ? mneeCoverage.coverWeight(
                               scene, sampler, wavelengths, time, allocator,
                               target, lightPdf, mneeOptions, guiding)
                         : weight);
    } else {
      factor = arrivalFactor(weight, prevShareCausticOnly && !causticTarget
                                         ? Color(0.0f)
                                         : prevClaimedShare);
    }
    auto contribution{beta * Li * factor};
    if (contribution.isAnyNonFinite()) return;
    const float scale{clampScale(contribution, bounces)};
    if (scale < 1.0f) contribution *= scale;
    L += contribution;
    foldArrivalIntoRecord(record, beta, Li * factor * scale);
  }};
  // The number of path vertices so far, counting the camera as the first.
  // The walk ends by escape, absorption, roulette, or the bounce bound,
  // never by this loop's own condition.
  uint64_t depth{1};
  while (true) {
    auto hit{Hit{}};
    bool hitSurface{scene.intersect(ray, hit)};
    // The stack being empty is the exterior segment, and with no haze
    // it is vacuum, the common case: the view is left alone rather than
    // resolved to nothing, which would still walk the stack.
    if (medium || scratch.medium.hasHaze()) {
      scratch.medium.reset(medium, wavelengths, time, ray.org, ray.dir);
      if (scratch.medium.hasMedium()) {
        // Sample a free-flight distance over the cast, which
        // `Scene::intersect` bounded at the hit parameter (or left
        // unbounded on a miss). The medium weighs `beta` itself: the
        // scattering weight on an event, the transmittance weight on
        // surviving to the surface or escape. The medium's own emission
        // along the segment accumulates separately, weighted by the
        // throughput from before the segment, and lands at weight 1:
        // light sampling never competes with it, the same as an
        // unregistered emitter. No guide record retains it, since the
        // trainer learns the reflected field.
        float t{};
        Color emitted{};
        const Color betaStart{beta};
        const bool scattered{
            scratch.medium.sampleDistance(sampler, ray.tmax, t, beta, emitted)};
        const Color Lemit{betaStart * emitted};
        if (!Lemit.isAnyNonFinite()) L += Lemit;
        if (scattered) {
          // A volume scattering event.
          ++depth;
          ++order;
          travel += t;
          width += spread * t;
          const float3 point{ray(t)};
          const float3 wo{-ray.dir};
          GuideRecord *record{records ? &records[numRecords++] : nullptr};
          if (record) {
            *record = GuideRecord{};
            record->point = point;
            record->beta = beta;
          }
          if (atMaxBounces(depth)) break;
          // The phase function of the vertex: the haze's own, the
          // medium's, or with additive overlap the component the
          // collision picked. Whatever it names outlives the view, so
          // the gather below is free to retarget the view.
          const Scatterer phase{scratch.medium.scatterer()};
          {
            // The SD-tree never participates at volume vertices, so the
            // continuation density the gather weighs against is the
            // phase function alone.
            Color direct{gatherDirect(scene, sampler, wavelengths, allocator,
                                      lightSampler, gatherState, phase,
                                      VertexKind::VOLUME, /*dtree=*/nullptr,
                                      /*bsdfFraction=*/1.0f, guiding, medium,
                                      scratch, point, wo, mneeOptions)};
            if (const float scale{clampScale(beta * direct, depth - 1)};
                scale < 1.0f)
              direct *= scale;
            L += beta * direct;
            if (record) record->direct = direct;
          }
          // Sample the vertex's phase function. It returns the phase
          // value, which is also the solid-angle PDF of having sampled
          // it, so the throughput weight is exactly 1 and `beta` is
          // unchanged.
          float3 wNext{};
          float phaseValue{
              phase.volumeScatterSample(float4(sampler), wo, wNext)};
          if (!(phaseValue > 0)) break;
          if (record) {
            record->wNext = wNext;
            record->wNextPdf = phaseValue;
          }
          wpdfPrev = phaseValue;
          prevDirac = false;
          prevPoint = point;
          // A volume vertex is a manifold-NEE receiver like any other.
          mneeCoverage.arm(mneeOptions.any(), point, phaseValue, medium);
          // Phase functions scatter wide, so grow the cone like a
          // diffuse bounce.
          spread = std::min(spread + ANGLE_GROWTH_DIFFUSE, ANGLE_MAX);
          // No SD-tree steers a volume vertex, so the throughput is the
          // only thing the roulette can weigh here.
          if (!rouletteSurvives(depth, /*dtree=*/nullptr, ROULETTE_VOLUME_GATE))
            break;
          ray = Ray{point, wNext, EPS, INF};
          continue;
        }
      }
    }
    if (!hitSurface) {
      // The walk escaped the scene: the segment is the BSDF-sampling half
      // of the MIS pair, so add the environment weighted against what the
      // light-sampling gather at the previous vertex would have produced.
      // No pdf gate on the radiance: with MIS compensation the environment
      // sampling density is zero below the mean radiance, but the radiance
      // is not; those directions are exactly the ones this half alone must
      // cover, at weight 1, which is what the power heuristic degrades to.
      if (envLight) {
        float Lipdf{};
        Color Li{envLight->Li(compiler, gatherState, ray.dir, Lipdf)};
        float weight{
            depth == 1 || prevDirac
                ? 1.0f
                : smdl::powerHeuristic(wpdfPrev,
                                       lightSampler.envSelectionPMF() * Lipdf)};
        // The re-walk's `coverWeight` returns 1 whenever the gather
        // cannot produce this transport, which covers the dim sky the
        // compensated environment sampler never draws (`Lipdf` zero),
        // fold solutions the walk does not find, and failed walks. A
        // sun-gated sky arrival reports no target at all, so it keeps
        // its ordinary weight without spending a re-walk; the gather
        // side stands down by the same predicate.
        addArrival(Li, weight, depth - 1, lightSampler.causticEnv(),
                   records && numRecords > 0 ? &records[numRecords - 1]
                                             : nullptr,
                   [&](ManifoldTarget &target) {
                     if (!mneeOptions.envTarget(ray.dir)) return -1.0f;
                     target.wl = ray.dir;
                     return lightSampler.envSelectionPMF() * Lipdf;
                   });
      }
      if (records) {
        records[numRecords] = GuideRecord{};
        records[numRecords].isInfiniteLight = true;
        ++numRecords;
      }
      break;
    }

    // The distance traveled by this cast, which the cone widens over. A
    // cutout passthrough below is not a scattering event, so it commits
    // the distance and width but not the order.
    const float castDistance{ray.tmax};
    hit.applyGeometryToState(state, ray.dir);
    state.scattering_order = order + 1;
    state.travel_distance = travel + castDistance;
    state.cone_angle = spread;
    state.cone_width = width + spread * castDistance;
    // Reseed the stochastic-evaluation generator at every vertex, so
    // stochastically evaluated BSDFs decorrelate across bounces, samples,
    // and pixels while staying deterministic for a given sampler state.
    const uint64_t seedHi{sampler.nextBits()};
    const uint64_t seedLo{sampler.nextBits()};
    state.rng = smdl::RNG((seedHi << 32) | seedLo, uint64_t(order));
    auto mat{smdl::JIT::MaterialInstance(state, hit.material)};
    mat.setExteriorIOR(ExteriorIOR(medium, mat, -ray.dir));
    // A hair vertex: a curve hit whose material binds `material.hair`,
    // which routes scattering through the hair entry points. A curve hit
    // whose material has no hair keeps the ordinary surface path, and a
    // hair material on non-curve geometry shades as its (typically
    // default) surface. Hair fibers are also not medium boundaries:
    // transmission through the fiber is part of the BSDF, so the
    // null-interface hop and the medium-stack bookkeeping stand down.
    const bool isHair{hit.instance->isCurves() && hit.material->hasHair()};
    // A null interface, a boundary that scatters nothing itself but
    // encloses a participating medium (e.g., a smoke container), and a
    // cutout the opacity draw passes both hop straight through:
    // committing the distance and width but not the order, with only the
    // medium-stack bookkeeping.
    if (const bool hopsThrough{[&] {
          if (hit.material->isNullInterface() && !isHair) return true;
          const float opacity{mat.getCutoutOpacity()};
          return opacity < 1 && (opacity == 0 || float(sampler) > opacity);
        }()}) {
      MediumStack::Update(medium, allocator, mat, hit.instance, -ray.dir,
                          ray.dir);
      travel += castDistance;
      width += spread * castDistance;
      ray = Ray{hit.point, ray.dir, EPS, INF};
      continue;
    }

    ++depth;
    ++order;
    travel += castDistance;
    width += spread * castDistance;
    const float3 wo{-ray.dir};
    GuideRecord *record{records ? &records[numRecords++] : nullptr};
    if (record) {
      *record = GuideRecord{};
      record->point = hit.point;
      record->beta = beta;
    }

    // A directly visible emitter: the segment that found it is the
    // BSDF-sampling half of the MIS pair, so weigh the emission against
    // what the light-sampling gather at the previous vertex would have
    // produced. The camera hit (`depth` counts the camera, so that is
    // depth 2) has no competing strategy, and neither does a Dirac bounce,
    // whose direction light sampling can never generate; both add at
    // weight 1. An unregistered emitter (one light selection never picks)
    // reports a zero density and lands at weight 1 the same way.
    if (mat.hasEmission()) {
      Color Le{};
      if (lightSampler.emittedRadiance(mat, hit.instIndex, wo, Le)) {
        float weight{
            depth == 2 || prevDirac
                ? 1.0f
                : smdl::powerHeuristic(wpdfPrev, lightSampler.solidAnglePDF(
                                                     hit.instIndex, hit.point,
                                                     hit.Ng, prevPoint))};
        addArrival(
            Le, weight, depth - 2, lightSampler.causticLight(hit.instIndex),
            records && numRecords > 1 ? &records[numRecords - 2] : nullptr,
            [&](ManifoldTarget &target) {
              const float3 toLight{hit.point - mneeCoverage.receiver()};
              const float distStraight{length(toLight)};
              if (!(distStraight > 0.0f)) return -1.0f;
              target.wl = toLight / distStraight;
              target.point = hit.point;
              target.infinite = false;
              target.normal = hit.Ng;
              return lightSampler.solidAnglePDF(
                  hit.instIndex, hit.point, hit.Ng, mneeCoverage.receiver());
            });
      }
    }
    if (atMaxBounces(depth)) break;
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
    const bool wasArmed{mneeCoverage.isArmed()};
    const DTree *dtree{
        guidingCellAt(guiding, hit.point,
                      !isHair && (dfLobesOf(mat) & smdl::JIT::DF_FINITE) != 0)};
    // The one-sample-MIS mixture weight at this vertex: the cell's
    // learned weight unless pinned for experiments. Meaningful only when
    // `dtree` is non-null, and shared by the gather below, whose MIS
    // weight has to match the continuation density.
    const float bsdfFraction{bsdfFractionAt(guiding, dtree)};
    // What the manifold estimators claim at this vertex: the instance's
    // claim, narrowed to what the gathers behind can actually reach from
    // here. This vertex gathers the rest, and the claimed share of its
    // continuation is dropped at the light.
    const auto claim{mneeOptions.any() && !isHair
                         ? manifoldClaim(mat, hit.instance->causticCaster,
                                         mneeOptions.maxRoughness)
                         : ManifoldClaim()};
    const auto reachable{mneeCoverage.reach(claim, mneeOptions, prevDirac)};
    // Whether this vertex is a manifold receiver: the gathers run from it
    // and it arms for the claims behind, or neither.
    const bool receiver{mneeOptions.any() && !isHair &&
                        isManifoldReceiver(
                            mat, dot(wo, hit.Ng) < 0.0f,
                            [&] { return float4(sampler); },
                            mneeOptions.minReceiverAlpha)};
    // Gather direct lighting at this vertex, before the bounce, so the
    // cone the gather rays inherit is the arrival cone.
    {
      Color direct{gatherDirect(
          scene, sampler, wavelengths, allocator, lightSampler, gatherState,
          mat, isHair ? VertexKind::HAIR : VertexKind::SURFACE, dtree,
          bsdfFraction, guiding, medium, scratch, hit.point, wo, mneeOptions,
          reachable, wasArmed, receiver)};
      if (const float scale{clampScale(beta * direct, depth - 1)}; scale < 1.0f)
        direct *= scale;
      L += beta * direct;
      if (record) record->direct = direct;
    }

    float3 wNext{};
    int sampledLobe{};
    bool isDiracBounce{};
    if (isHair) {
      // There are no Dirac hair distributions, so every accepted sample
      // is a finite-density direction.
      if (!mat.hairScatterSample(float4(sampler), wo, wNext, wpdf,
                                 wpdfRevUnused, f)) {
        break;
      }
    } else if (dtree) {
      // One-sample MIS between the BSDF and the SD-tree: either half's
      // sample is weighed by the mixture density.
      float bsdfPdf{};
      float guidePdf{};
      if (float(sampler) < bsdfFraction) {
        if (!mat.scatterSample(float4(sampler), wo, wNext, bsdfPdf,
                               wpdfRevUnused, f, sampledLobe)) {
          break;
        }
        if (isDiracBounce = (sampledLobe & smdl::JIT::DF_DIRAC) != 0;
            !isDiracBounce)
          guidePdf = dtree->pdf(wNext);
      } else {
        if (wNext = dtree->sampleDirection(sampler, guidePdf);
            !(guidePdf > 0) ||
            !mat.scatterEvaluate(wo, wNext, bsdfPdf, wpdfRevUnused, f))
          break;
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
    } else if (mat.scatterSample(float4(sampler), wo, wNext, wpdf,
                                 wpdfRevUnused, f, sampledLobe)) {
      isDiracBounce = (sampledLobe & smdl::JIT::DF_DIRAC) != 0;
    } else {
      break;
    }
    // Grow the ray cone for the bounce. Dirac bounces leave the spread
    // unchanged; otherwise the growth is a crude heuristic since the
    // instance exposes only the DF_* lobe word, and a material whose
    // sampled lobe was specular still gets its diffuse growth, which errs
    // toward more prefiltering deeper in the path.
    if (!isDiracBounce) {
      spread = std::min(spread + ((dfLobesOf(mat) & smdl::JIT::DF_GENERIC) != 0
                                      ? ANGLE_GROWTH_DIFFUSE
                                      : ANGLE_GROWTH_GLOSSY),
                        ANGLE_MAX);
    }
    if (record) {
      record->wNext = wNext;
      record->wNextPdf = wpdf;
      record->isDiracBounce = isDiracBounce;
    }
    wpdfPrev = wpdf;
    prevDirac = isDiracBounce;
    prevPoint = hit.point;
    const bool transmits{!isHair && mat.isTransmitting(wo, wNext)};
    const Color claimedShare{claimedShareOf(
        mat, reachable, wo, wNext, f, isDiracBounce, transmits, sampledLobe)};
    // Advance the MNEE coverage: a non-Dirac, non-hair
    // vertex is a fresh receiver whose gather may attempt a connection,
    // if it is a receiver at all (a narrow glossy vertex is not, and
    // disarms); a claimed transmission extends the chain from the
    // receiver behind, Dirac or glossy; anything else (a hair vertex, a
    // reflection, an unclaimed or index-matched transmission) breaks it. A
    // glossy transmission that extends a chain is a finite-density bounce that
    // would otherwise have armed a new receiver, and it must not: the
    // gather at the receiver behind it is what claims this chain.
    bool extendedChain{false};
    if (!isHair && mneeCoverage.isArmed() && transmits &&
        (claim.refractLobes & (isDiracBounce ? smdl::JIT::DF_DIRAC_BTDF
                                             : smdl::JIT::DF_GLOSSY_BTDF)) !=
            0) {
      extendedChain = true;
      mneeCoverage.extend(hit, !isDiracBounce, claimedShare);
    } else if (!isHair && !isDiracBounce) {
      mneeCoverage.arm(receiver, hit.point, wpdf, medium);
    } else {
      mneeCoverage.disarm();
    }
    // What the next arrival drops: the chain's claimed share if this bounce
    // extended a glossy chain the gather can reach (a Dirac chain is
    // weighed instead, and an overlong or mixed one is nobody's), else the
    // share of this one reflection.
    prevClaimedShare = extendedChain ? (mneeCoverage.coversGlossy(mneeOptions)
                                            ? mneeCoverage.chainShare()
                                            : Color(0.0f))
                                     : claimedShare;
    prevShareCausticOnly = !extendedChain;
    beta *= f / wpdf;
    if (beta.isAnyNonFinite()) break;
    if (!rouletteSurvives(depth, dtree)) break;
    if (!isHair)
      MediumStack::Update(medium, allocator, mat, hit.instance, wo, wNext);
    ray = Ray{hit.point, wNext, EPS, INF};
  }
  return L;
}
