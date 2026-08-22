#include "PathTracing.h"
#include "Guiding.h"
#include "Light.h"
#include "Manifold.h"

SegmentWalk::SegmentWalk(const Scene &scene, Sampler &sampler,
                         const Color &wavelengths,
                         smdl::BumpPtrAllocator &allocator,
                         const MediumStack *medium, const float3 &point0,
                         const float3 &point1, Color &beta)
    : mScene(scene), mSampler(sampler), mWavelengths(wavelengths),
      mAllocator(allocator), mMedium(medium), mBeta(beta) {
  mDistance = length(point1 - point0);
  mShadowDir = mDistance > 0 ? (point1 - point0) / mDistance : float3{};
  mParamEps = mDistance > 1.0f ? EPS / mDistance : EPS;
  mRay = Ray{point0, point1 - point0, mParamEps, 1.0f - mParamEps};
}

bool SegmentWalk::nextBlocker(Hit &hit) {
  while (mRay.tmin < mRay.tmax) {
    hit = Hit{};
    bool hitSurface{mScene.intersect(mRay, hit)};
    // Attenuate over the span actually traveled, hit or miss
    // (`Scene::intersect` narrows `tmax` to the hit parameter on a
    // hit). The parametrization spans `[0, 1]` over the segment, so the
    // world-space span is rescaled and the medium sees a unit direction
    // with distances in scene units. The epsilon slivers the casts
    // exclude are attributed to whichever side of the boundary this
    // iteration integrates.
    {
      const Medium segMedium{mMedium, mWavelengths, mRay(mTCovered),
                             mShadowDir};
      segMedium.attenuate(mSampler, (mRay.tmax - mTCovered) * mDistance, mBeta);
      mTCovered = mRay.tmax;
      if (!(mBeta.maxComponent() > 0.0f)) {
        return false; // Fully absorbed already.
      }
    }
    if (!hitSurface) {
      return false;
    }
    // A null interface passes shadow rays straight through: no opacity
    // and no blocking, only the medium-stack bookkeeping, which needs
    // the full instance.
    if (hit.material->isNullInterface()) {
      auto state{makeRenderState(mWavelengths, &mAllocator)};
      hit.applyGeometryToState(state, mShadowDir);
      passThrough(smdl::JIT::MaterialInstance{state, hit.material}, hit);
      continue;
    }
    // A statically opaque material blocks without any material work.
    if (hit.material->isAlwaysOpaque()) {
      return true;
    }
    auto state{makeRenderState(mWavelengths, &mAllocator)};
    // Only the ray direction is populated; the LOD fields stay zero so
    // opacity evaluates at full fidelity, the conservative choice for
    // shadow rays.
    hit.applyGeometryToState(state, mShadowDir);
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

void SegmentWalk::passThrough(const smdl::JIT::MaterialInstance &mat,
                              const Hit &hit) {
  MediumStack::Update(mMedium, mAllocator, mat, hit.instance, -mRay.dir,
                      mRay.dir);
  mRay.tmin = smdl::incrementFloat(mRay.tmax + mParamEps);
  mRay.tmax = 1.0f - mParamEps;
}

[[nodiscard]] static bool
testVisibility(const Scene &scene, Sampler &sampler, const Color &wavelengths,
               smdl::BumpPtrAllocator &allocator, const MediumStack *medium,
               const float3 &point0, const float3 &point1, Color &beta) {
  Hit hit{};
  SegmentWalk walk{scene,  sampler, wavelengths, allocator,
                   medium, point0,  point1,      beta};
  return walk.nextBlocker(hit) ? false : beta.maxComponent() > 0.0f;
}

// The depth after which the walk is terminated by Russian roulette rather
// than continued unconditionally.
static constexpr uint64_t ROULETTE_MIN_DEPTH{4};

// The largest survival probability Russian roulette will use, so that every
// path terminates eventually no matter how bright its throughput is.
static constexpr float ROULETTE_MAX_SURVIVAL{0.95f};

// The ray cone spread growth in radians added by a non-delta bounce whose
// material has no diffuse component. Crude heuristic: the JIT instance
// exposes only the DF_* lobe word, not per-lobe roughness.
// Tunable.
static constexpr float ANGLE_GROWTH_GLOSSY{0.05f};

// The ray cone spread growth in radians added by a bounce whose material
// has a diffuse component, or by a volume scattering event.
static constexpr float ANGLE_GROWTH_DIFFUSE{0.3f};

// The cap on the ray cone spread angle, keeping the cone width growth
// well-conditioned on long paths.
static constexpr float ANGLE_MAX{1.0f};

// The scattering role of a path vertex: a surface BSDF, a volume phase
// function, or the hair BSDF at a curve hit whose material binds
// `material.hair`.
enum class VertexKind { SURFACE, VOLUME, HAIR };

// Evaluate the scattering function at a path vertex: the BSDF at a
// surface or hair vertex, or the phase function at a volume vertex,
// whose value is also its own solid-angle density.
[[nodiscard]]
static bool scatterEvaluate(const smdl::JIT::MaterialInstance &mat,
                            VertexKind kind, const float3 &wo, const float3 &wi,
                            float &pdfFwd, Color &f,
                            int lobeMask = smdl::JIT::DF_ALL) {
  if (kind == VertexKind::VOLUME) {
    float phase{mat.volumeScatterEvaluate(wo, wi)};
    pdfFwd = phase;
    f = Color(phase);
    return phase > 0;
  } else {
    // The JIT ABI reports a reverse PDF alongside every forward PDF,
    // which a forward path tracer never consumes.
    float pdfRevUnused{};
    if (kind == VertexKind::HAIR)
      return mat.hairScatterEvaluate(wo, wi, pdfFwd, pdfRevUnused, f);
    if (!mat.scatterEvaluate(wo, wi, pdfFwd, pdfRevUnused, f)) return false;
    if (lobeMask == smdl::JIT::DF_ALL) return true;
    // The masked value over the UNMASKED density: the mask restricts what
    // is estimated, not the continuation sampler it competes with.
    float pdfMaskedUnused{};
    return mat.scatterEvaluate(wo, wi, pdfMaskedUnused, pdfRevUnused, f,
                               lobeMask);
  }
}

// The lobes the manifold estimators claim at a vertex, with today's
// eligibility: the Dirac lobe of a domain when the material has one, else
// its glossy lobe when glossy chains are on; zero when the vertex is
// neither an interface nor a caster. This is the skeleton the caster mark
// and per-lobe claim replace; the isolation modes and the PT estimator of
// the claimed paths are what read it.
[[nodiscard]] static int
manifoldClaimedLobes(const smdl::JIT::MaterialInstance &mat,
                     const float3 &stateNormal,
                     const ManifoldOptions &manifold) {
  if (manifold.depth <= 0) return 0;
  const int dfLobes{dfLobesOf(mat)};
  int claimed{};
  if (isManifoldInterface(mat, stateNormal, manifold.glossy))
    claimed |= (dfLobes & smdl::JIT::DF_DELTA_BTDF) != 0
                   ? smdl::JIT::DF_DELTA_BTDF
               : manifold.glossy ? (dfLobes & smdl::JIT::DF_GLOSSY_BTDF)
                                 : 0;
  if (manifold.reflect && isManifoldCaster(mat, stateNormal, manifold.glossy))
    claimed |= (dfLobes & smdl::JIT::DF_DELTA_BRDF) != 0
                   ? smdl::JIT::DF_DELTA_BRDF
               : manifold.glossy ? (dfLobes & smdl::JIT::DF_GLOSSY_BRDF)
                                 : 0;
  return claimed;
}

// Everything one manifold connection is weighed against that does not
// change from one connection to the next, which is every input the
// gather has except the connection itself.
class ManifoldGatherContext final {
public:
  const Scene &scene;
  Sampler &sampler;
  const Color &wavelengths;
  smdl::BumpPtrAllocator &allocator;
  const LightSampler &lights;
  const smdl::State &gatherState;
  const smdl::JIT::MaterialInstance &mat;
  VertexKind kind{};
  const DTree *dtree{};
  float bsdfFrac{};
  const MediumStack *medium{};
  float3 point{};
  float3 wo{};
  const LightSampler::LightSample &lightSample;

  /// What one converged connection is worth; see the definition.
  [[nodiscard]] Color contribution(const ManifoldChain &chain,
                                   const ManifoldConnection &connection,
                                   float inverseProbability) const;
};

// What one converged connection is worth: the transport along it, the
// throughput of the Dirac lobe at every crossing, and the re-walk MIS
// weight against the arrival that reaches the same light the same way.
// Every discovery funnels through here, so that solving the constraint a
// different way, or for a different lobe, changes nothing about what a
// solution is worth.
Color ManifoldGatherContext::contribution(const ManifoldChain &chain,
                                          const ManifoldConnection &connection,
                                          float inverseProbability) const {
  const bool glossy{chain.vertices[0].isGlossy};
  // Naming one LOBE forces that branch on the sampling path
  // however the material layers it, and reports both the weight of the
  // branch and the chance the ordinary continuation would have taken it.
  // A glossy crossing has a density instead, so it is evaluated rather
  // than sampled and this mask goes unused.
  const int lobeMask{chain.vertices[0].isReflect ? smdl::JIT::DF_DELTA_BRDF
                                                 : smdl::JIT::DF_DELTA_BTDF};
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
  if (lightSample.isDelta) {
    Li = lights.reevaluatePunctualLi(lightSample, point, lastPoint,
                                     gatherState.meters_per_scene_unit);
    if (Li.isAllZero()) return {};
  } else if (!lightSample.isInfinite) {
    Li = lights.reevaluateAreaLi(lightSample, gatherState, lastPoint);
    if (Li.isAllZero()) return {};
  }
  // The receiver BSDF toward the bent direction.
  float fpdfFwd{};
  Color f{};
  if (!scatterEvaluate(mat, kind, wo, connection.wr, fpdfFwd, f)) return {};
  // The per-crossing throughput and the continuation's chance of taking
  // this chain, both accumulated below from the interface material
  // itself. The material is the only thing that knows them: a tinted or
  // conducting interface has a weight no Fresnel term computed here
  // would carry, and a layered one has a selection chance to match. The
  // weight includes the radiance compression the specular BSDF applies
  // to refracted radiance, which telescopes to first-over-last across a
  // chain, so an air-glass-air chain compresses nothing; the transfer
  // Jacobian carries the purely geometric part.
  Color T{Color(1.0f)};
  float chainChance{1.0f};
  // Visibility and attenuation of every sub-segment, crossing the
  // nested-medium stack at each converged vertex, and finally toward
  // the light: the actual sample point for a finite light, or the far
  // environment target translated to the last vertex.
  Color vis{Color(1.0f)};
  Hit blockerUnused{};
  const MediumStack *segMedium{medium};
  float3 segStart{point};
  for (int i = 0; i < connection.count; i++) {
    const auto &vertex{connection.vertices[i]};
    SegmentWalk segWalk{scene,     sampler,  wavelengths,           allocator,
                        segMedium, segStart, vertex.geometry.point, vis};
    if (segWalk.nextBlocker(blockerUnused) || !(vis.maxComponent() > 0.0f))
      return {};
    auto crossState{makeRenderState(wavelengths, &allocator)};
    vertex.hit.applyGeometryToState(crossState, -vertex.wFront);
    smdl::JIT::MaterialInstance crossInst{crossState, vertex.hit.material};
    segMedium = segWalk.medium();
    crossInst.setExteriorIOR(ExteriorIOR(segMedium, crossInst, vertex.wFront));
    // Ask the interface for its Dirac transmission. The mask names that
    // one lobe, so the sampler takes the transmissive Dirac branch
    // with certainty however the material layers it, and reports both the
    // weight of that branch and the chance the ordinary continuation
    // would have chosen it.
    //
    // The sample is drawn rather than fixed. One live Dirac lobe leaves
    // nothing to choose and the draw goes unread, but a material mixing
    // several of them renormalizes over the mix, and a fixed sample would
    // take the first every time while weighting it as though it had been
    // chosen at random. See `isManifoldInterface()` for what remains.
    if (glossy) {
      // A glossy crossing has a density, so it is evaluated at the
      // directions the solve produced rather than sampled. No mask: the
      // direction pair already names the domain.
      float crossPdfFwd{}, crossPdfRev{};
      Color fCross{};
      if (!crossInst.scatterEvaluate(vertex.wFront, vertex.wBack, crossPdfFwd,
                                     crossPdfRev, fCross))
        return {};
      T *= fCross;
      if (T.isAllZero()) return {};
    } else {
      float3 wiDelta{};
      float deltaPdfFwd{}, deltaPdfRev{}, vertexChance{1.0f};
      Color fDelta{};
      int sampledLobe{};
      if (!crossInst.scatterSample(float4(sampler), vertex.wFront, wiDelta,
                                   deltaPdfFwd, deltaPdfRev, fDelta,
                                   sampledLobe, lobeMask, &vertexChance) ||
          (sampledLobe & smdl::JIT::DF_DELTA) == 0)
        return {};
      // The constraint was solved for this crossing, so the material has
      // to agree that it scatters that way. A disagreement means the
      // interface is not the one the solve differentiated.
      if (!(dot(wiDelta, vertex.wBack) > 1.0f - 1e-3f)) return {};
      T *= fDelta;
      chainChance *= vertexChance;
      if (!(chainChance > 0.0f) || T.isAllZero()) return {};
    }
    // A reflection stays on the side it arrived from, so it crosses no
    // boundary and the nested medium is the one it was already in.
    if (!chain.vertices[i].isReflect)
      MediumStack::Update(segMedium, allocator, crossInst, vertex.hit.instance,
                          vertex.wFront, vertex.wBack);
    segStart = vertex.geometry.point;
  }
  const float3 lightPoint{lightSample.isInfinite
                              ? segStart + (lightSample.target - point)
                              : lightSample.target};
  SegmentWalk lightWalk{scene,     sampler,  wavelengths, allocator,
                        segMedium, segStart, lightPoint,  vis};
  if (lightWalk.nextBlocker(blockerUnused) || !(vis.maxComponent() > 0.0f))
    return {};
  // Re-walk MIS: the competing density is the receiver's continuation
  // density toward the bent direction (the guided mixture when the
  // SD-tree participates at this vertex), carried through the chain by
  // the discrete Fresnel transmissions and the transfer Jacobian.
  const float contPdf{dtree ? bsdfFrac * fpdfFwd +
                                  (1.0f - bsdfFrac) * dtree->pdf(connection.wr)
                            : fpdfFwd};
  auto direct{f * vis * Li * T};
  if (glossy) {
    // A roughened connection is drawn in the light direction and one half
    // vector per crossing, so its measure is the offset Jacobian and its
    // density is the light sampler's times the offsets' own. No MIS
    // weight: the walk reaches one of several isolated solutions with a
    // probability it cannot compute, so there is no density to weigh
    // against the path tracer's; the reciprocal estimate stands in for the
    // sum over solutions, and the path tracer is barred from this
    // transport rather than sharing it, as in the reference
    // implementation.
    float offsetDensity{1.0f};
    for (int i = 0; i < connection.count; i++)
      offsetDensity *= chain.vertices[i].offsetDensity;
    if (!(offsetDensity > 0.0f) || !(connection.offsetJacobian > 0.0f))
      return {};
    direct *= inverseProbability * connection.offsetJacobian /
              (lightSample.pdf * offsetDensity);
  } else if (chain.vertices[0].isReflect) {
    // A reflective connection is claimed exclusively however it scatters,
    // Dirac or not: it was searched for rather than handed over, so the
    // walk reaches one of several solutions with a chance it cannot
    // report, and the path tracer is barred instead of weighed against.
    // Applying a heuristic here as well would lose whatever share it
    // assigns to a strategy that is no longer running.
    direct *= inverseProbability * connection.transfer / lightSample.pdf;
  } else {
    const float escPdf{contPdf * chainChance * connection.transfer};
    direct *= connection.transfer / lightSample.pdf;
    // A delta light is unreachable by the continuation, so its MIS
    // weight is 1, matching the plain branch.
    if (!lightSample.isDelta)
      direct *= smdl::powerHeuristic(lightSample.pdf, escPdf);
  }
  return direct.isAnyNonFinite() ? Color() : direct;
}

// Draw the offset a glossy crossing is solved for: a microfacet normal
// from the lobe the mask names, at the hit the caller passes (the straight
// segment's blocker for a refractive chain, a sampled caster point for a
// reflective one), converted into the walk's frame at that hit and into
// the measure the constraint lives in. The density reported is that of
// this draw, which is all the estimator divides by; where the draw is made
// matters only for variance.
[[nodiscard]] static bool
drawManifoldOffset(const Scene &scene, Sampler &sampler,
                   const smdl::JIT::MaterialInstance &mat, const Hit &hit,
                   const float3 &wo, ManifoldVertexSeed &seed, int lobeMask) {
  float3 normal{}, t1{}, t2{};
  if (!manifoldSeedFrame(scene, hit, normal, t1, t2)) return false;
  float3 wm{};
  float pdf{};
  float2 alpha{};
  if (!mat.scatterNormalSample(float4(sampler), wo, wm, pdf, alpha, lobeMask) ||
      !(pdf > 0.0f))
    return false;
  // The walk orients its half vector onto the shading normal, so the
  // offset has to name the representative on that side.
  if (dot(wm, normal) < 0.0f) wm = -wm;
  const float cosWm{dot(wm, normal)};
  if (!(cosWm > 1e-4f)) return false;
  seed.offset = float2(dot(wm, t1), dot(wm, t2));
  seed.offsetDensity = pdf / cosWm;
  return true;
}

// Manifold next-event estimation by REFLECTION off a caster, for a light
// sample at a receiver, whether or not the straight segment to it is clear.
//
// The structure is the refractive glossy one: fix a half vector, jitter the
// start, solve, and estimate the reciprocal of the chance of having reached
// that solution by drawing fresh starts until one lands on it again. What
// differs is where a start comes from. A mirror is nowhere near the line
// from the receiver to the light, so there is no crossing to seed from and
// the caster surface is sampled instead, which is exactly the seed the
// reflective family lacked when it needed a polynomial root finder to do
// without one.
[[nodiscard]]
static Color gatherManifoldReflection(
    const Scene &scene, Sampler &sampler, const Color &wavelengths,
    smdl::BumpPtrAllocator &allocator, const LightSampler &lights,
    const smdl::State &gatherState, const smdl::JIT::MaterialInstance &mat,
    VertexKind kind, const DTree *dtree, float bsdfFrac,
    const MediumStack *medium, const float3 &point, const float3 &wo,
    const LightSampler::LightSample &lightSample,
    const ManifoldOptions &manifold) {
  if (kind == VertexKind::SURFACE) {
    const int dfLobes{dfLobesOf(mat)};
    if ((dfLobes & smdl::JIT::DF_FINITE) == 0) return {};
  }
  ManifoldTarget target{};
  target.wl = lightSample.wi;
  target.point = lightSample.target;
  target.infinite = lightSample.isInfinite;
  if (!lightSample.isDelta && !lightSample.isInfinite)
    target.normal = lightSample.hit.Ng;
  const ManifoldGatherContext ctx{
      scene, sampler, wavelengths, allocator, lights, gatherState, mat,
      kind,  dtree,   bsdfFrac,    medium,    point,  wo,          lightSample};
  // A reflection changes no medium and has no index contrast to resolve, so
  // the two sides weigh the same and `H` is the reflection half vector.
  ManifoldChain chain{};
  chain.count = 1;
  auto &seed{chain.vertices[0]};
  seed.etaFront = seed.etaBack = 1.0f;
  seed.isReflect = true;
  auto drawSeedHit{[&](Hit &hit) {
    return manifold.casters->sampleSeed(scene, sampler, hit);
  }};
  if (!drawSeedHit(seed.hit)) return {};
  // The half vector is drawn once and held, as on the refractive path: with
  // it fixed the constraint has isolated solutions to recognize. It is a
  // direction in the surface's own frame, so where it was drawn matters
  // only through the roughness there.
  {
    auto crossState{makeRenderState(wavelengths, &allocator)};
    seed.hit.applyGeometryToState(crossState, seed.hit.point - point);
    smdl::JIT::MaterialInstance crossInst{crossState, seed.hit.material};
    seed.isGlossy = isGlossyManifoldCaster(crossInst);
    if (seed.isGlossy &&
        !drawManifoldOffset(scene, sampler, crossInst, seed.hit,
                            normalize(point - seed.hit.point), seed,
                            smdl::JIT::DF_GLOSSY_BRDF))
      return {};
  }
  auto &stats{ManifoldStats::global()};
  auto reseedAndSolve{[&](ManifoldConnection &connection) {
    if (!drawSeedHit(chain.vertices[0].hit)) return false;
    ManifoldWalkReport report{};
    const bool ok{solveManifoldConnection(scene, point, target, chain,
                                          connection, &report)};
    stats.recordWalk(report);
    return ok;
  }};
  ManifoldConnection connection{};
  const bool firstConverged{reseedAndSolve(connection)};
  stats.recordEstimate(ManifoldStats::REFLECT, firstConverged);
  if (!firstConverged) return {};
  float inverseProbability{1.0f};
  for (int trial = 0; trial < MANIFOLD_MAX_TRIALS; trial++) {
    ManifoldConnection other{};
    if (reseedAndSolve(other) &&
        isSameManifoldSolution(point, connection, other)) {
      stats.recordTrials(ManifoldStats::REFLECT, trial + 1, false);
      const Color value{
          ctx.contribution(chain, connection, inverseProbability)};
      stats.recordContribution(!value.isAllZero());
      return value;
    }
    inverseProbability += 1.0f;
  }
  stats.recordTrials(ManifoldStats::REFLECT, MANIFOLD_MAX_TRIALS, true);
  return {};
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
[[nodiscard]]
static Color gatherManifoldNEE(
    const Scene &scene, Sampler &sampler, const Color &wavelengths,
    smdl::BumpPtrAllocator &allocator, const LightSampler &lights,
    const smdl::State &gatherState, const smdl::JIT::MaterialInstance &mat,
    VertexKind kind, const DTree *dtree, float bsdfFrac,
    const MediumStack *medium, const float3 &point, const float3 &wo,
    const LightSampler::LightSample &lightSample, SegmentWalk &walk,
    Hit blocker, int maxDepth, const ManifoldOptions &manifold) {
  // The receiver must be able to scatter a finite-density direction at
  // all.
  if (kind == VertexKind::SURFACE) {
    const int dfLobes{dfLobesOf(mat)};
    if ((dfLobes & smdl::JIT::DF_FINITE) == 0) return {};
  }
  // Discover the seed chain: every blocker along the straight segment
  // must be an eligible interface the walk can differentiate, with its
  // per-side refractive indices resolved against the medium stack as of
  // the crossing, and the segment must clear past the last one.
  ManifoldChain chain{};
  // The medium on the receiver side of each crossing, kept so that the
  // offset draw below can resolve the same exterior index the transport
  // does. The arrival side keeps the same thing for the same reason.
  std::array<const MediumStack *, MNEE_MAX_DEPTH> seedMedium{};
  while (true) {
    if (chain.count == std::min(maxDepth, MNEE_MAX_DEPTH)) return {};
    if (blocker.instance->isCurves()) return {};
    auto state{makeRenderState(wavelengths, &allocator)};
    blocker.applyGeometryToState(state, lightSample.wi);
    smdl::JIT::MaterialInstance interfaceInst{state, blocker.material};
    auto &seed{chain.vertices[chain.count]};
    seedMedium[chain.count] = walk.medium();
    if (!makeManifoldSeed(walk.medium(), interfaceInst, state, blocker,
                          lightSample.wi, manifold.glossy, seed))
      return {};
    // A chain is all Dirac or all glossy; see `ManifoldVertexSeed`.
    if (seed.isGlossy != chain.vertices[0].isGlossy) return {};
    chain.count++;
    walk.passThrough(interfaceInst, blocker);
    if (!walk.nextBlocker(blocker)) break;
  }
  // Decline segments that passed a cutout: the pass is stochastic, and
  // the escape-side cancelation probes coverage with a deterministic
  // cast, so the two must agree on what is covered.
  if (walk.passedCutout()) return {};
  ManifoldTarget target{};
  target.wl = lightSample.wi;
  target.point = lightSample.target;
  target.infinite = lightSample.isInfinite;
  // An area sample carries the emitter's orientation, which the offset
  // Jacobian needs and a punctual or distant one does not have.
  if (!lightSample.isDelta && !lightSample.isInfinite)
    target.normal = lightSample.hit.Ng;
  const ManifoldGatherContext ctx{
      scene, sampler, wavelengths, allocator, lights, gatherState, mat,
      kind,  dtree,   bsdfFrac,    medium,    point,  wo,          lightSample};

  auto &stats{ManifoldStats::global()};
  if (!chain.vertices[0].isGlossy) {
    ManifoldConnection connection{};
    ManifoldWalkReport report{};
    const bool converged{solveManifoldConnection(scene, point, target, chain,
                                                 connection, &report)};
    stats.recordWalk(report);
    stats.recordEstimate(ManifoldStats::DIRAC_REFRACT, converged);
    if (!converged) return {};
    const Color value{ctx.contribution(chain, connection, 1.0f)};
    stats.recordContribution(!value.isAllZero());
    return value;
  }
  // A roughened connection, by the estimator of Zeltner, Georgiev and Jakob.
  //
  // Draw one half vector per crossing and hold it FIXED. With the offsets
  // fixed the constraint has isolated solutions, so "which one did the walk
  // reach" is a question with a probability rather than a certainty, and the
  // reciprocal of that probability is what an unbiased estimate of the sum
  // over them needs. Redrawing the offsets per trial would move the solutions
  // and leave nothing to recognize.
  for (int i = 0; i < chain.count; i++) {
    auto crossState{makeRenderState(wavelengths, &allocator)};
    chain.vertices[i].hit.applyGeometryToState(crossState, lightSample.wi);
    smdl::JIT::MaterialInstance crossInst{crossState,
                                          chain.vertices[i].hit.material};
    crossInst.setExteriorIOR(
        ExteriorIOR(seedMedium[i], crossInst, -lightSample.wi));
    if (!drawManifoldOffset(scene, sampler, crossInst, chain.vertices[i].hit,
                            -lightSample.wi, chain.vertices[i],
                            smdl::JIT::DF_GLOSSY_BTDF))
      return {};
  }
  auto jitterAndSolve{[&](ManifoldConnection &connection) {
    for (int i = 0; i < chain.count; i++)
      chain.vertices[i].seedJitter =
          MANIFOLD_SEED_JITTER * smdl::uniformDiskSample(float2(sampler));
    ManifoldWalkReport report{};
    const bool ok{solveManifoldConnection(scene, point, target, chain,
                                          connection, &report)};
    stats.recordWalk(report);
    return ok;
  }};
  ManifoldConnection connection{};
  const bool firstConverged{jitterAndSolve(connection)};
  stats.recordEstimate(ManifoldStats::GLOSSY_REFRACT, firstConverged);
  if (!firstConverged) return {};
  // The reciprocal estimate: keep drawing fresh starts until one lands on the
  // same solution, and count how many that took. The count is geometric with
  // mean one over the chance of reaching this solution, so it estimates that
  // reciprocal without ever computing it. Running out of trials drops the
  // sample rather than truncating the estimate, which is what the reference
  // does and is the one place this is knowingly not unbiased.
  float inverseProbability{1.0f};
  for (int trial = 0; trial < MANIFOLD_MAX_TRIALS; trial++) {
    ManifoldConnection other{};
    if (jitterAndSolve(other) &&
        isSameManifoldSolution(point, connection, other)) {
      stats.recordTrials(ManifoldStats::GLOSSY_REFRACT, trial + 1, false);
      const Color value{
          ctx.contribution(chain, connection, inverseProbability)};
      stats.recordContribution(!value.isAllZero());
      return value;
    }
    inverseProbability += 1.0f;
  }
  stats.recordTrials(ManifoldStats::GLOSSY_REFRACT, MANIFOLD_MAX_TRIALS, true);
  return {};
}

// The manifold-NEE re-walk MIS state the camera walk carries: armed at
// every vertex whose gather could attempt a manifold connection, along
// with that vertex and the identity of every eligible specular bounce
// the walk has taken unbroken since. An arrival at a light through such
// a chain is weighed against the gather at the receiver by
// `coverWeight()`; any other bounce breaks the chain and restores the
// ordinary weights.
class ManifoldCancelState final {
public:
  // Begin a fresh receiver, the vertex a gather could connect from.
  // `enabled` is false when manifold NEE is off, which leaves the state
  // permanently disarmed.
  void arm(bool enabled, const float3 &point, float pdf,
           const MediumStack *medium) noexcept {
    mArmed = enabled;
    mChainLength = 0;
    mReceiver = point;
    mReceiverPdf = pdf;
    mReceiverMedium = medium;
  }

  void disarm() noexcept { mArmed = false; }

  [[nodiscard]] bool armed() const noexcept { return mArmed; }

  // Extend the chain across an eligible specular bounce. The length
  // keeps counting past `MNEE_MAX_DEPTH` so that an overlong chain
  // reads as uncovered rather than as a shorter one.
  void extend(const Hit &hit) noexcept {
    if (mChainLength < MNEE_MAX_DEPTH) {
      mChainInstances[mChainLength] = hit.instance;
      mChainPieces[mChainLength] = hit.faceIndex;
      mChainHits[mChainLength] = hit;
    }
    mChainLength++;
  }

  // Is there a chain of connectable length for the gather to compete
  // with, so that `coverWeight()` replaces the ordinary MIS weight?
  [[nodiscard]] bool covers(const ManifoldOptions &manifold) const noexcept {
    return mArmed && mChainLength >= 1 && mChainLength <= manifold.depth;
  }

  [[nodiscard]] const float3 &receiver() const noexcept { return mReceiver; }

  // The MIS weight of a BSDF-side arrival at `target` through the
  // chain, by re-walk MIS; see the definition.
  [[nodiscard]] float coverWeight(const Scene &scene, Sampler &sampler,
                                  const Color &wavelengths,
                                  smdl::BumpPtrAllocator &allocator,
                                  const ManifoldTarget &target, float lightPdf,
                                  const ManifoldOptions &manifold) const;

private:
  bool mArmed{};
  int mChainLength{};
  float3 mReceiver{};
  float mReceiverPdf{};
  const MediumStack *mReceiverMedium{};
  std::array<const MeshInstance *, MNEE_MAX_DEPTH> mChainInstances{};
  std::array<uint32_t, MNEE_MAX_DEPTH> mChainPieces{};
  std::array<Hit, MNEE_MAX_DEPTH> mChainHits{};
};

// The MIS weight of a BSDF-side arrival at a light, an environment
// escape or an emitter hit, through a chain of eligible refractive
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
float ManifoldCancelState::coverWeight(const Scene &scene, Sampler &sampler,
                                       const Color &wavelengths,
                                       smdl::BumpPtrAllocator &allocator,
                                       const ManifoldTarget &target,
                                       float lightPdf,
                                       const ManifoldOptions &manifold) const {
  const float3 &receiver{mReceiver};
  const MediumStack *receiverMedium{mReceiverMedium};
  const float receiverPdf{mReceiverPdf};
  const auto &chainHits{mChainHits};
  const auto &chainInstances{mChainInstances};
  const auto &chainPieces{mChainPieces};
  const int chainLength{mChainLength};
  // A light the sampler cannot draw is covered by this arrival alone.
  if (!(lightPdf > 0.0f)) return 1.0f;
  // Build the seed chain along the straight cast, mirroring the
  // gather's discovery: the same interfaces in the same order and
  // nothing else, with per-side indices resolved against the
  // receiver's medium stack as it evolves across the crossings. A
  // finite target bounds each cast short of the light point itself.
  ManifoldChain seed{};
  // The receiver-side medium at each crossing, kept so that the chance
  // below can be asked of the interface material with the same exterior
  // index the gather resolves.
  std::array<const MediumStack *, MNEE_MAX_DEPTH> crossingMedium{};
  const MediumStack *medium{receiverMedium};
  const float3 wl{target.wl};
  const float3 woStraight{-wl};
  float3 origin{receiver};
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
    auto state{makeRenderState(wavelengths, &allocator)};
    hit.applyGeometryToState(state, wl);
    smdl::JIT::MaterialInstance interfaceInst{state, hit.material};
    if (hit.material->isNullInterface()) {
      MediumStack::Update(medium, allocator, interfaceInst, hit.instance,
                          woStraight, wl);
      origin = hit.point;
      continue;
    }
    if (seed.count == chainLength) return 1.0f;
    if (hit.instance != chainInstances[seed.count]) return 1.0f;
    if (hit.instance->isCurves()) return 1.0f;
    if (hit.instance->isPrimitive() && hit.faceIndex != chainPieces[seed.count])
      return 1.0f;
    if (!makeManifoldSeed(medium, interfaceInst, state, hit, wl,
                          manifold.glossy, seed.vertices[seed.count]))
      return 1.0f;
    // The gather declines a mixed chain, so this must decline it too or
    // the pair stops summing to one.
    if (seed.vertices[seed.count].isGlossy != seed.vertices[0].isGlossy)
      return 1.0f;
    crossingMedium[seed.count] = medium;
    seed.count++;
    MediumStack::Update(medium, allocator, interfaceInst, hit.instance,
                        woStraight, wl);
    origin = hit.point;
  }
  if (!reached || seed.count != chainLength) return 1.0f;
  // A glossy chain is claimed outright by the gather, which bars the path
  // tracer from it; see `prevQuasiSpecularBar` in `tracePath()`.
  if (seed.vertices[0].isGlossy) return 0.0f;
  ManifoldConnection connection{};
  ManifoldWalkReport report{};
  const bool converged{solveManifoldConnection(scene, receiver, target, seed,
                                               connection, &report)};
  ManifoldStats::global().recordRewalk(report);
  if (!converged) return 1.0f;
  for (int i = 0; i < chainLength; i++) {
    const float scale{std::max(1e-3f, length(chainHits[i].point - receiver))};
    if (!(length(connection.vertices[i].hit.point - chainHits[i].point) <
          MANIFOLD_IDENTITY_FRACTION * scale))
      return 1.0f;
  }
  const float transfer{connection.transfer};
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
    auto crossState{makeRenderState(wavelengths, &allocator)};
    chainHits[i].applyGeometryToState(crossState, travel);
    smdl::JIT::MaterialInstance crossInst{crossState, chainHits[i].material};
    crossInst.setExteriorIOR(
        ExteriorIOR(crossingMedium[i], crossInst, -travel));
    float3 wiDelta{};
    float deltaPdfFwd{}, deltaPdfRev{}, vertexChance{1.0f};
    Color fDelta{};
    int sampledLobe{};
    if (!crossInst.scatterSample(float4(sampler), -travel, wiDelta, deltaPdfFwd,
                                 deltaPdfRev, fDelta, sampledLobe,
                                 smdl::JIT::DF_DELTA_BTDF, &vertexChance) ||
        (sampledLobe & smdl::JIT::DF_DELTA) == 0)
      return 1.0f;
    Q *= vertexChance;
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
// With `manifold.depth > 0`, a light sample whose straight segment is
// blocked by smooth refractive interfaces routes through
// `gatherManifoldNEE()` instead of reading as occluded. Hair vertices
// keep plain gathering: the manifold estimator's MIS is not wired
// through the hair BSDF.
//
// A `lobeMask` other than `DF_ALL` is the PT estimator of the claimed
// paths gathering at a claimed vertex: light sampling restricted to the
// claimed lobes and nothing else, a claimed contribution for the isolation
// modes. The manifold gathers run only under the manifold estimator, and
// only from a receiver.
[[nodiscard]] static Color gatherDirect(
    const Scene &scene, Sampler &sampler, const Color &wavelengths,
    smdl::BumpPtrAllocator &allocator, const LightSampler &lights,
    const smdl::State &gatherState, const smdl::JIT::MaterialInstance &mat,
    VertexKind kind, const DTree *dtree, float bsdfFrac,
    const MediumStack *medium, const float3 &point, const float3 &wo,
    const ManifoldOptions &manifold, int lobeMask = smdl::JIT::DF_ALL) {
  const auto mneeDepth{kind == VertexKind::HAIR ? 0 : manifold.depth};
  const bool claimedGather{lobeMask != smdl::JIT::DF_ALL};
  const bool keepNEE{claimedGather ? manifold.keepClaimed()
                                   : manifold.keepOrdinary()};
  const bool runManifold{!claimedGather && mneeDepth > 0 &&
                         manifold.useManifold() && manifold.keepClaimed()};
  Color direct{};
  if (lights.empty()) return direct;
  LightSampler::LightSample lightSample{};
  if (lights.sample(gatherState, sampler, point, lightSample)) {
    if (!runManifold) {
      float fpdfFwd{};
      Color f{};
      if (keepNEE && scatterEvaluate(mat, kind, wo, lightSample.wi, fpdfFwd, f,
                                     lobeMask)) {
        // The competing density in the MIS weight must be the density the
        // continuation sampler actually assigns to this direction: the BSDF
        // alone, or the guided mixture when the SD-tree participates at
        // this vertex. Weighing against the raw BSDF density while the
        // continuation samples the mixture makes the two halves sum past 1
        // and reads several percent bright.
        const float contPdf{dtree ? bsdfFrac * fpdfFwd +
                                        (1.0f - bsdfFrac) *
                                            dtree->pdf(lightSample.wi)
                                  : fpdfFwd};
        if (testVisibility(scene, sampler, wavelengths, allocator, medium,
                           point, lightSample.target, f)) {
          auto D{f * lightSample.Li / lightSample.pdf};
          if (!D.isAnyNonFinite()) {
            // A delta light is unreachable by the continuation, so its
            // MIS weight is 1.
            if (!lightSample.isDelta)
              D *= smdl::powerHeuristic(lightSample.pdf, contPdf);
            direct += D;
          }
        }
      }
    } else {
      // Visibility first: whether the straight segment is clear decides
      // which estimator runs, and the manifold connection does not care
      // whether the straight direction can scatter at the receiver.
      Color vis{Color(1.0f)};
      Hit blocker{};
      SegmentWalk walk{scene,  sampler, wavelengths,        allocator,
                       medium, point,   lightSample.target, vis};
      if (!walk.nextBlocker(blocker)) {
        float fpdfFwd{};
        Color f{};
        if (keepNEE && vis.maxComponent() > 0.0f &&
            scatterEvaluate(mat, kind, wo, lightSample.wi, fpdfFwd, f)) {
          // The same estimator as the plain branch, with the walk's
          // attenuation carried separately instead of folded into `f`.
          const float contPdf{dtree ? bsdfFrac * fpdfFwd +
                                          (1.0f - bsdfFrac) *
                                              dtree->pdf(lightSample.wi)
                                    : fpdfFwd};
          auto D{f * vis * lightSample.Li / lightSample.pdf};
          if (!D.isAnyNonFinite()) {
            if (!lightSample.isDelta)
              D *= smdl::powerHeuristic(lightSample.pdf, contPdf);
            direct += D;
          }
        }
      } else {
        direct += gatherManifoldNEE(scene, sampler, wavelengths, allocator,
                                    lights, gatherState, mat, kind, dtree,
                                    bsdfFrac, medium, point, wo, lightSample,
                                    walk, blocker, mneeDepth, manifold);
      }
    }
    // The reflective gather is additive rather than an alternative: a mirror
    // is nowhere near the line to the light, so whether that line is clear
    // says nothing about whether there is a reflection to find.
    if (runManifold && manifold.reflect && manifold.casters &&
        !manifold.casters->empty()) {
      direct += gatherManifoldReflection(
          scene, sampler, wavelengths, allocator, lights, gatherState, mat,
          kind, dtree, bsdfFrac, medium, point, wo, lightSample, manifold);
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
static void foldArrivalIntoRecord(GuideRecord *record, const Color &beta,
                                  const Color &Larrival, float weight) {
  if (!record) return;
  for (size_t b = 0; b < record->beta.size(); b++) {
    if (record->beta[b] > 0)
      record->direct[b] += beta[b] / record->beta[b] * Larrival[b] * weight;
    record->continuationEmission[b] = Larrival[b] * weight;
  }
}

Color tracePath(smdl::Compiler &compiler, const Scene &scene, Sampler &sampler,
                const Color &wavelengths, smdl::BumpPtrAllocator &allocator,
                Ray ray, float cameraWeight, float cameraConeAngle,
                const MediumStack *exteriorMedium, uint64_t maxDepth,
                const LightSampler &lights, const Guiding *guiding,
                const ManifoldOptions &manifold, GuideRecord *records,
                uint64_t &numRecords) {
  numRecords = 0;
  Color L{};
  if (maxDepth <= 1) return L;
  const EnvLight *envLight{lights.env()};
  const STree *guideTree{guiding ? guiding->tree : nullptr};

  // The walk starts on the exterior of all scene geometry, inside the
  // scene-wide exterior medium if the composition names one (the bottom
  // of the nested-medium stack, owned by the caller), else in vacuum.
  const MediumStack *medium{exteriorMedium};

  // Set up the state variables that never change; the geometric ones are
  // updated at every vertex by `Hit::apply_geometry_to_state()`.
  auto state{makeRenderState(wavelengths, &allocator)};
  state.transport = smdl::TRANSPORT_RADIANCE;
  // The pristine gather-side state, see `gatherDirect()`.
  const auto gatherState{makeRenderState(wavelengths, &allocator)};

  Color beta{Color(cameraWeight)};
  Color f{};
  float wpdfFwd{};
  // The JIT ABI reports a reverse PDF alongside every forward PDF, which a
  // forward path tracer never consumes; every call shares this sink.
  float pdfRevUnused{};
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
  float prevWpdf{};
  bool prevDelta{};
  bool prevQuasiSpecularBar{};
  // Of the barred arrivals, whether this one is through a claimed lobe, so
  // the PT estimator of the claimed paths keeps it and the isolation modes
  // can tell it from the rest.
  bool prevClaimedArrival{};
  float3 prevPoint{};
  ManifoldCancelState mnee{};
  // The number of path vertices so far, counting the camera as the first.
  uint64_t depth{1};
  while (depth < maxDepth) {
    auto hit{Hit{}};
    bool hitSurface{scene.intersect(ray, hit)};
    if (const Medium segMedium{medium, wavelengths, ray.org, ray.dir};
        segMedium.hasMedium()) {
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
          segMedium.sampleDistance(sampler, ray.tmax, t, beta, emitted)};
      const Color Lemit{betaStart * emitted};
      if (!Lemit.isAnyNonFinite() && manifold.keepOrdinary()) L += Lemit;
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
        // The phase function of the vertex: the medium's own, or with
        // additive overlap the component the collision picked.
        const auto &phaseInst{*segMedium.scatterInstance()};
        {
          // The SD-tree never participates at volume vertices, so the
          // continuation density the gather weighs against is the phase
          // function alone.
          const Color direct{gatherDirect(
              scene, sampler, wavelengths, allocator, lights, gatherState,
              phaseInst, VertexKind::VOLUME, /*dtree=*/nullptr,
              /*bsdfFrac=*/1.0f, medium, point, wo, manifold)};
          L += beta * direct;
          if (record) record->direct = direct;
        }
        // Sample the vertex's phase function. It returns the phase
        // value, which is also the solid-angle PDF of having sampled it,
        // so the throughput weight is exactly 1 and `beta` is unchanged.
        float3 wNext{};
        float phase{phaseInst.volumeScatterSample(float4(sampler), wo, wNext)};
        if (!(phase > 0)) {
          break;
        }
        if (record) {
          record->wNext = wNext;
          record->pdfWNext = phase;
        }
        prevWpdf = phase;
        prevDelta = false;
        prevPoint = point;
        // A volume vertex is a manifold-NEE receiver like any other.
        mnee.arm(manifold.any(), point, phase, medium);
        // Phase functions scatter wide, so grow the cone like a diffuse
        // bounce.
        spread = std::min(spread + ANGLE_GROWTH_DIFFUSE, ANGLE_MAX);
        ray = Ray{point, wNext, EPS, INF};
        continue;
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
        float weight{depth == 1 || prevDelta
                         ? 1.0f
                         : smdl::powerHeuristic(
                               prevWpdf, lights.envSelectionPMF() * Lipdf)};
        // An escape through a manifold-connectable chain of eligible
        // refractive interfaces competes with the manifold gather at
        // its receiver by re-walk MIS; `coverWeight` returns 1 whenever
        // the gather cannot produce this transport, which covers the dim
        // sky the compensated environment sampler never draws (`envPdf`
        // zero), fold solutions the walk does not find, and failed walks.
        // The chain always follows a delta transmission, so the weight it
        // replaces is the `prevDelta` 1. A barred arrival is one the
        // gather claims outright.
        //
        // Under the PT estimator of the claimed paths a claimed arrival
        // keeps its ordinary weight (1 after a Dirac bounce, MIS after the
        // claimed gather a glossy vertex ran), and an arrival the manifold
        // estimator bars without claiming stays barred, so the complement
        // is the same under both estimators. The isolation modes then keep
        // one side or the other.
        const bool claimedArrival{prevClaimedArrival || mnee.covers(manifold)};
        if (manifold.useManifold()) {
          if (prevQuasiSpecularBar) {
            weight = 0.0f;
          } else if (mnee.covers(manifold)) {
            ManifoldTarget target{};
            target.wl = ray.dir;
            weight =
                mnee.coverWeight(scene, sampler, wavelengths, allocator, target,
                                 lights.envSelectionPMF() * Lipdf, manifold);
          }
        } else if (prevQuasiSpecularBar && !prevClaimedArrival) {
          weight = 0.0f;
        }
        if (claimedArrival ? !manifold.keepClaimed() : !manifold.keepOrdinary())
          weight = 0.0f;
        auto Lenv{beta * Li * weight};
        if (!Lenv.isAnyNonFinite()) {
          L += Lenv;
          foldArrivalIntoRecord(
              records && numRecords > 0 ? &records[numRecords - 1] : nullptr,
              beta, Li, weight);
        }
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
    // A null interface is a boundary that scatters nothing itself but
    // encloses a participating medium, e.g., a smoke container: pass
    // straight through like a cutout hop, committing the distance and
    // width but not the order, with only the medium-stack bookkeeping.
    if (hit.material->isNullInterface() && !isHair) {
      MediumStack::Update(medium, allocator, mat, hit.instance, -ray.dir,
                          ray.dir);
      travel += castDistance;
      width += spread * castDistance;
      ray = Ray{hit.point, ray.dir, EPS, INF};
      continue;
    }
    if (float opacity{mat.getCutoutOpacity()};
        opacity < 1 && (opacity == 0 || float(sampler) > opacity)) {
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
      if (lights.emittedRadiance(mat, hit.instIndex, wo, Le)) {
        float weight{
            depth == 2 || prevDelta
                ? 1.0f
                : smdl::powerHeuristic(
                      prevWpdf, lights.solidAnglePDF(hit.instIndex, hit.point,
                                                     hit.Ng, prevPoint))};
        // An emitter reached through a manifold-connectable chain of
        // eligible refractive interfaces competes with the manifold
        // gather at its receiver by re-walk MIS, exactly as an
        // environment escape does, and the estimator and isolation rules
        // are the same as there.
        const bool claimedArrival{prevClaimedArrival || mnee.covers(manifold)};
        if (manifold.useManifold()) {
          if (prevQuasiSpecularBar) {
            weight = 0.0f;
          } else if (mnee.covers(manifold)) {
            const float3 toLight{hit.point - mnee.receiver()};
            const float distStraight{length(toLight)};
            if (distStraight > 0.0f) {
              ManifoldTarget target{};
              target.wl = toLight / distStraight;
              target.point = hit.point;
              target.infinite = false;
              target.normal = hit.Ng;
              weight = mnee.coverWeight(
                  scene, sampler, wavelengths, allocator, target,
                  lights.solidAnglePDF(hit.instIndex, hit.point, hit.Ng,
                                       mnee.receiver()),
                  manifold);
            }
          }
        } else if (prevQuasiSpecularBar && !prevClaimedArrival) {
          weight = 0.0f;
        }
        if (claimedArrival ? !manifold.keepClaimed() : !manifold.keepOrdinary())
          weight = 0.0f;
        auto Lem{beta * Le * weight};
        if (!Lem.isAnyNonFinite()) {
          L += Lem;
          foldArrivalIntoRecord(
              records && numRecords > 1 ? &records[numRecords - 2] : nullptr,
              beta, Le, weight);
        }
      }
    }
    // With guiding active, non-delta surface bounces one-sample-MIS the
    // SD-tree against the BSDF. Materials whose scattering is purely a
    // Dirac delta bypass guiding entirely: the tree cannot produce their
    // directions and evaluating the BSDF at a guided direction would
    // always be zero. Looked up before the gather, whose MIS weight has
    // to match the continuation density. Hair vertices bypass guiding
    // outright: the guided halves would evaluate the surface BSDF rather
    // than the hair BSDF.
    //
    // An eligible glossy interface bypasses both guiding and the gather
    // below, because the manifold estimator claims its transport: it is a
    // link in a chain, not a receiver, and the gather at the receiver
    // behind it already competes with every path through it. Letting it
    // gather as well would count that transport twice, since neither
    // weight knows about the other. A Dirac interface needs no such rule,
    // its gather being identically zero.
    const bool quasiSpecular{
        (manifold.glossy && manifold.depth > 0 && !isHair &&
         isManifoldInterface(mat, state.normal, /*allowGlossy=*/true) &&
         isGlossyManifoldInterface(mat)) ||
        (manifold.reflect && manifold.depth > 0 && !isHair &&
         isManifoldCaster(mat, state.normal, manifold.glossy))};
    // A caster is a link and not a receiver for the same reason an
    // interface is: the reflective gather at the vertex behind it claims
    // the connection from there to the light, and this vertex gathering as
    // well would count that connection twice.
    const bool casterBounce{
        manifold.reflect && manifold.depth > 0 && !isHair &&
        isManifoldCaster(mat, state.normal, manifold.glossy)};
    const bool mneeWasArmed{mnee.armed()};
    const DTree *dtree{};
    if (guideTree && !isHair && !quasiSpecular) {
      int dfLobes{dfLobesOf(mat)};
      if ((dfLobes & smdl::JIT::DF_FINITE) != 0)
        dtree = &guideTree->samplingAt(hit.point);
    }
    // The one-sample-MIS mixture weight at this vertex: the cell's
    // learned weight unless pinned for experiments. Meaningful only when
    // `dtree` is non-null, and shared by the gather below, whose MIS
    // weight has to match the continuation density.
    const float bsdfFrac{!dtree || guiding->bsdfFractionFixed
                             ? guiding ? guiding->bsdfFraction : 1.0f
                             : dtree->mixtureAlpha};
    // The lobes the manifold estimators claim here, and whether this is a
    // claimed vertex reached from an armed receiver that the PT estimator
    // of the claimed paths gathers at instead of the manifold gather
    // behind it; see `ManifoldOptions::Estimator`.
    const int claimedLobes{manifoldClaimedLobes(mat, state.normal, manifold)};
    const bool claimedNEE{quasiSpecular && mneeWasArmed && !isHair &&
                          !manifold.useManifold() && claimedLobes != 0};
    // Gather direct lighting at this vertex, before the bounce, so the
    // cone the gather rays inherit is the arrival cone. A claimed vertex
    // gathers only under the PT estimator, and only its claimed lobes.
    if (!quasiSpecular || claimedNEE) {
      const Color direct{gatherDirect(
          scene, sampler, wavelengths, allocator, lights, gatherState, mat,
          isHair ? VertexKind::HAIR : VertexKind::SURFACE, dtree, bsdfFrac,
          medium, hit.point, wo, manifold,
          claimedNEE ? claimedLobes : smdl::JIT::DF_ALL)};
      L += beta * direct;
      if (record) record->direct = direct;
    }

    float3 wNext{};
    int sampledLobe{};
    bool isDeltaBounce{};
    if (isHair) {
      // There are no delta hair distributions, so every accepted sample
      // is a finite-density direction.
      if (!mat.hairScatterSample(float4(sampler), wo, wNext, wpdfFwd,
                                 pdfRevUnused, f)) {
        break;
      }
    } else if (dtree) {
      if (float(sampler) < bsdfFrac) {
        if (!mat.scatterSample(float4(sampler), wo, wNext, wpdfFwd,
                               pdfRevUnused, f, sampledLobe)) {
          break;
        }
        isDeltaBounce = (sampledLobe & smdl::JIT::DF_DELTA) != 0;
        if (isDeltaBounce) {
          // The delta lobe folds its density into `f` (unit PDF by
          // convention), and the tree cannot compete with it, so the only
          // density left is the discrete chance of having chosen BSDF
          // sampling at all.
          wpdfFwd = bsdfFrac;
        } else {
          const float fpdf{wpdfFwd};
          const float gpdf{dtree->pdf(wNext)};
          wpdfFwd = bsdfFrac * fpdf + (1.0f - bsdfFrac) * gpdf;
          if (record) {
            record->pdfBSDF = fpdf;
            record->pdfGuide = gpdf;
            record->fAvg = f.average();
          }
        }
      } else {
        float guidePDF{};
        float3 wi{dtree->sampleDirection(sampler, guidePDF)};
        float fpdfFwd{};
        if (!(guidePDF > 0) ||
            !mat.scatterEvaluate(wo, wi, fpdfFwd, pdfRevUnused, f)) {
          break;
        }
        wNext = wi;
        sampledLobe = 0;
        isDeltaBounce = false;
        wpdfFwd = bsdfFrac * fpdfFwd + (1.0f - bsdfFrac) * guidePDF;
        if (record) {
          record->pdfBSDF = fpdfFwd;
          record->pdfGuide = guidePDF;
          record->fAvg = f.average();
        }
      }
    } else if (!mat.scatterSample(float4(sampler), wo, wNext, wpdfFwd,
                                  pdfRevUnused, f, sampledLobe)) {
      break;
    } else {
      isDeltaBounce = (sampledLobe & smdl::JIT::DF_DELTA) != 0;
    }
    // Grow the ray cone for the bounce. Delta bounces leave the spread
    // unchanged; otherwise the growth is a crude heuristic since the
    // instance exposes only the DF_* lobe word, and a material whose
    // sampled lobe was specular still gets its diffuse growth, which errs
    // toward more prefiltering deeper in the path.
    if (!isDeltaBounce) {
      const int dfLobes{dfLobesOf(mat)};
      spread = std::min(spread + ((dfLobes & smdl::JIT::DF_GENERIC) != 0
                                      ? ANGLE_GROWTH_DIFFUSE
                                      : ANGLE_GROWTH_GLOSSY),
                        ANGLE_MAX);
    }
    if (record) {
      record->wNext = wNext;
      record->pdfWNext = wpdfFwd;
      record->isDeltaBounce = isDeltaBounce;
    }
    prevWpdf = wpdfFwd;
    // `prevDelta` is really "the previous vertex ran no gather", which a
    // delta bounce and a bypassed glossy interface both are, and a claimed
    // vertex gathering under the PT estimator is not. Getting this wrong
    // halves the transport rather than double counting it.
    prevDelta = isDeltaBounce || (quasiSpecular && !claimedNEE);
    prevPoint = hit.point;
    // Advance the manifold-NEE cancelation state: a non-delta,
    // non-hair vertex is a fresh receiver whose gather may attempt a
    // connection; a delta transmission through an eligible interface
    // extends the chain; anything else (a hair vertex, a specular
    // reflection, an ineligible or index-matched interface) breaks it.
    bool extendedChain{false};
    if (isHair) {
      mnee.disarm();
    } else if (mnee.armed() && manifold.depth > 0 &&
               isManifoldInterface(mat, state.normal, manifold.glossy) &&
               mat.isTransmitting(wo, wNext)) {
      extendedChain = true;
      // A transmission through an eligible interface extends the chain
      // whether it was Dirac or glossy. A glossy one is a finite-density
      // bounce that would otherwise have armed a new receiver, and it
      // must not: it ran no gather of its own, and the gather at the
      // receiver behind it is what competes with this path.
      mnee.extend(hit);
    } else if (!isDeltaBounce) {
      mnee.arm(manifold.any(), hit.point, wpdfFwd, medium);
    } else {
      mnee.disarm();
    }
    // A glossy crossing bars the path tracer from what lies beyond it. The
    // roughened gather reaches those solutions with a probability it cannot
    // report, so there is no weight that would let the two share the
    // transport, and the reference implementation makes the same trade: it
    // filters emitter hits behind a caustic caster outright and calls the
    // missing MIS an open problem.
    //
    // Only a crossing that EXTENDED a chain, though. Barring every bounce at
    // an eligible interface takes the reflection off it as well, and a
    // transmission with no armed receiver behind it, neither of which any
    // gather claims: that is transport thrown away for nothing, and it
    // measured as a 24 percent loss. What is still lost is what the gather
    // cannot find, chains past `MNEE_MAX_DEPTH` among them.
    // ...and a reflection off a caster is barred on the same terms, but
    // only when there IS a receiver behind it to have claimed it. A caster
    // the camera looks at directly is claimed by nobody, and barring it
    // would lose the mirror rather than deduplicate it.
    prevQuasiSpecularBar =
        (quasiSpecular && extendedChain) ||
        (casterBounce && mneeWasArmed && !mat.isTransmitting(wo, wNext));
    prevClaimedArrival =
        prevQuasiSpecularBar && (sampledLobe & claimedLobes) != 0;
    beta *= (1.0f / wpdfFwd) * f;
    if (beta.isAnyNonFinite()) break;
    // Terminate by Russian roulette instead of by a fixed depth limit, so that
    // high-albedo transport keeps the energy it is entitled to.
    if (depth > ROULETTE_MIN_DEPTH) {
      float survival{};
      float meanRadiance{
          dtree && guiding->pixelEstimate > 0 ? dtree->meanRadiance() : 0};
      if (meanRadiance > 0) {
        // Adjoint-driven Russian roulette (Vorba & Krivanek, SIGGRAPH
        // 2016; roulette only, no splitting): survive in proportion to the
        // expected pixel contribution of continuing the walk, which is the
        // throughput times the SD-tree's cached mean incident radiance,
        // relative to the pixel's estimate from the previous pass.
        survival = beta.average() * meanRadiance / guiding->pixelEstimate;
        survival = std::fmax(survival, 0.05f);
        survival = std::fmin(survival, 1.0f);
      } else {
        survival = std::fmin(ROULETTE_MAX_SURVIVAL, beta.maxComponent());
      }
      if (survival < 1.0f) {
        if (!(float(sampler) < survival)) break;
        beta *= 1.0f / survival;
      }
    }
    if (!isHair)
      MediumStack::Update(medium, allocator, mat, hit.instance, wo, wNext);
    ray = Ray{hit.point, wNext, EPS, INF};
  }
  return L;
}
