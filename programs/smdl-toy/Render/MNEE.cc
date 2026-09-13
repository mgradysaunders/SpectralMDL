#include "Render/MNEE.h"
#include "Render/Guiding.h"
#include "Render/Light.h"
#include "Render/Manifold.h"
#include "Render/PathStats.h"
#include "Render/Visibility.h"

#include <algorithm>

namespace {

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

// Whether a share (see `receiverShareOf()`) leaves anything to the other
// side of the partition in any band.
[[nodiscard]] bool isAnyBelowOne(const Color &share) noexcept {
  for (size_t b = 0; b < share.size(); b++)
    if (share[b] < 1.0f) return true;
  return false;
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

// What the straight-line refractive gather reports of its discovery: the
// family of the chain its straight segment crossed, empty when it never
// reached an estimate, and the kinds it ran one for. The caster
// refractive gather refuses that family for those kinds, since the
// straight-line gather owns it; see `MNEEChainFamily`.
struct StraightFamily final {
  MNEEChainFamily family{};
  int ranLobes{};
};

// Everything one manifold connection is weighed against that does not
// change from one connection to the next, which is every input the
// gather has except the connection itself.
class MNEEGather final {
public:
  // Manifold next-event estimation by reflection off the drawn caster;
  // see the definition.
  [[nodiscard]] Color gatherCasterReflection(const MNEECaster &caster,
                                             float casterPdf) const;

  // Manifold next-event estimation through the refractive interfaces
  // blocking the straight shadow segment; see the definition. Reports
  // what it discovered in `straight`.
  [[nodiscard]] Color gatherStraightRefraction(VisibilityWalk &walk,
                                               Hit &blocker, int maxDepth,
                                               int receiverMask,
                                               StraightFamily &straight) const;

  // Manifold next-event estimation through the drawn caster, seeded by
  // sampling it; see the definition.
  [[nodiscard]] Color
  gatherCasterRefraction(const MNEECaster &caster, float casterPdf,
                         int maxDepth, int receiverMask,
                         const StraightFamily &straight) const;

  // What one converged connection is worth; see the definition.
  // `isClaimed` sends a Dirac chain down the claimed-exclusive branch:
  // the caster refractive gather's chains, and every Dirac chain in
  // the biased claimed mode.
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
                           float scale, const Reseed &reseed,
                           bool isClaimed = false) const;
};

// Manifold next-event estimation by reflection off a caster for a light
// sample at a receiver, whether or not the straight segment to it is clear.
//
// The structure is the refractive glossy one: fix a half vector, solve from
// a start, and estimate the reciprocal of the chance of having reached that
// solution by drawing fresh starts until one lands on it again. What
// differs is where a start comes from. A mirror is nowhere near the line
// from the receiver to the light, so there is no crossing to seed from and
// the caster surface is sampled instead: one caster per estimate, drawn by
// the caller with the probability `casterPdf` the estimate divides out,
// and every start of the estimate drawn on it, so that every start comes
// from the same surface with the same material and the same frame and can
// re-find what the first walk found.
Color MNEEGather::gatherCasterReflection(const MNEECaster &caster,
                                         float casterPdf) const {
  const ManifoldTarget target{makeManifoldTarget(lightSample)};
  const SceneManifoldSurfaces surfaces{render.scene, path.time};
  Color result{};
  // One estimate per claimed kind, each with its own constraint (the
  // exact reflection, or a drawn microfacet normal), its own throughput
  // (the masked material query of that kind), and its own reciprocal
  // count. A material mixing both kinds weighs them inside the masked
  // query, so nothing else multiplies.
  for (const int kindLobe : {smdl::DF_DIRAC_BRDF, smdl::DF_GLOSSY_BRDF}) {
    if ((caster.reflectLobes & kindLobe) == 0) continue;
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
    if (!MNEECasterSet::samplePoint(render.scene, path.sampler, caster,
                                    path.time.fraction, startHit))
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
    const MNEEStats::Kind statKind{seed.isGlossy
                                       ? MNEEStats::CASTER_GLOSSY_REFLECT
                                       : MNEEStats::CASTER_DIRAC_REFLECT};
    // The first walk starts at the hit the offset was drawn at, which is a
    // start like any other: the offset's density cancels pointwise
    // whichever start it was drawn at, so nothing is gained by discarding
    // that one.
    result += reciprocalEstimate( //
        target, chain, statKind, smdl::DF_ALL, 1.0f / casterPdf,
        [&](ManifoldChain &reseeded) {
          Hit reseededHit{};
          if (MNEECasterSet::samplePoint(render.scene, path.sampler, caster,
                                         path.time.fraction, reseededHit)) {
            reseeded[0].vertex = vertexOf(reseededHit);
            return true;
          } else {
            return false;
          }
        });
  }
  return result;
}

// Manifold next-event estimation for a light sample whose straight
// shadow segment is blocked by claimed refractive interfaces (Hanika et
// al. 2015): discover the seed chain along the segment with
// `discoverStraightChain()`, solve the refracted connection, then
// assemble the receiver BSDF at the bent direction, the per-crossing
// Fresnel transmission and radiance compression, the transfer Jacobian,
// and the attenuation and visibility of every sub-segment. The Dirac
// result is MIS-weighted against the walk's own arrivals through the
// same chain, whose density per unit light solid angle is the receiver's
// continuation density times the host Fresnel transmissions times the
// transfer Jacobian; `MNEECoverage::coverWeight()` applies the
// complementary weight to those arrivals with the same formula from the
// same discovery, so the pair sums to one. The chain family the
// discovery found and the kinds estimated for it are reported in
// `straight`, which is how the caster refractive gather knows to leave
// that family alone.
Color MNEEGather::gatherStraightRefraction(VisibilityWalk &walk, Hit &blocker,
                                           int maxDepth, int receiverMask,
                                           StraightFamily &straight) const {
  // The sun gate: toward a gated environment sample the Dirac estimate
  // stands down, and its arrivals keep their ordinary weights by the
  // same predicate, so only a chain with a glossy claim is worth
  // discovering.
  const bool isEnvGated{lightSample.isInfinite &&
                        !render.mneeOptions.isEnvTarget(lightSample.wi)};
  MNEEChainSeed seed{};
  if (discoverStraightChain(path, walk, blocker, lightSample.wi,
                            isEnvGated
                                ? smdl::DF_GLOSSY_BTDF
                                : smdl::DF_DIRAC_BTDF | smdl::DF_GLOSSY_BTDF,
                            maxDepth, seed) != MNEEStraightEnd::REACHED)
    return {};
  ManifoldChain &chain{seed.chain};
  const int lobes{seed.lobes};
  straight.family = seed.family();
  const ManifoldTarget target{makeManifoldTarget(lightSample)};
  const SceneManifoldSurfaces surfaces{render.scene, path.time};
  MNEEStats *const stats{path.stats ? &path.stats->mnee() : nullptr};
  Color result{};
  // One estimate per kind the whole chain claims: the Dirac chain,
  // deterministic and weighed against the path tracer by re-walk MIS, and
  // the glossy chain below, claimed outright.
  if ((lobes & smdl::DF_DIRAC_BTDF) != 0) {
    straight.ranLobes |= smdl::DF_DIRAC_BTDF;
    for (int i = 0; i < chain.count; i++) chain[i].isGlossy = false;
    if (render.mneeOptions.biasedTrials > 0) {
      // The biased claimed mode: exactly `biasedTrials` walks, the
      // first from the straight seed and the rest jittered, the
      // converged solutions clustered and each distinct one summed
      // once at full weight. The arrival side drops every covered
      // arrival at a drawable target, so nothing is weighed twice, and
      // whatever the walks miss is the mode's knowing darkening. The
      // clustering needs the walks converged to the residual a re-find
      // is recognized at, as every clustered estimate's are: at the
      // sanity bound alone two walks to one solution can land outside
      // the identity fraction and be summed twice.
      chain.residualTolerance = MANIFOLD_RECIPROCAL_RESIDUAL;
      ManifoldSolutionSet solutions{};
      for (int trial = 0; trial < render.mneeOptions.biasedTrials; trial++) {
        if (trial > 0)
          for (int i = 0; i < chain.count; i++)
            chain[i].seedJitter = MNEE_STRAIGHT_SEED_JITTER *
                                  smdl::uniformDiskSample(float2(path.sampler));
        ManifoldConnection connection;
        ManifoldWalkReport report{};
        const bool hasConverged{solveManifoldConnection(
            surfaces, vertex.point, target, chain, connection, &report)};
        if (stats) stats->recordWalk(report);
        if (trial == 0 && stats)
          stats->recordEstimate(MNEEStats::STRAIGHT_DIRAC_REFRACT,
                                hasConverged);
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
        stats->recordTrials(MNEEStats::STRAIGHT_DIRAC_REFRACT,
                            render.mneeOptions.biasedTrials, false);
      result += solutions.sum();
    } else {
      ManifoldConnection connection;
      ManifoldWalkReport report{};
      const bool hasConverged{solveManifoldConnection(
          surfaces, vertex.point, target, chain, connection, &report)};
      if (stats) stats->recordWalk(report);
      if (stats)
        stats->recordEstimate(MNEEStats::STRAIGHT_DIRAC_REFRACT, hasConverged);
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
    if (!drawOffset(surfaces, seed.hits[i], seed.medium[i], lightSample.wi,
                    -lightSample.wi, smdl::DF_GLOSSY_BTDF, chain[i]))
      return result;
  straight.ranLobes |= smdl::DF_GLOSSY_BTDF;
  chain.residualTolerance = reciprocalResidualTolerance(chain);
  auto jitter{[&](ManifoldChain &reseeded) {
    for (int i = 0; i < reseeded.count; i++)
      reseeded[i].seedJitter = MNEE_STRAIGHT_SEED_JITTER *
                               smdl::uniformDiskSample(float2(path.sampler));
    return true;
  }};
  // Unlike the caster seeder, the straight-line crossings are one fixed
  // start, so the first walk is jittered like every trial or the
  // deterministic start would be over-counted.
  (void)jitter(chain);
  result +=
      reciprocalEstimate(target, chain, MNEEStats::STRAIGHT_GLOSSY_REFRACT,
                         receiverMask, 1.0f, jitter);
  return result;
}

// Manifold next-event estimation through a caster for a light sample at
// a receiver, whether or not the straight segment to it is clear: the
// prism's spectrum lands beside its shadow, where the straight line
// crosses nothing and the straight-line gather never runs.
//
// The structure is the reflective gather's: one caster per estimate,
// drawn by the caller with a probability the estimate divides out, a
// start drawn on it, a walk, and the reciprocal estimate of the chance
// of having reached the solution. What differs is that a refractive
// start is a whole chain, traced through the caster from the sampled
// crossing by `traceManifoldCasterSeed()`, and that the straight-line
// gather already owns one family of chains: the one its straight segment
// discovered, which this gather refuses for every kind the straight one
// estimated. Every other family that starts on the caster is this
// gather's, claimed exclusively for both kinds; the arrival side drops a
// Dirac arrival through such a family by the same family test, in
// `MNEECoverage::coverWeight()`.
//
// Each kind works on its own copy of the first trace's chain, because
// the reciprocal loop reseeds the chain it is given in place, and the
// glossy kind's offsets are drawn at the first trace's own hits.
Color MNEEGather::gatherCasterRefraction(const MNEECaster &caster,
                                         float casterPdf, int maxDepth,
                                         int receiverMask,
                                         const StraightFamily &straight) const {
  // The sun gate applies to this Dirac kind exactly as to the straight
  // one: the arrival keeps its ordinary weight toward a gated direction.
  int kinds{caster.refractLobes};
  if (lightSample.isInfinite && !render.mneeOptions.isEnvTarget(lightSample.wi))
    kinds &= ~smdl::DF_DIRAC_BTDF;
  if (kinds == 0) return {};
  const MNEEReceiver receiver{vertex.receiver()};
  const auto trace{[&](MNEEChainSeed &seed) {
    return traceManifoldCasterSeed(render, path, caster, receiver, maxDepth,
                                   seed);
  }};
  MNEEChainSeed first{};
  if (!trace(first)) return {};
  kinds &= first.lobes;
  if (kinds == 0) return {};
  const MNEEChainFamily family{first.family()};
  const bool isStraightFamily{straight.family.count > 0 &&
                              family == straight.family};
  const ManifoldTarget target{makeManifoldTarget(lightSample)};
  const SceneManifoldSurfaces surfaces{render.scene, path.time};
  Color result{};
  for (const int kindLobe : {smdl::DF_DIRAC_BTDF, smdl::DF_GLOSSY_BTDF}) {
    if ((kinds & kindLobe) == 0) continue;
    if (isStraightFamily && (straight.ranLobes & kindLobe) != 0) continue;
    ManifoldChain chain{first.chain};
    const bool isGlossy{kindLobe == smdl::DF_GLOSSY_BTDF};
    for (int i = 0; i < chain.count; i++) chain[i].isGlossy = isGlossy;
    if (isGlossy) {
      // The half vectors, drawn once and held, as on every glossy chain.
      bool hasOffsets{true};
      float3 prev{vertex.point};
      for (int i = 0; i < chain.count && hasOffsets; i++) {
        const float3 wTravel{normalize(first.hits[i].point - prev)};
        hasOffsets =
            drawOffset(surfaces, first.hits[i], first.medium[i], wTravel,
                       -wTravel, smdl::DF_GLOSSY_BTDF, chain[i]);
        prev = first.hits[i].point;
      }
      if (!hasOffsets) continue;
    }
    // Every kind is a reciprocal estimate here, so every kind's walks
    // converge to the residual a re-find is recognized at.
    chain.residualTolerance = reciprocalResidualTolerance(chain);
    const MNEEStats::Kind statKind{isGlossy ? MNEEStats::CASTER_GLOSSY_REFRACT
                                            : MNEEStats::CASTER_DIRAC_REFRACT};
    // A fresh start is a fresh trace, and only one of the same family
    // can re-find the solution: any other is a failed trial, which the
    // reciprocal count is free to include so long as every trial is
    // drawn the same way. The offsets and frames stay with the estimate.
    result += reciprocalEstimate(
        target, chain, statKind, receiverMask, 1.0f / casterPdf,
        [&](ManifoldChain &reseeded) {
          MNEEChainSeed fresh{};
          if (!trace(fresh) || (fresh.lobes & kindLobe) == 0 ||
              fresh.family() != family)
            return false;
          for (int i = 0; i < reseeded.count; i++) {
            const ManifoldVertexSeed &freshSeed{fresh.chain[i]};
            reseeded[i].vertex = freshSeed.vertex;
            reseeded[i].etaPrev = freshSeed.etaPrev;
            reseeded[i].etaNext = freshSeed.etaNext;
            reseeded[i].sideSign = freshSeed.sideSign;
            reseeded[i].claimedLobes = freshSeed.claimedLobes;
          }
          return true;
        },
        /*isClaimed=*/true);
  }
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
  // behind this receiver already claims through it (see `gatherDirect()`),
  // and the receiver's own mask keeps the lobes that receive, the rest of
  // its bounce staying with the arrivals; see `PathVertex::receiveMask`.
  float fPdf{};
  Color f{};
  if (!scatterEvaluate(vertex.scatterer, vertex.kind, vertex.wo, connection.wr,
                       fPdf, f, receiverMask & vertex.receiveMask))
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
  const MediumStack *segMedium{
      vertex.receiver().mediumToward(path.allocator, connection.wr)};
  float3 segStart{vertex.point};
  // One state for every converged crossing in turn; see
  // `Hit::applyGeometryToState()`.
  smdl::State crossState{makeRenderState(path.wavelengths, &path.allocator,
                                         gatherState.animationTime,
                                         path.wavelengthHero)};
  for (int i = 0; i < connection.count; i++) {
    const ManifoldConnectionVertex &crossing{connection.vertices[i]};
    VisibilityWalk segWalk{
        render, path, segMedium, segStart, crossing.geometry.point, &Tr};
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
  VisibilityWalk lightWalk{render, path, segMedium, segStart, lightPoint, &Tr};
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
    // A caster connection is claimed exclusively: a reflection was
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
// caster's) is divided out. `isClaimed` is `contribution()`'s, for a
// Dirac chain this loop claims rather than weighs.
template <typename Reseed>
Color MNEEGather::reciprocalEstimate(const ManifoldTarget &target,
                                     ManifoldChain &chain,
                                     MNEEStats::Kind statKind, int receiverMask,
                                     float scale, const Reseed &reseed,
                                     bool isClaimed) const {
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
      return contribution(chain, other, scale, receiverMask, isClaimed);
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
  Color value{contribution(chain, connection, scale, receiverMask, isClaimed)};
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

// Does the gather at a vertex of `kind` run the manifold estimators,
// whose light samples are drawn by area (`LightSampler::sample()`'s
// `shouldKeepDark`)? The answer is kept in `PathVertex::ranManifold`,
// which the walk carries to the arrival sites.
[[nodiscard]] bool gatherRunsManifold(const MNEEOptions &mneeOptions,
                                      VertexKind kind,
                                      bool isReceiver) noexcept {
  return kind != VertexKind::HAIR && mneeOptions.depth > 0 && isReceiver;
}
} // namespace

float MNEECoverage::coverWeight(const RenderContext &render, PathContext &path,
                                const ManifoldTarget &target, float lightPdf,
                                bool isCasterChain) const {
  // Whether the discovery reproduced the crossings the path took, and
  // whether the arrival was dropped as the caster gather's before the
  // solve; every other exit keeps the arrival at weight 1, so the
  // matched fraction is the share of covered arrivals the straight
  // gather can ever claim.
  bool isMatched{false};
  bool isClaimed{false};
  bool isDropped{false};
  SMDL_DEFER([&] {
    if (path.stats)
      path.stats->mnee().recordCover(isMatched   ? MNEEStats::Cover::MATCHED
                                     : isClaimed ? MNEEStats::Cover::CLAIMED
                                     : isDropped ? MNEEStats::Cover::DROPPED
                                                 : MNEEStats::Cover::UNMATCHED);
  });
  // A light the sampler cannot draw is covered by this arrival alone.
  if (!(lightPdf > 0.0f)) return 1.0f;
  // The weight of an arrival whose family the straight line does not
  // reproduce: the caster gather's to drop, or nobody's to keep.
  const float unownedWeight{isCasterChain ? 0.0f : 1.0f};
  isDropped = isCasterChain;
  // The gather's own walk of the straight line for this target: from
  // the receiver, in the medium its direction leaves into, aimed where
  // the gather's light sample aims (the finite light point, or the far
  // point an infinite light stands in with), asking for the Dirac kind.
  const float3 &wl{target.wl};
  const float3 aim{target.isInfinite
                       ? mReceiver.point + 2.0f * render.scene.boundRadius * wl
                       : target.point};
  VisibilityWalk walk{
      render,           path, mReceiver.mediumToward(path.allocator, wl),
      mReceiver.point,  aim,  nullptr,
      target.isInfinite};
  Hit blocker{};
  if (!walk.nextBlocker(&blocker)) return unownedWeight;
  MNEEChainSeed seed{};
  if (discoverStraightChain(path, walk, blocker, wl, smdl::DF_DIRAC_BTDF,
                            render.mneeOptions.depth,
                            seed) != MNEEStraightEnd::REACHED)
    return unownedWeight;
  if (seed.family() != mFamily) return unownedWeight;
  isDropped = false;
  // The biased claimed mode: the straight-line gather's clustered walks
  // claimed this family outright, so the arrival is dropped without a
  // solve; see `MNEEOptions::biasedTrials`.
  if (render.mneeOptions.biasedTrials > 0) {
    isClaimed = true;
    return 0.0f;
  }
  const ManifoldChain &chain{seed.chain};
  const int chainLength{mFamily.count};
  ManifoldConnection connection;
  ManifoldWalkReport report{};
  const SceneManifoldSurfaces surfaces{render.scene, path.time};
  const bool hasConverged{solveManifoldConnection(
      surfaces, mReceiver.point, target, chain, connection, &report)};
  if (path.stats) path.stats->mnee().recordRewalk(report);
  if (!hasConverged) return 1.0f;
  for (int i = 0; i < chainLength; i++) {
    const float scale{
        std::max(1e-3f, length(mChainHits[i].point - mReceiver.point))};
    if (!(length(connection.vertices[i].vertex.point - mChainHits[i].point) <
          MANIFOLD_IDENTITY_FRACTION * scale))
      return 1.0f;
  }
  isMatched = true;
  const float transfer{connection.measure(chain)};
  // The chance the continuation takes this chain, asked of each
  // interface rather than recomputed here, so that a layered or tinted
  // interface reports the selection it actually makes, with the
  // exterior index each crossing resolved against the discovery's medium
  // on its receiver side. The gather accumulates the same quantity the
  // same way, drawn from its own sampler, which is what keeps the pair
  // summing to one.
  float Q{1.0f};
  float3 prev{mReceiver.point};
  for (int i = 0; i < chainLength; i++) {
    const float3 toHit{mChainHits[i].point - prev};
    const float d{length(toHit)};
    if (!(d > 0.0f)) return 1.0f;
    const float3 travel{toHit / d};
    smdl::State &state{path.shadeHit(mChainHits[i], travel)};
    smdl::JIT::Material crossMaterial{state, mChainHits[i].materialDef};
    crossMaterial.setExteriorIOR(
        ExteriorIOR(seed.medium[i], crossMaterial, -travel));
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
             path.guiding, mChainHits[i].point,
             (crossMaterial.getLobes(crossMaterial.isInterior(-travel)) &
              smdl::DF_FINITE) != 0);
    prev = mChainHits[i].point;
  }
  const float arrivalPdf{mReceiverPdf * Q * transfer};
  if (!(arrivalPdf > 0.0f) || !std::isfinite(arrivalPdf)) return 1.0f;
  return smdl::powerHeuristic(arrivalPdf, lightPdf);
}

Color gatherDirect(const RenderContext &render, PathContext &path,
                   const smdl::State &gatherState, PathVertex &vertex) {
  const int mneeDepth{
      vertex.kind == VertexKind::HAIR ? 0 : render.mneeOptions.depth};
  vertex.receiveMask = smdl::DF_ALL;
  vertex.ranManifold = false;
  Color direct{};
  if (render.lights.empty()) return direct;
  LightSample &lightSample{path.gatherSample};
  // The light first, then which of the receiver's lobes receive it, then
  // the point on it. A manifold gather keeps the samples that radiate
  // nothing toward the receiver, since its connection arrives at the
  // light from elsewhere and reads the radiance from there, and draws a
  // sphere by area for the same reason, so whether one runs is settled
  // before the point is drawn; the environment's angular radius is that
  // of the direction drawn and its draw does not read the decision, so
  // it is drawn first. The plain estimate of a sample kept dark is zero
  // and is skipped below.
  float selectPMF{};
  const int lightIndex{
      render.lights.select(path.sampler, vertex.point, selectPMF)};
  if (lightIndex < 0) return direct;
  const bool isEnvLight{render.lights.isEnv(lightIndex)};
  if (isEnvLight &&
      !render.lights.sampleSelected(lightIndex, selectPMF, path.lightState,
                                    path.skyBasis, path.sampler, vertex.point,
                                    path.time.fraction, lightSample))
    return direct;
  const float angularRadius{
      isEnvLight ? render.lights.angularRadiusOfEnv(lightSample.wi)
                 : render.lights.angularRadius(lightIndex, vertex.point)};
  const int receiveLobes{
      manifoldReceiverLobes(vertex.finiteLobes, vertex.glossyWidth,
                            MNEE_RECEIVER_EXTENT_RATIO * angularRadius)};
  vertex.receiveMask = smdl::DF_ALL & ~(vertex.finiteLobes & ~receiveLobes);
  const bool shouldRunManifold{
      receiveLobes != 0 &&
      gatherRunsManifold(render.mneeOptions, vertex.kind, vertex.isReceiver)};
  vertex.ranManifold = shouldRunManifold;
  if (!isEnvLight &&
      !render.lights.sampleSelected(
          lightIndex, selectPMF, path.lightState, path.skyBasis, path.sampler,
          vertex.point, path.time.fraction, lightSample, shouldRunManifold))
    return direct;
  {
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
      if (lightSample.Li.isAllZero()) return;
      float fPdf{};
      Color f{};
      bool hasValue{neeMask != 0 &&
                    scatterEvaluate(vertex.scatterer, vertex.kind, vertex.wo,
                                    lightSample.wi, fPdf, f, neeMask)};
      if (!hasValue) f.fill(0.0f);
      // The claimed lobes stay with light sampling to the share of the
      // receiver's bounce its gather does not produce, read against this
      // light from that receiver; see `PathVertex::reflectShareBehind`.
      // Every evaluation reports the same unmasked density.
      if (shouldSplit) {
        const auto addRemainder{[&](int lobes, const ReceiverShare &behind,
                                    const float3 &pointBehind) {
          const Color &share{behind.at(
              render.lights.angularRadiusOf(lightSample, pointBehind))};
          if (lobes == 0 || !isAnyBelowOne(share)) return;
          float pdfLobes{};
          Color fLobes{};
          if (!scatterEvaluate(vertex.scatterer, vertex.kind, vertex.wo,
                               lightSample.wi, pdfLobes, fLobes, lobes))
            return;
          if (!hasValue) fPdf = pdfLobes;
          hasValue = true;
          for (size_t b = 0; b < f.size(); b++)
            f[b] += (1.0f - share[b]) * fLobes[b];
        }};
        addRemainder(lightClaim.reflectLobes, vertex.reflectShareBehind,
                     vertex.reflectPointBehind);
        addRemainder(lightClaim.refractLobes, vertex.refractShareBehind,
                     vertex.refractPointBehind);
      }
      if (!hasValue) return;
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
    // The medium the shadow segment starts in, across the receiver's
    // own interface where the light direction transmits through it.
    const MediumStack *gatherMedium{
        vertex.receiver().mediumToward(path.allocator, lightSample.wi)};
    if (!shouldRunManifold) {
      if (Color Tr{1.0f};
          neeMask != 0 &&
          testVisibility(render, path, gatherMedium, vertex.point,
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
                          gatherMedium,
                          vertex.point,
                          lightSample.target,
                          &Tr,
                          lightSample.isInfinite};
      // The chain gather of the receiver behind this vertex already
      // claims every chain that starts with this vertex's claimed
      // transmission kinds, so the refractive gathers weigh their
      // connections with the rest of the BSDF only.
      const int receiverMask{smdl::DF_ALL &
                             ~vertex.reachableClaim.refractLobes};
      StraightFamily straight{};
      if (!walk.nextBlocker(&blocker)) {
        if (Tr.maxComponent() > 0.0f) gatherPlain(Tr);
      } else {
        direct += mneeGather.gatherStraightRefraction(walk, blocker, mneeDepth,
                                                      receiverMask, straight);
      }
      // The caster gathers are additive rather than an alternative: a
      // mirror is nowhere near the line to the light, and a prism's
      // spectrum lands beside its shadow, so whether that line is clear
      // says nothing about whether there is a connection to find. One
      // caster is drawn for both, by its solid angle from this vertex,
      // with the probability each estimate divides out. The reflective
      // one searches toward the caustic targets only (see
      // `LightSample::caustic`); the refractive one toward every light,
      // as the straight-line gather does, since the glossy chains' share
      // is dropped at every arrival.
      if (render.mneeOptions.casters && !render.mneeOptions.casters->empty()) {
        float casterPdf{};
        if (const MNEECaster *caster{render.mneeOptions.casters->sampleCaster(
                path.sampler, vertex.point, casterPdf)}) {
          if (lightSample.isCaustic)
            direct += mneeGather.gatherCasterReflection(*caster, casterPdf);
          direct += mneeGather.gatherCasterRefraction(
              *caster, casterPdf, mneeDepth, receiverMask, straight);
        }
      }
    }
  }
  return direct;
}

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

ReceiverShare receiverShareOf(const smdl::JIT::Material &material,
                              int finiteLobes, float glossyWidth,
                              const float3 &wo, const float3 &wNext,
                              const Color &f, bool isDiracBounce,
                              bool isReceiver) {
  ReceiverShare share{};
  if (!isReceiver || isDiracBounce) return share;
  share.glossyWidth = glossyWidth;
  share.withGlossy.fill(1.0f);
  const int glossyLobes{finiteLobes & smdl::DF_GLOSSY};
  if (glossyLobes == 0) {
    share.withoutGlossy.fill(1.0f);
  } else if ((finiteLobes & smdl::DF_SMOOTH) != 0) {
    float pdfUnused{}, pdfRevUnused{};
    Color fGlossy{};
    if (material.scatterEvaluate(wo, wNext, pdfUnused, pdfRevUnused, fGlossy,
                                 glossyLobes))
      for (size_t b = 0; b < share.withoutGlossy.size(); b++)
        share.withoutGlossy[b] =
            f[b] > 0.0f ? std::clamp(1.0f - fGlossy[b] / f[b], 0.0f, 1.0f)
                        : 0.0f;
    else
      share.withoutGlossy.fill(1.0f);
  }
  return share;
}
