#include "PathTracing.h"
#include "Guiding.h"
#include "Light.h"

[[nodiscard]] static bool
testVisibility(const Scene &scene, Sampler &sampler, const Color &wavelengths,
               smdl::BumpPtrAllocator &allocator, const MediumStack *medium,
               const float3 &point0, const float3 &point1, Color &beta) {
  float d{length(point1 - point0)};
  // The normalized shadow ray direction for `State::direction`, or zero
  // when the endpoints coincide, honoring the zero-means-off convention.
  const float3 shadowDir{d > 0 ? (point1 - point0) / d : float3{}};
  // The self-intersection offsets are parametric, so for segments
  // longer than one scene unit they are rescaled to stay near `EPS` in
  // WORLD units: a sun shadow ray spans the whole scene, and an offset
  // scaled by that length is wide enough to skip real geometry, and a
  // boundary crossing inside the skipped sliver desyncs the medium
  // stack for the entire segment.
  const float paramEPS{d > 1.0f ? EPS / d : EPS};
  Ray ray{point0, point1 - point0, paramEPS, 1.0f - paramEPS};
  // The parameter up to which the medium has been integrated, tracked
  // separately from `ray.tmin` deliberately: integrating only
  // `[tmin, tmax]` of each cast would skip a scene-scaled sliver of
  // medium at every pass-through restart, and where such a gap crosses
  // dense medium the skipped optical depth reads as a bright seam in
  // the shadow.
  float tCovered{0.0f};
  while (ray.tmin < ray.tmax) {
    Hit hit{};
    bool hitSurface{scene.intersect(ray, hit)};
    // Attenuate over the span actually traveled, hit or miss
    // (`Scene::intersect` narrows `tmax` to the hit parameter on a
    // hit). The parametrization spans `[0, 1]` over the segment, so the
    // world-space span is scaled by `d` and the medium sees a unit
    // direction with distances in scene units. The epsilon slivers the
    // casts exclude are attributed to whichever side of the boundary
    // this iteration integrates.
    {
      const Medium segmentMedium{medium, wavelengths, ray(tCovered), shadowDir};
      segmentMedium.attenuate(sampler, (ray.tmax - tCovered) * d, beta);
      tCovered = ray.tmax;
      if (!(beta.maxComponent() > 0.0f)) {
        return false; // Fully absorbed already.
      }
    }
    if (!hitSurface) {
      break;
    }
    // A null interface passes shadow rays straight through: no opacity
    // and no blocking, only the medium-stack bookkeeping, which needs
    // the full instance.
    if (hit.material->isNullInterface()) {
      auto state{makeRenderState(wavelengths, &allocator)};
      hit.apply_geometry_to_state(state, shadowDir);
      smdl::JIT::MaterialInstance materialInstance{state, hit.material};
      MediumStack::Update(medium, allocator, materialInstance, hit.instance,
                          -ray.dir, ray.dir);
      ray.tmin = smdl::incrementFloat(ray.tmax + paramEPS);
      ray.tmax = 1.0f - paramEPS;
      continue;
    }
    // A statically opaque material blocks without any material work.
    if (hit.material->isAlwaysOpaque()) {
      return false;
    }
    auto state{makeRenderState(wavelengths, &allocator)};
    // Only the ray direction is populated; the LOD fields stay zero so
    // opacity evaluates at full fidelity, the conservative choice for
    // shadow rays.
    hit.apply_geometry_to_state(state, shadowDir);
    if (float opacity{hit.material->evaluateOpacity(state)};
        opacity == 1 || float(sampler) < opacity) {
      return false; // Blocks visibility!
    }
    // Only an actual pass-through needs the full instance, to keep the
    // medium stack current across the cutout.
    smdl::JIT::MaterialInstance materialInstance{state, hit.material};
    MediumStack::Update(medium, allocator, materialInstance, hit.instance,
                        -ray.dir, ray.dir);
    ray.tmin = smdl::incrementFloat(ray.tmax + paramEPS);
    ray.tmax = 1.0f - paramEPS;
  }
  return true;
}

// The depth after which the walk is terminated by Russian roulette rather
// than continued unconditionally.
static constexpr uint64_t ROULETTE_MIN_DEPTH{4};

// The largest survival probability Russian roulette will use, so that every
// path terminates eventually no matter how bright its throughput is.
static constexpr float ROULETTE_MAX_SURVIVAL{0.95f};

// The ray cone spread growth in radians added by a non-delta bounce whose
// material has no diffuse component. Crude lobe-class heuristic: the JIT
// instance exposes only the DF_* class flags, not per-lobe roughness.
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
[[nodiscard]] static bool
scatter_evaluate(const smdl::JIT::MaterialInstance &materialInstance,
                 VertexKind kind, const float3 &wo, const float3 &wi,
                 float &pdfFwd, Color &f) {
  if (kind == VertexKind::VOLUME) {
    float phase{materialInstance.volumeScatterEvaluate(wo, wi)};
    pdfFwd = phase;
    f = Color(phase);
    return phase > 0;
  } else {
    // The JIT ABI reports a reverse PDF alongside every forward PDF,
    // which a forward path tracer never consumes.
    float pdfRevUnused{};
    return kind == VertexKind::HAIR
               ? materialInstance.hairScatterEvaluate(wo, wi, pdfFwd,
                                                      pdfRevUnused, f)
               : materialInstance.scatterEvaluate(wo, wi, pdfFwd, pdfRevUnused,
                                                  f);
  }
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
[[nodiscard]] static Color
gatherDirect(const Scene &scene, Sampler &sampler, const Color &wavelengths,
             smdl::BumpPtrAllocator &allocator, const LightSampler &lights,
             const smdl::State &gatherState,
             const smdl::JIT::MaterialInstance &materialInstance,
             VertexKind kind, const DTree *dtree, float bsdfFraction,
             const MediumStack *medium, const float3 &point, const float3 &wo) {
  Color direct{};
  if (lights.empty()) return direct;
  LightSampler::LightSample lightSample{};
  if (lights.sample(gatherState, sampler, point, lightSample)) {
    float fpdfFwd{};
    Color f{};
    if (scatter_evaluate(materialInstance, kind, wo, lightSample.wi, fpdfFwd,
                         f)) {
      // The competing density in the MIS weight must be the density the
      // continuation sampler actually assigns to this direction: the BSDF
      // alone, or the guided mixture when the SD-tree participates at
      // this vertex. Weighing against the raw BSDF density while the
      // continuation samples the mixture makes the two halves sum past 1
      // and reads several percent bright.
      const float continuationPdf{dtree ? bsdfFraction * fpdfFwd +
                                              (1.0f - bsdfFraction) *
                                                  dtree->pdf(lightSample.wi)
                                        : fpdfFwd};
      if (testVisibility(scene, sampler, wavelengths, allocator, medium, point,
                         lightSample.target, f)) {
        auto D{f * lightSample.Li / lightSample.pdf};
        if (!D.isAnyNonFinite()) {
          // A delta light is unreachable by the continuation, so its
          // MIS weight is 1.
          if (!lightSample.isDelta)
            D *= powerHeuristic(lightSample.pdf, continuationPdf);
          direct += D;
        }
      }
    }
  }
  return direct;
}

Color tracePath(smdl::Compiler &compiler, const Scene &scene, Sampler &sampler,
                const Color &wavelengths, smdl::BumpPtrAllocator &allocator,
                Ray ray, float cameraWeight, float cameraConeAngle,
                const MediumStack *exteriorMedium, uint64_t maxDepth,
                const LightSampler &lights, const Guiding *guiding,
                GuideRecord *records, uint64_t &numRecords) {
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
  float3 prevPoint{};
  // The number of path vertices so far, counting the camera as the first.
  uint64_t depth{1};
  while (depth < maxDepth) {
    auto hit{Hit{}};
    bool hitSurface{scene.intersect(ray, hit)};
    if (const Medium segmentMedium{medium, wavelengths, ray.org, ray.dir};
        segmentMedium.hasMedium()) {
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
          segmentMedium.sampleDistance(sampler, ray.tmax, t, beta, emitted)};
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
        // The phase function of the vertex: the medium's own, or with
        // additive overlap the component the collision picked.
        const auto &phaseInstance{*segmentMedium.scatterInstance()};
        {
          // The SD-tree never participates at volume vertices, so the
          // continuation density the gather weighs against is the phase
          // function alone.
          const Color direct{gatherDirect(
              scene, sampler, wavelengths, allocator, lights, gatherState,
              phaseInstance, VertexKind::VOLUME, /*dtree=*/nullptr,
              /*bsdfFraction=*/1.0f, medium, point, wo)};
          L += beta * direct;
          if (record) record->direct = direct;
        }
        // Sample the vertex's phase function. It returns the phase
        // value, which is also the solid-angle PDF of having sampled it,
        // so the throughput weight is exactly 1 and `beta` is unchanged.
        float3 wNext{};
        float phase{
            phaseInstance.volumeScatterSample(float4(sampler), wo, wNext)};
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
        const float weight{
            depth == 1 || prevDelta
                ? 1.0f
                : powerHeuristic(prevWpdf, lights.envSelectionPMF() * Lipdf)};
        auto Lenv{beta * Li * weight};
        if (!Lenv.isAnyNonFinite()) {
          L += Lenv;
          // Fold the throughput-free estimate into the previous
          // vertex's record so the training target keeps the full
          // estimator's expectation; the copy without the bounce weight
          // trains the tree along the continuation direction itself.
          if (records && numRecords > 0) {
            GuideRecord &prev{records[numRecords - 1]};
            for (size_t b = 0; b < prev.beta.size(); b++) {
              if (prev.beta[b] > 0)
                prev.direct[b] += beta[b] / prev.beta[b] * Li[b] * weight;
              prev.continuationEmission[b] = Li[b] * weight;
            }
          }
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
    hit.apply_geometry_to_state(state, ray.dir);
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
    auto materialInstance{smdl::JIT::MaterialInstance(state, hit.material)};
    materialInstance.setExteriorIOR(
        ExteriorIOR(medium, materialInstance, -ray.dir));
    // A hair vertex: a curve hit whose material binds `material.hair`,
    // which routes scattering through the hair entry points. A curve hit
    // whose material has no hair keeps the ordinary surface path, and a
    // hair material on non-curve geometry shades as its (typically
    // default) surface. Hair fibers are also not medium boundaries:
    // transmission through the fiber is part of the BSDF, so the
    // null-interface hop and the medium-stack bookkeeping stand down.
    const bool hairVertex{hit.instance->isCurves() && hit.material->hasHair()};
    // A null interface is a boundary that scatters nothing itself but
    // encloses a participating medium, e.g., a smoke container: pass
    // straight through like a cutout hop, committing the distance and
    // width but not the order, with only the medium-stack bookkeeping.
    if (hit.material->isNullInterface() && !hairVertex) {
      MediumStack::Update(medium, allocator, materialInstance, hit.instance,
                          -ray.dir, ray.dir);
      travel += castDistance;
      width += spread * castDistance;
      ray = Ray{hit.point, ray.dir, EPS, INF};
      continue;
    }
    if (float opacity{materialInstance.getCutoutOpacity()};
        opacity < 1 && (opacity == 0 || float(sampler) > opacity)) {
      MediumStack::Update(medium, allocator, materialInstance, hit.instance,
                          -ray.dir, ray.dir);
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
    if (materialInstance.hasEmission()) {
      Color Le{};
      if (lights.emittedRadiance(materialInstance, hit.meshInstanceIndex, wo,
                                 Le)) {
        const float weight{
            depth == 2 || prevDelta
                ? 1.0f
                : powerHeuristic(prevWpdf, lights.solidAnglePDF(
                                               hit.meshInstanceIndex, hit.point,
                                               hit.geometryNormal, prevPoint))};
        auto Lem{beta * Le * weight};
        if (!Lem.isAnyNonFinite()) {
          L += Lem;
          // Fold the throughput-free estimate into the previous
          // vertex's record so the training target keeps the full
          // estimator's expectation; the copy without the bounce weight
          // trains the tree along the continuation direction itself.
          if (records && numRecords > 1) {
            GuideRecord &prev{records[numRecords - 2]};
            for (size_t b = 0; b < prev.beta.size(); b++) {
              if (prev.beta[b] > 0)
                prev.direct[b] += beta[b] / prev.beta[b] * Le[b] * weight;
              prev.continuationEmission[b] = Le[b] * weight;
            }
          }
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
    const DTree *dtree{};
    if (guideTree && !hairVertex) {
      int dfFlags{materialInstance.instance.df_flags_surface |
                  materialInstance.instance.df_flags_backface};
      if ((dfFlags & (smdl::JIT::DF_DIFFUSE | smdl::JIT::DF_GLOSSY)) != 0)
        dtree = &guideTree->samplingAt(hit.point);
    }
    // The one-sample-MIS mixture weight at this vertex: the cell's
    // learned weight unless pinned for experiments. Meaningful only when
    // `dtree` is non-null, and shared by the gather below, whose MIS
    // weight has to match the continuation density.
    const float bsdfFraction{!dtree || guiding->bsdfFractionFixed
                                 ? guiding ? guiding->bsdfFraction : 1.0f
                                 : dtree->mixtureAlpha};
    // Gather direct lighting at this vertex, before the bounce, so the
    // cone the gather rays inherit is the arrival cone.
    {
      const Color direct{gatherDirect(
          scene, sampler, wavelengths, allocator, lights, gatherState,
          materialInstance, hairVertex ? VertexKind::HAIR : VertexKind::SURFACE,
          dtree, bsdfFraction, medium, hit.point, wo)};
      L += beta * direct;
      if (record) record->direct = direct;
    }

    float3 wNext{};
    bool isDeltaBounce{};
    if (hairVertex) {
      // There are no delta hair distributions, so every accepted sample
      // is a finite-density direction.
      if (!materialInstance.hairScatterSample(float4(sampler), wo, wNext,
                                              wpdfFwd, pdfRevUnused, f)) {
        break;
      }
    } else if (dtree) {
      if (float(sampler) < bsdfFraction) {
        if (!materialInstance.scatterSample(float4(sampler), wo, wNext, wpdfFwd,
                                            pdfRevUnused, f, isDeltaBounce)) {
          break;
        }
        if (isDeltaBounce) {
          // The delta lobe folds its density into `f` (unit PDF by
          // convention), and the tree cannot compete with it, so the only
          // density left is the discrete chance of having chosen BSDF
          // sampling at all.
          wpdfFwd = bsdfFraction;
        } else {
          const float fpdf{wpdfFwd};
          const float gpdf{dtree->pdf(wNext)};
          wpdfFwd = bsdfFraction * fpdf + (1.0f - bsdfFraction) * gpdf;
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
        if (!(guidePDF > 0) || !materialInstance.scatterEvaluate(
                                   wo, wi, fpdfFwd, pdfRevUnused, f)) {
          break;
        }
        wNext = wi;
        isDeltaBounce = false;
        wpdfFwd = bsdfFraction * fpdfFwd + (1.0f - bsdfFraction) * guidePDF;
        if (record) {
          record->pdfBSDF = fpdfFwd;
          record->pdfGuide = guidePDF;
          record->fAvg = f.average();
        }
      }
    } else if (!materialInstance.scatterSample(float4(sampler), wo, wNext,
                                               wpdfFwd, pdfRevUnused, f,
                                               isDeltaBounce)) {
      break;
    }
    // Grow the ray cone for the bounce. Delta bounces leave the spread
    // unchanged; otherwise the growth is a crude lobe-class heuristic since
    // the instance exposes only the DF_* class flags, and a material whose
    // sampled lobe was specular still gets its diffuse growth, which errs
    // toward more prefiltering deeper in the path.
    if (!isDeltaBounce) {
      const int dfFlags{materialInstance.instance.df_flags_surface |
                        materialInstance.instance.df_flags_backface};
      spread = std::min(spread + ((dfFlags & smdl::JIT::DF_DIFFUSE) != 0
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
    prevDelta = isDeltaBounce;
    prevPoint = hit.point;
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
    if (!hairVertex)
      MediumStack::Update(medium, allocator, materialInstance, hit.instance, wo,
                          wNext);
    ray = Ray{hit.point, wNext, EPS, INF};
  }
  return L;
}
