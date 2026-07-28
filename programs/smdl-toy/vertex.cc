#include "vertex.h"
#include "guiding.h"
#include "light.h"

bool test_visibility(const Scene &scene, Sampler &sampler,
                     const Color &wavelengths,
                     smdl::BumpPtrAllocator &allocator, //
                     const MediumStack *medium, const float3 &point0,
                     const float3 &point1, Color &beta) {
  float d{length(point1 - point0)};
  Ray ray{point0, point1 - point0, EPS, 1.0f - EPS};
  while (ray.tmin < ray.tmax) {
    Hit hit{};
    bool hitSurface{scene.intersect(ray, hit)};
    // Attenuate over the span actually traveled, which must happen whether or
    // not there is a hit. `Scene::intersect` narrows `tmax` to the hit
    // parameter on a hit and leaves it at the end of the segment on a miss.
    if (medium && medium->materialInstance.hasMedium()) {
      Color muA = Color(medium->materialInstance.getAbsorptionCoefficient());
      Color muS = Color(medium->materialInstance.getScatteringCoefficient());
      Color mu = muA + muS;
      Color Tr{};
      for (size_t i = 0; i < WAVELENGTH_BASE_MAX; i++)
        Tr[i] = std::exp(-mu[i] * (ray.tmax - ray.tmin) * d);
      beta *= Tr;
    }
    if (!hitSurface) {
      break;
    }
    smdl::State state{};
    state.allocator = &allocator;
    state.wavelength_base = wavelengths.data();
    state.wavelength_min = WAVELENGTH_MIN;
    state.wavelength_max = WAVELENGTH_MAX;
    hit.apply_geometry_to_state(state);
    smdl::JIT::MaterialInstance materialInstance{state, hit.material};
    if (float opacity{materialInstance.getCutoutOpacity()};
        opacity == 1 || float(sampler) < opacity) {
      return false; // Blocks visibility!
    }
    MediumStack::Update(medium, allocator, materialInstance, -ray.dir, ray.dir);
    ray.tmin = smdl::incrementFloat(ray.tmax + EPS);
    ray.tmax = 1.0f - EPS;
  }
  return true;
}

bool trace_nearest(const Scene &scene, Sampler &sampler,
                   const Color &wavelengths,
                   smdl::BumpPtrAllocator &allocator, //
                   const MediumStack *medium, Ray ray, Hit &hit,
                   smdl::JIT::MaterialInstance &materialInstance, Color &beta) {
  while (true) {
    bool hitSurface{scene.intersect(ray, hit)};
    // Attenuate over the span actually traveled, whether or not there is a
    // hit. On a miss the span extends to infinity, so clamp the distance:
    // wavelengths with zero extinction keep transmittance 1 instead of
    // producing 0 × ∞.
    if (medium && medium->materialInstance.hasMedium()) {
      Color muA = Color(medium->materialInstance.getAbsorptionCoefficient());
      Color muS = Color(medium->materialInstance.getScatteringCoefficient());
      Color mu = muA + muS;
      float d{std::min(ray.tmax - ray.tmin, std::numeric_limits<float>::max())};
      for (size_t i = 0; i < WAVELENGTH_BASE_MAX; i++)
        beta[i] *= std::exp(-mu[i] * d);
    }
    if (!hitSurface) {
      return false;
    }
    smdl::State state{};
    state.allocator = &allocator;
    state.wavelength_base = wavelengths.data();
    state.wavelength_min = WAVELENGTH_MIN;
    state.wavelength_max = WAVELENGTH_MAX;
    hit.apply_geometry_to_state(state);
    materialInstance = smdl::JIT::MaterialInstance(state, hit.material);
    if (float opacity{materialInstance.getCutoutOpacity()};
        opacity == 1 || float(sampler) < opacity) {
      return true; // A real hit!
    }
    MediumStack::Update(medium, allocator, materialInstance, -ray.dir, ray.dir);
    ray.tmin = smdl::incrementFloat(ray.tmax + EPS);
    ray.tmax = INF;
  }
}

/// The depth after which the walk is terminated by Russian roulette rather
/// than continued unconditionally.
static constexpr uint64_t ROULETTE_MIN_DEPTH{4};

/// The largest survival probability Russian roulette will use, so that every
/// path terminates eventually no matter how bright its throughput is.
static constexpr float ROULETTE_MAX_SURVIVAL{0.95f};

/// The probability of drawing the bounce direction from the BSDF rather
/// than the SD-tree when guiding is active (the one-sample-MIS mixture
/// weight, α in the paper).
static constexpr float GUIDE_BSDF_FRACTION{0.5f};

uint64_t random_walk(smdl::Compiler &compiler, const Scene &scene,
                     Sampler &sampler, const Color &wavelengths,
                     smdl::BumpPtrAllocator &allocator,
                     smdl::Transport transport, Vertex path0, float wpdfFwd,
                     uint64_t maxDepth, Vertex *path,
                     const LightSampler *lights, const Guiding *guiding) {
  const EnvLight *envLight{lights ? lights->env() : nullptr};
  const STree *guideTree{guiding ? guiding->tree : nullptr};
  if (maxDepth == 0) return 0;
  path[0] = std::move(path0);
  if (maxDepth == 1) return 1;

  // Default construct a medium stack, assuming we start on
  // the exterior of all materials with interior participating
  // media.
  const MediumStack *medium{};

  // We declare the state here and set up the variables that never
  // change. The other state variables get updated at every vertex
  // on the path by `Hit::apply_geometry_to_state()`.
  smdl::State state{};
  state.allocator = &allocator;
  state.wavelength_base = wavelengths.data();
  state.wavelength_min = WAVELENGTH_MIN;
  state.wavelength_max = WAVELENGTH_MAX;
  state.transport = transport;

  Color beta{(1.0f / wpdfFwd) * path[0].beta};
  Color f{};
  float wpdfRev{};
  Ray ray{path[0].point, path[0].wNext, EPS, INF};
  uint64_t depth{1};
  while (depth < maxDepth) {
    auto &vertexPrev{path[depth - 1]};
    auto &vertex{path[depth]};
    vertex = Vertex{};

    auto hit{Hit{}};
    bool hitSurface{scene.intersect(ray, hit)};
    if (medium && medium->materialInstance.hasMedium()) {
      Color muA = Color(medium->materialInstance.getAbsorptionCoefficient());
      Color muS = Color(medium->materialInstance.getScatteringCoefficient());
      Color mu = muA + muS;
      float t =
          -std::log1p(-float(sampler)) / mu[sampler.index(WAVELENGTH_BASE_MAX)];
      t = std::min(t, ray.tmax);
      Color Tr{};
      for (size_t i = 0; i < WAVELENGTH_BASE_MAX; i++)
        Tr[i] =
            std::exp(-mu[i] * std::min(t, std::numeric_limits<float>::max()));
      if (t < ray.tmax) {
        beta *= muS * Tr / (mu * Tr).average();
        ++depth;
        vertex.point = ray(t);
        vertex.beta = beta;
        vertex.medium = medium;
        vertex.materialInstance = medium->materialInstance;
        vertex.pdfFwd = vertexPrev.convert_pdf(wpdfFwd, vertex);
        vertex.isVolume = true;
        // Sample the material's own phase function. It returns the phase
        // value, which is also the solid-angle PDF of having sampled it,
        // so the throughput weight is exactly 1 and `beta` is unchanged.
        float phase{medium->materialInstance.volumeScatterSample(
            float4(sampler), -ray.dir, vertex.wNext)};
        if (!(phase > 0)) {
          break;
        }
        wpdfFwd = phase;
        wpdfRev = phase;
        vertex.pdfWNext = phase;
        vertexPrev.pdfRev = vertex.convert_pdf(wpdfRev, vertexPrev);
        ray = Ray{vertex.point, vertex.wNext, EPS, INF};
        continue;
      } else {
        beta *= Tr / Tr.average();
      }
    }
    if (!hitSurface) {
      if (transport == smdl::TRANSPORT_RADIANCE) {
        ++depth;
        vertex.point =
            vertexPrev.point + 2 * scene.boundRadius * vertexPrev.wNext;
        vertex.beta = beta;
        vertex.wNext = vertexPrev.wNext;
        vertex.pdfFwd = wpdfFwd;
        vertex.isInfiniteLight = true;
      }
      break;
    }

    hit.apply_geometry_to_state(state);
    auto materialInstance{smdl::JIT::MaterialInstance(state, hit.material)};
    materialInstance.setExteriorIOR(
        ExteriorIOR(medium, materialInstance, -ray.dir));
    if (float opacity{materialInstance.getCutoutOpacity()};
        opacity < 1 && (opacity == 0 || float(sampler) > opacity)) {
      MediumStack::Update(medium, allocator, materialInstance, -ray.dir,
                          ray.dir);
      ray = Ray{hit.point, ray.dir, EPS, INF};
      continue;
    }

    ++depth;
    vertex.point = hit.point;
    vertex.beta = beta;
    vertex.medium = medium;
    vertex.materialInstance = materialInstance;
    vertex.meshInstanceIndex = hit.meshInstanceIndex;
    vertex.pdfFwd = vertexPrev.convert_pdf(wpdfFwd, vertex);

    // With guiding active, non-delta surface bounces one-sample-MIS the
    // SD-tree against the BSDF. Materials whose scattering is purely a
    // Dirac delta bypass guiding entirely — the tree cannot produce their
    // directions and evaluating the BSDF at a guided direction would
    // always be zero.
    const DTree *dtree{};
    if (guideTree) {
      int dfFlags{materialInstance.instance.df_flags_surface |
                  materialInstance.instance.df_flags_backface};
      if ((dfFlags & (smdl::JIT::DF_DIFFUSE | smdl::JIT::DF_GLOSSY)) != 0)
        dtree = &guideTree->samplingAt(vertex.point);
    }
    if (dtree) {
      const float3 wo{-vertexPrev.wNext};
      if (float(sampler) < GUIDE_BSDF_FRACTION) {
        if (!materialInstance.scatterSample(float4(sampler), wo, vertex.wNext,
                                            wpdfFwd, wpdfRev, f,
                                            vertex.isDeltaBounce)) {
          break;
        }
        if (vertex.isDeltaBounce) {
          // The delta lobe folds its density into `f` (unit PDF by
          // convention), and the tree cannot compete with it, so the only
          // density left is the discrete chance of having chosen BSDF
          // sampling at all.
          wpdfFwd = GUIDE_BSDF_FRACTION;
          wpdfRev = GUIDE_BSDF_FRACTION;
        } else {
          wpdfFwd = GUIDE_BSDF_FRACTION * wpdfFwd +
                    (1.0f - GUIDE_BSDF_FRACTION) * dtree->pdf(vertex.wNext);
        }
      } else {
        float guidePDF{};
        float3 wi{dtree->sampleDirection(sampler, guidePDF)};
        float fpdfFwd{};
        float fpdfRev{};
        if (!(guidePDF > 0) ||
            !materialInstance.scatterEvaluate(wo, wi, fpdfFwd, fpdfRev, f)) {
          break;
        }
        vertex.wNext = wi;
        vertex.isDeltaBounce = false;
        wpdfFwd = GUIDE_BSDF_FRACTION * fpdfFwd +
                  (1.0f - GUIDE_BSDF_FRACTION) * guidePDF;
        wpdfRev = fpdfRev;
      }
    } else if (depth > 2 || !envLight) {
      if (!materialInstance.scatterSample(float4(sampler), -vertexPrev.wNext,
                                          vertex.wNext, wpdfFwd, wpdfRev, f,
                                          vertex.isDeltaBounce)) {
        break;
      }
    } else {
      struct SampleResult final {
        float3 wi{};
        float Lpdf{};
        float fpdf{};
        float fpdfRev{};
        Color f{};
      };
      auto doSampleLight{[&] {
        SampleResult result{};
        Color Li{};
        result.wi = envLight->Li_sample(compiler, state, float2(sampler),
                                        result.Lpdf, Li);
        float fpdfFwd{};
        float fpdfRev{};
        if (materialInstance.scatterEvaluate(-vertexPrev.wNext, result.wi,
                                             fpdfFwd, fpdfRev, result.f)) {
          result.fpdf = fpdfFwd;
          result.fpdfRev = fpdfRev;
        }
        return result;
      }};
      auto doSampleBSDF{[&] {
        SampleResult result{};
        float fpdfFwd{};
        float fpdfRev{};
        if (materialInstance.scatterSample(float4(sampler), -vertexPrev.wNext,
                                           result.wi, fpdfFwd, fpdfRev,
                                           result.f, vertex.isDeltaBounce)) {
          result.fpdf = fpdfFwd;
          result.fpdfRev = fpdfRev;
          // Only the PDF is wanted here, to weigh this direction against what
          // light sampling would have produced. The radiance itself is
          // gathered by the caller.
          (void)envLight->Li(compiler, state, result.wi, result.Lpdf);
        }
        return result;
      }};
      auto sampleLight = doSampleLight();
      auto sampleBSDF = doSampleBSDF();
      float weightLight = powerHeuristic(sampleLight.Lpdf, sampleLight.fpdf);
      float weightBSDF = powerHeuristic(sampleBSDF.fpdf, sampleBSDF.Lpdf);
      float chanceLight = powerHeuristic(weightLight, weightBSDF);
      if (float(sampler) < chanceLight) {
        wpdfFwd = chanceLight * sampleLight.Lpdf +
                  (1 - chanceLight) * sampleLight.fpdf;
        wpdfRev = sampleLight.fpdfRev;
        vertex.wNext = sampleLight.wi;
        f = sampleLight.f;
      } else {
        wpdfFwd =
            (1 - chanceLight) * sampleBSDF.fpdf + chanceLight * sampleBSDF.Lpdf;
        wpdfRev = sampleBSDF.fpdfRev;
        vertex.wNext = sampleBSDF.wi;
        f = sampleBSDF.f;
      }
    }
    if (vertex.isDeltaBounce && !dtree) {
      // Dirac lobes report unit PDFs by convention and fold the division into
      // `f`, so neither the mixture PDF above nor any density from the other
      // sampling strategy applies to them. (The guided branch has already
      // accounted for its own selection probability.)
      wpdfFwd = 1;
      wpdfRev = 1;
    }
    vertex.pdfWNext = wpdfFwd;
    beta *= (1.0f / wpdfFwd) * f;
    if (beta.isAnyNonFinite()) {
      break;
    }
    // Terminate by Russian roulette instead of by a fixed depth limit, so that
    // high-albedo transport keeps the energy it is entitled to.
    if (depth > ROULETTE_MIN_DEPTH) {
      float survival{};
      float meanRadiance{};
      if (dtree && guiding->pixelEstimate > 0 &&
          (meanRadiance = dtree->meanRadiance()) > 0) {
        // Adjoint-driven Russian roulette (Vorba & Křivánek, SIGGRAPH
        // 2016; roulette only, no splitting): survive in proportion to the
        // expected pixel contribution of continuing the walk, which is the
        // throughput times the SD-tree's cached mean incident radiance,
        // relative to the pixel's estimate from the previous pass.
        survival = beta.average() * meanRadiance / guiding->pixelEstimate;
        survival = std::min(survival, 1.0f);
        survival = std::max(survival, 0.05f);
      } else {
        survival = std::min(ROULETTE_MAX_SURVIVAL, beta.maxComponent());
      }
      if (survival < 1.0f) {
        if (!(float(sampler) < survival)) {
          break;
        }
        beta *= 1.0f / survival;
      }
    }
    MediumStack::Update(medium, allocator, materialInstance, -vertexPrev.wNext,
                        vertex.wNext);
    vertexPrev.pdfRev = vertex.convert_pdf(wpdfRev, vertexPrev);
    ray = Ray{vertex.point, vertex.wNext, EPS, INF};
  }
  return depth;
}

#if 0
[[nodiscard]]
static float multiple_importance_weight(const Vertex *cameraVertex,
                                        const Vertex *lightVertex) {
  float termSum{0.0f};
  if (cameraVertex) {
    for (float term{1.0f}; cameraVertex->prevVertex;
         cameraVertex = cameraVertex->prevVertex) {
      term *= cameraVertex->pdfAdjoint / cameraVertex->pdf;
      termSum += term;
    }
  }
  if (lightVertex) {
    for (float term{1.0f}; lightVertex->prevVertex;
         lightVertex = lightVertex->prevVertex) {
      term *= lightVertex->pdfAdjoint / lightVertex->pdf;
      termSum += term;
    }
  }
  return 1.0f / (1.0f + termSum);
}

bool connect_bidirectional(const Scene &scene,
                           smdl::BumpPtrAllocator &allocator,
                           const std::function<float()> &rngf,
                           const Color &wavelengthBase, Vertex *cameraVertex,
                           Vertex *lightVertex, Color &beta, float &misWeight,
                           float2 &pixelCoord) {
  if (!cameraVertex) {
    return false;
  }
  if (!lightVertex) {
    return false;
  }
  SMDL_PRESERVE(*cameraVertex, *lightVertex);
  SMDL_SANITY_CHECK(cameraVertex->source == smdl::TRANSPORT_MODE_RADIANCE);
  SMDL_SANITY_CHECK(lightVertex->source == smdl::TRANSPORT_MODE_IMPORTANCE);
  if (!cameraVertex->prevVertex && lightVertex->prevVertex &&
      !lightVertex->isAtInfinity) {
    SMDL_PRESERVE(lightVertex->prevVertex->pdfAdjoint);
    auto result{Camera_last_vertex_sample(scene.camera,
                                          float2(rngf(), rngf()),
                                          *lightVertex, *cameraVertex)};
    beta = cameraVertex->beta;
    misWeight = multiple_importance_weight(cameraVertex, lightVertex);
    pixelCoord = cameraVertex->pixelCoord;
    return result && scene.test_visibility(allocator, rngf, wavelengthBase,
                                           *cameraVertex, *lightVertex, beta);
  }
  if (cameraVertex->prevVertex && !lightVertex->prevVertex &&
      !cameraVertex->isAtInfinity) {
    SMDL_PRESERVE(cameraVertex->prevVertex->pdfAdjoint);
    auto result{Light_last_vertex_sample(scene, rngf(),
                                         float2(rngf(), rngf()),
                                         *cameraVertex, *lightVertex)};
    beta = lightVertex->beta;
    misWeight = multiple_importance_weight(cameraVertex, lightVertex);
    return result && scene.test_visibility(allocator, rngf, wavelengthBase,
                                           *cameraVertex, *lightVertex, beta);
  }
  if (!cameraVertex->prevVertex || cameraVertex->isAtInfinity ||
      !lightVertex->prevVertex || lightVertex->isAtInfinity) {
    return false;
  }
  SMDL_PRESERVE(cameraVertex->prevVertex->pdfAdjoint,
                lightVertex->prevVertex->pdfAdjoint);
  float3 w{smdl::normalize(lightVertex->point - cameraVertex->point)};
  float cameraDirPdf{};
  float cameraDirPdfAdjoint{};
  Color cameraf{};
  if (!cameraVertex->scatter(w, cameraDirPdf, cameraDirPdfAdjoint, cameraf)) {
    return false;
  }
  lightVertex->pdfAdjoint = cameraVertex->convert_direction_pdf_to_point_pdf(
      cameraDirPdf, *lightVertex);
  if (cameraVertex->prevVertex && cameraVertex->prevVertex->prevVertex) {
    cameraVertex->prevVertex->pdfAdjoint =
        cameraVertex->convert_direction_pdf_to_point_pdf(
            cameraDirPdfAdjoint, *cameraVertex->prevVertex);
  }
  float lightDirPdf{};
  float lightDirPdfAdjoint{};
  Color lightf{};
  if (!lightVertex->scatter(-w, lightDirPdf, lightDirPdfAdjoint, lightf)) {
    return false;
  }
  cameraVertex->pdfAdjoint = lightVertex->convert_direction_pdf_to_point_pdf(
      lightDirPdf, *cameraVertex);
  if (lightVertex->prevVertex && lightVertex->prevVertex->prevVertex) {
    lightVertex->prevVertex->pdfAdjoint =
        lightVertex->convert_direction_pdf_to_point_pdf(
            lightDirPdfAdjoint, *lightVertex->prevVertex);
  }
  beta =
      cameraVertex->beta * cameraf * lightf * lightVertex->beta *
      (1.0f / smdl::length_squared(cameraVertex->point - lightVertex->point));
  misWeight = multiple_importance_weight(cameraVertex, lightVertex);
  return scene.test_visibility(allocator, rngf, wavelengthBase, *cameraVertex,
                               *lightVertex, beta);
}
#endif
