#include "vertex.h"
#include "light.h"

bool test_visibility(const Scene &scene, const AnyRandom &random,
                     const Color &wavelengths,
                     smdl::BumpPtrAllocator &allocator, //
                     const MediumStack *medium, const float3 &point0,
                     const float3 &point1, Color &beta) {
  float d{length(point1 - point0)};
  Ray ray{point0, point1 - point0, EPS, 1.0f - EPS};
  while (ray.tmin < ray.tmax) {
    Hit hit{};
    if (!scene.intersect(ray, hit)) {
      break;
    }
    if (medium && medium->materialInstance.has_medium()) {
      Color muA = Color(medium->materialInstance.absorption_coefficient());
      Color muS = Color(medium->materialInstance.scattering_coefficient());
      Color mu = muA + muS;
      Color Tr{};
      for (size_t i = 0; i < WAVELENGTH_BASE_MAX; i++)
        Tr[i] = std::exp(-mu[i] * (ray.tmax - ray.tmin) * d);
      beta *= Tr;
    }
    smdl::State state{};
    state.allocator = &allocator;
    state.wavelength_base = wavelengths.data();
    state.wavelength_min = WAVELENGTH_MIN;
    state.wavelength_max = WAVELENGTH_MAX;
    hit.apply_geometry_to_state(state);
    smdl::JIT::MaterialInstance materialInstance{state, hit.material};
    if (float opacity{materialInstance.cutout_opacity()};
        opacity == 1 || float(random) < opacity) {
      return false; // Blocks visibility!
    }
    MediumStack::Update(medium, allocator, materialInstance, -ray.dir, ray.dir);
    ray.tmin = smdl::incrementFloat(ray.tmax + EPS);
    ray.tmax = 1.0f - EPS;
  }
  return true;
}

uint64_t random_walk(smdl::Compiler &compiler, const Scene &scene,
                     const AnyRandom &random, const Color &wavelengths,
                     smdl::BumpPtrAllocator &allocator,
                     smdl::Transport transport, Vertex path0, float wpdfFwd,
                     uint64_t maxDepth, Vertex *path,
                     const EnvLight &envLight) {
  if (maxDepth == 0)
    return 0;
  path[0] = std::move(path0);
  if (maxDepth == 1)
    return 1;

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
  Ray ray{path[0].point, path[0].wNext, EPS, INF};
  uint64_t depth{1};
  while (depth < maxDepth) {
    auto &vertexPrev{path[depth - 1]};
    auto &vertex{path[depth]};

    auto hit{Hit{}};
    bool hitSurface{scene.intersect(ray, hit)};
    if (medium && medium->materialInstance.has_medium()) {
      Color muA = Color(medium->materialInstance.absorption_coefficient());
      Color muS = Color(medium->materialInstance.scattering_coefficient());
      Color mu = muA + muS;
      float t =
          -std::log1p(-float(random)) / mu[random.index(WAVELENGTH_BASE_MAX)];
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
        vertex.wNext = smdl::uniformSphereSample(float2(random));
        vertex.isVolume = true;
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
    if (float opacity{materialInstance.cutout_opacity()};
        opacity < 1 && (opacity == 0 || float(random) > opacity)) {
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
    vertex.pdfFwd = vertexPrev.convert_pdf(wpdfFwd, vertex);

    if (depth > 2) {
      float wpdfRev{};
      if (!materialInstance.scatter_sample(float4(random), -vertexPrev.wNext,
                                           vertex.wNext, wpdfFwd, wpdfRev, f,
                                           vertex.isDeltaBounce)) {
        break;
      }
    } else {
      struct SampleResult final {
        float3 wi{};
        float Lpdf{};
        float fpdf{};
        Color f{};
      };
      auto doSampleLight{[&] {
        SampleResult result{};
        Color Li{};
        result.wi = envLight.Li_sample(compiler, state, float2(random),
                                       result.Lpdf, Li);
        float fpdfFwd{};
        float fpdfRev{};
        if (materialInstance.scatter_evaluate(-vertexPrev.wNext, result.wi,
                                              fpdfFwd, fpdfRev, result.f)) {
          result.fpdf = fpdfFwd;
        }
        return result;
      }};
      auto doSampleBSDF{[&] {
        SampleResult result{};
        float fpdfFwd{};
        float fpdfRev{};
        if (materialInstance.scatter_sample(float4(random), -vertexPrev.wNext,
                                            result.wi, fpdfFwd, fpdfRev,
                                            result.f, vertex.isDeltaBounce)) {
          result.fpdf = fpdfFwd;
          Color Li = envLight.Li(compiler, state, result.wi, result.Lpdf);
          return result;
        }
        return result;
      }};
      auto sampleLight = doSampleLight();
      auto sampleBSDF = doSampleBSDF();
      auto balance{[](float a, float b) {
        return 1.0f / (1.0f + std::pow(b / a, 2.0f));
      }};
      float weightLight = balance(sampleLight.Lpdf, sampleLight.fpdf);
      float weightBSDF = balance(sampleBSDF.fpdf, sampleBSDF.Lpdf);
      if (depth > 2)
        weightBSDF = 1, weightLight = 0;
      float chanceLight = balance(weightLight, weightBSDF);
      if (float(random) < chanceLight) {
        wpdfFwd = chanceLight * sampleLight.Lpdf +
                  (1 - chanceLight) * sampleLight.fpdf;
        vertex.wNext = sampleLight.wi;
        f = sampleLight.f;
      } else {
        wpdfFwd =
            (1 - chanceLight) * sampleBSDF.fpdf + chanceLight * sampleBSDF.Lpdf;
        vertex.wNext = sampleBSDF.wi;
        f = sampleBSDF.f;
      }
    }
    if (vertex.isDeltaBounce) {
      wpdfFwd = 0;
      // wpdfRev = 0;
    }
    beta *= (1.0f / wpdfFwd) * f;
    if (beta.is_any_non_finite()) {
      break;
    }
    MediumStack::Update(medium, allocator, materialInstance, -vertexPrev.wNext,
                        vertex.wNext);
    // vertexPrev.pdfRev = vertex.convert_pdf(wpdfRev, vertexPrev);
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
