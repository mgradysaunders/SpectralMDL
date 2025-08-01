#include "Vertex.h"
#include "Scene.h"

float Vertex::convert_direction_pdf_to_point_pdf(
    float pdf, const Vertex &nextVertex) const {
  if (!nextVertex.isAtInfinity) {
    auto sep{nextVertex.point - point};
    auto invDistance2{smdl::finite_or_zero(1 / smdl::length_squared(sep))};
    pdf *= invDistance2;
    if (nextVertex.intersection)
      pdf *= smdl::abs_dot(nextVertex.intersection->geometryNormal,
                           sep * std::sqrt(invDistance2));
  }
  return pdf;
}

bool Camera_first_vertex_sample(const Camera &camera,
                                const smdl::float2 &imageCoord,
                                Vertex &firstVertex, float &dirPdf) {
  auto w{smdl::normalize(smdl::float3(
      +(imageCoord.x / float(camera.imageExtent.x) - 0.5f) * camera.imageAspect,
      -(imageCoord.y / float(camera.imageExtent.y) - 0.5f), -camera.focalLen))};
  auto cosTheta{std::abs(w.z)};
  firstVertex = Vertex{}; // Reset
  firstVertex.pathOrigin = PATH_ORIGIN_CAMERA;
  firstVertex.camera = &camera;
  firstVertex.imageCoord = imageCoord;
  firstVertex.point = camera.origin();
  firstVertex.wNext =
      smdl::normalize(camera.cameraToWorld * smdl::float4(w, 0.0f));
  firstVertex.pdf = DIRAC_DELTA;
  firstVertex.beta = Color(1.0f);
  dirPdf = (camera.focalLen * camera.focalLen) /
           (camera.imageAspect * cosTheta * cosTheta * cosTheta);
  return true;
}

bool Camera_last_vertex_sample(const Camera &camera, const smdl::float2 &xi,
                               const Vertex &lastLightVertex,
                               Vertex &cameraVertex) {
  SMDL_SANITY_CHECK(lastLightVertex.pathOrigin == PATH_ORIGIN_LIGHT);
  if (!lastLightVertex.prevVertex) {
    return false;
  }
  auto [direction, distance] =
      direction_and_distance(camera.origin(), lastLightVertex.point);
  cameraVertex = Vertex{}; // Reset
  cameraVertex.pathOrigin = PATH_ORIGIN_LIGHT;
  cameraVertex.camera = &camera;
  cameraVertex.point = camera.origin();
  cameraVertex.wPrev = direction;
  cameraVertex.pdf = cameraVertex.pdfAdjoint = DIRAC_DELTA;
  float cameraDirPdfAdjoint{
      camera.direction_pdf(direction, cameraVertex.imageCoord)};
  if (!(cameraDirPdfAdjoint > 0)) {
    return false;
  }
  Color f{};
  if (!lastLightVertex.reconnect(cameraVertex, cameraDirPdfAdjoint, f)) {
    return false;
  }
  cameraVertex.beta =
      lastLightVertex.beta * f * (cameraDirPdfAdjoint / (distance * distance));
  return true;
}

float Light_power_estimate(const Scene &scene, const Light &light) {
  struct Visitor final {
    [[nodiscard]] float operator()(const PointLight &light) const {
      return 4 * PI * light.intensity.average_value();
    }
    [[nodiscard]] float operator()(const DirectionLight &light) const {
      return PI * scene.boundRadius * scene.boundRadius *
             light.intensity.average_value();
    }
    [[nodiscard]] float operator()(const SpotLight &light) const {
      return 2 * PI * (1 - (light.cosThetaInner + light.cosThetaOuter) / 2) *
             light.intensity.average_value();
    }
    [[nodiscard]] float operator()(const DiskLight &light) const {
      return PI * light.radius * light.radius * PI *
             light.intensity.average_value();
    }
    [[nodiscard]] float operator()(const AmbientLight &light) const {
      return PI * scene.boundRadius * scene.boundRadius *
             light.intensity.average_value();
    }
    [[nodiscard]] float operator()(const EnvironmentLight &light) const {
      return PI * scene.boundRadius * scene.boundRadius * light.intensityScale;
    }
    const Scene &scene;
  };
  return std::visit(Visitor{scene}, light);
}

bool Light_first_vertex_sample(const Scene &scene, const Light &light,
                               const smdl::float2 &xi0, const smdl::float2 &xi1,
                               Vertex &firstVertex, float &dirPdf) {
  firstVertex = Vertex{}; // Reset
  firstVertex.pathOrigin = PATH_ORIGIN_LIGHT;
  firstVertex.light = &light;
  struct Visitor final {
    [[nodiscard]] bool operator()(const PointLight &light) const {
      firstVertex.point = light.origin;
      firstVertex.wNext = uniform_sphere_sample(xi0, &dirPdf);
      firstVertex.pdf = DIRAC_DELTA;
      firstVertex.beta = light.intensity / dirPdf;
      return true;
    }
    [[nodiscard]] bool operator()(const DirectionLight &light) const {
      firstVertex.point = scene.infinite_disk_emission_point_sample(
          light.direction, xi0, &firstVertex.pdf);
      firstVertex.wNext = light.direction;
      firstVertex.beta = light.intensity / firstVertex.pdf;
      firstVertex.isAtInfinity = true;
      dirPdf = DIRAC_DELTA;
      return true;
    }
    [[nodiscard]] bool operator()(const SpotLight &light) const {
      firstVertex.point = light.origin;
      firstVertex.pdf = DIRAC_DELTA;
      firstVertex.wNext =
          smdl::coordinate_system(light.direction) *
          uniform_cone_sample(xi0, light.cosThetaOuter, &dirPdf);
      firstVertex.beta =
          (light.falloff(firstVertex.wNext) / dirPdf) * light.intensity;
      return true;
    }
    [[nodiscard]] bool operator()(const DiskLight &light) const {
      auto lightToWorld{smdl::coordinate_system(light.direction)};
      firstVertex.point =
          light.origin +
          lightToWorld * smdl::float3(light.radius * uniform_disk_sample(xi0));
      firstVertex.wNext = smdl::normalize(
          lightToWorld * cosine_hemisphere_sample(xi1, &dirPdf));
      firstVertex.pdf = uniform_disk_pdf(light.radius);
      firstVertex.beta = (smdl::dot(firstVertex.wNext, light.direction) /
                          (firstVertex.pdf * dirPdf)) *
                         light.intensity;
      return true;
    }
    [[nodiscard]] bool operator()(const AmbientLight &light) const {
      firstVertex.wNext = uniform_sphere_sample(xi0, &dirPdf);
      firstVertex.point = scene.infinite_disk_emission_point_sample(
          firstVertex.wNext, xi1, &firstVertex.pdf);
      firstVertex.beta = (1.0f / (firstVertex.pdf * dirPdf)) * light.intensity;
      firstVertex.isAtInfinity = true;
      return true;
    }
    [[nodiscard]] bool operator()(const EnvironmentLight &light) const {
      // TODO
      return false;
    }
    const Scene &scene;
    const smdl::float2 &xi0;
    const smdl::float2 &xi1;
    Vertex &firstVertex;
    float &dirPdf;
  };
  return std::visit(Visitor{scene, xi0, xi1, firstVertex, dirPdf}, light);
}

bool Light_last_vertex_sample(const Scene &scene, /* const Light &light, */
                              float xi0, const smdl::float2 &xi1,
                              const Vertex &lastCameraVertex,
                              Vertex &lightVertex) {
  SMDL_SANITY_CHECK(lastCameraVertex.pathOrigin == PATH_ORIGIN_CAMERA);
  const Light &light = scene.light_sample(xi0);
  lightVertex = Vertex{};
  lightVertex.pathOrigin = PATH_ORIGIN_CAMERA;
  lightVertex.light = &light;
  struct Visitor final {
    [[nodiscard]] bool operator()(const PointLight &light) const {
      if (!lastCameraVertex.prevVertex) {
        return false;
      }
      auto [direction, distance] =
          direction_and_distance(light.origin, lastCameraVertex.point);
      lightVertex.point = light.origin;
      lightVertex.wPrev = direction;
      lightVertex.pdf = lightVertex.pdfAdjoint = DIRAC_DELTA;

      Color f{};
      float lightDirPdfAdjoint{uniform_sphere_pdf()};
      if (!lastCameraVertex.reconnect(lightVertex, lightDirPdfAdjoint, f)) {
        return false;
      }
      lightVertex.beta = lastCameraVertex.beta * f *
                         (1.0f / (distance * distance)) * light.intensity;
      return true;
    }
    [[nodiscard]] bool operator()(const DirectionLight &light) const {
      if (!lastCameraVertex.prevVertex) {
        return false;
      }
      lightVertex.point =
          lastCameraVertex.point - 2 * scene.boundRadius * light.direction;
      lightVertex.wPrev = light.direction;
      lightVertex.pdf = DIRAC_DELTA;
      lightVertex.pdfAdjoint = uniform_disk_pdf(scene.boundRadius);
      lightVertex.isAtInfinity = true;

      Color f{};
      float lightDirPdfAdjoint{DIRAC_DELTA}; // TODO Is this right?
      if (!lastCameraVertex.reconnect(lightVertex, lightDirPdfAdjoint, f)) {
        return false;
      }
      lightVertex.beta = lastCameraVertex.beta * f * light.intensity;
      return true;
    }
    [[nodiscard]] bool operator()(const SpotLight &light) const {
      if (!lastCameraVertex.prevVertex) {
        return false;
      }
      auto [direction, distance] =
          direction_and_distance(light.origin, lastCameraVertex.point);
      float falloff{light.falloff(direction)};
      if (!(falloff > 0.0f))
        return false;
      lightVertex.point = light.origin;
      lightVertex.wPrev = direction;
      lightVertex.pdf = lightVertex.pdfAdjoint = DIRAC_DELTA;

      Color f{};
      float lightDirPdfAdjoint{uniform_cone_pdf(light.cosThetaOuter)};
      if (!lastCameraVertex.reconnect(lightVertex, lightDirPdfAdjoint, f)) {
        return false;
      }
      lightVertex.beta = lastCameraVertex.beta * f *
                         (falloff / (distance * distance)) * light.intensity;
      return true;
    }
    [[nodiscard]] bool operator()(const DiskLight &light) const {
      if (!lastCameraVertex.prevVertex) {
        return false;
      }
      auto lightToWorld{smdl::coordinate_system(light.direction)};
      lightVertex.point =
          light.origin +
          lightToWorld * smdl::float3(light.radius * uniform_disk_sample(xi));
      lightVertex.wPrev =
          smdl::normalize(lastCameraVertex.point - lightVertex.point);
      lightVertex.pdf = lightVertex.pdfAdjoint = uniform_disk_pdf(light.radius);

      Color f{};
      float distance{smdl::length(lightVertex.point - lastCameraVertex.point)};
      float cosTheta{smdl::dot(lightVertex.wPrev, light.direction)};
      if (!(cosTheta > 0))
        return false;
      float lightDirPdfAdjoint{cosTheta / PI};
      if (!lastCameraVertex.reconnect(lightVertex, lightDirPdfAdjoint, f)) {
        return false;
      }
      lightVertex.beta = lastCameraVertex.beta * f *
                         (cosTheta / (distance * distance * lightVertex.pdf)) *
                         light.intensity;
      return true;
    }
    [[nodiscard]] bool operator()(const AmbientLight &light) const {
      // TODO
      return false;
    }
    [[nodiscard]] bool operator()(const EnvironmentLight &light) const {
      // TODO
      return false;
    }
    const Scene &scene;
    const smdl::float2 &xi;
    const Vertex &lastCameraVertex;
    Vertex &lightVertex;
  };
  bool result{
      std::visit(Visitor{scene, xi1, lastCameraVertex, lightVertex}, light)};
  if (result) {
    float invProbability = 1.0f / scene.light_probability(light);
    lightVertex.pdf *= invProbability;
    lightVertex.pdfAdjoint *= invProbability;
    lightVertex.beta *= invProbability;
  }
  return result;
}

[[nodiscard]]
static float multiple_importance_weight(const Vertex *cameraVertex,
                                        const Vertex *lightVertex) {
#if 0
  int cameraPathLen{};
  for (; cameraVertex; cameraVertex = cameraVertex->prevVertex)
    cameraPathLen++;
  int lightPathLen{};
  for (; lightVertex; lightVertex = lightVertex->prevVertex)
    lightPathLen++;
  if (cameraPathLen <= 1 && lightPathLen <= 1)
    return 1;
  return 1.0f / (cameraPathLen + lightPathLen - 1);
#else
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
#endif
}

bool connect_bidirectional(const Scene &scene,
                           smdl::BumpPtrAllocator &allocator,
                           const std::function<float()> &rngf,
                           const Color &wavelengthBase, Vertex *cameraVertex,
                           Vertex *lightVertex, Color &beta, float &misWeight,
                           smdl::float2 &imageCoord) {
  if (!cameraVertex) {
    return false;
  }
  if (!lightVertex) {
    return false;
  }
  SMDL_PRESERVE(*cameraVertex, *lightVertex);
  SMDL_SANITY_CHECK(cameraVertex->pathOrigin == PATH_ORIGIN_CAMERA);
  SMDL_SANITY_CHECK(lightVertex->pathOrigin == PATH_ORIGIN_LIGHT);
  if (!cameraVertex->prevVertex && lightVertex->prevVertex &&
      !lightVertex->isAtInfinity) {
    SMDL_PRESERVE(lightVertex->prevVertex->pdfAdjoint);
    auto result{Camera_last_vertex_sample(scene.camera,
                                          smdl::float2(rngf(), rngf()),
                                          *lightVertex, *cameraVertex)};
    beta = cameraVertex->beta;
    misWeight = multiple_importance_weight(cameraVertex, lightVertex);
    imageCoord = cameraVertex->imageCoord;
    return result && scene.test_visibility(allocator, rngf, wavelengthBase,
                                           *cameraVertex, *lightVertex, beta);
  }
  if (cameraVertex->prevVertex && !lightVertex->prevVertex &&
      !cameraVertex->isAtInfinity) {
    SMDL_PRESERVE(cameraVertex->prevVertex->pdfAdjoint);
    auto result{Light_last_vertex_sample(scene, rngf(),
                                         smdl::float2(rngf(), rngf()),
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
  smdl::float3 w{smdl::normalize(lightVertex->point - cameraVertex->point)};
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
