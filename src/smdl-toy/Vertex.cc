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
                               Vertex *secondToLastLightVertex,
                               Vertex &lastLightVertex, Vertex &cameraVertex) {
  if (!secondToLastLightVertex) {
    return false;
  }
  auto [direction, distance] =
      direction_and_distance(camera.origin(), lastLightVertex.point);
  cameraVertex = Vertex{}; // Reset
  cameraVertex.camera = &camera;
  cameraVertex.point = camera.origin();
  cameraVertex.wPrev = direction;
  float cameraDirPdfAdjoint{
      camera.direction_pdf(direction, cameraVertex.imageCoord)};
  if (!(cameraDirPdfAdjoint > 0)) {
    return false;
  }
  cameraVertex.pdf = DIRAC_DELTA;
  cameraVertex.pdfAdjoint = DIRAC_DELTA;

  lastLightVertex.pdfAdjoint = cameraVertex.convert_direction_pdf_to_point_pdf(
      cameraDirPdfAdjoint, lastLightVertex);
  float dirPdf{};
  float dirPdfAdjoint{};
  Color f{};
  if (!lastLightVertex.scatter(-direction, dirPdf, dirPdfAdjoint, f)) {
    return false;
  }
  secondToLastLightVertex->pdfAdjoint =
      lastLightVertex.convert_direction_pdf_to_point_pdf(
          dirPdfAdjoint, *secondToLastLightVertex);

  float cosTheta = smdl::abs_dot(camera.normal(), direction);
  cameraVertex.beta = /*(1.0f / (distance * distance)) */ cameraDirPdfAdjoint * cosTheta;
  cameraVertex.beta *= f;
  return true;
}

float Light_power_estimate(const Scene &scene, const Light &light) {
  struct Visitor final {
    [[nodiscard]] float operator()(const PointLight &light) const {
      return 4 * PI * light.intensity.average();
    }
    [[nodiscard]] float operator()(const DirectionLight &light) const {
      return PI * scene.boundRadius * scene.boundRadius *
             light.intensity.average();
    }
    [[nodiscard]] float operator()(const SpotLight &light) const {
      return 2 * PI * (1 - (light.cosThetaInner + light.cosThetaOuter) / 2) *
             light.intensity.average();
    }
    [[nodiscard]] float operator()(const DiskLight &light) const {
      return PI * light.radius * light.radius * PI * light.intensity.average();
    }
    [[nodiscard]] float operator()(const AmbientLight &light) const {
      return PI * scene.boundRadius * scene.boundRadius *
             light.intensity.average();
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
  firstVertex.light = &light;
  struct Visitor final {
    [[nodiscard]] bool operator()(const PointLight &light) const {
      firstVertex.point = light.origin;
      firstVertex.wNext = uniform_sphere_sample(xi0, &dirPdf);
      firstVertex.pdf = DIRAC_DELTA;
      firstVertex.beta = light.intensity;
      return true;
    }
    [[nodiscard]] bool operator()(const DirectionLight &light) const {
      firstVertex.wNext = light.direction;
      firstVertex.point = scene.infinite_disk_emission_point_sample(
          firstVertex.wNext, xi0, &firstVertex.pdf);
      firstVertex.beta = light.intensity;
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
      firstVertex.beta = light.intensity * light.falloff(firstVertex.wNext);
      return true;
    }
    [[nodiscard]] bool operator()(const DiskLight &light) const {
      auto lightToWorld{smdl::coordinate_system(light.direction)};
      firstVertex.point = light.radius * smdl::float3(uniform_disk_sample(xi0));
      firstVertex.point = light.origin + lightToWorld * firstVertex.point;
      firstVertex.wNext = lightToWorld * cosine_hemisphere_sample(xi1, &dirPdf);
      firstVertex.pdf = uniform_disk_pdf(light.radius);
      firstVertex.beta = light.intensity;
      return true;
    }
    [[nodiscard]] bool operator()(const AmbientLight &light) const {
      firstVertex.wNext = uniform_sphere_sample(xi0, &dirPdf);
      firstVertex.point = scene.infinite_disk_emission_point_sample(
          firstVertex.wNext, xi1, &firstVertex.pdf);
      firstVertex.beta = light.intensity;
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

bool Light_last_vertex_sample(const Scene &scene, const Light &light,
                              const smdl::float2 &xi0, const smdl::float2 &xi1,
                              Vertex *secondToLastCameraVertex,
                              Vertex &lastCameraVertex, Vertex &lightVertex) {
  lightVertex = Vertex{};
  lightVertex.light = &light;
  struct Visitor final {
    [[nodiscard]] bool operator()(const PointLight &light) const {
      if (!secondToLastCameraVertex) {
        return false;
      }
      auto [direction, distance] =
          direction_and_distance(light.origin, lastCameraVertex.point);
      lightVertex.point = light.origin;
      lightVertex.wPrev = direction;
      lightVertex.pdf = DIRAC_DELTA;
      lightVertex.pdfAdjoint = DIRAC_DELTA;

      lastCameraVertex.pdfAdjoint =
          lightVertex.convert_direction_pdf_to_point_pdf(uniform_sphere_pdf(),
                                                         lastCameraVertex);
      float dirPdf{};
      float dirPdfAdjoint{};
      Color f{};
      if (!lastCameraVertex.scatter(-direction, dirPdf, dirPdfAdjoint, f)) {
        return false;
      }
      secondToLastCameraVertex->pdfAdjoint =
          lastCameraVertex.convert_direction_pdf_to_point_pdf(
              dirPdfAdjoint, *secondToLastCameraVertex);

      lightVertex.beta = (1.0f / (distance * distance)) * light.intensity;
      lightVertex.beta *= f;
      return true;
    }
    [[nodiscard]] bool operator()(const DirectionLight &light) const {
#if 0
      lastVertex.point = vertex.point - 2 * scene.boundRadius * light.direction;
      lastVertex.wNext = light.direction;
      lastVertex.pdf = DIRAC_DELTA;
      lastVertex.pdfAdjoint = uniform_disk_pdf(scene.boundRadius);
      lastVertex.isAtInfinity = true;
      lastVertex.beta = light.intensity;
#endif
      return false;
    }
    [[nodiscard]] bool operator()(const SpotLight &light) const {
      auto [direction, distance] =
          direction_and_distance(light.origin, lastCameraVertex.point);
      float falloff{light.falloff(direction)};
      if (!(falloff > 0.0f))
        return false;
#if 0
      lastVertex.point = light.origin;
      lastVertex.wNext = direction;
      lastVertex.pdf = DIRAC_DELTA;
      lastVertex.pdfAdjoint = DIRAC_DELTA;
      lastVertex.beta = (falloff / (distance * distance)) * light.intensity;
#endif
      return false;
    }
    [[nodiscard]] bool operator()(const DiskLight &light) const {
      // TODO
      return false;
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
    const smdl::float2 &xi0;
    const smdl::float2 &xi1;
    Vertex *secondToLastCameraVertex{};
    Vertex &lastCameraVertex;
    Vertex &lightVertex;
  };
  return std::visit(Visitor{scene, xi0, xi1, secondToLastCameraVertex,
                            lastCameraVertex, lightVertex},
                    light);
}
