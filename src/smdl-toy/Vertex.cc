#include "Vertex.h"
#include "Scene.h"

bool Vertex::scatter_sample(const smdl::float4 &xi, const smdl::float3 &omegaO,
                            smdl::float3 &omegaI, float &pdfOmegaI,
                            float &pdfOmegaO, Color &f,
                            bool &isDeltaOmegaI) const {
  const auto &tangentSpace{jitMaterialInstance.tangent_space};
  auto isDelta{int(0)};
  auto success{jitMaterial->scatter_sample(
      jitMaterialInstance, xi, smdl::transpose(tangentSpace) * omegaO, omegaI,
      pdfOmegaI, pdfOmegaO, f.data(), isDelta)};
  if (success) {
    omegaI = tangentSpace * omegaI;
    omegaI = smdl::normalize(omegaI);
  }
  isDeltaOmegaI = (isDelta != 0);
  return success;
}

float Vertex::convert_solid_angle_to_point_density(
    float pdf, const Vertex &nextVertex) const {
  if (!nextVertex.isAtInfinity) {
    auto sep{nextVertex.point - point};
    auto invDistanceSquared{
        smdl::finite_or_zero(1.0f / smdl::length_squared(sep))};
    pdf *= invDistanceSquared;
    if (nextVertex.intersection)
      pdf *= smdl::abs_dot(nextVertex.intersection->geometryNormal,
                           sep * std::sqrt(invDistanceSquared));
  }
  return pdf;
}

bool Camera_first_vertex_sample(const Camera &camera,
                                const smdl::float2 &imageCoord,
                                Vertex &firstVertex) {
  firstVertex = Vertex{}; // Reset
  firstVertex.camera = &camera;
  firstVertex.imageCoord = imageCoord;
  firstVertex.point = camera.cameraToWorld[3];
  firstVertex.pdfPoint = 1, firstVertex.isDeltaPdfPoint = true;
  auto omega{smdl::normalize(smdl::float3(
      +(imageCoord.x / float(camera.imageExtent.x) - 0.5f) * camera.imageAspect,
      -(imageCoord.y / float(camera.imageExtent.y) - 0.5f), -camera.focalLen))};
  auto cosTheta{std::abs(omega.z)};
  firstVertex.omegaNext = camera.cameraToWorld * smdl::float4(omega, 0.0f);
  firstVertex.omegaNext = smdl::normalize(firstVertex.omegaNext);
  firstVertex.pdfOmega = (camera.focalLen * camera.focalLen) /
                         (camera.imageAspect * cosTheta * cosTheta * cosTheta);
  firstVertex.weight = Color(1.0f);
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
                               Vertex &firstVertex) {
  firstVertex = Vertex{}; // Reset
  firstVertex.light = &light;
  struct Visitor final {
    [[nodiscard]] bool operator()(const PointLight &light) const {
      firstVertex.point = light.origin, firstVertex.pdfPoint = 1,
      firstVertex.isDeltaPdfPoint = true;
      firstVertex.omegaNext = uniform_sphere_sample(xi0, &firstVertex.pdfOmega);
      firstVertex.weight = light.intensity;
      return true;
    }
    [[nodiscard]] bool operator()(const DirectionLight &light) const {
      firstVertex.omegaNext = light.direction, firstVertex.pdfOmega = 1,
      firstVertex.isDeltaPdfOmega = true;
      firstVertex.point = scene.infinite_disk_emission_point_sample(
          firstVertex.omegaNext, xi0, &firstVertex.pdfPoint);
      firstVertex.weight = light.intensity;
      firstVertex.isAtInfinity = true;
      return true;
    }
    [[nodiscard]] bool operator()(const SpotLight &light) const {
      firstVertex.point = light.origin, firstVertex.pdfPoint = 1,
      firstVertex.isDeltaPdfPoint = true;
#if 0
    Vector3f w = UniformSampleCone(u1, cosTotalWidth);
    *ray = Ray(pLight, LightToWorld(w), Infinity, time, mediumInterface.inside);
    *pdfDir = UniformConePdf(cosTotalWidth);
    return I * Falloff(ray->d);
#endif
#if 0
Float SpotLight::Falloff(const Vector3f &w) const {
    Vector3f wl = Normalize(WorldToLight(w));
    Float cosTheta = wl.z;
    if (cosTheta < cosTotalWidth) return 0;
    if (cosTheta >= cosFalloffStart) return 1;
    // Compute falloff inside spotlight cone
    Float delta =
        (cosTheta - cosTotalWidth) / (cosFalloffStart - cosTotalWidth);
    return (delta * delta) * (delta * delta);
}
#endif
      return false;
    }
    [[nodiscard]] bool operator()(const DiskLight &light) const {
      auto lightToWorld{smdl::coordinate_system(light.direction)};
      firstVertex.point = light.radius * smdl::float3(uniform_disk_sample(xi0));
      firstVertex.point = light.origin + lightToWorld * firstVertex.point;
      firstVertex.pdfPoint = 1.0f / (PI * light.radius * light.radius);
      firstVertex.omegaNext =
          lightToWorld * cosine_hemisphere_sample(xi1, &firstVertex.pdfOmega);
      firstVertex.weight = light.intensity;
      return true;
    }
    [[nodiscard]] bool operator()(const AmbientLight &light) const {
      firstVertex.omegaNext = uniform_sphere_sample(xi0, &firstVertex.pdfOmega);
      firstVertex.point = scene.infinite_disk_emission_point_sample(
          firstVertex.omegaNext, xi1, &firstVertex.pdfPoint);
      firstVertex.weight = light.intensity;
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
  };
  return std::visit(Visitor{scene, xi0, xi1, firstVertex}, light);
}

bool Light_last_vertex_sample(const Scene &scene, const Light &light,
                              const smdl::float2 &xi0, const smdl::float2 &xi1,
                              const Vertex &vertex, Vertex &lastVertex) {
  lastVertex = Vertex{};
  lastVertex.light = &light;
  struct Visitor final {
    [[nodiscard]] bool operator()(const PointLight &light) const {
      // TODO
      return false;
    }
    [[nodiscard]] bool operator()(const DirectionLight &light) const {
      // TODO
      return false;
    }
    [[nodiscard]] bool operator()(const SpotLight &light) const {
      // TODO
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
    const Vertex &vertex;
    Vertex &lastVertex;
  };
  return std::visit(Visitor{scene, xi0, xi1, vertex, lastVertex}, light);
}
