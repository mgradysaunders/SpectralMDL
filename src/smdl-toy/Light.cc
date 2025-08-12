#include "Light.h"
#include "Vertex.h"

void LightDistribution::finalize(const Scene &scene) {
  SMDL_SANITY_CHECK(!isFinalized);
  isFinalized = true;
  auto weights{std::vector<double>{}};
  for (size_t i = 0; i < lights.size(); i++)
    weights.emplace_back(lights[i]->power_estimate(scene));
  lightDistribution = smdl::DiscreteDistribution(weights);
  for (size_t i = 0; i < lights.size(); i++) {
    lights[i]->probability = lightDistribution.index_pmf(i);
  }
}

const Light *LightDistribution::light_sample(const Random &random) const {
  SMDL_SANITY_CHECK(isFinalized);
  if (lights.empty()) {
    return nullptr;
  } else if (lights.size() == 1) {
    return lights[0].get();
  } else {
    auto u{random.generate_canonical()};
    return lights[lightDistribution.index_sample(u).first].get();
  }
}

#if 0
bool Light_first_vertex_sample(const Scene &scene,
                               const std::function<float()> &rngf,
                               Vertex &firstVertex, float &dirPdf) {
  struct Visitor final {
    [[nodiscard]] bool operator()(const DirectionLight &light) const {
      firstVertex.point = scene.infinite_disk_emission_point_sample(
          light.direction, smdl::float2(rngf(), rngf()), &firstVertex.pdf);
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
          smdl::uniform_cone_sample(smdl::float2(rngf(), rngf()),
                                    light.cosThetaOuter, &dirPdf);
      firstVertex.beta =
          (light.falloff(firstVertex.wNext) / dirPdf) * light.intensity;
      return true;
    }
    [[nodiscard]] bool operator()(const DiskLight &light) const {
      auto lightToWorld{smdl::coordinate_system(light.direction)};
      firstVertex.point =
          light.origin +
          lightToWorld *
              smdl::float3(light.radius * smdl::uniform_disk_sample(
                                              smdl::float2(rngf(), rngf())));
      firstVertex.wNext = smdl::normalize(
          lightToWorld * smdl::cosine_hemisphere_sample(
                             smdl::float2(rngf(), rngf()), &dirPdf));
      firstVertex.pdf = smdl::uniform_disk_pdf(light.radius);
      firstVertex.beta = (smdl::dot(firstVertex.wNext, light.direction) /
                          (firstVertex.pdf * dirPdf)) *
                         light.intensity;
      return true;
    }
    [[nodiscard]] bool operator()(const AmbientLight &light) const {
      firstVertex.wNext =
          smdl::uniform_sphere_sample(smdl::float2(rngf(), rngf()), &dirPdf);
      firstVertex.point = scene.infinite_disk_emission_point_sample(
          firstVertex.wNext, smdl::float2(rngf(), rngf()), &firstVertex.pdf);
      firstVertex.beta = (1.0f / (firstVertex.pdf * dirPdf)) * light.intensity;
      firstVertex.isAtInfinity = true;
      return true;
    }
    [[nodiscard]] bool operator()(const EnvironmentLight &light) const {
      // TODO
      return false;
    }
    const Scene &scene;
    const std::function<float()> &rngf;
    Vertex &firstVertex;
    float &dirPdf;
  };
  bool result{std::visit(Visitor{scene, rngf, firstVertex, dirPdf}, light)};
  if (result) {
    float invProbability = 1.0f / scene.light_probability(light);
    firstVertex.pdf *= invProbability;
    firstVertex.pdfAdjoint *= invProbability;
    firstVertex.beta *= invProbability;
    dirPdf *= invProbability;
  }
  return result;
}

bool Light_last_vertex_sample(const Scene &scene, /* const Light &light, */
                              float xi0, const smdl::float2 &xi1,
                              const Vertex &lastCameraVertex,
                              Vertex &lightVertex) {
  SMDL_SANITY_CHECK(lastCameraVertex.source == smdl::TRANSPORT_MODE_RADIANCE);
  const Light &light = scene.light_sample(xi0);
  lightVertex = Vertex{};
  lightVertex.source = smdl::TRANSPORT_MODE_RADIANCE;
  lightVertex.light = &light;
  struct Visitor final {
    [[nodiscard]] bool operator()(const PointLight &light) const {

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
      lightVertex.pdfAdjoint = smdl::uniform_disk_pdf(scene.boundRadius);
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
      float lightDirPdfAdjoint{smdl::uniform_cone_pdf(light.cosThetaOuter)};
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
          lightToWorld *
              smdl::float3(light.radius * smdl::uniform_disk_sample(xi));
      lightVertex.wPrev =
          smdl::normalize(lastCameraVertex.point - lightVertex.point);
      lightVertex.pdf = lightVertex.pdfAdjoint =
          smdl::uniform_disk_pdf(light.radius);

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
#endif
