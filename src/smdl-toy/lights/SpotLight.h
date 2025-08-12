#pragma once

#include "../Light.h"

class SpotLight final : public Light {
public:
  explicit SpotLight(Color intensity, smdl::float3 origin,
                     smdl::float3 direction, float thetaInner, float thetaOuter)
      : Light(intensity), origin(origin), direction(smdl::normalize(direction)),
        cosThetaInner(std::cos(thetaInner)),
        cosThetaOuter(std::cos(thetaOuter)) {
    SMDL_SANITY_CHECK(thetaInner <= thetaOuter);
  }

  [[nodiscard]] float power_estimate(const Scene &scene) const final;

  [[nodiscard]] bool first_vertex_sample(const Scene &scene,
                                         const Random &random,
                                         Vertex &lightVertex,
                                         float &dirPdf) const final;

  [[nodiscard]] bool last_vertex_sample(const Scene &scene,
                                        const Random &random,
                                        const Vertex &cameraVertex,
                                        Vertex &lightVertex) const final;

private:
  const smdl::float3 origin;

  const smdl::float3 direction;

  const float cosThetaInner;

  const float cosThetaOuter;

  [[nodiscard]] float falloff(const smdl::float3 &w) const {
    float cosTheta{smdl::dot(direction, w)};
    cosTheta = std::max(cosTheta, cosThetaOuter);
    cosTheta = std::min(cosTheta, cosThetaInner);
    float result{(cosTheta - cosThetaOuter) / (cosThetaInner - cosThetaOuter)};
    result *= result;
    result *= result; // 4th power
    return result;
  }
};
