#pragma once

#include "../Light.h"

class DiskLight final : public Light {
public:
  explicit DiskLight(Color intensity, smdl::float3 origin,
                     smdl::float3 direction, float radius)
      : Light(intensity), origin(origin), direction(smdl::normalize(direction)),
        radius(radius) {}

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

  const float radius;
};
