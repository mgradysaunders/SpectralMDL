#pragma once

#include "../Light.h"

class PointLight final : public Light {
public:
  explicit PointLight(Color intensity, smdl::float3 origin)
      : Light(intensity), origin(origin) {}

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
};
