#pragma once

#include "../Light.h"

class DirectionLight final : public Light {
public:
  explicit DirectionLight(Color intensity, smdl::float3 dir)
      : Light(intensity), dir(dir) {}

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
  const smdl::float3 dir{};
};
