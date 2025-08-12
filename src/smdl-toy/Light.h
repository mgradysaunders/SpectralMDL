#pragma once

#include "Vertex.h"

class Light {
public:
  explicit Light(Color intensity) : intensity(intensity) {}

  virtual ~Light() = default;

  Light(const Light &) = delete;

  [[nodiscard]] virtual float power_estimate(const Scene &scene) const = 0;

  [[nodiscard]] virtual bool first_vertex_sample(const Scene &scene,
                                                 const Random &random,
                                                 Vertex &lightVertex,
                                                 float &dirPdf) const = 0;

  [[nodiscard]] virtual bool last_vertex_sample(const Scene &scene,
                                                const Random &random,
                                                const Vertex &cameraVertex,
                                                Vertex &lightVertex) const = 0;

protected:
  const Color intensity;

  float probability{QUIET_NAN};

  friend class LightDistribution;
};

class LightDistribution final {
public:
  LightDistribution() = default;

  template <typename T, typename... Args> void emplace(Args &&...args) {
    static_assert(std::is_base_of_v<Light, T>);
    SMDL_SANITY_CHECK(!isFinalized);
    lights.emplace_back(new T(std::forward<Args>(args)...));
  }

  void finalize(const Scene &scene);

  [[nodiscard]]
  const Light *light_sample(const Random &random) const;

private:
  /// The lights.
  std::vector<std::unique_ptr<Light>> lights{};

  /// The light distribution.
  smdl::DiscreteDistribution lightDistribution{};

  /// Is finalized?
  bool isFinalized{};
};

#if 0
  [[nodiscard]]
  smdl::float3 infinite_disk_emission_point_sample(smdl::float3 omega,
                                                   smdl::float2 xi,
                                                   float *pdf) const {
    if (pdf)
      *pdf = smdl::uniform_disk_pdf(boundRadius);
    auto pointOnDisk{uniform_disk_sample(xi)};
    auto point{smdl::float3(pointOnDisk.x, pointOnDisk.y, -1)};
    return boundCenter + boundRadius * (smdl::coordinate_system(omega) * point);
  }
#endif
