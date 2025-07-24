// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#pragma once

#include "common.h"

class Scene;

class Camera final {
public:
  explicit Camera(smdl::float4x4 cameraToWorld, smdl::int2 imageExtent,
                  float fovY)
      : cameraToWorld(cameraToWorld), imageExtent(imageExtent),
        imageAspect(float(imageExtent.x) / float(imageExtent.y)),
        focalLen(0.5f / std::tan(0.5f * fovY)) {}

public:
  /// The camera-to-world matrix.
  smdl::float4x4 cameraToWorld{1.0f};

  /// The image extent in pixels.
  smdl::int2 imageExtent{1920, 1080};

  /// The image aspect ratio.
  float imageAspect{16.0f / 9.0f};

  /// The focal length.
  float focalLen{1.0f};
};

/// A point light.
class PointLight final {
public:
  /// The intensity.
  Color intensity{1.0f};

  /// The origin.
  smdl::float3 origin{};
};

/// A direction light.
class DirectionLight final {
public:
  /// The intensity.
  Color intensity{1.0f};

  /// The direction.
  smdl::float3 direction{0, 0, -1};
};

/// A spot light.
class SpotLight final {
public:
  /// The intensity.
  Color intensity{1.0f};

  /// The origin.
  smdl::float3 origin{0, 0, 5};

  /// The direction.
  smdl::float3 direction{0, 0, -1};

  /// The cosine of the inner cone angle.
  float cosThetaInner{0.866f};

  /// The cosine of the outer cone angle.
  float cosThetaOuter{0.707f};
};

/// A disk light.
class DiskLight final {
public:
  /// The intensity.
  Color intensity{1.0f};

  /// The origin.
  smdl::float3 origin{0, 0, 5};

  /// The direction.
  smdl::float3 direction{0, 0, -1};

  /// The radius.
  float radius{1.0f};
};

/// An ambient light.
class AmbientLight final {
public:
  /// The intensity.
  Color intensity{1.0f};
};

/// An environment light.
class EnvironmentLight final {
public:
  /// The intensity scale.
  float intensityScale{1.0f};

  /// The intensity image.
  std::shared_ptr<smdl::Image> intensityImage{};
};

/// A light.
using Light = std::variant<PointLight, DirectionLight, SpotLight, DiskLight,
                           AmbientLight, EnvironmentLight>;

class Vertex final {
public:
  [[nodiscard]]
  bool scatter_sample(const smdl::float4 &xi, const smdl::float3 &omegaO,
                      smdl::float3 &omegaI, float &pdfOmegaI, float &pdfOmegaO,
                      Color &f, bool &isDeltaOmegaI) const;

  [[nodiscard]]
  float convert_solid_angle_to_point_density(float pdf,
                                             const Vertex &nextVertex) const;

public:
  /// The accumulated path weight.
  Color weight{QUIET_NAN};

  /// The point.
  smdl::float3 point{QUIET_NAN};

  /// If applicable, the associated image coordinate.
  smdl::float2 imageCoord{QUIET_NAN};

  /// If applicable, the camera. (Non-null if terminal vertex on camera)
  const Camera *camera{};

  /// If applicable, the light. (Non-null if terminal vertex on light)
  const Light *light{};

  /// If applicable, the intersection.
  std::optional<Intersection> intersection{};

  /// The material.
  const smdl::JIT::Material *jitMaterial{};

  /// The material instance.
  smdl::JIT::Material::Instance jitMaterialInstance{};

  /// The direction to the previous vertex.
  smdl::float3 omegaPrev{QUIET_NAN};

  /// The direction to the next vertex.
  smdl::float3 omegaNext{QUIET_NAN};

  /// Is PDF associated with sampling `point` a Dirac delta distribution?
  bool isDeltaPdfPoint{};

  /// Is PDF associated with sampling `omegaNext` a Dirac delta distribution?
  bool isDeltaPdfOmega{};

  /// The PDF associated with sampling `point`.
  float pdfPoint{QUIET_NAN};

  /// The PDF associated with sampling `omegaNext`.
  float pdfOmega{QUIET_NAN};

  /// The PDF associated with sampling `point` by the adjoint technique.
  float adjointPdfPoint{QUIET_NAN};

  /// The PDF associated with sampling `omegaPrev` by the adjoint technique.
  float adjointPdfOmega{QUIET_NAN};

  /// Is at infinity?
  bool isAtInfinity{};
};

/// Emission sample.
[[nodiscard]]
bool Camera_first_vertex_sample(const Camera &camera,
                                const smdl::float2 &imageCoord,
                                Vertex &firstVertex);

/// Calculate power estimate.
[[nodiscard]] float Light_power_estimate(const Scene &scene,
                                         const Light &light);

/// Is delta point?
[[nodiscard]]
inline bool Light_is_delta_point(const Light &light) {
  return std::holds_alternative<PointLight>(light) ||
         std::holds_alternative<SpotLight>(light);
}

/// Is delta direction?
[[nodiscard]]
inline bool Light_is_delta_direction(const Light &light) {
  return std::holds_alternative<DirectionLight>(light);
}

/// Sample first vertex from light.
[[nodiscard]]
bool Light_first_vertex_sample(const Scene &scene, const Light &light,
                               const smdl::float2 &xi0, const smdl::float2 &xi1,
                               Vertex &firstVertex);

/// Sample last vertex from light.
[[nodiscard]]
bool Light_last_vertex_sample(const Scene &scene, const Light &light,
                              const smdl::float2 &xi0, const smdl::float2 &xi1,
                              const Vertex &vertex, Vertex &lastVertex);
