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

  [[nodiscard]] smdl::float3 origin() const { return cameraToWorld[3]; }

  [[nodiscard]] smdl::float3 normal() const { return cameraToWorld[2]; }

  [[nodiscard]] float direction_pdf(smdl::float3 omega,
                                    smdl::float2 &imageCoord) const {
    omega = smdl::transpose(smdl::float3x3(cameraToWorld)) * omega;
    omega = smdl::normalize(omega);
    if (!(omega.z < 0.0f))
      return 0.0f;
    float cosTheta{std::abs(omega.z)};
    float u{+focalLen / cosTheta * omega.x / imageAspect};
    float v{-focalLen / cosTheta * omega.y};
    if (!(std::abs(u) < 0.5f && std::abs(v) < 0.5f))
      return 0.0f;
    imageCoord.x = imageExtent.x * std::max(0.0f, std::min(u + 0.5f, 1.0f));
    imageCoord.y = imageExtent.y * std::max(0.0f, std::min(v + 0.5f, 1.0f));
    return (focalLen * focalLen) /
           (imageAspect * cosTheta * cosTheta * cosTheta);
  }

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
  [[nodiscard]] float falloff(const smdl::float3 &w) const {
    float cosTheta{smdl::dot(w, direction)};
    cosTheta = std::max(cosTheta, cosThetaOuter);
    cosTheta = std::min(cosTheta, cosThetaInner);
    return std::pow(
        (cosTheta - cosThetaOuter) / (cosThetaInner - cosThetaOuter), 4.0f);
  }

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

enum PathOrigin {
  PATH_ORIGIN_UNINITIALIZED = -1,
  PATH_ORIGIN_CAMERA = 0,
  PATH_ORIGIN_LIGHT = 1
};

class Vertex final {
public:
  [[nodiscard]] Color scatter(const smdl::float3 &w) const {
    float dirPdf{};
    float dirPdfAdjoint{};
    Color f{};
    if (scatter(w, dirPdf, dirPdfAdjoint, f))
      return f;
    return Color(0.0f);
  }

  [[nodiscard]] bool scatter(const smdl::float3 &w, float &dirPdf,
                             float &dirPdfAdjoint, Color &f) const {
    SMDL_SANITY_CHECK(material);
    auto tbnInv{smdl::transpose(materialInstance.tangent_space)};
    if (!material->scatter_evaluate(materialInstance, tbnInv * wPrev,
                                    tbnInv * w, dirPdf, dirPdfAdjoint,
                                    f.data()))
      return false;
    if (pathOrigin == PATH_ORIGIN_LIGHT && intersection)
      f *= intersection->shading_normal_correction(wPrev, w);
    return true;
  }

  [[nodiscard]] bool scatter_sample(const smdl::float4 &xi, smdl::float3 &w,
                                    float &dirPdf, float &dirPdfAdjoint,
                                    Color &f, int &isDelta) const {
    auto tbn{materialInstance.tangent_space};
    auto tbnInv{smdl::transpose(tbn)};
    if (material->scatter_sample(materialInstance, xi, tbnInv * wPrev, w,
                                 dirPdf, dirPdfAdjoint, f.data(), isDelta)) {
      w = smdl::normalize(tbn * w);
      if (pathOrigin == PATH_ORIGIN_LIGHT && intersection)
        f *= intersection->shading_normal_correction(wPrev, w);
      return true;
    } else {
      dirPdf = 0;
      dirPdfAdjoint = 0;
      return false;
    }
  }

  [[nodiscard]]
  float convert_direction_pdf_to_point_pdf(float pdf,
                                           const Vertex &nextVertex) const;

  [[nodiscard]]
  bool reconnect(const Vertex &nextVertex, float dirPdfAdjoint,
                 Color &f) const {
    pdfAdjoint =
        nextVertex.convert_direction_pdf_to_point_pdf(dirPdfAdjoint, *this);
    if (float unused{}; scatter(smdl::normalize(nextVertex.point - point),
                                unused, dirPdfAdjoint, f)) {
      if (prevVertex && prevVertex->prevVertex) {
        prevVertex->pdfAdjoint =
            convert_direction_pdf_to_point_pdf(dirPdfAdjoint, *prevVertex);
      }
      return true;
    }
    return false;
  }

public:
  /// If applicable, the previous vertex.
  const Vertex *prevVertex{nullptr};

  /// Is from light?
  PathOrigin pathOrigin{PATH_ORIGIN_UNINITIALIZED};

  /// The accumulated path weight.
  Color beta{QUIET_NAN};

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
  const smdl::JIT::Material *material{};

  /// The material instance.
  smdl::JIT::Material::Instance materialInstance{};

  /// The direction to the previous vertex.
  smdl::float3 wPrev{QUIET_NAN};

  /// The direction to the next vertex.
  smdl::float3 wNext{QUIET_NAN};

  /// The PDF associated with sampling the vertex point.
  float pdf{QUIET_NAN};

  /// The PDF associated with sampling the vertex point by the adjoint
  /// technique.
  mutable float pdfAdjoint{QUIET_NAN};

  /// Is at infinity?
  bool isAtInfinity{};
};

/// Sample first vertex from camera.
[[nodiscard]]
bool Camera_first_vertex_sample(const Camera &camera,
                                const smdl::float2 &imageCoord,
                                Vertex &firstVertex, float &dirPdf);

/// Sample last vertex from camera.
[[nodiscard]]
bool Camera_last_vertex_sample(const Camera &camera, const smdl::float2 &xi,
                               const Vertex &lastLightVertex,
                               Vertex &cameraVertex);

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
                               Vertex &firstVertex, float &dirPdf);

/// Sample last vertex from light.
[[nodiscard]]
bool Light_last_vertex_sample(const Scene &scene, const Light &light,
                              const smdl::float2 &xi,
                              const Vertex &lastCameraVertex,
                              Vertex &lightVertex);

[[nodiscard]]
bool connect_bidirectional(const Scene &scene,
                           smdl::BumpPtrAllocator &allocator,
                           const std::function<float()> &rngf,
                           const Color &wavelengthBase, Vertex *cameraVertex,
                           Vertex *lightVertex, Color &beta, float &misWeight,
                           smdl::float2 &imageCoord);
