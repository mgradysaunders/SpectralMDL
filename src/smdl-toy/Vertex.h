// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#pragma once

#include "common.h"

class Camera;
class Light;
class Scene;

class Vertex final {
public:
  [[nodiscard]] Color scatter(const smdl::float3 &w) const {
    float dirPdf{};
    float dirPdfAdjoint{};
    Color f{};
    if (scatter(w, dirPdf, dirPdfAdjoint, f)) {
      return f;
    }
    return 0.0f;
  }

  [[nodiscard]]
  bool scatter(const smdl::float3 &w, float &dirPdf, float &dirPdfAdjoint,
               Color &f) const;

  [[nodiscard]]
  bool scatter_sample(const smdl::float4 &xi, smdl::float3 &w, float &dirPdf,
                      float &dirPdfAdjoint, Color &f, int &isDelta) const;

  [[nodiscard]]
  float convert_direction_pdf_to_point_pdf(float pdf,
                                           const Vertex &nextVertex) const;

  [[nodiscard]]
  bool reconnect(const Vertex &nextVertex, float dirPdfAdjoint, Color &f) const;

public:
  /// If applicable, the previous vertex.
  const Vertex *prevVertex{nullptr};

  /// The transport mode.
  smdl::TransportMode transportMode{smdl::TRANSPORT_MODE_RADIANCE};

  /// The accumulated path weight.
  Color beta{QUIET_NAN};

  /// The point.
  smdl::float3 point{QUIET_NAN};

  /// If applicable, the associated pixel coordinate.
  smdl::float2 pixelCoord{QUIET_NAN};

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

#if 0
[[nodiscard]]
bool connect_bidirectional(const Scene &scene,
                           smdl::BumpPtrAllocator &allocator,
                           const std::function<float()> &rngf,
                           const Color &wavelengthBase, Vertex *cameraVertex,
                           Vertex *lightVertex, Color &beta, float &misWeight,
                           smdl::float2 &pixelCoord);
#endif
