#pragma once

#include "Vertex.h"

class Camera final {
public:
  explicit Camera(smdl::float4x4 lookAtMatrix, smdl::int2 extent,
                  float fieldOfViewY)
      : lookAtMatrix(lookAtMatrix), extent(extent),
        aspect(static_cast<float>(extent.x) / static_cast<float>(extent.y)),
        focalLength(0.5f / std::tan(0.5f * fieldOfViewY)) {
    SMDL_SANITY_CHECK(extent.x > 0);
    SMDL_SANITY_CHECK(extent.y > 0);
  }

  Camera(const Camera &) = delete;

  [[nodiscard]] smdl::float3 origin() const noexcept { return lookAtMatrix[3]; }

  [[nodiscard]] smdl::float3 normal() const noexcept { return lookAtMatrix[2]; }

  [[nodiscard]] bool first_vertex_sample(const smdl::float2 &pixelCoord,
                                         Vertex &cameraVertex,
                                         float &dirPdf) const;

  [[nodiscard]] bool last_vertex_sample(const Random &random,
                                        const Vertex &lightVertex,
                                        Vertex &cameraVertex) const;

  [[nodiscard]] float direction_pdf(smdl::float3 w,
                                    smdl::float2 &pixelCoord) const noexcept;

public:
  /// The look-at matrix transforming from camera space to world space.
  const smdl::float4x4 lookAtMatrix;

  /// The extent in pixels.
  const smdl::int2 extent;

  /// The horizontal-to-vertical aspect ratio.
  const float aspect;

  /// The focal length.
  const float focalLength;
};
