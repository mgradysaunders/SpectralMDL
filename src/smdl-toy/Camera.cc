#include "Camera.h"

bool Camera::first_vertex_sample(const smdl::float2 &pixelCoord,
                                 Vertex &cameraVertex, float &dirPdf) const {
  auto wLocal{smdl::normalize(smdl::float3{
      +(pixelCoord.x / static_cast<float>(extent.x) - 0.5f) * aspect,
      -(pixelCoord.y / static_cast<float>(extent.y) - 0.5f), -focalLength})};
  auto cosTheta{std::abs(wLocal.z)};
  cameraVertex = Vertex{}; // Reset
  cameraVertex.transportMode = smdl::TRANSPORT_MODE_RADIANCE;
  cameraVertex.camera = this;
  cameraVertex.pixelCoord = pixelCoord;
  cameraVertex.point = origin();
  cameraVertex.wNext = smdl::normalize(smdl::float3x3(lookAtMatrix) * wLocal);
  cameraVertex.pdf = DIRAC_DELTA;
  cameraVertex.beta = Color(1.0f);
  dirPdf =
      (focalLength * focalLength) / (aspect * cosTheta * cosTheta * cosTheta);
  return true;
}

bool Camera::last_vertex_sample(const Random &, const Vertex &lightVertex,
                                Vertex &cameraVertex) const {
  SMDL_SANITY_CHECK(lightVertex.transportMode ==
                    smdl::TRANSPORT_MODE_IMPORTANCE);
  if (!lightVertex.prevVertex) {
    return false;
  }
  auto [w, distance] = direction_and_distance(origin(), lightVertex.point);
  cameraVertex = Vertex{}; // Reset
  cameraVertex.transportMode = smdl::TRANSPORT_MODE_IMPORTANCE;
  cameraVertex.camera = this;
  cameraVertex.point = origin();
  cameraVertex.wPrev = w;
  cameraVertex.pdf = DIRAC_DELTA;
  cameraVertex.pdfAdjoint = DIRAC_DELTA;
  float dirPdfAdjoint{direction_pdf(w, cameraVertex.pixelCoord)};
  if (!(dirPdfAdjoint > 0)) {
    return false;
  }
  Color f{};
  if (!lightVertex.reconnect(cameraVertex, dirPdfAdjoint, f)) {
    return false;
  }
  cameraVertex.beta =
      lightVertex.beta * f * (dirPdfAdjoint / (distance * distance));
  return true;
}

float Camera::direction_pdf(smdl::float3 w,
                            smdl::float2 &pixelCoord) const noexcept {
  w = smdl::transpose(smdl::float3x3(lookAtMatrix)) * w;
  w = smdl::normalize(w);
  if (!(w.z < 0.0f))
    return 0.0f;
  float cosTheta{std::abs(w.z)};
  float u{+focalLength / cosTheta * w.x / aspect};
  float v{-focalLength / cosTheta * w.y};
  if (!(std::abs(u) < 0.5f && std::abs(v) < 0.5f))
    return 0.0f;
  pixelCoord.x = extent.x * std::max(0.0f, std::min(u + 0.5f, 1.0f));
  pixelCoord.y = extent.y * std::max(0.0f, std::min(v + 0.5f, 1.0f));
  return (focalLength * focalLength) /
         (aspect * cosTheta * cosTheta * cosTheta);
}
