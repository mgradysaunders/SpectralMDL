#include "PointLight.h"

#include "../Scene.h"

float PointLight::power_estimate(const Scene &scene) const {
  return 4 * PI * intensity.average_value();
}

bool PointLight::first_vertex_sample(const Scene &scene, const Random &random,
                                     Vertex &lightVertex, float &dirPdf) const {
  lightVertex = Vertex{}; // Reset
  lightVertex.transportMode = smdl::TRANSPORT_MODE_IMPORTANCE;
  lightVertex.light = this;
  lightVertex.point = origin;
  lightVertex.wNext =
      smdl::uniform_sphere_sample(random.generate_canonical2(), &dirPdf);
  lightVertex.pdf = DIRAC_DELTA;
  lightVertex.pdfAdjoint = DIRAC_DELTA;
  lightVertex.beta = intensity / dirPdf;
  return true;
}

bool PointLight::last_vertex_sample(const Scene &scene, const Random &random,
                                    const Vertex &cameraVertex,
                                    Vertex &lightVertex) const {
  if (!cameraVertex.prevVertex) {
    return false;
  }
  auto [w, distance] = direction_and_distance(origin, cameraVertex.point);
  lightVertex = Vertex{}; // Reset
  lightVertex.transportMode = smdl::TRANSPORT_MODE_RADIANCE;
  lightVertex.light = this;
  lightVertex.point = origin;
  lightVertex.wPrev = w;
  Color f{};
  float dirPdfAdjoint{smdl::uniform_sphere_pdf()};
  if (!cameraVertex.reconnect(lightVertex, dirPdfAdjoint, f)) {
    return false;
  }
  lightVertex.pdf = DIRAC_DELTA;
  lightVertex.pdfAdjoint = DIRAC_DELTA;
  lightVertex.beta =
      cameraVertex.beta * f * (1.0f / (distance * distance)) * intensity;
  return true;
}
