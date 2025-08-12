#include "DiskLight.h"

float DiskLight::power_estimate(const Scene &scene) const {
  return PI * radius * radius * PI * intensity.average_value();
}

bool DiskLight::first_vertex_sample(const Scene &scene,
                                    const Random &random,
                                    Vertex &lightVertex, float &dirPdf) const {
  // TODO
  return false;
}

bool DiskLight::last_vertex_sample(const Scene &scene,
                                   const Random &random,
                                   const Vertex &cameraVertex,
                                   Vertex &lightVertex) const {
  // TODO
  return false;
}
