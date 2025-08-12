#include "AmbientLight.h"

#include "../Scene.h"

float AmbientLight::power_estimate(const Scene &scene) const {
  return PI * scene.boundRadius * scene.boundRadius * intensity.average_value();
}

bool AmbientLight::first_vertex_sample(const Scene &scene,
                                       const Random &random,
                                       Vertex &lightVertex,
                                       float &dirPdf) const {
  // TODO
  return false;
}

bool AmbientLight::last_vertex_sample(const Scene &scene,
                                      const Random &random,
                                      const Vertex &cameraVertex,
                                      Vertex &lightVertex) const {
  // TODO
  return false;
}
