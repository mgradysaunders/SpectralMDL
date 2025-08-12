#include "DirectionLight.h"

#include "../Scene.h"

float DirectionLight::power_estimate(const Scene &scene) const {
  return PI * scene.boundRadius * scene.boundRadius * intensity.average_value();
}

bool DirectionLight::first_vertex_sample(const Scene &scene,
                                         const Random &random,
                                         Vertex &lightVertex,
                                         float &dirPdf) const {
  // TODO
  return false;
}

bool DirectionLight::last_vertex_sample(const Scene &scene,
                                        const Random &random,
                                        const Vertex &cameraVertex,
                                        Vertex &lightVertex) const {
  // TODO
  return false;
}
