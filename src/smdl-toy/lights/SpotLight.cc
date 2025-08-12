#include "SpotLight.h"

float SpotLight::power_estimate(const Scene &scene) const {
  return 2 * PI * (1 - (cosThetaInner + cosThetaOuter) / 2) *
         intensity.average_value();
}

bool SpotLight::first_vertex_sample(const Scene &scene,
                                    const Random &random,
                                    Vertex &lightVertex, float &dirPdf) const {
  // TODO
  return false;
}

bool SpotLight::last_vertex_sample(const Scene &scene,
                                   const Random &random,
                                   const Vertex &cameraVertex,
                                   Vertex &lightVertex) const {
  // TODO
  return false;
}
