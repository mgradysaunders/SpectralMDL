#pragma once

#include "../Integrator.h"

class PTLightIntegrator final : public Integrator {
public:
  explicit PTLightIntegrator(size_t seed, size_t samplesPerPixel,
                             size_t minOrder, size_t maxOrder)
      : Integrator(seed, samplesPerPixel, minOrder, maxOrder) {}

  void integrate(const Scene &scene, const Color &wavelengthBase,
                 smdl::SpectralRenderImage &renderImage) const final;
};
