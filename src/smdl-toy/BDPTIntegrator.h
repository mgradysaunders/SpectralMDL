#pragma once

#include "Integrator.h"

class BDPTIntegrator final : public Integrator {
public:
  explicit BDPTIntegrator(unsigned seed, unsigned samplesPerPixel,
                          unsigned minOrder, unsigned maxOrder)
      : Integrator(seed, samplesPerPixel, minOrder, maxOrder) {}

  void integrate(const Scene &scene, const Color &wavelengthBase,
                 smdl::SpectralRenderImage &renderImage) const final;
};
