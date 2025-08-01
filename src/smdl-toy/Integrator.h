#pragma once

#include "Scene.h"

class Integrator {
public:
  explicit Integrator(unsigned seed, unsigned samplesPerPixel,
                      unsigned minOrder, unsigned maxOrder)
      : seed(seed), samplesPerPixel(samplesPerPixel), minOrder(minOrder),
        maxOrder(maxOrder) {}

  virtual ~Integrator() = default;

  void integrate_and_write_file(const Scene &scene, float imageScale,
                                const std::string &imageFileName) const;

  virtual void integrate(const Scene &scene, const Color &wavelengthBase,
                         smdl::SpectralRenderImage &renderImage) const = 0;

protected:
  const unsigned seed{};

  const unsigned samplesPerPixel{};

  const unsigned minOrder{};

  const unsigned maxOrder{};
};
