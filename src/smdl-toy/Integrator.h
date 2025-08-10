#pragma once

#include "Scene.h"

class Integrator {
public:
  explicit Integrator(size_t seed, size_t samplesPerPixel, size_t minOrder,
                      size_t maxOrder)
      : seed(seed), samplesPerPixel(samplesPerPixel), minOrder(minOrder),
        maxOrder(maxOrder) {}

  virtual ~Integrator() = default;

  virtual void integrate(const Scene &scene, const Color &wavelengthBase,
                         smdl::SpectralRenderImage &renderImage) const = 0;

  void integrate_and_write_file(const Scene &scene, float imageScale,
                                const std::string &imageFileName) const;

protected:
  const size_t seed{};

  const size_t samplesPerPixel{};

  const size_t minOrder{};

  const size_t maxOrder{};
};
