#include <cmath>
#include <cstdio>
#include <iostream>

#include "smdl/Compiler.h"

#include "Sensor/Colorimetry.h"
#include "Sensor/Develop.h"

namespace {

// The fraction of the photopic luminous mass the wavelength grid can
// see: a fine sweep of the visible, crediting the mass wherever some
// band lies within 30nm. This decides whether the CIE projection of
// the film means anything as a picture.
[[nodiscard]] double visibleCoverage(const Color &wavelengths) {
  double total{};
  double covered{};
  for (double lambda = 380.0; lambda <= 780.0; lambda += 5.0) {
    const double mass{photopicV(lambda)};
    total += mass;
    for (size_t i = 0; i < wavelengths.size(); i++) {
      if (std::abs(double(wavelengths[i]) - lambda) <= 30.0) {
        covered += mass;
        break;
      }
    }
  }
  return covered / total;
}

// The band nearest a wavelength in nm.
[[nodiscard]] size_t nearestBand(const Color &wavelengths, float lambda) {
  size_t best{0};
  for (size_t i = 1; i < wavelengths.size(); i++)
    if (std::abs(wavelengths[i] - lambda) <
        std::abs(wavelengths[best] - lambda))
      best = i;
  return best;
}

} // namespace

std::vector<float> resolveRGB(smdl::Compiler &compiler,
                              const smdl::SpectralFilm &film,
                              const Color &wavelengths,
                              const RGBPolicy &policy) {
  const size_t numPixelsX{film.getNumPixelsX()};
  const size_t numPixelsY{film.getNumPixelsY()};
  const size_t numBands{film.getNumBands()};
  auto rgbImage{std::vector<float>(numPixelsX * numPixelsY * 3)};
  // Down to 90% coverage the CIE projection is a faithful picture, and
  // down to 35% it is still the physically correct band-limited view
  // (tinted or dim) and only earns a note; below that a color has no
  // meaning and three bands map to R, G, B instead. Fewer than three
  // bands cannot even pretend, and write the grayscale mean radiance;
  // this outranks coverage because a lone mid-visible band can cover
  // much of the photopic mass while its "color" is still garbage.
  enum class Mode { TRUE_COLOR, FALSE_COLOR, GRAYSCALE };
  const double coverage{visibleCoverage(wavelengths)};
  auto mode{Mode::TRUE_COLOR};
  if (numBands < 3) {
    mode = Mode::GRAYSCALE;
  } else if (policy.shouldForceFalseColor || coverage < 0.35) {
    mode = Mode::FALSE_COLOR;
  }
  if (mode == Mode::GRAYSCALE) {
    char note[96]{};
    std::snprintf(note, sizeof(note),
                  "spectral to RGB: %zu band(s) cannot carry color, "
                  "writing the grayscale mean radiance\n",
                  numBands);
    std::cerr << note;
    for (size_t p = 0; p < numPixelsX * numPixelsY; p++) {
      const size_t x{p % numPixelsX}, y{p / numPixelsX};
      double mean{};
      for (size_t i = 0; i < numBands; i++) mean += filmMean(film, x, y, i);
      mean /= double(numBands);
      auto texel{&rgbImage[3 * p]};
      texel[0] = texel[1] = texel[2] = float(mean);
    }
    return rgbImage;
  }
  if (mode == Mode::FALSE_COLOR) {
    // Long wavelengths on red, remote-sensing style: by default the
    // bands nearest 5/6, 1/2, and 1/6 of the grid span.
    const float waveMin{wavelengths[0]};
    const float waveMax{wavelengths[wavelengths.size() - 1]};
    size_t bandR{}, bandG{}, bandB{};
    if (policy.falseColorWaves.size() == 3) {
      bandR = nearestBand(wavelengths, policy.falseColorWaves[0]);
      bandG = nearestBand(wavelengths, policy.falseColorWaves[1]);
      bandB = nearestBand(wavelengths, policy.falseColorWaves[2]);
    } else {
      bandR = nearestBand(wavelengths, waveMin + (waveMax - waveMin) * 5 / 6);
      bandG = nearestBand(wavelengths, waveMin + (waveMax - waveMin) / 2);
      bandB = nearestBand(wavelengths, waveMin + (waveMax - waveMin) / 6);
    }
    char note[160]{};
    std::snprintf(note, sizeof(note),
                  "spectral to RGB: false color%s, R=%.0fnm G=%.0fnm "
                  "B=%.0fnm (the grid covers %.0f%% of the visible)\n",
                  policy.shouldForceFalseColor ? " (forced)" : "",
                  double(wavelengths[bandR]), double(wavelengths[bandG]),
                  double(wavelengths[bandB]), 100.0 * coverage);
    std::cerr << note;
    for (size_t p = 0; p < numPixelsX * numPixelsY; p++) {
      const size_t x{p % numPixelsX}, y{p / numPixelsX};
      auto texel{&rgbImage[3 * p]};
      texel[0] = float(filmMean(film, x, y, bandR));
      texel[1] = float(filmMean(film, x, y, bandG));
      texel[2] = float(filmMean(film, x, y, bandB));
    }
    return rgbImage;
  }
  if (coverage < 0.9) {
    char note[160]{};
    std::snprintf(note, sizeof(note),
                  "spectral to RGB: the grid covers %.0f%% of the visible, "
                  "true color will be band-limited (dim or tinted)\n",
                  100.0 * coverage);
    std::cerr << note;
  }
  for (size_t y{}; y < numPixelsY; y++) {
    for (size_t x{}; x < numPixelsX; x++) {
      auto color{Color()};
      for (size_t i = 0; i < color.size(); i++)
        color[i] = float(filmMean(film, x, y, i));
      auto state{makeRenderState(wavelengths)};
      auto rgb{compiler.convertColorToRGB(state, color.data())};
      auto texel{&rgbImage[3 * (x + numPixelsX * y)]};
      texel[0] = rgb[0];
      texel[1] = rgb[1];
      texel[2] = rgb[2];
    }
  }
  return rgbImage;
}
