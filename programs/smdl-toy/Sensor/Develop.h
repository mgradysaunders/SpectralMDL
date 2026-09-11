/// \file
/// The observer's develop: the spectral film to linear sRGB through the
/// CIE observer, or false color where the grid cannot carry color. What
/// `-output-rgbf` holds, and what the display transform in `Tonemap.h`
/// starts from.
#pragma once

#include <cmath>
#include <vector>

#include "smdl/RenderUtil/SpectralFilm.h"

#include "Color.h"

namespace smdl {
class Compiler;
}

/// How `resolveRGB()` maps the film to RGB when the CIE projection is
/// not simply the right thing.
struct RGBPolicy final {
  /// Force the false-color band mapping even when the grid could carry
  /// true color.
  bool shouldForceFalseColor{};

  /// With false color, the wavelengths in nm mapped to R, G, and B, or
  /// empty to pick the bands at 5/6, 1/2, and 1/6 of the grid span,
  /// long wavelengths on red.
  std::vector<float> falseColorWaves{};
};

/// The film mean of one band, with a non-finite value (a pixel some
/// material poisoned) read as black, so that everything downstream can
/// assume finite input and use plain min and max.
[[nodiscard]] inline double filmMean(const smdl::SpectralFilm &film, size_t x,
                                     size_t y, size_t i) noexcept {
  const double value{film.mean(x, y, i)};
  return std::isfinite(value) ? value : 0.0;
}

/// Resolve the film to linear RGB once. This is the radiance the
/// renderer actually estimated, so it is what gets written to the
/// floating point file; everything in `tonemap()` is a display transform
/// applied only on the way to an 8-bit file.
///
/// How the spectrum becomes RGB depends on how much of the visible the
/// wavelength grid can see. A grid that covers the photopic luminous
/// mass projects through the CIE observer as always; a grid that covers
/// part of it still does, with a note that the picture is band-limited;
/// a grid that misses the visible (or `policy.shouldForceFalseColor`) maps
/// three bands to R, G, and B instead, and fewer than three bands write
/// the grayscale mean radiance. Every non-true-color choice is
/// announced on stderr; the spectral ENVI output is always the
/// radiometric record.
[[nodiscard]] std::vector<float> resolveRGB(smdl::Compiler &compiler,
                                            const smdl::SpectralFilm &film,
                                            const Color &wavelengths,
                                            const RGBPolicy &policy = {});
