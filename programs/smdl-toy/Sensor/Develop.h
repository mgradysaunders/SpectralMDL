/// \file
/// The two develops, each to linear sRGB: the observer's, from the
/// spectral film through the CIE observer, or false color where the grid
/// cannot carry color; and a physical sensor's, from its readout the way
/// a raw developer takes one. What `-output-rgbf` holds, and what the
/// display transform in `Tonemap.h` starts from.
#pragma once

#include <vector>

#include "smdl/RenderUtil/SpectralFilm.h"
#include "smdl/Support/Span.h"

#include "Color.h"
#include "Layout/CameraFile.h"
#include "Sensor/Detector.h"
#include "Sensor/Sensor.h"

namespace smdl {
class Compiler;
}

//--{ The observer's develop

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
//--}

//--{ The physical develop
/// Where the physical develop puts a neutral the meter aimed at: 0.18,
/// middle gray. The meter lands a frame's mean at `q K / 78`, 10.4% of
/// saturation, so the develop's baseline exposure is 0.18 over that,
/// +0.79 EV, close to darktable's +0.7 EV default.
constexpr double DEVELOP_MIDDLE_GRAY{0.18};

/// The linear luminance the physical develop gives a neutral whose
/// focal-plane exposure is `luxSeconds` at ISO `iso`, whatever the body:
/// `0.18 S H / (q K)`, which puts the exposure the meter aims at on
/// middle gray. `developReadout()` exposes to it, and so does the preview
/// of a body under -ideal.
[[nodiscard]] constexpr double developedLuminance(double iso,
                                                  double luxSeconds) noexcept {
  return DEVELOP_MIDDLE_GRAY * iso * luxSeconds / (METER_Q * METER_K);
}

/// How a mosaic becomes one value per band at every pixel.
enum class DemosaicMethod {
  /// No tile: every pixel holds every band already.
  NONE,

  /// Any tile: normalized convolution per band with a separable tent as
  /// wide and tall as the tile, keeping the pixel's own value where the
  /// band sampled it. Exact bilinear for a band sampled on a lattice,
  /// which is every band of a Bayer tile and of a tile with one pixel per
  /// band.
  BILINEAR,

  /// A 2 by 2 Bayer tile of the three bands: green along the direction
  /// with the smaller gradient, corrected by the sampled color's second
  /// difference (Hamilton and Adams 1997, US 5629734, expired), then red
  /// and blue as green plus the bilinear mean of their difference from
  /// it.
  HAMILTON_ADAMS
};

/// The method `response`'s tile interpolates `bands` by:
/// `HAMILTON_ADAMS` when the tile is 2 by 2 and holds exactly the three
/// bands, the second of them twice on a diagonal; `BILINEAR` for any
/// other tile; `NONE` without one.
[[nodiscard]] DemosaicMethod demosaicMethod(const ResponseSettings &response,
                                            smdl::Span<const size_t> bands);

/// Interpolate a mosaic, `mosaic` holding one value per pixel of a
/// `numPixelsX` by `numPixelsY` frame under `response`'s tile, to
/// `bands` at every pixel of `window`: `bands.size()` values per pixel,
/// in the order given, and zero outside the window. Taps outside the
/// window are dropped, since nothing was rendered there, and
/// `HAMILTON_ADAMS` keeps the bilinear values within two pixels of its
/// edge, where its taps would leave it.
[[nodiscard]] std::vector<float>
demosaic(DemosaicMethod method, const ResponseSettings &response,
         smdl::Span<const size_t> bands, smdl::Span<const float> mosaic,
         size_t numPixelsX, size_t numPixelsY, int4 window);

/// Develop a physical sensor's readout to linear sRGB, the way a raw
/// developer takes one:
///
/// 1. the digital numbers over the black level, as fractions of the top
///    code, so that the ISO still decides how bright the picture is when
///    the well clips below the top code;
/// 2. balanced to `whiteBalance`, each band times `n_G(S) / n_b(S)`;
/// 3. every sample held where the least multiplied band reaches the
///    white level, so that a saturated white stays white, as dcraw's
///    `-H 0` holds it;
/// 4. demosaicked, see `demosaicMethod()`;
/// 5. the matrix fitted to the body's curves under the illuminant, see
///    `Sensor::fitColor()`, Bradford to the sRGB white, and the builtin's
///    matrix to linear sRGB;
/// 6. exposed so that a neutral the meter aimed at develops to
///    `DEVELOP_MIDDLE_GRAY`: the baseline, and the ratio that holds it
///    there under the white balance's illuminant whichever band
///    saturates first, the most sensitive band's electrons per
///    lux-second under D55 over green's under the illuminant.
///
/// An `auto` white balance reads the frame's gray world, the mean of
/// each band's unsaturated samples, through the D65 fit; McCamy's cubic
/// gives its temperature, the fit is taken there, and the white is put
/// at the measured neutral itself. Three bands that fit the observer
/// badly develop as false color, each band on its own channel, and fewer
/// than three as gray, both balanced and exposed the same way.
///
/// The pixels outside `window` develop to black. `shouldLog` says what
/// was done; a preview passes false.
[[nodiscard]] std::vector<float>
developReadout(const Sensor &sensor, const Detector &detector,
               const Readout &readout, const WhiteBalance &whiteBalance,
               int4 window, bool shouldLog);
//--}
