/// \file
/// The CIE observer's curves, and the colorimetry a host's develop needs
/// beside the JIT's own color to RGB: the sRGB matrix and its white,
/// Bradford adaptation, McCamy's temperature, CIELAB, and the color
/// differences.
#pragma once

#include <cmath>

#include "smdl/Export.h"
#include "smdl/Support/VectorMath.h"

namespace smdl {

/// \addtogroup renderutil
/// \{

/// \name Functions (colorimetry)
/// \{

/// The CIE 1931 observer as the piecewise Gaussian fits of Wyman, Sloan,
/// and Shirley (2013), for wavelength in nanometers, unscaled: `y` peaks
/// at 1 near 555 nm and integrates to 107 nm. The JIT's color to RGB
/// evaluates the same fit scaled by 0.01 (the builtin `_wymanXYZ`), so a
/// host that integrates spectra with this one sees the observer the JIT
/// does.
[[nodiscard]] inline double3 wymanXYZ(double lambda) noexcept {
  // One lobe: a Gaussian with one width below its center and another
  // above, the widths as their reciprocals.
  const auto lobe{
      [lambda](double center, double belowInverse, double aboveInverse) {
        const double x{(lambda - center) *
                       (lambda < center ? belowInverse : aboveInverse)};
        return std::exp(-0.5 * x * x);
      }};
  return {0.362 * lobe(442.0, 0.0624, 0.0374) +
              1.056 * lobe(599.8, 0.0264, 0.0323) -
              0.065 * lobe(501.1, 0.0490, 0.0382),
          0.821 * lobe(568.8, 0.0213, 0.0247) +
              0.286 * lobe(530.9, 0.0613, 0.0322),
          1.217 * lobe(437.0, 0.0845, 0.0278) +
              0.681 * lobe(459.0, 0.0385, 0.0725)};
}

/// The photopic luminous efficiency `V(lambda)` of the same fit, the `y`
/// of `wymanXYZ()` alone, which is what a luminance and an illuminance
/// weigh a spectrum by. The 683 lm/W is the caller's.
[[nodiscard]] inline double wymanY(double lambda) noexcept {
  const auto lobe{
      [lambda](double center, double belowInverse, double aboveInverse) {
        const double x{(lambda - center) *
                       (lambda < center ? belowInverse : aboveInverse)};
        return std::exp(-0.5 * x * x);
      }};
  return 0.821 * lobe(568.8, 0.0213, 0.0247) +
         0.286 * lobe(530.9, 0.0613, 0.0322);
}

/// The photopic luminous efficiency `V(lambda)` as a single Gaussian over
/// wavelength in nanometers: `1.019 exp(-285.4 (l - 0.5590)^2)` with `l`
/// in micrometers, within 8% of `wymanY()` from 500 to 620 nm. The 683
/// lm/W a luminance weighs it by is the caller's.
[[nodiscard]] inline double photopicV(double lambda) noexcept {
  const double um{lambda * 1.0e-3};
  return 1.019 * std::exp(-285.4 * (um - 0.5590) * (um - 0.5590));
}

/// The scotopic luminous efficiency `V'(lambda)`, likewise:
/// `0.992 exp(-321.9 (l - 0.5030)^2)`. The 1700 lm/W is the caller's.
[[nodiscard]] inline double scotopicV(double lambda) noexcept {
  const double um{lambda * 1.0e-3};
  return 0.992 * std::exp(-321.9 * (um - 0.5030) * (um - 0.5030));
}

/// The matrix from XYZ to linear sRGB that the JIT's color to RGB applies
/// (the builtin `_colorToRgb`), so that a host's own develop ends in the
/// same space.
[[nodiscard]] SMDL_EXPORT double3x3 xyzToLinearSRGB() noexcept;

/// The XYZ that matrix takes to (1, 1, 1), its D65 white. A develop that
/// adapts every white to it develops a balanced white to exactly neutral,
/// where the observer's own D65 would land 0.3% off.
[[nodiscard]] SMDL_EXPORT double3 linearSRGBWhite() noexcept;

/// The linear Bradford adaptation from the white `from` to the white
/// `to`, both XYZ: Lam's cone responses scaled channel by channel, and
/// the identity when the two whites agree.
[[nodiscard]] SMDL_EXPORT double3x3
bradfordAdaptation(const double3 &from, const double3 &to) noexcept;

/// McCamy's cubic (1992): the correlated color temperature in kelvin of
/// the chromaticity `xy`, within a few kelvin of the Planckian locus's
/// from 2856 to 6504 K and close enough across daylight for a white
/// balance.
[[nodiscard]] SMDL_EXPORT double mccamyKelvin(const double2 &xy) noexcept;

/// CIELAB of `xyz` about the white `white`.
[[nodiscard]] SMDL_EXPORT double3 xyzToLab(const double3 &xyz,
                                           const double3 &white) noexcept;

/// The CIE 1976 color difference, the distance in CIELAB.
[[nodiscard]] SMDL_EXPORT double deltaEab(const double3 &lab0,
                                          const double3 &lab1) noexcept;

/// The CIEDE2000 color difference at unit weights, as Sharma, Wu, and
/// Dalal (2005) state it, whose published test pairs it reproduces.
[[nodiscard]] SMDL_EXPORT double deltaE00(const double3 &lab0,
                                          const double3 &lab1) noexcept;

/// \}

/// \}

} // namespace smdl
