/// \file
/// The observer's curves and the colorimetry the sensor's physics, the
/// develops, and the display transform share.
#pragma once

#include <cmath>

#include "smdl/Support/VectorMath.h"

/// The CIE 1931 observer as the piecewise Gaussian fits of Wyman, Sloan,
/// and Shirley (2013), for wavelength in nanometers, unscaled: `y` peaks
/// at 1 near 555 nm and integrates to 107 nm. The lobes are the ones the
/// builtin `_wymanXYZ` uses, with the same constants; the builtin scales
/// by 0.01 and takes the exponential through a short series whose tails
/// are fatter than the Gaussian's, which `builtinWymanXYZ()` mirrors. A
/// photometric integral, which stands in for the CIE table, takes this
/// one.
[[nodiscard]] inline smdl::double3 wymanXYZ(double lambda) noexcept {
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

/// The same fit as the builtin `_wymanXYZ` evaluates it, unscaled: each
/// lobe's exponential through the four-term series the builtin uses. It
/// is what the observer's develop projects through, so the physical
/// develop's color targets take it too, and the two develops share an
/// observer. Over the training reflectances under D65 it differs from
/// `wymanXYZ()` by half a CIEDE2000 on average, enough to show between
/// a preview and a develop that took different ones.
[[nodiscard]] smdl::double3 builtinWymanXYZ(double lambda) noexcept;

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

/// The photopic luminous efficiency `V(lambda)` as a single Gaussian fit
/// over wavelength in nanometers, unit peak: `1.019 exp(-285.4 (l -
/// 0.5590)^2)` in micrometers, which the night tonemap and the visible
/// coverage share use. The 683 lm/W a luminance weighs it by is the
/// caller's.
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

/// The builtin's matrix from XYZ to linear sRGB, as `_colorToRgb`
/// applies it, so that both develops end in the same space.
[[nodiscard]] smdl::double3x3 xyzToLinearSRGB() noexcept;

/// The XYZ that matrix takes to (1, 1, 1), its D65 white, which the
/// physical develop adapts every white to: a balanced white develops
/// to exactly neutral, where the builtin's observer puts D65 0.4% off.
[[nodiscard]] smdl::double3 linearSRGBWhite() noexcept;

/// The linear Bradford adaptation from the white `from` to the white
/// `to`, both XYZ: Lam's cone responses scaled channel by channel, and
/// the identity when the two whites agree.
[[nodiscard]] smdl::double3x3
bradfordAdaptation(const smdl::double3 &from, const smdl::double3 &to) noexcept;

/// McCamy's cubic (1992): the correlated color temperature in kelvin of
/// the chromaticity `xy`, within a few kelvin of the Planckian locus's
/// from 2856 to 6504 K and close enough across daylight for a white
/// balance.
[[nodiscard]] double mccamyKelvin(const smdl::double2 &xy) noexcept;

/// CIELAB of `xyz` about the white `white`.
[[nodiscard]] smdl::double3 xyzToLab(const smdl::double3 &xyz,
                                     const smdl::double3 &white) noexcept;

/// The CIE 1976 color difference, the distance in CIELAB.
[[nodiscard]] double deltaEab(const smdl::double3 &lab0,
                              const smdl::double3 &lab1) noexcept;

/// The CIEDE2000 color difference at unit weights, as Sharma, Wu, and
/// Dalal (2005) state it, whose published test pairs it reproduces.
[[nodiscard]] double deltaE00(const smdl::double3 &lab0,
                              const smdl::double3 &lab1) noexcept;

/// Invert `m` in place, unless its determinant is within 1e-12 of zero
/// against the cube of its largest entry, where it is left alone.
[[nodiscard]] bool tryInvert(smdl::double3x3 &m) noexcept;
