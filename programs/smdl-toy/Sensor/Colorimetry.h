/// \file
/// The observer's curves the sensor's physics, the develops, and the
/// display transform share.
#pragma once

#include <cmath>

#include "smdl/Support/VectorMath.h"

/// The CIE 1931 observer as the piecewise Gaussian fits of Wyman, Sloan,
/// and Shirley (2013), for wavelength in nanometers, unscaled: `y` peaks
/// at 1 near 555 nm and integrates to 107 nm. The lobes are the ones the
/// builtin `_wymanXYZ` uses, with the same constants; the builtin scales
/// by 0.01 and takes the exponential through a short series whose tails
/// are fatter than the Gaussian's, so a caller mirroring the JIT's own
/// projection to the bit cannot use this, and one integrating an
/// illuminance can.
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
