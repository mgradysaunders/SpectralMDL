/// \file
/// The observer's curves the develops and the display transform share.
#pragma once

#include <cmath>

/// The photopic luminous efficiency `V(lambda)` as a Gaussian fit over
/// wavelength in nanometers, unit peak: `1.019 exp(-285.4 (l - 0.5590)^2)`
/// in micrometers, which closely tracks the CIE y-bar observer. The 683
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
