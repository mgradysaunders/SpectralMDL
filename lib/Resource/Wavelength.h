#pragma once

#include <algorithm>
#include <numeric>
#include <vector>

#include "smdl/Support/Macros.h"

namespace smdl {

/// The wavelength units a spectrum file may declare.
enum WaveUnits : int {
  WAVE_UNITS_ANGSTROMS,
  WAVE_UNITS_WAVENUMBERS,
  WAVE_UNITS_MEGAHERTZ,
  WAVE_UNITS_GIGAHERTZ,
  WAVE_UNITS_MICROMETERS,
  WAVE_UNITS_NANOMETERS,
};

/// Convert a wavelength (or frequency, or wavenumber) in `units` to
/// nanometers.
[[nodiscard]] inline float toNanometers(WaveUnits units, float wave) {
  switch (units) {
  case WAVE_UNITS_ANGSTROMS:
    return 0.1f * wave;
  case WAVE_UNITS_WAVENUMBERS:
    return 10e6 / static_cast<double>(wave);
  case WAVE_UNITS_MEGAHERTZ:
    return 299792458.0e3 / static_cast<double>(wave);
  case WAVE_UNITS_GIGAHERTZ:
    return 299792458.0 / static_cast<double>(wave);
  case WAVE_UNITS_MICROMETERS:
    return 1e3f * wave;
  case WAVE_UNITS_NANOMETERS:
    return wave;
  default:
    break;
  }
  return 0;
}

/// Sort `wavelengths` into increasing order and permute each of the
/// `numCurves` curves stored back to back in `curveValues` alongside.
inline void sortByWavelength(std::vector<float> &wavelengths,
                             std::vector<float> &curveValues,
                             size_t numCurves) {
  const size_t n{wavelengths.size()};
  SMDL_SANITY_CHECK(curveValues.size() == n * numCurves);
  std::vector<size_t> order(n);
  std::iota(order.begin(), order.end(), size_t(0));
  std::sort(order.begin(), order.end(), [&](size_t i, size_t j) {
    return wavelengths[i] < wavelengths[j];
  });
  auto permute{[&](float *values) {
    std::vector<float> tmpValues(values, values + n);
    for (size_t i = 0; i < n; i++) values[i] = tmpValues[order[i]];
  }};
  permute(wavelengths.data());
  for (size_t i = 0; i < numCurves; i++) permute(curveValues.data() + n * i);
}

} // namespace smdl
