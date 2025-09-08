/// \file
#pragma once

#include <string>
#include <vector>

#include "smdl/Support/Error.h"

namespace smdl {

enum WaveUnits {
  WAVE_UNITS_ANGSTROMS,
  WAVE_UNITS_WAVENUMBERS,
  WAVE_UNITS_MICROMETERS,
  WAVE_UNITS_NANOMETERS,
};

class SMDL_EXPORT Spectrum final {
public:
  void clear() noexcept {
    wavelengths.clear();
    values.clear();
  }

  [[nodiscard]] std::optional<Error>
  load_txt(const std::string &fileName,
           WaveUnits units = WAVE_UNITS_MICROMETERS) noexcept;

private:
  /// The wavelengths in nanometers.
  std::vector<float> wavelengths{};

  /// The values.
  std::vector<float> values{};
};

class SMDL_EXPORT SpectrumLibrary final {
public:
  void clear() noexcept {
    wavelengths.clear();
    curveNames.clear();
    curves.clear();
  }

  [[nodiscard]]
  std::optional<Error> load_sli(const std::string &fileName) noexcept;

private:
  std::vector<float> wavelengths{};

  std::vector<std::string> curveNames{};

  std::vector<float> curves{};
};

} // namespace smdl
