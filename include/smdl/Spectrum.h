/// \file
#pragma once

#include <string>
#include <vector>

#include "smdl/Support/Error.h"
#include "smdl/Support/Span.h"

namespace smdl {

/// A light-weight non-owning view of a spectral curve.
class SpectrumView final {
public:
  /// The wavelengths in nanometers.
  Span<const float> wavelengths{};

  /// The values.
  Span<const float> curveValues{};
};

class SMDL_EXPORT Spectrum final {
public:
  void clear() noexcept {
    wavelengths.clear();
    curveValues.clear();
  }

  /// Load from file.
  ///
  /// The expected format is a two-column text file where the first
  /// column gives the wavelength and the second column gives the
  /// curve value. The implementation ignores blank lines and comments
  /// starting with the hash `#` character.
  ///
  /// ~~~~~~~~~~~~~~~~~
  /// # A comment
  /// 0.402 0.121
  /// 0.523 0.783
  /// # Another comment
  /// 0.611 0.694
  /// ~~~~~~~~~~~~~~~~~
  ///
  /// Optionally, the first non-commented line can specify the wavelength
  /// units using one of the following identifiers:
  /// - `angstroms`
  /// - `megahertz`
  /// - `gigahertz`
  /// - `wavenumbers`
  /// - `micrometers`
  /// - `nanometers`
  ///
  /// The implementation is not case sensitive. If no wavelength units
  /// are specified, the default is `micrometers`.
  ///
  [[nodiscard]]
  std::optional<Error> load_from_file(const std::string &fileName) noexcept;

  [[nodiscard]]
  operator SpectrumView() const noexcept {
    return {wavelengths, curveValues};
  }

private:
  /// The wavelengths in nanometers.
  std::vector<float> wavelengths{};

  /// The values.
  std::vector<float> curveValues{};
};

class SMDL_EXPORT SpectrumLibrary final {
public:
  void clear() noexcept {
    wavelengths.clear();
    curveValues.clear();
    curveNames.clear();
  }

  [[nodiscard]]
  std::optional<Error> load_from_file(const std::string &fileName) noexcept;

  [[nodiscard]]
  SpectrumView get_curve_by_name(std::string_view name) const noexcept;

  [[nodiscard]]
  SpectrumView get_curve_by_index(int i) const noexcept {
    if (0 <= i && i < int(numCurves)) {
      return {Span<const float>(wavelengths),
              Span<const float>(curveValues.data() + wavelengths.size() * i,
                                wavelengths.size())};
    }
    return {};
  }

private:
  size_t numCurves{};

  std::vector<float> wavelengths{};

  std::vector<float> curveValues{};

  std::vector<std::string> curveNames{};
};

} // namespace smdl
