/// \file
#pragma once

#include <string>
#include <vector>

#include "smdl/Support/Error.h"
#include "smdl/Support/Span.h"

namespace smdl {

/// \addtogroup resource
/// \{

/// A spectrum view.
class SpectrumView final {
public:
  /// The wavelengths in nanometers.
  Span<const float> wavelengths{};

  /// The values.
  Span<const float> curveValues{};
};

/// A spectrum loaded from a text file.
class SMDL_EXPORT Spectrum final {
public:
  void clear() noexcept {
    mWavelengths.clear();
    mCurveValues.clear();
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
  std::optional<Error> loadFromFile(const std::string &fileName) noexcept;

  [[nodiscard]]
  operator SpectrumView() const noexcept {
    return {mWavelengths, mCurveValues};
  }

private:
  /// The wavelengths in nanometers.
  std::vector<float> mWavelengths;

  /// The values.
  std::vector<float> mCurveValues;
};

/// \}

} // namespace smdl
