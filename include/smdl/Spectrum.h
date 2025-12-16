/// \file
#pragma once

#include <string>
#include <vector>

#include "smdl/Support/Error.h"
#include "smdl/Support/Span.h"

namespace smdl {

/// \addtogroup Main
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

/// A spectrum library loaded from an ENVI Spectral Library file.
class SMDL_EXPORT SpectrumLibrary final {
public:
  void clear() noexcept {
    mWavelengths.clear();
    mCurveValues.clear();
    mCurveNames.clear();
  }

  /// Load from file.
  ///
  /// The expected format is an ENVI Spectral Library, which is an uncompressed
  /// binary file with an associated plain-text header file. The implementation
  /// expects the filename of the binary file and infers the filename of the
  /// header by appending `.hdr`. For example, `myLibrary.sli` is associated
  /// with the header `myLibrary.sli.hdr`.
  ///
  /// The relevant fields in the header file are:
  /// - `file type`: this must be `ENVI Spectral Library`
  /// - `data type`: this must be `4` (float) or `5` (double)
  /// - `samples`: this is the number of wavelength samples
  /// - `lines`: this is the number of spectral curves
  /// - `bands`: this must be `1`
  /// - `wavelength`: this is the wavelength samples
  /// - `wavelength units`: this must be `Micrometers`, `Nanometers`,
  ///   `Wavenumber`, `MHz`, or `GHz`
  /// - `spectra names`: this is optional, if present contiains the names
  ///    for each spectral curve
  ///
  [[nodiscard]]
  std::optional<Error> loadFromFile(const std::string &fileName) noexcept;

  /// Get curve by name, or return empty view on failure.
  [[nodiscard]]
  SpectrumView getCurveByName(std::string_view name) const noexcept;

  /// Get curve by index, or return empty view on failure.
  [[nodiscard]]
  SpectrumView getCurveByIndex(int i) const noexcept {
    if (0 <= i && i < int(mNumCurves)) {
      return {Span<const float>(mWavelengths),
              Span<const float>(mCurveValues.data() + mWavelengths.size() * i,
                                mWavelengths.size())};
    }
    return {};
  }

private:
  /// The number of curves.
  size_t mNumCurves{};

  /// The wavelengths in nanometers.
  std::vector<float> mWavelengths;

  /// The curve values.
  std::vector<float> mCurveValues;

  /// The curve names. Optional!
  std::vector<std::string> mCurveNames;
};

/// \}

} // namespace smdl
