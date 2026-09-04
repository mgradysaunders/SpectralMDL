/// \file
#pragma once

#include <string>
#include <string_view>
#include <vector>

#include "smdl/Resource/Spectrum.h"

namespace smdl {

/// \addtogroup resource
/// \{

/// A spectrum library loaded from an ENVI Spectral Library file.
class SMDL_EXPORT SpectrumLibrary final {
public:
  void clear() noexcept {
    mNumCurves = 0;
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
  /// - `spectra names`: this is optional, if present contains the names
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
