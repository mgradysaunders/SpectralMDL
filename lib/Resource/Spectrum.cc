#include "smdl/Resource/Spectrum.h"
#include "smdl/Support/Filesystem.h"
#include "smdl/Support/Strings.h"
#include "llvm/ADT/StringRef.h"
#include <cstdio>

#include "Wavelength.h"

namespace smdl {

std::optional<Error>
Spectrum::loadFromFile(const std::string &fileName) noexcept {
  clear();
  auto error{catchAndReturnError([&] {
    auto file{openOrThrow(fileName, std::ios::in)};
    auto line{std::string()};
    auto units{WAVE_UNITS_MICROMETERS};
    bool hasUnitsYet{false};
    while (std::getline(file, line)) {
      auto lineRef{llvm::StringRef(line).trim()};
      if (lineRef.empty() || lineRef[0] == '#') continue;
      if (!hasUnitsYet) {
        hasUnitsYet = true;
        if (lineRef.equals_insensitive("angstroms")) {
          units = WAVE_UNITS_ANGSTROMS;
          continue;
        } else if (lineRef.equals_insensitive("micrometers")) {
          units = WAVE_UNITS_MICROMETERS;
          continue;
        } else if (lineRef.equals_insensitive("nanometers")) {
          units = WAVE_UNITS_NANOMETERS;
          continue;
        } else if (lineRef.equals_insensitive("wavenumbers")) {
          units = WAVE_UNITS_WAVENUMBERS;
          continue;
        } else if (lineRef.equals_insensitive("megahertz")) {
          units = WAVE_UNITS_MEGAHERTZ;
          continue;
        } else if (lineRef.equals_insensitive("gigahertz")) {
          units = WAVE_UNITS_GIGAHERTZ;
          continue;
        }
      }
      float wavelength{};
      float curveValue{};
      if (std::sscanf(lineRef.data(), "%f %f", &wavelength, &curveValue) != 2)
        throw Error(concat("cannot load ", QuotedPath(fileName),
                           ": expected 'wavelength value'"));
      mWavelengths.push_back(toNanometers(units, wavelength));
      mCurveValues.push_back(curveValue);
    }
    sortByWavelength(mWavelengths, mCurveValues, 1);
  })};
  if (error) {
    clear();
    return error;
  }
  return std::nullopt;
}

} // namespace smdl
