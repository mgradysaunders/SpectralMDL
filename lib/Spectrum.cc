#include "smdl/Spectrum.h"
#include "smdl/Support/Filesystem.h"
#include "smdl/Support/MacroHelpers.h"
#include "smdl/Support/StringHelpers.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Endian.h"
#include <cstdio>
#include <cstring>

namespace smdl {

[[nodiscard]]
static std::vector<size_t> sort_order(const std::vector<float> &wavelengths) {
  std::vector<size_t> order{};
  order.reserve(wavelengths.size());
  for (size_t i = 0; i < wavelengths.size(); i++)
    order.push_back(i);
  std::sort(order.begin(), order.end(), [&](size_t i, size_t j) {
    return wavelengths[i] < wavelengths[j];
  });
  return order;
}

void apply_sort_order(const std::vector<size_t> &order, Span<float> values) {
  SMDL_SANITY_CHECK(order.size() == values.size());
  std::vector<float> tmpValues{values.begin(), values.end()};
  for (size_t i = 0; i < order.size(); i++)
    values[i] = tmpValues[order[i]];
}

/// Wavelength units.
enum WaveUnits : int {
  WAVE_UNITS_ANGSTROMS,   ///< Angstroms.
  WAVE_UNITS_WAVENUMBERS, ///< Wavenumbers.
  WAVE_UNITS_MEGAHERTZ,   ///< Megahertz.
  WAVE_UNITS_GIGAHERTZ,   ///< Gigahertz.
  WAVE_UNITS_MICROMETERS, ///< Micrometers.
  WAVE_UNITS_NANOMETERS,  ///< Nanometers.
};

[[nodiscard]] static float to_nanometers(WaveUnits units, float wave) {
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

std::optional<Error>
Spectrum::load_from_file(const std::string &fileName) noexcept {
  clear();
  return catch_and_return_error([&] {
    auto file{open_or_throw(fileName, std::ios::in)};
    auto line{std::string()};
    auto units{WAVE_UNITS_MICROMETERS};
    bool hasUnitsYet{false};
    while (std::getline(file, line)) {
      auto lineRef{llvm::StringRef(line).trim()};
      if (lineRef.empty() || lineRef[0] == '#')
        continue;
      if (!hasUnitsYet) {
        hasUnitsYet = true;
        if (lineRef.equals_insensitive("angstroms")) {
          units = WAVE_UNITS_ANGSTROMS;
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
        throw Error(concat("cannot load ", quoted_path(fileName),
                           ": expected 'wavelength value'"));
      wavelengths.push_back(to_nanometers(units, wavelength));
      curveValues.push_back(curveValue);
    }
    // Sort wavelengths into increasing order
    auto order{sort_order(wavelengths)};
    apply_sort_order(order, wavelengths);
    apply_sort_order(order, curveValues);
  });
}

std::optional<Error>
SpectrumLibrary::load_from_file(const std::string &fileName) noexcept {
  clear();
  return catch_and_return_error([&] {
    auto throwError{[&](const char *message) {
      throw Error(concat("cannot load ", quoted_path(fileName), ": ", message));
    }};
    auto hdrFile{read_or_throw(fileName + ".hdr")};
    auto hdr{llvm::StringRef(hdrFile)};
    if (!hdr.consume_front("ENVI")) {
      throwError("not an ENVI header file");
    }
    auto split{[&](char ch) {
      if (ch == '\n') {
        auto i{hdr.rfind('\n', hdr.find('='))};
        if (i != hdr.npos) {
          auto split0{hdr.take_front(i)};
          auto split1{hdr.drop_front(i)};
          hdr = split1.trim();
          return split0.trim();
        }
      }
      auto [split0, split1] = hdr.split(ch);
      hdr = split1.trim();
      return split0.trim();
    }};
    auto splitKeyValue{[&]() {
      auto key{split('=')};
      if (hdr.starts_with('{')) {
        hdr = hdr.drop_front(1);
        return std::pair(key, split('}'));
      } else {
        return std::pair(key, split('\n'));
      }
    }};
    int dataType{};
    int byteOrder{};
    int headerOffset{};
    int samples{};
    int lines{};
    int bands{};
    WaveUnits units{WAVE_UNITS_MICROMETERS};
    while (!hdr.empty()) {
      auto [key, value] = splitKeyValue();
      if (key.equals_insensitive("file type")) {
        if (!value.equals_insensitive("ENVI Spectral Library")) {
          throwError("not an ENVI Spectral Library");
        }
      } else if (key.equals_insensitive("data type")) {
        if (value.getAsInteger(10, dataType)) {
          throwError("invalid 'data type'");
        }
      } else if (key.equals_insensitive("byte order")) {
        if (value.getAsInteger(10, byteOrder)) {
          throwError("invalid 'byte order'");
        }
      } else if (key.equals_insensitive("header offset")) {
        if (value.getAsInteger(10, headerOffset)) {
          throwError("invalid 'header offset'");
        }
      } else if (key.equals_insensitive("samples")) {
        if (value.getAsInteger(10, samples)) {
          throwError("invalid 'samples'");
        }
      } else if (key.equals_insensitive("lines")) {
        if (value.getAsInteger(10, lines)) {
          throwError("invalid 'lines'");
        }
      } else if (key.equals_insensitive("bands")) {
        if (value.getAsInteger(10, bands)) {
          throwError("invalid 'bands'");
        }
        if (bands != 1) {
          throwError("invalid 'bands', expected 1 for ENVI Spectral Library");
        }
      } else if (key.equals_insensitive("wavelength units")) {
        if (value.equals_insensitive("micrometers")) {
          units = WAVE_UNITS_MICROMETERS;
        } else if (value.equals_insensitive("nanometers")) {
          units = WAVE_UNITS_NANOMETERS;
        } else if (value.equals_insensitive("wavenumber")) {
          units = WAVE_UNITS_WAVENUMBERS;
        } else if (value.equals_insensitive("mhz")) {
          units = WAVE_UNITS_MEGAHERTZ;
        } else if (value.equals_insensitive("ghz")) {
          units = WAVE_UNITS_GIGAHERTZ;
        } else {
          throwError("unsupported 'wavelength units'");
        }
      } else if (key.equals_insensitive("wavelength")) {
        wavelengths.clear();
        llvm::SmallVector<llvm::StringRef> splits{};
        value.split(splits, ',');
        for (auto &split : splits) {
          double wavelength{};
          if (split.trim().getAsDouble(wavelength)) {
            throwError("invalid 'wavelength'");
          }
          wavelengths.push_back(wavelength);
        }
      } else if (key.equals_insensitive("spectra names")) {
        curveNames.clear();
        llvm::SmallVector<llvm::StringRef> splits{};
        value.split(splits, ',');
        for (auto &split : splits) {
          curveNames.push_back(split.trim().str());
        }
      }
    }
    if (wavelengths.size() != size_t(samples)) {
      throwError("invalid 'samples', inconsistent with 'wavelength'");
    }
    if (curveNames.size() != size_t(lines) && !curveNames.empty()) {
      throwError("invalid 'lines', inconsistent with 'spectra names'");
    }
    // data type
    // 1 = byte
    // 2 = 16-bit int
    // 3 = 32-bit int
    // 4 = 32-bit float
    // 5 = 64-bit float
    // 6 = 32-bit complex float
    // 9 = 64-bit complex float
    // 12 = 16-bit unsigned integer
    // 13 = 32-bit unsigned integer
    // 14 = 64-bit int
    // 15 = 64-bit unsigned integer
    if (!(dataType == 4 || dataType == 5)) {
      throwError("unsupported 'data type', expected 4 or 5");
    }
    for (auto &wavelength : wavelengths) {
      wavelength = to_nanometers(units, wavelength);
    }
    auto binFile{read_or_throw(fileName)};
    auto bin{llvm::StringRef(binFile)};
    bin = bin.drop_front(headerOffset);
    auto endianness{byteOrder == 0 ? llvm::endianness::little
                                   : llvm::endianness::big};
    auto numCurveValues{size_t(samples) * size_t(lines)};
    numCurves = size_t(lines);
    curveValues.clear();
    curveValues.reserve(numCurveValues);
    for (size_t i = 0; i < numCurveValues; i++) {
      switch (dataType) {
      case 4: {
        if (bin.size() < 4) {
          throwError("invalid binary data");
        }
        auto value{float()};
        auto valueData{llvm::support::endian::read32(bin.data(), endianness)};
        bin = bin.drop_front(4);
        std::memcpy(&value, &valueData, 4);
        curveValues.push_back(value);
        break;
      }
      case 5: {
        if (bin.size() < 8) {
          throwError("invalid binary data");
        }
        auto value{double()};
        auto valueData{llvm::support::endian::read64(bin.data(), endianness)};
        bin = bin.drop_front(8);
        std::memcpy(&value, &valueData, 8);
        curveValues.push_back(value);
        break;
      }
      default:
        SMDL_SANITY_CHECK(false);
        break;
      }
    }
    // Sort wavelengths into increasing order
    auto order{sort_order(wavelengths)};
    apply_sort_order(order, wavelengths);
    for (size_t i = 0; i < numCurves; i++)
      apply_sort_order(order,
                       Span<float>(curveValues.data() + wavelengths.size() * i,
                                   wavelengths.size()));
  });
}

SpectrumView
SpectrumLibrary::get_curve_by_name(std::string_view name) const noexcept {
  for (size_t i = 0; i < curveNames.size(); i++) {
    if (llvm::StringRef(curveNames[i])
            .equals_insensitive(llvm::StringRef(name)))
      return get_curve_by_index(i);
  }
  return {};
}

} // namespace smdl
