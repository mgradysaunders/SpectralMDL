#include "smdl/Support/SpectralData.h"
#include "smdl/Support/Filesystem.h"
#include "smdl/Support/MacroHelpers.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Endian.h"
#include <cstdio>
#include <cstring>

namespace smdl {

[[nodiscard]] static float to_nanometers(WaveUnits units, float wave) {
  switch (units) {
  case WAVE_UNITS_ANGSTROMS:
    return 0.1f * wave;
  case WAVE_UNITS_WAVENUMBERS:
    return 10e6 / static_cast<double>(wave);
  case WAVE_UNITS_MICROMETERS:
    return 1e3f * wave;
  case WAVE_UNITS_NANOMETERS:
    return wave;
  default:
    break;
  }
  return 0;
}

std::optional<Error> Spectrum::load_txt(const std::string &fileName,
                                        WaveUnits units) noexcept {
  clear();
  return catch_and_return_error([&] {
    auto file{open_or_throw(fileName, std::ios::in)};
    auto line{std::string()};
    while (std::getline(file, line)) {
      if (line.empty() || line[0] == '#')
        continue;
      float wavelength{};
      float value{};
      if (std::sscanf(line.c_str(), "%f %f", &wavelength, &value) != 2)
        throw Error("Expected 'wavelength value'");
      wavelengths.push_back(to_nanometers(units, wavelength));
      values.push_back(value);
    }
  });
}

std::optional<Error>
SpectrumLibrary::load_sli(const std::string &fileName) noexcept {
  clear();
  return catch_and_return_error([&] {
    auto hdrFile{read_or_throw(fileName + ".hdr")};
    auto hdr{llvm::StringRef(hdrFile)};
    if (!hdr.consume_front("ENVI")) {
      throw Error("not an ENVI header file");
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
          throw Error("not an ENVI Spectral Library");
        }
      } else if (key.equals_insensitive("data type")) {
        if (value.getAsInteger(10, dataType)) {
          throw Error("invalid 'data type'");
        }
      } else if (key.equals_insensitive("byte order")) {
        if (value.getAsInteger(10, byteOrder)) {
          throw Error("invalid 'byte order'");
        }
      } else if (key.equals_insensitive("header offset")) {
        if (value.getAsInteger(10, headerOffset)) {
          throw Error("invalid 'header offset'");
        }
      } else if (key.equals_insensitive("samples")) {
        if (value.getAsInteger(10, samples)) {
          throw Error("invalid 'samples'");
        }
      } else if (key.equals_insensitive("lines")) {
        if (value.getAsInteger(10, lines)) {
          throw Error("invalid 'lines'");
        }
      } else if (key.equals_insensitive("bands")) {
        if (value.getAsInteger(10, bands)) {
          throw Error("invalid 'bands'");
        }
        if (bands != 1) {
          throw Error("invalid 'bands', expected 1 for ENVI Spectral Library");
        }
      } else if (key.equals_insensitive("wavelength units")) {
        if (value.equals_insensitive("micrometers")) {
          units = WAVE_UNITS_MICROMETERS;
        } else if (value.equals_insensitive("nanometers")) {
          units = WAVE_UNITS_NANOMETERS;
        } else if (value.equals_insensitive("wavenumber")) {
          units = WAVE_UNITS_WAVENUMBERS;
        } else {
          // TODO GHz, MHz?
          throw Error("unsupported 'wavelength units'");
        }
      } else if (key.equals_insensitive("wavelength")) {
        wavelengths.clear();
        llvm::SmallVector<llvm::StringRef> splits{};
        value.split(splits, ',');
        for (auto &split : splits) {
          double wavelength{};
          if (split.trim().getAsDouble(wavelength)) {
            throw Error("invalid 'wavelength'");
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
      throw Error("invalid 'samples', inconsistent with 'wavelength'");
    }
    if (curveNames.size() != size_t(lines) && !curveNames.empty()) {
      throw Error("invalid 'lines', inconsistent with 'spectra names'");
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
      throw Error("unsupported 'data type', expected 4 or 5");
    }
    for (auto &wavelength : wavelengths) {
      wavelength = to_nanometers(units, wavelength);
    }
    auto binFile{read_or_throw(fileName)};
    auto bin{llvm::StringRef(binFile)};
    bin = bin.drop_front(headerOffset);
    auto endianness{byteOrder == 0 ? llvm::endianness::little
                                   : llvm::endianness::big};
    auto numValues{size_t(samples) * size_t(lines)};
    curves.clear();
    curves.reserve(numValues);
    for (size_t i = 0; i < numValues; i++) {
      switch (dataType) {
      case 4: {
        if (bin.size() < 4) {
          throw Error("invalid binary data");
        }
        auto value{float()};
        auto valueData{llvm::support::endian::read32(bin.data(), endianness)};
        bin = bin.drop_front(4);
        std::memcpy(&value, &valueData, 4);
        curves.push_back(value);
        break;
      }
      case 5: {
        if (bin.size() < 8) {
          throw Error("invalid binary data");
        }
        auto value{double()};
        auto valueData{llvm::support::endian::read64(bin.data(), endianness)};
        bin = bin.drop_front(8);
        std::memcpy(&value, &valueData, 8);
        curves.push_back(value);
        break;
      }
      default:
        SMDL_SANITY_CHECK(false);
        break;
      }
    }
  });
}

} // namespace smdl
