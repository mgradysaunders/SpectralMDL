#include "smdl/Resource/SpectrumLibrary.h"
#include "smdl/Support/Filesystem.h"
#include "smdl/Support/Macros.h"
#include "smdl/Support/Strings.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Endian.h"
#include <algorithm>
#include <cstring>

#include "Wavelength.h"

namespace smdl {

std::optional<Error>
SpectrumLibrary::loadFromFile(const std::string &fileName) noexcept {
  clear();
  auto error{catchAndReturnError([&] {
    auto throwError{[&](std::string_view message) {
      throw Error(concat("cannot load ", QuotedPath(fileName), ": ", message));
    }};
    auto hdrFile{readOrThrow(fileName + ".hdr")};
    auto hdr{llvm::StringRef(hdrFile)};
    // Where in the header a key was, for an error: nothing for a key the
    // header left out, whose value is the default.
    const auto onLine{[&](int lineNo) {
      return lineNo > 0 ? concat(" on line ", lineNo, " of its header")
                        : std::string();
    }};
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
    int dataTypeLine{};
    int samplesLine{};
    int linesLine{};
    while (!hdr.empty()) {
      const auto keyLine{
          1 + int(std::count(hdrFile.c_str(), hdr.ltrim().data(), '\n'))};
      const auto throwKeyError{[&](std::string_view message) {
        throwError(concat(message, onLine(keyLine)));
      }};
      auto [key, value] = splitKeyValue();
      if (key.equals_insensitive("file type")) {
        if (!value.equals_insensitive("ENVI Spectral Library")) {
          throwError(concat("'file type'", onLine(keyLine), " is ",
                            Quoted(value), ", not 'ENVI Spectral Library'"));
        }
      } else if (key.equals_insensitive("data type")) {
        if (value.getAsInteger(10, dataType)) {
          throwKeyError("invalid 'data type'");
        }
        dataTypeLine = keyLine;
      } else if (key.equals_insensitive("byte order")) {
        if (value.getAsInteger(10, byteOrder)) {
          throwKeyError("invalid 'byte order'");
        }
      } else if (key.equals_insensitive("header offset")) {
        if (value.getAsInteger(10, headerOffset)) {
          throwKeyError("invalid 'header offset'");
        }
      } else if (key.equals_insensitive("samples")) {
        if (value.getAsInteger(10, samples)) {
          throwKeyError("invalid 'samples'");
        }
        samplesLine = keyLine;
      } else if (key.equals_insensitive("lines")) {
        if (value.getAsInteger(10, lines)) {
          throwKeyError("invalid 'lines'");
        }
        linesLine = keyLine;
      } else if (key.equals_insensitive("bands")) {
        if (value.getAsInteger(10, bands)) {
          throwKeyError("invalid 'bands'");
        }
        if (bands != 1) {
          throwError(concat("'bands'", onLine(keyLine), " is ", bands,
                            ", but a spectral library has 1"));
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
          throwError(concat("unsupported 'wavelength units' ", Quoted(value),
                            onLine(keyLine)));
        }
      } else if (key.equals_insensitive("wavelength")) {
        mWavelengths.clear();
        llvm::SmallVector<llvm::StringRef> splits{};
        value.split(splits, ',');
        for (auto &split : splits) {
          double wavelength{};
          if (split.trim().getAsDouble(wavelength)) {
            throwKeyError("invalid 'wavelength'");
          }
          mWavelengths.push_back(wavelength);
        }
      } else if (key.equals_insensitive("spectra names")) {
        mCurveNames.clear();
        llvm::SmallVector<llvm::StringRef> splits{};
        value.split(splits, ',');
        for (auto &split : splits) {
          mCurveNames.push_back(split.trim().str());
        }
      }
    }
    if (mWavelengths.size() != size_t(samples)) {
      throwError(concat("'samples'", onLine(samplesLine), " is ", samples,
                        ", but 'wavelength' lists ", mWavelengths.size(),
                        " wavelengths"));
    }
    if (mCurveNames.size() != size_t(lines) && !mCurveNames.empty()) {
      throwError(concat("'lines'", onLine(linesLine), " is ", lines,
                        ", but 'spectra names' lists ", mCurveNames.size(),
                        " names"));
    }
    // Of the ENVI data types, only 4 (32-bit float) and 5 (64-bit float)
    // make sense for a spectral library.
    if (!(dataType == 4 || dataType == 5)) {
      throwError(concat("unsupported 'data type' ", dataType,
                        onLine(dataTypeLine), ", expected 4 or 5"));
    }
    for (auto &wavelength : mWavelengths) {
      wavelength = toNanometers(units, wavelength);
    }
    auto binFile{readOrThrow(fileName)};
    auto bin{llvm::StringRef(binFile)};
    bin =
        bin.drop_front(std::min(bin.size(), size_t(std::max(headerOffset, 0))));
    auto endianness{byteOrder == 0 ? llvm::endianness::little
                                   : llvm::endianness::big};
    auto numCurveValues{size_t(samples) * size_t(lines)};
    if (const auto valueSize{size_t(dataType == 4 ? 4 : 8)};
        bin.size() < numCurveValues * valueSize) {
      throwError(concat("the data holds ", bin.size() / valueSize, " of the ",
                        numCurveValues, " values its header describes"));
    }
    mNumCurves = size_t(lines);
    mCurveValues.clear();
    mCurveValues.reserve(numCurveValues);
    for (size_t i = 0; i < numCurveValues; i++) {
      switch (dataType) {
      case 4: {
        auto value{float()};
        auto valueData{llvm::support::endian::read32(bin.data(), endianness)};
        bin = bin.drop_front(4);
        std::memcpy(&value, &valueData, 4);
        mCurveValues.push_back(value);
        break;
      }
      case 5: {
        auto value{double()};
        auto valueData{llvm::support::endian::read64(bin.data(), endianness)};
        bin = bin.drop_front(8);
        std::memcpy(&value, &valueData, 8);
        mCurveValues.push_back(value);
        break;
      }
      default:
        SMDL_SANITY_CHECK(false);
        break;
      }
    }
    sortByWavelength(mWavelengths, mCurveValues, mNumCurves);
  })};
  if (error) {
    clear();
    return error;
  }
  return std::nullopt;
}

SpectrumView
SpectrumLibrary::getCurveByName(std::string_view name) const noexcept {
  for (size_t i = 0; i < mCurveNames.size(); i++) {
    if (llvm::StringRef(mCurveNames[i])
            .equals_insensitive(llvm::StringRef(name)))
      return getCurveByIndex(i);
  }
  return {};
}

} // namespace smdl
