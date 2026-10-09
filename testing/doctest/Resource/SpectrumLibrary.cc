#include "Fixtures.h"

#include <cstdint>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <string>
#include <string_view>

#include "smdl/Resource/SpectrumLibrary.h"

namespace fs = std::filesystem;

namespace {
// The library the tests write: three named curves over four wavelengths
// given out of order, so loading must sort them and permute every curve
// alongside. Each curve is the wavelength in micrometers times its scale,
// which makes the expected values obvious after the sort.
const float WAVELENGTHS_UM[4] = {0.7f, 0.4f, 0.6f, 0.5f};
const float CURVE_SCALES[3] = {10.0f, 100.0f, 1.0f};

struct LibraryOptions final {
  const char *fileType{"ENVI Spectral Library"};
  int dataType{4};
  int byteOrder{0};
  int headerOffset{0};
  int samples{4};
  int bands{1};
  const char *wavelengthUnits{"Micrometers"};
  float wavelengthScale{1.0f};
  bool useNames{true};
  bool isTruncated{false};
};

// Append the bytes of `bits` in the given order, independent of the
// host's own.
template <typename Bits>
void appendBits(std::string &out, Bits bits, bool isBigEndian) {
  for (size_t k = 0; k < sizeof(Bits); k++) {
    const size_t shift{8 * (isBigEndian ? sizeof(Bits) - 1 - k : k)};
    out += char((bits >> shift) & 0xff);
  }
}

void writeLibrary(const fs::path &fileName, const LibraryOptions &opts) {
  std::string hdr{"ENVI\n"};
  hdr += "description = {Synthetic test library}\n";
  hdr += "samples = " + std::to_string(opts.samples) + "\n";
  hdr += "lines = 3\n";
  hdr += "bands = " + std::to_string(opts.bands) + "\n";
  hdr += "header offset = " + std::to_string(opts.headerOffset) + "\n";
  hdr += "file type = " + std::string(opts.fileType) + "\n";
  hdr += "data type = " + std::to_string(opts.dataType) + "\n";
  hdr += "interleave = bsq\n";
  hdr += "byte order = " + std::to_string(opts.byteOrder) + "\n";
  hdr += "wavelength units = " + std::string(opts.wavelengthUnits) + "\n";
  // A braced list that spans lines, as ENVI writes them.
  hdr += "wavelength = {";
  for (int i = 0; i < 4; i++) {
    hdr += (i == 0 ? "" : i == 2 ? ",\n " : ", ");
    hdr += std::to_string(opts.wavelengthScale * WAVELENGTHS_UM[i]);
  }
  hdr += "}\n";
  if (opts.useNames) hdr += "spectra names = {Alpha, Beta, Gamma}\n";
  std::ofstream(fileName.string() + ".hdr") << hdr;
  // The binary: junk the offset must skip, then the curves back to back.
  std::string bin(size_t(opts.headerOffset), '\xAB');
  const bool isBigEndian{opts.byteOrder != 0};
  for (float scale : CURVE_SCALES) {
    for (float wavelengthUm : WAVELENGTHS_UM) {
      const float value{scale * wavelengthUm};
      if (opts.dataType == 5) {
        const double valueD{value};
        uint64_t bits{};
        std::memcpy(&bits, &valueD, 8);
        appendBits(bin, bits, isBigEndian);
      } else {
        uint32_t bits{};
        std::memcpy(&bits, &value, 4);
        appendBits(bin, bits, isBigEndian);
      }
    }
  }
  if (opts.isTruncated) bin.resize(bin.size() - 1);
  std::ofstream(fileName, std::ios::binary) << bin;
}

// Is `view` the curve of the given scale, sorted by wavelength?
void checkCurve(smdl::SpectrumView view, float scale) {
  REQUIRE(view.wavelengths.size() == 4);
  REQUIRE(view.curveValues.size() == 4);
  const float sortedUm[4] = {0.4f, 0.5f, 0.6f, 0.7f};
  for (size_t i = 0; i < 4; i++) {
    CHECK(view.wavelengths.data()[i] == doctest::Approx(1000 * sortedUm[i]));
    CHECK(view.curveValues.data()[i] == doctest::Approx(scale * sortedUm[i]));
  }
}
} // namespace

TEST_CASE("SpectrumLibrary: the ENVI variants it reads") {
  TempDir tmpDir{"spectrum-library"};
  smdl::SpectrumLibrary library{};
  SUBCASE("A float, little-endian, micrometer library reads back its curves") {
    std::string fileName{(tmpDir / "float.sli").string()};
    writeLibrary(fileName, {});
    REQUIRE_OK(library.loadFromFile(fileName));
    for (int i = 0; i < 3; i++)
      checkCurve(library.getCurveByIndex(i), CURVE_SCALES[i]);
    // Names match without regard to case.
    checkCurve(library.getCurveByName("Alpha"), CURVE_SCALES[0]);
    checkCurve(library.getCurveByName("beta"), CURVE_SCALES[1]);
    checkCurve(library.getCurveByName("GAMMA"), CURVE_SCALES[2]);
    // Out of range and unknown lookups give the empty view.
    CHECK(library.getCurveByIndex(-1).curveValues.empty());
    CHECK(library.getCurveByIndex(3).curveValues.empty());
    CHECK(library.getCurveByName("Delta").curveValues.empty());
  }
  SUBCASE(
      "A double, big-endian, nanometer library with a header offset does too") {
    LibraryOptions opts{};
    opts.dataType = 5;
    opts.byteOrder = 1;
    opts.headerOffset = 16;
    opts.wavelengthUnits = "Nanometers";
    opts.wavelengthScale = 1000.0f;
    std::string fileName{(tmpDir / "double.sli").string()};
    writeLibrary(fileName, opts);
    REQUIRE_OK(library.loadFromFile(fileName));
    for (int i = 0; i < 3; i++)
      checkCurve(library.getCurveByIndex(i), CURVE_SCALES[i]);
  }
  SUBCASE("A library with no spectra names still reads") {
    LibraryOptions opts{};
    opts.useNames = false;
    std::string fileName{(tmpDir / "unnamed.sli").string()};
    writeLibrary(fileName, opts);
    REQUIRE_OK(library.loadFromFile(fileName));
    checkCurve(library.getCurveByIndex(2), CURVE_SCALES[2]);
    CHECK(library.getCurveByName("Alpha").curveValues.empty());
  }
  SUBCASE("A malformed library is refused and leaves nothing behind") {
    // A good load first, so each rejection also shows the failure
    // leaves the library empty rather than half loaded.
    std::string goodName{(tmpDir / "good.sli").string()};
    writeLibrary(goodName, {});
    REQUIRE_OK(library.loadFromFile(goodName));
    // Each message names the header line of the key at fault, counting
    // the 'ENVI' magic as line 1.
    auto reject{[&](const char *name, const LibraryOptions &opts,
                    std::string_view message) {
      CAPTURE(name);
      std::string fileName{(tmpDir / name).string()};
      writeLibrary(fileName, opts);
      const std::optional<smdl::Error> error{library.loadFromFile(fileName)};
      REQUIRE(error.has_value());
      CHECK_CONTAINS(error->message, message);
      CHECK(library.getCurveByIndex(0).curveValues.empty());
    }};
    LibraryOptions opts{};
    opts.fileType = "ENVI Standard";
    reject("file_type.sli", opts,
           "'file type' on line 7 of its header is \"ENVI Standard\", not "
           "'ENVI Spectral Library'");
    opts = {};
    opts.bands = 2;
    reject("bands.sli", opts,
           "'bands' on line 5 of its header is 2, but a spectral library has "
           "1");
    opts = {};
    opts.samples = 5;
    reject("samples.sli", opts,
           "'samples' on line 3 of its header is 5, but 'wavelength' lists 4 "
           "wavelengths");
    opts = {};
    opts.dataType = 2;
    reject("data_type.sli", opts,
           "Unsupported 'data type' 2 on line 8 of its header, expected 4 or "
           "5");
    opts = {};
    opts.wavelengthUnits = "Furlongs";
    reject("units.sli", opts,
           "Unsupported 'wavelength units' \"Furlongs\" on line 11 of its "
           "header");
    opts = {};
    opts.isTruncated = true;
    reject("truncated.sli", opts,
           "The data holds 11 of the 12 values its header describes");
    // No header file at all.
    std::ofstream((tmpDir / "headerless.sli").string()) << "";
    CHECK(
        library.loadFromFile((tmpDir / "headerless.sli").string()).has_value());
    CHECK(library.loadFromFile((tmpDir / "missing.sli").string()).has_value());
  }
}
