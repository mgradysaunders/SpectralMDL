#include "doctest.h"

#include <cstdint>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <string>

#include "smdl/Resource/SpectrumLibrary.h"

namespace fs = std::filesystem;

// The library the tests write: three named curves over four wavelengths
// given out of order, so loading must sort them and permute every curve
// alongside. Each curve is the wavelength in micrometers times its scale,
// which makes the expected values obvious after the sort.
static const float WAVELENGTHS_UM[4] = {0.7f, 0.4f, 0.6f, 0.5f};
static const float CURVE_SCALES[3] = {10.0f, 100.0f, 1.0f};

struct LibraryOptions final {
  const char *fileType{"ENVI Spectral Library"};
  int dataType{4};
  int byteOrder{0};
  int headerOffset{0};
  int samples{4};
  int bands{1};
  const char *wavelengthUnits{"Micrometers"};
  float wavelengthScale{1.0f};
  bool withNames{true};
  bool truncated{false};
};

// Append the bytes of `bits` in the given order, independent of the
// host's own.
template <typename Bits>
static void appendBits(std::string &out, Bits bits, bool bigEndian) {
  for (size_t k = 0; k < sizeof(Bits); k++) {
    const size_t shift{8 * (bigEndian ? sizeof(Bits) - 1 - k : k)};
    out += char((bits >> shift) & 0xff);
  }
}

static void writeLibrary(const fs::path &fileName, const LibraryOptions &opts) {
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
  if (opts.withNames) hdr += "spectra names = {Alpha, Beta, Gamma}\n";
  std::ofstream(fileName.string() + ".hdr") << hdr;
  // The binary: junk the offset must skip, then the curves back to back.
  std::string bin(size_t(opts.headerOffset), '\xAB');
  const bool bigEndian{opts.byteOrder != 0};
  for (float scale : CURVE_SCALES) {
    for (float wavelengthUm : WAVELENGTHS_UM) {
      const float value{scale * wavelengthUm};
      if (opts.dataType == 5) {
        const double valueD{value};
        uint64_t bits{};
        std::memcpy(&bits, &valueD, 8);
        appendBits(bin, bits, bigEndian);
      } else {
        uint32_t bits{};
        std::memcpy(&bits, &value, 4);
        appendBits(bin, bits, bigEndian);
      }
    }
  }
  if (opts.truncated) bin.resize(bin.size() - 1);
  std::ofstream(fileName, std::ios::binary) << bin;
}

// Is `view` the curve of the given scale, sorted by wavelength?
static void checkCurve(smdl::SpectrumView view, float scale) {
  REQUIRE(view.wavelengths.size() == 4);
  REQUIRE(view.curveValues.size() == 4);
  const float sortedUm[4] = {0.4f, 0.5f, 0.6f, 0.7f};
  for (size_t i = 0; i < 4; i++) {
    CHECK(view.wavelengths.data()[i] == doctest::Approx(1000 * sortedUm[i]));
    CHECK(view.curveValues.data()[i] == doctest::Approx(scale * sortedUm[i]));
  }
}

TEST_CASE("SpectrumLibrary") {
  auto tmpDir{fs::temp_directory_path() / "smdl-spectrum-library-test"};
  fs::remove_all(tmpDir);
  fs::create_directories(tmpDir);
  smdl::SpectrumLibrary library{};
  SUBCASE("Float, little endian, micrometers") {
    auto fileName{(tmpDir / "float.sli").string()};
    writeLibrary(fileName, {});
    REQUIRE(!library.loadFromFile(fileName));
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
  SUBCASE("Double, big endian, nanometers, header offset") {
    LibraryOptions opts{};
    opts.dataType = 5;
    opts.byteOrder = 1;
    opts.headerOffset = 16;
    opts.wavelengthUnits = "Nanometers";
    opts.wavelengthScale = 1000.0f;
    auto fileName{(tmpDir / "double.sli").string()};
    writeLibrary(fileName, opts);
    REQUIRE(!library.loadFromFile(fileName));
    for (int i = 0; i < 3; i++)
      checkCurve(library.getCurveByIndex(i), CURVE_SCALES[i]);
  }
  SUBCASE("Without spectra names") {
    LibraryOptions opts{};
    opts.withNames = false;
    auto fileName{(tmpDir / "unnamed.sli").string()};
    writeLibrary(fileName, opts);
    REQUIRE(!library.loadFromFile(fileName));
    checkCurve(library.getCurveByIndex(2), CURVE_SCALES[2]);
    CHECK(library.getCurveByName("Alpha").curveValues.empty());
  }
  SUBCASE("Rejects") {
    // A good load first, so each rejection also shows the failure
    // leaves the library empty rather than half loaded.
    auto goodName{(tmpDir / "good.sli").string()};
    writeLibrary(goodName, {});
    REQUIRE(!library.loadFromFile(goodName));
    auto reject{[&](const char *name, const LibraryOptions &opts) {
      CAPTURE(name);
      auto fileName{(tmpDir / name).string()};
      writeLibrary(fileName, opts);
      CHECK(library.loadFromFile(fileName).has_value());
      CHECK(library.getCurveByIndex(0).curveValues.empty());
    }};
    LibraryOptions opts{};
    opts.fileType = "ENVI Standard";
    reject("file_type.sli", opts);
    opts = {};
    opts.bands = 2;
    reject("bands.sli", opts);
    opts = {};
    opts.samples = 5;
    reject("samples.sli", opts);
    opts = {};
    opts.dataType = 2;
    reject("data_type.sli", opts);
    opts = {};
    opts.wavelengthUnits = "Furlongs";
    reject("units.sli", opts);
    opts = {};
    opts.truncated = true;
    reject("truncated.sli", opts);
    // No header file at all.
    std::ofstream((tmpDir / "headerless.sli").string()) << "";
    CHECK(
        library.loadFromFile((tmpDir / "headerless.sli").string()).has_value());
    CHECK(library.loadFromFile((tmpDir / "missing.sli").string()).has_value());
  }
  fs::remove_all(tmpDir);
}
