#include "doctest.h"

#include <filesystem>
#include <fstream>
#include <string>

#include "smdl/Resource/Spectrum.h"

namespace fs = std::filesystem;

static void writeText(const fs::path &fileName, const std::string &text) {
  std::ofstream(fileName) << text;
}

// Load `text` as a spectrum and return the view, requiring success.
static smdl::SpectrumView loadText(smdl::Spectrum &spectrum,
                                   const fs::path &fileName,
                                   const std::string &text) {
  writeText(fileName, text);
  REQUIRE(!spectrum.loadFromFile(fileName.string()));
  return spectrum;
}

TEST_CASE("Spectrum") {
  auto tmpDir{fs::temp_directory_path() / "smdl-spectrum-test"};
  fs::remove_all(tmpDir);
  fs::create_directories(tmpDir);
  smdl::Spectrum spectrum{};
  SUBCASE("Rows sort by wavelength, comments and blanks are skipped") {
    // No units line, so the wavelengths are micrometers.
    auto view{loadText(spectrum, tmpDir / "sorted.txt",
                       "# A comment\n"
                       "0.7 7\n"
                       "\n"
                       "0.4 4\n"
                       "   # An indented comment\n"
                       "0.6 6\n"
                       "0.5 5\n")};
    REQUIRE(view.wavelengths.size() == 4);
    REQUIRE(view.curveValues.size() == 4);
    const float expected[4] = {4, 5, 6, 7};
    for (size_t i = 0; i < 4; i++) {
      CHECK(view.wavelengths.data()[i] == doctest::Approx(100 * expected[i]));
      CHECK(view.curveValues.data()[i] == doctest::Approx(expected[i]));
    }
  }
  SUBCASE("Units line") {
    // Each spells 400 nanometers in its own units, in whatever case.
    struct Case {
      const char *units{};
      const char *wavelength{};
    };
    const Case cases[] = {
        {"angstroms", "4000"},      {"Nanometers", "400"},
        {"MICROMETERS", "0.4"},     {"wavenumbers", "25000"},
        {"megahertz", "749481145"}, {"gigahertz", "749481.145"}};
    for (const auto &c : cases) {
      CAPTURE(c.units);
      auto view{loadText(spectrum, tmpDir / "units.txt",
                         std::string(c.units) + "\n" + c.wavelength + " 1\n")};
      REQUIRE(view.wavelengths.size() == 1);
      CHECK(view.wavelengths.data()[0] == doctest::Approx(400.0f));
      CHECK(view.curveValues.data()[0] == 1.0f);
    }
  }
  SUBCASE("Failure leaves the spectrum empty") {
    (void)loadText(spectrum, tmpDir / "good.txt", "0.4 4\n");
    auto fileName{(tmpDir / "bad.txt").string()};
    writeText(fileName, "0.4 4\n0.5 abc\n");
    CHECK(spectrum.loadFromFile(fileName).has_value());
    CHECK(smdl::SpectrumView(spectrum).wavelengths.empty());
    CHECK(spectrum.loadFromFile((tmpDir / "missing.txt").string()).has_value());
  }
  fs::remove_all(tmpDir);
}
