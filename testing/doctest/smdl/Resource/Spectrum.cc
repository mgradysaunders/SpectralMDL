#include "Fixtures.h"

#include <string>

#include "smdl/Resource/Spectrum.h"

namespace {
// Load `text` as a spectrum and return the view, requiring success.
smdl::SpectrumView loadText(smdl::Spectrum &spectrum, const TempDir &tmpDir,
                            std::string_view name, const std::string &text) {
  REQUIRE_OK(spectrum.loadFromFile(tmpDir.write(name, text).string()));
  return spectrum;
}
} // namespace

TEST_CASE("Spectrum: the text format and its failures") {
  TempDir tmpDir{"spectrum"};
  smdl::Spectrum spectrum{};
  SUBCASE("Rows sort by wavelength, comments and blanks are skipped") {
    // No units line, so the wavelengths are micrometers.
    smdl::SpectrumView view{loadText(spectrum, tmpDir, "sorted.txt",
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
  SUBCASE("A units line changes what the wavelengths mean") {
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
      smdl::SpectrumView view{
          loadText(spectrum, tmpDir, "units.txt",
                   std::string(c.units) + "\n" + c.wavelength + " 1\n")};
      REQUIRE(view.wavelengths.size() == 1);
      CHECK(view.wavelengths.data()[0] == doctest::Approx(400.0f));
      CHECK(view.curveValues.data()[0] == 1.0f);
    }
  }
  SUBCASE("A row that does not parse is named by its line") {
    std::optional<smdl::Error> error{spectrum.loadFromFile(
        tmpDir.write("row.txt", "# A comment\n0.4 4\n\n0.5 abc\n").string())};
    REQUIRE(error.has_value());
    CHECK_CONTAINS(error->message, ": expected 'wavelength value' on line 4");
    // The first row may name the units instead, so it is refused as both.
    error = spectrum.loadFromFile(
        tmpDir.write("units_typo.txt", "# A comment\nnm\n400 4\n").string());
    REQUIRE(error.has_value());
    CHECK_CONTAINS(error->message,
                   ": expected wavelength units or 'wavelength value' on "
                   "line 2");
  }
  SUBCASE("Failure leaves the spectrum empty") {
    (void)loadText(spectrum, tmpDir, "good.txt", "0.4 4\n");
    const std::string fileName{
        tmpDir.write("bad.txt", "0.4 4\n0.5 abc\n").string()};
    CHECK(spectrum.loadFromFile(fileName).has_value());
    CHECK(smdl::SpectrumView(spectrum).wavelengths.empty());
    CHECK(spectrum.loadFromFile((tmpDir / "missing.txt").string()).has_value());
  }
}
