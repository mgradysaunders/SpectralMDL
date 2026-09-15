#include "Fixtures.h"

#include <algorithm>
#include <cmath>
#include <limits>
#include <string_view>

#include "smdl/RenderUtil/OpticalGlass.h"

namespace {

using Kind = smdl::OpticalGlass::Kind;

// The nd and Vd the SCHOTT catalog prints beside each glass's coefficients,
// typed apart from the library's table so that the two transcriptions
// check each other.
struct Printed final {
  std::string_view name{};
  float nd{};
  float abbeNumber{};
};

constexpr Printed PRINTED[] = {
    {"N-FK51A", 1.48656f, 84.47f}, {"N-PK52A", 1.49700f, 81.61f},
    {"N-BK7", 1.51680f, 64.17f},   {"N-SK16", 1.62041f, 60.32f},
    {"N-K5", 1.52249f, 59.48f},    {"N-BAK1", 1.57250f, 57.55f},
    {"N-BAK4", 1.56883f, 55.98f},  {"N-LAK9", 1.69100f, 54.71f},
    {"N-BAF10", 1.67003f, 47.11f}, {"N-KZFS4", 1.61336f, 44.49f},
    {"LF5", 1.58144f, 40.85f},     {"N-F2", 1.62005f, 36.43f},
    {"F2", 1.62004f, 36.37f},      {"SF2", 1.64769f, 33.85f},
    {"N-SF2", 1.64769f, 33.82f},   {"N-SF5", 1.67271f, 32.25f},
    {"N-LASF9", 1.85025f, 32.17f}, {"N-SF10", 1.72828f, 28.53f},
    {"N-SF11", 1.78472f, 25.68f},  {"N-SF6", 1.80518f, 25.36f},
    {"N-SF57", 1.84666f, 23.78f}};

// Malitson's fit, with each C a wavelength in micrometers as his papers
// print it.
[[nodiscard]] double malitson(const double (&b)[3], const double (&c)[3],
                              double wavelength) {
  const double l2{1e-6 * wavelength * wavelength};
  double sum{1};
  for (int i = 0; i < 3; i++) sum += b[i] * l2 / (l2 - c[i] * c[i]);
  return std::sqrt(sum);
}

// The glass the catalog holds under `name`, which it must.
[[nodiscard]] const smdl::OpticalGlass &catalogGlass(std::string_view name) {
  const smdl::OpticalGlassEntry *entry{smdl::findOpticalGlass(name)};
  REQUIRE_MESSAGE(entry, "the catalog holds no ", asText(name));
  return entry->glass;
}

} // namespace

TEST_CASE("OpticalGlass: the three ways a glass is stated") {
  SUBCASE("Air is exactly 1 at every wavelength") {
    const smdl::OpticalGlass air{};
    CHECK(air.kind() == Kind::CONSTANT);
    CHECK_FALSE(air.isDispersive());
    for (const float wavelength : {300.0f, smdl::FRAUNHOFER_D_LINE, 2500.0f})
      CHECK(air.indexAt(wavelength) == 1.0f);
  }
  SUBCASE("A constant glass is its index everywhere and disperses nothing") {
    const smdl::OpticalGlass glass{smdl::OpticalGlass::constant(1.67f)};
    CHECK_FALSE(glass.isDispersive());
    CHECK(glass.indexAt(400.0f) == 1.67f);
    CHECK(glass.indexAt(700.0f) == 1.67f);
    CHECK(glass.nd() == 1.67f);
    CHECK(glass.abbeNumber() == smdl::INF);
    CHECK(glass.partialDispersion() == 0.0f);
  }
  SUBCASE("An Abbe fit passes through its nd, Vd, and partial dispersion") {
    const smdl::OpticalGlass glass{
        smdl::OpticalGlass::abbe(1.62041f, 60.32f, 0.5412f)};
    CHECK(glass.kind() == Kind::ABBE);
    CHECK(glass.isDispersive());
    CHECK_NEAR(glass.nd(), 1.62041f, 1e-6f);
    CHECK_NEAR(glass.abbeNumber(), 60.32f, 1e-3f);
    CHECK_NEAR(glass.partialDispersion(), 0.5412f, 1e-4f);
  }
  SUBCASE("Without a partial dispersion the fit takes Schott's normal line") {
    for (const float abbeNumber : {25.0f, 45.0f, 64.0f}) {
      const smdl::OpticalGlass glass{
          smdl::OpticalGlass::abbe(1.6f, abbeNumber)};
      CHECK_NEAR(glass.partialDispersion(), 0.6438f - 0.001682f * abbeNumber,
                 1e-4f);
    }
  }
  SUBCASE("Glasses of different Abbe number differ in partial dispersion") {
    // Which a two-term Cauchy through nd and Vd alone cannot say, and is
    // why an achromat designed on one has no secondary spectrum.
    const smdl::OpticalGlass crown{smdl::OpticalGlass::abbe(1.5168f, 64.17f)};
    const smdl::OpticalGlass flint{smdl::OpticalGlass::abbe(1.62004f, 36.37f)};
    CHECK(flint.partialDispersion() - crown.partialDispersion() > 0.04f);
  }
  SUBCASE("An Abbe fit tracks the maker's Sellmeier through the visible") {
    // On the glass's own partial dispersion, so that what is left is the
    // Cauchy form against the Sellmeier.
    for (const auto &entry : smdl::opticalGlassCatalog()) {
      const smdl::OpticalGlass &maker{entry.glass};
      const smdl::OpticalGlass fit{smdl::OpticalGlass::abbe(
          maker.nd(), maker.abbeNumber(), maker.partialDispersion())};
      float worst{0.0f};
      for (int wavelength = 450; wavelength <= 700; wavelength += 5)
        worst = std::max(worst, std::abs(fit.indexAt(float(wavelength)) -
                                         maker.indexAt(float(wavelength))));
      CHECK_MESSAGE(worst < 4e-4f, asText(entry.name), " is off by ", worst);
    }
  }
  SUBCASE("Every catalog glass's nd and Vd make a fit on the normal line") {
    // A patent prints no partial dispersion, so this is what a transcribed
    // glass gets, and a real one must never be refused.
    for (const auto &entry : smdl::opticalGlassCatalog()) {
      const smdl::OpticalGlass &maker{entry.glass};
      INFO(asText(entry.name));
      CHECK_OK(smdl::catchAndReturnError([&] {
        (void)smdl::OpticalGlass::abbe(maker.nd(), maker.abbeNumber());
      }));
    }
  }
  SUBCASE("A Sellmeier glass is the maker's formula") {
    const smdl::OpticalGlass glass{smdl::OpticalGlass::sellmeier(
        {1.03961212f, 0.231792344f, 1.01046945f},
        {0.00600069867f, 0.0200179144f, 103.560653f})};
    CHECK(glass.kind() == Kind::SELLMEIER);
    CHECK(glass.isDispersive());
    const smdl::OpticalGlass &catalog{catalogGlass("N-BK7")};
    for (const float wavelength : {400.0f, 550.0f, 700.0f, 1550.0f})
      CHECK(
          hasSameBits(glass.indexAt(wavelength), catalog.indexAt(wavelength)));
  }
  SUBCASE("A wavelength outside the domain reads as the nearest end") {
    const smdl::OpticalGlass &glass{catalogGlass("N-SF11")};
    const float shortEnd{glass.indexAt(smdl::OPTICAL_GLASS_WAVELENGTH_MIN)};
    const float longEnd{glass.indexAt(smdl::OPTICAL_GLASS_WAVELENGTH_MAX)};
    CHECK(glass.indexAt(100.0f) == shortEnd);
    CHECK(glass.indexAt(-smdl::INF) == shortEnd);
    CHECK(glass.indexAt(std::numeric_limits<float>::quiet_NaN()) == shortEnd);
    CHECK(glass.indexAt(5000.0f) == longEnd);
    CHECK(glass.indexAt(smdl::INF) == longEnd);
  }
}

TEST_CASE("OpticalGlass: what no glass can be") {
  using smdl::catchAndReturnError;
  using smdl::OpticalGlass;
  SUBCASE("An index below 1 is refused") {
    CHECK_ERROR(catchAndReturnError([] { (void)OpticalGlass::constant(0.9f); }),
                "at least 1");
    CHECK_ERROR(catchAndReturnError([] {
                  (void)OpticalGlass::constant(
                      std::numeric_limits<float>::quiet_NaN());
                }),
                "at least 1");
    CHECK_ERROR(
        catchAndReturnError([] { (void)OpticalGlass::abbe(1.0f, 50.0f); }),
        "greater than 1");
  }
  SUBCASE("An Abbe number or partial dispersion out of range is refused") {
    CHECK_ERROR(
        catchAndReturnError([] { (void)OpticalGlass::abbe(1.5f, 0.0f); }),
        "positive Abbe number");
    CHECK_ERROR(
        catchAndReturnError([] { (void)OpticalGlass::abbe(1.5f, -20.0f); }),
        "positive Abbe number");
    CHECK_ERROR(catchAndReturnError(
                    [] { (void)OpticalGlass::abbe(1.5f, 50.0f, 1.2f); }),
                "partial dispersion between 0 and 1");
  }
  SUBCASE("A partial dispersion far off the normal line bends the fit back") {
    // Too little turns the index over in the ultraviolet, and too much
    // turns it up in the infrared.
    CHECK_ERROR(catchAndReturnError(
                    [] { (void)OpticalGlass::abbe(1.5f, 50.0f, 0.3f); }),
                "rises with wavelength");
    CHECK_ERROR(catchAndReturnError(
                    [] { (void)OpticalGlass::abbe(1.5f, 50.0f, 0.9f); }),
                "rises with wavelength");
  }
  SUBCASE("A Sellmeier term with its pole in the domain is refused") {
    CHECK_ERROR(catchAndReturnError([] {
                  (void)OpticalGlass::sellmeier({1.0f, 0.2f, 1.0f},
                                                {0.25f, 0.02f, 100.0f});
                }),
                "pole at 500 nm");
  }
  SUBCASE("A Sellmeier index below 1 is refused") {
    CHECK_ERROR(catchAndReturnError([] {
                  (void)OpticalGlass::sellmeier({-0.5f, 0.0f, 0.0f},
                                                {0.01f, 0.0f, 0.0f});
                }),
                "below the index of air");
  }
  SUBCASE("A Sellmeier glass that does not disperse is refused") {
    CHECK_ERROR(catchAndReturnError([] {
                  (void)OpticalGlass::sellmeier({0.5f, 0.0f, 0.0f},
                                                {0.0f, 0.0f, 0.0f});
                }),
                "does not disperse");
  }
  SUBCASE("A coefficient that is not finite is refused") {
    CHECK_ERROR(catchAndReturnError([] {
                  (void)OpticalGlass::sellmeier({smdl::INF, 0.0f, 0.0f},
                                                {0.01f, 0.0f, 0.0f});
                }),
                "finite");
  }
}

TEST_CASE("OpticalGlass: the built-in catalog") {
  SUBCASE("Every SCHOTT glass reproduces the nd and Vd its maker prints") {
    // To half the last digit printed, which is 5e-6 in nd and 0.005 in Vd.
    for (const auto &printed : PRINTED) {
      const smdl::OpticalGlass &glass{catalogGlass(printed.name)};
      CHECK_MESSAGE(std::abs(glass.nd() - printed.nd) <= 5e-6f,
                    asText(printed.name), " has nd ", glass.nd(),
                    " against the printed ", printed.nd);
      CHECK_MESSAGE(std::abs(glass.abbeNumber() - printed.abbeNumber) <= 5e-3f,
                    asText(printed.name), " has Vd ", glass.abbeNumber(),
                    " against the printed ", printed.abbeNumber);
    }
  }
  SUBCASE("The crystals are Malitson's fits, whose C he prints as a "
          "wavelength") {
    constexpr double SILICA_B[3]{0.6961663, 0.4079426, 0.8974794};
    constexpr double SILICA_C[3]{0.0684043, 0.1162414, 9.896161};
    constexpr double FLUORITE_B[3]{0.5675888, 0.4710914, 3.8484723};
    constexpr double FLUORITE_C[3]{0.050263605, 0.1003909, 34.649040};
    const smdl::OpticalGlass &silica{catalogGlass("FUSED-SILICA")};
    const smdl::OpticalGlass &fluorite{catalogGlass("CAF2")};
    for (int wavelength = 300; wavelength <= 2500; wavelength += 100) {
      CHECK_NEAR(silica.indexAt(float(wavelength)),
                 malitson(SILICA_B, SILICA_C, wavelength), 1e-6);
      CHECK_NEAR(fluorite.indexAt(float(wavelength)),
                 malitson(FLUORITE_B, FLUORITE_C, wavelength), 1e-6);
    }
  }
  SUBCASE("Names match whatever their case, and an alias finds its target") {
    const smdl::OpticalGlassEntry *entry{smdl::findOpticalGlass("n-bk7")};
    REQUIRE(entry);
    CHECK(asText(entry->name) == "N-BK7");
    CHECK(smdl::findOpticalGlass("BK7") == entry);
    CHECK(smdl::findOpticalGlass("bk7") == entry);
    CHECK(asText(smdl::findOpticalGlass("Silica")->name) == "FUSED-SILICA");
    CHECK(asText(smdl::findOpticalGlass("FLUORITE")->name) == "CAF2");
  }
  SUBCASE("A name the catalog does not hold finds nothing") {
    CHECK(smdl::findOpticalGlass("N-BK8") == nullptr);
    CHECK(smdl::findOpticalGlass("N-BK7 ") == nullptr);
    CHECK(smdl::findOpticalGlass("") == nullptr);
  }
  SUBCASE("Every name finds its own entry") {
    // So that no two names match, and no alias shadows a name: a lookup is
    // never a choice between two glasses.
    for (const auto &entry : smdl::opticalGlassCatalog())
      CHECK_MESSAGE(smdl::findOpticalGlass(entry.name) == &entry,
                    asText(entry.name), " finds another entry");
  }
  SUBCASE("Every glass disperses, in order of falling Abbe number") {
    const smdl::Span<const smdl::OpticalGlassEntry> catalog{
        smdl::opticalGlassCatalog()};
    CHECK(catalog.size() == 23);
    for (size_t i = 0; i < catalog.size(); i++) {
      CHECK(catalog[i].glass.kind() == Kind::SELLMEIER);
      if (i > 0)
        CHECK_MESSAGE(catalog[i - 1].glass.abbeNumber() >
                          catalog[i].glass.abbeNumber(),
                      asText(catalog[i].name), " is out of order");
    }
  }
}

TEST_CASE("OpticalGlass: the entry points a material reaches") {
  SUBCASE("A catalog index evaluates the glass it indexes") {
    const smdl::Span<const smdl::OpticalGlassEntry> catalog{
        smdl::opticalGlassCatalog()};
    for (size_t i = 0; i < catalog.size(); i++) {
      CHECK_MESSAGE(
          smdl::smdlEvalOpticalGlassIOR(int(i), smdl::FRAUNHOFER_D_LINE) ==
              catalog[i].glass.indexAt(smdl::FRAUNHOFER_D_LINE),
          asText(catalog[i].name), " does not evaluate at its index");
      // `nd()` evaluates the same coefficients in double, so the two agree
      // to a float's precision and not to its last bit.
      CHECK_NEAR(smdl::smdlEvalOpticalGlassIOR(int(i), smdl::FRAUNHOFER_D_LINE),
                 catalog[i].glass.nd(), 1e-6f);
      CHECK(smdl::smdlEvalOpticalGlassIOR(int(i), 450.0f) ==
            catalog[i].glass.indexAt(450.0f));
    }
  }
  SUBCASE("A catalog index outside the catalog reads as vacuum") {
    CHECK(smdl::smdlEvalOpticalGlassIOR(-1, smdl::FRAUNHOFER_D_LINE) == 1.0f);
    CHECK(smdl::smdlEvalOpticalGlassIOR(int(smdl::opticalGlassCatalog().size()),
                                        smdl::FRAUNHOFER_D_LINE) == 1.0f);
  }
  SUBCASE("A catalog index holds its wavelength in the domain") {
    // The same clamp `indexAt()` applies, since it is `indexAt()`.
    const smdl::OpticalGlass &glass{catalogGlass("N-SF11")};
    const int index{int(smdl::findOpticalGlass("N-SF11") -
                        smdl::opticalGlassCatalog().begin())};
    CHECK(smdl::smdlEvalOpticalGlassIOR(index, 100.0f) ==
          glass.indexAt(smdl::OPTICAL_GLASS_WAVELENGTH_MIN));
    CHECK(smdl::smdlEvalOpticalGlassIOR(
              index, std::numeric_limits<float>::quiet_NaN()) ==
          glass.indexAt(smdl::OPTICAL_GLASS_WAVELENGTH_MIN));
    CHECK(smdl::smdlEvalOpticalGlassIOR(index, 5000.0f) ==
          glass.indexAt(smdl::OPTICAL_GLASS_WAVELENGTH_MAX));
  }
  SUBCASE("An Abbe fit is the one the factory builds") {
    // The entry point skips the walk of the domain that validates a glass,
    // and must otherwise be the same fit through the same conditions.
    const smdl::OpticalGlass stated{
        smdl::OpticalGlass::abbe(1.5168f, 64.17f, 0.5349f)};
    const smdl::OpticalGlass implied{smdl::OpticalGlass::abbe(1.5168f, 64.17f)};
    for (const float wavelength :
         {350.0f, smdl::FRAUNHOFER_G_LINE, smdl::FRAUNHOFER_F_LINE,
          smdl::FRAUNHOFER_D_LINE, smdl::FRAUNHOFER_C_LINE, 2000.0f}) {
      CAPTURE(wavelength);
      CHECK_NEAR(smdl::smdlEvalAbbeIOR(1.5168f, 64.17f, 0.5349f, wavelength),
                 stated.indexAt(wavelength), 1e-6f);
      // A partial dispersion outside 0 to 1 is the unstated one, which
      // takes Schott's normal line, as `std::nullopt` does.
      CHECK_NEAR(smdl::smdlEvalAbbeIOR(1.5168f, 64.17f, 0.0f, wavelength),
                 implied.indexAt(wavelength), 1e-6f);
      CHECK_NEAR(smdl::smdlEvalAbbeIOR(1.5168f, 64.17f, 2.0f, wavelength),
                 implied.indexAt(wavelength), 1e-6f);
    }
  }
  SUBCASE("An Abbe fit holds its wavelength in the domain") {
    const float shortEnd{smdl::smdlEvalAbbeIOR(
        1.5168f, 64.17f, 0.5349f, smdl::OPTICAL_GLASS_WAVELENGTH_MIN)};
    const float longEnd{smdl::smdlEvalAbbeIOR(
        1.5168f, 64.17f, 0.5349f, smdl::OPTICAL_GLASS_WAVELENGTH_MAX)};
    CHECK(smdl::smdlEvalAbbeIOR(1.5168f, 64.17f, 0.5349f, 100.0f) == shortEnd);
    CHECK(smdl::smdlEvalAbbeIOR(1.5168f, 64.17f, 0.5349f,
                                std::numeric_limits<float>::quiet_NaN()) ==
          shortEnd);
    CHECK(smdl::smdlEvalAbbeIOR(1.5168f, 64.17f, 0.5349f, 5000.0f) == longEnd);
  }
  SUBCASE("An Abbe fit that is no glass reads as vacuum") {
    // Where the factory throws, the entry point cannot, since it is
    // called from JIT'd code.
    CHECK(smdl::smdlEvalAbbeIOR(1.0f, 64.17f, 0.5349f,
                                smdl::FRAUNHOFER_D_LINE) == 1.0f);
    CHECK(smdl::smdlEvalAbbeIOR(1.5168f, 0.0f, 0.5349f,
                                smdl::FRAUNHOFER_D_LINE) == 1.0f);
    CHECK(smdl::smdlEvalAbbeIOR(smdl::INF, 64.17f, 0.5349f,
                                smdl::FRAUNHOFER_D_LINE) == 1.0f);
  }
  SUBCASE("An index below 1 is clamped rather than refused") {
    // A partial dispersion far off the normal line bends the fit back up
    // at an end of the domain, which the factory refuses; here the one
    // invariant a caller in JIT'd code relies on is kept instead.
    for (const float wavelength : {300.0f, 400.0f, 600.0f, 2500.0f}) {
      CAPTURE(wavelength);
      CHECK(smdl::smdlEvalAbbeIOR(1.5f, 90.0f, 0.99f, wavelength) >= 1.0f);
      CHECK(smdl::smdlEvalAbbeIOR(1.5f, 90.0f, 0.01f, wavelength) >= 1.0f);
    }
  }
}
