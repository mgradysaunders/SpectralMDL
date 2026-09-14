#include "Fixtures.h"

#include "smdl/Support/Strings.h"

#include <array>
#include <filesystem>
#include <limits>
#include <string>

TEST_CASE("Strings: the did-you-mean suggestion") {
  SUBCASE("suggestNearest picks the nearest candidate, or nothing") {
    const std::array<std::string_view, 5> candidates{
        "import", "place", "material", "camera", "sky"};
    auto suggest{[&](std::string_view name) {
      return smdl::suggestNearest(name, candidates);
    }};
    CHECK(suggest("improt") == "import");
    CHECK(suggest("plaec") == "place");
    CHECK(suggest("materiel") == "material");
    CHECK(suggest("Sky") == "sky");
    // Exact matches are distance zero, trivially suggested.
    CHECK(suggest("camera") == "camera");
    // Nothing within two edits.
    CHECK(suggest("subdivide") == "");
    CHECK(suggest("") == "");
    // Ties keep the earliest candidate.
    CHECK(smdl::suggestNearest("a", {"aa", "ab"}) == "aa");
    // The threshold is the caller's to widen or tighten. 'placement' is
    // four edits from 'place'; 'improt' is two from 'import'.
    CHECK(smdl::suggestNearest("placement", candidates) == "");
    CHECK(smdl::suggestNearest("placement", candidates, 4) == "place");
    CHECK(smdl::suggestNearest("improt", candidates, 1) == "");
  }
}

TEST_CASE("Spelling: the numbers") {
  SUBCASE("A bare float spells itself as SpellFloat does") {
    // The rule the whole log rests on: a number in a message is
    // significant digits, not the six decimal places `std::to_string`
    // writes whatever the magnitude.
    CHECK(smdl::concat(0.0f) == "0");
    CHECK(smdl::concat(0.46f) == "0.46");
    CHECK(smdl::concat(16.0f) == "16");
    CHECK(smdl::concat(1.0 / 3.0) == "0.333333");
    CHECK(smdl::concat(1e-8) == "1e-08");
    // An integer is not a float and keeps its every digit.
    CHECK(smdl::concat(1234567890) == "1234567890");
    CHECK(smdl::concat(size_t(0)) == "0");
  }
  SUBCASE("SpellFloat writes significant digits, not decimal places") {
    CHECK(smdl::concat(smdl::SpellFloat(0.0f)) == "0");
    CHECK(smdl::concat(smdl::SpellFloat(1.0f / 3.0f)) == "0.333333");
    CHECK(smdl::concat(smdl::SpellFloat(1.0f / 3.0f, 1)) == "0.3");
    CHECK(smdl::concat(smdl::SpellFloat(1.0f / 3.0f, 3)) == "0.333");
    // Six significant digits is chosen so that the scene-scale numbers
    // these mostly print stay written out rather than going exponential.
    CHECK(smdl::concat(smdl::SpellFloat(1000.0f)) == "1000");
    CHECK(smdl::concat(smdl::SpellFloat(20000.0f)) == "20000");
    CHECK(smdl::concat(smdl::SpellFloat(1000.0f, 3)) == "1e+03");
    // The digit count is clamped rather than trusted, since it reaches
    // straight into a format string.
    CHECK(smdl::concat(smdl::SpellFloat(1.0f / 3.0f, 0)) ==
          smdl::concat(smdl::SpellFloat(1.0f / 3.0f, 1)));
    CHECK(smdl::concat(smdl::SpellFloat(1.0f / 3.0f, 999)) ==
          smdl::concat(smdl::SpellFloat(1.0f / 3.0f, 17)));
  }
  SUBCASE("SpellFixed writes the same digits and never an exponent") {
    CHECK(smdl::concat(smdl::SpellFixed(1234.5678, 9)) == "1234.56780");
    CHECK(smdl::concat(smdl::SpellFixed(1234.5678, 6)) == "1234.57");
    // Where `SpellFloat` earns an exponent, this writes the places out.
    CHECK(smdl::concat(smdl::SpellFloat(1.5e-7, 3)) == "1.5e-07");
    CHECK(smdl::concat(smdl::SpellFixed(1.5e-7, 3)) == "0.000000150");
    CHECK(smdl::concat(smdl::SpellFixed(1.23456789e-12, 9)) ==
          "0.00000000000123456789");
    CHECK(smdl::concat(smdl::SpellFixed(1e9, 3)) == "1000000000");
    // The digits are significant digits, as they are for `SpellFloat`.
    CHECK(smdl::concat(smdl::SpellFixed(1.0 / 3.0, 3)) == "0.333");
    CHECK(smdl::concat(smdl::SpellFixed(123.456, 3)) == "123");
    // The places do not shrink to fit a value that ends in zeros, which
    // is what lines a column of them up.
    CHECK(smdl::concat(smdl::SpellFixed(2.0, 3)) == "2.00");
    CHECK(smdl::concat(smdl::SpellFixed(-2.5, 6)) == "-2.50000");
    CHECK(smdl::concat(smdl::SpellFixed(0.0, 3)) == "0.00");
  }
  SUBCASE("SpellExact writes the shortest spelling that reads back") {
    CHECK(smdl::concat(smdl::SpellExact(0.0f)) == "0");
    CHECK(smdl::concat(smdl::SpellExact(1.0f)) == "1");
    CHECK(smdl::concat(smdl::SpellExact(-2.5f)) == "-2.5");
    // Shortest by the string rather than by the digits, since `%g` turns
    // exponential once the exponent reaches the precision.
    CHECK(smdl::concat(smdl::SpellExact(600.0f)) == "600");
    CHECK(smdl::concat(smdl::SpellExact(2e-10)) == "2e-10");
    // A float is spelled as the float it is: nine digits of the double
    // it widens to would write this one as 0.0120000001.
    CHECK(smdl::concat(smdl::SpellExact(0.012f)) == "0.012");
    CHECK(smdl::concat(smdl::SpellExact(0.1f)) == "0.1");
    CHECK(smdl::concat(smdl::SpellExact(0.1)) == "0.1");
    // A double keeps every digit it needs, where nine would not do.
    CHECK(smdl::concat(smdl::SpellExact(1.0 / 3.0)) == "0.3333333333333333");
    for (float value : {0.1f, 1.0f / 3.0f, 1e-8f, 1.23456789e12f}) {
      const std::string str{smdl::concat(smdl::SpellExact(value))};
      CHECK(std::stof(str) == value);
    }
    for (double value : {0.1, 1.0 / 3.0, 1e-8, 1.23456789012345e12}) {
      const std::string str{smdl::concat(smdl::SpellExact(value))};
      CHECK(std::stod(str) == value);
    }
  }
  SUBCASE("SpellPercent takes the fraction, not the percentage") {
    CHECK(smdl::concat(smdl::SpellPercent(0.0)) == "0.0%");
    CHECK(smdl::concat(smdl::SpellPercent(1.0)) == "100.0%");
    CHECK(smdl::concat(smdl::SpellPercent(0.1234)) == "12.3%");
    CHECK(smdl::concat(smdl::SpellPercent(0.1234, 0)) == "12%");
    CHECK(smdl::concat(smdl::SpellPercent(0.1234, 3)) == "12.340%");
    // A ratio past one and a displacement below zero are measurements,
    // not mistakes: a frame corner may see more than its middle.
    CHECK(smdl::concat(smdl::SpellPercent(1.018)) == "101.8%");
    CHECK(smdl::concat(smdl::SpellPercent(-0.025)) == "-2.5%");
  }
  SUBCASE("A number that is not one is written as what it is") {
    const double inf{std::numeric_limits<double>::infinity()};
    const double nan{std::numeric_limits<double>::quiet_NaN()};
    for (const auto spelled :
         {smdl::concat(smdl::SpellFloat(inf)),
          smdl::concat(smdl::SpellFixed(inf)),
          smdl::concat(smdl::SpellExact(inf)), smdl::concat(inf)})
      CHECK(spelled == "inf");
    CHECK(smdl::concat(smdl::SpellPercent(inf)) == "inf%");
    CHECK(smdl::concat(smdl::SpellFloat(-inf)) == "-inf");
    for (const auto spelled : {smdl::concat(smdl::SpellFloat(nan)),
                               smdl::concat(smdl::SpellFixed(nan)),
                               smdl::concat(smdl::SpellExact(nan))})
      CHECK(spelled == "nan");
    // Negative zero is a sign the arithmetic put there, and is kept.
    CHECK(smdl::concat(smdl::SpellFloat(-0.0)) == "-0");
    CHECK(smdl::concat(smdl::SpellExact(-0.0)) == "-0");
  }
  SUBCASE("Every number spelling keeps the sign") {
    CHECK(smdl::concat(smdl::SpellFloat(-1.0 / 3.0, 3)) == "-0.333");
    CHECK(smdl::concat(smdl::SpellFixed(-1.0 / 3.0, 3)) == "-0.333");
    CHECK(smdl::concat(smdl::SpellExact(-0.25f)) == "-0.25");
    CHECK(smdl::concat(-0.5f) == "-0.5");
  }
}

TEST_CASE("Spelling: the strings, the counts and the sizes") {
  SUBCASE("SpellByteSize takes the largest binary unit that keeps the "
          "number at 1 or more") {
    CHECK(smdl::concat(smdl::SpellByteSize(0)) == "0 B");
    CHECK(smdl::concat(smdl::SpellByteSize(1023)) == "1023 B");
    CHECK(smdl::concat(smdl::SpellByteSize(1024)) == "1 KiB");
    CHECK(smdl::concat(smdl::SpellByteSize(1536)) == "1.5 KiB");
    CHECK(smdl::concat(smdl::SpellByteSize(2'800'000)) == "2.67 MiB");
    CHECK(smdl::concat(smdl::SpellByteSize(size_t(3) << 30)) == "3 GiB");
    // Where three significant digits would go exponential, the number is
    // written whole instead.
    CHECK(smdl::concat(smdl::SpellByteSize(size_t(1023) * 1024)) == "1023 KiB");
    CHECK(smdl::concat(smdl::SpellByteSize(1'023'590)) == "1000 KiB");
    // There is no unit past TiB.
    CHECK(smdl::concat(smdl::SpellByteSize(size_t(5000) << 40)) == "5000 TiB");
  }
  SUBCASE("SpellCounted is singular for exactly one") {
    CHECK(smdl::concat(smdl::SpellCounted(0, "image")) == "0 images");
    CHECK(smdl::concat(smdl::SpellCounted(1, "image")) == "1 image");
    CHECK(smdl::concat(smdl::SpellCounted(2, "image")) == "2 images");
    CHECK(smdl::concat(smdl::SpellCounted(1, "mesh", "meshes")) == "1 mesh");
    CHECK(smdl::concat(smdl::SpellCounted(3, "mesh", "meshes")) == "3 meshes");
  }
  SUBCASE("A location is written the one way every diagnostic writes one") {
    CHECK(smdl::concat(smdl::SpellLocation("<builtin ::df>", 12, 5,
                                           /*isPath=*/false)) ==
          "[<builtin ::df>:12:5]");
    // A column of zero is one nobody knows, and is left off.
    CHECK(smdl::concat(smdl::SpellLocation("<builtin ::df>", 12, 0,
                                           /*isPath=*/false)) ==
          "[<builtin ::df>:12]");
    // A path shortens as 'SpellFilePath' shortens it.
    const std::string path{
        (std::filesystem::current_path() / "main.mdl").string()};
    CHECK(smdl::concat(smdl::SpellLocation(path, 3, 1)) == "[main.mdl:3:1]");
    // Both quote the same way; what separates them is that 'SpellFilePath'
    // shortens the path and 'SpellQuoted' takes the string as it is.
    CHECK(smdl::concat(smdl::SpellFilePath(path)) == "\"main.mdl\"");
    CHECK(smdl::concat(smdl::SpellQuoted(path)) == "\"" + path + "\"");
  }
  SUBCASE("The manipulators compose with everything else concat takes") {
    CHECK(smdl::concat("z = ", smdl::SpellFloat(0.5f), " over ",
                       smdl::SpellQuoted("thing"), " x",
                       3) == "z = 0.5 over \"thing\" x3");
    CHECK(smdl::concat("has ", smdl::SpellCounted(1, "curve"), " of ",
                       smdl::SpellByteSize(2048)) == "has 1 curve of 2 KiB");
  }
}
