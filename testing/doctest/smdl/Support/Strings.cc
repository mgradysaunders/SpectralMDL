#include "doctest.h"

#include "smdl/Support/Strings.h"

#include <array>
#include <string>

TEST_CASE("Strings") {
  SUBCASE("suggestNearest") {
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
  SUBCASE("Precise round-trips a float through decimal") {
    CHECK(smdl::concat(smdl::Precise(0.0f)) == "0");
    CHECK(smdl::concat(smdl::Precise(1.0f)) == "1");
    CHECK(smdl::concat(smdl::Precise(-2.5f)) == "-2.5");
    // The point of the nine digits: these have no short decimal form,
    // and a shorter one would not read back as the same float.
    for (float value : {0.1f, 1.0f / 3.0f, 1e-8f, 1.23456789e12f}) {
      const auto str{smdl::concat(smdl::Precise(value))};
      CHECK(std::stof(str) == value);
    }
  }
  SUBCASE("Brief writes significant digits, not decimal places") {
    // What `concat` does with a bare float, for contrast: six decimal
    // places whatever the magnitude.
    CHECK(smdl::concat(0.0f) == "0.000000");
    CHECK(smdl::concat(smdl::Brief(0.0f)) == "0");
    CHECK(smdl::concat(smdl::Brief(1.0f / 3.0f)) == "0.333333");
    CHECK(smdl::concat(smdl::Brief(1.0f / 3.0f, 1)) == "0.3");
    CHECK(smdl::concat(smdl::Brief(1.0f / 3.0f, 3)) == "0.333");
    // Six significant digits is chosen so that the scene-scale numbers
    // these mostly print stay written out rather than going exponential.
    CHECK(smdl::concat(smdl::Brief(1000.0f)) == "1000");
    CHECK(smdl::concat(smdl::Brief(20000.0f)) == "20000");
    CHECK(smdl::concat(smdl::Brief(1000.0f, 3)) == "1e+03");
    // The digit count is clamped rather than trusted, since it reaches
    // straight into a format string.
    CHECK(smdl::concat(smdl::Brief(1.0f / 3.0f, 0)) ==
          smdl::concat(smdl::Brief(1.0f / 3.0f, 1)));
    CHECK(smdl::concat(smdl::Brief(1.0f / 3.0f, 999)) ==
          smdl::concat(smdl::Brief(1.0f / 3.0f, 17)));
  }
  SUBCASE("The manipulators compose with everything else concat takes") {
    CHECK(smdl::concat("z = ", smdl::Brief(0.5f), " over ",
                       smdl::Quoted("thing"), " x",
                       3) == "z = 0.5 over 'thing' x3");
  }
}
