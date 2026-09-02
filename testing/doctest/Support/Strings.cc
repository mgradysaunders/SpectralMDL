#include "doctest.h"

#include "smdl/Support/Strings.h"

#include <array>

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
}
