#include "Fixtures.h"

#include "smdl/Support/Strings.h"

#include <array>
#include <filesystem>
#include <string>

TEST_CASE("Strings: the suggestion and the float formatting") {
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
  SUBCASE("Precise round-trips a float through decimal") {
    CHECK(smdl::concat(smdl::Precise(0.0f)) == "0");
    CHECK(smdl::concat(smdl::Precise(1.0f)) == "1");
    CHECK(smdl::concat(smdl::Precise(-2.5f)) == "-2.5");
    // The point of the nine digits: these have no short decimal form,
    // and a shorter one would not read back as the same float.
    for (float value : {0.1f, 1.0f / 3.0f, 1e-8f, 1.23456789e12f}) {
      const std::string str{smdl::concat(smdl::Precise(value))};
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
  SUBCASE("Bytes takes the largest binary unit that keeps the number at 1 or "
          "more") {
    CHECK(smdl::concat(smdl::Bytes(0)) == "0 B");
    CHECK(smdl::concat(smdl::Bytes(1023)) == "1023 B");
    CHECK(smdl::concat(smdl::Bytes(1024)) == "1 KiB");
    CHECK(smdl::concat(smdl::Bytes(1536)) == "1.5 KiB");
    CHECK(smdl::concat(smdl::Bytes(2'800'000)) == "2.67 MiB");
    CHECK(smdl::concat(smdl::Bytes(size_t(3) << 30)) == "3 GiB");
    // Where three significant digits would go exponential, the number is
    // written whole instead.
    CHECK(smdl::concat(smdl::Bytes(size_t(1023) * 1024)) == "1023 KiB");
    CHECK(smdl::concat(smdl::Bytes(1'023'590)) == "1000 KiB");
    // There is no unit past TiB.
    CHECK(smdl::concat(smdl::Bytes(size_t(5000) << 40)) == "5000 TiB");
  }
  SUBCASE("Counted is singular for exactly one") {
    CHECK(smdl::concat(smdl::Counted(0, "image")) == "0 images");
    CHECK(smdl::concat(smdl::Counted(1, "image")) == "1 image");
    CHECK(smdl::concat(smdl::Counted(2, "image")) == "2 images");
    CHECK(smdl::concat(smdl::Counted(1, "mesh", "meshes")) == "1 mesh");
    CHECK(smdl::concat(smdl::Counted(3, "mesh", "meshes")) == "3 meshes");
  }
  SUBCASE("A location is written the one way every diagnostic writes one") {
    CHECK(smdl::concat(smdl::LocationMarkup("<builtin ::df>", 12, 5,
                                            /*isPath=*/false)) ==
          "[<builtin ::df>:12:5]");
    // A column of zero is one nobody knows, and is left off.
    CHECK(smdl::concat(smdl::LocationMarkup("<builtin ::df>", 12, 0,
                                            /*isPath=*/false)) ==
          "[<builtin ::df>:12]");
    // A path shortens as 'QuotedPath' shortens it.
    const std::string path{
        (std::filesystem::current_path() / "main.mdl").string()};
    CHECK(smdl::concat(smdl::LocationMarkup(path, 3, 1)) == "[main.mdl:3:1]");
    // Both quote the same way; what separates them is that 'QuotedPath'
    // shortens the path and 'Quoted' takes the string as it is.
    CHECK(smdl::concat(smdl::QuotedPath(path)) == "\"main.mdl\"");
    CHECK(smdl::concat(smdl::Quoted(path)) == "\"" + path + "\"");
  }
  SUBCASE("The manipulators compose with everything else concat takes") {
    CHECK(smdl::concat("z = ", smdl::Brief(0.5f), " over ",
                       smdl::Quoted("thing"), " x",
                       3) == "z = 0.5 over \"thing\" x3");
    CHECK(smdl::concat("has ", smdl::Counted(1, "curve"), " of ",
                       smdl::Bytes(2048)) == "has 1 curve of 2 KiB");
  }
}
