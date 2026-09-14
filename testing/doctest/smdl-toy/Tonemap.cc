#include "Fixtures.h"

#include <string>
#include <string_view>

#include "Tonemap.h"

// The display transform itself needs a spectral film and the JIT behind
// it; what is here is the spec parser, which is the one place the stage
// names are spelled and the only part a misspelling has to fail in
// before the render is paid for.

namespace {

// The message the parse refuses `spec` with, empty if it accepts it.
[[nodiscard]] std::string refusalOf(std::string_view spec) {
  try {
    parseTonemapOptions(spec);
  } catch (const smdl::Error &error) {
    return error.message;
  }
  return std::string();
}

} // namespace

TEST_CASE("parseTonemapOptions: the stages and their parameters") {
  SUBCASE("Each display curve names itself") {
    CHECK(parseTonemapOptions("gamma").curve == TonemapCurve::GAMMA);
    CHECK(parseTonemapOptions("log").curve == TonemapCurve::LOG);
    CHECK(parseTonemapOptions("filmic").curve == TonemapCurve::FILMIC);
  }
  SUBCASE("A stage left out keeps its default") {
    const TonemapOptions options{parseTonemapOptions("gamma")};
    CHECK(options.isNight == false);
    CHECK(options.useFusion == false);
    CHECK(options.logDecades == 4.0f);
    CHECK(options.fusionStrength == 0.75f);
    CHECK(options.fusionClamp == 3.0f);
    CHECK(options.fusionSpan == 0.0f);
  }
  SUBCASE("The exposure is left for the caller to fill") {
    // It comes from -exposure rather than from the spec, so the parse
    // must not touch it.
    CHECK(parseTonemapOptions("filmic").exposure == 1.0f);
    CHECK(parseTonemapOptions("night+fusion:1,8,4").exposure == 1.0f);
  }
  SUBCASE("log takes the decades below white") {
    CHECK(parseTonemapOptions("log:6").logDecades == 6.0f);
    CHECK(parseTonemapOptions("log:0.5").logDecades == 0.5f);
  }
  SUBCASE("fusion takes up to three positional parameters") {
    const TonemapOptions one{parseTonemapOptions("fusion:0.5")};
    CHECK(one.useFusion == true);
    CHECK(one.fusionStrength == 0.5f);
    CHECK(one.fusionClamp == 3.0f);
    CHECK(one.fusionSpan == 0.0f);
    const TonemapOptions two{parseTonemapOptions("fusion:0.5,2")};
    CHECK(two.fusionStrength == 0.5f);
    CHECK(two.fusionClamp == 2.0f);
    CHECK(two.fusionSpan == 0.0f);
    const TonemapOptions three{parseTonemapOptions("fusion:0.5,2,8")};
    CHECK(three.fusionStrength == 0.5f);
    CHECK(three.fusionClamp == 2.0f);
    CHECK(three.fusionSpan == 8.0f);
    // Bare, it turns on and keeps every default.
    CHECK(parseTonemapOptions("fusion").useFusion == true);
    CHECK(parseTonemapOptions("fusion").fusionStrength == 0.75f);
  }
  SUBCASE("night is a flag and takes none") {
    CHECK(parseTonemapOptions("night").isNight == true);
    // On its own it leaves the curve at the default.
    CHECK(parseTonemapOptions("night").curve == TonemapCurve::GAMMA);
  }
  SUBCASE("The stages compose in any order") {
    for (const char *spec :
         {"night+filmic+fusion:0.5,2", "filmic+night+fusion:0.5,2",
          "fusion:0.5,2+filmic+night", "night+fusion:0.5,2+filmic"}) {
      CAPTURE(std::string(spec));
      const TonemapOptions options{parseTonemapOptions(spec)};
      CHECK(options.isNight == true);
      CHECK(options.useFusion == true);
      CHECK(options.curve == TonemapCurve::FILMIC);
      CHECK(options.fusionStrength == 0.5f);
      CHECK(options.fusionClamp == 2.0f);
    }
  }
}

TEST_CASE("parseTonemapOptions: what it refuses") {
  SUBCASE("A stage it does not know") {
    CHECK_CONTAINS(refusalOf("reinhard"), "Unknown -tonemap stage");
    CHECK_CONTAINS(refusalOf("reinhard"), "\"reinhard\"");
    // And it says what it does know.
    CHECK_CONTAINS(refusalOf("reinhard"), "'filmic'");
  }
  SUBCASE("An empty stage name, however it is written") {
    CHECK_CONTAINS(refusalOf(""), "Expected a -tonemap stage name");
    CHECK_CONTAINS(refusalOf("filmic+"), "Expected a -tonemap stage name");
    CHECK_CONTAINS(refusalOf("+filmic"), "Expected a -tonemap stage name");
    CHECK_CONTAINS(refusalOf(":4"), "Expected a -tonemap stage name");
  }
  SUBCASE("The same stage twice") {
    CHECK_CONTAINS(refusalOf("night+night"), "at most one 'night'");
    CHECK_CONTAINS(refusalOf("fusion+fusion"), "at most one 'fusion'");
  }
  SUBCASE("Two display curves") {
    CHECK_CONTAINS(refusalOf("gamma+filmic"), "at most one display curve");
    CHECK_CONTAINS(refusalOf("log:6+log:2"), "at most one display curve");
  }
  SUBCASE("Parameters on a stage that takes none") {
    CHECK_CONTAINS(refusalOf("night:1"), "no parameters for -tonemap 'night'");
    CHECK_CONTAINS(refusalOf("gamma:2"),
                   "no parameters for -tonemap \"gamma\"");
    CHECK_CONTAINS(refusalOf("filmic:2"),
                   "no parameters for -tonemap \"filmic\"");
  }
  SUBCASE("A parameter that is not a finite number") {
    CHECK_CONTAINS(refusalOf("log:wide"), "Cannot parse -tonemap \"log\"");
    CHECK_CONTAINS(refusalOf("log:6x"), "Cannot parse -tonemap \"log\"");
    CHECK_CONTAINS(refusalOf("log:"), "Cannot parse -tonemap \"log\"");
    CHECK_CONTAINS(refusalOf("fusion:0.5,"),
                   "Cannot parse -tonemap \"fusion\"");
    CHECK_CONTAINS(refusalOf("log:inf"), "Cannot parse -tonemap \"log\"");
    CHECK_CONTAINS(refusalOf("log:nan"), "Cannot parse -tonemap \"log\"");
  }
  SUBCASE("More parameters than a stage has") {
    CHECK_CONTAINS(refusalOf("fusion:0.5,2,8,1"), "at most 3 parameters");
    CHECK_CONTAINS(refusalOf("log:6,2"), "at most 1 parameter");
  }
  SUBCASE("A parameter outside its range") {
    CHECK_CONTAINS(refusalOf("log:0"), "'log' DECADES to be positive");
    CHECK_CONTAINS(refusalOf("log:-1"), "'log' DECADES to be positive");
    CHECK_CONTAINS(refusalOf("fusion:1.5"), "STRENGTH between 0 and 1");
    CHECK_CONTAINS(refusalOf("fusion:-0.5"), "STRENGTH between 0 and 1");
    CHECK_CONTAINS(refusalOf("fusion:0.5,0"), "'fusion' CLAMP to be positive");
    CHECK_CONTAINS(refusalOf("fusion:0.5,2,-1"),
                   "'fusion' SPAN to be nonnegative");
  }
}
