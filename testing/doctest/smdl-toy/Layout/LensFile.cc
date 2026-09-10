#include "Fixtures.h"

#include <string>

#include "Layout/LensFile.h"

namespace {
// Parse from memory and require no errors.
LensDocument parseOK(LayoutDiagnostics &diags, std::string text) {
  const auto &source{diags.addSource("test.lens", std::move(text))};
  auto document{parseLens(diags, source)};
  if (diags.hasErrors()) MESSAGE(diags.renderAll(false));
  REQUIRE(!diags.hasErrors());
  return document;
}

// Parse from memory, require exactly one error, and hand back its
// message so the caller can say which one it expected.
std::string parseError(LayoutDiagnostics &diags, std::string text) {
  const auto &source{diags.addSource("test.lens", std::move(text))};
  (void)parseLens(diags, source);
  REQUIRE(diags.errorCount() == 1);
  return diags.all().front().message;
}

// The shortest thing that is a lens: one refracting surface and the
// stop. Enough to hang a single-surface test on, and the base every
// malformed case below perturbs.
constexpr const char *SINGLET = "lens {\n"
                                "  surface { radius 50 thickness 4 ior 1.5 "
                                "diameter 20 }\n"
                                "  stop { thickness 10 diameter 12 }\n"
                                "}\n";
} // namespace

TEST_CASE("LensFile: the shape of a prescription") {
  LayoutDiagnostics diags{};
  SUBCASE("It reads surfaces in file order, front first") {
    const auto document{parseOK(diags, SINGLET)};
    const auto &lens{document.lens};
    REQUIRE(lens.surfaces.size() == 2);
    CHECK(lens.surfaces[0].radius == doctest::Approx(50.0f));
    CHECK(lens.surfaces[0].thickness == doctest::Approx(4.0f));
    CHECK(lens.surfaces[0].ior == doctest::Approx(1.5f));
    CHECK(lens.surfaces[0].diameter == doctest::Approx(20.0f));
    CHECK(!lens.surfaces[0].isStop);
    CHECK(lens.surfaces[1].isStop);
  }
  SUBCASE("The stop is a surface, and knows its place among them") {
    const auto document{parseOK(diags, SINGLET)};
    CHECK(document.lens.stopIndex() == 1);
  }
  SUBCASE("An omitted 'ior' is air") {
    const auto document{parseOK(diags, "lens {\n"
                                       "  surface { radius 50 diameter 20 }\n"
                                       "  stop { thickness 10 diameter 12 }\n"
                                       "}\n")};
    CHECK(document.lens.surfaces[0].ior == doctest::Approx(1.0f));
  }
  SUBCASE("An omitted 'radius' is flat, and an omitted 'thickness' is zero") {
    const auto document{parseOK(diags, "lens {\n"
                                       "  stop { diameter 12 }\n"
                                       "}\n")};
    REQUIRE(document.lens.surfaces.size() == 1);
    CHECK(document.lens.surfaces[0].radius == 0.0f);
    CHECK(document.lens.surfaces[0].thickness == 0.0f);
  }
  SUBCASE("A name is free text, and is optional") {
    const auto document{
        parseOK(diags, std::string("lens { name \"Sonnar 50mm f/1.5\" ") +
                           "stop { diameter 12 } }\n")};
    CHECK(document.lens.name == "Sonnar 50mm f/1.5");
    CHECK(parseOK(diags, SINGLET).lens.name.empty());
  }
  SUBCASE("A radius is signed, positive toward the film") {
    const auto document{parseOK(diags, "lens {\n"
                                       "  surface { radius -39.73 "
                                       "diameter 20 }\n"
                                       "  stop { diameter 12 }\n"
                                       "}\n")};
    CHECK(document.lens.surfaces[0].radius == doctest::Approx(-39.73f));
  }
}

TEST_CASE("LensFile: the aperture stop") {
  LayoutDiagnostics diags{};
  SUBCASE("A lens without one is an error that says where to put it") {
    const auto &source{diags.addSource("test.lens",
                                       "lens {\n"
                                       "  surface { radius 50 diameter 20 }\n"
                                       "}\n")};
    (void)parseLens(diags, source);
    REQUIRE(diags.errorCount() == 1);
    CHECK_CONTAINS(diags.all().front().message, "exactly one aperture stop");
    REQUIRE(diags.all().front().notes.size() == 1);
    CHECK_CONTAINS(diags.all().front().notes.front().message,
                   "a patent usually leaves the diaphragm out");
  }
  SUBCASE("A second one is an error pointing at the first") {
    const auto &source{diags.addSource("test.lens",
                                       "lens {\n"
                                       "  stop { thickness 4 diameter 12 }\n"
                                       "  stop { thickness 4 diameter 12 }\n"
                                       "}\n")};
    (void)parseLens(diags, source);
    REQUIRE(diags.errorCount() == 1);
    CHECK_CONTAINS(diags.all().front().message, "this is the second 'stop'");
    REQUIRE(diags.all().front().notes.size() == 1);
    CHECK_CONTAINS(diags.all().front().notes.front().message,
                   "the first one is here");
  }
  SUBCASE("It has no shape of its own to state") {
    CHECK_CONTAINS(parseError(diags, "lens { stop { radius 50 diameter 12 } }"),
                   "'radius' has no meaning on the aperture stop");
  }
  SUBCASE("It has no index of its own, being in the space before it") {
    CHECK_CONTAINS(parseError(diags, "lens { stop { ior 1.5 diameter 12 } }"),
                   "'ior' has no meaning on the aperture stop");
  }
}

TEST_CASE("LensFile: the values a surface may take") {
  LayoutDiagnostics diags{};
  SUBCASE("Every surface needs a clear aperture") {
    CHECK_CONTAINS(parseError(diags, "lens { surface { radius 50 } "
                                     "stop { diameter 12 } }"),
                   "expected 'diameter'");
  }
  SUBCASE("A diameter must be positive") {
    CHECK_CONTAINS(parseError(diags, "lens { stop { diameter 0 } }"),
                   "positive number for 'diameter'");
  }
  SUBCASE("An index below 1 is an error") {
    CHECK_CONTAINS(parseError(diags, "lens { surface { ior 0.5 diameter 20 } "
                                     "stop { diameter 12 } }"),
                   "expected 'ior' to be at least 1");
  }
  SUBCASE("A thickness runs forward, so it is not negative") {
    CHECK_CONTAINS(parseError(diags, "lens { stop { thickness -1 "
                                     "diameter 12 } }"),
                   "nonnegative number for 'thickness'");
  }
  SUBCASE("A non-finite number is an error wherever it appears") {
    CHECK_CONTAINS(parseError(diags, "lens { surface { radius inf "
                                     "diameter 20 } stop { diameter 12 } }"),
                   "finite number for 'radius'");
  }
  SUBCASE("An unknown surface setting names the ones that exist") {
    CHECK_CONTAINS(parseError(diags, "lens { surface { curvature 50 "
                                     "diameter 20 } stop { diameter 12 } }"),
                   "unknown surface setting 'curvature'");
  }
  SUBCASE("The stop's message names only the two settings it takes") {
    CHECK_CONTAINS(parseError(diags, "lens { stop { blades 6 diameter 12 } }"),
                   "expected thickness or diameter");
  }
}

TEST_CASE("LensFile: conic and aspheric surfaces") {
  LayoutDiagnostics diags{};
  SUBCASE("A conic constant parses and defaults to a sphere") {
    const auto document{parseOK(diags, "lens {\n"
                                       "  surface { radius 21.48 conic -0.45 "
                                       "diameter 12 }\n"
                                       "  stop { diameter 10 }\n"
                                       "}\n")};
    CHECK(document.lens.surfaces[0].conic == doctest::Approx(-0.45f));
    CHECK(document.lens.surfaces[1].conic == 0.0f);
  }
  SUBCASE("Coefficients that are all zero parse, as printed tables have them") {
    const auto document{parseOK(diags, "lens {\n"
                                       "  surface { radius 21.48 "
                                       "aspheric 0 0 0 diameter 12 }\n"
                                       "  stop { diameter 10 }\n"
                                       "}\n")};
    CHECK(document.lens.surfaces[0].aspheric.size() == 3);
  }
  SUBCASE("Coefficients are kept in the order written, the r^4 term first") {
    const auto document{parseOK(diags, "lens {\n"
                                       "  surface { radius 21.48 "
                                       "aspheric 0 1.2e-5 -3e-9 diameter 12 }\n"
                                       "  stop { diameter 10 }\n"
                                       "}\n")};
    REQUIRE(document.lens.surfaces[0].aspheric.size() == 3);
    CHECK(document.lens.surfaces[0].aspheric[0] == 0.0f);
    CHECK(document.lens.surfaces[0].aspheric[1] == doctest::Approx(1.2e-5f));
    CHECK(document.lens.surfaces[0].aspheric[2] == doctest::Approx(-3e-9f));
  }
  SUBCASE("An empty coefficient list is an error") {
    CHECK_CONTAINS(parseError(diags, "lens { surface { aspheric "
                                     "diameter 12 } stop { diameter 10 } }"),
                   "at least one number after 'aspheric'");
  }
  SUBCASE("A runaway list is an error rather than a resize") {
    CHECK_CONTAINS(parseError(diags, "lens { surface { aspheric "
                                     "0 0 0 0 0 0 0 0 0 diameter 12 } "
                                     "stop { diameter 10 } }"),
                   "at most 8 coefficients");
  }
}

TEST_CASE("LensFile: the file holds one lens and nothing else") {
  LayoutDiagnostics diags{};
  SUBCASE("A second 'lens' block does not merge, since surfaces are ordered") {
    const auto &source{diags.addSource(
        "test.lens", std::string(SINGLET) + "lens { stop { diameter 12 } }\n")};
    (void)parseLens(diags, source);
    REQUIRE(diags.errorCount() == 1);
    CHECK_CONTAINS(diags.all().front().message, "the second 'lens' block");
    REQUIRE(diags.all().front().notes.size() == 1);
    CHECK_CONTAINS(diags.all().front().notes.front().message,
                   "the first one is here");
  }
  SUBCASE("A stray surface says where a surface belongs") {
    const auto &source{
        diags.addSource("test.lens", "surface { radius 50 diameter 20 }\n")};
    (void)parseLens(diags, source);
    REQUIRE(diags.errorCount() == 1);
    CHECK_CONTAINS(diags.all().front().message, "unknown directive 'surface'");
    REQUIRE(diags.all().front().notes.size() == 1);
    CHECK_CONTAINS(diags.all().front().notes.front().message,
                   "belongs inside the 'lens' block");
  }
  SUBCASE("A sensor is the camera's, and the note says so") {
    const auto &source{diags.addSource("test.lens", "sensor 36 24\n")};
    (void)parseLens(diags, source);
    REQUIRE(diags.errorCount() == 1);
    REQUIRE(diags.all().front().notes.size() == 1);
    CHECK_CONTAINS(diags.all().front().notes.front().message,
                   "belongs in the '.camera' file");
  }
  SUBCASE("An unknown lens setting names the ones that exist") {
    CHECK_CONTAINS(parseError(diags, "lens { element { diameter 12 } }"),
                   "unknown lens setting 'element'");
  }
}

TEST_CASE("LensFile: a transcribed prescription") {
  LayoutDiagnostics diags{};
  // The double Gauss pbrt distributes, from US 2,673,491 (Tronnier) by
  // way of Modern Lens Design p.312, scaled to 50 mm. Transcribing a
  // published table is the only thing anyone will ever do with this
  // format, so the test is that a whole one survives it.
  const auto document{
      parseOK(diags, "lens {\n"
                     "  name \"Double Gauss 50mm f/2\"\n"
                     "  surface { radius   29.475 thickness 3.76  ior 1.67  "
                     "diameter 25.2 }\n"
                     "  surface { radius   84.83  thickness 0.12            "
                     "diameter 25.2 }\n"
                     "  surface { radius   19.275 thickness 4.025 ior 1.67  "
                     "diameter 23.0 }\n"
                     "  surface { radius   40.77  thickness 3.275 ior 1.699 "
                     "diameter 23.0 }\n"
                     "  surface { radius   12.75  thickness 5.705           "
                     "diameter 18.0 }\n"
                     "  stop    {                 thickness 4.5             "
                     "diameter 17.1 }\n"
                     "  surface { radius  -14.495 thickness 1.18  ior 1.603 "
                     "diameter 17.0 }\n"
                     "  surface { radius   40.77  thickness 6.065 ior 1.658 "
                     "diameter 20.0 }\n"
                     "  surface { radius  -20.385 thickness 0.19            "
                     "diameter 20.0 }\n"
                     "  surface { radius  437.065 thickness 3.22  ior 1.717 "
                     "diameter 20.0 }\n"
                     "  surface { radius  -39.73                            "
                     "diameter 20.0 }\n"
                     "}\n")};
  const auto &lens{document.lens};
  SUBCASE("Every surface arrives, in order, with the stop among them") {
    REQUIRE(lens.surfaces.size() == 11);
    CHECK(lens.name == "Double Gauss 50mm f/2");
    CHECK(lens.stopIndex() == 5);
    CHECK(lens.surfaces.front().radius == doctest::Approx(29.475f));
    CHECK(lens.surfaces.back().radius == doctest::Approx(-39.73f));
  }
  SUBCASE("An air gap carries the default index and a glass carries its own") {
    CHECK(lens.surfaces[0].ior == doctest::Approx(1.67f));
    CHECK(lens.surfaces[1].ior == doctest::Approx(1.0f));
    CHECK(lens.surfaces[3].ior == doctest::Approx(1.699f));
  }
  SUBCASE("The last surface may leave its thickness to the focus solve") {
    CHECK(lens.surfaces.back().thickness == 0.0f);
  }
}
