#include "Fixtures.h"

#include <array>
#include <string>
#include <string_view>

#include "smdl/RenderUtil/OpticalGlass.h"
#include "smdl/Support/Strings.h"

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

// The singlet with the space behind its first surface named `glass`
// rather than stated by an index.
std::string singletOf(std::string_view glass) {
  return smdl::concat("lens {\n"
                      "  surface { radius 50 thickness 4 glass ",
                      glass,
                      " diameter 20 }\n"
                      "  stop { thickness 10 diameter 12 }\n"
                      "}\n");
}

// The singlet in a glass the file defines, `CROWN` followed by
// `definition`, below the surface that names it. The definition is on
// the fourth line.
std::string definingCrown(std::string_view definition) {
  return smdl::concat("lens {\n"
                      "  surface { radius 50 thickness 4 glass CROWN "
                      "diameter 20 }\n"
                      "  stop { thickness 10 diameter 12 }\n"
                      "  glass CROWN ",
                      definition, "\n}\n");
}

// N-BK7's coefficients, in the rows SCHOTT's datasheet prints them in.
constexpr const char *BK7_SELLMEIER =
    "sellmeier { b 1.03961212 0.231792344 1.01046945 "
    "c 0.00600069867 0.0200179144 103.560653 }";

// Where two glasses are compared: the ends of the visible, and the d line.
constexpr std::array<float, 3> WAVELENGTHS{400.0f, smdl::FRAUNHOFER_D_LINE,
                                           700.0f};
} // namespace

TEST_CASE("LensFile: the shape of a prescription") {
  LayoutDiagnostics diags{};
  SUBCASE("It reads surfaces in file order, front first") {
    const auto document{parseOK(diags, SINGLET)};
    const auto &lens{document.lens};
    REQUIRE(lens.surfaces.size() == 2);
    CHECK(lens.surfaces[0].radius == doctest::Approx(50.0f));
    CHECK(lens.surfaces[0].thickness == doctest::Approx(4.0f));
    CHECK(lens.surfaces[0].medium.nd() == doctest::Approx(1.5f));
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
    const auto &surface{document.lens.surfaces[0]};
    CHECK(surface.medium.nd() == 1.0f);
    CHECK(!surface.medium.isDispersive());
  }
  SUBCASE("An 'ior' is one index at every wavelength, and names no glass") {
    const auto document{parseOK(diags, SINGLET)};
    const auto &surface{document.lens.surfaces[0]};
    CHECK(!surface.medium.isDispersive());
    CHECK(surface.glassName.empty());
    for (const auto wavelength : WAVELENGTHS)
      CHECK(surface.medium.indexAt(wavelength) == 1.5f);
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
  SUBCASE("It holds at most 64 surfaces, the stop among them") {
    auto text{std::string("lens {\n  stop { diameter 12 }\n")};
    for (size_t i = 1; i < LENS_MAX_SURFACES; i++)
      text += "  surface { diameter 12 }\n";
    CHECK(parseOK(diags, text + "}\n").lens.surfaces.size() ==
          LENS_MAX_SURFACES);
    LayoutDiagnostics more{};
    CHECK_CONTAINS(parseError(more, text + "  surface { diameter 12 }\n}\n"),
                   "at most 64 surfaces");
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
  SUBCASE("It names no glass either, for the same reason") {
    CHECK_CONTAINS(
        parseError(diags, "lens { stop { glass N-BK7 diameter 12 } }"),
        "'glass' has no meaning on the aperture stop");
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

TEST_CASE("LensFile: naming a glass") {
  LayoutDiagnostics diags{};
  SUBCASE("A surface carries the built-in glass it names, by value") {
    const auto document{parseOK(diags, singletOf("N-BK7"))};
    const auto &surface{document.lens.surfaces[0]};
    const auto *entry{smdl::findOpticalGlass("N-BK7")};
    REQUIRE(entry != nullptr);
    CHECK(surface.glassName == "N-BK7");
    CHECK(surface.medium.kind() == smdl::OpticalGlass::Kind::SELLMEIER);
    for (const auto wavelength : WAVELENGTHS)
      CHECK(hasSameBits(surface.medium.indexAt(wavelength),
                        entry->glass.indexAt(wavelength)));
  }
  SUBCASE("A glass that disperses makes the prescription disperse, and an "
          "index alone does not") {
    CHECK(parseOK(diags, singletOf("N-BK7")).lens.isDispersive());
    CHECK(!parseOK(diags, SINGLET).lens.isDispersive());
  }
  SUBCASE("A name matches whatever its case or alias, and reads as the "
          "catalog spells it") {
    CHECK(parseOK(diags, singletOf("n-bk7")).lens.surfaces[0].glassName ==
          "N-BK7");
    CHECK(parseOK(diags, singletOf("BK7")).lens.surfaces[0].glassName ==
          "N-BK7");
    CHECK(parseOK(diags, singletOf("Fluorite")).lens.surfaces[0].glassName ==
          "CAF2");
  }
  SUBCASE("An unknown name suggests the nearest, and lists the built-in "
          "glasses") {
    const auto &source{diags.addSource("test.lens", singletOf("N-BK8"))};
    (void)parseLens(diags, source);
    REQUIRE(diags.errorCount() == 1);
    const auto &error{diags.all().front()};
    CHECK_CONTAINS(error.message, "unknown glass 'N-BK8'");
    REQUIRE(error.notes.size() == 2);
    CHECK_CONTAINS(error.notes[0].message, "did you mean 'N-BK7'?");
    CHECK_CONTAINS(error.notes[1].message,
                   "the built-in glasses are CAF2, N-FK51A,");
  }
  SUBCASE("The nearest may be the file's own glass, as the file spells it") {
    const auto &source{diags.addSource(
        "test.lens", "lens {\n"
                     "  surface { radius 50 thickness 4 glass CRWN_A "
                     "diameter 20 }\n"
                     "  stop { thickness 10 diameter 12 }\n"
                     "  glass Crown_A { ior 1.62 abbe 60.3 }\n"
                     "}\n")};
    (void)parseLens(diags, source);
    REQUIRE(diags.errorCount() == 1);
    REQUIRE(!diags.all().front().notes.empty());
    CHECK_CONTAINS(diags.all().front().notes.front().message,
                   "did you mean 'Crown_A'?");
  }
  SUBCASE("'glass' beside 'ior' is an error, since a glass states its own "
          "index") {
    const auto &source{diags.addSource("test.lens",
                                       "lens { surface { ior 1.5 glass N-BK7 "
                                       "diameter 20 } stop { diameter 12 } }")};
    (void)parseLens(diags, source);
    REQUIRE(diags.errorCount() == 1);
    CHECK_CONTAINS(diags.all().front().message, "a glass states its own index");
    REQUIRE(diags.all().front().notes.size() == 1);
    CHECK_CONTAINS(diags.all().front().notes.front().message, "'ior' is here");
  }
  SUBCASE("A name is a letter, then letters, digits, '-', and '_'") {
    CHECK_CONTAINS(parseError(diags, singletOf("7X")), "expected a glass name");
  }
  SUBCASE("A definition on a surface says where definitions go") {
    const auto &source{diags.addSource(
        "test.lens", "lens { surface { glass { ior 1.5 abbe 60 } "
                     "diameter 20 } stop { diameter 12 } }")};
    (void)parseLens(diags, source);
    REQUIRE(diags.errorCount() == 1);
    REQUIRE(diags.all().front().notes.size() == 1);
    CHECK_CONTAINS(diags.all().front().notes.front().message,
                   "in the 'lens' block");
  }
}

TEST_CASE("LensFile: defining a glass") {
  LayoutDiagnostics diags{};
  // A definition that must be refused, alone in its own diagnostics.
  const auto refusalOf{[](std::string_view definition) {
    LayoutDiagnostics each{};
    return parseError(each, definingCrown(definition));
  }};
  SUBCASE("A definition may follow the surface that names it") {
    const auto document{
        parseOK(diags, definingCrown("{ ior 1.62 abbe 60.3 }"))};
    CHECK(diags.warningCount() == 0);
    const auto &surface{document.lens.surfaces[0]};
    CHECK(surface.glassName == "CROWN");
    CHECK(surface.medium.kind() == smdl::OpticalGlass::Kind::ABBE);
    CHECK_NEAR(surface.medium.nd(), 1.62, 1e-6);
  }
  SUBCASE("'ior' and 'abbe' fit the normal line") {
    const auto document{
        parseOK(diags, definingCrown("{ ior 1.62 abbe 60.3 }"))};
    const auto expected{smdl::OpticalGlass::abbe(1.62f, 60.3f)};
    for (const auto wavelength : WAVELENGTHS)
      CHECK(hasSameBits(document.lens.surfaces[0].medium.indexAt(wavelength),
                        expected.indexAt(wavelength)));
  }
  SUBCASE("A stated partial dispersion is kept") {
    const auto document{parseOK(
        diags,
        definingCrown("{ ior 1.497 abbe 81.6 partial_dispersion 0.5377 }"))};
    CHECK_NEAR(document.lens.surfaces[0].medium.partialDispersion(), 0.5377,
               1e-4);
  }
  SUBCASE("A Sellmeier definition is the maker's formula") {
    const auto document{
        parseOK(diags, definingCrown(smdl::concat("{ ", BK7_SELLMEIER, " }")))};
    const auto &medium{document.lens.surfaces[0].medium};
    const auto &maker{smdl::findOpticalGlass("N-BK7")->glass};
    CHECK(medium.kind() == smdl::OpticalGlass::Kind::SELLMEIER);
    for (const auto wavelength : WAVELENGTHS)
      CHECK(hasSameBits(medium.indexAt(wavelength), maker.indexAt(wavelength)));
  }
  SUBCASE("Printed values beside the coefficients pass when they agree") {
    (void)parseOK(diags, definingCrown(smdl::concat(
                             "{ ", BK7_SELLMEIER, " ior 1.5168 abbe 64.17 }")));
    CHECK(diags.warningCount() == 0);
  }
  SUBCASE("A printed value the coefficients disagree with is a warning") {
    (void)parseOK(diags, definingCrown(smdl::concat(
                             "{ ", BK7_SELLMEIER, " ior 1.5268 abbe 64.17 }")));
    REQUIRE(diags.warningCount() == 1);
    CHECK_CONTAINS(diags.all().front().message,
                   "'ior' 1.5268 disagrees with the Sellmeier coefficients");
    CHECK_CONTAINS(diags.all().front().message, "which give 1.5168");
    LayoutDiagnostics abbe{};
    (void)parseOK(abbe, definingCrown(smdl::concat("{ ", BK7_SELLMEIER,
                                                   " ior 1.5168 abbe 64.5 }")));
    REQUIRE(abbe.warningCount() == 1);
    CHECK_CONTAINS(abbe.all().front().message, "'abbe' 64.5 disagrees");
  }
  SUBCASE("A use matches its definition whatever the case, and reads as the "
          "definition spells it") {
    const auto document{
        parseOK(diags, "lens {\n"
                       "  glass Crown_A { ior 1.62 abbe 60.3 }\n"
                       "  surface { radius 50 thickness 4 glass CROWN_A "
                       "diameter 20 }\n"
                       "  stop { thickness 10 diameter 12 }\n"
                       "}\n")};
    CHECK(document.lens.surfaces[0].glassName == "Crown_A");
  }
  SUBCASE("A built-in glass cannot be defined again, under any spelling") {
    for (const auto *name : {"N-BK7", "n-bk7", "BK7"}) {
      LayoutDiagnostics each{};
      const auto &source{each.addSource(
          "test.lens", smdl::concat("lens { stop { diameter 12 } glass ", name,
                                    " { ior 1.5 abbe 60 } }"))};
      (void)parseLens(each, source);
      REQUIRE(each.errorCount() == 1);
      CHECK_CONTAINS(each.all().front().message, "built-in glass");
      REQUIRE(each.all().front().notes.size() == 1);
      CHECK_CONTAINS(each.all().front().notes.front().message,
                     "shared by every lens");
    }
  }
  SUBCASE("A second definition of one name is an error pointing at the "
          "first") {
    const auto &source{diags.addSource("test.lens",
                                       "lens {\n"
                                       "  stop { diameter 12 }\n"
                                       "  glass A { ior 1.5 abbe 60 }\n"
                                       "  glass a { ior 1.6 abbe 50 }\n"
                                       "}\n")};
    (void)parseLens(diags, source);
    REQUIRE(diags.errorCount() == 1);
    CHECK_CONTAINS(diags.all().front().message, "glass 'a' is defined twice");
    REQUIRE(diags.all().front().notes.size() == 1);
    CHECK_CONTAINS(diags.all().front().notes.front().message,
                   "the first definition is here");
  }
  SUBCASE("A definition is one of the three methods and nothing else") {
    CHECK_CONTAINS(refusalOf("{ abbe 60 }"), "'abbe' needs 'ior' beside it");
    CHECK_CONTAINS(refusalOf("{ ior 1.5 partial_dispersion 0.53 }"),
                   "'partial_dispersion' needs 'ior' and 'abbe'");
    CHECK_CONTAINS(refusalOf(smdl::concat("{ ", BK7_SELLMEIER,
                                          " partial_dispersion 0.53 }")),
                   "no place beside 'sellmeier'");
    CHECK_CONTAINS(refusalOf("{ }"), "needs 'ior' and 'abbe', or 'sellmeier'");
    CHECK_CONTAINS(refusalOf("{ sellmeier { b 1 0 0 } }"), "both rows");
    CHECK_CONTAINS(refusalOf("{ refractive_index 1.5 }"),
                   "unknown glass setting 'refractive_index'");
  }
  SUBCASE("An index alone is refused, with a note that it belongs on the "
          "surface") {
    const auto &source{
        diags.addSource("test.lens", definingCrown("{ ior 1.5 }"))};
    (void)parseLens(diags, source);
    REQUIRE(diags.errorCount() == 1);
    CHECK_CONTAINS(diags.all().front().message, "states only an index");
    REQUIRE(diags.all().front().notes.size() == 1);
    CHECK_CONTAINS(diags.all().front().notes.front().message,
                   "write it as 'ior' on the surface");
  }
  SUBCASE("What the glass refuses is reported at its definition") {
    const auto &source{
        diags.addSource("test.lens", definingCrown("{ ior 1.5 abbe -5 }"))};
    (void)parseLens(diags, source);
    REQUIRE(diags.errorCount() == 1);
    const auto &error{diags.all().front()};
    CHECK_CONTAINS(error.message,
                   "glass 'CROWN': expected a positive Abbe number");
    CHECK(source.lineAndColumn(error.location.offset).lineNo == 4);
  }
  SUBCASE("Every refusal of the glass model reaches the file") {
    CHECK_CONTAINS(refusalOf("{ ior 0.9 abbe 60 }"),
                   "expected an index greater than 1");
    CHECK_CONTAINS(refusalOf("{ sellmeier { b 1 0 0 c 0.25 0 0 } }"),
                   "pole at 500 nm");
    CHECK_CONTAINS(refusalOf("{ ior 1.5 abbe 60 partial_dispersion 0.3 }"),
                   "rises with wavelength");
  }
  SUBCASE("A partial dispersion that bends the fit back is read against the "
          "normal line") {
    const auto &source{diags.addSource(
        "test.lens",
        definingCrown("{ ior 1.5 abbe 60 partial_dispersion 0.3 }"))};
    (void)parseLens(diags, source);
    REQUIRE(diags.errorCount() == 1);
    REQUIRE(diags.all().front().notes.size() == 1);
    CHECK_CONTAINS(diags.all().front().notes.front().message,
                   "the normal line puts a glass of this Abbe number at "
                   "0.5429");
  }
  SUBCASE("A glass no surface names is a warning") {
    (void)parseOK(diags, "lens {\n"
                         "  surface { radius 50 thickness 4 ior 1.5 "
                         "diameter 20 }\n"
                         "  stop { thickness 10 diameter 12 }\n"
                         "  glass SPARE { ior 1.5 abbe 60 }\n"
                         "}\n");
    REQUIRE(diags.warningCount() == 1);
    CHECK_CONTAINS(diags.all().front().message,
                   "glass 'SPARE' is defined, and no surface names it");
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
  SUBCASE("A stray glass says where a definition belongs") {
    const auto &source{
        diags.addSource("test.lens", "glass CROWN { ior 1.5 abbe 60 }\n")};
    (void)parseLens(diags, source);
    REQUIRE(diags.errorCount() == 1);
    REQUIRE(diags.all().front().notes.size() == 1);
    CHECK_CONTAINS(diags.all().front().notes.front().message,
                   "inside the 'lens' block");
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
    CHECK(lens.surfaces[0].medium.nd() == doctest::Approx(1.67f));
    CHECK(lens.surfaces[1].medium.nd() == doctest::Approx(1.0f));
    CHECK(lens.surfaces[3].medium.nd() == doctest::Approx(1.699f));
  }
  SUBCASE("The last surface may leave its thickness to the focus solve") {
    CHECK(lens.surfaces.back().thickness == 0.0f);
  }
}
