#include "doctest.h"

#include <string>

#include "Layout/Layout.h"

// Parse from memory and require no errors.
static LayoutDocument parseOK(LayoutDiagnostics &diags, std::string text) {
  const auto &source{diags.addSource("test.layout", std::move(text))};
  auto document{parseLayout(diags, source, "/nowhere")};
  if (diags.hasErrors()) MESSAGE(diags.renderAll(false));
  REQUIRE(!diags.hasErrors());
  return document;
}

TEST_CASE("LayoutParser: declarations and a placement") {
  LayoutDiagnostics diags{};
  const auto document{parseOK(diags, R"(#smdl layout
asset lamp = sphere { radius 0.2 material lamp_r020 caster }
light beam = spot { power 100 angle 40 blend 0.2 }
place lamp as hero translate 0 0 3
)")};
  CHECK(diags.empty());
  REQUIRE(document.assets.size() == 1);
  const auto &lamp{document.assets[0]};
  CHECK(lamp.name == "lamp");
  CHECK(lamp.path.empty());
  CHECK(lamp.primitive.shape == PrimitiveSpec::Shape::SPHERE);
  CHECK(lamp.primitive.radius == doctest::Approx(0.2f));
  CHECK(lamp.materials.all == "lamp_r020");
  CHECK(lamp.caster);
  CHECK(!lamp.caustic);
  REQUIRE(document.lights.size() == 1);
  const auto &beam{document.lights[0]};
  CHECK(beam.kind == LayoutLightDecl::Kind::SPOT);
  CHECK(beam.powerSet);
  CHECK(beam.power == doctest::Approx(100.0f));
  CHECK(beam.spotAngle == doctest::Approx(40.0f));
  CHECK(beam.spotBlend == doctest::Approx(0.2f));
  REQUIRE(document.placements.size() == 1);
  const auto &place{document.placements[0]};
  CHECK(place.kind == LayoutPlacement::Kind::PLACE);
  CHECK(place.assetName == "lamp");
  CHECK(place.asName == "hero");
  // Column-major: the translation is the last column.
  CHECK(place.transform[3].x == doctest::Approx(0.0f));
  CHECK(place.transform[3].y == doctest::Approx(0.0f));
  CHECK(place.transform[3].z == doctest::Approx(3.0f));
  CHECK(place.transform[3].w == doctest::Approx(1.0f));
  CHECK(document.findAsset("lamp") == &lamp);
  CHECK(document.findLight("beam") == &beam);
  CHECK(document.findAsset("beam") == nullptr);
  CHECK(document.findGroup("lamp") == nullptr);
}

TEST_CASE("LayoutParser: diagnostics") {
  LayoutDiagnostics diags{};
  SUBCASE("The magic line is required") {
    const auto &source{
        diags.addSource("test.layout", "asset rock = \"rock.obj\"\n")};
    (void)parseLayout(diags, source, "/nowhere");
    REQUIRE(diags.hasErrors());
    const auto &error{diags.all().front()};
    CHECK(error.kind == LayoutDiagnostic::Kind::ERROR);
    CHECK(error.message.find("#smdl layout") != std::string::npos);
    CHECK(error.location.source == &source);
    CHECK(error.location.offset == 0);
  }
  SUBCASE("An unknown asset operation is located") {
    const auto &source{diags.addSource(
        "test.layout", "#smdl layout\nasset rock = \"rock.obj\" {\n"
                       "  frobnicate\n}\n")};
    (void)parseLayout(diags, source, "/nowhere");
    REQUIRE(diags.errorCount() == 1);
    const auto &error{diags.all().front()};
    CHECK(error.message.find("unknown asset operation") != std::string::npos);
    CHECK(error.message.find("frobnicate") != std::string::npos);
    REQUIRE(error.location.source == &source);
    const auto where{source.lineAndColumn(error.location.offset)};
    CHECK(where.lineNo == 3);
    CHECK(where.charNo == 3);
  }
  SUBCASE("A redeclared light points back at the first") {
    const auto &source{diags.addSource(
        "test.layout", "#smdl layout\nlight a = point\nlight a = spot\n")};
    const auto document{parseLayout(diags, source, "/nowhere")};
    REQUIRE(diags.errorCount() == 1);
    const auto &error{diags.all().front()};
    CHECK(error.message.find("redeclaration of light") != std::string::npos);
    REQUIRE(error.notes.size() == 1);
    CHECK(source.lineAndColumn(error.location.offset).lineNo == 3);
    CHECK(source.lineAndColumn(error.notes[0].location.offset).lineNo == 2);
    // The second declaration is dropped, so the first survives intact.
    REQUIRE(document.lights.size() == 1);
    CHECK(document.lights[0].kind == LayoutLightDecl::Kind::POINT);
  }
}
