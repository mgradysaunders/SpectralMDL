#include "doctest.h"

#include <optional>
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
  SUBCASE("The box takes a size, and only the box does") {
    const auto document{parseOK(diags, R"(#smdl layout
asset crate = box { size 0.5 1.25 2 material wood }
asset plain = box { material wood }
)")};
    REQUIRE(document.assets.size() == 2);
    const auto &crate{document.assets[0]};
    CHECK(crate.primitive.shape == PrimitiveSpec::Shape::BOX);
    CHECK(crate.primitive.hasSize());
    CHECK(!crate.primitive.hasRadius());
    CHECK(!crate.primitive.hasHeight());
    CHECK(crate.primitive.size.x == doctest::Approx(0.5f));
    CHECK(crate.primitive.size.y == doctest::Approx(1.25f));
    CHECK(crate.primitive.size.z == doctest::Approx(2.0f));
    // The default is the unit cube, and it keys apart from a sized one.
    CHECK(document.assets[1].primitive.size.x == doctest::Approx(1.0f));
    CHECK(crate.primitive.key() != document.assets[1].primitive.key());
  }
  SUBCASE("A shape parameter the shape does not have is an error") {
    for (const char *text :
         {"#smdl layout\nasset a = box { radius 1 material m }\n",
          "#smdl layout\nasset a = box { height 1 material m }\n",
          "#smdl layout\nasset a = sphere { size 1 1 1 material m }\n",
          "#smdl layout\nasset a = disk { height 1 material m }\n"}) {
      CAPTURE(std::string(text));
      LayoutDiagnostics local{};
      const auto &source{local.addSource("test.layout", text)};
      (void)parseLayout(local, source, "/nowhere");
      // Recovery skips the rest of the block, so the missing `material`
      // is reported after it; the first error is the one under test.
      REQUIRE(local.hasErrors());
      CHECK(local.all().front().message.find("has no") != std::string::npos);
    }
  }
  SUBCASE("A box size must be three positive numbers") {
    const auto &source{diags.addSource(
        "test.layout",
        "#smdl layout\nasset a = box { size 1 0 2 material m }\n")};
    (void)parseLayout(diags, source, "/nowhere");
    REQUIRE(diags.hasErrors());
    CHECK(diags.all().front().message.find("three positive numbers") !=
          std::string::npos);
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

TEST_CASE("LayoutParser: the marks") {
  LayoutDiagnostics diags{};
  SUBCASE("Asset, place, and import spellings") {
    const auto document{parseOK(diags, R"(#smdl layout
asset a = sphere { radius 1 material m caster caustic light }
asset b = "b.gltf"
light l = point { caustic }
place a caster light
place a caster off light off
place a { caster off light }
place b
import "b.gltf" { caster off light off }
import "b.gltf" { caster light }
)")};
    REQUIRE(document.assets.size() == 2);
    CHECK(document.assets[0].caster);
    CHECK(bool(document.assets[0].casterLoc));
    CHECK(document.assets[0].light);
    CHECK(bool(document.assets[0].lightLoc));
    CHECK(document.assets[0].caustic);
    CHECK(!document.assets[1].caster);
    CHECK(!document.assets[1].light);
    CHECK(!document.assets[1].caustic);
    REQUIRE(document.lights.size() == 1);
    CHECK(document.lights[0].caustic);
    REQUIRE(document.placements.size() == 6);
    const auto casterOf{
        [&](size_t i) { return document.placements[i].casterOverride; }};
    const auto lightOf{
        [&](size_t i) { return document.placements[i].lightOverride; }};
    CHECK(casterOf(0) == std::optional<bool>(true));
    CHECK(lightOf(0) == std::optional<bool>(true));
    CHECK(casterOf(1) == std::optional<bool>(false));
    CHECK(lightOf(1) == std::optional<bool>(false));
    CHECK(casterOf(2) == std::optional<bool>(false));
    CHECK(lightOf(2) == std::optional<bool>(true));
    CHECK(casterOf(3) == std::nullopt);
    CHECK(lightOf(3) == std::nullopt);
    CHECK(document.placements[4].kind == LayoutPlacement::Kind::IMPORT);
    CHECK(casterOf(4) == std::optional<bool>(false));
    CHECK(lightOf(4) == std::optional<bool>(false));
    CHECK(casterOf(5) == std::optional<bool>(true));
    CHECK(lightOf(5) == std::optional<bool>(true));
    CHECK(bool(document.placements[0].casterLoc));
    CHECK(bool(document.placements[0].lightLoc));
    CHECK(!document.placements[3].casterLoc);
    CHECK(!document.placements[3].lightLoc);
  }
  SUBCASE("A mark written twice is an error") {
    const auto &source{diags.addSource(
        "test.layout", "#smdl layout\n"
                       "asset a = sphere { radius 1 material m }\n"
                       "place a caster caster\n"
                       "place a light off light\n"
                       "import \"b.gltf\" { caster off caster }\n"
                       "import \"b.gltf\" { light light }\n")};
    (void)parseLayout(diags, source, "/nowhere");
    REQUIRE(diags.errorCount() == 4);
    const char *expected[]{"'caster' appears twice in one place",
                           "'light' appears twice in one place",
                           "'caster' appears twice in one import",
                           "'light' appears twice in one import"};
    for (size_t i = 0; i < 4; i++) {
      CAPTURE(i);
      CHECK(diags.all()[i].message.find(expected[i]) != std::string::npos);
      CHECK(source.lineAndColumn(diags.all()[i].location.offset).lineNo ==
            uint32_t(3 + i));
    }
  }
  SUBCASE("The word lists name both marks") {
    const auto &source{diags.addSource(
        "test.layout", "#smdl layout\n"
                       "asset a = sphere { radius 1 material m frob }\n"
                       "asset b = \"b.gltf\" { frob }\n"
                       "place a frob\n"
                       "import \"b.gltf\" { frob }\n")};
    (void)parseLayout(diags, source, "/nowhere");
    REQUIRE(diags.errorCount() == 4);
    for (size_t i = 0; i < 4; i++) {
      CAPTURE(i);
      CHECK(diags.all()[i].message.find("caster, light, ") !=
            std::string::npos);
    }
  }
}
