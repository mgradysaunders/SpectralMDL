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
  CHECK(lamp.isCaster);
  CHECK(!lamp.isCaustic);
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
  SUBCASE("A shape light's extent must be positive") {
    for (const char *text : {"#smdl layout\nlight a = rect { size 1 0 }\n",
                             "#smdl layout\nlight a = rect { size -2 1 }\n",
                             "#smdl layout\nlight a = disk { radius 0 }\n"}) {
      CAPTURE(std::string(text));
      LayoutDiagnostics local{};
      const auto &source{local.addSource("test.layout", text)};
      (void)parseLayout(local, source, "/nowhere");
      REQUIRE(local.hasErrors());
      CHECK(local.all().front().message.find("positive number") !=
            std::string::npos);
    }
  }
  SUBCASE("A light setting the kind does not have is an error") {
    for (const auto &check :
         {std::pair{"#smdl layout\nlight a = disk { size 1 1 }\n",
                    "'size' applies to a rect"},
          std::pair{"#smdl layout\nlight a = rect { radius 1 }\n",
                    "'radius' applies to a disk"},
          std::pair{"#smdl layout\nlight a = point { radius 1 }\n",
                    "'radius' applies to a disk"},
          std::pair{"#smdl layout\nlight a = rect { angle 30 }\n",
                    "'angle' applies to a spot"},
          std::pair{"#smdl layout\nlight a = rect { scale 2 }\n",
                    "the place line's 'scale' stretches it"},
          std::pair{"#smdl layout\nlight a = disk { frobnicate }\n",
                    "radius, caustic"}}) {
      const std::string text{check.first};
      CAPTURE(text);
      LayoutDiagnostics local{};
      const auto &source{local.addSource("test.layout", text)};
      (void)parseLayout(local, source, "/nowhere");
      REQUIRE(local.hasErrors());
      CHECK(local.all().front().message.find(check.second) !=
            std::string::npos);
    }
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
    CHECK(document.assets[0].isCaster);
    CHECK(bool(document.assets[0].casterLoc));
    CHECK(document.assets[0].isLight);
    CHECK(bool(document.assets[0].lightLoc));
    CHECK(document.assets[0].isCaustic);
    CHECK(!document.assets[1].isCaster);
    CHECK(!document.assets[1].isLight);
    CHECK(!document.assets[1].isCaustic);
    REQUIRE(document.lights.size() == 1);
    CHECK(document.lights[0].isCaustic);
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

TEST_CASE("LayoutParser: the shape lights") {
  LayoutDiagnostics diags{};
  const auto document{parseOK(diags, R"(#smdl layout
light panel = rect { size 2 1 power 400 caustic }
light ring = disk { radius 0.25 }
light square = rect
light coin = disk { rotate_x 90 }
)")};
  CHECK(diags.empty());
  REQUIRE(document.lights.size() == 4);
  const auto &panel{document.lights[0]};
  CHECK(panel.kind == LayoutLightDecl::Kind::RECT);
  CHECK(panel.kindName() == "rect");
  CHECK(panel.size.x == doctest::Approx(2.0f));
  CHECK(panel.size.y == doctest::Approx(1.0f));
  CHECK(panel.powerSet);
  CHECK(panel.power == doctest::Approx(400.0f));
  CHECK(panel.isCaustic);
  const auto &ring{document.lights[1]};
  CHECK(ring.kind == LayoutLightDecl::Kind::DISK);
  CHECK(ring.kindName() == "disk");
  CHECK(ring.radius == doctest::Approx(0.25f));
  CHECK(!ring.powerSet);
  CHECK(!ring.isCaustic);
  // The defaults: a unit square and a unit diameter.
  CHECK(document.lights[2].size.x == doctest::Approx(1.0f));
  CHECK(document.lights[2].size.y == doctest::Approx(1.0f));
  CHECK(document.lights[3].radius == doctest::Approx(0.5f));
  // The block's transform operations apply to a shape as to any light:
  // a quarter turn about X carries the local Y axis onto Z.
  CHECK(document.lights[3].transform[1].z == doctest::Approx(1.0f));
  CHECK(document.lights[3].transform[1].y == doctest::Approx(0.0f));
}

TEST_CASE("LayoutParser: the time directive") {
  LayoutDiagnostics diags{};
  SUBCASE("Both settings parse, and the location is the first block's") {
    const auto document{
        parseOK(diags, "#smdl layout\ntime { base 2.5 shutter 0.02 }\n")};
    REQUIRE(document.time.base);
    REQUIRE(document.time.shutter);
    CHECK(*document.time.base == doctest::Approx(2.5f));
    CHECK(*document.time.shutter == doctest::Approx(0.02f));
    REQUIRE(document.timeLoc);
    CHECK(document.source->lineAndColumn(document.timeLoc.offset).lineNo == 2);
  }
  SUBCASE("Absent, both stay unset") {
    const auto document{parseOK(diags, "#smdl layout\n")};
    CHECK(!document.time.base);
    CHECK(!document.time.shutter);
    CHECK(!document.timeLoc);
  }
  SUBCASE("Two blocks merge per field, last one wins") {
    const auto document{parseOK(diags, "#smdl layout\n"
                                       "time { base 1 shutter 0.5 }\n"
                                       "time { base 2 }\n")};
    CHECK(*document.time.base == doctest::Approx(2.0f));
    CHECK(*document.time.shutter == doctest::Approx(0.5f));
  }
  SUBCASE("A zero shutter is a shut shutter, not an error") {
    const auto document{parseOK(diags, "#smdl layout\ntime { shutter 0 }\n")};
    CHECK(*document.time.shutter == 0.0f);
  }
  SUBCASE("A negative shutter is an error") {
    const auto &source{
        diags.addSource("test.layout", "#smdl layout\ntime { shutter -1 }\n")};
    (void)parseLayout(diags, source, "/nowhere");
    REQUIRE(diags.errorCount() == 1);
    CHECK(diags.all().front().message.find(
              "nonnegative number for 'shutter'") != std::string::npos);
  }
  SUBCASE("A non-numeric base is an error") {
    const auto &source{
        diags.addSource("test.layout", "#smdl layout\ntime { base noon }\n")};
    (void)parseLayout(diags, source, "/nowhere");
    REQUIRE(diags.errorCount() == 1);
    CHECK(diags.all().front().message.find("expected a number") !=
          std::string::npos);
  }
  SUBCASE("A non-finite base is an error") {
    const auto &source{
        diags.addSource("test.layout", "#smdl layout\ntime { base inf }\n")};
    (void)parseLayout(diags, source, "/nowhere");
    REQUIRE(diags.errorCount() == 1);
    CHECK(diags.all().front().message.find("finite number for 'base'") !=
          std::string::npos);
  }
  SUBCASE("An unknown setting names the two that exist") {
    const auto &source{
        diags.addSource("test.layout", "#smdl layout\ntime { fps 24 }\n")};
    (void)parseLayout(diags, source, "/nowhere");
    REQUIRE(diags.errorCount() == 1);
    const auto &error{diags.all().front()};
    CHECK(error.message.find("unknown time setting") != std::string::npos);
    CHECK(error.message.find("base or shutter") != std::string::npos);
  }
  SUBCASE("The parse resynchronizes at the next statement") {
    const auto &source{diags.addSource(
        "test.layout", "#smdl layout\ntime { shutter -1 }\nsky { none }\n")};
    const auto document{parseLayout(diags, source, "/nowhere")};
    REQUIRE(diags.errorCount() == 1);
    CHECK(document.sky.none == true);
  }
}

TEST_CASE("LayoutParser: the camera motion block") {
  LayoutDiagnostics diags{};
  SUBCASE("Absent, there is no motion") {
    const auto document{
        parseOK(diags, "#smdl layout\ncamera { look_from 1 2 3 }\n")};
    CHECK(!document.camera.motion);
  }
  SUBCASE("One key: the others stay unset for the merge to fill") {
    const auto document{parseOK(
        diags, "#smdl layout\ncamera { motion { look_to 0 1 0.5 } }\n")};
    REQUIRE(document.camera.motion);
    CHECK(!document.camera.motion->lookFrom);
    REQUIRE(document.camera.motion->lookTo);
    CHECK(document.camera.motion->lookTo->y == doctest::Approx(1.0f));
    CHECK(!document.camera.motion->lookUp);
  }
  SUBCASE("Two keys") {
    const auto document{parseOK(diags, "#smdl layout\ncamera {\n"
                                       "  look_from -6 0 2\n"
                                       "  motion { look_from -5 0 2 "
                                       "look_up 0.1 0 1 }\n"
                                       "}\n")};
    REQUIRE(document.camera.motion);
    REQUIRE(document.camera.motion->lookFrom);
    REQUIRE(document.camera.motion->lookUp);
    CHECK(document.camera.motion->lookFrom->x == doctest::Approx(-5.0f));
    CHECK(document.camera.motion->lookUp->x == doctest::Approx(0.1f));
    CHECK(!document.camera.motion->lookTo);
    REQUIRE(document.camera.lookFrom);
    CHECK(document.camera.lookFrom->x == doctest::Approx(-6.0f));
  }
  SUBCASE("Three keys") {
    const auto document{
        parseOK(diags, "#smdl layout\ncamera { motion { look_from 1 2 3 "
                       "look_to 4 5 6 look_up 7 8 9 } }\n")};
    REQUIRE(document.camera.motion);
    REQUIRE(document.camera.motion->lookFrom);
    REQUIRE(document.camera.motion->lookTo);
    REQUIRE(document.camera.motion->lookUp);
    CHECK(document.camera.motion->lookFrom->z == doctest::Approx(3.0f));
    CHECK(document.camera.motion->lookTo->z == doctest::Approx(6.0f));
    CHECK(document.camera.motion->lookUp->z == doctest::Approx(9.0f));
  }
  SUBCASE("A repeated block merges per field, last one wins") {
    const auto document{parseOK(diags,
                                "#smdl layout\n"
                                "camera { motion { look_from 1 0 0 "
                                "look_to 0 1 0 } }\n"
                                "camera { motion { look_from 2 0 0 } }\n")};
    REQUIRE(document.camera.motion);
    CHECK(document.camera.motion->lookFrom->x == doctest::Approx(2.0f));
    CHECK(document.camera.motion->lookTo->y == doctest::Approx(1.0f));
  }
  SUBCASE("An unknown key inside names the three that exist") {
    const auto &source{diags.addSource(
        "test.layout", "#smdl layout\ncamera { motion { fovy 30 } }\n")};
    (void)parseLayout(diags, source, "/nowhere");
    REQUIRE(diags.errorCount() == 1);
    const auto &error{diags.all().front()};
    CHECK(error.message.find("unknown camera motion setting") !=
          std::string::npos);
    CHECK(error.message.find("look_from, look_to, or look_up") !=
          std::string::npos);
  }
  SUBCASE("The block needs its brace") {
    const auto &source{diags.addSource(
        "test.layout", "#smdl layout\ncamera { motion look_to 0 0 0 }\n")};
    (void)parseLayout(diags, source, "/nowhere");
    REQUIRE(diags.errorCount() == 1);
    CHECK(diags.all().front().message.find("'{' after 'motion'") !=
          std::string::npos);
  }
  SUBCASE("At the top level, motion is still an unknown directive") {
    const auto &source{diags.addSource(
        "test.layout", "#smdl layout\nmotion { look_to 0 0 0 }\n")};
    (void)parseLayout(diags, source, "/nowhere");
    REQUIRE(diags.errorCount() == 1);
    CHECK(diags.all().front().message.find("unknown directive") !=
          std::string::npos);
  }
}

TEST_CASE("LayoutParser: the motion block on a place") {
  LayoutDiagnostics diags{};
  SUBCASE("Absent, the placement is static") {
    const auto document{parseOK(diags, "#smdl layout\n"
                                       "asset ball = sphere { material m }\n"
                                       "place ball translate 0 0 3\n")};
    REQUIRE(document.placements.size() == 1);
    CHECK(!document.placements[0].motion);
    CHECK(!document.placements[0].motionLoc);
  }
  SUBCASE("The one-line form: the block restates the transform at shut") {
    const auto document{parseOK(
        diags, "#smdl layout\n"
               "asset ball = sphere { material m }\n"
               "place ball translate 0 0 3 motion { translate 0 0 3.2 }\n")};
    REQUIRE(document.placements.size() == 1);
    const auto &place{document.placements[0]};
    CHECK(place.transform[3].z == doctest::Approx(3.0f));
    REQUIRE(place.motion);
    CHECK((*place.motion)[3].x == doctest::Approx(0.0f));
    CHECK((*place.motion)[3].z == doctest::Approx(3.2f));
    CHECK(place.motionLoc);
  }
  SUBCASE("The block form, beside a rename, with operations in order") {
    const auto document{parseOK(diags,
                                "#smdl layout\n"
                                "asset rock = \"rock.obj\"\n"
                                "place rock {\n"
                                "  material a = b\n"
                                "  translate 1 0 0\n"
                                "  motion { translate 2 0 0 rotate_z 90 }\n"
                                "}\n")};
    REQUIRE(document.placements.size() == 1);
    const auto &place{document.placements[0]};
    CHECK(place.overrides.size() == 1);
    CHECK(place.transform[3].x == doctest::Approx(1.0f));
    REQUIRE(place.motion);
    // The translation, then the turn about the origin, so the placed
    // origin lands on the y axis.
    CHECK((*place.motion)[3].x == doctest::Approx(0.0f));
    CHECK((*place.motion)[3].y == doctest::Approx(2.0f));
  }
  SUBCASE("A block spanning lines in the one-line form") {
    const auto document{parseOK(diags, "#smdl layout\n"
                                       "asset ball = sphere { material m }\n"
                                       "place ball translate 1 0 0 motion {\n"
                                       "  translate 2 0 0\n"
                                       "}\n"
                                       "place ball\n")};
    REQUIRE(document.placements.size() == 2);
    REQUIRE(document.placements[0].motion);
    CHECK((*document.placements[0].motion)[3].x == doctest::Approx(2.0f));
    CHECK(!document.placements[1].motion);
  }
  SUBCASE("On a group's place and on a bulk place") {
    const auto document{parseOK(
        diags, "#smdl layout\n"
               "asset ball = sphere { material m }\n"
               "group rig { place ball motion { translate 1 0 0 } }\n"
               "place ball * \"pair.places\" motion { rotate_z 10 }\n")};
    REQUIRE(document.groups.size() == 1);
    REQUIRE(document.groups[0].placements.size() == 1);
    CHECK(document.groups[0].placements[0].motion);
    REQUIRE(document.placements.size() == 1);
    CHECK(document.placements[0].placesPath == "pair.places");
    CHECK(document.placements[0].motion);
  }
  SUBCASE("A second block on one place is an error, not a merge") {
    const auto &source{diags.addSource(
        "test.layout", "#smdl layout\n"
                       "asset ball = sphere { material m }\n"
                       "place ball motion { translate 1 0 0 } motion { "
                       "translate 2 0 0 }\n"
                       "place ball\n")};
    const auto document{parseLayout(diags, source, "/nowhere")};
    REQUIRE(diags.errorCount() == 1);
    CHECK(diags.all().front().message.find("'motion' appears twice") !=
          std::string::npos);
    REQUIRE(document.placements.size() == 2);
    REQUIRE(document.placements[0].motion);
    CHECK((*document.placements[0].motion)[3].x == doctest::Approx(1.0f));
    CHECK(!document.placements[1].motion);
  }
  SUBCASE("On an import it is an error") {
    const auto &source{diags.addSource(
        "test.layout", "#smdl layout\n"
                       "import \"rock.obj\" { motion { translate 1 0 0 } }\n"
                       "asset ball = sphere { material m }\n"
                       "place ball\n")};
    const auto document{parseLayout(diags, source, "/nowhere")};
    REQUIRE(diags.errorCount() == 1);
    CHECK(diags.all().front().message.find("place operation") !=
          std::string::npos);
    CHECK(document.placements.size() == 2);
  }
  SUBCASE("Only transform operations are admitted inside") {
    const auto &source{diags.addSource("test.layout",
                                       "#smdl layout\n"
                                       "asset ball = sphere { material m }\n"
                                       "place ball motion { material a = b }\n"
                                       "place ball\n")};
    const auto document{parseLayout(diags, source, "/nowhere")};
    REQUIRE(diags.errorCount() == 1);
    const auto &error{diags.all().front()};
    CHECK(error.message.find("transform operation inside 'motion'") !=
          std::string::npos);
    CHECK(error.message.find("rotate_z, or matrix") != std::string::npos);
    REQUIRE(document.placements.size() == 2);
    CHECK(!document.placements[1].motion);
  }
  SUBCASE("The block needs its brace") {
    const auto &source{diags.addSource("test.layout",
                                       "#smdl layout\n"
                                       "asset ball = sphere { material m }\n"
                                       "place ball motion translate 1 0 0\n"
                                       "place ball\n")};
    const auto document{parseLayout(diags, source, "/nowhere")};
    REQUIRE(diags.errorCount() == 1);
    CHECK(diags.all().front().message.find("'{' after 'motion'") !=
          std::string::npos);
    CHECK(document.placements.size() == 2);
  }
}
