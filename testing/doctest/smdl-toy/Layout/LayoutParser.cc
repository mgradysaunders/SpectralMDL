#include "Fixtures.h"

#include <optional>
#include <string>

#include "Layout/Layout.h"

namespace {
// Parse from memory and require no errors.
LayoutDocument parseOK(LayoutDiagnostics &diags, std::string text) {
  const auto &source{diags.addSource("test.layout", std::move(text))};
  auto document{parseLayout(diags, source, "/nowhere")};
  if (diags.hasErrors()) MESSAGE(diags.renderAll(false));
  REQUIRE(!diags.hasErrors());
  return document;
}
} // namespace

TEST_CASE("LayoutParser: declarations and a placement") {
  LayoutDiagnostics diags{};
  const auto document{parseOK(
      diags, R"(asset lamp = sphere { radius 0.2 material lamp_r020 caster }
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
  CHECK(beam.isPowerSet);
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
  SUBCASE("An unknown asset operation is located") {
    const auto &source{diags.addSource("test.layout",
                                       "asset rock = \"rock.obj\" {\n"
                                       "  frobnicate\n}\n")};
    (void)parseLayout(diags, source, "/nowhere");
    REQUIRE(diags.errorCount() == 1);
    const auto &error{diags.all().front()};
    CHECK_CONTAINS(error.message, "unknown asset operation");
    CHECK_CONTAINS(error.message, "frobnicate");
    REQUIRE(error.location.source == &source);
    const auto where{source.lineAndColumn(error.location.offset)};
    CHECK(where.lineNo == 2);
    CHECK(where.charNo == 3);
  }
  SUBCASE("A 'camera' directive names the file it belongs in") {
    const auto &source{diags.addSource("test.layout", "camera { fovy 30 }\n")};
    (void)parseLayout(diags, source, "/nowhere");
    REQUIRE(diags.errorCount() == 1);
    const auto &error{diags.all().front()};
    REQUIRE(!error.notes.empty());
    CHECK_CONTAINS(error.notes.front().message, "'.camera' file");
  }
  SUBCASE("A 'time' directive names the flag, since no file holds the clock") {
    const auto &source{diags.addSource("test.layout", "time { base 2 }\n")};
    (void)parseLayout(diags, source, "/nowhere");
    REQUIRE(diags.errorCount() == 1);
    const auto &error{diags.all().front()};
    REQUIRE(!error.notes.empty());
    // Never the camera file, which rejects a 'time' block of its own.
    CHECK_CONTAINS(error.notes.front().message, "'-time'");
    CHECK_NOT_CONTAINS(error.notes.front().message, "belongs in");
  }
  SUBCASE("The box takes a size, and only the box does") {
    const auto document{
        parseOK(diags, R"(asset crate = box { size 0.5 1.25 2 material wood }
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
    for (const char *text : {"asset a = box { radius 1 material m }\n",
                             "asset a = box { height 1 material m }\n",
                             "asset a = sphere { size 1 1 1 material m }\n",
                             "asset a = disk { height 1 material m }\n"}) {
      CAPTURE(std::string(text));
      LayoutDiagnostics local{};
      const auto &source{local.addSource("test.layout", text)};
      (void)parseLayout(local, source, "/nowhere");
      // Recovery skips the rest of the block, so the missing `material`
      // is reported after it; the first error is the one under test.
      REQUIRE(local.hasErrors());
      CHECK_CONTAINS(local.all().front().message, "has no");
    }
  }
  SUBCASE("A box size must be three positive numbers") {
    const auto &source{diags.addSource(
        "test.layout", "asset a = box { size 1 0 2 material m }\n")};
    (void)parseLayout(diags, source, "/nowhere");
    REQUIRE(diags.hasErrors());
    CHECK_CONTAINS(diags.all().front().message, "three positive numbers");
  }
  SUBCASE("A shape light's extent must be positive") {
    for (const char *text :
         {"light a = rect { size 1 0 }\n", "light a = rect { size -2 1 }\n",
          "light a = disk { radius 0 }\n"}) {
      CAPTURE(std::string(text));
      LayoutDiagnostics local{};
      const auto &source{local.addSource("test.layout", text)};
      (void)parseLayout(local, source, "/nowhere");
      REQUIRE(local.hasErrors());
      CHECK_CONTAINS(local.all().front().message, "positive number");
    }
  }
  SUBCASE("A light setting the kind does not have is an error") {
    for (const auto &check :
         {std::pair{"light a = disk { size 1 1 }\n",
                    "'size' applies to a rect"},
          std::pair{"light a = rect { radius 1 }\n",
                    "'radius' applies to a disk"},
          std::pair{"light a = point { radius 1 }\n",
                    "'radius' applies to a disk"},
          std::pair{"light a = rect { angle 30 }\n",
                    "'angle' applies to a spot"},
          std::pair{"light a = rect { scale 2 }\n",
                    "the place line's 'scale' stretches it"},
          std::pair{"light a = disk { frobnicate }\n", "radius, caustic"}}) {
      const std::string text{check.first};
      CAPTURE(text);
      LayoutDiagnostics local{};
      const auto &source{local.addSource("test.layout", text)};
      (void)parseLayout(local, source, "/nowhere");
      REQUIRE(local.hasErrors());
      CHECK_CONTAINS(local.all().front().message, check.second);
    }
  }
  SUBCASE("A redeclared light points back at the first") {
    const auto &source{
        diags.addSource("test.layout", "light a = point\nlight a = spot\n")};
    const auto document{parseLayout(diags, source, "/nowhere")};
    REQUIRE(diags.errorCount() == 1);
    const auto &error{diags.all().front()};
    CHECK_CONTAINS(error.message, "redeclaration of light");
    REQUIRE(error.notes.size() == 1);
    CHECK(source.lineAndColumn(error.location.offset).lineNo == 2);
    CHECK(source.lineAndColumn(error.notes[0].location.offset).lineNo == 1);
    // The second declaration is dropped, so the first survives intact.
    REQUIRE(document.lights.size() == 1);
    CHECK(document.lights[0].kind == LayoutLightDecl::Kind::POINT);
  }
}

TEST_CASE("LayoutParser: the marks") {
  LayoutDiagnostics diags{};
  SUBCASE("Asset, place, and import spellings") {
    const auto document{parseOK(
        diags, R"(asset a = sphere { radius 1 material m caster caustic light }
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
    const auto isCasterOf{
        [&](size_t i) { return document.placements[i].casterOverride; }};
    const auto isLightOf{
        [&](size_t i) { return document.placements[i].lightOverride; }};
    CHECK(isCasterOf(0) == std::optional<bool>(true));
    CHECK(isLightOf(0) == std::optional<bool>(true));
    CHECK(isCasterOf(1) == std::optional<bool>(false));
    CHECK(isLightOf(1) == std::optional<bool>(false));
    CHECK(isCasterOf(2) == std::optional<bool>(false));
    CHECK(isLightOf(2) == std::optional<bool>(true));
    CHECK(isCasterOf(3) == std::nullopt);
    CHECK(isLightOf(3) == std::nullopt);
    CHECK(document.placements[4].kind == LayoutPlacement::Kind::IMPORT);
    CHECK(isCasterOf(4) == std::optional<bool>(false));
    CHECK(isLightOf(4) == std::optional<bool>(false));
    CHECK(isCasterOf(5) == std::optional<bool>(true));
    CHECK(isLightOf(5) == std::optional<bool>(true));
    CHECK(bool(document.placements[0].casterLoc));
    CHECK(bool(document.placements[0].lightLoc));
    CHECK(!document.placements[3].casterLoc);
    CHECK(!document.placements[3].lightLoc);
  }
  SUBCASE("A mark written twice is an error") {
    const auto &source{diags.addSource(
        "test.layout", "asset a = sphere { radius 1 material m }\n"
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
      CHECK_CONTAINS(diags.all()[i].message, expected[i]);
      CHECK(source.lineAndColumn(diags.all()[i].location.offset).lineNo ==
            uint32_t(2 + i));
    }
  }
  SUBCASE("The word lists name both marks") {
    const auto &source{diags.addSource(
        "test.layout", "asset a = sphere { radius 1 material m frob }\n"
                       "asset b = \"b.gltf\" { frob }\n"
                       "place a frob\n"
                       "import \"b.gltf\" { frob }\n")};
    (void)parseLayout(diags, source, "/nowhere");
    REQUIRE(diags.errorCount() == 4);
    for (size_t i = 0; i < 4; i++) {
      CAPTURE(i);
      CHECK_CONTAINS(diags.all()[i].message, "caster, light, ");
    }
  }
}

TEST_CASE("LayoutParser: the shape lights") {
  LayoutDiagnostics diags{};
  const auto document{
      parseOK(diags, R"(light panel = rect { size 2 1 power 400 caustic }
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
  CHECK(panel.isPowerSet);
  CHECK(panel.power == doctest::Approx(400.0f));
  CHECK(panel.isCaustic);
  const auto &ring{document.lights[1]};
  CHECK(ring.kind == LayoutLightDecl::Kind::DISK);
  CHECK(ring.kindName() == "disk");
  CHECK(ring.radius == doctest::Approx(0.25f));
  CHECK(!ring.isPowerSet);
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

TEST_CASE("LayoutParser: the motion block on a place") {
  LayoutDiagnostics diags{};
  SUBCASE("Absent, the placement is static") {
    const auto document{parseOK(diags, "asset ball = sphere { material m }\n"
                                       "place ball translate 0 0 3\n")};
    REQUIRE(document.placements.size() == 1);
    CHECK(document.placements[0].motion.empty());
    CHECK(!document.placements[0].motionLoc);
  }
  SUBCASE("The one-line form: keys at absolute times") {
    const auto document{
        parseOK(diags, "asset ball = sphere { material m }\n"
                       "place ball translate 0 0 3 motion { at 0 translate "
                       "0 0 0 at 0.5 translate 0 0 0.2 }\n")};
    REQUIRE(document.placements.size() == 1);
    const auto &place{document.placements[0]};
    CHECK(place.transform[3].z == doctest::Approx(3.0f));
    REQUIRE(place.motion.keys.size() == 2);
    CHECK(place.motion.keys[0].time == doctest::Approx(0.0f));
    CHECK(place.motion.keys[1].time == doctest::Approx(0.5f));
    CHECK(place.motion.keys[1].transform[3].z == doctest::Approx(0.2f));
    CHECK(place.motionLoc);
    // The place's own operations compose outside the track, so the world
    // transform rises from z = 3 to z = 3.2 over half a second.
    const auto worldAt{
        [&](float t) { return place.transform * place.motion.at(t); }};
    CHECK(worldAt(0.0f)[3].z == doctest::Approx(3.0f));
    CHECK(worldAt(0.5f)[3].z == doctest::Approx(3.2f));
    CHECK(worldAt(0.25f)[3].z == doctest::Approx(3.1f));
  }
  SUBCASE("The block form, beside a rename, with operations in order") {
    const auto document{
        parseOK(diags, "asset rock = \"rock.obj\"\n"
                       "place rock {\n"
                       "  material a = b\n"
                       "  translate 1 0 0\n"
                       "  motion { at 1 translate 2 0 0 rotate_z 90 }\n"
                       "}\n")};
    REQUIRE(document.placements.size() == 1);
    const auto &place{document.placements[0]};
    CHECK(place.overrides.size() == 1);
    CHECK(place.transform[3].x == doctest::Approx(1.0f));
    REQUIRE(place.motion.keys.size() == 1);
    // The translation, then the turn about the origin, so the key's own
    // origin lands on the y axis.
    CHECK(place.motion.keys[0].transform[3].x == doctest::Approx(0.0f));
    CHECK(place.motion.keys[0].transform[3].y == doctest::Approx(2.0f));
  }
  SUBCASE("A block spanning lines in the one-line form") {
    const auto document{parseOK(diags, "asset ball = sphere { material m }\n"
                                       "place ball translate 1 0 0 motion {\n"
                                       "  at 0 translate 2 0 0\n"
                                       "}\n"
                                       "place ball\n")};
    REQUIRE(document.placements.size() == 2);
    REQUIRE(document.placements[0].motion.keys.size() == 1);
    CHECK(document.placements[0].motion.keys[0].transform[3].x ==
          doctest::Approx(2.0f));
    CHECK(document.placements[1].motion.empty());
  }
  SUBCASE("On a group's place and on a bulk place") {
    const auto document{parseOK(
        diags, "asset ball = sphere { material m }\n"
               "group rig { place ball motion { at 0 translate 1 0 0 } }\n"
               "place ball * \"pair.places\" motion { at 0 rotate_z 10 }\n")};
    REQUIRE(document.groups.size() == 1);
    REQUIRE(document.groups[0].placements.size() == 1);
    CHECK(!document.groups[0].placements[0].motion.empty());
    REQUIRE(document.placements.size() == 1);
    CHECK(document.placements[0].placesPath == "pair.places");
    CHECK(!document.placements[0].motion.empty());
  }
  SUBCASE("Keys are written in ascending time") {
    const auto &source{diags.addSource(
        "test.layout", "asset ball = sphere { material m }\n"
                       "place ball motion { at 1 translate 1 0 0 at 0 "
                       "translate 2 0 0 }\n"
                       "place ball\n")};
    (void)parseLayout(diags, source, "/nowhere");
    REQUIRE(diags.errorCount() == 1);
    CHECK_CONTAINS(diags.all().front().message, "ascending time");
  }
  SUBCASE("An operation before the first key names the spelling") {
    const auto &source{diags.addSource("test.layout",
                                       "asset ball = sphere { material m }\n"
                                       "place ball motion { translate 1 0 0 }\n"
                                       "place ball\n")};
    (void)parseLayout(diags, source, "/nowhere");
    REQUIRE(diags.errorCount() == 1);
    const auto &error{diags.all().front()};
    CHECK_CONTAINS(error.message, "'at <seconds>' keys");
    REQUIRE(!error.notes.empty());
  }
  SUBCASE("An empty block is an error rather than a static placement") {
    const auto &source{diags.addSource("test.layout",
                                       "asset ball = sphere { material m }\n"
                                       "place ball motion { }\n")};
    (void)parseLayout(diags, source, "/nowhere");
    REQUIRE(diags.errorCount() == 1);
    CHECK_CONTAINS(diags.all().front().message, "at least one");
  }
  SUBCASE("A second block on one place is an error, not a merge") {
    const auto &source{diags.addSource(
        "test.layout", "asset ball = sphere { material m }\n"
                       "place ball motion { at 0 translate 1 0 0 } motion { "
                       "at 1 translate 2 0 0 }\n"
                       "place ball\n")};
    const auto document{parseLayout(diags, source, "/nowhere")};
    REQUIRE(diags.errorCount() == 1);
    CHECK_CONTAINS(diags.all().front().message, "'motion' appears twice");
    REQUIRE(document.placements.size() == 2);
    REQUIRE(document.placements[0].motion.keys.size() == 1);
    CHECK(document.placements[0].motion.keys[0].transform[3].x ==
          doctest::Approx(1.0f));
    CHECK(document.placements[1].motion.empty());
  }
  SUBCASE("On an import it is an error") {
    const auto &source{diags.addSource(
        "test.layout",
        "import \"rock.obj\" { motion { at 0 translate 1 0 0 } }\n"
        "asset ball = sphere { material m }\n"
        "place ball\n")};
    const auto document{parseLayout(diags, source, "/nowhere")};
    REQUIRE(diags.errorCount() == 1);
    CHECK_CONTAINS(diags.all().front().message, "place operation");
    CHECK(document.placements.size() == 2);
  }
  SUBCASE("Only transform operations are admitted inside a key") {
    const auto &source{diags.addSource(
        "test.layout", "asset ball = sphere { material m }\n"
                       "place ball motion { at 0 material a = b }\n"
                       "place ball\n")};
    const auto document{parseLayout(diags, source, "/nowhere")};
    REQUIRE(diags.errorCount() == 1);
    const auto &error{diags.all().front()};
    CHECK_CONTAINS(error.message, "transform operation inside 'motion'");
    CHECK_CONTAINS(error.message, "rotate_z, or matrix");
    REQUIRE(document.placements.size() == 2);
    CHECK(document.placements[1].motion.empty());
  }
  SUBCASE("The block needs its brace") {
    const auto &source{diags.addSource(
        "test.layout", "asset ball = sphere { material m }\n"
                       "place ball motion at 0 translate 1 0 0\n"
                       "place ball\n")};
    const auto document{parseLayout(diags, source, "/nowhere")};
    REQUIRE(diags.errorCount() == 1);
    CHECK_CONTAINS(diags.all().front().message, "'{' after 'motion'");
    CHECK(document.placements.size() == 2);
  }
}

namespace {
// Parse from memory, require exactly one error, and return its message.
std::string firstErrorOf(const std::string &text) {
  LayoutDiagnostics diags{};
  const auto &source{diags.addSource("test.layout", text)};
  (void)parseLayout(diags, source, "/nowhere");
  if (diags.errorCount() != 1) MESSAGE(diags.renderAll(false));
  REQUIRE(diags.errorCount() == 1);
  for (const auto &diagnostic : diags.all())
    if (diagnostic.kind == LayoutDiagnostic::Kind::ERROR)
      return diagnostic.message;
  return {};
}
} // namespace

TEST_CASE("LayoutParser: the animation operation") {
  LayoutDiagnostics diags{};
  SUBCASE(
      "A clip by name, settings in any order, ended by the next operation") {
    const auto document{parseOK(diags, "asset hero = \"hero.glb\" {\n"
                                       "  animation \"walk\" speed 2 offset "
                                       "0.25 once material m\n"
                                       "}\n"
                                       "place hero\n")};
    REQUIRE(document.assets.size() == 1);
    const auto &hero{document.assets[0]};
    CHECK(hero.animationLoc);
    CHECK(hero.animation.clipName == "walk");
    CHECK(hero.animation.clipIndex == INVALID_INDEX);
    CHECK(hero.animation.speed == doctest::Approx(2.0f));
    CHECK(hero.animation.offset == doctest::Approx(0.25f));
    CHECK(hero.animation.shouldPlayOnce);
    CHECK(!hero.animation.isOff);
    CHECK(hero.materials.all == "m");
  }
  SUBCASE("A clip by index, the bare word, off, and nothing") {
    const auto document{parseOK(diags, "asset a = \"a.glb\" { animation 2 }\n"
                                       "asset b = \"b.glb\" { animation }\n"
                                       "asset c = \"c.glb\" { animation off }\n"
                                       "asset d = \"d.glb\"\n"
                                       "place a\n")};
    REQUIRE(document.assets.size() == 4);
    CHECK(document.assets[0].animation.clipIndex == 2);
    CHECK(document.assets[0].animation.clipName.empty());
    CHECK(document.assets[0].animation.key() == "clip 2");
    CHECK(document.assets[1].animationLoc);
    CHECK(!document.assets[1].animation.hasClip());
    CHECK(document.assets[1].animation.key().empty());
    CHECK(document.assets[2].animation.isOff);
    CHECK(document.assets[2].animation.key() == "off");
    CHECK(!document.assets[3].animationLoc);
  }
  SUBCASE("The errors an animation operation raises") {
    const auto at{[](const std::string &body) {
      return firstErrorOf("asset h = \"h.glb\" { " + body + " }\nplace h\n");
    }};
    CHECK_CONTAINS(at("animation \"a\" animation \"b\""), "appears twice");
    CHECK_CONTAINS(at("animation { }"), "not a block");
    CHECK_CONTAINS(at("animation off once"), "takes no clip and no settings");
    CHECK_CONTAINS(at("animation once off"), "takes no clip and no settings");
    CHECK_CONTAINS(at("animation \"a\" 2"), "names two clips");
    CHECK_CONTAINS(at("animation 1.5"), "unsigned clip index");
    CHECK_CONTAINS(at("animation -1"), "unsigned clip index");
    CHECK_CONTAINS(at("animation speed 0"), "'speed' must be nonzero");
    CHECK_CONTAINS(at("animation offset fast"), "expected a number");
    CHECK_CONTAINS(at("frobnicate"), "animation");
    CHECK_CONTAINS(firstErrorOf("asset s = sphere { material m "
                                "animation \"x\" }\nplace s\n"),
                   "but this asset is a sphere");
    CHECK_CONTAINS(firstErrorOf("asset h = \"h.glb\"\n"
                                "place h animation \"x\"\n"),
                   "property of what is loaded");
    CHECK_CONTAINS(firstErrorOf("import \"h.glb\" { animation \"x\" }\n"),
                   "belongs on an 'asset' declaration");
  }
}

TEST_CASE("LayoutParser: the offset on a place") {
  LayoutDiagnostics diags{};
  SUBCASE("The one-line form, the block form, a group's place, a bulk place") {
    const auto document{parseOK(diags,
                                "asset hero = \"hero.glb\"\n"
                                "group rig { place hero offset 0.1 }\n"
                                "place hero translate 1 0 0 offset 0.4\n"
                                "place hero { offset 0.5 material a = b }\n"
                                "place hero * \"crowd.places\" offset 1\n"
                                "place hero\n")};
    REQUIRE(document.groups.size() == 1);
    REQUIRE(document.groups[0].placements.size() == 1);
    CHECK(document.groups[0].placements[0].animationOffset ==
          std::optional<float>(0.1f));
    REQUIRE(document.placements.size() == 4);
    CHECK(document.placements[0].animationOffset == std::optional<float>(0.4f));
    CHECK(document.placements[0].animationOffsetLoc);
    CHECK(document.placements[0].transform[3].x == doctest::Approx(1.0f));
    CHECK(document.placements[1].animationOffset == std::optional<float>(0.5f));
    CHECK(document.placements[1].overrides.size() == 1);
    CHECK(document.placements[2].placesPath == "crowd.places");
    CHECK(document.placements[2].animationOffset == std::optional<float>(1.0f));
    CHECK(!document.placements[3].animationOffset);
    CHECK(!document.placements[3].animationOffsetLoc);
  }
  SUBCASE("The errors an offset raises") {
    CHECK_CONTAINS(firstErrorOf("asset h = \"h.glb\"\n"
                                "place h offset 1 offset 2\n"),
                   "'offset' appears twice");
    CHECK_CONTAINS(firstErrorOf("asset h = \"h.glb\"\n"
                                "place h offset fast\n"),
                   "expected a number");
    CHECK_CONTAINS(firstErrorOf("import \"h.glb\" { offset 1 }\n"),
                   "'offset' is a place operation");
    CHECK_CONTAINS(firstErrorOf("asset h = \"h.glb\"\n"
                                "place h frobnicate\n"),
                   "offset");
  }
}
