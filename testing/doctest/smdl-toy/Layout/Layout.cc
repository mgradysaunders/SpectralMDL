#include "doctest.h"

#include <filesystem>
#include <fstream>
#include <string>
#include <vector>

#include "smdl/Support/Error.h"

#include "IO/CurvesFile.h"
#include "IO/PlacesFile.h"
#include "Layout/Layout.h"
#include "Layout/LayoutTables.h"

namespace fs = std::filesystem;

// A scratch directory of layout files, removed on destruction.
class LayoutDir final {
public:
  LayoutDir() {
    fs::remove_all(root);
    fs::create_directories(root);
  }
  ~LayoutDir() { fs::remove_all(root); }

  std::string write(const std::string &name, const std::string &text) const {
    const auto path{(root / name).string()};
    std::ofstream file(path, std::ios::binary | std::ios::trunc);
    file << text;
    return path;
  }

  /// A minimal linear groom: one strand of two points.
  std::string writeCurves(const std::string &name) const {
    const auto path{(root / name).string()};
    auto curves{CurvesFile()};
    curves.basis = CurvesFile::Basis::LINEAR;
    curves.strandOffsets = {0, 2};
    curves.points = {float4(0.0f, 0.0f, 0.0f, 0.01f),
                     float4(0.0f, 0.0f, 1.0f, 0.01f)};
    writeCurvesFile(path, curves);
    return path;
  }

  fs::path root{fs::temp_directory_path() / "smdl-toy-layout-test"};
};

// Lower the entry file and require no errors.
static Layout lowerOK(LayoutDiagnostics &diags, const std::string &fileName) {
  auto layout{lowerLayout(diags, fileName)};
  if (diags.hasErrors()) MESSAGE(diags.renderAll(false));
  REQUIRE(!diags.hasErrors());
  return layout;
}

// Is there a diagnostic of `kind` whose message contains `fragment`?
static bool hasDiagnostic(const LayoutDiagnostics &diags,
                          LayoutDiagnostic::Kind kind,
                          const std::string &fragment) {
  for (const auto &diagnostic : diags.all())
    if (diagnostic.kind == kind &&
        diagnostic.message.find(fragment) != std::string::npos)
      return true;
  return false;
}

// The two marks compose by the same rules, so one document exercises
// both, spelled side by side: the asset's word is the default, a site's
// word overrides it, a group or a layout target passes the effective
// word down, and the innermost explicit word wins.
TEST_CASE("Layout lowering: the marks compose") {
  LayoutDir dir{};
  dir.write("sub.layout", "#smdl layout\n"
                          "asset inner = sphere { radius 1 material m }\n"
                          "place inner\n");
  dir.write("sub2.layout", "#smdl layout\n"
                           "asset inner = sphere { radius 1 material m }\n"
                           "place inner caster light\n");
  const auto entry{dir.write(
      "entry.layout",
      "#smdl layout\n"
      "asset ball = sphere { radius 1 material m caster light }\n"
      "asset plain = sphere { radius 1 material m }\n"
      "asset lamp = sphere { radius 1 material m caustic }\n"
      "group pair {\n"
      "  place ball translate 1 0 0\n"
      "  place plain translate 2 0 0\n"
      "}\n"
      "asset subA = \"sub.layout\"\n"
      "asset subB = \"sub.layout\" { caster light }\n"
      "asset subC = \"sub2.layout\"\n"
      "asset subD = \"sub.layout\" { caustic }\n"
      "place ball\n"                      // 0: the asset's own marks
      "place ball caster off light off\n" // 1: the site turns them off
      "place plain caster light\n"        // 2: the site turns them on
      "place lamp\n"                      // 3: caustic implies light
      "place pair\n"                      // 4, 5: the group inherits nothing
      "place pair caster light\n"         // 6, 7: the outer words mark both
      "place pair caster off light off\n" // 8, 9: the outer words clear both
      "place subA\n"                      // 10: the sub-layout's own (none)
      "place subB\n"                      // 11: the asset's marks pass down
      "place subA caster light\n"         // 12: the site's words pass down
      "import \"sub.layout\"\n"           // 13: an import, unmarked
      "import \"sub.layout\" { caster light }\n" // 14: the import's words
      "place subC caster off light off\n"        // 15: the inner words win
      "place subD\n"        // 16: caustic on a layout target
      "place plain light\n" // 17: one mark without the other
      )};
  LayoutDiagnostics diags{};
  const auto layout{lowerOK(diags, entry)};
  const std::vector<bool> caster{true, false, true,  false, true,  false,
                                 true, true,  false, false, false, true,
                                 true, false, true,  true,  false, false};
  const std::vector<bool> light{true, false, true,  true,  true,  false,
                                true, true,  false, false, false, true,
                                true, false, true,  true,  true,  true};
  REQUIRE(layout.items.size() == caster.size());
  for (size_t i = 0; i < caster.size(); i++) {
    CAPTURE(i);
    CHECK(layout.items[i].isCaster == caster[i]);
    CHECK(layout.items[i].isLight == light[i]);
    CHECK(layout.items[i].primitive.shape == PrimitiveSpec::Shape::SPHERE);
    // The caustic mark itself is asset-level: it never passes down.
    CHECK(layout.items[i].isCausticLight == (i == 3));
  }
  // The group's translations survive the composition.
  CHECK(layout.items[4].objectToWorld[3].x == doctest::Approx(1.0f));
  CHECK(layout.items[5].objectToWorld[3].x == doctest::Approx(2.0f));
}

TEST_CASE("Layout lowering: 'light off' cannot undo 'caustic'") {
  LayoutDir dir{};
  dir.write("sub.layout", "#smdl layout\n"
                          "asset inner = sphere { radius 1 material m }\n"
                          "place inner\n");
  SUBCASE("At the place") {
    const auto entry{dir.write(
        "entry.layout", "#smdl layout\n"
                        "asset lamp = sphere { radius 1 material m caustic }\n"
                        "place lamp light off\n")};
    LayoutDiagnostics diags{};
    const auto layout{lowerLayout(diags, entry)};
    CHECK(layout.items.empty());
    REQUIRE(diags.errorCount() == 1);
    const auto &error{diags.all().front()};
    CHECK(error.message.find("'light off' cannot apply to ") !=
          std::string::npos);
    CHECK(error.message.find("lamp") != std::string::npos);
    REQUIRE(error.notes.size() == 1);
    CHECK(error.notes[0].message.find("declared 'caustic' here") !=
          std::string::npos);
    CHECK(error.location.source->lineAndColumn(error.location.offset).lineNo ==
          3);
    CHECK(error.notes[0]
              .location.source->lineAndColumn(error.notes[0].location.offset)
              .lineNo == 2);
  }
  SUBCASE("Passed down from a group placement") {
    const auto entry{dir.write(
        "entry.layout", "#smdl layout\n"
                        "asset lamp = sphere { radius 1 material m caustic }\n"
                        "group g { place lamp }\n"
                        "place g light off\n")};
    LayoutDiagnostics diags{};
    const auto layout{lowerLayout(diags, entry)};
    CHECK(layout.items.empty());
    REQUIRE(diags.errorCount() == 1);
    CHECK(hasDiagnostic(diags, LayoutDiagnostic::Kind::ERROR,
                        "'light off' cannot apply to "));
  }
  SUBCASE("On a layout target") {
    const auto entry{dir.write("entry.layout",
                               "#smdl layout\n"
                               "asset sub = \"sub.layout\" { caustic }\n"
                               "place sub light off\n")};
    LayoutDiagnostics diags{};
    const auto layout{lowerLayout(diags, entry)};
    CHECK(layout.items.empty());
    REQUIRE(diags.errorCount() == 1);
    CHECK(hasDiagnostic(diags, LayoutDiagnostic::Kind::ERROR,
                        "'light off' cannot apply to "));
  }
}

TEST_CASE("Layout lowering: the marks are refused on a groom") {
  LayoutDir dir{};
  dir.writeCurves("hair.curves");
  const char *words[]{"caster", "light"};
  for (const char *word : words) {
    CAPTURE(word);
    const auto refused{std::string("'") + word +
                       "' applies to a mesh file or a shape"};
    SUBCASE("On the asset") {
      const auto entry{dir.write("entry.layout",
                                 std::string("#smdl layout\n"
                                             "asset hair = \"hair.curves\" { "
                                             "material m ") +
                                     word + " }\nplace hair\n")};
      LayoutDiagnostics diags{};
      const auto layout{lowerLayout(diags, entry)};
      CHECK(layout.items.empty());
      REQUIRE(diags.errorCount() == 1);
      CHECK(hasDiagnostic(diags, LayoutDiagnostic::Kind::ERROR, refused));
    }
    SUBCASE("On the import") {
      const auto entry{dir.write(
          "entry.layout", std::string("#smdl layout\n"
                                      "import \"hair.curves\" { material m ") +
                              word + " }\n")};
      LayoutDiagnostics diags{};
      const auto layout{lowerLayout(diags, entry)};
      CHECK(layout.items.empty());
      REQUIRE(diags.errorCount() == 1);
      CHECK(hasDiagnostic(diags, LayoutDiagnostic::Kind::ERROR, refused));
    }
  }
  SUBCASE("Unmarked, the groom lowers") {
    const auto entry{dir.write("entry.layout",
                               "#smdl layout\n"
                               "asset hair = \"hair.curves\" { material m }\n"
                               "place hair\n")};
    LayoutDiagnostics diags{};
    const auto layout{lowerOK(diags, entry)};
    REQUIRE(layout.items.size() == 1);
    CHECK(layout.items[0].curves.active);
    CHECK(!layout.items[0].isCaster);
    CHECK(!layout.items[0].isLight);
    CHECK(layout.items[0].materials.all == "m");
  }
}

TEST_CASE("Layout lowering: marks on a light placement warn") {
  LayoutDir dir{};
  const auto entry{dir.write("entry.layout", "#smdl layout\n"
                                             "light lamp = point\n"
                                             "place lamp caster light\n")};
  LayoutDiagnostics diags{};
  const auto layout{lowerOK(diags, entry)};
  CHECK(layout.items.empty());
  REQUIRE(layout.lights.size() == 1);
  CHECK(layout.lights[0].decl.kind == LayoutLightDecl::Kind::POINT);
  CHECK(diags.warningCount() == 2);
  CHECK(hasDiagnostic(diags, LayoutDiagnostic::Kind::WARNING,
                      "'caster' on the light"));
  CHECK(hasDiagnostic(diags, LayoutDiagnostic::Kind::WARNING,
                      "'light' on the light"));
}

TEST_CASE("Layout packing: a per-place mark has no record to live in") {
  LayoutDir dir{};
  const auto output{(dir.root / "out.places").string()};
  const char *words[]{"caster", "light"};
  for (const char *word : words) {
    CAPTURE(word);
    const auto entry{dir.write(
        "entry.layout", std::string("#smdl layout\n"
                                    "asset ball = sphere { radius 1 material m "
                                    "}\nplace ball ") +
                            word + " off\n")};
    try {
      packPlaces(entry, output);
      FAIL("expected the pack to be refused");
    } catch (const smdl::Error &error) {
      CAPTURE(error.message);
      CHECK(error.message.find(std::string("a '") + word +
                               "' override on a place has no record") !=
            std::string::npos);
    }
  }
  // Without a mark the same place packs.
  const auto entry{dir.write("entry.layout",
                             "#smdl layout\n"
                             "asset ball = sphere { radius 1 material m }\n"
                             "place ball translate 1 2 3\n")};
  packPlaces(entry, output);
  CHECK(fs::exists(output));
}

TEST_CASE("Layout lowering: a shape light keeps the placement's scale") {
  LayoutDir dir{};
  const auto entry{dir.write("entry.layout",
                             "#smdl layout\n"
                             "light panel = rect { size 2 1 }\n"
                             "place panel scale 3 2 1 translate 0 0 5\n")};
  LayoutDiagnostics diags{};
  const auto layout{lowerOK(diags, entry)};
  CHECK(diags.empty());
  REQUIRE(layout.lights.size() == 1);
  const auto &light{layout.lights[0]};
  CHECK(light.decl.kind == LayoutLightDecl::Kind::RECT);
  CHECK(light.decl.size.x == doctest::Approx(2.0f));
  CHECK(light.decl.size.y == doctest::Approx(1.0f));
  // The columns carry the scale unnormalized: the light's extent is the
  // declared extent through the placement.
  CHECK(length(float3(light.lightToWorld[0])) == doctest::Approx(3.0f));
  CHECK(length(float3(light.lightToWorld[1])) == doctest::Approx(2.0f));
  CHECK(length(float3(light.lightToWorld[2])) == doctest::Approx(1.0f));
  CHECK(light.lightToWorld[3].z == doctest::Approx(5.0f));
}

TEST_CASE("Layout lowering: only the entry file's time takes effect") {
  LayoutDir dir{};
  dir.write("inner.layout", "#smdl layout\n"
                            "time { base 9 shutter 1 }\n"
                            "asset ball = sphere { material m }\n"
                            "place ball\n");
  SUBCASE("An imported layout's time warns and is ignored") {
    const auto entry{dir.write("entry.layout", "#smdl layout\n"
                                               "time { base 2.5 }\n"
                                               "import \"inner.layout\"\n")};
    LayoutDiagnostics diags{};
    const auto layout{lowerOK(diags, entry)};
    REQUIRE(layout.items.size() == 1);
    REQUIRE(layout.time.base);
    CHECK(*layout.time.base == doctest::Approx(2.5f));
    CHECK(!layout.time.shutter);
    CHECK(diags.warningCount() == 1);
    CHECK(hasDiagnostic(diags, LayoutDiagnostic::Kind::WARNING,
                        "'time' of an imported layout is ignored"));
  }
  SUBCASE("An entry without the directive leaves both unset") {
    const auto entry{dir.write("entry.layout", "#smdl layout\n"
                                               "asset ball = sphere { "
                                               "material m }\n"
                                               "place ball\n")};
    LayoutDiagnostics diags{};
    const auto layout{lowerOK(diags, entry)};
    CHECK(!layout.time.base);
    CHECK(!layout.time.shutter);
  }
}

TEST_CASE("Layout lowering: motion composes pairwise") {
  LayoutDir dir{};
  dir.write("sub.layout", "#smdl layout\n"
                          "asset inner = sphere { material m }\n"
                          "place inner translate 0 0 1\n");
  {
    // Two records, one unit apart along y.
    auto places{PlacesFile()};
    const auto translate{[](float x, float y, float z) {
      auto xf{float4x4(1.0f)};
      xf[3] = float4(x, y, z, 1.0f);
      return xf;
    }};
    places.transforms = {translate(0.0f, 1.0f, 0.0f),
                         translate(0.0f, 2.0f, 0.0f)};
    writePlacesFile((dir.root / "pair.places").string(), places);
  }
  const auto entry{dir.write(
      "entry.layout",
      "#smdl layout\n"
      "asset ball = sphere { radius 1 material m translate 0 0 1 }\n"
      "light lamp = point { power 10 }\n"
      "asset sub = \"sub.layout\"\n"
      "group rig {\n"
      "  place ball translate 1 0 0\n"
      "  place ball translate 2 0 0 motion { translate 2 0 0 rotate_z 90 }\n"
      "  place lamp translate 0 0 5\n"
      "}\n"
      "place ball\n"                                              // 0
      "place ball translate 5 0 0 motion { translate 6 0 0 }\n"   // 1
      "place ball translate 5 0 0 motion { translate 5 0 0 }\n"   // 2
      "place rig translate 10 0 0 motion { translate 11 0 0 }\n"  // 3, 4
      "place sub translate 20 0 0 motion { translate 21 0 0 }\n"  // 5
      "place ball * \"pair.places\" motion { translate 0 0 1 }\n" // 6
      "place ball * \"pair.places\"\n"                            // 7
      )};
  LayoutDiagnostics diags{};
  const auto layout{lowerOK(diags, entry)};
  CHECK(diags.empty());
  REQUIRE(layout.items.size() == 8);
  const auto translationOf{[](const float4x4 &xf) { return float3(xf[3]); }};
  const auto near{[](const float3 &a, float x, float y, float z) {
    return a.x == doctest::Approx(x) && a.y == doctest::Approx(y) &&
           a.z == doctest::Approx(z);
  }};
  // 0: static, the asset's correction alone.
  CHECK(near(translationOf(layout.items[0].objectToWorld), 0, 0, 1));
  CHECK(!layout.items[0].objectToWorldShut);
  // 1: the block's shut key over the correction.
  CHECK(near(translationOf(layout.items[1].objectToWorld), 5, 0, 1));
  REQUIRE(layout.items[1].objectToWorldShut);
  CHECK(near(translationOf(*layout.items[1].objectToWorldShut), 6, 0, 1));
  // 2: a block restating the open key lowers static.
  CHECK(near(translationOf(layout.items[2].objectToWorld), 5, 0, 1));
  CHECK(!layout.items[2].objectToWorldShut);
  // 3: a static member moves rigidly with its group.
  CHECK(near(translationOf(layout.items[3].objectToWorld), 11, 0, 1));
  REQUIRE(layout.items[3].objectToWorldShut);
  CHECK(near(translationOf(*layout.items[3].objectToWorldShut), 12, 0, 1));
  // 4: a moving member composes its shut key under the group's: the
  // correction, then the member's translate and turn, then the group's.
  CHECK(near(translationOf(layout.items[4].objectToWorld), 12, 0, 1));
  REQUIRE(layout.items[4].objectToWorldShut);
  CHECK(near(translationOf(*layout.items[4].objectToWorldShut), 11, 2, 1));
  CHECK(near(float3((*layout.items[4].objectToWorldShut)[0]), 0, 1, 0));
  // 5: a layout target recurses under both keys.
  CHECK(near(translationOf(layout.items[5].objectToWorld), 20, 0, 1));
  REQUIRE(layout.items[5].objectToWorldShut);
  CHECK(near(translationOf(*layout.items[5].objectToWorldShut), 21, 0, 1));
  // 6: a bulk place's block moves every record; 7: a static scatter
  // carries no shut keys.
  REQUIRE(layout.items[6].batchXfs.size() == 2);
  REQUIRE(layout.items[6].batchXfsShut.size() == 2);
  CHECK(near(translationOf(layout.items[6].batchXfs[1]), 0, 2, 1));
  CHECK(near(translationOf(layout.items[6].batchXfsShut[1]), 0, 2, 2));
  REQUIRE(layout.items[7].batchXfs.size() == 2);
  CHECK(layout.items[7].batchXfsShut.empty());
  CHECK(near(translationOf(layout.items[7].batchXfs[0]), 0, 1, 1));
  // The group's light moves with it.
  REQUIRE(layout.lights.size() == 1);
  CHECK(near(translationOf(layout.lights[0].lightToWorld), 10, 0, 5));
  REQUIRE(layout.lights[0].lightToWorldShut);
  CHECK(near(translationOf(*layout.lights[0].lightToWorldShut), 11, 0, 5));
}

TEST_CASE("Layout lowering: the animation spec reaches the item with the "
          "path's offsets") {
  LayoutDir dir{};
  dir.write("hero.obj", "v 0 0 0\nv 1 0 0\nv 0 1 0\nf 1 2 3\n");
  dir.write("sub.layout",
            "#smdl layout\n"
            "asset inner = \"hero.obj\" { animation \"walk\" offset 0.5 }\n"
            "place inner offset 0.25\n");
  dir.write("sub2.layout", "#smdl layout\nimport \"hero.obj\"\n");
  {
    auto places{PlacesFile()};
    places.transforms = {float4x4(1.0f), float4x4(1.0f)};
    places.transforms[1][3] = float4(0.0f, 1.0f, 0.0f, 1.0f);
    writePlacesFile((dir.root / "pair.places").string(), places);
  }
  const auto entry{
      dir.write("entry.layout",
                "#smdl layout\n"
                "asset hero = \"hero.obj\" { animation \"walk\" offset 0.25 }\n"
                "asset plain = \"hero.obj\"\n"
                "asset still = \"hero.obj\" { animation off }\n"
                "asset sub = \"sub.layout\"\n"
                "asset sub2 = \"sub2.layout\"\n"
                "group rig { place hero offset 0.1 }\n"
                "place hero\n"                            // 0
                "place hero offset 0.5\n"                 // 1
                "place rig offset 1\n"                    // 2
                "place plain offset 2\n"                  // 3
                "place still offset 3\n"                  // 4
                "place sub offset 1\n"                    // 5
                "place hero * \"pair.places\" offset 2\n" // 6
                "import \"hero.obj\"\n"                   // 7
                "place sub2 offset 4\n"                   // 8
                "place hero\n"                            // 9
                )};
  LayoutDiagnostics diags{};
  const auto layout{lowerOK(diags, entry)};
  CHECK(diags.empty());
  REQUIRE(layout.items.size() == 10);
  const auto offsetOf{
      [&](size_t i) { return layout.items[i].animation.offset; }};
  // 0: the asset's own offset; 1: the place's added; 2: the group's and
  // the member's added; 3: an asset that said nothing takes the place's
  // offset over the default spec.
  CHECK(layout.items[0].animation.clipName == "walk");
  CHECK(offsetOf(0) == doctest::Approx(0.25f));
  CHECK(offsetOf(1) == doctest::Approx(0.75f));
  CHECK(offsetOf(2) == doctest::Approx(1.35f));
  CHECK(layout.items[3].animation.clipName.empty());
  CHECK(offsetOf(3) == doctest::Approx(2.0f));
  CHECK(layout.items[3].animation.key() == "offset 2");
  // 4: off stays off whatever the path adds.
  CHECK(layout.items[4].animation.off);
  CHECK(layout.items[4].animation.key() == "off");
  // 5: a layout target passes the offset down to the meshes inside.
  CHECK(layout.items[5].animation.clipName == "walk");
  CHECK(offsetOf(5) == doctest::Approx(1.75f));
  // 6: every record of a scatter shares the place's offset.
  REQUIRE(layout.items[6].batchXfs.size() == 2);
  CHECK(offsetOf(6) == doctest::Approx(2.25f));
  // 7: an anonymous import at the top carries the default spec; 8: one
  // under a placed layout carries the path's offset.
  CHECK(layout.items[7].animation.key().empty());
  CHECK(layout.items[8].animation.key() == "offset 4");
  // The key tells two phases apart and equates two equal ones.
  CHECK(layout.items[0].animation.key() != layout.items[1].animation.key());
  CHECK(layout.items[0].animation.key() == layout.items[9].animation.key());
}

TEST_CASE("Layout lowering: 'animation' is refused on a groom and a layout") {
  LayoutDir dir{};
  dir.writeCurves("fur.curves");
  dir.write("sub.layout", "#smdl layout\n"
                          "asset inner = sphere { material m }\n"
                          "place inner\n");
  SUBCASE("On a groom") {
    const auto entry{dir.write(
        "entry.layout", "#smdl layout\n"
                        "asset fur = \"fur.curves\" { material m animation "
                        "\"x\" }\n"
                        "place fur\n")};
    LayoutDiagnostics diags{};
    (void)lowerLayout(diags, entry);
    CHECK(hasDiagnostic(diags, LayoutDiagnostic::Kind::ERROR,
                        "'animation' applies to a mesh file"));
    CHECK(hasDiagnostic(diags, LayoutDiagnostic::Kind::ERROR, "curves file"));
  }
  SUBCASE("On a layout") {
    const auto entry{dir.write("entry.layout",
                               "#smdl layout\n"
                               "asset sub = \"sub.layout\" { animation 0 }\n"
                               "place sub\n")};
    LayoutDiagnostics diags{};
    (void)lowerLayout(diags, entry);
    CHECK(hasDiagnostic(diags, LayoutDiagnostic::Kind::ERROR,
                        "'animation' applies to a mesh file"));
    CHECK(hasDiagnostic(diags, LayoutDiagnostic::Kind::ERROR, "is a layout"));
  }
}
