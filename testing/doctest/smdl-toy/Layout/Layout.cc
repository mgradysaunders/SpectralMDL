#include "doctest.h"

#include <filesystem>
#include <fstream>
#include <string>
#include <vector>

#include "smdl/Support/Error.h"

#include "IO/CurvesFile.h"
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
    CHECK(layout.items[i].caster == caster[i]);
    CHECK(layout.items[i].light == light[i]);
    CHECK(layout.items[i].primitive.shape == PrimitiveSpec::Shape::SPHERE);
    // The caustic mark itself is asset-level: it never passes down.
    CHECK(layout.items[i].causticLight == (i == 3));
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
    CHECK(!layout.items[0].caster);
    CHECK(!layout.items[0].light);
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
