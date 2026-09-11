#include "CompileFixtures.h"

#include <filesystem>
#include <string>
#include <vector>

#include "smdl/Support/FileLocator.h"

namespace fs = std::filesystem;

namespace {
// An empty file at `name`: the locator matches on names alone.
void touch(const TempDir &dir, std::string_view name) { dir.write(name, "\n"); }

// The list separator of 'SMDL_DEFAULT_SEARCH_DIRS', as 'PATH' has it.
#if defined(_WIN32)
constexpr char SEPARATOR{';'};
#else
constexpr char SEPARATOR{':'};
#endif
} // namespace

TEST_CASE("FileLocator: the tile markers and the search order") {
  TempDir tmpDir{"filelocator"};
  fs::create_directories(tmpDir / "dirA" / "sub");
  fs::create_directories(tmpDir / "dirB");
  touch(tmpDir, "dirA/plain.png");
  touch(tmpDir, "dirA/tex_1001.png");
  touch(tmpDir, "dirA/tex_1012.png");
  touch(tmpDir, "dirA/tex_1101.png");
  touch(tmpDir, "dirA/tex_01001.png");
  touch(tmpDir, "dirA/tex_10011.png");
  touch(tmpDir, "dirA/tex_999.png");
  touch(tmpDir, "dirA/notmytex_1001.png");
  touch(tmpDir, "dirA/1001.png");
  touch(tmpDir, "dirA/1002.png");
  touch(tmpDir, "dirA/uv_u0_v0.png");
  touch(tmpDir, "dirA/uv_u3_v2.png");
  touch(tmpDir, "dirA/t_u0_v0.exr");
  touch(tmpDir, "dirA/t_u1_v1.exr");
  touch(tmpDir, "dirA/t_u2_v1.exr");
  touch(tmpDir, "dirA/sub/n_1001.png");
  touch(tmpDir, "dirB/tex_1002.png");
  auto locator{smdl::FileLocator()};
  locator.setSearchPwd(false);
  locator.setSearchDefaultDirs(false);
  REQUIRE(locator.addSearchDir((tmpDir / "dirA").string()));
  REQUIRE(locator.addSearchDir((tmpDir / "dirB").string()));
  SUBCASE("Without tile marker") {
    auto images{locator.locateImages("plain.png")};
    REQUIRE(images.size() == 1);
    CHECK(images[0].tileIndexU == 0);
    CHECK(images[0].tileIndexV == 0);
    CHECK(fs::path(images[0].path).filename() == "plain.png");
    CHECK(locator.locateImages("missing.png").empty());
  }
  SUBCASE("With '<UDIM>' tile marker") {
    // Must accept the well-formed 4-digit UDIM names, including
    // 1101 and beyond, and must reject near-misses: names that only
    // end with the pattern, extra digits, leading zeros, and numbers
    // below 1001.
    auto images{locator.locateImages("tex_<UDIM>.png")};
    REQUIRE(images.size() == 3);
    CHECK(images[0].tileIndexU == 0);
    CHECK(images[0].tileIndexV == 0);
    CHECK(fs::path(images[0].path).filename() == "tex_1001.png");
    CHECK(images[1].tileIndexU == 0);
    CHECK(images[1].tileIndexV == 10);
    CHECK(fs::path(images[1].path).filename() == "tex_1101.png");
    CHECK(images[2].tileIndexU == 1);
    CHECK(images[2].tileIndexV == 1);
    CHECK(fs::path(images[2].path).filename() == "tex_1012.png");
  }
  SUBCASE("With '<UDIM>' tile marker at the beginning") {
    auto images{locator.locateImages("<UDIM>.png")};
    REQUIRE(images.size() == 2);
    CHECK(fs::path(images[0].path).filename() == "1001.png");
    CHECK(fs::path(images[1].path).filename() == "1002.png");
  }
  SUBCASE("With '<UVTILE0>' tile marker") {
    auto images{locator.locateImages("uv<UVTILE0>.png")};
    REQUIRE(images.size() == 2);
    CHECK(images[0].tileIndexU == 0);
    CHECK(images[0].tileIndexV == 0);
    CHECK(images[1].tileIndexU == 3);
    CHECK(images[1].tileIndexV == 2);
  }
  SUBCASE("With '<UVTILE1>' tile marker") {
    // Must normalize the 1-based tile indexes to be 0-based, and
    // must not match '_u0_v0'.
    auto images{locator.locateImages("t<UVTILE1>.exr")};
    REQUIRE(images.size() == 2);
    CHECK(images[0].tileIndexU == 0);
    CHECK(images[0].tileIndexV == 0);
    CHECK(fs::path(images[0].path).filename() == "t_u1_v1.exr");
    CHECK(images[1].tileIndexU == 1);
    CHECK(images[1].tileIndexV == 0);
    CHECK(fs::path(images[1].path).filename() == "t_u2_v1.exr");
  }
  SUBCASE("Every tile must reside in the same directory") {
    // Both 'dirA' and 'dirB' match the pattern, but 'dirA' is
    // scanned first, so 'dirB/tex_1002.png' must not appear.
    for (auto &image : locator.locateImages("tex_<UDIM>.png")) {
      CHECK(fs::path(image.path).parent_path().filename() == "dirA");
    }
  }
  SUBCASE("With directory portion in the pattern") {
    auto images{locator.locateImages("sub/n_<UDIM>.png")};
    REQUIRE(images.size() == 1);
    CHECK(fs::path(images[0].path).filename() == "n_1001.png");
  }
  SUBCASE("With absolute pattern") {
    auto images{
        locator.locateImages((tmpDir / "dirB" / "tex_<UDIM>.png").string())};
    REQUIRE(images.size() == 1);
    CHECK(images[0].tileIndexU == 1);
    CHECK(images[0].tileIndexV == 0);
    CHECK(fs::path(images[0].path).filename() == "tex_1002.png");
  }
  SUBCASE("A priority directory outranks every other") {
    fs::create_directories(tmpDir / "dirP");
    touch(tmpDir, "dirP/plain.png");
    touch(tmpDir, "dirP/tex_1005.png");
    auto priorityDirs{std::vector<std::string>{(tmpDir / "dirP").string()}};
    // Without priority dirs, 'plain.png' resolves in 'dirA', even more so
    // with 'dirA' as the relative-to anchor. With priority dirs, 'dirP'
    // must win over both.
    auto located{locator.locate("plain.png")};
    REQUIRE(located);
    CHECK(fs::path(*located).parent_path().filename() == "dirA");
    located = locator.locate("plain.png", (tmpDir / "dirA").string(),
                             smdl::FileLocator::REGULAR_FILES, priorityDirs);
    REQUIRE(located);
    CHECK(fs::path(*located).parent_path().filename() == "dirP");
    // The first directory that matches a tile pattern provides all of
    // the results, so the priority dir must eclipse the tiles in 'dirA'.
    auto images{locator.locateImages("tex_<UDIM>.png",
                                     (tmpDir / "dirA").string(), priorityDirs)};
    REQUIRE(images.size() == 1);
    CHECK(fs::path(images[0].path).filename() == "tex_1005.png");
    // A priority dir must not disable the regular search dirs: a file
    // that only exists in 'dirB' must still resolve.
    located = locator.locate("tex_1002.png", {},
                             smdl::FileLocator::REGULAR_FILES, priorityDirs);
    REQUIRE(located);
    CHECK(fs::path(*located).parent_path().filename() == "dirB");
  }
}

TEST_CASE("FileLocator: the default search directories") {
  TempDir tmpDir{"filelocator-default"};
  touch(tmpDir, "added/both.png");
  touch(tmpDir, "added/tex_1001.png");
  touch(tmpDir, "default1/both.png");
  touch(tmpDir, "default1/only_default.png");
  touch(tmpDir, "default1/tex_1002.png");
  touch(tmpDir, "default1/uv_u0_v0.png");
  touch(tmpDir, "default2/only_default.png");
  touch(tmpDir, "default2/only_default2.png");
  // An empty entry and one that is not a directory sit between the two
  // that are, and the list ends in a separator.
  const ScopedEnv defaultDirs{"SMDL_DEFAULT_SEARCH_DIRS",
                              (tmpDir / "default1").string() + SEPARATOR +
                                  SEPARATOR + (tmpDir / "missing").string() +
                                  SEPARATOR + (tmpDir / "default2").string() +
                                  SEPARATOR};
  auto locator{smdl::FileLocator()};
  locator.setSearchPwd(false);
  REQUIRE(locator.addSearchDir((tmpDir / "added").string()));
  SUBCASE("Rank after every other search directory") {
    auto located{locator.locate("both.png")};
    REQUIRE(located);
    CHECK(fs::path(*located).parent_path().filename() == "added");
    located = locator.locate("only_default.png");
    REQUIRE(located);
    CHECK(fs::path(*located).parent_path().filename() == "default1");
    located = locator.locate("only_default2.png");
    REQUIRE(located);
    CHECK(fs::path(*located).parent_path().filename() == "default2");
  }
  SUBCASE("Keep their order and skip empty or missing entries") {
    auto searchDirs{locator.getSearchDirs()};
    REQUIRE(searchDirs.size() == 3);
    CHECK(fs::path(searchDirs[0]) == fs::weakly_canonical(tmpDir / "added"));
    CHECK(fs::path(searchDirs[1]) == fs::weakly_canonical(tmpDir / "default1"));
    CHECK(fs::path(searchDirs[2]) == fs::weakly_canonical(tmpDir / "default2"));
  }
  SUBCASE("Supply tiles only when no earlier directory matches") {
    // Both 'added' and 'default1' match the UDIM pattern, so 'added'
    // provides every tile; only 'default1' matches the UVTILE0 pattern.
    auto images{locator.locateImages("tex_<UDIM>.png")};
    REQUIRE(images.size() == 1);
    CHECK(fs::path(images[0].path).filename() == "tex_1001.png");
    images = locator.locateImages("uv<UVTILE0>.png");
    REQUIRE(images.size() == 1);
    CHECK(fs::path(images[0].path).parent_path().filename() == "default1");
  }
  SUBCASE("Are skipped when disabled") {
    locator.setSearchDefaultDirs(false);
    CHECK(locator.getSearchDirs().size() == 1);
    CHECK_FALSE(locator.locate("only_default.png"));
  }
}

TEST_CASE("FileLocator: a default search directory that is not one") {
  TempDir tmpDir{"filelocator-not-a-dir"};
  touch(tmpDir, "a-file.txt");
  // Names used by no other test, since each is reported once per process.
  const auto missing{(tmpDir / "missing-for-the-warning").string()};
  const auto file{(tmpDir / "a-file.txt").string()};
  const ScopedEnv defaultDirs{"SMDL_DEFAULT_SEARCH_DIRS",
                              missing + SEPARATOR + file};
  const CollectedLog logged{"SMDL_DEFAULT_SEARCH_DIRS"};
  auto locator{smdl::FileLocator()};
  // The variable is read on every lookup, and each entry is reported
  // once however many lookups read it.
  for (int i = 0; i < 3; i++) CHECK(locator.getSearchDirs().size() == 1);
  REQUIRE(logged.messages().size() == 2);
  CHECK(logged.warningCount() == 2);
  CHECK(logged.messages()[0] == "SMDL_DEFAULT_SEARCH_DIRS names '" + missing +
                                    "', which is not a directory");
  CHECK_CONTAINS(logged.messages()[1], "'" + file + "'");
}
