#include "doctest.h"

#include <filesystem>
#include <fstream>

#include "smdl/FileLocator.h"

namespace fs = std::filesystem;

static void touch(const fs::path &path) { std::ofstream(path) << '\n'; }

TEST_CASE("FileLocator") {
  auto tmpDir{fs::temp_directory_path() / "smdl-filelocator-test"};
  fs::remove_all(tmpDir);
  fs::create_directories(tmpDir / "dirA" / "sub");
  fs::create_directories(tmpDir / "dirB");
  touch(tmpDir / "dirA" / "plain.png");
  touch(tmpDir / "dirA" / "tex_1001.png");
  touch(tmpDir / "dirA" / "tex_1012.png");
  touch(tmpDir / "dirA" / "tex_1101.png");
  touch(tmpDir / "dirA" / "tex_01001.png");
  touch(tmpDir / "dirA" / "tex_10011.png");
  touch(tmpDir / "dirA" / "tex_999.png");
  touch(tmpDir / "dirA" / "notmytex_1001.png");
  touch(tmpDir / "dirA" / "1001.png");
  touch(tmpDir / "dirA" / "1002.png");
  touch(tmpDir / "dirA" / "uv_u0_v0.png");
  touch(tmpDir / "dirA" / "uv_u3_v2.png");
  touch(tmpDir / "dirA" / "t_u0_v0.exr");
  touch(tmpDir / "dirA" / "t_u1_v1.exr");
  touch(tmpDir / "dirA" / "t_u2_v1.exr");
  touch(tmpDir / "dirA" / "sub" / "n_1001.png");
  touch(tmpDir / "dirB" / "tex_1002.png");
  auto locator{smdl::FileLocator()};
  locator.setSearchPwd(false);
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
  SUBCASE("All tiles must reside in the same directory") {
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
  fs::remove_all(tmpDir);
}
