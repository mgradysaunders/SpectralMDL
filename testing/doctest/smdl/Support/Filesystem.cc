#include "doctest.h"

#include <filesystem>
#include <fstream>
#include <string>

#include "smdl/Support/Error.h"
#include "smdl/Support/Filesystem.h"

namespace fs = std::filesystem;

static void writeText(const fs::path &path, const char *text) {
  std::ofstream(path, std::ios::trunc) << text;
}

[[nodiscard]] static std::string readText(const fs::path &path) {
  auto stream{std::ifstream(path)};
  return std::string((std::istreambuf_iterator<char>(stream)),
                     std::istreambuf_iterator<char>());
}

TEST_CASE("Filesystem") {
  auto tmpDir{fs::temp_directory_path() / "smdl-filesystem-test"};
  fs::remove_all(tmpDir);
  fs::create_directories(tmpDir);
  const auto part{(tmpDir / "image.png.part").string()};
  const auto final{(tmpDir / "image.png").string()};
  SUBCASE("renameOnto replaces the destination") {
    writeText(final, "stale");
    writeText(part, "fresh");
    smdl::renameOnto(part, final);
    CHECK(readText(final) == "fresh");
    CHECK(!smdl::exists(part));
  }
  SUBCASE("renameOnto creates a destination that was not there") {
    writeText(part, "fresh");
    smdl::renameOnto(part, final);
    CHECK(readText(final) == "fresh");
  }
  SUBCASE("renameOnto throws, and leaves the destination alone") {
    writeText(final, "kept");
    CHECK_THROWS_AS(smdl::renameOnto((tmpDir / "absent.part").string(), final),
                    smdl::Error);
    // The whole point of the discipline: a failed write cannot destroy
    // what the destination already held.
    CHECK(readText(final) == "kept");
  }
  SUBCASE("tryRenameOnto reports the same outcomes without throwing") {
    writeText(part, "done=1");
    CHECK(smdl::tryRenameOnto(part, final));
    CHECK(readText(final) == "done=1");
    CHECK(!smdl::tryRenameOnto((tmpDir / "absent.part").string(), final));
    CHECK(readText(final) == "done=1");
  }
  fs::remove_all(tmpDir);
}
