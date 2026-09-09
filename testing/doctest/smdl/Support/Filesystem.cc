#include "Fixtures.h"

#include <string>

#include "smdl/Support/Error.h"
#include "smdl/Support/Filesystem.h"

TEST_CASE("Filesystem: renaming one file onto another") {
  TempDir tmpDir{"filesystem"};
  const auto part{(tmpDir / "image.png.part").string()};
  const auto final{(tmpDir / "image.png").string()};
  SUBCASE("renameOnto replaces the destination") {
    tmpDir.write("image.png", "stale");
    tmpDir.write("image.png.part", "fresh");
    smdl::renameOnto(part, final);
    CHECK(tmpDir.read("image.png") == "fresh");
    CHECK(!smdl::exists(part));
  }
  SUBCASE("renameOnto creates a destination that was not there") {
    tmpDir.write("image.png.part", "fresh");
    smdl::renameOnto(part, final);
    CHECK(tmpDir.read("image.png") == "fresh");
  }
  SUBCASE("renameOnto throws, and leaves the destination alone") {
    tmpDir.write("image.png", "kept");
    CHECK_THROWS_AS(smdl::renameOnto((tmpDir / "absent.part").string(), final),
                    smdl::Error);
    // The whole point of the discipline: a failed write cannot destroy
    // what the destination already held.
    CHECK(tmpDir.read("image.png") == "kept");
  }
  SUBCASE("tryRenameOnto reports the same outcomes without throwing") {
    tmpDir.write("image.png.part", "done=1");
    CHECK(smdl::tryRenameOnto(part, final));
    CHECK(tmpDir.read("image.png") == "done=1");
    CHECK(!smdl::tryRenameOnto((tmpDir / "absent.part").string(), final));
    CHECK(tmpDir.read("image.png") == "done=1");
  }
}
