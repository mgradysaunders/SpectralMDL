#include "Fixtures.h"

#include <cstddef>
#include <string>

#include "smdl/Support/Error.h"
#include "smdl/Support/Filesystem.h"

TEST_CASE("Filesystem: renaming one file onto another") {
  TempDir tmpDir{"filesystem"};
  const std::string part{(tmpDir / "image.png.part").string()};
  const std::string final{(tmpDir / "image.png").string()};
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

TEST_CASE("Filesystem: sniffing a file's magic") {
  TempDir tmpDir{"filesystem"};
  const std::byte magic[]{std::byte('G'), std::byte('U'), std::byte('N'),
                          std::byte('G')};
  const smdl::Span<const std::byte> span{magic, sizeof(magic)};
  CHECK(smdl::sniffMagic(tmpDir.write("match.bin", "GUNGrest").string(), span));
  CHECK(smdl::sniffMagic(tmpDir.write("exact.bin", "GUNG").string(), span));
  CHECK(
      !smdl::sniffMagic(tmpDir.write("other.bin", "GLTFrest").string(), span));
  CHECK(!smdl::sniffMagic(tmpDir.write("short.bin", "GUN").string(), span));
  CHECK(!smdl::sniffMagic((tmpDir / "absent.bin").string(), span));
}
