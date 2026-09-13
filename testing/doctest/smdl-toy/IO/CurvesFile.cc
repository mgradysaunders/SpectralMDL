#include "Fixtures.h"

#include <fstream>
#include <string>

#include "smdl/Support/Error.h"

#include "IO/CurvesFile.h"

namespace {

// A groom with two strands of different lengths, so that the offset
// table is exercised rather than being a uniform stride, and with radii
// and coordinates that are all distinct, so a transposed or truncated
// read shows up as a wrong value rather than as a coincidence.
[[nodiscard]] CurvesFile makeGroom(CurvesFile::Basis basis) {
  CurvesFile curves{};
  curves.basis = basis;
  const size_t counts[2]{6, 4};
  curves.strandOffsets.push_back(0);
  for (const size_t count : counts) {
    for (size_t i = 0; i < count; i++) {
      const float t{float(curves.points.size())};
      curves.points.push_back(
          float4(0.125f * t, -0.5f + t, 2.0f * t, 0.01f + 0.001f * t));
    }
    curves.strandOffsets.push_back(uint32_t(curves.points.size()));
  }
  return curves;
}

// A groom of the same shape with `keyCount` keys: every stored entry
// distinct, so a transposed or wrongly strided read shows up as a wrong
// value rather than as a coincidence.
[[nodiscard]] CurvesFile makeMovingGroom(uint32_t keyCount) {
  CurvesFile groom{makeGroom(CurvesFile::Basis::LINEAR)};
  const std::vector<float4> stills{groom.points};
  groom.keyTimes.clear();
  for (uint32_t key = 0; key < keyCount; key++)
    groom.keyTimes.push_back(0.25f * float(key));
  groom.points.clear();
  for (const auto &point : stills)
    for (uint32_t key = 0; key < keyCount; key++)
      groom.points.push_back(point + float4(100.0f * float(key), 0, 0, 0));
  return groom;
}

void checkSame(const CurvesFile &read, const CurvesFile &written) {
  CHECK(read.version == 1);
  CHECK(read.basis == written.basis);
  CHECK(read.strandOffsets == written.strandOffsets);
  CHECK(read.keyTimes == written.keyTimes);
  REQUIRE(read.points.size() == written.points.size());
  for (size_t i = 0; i < read.points.size(); i++) {
    CAPTURE(i);
    CHECK(read.points[i].x == written.points[i].x);
    CHECK(read.points[i].y == written.points[i].y);
    CHECK(read.points[i].z == written.points[i].z);
    CHECK(read.points[i].w == written.points[i].w);
  }
}

} // namespace

TEST_CASE("CurvesFile: round trip") {
  TempDir tmpDir{"toy-curves"};
  const std::string fileName{(tmpDir / "groom.curves").string()};
  SUBCASE("Every basis survives") {
    for (const auto basis :
         {CurvesFile::Basis::LINEAR, CurvesFile::Basis::BSPLINE,
          CurvesFile::Basis::CATMULL_ROM}) {
      CAPTURE(uint16_t(basis));
      const CurvesFile groom{makeGroom(basis)};
      writeCurvesFile(fileName, groom);
      const CurvesFile read{readCurvesFile(fileName)};
      checkSame(read, groom);
      CHECK(!read.hasRootUVs());
      CHECK(read.strandCount() == 2);
    }
  }
  SUBCASE("The root UV column survives") {
    CurvesFile groom{makeGroom(CurvesFile::Basis::CATMULL_ROM)};
    groom.rootUVs = {float2(0.25f, 0.75f), float2(-1.0f, 2.0f)};
    writeCurvesFile(fileName, groom);
    const CurvesFile read{readCurvesFile(fileName)};
    checkSame(read, groom);
    REQUIRE(read.hasRootUVs());
    REQUIRE(read.rootUVs.size() == 2);
    CHECK(read.rootUVs[0].x == 0.25f);
    CHECK(read.rootUVs[0].y == 0.75f);
    CHECK(read.rootUVs[1].x == -1.0f);
    CHECK(read.rootUVs[1].y == 2.0f);
  }
  SUBCASE("Writing the same groom twice gives the same bytes") {
    // The format is the host's own bytes, so a round trip that agrees on
    // values must also agree on the file, which is what lets a '.curves'
    // be content-hashed or cached.
    const CurvesFile groom{makeGroom(CurvesFile::Basis::BSPLINE)};
    writeCurvesFile(fileName, groom);
    const auto readBytes{[&] {
      std::ifstream file(fileName, std::ios::binary);
      return std::string(std::istreambuf_iterator<char>(file), {});
    }};
    const std::string first{readBytes()};
    writeCurvesFile(fileName, readCurvesFile(fileName));
    CHECK(readBytes() == first);
  }
  SUBCASE("A truncated file is refused") {
    writeCurvesFile(fileName, makeGroom(CurvesFile::Basis::LINEAR));
    std::string bytes{};
    {
      std::ifstream file(fileName, std::ios::binary);
      bytes.assign(std::istreambuf_iterator<char>(file), {});
    }
    REQUIRE(bytes.size() > 40);
    {
      std::ofstream file(fileName, std::ios::binary | std::ios::trunc);
      file.write(bytes.data(), 40);
    }
    CHECK_THROWS_AS((void)readCurvesFile(fileName), smdl::Error);
  }
  SUBCASE("A file that is not a groom is refused") {
    {
      std::ofstream file(fileName, std::ios::binary | std::ios::trunc);
      file << "place rock\n";
    }
    CHECK_THROWS_AS((void)readCurvesFile(fileName), smdl::Error);
  }
  SUBCASE("A groom the basis cannot support is refused on the way out") {
    // The writer checks the same shape the reader does, so a bad groom
    // cannot reach disk. B-spline is the basis with a real floor: it
    // needs a whole four-point window, where Catmull-Rom takes two and
    // the loader pads the phantom ends itself.
    CurvesFile groom{};
    groom.points.assign(3, float4(0.0f, 0.0f, 0.0f, 0.01f));
    groom.strandOffsets = {0, 3};
    groom.basis = CurvesFile::Basis::BSPLINE;
    CHECK_THROWS_AS(writeCurvesFile(fileName, groom), smdl::Error);
    groom.basis = CurvesFile::Basis::CATMULL_ROM;
    CHECK_NOTHROW(writeCurvesFile(fileName, groom));
  }
  SUBCASE("An offset table that does not add up is refused") {
    CurvesFile groom{makeGroom(CurvesFile::Basis::LINEAR)};
    SUBCASE("Not ending at the point count") {
      groom.strandOffsets.back()--;
      CHECK_THROWS_AS(writeCurvesFile(fileName, groom), smdl::Error);
    }
    SUBCASE("Not increasing") {
      groom.strandOffsets[1] = groom.strandOffsets[2];
      CHECK_THROWS_AS(writeCurvesFile(fileName, groom), smdl::Error);
    }
    SUBCASE("A root UV column of the wrong length") {
      groom.rootUVs = {float2(0.0f, 0.0f)};
      CHECK_THROWS_AS(writeCurvesFile(fileName, groom), smdl::Error);
    }
  }
}

TEST_CASE("CurvesFile: keys") {
  TempDir tmpDir{"toy-curves-keys"};
  const std::string fileName{(tmpDir / "groom.curves").string()};
  SUBCASE("A still groom has one key and never consults its time") {
    const CurvesFile groom{makeGroom(CurvesFile::Basis::BSPLINE)};
    CHECK(groom.keyCount() == 1);
    CHECK(!groom.isMoving());
    CHECK(groom.pointCount() == groom.points.size());
    for (const float seconds : {-100.0f, 0.0f, 100.0f}) {
      CAPTURE(seconds);
      const CurvesKeyBlend blend{groom.blendAt(seconds)};
      CHECK(blend.isStill());
      CHECK(blend.lo == 0);
    }
  }
  SUBCASE("A moving groom round trips") {
    const CurvesFile groom{makeMovingGroom(3)};
    REQUIRE(groom.keyCount() == 3);
    REQUIRE(groom.pointCount() == 10);
    REQUIRE(groom.points.size() == 30);
    writeCurvesFile(fileName, groom);
    const CurvesFile read{readCurvesFile(fileName)};
    checkSame(read, groom);
    CHECK(read.strandCount() == 2);
    CHECK(read.isMoving());
  }
  SUBCASE("A key's own time answers that key exactly") {
    const CurvesFile groom{makeMovingGroom(3)};
    for (uint32_t key = 0; key < groom.keyCount(); key++) {
      CAPTURE(key);
      const CurvesKeyBlend blend{groom.blendAt(groom.keyTimes[key])};
      CHECK(blend.isStill());
      CHECK(blend.lo == key);
      CHECK(groom.pointAt(4, blend).x == groom.keysOf(4)[key].x);
    }
  }
  SUBCASE("Outside the keys the blend clamps") {
    const CurvesFile groom{makeMovingGroom(3)};
    CHECK(groom.blendAt(-5.0f).isStill());
    CHECK(groom.blendAt(-5.0f).lo == 0);
    CHECK(groom.blendAt(99.0f).isStill());
    CHECK(groom.blendAt(99.0f).lo == 2);
  }
  SUBCASE("Between keys the blend interpolates") {
    const CurvesFile groom{makeMovingGroom(3)};
    // The keys of point 4 are 100 apart in x, at 0, 0.25 and 0.5 s.
    const CurvesKeyBlend blend{groom.blendAt(0.125f)};
    CHECK(!blend.isStill());
    CHECK(blend.fraction == doctest::Approx(0.5f));
    CHECK(groom.pointAt(4, blend).x ==
          doctest::Approx(groom.keysOf(4)[0].x + 50.0f));
  }
  SUBCASE("Key times that do not ascend are refused") {
    CurvesFile groom{makeMovingGroom(3)};
    groom.keyTimes[2] = groom.keyTimes[1];
    CHECK_THROWS_AS(writeCurvesFile(fileName, groom), smdl::Error);
  }
  SUBCASE("A point block that is not a whole number of points is refused") {
    CurvesFile groom{makeMovingGroom(3)};
    groom.points.pop_back();
    CHECK_THROWS_AS(writeCurvesFile(fileName, groom), smdl::Error);
  }
  SUBCASE("A key inside the shutter is reported") {
    const CurvesFile groom{makeMovingGroom(3)};
    CHECK(groom.hasKeyBetween(0.0f, 0.5f));
    CHECK(!groom.hasKeyBetween(0.25f, 0.5f));
    CHECK(!groom.hasKeyBetween(0.5f, 1.0f));
  }
}

TEST_CASE("CurvesFile: a compressed payload") {
  TempDir tmpDir{"toy-curves-zip"};
  const std::string fileName{(tmpDir / "groom.curves").string()};
  SUBCASE("Survives with keys and a root UV column") {
    CurvesFile groom{makeMovingGroom(2)};
    groom.isCompressed = true;
    groom.rootUVs = {float2(0.25f, 0.75f), float2(-1.0f, 2.0f)};
    writeCurvesFile(fileName, groom);
    const CurvesFile read{readCurvesFile(fileName)};
    CHECK(read.isCompressed);
    checkSame(read, groom);
    REQUIRE(read.rootUVs.size() == 2);
    CHECK(read.rootUVs[0].x == 0.25f);
  }
  SUBCASE("A groom that repeats itself gets smaller") {
    CurvesFile groom{};
    groom.basis = CurvesFile::Basis::LINEAR;
    groom.strandOffsets.push_back(0);
    for (int strand = 0; strand < 200; strand++) {
      for (int i = 0; i < 5; i++)
        groom.points.push_back(float4(0.0f, float(i), 0.0f, 0.01f));
      groom.strandOffsets.push_back(uint32_t(groom.points.size()));
    }
    const auto sizeOf{[&] {
      std::ifstream file(fileName, std::ios::binary | std::ios::ate);
      return size_t(file.tellg());
    }};
    writeCurvesFile(fileName, groom);
    const size_t plainSize{sizeOf()};
    groom.isCompressed = true;
    writeCurvesFile(fileName, groom);
    CHECK(sizeOf() < plainSize / 4);
    CHECK(readCurvesFile(fileName).strandCount() == 200);
  }
}
