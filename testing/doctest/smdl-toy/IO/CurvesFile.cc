#include "doctest.h"

#include <filesystem>
#include <fstream>
#include <string>

#include "smdl/Support/Error.h"

#include "IO/CurvesFile.h"

namespace fs = std::filesystem;

namespace {

// A groom with two strands of different lengths, so that the offset
// table is exercised rather than being a uniform stride, and with radii
// and coordinates that are all distinct, so a transposed or truncated
// read shows up as a wrong value rather than as a coincidence.
[[nodiscard]] CurvesFile makeGroom(CurvesFile::Basis basis) {
  auto curves{CurvesFile()};
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

void checkSame(const CurvesFile &read, const CurvesFile &written) {
  CHECK(read.version == 1);
  CHECK(read.basis == written.basis);
  CHECK(read.strandOffsets == written.strandOffsets);
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
  const auto tmpDir{fs::temp_directory_path() / "smdl-toy-curves-test"};
  fs::remove_all(tmpDir);
  fs::create_directories(tmpDir);
  const auto fileName{(tmpDir / "groom.curves").string()};
  SUBCASE("Every basis survives") {
    for (const auto basis :
         {CurvesFile::Basis::LINEAR, CurvesFile::Basis::BSPLINE,
          CurvesFile::Basis::CATMULL_ROM}) {
      CAPTURE(uint16_t(basis));
      const auto groom{makeGroom(basis)};
      writeCurvesFile(fileName, groom);
      const auto read{readCurvesFile(fileName)};
      checkSame(read, groom);
      CHECK(!read.hasRootUVs());
      CHECK(read.strandCount() == 2);
    }
  }
  SUBCASE("The root UV column survives") {
    auto groom{makeGroom(CurvesFile::Basis::CATMULL_ROM)};
    groom.rootUVs = {float2(0.25f, 0.75f), float2(-1.0f, 2.0f)};
    writeCurvesFile(fileName, groom);
    const auto read{readCurvesFile(fileName)};
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
    const auto groom{makeGroom(CurvesFile::Basis::BSPLINE)};
    writeCurvesFile(fileName, groom);
    const auto readBytes{[&] {
      std::ifstream file(fileName, std::ios::binary);
      return std::string(std::istreambuf_iterator<char>(file), {});
    }};
    const auto first{readBytes()};
    writeCurvesFile(fileName, readCurvesFile(fileName));
    CHECK(readBytes() == first);
  }
  SUBCASE("A truncated file is refused") {
    writeCurvesFile(fileName, makeGroom(CurvesFile::Basis::LINEAR));
    auto bytes{std::string()};
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
      file << "#smdl layout\n";
    }
    CHECK_THROWS_AS((void)readCurvesFile(fileName), smdl::Error);
  }
  SUBCASE("A groom the basis cannot support is refused on the way out") {
    // The writer checks the same shape the reader does, so a bad groom
    // cannot reach disk. B-spline is the basis with a real floor: it
    // needs a whole four-point window, where Catmull-Rom takes two and
    // the loader pads the phantom ends itself.
    auto groom{CurvesFile()};
    groom.points.assign(3, float4(0.0f, 0.0f, 0.0f, 0.01f));
    groom.strandOffsets = {0, 3};
    groom.basis = CurvesFile::Basis::BSPLINE;
    CHECK_THROWS_AS(writeCurvesFile(fileName, groom), smdl::Error);
    groom.basis = CurvesFile::Basis::CATMULL_ROM;
    CHECK_NOTHROW(writeCurvesFile(fileName, groom));
  }
  SUBCASE("An offset table that does not add up is refused") {
    auto groom{makeGroom(CurvesFile::Basis::LINEAR)};
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
  fs::remove_all(tmpDir);
}
