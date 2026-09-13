#include "Fixtures.h"
#include "TransformFixtures.h"

#include <fstream>
#include <string>

#include "smdl/Support/Error.h"

#include "IO/PlacesFile.h"

namespace {

// Sheared, scaled, and translated, so that every stored entry of the top
// three rows is exercised and none is a matrix default.
[[nodiscard]] PlacesFile makeGeneral() {
  PlacesFile places{};
  for (int i = 0; i < 5; i++) {
    float4x4 transform{1.0f};
    transform[0][1] = 0.25f * float(i);
    transform[1][1] = 2.0f + float(i);
    transform[2][0] = -0.5f;
    transform[3] = float4(float(i), 2.0f * float(i), -1.0f, 1.0f);
    places.transforms.push_back(transform);
  }
  return places;
}

// The same count of records, every one a rigid motion: a turn about z
// and a translation, with one identity so the snorm endpoints are hit.
[[nodiscard]] PlacesFile makeRigid() {
  PlacesFile places{};
  places.isRigid = true;
  for (int i = 0; i < 5; i++)
    places.transforms.push_back(xf::translation(float(i), -2.0f, 0.5f) *
                                xf::rotationZ(30.0f * float(i)));
  return places;
}

} // namespace

TEST_CASE("PlacesFile: round trip") {
  TempDir tmpDir{"toy-places"};
  const std::string fileName{(tmpDir / "scatter.places").string()};
  const PlacesFile places{makeGeneral()};
  auto checkTransforms{[&](const PlacesFile &read) {
    REQUIRE(read.transforms.size() == places.transforms.size());
    for (size_t i = 0; i < read.transforms.size(); i++) {
      CAPTURE(i);
      CHECK_SAME(read.transforms[i], places.transforms[i]);
    }
  }};
  SUBCASE("Without variants") {
    writePlacesFile(fileName, places);
    const PlacesFile read{readPlacesFile(fileName)};
    CHECK(read.version == 1);
    CHECK(!read.hasVariants());
    CHECK(!read.isRigid);
    CHECK(!read.isCompressed);
    checkTransforms(read);
  }
  SUBCASE("With variants") {
    PlacesFile written{places};
    written.variants = {0, PlacesFile::NO_VARIANT, 2, 1,
                        PlacesFile::NO_VARIANT};
    writePlacesFile(fileName, written);
    const PlacesFile read{readPlacesFile(fileName)};
    REQUIRE(read.hasVariants());
    CHECK(read.variants == written.variants);
    checkTransforms(read);
  }
  SUBCASE("A truncated buffer is refused") {
    writePlacesFile(fileName, places);
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
    CHECK_THROWS_AS((void)readPlacesFile(fileName), smdl::Error);
  }
  SUBCASE("A file that is not a places buffer is refused") {
    {
      std::ofstream file(fileName, std::ios::binary | std::ios::trunc);
      file << "place rock\n";
    }
    CHECK_THROWS_AS((void)readPlacesFile(fileName), smdl::Error);
  }
}

TEST_CASE("PlacesFile: a compressed payload") {
  TempDir tmpDir{"toy-places-zip"};
  const std::string fileName{(tmpDir / "scatter.places").string()};
  const auto sizeOf{[&] {
    std::ifstream file(fileName, std::ios::binary | std::ios::ate);
    return size_t(file.tellg());
  }};
  SUBCASE("Survives with the variant column") {
    PlacesFile written{makeGeneral()};
    written.isCompressed = true;
    written.variants = {0, 1, 0, 1, 0};
    writePlacesFile(fileName, written);
    const PlacesFile read{readPlacesFile(fileName)};
    CHECK(read.isCompressed);
    CHECK(read.variants == written.variants);
    REQUIRE(read.transforms.size() == written.transforms.size());
    for (size_t i = 0; i < read.transforms.size(); i++) {
      CAPTURE(i);
      CHECK_SAME(read.transforms[i], written.transforms[i]);
    }
  }
  SUBCASE("Compresses a scatter that repeats itself") {
    // A thousand records of one transform is the shape a real scatter
    // tends toward, and the shape deflate is worth having for.
    PlacesFile written{};
    written.transforms.assign(1000, xf::SHEAR);
    writePlacesFile(fileName, written);
    const size_t plainSize{sizeOf()};
    written.isCompressed = true;
    writePlacesFile(fileName, written);
    CHECK(sizeOf() < plainSize / 4);
    CHECK(readPlacesFile(fileName).transforms.size() == 1000);
  }
}

TEST_CASE("PlacesFile: rigid records") {
  TempDir tmpDir{"toy-places-rigid"};
  const std::string fileName{(tmpDir / "scatter.places").string()};
  SUBCASE("A rotation survives the snorm quantization") {
    const PlacesFile written{makeRigid()};
    writePlacesFile(fileName, written);
    const PlacesFile read{readPlacesFile(fileName)};
    CHECK(read.isRigid);
    REQUIRE(read.transforms.size() == written.transforms.size());
    for (size_t i = 0; i < read.transforms.size(); i++) {
      CAPTURE(i);
      // The stated guarantee is about 3e-5 radians of rotation; on a
      // unit-length basis vector that is the same figure of position.
      CHECK_NEAR(read.transforms[i], written.transforms[i], 1.0e-4f);
      // The translation is stored as a float and is exact.
      CHECK_SAME(read.transforms[i][3], written.transforms[i][3]);
    }
  }
  SUBCASE("A rigid record is smaller than a general one") {
    PlacesFile written{makeRigid()};
    const auto sizeOf{[&] {
      std::ifstream file(fileName, std::ios::binary | std::ios::ate);
      return size_t(file.tellg());
    }};
    writePlacesFile(fileName, written);
    const size_t rigidSize{sizeOf()};
    written.isRigid = false;
    writePlacesFile(fileName, written);
    CHECK(rigidSize < sizeOf());
  }
  SUBCASE("Compression and the variant column compose with it") {
    PlacesFile written{makeRigid()};
    written.isCompressed = true;
    written.variants = {2, PlacesFile::NO_VARIANT, 0, 1, 1};
    writePlacesFile(fileName, written);
    const PlacesFile read{readPlacesFile(fileName)};
    CHECK(read.isRigid);
    CHECK(read.isCompressed);
    CHECK(read.variants == written.variants);
    CHECK_NEAR(read.transforms.back(), written.transforms.back(), 1.0e-4f);
  }
  SUBCASE("A scaled transform is refused rather than quantized") {
    PlacesFile written{makeRigid()};
    written.transforms[2] = written.transforms[2] * float4x4(2.0f);
    written.transforms[2][3][3] = 1.0f;
    CHECK_THROWS_AS(writePlacesFile(fileName, written), smdl::Error);
  }
  SUBCASE("A mirrored transform is refused") {
    PlacesFile written{makeRigid()};
    written.transforms[1][2] = -written.transforms[1][2];
    CHECK_THROWS_AS(writePlacesFile(fileName, written), smdl::Error);
  }
  SUBCASE("A sheared transform is refused") {
    PlacesFile written{makeRigid()};
    written.transforms[0] = xf::SHEAR;
    CHECK_THROWS_AS(writePlacesFile(fileName, written), smdl::Error);
  }
}
