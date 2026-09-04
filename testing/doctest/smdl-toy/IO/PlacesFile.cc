#include "doctest.h"

#include <filesystem>
#include <fstream>
#include <string>

#include "smdl/Support/Error.h"

#include "IO/PlacesFile.h"

namespace fs = std::filesystem;

TEST_CASE("PlacesFile: round trip") {
  const auto tmpDir{fs::temp_directory_path() / "smdl-toy-places-test"};
  fs::remove_all(tmpDir);
  fs::create_directories(tmpDir);
  const auto fileName{(tmpDir / "scatter.places").string()};
  // Sheared, scaled, and translated, so that every stored entry of the
  // top three rows is exercised and none is a matrix default.
  auto places{PlacesFile()};
  for (int i = 0; i < 5; i++) {
    auto xf{float4x4(1.0f)};
    xf[0][1] = 0.25f * float(i);
    xf[1][1] = 2.0f + float(i);
    xf[2][0] = -0.5f;
    xf[3] = float4(float(i), 2.0f * float(i), -1.0f, 1.0f);
    places.transforms.push_back(xf);
  }
  auto checkTransforms{[&](const PlacesFile &read) {
    REQUIRE(read.transforms.size() == places.transforms.size());
    for (size_t i = 0; i < read.transforms.size(); i++)
      for (int column = 0; column < 4; column++)
        for (int row = 0; row < 4; row++) {
          CAPTURE(i);
          CAPTURE(column);
          CAPTURE(row);
          CHECK(read.transforms[i][column][row] ==
                places.transforms[i][column][row]);
        }
  }};
  SUBCASE("Without variants") {
    writePlacesFile(fileName, places);
    const auto read{readPlacesFile(fileName)};
    CHECK(read.version == 1);
    CHECK(!read.hasVariants());
    checkTransforms(read);
  }
  SUBCASE("With variants") {
    places.variants = {0, PlacesFile::NO_VARIANT, 2, 1, PlacesFile::NO_VARIANT};
    writePlacesFile(fileName, places);
    const auto read{readPlacesFile(fileName)};
    REQUIRE(read.hasVariants());
    CHECK(read.variants == places.variants);
    checkTransforms(read);
  }
  SUBCASE("A truncated buffer is refused") {
    writePlacesFile(fileName, places);
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
    CHECK_THROWS_AS((void)readPlacesFile(fileName), smdl::Error);
  }
  SUBCASE("A file that is not a places buffer is refused") {
    {
      std::ofstream file(fileName, std::ios::binary | std::ios::trunc);
      file << "#smdl layout\n";
    }
    CHECK_THROWS_AS((void)readPlacesFile(fileName), smdl::Error);
  }
  fs::remove_all(tmpDir);
}
