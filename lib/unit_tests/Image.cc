#include "doctest.h"

#include <cmath>
#include <filesystem>
#include <fstream>

#include "smdl/Image.h"

namespace fs = std::filesystem;

TEST_CASE("Image") {
  auto tmpDir{fs::temp_directory_path() / "smdl-image-test"};
  fs::remove_all(tmpDir);
  fs::create_directories(tmpDir);
  SUBCASE("PNG round trip with RGB rounded up to RGBA") {
    // A 2x3 RGB image with distinct texels.
    const uint8_t texels[18] = {255, 0,   0,   0,   255, 0,   //
                                0,   0,   255, 255, 255, 255, //
                                51,  102, 153, 0,   0,   0};
    auto fileName{(tmpDir / "test.png").string()};
    REQUIRE(!smdl::write8bitImage(fileName, 2, 3, 3, texels));
    smdl::Image image{};
    REQUIRE(!image.startLoad(fileName));
    CHECK(image.getFormat() == smdl::Image::UINT8);
    CHECK(image.getNumTexelsX() == 2);
    CHECK(image.getNumTexelsY() == 3);
    CHECK(image.getNumChannels() == 4); // 3 must round up to 4
    CHECK(image.getTexelSizeInBytes() == 4);
    image.finishLoad();
    auto texel{image.fetch(0, 0)};
    CHECK(texel[0] == 1.0f);
    CHECK(texel[1] == 0.0f);
    CHECK(texel[2] == 0.0f);
    CHECK(texel[3] == 1.0f); // Alpha must fill with 1
    texel = image.fetch(0, 2);
    CHECK(texel[0] == 51.0f / 255.0f);
    CHECK(texel[1] == 102.0f / 255.0f);
    CHECK(texel[2] == 153.0f / 255.0f);
    // Flipping vertically must swap the first and last rows.
    image.flipVertically();
    CHECK(image.fetch(0, 0)[0] == 51.0f / 255.0f);
    CHECK(image.fetch(0, 2)[0] == 1.0f);
  }
  SUBCASE("Gray PNG") {
    const uint8_t texels[4] = {0, 85, 170, 255};
    auto fileName{(tmpDir / "gray.png").string()};
    REQUIRE(!smdl::write8bitImage(fileName, 2, 2, 1, texels));
    smdl::Image image{};
    REQUIRE(!image.startLoad(fileName));
    CHECK(image.getNumChannels() == 1);
    image.finishLoad();
    CHECK(image.fetch(1, 0)[0] == 85.0f / 255.0f);
    CHECK(image.fetch(1, 1)[0] == 1.0f);
    // Channels not present must be NaN.
    CHECK(std::isnan(image.fetch(0, 0)[1]));
  }
  SUBCASE("Unrecognized or missing files must be load errors") {
    auto fileName{(tmpDir / "test.txt").string()};
    std::ofstream(fileName) << "This is not an image!\n";
    smdl::Image image{};
    CHECK(image.startLoad(fileName).has_value());
    CHECK(image.startLoad((tmpDir / "missing.png").string()).has_value());
  }
  SUBCASE("Unrecognized extension must be a write error") {
    const uint8_t texels[4] = {0, 0, 0, 0};
    CHECK(smdl::write8bitImage((tmpDir / "test.webp").string(), 2, 2, 1, texels)
              .has_value());
  }
  fs::remove_all(tmpDir);
}
