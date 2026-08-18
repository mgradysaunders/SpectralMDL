#include "doctest.h"

#include <cmath>
#include <filesystem>
#include <fstream>
#include <vector>

#include "smdl/Resource/Image.h"

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
  SUBCASE("Mip chain") {
    // A 4x4 gray PNG whose 2x2 box averages are exact integers, so
    // every mip texel has a predictable value.
    const uint8_t texels[16] = {0,   16,  32,  48,  //
                                64,  80,  96,  112, //
                                128, 144, 160, 176, //
                                192, 208, 224, 240};
    auto fileName{(tmpDir / "mip.png").string()};
    REQUIRE(!smdl::write8bitImage(fileName, 4, 4, 1, texels));
    smdl::Image image{};
    REQUIRE(!image.startLoad(fileName));
    image.requestMipLevels();
    CHECK(image.getNumLevels() == 3);
    CHECK(image.getNumTexelsX(1) == 2);
    CHECK(image.getNumTexelsY(1) == 2);
    CHECK(image.getNumTexelsX(2) == 1);
    CHECK(image.getNumTexelsY(2) == 1);
    image.finishLoad();
    // Level 1 must hold the 2x2 box averages, ...
    CHECK(image.fetch(0, 0, 1)[0] == doctest::Approx(40.0f / 255.0f));
    CHECK(image.fetch(1, 0, 1)[0] == doctest::Approx(72.0f / 255.0f));
    CHECK(image.fetch(0, 1, 1)[0] == doctest::Approx(168.0f / 255.0f));
    CHECK(image.fetch(1, 1, 1)[0] == doctest::Approx(200.0f / 255.0f));
    // ... and level 2 the global average.
    CHECK(image.fetch(0, 0, 2)[0] == doctest::Approx(120.0f / 255.0f));
    // Flipping vertically must flip every level.
    image.flipVertically();
    CHECK(image.fetch(0, 0, 0)[0] == doctest::Approx(192.0f / 255.0f));
    CHECK(image.fetch(0, 0, 1)[0] == doctest::Approx(168.0f / 255.0f));
    CHECK(image.fetch(0, 0, 2)[0] == doctest::Approx(120.0f / 255.0f));
  }
  SUBCASE("Mip chain of non-power-of-two extents") {
    // 5x3: the chain is 5x3 -> 2x1 -> 1x1, saturating each axis at 1.
    const uint8_t texels[15] = {10,  20,  30,  40,  50,  //
                                60,  70,  80,  90,  100, //
                                110, 120, 130, 140, 150};
    auto fileName{(tmpDir / "npot.png").string()};
    REQUIRE(!smdl::write8bitImage(fileName, 5, 3, 1, texels));
    smdl::Image image{};
    REQUIRE(!image.startLoad(fileName));
    image.requestMipLevels();
    CHECK(image.getNumLevels() == 3);
    CHECK(image.getNumTexelsX(1) == 2);
    CHECK(image.getNumTexelsY(1) == 1);
    CHECK(image.getNumTexelsX(2) == 1);
    CHECK(image.getNumTexelsY(2) == 1);
    image.finishLoad();
    // The box weighting over odd extents is fractional; just require
    // every level to stay within the level 0 value range.
    for (int level = 1; level < 3; level++)
      for (int y = 0; y < image.getNumTexelsY(level); y++)
        for (int x = 0; x < image.getNumTexelsX(level); x++) {
          auto value{image.fetch(x, y, level)[0]};
          CHECK(value >= 10.0f / 255.0f);
          CHECK(value <= 150.0f / 255.0f);
        }
  }
  SUBCASE("Mip levels are opt in") {
    const uint8_t texels[16] = {0,   16,  32,  48,  //
                                64,  80,  96,  112, //
                                128, 144, 160, 176, //
                                192, 208, 224, 240};
    auto fileName{(tmpDir / "nomip.png").string()};
    REQUIRE(!smdl::write8bitImage(fileName, 4, 4, 1, texels));
    smdl::Image image{};
    REQUIRE(!image.startLoad(fileName));
    // Nothing asked, so the image holds level 0 only. The chain is laid
    // out behind it, but it is never generated and never reported.
    CHECK(image.getNumLevels() == 1);
    image.finishLoad();
    CHECK(image.getNumLevels() == 1);
    // Level 0 must be identical to a with-mips load of the same file.
    smdl::Image imageWithMips{};
    REQUIRE(!imageWithMips.startLoad(fileName));
    imageWithMips.requestMipLevels();
    imageWithMips.finishLoad();
    CHECK(imageWithMips.getNumLevels() == 3);
    for (int y = 0; y < 4; y++)
      for (int x = 0; x < 4; x++)
        CHECK(image.fetch(x, y)[0] == imageWithMips.fetch(x, y)[0]);
    // Flipping vertically must still work with a single level, and must
    // not walk the ungenerated chain.
    image.flipVertically();
    CHECK(image.fetch(0, 0)[0] == doctest::Approx(192.0f / 255.0f));
  }
  SUBCASE("Mip levels can be disallowed up front") {
    // The compiler-wide kill switch: not 'nobody has asked yet' but
    // 'nobody may ask', which is what lets the chain go unreserved.
    const uint8_t texels[16] = {0,   16,  32,  48,  //
                                64,  80,  96,  112, //
                                128, 144, 160, 176, //
                                192, 208, 224, 240};
    auto fileName{(tmpDir / "nochain.png").string()};
    REQUIRE(!smdl::write8bitImage(fileName, 4, 4, 1, texels));
    smdl::Image image{};
    REQUIRE(!image.startLoad(fileName, /*allowMipLevels=*/false));
    // The request must be refused, before and after loading, so that no
    // texture can ever bake a level count the image does not hold.
    image.requestMipLevels();
    CHECK(image.getNumLevels() == 1);
    image.finishLoad();
    image.requestMipLevels();
    image.finishLoad();
    CHECK(image.getNumLevels() == 1);
    // And the point of the exercise: the allocation is exactly the
    // level 0 texels, where the same load with a chain is larger.
    CHECK(image.getSizeInBytes() == 4 * 4 * 1);
    smdl::Image imageWithChain{};
    REQUIRE(!imageWithChain.startLoad(fileName));
    CHECK(imageWithChain.getSizeInBytes() > image.getSizeInBytes());
    // Level 0 must be identical either way, and still flippable.
    imageWithChain.finishLoad();
    for (int y = 0; y < 4; y++)
      for (int x = 0; x < 4; x++)
        CHECK(image.fetch(x, y)[0] == imageWithChain.fetch(x, y)[0]);
    image.flipVertically();
    CHECK(image.fetch(0, 0)[0] == doctest::Approx(192.0f / 255.0f));
  }
  SUBCASE("Mip levels requested after loading") {
    // The whole point of laying the chain out unconditionally: a
    // reference that shows up after the image is already loaded still
    // gets its mip levels, and level 0 does not move or change.
    const uint8_t texels[16] = {0,   16,  32,  48,  //
                                64,  80,  96,  112, //
                                128, 144, 160, 176, //
                                192, 208, 224, 240};
    auto fileName{(tmpDir / "latemip.png").string()};
    REQUIRE(!smdl::write8bitImage(fileName, 4, 4, 1, texels));
    smdl::Image image{};
    REQUIRE(!image.startLoad(fileName));
    image.finishLoad();
    CHECK(image.getNumLevels() == 1);
    auto texel00{image.fetch(0, 0)[0]};
    image.requestMipLevels();
    image.finishLoad();
    CHECK(image.getNumLevels() == 3);
    CHECK(image.fetch(0, 0)[0] == texel00);
    CHECK(image.fetch(0, 0, 1)[0] == doctest::Approx(40.0f / 255.0f));
    CHECK(image.fetch(0, 0, 2)[0] == doctest::Approx(120.0f / 255.0f));
    // And repeating the request must not regenerate or disturb anything.
    image.requestMipLevels();
    image.finishLoad();
    CHECK(image.fetch(0, 0, 2)[0] == doctest::Approx(120.0f / 255.0f));
  }
  SUBCASE("A decode failure must leave every level zeroed") {
    // A truncated PNG: the header still parses, so 'startLoad()'
    // succeeds and allocates, and the failure lands in 'finishLoad()'.
    // Only level 0 is zeroed at allocation, so this pins that the
    // failure path zeroes the chain as well.
    // 64x64 so that the truncation lands well past the PNG header and
    // inside the pixel data, and so that the chain is several levels.
    std::vector<uint8_t> texels(size_t(64) * 64);
    for (size_t i = 0; i < texels.size(); i++) texels[i] = uint8_t(i);
    auto fileName{(tmpDir / "truncated.png").string()};
    REQUIRE(!smdl::write8bitImage(fileName, 64, 64, 1, texels.data()));
    auto sizeInBytes{fs::file_size(fileName)};
    REQUIRE(sizeInBytes > 128);
    fs::resize_file(fileName, sizeInBytes / 2);
    smdl::Image image{};
    REQUIRE(!image.startLoad(fileName));
    image.requestMipLevels();
    CHECK_THROWS(image.finishLoad());
    CHECK(image.getNumLevels() == 7);
    for (int level = 0; level < image.getNumLevels(); level++)
      for (int y = 0; y < image.getNumTexelsY(level); y++)
        for (int x = 0; x < image.getNumTexelsX(level); x++)
          CHECK(image.fetch(x, y, level)[0] == 0.0f);
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
