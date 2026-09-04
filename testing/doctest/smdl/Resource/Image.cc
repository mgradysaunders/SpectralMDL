#include "doctest.h"

#include <algorithm>
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
  SUBCASE("Maximum mip chain") {
    // 7x5 gray, odd on both axes, with the maxima placed so that the
    // border and the widened last cells matter: texel (I, J) of level l
    // must be the maximum over level 0 texels [I << l, (I + 1) << l),
    // widened to the edge for the last texel, plus a one-texel wrapped
    // border on every side.
    const int numX{7}, numY{5};
    const uint8_t texels[35] = {200, 10,  20,  30,  40,  50,  60,  //
                                70,  80,  90,  100, 110, 120, 130, //
                                140, 150, 160, 170, 180, 190, 255, //
                                5,   15,  25,  35,  45,  55,  65,  //
                                75,  85,  95,  105, 115, 125, 135};
    auto fileName{(tmpDir / "max.png").string()};
    REQUIRE(!smdl::write8bitImage(fileName, numX, numY, 1, texels));
    smdl::Image image{};
    REQUIRE(!image.startLoad(fileName));
    CHECK(image.requestMipLevels(smdl::Image::MIP_MAX));
    CHECK(image.getMipFilter() == smdl::Image::MIP_MAX);
    // Asking again for the same kind is a no-op; the other kind is
    // refused and changes nothing.
    CHECK(image.requestMipLevels(smdl::Image::MIP_MAX));
    CHECK(!image.requestMipLevels(smdl::Image::MIP_MEAN));
    CHECK(image.getMipFilter() == smdl::Image::MIP_MAX);
    CHECK(image.getNumLevels() == 3);
    image.finishLoad();
    auto wrap{[](int i, int n) { return ((i % n) + n) % n; }};
    for (int level = 1; level < image.getNumLevels(); level++) {
      const int levelX{image.getNumTexelsX(level)};
      const int levelY{image.getNumTexelsY(level)};
      for (int j = 0; j < levelY; j++) {
        for (int i = 0; i < levelX; i++) {
          const int x0{i << level};
          const int x1{i == levelX - 1 ? numX : (i + 1) << level};
          const int y0{j << level};
          const int y1{j == levelY - 1 ? numY : (j + 1) << level};
          int expected{0};
          for (int y = y0 - 1; y <= y1; y++)
            for (int x = x0 - 1; x <= x1; x++)
              expected = std::max(
                  expected, int(texels[wrap(x, numX) + numX * wrap(y, numY)]));
          CAPTURE(level);
          CAPTURE(i);
          CAPTURE(j);
          CHECK(image.fetch(i, j, level)[0] == float(expected) / 255.0f);
        }
      }
    }
    // Level 1 texel (1, 0) covers texels 2..3 by 0..1 plus the border,
    // which reaches the 180 at (4, 2) but not the 200 at (0, 0); the last
    // texel of level 1 in X covers texels 4..6, where the 255 at (6, 2)
    // sits inside its border.
    CHECK(image.fetch(1, 0, 1)[0] == 180.0f / 255.0f);
    CHECK(image.fetch(2, 0, 1)[0] == 1.0f);
    CHECK(image.fetch(0, 0, 2)[0] == 1.0f);
  }
  SUBCASE("Maximum mip chain reduces per channel") {
    // 2x2 RGBA whose per-channel maxima sit in four different texels.
    const uint8_t texels[16] = {200, 1,   2,   3,  //
                                4,   210, 6,   7,  //
                                8,   9,   220, 11, //
                                12,  13,  14,  230};
    auto fileName{(tmpDir / "max_rgba.png").string()};
    REQUIRE(!smdl::write8bitImage(fileName, 2, 2, 4, texels));
    smdl::Image image{};
    REQUIRE(!image.startLoad(fileName));
    CHECK(image.requestMipLevels(smdl::Image::MIP_MAX));
    image.finishLoad();
    auto texel{image.fetch(0, 0, 1)};
    CHECK(texel[0] == 200.0f / 255.0f);
    CHECK(texel[1] == 210.0f / 255.0f);
    CHECK(texel[2] == 220.0f / 255.0f);
    CHECK(texel[3] == 230.0f / 255.0f);
  }
  SUBCASE("Maximum mip chain of a float image") {
    // The reduction copies stored bytes, so a float image must come back
    // with its exact values, including ones beyond [0, 1].
    const float texels[8] = {0.25f, -1.0f, 2.5f,  0.75f,
                             0.5f,  0.5f,  1.25f, 0.0f};
    auto fileName{(tmpDir / "max.exr").string()};
    REQUIRE(!smdl::writeFloatImage(fileName, 4, 2, 1, texels));
    smdl::Image image{};
    REQUIRE(!image.startLoad(fileName));
    CHECK(image.requestMipLevels(smdl::Image::MIP_MAX));
    image.finishLoad();
    CHECK(image.getNumLevels() == 3);
    CHECK(image.fetch(0, 0, 1)[0] == 2.5f);
    CHECK(image.fetch(1, 0, 1)[0] == 2.5f);
    CHECK(image.fetch(0, 0, 2)[0] == 2.5f);
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
    // Nothing asked, so the image holds level 0 only.
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
    // not walk levels that were never allocated.
    image.flipVertically();
    CHECK(image.fetch(0, 0)[0] == doctest::Approx(192.0f / 255.0f));
  }
  SUBCASE("An image nobody mips allocates level 0 alone") {
    // The point of allocating in 'finishLoad()' rather than in
    // 'startLoad()': the level count is settled by then, so a chain
    // nobody asked for costs nothing at all.
    const uint8_t texels[16] = {0,   16,  32,  48,  //
                                64,  80,  96,  112, //
                                128, 144, 160, 176, //
                                192, 208, 224, 240};
    auto fileName{(tmpDir / "nochain.png").string()};
    REQUIRE(!smdl::write8bitImage(fileName, 4, 4, 1, texels));
    smdl::Image image{};
    REQUIRE(!image.startLoad(fileName));
    // Nothing is obtained up front, whatever the file turns out to be.
    CHECK(image.getSizeInBytes() == 0);
    image.finishLoad();
    CHECK(image.getNumLevels() == 1);
    CHECK(image.getSizeInBytes() == 4 * 4 * 1);
    // The same file with the chain asked for is larger by the levels it
    // actually holds, and level 0 reads the same either way.
    smdl::Image imageWithChain{};
    REQUIRE(!imageWithChain.startLoad(fileName));
    imageWithChain.requestMipLevels();
    imageWithChain.finishLoad();
    CHECK(imageWithChain.getNumLevels() == 3);
    CHECK(imageWithChain.getSizeInBytes() == 4 * 4 + 2 * 2 + 1);
    for (int y = 0; y < 4; y++)
      for (int x = 0; x < 4; x++)
        CHECK(image.fetch(x, y)[0] == imageWithChain.fetch(x, y)[0]);
    image.flipVertically();
    CHECK(image.fetch(0, 0)[0] == doctest::Approx(192.0f / 255.0f));
  }
  SUBCASE("Mip levels must be requested before the load finishes") {
    // The request is what sizes the allocation, so it has to arrive
    // first: there is no reserved chain to fill in afterward.
    const uint8_t texels[16] = {0,   16,  32,  48,  //
                                64,  80,  96,  112, //
                                128, 144, 160, 176, //
                                192, 208, 224, 240};
    auto fileName{(tmpDir / "earlymip.png").string()};
    REQUIRE(!smdl::write8bitImage(fileName, 4, 4, 1, texels));
    smdl::Image image{};
    REQUIRE(!image.startLoad(fileName));
    image.requestMipLevels();
    image.finishLoad();
    CHECK(image.getNumLevels() == 3);
    CHECK(image.fetch(0, 0)[0] == doctest::Approx(0.0f));
    CHECK(image.fetch(0, 0, 1)[0] == doctest::Approx(40.0f / 255.0f));
    CHECK(image.fetch(0, 0, 2)[0] == doctest::Approx(120.0f / 255.0f));
    // And finishing again must not regenerate or disturb anything.
    image.finishLoad();
    CHECK(image.fetch(0, 0, 2)[0] == doctest::Approx(120.0f / 255.0f));
  }
  SUBCASE("A decode failure must leave every level zeroed") {
    // A truncated PNG: the header still parses, so 'startLoad()'
    // succeeds and the failure lands in 'finishLoad()', after it has
    // allocated. Only level 0 is zeroed there, so this pins that the
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
