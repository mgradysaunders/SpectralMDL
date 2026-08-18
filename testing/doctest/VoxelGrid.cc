#include "doctest.h"

#include <cmath>
#include <cstdint>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <vector>

#include "smdl/VoxelGrid.h"

namespace fs = std::filesystem;

using smdl::float3;

/// Write a version-3 Mitsuba volume: the 48-byte header, then the
/// single-channel float32 values x-fastest. Assumes a little-endian
/// host, like the loader's own test fixtures elsewhere.
static void writeVol(const std::string &fileName, int nx, int ny, int nz,
                     const std::vector<float> &values, int32_t encoding = 1,
                     int32_t numChannels = 1) {
  std::ofstream file(fileName, std::ios::binary);
  file.write("VOL", 3);
  const char version{3};
  file.write(&version, 1);
  const int32_t header[5] = {encoding, nx, ny, nz, numChannels};
  file.write(reinterpret_cast<const char *>(header), sizeof(header));
  const float bound[6] = {-1.0f, -2.0f, -3.0f, 1.0f, 2.0f, 3.0f};
  file.write(reinterpret_cast<const char *>(bound), sizeof(bound));
  file.write(reinterpret_cast<const char *>(values.data()),
             std::streamsize(values.size() * sizeof(float)));
}

TEST_CASE("VoxelGrid") {
  auto tmpDir{fs::temp_directory_path() / "smdl-voxel-grid-test"};
  fs::remove_all(tmpDir);
  fs::create_directories(tmpDir);
  SUBCASE("Mitsuba volume round trip") {
    // A 20x24x28 linear field, exactly representable in float.
    const int NX{20}, NY{24}, NZ{28};
    auto values{std::vector<float>()};
    for (int z = 0; z < NZ; z++)
      for (int y = 0; y < NY; y++)
        for (int x = 0; x < NX; x++)
          values.push_back(float(x) + 100.0f * float(y) + 10000.0f * float(z));
    auto fileName{(tmpDir / "linear.vol").string()};
    writeVol(fileName, NX, NY, NZ, values);
    smdl::VoxelGrid grid{};
    REQUIRE(!grid.loadFromFile(fileName));
    CHECK(grid.isValid());
    CHECK(grid.getExtent().x == NX);
    CHECK(grid.getExtent().y == NY);
    CHECK(grid.getExtent().z == NZ);
    CHECK(grid.getBrickCount().x == 2);
    CHECK(grid.getBrickCount().y == 2);
    CHECK(grid.getBrickCount().z == 2);
    CHECK(grid.getBackground() == 0.0f);
    CHECK(grid.getMinValue() == 0.0f);
    CHECK(grid.getMaxValue() == 19.0f + 2300.0f + 270000.0f);
    CHECK(grid.getWorldBoundMin().x == -1.0f);
    CHECK(grid.getWorldBoundMin().z == -3.0f);
    CHECK(grid.getWorldBoundMax().y == 2.0f);
    // Every voxel fetches back exactly, and out-of-extent coordinates
    // resolve to the background.
    bool allExact{true};
    for (int z = 0; z < NZ; z++)
      for (int y = 0; y < NY; y++)
        for (int x = 0; x < NX; x++)
          allExact &= grid.fetch(x, y, z) ==
                      float(x) + 100.0f * float(y) + 10000.0f * float(z);
    CHECK(allExact);
    CHECK(grid.fetch(-1, 0, 0) == 0.0f);
    CHECK(grid.fetch(NX, 0, 0) == 0.0f);
    CHECK(grid.fetch(0, 0, NZ) == 0.0f);
    // Trilinear interpolation of a linear field is the field itself,
    // clamped at the boundary half-voxel band.
    for (float3 coord : {float3(0.3f, 0.4f, 0.6f), float3(0.111f, 0.9f, 0.5f),
                         float3(0.77f, 0.123f, 0.321f)}) {
      const float px{coord.x * NX - 0.5f};
      const float py{coord.y * NY - 0.5f};
      const float pz{coord.z * NZ - 0.5f};
      CHECK(grid.sample(coord) ==
            doctest::Approx(px + 100.0f * py + 10000.0f * pz).epsilon(1e-5));
    }
    // Wild coordinates clamp instead of misbehaving.
    CHECK(std::isfinite(grid.sample(float3(-1e30f, 0.5f, 0.5f))));
    CHECK(grid.sample(float3(2.0f, 2.0f, 2.0f)) ==
          grid.sample(float3(1.0f, 1.0f, 1.0f)));
  }
  SUBCASE("Sparsity and per-brick bounds") {
    // A 48x16x16 field that is zero except over x in [24, 40), so brick
    // (0,0,0) is empty even after its one-voxel dilation, brick (1,0,0)
    // sees the nonzero region through dilation and content, and brick
    // (2,0,0) holds the tail.
    const int NX{48}, NY{16}, NZ{16};
    auto values{std::vector<float>()};
    for (int z = 0; z < NZ; z++)
      for (int y = 0; y < NY; y++)
        for (int x = 0; x < NX; x++)
          values.push_back(x >= 24 && x < 40 ? 2.0f + float(x - 24) : 0.0f);
    auto fileName{(tmpDir / "sparse.vol").string()};
    writeVol(fileName, NX, NY, NZ, values);
    smdl::VoxelGrid grid{};
    REQUIRE(!grid.loadFromFile(fileName));
    CHECK(grid.getBrickCount().x == 3);
    // Brick 0 covers x in [0,16), its dilation reaches x of 16, still
    // zero: the empty-brick bound is the background.
    CHECK(grid.getBrickMinValue(0, 0, 0) == 0.0f);
    CHECK(grid.getBrickMaxValue(0, 0, 0) == 0.0f);
    // Brick 1 covers x in [16,32), dilated to [15,32]: the maximum in
    // that window is at x of 32.
    CHECK(grid.getBrickMaxValue(1, 0, 0) == 2.0f + 8.0f);
    // Brick 2 covers x in [32,48): the maximum of the whole field.
    CHECK(grid.getBrickMaxValue(2, 0, 0) == 2.0f + 15.0f);
    CHECK(grid.getMaxValue() == 2.0f + 15.0f);
    // An empty brick exists, so the global minimum folds in the
    // background.
    CHECK(grid.getMinValue() == 0.0f);
    // Out-of-count brick queries resolve to the background.
    CHECK(grid.getBrickMaxValue(3, 0, 0) == 0.0f);
    CHECK(grid.getBrickMaxValue(-1, 0, 0) == 0.0f);
    // The per-brick maximum must bound every trilinear sample whose
    // support touches the brick; spot check against a sweep through the
    // brick that owns the discontinuity.
    bool bounded{true};
    for (int i = 0; i < 1000; i++) {
      const float3 coord{(16.0f + 16.0f * float(i) / 999.0f) / NX, 0.4f, 0.6f};
      bounded &= grid.sample(coord) <= grid.getBrickMaxValue(1, 0, 0);
    }
    CHECK(bounded);
  }
  SUBCASE("Errors") {
    smdl::VoxelGrid grid{};
    // Unknown extension.
    CHECK(grid.loadFromFile((tmpDir / "nope.xyz").string()).has_value());
    CHECK(!grid.isValid());
    // Missing file.
    CHECK(grid.loadFromFile((tmpDir / "missing.vol").string()).has_value());
    CHECK(!grid.isValid());
    // Bad magic.
    {
      std::ofstream file((tmpDir / "bad.vol").string(), std::ios::binary);
      file.write("NOT A VOLUME AT ALL, NOWHERE NEAR LONG ENOUGH?", 46);
    }
    CHECK(grid.loadFromFile((tmpDir / "bad.vol").string()).has_value());
    // Unsupported encoding and channel count.
    writeVol((tmpDir / "half.vol").string(), 2, 2, 2,
             std::vector<float>(8, 1.0f), /*encoding=*/2);
    CHECK(grid.loadFromFile((tmpDir / "half.vol").string()).has_value());
    writeVol((tmpDir / "rgb.vol").string(), 2, 2, 2,
             std::vector<float>(24, 1.0f), /*encoding=*/1, /*numChannels=*/3);
    CHECK(grid.loadFromFile((tmpDir / "rgb.vol").string()).has_value());
    // Truncated values.
    writeVol((tmpDir / "short.vol").string(), 4, 4, 4,
             std::vector<float>(10, 1.0f));
    CHECK(grid.loadFromFile((tmpDir / "short.vol").string()).has_value());
    // Grid names are a NanoVDB concept.
    writeVol((tmpDir / "named.vol").string(), 2, 2, 2,
             std::vector<float>(8, 1.0f));
    CHECK(grid.loadFromFile((tmpDir / "named.vol").string(), "density")
              .has_value());
    // And a failed load always leaves the grid cleared.
    CHECK(!grid.isValid());
    CHECK(grid.getExtent().x == 0);
  }
}
