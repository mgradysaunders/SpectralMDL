#include "doctest.h"

#include <cmath>
#include <cstdint>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <vector>

#include "smdl/Common.h"
#include "smdl/Resource/VoxelGrid.h"

namespace fs = std::filesystem;

using smdl::float3;

// Is NanoVDB in this build? The macro is private to the library, so the
// suite asks the same question the '--version' banner answers.
static bool hasNanoVDB() {
  return smdl::BuildInfo::get().withNanoVDB != nullptr;
}

// Do two grids agree everywhere: extent, background, value bounds,
// world bounds, and every voxel of the extent?
static bool sameGrid(const smdl::VoxelGrid &grid0,
                     const smdl::VoxelGrid &grid1) {
  const auto extent{grid0.getExtent()};
  if (!(extent.x == grid1.getExtent().x && extent.y == grid1.getExtent().y &&
        extent.z == grid1.getExtent().z))
    return false;
  if (grid0.getBackground() != grid1.getBackground()) return false;
  if (grid0.getMinValue() != grid1.getMinValue()) return false;
  if (grid0.getMaxValue() != grid1.getMaxValue()) return false;
  const auto sameFloat3{[](const float3 &a, const float3 &b) {
    return a.x == b.x && a.y == b.y && a.z == b.z;
  }};
  if (!sameFloat3(grid0.getWorldBoundMin(), grid1.getWorldBoundMin()) ||
      !sameFloat3(grid0.getWorldBoundMax(), grid1.getWorldBoundMax()))
    return false;
  for (int z = 0; z < extent.z; z++)
    for (int y = 0; y < extent.y; y++)
      for (int x = 0; x < extent.x; x++)
        if (grid0.fetch(x, y, z) != grid1.fetch(x, y, z)) return false;
  return true;
}

// Write a version-3 Mitsuba volume: the 48-byte header, then the
// single-channel float32 values x-fastest. Assumes a little-endian
// host, like the loader's own test fixtures elsewhere.
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
  SUBCASE("Saving") {
    // A 20x24x28 field with structure in every axis, so a transposed or
    // shifted write cannot pass.
    const int NX{20}, NY{24}, NZ{28};
    auto values{std::vector<float>()};
    for (int z = 0; z < NZ; z++)
      for (int y = 0; y < NY; y++)
        for (int x = 0; x < NX; x++)
          values.push_back(float(x) + 100.0f * float(y) + 10000.0f * float(z));
    auto sourceName{(tmpDir / "source.vol").string()};
    writeVol(sourceName, NX, NY, NZ, values);
    smdl::VoxelGrid source{};
    REQUIRE(!source.loadFromFile(sourceName));
    SUBCASE("Mitsuba volume") {
      auto fileName{(tmpDir / "saved.vol").string()};
      REQUIRE(!source.saveToFile(fileName));
      smdl::VoxelGrid grid{};
      REQUIRE(!grid.loadFromFile(fileName));
      CHECK(sameGrid(source, grid));
    }
    SUBCASE("NanoVDB") {
      if (!hasNanoVDB()) return;
      auto fileName{(tmpDir / "saved.nvdb").string()};
      REQUIRE(!source.saveToFile(fileName));
      smdl::VoxelGrid grid{};
      // The default name is what an unnamed save writes.
      REQUIRE(!grid.loadFromFile(fileName, "density"));
      CHECK(sameGrid(source, grid));
      // And back again, so the whole conversion the CLI performs is
      // covered in both directions.
      auto backName{(tmpDir / "back.vol").string()};
      REQUIRE(!grid.saveToFile(backName));
      smdl::VoxelGrid back{};
      REQUIRE(!back.loadFromFile(backName));
      CHECK(sameGrid(source, back));
    }
    SUBCASE("NanoVDB keeps the extent") {
      if (!hasNanoVDB()) return;
      // A 40x8x8 field that is nonzero only over x in [16, 24). NanoVDB
      // stores the active index bounds and the loader takes the extent
      // from them, so without anchoring this would come back 8x8x8 and
      // silently rescale texture space.
      const int MX{40}, MY{8}, MZ{8};
      auto sparse{std::vector<float>()};
      for (int z = 0; z < MZ; z++)
        for (int y = 0; y < MY; y++)
          for (int x = 0; x < MX; x++)
            sparse.push_back(x >= 16 && x < 24 ? 1.0f + float(x - 16) : 0.0f);
      auto borderName{(tmpDir / "border.vol").string()};
      writeVol(borderName, MX, MY, MZ, sparse);
      smdl::VoxelGrid border{};
      REQUIRE(!border.loadFromFile(borderName));
      REQUIRE(border.getExtent().x == MX);
      auto fileName{(tmpDir / "border.nvdb").string()};
      REQUIRE(!border.saveToFile(fileName));
      smdl::VoxelGrid grid{};
      REQUIRE(!grid.loadFromFile(fileName, "density"));
      CHECK(grid.getExtent().x == MX);
      CHECK(sameGrid(border, grid));
      // The anchor changes no value: the corner it pins still holds the
      // background, and the per-brick bounds are what they were.
      CHECK(grid.fetch(0, 0, 0) == 0.0f);
      CHECK(grid.fetch(MX - 1, MY - 1, MZ - 1) == 0.0f);
      CHECK(grid.getBrickMinValue(0, 0, 0) == border.getBrickMinValue(0, 0, 0));
      CHECK(grid.getBrickMaxValue(0, 0, 0) == border.getBrickMaxValue(0, 0, 0));
    }
    SUBCASE("Several named grids in one NanoVDB file") {
      if (!hasNanoVDB()) return;
      // A second field, distinguishable from the first everywhere.
      auto other{std::vector<float>()};
      for (float value : values) other.push_back(-2.0f * value - 1.0f);
      auto otherName{(tmpDir / "other.vol").string()};
      writeVol(otherName, NX, NY, NZ, other);
      smdl::VoxelGrid temperature{};
      REQUIRE(!temperature.loadFromFile(otherName));
      auto fileName{(tmpDir / "both.nvdb").string()};
      REQUIRE(!smdl::VoxelGrid::saveToFile(fileName, {&source, &temperature},
                                           {"density", "temperature"}));
      smdl::VoxelGrid grid{};
      REQUIRE(!grid.loadFromFile(fileName, "density"));
      CHECK(sameGrid(source, grid));
      REQUIRE(!grid.loadFromFile(fileName, "temperature"));
      CHECK(sameGrid(temperature, grid));
      // A name the file does not carry is an error, not the first grid.
      CHECK(grid.loadFromFile(fileName, "flame").has_value());
    }
    SUBCASE("Errors") {
      CHECK(source.saveToFile((tmpDir / "nope.xyz").string()).has_value());
      // Grid names are a NanoVDB concept.
      CHECK(source.saveToFile((tmpDir / "named.vol").string(), "density")
                .has_value());
      // An empty grid has nothing to write.
      smdl::VoxelGrid empty{};
      CHECK(empty.saveToFile((tmpDir / "empty.vol").string()).has_value());
      // Several grids need a NanoVDB file and matching, distinct names.
      auto fileName{(tmpDir / "several.nvdb").string()};
      CHECK(smdl::VoxelGrid::saveToFile((tmpDir / "several.vol").string(),
                                        {&source}, {"density"})
                .has_value());
      CHECK(smdl::VoxelGrid::saveToFile(fileName, {}, {}).has_value());
      CHECK(smdl::VoxelGrid::saveToFile(fileName, {&source}, {}).has_value());
      CHECK(smdl::VoxelGrid::saveToFile(fileName, {&source, &empty},
                                        {"density", "temperature"})
                .has_value());
      CHECK(smdl::VoxelGrid::saveToFile(fileName, {&source}, {""}).has_value());
      CHECK(smdl::VoxelGrid::saveToFile(fileName, {&source, &source},
                                        {"density", "density"})
                .has_value());
    }
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
