#include "Fixtures.h"

#include <cmath>
#include <cstdint>
#include <cstring>
#include <fstream>
#include <string>
#include <vector>

#include "smdl/Common.h"
#include "smdl/Resource/VoxelGrid.h"

using smdl::float3;

namespace {
// Is NanoVDB in this build? The macro is private to the library, so the
// suite asks the same question the '--version' banner answers.
bool hasNanoVDB() { return smdl::BuildInfo::get().withNanoVDB != nullptr; }

// Do two grids agree everywhere: extent, background, value bounds,
// world bounds, and every voxel of the extent?
bool isSameGrid(const smdl::VoxelGrid &grid0, const smdl::VoxelGrid &grid1) {
  const smdl::int3 extent{grid0.getExtent()};
  if (!(extent.x == grid1.getExtent().x && extent.y == grid1.getExtent().y &&
        extent.z == grid1.getExtent().z))
    return false;
  if (grid0.getBackground() != grid1.getBackground()) return false;
  if (grid0.getMinValue() != grid1.getMinValue()) return false;
  if (grid0.getMaxValue() != grid1.getMaxValue()) return false;
  if (!isSame(grid0.getWorldBoundMin(), grid1.getWorldBoundMin()) ||
      !isSame(grid0.getWorldBoundMax(), grid1.getWorldBoundMax()))
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
void writeVol(const std::string &fileName, int nx, int ny, int nz,
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
} // namespace

TEST_CASE("VoxelGrid: the formats it round-trips and the majorants it builds") {
  TempDir tmpDir{"voxel-grid"};
  SUBCASE("A Mitsuba volume round-trips through the file") {
    // A 20x24x28 linear field, exactly representable in float.
    const int NX{20}, NY{24}, NZ{28};
    std::vector<float> values{};
    for (int z = 0; z < NZ; z++)
      for (int y = 0; y < NY; y++)
        for (int x = 0; x < NX; x++)
          values.push_back(float(x) + 100.0f * float(y) + 10000.0f * float(z));
    std::string fileName{(tmpDir / "linear.vol").string()};
    writeVol(fileName, NX, NY, NZ, values);
    smdl::VoxelGrid grid{};
    REQUIRE_OK(grid.loadFromFile(fileName));
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
  SUBCASE("A brick of nothing but zeros is not stored") {
    // A field three bricks wide in x that is zero except over the middle
    // brick and half of the last, so brick (0,0,0) is empty outright.
    const int B{smdl::VoxelGrid::BRICK_EXTENT};
    const int NX{3 * B}, NY{B}, NZ{B};
    const int lo{3 * B / 2}, hi{5 * B / 2};
    std::vector<float> values{};
    for (int z = 0; z < NZ; z++)
      for (int y = 0; y < NY; y++)
        for (int x = 0; x < NX; x++)
          values.push_back(x >= lo && x < hi ? 2.0f + float(x - lo) : 0.0f);
    std::string fileName{(tmpDir / "sparse.vol").string()};
    writeVol(fileName, NX, NY, NZ, values);
    smdl::VoxelGrid grid{};
    REQUIRE_OK(grid.loadFromFile(fileName));
    CHECK(grid.getBrickCount().x == 3);
    // The empty brick reads back as the background, and folds it into
    // the global minimum.
    CHECK(grid.fetch(0, 0, 0) == 0.0f);
    CHECK(grid.fetch(B - 1, B - 1, B - 1) == 0.0f);
    CHECK(grid.getMinValue() == 0.0f);
    CHECK(grid.getMaxValue() == 2.0f + float(B - 1));
    // Out-of-extent coordinates resolve to the background.
    CHECK(grid.fetch(NX, 0, 0) == 0.0f);
    CHECK(grid.fetch(-1, 0, 0) == 0.0f);
  }
  SUBCASE("A majorant cell bounds the voxels it covers") {
    // Three times the target cell count in x, which puts the derived
    // cell at four voxels whatever the target is, and one cell in the
    // other axes so the sweep at the end stays inside the cell it
    // checks. The field is zero except over x in [3E/2, 5E/2), so cell
    // 0 is empty even after its one-voxel dilation, cell 1 sees the
    // nonzero region through dilation and content, and cell 2 holds the
    // tail.
    const int NX{3 * smdl::VoxelGrid::MAJORANT_TARGET_CELLS};
    const int E{smdl::VoxelGrid::majorantExtentFor(smdl::int3(NX, 1, 1))};
    REQUIRE(E == 4);
    const int NY{E}, NZ{E};
    const int lo{3 * E / 2}, hi{5 * E / 2};
    std::vector<float> values{};
    for (int z = 0; z < NZ; z++)
      for (int y = 0; y < NY; y++)
        for (int x = 0; x < NX; x++)
          values.push_back(x >= lo && x < hi ? 2.0f + float(x - lo) : 0.0f);
    std::string fileName{(tmpDir / "cells.vol").string()};
    writeVol(fileName, NX, NY, NZ, values);
    smdl::VoxelGrid grid{};
    REQUIRE_OK(grid.loadFromFile(fileName));
    CHECK(grid.getMajorantExtent() == E);
    CHECK(grid.getMajorantCount().x == NX / E);
    CHECK(grid.getMajorantCount().y == 1);
    CHECK(grid.getMajorantCount().z == 1);
    // Cell 0 covers x in [0,E), its dilation reaches x of E, still
    // zero: the empty-cell bound is the background, twice.
    CHECK(grid.getMajorantBounds(0, 0, 0).x == 0.0f);
    CHECK(grid.getMajorantBounds(0, 0, 0).y == 0.0f);
    // Cell 1 covers x in [E,2E), dilated to [E-1,2E]: the maximum in
    // that window is at x of 2E, and the minimum is the zero it still
    // reaches at x of E-1.
    CHECK(grid.getMajorantBounds(1, 0, 0).y == 2.0f + float(E / 2));
    CHECK(grid.getMajorantBounds(1, 0, 0).x == 0.0f);
    // Cell 2 covers x in [2E,3E): the maximum of the whole field.
    CHECK(grid.getMajorantBounds(2, 0, 0).y == 2.0f + float(E - 1));
    CHECK(grid.getMaxValue() == 2.0f + float(E - 1));
    // Cell 3 is past the tail, and out-of-count cell queries resolve to
    // the background.
    CHECK(grid.getMajorantBounds(3, 0, 0).y == 0.0f);
    CHECK(grid.getMajorantBounds(NX / E, 0, 0).y == 0.0f);
    CHECK(grid.getMajorantBounds(-1, 0, 0).y == 0.0f);
    // The per-cell maximum must bound every trilinear sample whose
    // support touches the cell, and the minimum must fall under every
    // one; spot check against a sweep through the cell that owns the
    // discontinuity.
    bool isBounded{true};
    const smdl::float2 bounds{grid.getMajorantBounds(1, 0, 0)};
    for (int i = 0; i < 1000; i++) {
      const float3 coord{(float(E) + float(E) * float(i) / 999.0f) / float(NX),
                         0.4f, 0.6f};
      const float value{grid.sample(coord)};
      isBounded &= bounds.x <= value && value <= bounds.y;
    }
    CHECK(isBounded);
  }
  SUBCASE("Saving and loading back") {
    // A 20x24x28 field with structure in every axis, so a transposed or
    // shifted write cannot pass.
    const int NX{20}, NY{24}, NZ{28};
    std::vector<float> values{};
    for (int z = 0; z < NZ; z++)
      for (int y = 0; y < NY; y++)
        for (int x = 0; x < NX; x++)
          values.push_back(float(x) + 100.0f * float(y) + 10000.0f * float(z));
    std::string sourceName{(tmpDir / "source.vol").string()};
    writeVol(sourceName, NX, NY, NZ, values);
    smdl::VoxelGrid source{};
    REQUIRE_OK(source.loadFromFile(sourceName));
    SUBCASE("As a Mitsuba volume") {
      std::string fileName{(tmpDir / "saved.vol").string()};
      REQUIRE_OK(source.saveToFile(fileName));
      smdl::VoxelGrid grid{};
      REQUIRE_OK(grid.loadFromFile(fileName));
      CHECK(isSameGrid(source, grid));
    }
    SUBCASE("As a NanoVDB grid") {
      if (!hasNanoVDB()) return;
      std::string fileName{(tmpDir / "saved.nvdb").string()};
      REQUIRE_OK(source.saveToFile(fileName));
      smdl::VoxelGrid grid{};
      // The default name is what an unnamed save writes.
      REQUIRE_OK(grid.loadFromFile(fileName, "density"));
      CHECK(isSameGrid(source, grid));
      // And back again, so the whole conversion the CLI performs is
      // covered in both directions.
      std::string backName{(tmpDir / "back.vol").string()};
      REQUIRE_OK(grid.saveToFile(backName));
      smdl::VoxelGrid back{};
      REQUIRE_OK(back.loadFromFile(backName));
      CHECK(isSameGrid(source, back));
    }
    SUBCASE("As a NanoVDB grid, keeping the extent") {
      if (!hasNanoVDB()) return;
      // A 40x8x8 field that is nonzero only over x in [16, 24). NanoVDB
      // stores the active index bounds and the loader takes the extent
      // from them, so without anchoring this would come back 8x8x8 and
      // silently rescale texture space.
      const int MX{40}, MY{8}, MZ{8};
      std::vector<float> sparse{};
      for (int z = 0; z < MZ; z++)
        for (int y = 0; y < MY; y++)
          for (int x = 0; x < MX; x++)
            sparse.push_back(x >= 16 && x < 24 ? 1.0f + float(x - 16) : 0.0f);
      std::string borderName{(tmpDir / "border.vol").string()};
      writeVol(borderName, MX, MY, MZ, sparse);
      smdl::VoxelGrid border{};
      REQUIRE_OK(border.loadFromFile(borderName));
      REQUIRE(border.getExtent().x == MX);
      std::string fileName{(tmpDir / "border.nvdb").string()};
      REQUIRE_OK(border.saveToFile(fileName));
      smdl::VoxelGrid grid{};
      REQUIRE_OK(grid.loadFromFile(fileName, "density"));
      CHECK(grid.getExtent().x == MX);
      CHECK(isSameGrid(border, grid));
      // The anchor changes no value: the corner it pins still holds the
      // background, and the majorant bounds are what they were.
      CHECK(grid.fetch(0, 0, 0) == 0.0f);
      CHECK(grid.fetch(MX - 1, MY - 1, MZ - 1) == 0.0f);
      CHECK(grid.getMajorantBounds(0, 0, 0).x ==
            border.getMajorantBounds(0, 0, 0).x);
      CHECK(grid.getMajorantBounds(0, 0, 0).y ==
            border.getMajorantBounds(0, 0, 0).y);
    }
    SUBCASE("As several named grids in one NanoVDB file") {
      if (!hasNanoVDB()) return;
      // A second field, distinguishable from the first everywhere.
      std::vector<float> other{};
      for (float value : values) other.push_back(-2.0f * value - 1.0f);
      std::string otherName{(tmpDir / "other.vol").string()};
      writeVol(otherName, NX, NY, NZ, other);
      smdl::VoxelGrid temperature{};
      REQUIRE_OK(temperature.loadFromFile(otherName));
      std::string fileName{(tmpDir / "both.nvdb").string()};
      REQUIRE(!smdl::VoxelGrid::saveToFile(fileName, {&source, &temperature},
                                           {"density", "temperature"}));
      smdl::VoxelGrid grid{};
      REQUIRE_OK(grid.loadFromFile(fileName, "density"));
      CHECK(isSameGrid(source, grid));
      REQUIRE_OK(grid.loadFromFile(fileName, "temperature"));
      CHECK(isSameGrid(temperature, grid));
      // A name the file does not carry is an error, not the first grid,
      // and the error says which names it does carry.
      const std::optional<smdl::Error> error{
          grid.loadFromFile(fileName, "flame")};
      REQUIRE(error.has_value());
      CHECK_CONTAINS(error->message,
                     "No grid named \"flame\" in NanoVDB file, which holds "
                     "\"density\" and \"temperature\"");
      CHECK_NOT_CONTAINS(error->message, "converted from");
    }
    SUBCASE("An unwritable target is refused") {
      CHECK(source.saveToFile((tmpDir / "nope.xyz").string()).has_value());
      // Grid names are a NanoVDB concept.
      CHECK(source.saveToFile((tmpDir / "named.vol").string(), "density")
                .has_value());
      // An empty grid has nothing to write.
      smdl::VoxelGrid empty{};
      CHECK(empty.saveToFile((tmpDir / "empty.vol").string()).has_value());
      // Several grids need a NanoVDB file and matching, distinct names.
      std::string fileName{(tmpDir / "several.nvdb").string()};
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
  SUBCASE("A malformed or missing file is refused") {
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
    // A NanoVDB file that is missing or cannot be one is refused in the
    // library's own words, and a short one is refused before NanoVDB,
    // which never returns from reading it, can see it.
    if (hasNanoVDB()) {
      std::optional<smdl::Error> error{
          grid.loadFromFile((tmpDir / "missing.nvdb").string())};
      REQUIRE(error.has_value());
      CHECK_CONTAINS(error->message, "Cannot open");
      CHECK_NOT_CONTAINS(error->message, "converted from");
      std::ofstream((tmpDir / "short.nvdb").string()) << "not a NanoVDB file";
      error = grid.loadFromFile((tmpDir / "short.nvdb").string());
      REQUIRE(error.has_value());
      CHECK_CONTAINS(error->message, "Too short to be a NanoVDB file");
      std::ofstream((tmpDir / "junk.nvdb").string()) << std::string(4096, 'x');
      error = grid.loadFromFile((tmpDir / "junk.nvdb").string());
      REQUIRE(error.has_value());
      CHECK_NOT_CONTAINS(error->message, "converted from");
    }
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
