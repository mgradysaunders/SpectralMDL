#include "Fixtures.h"

#include <algorithm>
#include <cmath>
#include <fstream>
#include <vector>

#include "smdl/RenderUtil/MajorantWalk.h"
#include "smdl/Resource/VoxelGrid.h"
#include "smdl/Support/RNG.h"

using smdl::float2;
using smdl::float3;
using smdl::int3;
using smdl::MajorantSpan;
using smdl::MajorantSpanWalk;

// The majorant span walk against the grid it walks: every span a segment
// yields is checked against the cell its midpoint lies in, read back
// through the grid's own bounded lookup, and the spans of a segment tile
// it, so that the flat-index stepping and the outside detection cannot
// drift from the coordinate form without a mismatch here.

namespace {
// Write a version-3 Mitsuba volume, single-channel float32 x-fastest.
void writeVol(const std::string &fileName, int nx, int ny, int nz,
              const std::vector<float> &values) {
  std::ofstream file(fileName, std::ios::binary);
  file.write("VOL", 3);
  const char version{3};
  file.write(&version, 1);
  const int32_t header[5] = {1, nx, ny, nz, 1};
  file.write(reinterpret_cast<const char *>(header), sizeof(header));
  const float bound[6] = {0.0f, 0.0f, 0.0f, 1.0f, 1.0f, 1.0f};
  file.write(reinterpret_cast<const char *>(bound), sizeof(bound));
  file.write(reinterpret_cast<const char *>(values.data()),
             std::streamsize(values.size() * sizeof(float)));
}

// Every span of the segment, in order.
std::vector<MajorantSpan> spansOf(const smdl::VoxelGrid *grid,
                                  const float3 &org, const float3 &dir,
                                  float invMaxValue, float tEnd,
                                  bool shouldSkipEmpty) {
  std::vector<MajorantSpan> spans{};
  MajorantSpanWalk walk{grid, org, dir, invMaxValue, tEnd, shouldSkipEmpty};
  MajorantSpan span{};
  while (walk.next(span)) {
    spans.push_back(span);
    REQUIRE(spans.size() < 100000);
  }
  return spans;
}
} // namespace

TEST_CASE("MajorantSpanWalk: the spans a ray crosses through a grid") {
  TempDir tmpDir{"majorant-walk"};
  // A field whose cells are its voxels (the extent is within the cell
  // target), with a dense blob, a plane of small values, and empty space
  // around them, so that spans of every kind occur.
  constexpr int NX{40}, NY{24}, NZ{8};
  std::vector<float> values(size_t(NX) * NY * NZ);
  for (int z = 0; z < NZ; z++)
    for (int y = 0; y < NY; y++)
      for (int x = 0; x < NX; x++) {
        const float dx{float(x) - 26.0f}, dy{float(y) - 12.0f},
            dz{float(z) - 3.5f};
        float v{std::max(0.0f, 1.0f - (dx * dx + dy * dy + 4 * dz * dz) / 64)};
        if (x == 5) v = std::max(v, 0.05f);
        values[size_t(x + NX * (y + NY * z))] = v;
      }
  const auto fileName{(tmpDir / "field.vol").string()};
  writeVol(fileName, NX, NY, NZ, values);
  smdl::VoxelGrid grid{};
  REQUIRE_OK(grid.loadFromFile(fileName));
  REQUIRE(grid.getMajorantExtent() == 1);
  REQUIRE(smdl::isAllTrue(grid.getMajorantCount() == int3(NX, NY, NZ)));
  const float invMaxValue{1.0f / grid.getMaxValue()};
  const float3 boxMax{float(NX), float(NY), float(NZ)};

  const auto boundsAt{[&](const float3 &p) {
    return grid.getMajorantBounds(int(std::floor(p.x)), int(std::floor(p.y)),
                                  int(std::floor(p.z)));
  }};
  const auto isInside{[&](const float3 &p) {
    return p.x >= 0 && p.x < boxMax.x && p.y >= 0 && p.y < boxMax.y &&
           p.z >= 0 && p.z < boxMax.z;
  }};

  // Segments from inside and outside the box in random directions, and
  // axis-aligned ones so that zero direction components are covered.
  smdl::RNG rng{7};
  int numCellSpans{0}, numSkipped{0}, numOutsideSpans{0};
  for (int trial = 0; trial < 400; trial++) {
    float3 org{(rng.generateFloat() * 1.6f - 0.3f) * boxMax.x,
               (rng.generateFloat() * 1.6f - 0.3f) * boxMax.y,
               (rng.generateFloat() * 1.6f - 0.3f) * boxMax.z};
    float3 dir{rng.generateFloat() - 0.5f, rng.generateFloat() - 0.5f,
               rng.generateFloat() - 0.5f};
    if (trial % 7 == 1) dir.y = 0;
    if (trial % 7 == 2) dir.x = dir.z = 0;
    if (trial % 7 == 3) org = float3(3.5f, 3.5f, 3.5f);
    const float len{length(dir)};
    if (!(len > 0)) continue;
    dir = dir / len;
    const float tEnd{rng.generateFloat() * 120.0f};
    INFO("trial " << trial << " org " << org.x << "," << org.y << "," << org.z
                  << " dir " << dir.x << "," << dir.y << "," << dir.z
                  << " tEnd " << tEnd);

    const auto all{spansOf(&grid, org, dir, invMaxValue, tEnd, false)};
    REQUIRE(!all.empty());
    // The spans tile [0, tEnd) in order.
    CHECK(all.front().t0 == 0.0f);
    CHECK(all.back().t1 == tEnd);
    for (size_t i = 1; i < all.size(); i++) CHECK(all[i].t0 == all[i - 1].t1);
    // Each span is what the grid says about the cell of its midpoint, or
    // a global span outside the box. A sliver of a rounding step, at a
    // cell face or past the box, may land its midpoint in the neighbor
    // and is not held to either.
    for (const auto &span : all) {
      CHECK(span.t1 >= span.t0);
      const bool isSliver{span.t1 - span.t0 < 1e-3f};
      const float3 mid{org + 0.5f * (span.t0 + span.t1) * dir};
      const bool isGlobalSpan{span.scale == 1.0f && span.scaleMin == 0.0f};
      if (isInside(mid)) {
        const float2 bounds{boundsAt(mid)};
        const float scale{std::min(bounds.y * invMaxValue, 1.0f)};
        const float scaleMin{std::clamp(bounds.x * invMaxValue, 0.0f, scale)};
        const bool isCellSpan{span.scale == scale && span.scaleMin == scaleMin};
        CHECK((isCellSpan || isSliver));
        numCellSpans += isCellSpan;
      } else {
        CHECK((isGlobalSpan || isSliver));
        numOutsideSpans += isGlobalSpan;
      }
    }
    // Skipping empty cells drops exactly the spans with a zero scale.
    const auto kept{spansOf(&grid, org, dir, invMaxValue, tEnd, true)};
    std::vector<MajorantSpan> expected{};
    for (const auto &span : all)
      if (span.scale > 0.0f) expected.push_back(span);
    REQUIRE(kept.size() == expected.size());
    for (size_t i = 0; i < kept.size(); i++) {
      CHECK(kept[i].t0 == expected[i].t0);
      CHECK(kept[i].t1 == expected[i].t1);
      CHECK(kept[i].scale == expected[i].scale);
      CHECK(kept[i].scaleMin == expected[i].scaleMin);
    }
    numSkipped += int(all.size() - kept.size());
  }
  // The field was built so that every kind of span occurs.
  CHECK(numCellSpans > 1000);
  CHECK(numSkipped > 1000);
  CHECK(numOutsideSpans > 100);

  // No grid: the one global span, and nothing for an empty segment.
  {
    const auto spans{
        spansOf(nullptr, float3(), float3(1, 0, 0), 0.0f, 3.0f, true)};
    REQUIRE(spans.size() == 1);
    CHECK(spans[0].t0 == 0.0f);
    CHECK(spans[0].t1 == 3.0f);
    CHECK(spans[0].scale == 1.0f);
    CHECK(
        spansOf(nullptr, float3(), float3(1, 0, 0), 0.0f, 0.0f, true).empty());
  }
}
