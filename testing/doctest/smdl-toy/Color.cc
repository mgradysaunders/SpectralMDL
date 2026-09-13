#include "RenderFixtures.h"

#include <algorithm>
#include <vector>

#include "Color.h"
#include "Render/Sampler.h"

// The wavelength grid: the cells a list of wavelengths tiles into, the
// per-sample grid drawn inside them, and the offset sequence that draws
// it. What matters is that the cells tile the list's span with no gap
// or overlap, each as wide as the trapezoid rule weighs its wavelength,
// and that a band's samples are uniform over its own cell, since that
// is what makes the accumulated band the mean radiance over the band and
// the jittered grid integrate what the grid held still does.

TEST_CASE("WavelengthGrid: the cells a list of wavelengths tiles into") {
  SUBCASE("A uniform list has cells of the spacing, halved at the ends, "
          "edged halfway between neighbors") {
    const ScopedGrid scoped{{400, 500, 600, 700}, false};
    REQUIRE(gRenderGrid.numBands == 4);
    CHECK(gRenderGrid.first().bandEdges ==
          std::vector<double>{400, 450, 550, 650, 700});
    CHECK(gRenderGrid.first().widths == std::vector<double>{50, 100, 100, 50});
    // The inner cells are centered on their wavelengths; the end cells
    // stop at the list's ends, so their wavelengths sit on their outer
    // edges.
    for (size_t i = 1; i + 1 < 4; i++)
      CHECK(0.5 * (gRenderGrid.first().bandEdges[i] +
                   gRenderGrid.first().bandEdges[i + 1]) ==
            double(gRenderGrid.first().wavelengths[i]));
    CHECK(gRenderGrid.first().bandEdges.front() ==
          double(gRenderGrid.first().wavelengths[0]));
    CHECK(gRenderGrid.first().bandEdges.back() ==
          double(gRenderGrid.first().wavelengths[3]));
  }
  SUBCASE("A non-uniform list splits each gap down the middle, so a band "
          "weighs half the distance between its neighbors") {
    const ScopedGrid scoped{{400, 450, 600, 700}, false};
    CHECK(gRenderGrid.first().bandEdges ==
          std::vector<double>{400, 425, 525, 650, 700});
    CHECK(gRenderGrid.first().widths == std::vector<double>{25, 100, 125, 50});
    const ScopedGrid wider{{400, 420, 500, 900}, false};
    CHECK(gRenderGrid.first().bandEdges ==
          std::vector<double>{400, 410, 460, 700, 900});
    CHECK(gRenderGrid.first().widths == std::vector<double>{10, 50, 240, 200});
  }
  SUBCASE("The widths are the cells' own and tile the span, on any list") {
    for (const auto &grid :
         {std::vector<float>{400, 500, 600, 700},
          std::vector<float>{400, 450, 600, 700},
          std::vector<float>{380, 391.3f, 402.7f, 461, 720}}) {
      const ScopedGrid scoped{grid, false};
      const std::vector<double> &edges{gRenderGrid.first().bandEdges};
      const std::vector<double> &widths{gRenderGrid.first().widths};
      REQUIRE(edges.size() == widths.size() + 1);
      double sum{};
      for (size_t i = 0; i < widths.size(); i++) {
        CHECK(widths[i] == edges[i + 1] - edges[i]);
        sum += widths[i];
      }
      CHECK(sum == doctest::Approx(double(grid.back()) - double(grid.front())));
    }
  }
  SUBCASE("The JIT is handed the widths as floats, on any list") {
    for (const auto &grid : {std::vector<float>{400, 500, 600, 700},
                             std::vector<float>{400, 450, 600, 700}}) {
      const ScopedGrid scoped{grid, false};
      REQUIRE(gRenderGrid.first().weights.size() ==
              gRenderGrid.first().widths.size());
      CHECK(gRenderGrid.stateBase.wavelengthWeight ==
            gRenderGrid.first().weights.data());
      for (size_t i = 0; i < gRenderGrid.first().widths.size(); i++)
        CHECK(gRenderGrid.first().weights[i] ==
              float(gRenderGrid.first().widths[i]));
    }
  }
  SUBCASE("The jitter is a flag on the grid") {
    const ScopedGrid still{{400, 500, 600, 700}, false};
    CHECK(!gRenderGrid.isJittering);
    const ScopedGrid jittered{{400, 500, 600, 700}, true};
    CHECK(gRenderGrid.isJittering);
  }
  SUBCASE("A placed grid keeps its own cells, its labels inside them, and "
          "spans its edges") {
    WavelengthCells cells{};
    cells.edges = {400, 430, 500, 700};
    cells.wavelengths = {410, 470, 560};
    REQUIRE(cells.isValid());
    const ScopedGrid scoped{cells, true};
    CHECK(gRenderGrid.first().bandEdges == cells.edges);
    CHECK(gRenderGrid.first().widths == std::vector<double>{30, 70, 200});
    CHECK(gRenderGrid.first().weights == std::vector<float>{30, 70, 200});
    CHECK(gRenderGrid.first().wavelengths[1] == 470.0f);
    CHECK(gRenderGrid.stateBase.wavelengthMin == 400.0f);
    CHECK(gRenderGrid.stateBase.wavelengthMax == 700.0f);
    CHECK(gRenderGrid.isJittering);
  }
  SUBCASE("Cells that do not describe a grid are told apart") {
    WavelengthCells cells{};
    cells.edges = {400, 430, 500, 700};
    cells.wavelengths = {410, 470, 560};
    CHECK(cells.isValid());
    cells.wavelengths[2] = 720;
    CHECK(!cells.isValid());
    cells.wavelengths[2] = 560;
    cells.edges[2] = 430;
    CHECK(!cells.isValid());
    cells.edges = {400, 700};
    CHECK(!cells.isValid());
    cells.edges.clear();
    CHECK(!cells.isValid());
    cells.wavelengths = {550};
    CHECK(cells.isValid());
    cells.wavelengths.clear();
    CHECK(!cells.isValid());
  }
  SUBCASE("A list of one wavelength has no cells, weighs half a unit by "
          "convention, and cannot jitter") {
    const ScopedGrid scoped{{550}, true};
    CHECK(gRenderGrid.first().bandEdges.empty());
    CHECK(gRenderGrid.first().widths == std::vector<double>{0.5});
    CHECK(gRenderGrid.first().weights == std::vector<float>{0.5f});
    CHECK(!gRenderGrid.isJittering);
    CHECK(wavelengthBandEdges(smdl::Span<const float>()).empty());
  }
}

TEST_CASE("RenderGrid: one grid per tile band") {
  const auto listOf{[](std::vector<float> list) {
    return WavelengthCells::fromWavelengths(
        smdl::Span<const float>(list.data(), list.size()));
  }};
  const std::vector<WavelengthCells> family{listOf({400, 500, 600, 700}),
                                            listOf({500, 550, 600, 650})};
  SUBCASE("Each pixel evaluates on the grid its tile cell names, anchored "
          "at the frame's origin") {
    const ScopedGrid scoped{family, 2, {0, 1, 1, 0}, false};
    REQUIRE(gRenderGrid.grids.size() == 2);
    CHECK(gRenderGrid.hasTile());
    CHECK(gRenderGrid.numBands == 4);
    CHECK(gRenderGrid.gridIndexAt(0, 0) == 0);
    CHECK(gRenderGrid.gridIndexAt(1, 0) == 1);
    CHECK(gRenderGrid.gridIndexAt(0, 1) == 1);
    CHECK(gRenderGrid.gridIndexAt(1, 1) == 0);
    CHECK(gRenderGrid.gridIndexAt(2, 0) == 0);
    CHECK(gRenderGrid.gridIndexAt(3, 1) == 0);
    CHECK(gRenderGrid.at(1, 0).wavelengths[0] == 500.0f);
    CHECK(&gRenderGrid.first() == &gRenderGrid.grids[0]);
    CHECK(gRenderGrid.stateBase.wavelengthMin == 400.0f);
    CHECK(gRenderGrid.minWavelength() == 400.0f);
    CHECK(gRenderGrid.maxWavelength() == 700.0f);
  }
  SUBCASE("One grid has no tile and is every pixel's") {
    const ScopedGrid scoped{{400, 500, 600, 700}, false};
    CHECK(!gRenderGrid.hasTile());
    CHECK(gRenderGrid.grids.size() == 1);
    CHECK(gRenderGrid.gridIndexAt(5, 7) == 0);
  }
  SUBCASE("A state built for a path on a grid carries that grid") {
    const ScopedGrid scoped{family, 2, {0, 1, 1, 0}, true};
    const smdl::State state{
        makeRenderState(gRenderGrid.at(1, 0).wavelengths, nullptr, 0.0f,
                        gRenderGrid.stateBase.wavelengthHero, 1)};
    CHECK(state.wavelengthMin == 500.0f);
    CHECK(state.wavelengthMax == 650.0f);
    CHECK(state.wavelengthWeight == gRenderGrid.grids[1].weights.data());
    Color drawn{};
    jitterWavelengths(drawn, gRenderGrid.at(1, 0), 0.5f);
    CHECK(drawn[0] == doctest::Approx(512.5f));
  }
}

TEST_CASE("jitterWavelengths: every sample inside its own band") {
  const std::vector<float> wavelens{400, 420, 500, 900};
  const ScopedGrid grid{wavelens, true};
  const std::vector<double> &edges{gRenderGrid.first().bandEdges};
  SUBCASE("The offset places every band at the same point of its band") {
    Color wavelengths{Color(smdl::Span<const float>(wavelens.data(), //
                                                    wavelens.size()))};
    jitterWavelengths(wavelengths, gRenderGrid.first(), 0.0f);
    for (size_t i = 0; i < wavelens.size(); i++)
      CHECK(wavelengths[i] == doctest::Approx(edges[i]));
    jitterWavelengths(wavelengths, gRenderGrid.first(), 1.0f);
    for (size_t i = 0; i < wavelens.size(); i++)
      CHECK(wavelengths[i] == doctest::Approx(edges[i + 1]));
    // The midpoint is the band center, which is NOT the nominal
    // wavelength of an end band, or of a band whose neighbors sit at
    // unequal distances: the bands have to tile the grid's span, so the
    // first runs from its nominal wavelength to the halfway point.
    jitterWavelengths(wavelengths, gRenderGrid.first(), 0.5f);
    for (size_t i = 0; i < wavelens.size(); i++)
      CHECK(wavelengths[i] ==
            doctest::Approx(0.5f * (edges[i] + edges[i + 1])));
    CHECK(wavelengths[0] == doctest::Approx(405.0f));
    CHECK(wavelengths[1] == doctest::Approx(435.0f));
    // Still increasing, which the library requires of the grid: the
    // rectangles tile, so a shared offset cannot reorder them.
    CHECK(std::is_sorted(wavelengths.data(),
                         wavelengths.data() + wavelengths.size()));
  }
  SUBCASE("Every sample stays inside its own band and averages to it") {
    constexpr uint32_t NUM_SAMPLES = 4096;
    Color wavelengths{};
    std::vector<double> sums(wavelens.size());
    for (uint32_t index = 0; index < NUM_SAMPLES; index++) {
      jitterWavelengths(wavelengths, gRenderGrid.first(),
                        wavelengthJitterOffset(7, index));
      for (size_t i = 0; i < wavelens.size(); i++) {
        CHECK(wavelengths[i] >= edges[i]);
        CHECK(wavelengths[i] <= edges[i + 1]);
        sums[i] += wavelengths[i];
      }
    }
    // The mean of a uniform draw over the rectangle is its center, which
    // is what makes the accumulated band the mean over the band.
    for (size_t i = 0; i < wavelens.size(); i++)
      CHECK(sums[i] / NUM_SAMPLES ==
            doctest::Approx(0.5f * (edges[i] + edges[i + 1])).epsilon(0.001));
  }
}

TEST_CASE("wavelengthJitterOffset: the sequence a pixel draws") {
  SUBCASE("A pixel's offsets stratify") {
    // A power of two of an Owen-scrambled radical inverse falls exactly
    // one per stratum, which is the property the jitter is drawn this
    // way for.
    constexpr uint32_t NUM_SAMPLES = 256;
    std::vector<int> hits(NUM_SAMPLES);
    for (uint32_t index = 0; index < NUM_SAMPLES; index++) {
      const float xi{wavelengthJitterOffset(12345, index)};
      REQUIRE(xi > 0.0f);
      REQUIRE(xi < 1.0f);
      hits[size_t(xi * NUM_SAMPLES)]++;
    }
    for (auto count : hits) CHECK(count == 1);
  }
  SUBCASE("Neighboring pixels draw different sequences") {
    int same{};
    for (uint32_t index = 0; index < 64; index++)
      if (wavelengthJitterOffset(100, index) ==
          wavelengthJitterOffset(101, index))
        same++;
    CHECK(same == 0);
  }
}

TEST_CASE("Shutter: the frame fraction of a line") {
  Shutter shutter{};
  shutter.time = 2.0f;
  shutter.exposure = 0.01f;
  shutter.readout = 0.03f;
  shutter.numReadoutLines = 7;
  SUBCASE("Without a readout the draw is the fraction, bit for bit") {
    shutter.readout = 0;
    for (const float exposure : {0.0f, 0.01f, 0.3f, 7.0f}) {
      shutter.exposure = exposure;
      for (const float xi : {0.0f, 0.1f, 1.0f / 3.0f, 0.7f, 0.999f, 1.0f})
        CHECK(shutter.fractionAt(3, 5, xi) == xi);
    }
  }
  SUBCASE("The first line at xi 0 opens the frame and the last at xi 1 "
          "shuts it") {
    // Lengths whose sum and ratio are not representable, so that an
    // offset spelled as the readout times the line over the count would
    // not land on the readout at the last line.
    shutter.exposure = 0.1f;
    shutter.readout = 0.7f;
    CHECK(shutter.fractionAt(4, 0, 0.0f) == 0.0f);
    CHECK(shutter.fractionAt(4, 6, 1.0f) == 1.0f);
    // Nothing leaves the frame, and a later line is never earlier.
    for (const float xi : {0.0f, 0.25f, 0.999f, 1.0f}) {
      float previous{-1.0f};
      for (size_t y = 0; y < 7; y++) {
        const float fraction{shutter.fractionAt(4, y, xi)};
        CHECK(fraction >= 0.0f);
        CHECK(fraction <= 1.0f);
        CHECK(fraction > previous);
        previous = fraction;
      }
    }
  }
  SUBCASE("A shut exposure lands each line at its own instant, whatever "
          "the draw") {
    shutter.exposure = 0;
    for (size_t y = 0; y < 7; y++)
      for (const float xi : {0.0f, 0.5f, 1.0f})
        CHECK(shutter.fractionAt(2, y, xi) == float(y) / 6.0f);
  }
  SUBCASE("Each direction orders the lines its own way") {
    shutter.exposure = 0;
    for (size_t i = 0; i < 7; i++) {
      const float forward{float(i) / 6.0f};
      const float backward{float(6 - i) / 6.0f};
      shutter.isReadoutAlongX = false;
      shutter.isReadoutReversed = false;
      CHECK(shutter.fractionAt(0, i, 0.5f) == forward); // down
      shutter.isReadoutReversed = true;
      CHECK(shutter.fractionAt(0, i, 0.5f) == backward); // up
      shutter.isReadoutAlongX = true;
      shutter.isReadoutReversed = false;
      CHECK(shutter.fractionAt(i, 0, 0.5f) == forward); // right
      shutter.isReadoutReversed = true;
      CHECK(shutter.fractionAt(i, 0, 0.5f) == backward); // left
    }
  }
  SUBCASE("One line has no sweep") {
    shutter.numReadoutLines = 1;
    CHECK(shutter.fractionAt(0, 0, 0.5f) ==
          doctest::Approx(0.5f * 0.01f / 0.04f));
  }
  SUBCASE("The seconds follow the frame") {
    CHECK(shutter.length() == doctest::Approx(0.04f));
    CHECK(shutter.secondsAt(0.0f) == doctest::Approx(2.0f));
    CHECK(shutter.secondsAt(1.0f) == doctest::Approx(2.04f));
    CHECK(shutter.spansTime());
    CHECK(shutter.hasExposure());
    shutter.exposure = 0;
    CHECK(shutter.spansTime());
    CHECK(!shutter.hasExposure());
    shutter.readout = 0;
    CHECK(!shutter.spansTime());
  }
}
