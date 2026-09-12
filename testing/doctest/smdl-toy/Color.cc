#include "RenderFixtures.h"

#include <algorithm>
#include <vector>

#include "Color.h"
#include "Render/Sampler.h"

// The wavelength jitter: the band rectangles the grid implies, the
// per-sample grid drawn inside them, and the offset sequence that draws
// it. What matters is that the rectangles tile the grid's span with no
// gap or overlap, each as wide as the trapezoid weighs its band, and
// that a band's samples are uniform over its own rectangle, since that
// is what makes the accumulated band the mean radiance over the band and
// the jittered grid integrate what the grid held still does.

TEST_CASE("wavelengthTrapezoidWidths: what every integral over the grid "
          "weighs") {
  SUBCASE("A uniform grid weighs every band its spacing, halved at the ends") {
    ScopedGrid scoped{{400, 500, 600, 700}, false};
    const std::vector<double> widths{
        wavelengthTrapezoidWidths(scoped.wavelengths())};
    REQUIRE(widths.size() == 4);
    CHECK(widths[0] == 50.0);
    CHECK(widths[1] == 100.0);
    CHECK(widths[2] == 100.0);
    CHECK(widths[3] == 50.0);
    CHECK(widths[0] + widths[1] + widths[2] + widths[3] == 300.0);
  }
  SUBCASE("A non-uniform grid weighs each band half the distance between its "
          "neighbors") {
    const std::vector<float> grid{400, 450, 600, 700};
    const std::vector<double> widths{wavelengthTrapezoidWidths(
        smdl::Span<const float>(grid.data(), grid.size()))};
    REQUIRE(widths.size() == 4);
    CHECK(widths[0] == 25.0);
    CHECK(widths[1] == 100.0);
    CHECK(widths[2] == 125.0);
    CHECK(widths[3] == 50.0);
    CHECK(widths[0] + widths[1] + widths[2] + widths[3] == 300.0);
  }
  SUBCASE("A grid of one band is half a unit wide") {
    const std::vector<float> grid{550};
    const std::vector<double> widths{wavelengthTrapezoidWidths(
        smdl::Span<const float>(grid.data(), grid.size()))};
    REQUIRE(widths.size() == 1);
    CHECK(widths[0] == 0.5);
  }
  SUBCASE("The JIT is handed the same widths, on any grid") {
    for (const auto &grid : {std::vector<float>{400, 500, 600, 700},
                             std::vector<float>{400, 450, 600, 700}}) {
      const ScopedGrid scoped{grid, false};
      const std::vector<double> widths{
          wavelengthTrapezoidWidths(scoped.wavelengths())};
      REQUIRE(gRenderGrid.weights.size() == widths.size());
      CHECK(gRenderGrid.stateBase.wavelengthWeight ==
            gRenderGrid.weights.data());
      for (size_t i = 0; i < widths.size(); i++)
        CHECK(gRenderGrid.weights[i] == float(widths[i]));
    }
  }
}

TEST_CASE("wavelengthBandEdges: the rectangles a grid tiles into") {
  SUBCASE("A uniform grid tiles its span with bands of the spacing, halved "
          "at the ends") {
    const std::vector<float> wavelens{400, 500, 600, 700};
    const std::vector<float> edges{wavelengthBandEdges(
        smdl::Span<const float>(wavelens.data(), wavelens.size()))};
    REQUIRE(edges.size() == wavelens.size() + 1);
    // The end bands stop at the grid's ends, half the spacing wide; the
    // inner ones are the spacing wide and centered on their nominal
    // wavelengths.
    CHECK(edges[0] == doctest::Approx(400.0f));
    CHECK(edges[1] == doctest::Approx(450.0f));
    CHECK(edges[2] == doctest::Approx(550.0f));
    CHECK(edges[3] == doctest::Approx(650.0f));
    CHECK(edges[4] == doctest::Approx(700.0f));
    for (size_t i = 1; i + 1 < wavelens.size(); i++) {
      CHECK(edges[i + 1] - edges[i] == doctest::Approx(100.0f));
      CHECK(0.5f * (edges[i] + edges[i + 1]) == doctest::Approx(wavelens[i]));
    }
  }
  SUBCASE("A non-uniform grid splits each gap down the middle") {
    const std::vector<float> wavelens{400, 420, 500, 900};
    const std::vector<float> edges{wavelengthBandEdges(
        smdl::Span<const float>(wavelens.data(), wavelens.size()))};
    REQUIRE(edges.size() == wavelens.size() + 1);
    CHECK(edges[0] == doctest::Approx(400.0f));
    CHECK(edges[1] == doctest::Approx(410.0f));
    CHECK(edges[2] == doctest::Approx(460.0f));
    CHECK(edges[3] == doctest::Approx(700.0f));
    CHECK(edges[4] == doctest::Approx(900.0f));
  }
  SUBCASE("Every band is as wide as the trapezoid weighs it") {
    for (const auto &wavelens : {std::vector<float>{400, 500, 600, 700},
                                 std::vector<float>{400, 420, 500, 900}}) {
      const ScopedGrid grid{wavelens, true};
      const std::vector<float> &edges{gRenderGrid.bandEdges};
      const std::vector<double> widths{
          wavelengthTrapezoidWidths(grid.wavelengths())};
      REQUIRE(edges.size() == widths.size() + 1);
      for (size_t i = 0; i < widths.size(); i++)
        CHECK(double(edges[i + 1]) - double(edges[i]) ==
              doctest::Approx(widths[i]));
    }
  }
  SUBCASE("A grid with no band width has no rectangles") {
    const std::vector<float> wavelens{550};
    CHECK(wavelengthBandEdges(
              smdl::Span<const float>(wavelens.data(), wavelens.size()))
              .empty());
    CHECK(wavelengthBandEdges(smdl::Span<const float>()).empty());
  }
}

TEST_CASE("jitterWavelengths: every sample inside its own band") {
  const std::vector<float> wavelens{400, 420, 500, 900};
  const ScopedGrid grid{wavelens, true};
  const std::vector<float> &edges{gRenderGrid.bandEdges};
  SUBCASE("The offset places every band at the same point of its band") {
    Color wavelengths{Color(smdl::Span<const float>(wavelens.data(), //
                                                    wavelens.size()))};
    jitterWavelengths(wavelengths, 0.0f);
    for (size_t i = 0; i < wavelens.size(); i++)
      CHECK(wavelengths[i] == doctest::Approx(edges[i]));
    jitterWavelengths(wavelengths, 1.0f);
    for (size_t i = 0; i < wavelens.size(); i++)
      CHECK(wavelengths[i] == doctest::Approx(edges[i + 1]));
    // The midpoint is the band center, which is NOT the nominal
    // wavelength of an end band, or of a band whose neighbors sit at
    // unequal distances: the bands have to tile the grid's span, so the
    // first runs from its nominal wavelength to the halfway point.
    jitterWavelengths(wavelengths, 0.5f);
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
      jitterWavelengths(wavelengths, wavelengthJitterOffset(7, index));
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
