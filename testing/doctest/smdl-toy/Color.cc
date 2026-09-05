#include "doctest.h"

#include <algorithm>
#include <vector>

#include "Color.h"
#include "Render/Sampler.h"

// The wavelength jitter: the band rectangles the grid implies, the
// per-sample grid drawn inside them, and the offset sequence that draws
// it. What matters is that the rectangles tile the render band with no
// gap or overlap and that a band's samples are uniform over its own
// rectangle, since that is what makes the accumulated band the mean
// radiance over the band.

// The render-wide grid set the way `main()` does, so that the `Color`
// constructors and `jitterWavelengths()` agree on the band count, and
// put back on the way out: the whole suite shares one process, and the
// other cases build materials against the grid this one would leave
// behind.
namespace {

class ScopedGrid final {
public:
  explicit ScopedGrid(const std::vector<float> &wavelens) {
    renderGrid().reset(
        smdl::Span<const float>(wavelens.data(), wavelens.size()), true);
  }

  ScopedGrid(const ScopedGrid &) = delete;

  ScopedGrid &operator=(const ScopedGrid &) = delete;

  ~ScopedGrid() { renderGrid() = mSaved; }

private:
  const WavelengthGrid mSaved{renderGrid()};
};

} // namespace

TEST_CASE("Color wavelength band edges") {
  SUBCASE("A uniform grid tiles with bands of the spacing") {
    const auto wavelens{std::vector<float>{400, 500, 600, 700}};
    const auto edges{wavelengthBandEdges(
        smdl::Span<const float>(wavelens.data(), wavelens.size()))};
    REQUIRE(edges.size() == wavelens.size() + 1);
    // Mirrored ends, so every band is the spacing wide and centered on
    // its nominal wavelength.
    CHECK(edges[0] == doctest::Approx(350.0f));
    CHECK(edges[1] == doctest::Approx(450.0f));
    CHECK(edges[2] == doctest::Approx(550.0f));
    CHECK(edges[3] == doctest::Approx(650.0f));
    CHECK(edges[4] == doctest::Approx(750.0f));
    for (size_t i = 0; i < wavelens.size(); i++) {
      CHECK(edges[i + 1] - edges[i] == doctest::Approx(100.0f));
      CHECK(0.5f * (edges[i] + edges[i + 1]) == doctest::Approx(wavelens[i]));
    }
  }
  SUBCASE("A non-uniform grid splits each gap down the middle") {
    const auto wavelens{std::vector<float>{400, 420, 500, 900}};
    const auto edges{wavelengthBandEdges(
        smdl::Span<const float>(wavelens.data(), wavelens.size()))};
    REQUIRE(edges.size() == wavelens.size() + 1);
    CHECK(edges[0] == doctest::Approx(390.0f));
    CHECK(edges[1] == doctest::Approx(410.0f));
    CHECK(edges[2] == doctest::Approx(460.0f));
    CHECK(edges[3] == doctest::Approx(700.0f));
    CHECK(edges[4] == doctest::Approx(1100.0f));
  }
  SUBCASE("A grid with no band width has no rectangles") {
    const auto wavelens{std::vector<float>{550}};
    CHECK(wavelengthBandEdges(
              smdl::Span<const float>(wavelens.data(), wavelens.size()))
              .empty());
    CHECK(wavelengthBandEdges(smdl::Span<const float>()).empty());
  }
}

TEST_CASE("Color wavelength jitter") {
  const auto wavelens{std::vector<float>{400, 420, 500, 900}};
  const ScopedGrid grid{wavelens};
  const auto &edges{renderGrid().bandEdges};
  SUBCASE("The offset places every band at the same point of its band") {
    auto wavelengths{Color(smdl::Span<const float>(wavelens.data(), //
                                                   wavelens.size()))};
    jitterWavelengths(wavelengths, 0.0f);
    for (size_t i = 0; i < wavelens.size(); i++)
      CHECK(wavelengths[i] == doctest::Approx(edges[i]));
    jitterWavelengths(wavelengths, 1.0f);
    for (size_t i = 0; i < wavelens.size(); i++)
      CHECK(wavelengths[i] == doctest::Approx(edges[i + 1]));
    // The midpoint is the band center, which on a non-uniform grid is
    // NOT the nominal wavelength: the bands have to tile, so a band whose
    // neighbors sit at unequal distances holds its nominal wavelength off
    // center.
    jitterWavelengths(wavelengths, 0.5f);
    for (size_t i = 0; i < wavelens.size(); i++)
      CHECK(wavelengths[i] ==
            doctest::Approx(0.5f * (edges[i] + edges[i + 1])));
    CHECK(wavelengths[0] == doctest::Approx(400.0f));
    CHECK(wavelengths[1] == doctest::Approx(435.0f));
    // Still increasing, which the library requires of the grid: the
    // rectangles tile, so a shared offset cannot reorder them.
    CHECK(std::is_sorted(wavelengths.data(),
                         wavelengths.data() + wavelengths.size()));
  }
  SUBCASE("Every sample stays inside its own band and averages to it") {
    constexpr uint32_t NUM_SAMPLES = 4096;
    auto wavelengths{Color()};
    auto sums{std::vector<double>(wavelens.size())};
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

TEST_CASE("Color wavelength jitter offset") {
  SUBCASE("A pixel's offsets stratify") {
    // A power of two of an Owen-scrambled radical inverse falls exactly
    // one per stratum, which is the property the jitter is drawn this
    // way for.
    constexpr uint32_t NUM_SAMPLES = 256;
    auto hits{std::vector<int>(NUM_SAMPLES)};
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
