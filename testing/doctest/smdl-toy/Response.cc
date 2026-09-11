#include "RenderFixtures.h"

#include <cmath>
#include <string>
#include <vector>

#include "smdl/RenderUtil/SpectralFilm.h"

#include "Response.h"

// The response is a per-sample quadrature of radiance against a curve,
// under the rule the sample's wavelengths follow. What matters is that
// a flat curve returns the radiance it averages, that the two rules
// agree with each other and with a hand integration, and that a band
// narrower than the grid is honest only under the jitter, which is what
// the warning about it says.

namespace {

// A 25 nm grid from 400 to 700 nm, 13 bands.
[[nodiscard]] std::vector<float> coarseGrid() {
  auto grid{std::vector<float>()};
  for (float w = 400; w <= 700; w += 25) grid.push_back(w);
  return grid;
}

// One `relative` band from its knots.
[[nodiscard]] ResponseSettings
oneBand(const char *name, std::vector<float> wavelengths,
        std::vector<float> values, ResponseKind kind = ResponseKind::RELATIVE) {
  auto settings{ResponseSettings()};
  settings.kind = kind;
  auto &band{settings.bands.emplace_back()};
  band.name = name;
  band.wavelengths = std::move(wavelengths);
  band.values = std::move(values);
  return settings;
}

// The radiance `lambda / 1000` at each wavelength: a slope, so that the
// weights matter.
[[nodiscard]] Color sloped(const Color &wavelengths) {
  auto L{Color()};
  for (size_t i = 0; i < L.size(); i++) L[i] = wavelengths[i] / 1000.0f;
  return L;
}

// One sample's projection onto the film bands.
[[nodiscard]] std::vector<double> projectOnce(const Response &response,
                                              const Color &wavelengths,
                                              const Color &L, size_t x = 0,
                                              size_t y = 0) {
  auto sums{std::vector<double>(response.filmBandCount())};
  response.accumulate(smdl::Span<const float>(wavelengths),
                      smdl::Span<const float>(L), x, y, sums.data());
  return sums;
}

// The mean projection over a stratified sweep of the jitter offset,
// which is what the band film converges to under the jitter. The
// radiance is evaluated at each sweep's own wavelengths, as a render
// evaluates its materials at the sample's.
template <typename Radiance>
[[nodiscard]] std::vector<double>
projectSwept(const Response &response, const Color &wavelengths,
             Radiance &&radiance, size_t numOffsets = 4096) {
  auto total{std::vector<double>(response.filmBandCount())};
  auto jittered{wavelengths};
  for (size_t k = 0; k < numOffsets; k++) {
    jitterWavelengths(jittered, (float(k) + 0.5f) / float(numOffsets));
    const auto sums{projectOnce(response, jittered, radiance(jittered))};
    for (size_t b = 0; b < total.size(); b++) total[b] += sums[b];
  }
  for (auto &value : total) value /= double(numOffsets);
  return total;
}

} // namespace

TEST_CASE("Response: a relative band is the radiance averaged over its "
          "curve") {
  const auto flat{oneBand("vis", {380, 720}, {1, 1})};
  SUBCASE("A flat curve over a sloped spectrum returns the mean, with the "
          "grid held still") {
    ScopedGrid scoped{coarseGrid(), false};
    const Response response{flat, scoped.wavelengths()};
    CHECK(response.bandCount() == 1);
    CHECK(response.filmBandCount() == 1);
    CHECK(response.filmBandNames() == std::vector<std::string>{"vis"});
    CHECK(std::string(response.units()) == "W/(m^2 sr nm)");
    // The trapezoid mean of a line over 400 to 700 is its midpoint, to
    // the precision of the float radiance.
    const auto sums{projectOnce(response, scoped.wavelengths(),
                                sloped(scoped.wavelengths()))};
    CHECK(sums[0] == doctest::Approx(0.55).epsilon(1e-6));
  }
  SUBCASE("And under the jitter, averaged over the offset") {
    ScopedGrid scoped{coarseGrid(), true};
    const Response response{flat, scoped.wavelengths()};
    // The rectangles run 387.5 to 712.5, whose mean is the same midpoint.
    const auto sums{projectSwept(response, scoped.wavelengths(), sloped)};
    CHECK(sums[0] == doctest::Approx(0.55).epsilon(1e-6));
  }
  SUBCASE("A flat spectrum passes through any curve as itself") {
    const auto bumpy{
        oneBand("g", {400, 500, 550, 600, 700}, {0, 1, 0.2, 1, 0})};
    ScopedGrid scoped{coarseGrid(), false};
    const Response response{bumpy, scoped.wavelengths()};
    const auto sums{projectOnce(response, scoped.wavelengths(), Color(2.5f))};
    CHECK(sums[0] == doctest::Approx(2.5).epsilon(1e-12));
  }
  SUBCASE("A curve reaching past the grid averages over the part inside") {
    const auto wide{oneBand("all", {300, 1000}, {1, 1})};
    ScopedGrid scoped{coarseGrid(), false};
    const Response response{wide, scoped.wavelengths()};
    const auto sums{projectOnce(response, scoped.wavelengths(),
                                sloped(scoped.wavelengths()))};
    CHECK(sums[0] == doctest::Approx(0.55).epsilon(1e-6));
  }
  SUBCASE("A curve the grid cannot see at all is refused") {
    const auto nir{oneBand("nir", {800, 900, 1000}, {0, 1, 0})};
    ScopedGrid scoped{coarseGrid(), false};
    CHECK_THROWS((void)Response(nir, scoped.wavelengths()));
  }
}

TEST_CASE("Response: a qe band counts electrons") {
  ScopedGrid scoped{coarseGrid(), false};
  const auto qe{oneBand("e", {380, 720}, {0.5, 0.5}, ResponseKind::QE)};
  const Response response{qe, scoped.wavelengths()};
  CHECK(std::string(response.units()) == "electrons/(m^2 sr s)");
  const auto &wavelengths{scoped.wavelengths()};
  const auto widths{wavelengthTrapezoidWidths(wavelengths)};
  // Half an electron per photon of a flat 2 W/(m^2 sr nm), summed over the
  // grid: the photon rate is the radiance times lambda over h c.
  double expected{};
  for (size_t i = 0; i < wavelengths.size(); i++)
    expected += 0.5 * 2.0 * widths[i] * double(wavelengths[i]) * 1e-9 /
                (PLANCK * SPEED_OF_LIGHT);
  const auto sums{projectOnce(response, wavelengths, Color(2.0f))};
  CHECK(sums[0] == doctest::Approx(expected).epsilon(1e-12));
  // About 1e20: within reach of a float, but the sums are double anyway.
  CHECK(expected > 1e19);
  CHECK(expected < 1e21);
}

TEST_CASE("Response: a band narrower than the grid needs the jitter") {
  // A 10 nm triangle centered between two grid wavelengths.
  const auto narrow{oneBand("line", {557, 562, 567}, {0, 1, 0})};
  SUBCASE("With the grid held still no sample can see it, which is refused") {
    ScopedGrid scoped{coarseGrid(), false};
    CHECK_THROWS((void)Response(narrow, scoped.wavelengths()));
  }
  SUBCASE("Under the jitter it averages the radiance over its own width") {
    ScopedGrid scoped{coarseGrid(), true};
    const Response response{narrow, scoped.wavelengths()};
    // The line at 562 nm reads the slope there, to the sweep's precision,
    // though it straddles the edge between two of the grid's rectangles.
    const auto sums{projectSwept(response, scoped.wavelengths(), sloped)};
    CHECK(sums[0] == doctest::Approx(0.562).epsilon(1e-3));
  }
  SUBCASE("A narrow band that does land on a grid wavelength projects, with "
          "the warning") {
    ScopedGrid scoped{coarseGrid(), false};
    const auto onNode{oneBand("line", {545, 550, 555}, {0, 1, 0})};
    const Response response{onNode, scoped.wavelengths()};
    const auto sums{projectOnce(response, scoped.wavelengths(), Color(3.0f))};
    CHECK(sums[0] == doctest::Approx(3.0).epsilon(1e-12));
  }
}

TEST_CASE("Response: the band film is the projection of the spectral film") {
  // With the grid held still the projection is linear in the radiance,
  // so the mean of the per-sample projections is the projection of the
  // mean: the band file is a hand integration of the spectral one.
  ScopedGrid scoped{coarseGrid(), false};
  const auto bumpy{oneBand("g", {400, 500, 550, 600, 700}, {0, 1, 0.2, 1, 0})};
  const Response response{bumpy, scoped.wavelengths()};
  const auto &wavelengths{scoped.wavelengths()};
  constexpr size_t NUM_SAMPLES = 8;
  auto film{smdl::SpectralFilm(wavelengths.size(), 1, 1)};
  auto sums{std::vector<double>(1)};
  for (size_t s = 0; s < NUM_SAMPLES; s++) {
    auto L{Color()};
    for (size_t i = 0; i < L.size(); i++)
      L[i] = 0.1f * float((s * 7 + i * 3) % 11) + 0.01f * float(i);
    film.addTotals(0, 0, L.data());
    response.accumulate(smdl::Span<const float>(wavelengths),
                        smdl::Span<const float>(L), 0, 0, sums.data());
  }
  film.addSamples(NUM_SAMPLES);
  auto mean{Color()};
  for (size_t i = 0; i < mean.size(); i++) mean[i] = float(film.mean(0, 0, i));
  const auto ofMean{projectOnce(response, wavelengths, mean)};
  CHECK(sums[0] / double(NUM_SAMPLES) ==
        doctest::Approx(ofMean[0]).epsilon(1e-6));
}

TEST_CASE("Response: the tile picks one band per pixel") {
  ScopedGrid scoped{coarseGrid(), false};
  auto settings{ResponseSettings()};
  for (const auto *name : {"R", "G", "B"}) {
    auto &band{settings.bands.emplace_back()};
    band.name = name;
    band.wavelengths = {400, 550, 700};
    band.values = {name[0] == 'B' ? 1.0f : 0.0f, name[0] == 'G' ? 1.0f : 0.0f,
                   name[0] == 'R' ? 1.0f : 0.0f};
  }
  const auto untiled{settings};
  settings.cfaColumns = 2;
  settings.cfa = {0, 1, 1, 2};
  SUBCASE("The film has one band, and the tile repeats from the frame's "
          "origin") {
    const Response response{settings, scoped.wavelengths()};
    CHECK(response.hasTile());
    CHECK(response.bandCount() == 3);
    CHECK(response.filmBandCount() == 1);
    CHECK(response.filmBandNames() == std::vector<std::string>{"mosaic"});
    CHECK(response.tileColumns() == 2);
    CHECK(response.tileNames() == std::vector<std::string>{"R", "G", "G", "B"});
    for (size_t y = 0; y < 4; y++)
      for (size_t x = 0; x < 4; x++)
        CHECK(response.bandAt(x, y) == settings.cfa[(y % 2) * 2 + x % 2]);
    CHECK(response.bandAt(7, 5) == 2);
    CHECK(response.bandAt(6, 5) == 1);
  }
  SUBCASE("A 5 by 5 tile indexes the same way at a crop window's offset") {
    auto five{untiled};
    five.cfaColumns = 5;
    for (size_t i = 0; i < 25; i++) five.cfa.push_back(i % 3);
    const Response response{five, scoped.wavelengths()};
    for (size_t y = 30; y < 40; y++)
      for (size_t x = 100; x < 110; x++)
        CHECK(response.bandAt(x, y) == five.cfa[(y % 5) * 5 + x % 5]);
  }
  SUBCASE("The mosaic value is the picked band of the untiled projection, "
          "with the two greens identical") {
    const Response tiled{settings, scoped.wavelengths()};
    const Response plain{untiled, scoped.wavelengths()};
    const auto L{sloped(scoped.wavelengths())};
    const auto all{projectOnce(plain, scoped.wavelengths(), L)};
    REQUIRE(all.size() == 3);
    CHECK(projectOnce(tiled, scoped.wavelengths(), L, 0, 0)[0] == all[0]);
    CHECK(projectOnce(tiled, scoped.wavelengths(), L, 1, 0)[0] == all[1]);
    CHECK(projectOnce(tiled, scoped.wavelengths(), L, 0, 1)[0] == all[1]);
    CHECK(projectOnce(tiled, scoped.wavelengths(), L, 1, 1)[0] == all[2]);
    CHECK(projectOnce(tiled, scoped.wavelengths(), L, 1, 0)[0] ==
          projectOnce(tiled, scoped.wavelengths(), L, 0, 1)[0]);
  }
}

TEST_CASE("Response: the fingerprint and the file beside the film") {
  SUBCASE("The hash ignores the name and follows the knots") {
    auto a{oneBand("vis", {380, 720}, {1, 1})};
    auto b{a};
    b.name = "Relabeled";
    CHECK(responseHash(a) == responseHash(b));
    CHECK(responseHash(a).size() == 32);
    auto c{a};
    c.bands[0].values[1] = 0.999f;
    CHECK(responseHash(a) != responseHash(c));
    auto d{a};
    d.kind = ResponseKind::QE;
    CHECK(responseHash(a) != responseHash(d));
    auto e{a};
    e.cfaColumns = 1;
    e.cfa = {0};
    CHECK(responseHash(a) != responseHash(e));
  }
  SUBCASE("The film band names are the bands, or the one mosaic") {
    auto settings{oneBand("vis", {380, 720}, {1, 1})};
    auto &nir{settings.bands.emplace_back()};
    nir.name = "nir";
    nir.wavelengths = {700, 1000};
    nir.values = {1, 1};
    CHECK(responseFilmBandNames(settings) ==
          std::vector<std::string>{"vis", "nir"});
    settings.cfaColumns = 2;
    settings.cfa = {0, 1};
    CHECK(responseFilmBandNames(settings) ==
          std::vector<std::string>{"mosaic"});
  }
  SUBCASE("The band film sits beside the spectral one, before the extension") {
    CHECK(bandFilmFileName("dir/out.img") == "dir/out-bands.img");
    CHECK(bandFilmFileName("out") == "out-bands");
    CHECK(bandFilmFileName("a.b.img") == "a.b-bands.img");
  }
}
