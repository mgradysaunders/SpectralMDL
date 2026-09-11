#include "RenderFixtures.h"

#include <cmath>
#include <string>
#include <vector>

#include "smdl/RenderUtil/SpectralFilm.h"

#include "Sensor/Response.h"

// The response is a per-sample photon integral of the spectral film
// against a curve in electrons per photon, under the rule the sample's
// wavelengths follow. What matters is that a curve counts the electrons
// its quantum efficiency says, that a relative curve is scaled to its
// peak with its ratios kept, that the two rules agree with each other
// and with a hand integration, and that a band narrower than the grid is
// honest only under the jitter, which is what the warning about it says.

namespace {

constexpr double PHOTONS_PER_JOULE_NM{1e-9 / (PLANCK * SPEED_OF_LIGHT)};

// A 25 nm grid from 400 to 700 nm, 13 bands.
[[nodiscard]] std::vector<float> coarseGrid() {
  auto grid{std::vector<float>()};
  for (float w = 400; w <= 700; w += 25) grid.push_back(w);
  return grid;
}

// One band from its knots, `relative` unless stated.
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

// The irradiance `lambda / 1000` at each wavelength: a slope, so that
// the weights matter.
[[nodiscard]] Color sloped(const Color &wavelengths) {
  auto E{Color()};
  for (size_t i = 0; i < E.size(); i++) E[i] = wavelengths[i] / 1000.0f;
  return E;
}

// One sample's projection onto the film bands.
[[nodiscard]] std::vector<double> projectOnce(const Response &response,
                                              const Color &wavelengths,
                                              const Color &E, size_t x = 0,
                                              size_t y = 0) {
  auto sums{std::vector<double>(response.filmBandCount())};
  response.accumulate(smdl::Span<const float>(wavelengths),
                      smdl::Span<const float>(E), x, y, sums.data());
  return sums;
}

// The mean projection over a stratified sweep of the jitter offset,
// which is what the band film converges to under the jitter. The
// irradiance is evaluated at each sweep's own wavelengths, as a render
// evaluates its materials at the sample's.
template <typename Irradiance>
[[nodiscard]] std::vector<double>
projectSwept(const Response &response, const Color &wavelengths,
             Irradiance &&irradiance, size_t numOffsets = 4096) {
  auto total{std::vector<double>(response.filmBandCount())};
  auto jittered{wavelengths};
  for (size_t k = 0; k < numOffsets; k++) {
    jitterWavelengths(jittered, (float(k) + 0.5f) / float(numOffsets));
    const auto sums{projectOnce(response, jittered, irradiance(jittered))};
    for (size_t b = 0; b < total.size(); b++) total[b] += sums[b];
  }
  for (auto &value : total) value /= double(numOffsets);
  return total;
}

// The hand integral of a flat `qe` under a flat irradiance `E` over the
// grid held still: the photon rate is the irradiance times lambda over
// h c, summed with the trapezoid widths.
[[nodiscard]] double flatOnStillGrid(const Color &wavelengths, double qe,
                                     double E) {
  const auto widths{wavelengthTrapezoidWidths(wavelengths)};
  double expected{};
  for (size_t i = 0; i < wavelengths.size(); i++)
    expected +=
        qe * E * widths[i] * double(wavelengths[i]) * PHOTONS_PER_JOULE_NM;
  return expected;
}

} // namespace

TEST_CASE("Response: a band counts the photoelectrons its curve turns the "
          "irradiance into") {
  const auto qe{oneBand("e", {380, 720}, {0.5, 0.5}, ResponseKind::QE)};
  SUBCASE("A flat curve over a flat irradiance is the photon integral, with "
          "the grid held still") {
    ScopedGrid scoped{coarseGrid(), false};
    const Response response{qe, scoped.wavelengths()};
    CHECK(response.bandCount() == 1);
    CHECK(response.filmBandCount() == 1);
    CHECK(response.filmBandNames() == std::vector<std::string>{"e"});
    CHECK(std::string(BAND_UNITS) == "electrons/(m^2 s)");
    const auto expected{flatOnStillGrid(scoped.wavelengths(), 0.5, 2.0)};
    const auto sums{projectOnce(response, scoped.wavelengths(), Color(2.0f))};
    CHECK(sums[0] == doctest::Approx(expected).epsilon(1e-12));
    // About 1e20: within reach of a float, but the sums are double anyway.
    CHECK(expected > 1e19);
    CHECK(expected < 1e21);
  }
  SUBCASE("And under the jitter, averaged over the offset, it is the "
          "integral over the rectangles") {
    ScopedGrid scoped{coarseGrid(), true};
    const Response response{qe, scoped.wavelengths()};
    // The rectangles run 387.5 to 712.5 nm, over which the integral of
    // lambda is closed form.
    const double integralOfLambda{(712.5 * 712.5 - 387.5 * 387.5) / 2};
    const double expected{0.5 * 2.0 * integralOfLambda * PHOTONS_PER_JOULE_NM};
    const auto sums{projectSwept(response, scoped.wavelengths(),
                                 [](const Color &) { return Color(2.0f); })};
    CHECK(sums[0] == doctest::Approx(expected).epsilon(1e-4));
  }
  SUBCASE("A curve reaching past the grid integrates the part inside") {
    const auto wide{oneBand("all", {300, 1000}, {0.5, 0.5}, ResponseKind::QE)};
    ScopedGrid scoped{coarseGrid(), false};
    const Response response{wide, scoped.wavelengths()};
    const auto expected{flatOnStillGrid(scoped.wavelengths(), 0.5, 2.0)};
    const auto sums{projectOnce(response, scoped.wavelengths(), Color(2.0f))};
    CHECK(sums[0] == doctest::Approx(expected).epsilon(1e-12));
  }
  SUBCASE("A curve the grid cannot see at all is refused") {
    const auto nir{oneBand("nir", {800, 900, 1000}, {0, 1, 0})};
    ScopedGrid scoped{coarseGrid(), false};
    CHECK_THROWS((void)Response(nir, scoped.wavelengths()));
  }
}

TEST_CASE("Response: a relative curve is scaled to its peak quantum "
          "efficiency, ratios kept") {
  ScopedGrid scoped{coarseGrid(), false};
  const auto &wavelengths{scoped.wavelengths()};
  auto settings{oneBand("a", {380, 720}, {1, 1})};
  auto &b{settings.bands.emplace_back()};
  b.name = "b";
  b.wavelengths = {380, 720};
  b.values = {0.5f, 0.5f};
  SUBCASE("Unstated, the peak is the generic one, and the second band reads "
          "its ratio of it") {
    CHECK(settings.qeScale() == doctest::Approx(DEFAULT_PEAK_QE));
    const Response response{settings, wavelengths};
    const auto sums{projectOnce(response, wavelengths, Color(2.0f))};
    CHECK(sums[0] ==
          doctest::Approx(flatOnStillGrid(wavelengths, DEFAULT_PEAK_QE, 2.0))
              .epsilon(1e-12));
    CHECK(sums[1] == doctest::Approx(0.5 * sums[0]).epsilon(1e-12));
  }
  SUBCASE("Stated, it scales the whole set, so the ratio holds") {
    settings.peakQE = 0.8f;
    CHECK(settings.qeScale() == doctest::Approx(0.8));
    const Response response{settings, wavelengths};
    const auto sums{projectOnce(response, wavelengths, Color(2.0f))};
    CHECK(sums[0] ==
          doctest::Approx(flatOnStillGrid(wavelengths, double(0.8f), 2.0))
              .epsilon(1e-12));
    CHECK(sums[1] == doctest::Approx(0.5 * sums[0]).epsilon(1e-12));
  }
  SUBCASE("A qe curve is taken as written, and a relative one at the same "
          "peak reads the same") {
    const auto asQE{oneBand("a", {380, 720}, {0.5, 0.5}, ResponseKind::QE)};
    const auto asRelative{oneBand("a", {380, 720}, {1, 1})};
    CHECK(asQE.qeScale() == 1.0);
    const auto qeSums{
        projectOnce(Response{asQE, wavelengths}, wavelengths, Color(2.0f))};
    const auto relativeSums{projectOnce(Response{asRelative, wavelengths},
                                        wavelengths, Color(2.0f))};
    CHECK(qeSums[0] == doctest::Approx(relativeSums[0]).epsilon(1e-12));
    CHECK(responseHash(asQE) == responseHash(asRelative));
  }
  SUBCASE("A set of zeros has no peak and scales to nothing") {
    const auto zeros{oneBand("z", {380, 720}, {0, 0})};
    CHECK(zeros.qeScale() == 0.0);
  }
}

TEST_CASE("Response: a band narrower than the grid needs the jitter") {
  // A 10 nm triangle centered between two grid wavelengths, at the
  // generic peak.
  const auto narrow{oneBand("line", {557, 562, 567}, {0, 1, 0})};
  SUBCASE("With the grid held still no sample can see it, which is refused") {
    ScopedGrid scoped{coarseGrid(), false};
    CHECK_THROWS((void)Response(narrow, scoped.wavelengths()));
  }
  SUBCASE("Under the jitter it integrates the irradiance over its own "
          "width") {
    ScopedGrid scoped{coarseGrid(), true};
    const Response response{narrow, scoped.wavelengths()};
    // The triangle has an area of 5 nm about 562 nm, where the slope
    // reads 0.562, to the sweep's precision and the triangle's own
    // width, though it straddles the edge between two of the grid's
    // rectangles.
    const double expected{double(DEFAULT_PEAK_QE) * 5.0 * 0.562 * 562.0 *
                          PHOTONS_PER_JOULE_NM};
    const auto sums{projectSwept(response, scoped.wavelengths(), sloped)};
    CHECK(sums[0] == doctest::Approx(expected).epsilon(1e-3));
  }
  SUBCASE("A narrow band that does land on a grid wavelength projects, with "
          "the warning") {
    ScopedGrid scoped{coarseGrid(), false};
    const auto onNode{oneBand("line", {545, 550, 555}, {0, 1, 0})};
    const Response response{onNode, scoped.wavelengths()};
    // The one node it lands on, at the generic peak, over the 25 nm the
    // grid gives that node.
    const double expected{double(DEFAULT_PEAK_QE) * 25.0 * 3.0 * 550.0 *
                          PHOTONS_PER_JOULE_NM};
    const auto sums{projectOnce(response, scoped.wavelengths(), Color(3.0f))};
    CHECK(sums[0] == doctest::Approx(expected).epsilon(1e-12));
  }
}

TEST_CASE("Response: the band film is the projection of the spectral film") {
  // With the grid held still the projection is linear in the irradiance,
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
    auto E{Color()};
    for (size_t i = 0; i < E.size(); i++)
      E[i] = 0.1f * float((s * 7 + i * 3) % 11) + 0.01f * float(i);
    film.addTotals(0, 0, E.data());
    response.accumulate(smdl::Span<const float>(wavelengths),
                        smdl::Span<const float>(E), 0, 0, sums.data());
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
    const auto E{sloped(scoped.wavelengths())};
    const auto all{projectOnce(plain, scoped.wavelengths(), E)};
    REQUIRE(all.size() == 3);
    CHECK(projectOnce(tiled, scoped.wavelengths(), E, 0, 0)[0] == all[0]);
    CHECK(projectOnce(tiled, scoped.wavelengths(), E, 1, 0)[0] == all[1]);
    CHECK(projectOnce(tiled, scoped.wavelengths(), E, 0, 1)[0] == all[1]);
    CHECK(projectOnce(tiled, scoped.wavelengths(), E, 1, 1)[0] == all[2]);
    CHECK(projectOnce(tiled, scoped.wavelengths(), E, 1, 0)[0] ==
          projectOnce(tiled, scoped.wavelengths(), E, 0, 1)[0]);
  }
}

TEST_CASE("Response: the fingerprint and the file beside the film") {
  SUBCASE("The hash follows the knots in electrons per photon, the peak "
          "included") {
    auto a{oneBand("vis", {380, 720}, {1, 1})};
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
    auto f{a};
    f.peakQE = 0.6f;
    CHECK(responseHash(a) != responseHash(f));
    auto g{a};
    g.peakQE = DEFAULT_PEAK_QE;
    CHECK(responseHash(a) == responseHash(g));
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
