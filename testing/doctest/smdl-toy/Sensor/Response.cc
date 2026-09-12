#include "RenderFixtures.h"

#include <algorithm>
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
  std::vector<float> grid{};
  for (float w = 400; w <= 700; w += 25) grid.push_back(w);
  return grid;
}

// The illuminant a tiled response draws a dispersive lens's wavelengths
// under, which only the tests of that draw look at.
[[nodiscard]] SensorSpectrum d65() { return daylightSpectrum(D65_KELVIN); }

// One band from its knots, `relative` unless stated.
[[nodiscard]] ResponseSettings
oneBand(const char *name, std::vector<float> wavelengths,
        std::vector<float> values, ResponseKind kind = ResponseKind::RELATIVE) {
  ResponseSettings settings{};
  settings.kind = kind;
  ResponseBand &band{settings.bands.emplace_back()};
  band.name = name;
  band.wavelengths = std::move(wavelengths);
  band.values = std::move(values);
  return settings;
}

// The irradiance `lambda / 1000` at each wavelength: a slope, so that
// the weights matter.
[[nodiscard]] Color sloped(const Color &wavelengths) {
  Color E{};
  for (size_t i = 0; i < E.size(); i++) E[i] = wavelengths[i] / 1000.0f;
  return E;
}

// One sample's projection onto the film bands.
[[nodiscard]] std::vector<double> projectOnce(const Response &response,
                                              const Color &wavelengths,
                                              const Color &E, size_t x = 0,
                                              size_t y = 0) {
  std::vector<double> sums(response.filmBandCount());
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
  std::vector<double> total(response.filmBandCount());
  Color jittered{wavelengths};
  for (size_t k = 0; k < numOffsets; k++) {
    jitterWavelengths(jittered, (float(k) + 0.5f) / float(numOffsets));
    const std::vector<double> sums{
        projectOnce(response, jittered, irradiance(jittered))};
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
  const std::vector<double> widths{wavelengthTrapezoidWidths(wavelengths)};
  double expected{};
  for (size_t i = 0; i < wavelengths.size(); i++)
    expected +=
        qe * E * widths[i] * double(wavelengths[i]) * PHOTONS_PER_JOULE_NM;
  return expected;
}

} // namespace

TEST_CASE("Response: a band counts the photoelectrons its curve turns the "
          "irradiance into") {
  const ResponseSettings qe{
      oneBand("e", {380, 720}, {0.5, 0.5}, ResponseKind::QE)};
  SUBCASE("A flat curve over a flat irradiance is the photon integral, with "
          "the grid held still") {
    ScopedGrid scoped{coarseGrid(), false};
    const Response response{qe, scoped.wavelengths(), d65()};
    CHECK(response.bandCount() == 1);
    CHECK(response.filmBandCount() == 1);
    CHECK(response.filmBandNames() == std::vector<std::string>{"e"});
    CHECK(std::string(BAND_UNITS) == "electrons/(m^2 s)");
    const double expected{flatOnStillGrid(scoped.wavelengths(), 0.5, 2.0)};
    const std::vector<double> sums{
        projectOnce(response, scoped.wavelengths(), Color(2.0f))};
    CHECK(sums[0] == doctest::Approx(expected).epsilon(1e-12));
    // About 1e20: within reach of a float, but the sums are double anyway.
    CHECK(expected > 1e19);
    CHECK(expected < 1e21);
  }
  SUBCASE("And under the jitter, averaged over the offset, it is the same "
          "integral") {
    ScopedGrid scoped{coarseGrid(), true};
    const Response response{qe, scoped.wavelengths(), d65()};
    // The rectangles tile the grid's own 400 to 700 nm, over which the
    // integral of lambda is closed form, and which the trapezoid of the
    // grid held still takes exactly, lambda being a line.
    const double integralOfLambda{(700.0 * 700.0 - 400.0 * 400.0) / 2};
    const double expected{0.5 * 2.0 * integralOfLambda * PHOTONS_PER_JOULE_NM};
    CHECK(flatOnStillGrid(scoped.wavelengths(), 0.5, 2.0) ==
          doctest::Approx(expected).epsilon(1e-12));
    const std::vector<double> sums{
        projectSwept(response, scoped.wavelengths(),
                     [](const Color &) { return Color(2.0f); })};
    CHECK(sums[0] == doctest::Approx(expected).epsilon(1e-4));
  }
  SUBCASE("A curve reaching past the grid integrates the part inside") {
    const ResponseSettings wide{
        oneBand("all", {300, 1000}, {0.5, 0.5}, ResponseKind::QE)};
    ScopedGrid scoped{coarseGrid(), false};
    const Response response{wide, scoped.wavelengths(), d65()};
    const double expected{flatOnStillGrid(scoped.wavelengths(), 0.5, 2.0)};
    const std::vector<double> sums{
        projectOnce(response, scoped.wavelengths(), Color(2.0f))};
    CHECK(sums[0] == doctest::Approx(expected).epsilon(1e-12));
  }
  SUBCASE("A curve the grid cannot see at all is refused") {
    const ResponseSettings nir{oneBand("nir", {800, 900, 1000}, {0, 1, 0})};
    ScopedGrid scoped{coarseGrid(), false};
    CHECK_THROWS((void)Response(nir, scoped.wavelengths(), d65()));
  }
}

TEST_CASE("Response: a relative curve is scaled to its peak quantum "
          "efficiency, ratios kept") {
  ScopedGrid scoped{coarseGrid(), false};
  const Color &wavelengths{scoped.wavelengths()};
  ResponseSettings settings{oneBand("a", {380, 720}, {1, 1})};
  ResponseBand &b{settings.bands.emplace_back()};
  b.name = "b";
  b.wavelengths = {380, 720};
  b.values = {0.5f, 0.5f};
  SUBCASE("Unstated, the peak is the generic one, and the second band reads "
          "its ratio of it") {
    CHECK(settings.qeScale() == doctest::Approx(DEFAULT_PEAK_QE));
    const Response response{settings, wavelengths, d65()};
    const std::vector<double> sums{
        projectOnce(response, wavelengths, Color(2.0f))};
    CHECK(sums[0] ==
          doctest::Approx(flatOnStillGrid(wavelengths, DEFAULT_PEAK_QE, 2.0))
              .epsilon(1e-12));
    CHECK(sums[1] == doctest::Approx(0.5 * sums[0]).epsilon(1e-12));
  }
  SUBCASE("Stated, it scales the whole set, so the ratio holds") {
    settings.peakQE = 0.8f;
    CHECK(settings.qeScale() == doctest::Approx(0.8));
    const Response response{settings, wavelengths, d65()};
    const std::vector<double> sums{
        projectOnce(response, wavelengths, Color(2.0f))};
    CHECK(sums[0] ==
          doctest::Approx(flatOnStillGrid(wavelengths, double(0.8f), 2.0))
              .epsilon(1e-12));
    CHECK(sums[1] == doctest::Approx(0.5 * sums[0]).epsilon(1e-12));
  }
  SUBCASE("A qe curve is taken as written, and a relative one at the same "
          "peak reads the same") {
    const ResponseSettings asQE{
        oneBand("a", {380, 720}, {0.5, 0.5}, ResponseKind::QE)};
    const ResponseSettings asRelative{oneBand("a", {380, 720}, {1, 1})};
    CHECK(asQE.qeScale() == 1.0);
    const std::vector<double> qeSums{projectOnce(
        Response{asQE, wavelengths, d65()}, wavelengths, Color(2.0f))};
    const std::vector<double> relativeSums{projectOnce(
        Response{asRelative, wavelengths, d65()}, wavelengths, Color(2.0f))};
    CHECK(qeSums[0] == doctest::Approx(relativeSums[0]).epsilon(1e-12));
    CHECK(responseHash(asQE) == responseHash(asRelative));
  }
  SUBCASE("A set of zeros has no peak and scales to nothing") {
    const ResponseSettings zeros{oneBand("z", {380, 720}, {0, 0})};
    CHECK(zeros.qeScale() == 0.0);
  }
}

TEST_CASE("Response: a band narrower than the grid needs the jitter") {
  // A 10 nm triangle centered between two grid wavelengths, at the
  // generic peak.
  const ResponseSettings narrow{oneBand("line", {557, 562, 567}, {0, 1, 0})};
  SUBCASE("With the grid held still no sample can see it, which is refused") {
    ScopedGrid scoped{coarseGrid(), false};
    CHECK_THROWS((void)Response(narrow, scoped.wavelengths(), d65()));
  }
  SUBCASE("Under the jitter it integrates the irradiance over its own "
          "width") {
    ScopedGrid scoped{coarseGrid(), true};
    const Response response{narrow, scoped.wavelengths(), d65()};
    // The triangle has an area of 5 nm about 562 nm, where the slope
    // reads 0.562, to the sweep's precision and the triangle's own
    // width, though it straddles the edge between two of the grid's
    // rectangles.
    const double expected{double(DEFAULT_PEAK_QE) * 5.0 * 0.562 * 562.0 *
                          PHOTONS_PER_JOULE_NM};
    const std::vector<double> sums{
        projectSwept(response, scoped.wavelengths(), sloped)};
    CHECK(sums[0] == doctest::Approx(expected).epsilon(1e-3));
  }
  SUBCASE("A narrow band that does land on a grid wavelength projects, with "
          "the warning") {
    ScopedGrid scoped{coarseGrid(), false};
    const ResponseSettings onNode{oneBand("line", {545, 550, 555}, {0, 1, 0})};
    const Response response{onNode, scoped.wavelengths(), d65()};
    // The one node it lands on, at the generic peak, over the 25 nm the
    // grid gives that node.
    const double expected{double(DEFAULT_PEAK_QE) * 25.0 * 3.0 * 550.0 *
                          PHOTONS_PER_JOULE_NM};
    const std::vector<double> sums{
        projectOnce(response, scoped.wavelengths(), Color(3.0f))};
    CHECK(sums[0] == doctest::Approx(expected).epsilon(1e-12));
  }
}

TEST_CASE("Response: the band film is the projection of the spectral film") {
  // With the grid held still the projection is linear in the irradiance,
  // so the mean of the per-sample projections is the projection of the
  // mean: the band file is a hand integration of the spectral one.
  ScopedGrid scoped{coarseGrid(), false};
  const ResponseSettings bumpy{
      oneBand("g", {400, 500, 550, 600, 700}, {0, 1, 0.2, 1, 0})};
  const Response response{bumpy, scoped.wavelengths(), d65()};
  const Color &wavelengths{scoped.wavelengths()};
  constexpr size_t NUM_SAMPLES = 8;
  smdl::SpectralFilm film{wavelengths.size(), 1, 1};
  std::vector<double> sums(1);
  for (size_t s = 0; s < NUM_SAMPLES; s++) {
    Color E{};
    for (size_t i = 0; i < E.size(); i++)
      E[i] = 0.1f * float((s * 7 + i * 3) % 11) + 0.01f * float(i);
    film.addTotals(0, 0, E.data());
    response.accumulate(smdl::Span<const float>(wavelengths),
                        smdl::Span<const float>(E), 0, 0, sums.data());
  }
  film.addSamples(NUM_SAMPLES);
  Color mean{};
  for (size_t i = 0; i < mean.size(); i++) mean[i] = float(film.mean(0, 0, i));
  const std::vector<double> ofMean{projectOnce(response, wavelengths, mean)};
  CHECK(sums[0] / double(NUM_SAMPLES) ==
        doctest::Approx(ofMean[0]).epsilon(1e-6));
}

TEST_CASE("Response: the tile picks one band per pixel") {
  ScopedGrid scoped{coarseGrid(), false};
  ResponseSettings settings{};
  for (const auto *name : {"R", "G", "B"}) {
    ResponseBand &band{settings.bands.emplace_back()};
    band.name = name;
    band.wavelengths = {400, 550, 700};
    band.values = {name[0] == 'B' ? 1.0f : 0.0f, name[0] == 'G' ? 1.0f : 0.0f,
                   name[0] == 'R' ? 1.0f : 0.0f};
  }
  const ResponseSettings untiled{settings};
  settings.cfaColumns = 2;
  settings.cfa = {0, 1, 1, 2};
  SUBCASE("The film has one band, and the tile repeats from the frame's "
          "origin") {
    const Response response{settings, scoped.wavelengths(), d65()};
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
    ResponseSettings five{untiled};
    five.cfaColumns = 5;
    for (size_t i = 0; i < 25; i++) five.cfa.push_back(i % 3);
    const Response response{five, scoped.wavelengths(), d65()};
    for (size_t y = 30; y < 40; y++)
      for (size_t x = 100; x < 110; x++)
        CHECK(response.bandAt(x, y) == five.cfa[(y % 5) * 5 + x % 5]);
  }
  SUBCASE("The mosaic value is the picked band of the untiled projection, "
          "with the two greens identical") {
    const Response tiled{settings, scoped.wavelengths(), d65()};
    const Response plain{untiled, scoped.wavelengths(), d65()};
    const Color E{sloped(scoped.wavelengths())};
    const std::vector<double> all{projectOnce(plain, scoped.wavelengths(), E)};
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
    ResponseSettings a{oneBand("vis", {380, 720}, {1, 1})};
    CHECK(responseHash(a).size() == 32);
    ResponseSettings c{a};
    c.bands[0].values[1] = 0.999f;
    CHECK(responseHash(a) != responseHash(c));
    ResponseSettings d{a};
    d.kind = ResponseKind::QE;
    CHECK(responseHash(a) != responseHash(d));
    ResponseSettings e{a};
    e.cfaColumns = 1;
    e.cfa = {0};
    CHECK(responseHash(a) != responseHash(e));
    ResponseSettings f{a};
    f.peakQE = 0.6f;
    CHECK(responseHash(a) != responseHash(f));
    ResponseSettings g{a};
    g.peakQE = DEFAULT_PEAK_QE;
    CHECK(responseHash(a) == responseHash(g));
  }
  SUBCASE("The film band names are the bands, or the one mosaic") {
    ResponseSettings settings{oneBand("vis", {380, 720}, {1, 1})};
    ResponseBand &nir{settings.bands.emplace_back()};
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

namespace {

// The illuminant at `lambda` nanometers, linear between the whole
// nanometers of the sensor's grid, as the draw reads it.
[[nodiscard]] double illuminantAt(const SensorSpectrum &illuminant,
                                  double lambda) {
  const double t{lambda - SENSOR_WAVELENGTH_MIN};
  const size_t i{size_t(t)};
  return illuminant[i] + (t - double(i)) * (illuminant[i + 1] - illuminant[i]);
}

// Three shaped bands tiled R G / G B: R reaches past the coarse grid's red
// end, G is a line 10 nm wide, and B starts before the grid's blue end.
[[nodiscard]] ResponseSettings shapedTile() {
  ResponseSettings settings{};
  const auto add{[&](const char *name, std::vector<float> wavelengths,
                     std::vector<float> values) {
    ResponseBand &band{settings.bands.emplace_back()};
    band.name = name;
    band.wavelengths = std::move(wavelengths);
    band.values = std::move(values);
  }};
  add("R", {550, 600, 700, 720}, {0, 1, 1, 0});
  add("G", {557, 562, 567}, {0, 1, 0});
  add("B", {380, 420, 480, 500}, {0, 1, 1, 0});
  settings.cfaColumns = 2;
  settings.cfa = {0, 1, 1, 2};
  return settings;
}

// What pixel `(x, y)` draws over a stratified sweep of the number it
// draws at.
[[nodiscard]] std::vector<float> drawsAt(const Response &response, size_t x,
                                         size_t y) {
  constexpr size_t NUM_DRAWS = 4096;
  std::vector<float> draws{};
  for (size_t k = 0; k < NUM_DRAWS; k++)
    draws.push_back(
        response.traceWavelengthAt(x, y, (float(k) + 0.5f) / float(NUM_DRAWS)));
  return draws;
}

} // namespace

TEST_CASE("Response: the wavelength a lens whose glasses disperse is traced "
          "at") {
  // Under the jitter, since the line falls between the grid's wavelengths.
  ScopedGrid scoped{coarseGrid(), true};
  const Response response{shapedTile(), scoped.wavelengths(), d65()};
  SUBCASE("Every draw lies inside its band and inside what the grid sees") {
    // The grid cuts R at its red end, 700 nm, and B at its blue end, 400.
    const auto numOutside{[&](size_t x, size_t y, float lo, float hi) {
      int count{0};
      for (const auto draw : drawsAt(response, x, y))
        if (!(draw >= lo && draw <= hi)) count++;
      return count;
    }};
    CHECK(numOutside(0, 0, 550, 700) == 0);
    CHECK(numOutside(1, 0, 557, 567) == 0);
    CHECK(numOutside(0, 1, 557, 567) == 0);
    CHECK(numOutside(1, 1, 400, 500) == 0);
  }
  SUBCASE("The draw rises with the number it is drawn at") {
    for (const auto pixel : {int2(0, 0), int2(1, 0), int2(1, 1)}) {
      const std::vector<float> draws{
          drawsAt(response, size_t(pixel.x), size_t(pixel.y))};
      CHECK(std::is_sorted(draws.begin(), draws.end()));
    }
  }
  SUBCASE("A narrow band's draws average to its photon-weighted mean") {
    const std::vector<double> illuminant{d65()};
    double sum{}, weight{};
    for (int k = 0; k < 10000; k++) {
      const double lambda{557 + 10 * (k + 0.5) / 10000};
      const double curve{1 - std::abs(lambda - 562) / 5};
      const double density{curve * illuminantAt(illuminant, lambda) * lambda};
      sum += lambda * density, weight += density;
    }
    const std::vector<float> draws{drawsAt(response, 1, 0)};
    double mean{};
    for (const auto draw : draws) mean += double(draw);
    CHECK_NEAR(mean / double(draws.size()), sum / weight, 0.01);
  }
  SUBCASE("The draw leans the way the illuminant does") {
    // Tungsten is red-heavy against daylight, so the R pixels trace
    // redder under it.
    const Response underTungsten{shapedTile(), scoped.wavelengths(),
                                 planckSpectrum(ILLUMINANT_A_KELVIN)};
    CHECK(underTungsten.traceWavelengthAt(0, 0, 0.5f) >
          response.traceWavelengthAt(0, 0, 0.5f));
  }
  SUBCASE("Over a band's own curve the span is where the curve is not "
          "zero") {
    const std::optional<TracedSpan> span{
        tracedSpanOf(shapedTile().bands[0], d65())};
    REQUIRE(span);
    CHECK(span->lo == 550.0f);
    CHECK(span->hi == 720.0f);
    CHECK(span->median > 600.0f);
    CHECK(span->median < 700.0f);
  }
  SUBCASE("A band the illuminant leaves dark has nothing to draw, and its "
          "pixels trace the reference") {
    const std::vector<double> dark{SensorSpectrum(SENSOR_WAVELENGTH_COUNT)};
    CHECK(!tracedSpanOf(shapedTile().bands[0], dark));
    const Response unlit{shapedTile(), scoped.wavelengths(), dark};
    CHECK(unlit.traceWavelengthAt(0, 0, 0.5f) == 0.0f);
  }
}
