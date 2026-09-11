#include "RenderFixtures.h"

#include <algorithm>
#include <array>
#include <cmath>
#include <string>
#include <string_view>
#include <vector>

#include "smdl/RenderUtil/SpectralFilm.h"

#include "Sensor/Colorimetry.h"
#include "Sensor/Detector.h"
#include "Sensor/Develop.h"
#include "Sensor/Sensor.h"

// The physical develop is a raw developer's chain from digital numbers
// to linear sRGB. What matters is that each demosaic reproduces a plane
// on its lattice, that a neutral and a clipped white develop neutral,
// that a body whose curves are the observer's develops the training
// reflectances to the observer's own colors, that a neutral the meter
// aimed at lands at middle gray whichever band the ISO was rated in,
// that a body that cannot carry color still develops, and that the
// picture is a function of the readout alone. The observer's develop
// needs the JIT and is not here.

namespace {

// Three bands R, G, and B under a 2 by 2 tile that `tile` spells row by
// row, one letter per pixel. The curves are not the demosaic's concern.
[[nodiscard]] ResponseSettings bayerTile(std::string_view tile) {
  auto response{ResponseSettings{}};
  for (const char *name : {"R", "G", "B"})
    response.bands.emplace_back().name = name;
  response.cfaColumns = 2;
  for (const char letter : tile)
    response.cfa.push_back(letter == 'R' ? 0 : letter == 'G' ? 1 : 2);
  return response;
}

// Sixteen bands under a 4 by 4 tile, one pixel per band.
[[nodiscard]] ResponseSettings quadTile() {
  auto response{ResponseSettings{}};
  for (size_t b = 0; b < 16; b++) {
    response.bands.emplace_back().name = "B" + std::to_string(b);
    response.cfa.push_back(b);
  }
  response.cfaColumns = 4;
  return response;
}

// A plane per band, a different one for each: the demosaic's truth.
[[nodiscard]] double planeAt(size_t band, double x, double y) {
  const double b{double(band)};
  return 0.1 + 0.05 * b + 0.01 * (b + 1.0) * x - 0.007 * (3.0 - b) * y;
}

[[nodiscard]] size_t bandAt(const ResponseSettings &response, size_t x,
                            size_t y) {
  return response.cfa[(y % response.cfaRows()) * response.cfaColumns +
                      x % response.cfaColumns];
}

// The mosaic `response`'s tile makes of the planes.
[[nodiscard]] std::vector<float> mosaicOf(const ResponseSettings &response,
                                          size_t numX, size_t numY) {
  auto mosaic{std::vector<float>(numX * numY)};
  for (size_t y = 0; y < numY; y++)
    for (size_t x = 0; x < numX; x++)
      mosaic[y * numX + x] =
          float(planeAt(bandAt(response, x, y), double(x), double(y)));
  return mosaic;
}

// Does every pixel `inset` or more in from the frame's edge hold the
// planes of `bands`?
void checkPlanes(const std::vector<float> &planes,
                 smdl::Span<const size_t> bands, size_t numX, size_t numY,
                 size_t inset) {
  for (size_t y = inset; y < numY - inset; y++)
    for (size_t x = inset; x < numX - inset; x++)
      for (size_t k = 0; k < bands.size(); k++)
        CHECK(std::abs(double(planes[(y * numX + x) * bands.size() + k]) -
                       planeAt(bands[k], double(x), double(y))) < 1e-5);
}

// A body whose three bands are the observer over photons, so that its
// responses are the observer's XYZ, with blue's scaled by `blueScale`,
// which a fit absorbs; under the RGGB tile, or none. Knots at 1 nm,
// where the fit integrates.
[[nodiscard]] SensorSettings lutherBody(bool isTiled, double blueScale = 1.0) {
  auto value{SensorSettings{}};
  value.pixels = int2(8, 6);
  value.pitchUM = float2(4.0f, 4.0f);
  value.response.kind = ResponseKind::QE;
  for (size_t k = 0; k < 3; k++) {
    auto &band{value.response.bands.emplace_back()};
    band.name = std::string(1, "RGB"[k]);
    const double scale{k == 2 ? 0.25 * blueScale : 0.25};
    for (int lambda = 360; lambda <= 830; lambda++) {
      band.wavelengths.push_back(float(lambda));
      band.values.push_back(float(
          std::max(0.0, scale * wymanXYZ(lambda)[k] * 555.0 / double(lambda))));
    }
  }
  if (isTiled) {
    value.response.cfaColumns = 2;
    value.response.cfa = {0, 1, 1, 2};
  }
  value.detector.bits = 16;
  value.detector.blackLevel = 1024.0f;
  value.detector.baseISO = 100.0f;
  return value;
}

// A body of `numBands` identical flat bands, untiled.
[[nodiscard]] SensorSettings flatBands(size_t numBands) {
  auto value{SensorSettings{}};
  value.pixels = int2(4, 4);
  value.pitchUM = float2(4.0f, 4.0f);
  value.response.kind = ResponseKind::QE;
  for (size_t b = 0; b < numBands; b++) {
    auto &band{value.response.bands.emplace_back()};
    band.name = std::string(1, "RGB"[b]);
    band.wavelengths = {400.0f, 700.0f};
    band.values = {0.5f, 0.5f};
  }
  value.detector.bits = 16;
  value.detector.blackLevel = 1024.0f;
  value.detector.baseISO = 100.0f;
  return value;
}

// The detector at the body's base ISO, whose levels the develop reads.
[[nodiscard]] Detector detectorFor(const Sensor &sensor) {
  auto shot{DetectorShot{}};
  shot.exposure = 0.01;
  shot.fNumber = 8;
  shot.iso = sensor.baseISO();
  return Detector{sensor, shot};
}

// The readout of `fractions`, each sample a fraction of the top code over
// the black level.
[[nodiscard]] Readout readoutOf(const Detector &detector, size_t bandCount,
                                size_t numX, size_t numY,
                                const std::vector<double> &fractions) {
  auto readout{Readout{}};
  readout.bandCount = bandCount;
  readout.pixelCountX = numX;
  readout.pixelCountY = numY;
  const double black{detector.blackLevel()};
  const double range{double(detector.topCode()) - black};
  for (const auto fraction : fractions)
    readout.digitalNumbers.push_back(
        uint16_t(std::clamp(std::round(black + fraction * range), 0.0,
                            double(detector.topCode()))));
  return readout;
}

// Each band's electron rate under `illuminant` reflected by
// `reflectance`, which is proportional to what a readout counts.
[[nodiscard]] std::array<double, 3>
responseOf(const Sensor &sensor, const SensorSpectrum &illuminant,
           const SensorSpectrum &reflectance) {
  auto lit{illuminant};
  for (size_t i = 0; i < lit.size(); i++) lit[i] *= reflectance[i];
  return {sensor.electronRate(0, lit), sensor.electronRate(1, lit),
          sensor.electronRate(2, lit)};
}

// The observer's XYZ of `reflectance` under `illuminant`, the
// illuminant's white at Y = 1.
[[nodiscard]] smdl::double3 observerXYZ(const SensorSpectrum &illuminant,
                                        const SensorSpectrum &reflectance) {
  auto total{smdl::double3()};
  double whiteY{};
  for (size_t i = 0; i < SENSOR_WAVELENGTH_COUNT; i++) {
    const auto xyz{wymanXYZ(sensorWavelength(i))};
    total += illuminant[i] * reflectance[i] * xyz;
    whiteY += illuminant[i] * xyz.y;
  }
  return total / whiteY;
}

[[nodiscard]] smdl::double3 pixelOf(const std::vector<float> &rgbImage,
                                    size_t pixel) {
  return {rgbImage[3 * pixel], rgbImage[3 * pixel + 1],
          rgbImage[3 * pixel + 2]};
}

// Is every pixel of the window neutral to one part in a thousand?
[[nodiscard]] bool isNeutral(const std::vector<float> &rgbImage) {
  for (size_t pixel = 0; pixel < rgbImage.size() / 3; pixel++) {
    const auto rgb{pixelOf(rgbImage, pixel)};
    if (!(rgb.y > 0 && std::abs(rgb.x - rgb.y) < 1e-3 * rgb.y &&
          std::abs(rgb.z - rgb.y) < 1e-3 * rgb.y))
      return false;
  }
  return true;
}

} // namespace

TEST_CASE("Develop: the demosaic methods") {
  const std::array<size_t, 3> rgb{0, 1, 2};
  const auto bands{smdl::Span<const size_t>(rgb.data(), rgb.size())};
  SUBCASE("A 2 by 2 tile of the three bands, green on a diagonal, takes "
          "Hamilton and Adams") {
    CHECK(demosaicMethod(bayerTile("RGGB"), bands) ==
          DemosaicMethod::HAMILTON_ADAMS);
    CHECK(demosaicMethod(bayerTile("GRBG"), bands) ==
          DemosaicMethod::HAMILTON_ADAMS);
    CHECK(demosaicMethod(bayerTile("BGGR"), bands) ==
          DemosaicMethod::HAMILTON_ADAMS);
  }
  SUBCASE("Green in a column, or green not the middle band, takes the "
          "bilinear") {
    CHECK(demosaicMethod(bayerTile("RGBG"), bands) == DemosaicMethod::BILINEAR);
    const std::array<size_t, 3> swapped{0, 2, 1};
    CHECK(demosaicMethod(bayerTile("RGGB"),
                         smdl::Span<const size_t>(swapped.data(), 3)) ==
          DemosaicMethod::BILINEAR);
  }
  SUBCASE("Any other tile takes the bilinear, and no tile none") {
    CHECK(demosaicMethod(quadTile(), bands) == DemosaicMethod::BILINEAR);
    CHECK(demosaicMethod(ResponseSettings{}, bands) == DemosaicMethod::NONE);
  }
}

TEST_CASE("Develop: each demosaic reproduces a plane on its lattice") {
  constexpr size_t NUM_X{12};
  constexpr size_t NUM_Y{10};
  const int4 whole{0, 0, int(NUM_X), int(NUM_Y)};
  const std::array<size_t, 3> rgb{0, 1, 2};
  const auto bands{smdl::Span<const size_t>(rgb.data(), rgb.size())};
  const auto run{[&](DemosaicMethod method, const ResponseSettings &response,
                     smdl::Span<const size_t> which, int4 window) {
    const auto mosaic{mosaicOf(response, NUM_X, NUM_Y)};
    return demosaic(method, response, which,
                    smdl::Span<const float>(mosaic.data(), mosaic.size()),
                    NUM_X, NUM_Y, window);
  }};
  SUBCASE("Bilinear over a Bayer tile, one pixel in from the edge") {
    for (const char *tile : {"RGGB", "GRBG"})
      checkPlanes(run(DemosaicMethod::BILINEAR, bayerTile(tile), bands, whole),
                  bands, NUM_X, NUM_Y, 1);
  }
  SUBCASE("Hamilton and Adams over a Bayer tile, one pixel in from the "
          "edge") {
    for (const char *tile : {"RGGB", "GRBG", "BGGR"})
      checkPlanes(
          run(DemosaicMethod::HAMILTON_ADAMS, bayerTile(tile), bands, whole),
          bands, NUM_X, NUM_Y, 1);
  }
  SUBCASE("Bilinear over a 4 by 4 tile of one pixel per band, three in") {
    const std::array<size_t, 3> some{0, 5, 10};
    const auto which{smdl::Span<const size_t>(some.data(), some.size())};
    checkPlanes(run(DemosaicMethod::BILINEAR, quadTile(), which, whole), which,
                NUM_X, NUM_Y, 3);
  }
  SUBCASE("A pixel keeps the value its band sampled there, to the frame's "
          "edge") {
    const auto response{bayerTile("RGGB")};
    const auto planes{
        run(DemosaicMethod::HAMILTON_ADAMS, response, bands, whole)};
    for (size_t y = 0; y < NUM_Y; y++)
      for (size_t x = 0; x < NUM_X; x++) {
        const size_t band{bandAt(response, x, y)};
        CHECK(planes[(y * NUM_X + x) * 3 + band] ==
              float(planeAt(band, double(x), double(y))));
      }
  }
  SUBCASE("Nothing is written outside the window, and nothing read from "
          "it") {
    const auto response{bayerTile("RGGB")};
    const int4 window{2, 2, 10, 8};
    auto mosaic{mosaicOf(response, NUM_X, NUM_Y)};
    const auto clean{
        demosaic(DemosaicMethod::HAMILTON_ADAMS, response, bands,
                 smdl::Span<const float>(mosaic.data(), mosaic.size()), NUM_X,
                 NUM_Y, window)};
    for (size_t y = 0; y < NUM_Y; y++)
      for (size_t x = 0; x < NUM_X; x++)
        if (!(int(x) >= window[0] && int(x) < window[2] &&
              int(y) >= window[1] && int(y) < window[3]))
          mosaic[y * NUM_X + x] = 100.0f;
    const auto dirty{
        demosaic(DemosaicMethod::HAMILTON_ADAMS, response, bands,
                 smdl::Span<const float>(mosaic.data(), mosaic.size()), NUM_X,
                 NUM_Y, window)};
    CHECK(clean == dirty);
    CHECK(clean[0] == 0.0f);
    CHECK(clean[(2 * NUM_X + 2) * 3] != 0.0f);
  }
}

TEST_CASE("Develop: a neutral develops neutral") {
  const Sensor sensor{lutherBody(true)};
  const auto detector{detectorFor(sensor)};
  const auto &response{sensor.settings().response};
  const auto white{SensorSpectrum(SENSOR_WAVELENGTH_COUNT, 1.0)};
  const auto d65{daylightSpectrum(D65_KELVIN)};
  const auto tungsten{planckSpectrum(ILLUMINANT_A_KELVIN)};
  constexpr size_t NUM_X{8};
  constexpr size_t NUM_Y{6};
  const int4 whole{0, 0, int(NUM_X), int(NUM_Y)};
  // A gray under `illuminant` as the tile samples it, green at a fifth of
  // the top code.
  const auto grayField{[&](const SensorSpectrum &illuminant) {
    const auto rates{responseOf(sensor, illuminant, white)};
    auto fractions{std::vector<double>(NUM_X * NUM_Y)};
    for (size_t y = 0; y < NUM_Y; y++)
      for (size_t x = 0; x < NUM_X; x++)
        fractions[y * NUM_X + x] =
            0.2 * rates[bandAt(response, x, y)] / rates[1];
    return readoutOf(detector, 1, NUM_X, NUM_Y, fractions);
  }};
  const auto develop{[&](const Readout &readout, WhiteBalanceKind kind) {
    return developReadout(sensor, detector, readout, WhiteBalance{kind, 0.0f},
                          whole, false);
  }};
  SUBCASE("Under D65, balanced to D65") {
    CHECK(isNeutral(develop(grayField(d65), WhiteBalanceKind::D65)));
  }
  SUBCASE("Under tungsten, balanced to tungsten") {
    CHECK(isNeutral(develop(grayField(tungsten), WhiteBalanceKind::TUNGSTEN)));
  }
  SUBCASE("Under tungsten, balanced by the frame's gray world") {
    CHECK(isNeutral(develop(grayField(tungsten), WhiteBalanceKind::AUTO)));
  }
  SUBCASE("And not under tungsten balanced to D65, which comes out warm") {
    const auto rgbImage{develop(grayField(tungsten), WhiteBalanceKind::D65)};
    CHECK(!isNeutral(rgbImage));
    const auto rgb{pixelOf(rgbImage, 3 * NUM_X + 4)};
    CHECK(rgb.x > 1.5 * rgb.z);
  }
}

TEST_CASE("Develop: a clipped white stays white") {
  for (const bool isTiled : {true, false}) {
    const Sensor sensor{lutherBody(isTiled)};
    const auto detector{detectorFor(sensor)};
    const size_t bandCount{isTiled ? size_t(1) : size_t(3)};
    const double fraction{
        (double(detector.whiteLevel()) - detector.blackLevel()) /
        (double(detector.topCode()) - detector.blackLevel())};
    const auto readout{
        readoutOf(detector, bandCount, 4, 4,
                  std::vector<double>(16 * bandCount, fraction))};
    CHECK(isNeutral(developReadout(sensor, detector, readout, WhiteBalance{},
                                   int4{0, 0, 4, 4}, false)));
  }
}

TEST_CASE("Develop: a body whose curves are the observer's develops the "
          "training set to its colors") {
  const Sensor sensor{lutherBody(false)};
  const auto detector{detectorFor(sensor)};
  const auto d65{daylightSpectrum(D65_KELVIN)};
  const auto white{SensorSpectrum(SENSOR_WAVELENGTH_COUNT, 1.0)};
  const auto &patches{trainingReflectances()};
  const size_t numPixels{patches.size() + 1};
  // Each patch a pixel, and the perfect white after them, its green at
  // half the top code.
  const double green{responseOf(sensor, d65, white)[1]};
  auto fractions{std::vector<double>()};
  for (size_t j = 0; j < numPixels; j++) {
    const auto rates{
        responseOf(sensor, d65, j < patches.size() ? patches[j] : white)};
    for (const auto rate : rates) fractions.push_back(0.5 * rate / green);
  }
  const auto rgbImage{developReadout(
      sensor, detector, readoutOf(detector, 3, numPixels, 1, fractions),
      WhiteBalance{}, int4{0, 0, int(numPixels), 1}, false)};
  // Back to XYZ through the builtin's matrix; each patch against the
  // observer's own XYZ of it, adapted as the develop adapts, both in
  // CIELAB about their white, so that the exposure cancels.
  auto toXYZ{xyzToLinearSRGB()};
  REQUIRE(tryInvert(toXYZ));
  const auto developedWhite{toXYZ * pixelOf(rgbImage, patches.size())};
  const auto adapt{bradfordAdaptation(illuminantWhite(d65), linearSRGBWhite())};
  double worst{};
  for (size_t j = 0; j < patches.size(); j++) {
    const auto truth{
        xyzToLab(adapt * observerXYZ(d65, patches[j]), linearSRGBWhite())};
    const auto developed{
        xyzToLab(toXYZ * pixelOf(rgbImage, j), developedWhite)};
    worst = std::max(worst, deltaE00(truth, developed));
  }
  CHECK(worst < 0.5);
}

TEST_CASE("Develop: a metered neutral develops to middle gray") {
  // A D65 field on a 5 nm grid, metered at 10 ms, and read out
  // noise-free at the ISO the meter asks for. The body's blue is its most
  // sensitive band, so the ISO is rated in blue and the develop has to
  // carry that over to green.
  auto grid{std::vector<float>()};
  for (float w = 380; w <= 780; w += 5) grid.push_back(w);
  ScopedGrid scoped{grid, false};
  const Sensor sensor{lutherBody(true, 1.5)};
  REQUIRE(sensor.peakBand() == 2);
  const auto &response{sensor.settings().response};
  constexpr size_t NUM_X{8};
  constexpr size_t NUM_Y{6};
  const int4 whole{0, 0, int(NUM_X), int(NUM_Y)};
  const auto d65{daylightSpectrum(D65_KELVIN)};
  const double seconds{0.01};
  // 2.03 lux, which wants ISO 400 at 10 ms.
  const double scale{0.0203 / (seconds * Sensor::illuminance(d65))};
  auto film{smdl::SpectralFilm(grid.size(), NUM_X, NUM_Y)};
  auto bandFilm{smdl::SpectralFilm(1, NUM_X, NUM_Y)};
  auto sums{std::vector<double>(grid.size())};
  for (size_t i = 0; i < grid.size(); i++)
    sums[i] = scale * d65[size_t(grid[i]) - 300];
  for (size_t y = 0; y < NUM_Y; y++) {
    for (size_t x = 0; x < NUM_X; x++) {
      film.addTotals(x, y, sums.data());
      const double rate{scale *
                        sensor.electronRate(bandAt(response, x, y), d65)};
      bandFilm.addTotals(x, y, &rate);
    }
  }
  film.addSamples(1);
  bandFilm.addSamples(1);
  const auto metered{sensor.meter(film, scoped.wavelengths(), whole, seconds)};
  CHECK(metered.iso == doctest::Approx(400.0).epsilon(0.01));
  auto shot{DetectorShot{}};
  shot.exposure = seconds;
  shot.fNumber = 8;
  shot.iso = metered.iso;
  const Detector detector{sensor, shot};
  const auto readout{detector.readOut(
      bandFilm, DetectorReadoutOptions{0, DetectorNoise::NONE}, whole)};
  const auto rgbImage{
      developReadout(sensor, detector, readout, WhiteBalance{}, whole, false)};
  for (const auto value : rgbImage)
    CHECK(double(value) == doctest::Approx(DEVELOP_MIDDLE_GRAY).epsilon(0.01));
}

TEST_CASE("Develop: a body that cannot carry color still develops") {
  SUBCASE("Three bands too much alike develop as false color, each band on "
          "its own channel") {
    const Sensor sensor{flatBands(3)};
    const auto detector{detectorFor(sensor)};
    const auto rgbImage{developReadout(
        sensor, detector, readoutOf(detector, 3, 1, 1, {0.1, 0.2, 0.3}),
        WhiteBalance{}, int4{0, 0, 1, 1}, false)};
    const auto rgb{pixelOf(rgbImage, 0)};
    CHECK(rgb.y == doctest::Approx(2.0 * rgb.x).epsilon(1e-3));
    CHECK(rgb.z == doctest::Approx(3.0 * rgb.x).epsilon(1e-3));
  }
  SUBCASE("One band develops as gray") {
    const Sensor sensor{flatBands(1)};
    const auto detector{detectorFor(sensor)};
    const auto rgbImage{developReadout(
        sensor, detector, readoutOf(detector, 1, 2, 1, {0.1, 0.3}),
        WhiteBalance{}, int4{0, 0, 2, 1}, false)};
    CHECK(isNeutral(rgbImage));
    CHECK(rgbImage[3] == doctest::Approx(3.0 * rgbImage[0]).epsilon(1e-3));
  }
}

TEST_CASE("Develop: the picture is a function of the readout alone") {
  const Sensor sensor{lutherBody(true)};
  const auto detector{detectorFor(sensor)};
  auto fractions{std::vector<double>(48)};
  for (size_t i = 0; i < fractions.size(); i++)
    fractions[i] = 0.05 + 0.01 * double(i % 7);
  const auto readout{readoutOf(detector, 1, 8, 6, fractions)};
  SUBCASE("The same readout develops the same, gray world and all") {
    const WhiteBalance automatic{WhiteBalanceKind::AUTO, 0.0f};
    CHECK(developReadout(sensor, detector, readout, automatic, int4{0, 0, 8, 6},
                         false) == developReadout(sensor, detector, readout,
                                                  automatic, int4{0, 0, 8, 6},
                                                  false));
  }
  SUBCASE("Outside the window is black") {
    const int4 window{2, 1, 6, 5};
    const auto rgbImage{developReadout(sensor, detector, readout,
                                       WhiteBalance{}, window, false)};
    for (size_t y = 0; y < 6; y++)
      for (size_t x = 0; x < 8; x++) {
        const bool isInside{int(x) >= window[0] && int(x) < window[2] &&
                            int(y) >= window[1] && int(y) < window[3]};
        CHECK((pixelOf(rgbImage, y * 8 + x).y != 0.0) == isInside);
      }
  }
}
