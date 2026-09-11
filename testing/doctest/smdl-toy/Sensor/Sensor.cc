#include "RenderFixtures.h"

#include <algorithm>
#include <array>
#include <cmath>
#include <string>
#include <vector>

#include "smdl/RenderUtil/Colorimetry.h"
#include "smdl/RenderUtil/SpectralFilm.h"

#include "Sensor/Response.h"
#include "Sensor/Sensor.h"

// The sensor's physics is one line of ISO 12232 read forward and
// backward: the electrons a lux-second of D55 puts in the most sensitive
// band, over which the well and the base ISO are one fact and the gain
// follows the ISO. What matters is that the integrals match hand
// integrals, that the well and the base ISO invert each other, that the
// gain at the base fills the well and scales with the ISO, and that the
// meter reads a flat field of known luminance to the ISO a
// reflected-light meter would set, held within the instrument's range.

namespace {

// A body of 4 um pixels with one flat `qe` band of 0.5 from 400 to 700
// nm, stating nothing about its well.
[[nodiscard]] SensorSettings flatBody() {
  auto value{SensorSettings{}};
  value.pixels = int2(4, 4);
  value.pitchUM = float2(4.0f, 4.0f);
  value.response.kind = ResponseKind::QE;
  auto &band{value.response.bands.emplace_back()};
  band.name = "L";
  band.wavelengths = {400.0f, 700.0f};
  band.values = {0.5f, 0.5f};
  value.detector.bits = 16;
  value.detector.blackLevel = 0.0f;
  return value;
}

// A flat spectrum of unit power per nanometer on the sensor's grid.
[[nodiscard]] SensorSpectrum flatSpectrum() {
  return SensorSpectrum(SENSOR_WAVELENGTH_COUNT, 1.0);
}

// The integral of y-bar at 1 nm, which a unit flat spectrum's
// illuminance is 683 times.
[[nodiscard]] double integralOfY() {
  double total{};
  for (size_t i = 0; i < SENSOR_WAVELENGTH_COUNT; i++)
    total += smdl::wymanY(sensorWavelength(i));
  return total;
}

// A film of `numX` by `numY` pixels holding the flat spectral irradiance
// `E` in every band inside `window`, and nothing outside it.
[[nodiscard]] smdl::SpectralFilm flatFilm(size_t numBands, size_t numX,
                                          size_t numY, double E, int4 window) {
  auto film{smdl::SpectralFilm(numBands, numX, numY)};
  auto sums{std::vector<double>(numBands)};
  for (size_t y = 0; y < numY; y++) {
    for (size_t x = 0; x < numX; x++) {
      const bool isLit{int(x) >= window[0] && int(x) < window[2] &&
                       int(y) >= window[1] && int(y) < window[3]};
      for (size_t b = 0; b < numBands; b++) sums[b] = isLit ? E : 0.0;
      film.addTotals(x, y, sums.data());
    }
  }
  film.addSamples(1);
  return film;
}

// A 10 nm grid from 400 to 700 nm, 31 bands.
[[nodiscard]] std::vector<float> fineGrid() {
  auto grid{std::vector<float>()};
  for (float w = 400; w <= 700; w += 10) grid.push_back(w);
  return grid;
}

// A body whose three bands are the observer over photons, so that its
// responses are the observer's XYZ: `QE_b lambda` is the curve. Knots at
// 1 nm, where the fit integrates.
[[nodiscard]] SensorSettings lutherBody() {
  auto value{flatBody()};
  value.response.bands.clear();
  for (size_t k = 0; k < 3; k++) {
    auto &band{value.response.bands.emplace_back()};
    band.name = std::string(1, "RGB"[k]);
    for (int lambda = 360; lambda <= 830; lambda++) {
      band.wavelengths.push_back(float(lambda));
      band.values.push_back(float(std::max(
          0.0, 0.25 * smdl::wymanXYZ(lambda)[k] * 555.0 / double(lambda))));
    }
  }
  return value;
}

// A body of three overlapping Gaussian bands, red, green, and blue: the
// shape of a camera's curves and no camera's.
[[nodiscard]] SensorSettings gaussianBody() {
  auto value{flatBody()};
  value.response.bands.clear();
  constexpr double CENTERS[3]{600.0, 540.0, 460.0};
  constexpr double WIDTHS[3]{40.0, 40.0, 30.0};
  for (size_t k = 0; k < 3; k++) {
    auto &band{value.response.bands.emplace_back()};
    band.name = std::string(1, "RGB"[k]);
    for (int lambda = 380; lambda <= 780; lambda += 5) {
      const double x{(double(lambda) - CENTERS[k]) / WIDTHS[k]};
      band.wavelengths.push_back(float(lambda));
      band.values.push_back(float(0.5 * std::exp(-0.5 * x * x)));
    }
  }
  return value;
}

} // namespace

TEST_CASE("Sensor: the integrals against hand integrals") {
  const Sensor sensor{flatBody()};
  const auto flat{flatSpectrum()};
  SUBCASE("A flat band under a flat spectrum counts the photon integral, "
          "to the half nanometer the 1 nm rule leaves at each edge") {
    // integral(0.5 lambda / (h c)) from 400 to 700 nm, in nanometers.
    const double expected{0.5 * 1e-9 / (PLANCK * SPEED_OF_LIGHT) * 0.5 *
                          (700.0 * 700.0 - 400.0 * 400.0)};
    CHECK(sensor.electronRate(0, flat) ==
          doctest::Approx(expected).epsilon(0.005));
  }
  SUBCASE("A flat spectrum's illuminance is 683 times the integral of y-bar") {
    CHECK(Sensor::illuminance(flat) ==
          doctest::Approx(LUMENS_PER_WATT * integralOfY()));
    CHECK(Sensor::illuminance(flat) == doctest::Approx(73000.0).epsilon(0.002));
  }
  SUBCASE("The electrons per lux-second are the pixel's, with the scale of "
          "the illuminant cancelled") {
    auto brighter{flat};
    for (auto &value : brighter) value *= 7.0;
    const double expected{16e-12 * sensor.electronRate(0, flat) /
                          Sensor::illuminance(flat)};
    CHECK(sensor.electronsPerLuxSecond(0, flat) == doctest::Approx(expected));
    CHECK(sensor.electronsPerLuxSecond(0, brighter) ==
          doctest::Approx(expected));
  }
  SUBCASE("D55 is 1 at 560 nm, and rates the body a little differently "
          "from a flat spectrum") {
    const auto d55{daylightSpectrum(D55_KELVIN)};
    REQUIRE(d55.size() == SENSOR_WAVELENGTH_COUNT);
    CHECK(d55[260] == doctest::Approx(1.0).epsilon(1e-3));
    CHECK(sensor.peakBand() == 0);
    CHECK(sensor.peakElectronsPerLuxSecond() ==
          doctest::Approx(sensor.electronsPerLuxSecond(0, d55)));
    CHECK(sensor.peakElectronsPerLuxSecond() !=
          doctest::Approx(sensor.electronsPerLuxSecond(0, flat)));
  }
  SUBCASE("The most sensitive band is the one that counts most under D55") {
    auto settings{flatBody()};
    auto &blue{settings.response.bands.emplace_back()};
    blue.name = "B";
    blue.wavelengths = {400.0f, 500.0f};
    blue.values = {0.9f, 0.9f};
    const Sensor two{settings};
    CHECK(two.peakBand() == 0);
    blue.values = {5.0f, 5.0f};
    const Sensor other{settings};
    CHECK(other.peakBand() == 1);
  }
}

TEST_CASE("Sensor: the well and the base ISO are one fact") {
  auto settings{flatBody()};
  SUBCASE("Neither stated: the generic well over the pitch, and the base "
          "ISO it implies") {
    const Sensor sensor{settings};
    CHECK(sensor.fullWell() == doctest::Approx(16000.0));
    CHECK(sensor.wellSource() == WellSource::FROM_PITCH);
    CHECK(!sensor.isBaseISOStated());
    CHECK(sensor.baseISO() ==
          doctest::Approx(ISO_SATURATION_LUX_SECONDS *
                          sensor.peakElectronsPerLuxSecond() / 16000.0));
  }
  SUBCASE("A stated base ISO gives the well, and that well gives the base "
          "ISO back") {
    settings.detector.baseISO = 200.0f;
    const Sensor fromISO{settings};
    CHECK(fromISO.wellSource() == WellSource::FROM_BASE_ISO);
    CHECK(fromISO.isBaseISOStated());
    CHECK(fromISO.baseISO() == 200.0);
    CHECK(fromISO.fullWell() ==
          doctest::Approx(ISO_SATURATION_LUX_SECONDS *
                          fromISO.peakElectronsPerLuxSecond() / 200.0));
    settings.detector.baseISO = {};
    settings.detector.fullWell = float(fromISO.fullWell());
    const Sensor fromWell{settings};
    CHECK(fromWell.wellSource() == WellSource::STATED);
    CHECK(fromWell.baseISO() == doctest::Approx(200.0).epsilon(1e-5));
  }
  SUBCASE("The top ISO is never below the base") {
    settings.detector.maxISO = 50.0f;
    settings.detector.fullWell = 1000.0f;
    const Sensor sensor{settings};
    CHECK(sensor.baseISO() > 50.0);
    CHECK(sensor.maxISO() == sensor.baseISO());
  }
}

TEST_CASE("Sensor: the gain follows the ISO") {
  auto settings{flatBody()};
  settings.detector.baseISO = 100.0f;
  settings.detector.blackLevel = 535.0f;
  const Sensor sensor{settings};
  SUBCASE("At the base ISO the gain fills the well to the top code") {
    CHECK(sensor.gain(100.0) ==
          doctest::Approx((65535.0 - 535.0) / sensor.fullWell()));
    CHECK(sensor.topCodeElectrons(100.0) == doctest::Approx(sensor.fullWell()));
  }
  SUBCASE("Above it the gain scales with the ISO and the top code sits "
          "below the well") {
    CHECK(sensor.gain(800.0) == doctest::Approx(8.0 * sensor.gain(100.0)));
    CHECK(sensor.topCodeElectrons(800.0) ==
          doctest::Approx(sensor.fullWell() / 8.0));
  }
  SUBCASE("A stated gain is fixed, and its saturation speed is the ISO "
          "whose derived gain it is") {
    CHECK(!sensor.hasFixedGain());
    const double gainAt640{sensor.gain(640.0)};
    settings.detector.gain = float(gainAt640);
    const Sensor fixed{settings};
    CHECK(fixed.hasFixedGain());
    CHECK(fixed.gain(100.0) == doctest::Approx(gainAt640));
    CHECK(fixed.gain(3200.0) == doctest::Approx(gainAt640));
    CHECK(fixed.fixedGainISO() == doctest::Approx(640.0).epsilon(1e-5));
  }
}

TEST_CASE("Sensor: the meter") {
  auto settings{flatBody()};
  settings.detector.baseISO = 100.0f;
  settings.detector.maxISO = 6400.0f;
  const Sensor sensor{settings};
  const auto grid{fineGrid()};
  ScopedGrid scoped{grid, false};
  const auto &wavelengths{scoped.wavelengths()};
  const int4 whole{0, 0, 4, 4};
  // A flat spectral irradiance `E` over the grid reads
  // `683 E integral(y-bar)` lux over the grid's span, by the trapezoid.
  const auto luxOf{[&](double E) {
    double total{};
    for (const auto weight : Sensor::luminanceWeights(wavelengths))
      total += weight;
    return LUMENS_PER_WATT * E * total;
  }};
  SUBCASE("The weights integrate y-bar over the grid, still or jittered") {
    double still{};
    for (const auto weight : Sensor::luminanceWeights(wavelengths))
      still += weight;
    double expected{};
    for (double lambda = 400; lambda <= 700; lambda += 1)
      expected +=
          smdl::wymanY(lambda) * (lambda == 400 || lambda == 700 ? 0.5 : 1.0);
    CHECK(still == doctest::Approx(expected).epsilon(0.002));
    ScopedGrid jittered{grid, true};
    double moving{};
    for (const auto weight : Sensor::luminanceWeights(jittered.wavelengths()))
      moving += weight;
    // The jitter's rectangles tile the same span and average y-bar over
    // each, which integrates it more closely than the trapezoid does.
    CHECK(moving == doctest::Approx(expected).epsilon(1e-4));
  }
  SUBCASE("A flat field of known luminance meters to q K over its exposure") {
    // 2.03 lux at 10 ms is 0.0203 lux-seconds, which wants ISO 400.
    const double E{2.03 / luxOf(1.0)};
    const auto film{flatFilm(grid.size(), 4, 4, E, whole)};
    const auto metered{sensor.meter(film, wavelengths, whole, 0.01)};
    CHECK(metered.luxSeconds == doctest::Approx(0.0203).epsilon(1e-4));
    CHECK(metered.wantedISO ==
          doctest::Approx(METER_Q * METER_K / 0.0203).epsilon(1e-4));
    CHECK(metered.iso == doctest::Approx(400.25).epsilon(1e-3));
    CHECK(metered.stopsOff == 0.0);
    CHECK(!metered.isOverexposed());
    CHECK(!metered.isUnderexposed());
  }
  SUBCASE("A bright field wants less than the base, and is overexposed by "
          "the stops between") {
    const double E{2.03 / luxOf(1.0)};
    const auto film{flatFilm(grid.size(), 4, 4, E, whole)};
    // Eight times the exposure wants ISO 50, a stop under the base.
    const auto metered{sensor.meter(film, wavelengths, whole, 0.08)};
    CHECK(metered.wantedISO == doctest::Approx(50.0).epsilon(1e-3));
    CHECK(metered.iso == 100.0);
    CHECK(metered.stopsOff == doctest::Approx(1.0).epsilon(1e-3));
    CHECK(metered.isOverexposed());
  }
  SUBCASE("A dim field wants more than the top, and is underexposed") {
    const double E{2.03 / luxOf(1.0)};
    const auto film{flatFilm(grid.size(), 4, 4, E, whole)};
    // A 64th of the exposure wants ISO 25600, two stops over the top.
    const auto metered{sensor.meter(film, wavelengths, whole, 0.01 / 64.0)};
    CHECK(metered.iso == 6400.0);
    CHECK(metered.stopsOff == doctest::Approx(-2.0).epsilon(1e-3));
    CHECK(metered.isUnderexposed());
  }
  SUBCASE("A dark film wants everything, and gets the top") {
    const auto film{smdl::SpectralFilm(grid.size(), 4, 4)};
    const auto metered{sensor.meter(film, wavelengths, whole, 0.01)};
    CHECK(metered.luxSeconds == 0.0);
    CHECK(std::isinf(metered.wantedISO));
    CHECK(metered.iso == 6400.0);
    CHECK(metered.isUnderexposed());
  }
  SUBCASE("The mean is over the window, and a poisoned pixel reads as "
          "black") {
    const double E{2.03 / luxOf(1.0)};
    const int4 window{0, 0, 2, 2};
    auto film{flatFilm(grid.size(), 4, 4, E, window)};
    const auto over{sensor.meter(film, wavelengths, whole, 0.01)};
    const auto within{sensor.meter(film, wavelengths, window, 0.01)};
    CHECK(over.luxSeconds == doctest::Approx(0.0203 / 4.0).epsilon(1e-4));
    CHECK(within.luxSeconds == doctest::Approx(0.0203).epsilon(1e-4));
    auto poison{std::vector<double>(grid.size(), double(INF))};
    film.addTotals(0, 0, poison.data());
    const auto poisoned{sensor.meter(film, wavelengths, window, 0.01)};
    CHECK(poisoned.luxSeconds == doctest::Approx(0.75 * 0.0203).epsilon(1e-4));
  }
}

TEST_CASE("Sensor: the illuminants a white balance names") {
  SUBCASE("The Planckian radiator at 2856 K is CIE illuminant A") {
    const auto a{planckSpectrum(ILLUMINANT_A_KELVIN)};
    CHECK(a[260] == doctest::Approx(1.0));
    CHECK(a[100] == doctest::Approx(0.147080).epsilon(1e-3));
    CHECK(a[200] == doctest::Approx(0.598611).epsilon(1e-3));
    CHECK(a[400] == doctest::Approx(1.982612).epsilon(1e-3));
  }
  SUBCASE("A temperature is daylight from 4000 K up, and Planck below") {
    CHECK(kelvinSpectrum(5000.0) == daylightSpectrum(5000.0));
    CHECK(kelvinSpectrum(4000.0) == daylightSpectrum(4000.0));
    CHECK(kelvinSpectrum(3200.0) == planckSpectrum(3200.0));
  }
  SUBCASE("Each preset names its illuminant") {
    const auto of{[](WhiteBalanceKind kind) {
      return whiteBalanceSpectrum(WhiteBalance{kind, 0.0f});
    }};
    CHECK(of(WhiteBalanceKind::D65) == daylightSpectrum(D65_KELVIN));
    CHECK(of(WhiteBalanceKind::CLOUDY) == daylightSpectrum(D65_KELVIN));
    CHECK(of(WhiteBalanceKind::AUTO) == daylightSpectrum(D65_KELVIN));
    CHECK(of(WhiteBalanceKind::DAYLIGHT) == daylightSpectrum(D55_KELVIN));
    CHECK(of(WhiteBalanceKind::SHADE) == daylightSpectrum(D75_KELVIN));
    CHECK(of(WhiteBalanceKind::TUNGSTEN) ==
          planckSpectrum(ILLUMINANT_A_KELVIN));
    CHECK(whiteBalanceSpectrum(WhiteBalance{
              WhiteBalanceKind::KELVIN, 3200.0f}) == planckSpectrum(3200.0));
    // F2 is tabulated from 380 to 780 nm, and its mercury line at 546 nm
    // stands out of its neighbors.
    const auto f2{of(WhiteBalanceKind::FLUORESCENT)};
    CHECK(f2[79] == 0.0);
    CHECK(f2[80] > 0.0);
    CHECK(f2[481] == 0.0);
    CHECK(f2[245] > 2.0 * f2[235]);
  }
  SUBCASE("A white is the illuminant through the observer, Y = 1") {
    const auto white{illuminantWhite(daylightSpectrum(D65_KELVIN))};
    CHECK(white.y == 1.0);
    const double sum{white.x + white.y + white.z};
    CHECK(white.x / sum == doctest::Approx(0.3127).epsilon(0.002));
    CHECK(white.y / sum == doctest::Approx(0.3290).epsilon(0.002));
  }
}

TEST_CASE("Sensor: the training reflectances") {
  const auto &patches{trainingReflectances()};
  REQUIRE(patches.size() == 190);
  REQUIRE(patches[0].size() == SENSOR_WAVELENGTH_COUNT);
  SUBCASE("They are the table's at its own wavelengths, and linear between") {
    CHECK(patches[0][80] == doctest::Approx(0.06));
    CHECK(patches[0][85] == doctest::Approx(0.0501124));
    CHECK(patches[0][82] ==
          doctest::Approx(0.06 + 0.4 * (0.0501124 - 0.06)).epsilon(1e-6));
  }
  SUBCASE("Past either end they hold the end values") {
    CHECK(patches[0][0] == doctest::Approx(0.06));
    CHECK(patches[0][480] == doctest::Approx(0.8866));
    CHECK(patches[0][530] == doctest::Approx(0.8866));
  }
}

TEST_CASE("Sensor: the color fit") {
  const auto d65{daylightSpectrum(D65_KELVIN)};
  const std::array<size_t, 3> rgb{0, 1, 2};
  SUBCASE("A body whose curves are the observer's fits it to nothing, an "
          "index of 100") {
    const auto fit{Sensor{lutherBody()}.fitColor(rgb, d65)};
    CHECK(!fit.isSingular);
    CHECK(fit.isFaithful());
    CHECK(fit.meanDeltaE00 < 0.01);
    CHECK(fit.maxDeltaE00 < 0.05);
    CHECK(fit.index() > 99.9);
  }
  SUBCASE("The white lands exactly, and the multipliers balance it against "
          "green") {
    const Sensor sensor{gaussianBody()};
    const auto fit{sensor.fitColor(rgb, d65)};
    const auto white{fit.cameraToXYZ * smdl::double3(1.0)};
    CHECK(white.x == doctest::Approx(fit.white.x).epsilon(1e-12));
    CHECK(white.y == doctest::Approx(fit.white.y).epsilon(1e-12));
    CHECK(white.z == doctest::Approx(fit.white.z).epsilon(1e-12));
    CHECK(fit.multipliers.y == 1.0);
    CHECK(fit.multipliers.x == doctest::Approx(sensor.electronRate(1, d65) /
                                               sensor.electronRate(0, d65))
                                   .epsilon(1e-12));
    CHECK(fit.multipliers.z == doctest::Approx(sensor.electronRate(1, d65) /
                                               sensor.electronRate(2, d65))
                                   .epsilon(1e-12));
  }
  SUBCASE("A camera's shape of curves fits the way a camera's do") {
    const auto fit{Sensor{gaussianBody()}.fitColor(rgb, d65)};
    CHECK(fit.isFaithful());
    CHECK(fit.meanDeltaE00 > 0.5);
    CHECK(fit.meanDeltaE00 < 5.0);
    CHECK(fit.index() > 60.0);
    CHECK(fit.index() < 99.0);
  }
  SUBCASE("Bands too much alike have no fit, and are not faithful") {
    auto settings{lutherBody()};
    settings.response.bands[2] = settings.response.bands[1];
    const auto fit{Sensor{settings}.fitColor(rgb, d65)};
    CHECK(fit.isSingular);
    CHECK(!fit.isFaithful());
  }
  SUBCASE("A band the illuminant does not reach has no fit") {
    auto settings{lutherBody()};
    auto &band{settings.response.bands[2]};
    band.wavelengths = {840.0f, 900.0f};
    band.values = {0.5f, 0.5f};
    const auto fit{Sensor{settings}.fitColor(rgb, d65)};
    CHECK(fit.isSingular);
    CHECK(fit.multipliers.x == 1.0);
  }
}
