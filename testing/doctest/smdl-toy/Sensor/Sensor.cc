#include "RenderFixtures.h"

#include <algorithm>
#include <array>
#include <cmath>
#include <string>
#include <vector>

#include "smdl/RenderUtil/Colorimetry.h"

#include "Sensor/Response.h"
#include "Sensor/Sensor.h"

// The sensor's physics is one line of ISO 12232 read forward and
// backward: the electrons a lux-second of D55 puts in the most sensitive
// band, over which the well and the base ISO are one fact and the gain
// follows the ISO. What matters is that the integrals match hand
// integrals, that the well and the base ISO invert each other, that the
// gain at the base fills the well and scales with the ISO, and that the
// meter reads a field of known exposure to the ISO a reflected-light
// meter would set, on the nearest rung of the series, held within the
// instrument's range.

namespace {

// A sensor of 4 um pixels with one flat `qe` band of 0.5 from 400 to 700
// nm, stating nothing about its well.
[[nodiscard]] SensorSettings flatSensor() {
  SensorSettings value{};
  value.pixels = int2(4, 4);
  value.pitchUM = float2(4.0f, 4.0f);
  value.response.kind = ResponseKind::QE;
  ResponseBand &band{value.response.bands.emplace_back()};
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

// A 10 nm grid from 400 to 700 nm, 31 bands.
[[nodiscard]] std::vector<float> fineGrid() {
  std::vector<float> grid{};
  for (float w = 400; w <= 700; w += 10) grid.push_back(w);
  return grid;
}

// A sensor whose three bands are the observer over photons, so that its
// responses are the observer's XYZ: `QE_b lambda` is the curve. Knots at
// 1 nm, where the fit integrates.
[[nodiscard]] SensorSettings lutherSensor() {
  SensorSettings value{flatSensor()};
  value.response.bands.clear();
  for (size_t k = 0; k < 3; k++) {
    ResponseBand &band{value.response.bands.emplace_back()};
    band.name = std::string(1, "RGB"[k]);
    for (int lambda = 360; lambda <= 830; lambda++) {
      band.wavelengths.push_back(float(lambda));
      band.values.push_back(float(std::max(
          0.0, 0.25 * smdl::wymanXYZ(lambda)[k] * 555.0 / double(lambda))));
    }
  }
  return value;
}

// A sensor of three overlapping Gaussian bands, red, green, and blue: the
// shape of a camera's curves and no camera's.
[[nodiscard]] SensorSettings gaussianSensor() {
  SensorSettings value{flatSensor()};
  value.response.bands.clear();
  constexpr double CENTERS[3]{600.0, 540.0, 460.0};
  constexpr double WIDTHS[3]{40.0, 40.0, 30.0};
  for (size_t k = 0; k < 3; k++) {
    ResponseBand &band{value.response.bands.emplace_back()};
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
  const Sensor sensor{flatSensor()};
  const std::vector<double> flat{flatSpectrum()};
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
    std::vector<double> brighter{flat};
    for (auto &value : brighter) value *= 7.0;
    const double expected{16e-12 * sensor.electronRate(0, flat) /
                          Sensor::illuminance(flat)};
    CHECK(sensor.electronsPerLuxSecond(0, flat) == doctest::Approx(expected));
    CHECK(sensor.electronsPerLuxSecond(0, brighter) ==
          doctest::Approx(expected));
  }
  SUBCASE("D55 is 1 at 560 nm, and rates the sensor a little differently "
          "from a flat spectrum") {
    const std::vector<double> d55{daylightSpectrum(D55_KELVIN)};
    REQUIRE(d55.size() == SENSOR_WAVELENGTH_COUNT);
    CHECK(d55[260] == doctest::Approx(1.0).epsilon(1e-3));
    CHECK(sensor.peakBand() == 0);
    CHECK(sensor.peakElectronsPerLuxSecond() ==
          doctest::Approx(sensor.electronsPerLuxSecond(0, d55)));
    CHECK(sensor.peakElectronsPerLuxSecond() !=
          doctest::Approx(sensor.electronsPerLuxSecond(0, flat)));
  }
  SUBCASE("The most sensitive band is the one that counts most under D55") {
    SensorSettings settings{flatSensor()};
    ResponseBand &blue{settings.response.bands.emplace_back()};
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
  SensorSettings settings{flatSensor()};
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
  SensorSettings settings{flatSensor()};
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
  SensorSettings settings{flatSensor()};
  settings.detector.baseISO = 100.0f;
  settings.detector.maxISO = 6400.0f;
  const Sensor sensor{settings};
  const std::vector<float> grid{fineGrid()};
  ScopedGrid scoped{grid, false};
  SUBCASE("The luminance weights integrate y-bar over the grid, still or "
          "jittered") {
    double still{};
    for (const auto weight :
         Sensor::luminanceWeights(gRenderGrid.first(), false))
      still += weight;
    double expected{};
    for (double lambda = 400; lambda <= 700; lambda += 1)
      expected +=
          smdl::wymanY(lambda) * (lambda == 400 || lambda == 700 ? 0.5 : 1.0);
    CHECK(still == doctest::Approx(expected).epsilon(0.002));
    ScopedGrid jittered{grid, true};
    double moving{};
    for (const auto weight :
         Sensor::luminanceWeights(gRenderGrid.first(), true))
      moving += weight;
    // The jitter's rectangles tile the same span and average y-bar over
    // each, which integrates it more closely than the trapezoid does.
    CHECK(moving == doctest::Approx(expected).epsilon(1e-4));
  }
  SUBCASE("A reading through the peak band is the D55 exposure that counts "
          "the same electrons") {
    // A D55 field of 2.03 lux: the band counts its electrons per
    // lux-second of D55 times 2.03 per second, and 10 ms of that reads
    // back as 0.0203 lux-seconds exactly, whatever the pixel.
    const SensorSpectrum d55{daylightSpectrum(D55_KELVIN)};
    const double rate{sensor.electronRate(0, d55) * 2.03 /
                      Sensor::illuminance(d55)};
    CHECK(sensor.luxSecondsOf(rate, 0.01) ==
          doctest::Approx(0.0203).epsilon(1e-9));
    // Under a flat field the band reads what a flat field puts in it
    // against what D55 does, which is not the illuminance: the meter is
    // through the sensor's band, not the observer.
    const double flatRate{sensor.electronRate(0, flatSpectrum()) * 2.03 /
                          Sensor::illuminance(flatSpectrum())};
    CHECK(sensor.luxSecondsOf(flatRate, 0.01) != doctest::Approx(0.0203));
    CHECK(sensor.luxSecondsOf(0.0, 0.01) == 0.0);
  }
  SUBCASE("A field of known exposure meters to q K over it, on the nearest "
          "rung") {
    // 0.0203 lux-seconds wants ISO 400.25, whose rung is 400.
    const MeteredExposure metered{sensor.meter(0.0203)};
    CHECK(metered.luxSeconds == 0.0203);
    CHECK(metered.wantedISO ==
          doctest::Approx(METER_Q * METER_K / 0.0203).epsilon(1e-9));
    CHECK(metered.iso == 400.0);
    CHECK(metered.stopsOff == 0.0);
    CHECK(!metered.isOverexposed());
    CHECK(!metered.isUnderexposed());
  }
  SUBCASE("The rung is the nearest in stops") {
    // 450 sits 0.17 stops over 400 and 0.15 under 500; 440 sits 0.14
    // over 400 and 0.18 under 500.
    CHECK(sensor.nearestISO(450) == 500.0);
    CHECK(sensor.nearestISO(440) == 400.0);
    CHECK(sensor.nearestISO(125) == 125.0);
    CHECK(sensor.nearestISO(6399) == 6400.0);
    CHECK(sensor.meter(METER_Q * METER_K / 450).iso == 500.0);
  }
  SUBCASE("A base the series does not name is its own floor rung, and the "
          "top its own ceiling") {
    SensorSettings odd{settings};
    odd.detector.baseISO = 83.7f;
    odd.detector.maxISO = 7000.0f;
    const Sensor sensor2{odd};
    CHECK(sensor2.nearestISO(10) == doctest::Approx(83.7));
    CHECK(sensor2.nearestISO(90) == doctest::Approx(83.7));
    CHECK(sensor2.nearestISO(95) == 100.0);
    CHECK(sensor2.nearestISO(6800) == 7000.0);
    CHECK(sensor2.nearestISO(1e6) == 7000.0);
  }
  SUBCASE("A bright field wants less than the base, and is overexposed by "
          "the stops between") {
    // Eight times the exposure wants ISO 50, a stop under the base.
    const MeteredExposure metered{sensor.meter(0.0203 * 8)};
    CHECK(metered.wantedISO == doctest::Approx(50.0).epsilon(1e-3));
    CHECK(metered.iso == 100.0);
    CHECK(metered.stopsOff == doctest::Approx(1.0).epsilon(1e-3));
    CHECK(metered.isOverexposed());
  }
  SUBCASE("A dim field wants more than the top, and is underexposed") {
    // A 64th of the exposure wants ISO 25600, two stops over the top.
    const MeteredExposure metered{sensor.meter(0.0203 / 64)};
    CHECK(metered.iso == 6400.0);
    CHECK(metered.stopsOff == doctest::Approx(-2.0).epsilon(1e-3));
    CHECK(metered.isUnderexposed());
  }
  SUBCASE("A dark reading wants everything, and gets the top") {
    const MeteredExposure metered{sensor.meter(0.0)};
    CHECK(metered.luxSeconds == 0.0);
    CHECK(std::isinf(metered.wantedISO));
    CHECK(metered.iso == 6400.0);
    CHECK(metered.isUnderexposed());
  }
}

TEST_CASE("Sensor: the illuminants a white balance names") {
  SUBCASE("The Planckian radiator at 2856 K is CIE illuminant A") {
    const std::vector<double> a{planckSpectrum(ILLUMINANT_A_KELVIN)};
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
    const std::vector<double> f2{of(WhiteBalanceKind::FLUORESCENT)};
    CHECK(f2[79] == 0.0);
    CHECK(f2[80] > 0.0);
    CHECK(f2[481] == 0.0);
    CHECK(f2[245] > 2.0 * f2[235]);
  }
  SUBCASE("A white is the illuminant through the observer, Y = 1") {
    const double3 white{illuminantWhite(daylightSpectrum(D65_KELVIN))};
    CHECK(white.y == 1.0);
    const double sum{white.x + white.y + white.z};
    CHECK(white.x / sum == doctest::Approx(0.3127).epsilon(0.002));
    CHECK(white.y / sum == doctest::Approx(0.3290).epsilon(0.002));
  }
}

TEST_CASE("Sensor: the training reflectances") {
  const std::vector<SensorSpectrum> &patches{trainingReflectances()};
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
  const std::vector<double> d65{daylightSpectrum(D65_KELVIN)};
  const std::array<size_t, 3> rgb{0, 1, 2};
  SUBCASE("A sensor whose curves are the observer's fits it to nothing, an "
          "index of 100") {
    const ColorFit fit{Sensor{lutherSensor()}.fitColor(rgb, d65)};
    CHECK(!fit.isSingular);
    CHECK(fit.isFaithful());
    CHECK(fit.meanDeltaE00 < 0.01);
    CHECK(fit.maxDeltaE00 < 0.05);
    CHECK(fit.index() > 99.9);
  }
  SUBCASE("The white lands exactly, and the multipliers balance it against "
          "green") {
    const Sensor sensor{gaussianSensor()};
    const ColorFit fit{sensor.fitColor(rgb, d65)};
    const double3 white{fit.cameraToXYZ * smdl::double3(1.0)};
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
    const ColorFit fit{Sensor{gaussianSensor()}.fitColor(rgb, d65)};
    CHECK(fit.isFaithful());
    CHECK(fit.meanDeltaE00 > 0.5);
    CHECK(fit.meanDeltaE00 < 5.0);
    CHECK(fit.index() > 60.0);
    CHECK(fit.index() < 99.0);
  }
  SUBCASE("Bands too much alike have no fit, and are not faithful") {
    SensorSettings settings{lutherSensor()};
    settings.response.bands[2] = settings.response.bands[1];
    const ColorFit fit{Sensor{settings}.fitColor(rgb, d65)};
    CHECK(fit.isSingular);
    CHECK(!fit.isFaithful());
  }
  SUBCASE("A band the illuminant does not reach has no fit") {
    SensorSettings settings{lutherSensor()};
    ResponseBand &band{settings.response.bands[2]};
    band.wavelengths = {840.0f, 900.0f};
    band.values = {0.5f, 0.5f};
    const ColorFit fit{Sensor{settings}.fitColor(rgb, d65)};
    CHECK(fit.isSingular);
    CHECK(fit.multipliers.x == 1.0);
  }
}
