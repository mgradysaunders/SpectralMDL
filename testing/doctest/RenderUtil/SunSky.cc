#include "doctest.h"

#include <cmath>
#include <random>
#include <vector>

#include "smdl/RenderUtil/SunSky.h"

namespace {

#include "SunSkyGolden.inl"

constexpr double DEG_TO_RAD = 0.017453292519943295;

smdl::float3 makeDirection(double zenithDeg, double azimuthDeg) {
  const double zenith{zenithDeg * DEG_TO_RAD};
  const double azimuth{azimuthDeg * DEG_TO_RAD};
  return {float(std::sin(zenith) * std::cos(azimuth)),
          float(std::sin(zenith) * std::sin(azimuth)), float(std::cos(zenith))};
}

// The wavelength in nanometers of the given model channel.
float channelWavelength(int channel) { return 400.0f + 5.0f * channel; }

} // namespace

TEST_CASE("SunSky") {
  SUBCASE("sky radiance golden") {
    // The golden spectra come from the Python fit at exact grid
    // wavelengths, so the port must reproduce them to float roundoff;
    // the tolerance only absorbs the float3 direction round trip. The
    // fit is in the native W/(cm^2 sr um), so the scale factor undoes
    // the conversion to W/(m^2 sr nm) built into the outputs.
    for (size_t c = 0; c < GOLDEN_CASE_COUNT; c++) {
      const double sunAzimuthDeg{25.0};
      smdl::SunSkyOptions options{};
      options.sunDirection = makeDirection(SKY_CASES[c][0], sunAzimuthDeg);
      options.visibility = float(SKY_CASES[c][3]);
      options.waterVaporScale = float(SKY_CASES[c][4]);
      options.scaleFactor = 0.1f;
      const auto sunSky{smdl::SunSky(options)};
      const auto direction{
          makeDirection(SKY_CASES[c][1], sunAzimuthDeg + SKY_CASES[c][2])};
      std::vector<float> wavelens(GOLDEN_CHANNEL_COUNT);
      std::vector<float> radiance(GOLDEN_CHANNEL_COUNT);
      for (size_t j = 0; j < GOLDEN_CHANNEL_COUNT; j++)
        wavelens[j] = channelWavelength(GOLDEN_CHANNELS[j]);
      sunSky.skyRadiance(direction, int(GOLDEN_CHANNEL_COUNT), wavelens.data(),
                         radiance.data());
      float peak{0.0f};
      for (size_t j = 0; j < GOLDEN_CHANNEL_COUNT; j++)
        peak = std::max(peak, SKY_EXPECTED[c][j]);
      float worst{0.0f};
      for (size_t j = 0; j < GOLDEN_CHANNEL_COUNT; j++)
        worst =
            std::max(worst, std::abs(radiance[j] - SKY_EXPECTED[c][j]) / peak);
      CAPTURE(c);
      CHECK(worst < 5e-4f);
    }
  }
  SUBCASE("sun radiance golden") {
    // The golden direct irradiance is recovered from the disk radiance
    // by multiplying the solid angle back, in the fit's native units
    // exactly as in the sky case above.
    for (size_t c = 0; c < GOLDEN_CASE_COUNT; c++) {
      smdl::SunSkyOptions options{};
      options.sunDirection = makeDirection(SUN_CASES[c][0], 0.0);
      options.visibility = float(SUN_CASES[c][1]);
      options.waterVaporScale = float(SUN_CASES[c][2]);
      options.scaleFactor = 0.1f;
      const auto sunSky{smdl::SunSky(options)};
      std::vector<float> wavelens(GOLDEN_CHANNEL_COUNT);
      std::vector<float> radiance(GOLDEN_CHANNEL_COUNT);
      for (size_t j = 0; j < GOLDEN_CHANNEL_COUNT; j++)
        wavelens[j] = channelWavelength(GOLDEN_CHANNELS[j]);
      sunSky.sunRadiance(int(GOLDEN_CHANNEL_COUNT), wavelens.data(),
                         radiance.data());
      float peak{0.0f};
      for (size_t j = 0; j < GOLDEN_CHANNEL_COUNT; j++)
        peak = std::max(peak, SUN_EXPECTED[c][j]);
      float worst{0.0f};
      for (size_t j = 0; j < GOLDEN_CHANNEL_COUNT; j++)
        worst = std::max(worst, std::abs(radiance[j] * sunSky.sunSolidAngle() -
                                         SUN_EXPECTED[c][j]) /
                                    peak);
      CAPTURE(c);
      CHECK(worst < 5e-4f);
    }
  }
  SUBCASE("wavelength interpolation and clamping") {
    const auto sunSky{
        smdl::SunSky(smdl::SunSkyOptions{makeDirection(40.0, 0.0)})};
    const auto direction{makeDirection(60.0, 120.0)};
    // A wavelength midway between grid channels must equal the average
    // of the neighbors, and wavelengths outside 400-2500nm must clamp
    // to the end channels.
    const float wavelens[7] = {500.0f, 505.0f,  502.5f, 380.0f,
                               400.0f, 2600.0f, 2500.0f};
    float radiance[7]{};
    sunSky.skyRadiance(direction, 7, wavelens, radiance);
    CHECK(radiance[2] ==
          doctest::Approx(0.5f * (radiance[0] + radiance[1])).epsilon(1e-5));
    CHECK(radiance[3] == radiance[4]);
    CHECK(radiance[5] == radiance[6]);
    float sunRadiance[7]{};
    sunSky.sunRadiance(7, wavelens, sunRadiance);
    CHECK(sunRadiance[2] ==
          doctest::Approx(0.5f * (sunRadiance[0] + sunRadiance[1]))
              .epsilon(1e-5));
    CHECK(sunRadiance[3] == sunRadiance[4]);
    CHECK(sunRadiance[5] == sunRadiance[6]);
  }
  SUBCASE("azimuthal symmetry and below-horizon continuation") {
    const auto sunSky{
        smdl::SunSky(smdl::SunSkyOptions{makeDirection(50.0, 30.0)})};
    const float wavelens[3] = {550.0f, 1000.0f, 1650.0f};
    // The model is symmetric in the relative azimuth.
    float positive[3]{}, negative[3]{};
    sunSky.skyRadiance(makeDirection(70.0, 30.0 + 40.0), 3, wavelens, positive);
    sunSky.skyRadiance(makeDirection(70.0, 30.0 - 40.0), 3, wavelens, negative);
    for (int j = 0; j < 3; j++)
      CHECK(positive[j] == doctest::Approx(negative[j]).epsilon(1e-5));
    // Below the horizon, the sky continues the horizon-ring value.
    float below[3]{}, horizon[3]{};
    sunSky.skyRadiance(makeDirection(95.0, 200.0), 3, wavelens, below);
    sunSky.skyRadiance(makeDirection(88.0, 200.0), 3, wavelens, horizon);
    for (int j = 0; j < 3; j++)
      CHECK(below[j] == doctest::Approx(horizon[j]).epsilon(1e-5));
    // Radiance is nonnegative over a sweep of directions.
    for (int zenith = 0; zenith <= 180; zenith += 15)
      for (int azimuth = 0; azimuth < 360; azimuth += 45) {
        float radiance[3]{};
        sunSky.skyRadiance(makeDirection(zenith, azimuth), 3, wavelens,
                           radiance);
        for (int j = 0; j < 3; j++) CHECK(radiance[j] >= 0.0f);
      }
  }
  SUBCASE("sun physics sanity") {
    // Broadband direct-normal irradiance at mid sun elevation must land
    // in the clear-sky range (a MODTRAN run at these parameters gives
    // 662 W/m^2 over 0.4-2.5um), and it must decrease with airmass.
    auto directNormalIrradiance{[](double sunZenithDeg) {
      smdl::SunSkyOptions options{};
      options.sunDirection = makeDirection(sunZenithDeg, 70.0);
      const auto sunSky{smdl::SunSky(options)};
      double sum{0.0};
      for (int i = 0; i < 421; i++) {
        const float wavelen{channelWavelength(i)};
        float radiance{};
        sunSky.sunRadiance(1, &wavelen, &radiance);
        // W/(m^2 sr nm) * sr * (5 nm)
        sum += double(radiance) * sunSky.sunSolidAngle() * 5.0;
      }
      return sum;
    }};
    const double dni40{directNormalIrradiance(40.0)};
    const double dni70{directNormalIrradiance(70.0)};
    const double dni85{directNormalIrradiance(85.0)};
    CHECK(dni40 > 500.0);
    CHECK(dni40 < 800.0);
    CHECK(dni70 < dni40);
    CHECK(dni85 < dni70);
    // The saturated 1.38um water band must be dark relative to the
    // 1.6um window, and deepen further with more water vapor.
    smdl::SunSkyOptions options{};
    options.sunDirection = makeDirection(40.0, 0.0);
    const auto sunSky{smdl::SunSky(options)};
    const float wavelens[2] = {1380.0f, 1600.0f};
    float radiance[2]{};
    sunSky.sunRadiance(2, wavelens, radiance);
    CHECK(radiance[0] < 0.01f * radiance[1]);
    // Disabling the sun zeroes the disk but not the sky.
    options.enableSun = false;
    const auto skyOnly{smdl::SunSky(options)};
    skyOnly.sunRadiance(2, wavelens, radiance);
    CHECK(radiance[0] == 0.0f);
    CHECK(radiance[1] == 0.0f);
    float skyRadiance[2]{};
    skyOnly.skyRadiance(makeDirection(50.0, 90.0), 2, wavelens, skyRadiance);
    CHECK(skyRadiance[1] > 0.0f);
  }
  SUBCASE("moon multiplier golden") {
    // Golden values from the Python reference implementation
    // (rolo_table.npz, gen_rolo_table.py in Empirical-Atm), which is
    // validated to float roundoff against the rimopy reference of the
    // ROLO model. The C++ path is all double, so agreement here is
    // double roundoff. Cases cover anchor interpolation, the clamped
    // wavelength ends, the rebuilt 2126.3nm band, waxing vs waning,
    // the distance scale, and the crescent-fade tail.
    struct MoonCase {
      double wavelenNm, phaseDeg, distanceScale, expected;
    };
    constexpr MoonCase kMoonCases[] = {
        {400, 2, 1, 2.0117149621426005e-06},
        {550, 7.5, 1, 1.9950311760825594e-06},
        {555, -30, 1, 1.0810244917549532e-06},
        {1234.5, 60, 1.14, 1.24729630779604e-06},
        {2126.3, 45, 1, 2.3368327193376489e-06},
        {2450, 90, 1, 8.3939129603826952e-07},
        {2500, 120, 1, 2.94580840565986e-07},
        {350, 2, 1, 1.7128284459452491e-06},
        {550, 155, 1, 4.8203680193892179e-09},
        {1600, 178, 1, 1.8678159109339203e-11},
    };
    for (const auto &moonCase : kMoonCases) {
      CAPTURE(moonCase.wavelenNm);
      CAPTURE(moonCase.phaseDeg);
      CHECK(smdl::SunSky::moonMultiplier(moonCase.wavelenNm, moonCase.phaseDeg,
                                         moonCase.distanceScale) ==
            doctest::Approx(moonCase.expected).epsilon(1e-12));
    }
    // Exactly zero at new moon, either sign.
    CHECK(smdl::SunSky::moonMultiplier(550.0, 180.0) == 0.0);
    CHECK(smdl::SunSky::moonMultiplier(550.0, -180.0) == 0.0);
  }
  SUBCASE("moonlight composition") {
    // Moonlight mode must be exactly the sun-sky evaluation at the
    // same geometry times the per-channel lunar multiplier, for the
    // sky dome and the disk alike. Compare at exact grid channels,
    // where the mode's precomputed per-channel multiplier and the
    // scalar moonMultiplier() coincide.
    smdl::SunSkyOptions options{};
    options.sunDirection = makeDirection(35.0, 40.0);
    const auto sunSky{smdl::SunSky(options)};
    options.moon = true;
    options.moonPhase = 30.0f;
    options.moonDistanceScale = 1.1f;
    const auto moonSky{smdl::SunSky(options)};
    const int channels[5] = {0, 30, 110, 240, 420};
    float wavelens[5]{};
    for (int j = 0; j < 5; j++) wavelens[j] = channelWavelength(channels[j]);
    for (const auto &direction :
         {makeDirection(60.0, 120.0), makeDirection(20.0, 300.0)}) {
      float sunValues[5]{}, moonValues[5]{};
      sunSky.skyRadiance(direction, 5, wavelens, sunValues);
      moonSky.skyRadiance(direction, 5, wavelens, moonValues);
      for (int j = 0; j < 5; j++) {
        const double m{smdl::SunSky::moonMultiplier(
            double(wavelens[j]), double(options.moonPhase),
            double(options.moonDistanceScale))};
        CHECK(moonValues[j] ==
              doctest::Approx(float(m * sunValues[j])).epsilon(1e-5));
      }
    }
    float sunDisk[5]{}, moonDisk[5]{};
    sunSky.sunRadiance(5, wavelens, sunDisk);
    moonSky.sunRadiance(5, wavelens, moonDisk);
    for (int j = 0; j < 5; j++) {
      const double m{smdl::SunSky::moonMultiplier(
          double(wavelens[j]), double(options.moonPhase),
          double(options.moonDistanceScale))};
      CHECK(moonDisk[j] ==
            doctest::Approx(float(m * sunDisk[j])).epsilon(1e-5));
    }
    // Sampling machinery stays functional under the tiny radiances.
    float samplePDF{};
    const auto wi{moonSky.sample(smdl::float2(0.37f, 0.61f), &samplePDF)};
    CHECK(smdl::lengthSquared(wi) == doctest::Approx(1.0f));
    CHECK(samplePDF > 0.0f);
    CHECK(moonSky.averageRadiance() > 0.0f);
    // A new moon is completely dark.
    options.moonPhase = 180.0f;
    const auto newMoon{smdl::SunSky(options)};
    float dark[5]{};
    newMoon.skyRadiance(makeDirection(60.0, 120.0), 5, wavelens, dark);
    for (int j = 0; j < 5; j++) CHECK(dark[j] == 0.0f);
    newMoon.sunRadiance(5, wavelens, dark);
    for (int j = 0; j < 5; j++) CHECK(dark[j] == 0.0f);
    CHECK(newMoon.averageRadiance() == 0.0f);
  }
  SUBCASE("sampling") {
    // For each compensation setting, `sample()` must agree with
    // `pdf()`, and the Monte Carlo estimate of the radiance integral
    // must match quadrature over the support of the pdf: the smooth
    // sky part by Riemann sum, the sun disk analytically (it is far
    // too small for any practical grid to resolve). With compensation
    // the support deliberately excludes the at-or-below-mean sky (BSDF
    // sampling covers it at MIS weight 1), so the supported integral
    // is strictly smaller than the full one; without compensation the
    // two coincide.
    for (const bool compensation : {true, false}) {
      smdl::SunSkyOptions options{};
      options.sunDirection = makeDirection(40.0, 25.0);
      options.enableMISCompensation = compensation;
      const auto sunSky{smdl::SunSky(options)};
      const float wavelens[3] = {550.0f, 1000.0f, 1650.0f};

      auto broadband{[&](const smdl::float3 &wi) {
        float radiance[3]{};
        sunSky.radiance(wi, 3, wavelens, radiance);
        return double(radiance[0]) + double(radiance[1]) + double(radiance[2]);
      }};
      double quadrature{0.0};
      double quadratureSupported{0.0};
      const int numTheta{512}, numPhi{1024};
      for (int iTheta = 0; iTheta < numTheta; iTheta++) {
        const double theta{smdl::PI * (iTheta + 0.5) / numTheta};
        for (int iPhi = 0; iPhi < numPhi; iPhi++) {
          const double phi{2.0 * smdl::PI * (iPhi + 0.5) / numPhi};
          const auto wi{makeDirection(theta / DEG_TO_RAD, phi / DEG_TO_RAD)};
          float radiance[3]{};
          sunSky.skyRadiance(wi, 3, wavelens, radiance);
          const double term{(double(radiance[0]) + double(radiance[1]) +
                             double(radiance[2])) *
                            std::sin(theta) * (smdl::PI / numTheta) *
                            (2.0 * smdl::PI / numPhi)};
          quadrature += term;
          if (sunSky.pdf(wi) > 0.0f) quadratureSupported += term;
        }
      }
      float sunRadiance[3]{};
      sunSky.sunRadiance(3, wavelens, sunRadiance);
      const double sunTerm{(double(sunRadiance[0]) + double(sunRadiance[1]) +
                            double(sunRadiance[2])) *
                           double(sunSky.sunSolidAngle())};
      quadrature += sunTerm;
      quadratureSupported += sunTerm;

      std::mt19937 prng{42};
      const int numSamples{100000};
      double estimate{0.0};
      int pdfAgreements{0};
      for (int s = 0; s < numSamples; s++) {
        float samplePDF{};
        const auto wi{
            sunSky.sample(smdl::generateCanonical2(prng), &samplePDF)};
        REQUIRE(samplePDF > 0.0f);
        CHECK(smdl::lengthSquared(wi) == doctest::Approx(1.0f));
        const float checkPDF{sunSky.pdf(wi)};
        if (std::abs(checkPDF - samplePDF) <= 1e-3f * samplePDF)
          pdfAgreements++;
        estimate += broadband(wi) / double(samplePDF);
      }
      estimate /= numSamples;
      CAPTURE(compensation);
      // Texel-boundary round trips may land in a neighboring texel.
      CHECK(pdfAgreements > int(0.99 * numSamples));
      CHECK(estimate == doctest::Approx(quadratureSupported).epsilon(0.03));
      if (compensation)
        CHECK(quadratureSupported < quadrature);
      else
        CHECK(quadratureSupported == doctest::Approx(quadrature).epsilon(1e-6));
    }
  }
}
