#include "Fixtures.h"

#include "smdl/RenderUtil/Colorimetry.h"
#include "smdl/RenderUtil/Illuminant.h"

namespace {
smdl::float2 kelvinToChromaticity(float kelvin) {
  smdl::float2 xy{};
  smdl::smdlKelvinToChromaticity(kelvin, &xy);
  return xy;
}

float evalIlluminantD(const smdl::float2 &xy, float wavelen) {
  float illum{};
  smdl::smdlEvalIlluminantD(1, &wavelen, &illum, xy);
  return illum;
}

float evalIlluminantF(int number, float wavelen) {
  float illum{};
  smdl::smdlEvalIlluminantF(1, &wavelen, &illum, number);
  return illum;
}

float evalIlluminantHP(int number, float wavelen) {
  float illum{};
  smdl::smdlEvalIlluminantHP(1, &wavelen, &illum, number);
  return illum;
}

float evalIlluminantLED(int number, float wavelen) {
  float illum{};
  smdl::smdlEvalIlluminantLED(1, &wavelen, &illum, number);
  return illum;
}

// Integrate the given spectral power distribution against the CIE 1931
// color matching functions, as the Wyman fit, and return the resulting
// chromaticity: independent of the illuminant tables under test.
template <typename Spd> smdl::float2 integrateChromaticity(Spd &&spd) {
  smdl::double3 xyz{0.0};
  for (int w = 300; w <= 830; w++)
    xyz += double(spd(float(w))) * smdl::wymanXYZ(double(w));
  const double sum{xyz.x + xyz.y + xyz.z};
  return {float(xyz.x / sum), float(xyz.y / sum)};
}
} // namespace

TEST_CASE(
    "Illuminant: the standard illuminants against published chromaticities") {
  SUBCASE("smdlKelvinToChromaticity matches the published locus") {
    // Null output must not crash.
    smdl::smdlKelvinToChromaticity(6504.0f, nullptr);

    // The published chromaticities of the standard D illuminants.
    static const struct {
      float kelvin{};
      smdl::float2 expectedXY{};
    } DAYLIGHTS[] = {{5003.0f, {0.34567f, 0.35850f}},  // D50
                     {5503.0f, {0.33242f, 0.34743f}},  // D55
                     {6504.0f, {0.31272f, 0.32903f}},  // D65
                     {7504.0f, {0.29902f, 0.31485f}}}; // D75
    for (const auto &daylight : DAYLIGHTS) {
      smdl::float2 xy = kelvinToChromaticity(daylight.kelvin);
      CHECK(xy[0] == doctest::Approx(daylight.expectedXY[0]).epsilon(5e-4));
      CHECK(xy[1] == doctest::Approx(daylight.expectedXY[1]).epsilon(5e-4));
    }

    // The temperature must clamp to the valid range of 4000K to 25000K.
    CHECK(kelvinToChromaticity(1000.0f)[0] == kelvinToChromaticity(4000.0f)[0]);
    CHECK(kelvinToChromaticity(1000.0f)[1] == kelvinToChromaticity(4000.0f)[1]);
    CHECK(kelvinToChromaticity(1e+6f)[0] == kelvinToChromaticity(25000.0f)[0]);
    CHECK(kelvinToChromaticity(1e+6f)[1] == kelvinToChromaticity(25000.0f)[1]);

    // The piecewise curves must agree at the crossover at 7000K.
    CHECK(kelvinToChromaticity(6999.0f)[0] ==
          doctest::Approx(kelvinToChromaticity(7001.0f)[0]).epsilon(1e-4));
    CHECK(kelvinToChromaticity(6999.0f)[1] ==
          doctest::Approx(kelvinToChromaticity(7001.0f)[1]).epsilon(1e-4));
  }
  SUBCASE("smdlEvalIlluminantD matches the published chromaticities") {
    // Null arguments must not crash.
    smdl::smdlEvalIlluminantD(1, nullptr, nullptr, smdl::float2(0.0f));

    // Compare the reconstruction to the published D65 table, which was
    // itself computed from the S0, S1, and S2 components with the M1 and M2
    // factors rounded to 3 decimal places, so agreement is expected to
    // within about 1%.
    smdl::float2 xyD65 = kelvinToChromaticity(6504.0f);
    static const struct {
      float wavelen{};
      float expectedIllum{};
    } D65_TABLE[] = {
        {300.0f, 0.000341f}, {320.0f, 0.202360f}, {340.0f, 0.399488f},
        {380.0f, 0.499755f}, {400.0f, 0.827549f}, {450.0f, 1.170080f},
        {500.0f, 1.093540f}, {550.0f, 1.040460f}, {560.0f, 1.000000f},
        {600.0f, 0.900062f}, {650.0f, 0.800268f}, {700.0f, 0.716091f},
        {720.0f, 0.616042f}, {780.0f, 0.633828f}, {830.0f, 0.603125f}};
    for (const auto &entry : D65_TABLE) {
      CHECK(evalIlluminantD(xyD65, entry.wavelen) ==
            doctest::Approx(entry.expectedIllum).epsilon(0.01));
    }

    // The reconstruction must be normalized to 1 at 560nm no matter the
    // chromaticity.
    for (float kelvin : {4000.0f, 5003.0f, 10000.0f, 25000.0f}) {
      CHECK(evalIlluminantD(kelvinToChromaticity(kelvin), 560.0f) ==
            doctest::Approx(1.0f));
    }

    // The reconstruction must interpolate linearly between table entries.
    CHECK(evalIlluminantD(xyD65, 565.0f) ==
          doctest::Approx(0.5f * (evalIlluminantD(xyD65, 560.0f) +
                                  evalIlluminantD(xyD65, 570.0f))));

    // Wavelengths outside the table must evaluate to zero.
    CHECK(evalIlluminantD(xyD65, 250.0f) == 0.0f);
    CHECK(evalIlluminantD(xyD65, 900.0f) == 0.0f);

    // Integrating the reconstruction against the color matching functions
    // must recover the chromaticity it was built from.
    for (float kelvin : {5003.0f, 7504.0f, 25000.0f}) {
      smdl::float2 xy = kelvinToChromaticity(kelvin);
      smdl::float2 integratedXY = integrateChromaticity(
          [&](float wavelen) { return evalIlluminantD(xy, wavelen); });
      CHECK(integratedXY[0] == doctest::Approx(xy[0]).epsilon(2e-3));
      CHECK(integratedXY[1] == doctest::Approx(xy[1]).epsilon(2e-3));
    }
  }
  SUBCASE("smdlEvalIlluminantF matches the published chromaticities") {
    // Null arguments must not crash.
    smdl::smdlEvalIlluminantF(1, nullptr, nullptr, 1);

    // An out-of-range number must fill with zeros.
    for (int number : {-1, 0, 13}) {
      float illum[3] = {7.0f, 7.0f, 7.0f};
      const float wavelens[3] = {450.0f, 550.0f, 650.0f};
      smdl::smdlEvalIlluminantF(3, wavelens, illum, number);
      CHECK(illum[0] == 0.0f);
      CHECK(illum[1] == 0.0f);
      CHECK(illum[2] == 0.0f);
    }

    // Spot check well-known published values at exact table wavelengths,
    // including the mercury peaks at 435nm and 545nm.
    CHECK(evalIlluminantF(2, 380.0f) == doctest::Approx(0.0118f));
    CHECK(evalIlluminantF(2, 405.0f) == doctest::Approx(0.1569f));
    CHECK(evalIlluminantF(2, 435.0f) == doctest::Approx(0.3498f));
    CHECK(evalIlluminantF(2, 545.0f) == doctest::Approx(0.2488f));
    CHECK(evalIlluminantF(2, 780.0f) == doctest::Approx(0.0027f));
    CHECK(evalIlluminantF(7, 380.0f) == doctest::Approx(0.0256f));
    CHECK(evalIlluminantF(11, 545.0f) == doctest::Approx(0.7284f));

    // The evaluation must interpolate linearly between table entries.
    CHECK(evalIlluminantF(1, 382.5f) ==
          doctest::Approx(0.5f * (0.0187f + 0.0236f)));

    // Wavelengths outside the table must evaluate to zero.
    CHECK(evalIlluminantF(1, 300.0f) == 0.0f);
    CHECK(evalIlluminantF(1, 800.0f) == 0.0f);

    // Integrating against the color matching functions must recover the
    // published chromaticities from CIE 15.
    static const smdl::float2 EXPECTED_XY[12] = {
        {0.3131f, 0.3371f}, {0.3721f, 0.3751f}, {0.4091f, 0.3941f},
        {0.4402f, 0.4031f}, {0.3138f, 0.3452f}, {0.3779f, 0.3882f},
        {0.3129f, 0.3292f}, {0.3458f, 0.3586f}, {0.3741f, 0.3727f},
        {0.3458f, 0.3588f}, {0.3805f, 0.3769f}, {0.4370f, 0.4042f}};
    for (int number = 1; number <= 12; number++) {
      smdl::float2 integratedXY = integrateChromaticity(
          [&](float wavelen) { return evalIlluminantF(number, wavelen); });
      CHECK(integratedXY[0] ==
            doctest::Approx(EXPECTED_XY[number - 1][0]).epsilon(2e-3));
      CHECK(integratedXY[1] ==
            doctest::Approx(EXPECTED_XY[number - 1][1]).epsilon(2e-3));
    }
  }
  SUBCASE("smdlEvalIlluminantHP matches the published chromaticities") {
    // Null arguments must not crash.
    smdl::smdlEvalIlluminantHP(1, nullptr, nullptr, 1);

    // An out-of-range number must fill with zeros.
    for (int number : {-1, 0, 6}) {
      float illum[3] = {7.0f, 7.0f, 7.0f};
      const float wavelens[3] = {450.0f, 550.0f, 650.0f};
      smdl::smdlEvalIlluminantHP(3, wavelens, illum, number);
      CHECK(illum[0] == 0.0f);
      CHECK(illum[1] == 0.0f);
      CHECK(illum[2] == 0.0f);
    }

    // Spot check published values at exact table wavelengths, including
    // the sodium peak of HP1 at 595nm.
    CHECK(evalIlluminantHP(1, 380.0f) == doctest::Approx(0.0190f));
    CHECK(evalIlluminantHP(1, 560.0f) == doctest::Approx(0.2078f));
    CHECK(evalIlluminantHP(1, 595.0f) == doctest::Approx(3.3484f));
    CHECK(evalIlluminantHP(1, 600.0f) == doctest::Approx(1.8940f));

    // The evaluation must interpolate linearly between table entries.
    CHECK(evalIlluminantHP(1, 597.5f) ==
          doctest::Approx(0.5f * (3.3484f + 1.8940f)));

    // Wavelengths outside the table must evaluate to zero.
    CHECK(evalIlluminantHP(1, 300.0f) == 0.0f);
    CHECK(evalIlluminantHP(1, 800.0f) == 0.0f);

    // Integrating against the color matching functions must recover the
    // published chromaticities from CIE 15:2004.
    static const smdl::float2 EXPECTED_XY[5] = {{0.5330f, 0.4150f},
                                                {0.4778f, 0.4158f},
                                                {0.4302f, 0.4075f},
                                                {0.3812f, 0.3797f},
                                                {0.3776f, 0.3713f}};
    for (int number = 1; number <= 5; number++) {
      smdl::float2 integratedXY = integrateChromaticity(
          [&](float wavelen) { return evalIlluminantHP(number, wavelen); });
      CHECK(integratedXY[0] ==
            doctest::Approx(EXPECTED_XY[number - 1][0]).epsilon(3e-3));
      CHECK(integratedXY[1] ==
            doctest::Approx(EXPECTED_XY[number - 1][1]).epsilon(3e-3));
    }
  }
  SUBCASE("smdlEvalIlluminantLED matches the published chromaticities") {
    // Null arguments must not crash.
    smdl::smdlEvalIlluminantLED(1, nullptr, nullptr, 1);

    // An out-of-range number must fill with zeros.
    for (int number : {-1, 0, 10}) {
      float illum[3] = {7.0f, 7.0f, 7.0f};
      const float wavelens[3] = {450.0f, 550.0f, 650.0f};
      smdl::smdlEvalIlluminantLED(3, wavelens, illum, number);
      CHECK(illum[0] == 0.0f);
      CHECK(illum[1] == 0.0f);
      CHECK(illum[2] == 0.0f);
    }

    // Spot check published values at exact table wavelengths: the blue
    // pump of LED-B1 and LED-B5 and the red emitter of LED-RGB1.
    CHECK(evalIlluminantLED(1, 380.0f) == 0.0f);
    CHECK(evalIlluminantLED(1, 455.0f) == doctest::Approx(0.0757f));
    CHECK(evalIlluminantLED(1, 560.0f) == doctest::Approx(0.1423f));
    CHECK(evalIlluminantLED(5, 450.0f) == doctest::Approx(0.3234f));
    CHECK(evalIlluminantLED(7, 560.0f) == doctest::Approx(0.0860f));
    CHECK(evalIlluminantLED(7, 630.0f) == doctest::Approx(0.5110f));
    CHECK(evalIlluminantLED(9, 560.0f) == doctest::Approx(0.1335f));

    // Wavelengths outside the table must evaluate to zero.
    CHECK(evalIlluminantLED(1, 300.0f) == 0.0f);
    CHECK(evalIlluminantLED(1, 800.0f) == 0.0f);

    // Integrating against the color matching functions must recover the
    // published chromaticities from CIE 15:2018, in the canonical order
    // LED-B1..B5, LED-BH1, LED-RGB1, LED-V1, LED-V2.
    static const smdl::float2 EXPECTED_XY[9] = {
        {0.4560f, 0.4078f}, {0.4357f, 0.4012f}, {0.3756f, 0.3723f},
        {0.3422f, 0.3502f}, {0.3118f, 0.3236f}, {0.4474f, 0.4066f},
        {0.4557f, 0.4211f}, {0.4548f, 0.4044f}, {0.3781f, 0.3775f}};
    for (int number = 1; number <= 9; number++) {
      smdl::float2 integratedXY = integrateChromaticity(
          [&](float wavelen) { return evalIlluminantLED(number, wavelen); });
      CHECK(integratedXY[0] ==
            doctest::Approx(EXPECTED_XY[number - 1][0]).epsilon(3e-3));
      CHECK(integratedXY[1] ==
            doctest::Approx(EXPECTED_XY[number - 1][1]).epsilon(3e-3));
    }
  }
}
