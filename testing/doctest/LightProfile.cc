#include "doctest.h"

#include <random>

#include "smdl/LightProfile.h"

// An axially symmetric Type C profile, brightest toward +Z and dimming
// toward -Z. Candela values are multiples of 683 so the parsed
// radiometric intensities are 1.0, 0.8, 0.6, 0.4, 0.2 W/sr.
static const char *axialIES = //
    "IESNA:LM-63-1995\n"
    "[TEST] Synthetic axially symmetric\n"
    "TILT=NONE\n"
    "1 1000 1 5 1 1 2 0.1 0.1 0.1\n"
    "1.0 1.0 100\n"
    "0 45 90 135 180\n"
    "0\n"
    "683 546.4 409.8 273.2 136.6\n";

// A quadrant symmetric Type C profile that varies in both the vertical
// and horizontal angles, with no emission below the horizon.
static const char *quadrantIES = //
    "IESNA:LM-63-1995\n"
    "[TEST] Synthetic quadrant symmetric\n"
    "TILT=NONE\n"
    "1 1000 1 3 3 1 2 0.1 0.1 0.1\n"
    "1.0 1.0 100\n"
    "0 45 90\n"
    "0 45 90\n"
    "683 546.4 409.8\n"
    "546.4 409.8 273.2\n"
    "409.8 273.2 136.6\n";

// Check that `directionPDF` and `directionSample` are consistent with
// each other and with `interpolate`:
// 1. The PDF must integrate to one over the sphere.
// 2. The PDF returned by sampling must agree with `directionPDF` at the
//    sampled direction.
// 3. Importance sampling `interpolate` must reproduce the intensity
//    integral computed by independent quadrature.
static void checkDistributionConsistency(const smdl::LightProfile &profile) {
  double pdfIntegral{};
  double intensityIntegral{};
  const int nY{256};
  const int nX{512};
  for (int iY = 0; iY < nY; iY++) {
    double theta{smdl::PI * (iY + 0.5) / nY};
    double sinTheta{std::sin(theta)};
    double cosTheta{std::cos(theta)};
    for (int iX = 0; iX < nX; iX++) {
      double phi{2.0 * smdl::PI * (iX + 0.5) / nX};
      auto w{smdl::float3(float(sinTheta * std::cos(phi)),
                          float(sinTheta * std::sin(phi)), float(cosTheta))};
      double dOmega{sinTheta * (smdl::PI / nY) * (2.0 * smdl::PI / nX)};
      pdfIntegral += profile.directionPDF(w) * dOmega;
      intensityIntegral += profile.interpolate(w) * dOmega;
    }
  }
  CHECK(pdfIntegral == doctest::Approx(1.0).epsilon(0.02));
  // The radiometric power is the intensity integrated over the sphere.
  CHECK(profile.power() == doctest::Approx(intensityIntegral).epsilon(0.02));
  std::mt19937 prng{};
  for (int iter = 0; iter < 100; iter++) {
    float pdf{};
    auto w{profile.directionSample(smdl::generateCanonical2(prng), &pdf)};
    REQUIRE(pdf > 0);
    CHECK(profile.directionPDF(w) == doctest::Approx(pdf).epsilon(1e-3));
  }
  double mcIntegral{};
  int numInvalid{};
  const int n{100'000};
  for (int iter = 0; iter < n; iter++) {
    float pdf{};
    auto w{profile.directionSample(smdl::generateCanonical2(prng), &pdf)};
    if (!(pdf > 0)) {
      numInvalid++;
      continue;
    }
    mcIntegral += profile.interpolate(w) / pdf;
  }
  CHECK(numInvalid == 0);
  CHECK(mcIntegral / n == doctest::Approx(intensityIntegral).epsilon(0.02));
}

TEST_CASE("LightProfile") {
  SUBCASE("Invalid profile") {
    auto profile{smdl::LightProfile()};
    CHECK(!profile.isValid());
    CHECK(profile.directionPDF(smdl::float3(0, 0, 1)) == 0.0f);
    float pdf{1.0f};
    auto w{profile.directionSample(smdl::float2(0.5f, 0.5f), &pdf)};
    CHECK(pdf == 0.0f);
    CHECK(w.x == 0.0f);
    CHECK(w.y == 0.0f);
    CHECK(w.z == 0.0f);
  }
  SUBCASE("Axially symmetric") {
    auto profile{smdl::LightProfile()};
    REQUIRE(!profile.loadFromFileMemory(axialIES));
    REQUIRE(profile.isValid());
    CHECK(profile.maxIntensity() == doctest::Approx(1.0f));
    // Spot check the parsed intensities through the interpolator.
    CHECK(profile.interpolate(smdl::float3(0, 0, +1)) ==
          doctest::Approx(1.0f).epsilon(1e-3));
    CHECK(profile.interpolate(smdl::float3(1, 0, 0)) ==
          doctest::Approx(0.6f).epsilon(1e-3));
    CHECK(profile.interpolate(smdl::float3(0, 0, -1)) ==
          doctest::Approx(0.2f).epsilon(1e-3));
    checkDistributionConsistency(profile);
  }
  SUBCASE("Quadrant symmetric") {
    auto profile{smdl::LightProfile()};
    REQUIRE(!profile.loadFromFileMemory(quadrantIES));
    REQUIRE(profile.isValid());
    // The quadrant unfolds by mirroring, so the +X and -X directions
    // agree, and there is no emission below the horizon.
    CHECK(profile.interpolate(smdl::float3(+1, 0, 0)) ==
          doctest::Approx(profile.interpolate(smdl::float3(-1, 0, 0))));
    CHECK(profile.interpolate(smdl::float3(0, 0, -1)) == 0.0f);
    checkDistributionConsistency(profile);
  }
}
