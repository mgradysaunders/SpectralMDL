#include "doctest.h"

#include <cstring>
#include <random>
#include <vector>

#include "smdl/BSDFMeasurement.h"

// Build an MBSDF file in memory. The data must contain
// `numTheta * numTheta * numPhi` entries of 1 float (`TYPE_FLOAT`) or
// 3 floats (`TYPE_FLOAT3`) in `(iThetao, iThetai, iPhi)` order.
static std::string makeMBSDF(uint32_t type, uint32_t numTheta, uint32_t numPhi,
                             const std::vector<float> &data) {
  std::string s{"NVIDIA ARC MBSDF V1\n"};
  s += "description=\"synthetic test data\"\n";
  s += "MBSDF_DATA=\n";
  auto append32{[&](uint32_t v) {
    s += char(v & 0xff);
    s += char((v >> 8) & 0xff);
    s += char((v >> 16) & 0xff);
    s += char((v >> 24) & 0xff);
  }};
  append32(type);
  append32(numTheta);
  append32(numPhi);
  for (float value : data) {
    uint32_t bits{};
    std::memcpy(&bits, &value, 4);
    append32(bits);
  }
  return s;
}

// A smooth positive test value, symmetric in `(iThetao, iThetai)` so the
// synthetic BSDF is reciprocal, and varying on all three axes.
static float testValue(int iO, int iI, int iP) {
  return 0.1f + 0.05f * float(iO + iI) + 0.025f * float(iP);
}

static const int N_THETA{4};
static const int N_PHI{4};

static std::vector<float> testValuesFloat() {
  auto data{std::vector<float>()};
  for (int iO = 0; iO < N_THETA; iO++)
    for (int iI = 0; iI < N_THETA; iI++)
      for (int iP = 0; iP < N_PHI; iP++)
        data.push_back(testValue(iO, iI, iP));
  return data;
}

static std::vector<float> testValuesFloat3() {
  auto data{std::vector<float>()};
  for (int iO = 0; iO < N_THETA; iO++)
    for (int iI = 0; iI < N_THETA; iI++)
      for (int iP = 0; iP < N_PHI; iP++) {
        data.push_back(1.0f * testValue(iO, iI, iP));
        data.push_back(2.0f * testValue(iO, iI, iP));
        data.push_back(3.0f * testValue(iO, iI, iP));
      }
  return data;
}

// The zenith cell-center angle for the given index.
static float thetaCenter(int i) {
  return 0.5f * smdl::PI * (float(i) + 0.5f) / float(N_THETA);
}

// The azimuth-difference cell-center angle for the given index.
static float phiCenter(int i) {
  return smdl::PI * (float(i) + 0.5f) / float(N_PHI);
}

// Check that `directionPDF` and `directionSample` are consistent with each
// other and with `interpolate` for the given outgoing direction:
// 1. The PDF must integrate to one over the upper hemisphere.
// 2. The PDF returned by sampling must agree with `directionPDF` at the
//    sampled direction.
// 3. Importance sampling must reproduce the projected-solid-angle integral
//    of the measurement computed by independent quadrature.
static void checkDistributionConsistency(const smdl::BSDFMeasurement &measured,
                                         smdl::float3 wo) {
  double pdfIntegral{};
  double valueIntegral{};
  const int nY{128};
  const int nX{256};
  for (int iY = 0; iY < nY; iY++) {
    double theta{0.5 * smdl::PI * (iY + 0.5) / nY};
    double sinTheta{std::sin(theta)};
    double cosTheta{std::cos(theta)};
    for (int iX = 0; iX < nX; iX++) {
      double phi{2.0 * smdl::PI * (iX + 0.5) / nX};
      auto wi{smdl::float3(float(sinTheta * std::cos(phi)),
                           float(sinTheta * std::sin(phi)), float(cosTheta))};
      double dOmega{sinTheta * (0.5 * smdl::PI / nY) * (2.0 * smdl::PI / nX)};
      pdfIntegral += measured.directionPDF(wo, wi) * dOmega;
      auto value{measured.interpolate(wo, wi)};
      valueIntegral += (value.x + value.y + value.z) / 3.0 * cosTheta * dOmega;
    }
  }
  CHECK(pdfIntegral == doctest::Approx(1.0).epsilon(0.02));
  std::mt19937 prng{};
  for (int iter = 0; iter < 100; iter++) {
    float pdf{};
    auto wi{measured.directionSample(smdl::generateCanonical2(prng), wo, &pdf)};
    REQUIRE(pdf > 0);
    CHECK(measured.directionPDF(wo, wi) == doctest::Approx(pdf).epsilon(1e-3));
  }
  double mcIntegral{};
  int numInvalid{};
  const int n{100'000};
  for (int iter = 0; iter < n; iter++) {
    float pdf{};
    auto wi{measured.directionSample(smdl::generateCanonical2(prng), wo, &pdf)};
    if (!(pdf > 0)) {
      numInvalid++;
      continue;
    }
    auto value{measured.interpolate(wo, wi)};
    mcIntegral += (value.x + value.y + value.z) / 3.0 * std::abs(wi.z) / pdf;
  }
  CHECK(numInvalid == 0);
  CHECK(mcIntegral / n == doctest::Approx(valueIntegral).epsilon(0.02));
}

TEST_CASE("BSDFMeasurement") {
  SUBCASE("Invalid measurement") {
    auto measured{smdl::BSDFMeasurement()};
    CHECK(measured.directionPDF(smdl::float3(0, 0, 1), smdl::float3(0, 0, 1)) ==
          0.0f);
    float pdf{1.0f};
    auto wi{measured.directionSample(smdl::float2(0.5f, 0.5f),
                                     smdl::float3(0, 0, 1), &pdf)};
    CHECK(pdf == 0.0f);
    CHECK(wi.z == 0.0f);
  }
  SUBCASE("Float") {
    auto measured{smdl::BSDFMeasurement()};
    REQUIRE(!measured.loadFromFileMemory(
        makeMBSDF(0, N_THETA, N_PHI, testValuesFloat())));
    CHECK(measured.kind == smdl::BSDFMeasurement::KIND_REFLECTION);
    CHECK(measured.type == smdl::BSDFMeasurement::TYPE_FLOAT);
    CHECK(measured.numTheta == N_THETA);
    CHECK(measured.numPhi == N_PHI);
    // The fetch must splat scalar values to all three components.
    auto value{measured.fetch(1, 2, 3)};
    CHECK(value.x == doctest::Approx(testValue(1, 2, 3)));
    CHECK(value.y == doctest::Approx(testValue(1, 2, 3)));
    CHECK(value.z == doctest::Approx(testValue(1, 2, 3)));
    // Interpolating at cell centers reproduces the table exactly, and
    // interpolating between centers averages the neighboring values.
    CHECK(measured.interpolate(thetaCenter(1), thetaCenter(2), phiCenter(3)).x ==
          doctest::Approx(testValue(1, 2, 3)));
    CHECK(measured
              .interpolate(0.5f * (thetaCenter(1) + thetaCenter(2)),
                           thetaCenter(0), phiCenter(0))
              .x == doctest::Approx(0.5f * (testValue(1, 0, 0) + //
                                            testValue(2, 0, 0))));
    // Interpolating out of range clamps to the boundary values.
    CHECK(measured.interpolate(0.0f, thetaCenter(0), phiCenter(0)).x ==
          doctest::Approx(testValue(0, 0, 0)));
    CHECK(
        measured.interpolate(0.5f * smdl::PI, thetaCenter(0), phiCenter(0)).x ==
        doctest::Approx(testValue(N_THETA - 1, 0, 0)));
  }
  SUBCASE("Float3") {
    auto measured{smdl::BSDFMeasurement()};
    REQUIRE(!measured.loadFromFileMemory(
        makeMBSDF(1, N_THETA, N_PHI, testValuesFloat3())));
    CHECK(measured.type == smdl::BSDFMeasurement::TYPE_FLOAT3);
    auto value{measured.fetch(1, 2, 3)};
    CHECK(value.x == doctest::Approx(1.0f * testValue(1, 2, 3)));
    CHECK(value.y == doctest::Approx(2.0f * testValue(1, 2, 3)));
    CHECK(value.z == doctest::Approx(3.0f * testValue(1, 2, 3)));
    // Interpolate through directions: outgoing at the center of zenith
    // cell 1 with azimuth zero, incoming at the center of zenith cell 2
    // with azimuth difference at the center of cell 0.
    auto wo{smdl::float3(std::sin(thetaCenter(1)), 0.0f, //
                         std::cos(thetaCenter(1)))};
    auto wi{smdl::float3(std::sin(thetaCenter(2)) * std::cos(phiCenter(0)),
                         std::sin(thetaCenter(2)) * std::sin(phiCenter(0)),
                         std::cos(thetaCenter(2)))};
    CHECK(measured.interpolate(wo, wi).y ==
          doctest::Approx(2.0f * testValue(1, 2, 0)).epsilon(1e-3));
    // Reciprocity of the synthetic data carries through interpolation.
    CHECK(measured.interpolate(wo, wi).x ==
          doctest::Approx(measured.interpolate(wi, wo).x).epsilon(1e-3));
  }
  SUBCASE("Sampling consistency") {
    auto measured{smdl::BSDFMeasurement()};
    REQUIRE(!measured.loadFromFileMemory(
        makeMBSDF(1, N_THETA, N_PHI, testValuesFloat3())));
    checkDistributionConsistency(measured, smdl::float3(0, 0, 1));
    checkDistributionConsistency(measured,
                                 normalize(smdl::float3(0.4f, -0.2f, 0.8f)));
    checkDistributionConsistency(measured,
                                 normalize(smdl::float3(-0.7f, 0.3f, 0.4f)));
  }
}
