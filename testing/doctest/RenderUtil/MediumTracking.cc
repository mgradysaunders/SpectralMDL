#include "Fixtures.h"

#include <algorithm>
#include <array>
#include <cmath>
#include <cstdint>
#include <fstream>
#include <limits>
#include <random>
#include <string>
#include <vector>

#include "smdl/RenderUtil/MediumTracking.h"
#include "smdl/RenderUtil/MonteCarlo.h"
#include "smdl/Resource/VoxelGrid.h"

using smdl::float3;
using smdl::int3;
using smdl::MajorantSpanWalk;
using smdl::Span;
using smdl::TrackingMajorant;
using smdl::TrackingScratch;
using smdl::TrackOutcome;

// The estimators against closed forms, and against numerically integrated
// ones where there is no closed form, by their means over many fixed-seed
// draws. Every case runs at band counts on either side of a 16-wide
// vector and well past it, each spectrum sized exactly, so that the
// address sanitizer catches a kernel that touches a band past the last.

namespace {

constexpr std::array<size_t, 4> BAND_COUNTS{1, 16, 17, 200};

constexpr int NUM_SAMPLES{16384};

// Several standard errors of the noisiest estimator here at
// `NUM_SAMPLES`, so that the fixed seeds pass with room and a biased
// estimator, whose error does not shrink with the count, does not.
constexpr double MEAN_TOLERANCE{0.02};

// A tilt across the bands from 0.5 to 1.5, or 1 at a single band, so that
// the hero weighting is exercised wherever there is more than one band.
[[nodiscard]] std::vector<float> tiltOf(size_t numBands) {
  std::vector<float> tilt(numBands, 1.0f);
  for (size_t i = 0; numBands > 1 && i < numBands; i++)
    tilt[i] = 0.5f + float(i) / float(numBands - 1);
  return tilt;
}

// Canonical draws from a fixed seed, and a hero band drawn from them.
class Draws final {
public:
  explicit Draws(uint32_t seed) : rng_(seed) {}

  [[nodiscard]] float operator()() { return smdl::generateCanonical(rng_); }

  [[nodiscard]] size_t hero(size_t numBands) {
    return std::min(size_t((*this)() * float(numBands)), numBands - 1);
  }

private:
  std::mt19937 rng_;
};

// The per-band working storage of one tracked segment, sized exactly.
class Scratch final {
public:
  explicit Scratch(size_t numBands)
      : weight_(numBands), sigmaA_(numBands), sigmaS_(numBands),
        emission_(numBands) {}

  [[nodiscard]] TrackingScratch get() {
    return {weight_, sigmaA_, sigmaS_, emission_};
  }

private:
  std::vector<float> weight_{};

  std::vector<float> sigmaA_{};

  std::vector<float> sigmaS_{};

  std::vector<float> emission_{};
};

// Add `values` into the running `sums`.
void accumulate(std::vector<double> &sums, const std::vector<float> &values) {
  for (size_t i = 0; i < sums.size(); i++) sums[i] += double(values[i]);
}

// Check every band of `sums` over `NUM_SAMPLES` against `expect`.
void checkMeans(const std::vector<double> &sums,
                const std::vector<double> &expect, const char *what) {
  INFO(what);
  for (size_t i = 0; i < sums.size(); i++) {
    CAPTURE(i);
    CHECK_NEAR(sums[i] / NUM_SAMPLES, expect[i], MEAN_TOLERANCE);
  }
}

// Write a version-3 Mitsuba volume, single-channel float32 x-fastest.
void writeVol(const std::string &fileName, int nx, int ny, int nz,
              const std::vector<float> &values) {
  std::ofstream file(fileName, std::ios::binary);
  file.write("VOL", 3);
  const char version{3};
  file.write(&version, 1);
  const int32_t header[5] = {1, nx, ny, nz, 1};
  file.write(reinterpret_cast<const char *>(header), sizeof(header));
  const float bound[6] = {0.0f, 0.0f, 0.0f, 1.0f, 1.0f, 1.0f};
  file.write(reinterpret_cast<const char *>(bound), sizeof(bound));
  file.write(reinterpret_cast<const char *>(values.data()),
             std::streamsize(values.size() * sizeof(float)));
}

// A ramp of extinction along the segment, `tilt * (A + B * t)` per band,
// of which `ALBEDO` scatters, emitting `tilt * (1 + t)` per unit length.
// Over `[0, D]` the optical depth is `tilt * (A * D + B * D * D / 2)`.
struct Ramp final {
  static constexpr float A{0.2f};
  static constexpr float B{0.6f};
  static constexpr float D{2.0f};
  static constexpr float ALBEDO{0.7f};

  explicit Ramp(size_t numBands) : tilt(tiltOf(numBands)) {}

  [[nodiscard]] double opticalDepth(size_t i, double t) const {
    return double(tilt[i]) * (A * t + B * t * t / 2);
  }

  [[nodiscard]] double transmittance(size_t i, double t) const {
    return std::exp(-opticalDepth(i, t));
  }

  // The integral of the emission against the transmittance over
  // `[0, D]`, by Simpson's rule.
  [[nodiscard]] double emitted(size_t i) const {
    constexpr int N{4096};
    const double h{double(D) / N};
    double sum{};
    for (int j = 0; j <= N; j++) {
      const double t{j * h};
      const double f{double(tilt[i]) * (1 + t) * transmittance(i, t)};
      sum += f * (j == 0 || j == N ? 1 : j % 2 == 1 ? 4 : 2);
    }
    return sum * h / 3;
  }

  // The majorant over the whole segment, in the band of most extinction.
  [[nodiscard]] TrackingMajorant majorant() const {
    return {0.0f, *std::max_element(tilt.begin(), tilt.end()) * (A + B * D)};
  }

  void operator()(float t, float, Span<float> sigmaA, Span<float> sigmaS,
                  Span<float> emission) const {
    for (size_t i = 0; i < tilt.size(); i++) {
      const float sigmaT{tilt[i] * (A + B * t)};
      sigmaA[i] = (1.0f - ALBEDO) * sigmaT;
      sigmaS[i] = ALBEDO * sigmaT;
      emission[i] = tilt[i] * (1.0f + t);
    }
  }

  std::vector<float> tilt{};
};

// The one global span of a segment over `[0, tEnd)` with no grid.
[[nodiscard]] MajorantSpanWalk spansOver(float tEnd) {
  return {nullptr, float3(), float3(1.0f, 0.0f, 0.0f), 1.0f, tEnd, false};
}

} // namespace

TEST_CASE("beerLambert: the transmittance of an optical depth") {
  CHECK(smdl::beerLambert(-1.0f) == 1.0f);
  for (float opticalDepth : {0.25f, 1.0f, 10.0f, 80.0f}) {
    CAPTURE(opticalDepth);
    CHECK_NEAR(smdl::beerLambert(opticalDepth) / std::exp(-opticalDepth), 1.0,
               1e-4);
  }
  CHECK(smdl::beerLambert(87.0f) == 0.0f);
  CHECK(smdl::beerLambert(smdl::INF) == 0.0f);
  CHECK(smdl::beerLambert(std::numeric_limits<float>::quiet_NaN()) == 0.0f);
}

TEST_CASE("MediumTracking: the homogeneous closed forms band by band") {
  for (size_t numBands : BAND_COUNTS) {
    CAPTURE(numBands);
    const std::vector<float> tilt{tiltOf(numBands)};
    std::vector<float> sigma(numBands);
    for (size_t i = 0; i < numBands; i++) sigma[i] = 0.7f * tilt[i];
    SUBCASE("Beer-Lambert attenuation") {
      std::vector<float> beta(numBands, 2.0f);
      smdl::attenuateBeerLambert(beta, sigma, 1.5f);
      for (size_t i = 0; i < numBands; i++) {
        CAPTURE(i);
        CHECK_NEAR(beta[i], 2.0 * std::exp(-1.5 * sigma[i]), 1e-5);
      }
    }
    SUBCASE("A band of no extinction keeps its throughput at any depth") {
      sigma[0] = 0.0f;
      std::vector<float> beta(numBands, 1.0f);
      smdl::attenuateBeerLambert(beta, sigma, smdl::INF);
      CHECK(beta[0] == 1.0f);
      for (size_t i = 1; i < numBands; i++) CHECK(beta[i] == 0.0f);
    }
    SUBCASE("Emission integrates the transmittance") {
      sigma[0] = 0.0f;
      std::vector<float> emission(numBands);
      for (size_t i = 0; i < numBands; i++) emission[i] = 0.5f * tilt[i];
      std::vector<float> emitted(numBands, 1.0f);
      smdl::accumulateHomogeneousEmission(emitted, emission, sigma, 1.5f);
      CHECK_NEAR(emitted[0], 1.0 + 1.5 * emission[0], 1e-5);
      for (size_t i = 1; i < numBands; i++) {
        CAPTURE(i);
        CHECK_NEAR(emitted[i],
                   1.0 + emission[i] * (1.0 - std::exp(-1.5 * sigma[i])) /
                             sigma[i],
                   1e-5);
      }
    }
    SUBCASE("Coefficients clamp to the majorants at the local scale") {
      std::vector<float> sigmaA(numBands, 1.0f);
      std::vector<float> sigmaS(numBands, -1.0f);
      std::vector<float> emission(numBands, -1.0f);
      sigmaA[0] = 0.1f;
      smdl::clampToMajorants(sigmaA, sigmaS, emission, sigma, sigma, 0.5f);
      CHECK(sigmaA[0] == doctest::Approx(std::min(0.1f, 0.5f * sigma[0])));
      for (size_t i = 1; i < numBands; i++) {
        CAPTURE(i);
        CHECK(sigmaA[i] == 0.5f * sigma[i]);
      }
      for (size_t i = 0; i < numBands; i++) {
        CAPTURE(i);
        CHECK(sigmaS[i] == 0.0f);
        CHECK(emission[i] == 0.0f);
      }
    }
  }
}

TEST_CASE("homogeneousFreeFlight: the means are the closed forms") {
  constexpr float DISTANCE{1.5f};
  constexpr float ALBEDO{0.6f};
  for (size_t numBands : BAND_COUNTS) {
    CAPTURE(numBands);
    const std::vector<float> tilt{tiltOf(numBands)};
    std::vector<float> sigmaT(numBands), sigmaS(numBands);
    for (size_t i = 0; i < numBands; i++) {
      sigmaT[i] = 0.8f * tilt[i];
      sigmaS[i] = ALBEDO * sigmaT[i];
    }
    std::vector<float> scratch(numBands);
    SUBCASE("The survival and scattering weights, and where it scatters") {
      std::vector<double> survived(numBands), scattered(numBands),
          scatteredNear(numBands);
      Draws draws{1};
      for (int sample = 0; sample < NUM_SAMPLES; sample++) {
        const float xi{draws()};
        std::vector<float> beta(numBands, 1.0f);
        float t{-1.0f};
        if (smdl::homogeneousFreeFlight(xi, draws.hero(numBands), sigmaT,
                                        sigmaS, DISTANCE, scratch, beta, t)) {
          REQUIRE(t >= 0.0f);
          REQUIRE(t < DISTANCE);
          accumulate(scattered, beta);
          if (t < DISTANCE / 2) accumulate(scatteredNear, beta);
        } else {
          accumulate(survived, beta);
        }
      }
      std::vector<double> expectSurvived(numBands),
          expectScattered(numBands), expectScatteredNear(numBands);
      for (size_t i = 0; i < numBands; i++) {
        const double sigma{sigmaT[i]};
        expectSurvived[i] = std::exp(-sigma * DISTANCE);
        expectScattered[i] = ALBEDO * (1.0 - std::exp(-sigma * DISTANCE));
        expectScatteredNear[i] =
            ALBEDO * (1.0 - std::exp(-sigma * DISTANCE / 2));
      }
      checkMeans(survived, expectSurvived, "the survival weight");
      checkMeans(scattered, expectScattered, "the scattering weight");
      checkMeans(scatteredNear, expectScatteredNear,
                 "the scattering weight over the near half");
    }
    SUBCASE("A hero of no extinction survives any distance") {
      sigmaT[0] = sigmaS[0] = 0.0f;
      std::vector<float> beta(numBands, 1.0f);
      float t{-1.0f};
      CHECK_FALSE(smdl::homogeneousFreeFlight(0.5f, 0, sigmaT, sigmaS,
                                              smdl::INF, scratch, beta, t));
      CHECK(t == -1.0f);
      CHECK(beta[0] == float(numBands));
      for (size_t i = 1; i < numBands; i++) CHECK(beta[i] == 0.0f);
    }
  }
}

TEST_CASE("deltaTrack: a ramp's survival, scattering, and emission are "
          "unbiased") {
  for (size_t numBands : BAND_COUNTS) {
    CAPTURE(numBands);
    const Ramp ramp{numBands};
    Scratch scratch{numBands};
    SUBCASE("The means against the integrals") {
      std::vector<double> survived(numBands), scattered(numBands),
          scatteredNear(numBands), emitted(numBands);
      Draws draws{2};
      for (int sample = 0; sample < NUM_SAMPLES; sample++) {
        std::vector<float> beta(numBands, 1.0f);
        std::vector<float> emission(numBands, 0.0f);
        float t{-1.0f};
        const TrackOutcome outcome{smdl::deltaTrack(
            spansOver(Ramp::D), ramp.majorant(), draws.hero(numBands), draws,
            ramp, scratch.get(), beta, emission, t)};
        REQUIRE(outcome != TrackOutcome::DEAD);
        if (outcome == TrackOutcome::SCATTERED) {
          REQUIRE(t >= 0.0f);
          REQUIRE(t < Ramp::D);
          accumulate(scattered, beta);
          if (t < Ramp::D / 2) accumulate(scatteredNear, beta);
        } else {
          accumulate(survived, beta);
        }
        accumulate(emitted, emission);
      }
      std::vector<double> expectSurvived(numBands),
          expectScattered(numBands), expectScatteredNear(numBands),
          expectEmitted(numBands);
      for (size_t i = 0; i < numBands; i++) {
        expectSurvived[i] = ramp.transmittance(i, Ramp::D);
        expectScattered[i] =
            Ramp::ALBEDO * (1.0 - ramp.transmittance(i, Ramp::D));
        expectScatteredNear[i] =
            Ramp::ALBEDO * (1.0 - ramp.transmittance(i, Ramp::D / 2));
        expectEmitted[i] = ramp.emitted(i);
      }
      checkMeans(survived, expectSurvived, "the survival weight");
      checkMeans(scattered, expectScattered, "the scattering weight");
      checkMeans(scatteredNear, expectScatteredNear,
                 "the scattering weight over the near half");
      checkMeans(emitted, expectEmitted, "the emitted radiance");
    }
    SUBCASE("The collision cap kills the segment") {
      // Nothing there, under a majorant that takes a billion null
      // collisions to cross it.
      const auto empty{[](float, float, Span<float> sigmaA,
                          Span<float> sigmaS, Span<float> emission) {
        smdl::detail::bandFill(sigmaA, 0.0f);
        smdl::detail::bandFill(sigmaS, 0.0f);
        smdl::detail::bandFill(emission, 0.0f);
      }};
      Draws draws{3};
      std::vector<float> beta(numBands, 1.0f);
      float t{-1.0f};
      CHECK(smdl::deltaTrack(spansOver(1.0f), TrackingMajorant{0.0f, 1e9f}, 0,
                             draws, empty, scratch.get(), beta, {}, t) ==
            TrackOutcome::DEAD);
      for (float value : beta) CHECK(value == 0.0f);
    }
  }
}

TEST_CASE("residualRatioTrack: a ramp's transmittance is unbiased") {
  for (size_t numBands : BAND_COUNTS) {
    CAPTURE(numBands);
    const Ramp ramp{numBands};
    Scratch scratch{numBands};
    std::vector<double> transmittance(numBands);
    Draws draws{4};
    for (int sample = 0; sample < NUM_SAMPLES; sample++) {
      std::vector<float> beta(numBands, 1.0f);
      REQUIRE(smdl::residualRatioTrack(spansOver(Ramp::D), ramp.majorant(), {},
                                       draws, ramp, scratch.get(),
                                       beta) == TrackOutcome::SURVIVED);
      accumulate(transmittance, beta);
    }
    std::vector<double> expect(numBands);
    for (size_t i = 0; i < numBands; i++)
      expect[i] = ramp.transmittance(i, Ramp::D);
    checkMeans(transmittance, expect, "the transmittance");
  }
}

TEST_CASE("MediumTracking: the majorant spans of a density grid") {
  TempDir tmpDir{"medium-tracking"};
  // A density stepping along X in voxels of unit width, three voxels deep
  // in Y and Z, and segments along X through the middle row, where the
  // dilated cell bounds see only the row's own densities. The extinction
  // is the voxel's density times the coefficient at full density, so the
  // walk's cell bounds bound it, and the optical depth is the coefficient
  // times the sum of the densities crossed.
  constexpr int NX{8}, NY{3}, NZ{3};
  const auto writeGrid{[&](const char *name, const std::vector<float> &row) {
    std::vector<float> values(size_t(NX) * NY * NZ);
    for (size_t j = 0; j < values.size(); j++) values[j] = row[j % NX];
    const std::string fileName{(tmpDir / name).string()};
    writeVol(fileName, NX, NY, NZ, values);
    return fileName;
  }};
  const float3 dir{1.0f, 0.0f, 0.0f};
  for (size_t numBands : BAND_COUNTS) {
    CAPTURE(numBands);
    const std::vector<float> tilt{tiltOf(numBands)};
    std::vector<float> sigmaMax(numBands);
    for (size_t i = 0; i < numBands; i++) sigmaMax[i] = 0.4f * tilt[i];
    const TrackingMajorant majorant{
        0.0f, *std::max_element(sigmaMax.begin(), sigmaMax.end())};
    Scratch scratch{numBands};
    const auto queryOf{[&](const std::vector<float> &row, const float3 &org) {
      return [&, org](float t, float, Span<float> sigmaA, Span<float> sigmaS,
                      Span<float> emission) {
        const float x{org.x + t};
        const float density{x >= 0.0f && x < float(NX) ? row[size_t(x)]
                                                        : 0.0f};
        for (size_t i = 0; i < sigmaMax.size(); i++) {
          sigmaA[i] = sigmaMax[i] * density;
          sigmaS[i] = 0.0f;
          emission[i] = 0.0f;
        }
      };
    }};
    SUBCASE("A stepped density, tracked and ratio tracked with a control") {
      // From outside the grid to outside it again.
      const float3 org{-1.0f, 1.5f, 1.5f};
      constexpr float DISTANCE{float(NX) + 2.0f};
      const std::vector<float> row{0.0f, 0.25f, 1.0f,  0.5f,
                                   0.5f, 0.0f,  0.75f, 0.125f};
      smdl::VoxelGrid grid{};
      REQUIRE_OK(grid.loadFromFile(writeGrid("stepped.vol", row)));
      REQUIRE(grid.getMajorantExtent() == 1);
      REQUIRE(smdl::isAllTrue(grid.getMajorantCount() == int3(NX, NY, NZ)));
      const float invMaxValue{1.0f / grid.getMaxValue()};
      const auto query{queryOf(row, org)};
      std::vector<double> survived(numBands), ratio(numBands);
      Draws draws{5};
      for (int sample = 0; sample < NUM_SAMPLES; sample++) {
        std::vector<float> beta(numBands, 1.0f);
        float t{-1.0f};
        const TrackOutcome outcome{smdl::deltaTrack(
            MajorantSpanWalk{&grid, org, dir, invMaxValue, DISTANCE, true},
            majorant, draws.hero(numBands), draws, query, scratch.get(), beta,
            {}, t)};
        // Nothing scatters, so the walk absorbs or survives.
        REQUIRE(outcome != TrackOutcome::DEAD);
        if (outcome == TrackOutcome::SURVIVED) accumulate(survived, beta);
        else
          for (float value : beta) REQUIRE(value == 0.0f);
        // A collision in the densest voxel, whose cell majorant is its own
        // density, zeroes the ratio and is a legitimate estimate of zero.
        std::fill(beta.begin(), beta.end(), 1.0f);
        if (smdl::residualRatioTrack(
                MajorantSpanWalk{&grid, org, dir, invMaxValue, DISTANCE, true},
                majorant, sigmaMax, draws, query, scratch.get(),
                beta) == TrackOutcome::DEAD)
          for (float value : beta) REQUIRE(value == 0.0f);
        accumulate(ratio, beta);
      }
      float densitySum{};
      for (float density : row) densitySum += density;
      std::vector<double> expect(numBands);
      for (size_t i = 0; i < numBands; i++)
        expect[i] = std::exp(-double(sigmaMax[i]) * densitySum);
      checkMeans(survived, expect, "the delta-tracked survival weight");
      checkMeans(ratio, expect, "the ratio-tracked transmittance");
    }
    SUBCASE("A constant density is all control, with no variance") {
      // Through the cells whose dilation stays inside the grid.
      const float3 org{1.0f, 1.5f, 1.5f};
      constexpr float DISTANCE{float(NX) - 2.0f};
      const std::vector<float> row(NX, 1.0f);
      smdl::VoxelGrid grid{};
      REQUIRE_OK(grid.loadFromFile(writeGrid("constant.vol", row)));
      const float invMaxValue{1.0f / grid.getMaxValue()};
      const auto query{queryOf(row, org)};
      Draws draws{6};
      for (int sample = 0; sample < 64; sample++) {
        std::vector<float> beta(numBands, 1.0f);
        REQUIRE(smdl::residualRatioTrack(
                    MajorantSpanWalk{&grid, org, dir, invMaxValue, DISTANCE,
                                     true},
                    majorant, sigmaMax, draws, query, scratch.get(),
                    beta) == TrackOutcome::SURVIVED);
        for (size_t i = 0; i < numBands; i++) {
          CAPTURE(i);
          CHECK_NEAR(beta[i] / std::exp(-double(sigmaMax[i]) * DISTANCE), 1.0,
                     1e-4);
        }
      }
    }
  }
}

TEST_CASE("pickScatterComponent: picks in proportion and keeps the per-band "
          "share") {
  for (size_t numBands : BAND_COUNTS) {
    CAPTURE(numBands);
    const std::vector<float> tilt{tiltOf(numBands)};
    // Two media of different spectral shape around one that does not
    // scatter, with nothing scattering in the first band of several.
    std::vector<float> first(numBands), none(numBands), last(numBands),
        sum(numBands);
    for (size_t i = 0; i < numBands; i++) {
      first[i] = 0.3f * tilt[i];
      last[i] = 0.2f * (2.0f - tilt[i]);
    }
    if (numBands > 1) first[0] = last[0] = 0.0f;
    for (size_t i = 0; i < numBands; i++) sum[i] = first[i] + last[i];
    const std::array<const std::vector<float> *, 3> components{&first, &none,
                                                              &last};
    const auto sigmaSOf{
        [&](size_t k) { return Span<const float>(*components[k]); }};
    SUBCASE("The per-component means are the per-band shares") {
      std::vector<double> firstSums(numBands), lastSums(numBands);
      for (int sample = 0; sample < NUM_SAMPLES; sample++) {
        const float xi{(float(sample) + 0.5f) / float(NUM_SAMPLES)};
        std::vector<float> beta(numBands, 1.0f);
        const size_t picked{
            smdl::pickScatterComponent(xi, 3, sigmaSOf, sum, beta)};
        REQUIRE(picked != 1);
        accumulate(picked == 0 ? firstSums : lastSums, beta);
        if (numBands > 1) REQUIRE(beta[0] == 0.0f);
      }
      std::vector<double> firstShare(numBands), lastShare(numBands);
      for (size_t i = 0; i < numBands; i++) {
        firstShare[i] = sum[i] > 0.0f ? first[i] / sum[i] : 0.0;
        lastShare[i] = sum[i] > 0.0f ? last[i] / sum[i] : 0.0;
      }
      checkMeans(firstSums, firstShare, "the first medium's share");
      checkMeans(lastSums, lastShare, "the last medium's share");
    }
    SUBCASE("Where nothing scatters, nothing changes") {
      const auto noneOf{[&](size_t) { return Span<const float>(none); }};
      std::vector<float> beta(numBands, 1.0f);
      CHECK(smdl::pickScatterComponent(0.5f, 2, noneOf, none, beta) == 0);
      for (float value : beta) CHECK(value == 1.0f);
    }
  }
}
