#include "doctest.h"

#include <array>
#include <cmath>
#include <cstdint>
#include <random>
#include <type_traits>
#include <vector>

#include "smdl/RenderUtil/MonteCarlo.h"

TEST_CASE("MonteCarlo") {
  SUBCASE("Distribution1D") {
    auto distr =
        smdl::Distribution1D(std::vector<float>{1.0f, 2.0f, 3.0f, 1.0f});
    CHECK(distr.indexPMF(0) == doctest::Approx(1.0 / 7.0));
    CHECK(distr.indexPMF(1) == doctest::Approx(2.0 / 7.0));
    CHECK(distr.indexPMF(2) == doctest::Approx(3.0 / 7.0));
    CHECK(distr.indexPMF(3) == doctest::Approx(1.0 / 7.0));
    CHECK(distr.indexCMF(0) == doctest::Approx(0.0 / 7.0));
    CHECK(distr.indexCMF(1) == doctest::Approx(1.0 / 7.0));
    CHECK(distr.indexCMF(2) == doctest::Approx(3.0 / 7.0));
    CHECK(distr.indexCMF(3) == doctest::Approx(6.0 / 7.0));
    CHECK(distr.indexCMF(4) == doctest::Approx(7.0 / 7.0));
    std::mt19937 prng{};
    std::array<int, 4> histogram{};
    for (int iter = 0; iter < 100'000; iter++) {
      auto i = distr.indexSample(smdl::generateCanonical(prng));
      histogram[i]++;
    }
    CHECK(histogram[0] * 1e-5 ==
          doctest::Approx(distr.indexPMF(0)).epsilon(1e-3));
    CHECK(histogram[1] * 1e-5 ==
          doctest::Approx(distr.indexPMF(1)).epsilon(1e-3));
    CHECK(histogram[2] * 1e-5 ==
          doctest::Approx(distr.indexPMF(2)).epsilon(1e-3));
    CHECK(histogram[3] * 1e-5 ==
          doctest::Approx(distr.indexPMF(3)).epsilon(1e-3));
  }
  SUBCASE("Distribution1D with all-zero values") {
    auto distr = smdl::Distribution1D(std::vector<float>{0.0f, 0.0f, 0.0f});
    CHECK(distr.indexPMF(0) == 0.0f);
    CHECK(distr.indexPMF(1) == 0.0f);
    CHECK(distr.indexPMF(2) == 0.0f);
    CHECK(distr.unnormalizedSum() == 0.0f);
  }
  SUBCASE("Distribution2D") {
    auto distr =
        smdl::Distribution2D(4, 2,
                             std::vector<float>{1.0f, 2.0f, 3.0f, 4.0f, //
                                                6.0f, 3.0f, 1.0f, 2.0f});
    CHECK(distr.pixelPMF(smdl::int2(0, 0)) == doctest::Approx(1.0 / 22.0));
    CHECK(distr.pixelPMF(smdl::int2(1, 0)) == doctest::Approx(2.0 / 22.0));
    CHECK(distr.pixelPMF(smdl::int2(2, 0)) == doctest::Approx(3.0 / 22.0));
    CHECK(distr.pixelPMF(smdl::int2(3, 0)) == doctest::Approx(4.0 / 22.0));
    CHECK(distr.pixelPMF(smdl::int2(0, 1)) == doctest::Approx(6.0 / 22.0));
    CHECK(distr.pixelPMF(smdl::int2(1, 1)) == doctest::Approx(3.0 / 22.0));
    CHECK(distr.pixelPMF(smdl::int2(2, 1)) == doctest::Approx(1.0 / 22.0));
    CHECK(distr.pixelPMF(smdl::int2(3, 1)) == doctest::Approx(2.0 / 22.0));
    std::mt19937 prng{};
    std::array<std::array<int, 4>, 2> histogram{};
    for (int iter = 0; iter < 1'000'000; iter++) {
      auto i = distr.pixelSample(smdl::generateCanonical2(prng));
      histogram[i.y][i.x]++;
    }
    for (int iY = 0; iY < 2; iY++) {
      for (int iX = 0; iX < 4; iX++) {
        CHECK(
            histogram[iY][iX] * 1e-6 ==
            doctest::Approx(distr.pixelPMF(smdl::int2(iX, iY))).epsilon(1e-3));
      }
    }
  }
}

// Checks that every elementary interval of area 2^-m contains exactly one
// of the 2^m points, for every split of m between the two axes. This is
// the defining property of a (0,2)-net in base 2.
static void checkNet(const std::vector<uint32_t> &X,
                     const std::vector<uint32_t> &Y, int m) {
  REQUIRE(X.size() == size_t(1) << m);
  REQUIRE(Y.size() == size_t(1) << m);
  for (int k1 = 0; k1 <= m; k1++) {
    const int k2{m - k1};
    auto counts{std::vector<int>(size_t(1) << m, 0)};
    for (size_t i = 0; i < X.size(); i++) {
      const uint32_t cellX{k1 == 0 ? 0U : X[i] >> (32 - k1)};
      const uint32_t cellY{k2 == 0 ? 0U : Y[i] >> (32 - k2)};
      counts[(cellX << k2) | cellY]++;
    }
    bool onePerCell{true};
    for (int count : counts) onePerCell &= count == 1;
    CHECK(onePerCell);
  }
}

TEST_CASE("QMC helpers") {
  // The golden values pin the exact bit patterns, which the sampler
  // sequence and thus resumable renders depend on.
  SUBCASE("reverseBits") {
    CHECK(smdl::reverseBits(0x00000000U) == 0x00000000U);
    CHECK(smdl::reverseBits(0xFFFFFFFFU) == 0xFFFFFFFFU);
    CHECK(smdl::reverseBits(0x00000001U) == 0x80000000U);
    CHECK(smdl::reverseBits(0x12345678U) == 0x1E6A2C48U);
    std::mt19937 prng{};
    bool involution{true};
    for (int iter = 0; iter < 1000; iter++) {
      const auto x{uint32_t(prng())};
      involution &= smdl::reverseBits(smdl::reverseBits(x)) == x;
    }
    CHECK(involution);
  }
  SUBCASE("mixBits") {
    static_assert(
        std::is_same_v<decltype(smdl::mixBits(uint32_t(1))), uint32_t>);
    static_assert(
        std::is_same_v<decltype(smdl::mixBits(uint64_t(1))), uint64_t>);
    // Both finalizers fix zero.
    CHECK(smdl::mixBits(uint32_t(0)) == 0U);
    CHECK(smdl::mixBits(uint64_t(0)) == 0ULL);
    CHECK(smdl::mixBits(uint32_t(1)) == 0x514E28B7U);
    CHECK(smdl::mixBits(uint32_t(42)) == 0x087FCD5CU);
    CHECK(smdl::mixBits(uint32_t(0xDEADBEEFU)) == 0x0DE5C6A9U);
    CHECK(smdl::mixBits(uint64_t(1)) == 0x5692161D100B05E5ULL);
    CHECK(smdl::mixBits(uint64_t(0x9E3779B97F4A7C15ULL)) ==
          0xE220A8397B1DCDAFULL);
  }
  SUBCASE("nestedUniformScramble nested property") {
    CHECK(smdl::nestedUniformScramble(0x12345678U, 0xCAFEBABEU) ==
          0x7530FA95U);
    // The property that makes the scramble Owen-style: inputs agreeing in
    // their top k bits map to outputs agreeing in their top k bits, for
    // every k, which is what preserves net structure.
    std::mt19937 prng{};
    bool nested{true};
    for (int iter = 0; iter < 10000; iter++) {
      const auto x{uint32_t(prng())};
      const auto low{uint32_t(prng())};
      const auto seed{uint32_t(prng())};
      const auto k{int(prng() % 33U)};
      const uint32_t mask{k == 0 ? 0U : ~uint32_t(0) << (32 - k)};
      const uint32_t x1{(x & mask) | (low & ~mask)};
      nested &= ((smdl::nestedUniformScramble(x, seed) ^
                  smdl::nestedUniformScramble(x1, seed)) &
                 mask) == 0U;
    }
    CHECK(nested);
  }
  SUBCASE("nestedUniformScramble is a permutation") {
    // Round-trip through the inverse: subtraction inverts the seed
    // addition, and 'x ^= x * c' for even c fixes at least one more low
    // bit per iteration, so 32 iterations of the recurrence recover the
    // input exactly.
    const auto invert{[](uint32_t y, uint32_t seed) {
      y = smdl::reverseBits(y);
      for (const uint32_t c :
           {0x8D22F6E6U, 0xC7AFE638U, 0xB82F1E52U, 0x6C50B47CU}) {
        uint32_t x{y};
        for (int i = 0; i < 32; i++) x = y ^ (x * c);
        y = x;
      }
      y -= seed;
      return smdl::reverseBits(y);
    }};
    std::mt19937 prng{};
    bool roundTrips{true};
    for (int iter = 0; iter < 10000; iter++) {
      const auto x{uint32_t(prng())};
      const auto seed{uint32_t(prng())};
      roundTrips &= invert(smdl::nestedUniformScramble(x, seed), seed) == x;
    }
    CHECK(roundTrips);
  }
  SUBCASE("Sobol pair is a (0,2)-net") {
    CHECK(smdl::sobolDim1(5) == 0x20000000U);
    CHECK(smdl::sobolDim1(0xFFFFU) == 0x00010000U);
    for (int m = 0; m <= 12; m++) {
      const auto N{uint32_t(1) << m};
      auto X{std::vector<uint32_t>(N)};
      auto Y{std::vector<uint32_t>(N)};
      for (uint32_t i = 0; i < N; i++) {
        X[i] = smdl::reverseBits(i);
        Y[i] = smdl::sobolDim1(i);
      }
      checkNet(X, Y, m);
    }
  }
}

TEST_CASE("OwenSobolSampler") {
  SUBCASE("determinism and golden sequence") {
    auto sampler{smdl::OwenSobolSampler()};
    sampler.start(0xC0FFEEU, 12345U);
    CHECK(sampler.generate() == 0xDACD9D31U);
    CHECK(sampler.generate() == 0x694EADCBU);
    CHECK(sampler.generate() == 0x0593BD4EU);
    CHECK(sampler.generate() == 0xE36AC6BBU);
    CHECK(sampler.generate() == 0x5622D566U);
    CHECK(sampler.generate() == 0xB5334A62U);
    // Restarting reproduces the sequence.
    sampler.start(0xC0FFEEU, 12345U);
    CHECK(sampler.generate() == 0xDACD9D31U);
    // A different seed diverges.
    auto other{smdl::OwenSobolSampler()};
    other.start(0xC0FFEFU, 12345U);
    sampler.start(0xC0FFEEU, 12345U);
    bool anyDiff{false};
    for (int i = 0; i < 8; i++) anyDiff |= other.generate() != sampler.generate();
    CHECK(anyDiff);
  }
  SUBCASE("draws in strict (0,1)") {
    auto sampler{smdl::OwenSobolSampler()};
    bool inRange{true};
    for (uint32_t index = 0; index < 256; index++) {
      sampler.start(0x51U, index);
      for (int d = 0; d < 16; d++) {
        const float xi{sampler.generateFloat()};
        inRange &= xi > 0.0f && xi < 1.0f;
      }
      const auto xi4{sampler.generateFloat4()};
      inRange &= xi4.x > 0.0f && xi4.x < 1.0f;
      inRange &= xi4.w > 0.0f && xi4.w < 1.0f;
    }
    CHECK(inRange);
  }
  SUBCASE("scrambling preserves the net") {
    // The index shuffle maps the first 2^m indexes among themselves (a
    // consequence of the nested property), and the per-dimension Owen
    // scrambles preserve elementary intervals, so every aligned pair of
    // the first 2^m points must remain a (0,2)-net for any seed.
    std::mt19937 prng{};
    for (int rep = 0; rep < 4; rep++) {
      const auto seed{uint32_t(prng())};
      for (const int pair : {0, 2}) {
        const int m{10};
        const auto N{uint32_t(1) << m};
        auto X{std::vector<uint32_t>(N)};
        auto Y{std::vector<uint32_t>(N)};
        for (uint32_t i = 0; i < N; i++) {
          auto sampler{smdl::OwenSobolSampler()};
          sampler.start(seed, i);
          for (int d = 0; d < 2 * pair; d++) (void)sampler.generate();
          X[i] = sampler.generate();
          Y[i] = sampler.generate();
        }
        checkNet(X, Y, m);
      }
    }
  }
  SUBCASE("alignPair and dimension accounting") {
    auto sampler{smdl::OwenSobolSampler()};
    sampler.start(1U, 2U);
    CHECK(sampler.dimension() == 0U);
    sampler.alignPair(); // aligned already, no move
    CHECK(sampler.dimension() == 0U);
    (void)sampler.generate();
    CHECK(sampler.dimension() == 1U);
    sampler.alignPair();
    CHECK(sampler.dimension() == 2U);
    sampler.alignPair(); // aligned already, no move
    CHECK(sampler.dimension() == 2U);
    (void)sampler.generateFloat2();
    CHECK(sampler.dimension() == 4U);
    (void)sampler.generateFloat3();
    CHECK(sampler.dimension() == 7U);
    sampler.alignPair();
    CHECK(sampler.dimension() == 8U);
    (void)sampler.generateFloat4();
    CHECK(sampler.dimension() == 12U);
    sampler.start(1U, 3U); // restart resets
    CHECK(sampler.dimension() == 0U);
  }
  SUBCASE("pairs decorrelate") {
    // The first components of pairs 0 and 1 across many indexes should
    // be uncorrelated; the tolerance is a few times 1/sqrt(N).
    const int N{4096};
    double sumX{}, sumY{}, sumXX{}, sumYY{}, sumXY{};
    for (int i = 0; i < N; i++) {
      auto sampler{smdl::OwenSobolSampler()};
      sampler.start(0xABCDEFU, uint32_t(i));
      const double x{sampler.generateFloat()};
      (void)sampler.generate();
      const double y{sampler.generateFloat()};
      sumX += x;
      sumY += y;
      sumXX += x * x;
      sumYY += y * y;
      sumXY += x * y;
    }
    const double correlation{
        (N * sumXY - sumX * sumY) /
        std::sqrt((N * sumXX - sumX * sumX) * (N * sumYY - sumY * sumY))};
    CHECK(std::abs(correlation) < 0.05);
  }
  SUBCASE("mean converges at the QMC rate") {
    // The first 2^m points of a scrambled dimension put one point in
    // each interval of width 2^-m, so the mean sits within 2^-m of 0.5,
    // far tighter than the Monte Carlo rate.
    const int N{4096};
    double sum{};
    for (int i = 0; i < N; i++) {
      auto sampler{smdl::OwenSobolSampler()};
      sampler.start(0x1234U, uint32_t(i));
      sum += sampler.generateFloat();
    }
    CHECK(sum / N == doctest::Approx(0.5).epsilon(1e-3));
  }
}
