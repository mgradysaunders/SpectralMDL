#include "Fixtures.h"

#include <algorithm>
#include <array>
#include <cmath>
#include <random>
#include <vector>

#include "smdl/RenderUtil/MonteCarlo.h"

TEST_CASE("MonteCarlo: the piecewise-constant distributions") {
  SUBCASE("Distribution1D draws in proportion to its values") {
    smdl::Distribution1D distr =
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
      int i = distr.indexSample(smdl::generateCanonical(prng));
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
  SUBCASE("Distribution1D never draws an index with no probability, even "
          "at the bottom of the range") {
    smdl::Distribution1D distr =
        smdl::Distribution1D(std::vector<float>{0.0f, 0.0f, 1.0f, 2.0f});
    float xiRemap{-1.0f};
    CHECK(distr.indexSample(0.0f, &xiRemap) == 2);
    CHECK(xiRemap >= 0.0f);
    CHECK(distr.indexSample(1e-30f) == 2);
    CHECK(distr.indexSample(0.5f) == 3);
    CHECK(distr.indexSample(1.0f) == 3);
  }
  SUBCASE("Distribution1D with all-zero values draws uniformly") {
    smdl::Distribution1D distr =
        smdl::Distribution1D(std::vector<float>{0.0f, 0.0f, 0.0f});
    CHECK(distr.indexPMF(0) == 0.0f);
    CHECK(distr.indexPMF(1) == 0.0f);
    CHECK(distr.indexPMF(2) == 0.0f);
    CHECK(distr.unnormalizedSum() == 0.0f);
  }
  SUBCASE("Distribution2D draws in proportion to its values") {
    smdl::Distribution2D distr =
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
      smdl::int2 i = distr.pixelSample(smdl::generateCanonical2(prng));
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

TEST_CASE("MonteCarlo: the canonical sample helpers") {
  SUBCASE("canonicalize holds anything inside the open interval") {
    CHECK(smdl::canonicalize(0.0f) > 0.0f);
    CHECK(smdl::canonicalize(-1.0f) > 0.0f);
    CHECK(smdl::canonicalize(1.0f) < 1.0f);
    CHECK(smdl::canonicalize(2.0f) < 1.0f);
    CHECK(smdl::canonicalize(0.5f) == 0.5f);
    // Every value it can return is safe to divide by and to take the
    // logarithm of, which is the whole contract.
    for (const float x : {-1.0f, 0.0f, 0x1p-32f, 0.5f, 1.0f, 2.0f}) {
      const float xi{smdl::canonicalize(x)};
      CHECK(std::isfinite(1.0f / xi));
      CHECK(std::isfinite(std::log(xi)));
      CHECK(std::isfinite(std::log1p(-xi)));
    }
  }
  SUBCASE("uniformTriangleSample stays in the triangle") {
    std::mt19937 prng{};
    bool isInside{true};
    smdl::float3 mean{};
    constexpr int NUM_SAMPLES{100'000};
    for (int iter = 0; iter < NUM_SAMPLES; iter++) {
      const smdl::float3 bary{
          smdl::uniformTriangleSample(smdl::generateCanonical2(prng))};
      isInside &= bary.x >= 0.0f && bary.y >= 0.0f && bary.z >= 0.0f;
      isInside &= std::abs(bary.x + bary.y + bary.z - 1.0f) < 1e-5f;
      mean = mean + bary * (1.0f / float(NUM_SAMPLES));
    }
    CHECK(isInside);
    // Uniform over the triangle puts the mean at the centroid.
    CHECK(mean.x == doctest::Approx(1.0 / 3.0).epsilon(1e-2));
    CHECK(mean.y == doctest::Approx(1.0 / 3.0).epsilon(1e-2));
    CHECK(mean.z == doctest::Approx(1.0 / 3.0).epsilon(1e-2));
  }
}

TEST_CASE("MonteCarlo: the inverse error function") {
  using smdl::simd::float4;
  using smdl::simd::float8;
  // A Newton refinement through the double-precision `std::erf`, from
  // the float answer, is the reference: the answer is right when the
  // refinement moves it by less than the float itself carries.
  const auto refined{[](float y) {
    double z{double(smdl::erfInverse(y))};
    for (int i = 0; i < 8; i++)
      z -= (std::erf(z) - double(y)) /
           (2.0 / std::sqrt(3.14159265358979323846) * std::exp(-z * z));
    return z;
  }};
  // Fixed-seed draws in [lo, hi), a whole number of eight-wide packs.
  const auto draws{[](float lo, float hi) {
    std::mt19937 rng{20260916};
    std::uniform_real_distribution<float> distr(lo, hi);
    std::vector<float> values(100'000);
    for (float &value : values) value = distr(rng);
    return values;
  }};
  SUBCASE("Both branches hold the relative bound against the refinement") {
    // The branch changes at `-log((1 - y) (1 + y)) = 5`, near y = 0.9966,
    // which is also where the error peaks, and the tail runs to the last
    // float below 1. So every float in [0.99, 1) is visited, and draws
    // cover the rest; the result is exactly odd, so the negative half
    // needs no visit of its own.
    std::vector<float> ys{draws(0.0f, 0.99f)};
    for (float y{0.99f}; y < 1.0f; y = std::nextafter(y, 2.0f)) ys.push_back(y);
    ys.resize((ys.size() + 7) / 8 * 8, 0.5f);
    double worst{};
    for (size_t i = 0; i < ys.size(); i += 8) {
      std::array<float, 8> xs{};
      smdl::simd::erfInverse(float8::load(&ys[i])).store(xs.data());
      for (size_t j = 0; j < 8; j++) {
        const double z{refined(ys[i + j])};
        if (z != 0) worst = std::max(worst, std::abs(xs[j] - z) / std::abs(z));
      }
    }
    CHECK(worst < 5e-7);
  }
  SUBCASE("Every width computes what the scalar function does") {
    // The scalar function is the library's width-one instance, which may
    // be built for another instruction set than this suite is, so this
    // also pins that nothing in the kernel depends on one.
    std::vector<float> ys{draws(-1.0f, 1.0f)};
    ys.insert(ys.begin(), {-1.0f, 1.0f, 0.0f, -0.0f, 0.5f, -0.5f, 0.9966f,
                           std::nextafter(1.0f, 0.0f)});
    std::vector<float> xis{draws(0.0f, 1.0f)};
    xis.insert(xis.begin(), {0x1p-32f, smdl::FLOAT_MIN, smdl::ONE_MINUS_EPS,
                             0.5f, 0.0001f, 0.9999f, 0.25f, 0.75f});
    bool isSame{true};
    for (size_t i = 0; i < ys.size(); i += 8) {
      std::array<float, 8> wide{};
      std::array<float, 8> narrow{};
      smdl::simd::erfInverse(float8::load(&ys[i])).store(wide.data());
      smdl::simd::erfInverse(float4::load(&ys[i])).store(&narrow[0]);
      smdl::simd::erfInverse(float4::load(&ys[i + 4])).store(&narrow[4]);
      for (size_t j = 0; j < 8; j++)
        isSame = isSame && wide[j] == narrow[j] &&
                 wide[j] == smdl::erfInverse(ys[i + j]);
    }
    for (size_t i = 0; i < xis.size(); i += 8) {
      std::array<float, 8> samples{};
      smdl::simd::standardNormalSample(float8::load(&xis[i]))
          .store(samples.data());
      for (size_t j = 0; j < 8; j++)
        isSame = isSame && samples[j] == smdl::standardNormalSample(xis[i + j]);
    }
    CHECK(isSame);
  }
  SUBCASE("The result is exactly odd") {
    const std::vector<float> ys{draws(0.0f, 1.0f)};
    bool isOdd{true};
    for (size_t i = 0; i < ys.size(); i += 8) {
      const float8 y{float8::load(&ys[i])};
      std::array<float, 8> positive{};
      std::array<float, 8> negative{};
      smdl::simd::erfInverse(y).store(positive.data());
      smdl::simd::erfInverse(-y).store(negative.data());
      for (size_t j = 0; j < 8; j++)
        isOdd = isOdd && negative[j] == -positive[j];
    }
    CHECK(isOdd);
  }
  SUBCASE("The result falls by at most 2 ulp from one float to the next") {
    // What keeps stratified samples stratified. The worst fall over every
    // float in [0, 1] is 2 ulp at y = 0.03106, which the window at 0.031
    // reaches, and the walk from 0.99 crosses the branch change and ends
    // on 1 itself.
    const auto worstFall{[](float from, size_t count) {
      std::vector<float> ys;
      for (float y{from}; ys.size() < count && y <= 1.0f;
           y = std::nextafter(y, 2.0f))
        ys.push_back(y);
      ys.resize((ys.size() + 7) / 8 * 8, ys.back());
      std::vector<float> xs(ys.size());
      for (size_t i = 0; i < ys.size(); i += 8)
        smdl::simd::erfInverse(float8::load(&ys[i])).store(&xs[i]);
      double worst{};
      for (size_t i = 1; i < xs.size(); i++) {
        const float ulp{std::nextafter(std::abs(xs[i - 1]), 10.0f) -
                        std::abs(xs[i - 1])};
        worst = std::max(worst, double(xs[i - 1] - xs[i]) / double(ulp));
      }
      return worst;
    }};
    for (const float from : {1e-6f, 0.031f, 0.5f, 0.9f}) {
      CAPTURE(from);
      CHECK(worstFall(from, 65'536) <= 2.0);
    }
    CHECK(worstFall(0.99f, 262'144) <= 2.0);
  }
  SUBCASE("An end of the interval continues the curve") {
    const float last{std::nextafter(1.0f, 0.0f)};
    CHECK(smdl::erfInverse(1.0f) > smdl::erfInverse(last));
    CHECK(smdl::erfInverse(1.0f) < 4.0f);
    CHECK(smdl::erfInverse(-1.0f) == -smdl::erfInverse(1.0f));
    // Every canonical sample below 2^-25 rounds `2 xi - 1` to -1.
    CHECK(smdl::standardNormalSample(0x1p-32f) < -5.0f);
  }
  SUBCASE("A standard normal sample from the tail of the unit interval is "
          "past three sigma") {
    CHECK(smdl::standardNormalSample(0.9999f) > 3.7f);
    CHECK(smdl::standardNormalSample(0.0001f) < -3.7f);
  }
}
