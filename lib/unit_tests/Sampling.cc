#include "doctest.h"

#include <array>
#include <fstream>
#include <random>

#include "smdl/Support/Sampling.h"

TEST_CASE("Sampling") {
  SUBCASE("Distribution1D") {
    auto distr =
        smdl::Distribution1D(std::vector<float>{1.0f, 2.0f, 3.0f, 1.0f});
    CHECK(distr.index_pmf(0) == doctest::Approx(1.0 / 7.0));
    CHECK(distr.index_pmf(1) == doctest::Approx(2.0 / 7.0));
    CHECK(distr.index_pmf(2) == doctest::Approx(3.0 / 7.0));
    CHECK(distr.index_pmf(3) == doctest::Approx(1.0 / 7.0));
    CHECK(distr.index_cmf(0) == doctest::Approx(0.0 / 7.0));
    CHECK(distr.index_cmf(1) == doctest::Approx(1.0 / 7.0));
    CHECK(distr.index_cmf(2) == doctest::Approx(3.0 / 7.0));
    CHECK(distr.index_cmf(3) == doctest::Approx(6.0 / 7.0));
    CHECK(distr.index_cmf(4) == doctest::Approx(7.0 / 7.0));
    std::mt19937 prng{};
    std::array<int, 4> histogram{};
    for (int iter = 0; iter < 100'000; iter++) {
      auto i = distr.index_sample(smdl::generate_canonical(prng));
      histogram[i]++;
    }
    CHECK(histogram[0] * 1e-5 ==
          doctest::Approx(distr.index_pmf(0)).epsilon(1e-3));
    CHECK(histogram[1] * 1e-5 ==
          doctest::Approx(distr.index_pmf(1)).epsilon(1e-3));
    CHECK(histogram[2] * 1e-5 ==
          doctest::Approx(distr.index_pmf(2)).epsilon(1e-3));
    CHECK(histogram[3] * 1e-5 ==
          doctest::Approx(distr.index_pmf(3)).epsilon(1e-3));
  }
  SUBCASE("Distribution2D") {
    auto distr =
        smdl::Distribution2D(4, 2,
                             std::vector<float>{1.0f, 2.0f, 3.0f, 4.0f, //
                                                6.0f, 3.0f, 1.0f, 2.0f});
    CHECK(distr.pixel_pmf(smdl::int2(0, 0)) == doctest::Approx(1.0 / 22.0));
    CHECK(distr.pixel_pmf(smdl::int2(1, 0)) == doctest::Approx(2.0 / 22.0));
    CHECK(distr.pixel_pmf(smdl::int2(2, 0)) == doctest::Approx(3.0 / 22.0));
    CHECK(distr.pixel_pmf(smdl::int2(3, 0)) == doctest::Approx(4.0 / 22.0));
    CHECK(distr.pixel_pmf(smdl::int2(0, 1)) == doctest::Approx(6.0 / 22.0));
    CHECK(distr.pixel_pmf(smdl::int2(1, 1)) == doctest::Approx(3.0 / 22.0));
    CHECK(distr.pixel_pmf(smdl::int2(2, 1)) == doctest::Approx(1.0 / 22.0));
    CHECK(distr.pixel_pmf(smdl::int2(3, 1)) == doctest::Approx(2.0 / 22.0));
    std::mt19937 prng{};
    std::array<std::array<int, 4>, 2> histogram{};
    for (int iter = 0; iter < 1'000'000; iter++) {
      auto i = distr.pixel_sample(smdl::generate_canonical2(prng));
      histogram[i.y][i.x]++;
    }
    for (int iY = 0; iY < 2; iY++) {
      for (int iX = 0; iX < 4; iX++) {
        CHECK(
            histogram[iY][iX] * 1e-6 ==
            doctest::Approx(distr.pixel_pmf(smdl::int2(iX, iY))).epsilon(1e-3));
      }
    }
  }
}
