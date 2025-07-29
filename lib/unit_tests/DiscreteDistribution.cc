#include "doctest.h"

#include <array>
#include <random>

#include "smdl/Support/DiscreteDistribution.h"

TEST_CASE("DiscreteDistribution") {
  smdl::DiscreteDistribution distr = std::vector<double>{1.0, 2.0, 3.0, 1.0};
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
    float u{std::generate_canonical<float, 32>(prng)};
    auto [i, pmf] = distr.index_sample(u);
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
