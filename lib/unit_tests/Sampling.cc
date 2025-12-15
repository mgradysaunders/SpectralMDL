#include "doctest.h"

#include <array>
#include <fstream>
#include <random>

#include "smdl/Support/Sampling.h"

TEST_CASE("Sampling") {
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
