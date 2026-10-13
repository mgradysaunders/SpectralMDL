#include "Fixtures.h"

#include <array>
#include <random>
#include <vector>

#include "smdl/RenderUtil/Distribution1D.h"
#include "smdl/RenderUtil/MonteCarlo.h"

TEST_CASE("Distribution1D: the piecewise-constant distribution") {
  SUBCASE("Draws in proportion to its values") {
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
      int i = distr.indexDraw(smdl::generateCanonical(prng));
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
  SUBCASE("Never draws an index with no probability, even at the bottom "
          "of the range") {
    smdl::Distribution1D distr =
        smdl::Distribution1D(std::vector<float>{0.0f, 0.0f, 1.0f, 2.0f});
    float xiRemap{-1.0f};
    CHECK(distr.indexDraw(0.0f, &xiRemap) == 2);
    CHECK(xiRemap >= 0.0f);
    CHECK(distr.indexDraw(1e-30f) == 2);
    CHECK(distr.indexDraw(0.5f) == 3);
    CHECK(distr.indexDraw(1.0f) == 3);
  }
  SUBCASE("With all-zero values has no mass anywhere") {
    smdl::Distribution1D distr =
        smdl::Distribution1D(std::vector<float>{0.0f, 0.0f, 0.0f});
    CHECK(distr.indexPMF(0) == 0.0f);
    CHECK(distr.indexPMF(1) == 0.0f);
    CHECK(distr.indexPMF(2) == 0.0f);
    CHECK(distr.unnormalizedSum() == 0.0f);
  }
}
