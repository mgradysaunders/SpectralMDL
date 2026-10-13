#include "Fixtures.h"

#include <array>
#include <random>
#include <vector>

#include "smdl/RenderUtil/Distribution2D.h"
#include "smdl/RenderUtil/MonteCarlo.h"

TEST_CASE("Distribution2D: draws in proportion to its values") {
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
    smdl::int2 i = distr.pixelDraw(smdl::generateCanonical2(prng));
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
