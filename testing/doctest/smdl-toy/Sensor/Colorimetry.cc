#include "Fixtures.h"

#include <cmath>

#include "Sensor/Colorimetry.h"

// The observer is a fit, and what matters is that it is the fit it claims
// to be: the peaks where the CIE curves peak, the integral of y-bar that
// every lux rests on, and an equal-energy white that reads neutral, which
// is what makes the three integrals agree.

namespace {

// The integral of `f` at 1 nm from 300 to 830 nm.
template <typename F> [[nodiscard]] double integral(F &&f) {
  double total{};
  for (int lambda = 300; lambda <= 830; lambda++) total += f(double(lambda));
  return total;
}

} // namespace

TEST_CASE("Colorimetry: the observer's fit") {
  SUBCASE("The y-bar peaks at 1 near 555 nm, and is the y of the three") {
    double peak{};
    double peakLambda{};
    for (int half = 760; half <= 1560; half++) {
      const double lambda{0.5 * double(half)};
      CHECK(wymanY(lambda) == doctest::Approx(wymanXYZ(lambda).y));
      if (wymanY(lambda) > peak) {
        peak = wymanY(lambda);
        peakLambda = lambda;
      }
    }
    CHECK(peak == doctest::Approx(1.0).epsilon(0.01));
    CHECK(peakLambda == doctest::Approx(556.0).epsilon(0.01));
  }
  SUBCASE("The x-bar and z-bar peak where the CIE curves do") {
    CHECK(wymanXYZ(600.0).x == doctest::Approx(1.06).epsilon(0.01));
    CHECK(wymanXYZ(445.0).z == doctest::Approx(1.78).epsilon(0.01));
    CHECK(wymanXYZ(445.0).x == doctest::Approx(0.35).epsilon(0.05));
  }
  SUBCASE("The y-bar integrates to the 107 nm the lit world's lux rest on") {
    CHECK(integral(wymanY) == doctest::Approx(106.9).epsilon(0.002));
  }
  SUBCASE("An equal-energy white reads neutral: the three integrals agree") {
    const double x{integral([](double l) { return wymanXYZ(l).x; })};
    const double y{integral([](double l) { return wymanXYZ(l).y; })};
    const double z{integral([](double l) { return wymanXYZ(l).z; })};
    CHECK(x == doctest::Approx(y).epsilon(0.005));
    CHECK(z == doctest::Approx(y).epsilon(0.005));
  }
  SUBCASE("Nothing outside the visible") {
    CHECK(wymanY(300.0) < 1e-6);
    CHECK(wymanY(830.0) < 1e-6);
    CHECK(wymanXYZ(300.0).z < 1e-3);
  }
}

TEST_CASE("Colorimetry: the two Gaussian efficiencies the display uses") {
  CHECK(photopicV(559.0) == doctest::Approx(1.019).epsilon(1e-3));
  CHECK(scotopicV(503.0) == doctest::Approx(0.992).epsilon(1e-3));
  SUBCASE("The photopic Gaussian tracks the fit to a few percent where it "
          "matters") {
    for (int lambda = 500; lambda <= 620; lambda += 10)
      CHECK(photopicV(double(lambda)) ==
            doctest::Approx(wymanY(double(lambda))).epsilon(0.08));
  }
}
