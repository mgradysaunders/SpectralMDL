#include "Fixtures.h"

#include <cmath>

#include "smdl/RenderUtil/Colorimetry.h"

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

// Sharma, Wu, and Dalal's test pairs for CIEDE2000 (Color Research and
// Application 30(1), 2005): two colors in CIELAB and the difference
// they publish, to four places.
struct SharmaPair final {
  smdl::double3 lab0{};
  smdl::double3 lab1{};
  double expected{};
};

const SharmaPair SHARMA_PAIRS[]{
    {{50.0000, 2.6772, -79.7751}, {50.0000, 0.0000, -82.7485}, 2.0425},
    {{50.0000, 3.1571, -77.2803}, {50.0000, 0.0000, -82.7485}, 2.8615},
    {{50.0000, 2.8361, -74.0200}, {50.0000, 0.0000, -82.7485}, 3.4412},
    {{50.0000, -1.3802, -84.2814}, {50.0000, 0.0000, -82.7485}, 1.0000},
    {{50.0000, -1.1848, -84.8006}, {50.0000, 0.0000, -82.7485}, 1.0000},
    {{50.0000, -0.9009, -85.5211}, {50.0000, 0.0000, -82.7485}, 1.0000},
    {{50.0000, 0.0000, 0.0000}, {50.0000, -1.0000, 2.0000}, 2.3669},
    {{50.0000, -1.0000, 2.0000}, {50.0000, 0.0000, 0.0000}, 2.3669},
    {{50.0000, 2.4900, -0.0010}, {50.0000, -2.4900, 0.0009}, 7.1792},
    {{50.0000, 2.4900, -0.0010}, {50.0000, -2.4900, 0.0010}, 7.1792},
    {{50.0000, 2.4900, -0.0010}, {50.0000, -2.4900, 0.0011}, 7.2195},
    {{50.0000, 2.4900, -0.0010}, {50.0000, -2.4900, 0.0012}, 7.2195},
    {{50.0000, -0.0010, 2.4900}, {50.0000, 0.0009, -2.4900}, 4.8045},
    {{50.0000, -0.0010, 2.4900}, {50.0000, 0.0010, -2.4900}, 4.8045},
    {{50.0000, -0.0010, 2.4900}, {50.0000, 0.0011, -2.4900}, 4.7461},
    {{50.0000, 2.5000, 0.0000}, {50.0000, 0.0000, -2.5000}, 4.3065},
    {{50.0000, 2.5000, 0.0000}, {73.0000, 25.0000, -18.0000}, 27.1492},
    {{50.0000, 2.5000, 0.0000}, {61.0000, -5.0000, 29.0000}, 22.8977},
    {{50.0000, 2.5000, 0.0000}, {56.0000, -27.0000, -3.0000}, 31.9030},
    {{50.0000, 2.5000, 0.0000}, {58.0000, 24.0000, 15.0000}, 19.4535},
    {{50.0000, 2.5000, 0.0000}, {50.0000, 3.1736, 0.5854}, 1.0000},
    {{50.0000, 2.5000, 0.0000}, {50.0000, 3.2972, 0.0000}, 1.0000},
    {{50.0000, 2.5000, 0.0000}, {50.0000, 1.8634, 0.5757}, 1.0000},
    {{50.0000, 2.5000, 0.0000}, {50.0000, 3.2592, 0.3350}, 1.0000},
    {{60.2574, -34.0099, 36.2677}, {60.4626, -34.1751, 39.4387}, 1.2644},
    {{63.0109, -31.0961, -5.8663}, {62.8187, -29.7946, -4.0864}, 1.2630},
    {{61.2901, 3.7196, -5.3901}, {61.4292, 2.2480, -4.9620}, 1.8731},
    {{35.0831, -44.1164, 3.7933}, {35.0232, -40.0716, 1.5901}, 1.8645},
    {{22.7233, 20.0904, -46.6940}, {23.0331, 14.9730, -42.5619}, 2.0373},
    {{36.4612, 47.8580, 18.3852}, {36.2715, 50.5065, 21.2231}, 1.4146},
    {{90.8027, -2.0831, 1.4410}, {91.1528, -1.6435, 0.0447}, 1.4441},
    {{90.9257, -0.5406, -0.9208}, {88.6381, -0.8985, -0.7239}, 1.5381},
    {{6.7747, -0.2908, -2.4247}, {5.8714, -0.0985, -2.2286}, 0.6377},
    {{2.0776, 0.0795, -1.1350}, {0.9033, -0.0636, -0.5514}, 0.9082},
};

// Is `m` the identity, to `tolerance`?
[[nodiscard]] bool isIdentity(const smdl::double3x3 &m, double tolerance) {
  for (size_t j = 0; j < 3; j++)
    for (size_t i = 0; i < 3; i++)
      if (!(std::abs(m[j][i] - (i == j ? 1.0 : 0.0)) < tolerance)) return false;
  return true;
}

} // namespace

TEST_CASE("Colorimetry: the observer's fit") {
  SUBCASE("The y-bar peaks at 1 near 555 nm, and is the y of the three") {
    double peak{};
    double peakLambda{};
    for (int half = 760; half <= 1560; half++) {
      const double lambda{0.5 * double(half)};
      CHECK(smdl::wymanY(lambda) == doctest::Approx(smdl::wymanXYZ(lambda).y));
      if (smdl::wymanY(lambda) > peak) {
        peak = smdl::wymanY(lambda);
        peakLambda = lambda;
      }
    }
    CHECK(peak == doctest::Approx(1.0).epsilon(0.01));
    CHECK(peakLambda == doctest::Approx(556.0).epsilon(0.01));
  }
  SUBCASE("The x-bar and z-bar peak where the CIE curves do") {
    CHECK(smdl::wymanXYZ(600.0).x == doctest::Approx(1.06).epsilon(0.01));
    CHECK(smdl::wymanXYZ(445.0).z == doctest::Approx(1.78).epsilon(0.01));
    CHECK(smdl::wymanXYZ(445.0).x == doctest::Approx(0.35).epsilon(0.05));
  }
  SUBCASE("The y-bar integrates to the 107 nm the lit world's lux rest on") {
    CHECK(integral(smdl::wymanY) == doctest::Approx(106.9).epsilon(0.002));
  }
  SUBCASE("An equal-energy white reads neutral: the three integrals agree") {
    const double x{integral([](double l) { return smdl::wymanXYZ(l).x; })};
    const double y{integral([](double l) { return smdl::wymanXYZ(l).y; })};
    const double z{integral([](double l) { return smdl::wymanXYZ(l).z; })};
    CHECK(x == doctest::Approx(y).epsilon(0.005));
    CHECK(z == doctest::Approx(y).epsilon(0.005));
  }
  SUBCASE("Nothing outside the visible") {
    CHECK(smdl::wymanY(300.0) < 1e-6);
    CHECK(smdl::wymanY(830.0) < 1e-6);
    CHECK(smdl::wymanXYZ(300.0).z < 1e-3);
  }
}

TEST_CASE("Colorimetry: the two Gaussian efficiencies the display uses") {
  CHECK(smdl::photopicV(559.0) == doctest::Approx(1.019).epsilon(1e-3));
  CHECK(smdl::scotopicV(503.0) == doctest::Approx(0.992).epsilon(1e-3));
  SUBCASE("The photopic Gaussian tracks the fit to a few percent where it "
          "matters") {
    for (int lambda = 500; lambda <= 620; lambda += 10)
      CHECK(smdl::photopicV(double(lambda)) ==
            doctest::Approx(smdl::wymanY(double(lambda))).epsilon(0.08));
  }
}

TEST_CASE("Colorimetry: the sRGB white and Bradford") {
  const auto white{smdl::linearSRGBWhite()};
  SUBCASE("The builtin's matrix takes its white to (1, 1, 1), and the white "
          "is D65") {
    const auto rgb{smdl::xyzToLinearSRGB() * white};
    CHECK(rgb.x == doctest::Approx(1.0).epsilon(1e-9));
    CHECK(rgb.y == doctest::Approx(1.0).epsilon(1e-9));
    CHECK(rgb.z == doctest::Approx(1.0).epsilon(1e-9));
    CHECK(white.x == doctest::Approx(0.95047).epsilon(1e-4));
    CHECK(white.y == doctest::Approx(1.0).epsilon(1e-5));
    CHECK(white.z == doctest::Approx(1.08883).epsilon(1e-4));
  }
  SUBCASE("From a white to itself is the identity") {
    CHECK(isIdentity(smdl::bradfordAdaptation(white, white), 1e-12));
  }
  SUBCASE("It takes the one white to the other") {
    const auto d50{smdl::double3(0.96422, 1.0, 0.82521)};
    const auto adapted{smdl::bradfordAdaptation(d50, white) * d50};
    CHECK(adapted.x == doctest::Approx(white.x).epsilon(1e-12));
    CHECK(adapted.y == doctest::Approx(white.y).epsilon(1e-12));
    CHECK(adapted.z == doctest::Approx(white.z).epsilon(1e-12));
  }
}

TEST_CASE("Colorimetry: McCamy's temperature") {
  CHECK(smdl::mccamyKelvin(smdl::double2(0.31271, 0.32902)) ==
        doctest::Approx(6504.0).epsilon(2e-4));
  CHECK(smdl::mccamyKelvin(smdl::double2(0.34567, 0.35850)) ==
        doctest::Approx(5003.0).epsilon(2e-4));
  // CIE illuminant A.
  CHECK(smdl::mccamyKelvin(smdl::double2(0.44757, 0.40745)) ==
        doctest::Approx(2856.0).epsilon(1e-3));
}

TEST_CASE("Colorimetry: CIELAB and the color differences") {
  const auto white{smdl::linearSRGBWhite()};
  SUBCASE("The white is L 100 and neutral, black is L 0, and a gray is "
          "neutral") {
    const auto lab{smdl::xyzToLab(white, white)};
    CHECK(lab.x == doctest::Approx(100.0));
    CHECK(std::abs(lab.y) < 1e-9);
    CHECK(std::abs(lab.z) < 1e-9);
    CHECK(std::abs(smdl::xyzToLab(smdl::double3(0.0), white).x) < 1e-12);
    const auto gray{smdl::xyzToLab(0.18 * white, white)};
    CHECK(gray.x == doctest::Approx(49.496).epsilon(1e-4));
    CHECK(std::abs(gray.y) < 1e-9);
    CHECK(std::abs(gray.z) < 1e-9);
  }
  SUBCASE("CIE 1976 is the distance") {
    CHECK(smdl::deltaEab(smdl::double3(50, 0, 0), smdl::double3(53, 4, 0)) ==
          doctest::Approx(5.0));
  }
  SUBCASE("CIEDE2000 reproduces the published pairs, either way round") {
    for (const auto &pair : SHARMA_PAIRS) {
      CHECK(std::abs(smdl::deltaE00(pair.lab0, pair.lab1) - pair.expected) <
            1e-4);
      CHECK(std::abs(smdl::deltaE00(pair.lab1, pair.lab0) - pair.expected) <
            1e-4);
    }
  }
}
