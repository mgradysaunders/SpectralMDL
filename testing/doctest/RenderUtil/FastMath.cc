#include "Fixtures.h"

#include <cmath>
#include <limits>
#include <random>

#include "smdl/RenderUtil/FastMath.h"

namespace {

// Visit a uniform grid over [lo, hi] and as many fixed-seed random points
// in the same interval.
template <typename T, typename F> void sweep(T lo, T hi, F &&visit) {
  constexpr int N = 250'000;
  for (int i = 0; i <= N; i++) visit(lo + (hi - lo) * (T(i) / T(N)));
  std::mt19937 rng{20260902};
  std::uniform_real_distribution<T> distr(lo, hi);
  for (int i = 0; i < N; i++) visit(distr(rng));
}

} // namespace

TEST_CASE("FastMath: the error bounds of the approximations") {
  SUBCASE("The exact points are exact") {
    CHECK(smdl::fastExp(0.0f) == 1.0f);
    CHECK(smdl::fastExp(0.0) == 1.0);
    CHECK(smdl::fastLog(1.0f) == 0.0f);
    CHECK(smdl::fastLog(1.0) == 0.0);
    CHECK(smdl::fastAcos(1.0f) == 0.0f);
    CHECK(smdl::fastAcos(-1.0f) == 3.14159265f);
    CHECK(smdl::fastAcos(0.0f) == doctest::Approx(1.57079633).epsilon(1e-6));
  }
  SUBCASE("fastExp holds its relative bound over the normal float range") {
    double worst{};
    sweep(-87.3f, 88.3f, [&](float x) {
      const double ref{std::exp(double(x))};
      worst = std::max(worst, std::abs(double(smdl::fastExp(x)) - ref) / ref);
    });
    CHECK(worst < 3e-7);
  }
  SUBCASE("fastExp holds its relative bound over the normal double range") {
    long double worst{};
    sweep(-708.3, 709.4, [&](double x) {
      const long double ref{std::exp(static_cast<long double>(x))};
      worst = std::max(
          worst,
          std::abs(static_cast<long double>(smdl::fastExp(x)) - ref) / ref);
    });
    CHECK(worst < 1e-8);
  }
  SUBCASE("fastExp saturates at the extremes rather than returning a NaN") {
    CHECK(smdl::fastExp(-87.34f) == 0.0f);
    CHECK(smdl::fastExp(-200.0f) == 0.0f);
    CHECK(smdl::fastExp(-87.0f) > 0.0f);
    CHECK(std::isfinite(smdl::fastExp(88.0f)));
    CHECK(std::isinf(smdl::fastExp(88.4f)));
    CHECK(std::isinf(smdl::fastExp(200.0f)));
    CHECK(smdl::fastExp(-708.4) == 0.0);
    CHECK(smdl::fastExp(-1000.0) == 0.0);
    CHECK(smdl::fastExp(-708.0) > 0.0);
    CHECK(std::isfinite(smdl::fastExp(709.0)));
    CHECK(std::isinf(smdl::fastExp(709.5)));
    CHECK(std::isinf(smdl::fastExp(1000.0)));
    bool isSane{true};
    sweep(-1e4f, 1e4f, [&](float x) {
      const float y{smdl::fastExp(x)};
      isSane = isSane && !std::isnan(y) && y >= 0.0f;
    });
    sweep(-1e4, 1e4, [&](double x) {
      const double y{smdl::fastExp(x)};
      isSane = isSane && !std::isnan(y) && y >= 0.0;
    });
    CHECK(isSane);
  }
  SUBCASE("fastLog holds its bound over every float binade") {
    // The bound is relative to ln(x) away from 1 and absolute near it.
    double worst{};
    sweep(-126.0f, 127.99f, [&](float u) {
      const float x{std::exp2(u)};
      const double ref{std::log(double(x))};
      const double err{std::abs(double(smdl::fastLog(x)) - ref)};
      worst = std::max(worst, err / std::max(1.0, std::abs(ref)));
    });
    CHECK(worst < 3e-7);
  }
  SUBCASE("fastLog holds its bound over every double binade") {
    long double worst{};
    sweep(-1022.0, 1023.99, [&](double u) {
      const double x{std::exp2(u)};
      const long double ref{std::log(static_cast<long double>(x))};
      const long double err{
          std::abs(static_cast<long double>(smdl::fastLog(x)) - ref)};
      worst = std::max(worst, err / std::max(1.0L, std::abs(ref)));
    });
    CHECK(worst < 2e-11);
  }
  SUBCASE("fastAcos holds its absolute bound in radians") {
    double worst{};
    sweep(-1.0f, 1.0f, [&](float x) {
      const double ref{std::acos(double(x))};
      worst = std::max(worst, std::abs(double(smdl::fastAcos(x)) - ref));
    });
    CHECK(worst < 5e-7);
  }
  SUBCASE("fastAcos is continuous through zero") {
    const float tiny{std::numeric_limits<float>::denorm_min()};
    CHECK(std::abs(smdl::fastAcos(-tiny) - smdl::fastAcos(+tiny)) < 1e-6f);
    CHECK(std::abs(smdl::fastAcos(-1e-7f) - smdl::fastAcos(+1e-7f)) < 1e-6f);
  }
  SUBCASE("fastAtan2 holds its absolute bound around the circle") {
    double worst{};
    // Over angles rather than over the plane, so that every eighth of
    // the circle the reflections stitch together is swept evenly, and
    // over several magnitudes, because the result is scale invariant
    // only up to the rounding of the one division.
    for (const double radius : {1.0e-4, 1.0, 3.7, 1.0e5}) {
      sweep(-3.14159265358979, 3.14159265358979, [&](double angle) {
        const float y{float(radius * std::sin(angle))};
        const float x{float(radius * std::cos(angle))};
        const double ref{std::atan2(double(y), double(x))};
        double err{std::abs(double(smdl::fastAtan2(y, x)) - ref)};
        if (err > 3.14159265358979)
          err = 6.28318530717959 - err; // across the branch cut
        worst = std::max(worst, err);
      });
    }
    CHECK(worst < 4e-7);
  }
  SUBCASE("fastAtan2 agrees on the axes and at the origin") {
    CHECK(smdl::fastAtan2(0.0f, 0.0f) == 0.0f);
    CHECK(smdl::fastAtan2(0.0f, 1.0f) == 0.0f);
    CHECK(smdl::fastAtan2(0.0f, -1.0f) == doctest::Approx(3.14159265));
    CHECK(smdl::fastAtan2(1.0f, 0.0f) == doctest::Approx(1.57079633));
    CHECK(smdl::fastAtan2(-1.0f, 0.0f) == doctest::Approx(-1.57079633));
    // The one place it parts with 'std::atan2', which reads the sign of
    // a zero and answers -pi here.
    CHECK(smdl::fastAtan2(-0.0f, -1.0f) == doctest::Approx(3.14159265));
  }
}
