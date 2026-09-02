#include "doctest.h"

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

TEST_CASE("FastMath") {
  SUBCASE("exact points") {
    CHECK(smdl::fastExp(0.0f) == 1.0f);
    CHECK(smdl::fastExp(0.0) == 1.0);
    CHECK(smdl::fastLog(1.0f) == 0.0f);
    CHECK(smdl::fastLog(1.0) == 0.0);
    CHECK(smdl::fastAcos(1.0f) == 0.0f);
    CHECK(smdl::fastAcos(-1.0f) == 3.14159265f);
    CHECK(smdl::fastAcos(0.0f) == doctest::Approx(1.57079633).epsilon(1e-6));
  }
  SUBCASE("exp float, relative error over the normal range") {
    double worst{};
    sweep(-87.3f, 88.3f, [&](float x) {
      const double ref{std::exp(double(x))};
      worst = std::max(worst, std::abs(double(smdl::fastExp(x)) - ref) / ref);
    });
    CHECK(worst < 3e-7);
  }
  SUBCASE("exp double, relative error over the normal range") {
    long double worst{};
    sweep(-708.3, 709.4, [&](double x) {
      const long double ref{std::exp(static_cast<long double>(x))};
      worst = std::max(
          worst,
          std::abs(static_cast<long double>(smdl::fastExp(x)) - ref) / ref);
    });
    CHECK(worst < 1e-8);
  }
  SUBCASE("exp saturation, never a NaN") {
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
    bool sane{true};
    sweep(-1e4f, 1e4f, [&](float x) {
      const float y{smdl::fastExp(x)};
      sane = sane && !std::isnan(y) && y >= 0.0f;
    });
    sweep(-1e4, 1e4, [&](double x) {
      const double y{smdl::fastExp(x)};
      sane = sane && !std::isnan(y) && y >= 0.0;
    });
    CHECK(sane);
  }
  SUBCASE("log float, over every binade") {
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
  SUBCASE("log double, over every binade") {
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
  SUBCASE("log float, denormal arguments") {
    double worst{};
    const float tiny{std::numeric_limits<float>::denorm_min()};
    sweep(1.0f, 8388607.0f, [&](float k) {
      const float x{std::floor(k) * tiny};
      const double ref{std::log(double(x))};
      const double err{std::abs(double(smdl::fastLog(x)) - ref)};
      worst = std::max(worst, err / std::max(1.0, std::abs(ref)));
    });
    CHECK(worst < 3e-7);
  }
  SUBCASE("log double, denormal arguments") {
    long double worst{};
    sweep(-1074.0, -1022.01, [&](double u) {
      const double x{std::exp2(u)};
      const long double ref{std::log(static_cast<long double>(x))};
      const long double err{
          std::abs(static_cast<long double>(smdl::fastLog(x)) - ref)};
      worst = std::max(worst, err / std::max(1.0L, std::abs(ref)));
    });
    CHECK(worst < 2e-11);
  }
  SUBCASE("acos, absolute error in radians") {
    double worst{};
    sweep(-1.0f, 1.0f, [&](float x) {
      const double ref{std::acos(double(x))};
      worst = std::max(worst, std::abs(double(smdl::fastAcos(x)) - ref));
    });
    CHECK(worst < 5e-7);
  }
  SUBCASE("acos, continuous through zero") {
    const float tiny{std::numeric_limits<float>::denorm_min()};
    CHECK(std::abs(smdl::fastAcos(-tiny) - smdl::fastAcos(+tiny)) < 1e-6f);
    CHECK(std::abs(smdl::fastAcos(-1e-7f) - smdl::fastAcos(+1e-7f)) < 1e-6f);
  }
}
