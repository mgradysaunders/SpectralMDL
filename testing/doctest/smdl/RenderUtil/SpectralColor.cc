#include "doctest.h"

#include <cmath>
#include <initializer_list>
#include <limits>
#include <utility>
#include <vector>

#include "smdl/RenderUtil/SpectralColor.h"

using smdl::SpectralColor;

TEST_CASE("SpectralColor construction and storage") {
  SUBCASE("Default is empty") {
    SpectralColor c{};
    CHECK(c.size() == 0);
  }
  SUBCASE("Sized fill, inline and heap") {
    for (size_t n : {size_t(1), size_t(16), size_t(17), size_t(421)}) {
      CAPTURE(n);
      SpectralColor c{n, 2.5f};
      REQUIRE(c.size() == n);
      for (size_t i = 0; i < n; i++) CHECK(c[i] == 2.5f);
    }
  }
  SUBCASE("Span copies exactly its size") {
    const float values[5]{1, 2, 3, 4, 5};
    SpectralColor c{smdl::Span<const float>(values, 5)};
    REQUIRE(c.size() == 5);
    for (size_t i = 0; i < 5; i++) CHECK(c[i] == values[i]);
  }
}

TEST_CASE("SpectralColor copy and move") {
  for (size_t n : {size_t(3), size_t(16), size_t(40)}) {
    CAPTURE(n);
    SpectralColor a{n, 1.0f};
    for (size_t i = 0; i < n; i++) a[i] = float(i);
    SUBCASE("Copy construct") {
      SpectralColor b{a};
      REQUIRE(b.size() == n);
      for (size_t i = 0; i < n; i++) CHECK(b[i] == float(i));
      b[0] = 99.0f; // No aliasing.
      CHECK(a[0] == 0.0f);
    }
    SUBCASE("Copy assign over different sizes") {
      SpectralColor b{n + 7, 0.0f};
      b = a;
      REQUIRE(b.size() == n);
      for (size_t i = 0; i < n; i++) CHECK(b[i] == float(i));
      SpectralColor c{};
      c = a;
      REQUIRE(c.size() == n);
      for (size_t i = 0; i < n; i++) CHECK(c[i] == float(i));
    }
    SUBCASE("Move construct empties the source") {
      SpectralColor b{std::move(a)};
      REQUIRE(b.size() == n);
      for (size_t i = 0; i < n; i++) CHECK(b[i] == float(i));
      CHECK(a.size() == 0);
    }
    SUBCASE("Move assign empties the source") {
      SpectralColor b{2, 0.0f};
      b = std::move(a);
      REQUIRE(b.size() == n);
      for (size_t i = 0; i < n; i++) CHECK(b[i] == float(i));
      CHECK(a.size() == 0);
    }
  }
}

TEST_CASE("SpectralColor arithmetic") {
  // Inline (16 and under) and heap (over 16) storage run different
  // loops, so both sizes are exercised.
  for (size_t n : {size_t(16), size_t(21)}) {
    CAPTURE(n);
    SpectralColor a{n, 2.0f};
    SpectralColor b{n, 3.0f};
    auto expectAll{[&](const SpectralColor &c, float value) {
      REQUIRE(c.size() == n);
      for (size_t i = 0; i < n; i++) CHECK(c[i] == value);
    }};
    expectAll(a + b, 5.0f);
    expectAll(b - a, 1.0f);
    expectAll(a * b, 6.0f);
    expectAll(b / a, 1.5f);
    expectAll(a + 1.0f, 3.0f);
    expectAll(a - 1.0f, 1.0f);
    expectAll(a * 2.0f, 4.0f);
    expectAll(a / 2.0f, 1.0f);
    expectAll(1.0f + a, 3.0f);
    expectAll(7.0f - a, 5.0f);
    expectAll(2.0f * a, 4.0f);
    expectAll(6.0f / a, 3.0f);
    expectAll(-a, -2.0f);
    SpectralColor c{a};
    c += b;
    expectAll(c, 5.0f);
    c -= a;
    expectAll(c, 3.0f);
    c *= a;
    expectAll(c, 6.0f);
    c /= b;
    expectAll(c, 2.0f);
  }
}

TEST_CASE("SpectralColor reductions and predicates") {
  SpectralColor c{4, 0.0f};
  c[0] = 1.0f, c[1] = 2.0f, c[2] = 3.0f, c[3] = 6.0f;
  CHECK(c.average() == 3.0f);
  CHECK(c.maxComponent() == 6.0f);
  CHECK(c.minComponent() == 1.0f);
  CHECK(!c.isAllZero());
  CHECK(SpectralColor(7, 0.0f).isAllZero());
  CHECK(!c.isAnyNonFinite());
  c[2] = std::numeric_limits<float>::infinity();
  CHECK(c.isAnyInf());
  CHECK(c.isAnyNonFinite());
  c.setNonFiniteToZero();
  CHECK(c[2] == 0.0f);
  c[1] = -2.0f;
  c.setNonPositiveToZero();
  CHECK(c[1] == 0.0f);
  CHECK(c[3] == 6.0f);
}

// A color of `values` whose inline lanes past the size hold `past`, the
// garbage the fixed-length arithmetic is allowed to leave there: the
// span constructor leaves those lanes zero and a fill sets every lane,
// so dividing, multiplying and subtracting the two plants whatever a
// reduction has to ignore.
static SpectralColor withLanesPastSize(std::initializer_list<float> values,
                                       float past) {
  const size_t n{values.size()};
  const std::vector<float> ones(n, 1.0f);
  const SpectralColor within{smdl::Span<const float>(ones.data(), n)};
  const SpectralColor given{smdl::Span<const float>(values.begin(), n)};
  if (std::isnan(past)) {
    SpectralColor result{given};
    result /= within; // 0 / 0 past the size
    return result;
  }
  SpectralColor result{n, std::isinf(past) ? 1.0f : past};
  if (std::isinf(past)) {
    result /= within; // 1 / 0 past the size
    result -= within;
  } else {
    result -= result * within;
  }
  result += given;
  return result;
}

TEST_CASE("SpectralColor reductions ignore the inline lanes past the size") {
  const float nan{std::numeric_limits<float>::quiet_NaN()};
  const float inf{std::numeric_limits<float>::infinity()};
  const auto same{[](float a, float b) {
    return (std::isnan(a) && std::isnan(b)) || a == b;
  }};
  for (float past : {nan, inf, 1e30f, -1e30f}) {
    CAPTURE(past);
    const SpectralColor c{
        withLanesPastSize({1.0f, -2.0f, 3.0f, 6.0f, 0.5f}, past)};
    REQUIRE(c.size() == 5);
    for (size_t i = 5; i < SpectralColor::INLINE_CAPACITY; i++)
      REQUIRE(same(c.data()[i], past));
    CHECK(c.average() == 8.5f / 5.0f);
    CHECK(c.maxComponent() == 6.0f);
    CHECK(c.minComponent() == -2.0f);
    CHECK(!c.isAllZero());
    CHECK(!c.isAnyInf());
    CHECK(!c.isAnyNan());
    CHECK(!c.isAnyNonFinite());
    const SpectralColor z{withLanesPastSize({0.0f, 0.0f, 0.0f}, past)};
    CHECK(z.isAllZero());
    CHECK(z.isAllZero(1e-3f));
    CHECK(z.average() == 0.0f);
    // A non-finite lane within the size still shows through the mask,
    // and the maximum keeps the sequential semantics of `std::max`: a
    // NaN in the first lane sticks, one anywhere else is skipped.
    SpectralColor d{c};
    d[1] = inf;
    CHECK(d.isAnyInf());
    CHECK(!d.isAnyNan());
    CHECK(d.isAnyNonFinite());
    CHECK(d.maxComponent() == inf);
    d[1] = nan;
    CHECK(d.isAnyNan());
    CHECK(d.isAnyNonFinite());
    CHECK(d.maxComponent() == 6.0f);
    CHECK(d.minComponent() == 0.5f);
    d.setNonFiniteToZero();
    CHECK(d[1] == 0.0f);
    CHECK(d[3] == 6.0f);
    CHECK(!d.isAnyNonFinite());
    d[0] = nan;
    CHECK(std::isnan(d.maxComponent()));
    CHECK(std::isnan(d.minComponent()));
    SpectralColor e{c};
    e.setNonPositiveToZero();
    CHECK(e[0] == 1.0f);
    CHECK(e[1] == 0.0f);
    CHECK(e[2] == 3.0f);
  }
  SUBCASE("The average keeps the sequential summation order") {
    const std::initializer_list<float> values{1e8f,  1.0f, -1e8f, 1.0f,
                                              0.25f, 3.0f, -7.0f};
    const SpectralColor c{withLanesPastSize(values, nan)};
    float sum{};
    for (float v : values) sum += v;
    CHECK(c.average() == sum / 7.0f);
  }
  SUBCASE("A full inline buffer and a heap buffer") {
    SpectralColor f{16, 2.0f};
    f[3] = -1.0f;
    f[15] = 9.0f;
    CHECK(f.maxComponent() == 9.0f);
    CHECK(f.minComponent() == -1.0f);
    CHECK(f.average() == 36.0f / 16.0f);
    CHECK(!f.isAllZero());
    SpectralColor h{17, 2.0f};
    h[16] = 9.0f;
    CHECK(h.maxComponent() == 9.0f);
    CHECK(h.minComponent() == 2.0f);
    CHECK(h.average() == 41.0f / 17.0f);
    CHECK(!h.isAnyNonFinite());
    h[16] = inf;
    CHECK(h.isAnyInf());
    CHECK(h.isAnyNonFinite());
    h.setNonFiniteToZero();
    CHECK(h[16] == 0.0f);
  }
}
