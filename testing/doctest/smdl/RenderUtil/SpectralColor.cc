#include "doctest.h"

#include <limits>
#include <utility>

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
