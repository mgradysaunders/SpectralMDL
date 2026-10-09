#include "Fixtures.h"

#include <algorithm>
#include <array>
#include <cmath>
#include <random>
#include <vector>

#include "smdl/Support/SIMD.h"

using namespace smdl::simd;

namespace {
// The lanes the tests run on, chosen so that the two halves of a
// comparison land on both sides of it.
constexpr std::array<float, 8> A{1, 2, 3, 4, 5, 6, 7, 8};
constexpr std::array<float, 8> B{8, 7, 6, 5, 4, 3, 2, 1};

template <typename Pack> std::array<float, 8> spill(const Pack &pack) {
  std::array<float, 8> out{};
  pack.store(out.data());
  return out;
}

// A uniform grid over [lo, hi] and as many fixed-seed random points in the
// same interval, padded with `hi` to a whole number of eight-wide packs.
std::vector<float> sweep(float lo, float hi) {
  constexpr int N{250'000};
  std::vector<float> points;
  for (int i = 0; i <= N; i++)
    points.push_back(lo + (hi - lo) * (float(i) / float(N)));
  std::mt19937 rng{20260902};
  std::uniform_real_distribution<float> distr(lo, hi);
  for (int i = 0; i < N; i++) points.push_back(distr(rng));
  while (points.size() % 8 != 0) points.push_back(hi);
  return points;
}

// The worst error of `log` over `points`, eight lanes at a time, relative
// to the larger of 1 and the magnitude of the logarithm, which makes it
// absolute near 1.
double worstLogError(const std::vector<float> &points) {
  double worst{};
  for (size_t i = 0; i < points.size(); i += 8) {
    const std::array<float, 8> values{spill(log(float8::load(&points[i])))};
    for (size_t j = 0; j < 8; j++) {
      const double ref{std::log(double(points[i + j]))};
      worst = std::max(worst, std::abs(double(values[j]) - ref) /
                                  std::max(1.0, std::abs(ref)));
    }
  }
  return worst;
}
} // namespace

TEST_CASE("SIMD: the element-wise operators") {
  const float8 a{float8::load(A.data())};
  const float8 b{float8::load(B.data())};
  SUBCASE("Arithmetic runs lane by lane") {
    const std::array<float, 8> sum{spill(a + b)};
    const std::array<float, 8> product{spill(a * b)};
    const std::array<float, 8> difference{spill(a - b)};
    const std::array<float, 8> quotient{spill(a / b)};
    const std::array<float, 8> negated{spill(-a)};
    for (size_t i = 0; i < 8; i++) {
      CHECK(sum[i] == A[i] + B[i]);
      CHECK(product[i] == A[i] * B[i]);
      CHECK(difference[i] == A[i] - B[i]);
      CHECK(quotient[i] == A[i] / B[i]);
      CHECK(negated[i] == -A[i]);
    }
  }
  SUBCASE("A broadcast fills every lane") {
    const std::array<float, 8> values{spill(float8(2.5f))};
    for (size_t i = 0; i < 8; i++) CHECK(values[i] == 2.5f);
  }
  SUBCASE("The square root is exact on exact squares") {
    const std::array<float, 8> squares{1, 4, 9, 16, 25, 36, 49, 64};
    const float8 pack{float8::load(squares.data())};
    const std::array<float, 8> roots{spill(sqrt(pack))};
    for (size_t i = 0; i < 8; i++) CHECK(roots[i] == std::sqrt(squares[i]));
  }
  SUBCASE("Minimum, maximum and magnitude") {
    const std::array<float, 8> low{spill(min(a, b))};
    const std::array<float, 8> high{spill(max(a, b))};
    const std::array<float, 8> magnitude{spill(abs(-a))};
    for (size_t i = 0; i < 8; i++) {
      CHECK(low[i] == std::min(A[i], B[i]));
      CHECK(high[i] == std::max(A[i], B[i]));
      CHECK(magnitude[i] == A[i]);
    }
  }
}

TEST_CASE("SIMD: the logarithm") {
  SUBCASE("One maps to exactly zero") {
    const std::array<float, 8> values{spill(log(float8(1.0f)))};
    for (size_t i = 0; i < 8; i++) CHECK(values[i] == 0.0f);
  }
  SUBCASE("The relative bound holds over every float binade") {
    std::vector<float> points{sweep(-126.0f, 127.99f)};
    for (float &point : points) point = std::exp2(point);
    CHECK(worstLogError(points) < 3e-7);
  }
  SUBCASE("The absolute bound holds within [0.5, 2]") {
    CHECK(worstLogError(sweep(0.5f, 2.0f)) < 1e-7);
  }
  SUBCASE("Every width computes the same lanes") {
    // Bits and exponents are where the widths could part ways: the
    // integer lanes and their conversion are the only work here that is
    // not ordinary float arithmetic.
    std::vector<float> points{sweep(-126.0f, 127.99f)};
    points.resize(4096);
    for (float &point : points) point = std::exp2(point);
    bool isSame{true};
    for (size_t i = 0; i < points.size(); i += 8) {
      const std::array<float, 8> wide{spill(log(float8::load(&points[i])))};
      std::array<float, 8> narrow{};
      log(float4::load(&points[i])).store(&narrow[0]);
      log(float4::load(&points[i + 4])).store(&narrow[4]);
      for (size_t j = 0; j < 8; j++) {
        std::array<float, 1> single{};
        log(float1::load(&points[i + j])).store(single.data());
        isSame = isSame && wide[j] == narrow[j] && wide[j] == single[0];
      }
    }
    CHECK(isSame);
  }
}

TEST_CASE("SIMD: comparisons, masks and select") {
  const float8 a{float8::load(A.data())};
  const float8 b{float8::load(B.data())};
  SUBCASE("A comparison marks the lanes it holds on") {
    const Mask<float, 8> mask{a < b};
    for (size_t i = 0; i < 8; i++) CHECK(mask[i] == (A[i] < B[i]));
  }
  SUBCASE("Select takes each lane from the side its mask names") {
    const std::array<float, 8> picked{spill(select(a < b, a, b))};
    for (size_t i = 0; i < 8; i++)
      CHECK(picked[i] == (A[i] < B[i] ? A[i] : B[i]));
  }
  SUBCASE("Lane-wise logic composes") {
    const Mask<float, 8> both{(a < b) & (a > float8(2.0f))};
    for (size_t i = 0; i < 8; i++) CHECK(both[i] == (A[i] < B[i] && A[i] > 2));
    const Mask<float, 8> either{(a < b) | (a > float8(7.0f))};
    for (size_t i = 0; i < 8; i++)
      CHECK(either[i] == (A[i] < B[i] || A[i] > 7));
    const Mask<float, 8> negated{~(a < b)};
    for (size_t i = 0; i < 8; i++) CHECK(negated[i] == !(A[i] < B[i]));
    const Mask<float, 8> differing{(a < b) ^ (a < float8(4.0f))};
    for (size_t i = 0; i < 8; i++)
      CHECK(differing[i] == ((A[i] < B[i]) != (A[i] < 4)));
  }
  SUBCASE("anyTrue and allTrue are what a masked loop stops on") {
    CHECK(anyTrue(a < b));
    CHECK_FALSE(allTrue(a < b));
    CHECK(allTrue(a < float8(9.0f)));
    CHECK_FALSE(anyTrue(a < float8(0.0f)));
    CHECK(allTrue(Mask<float, 8>(true)));
    CHECK_FALSE(anyTrue(Mask<float, 8>(false)));
  }
}

TEST_CASE("SIMD: width one is the same kernel with one lane") {
  // What lets a packed kernel serve a caller with a single item rather
  // than keeping a scalar copy of itself alongside.
  const float1 a{float1::load(A.data())};
  const float1 b{float1::load(B.data())};
  std::array<float, 1> out{};
  (a * b + a - b).store(out.data());
  CHECK(out[0] == A[0] * B[0] + A[0] - B[0]);
  sqrt(a).store(out.data());
  CHECK(out[0] == std::sqrt(A[0]));
  select(a < b, a, b).store(out.data());
  CHECK(out[0] == std::min(A[0], B[0]));
  CHECK(anyTrue(a < b));
  CHECK(allTrue(a < b));
  CHECK_FALSE(anyTrue(a > b));
}

TEST_CASE("SIMD: the four-wide pack agrees with the eight-wide one") {
  const float4 a{float4::load(A.data())};
  const float4 b{float4::load(B.data())};
  std::array<float, 4> out{};
  select(a < b, sqrt(a * a), a / b).store(out.data());
  for (size_t i = 0; i < 4; i++)
    CHECK(out[i] == doctest::Approx(A[i] < B[i] ? A[i] : A[i] / B[i]));
}
