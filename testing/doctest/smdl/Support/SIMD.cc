#include "Fixtures.h"

#include <array>
#include <cmath>

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

TEST_CASE("SIMD: the four-wide pack agrees with the eight-wide one") {
  const float4 a{float4::load(A.data())};
  const float4 b{float4::load(B.data())};
  std::array<float, 4> out{};
  select(a < b, sqrt(a * a), a / b).store(out.data());
  for (size_t i = 0; i < 4; i++)
    CHECK(out[i] == doctest::Approx(A[i] < B[i] ? A[i] : A[i] / B[i]));
}
