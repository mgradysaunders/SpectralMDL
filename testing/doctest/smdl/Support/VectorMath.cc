#include "Fixtures.h"

#include <cmath>

#include "smdl/Support/VectorMath.h"

using namespace smdl::vector_type_aliases;
using namespace smdl::matrix_type_aliases;

namespace {

// An affine transform with a translation, a non-uniform scale and a
// shear, so that a point and a direction genuinely disagree under it.
[[nodiscard]] float4x4 awkwardTransform() {
  return float4x4{float4{2.0f, 0.0f, 0.0f, 0.0f},  //
                  float4{0.5f, 3.0f, 0.0f, 0.0f},  //
                  float4{0.0f, 0.0f, -1.0f, 0.0f}, //
                  float4{7.0f, -2.0f, 5.0f, 1.0f}};
}

} // namespace

TEST_CASE("VectorMath: transforming points and directions") {
  SUBCASE("transformPoint carries the translation") {
    const auto xf{awkwardTransform()};
    const auto origin{smdl::transformPoint(xf, float3(0.0f, 0.0f, 0.0f))};
    CHECK(origin.x == doctest::Approx(7.0));
    CHECK(origin.y == doctest::Approx(-2.0));
    CHECK(origin.z == doctest::Approx(5.0));
    // The whole column, shear included.
    const auto point{smdl::transformPoint(xf, float3(1.0f, 1.0f, 1.0f))};
    CHECK(point.x == doctest::Approx(2.0 + 0.5 + 0.0 + 7.0));
    CHECK(point.y == doctest::Approx(0.0 + 3.0 + 0.0 - 2.0));
    CHECK(point.z == doctest::Approx(0.0 + 0.0 - 1.0 + 5.0));
  }
  SUBCASE("transformDirection drops the translation") {
    const auto xf{awkwardTransform()};
    const auto zero{smdl::transformDirection(xf, float3(0.0f, 0.0f, 0.0f))};
    CHECK(zero.x == 0.0f);
    CHECK(zero.y == 0.0f);
    CHECK(zero.z == 0.0f);
    // A direction is the difference of two points, so it must be what
    // transforming both and subtracting gives.
    const float3 from{1.0f, -4.0f, 2.0f};
    const float3 to{-3.0f, 0.5f, 6.0f};
    const auto direct{smdl::transformDirection(xf, to - from)};
    const auto viaPoints{smdl::transformPoint(xf, to) -
                         smdl::transformPoint(xf, from)};
    CHECK(direct.x == doctest::Approx(viaPoints.x));
    CHECK(direct.y == doctest::Approx(viaPoints.y));
    CHECK(direct.z == doctest::Approx(viaPoints.z));
  }
  SUBCASE("A rigid transform round-trips through its affine inverse") {
    const auto rigid{smdl::lookAt(float3(3.0f, -1.0f, 2.0f), float3(0.0f))};
    const auto inverse{smdl::affineInverse(rigid)};
    const float3 point{1.5f, 2.5f, -0.5f};
    const auto back{
        smdl::transformPoint(inverse, smdl::transformPoint(rigid, point))};
    CHECK(back.x == doctest::Approx(point.x));
    CHECK(back.y == doctest::Approx(point.y));
    CHECK(back.z == doctest::Approx(point.z));
    // A rigid frame preserves length, which is the property every
    // caller that transforms a direction through one relies on.
    const float3 direction{smdl::normalize(float3(1.0f, 2.0f, 3.0f))};
    CHECK(smdl::length(smdl::transformDirection(rigid, direction)) ==
          doctest::Approx(1.0));
  }
  SUBCASE("A pure translation moves points and leaves directions alone") {
    const float4x4 shift{float4{1, 0, 0, 0}, float4{0, 1, 0, 0},
                         float4{0, 0, 1, 0}, float4{4, 5, 6, 1}};
    const auto point{smdl::transformPoint(shift, float3(1, 1, 1))};
    const auto direction{smdl::transformDirection(shift, float3(1, 1, 1))};
    CHECK(point.x == 5.0f);
    CHECK(point.y == 6.0f);
    CHECK(point.z == 7.0f);
    CHECK(direction.x == 1.0f);
    CHECK(direction.y == 1.0f);
    CHECK(direction.z == 1.0f);
  }
}

TEST_CASE("VectorMath: the three by three inverse") {
  // The XYZ to linear sRGB matrix, well conditioned and of mixed signs.
  const double3x3 m{double3(3.240450, -0.969266, 0.0556434),
                    double3(-1.537140, 1.876010, -0.2040260),
                    double3(-0.498532, 0.041556, 1.0572300)};
  SUBCASE("A matrix times its inverse is the identity") {
    auto inverse{m};
    REQUIRE(smdl::tryInvert(inverse));
    const auto product{m * inverse};
    for (size_t j = 0; j < 3; j++)
      for (size_t i = 0; i < 3; i++)
        CHECK(std::abs(product[j][i] - (i == j ? 1.0 : 0.0)) < 1e-12);
  }
  SUBCASE("A singular one is left alone") {
    double3x3 singular{double3(1, 2, 3), double3(2, 4, 6), double3(0, 0, 1)};
    const auto before{singular};
    CHECK(!smdl::tryInvert(singular));
    for (size_t j = 0; j < 3; j++)
      for (size_t i = 0; i < 3; i++) CHECK(singular[j][i] == before[j][i]);
  }
}
