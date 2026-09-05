/// \file
/// The vocabulary every part of the program shares: the vector and
/// matrix aliases, the math constants, the invalid index, the bounding
/// box, and the ray. Nothing here depends on Embree, assimp, or the
/// compiler, so the format readers include it without pulling in the
/// renderer.
#pragma once

#include <algorithm>
#include <cstdint>

#include "smdl/Support/Macros.h"
#include "smdl/Support/VectorMath.h"

using namespace smdl::vector_type_aliases;
using namespace smdl::matrix_type_aliases;

// The math constants the programs spell unqualified. They live in the
// library so that host and library agree on them by construction.
using smdl::INF;
using smdl::ONE_MINUS_EPS;
using smdl::PI;
using smdl::TWO_PI;

constexpr uint32_t INVALID_INDEX = uint32_t(-1);

/// An axis-aligned box, accumulated by folding in points or other
/// boxes. The default is the empty box, whose `lower` exceeds its
/// `upper` on every axis, so folding the first point in makes it that
/// point and `isEmpty()` reports what "no geometry at all" looks like.
class BoundBox3 final {
public:
  BoundBox3() = default;

  BoundBox3(const float3 &lower, const float3 &upper)
      : lower(lower), upper(upper) {}

  void extend(const float3 &point) noexcept {
    for (int axis = 0; axis < 3; axis++) {
      lower[axis] = std::min(lower[axis], point[axis]);
      upper[axis] = std::max(upper[axis], point[axis]);
    }
  }

  void extend(const BoundBox3 &other) noexcept {
    extend(other.lower);
    extend(other.upper);
  }

  /// Did nothing ever fold in?
  [[nodiscard]] bool isEmpty() const noexcept { return !(lower.x <= upper.x); }

  [[nodiscard]] float3 center() const noexcept {
    return 0.5f * (lower + upper);
  }

  [[nodiscard]] float3 extent() const noexcept { return upper - lower; }

  float3 lower{+INF, +INF, +INF};
  float3 upper{-INF, -INF, -INF};
};

/// The self-intersection offset, in scene units.
constexpr float EPS = 0.0001f;

/// The area of the triangle on the given corners, in whatever space
/// they are given in. Zero for a degenerate triangle, never negative.
[[nodiscard]] SMDL_ALWAYS_INLINE float
triangleArea(const float3 &point0, const float3 &point1,
             const float3 &point2) noexcept {
  return 0.5f * smdl::length(smdl::cross(point1 - point0, point2 - point0));
}

/// The barycentric coordinate of the triangle parameters `(u, v)` an
/// intersector reports. Both are clamped into the triangle before the
/// first weight is completed, so that rounding at an edge cannot make
/// the three weights sum past 1 or send one of them negative.
[[nodiscard]] SMDL_ALWAYS_INLINE float3 baryFromUV(float u, float v) noexcept {
  const auto clamp01{[](float value) { return std::clamp(value, 0.0f, 1.0f); }};
  return {clamp01(1.0f - u - v), clamp01(u), clamp01(v)};
}

class Ray final {
public:
  /// Evaluate.
  [[nodiscard]] float3 operator()(float t) const noexcept {
    return org + t * dir;
  }

  /// Apply transform.
  void transform(const float4x4 &xf) noexcept {
    org = transformPoint(xf, org);
    dir = transformDirection(xf, dir);
  }

public:
  float3 org{};    ///< The origin.
  float3 dir{};    ///< The direction.
  float tmin{EPS}; ///< The minimum parameter.
  float tmax{INF}; ///< The maximum parameter.

  /// The shutter fraction the ray traces at, in `[0, 1]`; see
  /// `PathTime`. Zero is shutter open, which is every ray of a shut
  /// shutter and the right time for a probe that has none.
  float time{};
};
