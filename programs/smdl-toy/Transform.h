/// \file
/// A transform, the factors it splits into, and how two of them blend.
#pragma once

#include <cmath>

#include "Common.h"

/// A transform split into the factors that interpolate independently:
/// `xf = T R S`, with `S` the upper-triangular scale and skew, `R` the
/// rotation, and `T` the translation.
///
/// This is exactly the form Embree interpolates a moving instance in
/// (`RTCQuaternionDecomposition`), and deliberately so: a track sampled
/// here and the shutter Embree spans between two samples then agree
/// about what "linearly interpolated" means, and a turning object turns
/// at every scale rather than shrinking through the chord of its
/// rotation at one of them.
///
/// `R S` reproduces the linear part exactly; only the rotation-to-
/// quaternion step rounds.
///
class TransformDecomposition final {
public:
  /// The translation.
  float3 translation{};

  /// The rotation as a quaternion, spelled `(w, x, y, z)` in the order
  /// Embree takes it. Unit length by construction.
  float4 quaternion{1, 0, 0, 0};

  /// The scales along the three axes. A key that reverses handedness
  /// comes out with a negative `z` scale rather than an improper
  /// rotation.
  float3 scale{1, 1, 1};

  /// The skews, in the order `xy`, `xz`, `yz`, which are the three
  /// entries above the diagonal of `S`.
  float3 skew{};

  /// Is this a rigid motion, a rotation and a translation with no scale
  /// or skew, to within `tolerance`? A mirrored transform is not, since
  /// handedness reversal lands in a negative `z` scale.
  [[nodiscard]] bool isRigid(float tolerance = 1.0e-5f) const noexcept {
    return std::abs(scale.x - 1.0f) <= tolerance &&
           std::abs(scale.y - 1.0f) <= tolerance &&
           std::abs(scale.z - 1.0f) <= tolerance &&
           std::abs(skew.x) <= tolerance && std::abs(skew.y) <= tolerance &&
           std::abs(skew.z) <= tolerance;
  }
};

/// Decompose `xf`, by one fixed procedure so that two keys of a pair
/// come out as comparable rotations and like-named skews: a
/// Gram-Schmidt over the columns in the order x, y, z, with the third
/// axis by cross product.
[[nodiscard]] TransformDecomposition
decomposeTransform(const float4x4 &xf) noexcept;

/// Reassemble what `decomposeTransform()` took apart.
[[nodiscard]] float4x4
composeTransform(const TransformDecomposition &parts) noexcept;

/// The transform `t` of the way from `a` to `b`: the rotations slerped
/// and everything else interpolated componentwise.
///
/// The ends are exact: `t` at or past 0 returns `a` itself and at or
/// past 1 returns `b` itself, with no decomposition round trip, so a
/// track sampled at one of its own keys reproduces that key bit for
/// bit.
[[nodiscard]] float4x4
interpolateTransform(const float4x4 &a, const float4x4 &b, float t) noexcept;
