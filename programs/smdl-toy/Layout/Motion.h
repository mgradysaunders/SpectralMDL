/// \file
/// How the layout family's text formats say that something moves: a
/// track of transform keys at absolute times, the two instants a
/// lowering samples one at, and the decomposition the interpolation
/// runs in.
#pragma once

#include <vector>

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

/// One key of a motion track: a transform and the absolute time in
/// seconds at which it holds.
class MotionKey final {
public:
  float time{};
  float4x4 transform{float4x4(1.0f)};
};

/// A motion track: what a `motion { at <seconds> ... }` block says, as
/// keys in ascending time.
///
/// The times are absolute readings of the render clock, not fractions
/// of a shutter, which is what makes a file that carries motion mean
/// the same thing at every shutter and lets one file describe a whole
/// span of time rather than one frame.
///
class MotionTrack final {
public:
  /// The keys, in ascending time. The parser rejects any other order,
  /// so nothing downstream sorts.
  std::vector<MotionKey> keys{};

  /// Does the track say anything at all? An empty track is a static
  /// placement.
  [[nodiscard]] bool empty() const noexcept { return keys.empty(); }

  /// The transform at `seconds`: interpolated between the surrounding
  /// keys, clamped to the first key before the track begins and to the
  /// last after it ends, and returned verbatim at a key's own time.
  ///
  /// An empty track answers the identity, which no caller asks for: a
  /// static placement keeps the transform its operations spell.
  [[nodiscard]] float4x4 at(float seconds) const noexcept;

  /// Does the track have a key strictly inside the open interval
  /// `(open, shut)`? Such a key is not represented by the two samples
  /// the renderer takes, and the lowering says so.
  [[nodiscard]] bool hasKeyBetween(float open, float shut) const noexcept;
};

/// The two instants a lowering samples motion at: the render clock in
/// seconds at shutter open and at shutter shut.
///
/// A separate small type rather than `Shutter` from `Color.h` because
/// the layout toolchain sits below the renderer's own vocabulary and
/// must not reach up into it.
///
class MotionSampling final {
public:
  MotionSampling() = default;

  MotionSampling(float open, float shut) noexcept : open(open), shut(shut) {}

  /// The render clock at shutter open, in seconds.
  float open{};

  /// The render clock at shutter shut, in seconds. Equal to `open` for
  /// a shut shutter, which is what makes every track lower static.
  float shut{};

  /// Is the shutter shut, so that both samples land on one instant?
  [[nodiscard]] bool isStill() const noexcept { return !(shut > open); }

  /// The same pair shifted later by `seconds`, which is what a place's
  /// `offset` does to the clock of everything below it.
  [[nodiscard]] MotionSampling shiftedBy(float seconds) const noexcept {
    return {open + seconds, shut + seconds};
  }
};
