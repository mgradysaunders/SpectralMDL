/// \file
/// How the layout family's text formats say that something moves: a
/// track of transform keys at absolute times, and the two instants a
/// lowering samples one at. The decomposition the interpolation runs
/// in is `Transform.h`.
#pragma once

#include <vector>

#include "Transform.h"

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
  /// a frame that spans no time, which is what makes every track lower
  /// static.
  float shut{};

  /// Does the frame span no time, so that both samples land on one
  /// instant?
  [[nodiscard]] bool isStill() const noexcept { return !(shut > open); }

  /// The same pair shifted later by `seconds`, which is what a place's
  /// `offset` does to the clock of everything below it.
  [[nodiscard]] MotionSampling shiftedBy(float seconds) const noexcept {
    return {open + seconds, shut + seconds};
  }
};
