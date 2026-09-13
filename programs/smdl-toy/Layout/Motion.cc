#include "Layout/Motion.h"

// The track that samples `Transform.h`'s blend at absolute times.

float4x4 MotionTrack::at(float seconds) const noexcept {
  if (keys.empty()) return float4x4(1.0f);
  if (!(seconds > keys.front().time)) return keys.front().transform;
  if (!(seconds < keys.back().time)) return keys.back().transform;
  size_t i{1};
  while (i < keys.size() && keys[i].time < seconds) i++;
  const MotionKey &lo{keys[i - 1]};
  const MotionKey &hi{keys[i]};
  if (seconds == lo.time) return lo.transform;
  if (seconds == hi.time) return hi.transform;
  const float span{hi.time - lo.time};
  return interpolateTransform(lo.transform, hi.transform,
                              (seconds - lo.time) / span);
}

bool MotionTrack::hasKeyBetween(float open, float shut) const noexcept {
  for (const auto &key : keys)
    if (key.time > open && key.time < shut) return true;
  return false;
}
