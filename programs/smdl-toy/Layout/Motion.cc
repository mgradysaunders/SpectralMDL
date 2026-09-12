#include "Layout/Motion.h"

#include <algorithm>
#include <cmath>

// The transform decomposition and the track that interpolates in it.
// Both the layout's `motion` blocks and the Embree hand-off in
// `Scene.cc` run through `decomposeTransform()`, so what a file means by
// a key and what the traversal does between two of them are one
// procedure.

TransformDecomposition decomposeTransform(const float4x4 &xf) noexcept {
  const float3 m0{float3(xf[0])};
  const float3 m1{float3(xf[1])};
  const float3 m2{float3(xf[2])};
  const float sx{length(m0)};
  const float3 q0{m0 / sx};
  const float skewXY{dot(q0, m1)};
  const float3 v1{m1 - skewXY * q0};
  const float sy{length(v1)};
  const float3 q1{v1 / sy};
  const float3 q2{cross(q0, q1)};
  const float skewXZ{dot(q0, m2)};
  const float skewYZ{dot(q1, m2)};
  const float sz{dot(q2, m2)};
  // The rotation (q0 q1 q2) as a quaternion, by the largest of the
  // trace and the diagonal, which keeps the divisor away from zero.
  // `r<row><column>`.
  const float r00{q0.x}, r10{q0.y}, r20{q0.z};
  const float r01{q1.x}, r11{q1.y}, r21{q1.z};
  const float r02{q2.x}, r12{q2.y}, r22{q2.z};
  float w{}, x{}, y{}, z{};
  if (const float trace{r00 + r11 + r22}; trace >= 0.0f) {
    const float t{1.0f + trace};
    const float s{0.5f / std::sqrt(t)};
    w = t * s;
    x = (r21 - r12) * s;
    y = (r02 - r20) * s;
    z = (r10 - r01) * s;
  } else if (r00 >= std::max(r11, r22)) {
    const float t{(1.0f + r00) - (r11 + r22)};
    const float s{0.5f / std::sqrt(t)};
    w = (r21 - r12) * s;
    x = t * s;
    y = (r10 + r01) * s;
    z = (r02 + r20) * s;
  } else if (r11 >= r22) {
    const float t{(1.0f + r11) - (r22 + r00)};
    const float s{0.5f / std::sqrt(t)};
    w = (r02 - r20) * s;
    x = (r10 + r01) * s;
    y = t * s;
    z = (r21 + r12) * s;
  } else {
    const float t{(1.0f + r22) - (r00 + r11)};
    const float s{0.5f / std::sqrt(t)};
    w = (r10 - r01) * s;
    x = (r02 + r20) * s;
    y = (r21 + r12) * s;
    z = t * s;
  }
  TransformDecomposition parts{};
  parts.translation = float3(xf[3]);
  parts.quaternion = float4(w, x, y, z);
  parts.scale = float3(sx, sy, sz);
  parts.skew = float3(skewXY, skewXZ, skewYZ);
  return parts;
}

float4x4 composeTransform(const TransformDecomposition &parts) noexcept {
  float4 q{parts.quaternion};
  if (!smdl::tryNormalize(q)) q = float4(1, 0, 0, 0);
  const float w{q[0]}, x{q[1]}, y{q[2]}, z{q[3]};
  // The rotation's columns, which the scale and skew then mix.
  const float3 r0{1 - 2 * (y * y + z * z), 2 * (x * y + z * w),
                  2 * (x * z - y * w)};
  const float3 r1{2 * (x * y - z * w), 1 - 2 * (x * x + z * z),
                  2 * (y * z + x * w)};
  const float3 r2{2 * (x * z + y * w), 2 * (y * z - x * w),
                  1 - 2 * (x * x + y * y)};
  const float3 m0{parts.scale.x * r0};
  const float3 m1{parts.skew.x * r0 + parts.scale.y * r1};
  const float3 m2{parts.skew.y * r0 + parts.skew.z * r1 + parts.scale.z * r2};
  return float4x4{
      float4(m0.x, m0.y, m0.z, 0), float4(m1.x, m1.y, m1.z, 0),
      float4(m2.x, m2.y, m2.z, 0),
      float4(parts.translation.x, parts.translation.y, parts.translation.z, 1)};
}

namespace {
// The shorter arc between two unit quaternions, falling back to a
// normalized lerp when they are near enough that the sine divisor is
// worth avoiding. The same shape as Embree's own slerp, which is what
// carries the interpolation the rest of the way through the shutter.
[[nodiscard]] float4 slerp(const float4 &a, float4 b, float t) noexcept {
  float cosine{dot(a, b)};
  if (cosine < 0.0f) {
    b = -b;
    cosine = -cosine;
  }
  if (cosine > 0.9995f) {
    float4 result{(1.0f - t) * a + t * b};
    if (!smdl::tryNormalize(result)) return a;
    return result;
  }
  const float theta{std::acos(std::clamp(cosine, -1.0f, 1.0f))};
  const float sine{std::sin(theta)};
  return (std::sin((1.0f - t) * theta) / sine) * a +
         (std::sin(t * theta) / sine) * b;
}
} // namespace

float4x4 interpolateTransform(const float4x4 &a, const float4x4 &b,
                              float t) noexcept {
  if (!(t > 0.0f)) return a;
  if (!(t < 1.0f)) return b;
  const TransformDecomposition lo{decomposeTransform(a)};
  const TransformDecomposition hi{decomposeTransform(b)};
  TransformDecomposition parts{};
  parts.translation = (1.0f - t) * lo.translation + t * hi.translation;
  parts.quaternion = slerp(lo.quaternion, hi.quaternion, t);
  parts.scale = (1.0f - t) * lo.scale + t * hi.scale;
  parts.skew = (1.0f - t) * lo.skew + t * hi.skew;
  return composeTransform(parts);
}

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
