/// \file
/// The hand-written transforms the suite builds its keys and records
/// out of: one rotation, one translation, and one that shears, scales
/// and translates at once so that no stored entry is a matrix default.
/// Shared by the transform, motion, and `.places` doctests.
#pragma once

#include <cmath>

#include "Transform.h"

namespace xf {

[[nodiscard]] inline float4x4 rotationZ(float degrees) {
  const float radians{smdl::radians(degrees)};
  const float c{std::cos(radians)}, s{std::sin(radians)};
  return float4x4{float4(c, s, 0, 0), float4(-s, c, 0, 0), float4(0, 0, 1, 0),
                  float4(0, 0, 0, 1)};
}

[[nodiscard]] inline float4x4 translation(float x, float y, float z) {
  return float4x4{float4(1, 0, 0, 0), float4(0, 1, 0, 0), float4(0, 0, 1, 0),
                  float4(x, y, z, 1)};
}

const float4x4 SHEAR{
    float4(1.0f, 0.2f, 0.0f, 0.0f), float4(0.3f, 1.5f, 0.1f, 0.0f),
    float4(0.0f, -0.4f, 0.8f, 0.0f), float4(2.0f, -1.0f, 3.0f, 1.0f)};

} // namespace xf
