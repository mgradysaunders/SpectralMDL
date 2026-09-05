/// \file
/// Conversions from assimp's vocabulary types into this renderer's.
///
/// Deliberately not part of `MeshImport.h` or `MeshDeform.h`: those
/// forward declare the assimp types so that including them costs
/// nothing, and this has to see the definitions. Only the two source
/// files that talk to assimp include this.
#pragma once

#include "assimp/matrix4x4.h"
#include "assimp/quaternion.h"
#include "assimp/vector3.h"

#include "Common.h"

/// An assimp matrix as a `float4x4`. assimp stores rows, this stores
/// columns; both denote the same map, so nothing about the composition
/// order changes with the conversion.
[[nodiscard]] inline float4x4 fromAssimp(const aiMatrix4x4 &m) noexcept {
  return float4x4{
      float4{m.a1, m.b1, m.c1, m.d1}, float4{m.a2, m.b2, m.c2, m.d2},
      float4{m.a3, m.b3, m.c3, m.d3}, float4{m.a4, m.b4, m.c4, m.d4}};
}

[[nodiscard]] inline float3 fromAssimp(const aiVector3D &v) noexcept {
  return float3(v.x, v.y, v.z);
}

/// A quaternion as (x, y, z, w).
[[nodiscard]] inline float4 fromAssimp(const aiQuaternion &q) noexcept {
  return float4(q.x, q.y, q.z, q.w);
}
