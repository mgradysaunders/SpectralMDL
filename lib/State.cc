#include "smdl/State.h"

#include <algorithm>

namespace smdl {

void State::finalize() noexcept {
  // 1. Every loop below indexes the tangent arrays by it, and so does the
  // generated code that reads them, so a host asking for more spaces than
  // are there is clamped once, here, rather than running off the end.
  textureSpaceCount = std::clamp(textureSpaceCount, 0, int(TEXTURE_SPACE_MAX));
  vertexColorCount = std::clamp(vertexColorCount, 0, int(VERTEX_COLOR_MAX));

  // 2. Orthonormalize normal and tangent vectors.
  if (!tryNormalize(normal)) normal = {0, 0, 1};
  for (int i = 0; i < textureSpaceCount; i++)
    gramSchmidtOrthonormalize(normal, textureTangentU[i], textureTangentV[i]);

  // 3. Orthonormalize geometry normal and tangent vectors.
  if (!tryNormalize(geometryNormal)) geometryNormal = normal;
  for (int i = 0; i < textureSpaceCount; i++)
    gramSchmidtOrthonormalize(geometryNormal, geometryTangentU[i],
                              geometryTangentV[i]);

  // 4. Orthonormalize object-to-world matrix. An already orthonormal one
  // is left exactly as the host set it; otherwise this is `orthonormalize()`
  // of it, which a host can call to predict the answer bit for bit.
  //
  // The matrix is a per-instance constant that arrives again at every
  // shading point, and a renderer that hands over the rigid frame it
  // already derived takes the first branch every time, so the six dot
  // products that recognize the case are worth their cost against the
  // three square roots and six divides they skip.
  const float3 axisX{float3(objectToWorld[0])};
  const float3 axisY{float3(objectToWorld[1])};
  const float3 axisZ{float3(objectToWorld[2])};
  constexpr float ORTHONORMAL_EPS = 1e-6f;
  const auto isOrthonormal{[&] {
    return std::abs(lengthSquared(axisX) - 1) < ORTHONORMAL_EPS &&
           std::abs(lengthSquared(axisY) - 1) < ORTHONORMAL_EPS &&
           std::abs(lengthSquared(axisZ) - 1) < ORTHONORMAL_EPS &&
           std::abs(dot(axisX, axisY)) < ORTHONORMAL_EPS &&
           std::abs(dot(axisX, axisZ)) < ORTHONORMAL_EPS &&
           std::abs(dot(axisY, axisZ)) < ORTHONORMAL_EPS;
  }};
  if (!isOrthonormal()) {
    float3x3 axes{orthonormalize(float3x3(axisX, axisY, axisZ))};
    objectToWorld[0] = float4(axes[0], 0.0f);
    objectToWorld[1] = float4(axes[1], 0.0f);
    objectToWorld[2] = float4(axes[2], 0.0f);
  }

  // 5 and 6.
  finalizeUnchecked();
}

} // namespace smdl
