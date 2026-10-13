#include "smdl/RenderUtil/MonteCarlo.h"

#include <cmath>
#include <limits>

namespace smdl {

float2 uniformDiskDraw(float2 xi) noexcept {
  xi = xi * 2.0f - float2(1.0f);
  xi.x = (xi.x == 0.0f) ? std::numeric_limits<float>::epsilon() : xi.x;
  xi.y = (xi.y == 0.0f) ? std::numeric_limits<float>::epsilon() : xi.y;
  bool isXMajor = std::abs(xi.x) > std::abs(xi.y);
  float rad = isXMajor ? xi.x : xi.y;
  float phi = isXMajor ? (PI / 4.0f) * xi.y / xi.x
                       : (PI / 2.0f) - (PI / 4.0f) * xi.x / xi.y;
  return {rad * std::cos(phi), rad * std::sin(phi)};
}

float erfInverse(float y) noexcept {
  return simd::erfInverse(simd::float1(y))[0];
}

} // namespace smdl
