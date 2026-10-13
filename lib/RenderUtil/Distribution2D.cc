#include "smdl/RenderUtil/Distribution2D.h"

#include <algorithm>
#include <cmath>

#include "smdl/RenderUtil/FastMath.h"
#include "smdl/Support/Macros.h"

namespace smdl {

Distribution2D::Distribution2D(int numTexelsX, int numTexelsY,
                               Span<const float> values)
    : numTexelsX_(numTexelsX), numTexelsY_(numTexelsY) {
  SMDL_SANITY_CHECK(numTexelsX >= 0);
  SMDL_SANITY_CHECK(numTexelsY >= 0);
  SMDL_SANITY_CHECK(numTexelsX * numTexelsY == int(values.size()));
  conditionals_.reserve(numTexelsY);
  std::vector<float> margins(static_cast<size_t>(numTexelsY));
  for (int iY = 0; iY < numTexelsY; iY++) {
    conditionals_.emplace_back(
        values.subspan(size_t(numTexelsX) * size_t(iY), numTexelsX));
    margins[iY] = conditionals_.back().unnormalizedSum();
  }
  marginal_ = Distribution1D(margins);
}

int2 Distribution2D::pixelDraw(float2 xi, float2 *xiRemap,
                               float *pmf) const noexcept {
  if (numTexelsX_ == 0 || numTexelsY_ == 0) {
    if (pmf) *pmf = 1.0f;
    return {};
  } else {
    float pmfX{};
    float pmfY{};
    int iY{marginal_.indexDraw(xi.y, &xi.y, &pmfY)};
    SMDL_SANITY_CHECK(iY >= 0);
    SMDL_SANITY_CHECK(iY < int(conditionals_.size()));
    int iX{conditionals_[iY].indexDraw(xi.x, &xi.x, &pmfX)};
    if (xiRemap) *xiRemap = xi;
    if (pmf) *pmf = pmfX * pmfY;
    return {iX, iY};
  }
}

float Distribution2D::directionPDF(float3 wi, int2 *iPixel) const noexcept {
  // Deliberately not 'atan2(hypot(wi.x, wi.y), wi.z)'. Dividing out the
  // length keeps this independent of the scale of 'wi' exactly as that
  // form was, but 'hypot' is careful about intermediate overflow in a way
  // that costs more than everything else here put together.
  const float lenSq{lengthSquared(wi)};
  if (!(lenSq > 0)) return 0.0f;
  const float cosTheta{std::clamp(wi.z / std::sqrt(lenSq), -1.0f, 1.0f)};
  const float theta{fastAcos(cosTheta)};
  const float sinTheta{std::sqrt(std::max(1.0f - cosTheta * cosTheta, 0.0f))};
  if (!(sinTheta > 0)) return 0.0f;
  float phi{fastAtan2(wi.y, wi.x)};
  if (phi < 0.0f) phi += TWO_PI;
  phi = std::clamp(phi, 0.0f, TWO_PI);
  const int nX{numTexelsX_};
  const int nY{numTexelsY_};
  const int iX{std::clamp(int(float(nX) * (phi * INV_TWO_PI)), 0, nX - 1)};
  const int iY{std::clamp(int(float(nY) * (theta * INV_PI)), 0, nY - 1)};
  if (iPixel) *iPixel = {iX, iY};
  return pixelPMF(int2(iX, iY)) *
         (float(numTexelsX_ * numTexelsY_) / (TWO_PI * PI * sinTheta));
}

float3 Distribution2D::directionDraw(float2 xi, int2 *iPixel,
                                     float *pdf) const noexcept {
  if (iPixel) *iPixel = int2(-1, -1);
  if (pdf) *pdf = 0.0f;
  const int2 i{pixelDraw(xi, &xi)};
  const float phi{(float(i.x) + xi.x) * (TWO_PI / float(numTexelsX_))};
  const float theta{(float(i.y) + xi.y) * (PI / float(numTexelsY_))};
  const float cosTheta{std::cos(theta)};
  const float sinTheta{std::sin(theta)};
  if (sinTheta == 0.0f) return {};
  const float3 wi{normalize(float3(sinTheta * std::cos(phi), //
                                   sinTheta * std::sin(phi), cosTheta))};
  // The pixel and the density of the direction in hand, not of the
  // rectangle it was drawn from; see the header.
  int2 iRecovered{-1, -1};
  const float density{directionPDF(wi, &iRecovered)};
  if (iPixel) *iPixel = iRecovered;
  if (pdf) *pdf = density;
  return wi;
}

} // namespace smdl
