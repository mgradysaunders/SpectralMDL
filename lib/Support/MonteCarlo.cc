#include "smdl/Support/MonteCarlo.h"

namespace smdl {

Distribution1D::Distribution1D(Span<const float> values) {
  cmfs.reserve(values.size() + 1);
  cmfs.emplace_back(0.0);
  for (const auto &value : values) {
    totalSum += std::fmax(static_cast<double>(value), 0.0);
    cmfs.emplace_back(totalSum);
  }
  // An all-zero distribution stays all-zero, so every PMF is zero rather
  // than NaN. This arises legitimately, e.g., as a conditional row of a
  // `Distribution2D` over a region with no density.
  if (totalSum > 0) {
    for (auto &cmf : cmfs) {
      cmf /= totalSum;
    }
  }
}

float Distribution1D::indexPMF(int i) const noexcept {
  if (0 <= i && i < size()) return static_cast<float>(cmfs[i + 1] - cmfs[i]);
  return 0.0f;
}

float Distribution1D::indexCMF(int i) const noexcept {
  if (0 <= i && i < size()) return static_cast<float>(cmfs[i]);
  return i < 0 ? 0.0f : 1.0f;
}

int Distribution1D::indexSample(float xi, float *xiRemap,
                                float *pmf) const noexcept {
  if (cmfs.size() < 2) {
    if (pmf) *pmf = 1;
    return 0;
  }
  auto itr{std::lower_bound(cmfs.begin(), cmfs.end(), double(xi))};
  if (itr == cmfs.begin()) ++itr;
  if (itr == cmfs.end()) --itr;
  --itr;
  auto i{int(itr - cmfs.begin())};
  auto cmf0{*itr++};
  auto cmf1{*itr};
  if (xiRemap) {
    *xiRemap = float((double(xi) - cmf0) / (cmf1 - cmf0));
    *xiRemap = std::fmax(*xiRemap, std::numeric_limits<float>::denorm_min());
    *xiRemap =
        std::fmin(*xiRemap, 1 - std::numeric_limits<float>::epsilon() / 2);
  }
  if (pmf) {
    *pmf = float(cmf1 - cmf0);
  }
  return i;
}

float2 uniformDiskSample(float2 xi) noexcept {
  xi = xi * 2.0f - float2(1.0f);
  xi.x = (xi.x == 0.0f) ? std::numeric_limits<float>::epsilon() : xi.x;
  xi.y = (xi.y == 0.0f) ? std::numeric_limits<float>::epsilon() : xi.y;
  bool cond = std::abs(xi.x) > std::abs(xi.y);
  float rad = cond ? xi.x : xi.y;
  float phi = cond ? (PI / 4.0f) * xi.y / xi.x
                   : (PI / 2.0f) - (PI / 4.0f) * xi.x / xi.y;
  return float2(rad * std::cos(phi), rad * std::sin(phi));
}

float3 uniformConeSample(float cosThetaC, float2 xi) noexcept {
  float cosTheta{(1.0f - xi.x) * cosThetaC + xi.x};
  if (cosTheta < -1.0f) cosTheta = -1.0f;
  if (cosTheta > +1.0f) cosTheta = +1.0f;
  float sinTheta{std::sqrt(std::max(1.0f - cosTheta * cosTheta, 0.0f))};
  float phi{2 * PI * xi.y};
  return float3(sinTheta * std::cos(phi), sinTheta * std::sin(phi), cosTheta);
}

float2 uniformApertureSample(int numBlades, float bladeAngle,
                             float2 xi) noexcept {
  if (numBlades < 3) return uniformDiskSample(xi);
  // Equal area with the unit disk
  const float n{float(numBlades)};
  const float circumRadius{std::sqrt(2 * PI / (n * std::sin(2 * PI / n)))};
  // Pick one of the `n` triangles that meet at the center with the first
  // dimension and rescale it back to (0,1), so the polygon costs nothing in
  // dimensions over the disk.
  const float i{std::floor(xi.x * n)};
  xi.x = std::fmin(xi.x * n - i, 1.0f);
  // Heitz's low-distortion square-to-triangle map
  if (xi.y > xi.x) {
    xi.x *= 0.5f;
    xi.y -= xi.x;
  } else {
    xi.y *= 0.5f;
    xi.x -= xi.y;
  }
  const float theta0{bladeAngle + 2 * PI * i / n};
  const float theta1{theta0 + 2 * PI / n};
  // Barycentric over (center, v0, v1); the center contributes nothing.
  return circumRadius * (xi.x * float2(std::cos(theta0), std::sin(theta0)) +
                         xi.y * float2(std::cos(theta1), std::sin(theta1)));
}

float erfInverse(float y) noexcept {
  float w = -std::log(
      std::max(std::numeric_limits<float>::denorm_min(), (1 - y) * (1 + y)));
  float x = 0;
  if (w < 5) {
    w = w - 2.5f;
    x = w * 2.81022636e-08f + 3.43273939e-7f;
    x = w * x - 3.52338770e-6f;
    x = w * x - 4.39150654e-6f;
    x = w * x + 2.18580870e-4f;
    x = w * x - 1.25372503e-3f;
    x = w * x - 4.17768164e-3f;
    x = w * x + 2.46640727e-1f;
    x = w * x + 1.50140941f;
  } else {
    w = std::sqrt(w) - 3;
    x = x * -2.00214257e-4f + 1.00950558e-4f;
    x = w * x + 1.34934322e-3f;
    x = w * x - 3.67342844e-3f;
    x = w * x + 5.73950773e-3f;
    x = w * x - 7.62246130e-3f;
    x = w * x + 9.43887047e-3f;
    x = w * x + 1.00167406f;
    x = w * x + 2.83297682f;
  }
  x *= y;
  return x;
}

Distribution2D::Distribution2D(int numTexelsX, int numTexelsY,
                               Span<const float> values)
    : numTexelsX(numTexelsX), numTexelsY(numTexelsY) {
  SMDL_SANITY_CHECK(numTexelsX >= 0);
  SMDL_SANITY_CHECK(numTexelsY >= 0);
  SMDL_SANITY_CHECK(numTexelsX * numTexelsY == int(values.size()));
  conditionals.reserve(numTexelsY);
  auto margins{std::vector<float>(size_t(numTexelsY))};
  for (int iY{}; iY < numTexelsY; iY++) {
    conditionals.emplace_back(values.subspan(numTexelsX * iY, numTexelsX));
    margins[iY] = conditionals.back().unnormalizedSum();
  }
  marginal = Distribution1D(margins);
}

int2 Distribution2D::pixelSample(float2 xi, float2 *xiRemap,
                                 float *pmf) const noexcept {
  if (numTexelsX == 0 || numTexelsY == 0) {
    if (pmf) {
      *pmf = 1.0f;
    }
    return {};
  }
  float pmfX{};
  float pmfY{};
  int iY{marginal.indexSample(xi.y, &xi.y, &pmfY)};
  SMDL_SANITY_CHECK(iY >= 0);
  SMDL_SANITY_CHECK(iY < int(conditionals.size()));
  int iX{conditionals[iY].indexSample(xi.x, &xi.x, &pmfX)};
  if (xiRemap) {
    *xiRemap = xi;
  }
  if (pmf) {
    *pmf = pmfX * pmfY;
  }
  return int2(iX, iY);
}

float Distribution2D::directionPDF(float3 wi, int2 *iPixel) const noexcept {
  float theta = std::atan2(std::hypot(wi.x, wi.y), wi.z);
  theta = std::max(theta, 0.0f);
  theta = std::min(theta, PI);
  float sinTheta{std::sin(theta)};
  if (!(sinTheta > 0)) return 0.0f;
  float phi = std::atan2(wi.y, wi.x);
  if (phi < 0.0f) phi += 2.0f * PI;
  phi = std::max(phi, 0.0f);
  phi = std::min(phi, 2.0f * PI);
  int nX = numTexelsX, iX = int(nX * phi / (2.0f * PI));
  int nY = numTexelsY, iY = int(nY * theta / PI);
  iX = std::max(0, std::min(iX, nX - 1));
  iY = std::max(0, std::min(iY, nY - 1));
  if (iPixel) *iPixel = {iX, iY};
  float pdf = pixelPMF(int2(iX, iY));
  pdf *= numTexelsX * numTexelsY;
  pdf /= 2.0f * PI * PI * sinTheta;
  return pdf;
}

float3 Distribution2D::directionSample(float2 xi, int2 *iPixel,
                                       float *pdf) const noexcept {
  auto i{pixelSample(xi, &xi, pdf)};
  if (iPixel) *iPixel = i;
  auto phi{2.0f * PI * (i.x + xi.x) / float(numTexelsX)};
  auto theta{PI * (i.y + xi.y) / float(numTexelsY)};
  auto cosTheta{std::cos(theta)};
  auto sinTheta{std::sin(theta)};
  if (sinTheta == 0.0f) {
    if (pdf) *pdf = 0.0f;
    return {};
  } else {
    if (pdf) {
      *pdf *= numTexelsX * numTexelsY;
      *pdf /= 2.0f * PI * PI * sinTheta;
    }
    return normalize(float3(sinTheta * std::cos(phi), //
                            sinTheta * std::sin(phi), cosTheta));
  }
}

} // namespace smdl
