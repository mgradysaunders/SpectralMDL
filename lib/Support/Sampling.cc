#include "smdl/Support/Sampling.h"

namespace smdl {

Distribution1D::Distribution1D(Span<const float> values) {
  cmfs.reserve(values.size() + 1);
  cmfs.emplace_back(0.0);
  for (const auto &value : values) {
    totalSum += std::fmax(static_cast<double>(value), 0.0);
    cmfs.emplace_back(totalSum);
  }
  for (auto &cmf : cmfs) {
    cmf /= totalSum;
  }
}

float Distribution1D::index_pmf(int i) const noexcept {
  if (0 <= i && i < size())
    return static_cast<float>(cmfs[i + 1] - cmfs[i]);
  return 0.0f;
}

float Distribution1D::index_cmf(int i) const noexcept {
  if (0 <= i && i < size())
    return static_cast<float>(cmfs[i]);
  return i < 0 ? 0.0f : 1.0f;
}

int Distribution1D::index_sample(float xi, float *xiRemap,
                                 float *pmf) const noexcept {
  if (cmfs.size() < 2) {
    if (pmf)
      *pmf = 1;
    return 0;
  }
  auto itr{std::lower_bound(cmfs.begin(), cmfs.end(), double(xi))};
  if (itr == cmfs.begin())
    ++itr;
  if (itr == cmfs.end())
    --itr;
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

Distribution2D::Distribution2D(int numPixelsX, int numPixelsY,
                               Span<const float> values)
    : numPixelsX(numPixelsX), numPixelsY(numPixelsY) {
  SMDL_SANITY_CHECK(numPixelsX >= 0);
  SMDL_SANITY_CHECK(numPixelsY >= 0);
  SMDL_SANITY_CHECK(numPixelsX * numPixelsY == int(values.size()));
  conditionals.reserve(numPixelsY);
  auto margins{std::vector<float>(size_t(numPixelsY))};
  for (int iY{}; iY < numPixelsY; iY++) {
    conditionals.emplace_back(values.subspan(numPixelsX * iY, numPixelsX));
    margins[iY] = conditionals.back().non_normalized_sum();
  }
  marginal = Distribution1D(margins);
}

int2 Distribution2D::pixel_sample(float2 xi, float2 *xiRemap,
                                  float *pmf) const noexcept {
  if (numPixelsX == 0 || numPixelsY == 0) {
    if (pmf) {
      *pmf = 1.0f;
    }
    return {};
  }
  float pmfX{};
  float pmfY{};
  int iY{marginal.index_sample(xi.y, &xi.y, &pmfY)};
  SMDL_SANITY_CHECK(iY >= 0);
  SMDL_SANITY_CHECK(iY < int(conditionals.size()));
  int iX{conditionals[iY].index_sample(xi.x, &xi.x, &pmfX)};
  if (xiRemap) {
    *xiRemap = xi;
  }
  if (pmf) {
    *pmf = pmfX * pmfY;
  }
  return int2(iX, iY);
}

IBLDistribution2D::IBLDistribution2D(int numPixelsX, int numPixelsY,
                                     Span<const float> values,
                                     float3x3 lightToWorld)
    : lightToWorld(orthonormalize(lightToWorld)) {
  SMDL_SANITY_CHECK(numPixelsX >= 0);
  SMDL_SANITY_CHECK(numPixelsY >= 0);
  SMDL_SANITY_CHECK(numPixelsX * numPixelsY == int(values.size()));
  auto sinWeighted{std::vector<float>(values.size())};
  for (int iY = 0; iY < numPixelsY; iY++) {
    auto theta{PI * (iY + 0.5f) / float(numPixelsY)};
    auto sinTheta{std::sin(theta)};
    for (int iX = 0; iX < numPixelsX; iX++) {
      sinWeighted[iY * numPixelsX + iX] =
          sinTheta * values[iY * numPixelsX + iX];
    }
  }
  lightDistr = Distribution2D(numPixelsX, numPixelsY, sinWeighted);
}

float IBLDistribution2D::direction_pdf(float3 w) const noexcept {
  w = transpose(lightToWorld) * w;
  float theta = std::atan2(std::hypot(w.x, w.y), w.z);
  theta = std::max(theta, 0.0f);
  theta = std::min(theta, PI);
  float sinTheta = std::sin(theta);
  if (!(sinTheta > 0.0f))
    return 0.0f;
  float phi = std::atan2(w.y, w.x);
  if (phi < 0.0f)
    phi += 2.0f * PI;
  phi = std::max(phi, 0.0f);
  phi = std::min(phi, 2.0f * PI);
  return lightDistr.pixel_pmf(int2(int(num_pixels_x() * phi / (2.0f * PI)),
                                   int(num_pixels_y() * theta / PI))) /
         (2.0f * PI * PI * sinTheta);
}

float3 IBLDistribution2D::direction_sample(float2 xi,
                                           float *pdf) const noexcept {
  auto i{lightDistr.pixel_sample(xi, &xi, pdf)};
  auto phi{2.0f * PI * (i.x + xi.x) / float(num_pixels_x())};
  auto theta{PI * (i.y + xi.y) / float(num_pixels_y())};
  auto cosTheta{std::cos(theta)};
  auto sinTheta{std::sin(theta)};
  if (pdf) {
    if (sinTheta == 0.0f) {
      *pdf = 0;
    } else {
      *pdf /= 2.0f * PI * PI * sinTheta;
    }
  }
  return normalize(lightToWorld * float3(sinTheta * std::cos(phi),
                                         sinTheta * std::sin(phi), cosTheta));
}

} // namespace smdl
