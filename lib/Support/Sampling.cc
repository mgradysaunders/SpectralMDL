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
    margins[iY] = conditionals.back().non_normalized_sum();
  }
  marginal = Distribution1D(margins);
}

int2 Distribution2D::pixel_sample(float2 xi, float2 *xiRemap,
                                  float *pmf) const noexcept {
  if (numTexelsX == 0 || numTexelsY == 0) {
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

float Distribution2D::direction_pdf(float3 wi, int2 *iPixel) const noexcept {
  float theta = std::atan2(std::hypot(wi.x, wi.y), wi.z);
  theta = std::max(theta, 0.0f);
  theta = std::min(theta, PI);
  float sinTheta{std::sin(theta)};
  if (!(sinTheta > 0))
    return 0.0f;
  float phi = std::atan2(wi.y, wi.x);
  if (phi < 0.0f)
    phi += 2.0f * PI;
  phi = std::max(phi, 0.0f);
  phi = std::min(phi, 2.0f * PI);
  int nX = numTexelsX, iX = int(nX * phi / (2.0f * PI));
  int nY = numTexelsY, iY = int(nY * theta / PI);
  iX = std::max(0, std::min(iX, nX - 1));
  iY = std::max(0, std::min(iY, nY - 1));
  if (iPixel)
    *iPixel = {iX, iY};
  float pdf = pixel_pmf(int2(iX, iY));
  pdf *= numTexelsX * numTexelsY;
  pdf /= 2.0f * PI * PI * sinTheta;
  return pdf;
}

float3 Distribution2D::direction_sample(float2 xi, int2 *iPixel,
                                        float *pdf) const noexcept {
  auto i{pixel_sample(xi, &xi, pdf)};
  if (iPixel)
    *iPixel = i;
  auto phi{2.0f * PI * (i.x + xi.x) / float(numTexelsX)};
  auto theta{PI * (i.y + xi.y) / float(numTexelsY)};
  auto cosTheta{std::cos(theta)};
  auto sinTheta{std::sin(theta)};
  if (sinTheta == 0.0f) {
    if (pdf)
      *pdf = 0.0f;
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
