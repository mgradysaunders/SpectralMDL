#include "smdl/RenderUtil/MonteCarlo.h"

#include <algorithm>

namespace smdl {

namespace {

// The CMF is stored as a fixed-point fraction of the unit interval, in
// units of 2^-32. Fixed point costs four bytes an entry like a `float`
// would, so a large table stays half the size the equivalent `double`
// one would be and has a real chance of fitting in cache. Unlike a
// `float`, though, the difference of two neighboring entries is an exact
// integer subtraction, and that difference is the PMF: in floating point
// it cancels away most of the mantissa as soon as the entries are close
// together, which is precisely what happens as a distribution grows. At
// a million entries the float form loses over a tenth of the PMF, while
// this form is still good to a part in ten thousand.
constexpr double CMF_SCALE = 4294967296.0; // 2^32
constexpr double INV_CMF_SCALE = 1.0 / CMF_SCALE;

// Quantize a CMF value in [0, 1]. Truncating keeps a nondecreasing
// sequence nondecreasing; the top of the range saturates because 1.0
// scales to exactly one past the largest representable value.
[[nodiscard]] std::uint32_t quantizeCMF(double cmf) noexcept {
  return static_cast<std::uint32_t>(std::min(cmf * CMF_SCALE, 4294967295.0));
}

} // namespace

Distribution1D::Distribution1D(Span<const float> values) {
  // Accumulate and normalize in double, then quantize once, so that each
  // stored entry carries the rounding of a single conversion rather than
  // the accumulated drift of a running fixed-point sum.
  auto sums{std::vector<double>{}};
  sums.reserve(values.size() + 1);
  sums.emplace_back(0.0);
  for (const auto &value : values) {
    // Deliberately `fmax`: the values come from files (image texels, IES
    // profiles, BSDF tables), and this is the one place a NaN among them
    // is dropped rather than left to poison the whole table.
    totalSum += std::fmax(static_cast<double>(value), 0.0);
    sums.emplace_back(totalSum);
  }
  cmfs.resize(sums.size());
  // An all-zero distribution stays all-zero, so every PMF is zero rather
  // than NaN. This arises legitimately, e.g., as a conditional row of a
  // `Distribution2D` over a region with no density.
  if (totalSum > 0) {
    for (size_t i = 0; i < sums.size(); i++)
      cmfs[i] = quantizeCMF(sums[i] / totalSum);
  }
}

float Distribution1D::indexPMF(int i) const noexcept {
  if (0 <= i && i < size())
    return static_cast<float>(double(cmfs[i + 1] - cmfs[i]) * INV_CMF_SCALE);
  return 0.0f;
}

float Distribution1D::indexCMF(int i) const noexcept {
  if (0 <= i && i < size())
    return static_cast<float>(double(cmfs[i]) * INV_CMF_SCALE);
  return i < 0 ? 0.0f : 1.0f;
}

int Distribution1D::indexSample(float xi, float *xiRemap,
                                float *pmf) const noexcept {
  if (cmfs.size() < 2) {
    if (pmf) *pmf = 1;
    return 0;
  }
  const std::uint32_t key{quantizeCMF(std::clamp(double(xi), 0.0, 1.0))};
  auto itr{std::lower_bound(cmfs.begin(), cmfs.end(), key)};
  if (itr == cmfs.begin()) ++itr;
  if (itr == cmfs.end()) --itr;
  --itr;
  auto i{int(itr - cmfs.begin())};
  auto cmf0{*itr++};
  auto cmf1{*itr};
  // Nondecreasing entries, so this cannot wrap.
  const std::uint32_t width{cmf1 - cmf0};
  if (xiRemap) {
    // Against the dequantized bounds rather than against `key`, so the
    // remapped sample keeps the resolution of the incoming float instead
    // of inheriting that of the table.
    const double bound0{double(cmf0) * INV_CMF_SCALE};
    const double bound1{double(cmf1) * INV_CMF_SCALE};
    // Zero width means the entry cannot be sampled at all, which only
    // arises from an all-zero distribution; the remap has nowhere to
    // land, so give it the bottom of the interval rather than a
    // division by zero.
    *xiRemap =
        width > 0 ? float((double(xi) - bound0) / (bound1 - bound0)) : 0.0f;
    *xiRemap = std::clamp(*xiRemap, std::numeric_limits<float>::denorm_min(),
                          ONE_MINUS_EPS);
  }
  if (pmf) {
    *pmf = static_cast<float>(double(width) * INV_CMF_SCALE);
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
  return {rad * std::cos(phi), rad * std::sin(phi)};
}

float3 uniformConeSample(float cosThetaC, float2 xi) noexcept {
  float cosTheta{(1.0f - xi.x) * cosThetaC + xi.x};
  if (cosTheta < -1.0f) cosTheta = -1.0f;
  if (cosTheta > +1.0f) cosTheta = +1.0f;
  float sinTheta{std::sqrt(std::max(1.0f - cosTheta * cosTheta, 0.0f))};
  float phi{TWO_PI * xi.y};
  return {sinTheta * std::cos(phi), sinTheta * std::sin(phi), cosTheta};
}

float2 uniformApertureSample(int numBlades, float bladeAngle,
                             float2 xi) noexcept {
  if (numBlades < 3) return uniformDiskSample(xi);
  // Equal area with the unit disk
  const float n{float(numBlades)};
  const float circumRadius{std::sqrt(TWO_PI / (n * std::sin(TWO_PI / n)))};
  // Pick one of the `n` triangles that meet at the center with the first
  // dimension and rescale it back to (0,1), so the polygon costs nothing in
  // dimensions over the disk.
  const float i{std::floor(xi.x * n)};
  xi.x = std::min(xi.x * n - i, 1.0f);
  // Heitz's low-distortion square-to-triangle map
  if (xi.y > xi.x) {
    xi.x *= 0.5f;
    xi.y -= xi.x;
  } else {
    xi.y *= 0.5f;
    xi.x -= xi.y;
  }
  const float theta0{bladeAngle + TWO_PI * i / n};
  const float theta1{theta0 + TWO_PI / n};
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
    if (pmf) *pmf = 1.0f;
    return {};
  } else {
    float pmfX{};
    float pmfY{};
    int iY{marginal.indexSample(xi.y, &xi.y, &pmfY)};
    SMDL_SANITY_CHECK(iY >= 0);
    SMDL_SANITY_CHECK(iY < int(conditionals.size()));
    int iX{conditionals[iY].indexSample(xi.x, &xi.x, &pmfX)};
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
  const float theta{std::acos(cosTheta)};
  const float sinTheta{std::sqrt(std::max(1.0f - cosTheta * cosTheta, 0.0f))};
  if (!(sinTheta > 0)) return 0.0f;
  float phi{std::atan2(wi.y, wi.x)};
  if (phi < 0.0f) phi += TWO_PI;
  phi = std::clamp(phi, 0.0f, TWO_PI);
  const int nX{numTexelsX};
  const int nY{numTexelsY};
  const int iX{std::clamp(int(float(nX) * (phi / TWO_PI)), 0, nX - 1)};
  const int iY{std::clamp(int(float(nY) * (theta / PI)), 0, nY - 1)};
  if (iPixel) *iPixel = {iX, iY};
  return pixelPMF(int2(iX, iY)) *
         (float(numTexelsX * numTexelsY) / (TWO_PI * PI * sinTheta));
}

float3 Distribution2D::directionSample(float2 xi, int2 *iPixel,
                                       float *pdf) const noexcept {
  int2 i{pixelSample(xi, &xi, pdf)};
  if (iPixel) *iPixel = i;
  const float phi{(float(i.x) + xi.x) * (TWO_PI / float(numTexelsX))};
  const float theta{(float(i.y) + xi.y) * (PI / float(numTexelsY))};
  const float cosTheta{std::cos(theta)};
  const float sinTheta{std::sin(theta)};
  if (sinTheta == 0.0f) {
    if (pdf) *pdf = 0.0f;
    return {};
  } else {
    if (pdf) *pdf *= float(numTexelsX * numTexelsY) / (TWO_PI * PI * sinTheta);
    return normalize(float3(sinTheta * std::cos(phi), //
                            sinTheta * std::sin(phi), cosTheta));
  }
}

} // namespace smdl
