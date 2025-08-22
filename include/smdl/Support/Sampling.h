/// \file
#pragma once

#include <random>

#include "smdl/Export.h"
#include "smdl/Support/VectorMath.h"

namespace smdl {

/// \addtogroup Support
/// \{

/// A data-driven distribution in 1 dimension.
class SMDL_EXPORT Distribution1D final {
public:
  Distribution1D() = default;

  Distribution1D(const std::vector<double> &weights);

public:
  /// The number of indexes.
  [[nodiscard]] int size() const { return int(cmfs.size()) - 1; }

  /// The index probability mass function (PMF).
  [[nodiscard]] float index_pmf(int i) const {
    if (0 <= i && i < size())
      return static_cast<float>(cmfs[i + 1] - cmfs[i]);
    return 0.0f;
  }

  /// The index cumulative mass function (CMF).
  [[nodiscard]] float index_cmf(int i) const {
    if (0 <= i && i < size())
      return static_cast<float>(cmfs[i]);
    return i < 0 ? 0.0f : 1.0f;
  }

  /// The index sample function.
  ///
  /// \param[in] u
  /// The random sample in \f$ (0,1) \f$.
  ///
  /// \param[out] uRemap
  /// If non-null, receives the random sample remapped back into \f$ (0,1) \f$
  /// so it can be reused.
  ///
  /// \returns
  /// The sampled index and the associated PMF.
  ///
  [[nodiscard]] std::pair<int, float> index_sample(float u,
                                                   float *uRemap = {}) const;

  template <typename G> [[nodiscard]] int operator()(G &gen) const {
    return index_sample(std::generate_canonical<float, 32>(gen)).first;
  }

private:
  /// The cumulative mass function values.
  std::vector<double> cmfs{};
};

/// \name Functions (sampling)
/// \{

/// Generate canonical random sample in \f$ (0,1) \f$.
template <typename G> [[nodiscard]] inline float generate_canonical(G &g) {
  float xi{std::generate_canonical<float, 32>(g)};
  xi = std::fmax(xi, std::numeric_limits<float>::denorm_min());      // > 0.0f
  xi = std::fmin(xi, 1 - std::numeric_limits<float>::epsilon() / 2); // < 1.0f
  return xi;
}

/// Generate canonical random sample in \f$ (0,1)^2 \f$.
template <typename G> [[nodiscard]] inline float2 generate_canonical2(G &g) {
  return {generate_canonical(g), generate_canonical(g)};
}

/// Generate canonical random sample in \f$ (0,1)^3 \f$.
template <typename G> [[nodiscard]] inline float3 generate_canonical3(G &g) {
  return {generate_canonical(g), generate_canonical(g), generate_canonical(g)};
}

/// Generate canonical random sample in \f$ (0,1)^4 \f$.
template <typename G> [[nodiscard]] inline float4 generate_canonical4(G &g) {
  return {generate_canonical(g), generate_canonical(g), generate_canonical(g),
          generate_canonical(g)};
}

/// Uniform disk PDF.
///
/// \f[ p(\mathbf{X}) = \frac{1}{\pi r^2} \f]
///
[[nodiscard]] inline float uniform_disk_pdf(float r = 1) noexcept {
  return 1.0f / (PI * r * r);
}

/// Uniform disk sample using concentric mapping to better preserve stratified
/// samples.
///
/// \param[in] xi
/// The random sample \f$ \xi \in (0,1)^2 \f$.
///
[[nodiscard]] inline float2 uniform_disk_sample(float2 xi) noexcept {
  xi = xi * 2.0f - float2(1.0f);
  xi.x = (xi.x == 0.0f) ? std::numeric_limits<float>::epsilon() : xi.x;
  xi.y = (xi.y == 0.0f) ? std::numeric_limits<float>::epsilon() : xi.y;
  bool cond = std::abs(xi.x) > std::abs(xi.y);
  float rad = cond ? xi.x : xi.y;
  float phi = cond ? (PI / 4.0f) * xi.y / xi.x
                   : (PI / 2.0f) - (PI / 4.0f) * xi.x / xi.y;
  return float2(rad * std::cos(phi), rad * std::sin(phi));
}

/// Cosine-weighted hemisphere direction PDF.
///
/// \f[ p(\omega) = \frac{\max(\omega\cdot\hat{z}, 0)}{\pi} \f]
///
[[nodiscard]] inline float cosine_hemisphere_pdf(float cosTheta) noexcept {
  return std::max(cosTheta, 0.0f) / PI;
}

/// Cosine-weighted hemisphere direction sample.
///
/// \param[in] xi
/// The random sample \f$ \xi \in (0,1)^2 \f$.
///
[[nodiscard]] inline float3 cosine_hemisphere_sample(float2 xi) noexcept {
  auto sinTheta{uniform_disk_sample(xi)};
  auto cosTheta{std::sqrt(std::max(0.0f, 1.0f - length_squared(sinTheta)))};
  return float3(sinTheta.x, sinTheta.y, cosTheta);
}

/// Uniform sphere direction PDF.
///
/// \f[ p(\omega) = \frac{1}{4\pi} \f]
///
[[nodiscard]] inline float uniform_sphere_pdf() noexcept { return 0.25f / PI; }

/// Uniform sphere direction sample.
///
/// \param[in] xi
/// The random sample \f$ \xi \in (0,1)^2 \f$.
///
[[nodiscard]] inline float3 uniform_sphere_sample(float2 xi) {
  float cosTheta{std::max(-1.0f, std::min(2.0f * xi.x - 1.0f, 1.0f))};
  float sinTheta{std::sqrt(1.0f - cosTheta * cosTheta)};
  float phi{2.0f * PI * xi.y};
  return float3(sinTheta * std::cos(phi), sinTheta * std::sin(phi), cosTheta);
}

/// Uniform cone direction PDF.
///
/// \f[ p(\omega) = \frac{1}{2\pi(1 - \cos\theta_C)} \f]
///
/// \param[in] cosThetaC
/// The cosine of the cone angle \f$ \theta_C \f$.
///
[[nodiscard]] inline float uniform_cone_pdf(float cosThetaC) {
  return 0.5f / (PI * (1.0f - cosThetaC));
}

/// Uniform cone direction sample.
///
/// \param[in] cosThetaC
/// The cosine of the cone angle \f$ \theta_C \f$.
///
/// \param[in] xi
/// The random sample \f$ \xi \in (0,1)^2 \f$.
///
[[nodiscard]] inline float3 uniform_cone_sample(float cosThetaC, float2 xi) {
  float cosTheta{(1.0f - xi.x) * cosThetaC + xi.x};
  float sinTheta{std::sqrt(std::max(1.0f - cosTheta * cosTheta, 0.0f))};
  float phi{2.0f * PI * xi.y};
  return float3(sinTheta * std::cos(phi), sinTheta * std::sin(phi), cosTheta);
}

/// The error function inverse, necessary to sample the standard normal
/// distribution.
[[nodiscard]] inline float erf_inverse(float y) {
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

/// Standard normal distribution PDF.
///
/// \f[ p(x) = \frac{1}{\sqrt{2\pi}} \exp\left(-\frac{1}{2}x^2\right) \f]
///
[[nodiscard]] inline float standard_normal_pdf(float x) {
  return /*1/sqrt(2pi)=*/0.398942280401f * std::exp(-0.5f * x * x);
}

/// Standard normal distribution CDF.
///
/// \f[ P(x) = \frac{1}{2}\left(1 + \mathrm{erf}\frac{x}{\sqrt2}\right) \f]
///
[[nodiscard]] inline float standard_normal_cdf(float x) {
  return 0.5f * (1 + std::erf(/*1/sqrt(2)=*/0.707106781187f * x));
}

/// Standard normal distribution sample.
[[nodiscard]] inline float standard_normal_sample(float u) {
  return /*sqrt(2)=*/1.41421356237f * erf_inverse(2 * u - 1);
}

/// \}

/// \}

} // namespace smdl
