/// \file
#pragma once

#include <random>

#include "smdl/Export.h"
#include "smdl/Support/Macros.h"
#include "smdl/Support/Span.h"
#include "smdl/Support/VectorMath.h"

namespace smdl {

/// \addtogroup Support
/// \{

/// A data-driven distribution in 1 dimension.
class SMDL_EXPORT Distribution1D final {
public:
  /// Default constructor.
  Distribution1D() = default;

  /// Constructor.
  Distribution1D(Span<const float> values);

public:
  /// Clear.
  void clear() noexcept {
    totalSum = 0;
    cmfs.clear();
  }

  /// The number of indexes.
  [[nodiscard]] int size() const noexcept { return int(cmfs.size()) - 1; }

  /// The index probability mass function (PMF).
  [[nodiscard]] float indexPMF(int i) const noexcept;

  /// The index cumulative mass function (CMF).
  [[nodiscard]] float indexCMF(int i) const noexcept;

  /// The index sampling routine.
  ///
  /// \param[in]  xi       The random sample \f$ \xi \in (0,1) \f$.
  /// \param[out] xiRemap  If non-null, receives the remapped random sample.
  /// \param[out] pmf      If non-null, receives the associated PMF.
  ///
  [[nodiscard]] int indexSample(float xi, float *xiRemap = {},
                                float *pmf = {}) const noexcept;

  /// The unnormalized sum.
  [[nodiscard]] float unnormalizedSum() const noexcept {
    return static_cast<float>(totalSum);
  }

private:
  double totalSum{};

  std::vector<double> cmfs{};
};

/// \name Functions (sampling)
/// \{

/// Generate canonical random sample in \f$ (0,1) \f$.
template <typename G> [[nodiscard]] inline float generateCanonical(G &g) {
  float xi{std::generate_canonical<float, 32>(g)};
  xi = std::fmax(xi, std::numeric_limits<float>::denorm_min());      // > 0.0f
  xi = std::fmin(xi, 1 - std::numeric_limits<float>::epsilon() / 2); // < 1.0f
  return xi;
}

/// Generate canonical random sample in \f$ (0,1)^2 \f$.
template <typename G> [[nodiscard]] inline float2 generateCanonical2(G &g) {
  return {generateCanonical(g), generateCanonical(g)};
}

/// Generate canonical random sample in \f$ (0,1)^3 \f$.
template <typename G> [[nodiscard]] inline float3 generateCanonical3(G &g) {
  return {generateCanonical(g), generateCanonical(g), generateCanonical(g)};
}

/// Generate canonical random sample in \f$ (0,1)^4 \f$.
template <typename G> [[nodiscard]] inline float4 generateCanonical4(G &g) {
  return {generateCanonical(g), generateCanonical(g), generateCanonical(g),
          generateCanonical(g)};
}

/// Advance the given quasi-random sample according to a 2-D low
/// discrepancy sequence.
[[nodiscard]] inline float2 advanceLowDiscrepancy2(float2 &xi) {
  xi = xi + float2(0.7548776662466927f, 0.5698402909980532f);
  xi.x -= std::floor(xi.x);
  xi.y -= std::floor(xi.y);
  return xi;
}

/// Advance the given quasi-random sample according to a 3-D low
/// discrepancy sequence.
[[nodiscard]] inline float3 advanceLowDiscrepancy3(float3 &xi) {
  xi = xi + float3(0.8191725133961644f, 0.671043606703789f, //
                   0.5497004779019701f);
  xi.x -= std::floor(xi.x);
  xi.y -= std::floor(xi.y);
  xi.z -= std::floor(xi.z);
  return xi;
}

/// Advance the given quasi-random sample according to a 4-D low
/// discrepancy sequence.
[[nodiscard]] inline float4 advanceLowDiscrepancy4(float4 &xi) {
  xi = xi + float4(0.8566748838545029f, 0.733891856627126f, 0.6287067210378086f,
                   0.53859725722361f);
  xi.x -= std::floor(xi.x);
  xi.y -= std::floor(xi.y);
  xi.z -= std::floor(xi.z);
  xi.w -= std::floor(xi.w);
  return xi;
}

/// Uniform disk PDF.
///
/// \f[ p(\mathbf{X}) = \frac{1}{\pi r^2} \f]
///
[[nodiscard]] inline float uniformDiskPDF(float r = 1) noexcept {
  return 1.0f / (PI * r * r);
}

/// Uniform disk sample using concentric mapping to better preserve
/// stratification.
///
/// \param[in] xi
/// The random sample \f$ \xi \in (0,1)^2 \f$.
///
[[nodiscard]] SMDL_EXPORT float2 uniformDiskSample(float2 xi) noexcept;

/// Cosine-weighted hemisphere direction PDF.
///
/// \f[
///   p(\omega) = \frac{\max(\omega\cdot\hat{z}, 0)}{\pi}
/// \f]
///
/// \param[in] cosTheta
/// The cosine of the sampled direction \f$ \omega\cdot\hat{z} \f$.
///
[[nodiscard]] inline float cosineHemispherePDF(float cosTheta) noexcept {
  return std::max(cosTheta, 0.0f) / PI;
}

/// Cosine-weighted hemisphere direction sample.
///
/// \param[in] xi
/// The random sample \f$ \xi \in (0,1)^2 \f$.
///
[[nodiscard]] inline float3 cosineHemisphereSample(float2 xi) noexcept {
  auto sinTheta{uniformDiskSample(xi)};
  auto cosTheta{std::sqrt(std::max(0.0f, 1.0f - lengthSquared(sinTheta)))};
  return float3(sinTheta.x, sinTheta.y, cosTheta);
}

/// Uniform sphere direction PDF.
///
/// \f[
///   p(\omega) = \frac{1}{4\pi}
/// \f]
///
[[nodiscard]] inline float uniformSpherePDF() noexcept { return 0.25f / PI; }

/// Uniform sphere direction sample.
///
/// \param[in] xi
/// The random sample \f$ \xi \in (0,1)^2 \f$.
///
[[nodiscard]] inline float3 uniformSphereSample(float2 xi) noexcept {
  float cosTheta{std::max(-1.0f, std::min(2.0f * xi.x - 1.0f, 1.0f))};
  float sinTheta{std::sqrt(1.0f - cosTheta * cosTheta)};
  float phi{2.0f * PI * xi.y};
  return float3(sinTheta * std::cos(phi), sinTheta * std::sin(phi), cosTheta);
}

/// Uniform cone direction PDF.
///
/// \f[
///   p(\omega) = \frac{1}{2\pi(1 - \cos\theta_C)}
/// \f]
///
/// \param[in] cosThetaC
/// The cosine of the cone angle \f$ \theta_C \f$.
///
[[nodiscard]] inline float uniformConePDF(float cosThetaC) noexcept {
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
[[nodiscard]] SMDL_EXPORT float3 uniformConeSample(float cosThetaC,
                                                   float2 xi) noexcept;

/// The error function inverse, necessary to sample the standard normal
/// distribution.
[[nodiscard]] SMDL_EXPORT float erfInverse(float y) noexcept;

/// The standard normal distribution PDF.
[[nodiscard]] inline float standardNormalPDF(float x) noexcept {
  return /*1/sqrt(2pi)=*/0.398942280401f * std::exp(-0.5f * x * x);
}

/// The standard normal distribution CDF.
[[nodiscard]] inline float standardNormalCDF(float x) noexcept {
  return 0.5f * (1 + std::erf(/*1/sqrt(2)=*/0.707106781187f * x));
}

/// The standard normal distribution sample.
[[nodiscard]] inline float standardNormalSample(float xi) noexcept {
  return /*sqrt(2)=*/1.41421356237f * erfInverse(2 * xi - 1);
}

/// \}

/// A data-driven distribution in 2 dimensions.
class SMDL_EXPORT Distribution2D final {
public:
  /// Default constructor.
  Distribution2D() = default;

  /// Constructor.
  ///
  /// \param[in] numTexelsX  The number of texels in X.
  /// \param[in] numTexelsY  The number of texels in Y.
  /// \param[in] values      The values in row-major order.
  ///
  explicit Distribution2D(int numTexelsX, int numTexelsY,
                          Span<const float> values);

public:
  /// Clear.
  void clear() noexcept {
    numTexelsX = 0;
    numTexelsY = 0;
    conditionals.clear();
    marginal.clear();
  }

  /// The number of pixels in X.
  [[nodiscard]] int getNumTexelsX() const noexcept { return numTexelsX; }

  /// The number of pixels in Y.
  [[nodiscard]] int getNumTexelsY() const noexcept { return numTexelsY; }

  /// The unnormalized sum over all values.
  [[nodiscard]] float unnormalizedSum() const noexcept {
    return marginal.unnormalizedSum();
  }

  /// The pixel probability mass function (PMF).
  [[nodiscard]] float pixelPMF(int2 i) const noexcept {
    if (0 <= i.y && i.y < numTexelsY)
      return marginal.indexPMF(i.y) * conditionals[i.y].indexPMF(i.x);
    return 0.0f;
  }

  /// The pixel sampling routine.
  ///
  /// \param[in]  xi       The random sample \f$ \xi \in (0,1)^2 \f$.
  /// \param[out] xiRemap  If non-null, receives the remapped random sample.
  /// \param[out] pmf      If non-null, receives the associated PMF.
  ///
  [[nodiscard]] int2 pixelSample(float2 xi, float2 *xiRemap = {},
                                 float *pmf = {}) const noexcept;

  /// The direction PDF.
  ///
  /// \param[in]  wi      The incident direction \f$ \omega_i \f$.
  /// \param[out] iPixel  If non-null, receives the associated pixel index.
  ///
  [[nodiscard]] float directionPDF(float3 wi, int2 *iPixel = {}) const noexcept;

  /// The direction sampling routine.
  ///
  /// \param[in]  xi      The random sample \f$ \xi \in (0,1)^2 \f$.
  /// \param[out] iPixel  If non-null, receives the associated pixel index.
  /// \param[out] pdf     If non-null, receives the associated PDF.
  ///
  [[nodiscard]] float3 directionSample(float2 xi, int2 *iPixel = {},
                                       float *pdf = {}) const noexcept;

private:
  int numTexelsX{};
  int numTexelsY{};
  std::vector<Distribution1D> conditionals{};
  Distribution1D marginal{};
};

/// \}

} // namespace smdl
