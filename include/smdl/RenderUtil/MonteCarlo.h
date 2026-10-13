/// \file
#pragma once

#include <algorithm>
#include <cmath>
#include <random>
#include <type_traits>
#include <utility>

#include "smdl/Export.h"
#include "smdl/Support/Macros.h"
#include "smdl/Support/SIMD.h"
#include "smdl/Support/VectorMath.h"

namespace smdl {

/// \addtogroup renderutil
/// \{

/// \name Functions (sampling)
/// \{

/// Hold a canonical random sample strictly inside \f$ (0,1) \f$.
///
/// Both endpoints are excluded because a canonical sample is divided by
/// and passed to a logarithm, so this is a guard against zero and one
/// rather than against any particular magnitude: the bound is the
/// smallest normal float only because nothing here needs a smaller one.
[[nodiscard]] SMDL_ALWAYS_INLINE float canonicalize(float xi) noexcept {
  return std::clamp(xi, FLOAT_MIN, ALMOST_ONE);
}

/// Generate canonical random sample in \f$ (0,1) \f$.
template <typename G> [[nodiscard]] inline float generateCanonical(G &g) {
  return canonicalize(std::generate_canonical<float, 32>(g));
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

/// Uniform disk PDF.
[[nodiscard]] inline float uniformDiskPDF(float r = 1) noexcept {
  return 1.0f / (PI * r * r);
}

/// Uniform disk sample using concentric mapping to better preserve
/// stratification.
[[nodiscard]] SMDL_EXPORT float2 uniformDiskDraw(float2 xi) noexcept;

/// Uniform triangle sample, as the barycentric coordinates of the point.
/// The three weights sum to 1 and each is nonnegative, so the caller
/// interpolates whatever the triangle's corners carry.
[[nodiscard]] inline float3 uniformTriangleDraw(float2 xi) noexcept {
  const float sqrtXi{std::sqrt(xi.x)};
  return {1.0f - sqrtXi, sqrtXi * (1.0f - xi.y), sqrtXi * xi.y};
}

/// Cosine-weighted hemisphere direction PDF.
[[nodiscard]] inline float cosineHemispherePDF(float cosTheta) noexcept {
  return std::max(cosTheta, 0.0f) / PI;
}

/// Cosine-weighted hemisphere direction sample.
[[nodiscard]] inline float3 cosineHemisphereDraw(float2 xi) noexcept {
  float2 sinTheta{uniformDiskDraw(xi)};
  float cosTheta{std::sqrt(std::max(0.0f, 1.0f - lengthSquared(sinTheta)))};
  return {sinTheta.x, sinTheta.y, cosTheta};
}

/// The power heuristic with \f$ \beta = 2 \f$ for two sampling strategies,
/// the multiple-importance-sampling weight of a sample drawn from the
/// strategy with density `pdf0` against a competing strategy with density
/// `pdf1` (Veach & Guibas, SIGGRAPH 1995).
///
/// This is written as \f$ 1/(1+(q/p)^2) \f$ rather than the equivalent
/// \f$ p^2/(p^2+q^2) \f$ to avoid overflowing on the enormous PDFs that
/// near-specular lobes produce.
[[nodiscard]] inline float powerHeuristic(float pdf0, float pdf1) noexcept {
  if (!(pdf0 > 0)) return 0.0f;
  float ratio{pdf1 / pdf0};
  return 1.0f / (1.0f + ratio * ratio);
}

/// Uniform sphere direction PDF.
[[nodiscard]] inline float uniformSpherePDF() noexcept { return 0.25f / PI; }

/// Uniform sphere direction sample.
[[nodiscard]] inline float3 uniformSphereDraw(float2 xi) noexcept {
  float cosTheta{std::clamp(2.0f * xi.x - 1.0f, -1.0f, 1.0f)};
  float sinTheta{std::sqrt(1.0f - cosTheta * cosTheta)};
  float phi{2.0f * PI * xi.y};
  return {sinTheta * std::cos(phi), sinTheta * std::sin(phi), cosTheta};
}

/// The solid angle of a cone whose half angle has versine `oneMinusCos`,
/// which is what a draw over it is weighed by; see
/// `uniformConeDrawVersine()`.
template <typename T>
[[nodiscard]] SMDL_ALWAYS_INLINE T coneSolidAngle(T oneMinusCos) noexcept {
  static_assert(std::is_floating_point_v<T>);
  return T(TWO_PI) * oneMinusCos;
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
  return 1.0f / coneSolidAngle(1.0f - cosThetaC);
}

/// Uniform cone direction sample.
///
/// \param[in] cosThetaC
/// The cosine of the cone angle \f$ \theta_C \f$.
///
/// \param[in] xi
/// The random sample \f$ \xi \in (0,1)^2 \f$.
///
[[nodiscard]] inline float3 uniformConeDraw(float cosThetaC,
                                            float2 xi) noexcept {
  float cosTheta{(1.0f - xi.x) * cosThetaC + xi.x};
  if (cosTheta < -1.0f) cosTheta = -1.0f;
  if (cosTheta > +1.0f) cosTheta = +1.0f;
  float sinTheta{std::sqrt(std::max(1.0f - cosTheta * cosTheta, 0.0f))};
  float phi{TWO_PI * xi.y};
  return {sinTheta * std::cos(phi), sinTheta * std::sin(phi), cosTheta};
}

/// A direction drawn uniformly over a cone, in the cone's own frame with
/// +z toward the middle: `oneMinusCos` is the versine of its half angle,
/// and `xiX` and `xiY` the pair the draw spends.
///
/// Parametrized on the versine and not on the cosine, as
/// `uniformConeDraw()` is, which is what lets one sampler serve a cone
/// of any size. A cone the size of the sun's disk has a cosine within a
/// few ulps of one, so a draw that interpolates the cosine covers
/// whatever solid angle those few representable values happen to span
/// rather than the one its density is written for. Here the transverse
/// components carry the angle instead, and they hold it at any size; the
/// `z` rounding to one is the cosine of a very small angle honestly
/// reported.
template <typename T>
[[nodiscard]] SMDL_ALWAYS_INLINE Vector<T, 3>
uniformConeDrawVersine(T oneMinusCos, T xiX, T xiY) noexcept {
  static_assert(std::is_floating_point_v<T>);
  const T versine{xiX * oneMinusCos};
  const T sinTheta{std::sqrt(versine * (T(2) - versine))};
  const T phi{T(TWO_PI) * xiY};
  return {sinTheta * std::cos(phi), sinTheta * std::sin(phi), T(1) - versine};
}

/// The inverse error function on \f$ [-1, 1] \f$, necessary to sample the
/// standard normal distribution: the width-one instance of
/// `simd::erfInverse`, which states the approximation and its bounds.
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
[[nodiscard]] inline float standardNormalDraw(float xi) noexcept {
  return /*sqrt(2)=*/1.41421356237f * erfInverse(2 * xi - 1);
}

/// Two independent standard normal samples from two canonical samples,
/// the first in \f$ (0,1] \f$ so that its logarithm is finite, by the
/// Box-Muller transform in double precision.
///
/// For a caller that needs the tails as they are, which the
/// single-precision `erfInverse()` behind `standardNormalDraw()` holds
/// to 5e-7 relative and no further out than its last float allows. The
/// price is stratification: the pair is not a monotone image of either
/// sample.
[[nodiscard]] inline std::pair<double, double>
standardNormalDrawPair(double xi0, double xi1) noexcept {
  const double r{std::sqrt(-2.0 * std::log(xi0))};
  const double theta{/*2pi=*/6.283185307179586476925 * xi1};
  return {r * std::cos(theta), r * std::sin(theta)};
}

/// \}

namespace simd {

/// The inverse error function of every element, each in \f$ [-1, 1] \f$,
/// to 5e-7 relative, exactly odd, and nondecreasing up to 2 ulp of the
/// result, which is what keeps stratified samples stratified through
/// `standardNormalDraw`.
///
/// Giles' single-precision approximation (M. Giles, "Approximating the
/// erfinv function", GPU Computing Gems Jade Edition, 2011): with
/// \f$ w = -\ln((1 - y)(1 + y)) \f$, `y` times a polynomial in `w` below
/// `w = 5` and one in \f$ \sqrt{w} \f$ above it, the two differing by 1e-7
/// where they meet. Every lane evaluates both and selects, in Estrin's form
/// rather than Horner's, because what bounds a caller drawing sample after
/// sample is the longest chain of dependent multiplies, and Estrin's form
/// halves it.
///
/// The product is held at \f$ 2^{-24} \f$, half the smallest any float
/// inside the interval makes, so an end of the interval continues the
/// curve (`y = 1` gives 3.92, the last float below it 3.83) rather than
/// running the tail polynomial out to where it no longer holds.
template <std::size_t N>
[[nodiscard]] inline Pack<float, N>
erfInverse(const Pack<float, N> &y) noexcept {
  using P = Pack<float, N>;
  const P w{-log(max((P(1.0f) - y) * (P(1.0f) + y), P(0x1p-24f)))};
  const P u{w - P(2.5f)};
  const P u2{u * u};
  const P u4{u2 * u2};
  const P central{(P(1.50140941f) + P(2.46640727e-1f) * u) +
                  u2 * (P(-4.17768164e-3f) + P(-1.25372503e-3f) * u) +
                  u4 * ((P(2.18580870e-4f) + P(-4.39150654e-6f) * u) +
                        u2 * (P(-3.52338770e-6f) + P(3.43273939e-7f) * u)) +
                  (u4 * u4) * P(2.81022636e-8f)};
  const P v{sqrt(w) - P(3.0f)};
  const P v2{v * v};
  const P v4{v2 * v2};
  const P tail{(P(2.83297682f) + P(1.00167406f) * v) +
               v2 * (P(9.43887047e-3f) + P(-7.62246130e-3f) * v) +
               v4 * ((P(5.73950773e-3f) + P(-3.67342844e-3f) * v) +
                     v2 * (P(1.34934322e-3f) + P(1.00950558e-4f) * v)) +
               (v4 * v4) * P(-2.00214257e-4f)};
  return y * select(w < P(5.0f), central, tail);
}

/// The standard normal distribution sample of every element, each a
/// canonical sample in \f$ (0,1) \f$; the packed `smdl::standardNormalDraw`.
template <std::size_t N>
[[nodiscard]] inline Pack<float, N>
standardNormalDraw(const Pack<float, N> &xi) noexcept {
  using P = Pack<float, N>;
  return P(/*sqrt(2)=*/1.41421356237f) * erfInverse(P(2.0f) * xi - P(1.0f));
}

} // namespace simd

/// \}

} // namespace smdl
