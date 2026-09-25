/// \file
#pragma once

#include <algorithm>
#include <cstdint>
#include <random>
#include <vector>

#include "smdl/Export.h"
#include "smdl/Support/Macros.h"
#include "smdl/Support/SIMD.h"
#include "smdl/Support/Span.h"
#include "smdl/Support/VectorMath.h"

namespace smdl {

/// \addtogroup renderutil
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
    mTotalSum = 0;
    mCMFs.clear();
  }

  /// The number of indexes.
  [[nodiscard]] int size() const noexcept { return int(mCMFs.size()) - 1; }

  /// The index probability mass function (PMF). Inline, because a
  /// light sampler asks for it per sample.
  [[nodiscard]] float indexPMF(int i) const noexcept {
    if (0 <= i && i < size())
      return float(INV_CMF_SCALE * double(mCMFs[i + 1] - mCMFs[i]));
    return 0.0f;
  }

  /// The index cumulative mass function (CMF).
  [[nodiscard]] float indexCMF(int i) const noexcept {
    if (0 <= i && i < size()) return float(INV_CMF_SCALE * double(mCMFs[i]));
    return i < 0 ? 0.0f : 1.0f;
  }

  /// The index sampling routine. Never an index with no probability,
  /// even at the bottom of the range, so long as some index has one.
  ///
  /// \param[in]  xi       The random sample \f$ \xi \in (0,1) \f$.
  /// \param[out] xiRemap  If non-null, receives the remapped random sample.
  /// \param[out] pmf      If non-null, receives the associated PMF.
  ///
  [[nodiscard]] int indexSample(float xi, float *xiRemap = {},
                                float *pmf = {}) const noexcept;

  /// The unnormalized sum.
  [[nodiscard]] float unnormalizedSum() const noexcept {
    return static_cast<float>(mTotalSum);
  }

private:
  double mTotalSum{};

  /// The cumulative mass function over the unit interval, as 32-bit
  /// fixed point: half the size of a table of `double`, and unlike a
  /// table of `float` the difference of two entries stays exact, which
  /// is what `indexPMF()` reads.
  std::vector<std::uint32_t> mCMFs{};

  /// The unit of `mCMFs`, `2^-32`.
  static constexpr double INV_CMF_SCALE = 1.0 / 4294967296.0;
};

/// A discrete distribution sampled in constant time (Walker, "New Fast
/// Method for Generating Discrete Random Numbers with Arbitrary Frequency
/// Distributions," Electronics Letters 10(8) 1974; built by Vose's
/// linear-time algorithm).
///
/// The draw is exactly in proportion to the weights. A caller that
/// weighs a draw against a per-entry quantity needs that exactness: a
/// distribution over triangle areas, divided by the area of the face it
/// draws, is uniform over the surface exactly, and the two cancel rather
/// than nearly cancel.
///
/// Two words an entry, against the one `Distribution1D` spends, bought
/// with the binary search that table costs on every draw.
class SMDL_EXPORT AliasTable final {
public:
  AliasTable() = default;

  /// Build over `weights`, in proportion to them. A negative or NaN
  /// weight counts as zero. An empty span, or one summing to zero,
  /// leaves the table empty.
  explicit AliasTable(Span<const float> weights);

  [[nodiscard]] bool empty() const noexcept { return mEntries.empty(); }

  /// The number of indexes.
  [[nodiscard]] int size() const noexcept { return int(mEntries.size()); }

  /// Draw an index in `[0, size())` from the sample `xi` in `(0,1)`.
  ///
  /// The sample is spent twice, on the entry and on the choice between
  /// that entry and its alias. For `xi` uniform the scaled sample's
  /// integer and fractional parts are independent, so this is exact and
  /// costs one dimension where two would do.
  [[nodiscard]] int indexSample(float xi) const noexcept {
    SMDL_SANITY_CHECK(!empty());
    const float scaled{float(size()) * std::clamp(xi, 0.0f, ONE_MINUS_EPS)};
    const int entry{std::clamp(int(scaled), 0, size() - 1)};
    const Entry &e{mEntries[entry]};
    return scaled - float(entry) < e.threshold ? entry : int(e.alias);
  }

private:
  /// One entry: the share of it that stays with the entry itself, and
  /// the index the rest of it is given away to.
  struct Entry final {
    float threshold{1.0f};
    std::uint32_t alias{};
  };

  std::vector<Entry> mEntries{};
};

/// \name Functions (sampling)
/// \{

/// Hold a canonical random sample strictly inside \f$ (0,1) \f$.
///
/// Both endpoints are excluded because a canonical sample is divided by
/// and passed to a logarithm, so this is a guard against zero and one
/// rather than against any particular magnitude: the bound is the
/// smallest normal float only because nothing here needs a smaller one.
[[nodiscard]] SMDL_ALWAYS_INLINE float canonicalize(float xi) noexcept {
  return std::clamp(xi, FLOAT_MIN, ONE_MINUS_EPS);
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

/// Uniform triangle sample, as the barycentric coordinates of the point.
/// The three weights sum to 1 and each is nonnegative, so the caller
/// interpolates whatever the triangle's corners carry.
///
/// \param[in] xi
/// The random sample \f$ \xi \in (0,1)^2 \f$.
///
[[nodiscard]] inline float3 uniformTriangleSample(float2 xi) noexcept {
  const float sqrtXi{std::sqrt(xi.x)};
  return {1.0f - sqrtXi, sqrtXi * (1.0f - xi.y), sqrtXi * xi.y};
}

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
  float2 sinTheta{uniformDiskSample(xi)};
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
///
[[nodiscard]] inline float powerHeuristic(float pdf0, float pdf1) noexcept {
  if (!(pdf0 > 0)) return 0.0f;
  float ratio{pdf1 / pdf0};
  return 1.0f / (1.0f + ratio * ratio);
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
  float cosTheta{std::clamp(2.0f * xi.x - 1.0f, -1.0f, 1.0f)};
  float sinTheta{std::sqrt(1.0f - cosTheta * cosTheta)};
  float phi{2.0f * PI * xi.y};
  return {sinTheta * std::cos(phi), sinTheta * std::sin(phi), cosTheta};
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
[[nodiscard]] inline float3 uniformConeSample(float cosThetaC,
                                              float2 xi) noexcept {
  float cosTheta{(1.0f - xi.x) * cosThetaC + xi.x};
  if (cosTheta < -1.0f) cosTheta = -1.0f;
  if (cosTheta > +1.0f) cosTheta = +1.0f;
  float sinTheta{std::sqrt(std::max(1.0f - cosTheta * cosTheta, 0.0f))};
  float phi{TWO_PI * xi.y};
  return {sinTheta * std::cos(phi), sinTheta * std::sin(phi), cosTheta};
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
[[nodiscard]] inline float standardNormalSample(float xi) noexcept {
  return /*sqrt(2)=*/1.41421356237f * erfInverse(2 * xi - 1);
}

/// \}

namespace simd {

/// The inverse error function of every element, each in \f$ [-1, 1] \f$,
/// to 5e-7 relative, exactly odd, and nondecreasing up to 2 ulp of the
/// result, which is what keeps stratified samples stratified through
/// `standardNormalSample`.
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
/// canonical sample in \f$ (0,1) \f$; the packed `smdl::standardNormalSample`.
template <std::size_t N>
[[nodiscard]] inline Pack<float, N>
standardNormalSample(const Pack<float, N> &xi) noexcept {
  using P = Pack<float, N>;
  return P(/*sqrt(2)=*/1.41421356237f) * erfInverse(P(2.0f) * xi - P(1.0f));
}

} // namespace simd

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
    mNumTexelsX = 0;
    mNumTexelsY = 0;
    mConditionals.clear();
    mMarginal.clear();
  }

  /// The number of pixels in X.
  [[nodiscard]] int getNumTexelsX() const noexcept { return mNumTexelsX; }

  /// The number of pixels in Y.
  [[nodiscard]] int getNumTexelsY() const noexcept { return mNumTexelsY; }

  /// The unnormalized sum over all values.
  [[nodiscard]] float unnormalizedSum() const noexcept {
    return mMarginal.unnormalizedSum();
  }

  /// The pixel probability mass function (PMF).
  [[nodiscard]] float pixelPMF(int2 i) const noexcept {
    if (0 <= i.y && i.y < mNumTexelsY)
      return mMarginal.indexPMF(i.y) * mConditionals[i.y].indexPMF(i.x);
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
  /// The pixel and the density both come back from `directionPDF()` of
  /// the direction returned, rather than from the pixel the draw picked.
  /// The two disagree whenever rounding puts the direction across a pixel
  /// boundary from the rectangle it was drawn in, and where the
  /// neighboring pixel is much darker the density reported that way is
  /// wrong by that whole ratio, which multiple importance sampling then
  /// divides by. Reporting the density of the direction in hand costs one
  /// more arccosine and arctangent and makes the two exactly agree.
  ///
  /// A direction whose recovered density is zero comes back with a pdf of
  /// zero and a pixel index of `(-1, -1)`, which callers already have to
  /// handle: it means this direction cannot be drawn, so the sample
  /// carries nothing.
  ///
  /// \param[in]  xi      The random sample \f$ \xi \in (0,1)^2 \f$.
  /// \param[out] iPixel  If non-null, receives the associated pixel index.
  /// \param[out] pdf     If non-null, receives the associated PDF.
  ///
  [[nodiscard]] float3 directionSample(float2 xi, int2 *iPixel = {},
                                       float *pdf = {}) const noexcept;

private:
  int mNumTexelsX{};
  int mNumTexelsY{};
  std::vector<Distribution1D> mConditionals{};
  Distribution1D mMarginal{};
};

/// \}

} // namespace smdl
