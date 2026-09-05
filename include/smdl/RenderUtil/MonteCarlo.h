/// \file
#pragma once

#include <algorithm>
#include <array>
#include <cstdint>
#include <random>

#include "smdl/Export.h"
#include "smdl/Support/Macros.h"
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

  /// The cumulative mass function over the unit interval, as 32-bit
  /// fixed point: half the size of a table of `double`, and unlike a
  /// table of `float` the difference of two entries stays exact, which
  /// is what `indexPMF()` reads.
  std::vector<std::uint32_t> cmfs{};
};

/// \name Functions (sampling)
/// \{

/// Generate canonical random sample in \f$ (0,1) \f$.
template <typename G> [[nodiscard]] inline float generateCanonical(G &g) {
  return std::clamp(std::generate_canonical<float, 32>(g),
                    std::numeric_limits<float>::min(), ONE_MINUS_EPS);
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
[[nodiscard]] SMDL_EXPORT float3 uniformConeSample(float cosThetaC,
                                                   float2 xi) noexcept;

/// Uniform aperture sample.
///
/// \param[in] numBlades
/// The number of blades, should be at least 3. The implementation
/// falls back to the unit disk otherwise.
///
/// \param[in] bladeAngle
/// The blade offset angle in radians. Passing zero aligns a regular polygon
/// vertex to the +X axis.
///
/// \param[in] xi
/// The random sample \f$ \xi \in (0,1)^2 \f$.
///
[[nodiscard]] SMDL_EXPORT float2 uniformApertureSample(int numBlades,
                                                       float bladeAngle,
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

/// \name Functions (quasi-Monte Carlo)
/// \{

/// The murmur3 finalizer, spreading every input bit over the whole word.
[[nodiscard]] inline uint32_t mixBits(uint32_t x) noexcept {
  x ^= x >> 16;
  x *= 0x85EBCA6BU;
  x ^= x >> 13;
  x *= 0xC2B2AE35U;
  x ^= x >> 16;
  return x;
}

/// The splitmix64 finalizer, the 64-bit analogue of `mixBits(uint32_t)`.
/// Overload resolution needs an exact-width argument: in particular a
/// `ULL` literal is `unsigned long long`, which is ambiguous against the
/// 32-bit overload on LP64, so cast to `uint64_t` first.
[[nodiscard]] inline uint64_t mixBits(uint64_t x) noexcept {
  x ^= x >> 30;
  x *= 0xBF58476D1CE4E5B9ULL;
  x ^= x >> 27;
  x *= 0x94D049BB133111EBULL;
  x ^= x >> 31;
  return x;
}

/// Reverse the order of the bits.
[[nodiscard]] inline uint32_t reverseBits(uint32_t x) noexcept {
  x = (x << 16) | (x >> 16);
  x = ((x & 0x00FF00FFU) << 8) | ((x & 0xFF00FF00U) >> 8);
  x = ((x & 0x0F0F0F0FU) << 4) | ((x & 0xF0F0F0F0U) >> 4);
  x = ((x & 0x33333333U) << 2) | ((x & 0xCCCCCCCCU) >> 2);
  x = ((x & 0x55555555U) << 1) | ((x & 0xAAAAAAAAU) >> 1);
  return x;
}

/// The hash-based Owen scramble (Burley, "Practical Hash-Based Owen
/// Scrambling," JCGT 9(4) 2020): reverse so the high (most significant)
/// bits sit low, run the Laine-Karras permutation, which only lets each
/// bit affect bits above it, and reverse back. Inputs agreeing in their
/// most significant bits therefore map to outputs agreeing in at least as
/// many most significant bits, which is what preserves net structure in
/// scrambled low-discrepancy points.
[[nodiscard]] inline uint32_t nestedUniformScramble(uint32_t x,
                                                    uint32_t seed) noexcept {
  x = reverseBits(x);
  x += seed;
  x ^= x * 0x6C50B47CU;
  x ^= x * 0xB82F1E52U;
  x ^= x * 0xC7AFE638U;
  x ^= x * 0x8D22F6E6U;
  return reverseBits(x);
}

/// The second Sobol dimension at `index`. (The first Sobol dimension is
/// just `reverseBits`.)
[[nodiscard]] inline uint32_t sobolDim1(uint32_t index) noexcept {
  static constexpr std::array<uint32_t, 32> directions = {
      0x80000000U, 0xC0000000U, 0xA0000000U, 0xF0000000U, //
      0x88000000U, 0xCC000000U, 0xAA000000U, 0xFF000000U, //
      0x80800000U, 0xC0C00000U, 0xA0A00000U, 0xF0F00000U, //
      0x88880000U, 0xCCCC0000U, 0xAAAA0000U, 0xFFFF0000U, //
      0x80008000U, 0xC000C000U, 0xA000A000U, 0xF000F000U, //
      0x88008800U, 0xCC00CC00U, 0xAA00AA00U, 0xFF00FF00U, //
      0x80808080U, 0xC0C0C0C0U, 0xA0A0A0A0U, 0xF0F0F0F0U, //
      0x88888888U, 0xCCCCCCCCU, 0xAAAAAAAAU, 0xFFFFFFFFU};
  // Fixed trip count with a mask instead of a data-dependent branch: the
  // callers pass scrambled indexes whose bits are coin flips, so the branch
  // mispredicts every other bit and dominates the sampler's cost, while the
  // masked form unrolls and vectorizes.
  uint32_t X{};
  for (int bit = 0; bit < 32; bit++)
    X ^= directions[bit] & (0U - ((index >> bit) & 1U));
  return X;
}

/// \}

/// A hash-based Owen-scrambled Sobol sampler after Burley, "Practical
/// Hash-Based Owen Scrambling," JCGT 9(4) 2020.
///
/// Each (seed, index) pair yields a deterministic low-discrepancy point
/// sequence consumed two dimensions at a time; the seed selects the
/// sequence and the index the point within it. Every 2D pair reuses the
/// first two Sobol dimensions with an independently hashed index shuffle
/// and per-dimension Owen scramble, which keeps each pair's stratification
/// while decorrelating the pairs from one another, so the sequence
/// extends to arbitrarily many dimensions with no direction-number tables
/// beyond the second dimension's.
///
/// The stratification is per 2D pair and lives entirely in the dimension
/// counter: a pair is stratified across the point indexes of a seed only
/// when every index reaches it at the same dimension, aligned on an even
/// one. A caller that consumes a data-dependent count therefore costs
/// every draw after it, not just its own. None of the draw methods
/// realign implicitly; when to call `alignPair()` is caller policy.
class OwenSobolSampler final {
public:
  OwenSobolSampler() = default;

  /// Begin the point `index` of the sequence selected by `seed`,
  /// resetting the dimension counter.
  void start(uint32_t seed, uint32_t index) noexcept {
    mSeedHash = mixBits(seed);
    mIndex = index;
    mDimension = 0;
  }

  /// Generates the next scrambled sample as raw bits, advancing the
  /// dimension counter.
  [[nodiscard]] uint32_t generate() noexcept {
    const uint32_t pair{mDimension >> 1};
    const uint32_t component{mDimension & 1};
    ++mDimension;
    const uint32_t seed{mixBits(mSeedHash ^ (0x9E3779B9U * pair))};
    const uint32_t shuffledIndex{nestedUniformScramble(mIndex, seed)};
    const uint32_t X{component == 0 ? reverseBits(shuffledIndex)
                                    : sobolDim1(shuffledIndex)};
    return nestedUniformScramble(X, mixBits(seed ^ (0x55555555U + component)));
  }

  /// Generates the next canonical sample in `(0,1)`.
  [[nodiscard]] float generateFloat() noexcept {
    return std::clamp(float(generate()) * 0x1p-32f,
                      std::numeric_limits<float>::min(), ONE_MINUS_EPS);
  }

  /// Generates the next 2 canonical samples in `(0,1)^2`.
  [[nodiscard]] float2 generateFloat2() noexcept {
    return {generateFloat(), generateFloat()};
  }

  /// Generates the next 3 canonical samples in `(0,1)^3`.
  [[nodiscard]] float3 generateFloat3() noexcept {
    return {generateFloat(), generateFloat(), generateFloat()};
  }

  /// Generates the next 4 canonical samples in `(0,1)^4`.
  [[nodiscard]] float4 generateFloat4() noexcept {
    return {generateFloat(), generateFloat(), generateFloat(), generateFloat()};
  }

  /// Round the dimension counter up to a pair boundary, so that the next
  /// draw begins a jointly stratified 2D pair. The skipped dimension
  /// costs nothing, the pairs being padded rather than consecutive Sobol
  /// dimensions.
  void alignPair() noexcept { mDimension = (mDimension + 1U) & ~1U; }

  /// The dimension counter, i.e., the number of dimensions consumed.
  [[nodiscard]] uint32_t dimension() const noexcept { return mDimension; }

private:
  /// The hashed sequence-selecting seed.
  uint32_t mSeedHash{};

  /// The point index.
  uint32_t mIndex{};

  /// The dimension counter.
  uint32_t mDimension{};
};

/// \}

} // namespace smdl
