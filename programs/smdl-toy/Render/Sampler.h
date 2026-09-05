/// \file
/// The rendering sampler: the draw policy around the library's
/// `smdl::OwenSobolSampler`, the version tag a resumable output records
/// so that a continuation knows which sequence it continues, and the
/// wavelength jitter offset, which is deliberately drawn outside the
/// sampler.
#pragma once

#include <algorithm>
#include <cmath>
#include <limits>

#include "smdl/RenderUtil/MonteCarlo.h"
#include "smdl/Support/Macros.h"
#include "smdl/Support/RNG.h"

#include "Common.h"

/// Selects the sampler implementation. Zero, the default, is the
/// hash-based Owen-scrambled Sobol sequence; nonzero substitutes plain
/// PCG32, which is unstratified but unquestionably independent, so that
/// a suspect result can be A/B'd against an estimator whose only claim
/// is uniformity. Define it on the compiler command line
/// (`-DSMDL_TOY_SAMPLER_PCG32=1`) and rebuild.
#ifndef SMDL_TOY_SAMPLER_PCG32
#define SMDL_TOY_SAMPLER_PCG32 0
#endif

/// The sampler version tag written into resumable output metadata. A
/// resumed render continues the sampler's deterministic (pixel, sample
/// index) sequence, so after a change to the sampling scheme the
/// continuation samples are merely independent of the first session's
/// rather than jointly stratified with them; still unbiased, but worth
/// a warning. Bump this whenever the sequence changes.
constexpr const char *SAMPLER_VERSION =
#if SMDL_TOY_SAMPLER_PCG32
    "pcg32-1";
#else
    "owen-sobol-1";
#endif

/// The rendering sampler: the draw policy around the library's
/// `smdl::OwenSobolSampler`.
///
/// Draws happen through the vector conversion operators, which quantize
/// every draw to a whole Sobol pair so that a 2D draw is jointly
/// stratified instead of straddling two independently scrambled pairs.
/// The skipped dimensions cost nothing, the pairs being padded rather
/// than consecutive Sobol dimensions, and the joint 2D stratification
/// dominates convergence wherever the integrand is smooth, so the
/// quantization is unconditional. A pair is stratified across the
/// samples of a pixel only when every sample reaches it at the same
/// dimension, so a caller that consumes a data-dependent count costs
/// every draw after it, not just its own. Under `SMDL_TOY_SAMPLER_PCG32`
/// none of this applies and the draws are simply independent.
///
/// The draw path is left to the inliner, which inlines everything but
/// `smdl::OwenSobolSampler::generate()`, so every draw is one call.
/// Forcing that call inline too was measured and rejected: it removes
/// about 2% of the instructions and still renders 1.4% slower on the
/// sky-lit scenes, because the hashing lands at every draw site and the
/// hot code outgrows the front end, a quarter more front-end stall
/// cycles spread over code that did not change.
class Sampler final {
public:
  Sampler() = default;

  /// Begin the sample `sampleIndex` of the pixel `pixelIndex`, resetting
  /// the dimension counter.
  void startPixelSample(uint32_t pixelIndex, uint32_t sampleIndex) noexcept {
#if SMDL_TOY_SAMPLER_PCG32
    // Seeded rather than strided so a resumed or windowed render
    // reproduces the same draws for the same (pixel, sample) the
    // low-discrepancy sequence does. The stream keeps two pixels whose
    // seeds happen to collide on separate sequences anyway.
    mRng = smdl::RNG(
        smdl::mixBits((uint64_t(pixelIndex) << 32) | uint64_t(sampleIndex)),
        smdl::mixBits(uint64_t(0x9E3779B97F4A7C15ULL) ^ uint64_t(pixelIndex)));
#else
    mSobol.start(pixelIndex, sampleIndex);
#endif
  }

  [[nodiscard]] explicit operator float() {
    alignPair();
    const float xi{next()};
    alignPair();
    return xi;
  }

  [[nodiscard]] explicit operator float2() {
    alignPair();
    const float x{next()};
    return {x, next()};
  }

  [[nodiscard]] explicit operator float3() {
    alignPair();
    const float x{next()}, y{next()}, z{next()};
    alignPair();
    return {x, y, z};
  }

  [[nodiscard]] explicit operator float4() {
    alignPair();
    const float x{next()}, y{next()}, z{next()};
    return {x, y, z, next()};
  }

  [[nodiscard]] int index(int n) {
    SMDL_SANITY_CHECK(n > 0);
    return std::clamp(int(std::floor(float(n) * float(*this))), 0, n - 1);
  }

  /// The next sample as raw bits.
  [[nodiscard]] uint32_t nextBits() noexcept {
#if SMDL_TOY_SAMPLER_PCG32
    return mRng.generate();
#else
    return mSobol.generate();
#endif
  }

private:
  /// Round the dimension up to a pair boundary, called before and after
  /// every draw so that none of them straddles two pairs.
  void alignPair() noexcept {
#if !SMDL_TOY_SAMPLER_PCG32
    mSobol.alignPair();
#endif
  }

  /// The next canonical sample in `(0,1)`.
  [[nodiscard]] float next() noexcept {
    return smdl::canonicalFromBits(nextBits());
  }

#if SMDL_TOY_SAMPLER_PCG32
  smdl::RNG mRng{};
#else
  smdl::OwenSobolSampler mSobol{};
#endif
};

/// The 64-bit seed of two consecutive draws, high half first.
///
/// Spelled out rather than written inline as `(next() << 32) | next()`,
/// because `operator|` does not sequence its operands: which draw lands
/// in the high half would be the compiler's choice, and the two draws
/// are different dimensions of the sequence.
[[nodiscard]] SMDL_ALWAYS_INLINE uint64_t
nextSeed64(Sampler &sampler) noexcept {
  const uint64_t hi{sampler.nextBits()};
  const uint64_t lo{sampler.nextBits()};
  return (hi << 32) | lo;
}

/// The `-wavelength-jitter` offset of one sample: the Owen-scrambled
/// radical inverse of the sample index, seeded per pixel.
///
/// Deliberately drawn outside `Sampler`, whose draws quantize to Sobol
/// pairs, so that turning the jitter on shifts no path dimension and an
/// A/B of the flag isolates the jitter alone. Keyed on the absolute
/// sample index, the same one `startPixelSample()` takes, so a resumed
/// session continues the sequence where the last one left off.
[[nodiscard]] inline float
wavelengthJitterOffset(uint32_t pixelIndex, uint32_t sampleIndex) noexcept {
  return smdl::canonicalFromBits(smdl::nestedUniformScramble(
      smdl::reverseBits(sampleIndex),
      smdl::mixBits(pixelIndex ^ uint32_t(0x5CE4B17DU))));
}
