#include "smdl/RenderUtil/Haze.h"

#include <algorithm>
#include <cmath>

namespace smdl {

// The Koschmieder constant: the meteorological range is the distance at
// which a black target against the horizon falls to the 2 percent
// contrast threshold, so the extinction it names is -ln(0.02) over it.
static constexpr float KOSCHMIEDER{3.912f};

// The wavelength the visibility is quoted at, in nanometers.
static constexpr float REFERENCE_WAVELENGTH{550.0f};

// The largest exponent `extinctionAt` will raise the reference
// extinction by, which bounds a scene whose origin sits far below the
// reference height instead of letting it become an opaque wall.
static constexpr float MAX_EXPONENT{30.0f};

MiePhase::MiePhase(float dropletSize) {
  // The piecewise diameter-to-parameter fits, supplemental sections 3.1
  // through 3.4 of Jendersie and d'Eon, natural logarithms throughout;
  // the seams between the ranges agree only approximately, like the fits
  // themselves. Kept identical to `df::fog_vdf`.
  const float d{std::clamp(dropletSize, 0.0f, 50.0f)};
  if (d <= 0.1f) {
    mGHG = 13.8f * d * d;
    mGD = 1.1456f * d * std::sin(9.29044f * d);
    mAlpha = 250.0f;
    mWD = 0.252977f - 312.983f * std::pow(d, 4.3f);
  } else if (d < 1.5f) {
    const float logD{std::log(d)};
    mGHG = 0.862f - 0.143f * logD * logD;
    mGD = 0.379685f *
              std::cos(1.19692f *
                           std::cos((logD - 0.238604f) * (logD + 1.00667f) /
                                    (0.507522f - 0.15677f * logD)) +
                       1.37932f * logD + 0.0625835f) +
          0.344213f;
    mAlpha = 250.0f;
    mWD = 0.146209f * std::cos(3.38707f * logD + 2.11193f) + 0.316072f +
          0.0778917f * logD;
  } else if (d < 5.0f) {
    const float logD{std::log(d)};
    mGHG = 0.0604931f * std::log(logD) + 0.940256f;
    mGD = 0.500411f - 0.081287f / (-2.0f * logD + std::tan(logD) + 1.27551f);
    mAlpha = 7.30354f * logD + 6.31675f;
    mWD = 0.026914f *
              (logD - std::cos(5.68947f * (std::log(logD) - 0.0292149f))) +
          0.376475f;
  } else {
    mGHG = std::exp(-0.0990567f / (d - 1.67154f));
    mGD = std::exp(-2.20679f / (d + 3.91029f) - 0.428934f);
    mAlpha = std::exp(3.62489f - 8.29288f / (d + 5.52825f));
    mWD = std::exp(-0.599085f / (d - 0.641583f) - 0.665888f);
  }
  mWD = std::clamp(mWD, 0.0f, 1.0f);
}

// The Henyey-Greenstein phase function of the deflection cosine `u`.
[[nodiscard]] static float hgPhase(float g, float u) noexcept {
  const float denom{1.0f + g * g - 2.0f * g * u};
  return denom > 0.0f ? (1.0f - g * g) / (4.0f * PI * denom * std::sqrt(denom))
                      : 0.0f;
}

// The Draine phase function of the deflection cosine `u`, which is the
// Henyey-Greenstein lobe reshaped by `a` and renormalized.
[[nodiscard]] static float drainePhase(float g, float a, float u) noexcept {
  return hgPhase(g, u) * (1.0f + a * u * u) /
         (1.0f + a * (1.0f + 2.0f * g * g) / 3.0f);
}

// Sample the deflection cosine of the Henyey-Greenstein phase function
// by analytic CDF inversion.
[[nodiscard]] static float hgSample(float g, float xi) noexcept {
  if (std::abs(g) < 1e-3f) return 1.0f - 2.0f * xi;
  const float t{(1.0f - g * g) / (1.0f - g + 2.0f * g * xi)};
  return (1.0f + g * g - t * t) / (2.0f * g);
}

// Sample the deflection cosine of the Draine phase function by analytic
// CDF inversion: the quartic solution of the Jendersie and d'Eon
// supplemental, in the numerically robust factoring the builtin
// `df::_fogDraineSample` carries, evaluated in double because the
// factoring cancels heavily.
[[nodiscard]] static float draineSample(float gf, float af,
                                        float xif) noexcept {
  // Degenerates to Henyey-Greenstein as `alpha` vanishes, where the
  // quartic terms divide by zero.
  if (af < 1e-3f) return hgSample(gf, xif);
  const double g{gf}, a{af}, xi{xif};
  if (std::abs(g) < 0.01) {
    // Near-isotropic: the `g` of zero CDF is a depressed cubic with one
    // real root by Cardano. Using it below this threshold costs a CDF
    // error of order `g`, following the reference implementation; the
    // fitted anisotropy only lands here for particles under about 0.03
    // micrometers.
    const double invA{1.0 / a};
    const double b2{(3.0 + a) * invA * (0.5 - xi)};
    const double invU{-1.0 /
                      std::cbrt(b2 + std::sqrt(b2 * b2 + invA * invA * invA))};
    return float(1.0 / invU - invU * invA);
  }
  const double g2{g * g}, g3{g * g2}, g4{g2 * g2}, g6{g2 * g4};
  const double onePlusG2{1.0 + g2};
  const double t1a{a * (g4 - 1.0)};
  const double t2{-1296.0 * (g2 - 1.0) * (a - a * g2) * t1a *
                  (4.0 * g2 + a * onePlusG2 * onePlusG2)};
  const double t9{2.0 + g2 + g3 * (1.0 + 2.0 * g2) * (2.0 * xi - 1.0)};
  const double t3{3.0 * g2 * (1.0 + g * (2.0 * xi - 1.0)) + a * t9};
  const double t4a{432.0 * t1a * t1a * t1a + t2 +
                   432.0 * (a * (1.0 - g2)) * t3 * t3};
  const double t10{a * (2.0 * g4 - g2 - g6)};
  const double t4b{144.0 * t10};
  const double t4{t4a +
                  std::sqrt(std::max(-4.0 * t4b * t4b * t4b + t4a * t4a, 0.0))};
  const double invT4p3{1.0 / std::cbrt(std::max(t4, 1e-30))};
  constexpr double CBRT2{1.2599210498948732};
  const double t8{48.0 * CBRT2 * t10};
  const double t6{(2.0 * t1a + t8 * invT4p3 + 1.0 / (3.0 * CBRT2 * invT4p3)) /
                  (a * (1.0 - g2))};
  const double t5{6.0 * onePlusG2 + t6};
  const double t7{
      6.0 * onePlusG2 -
      (8.0 * t3) / (a * (g2 - 1.0) * std::sqrt(std::max(t5, 1e-30))) - t6};
  const double s{std::sqrt(std::max(t7, 0.0)) - std::sqrt(std::max(t5, 0.0))};
  return float(std::clamp((1.0 + g2 - 0.25 * s * s) / (2.0 * g), -1.0, 1.0));
}

float MiePhase::evaluate(float u) const noexcept {
  return (1.0f - mWD) * hgPhase(mGHG, u) + mWD * drainePhase(mGD, mAlpha, u);
}

float MiePhase::sample(float3 xi, const float3 &wo, float3 &wi) const noexcept {
  // Pick the lobe by the mixture weight and sample it exactly, so the
  // mixture density of the sampled direction is the phase value itself.
  const float u{xi.z < mWD ? draineSample(mGD, mAlpha, xi.x)
                           : hgSample(mGHG, xi.x)};
  const float sinTheta{std::sqrt(std::max(0.0f, 1.0f - u * u))};
  const float phi{TWO_PI * xi.y};
  wi = coordinateSystem(wo) *
       float3(sinTheta * std::cos(phi), sinTheta * std::sin(phi), -u);
  return evaluate(u);
}

Haze::Haze(const HazeOptions &options, Span<const float> wavelens,
           float metersPerSceneUnit, HazeSun sun)
    : mAlbedo(std::clamp(options.albedo, 0.0f, 1.0f)),
      mInvScaleHeight(metersPerSceneUnit /
                      std::max(options.scaleHeight, 1e-3f)),
      mBaseHeight(options.baseHeight), mPhase(MiePhase(options.dropletSize)),
      mSun(std::move(sun)) {
  // Koschmieder fixes the extinction at the reference wavelength; the
  // Angstrom exponent carries it across the spectrum. Coefficients are
  // in inverse meters, and distances here are in scene units, the same
  // convention (and the same conversion) as an MDL volume.
  const float sigmaRef{KOSCHMIEDER /
                       (1000.0f * std::max(options.visibility, 1e-3f))};
  mSigmaRef = SpectralColor(wavelens.size());
  for (size_t i = 0; i < mSigmaRef.size(); i++)
    mSigmaRef[i] =
        sigmaRef * metersPerSceneUnit *
        std::pow(wavelens[i] / REFERENCE_WAVELENGTH, -options.angstrom);
  SMDL_SANITY_CHECK(!mSun.isValid() || mSun.irradiance.size() == size());
  mSun.direction = normalize(mSun.direction);
}

void Haze::extinctionAt(float height, Span<float> sigma) const noexcept {
  SMDL_SANITY_CHECK(sigma.size() == size());
  const float scale{std::exp(
      std::min((mBaseHeight - height) * mInvScaleHeight, MAX_EXPONENT))};
  for (size_t i = 0; i < sigma.size(); i++) sigma[i] = mSigmaRef[i] * scale;
}

float Haze::shape(float k, float t) noexcept {
  // The horizontal ray is not a special case of the formula below but
  // its removable singularity, and the series is what keeps a shallow
  // one from evaluating a difference of nearly equal exponentials over
  // a nearly zero denominator. Branching on `k` first also keeps an
  // unbounded segment from forming 0 times infinity.
  if (k == 0.0f) return t;
  const float kt{k * t};
  if (std::abs(kt) < 1e-4f) return t * (1.0f - 0.5f * kt);
  return -std::expm1(-kt) / k;
}

float Haze::shapeInverse(float k, float s) noexcept {
  if (!(s > 0.0f)) return 0.0f;
  if (k == 0.0f) return s;
  // An upward ray reaches at most `1/k`, the finite zenith shape; past
  // that there is no collision and the ray leaves the atmosphere.
  const float ks{k * s};
  if (!(ks < 1.0f)) return INF;
  return -std::log1p(-ks) / k;
}

float Haze::phaseOverSunDisk(const float3 &dir) const noexcept {
  constexpr int N{16};
  // The golden angle, which spreads the azimuths of a small stratified
  // set about as evenly as anything without a second dimension to
  // stratify in.
  constexpr float GOLDEN{2.39996323f};
  const auto frame{coordinateSystem(mSun.direction)};
  double sum{0.0};
  for (int i = 0; i < N; i++) {
    const float cosTheta{1.0f -
                         (float(i) + 0.5f) / N * (1.0f - mSun.cosRadius)};
    const float sinTheta{std::sqrt(std::max(0.0f, 1.0f - cosTheta * cosTheta))};
    const float phi{GOLDEN * float(i)};
    const float3 wi{frame * float3(sinTheta * std::cos(phi),
                                   sinTheta * std::sin(phi), cosTheta)};
    sum += mPhase.evaluate(dot(dir, wi));
  }
  return float(sum / N);
}

bool Haze::sunInscatter(const float3 &org, const float3 &dir, float tEnd,
                        float xi, Span<float> radiance, float &tShadow) const {
  SMDL_SANITY_CHECK(radiance.size() == size());
  if (!mSun.isValid() || !(tEnd > 0.0f)) return false;
  const float t{std::min(tEnd, 1e12f)};
  const float k{shapeExponent(dir.z)};
  const float phase{phaseOverSunDisk(dir)};
  if (!(phase > 0.0f)) return false;
  // The density at the far end relative to the origin, which scales the
  // sun path there. An unbounded segment that never turns upward drives
  // it and the span below past what a float holds, while the integral
  // they form converges: clamping it is what keeps the arithmetic
  // finite, and the span is then derived from the clamped value rather
  // than clamped on its own, because the limit is a ratio of the two and
  // a clamp applied to one alone destroys it.
  const float growth{-k * t};
  const float uEnd{std::exp(std::min(growth, 60.0f))};
  const float span{growth > 60.0f ? (1.0f - uEnd) / k : shape(k, t)};
  if (!(span > 0.0f)) return false;
  const float invSunSlope{1.0f / shapeExponent(mSun.direction.z)};
  auto sigmaC{SpectralColor(size())};
  extinctionAt(org.z, Span<float>(sigmaC.data(), sigmaC.size()));
  double sigmaMean{0.0};
  for (size_t i = 0; i < sigmaC.size(); i++) {
    const float a{sigmaC[i] * span};        // tau_cam(tEnd)
    const float b{sigmaC[i] * invSunSlope}; // tau_sun(0)
    const float c{b * uEnd};                // tau_sun(tEnd)
    const float x{a - b + c};
    // Both exponents are non-positive, so neither overflows, and the
    // series covers the cancellation as `x` approaches zero.
    const float ratio{std::abs(x) < 1e-3f
                          ? std::exp(-b) * (1.0f - 0.5f * x)
                          : (std::exp(-b) - std::exp(-(a + c))) / x};
    radiance[i] = phase * mSun.irradiance[i] * mAlbedo * a * ratio;
    sigmaMean += double(sigmaC[i]);
  }
  sigmaMean /= double(sigmaC.size());
  // Where to test the source's visibility. In the optical depth `tau`
  // the integrand is proportional to `exp(-m tau)` with `m = 1 - dir.z /
  // sun.z` purely geometric, so the sampling inverts in closed form on
  // the normalized depth, and the distance follows from the shape.
  const float m{1.0f - dir.z / mSun.direction.z};
  const float beta{m * float(sigmaMean) * span};
  float w{xi};
  if (std::abs(beta) > 1e-6f) w = -std::log1p(xi * std::expm1(-beta)) / beta;
  tShadow = std::min(shapeInverse(k, std::clamp(w, 0.0f, 1.0f) * span), tEnd);
  return true;
}

} // namespace smdl
