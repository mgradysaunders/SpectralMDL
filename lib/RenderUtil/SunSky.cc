#include "smdl/RenderUtil/SunSky.h"

#include <algorithm>
#include <array>
#include <cmath>
#include <cstddef>
#include <cstdint>

#include "smdl/RenderUtil/FastMath.h"
#include "smdl/Support/Macros.h"

#include "SunSkyRoloMoon.h"
#include "SunSkyRural.h"
#include "Support/SIMD.h"

// The lunar multiplier is generated on the same grid as the sun-sky fit,
// so the channels line up one-to-one.
static_assert(roloMoon::WAVELENGTH_COUNT == rural::WAVELENGTH_COUNT &&
              roloMoon::WAVELENGTH_MIN == rural::WAVELENGTH_MIN &&
              roloMoon::WAVELENGTH_DELTA == rural::WAVELENGTH_DELTA);

namespace smdl {

namespace {

// The resolution of the tabulated sky sampling distribution.
constexpr int SKY_DISTR_SIZE_X = 256;
constexpr int SKY_DISTR_SIZE_Y = 128;

// The reciprocal of the channel spacing, so locating a wavelength on the
// grid is a multiply. The per-wavelength loops below run this once per
// wavelength, where a divide is the most expensive thing in them.
constexpr float INV_WAVELENGTH_DELTA = 1.0f / rural::WAVELENGTH_DELTA;

// The reciprocals of a fit's feature standard deviations, so that
// standardizing is a multiply. Every evaluation standardizes every
// feature.
template <size_t N>
[[nodiscard]] constexpr std::array<float, N>
reciprocals(const float (&values)[N]) {
  std::array<float, N> result{};
  for (size_t i = 0; i < N; i++) result[i] = 1.0f / values[i];
  return result;
}
constexpr auto SKY_FEATURE_INV_STD = reciprocals(rural::SKY_FEATURE_STD);
constexpr auto SUN_FEATURE_INV_STD = reciprocals(rural::SUN_FEATURE_STD);

[[nodiscard]] SMDL_ALWAYS_INLINE float standardizeSky(size_t i, float raw) {
  return (raw - rural::SKY_FEATURE_MEAN[i]) * SKY_FEATURE_INV_STD[i];
}

// Kasten-Young relative airmass. The cosine of the zenith angle is a
// separate argument because every caller already has it. Only the
// construction path forms the airmass itself, so this takes the accurate
// transcendentals and leaves the inline approximations to the reciprocal
// form below.
[[nodiscard]] float airmass(float zenithDeg, float cosZenith) {
  return 1.0f /
         (cosZenith +
          0.50572f * std::exp(-1.6364f * std::log(96.07995f - zenithDeg)));
}

// The reciprocal of the Kasten-Young airmass, which is the form the sky
// fit wants: it takes the logarithm of the airmass, and the logarithm of
// a reciprocal is a negation, so forming the airmass first would spend a
// division to be undone.
[[nodiscard]]
SMDL_ALWAYS_INLINE float airmassReciprocal(float zenithDeg, float cosZenith) {
  return cosZenith +
         0.50572f * fastExp(-1.6364f * fastLog(96.07995f - zenithDeg));
}

// Expand standardized features into the polynomial's terms. Each term is
// a product of up to `TermWidth` features, with a negative index marking
// an unused slot.
template <size_t NumFeatures, size_t NumTerms, size_t TermWidth>
void expandTerms(const float (&z)[NumFeatures],
                 const int8_t (&termFeatures)[NumTerms][TermWidth],
                 float (&terms)[NumTerms]) {
  for (size_t t = 0; t < NumTerms; ++t) {
    float value = 1.0f;
    for (size_t slot = 0; slot < TermWidth; ++slot) {
      if (const int8_t f = termFeatures[t][slot]; f >= 0) value *= z[size_t(f)];
    }
    terms[t] = value;
  }
}

// The number of outputs rounded up to a vector, so one row of the matrix
// below is one whole pack.
constexpr size_t SKY_PACK_WIDTH = 8;
static_assert(rural::SKY_OUTPUT_COUNT <= SKY_PACK_WIDTH);

// Which of the fit's nine standardized features the options fix and which
// the view direction supplies. The split is what lets the constructor
// specialize the polynomial; see `SunSky::mSkyMatrix`.
constexpr size_t SKY_FIXED_FEATURES[]{0, 2, 7, 8};
constexpr size_t SKY_VIEW_FEATURES[]{1, 3, 4, 5, 6};
constexpr size_t SKY_VIEW_FEATURE_COUNT =
    sizeof(SKY_VIEW_FEATURES) / sizeof(SKY_VIEW_FEATURES[0]);
static_assert(sizeof(SKY_FIXED_FEATURES) / sizeof(SKY_FIXED_FEATURES[0]) +
                  SKY_VIEW_FEATURE_COUNT ==
              rural::SKY_FEATURE_COUNT);

// Is this feature one the view direction supplies, and if so, which slot
// of the view feature vector is it?
[[nodiscard]] constexpr int skyViewSlot(int8_t feature) {
  for (size_t slot = 0; slot < SKY_VIEW_FEATURE_COUNT; slot++)
    if (SKY_VIEW_FEATURES[slot] == size_t(feature)) return int(slot);
  return -1;
}

// How many view features the fit's heaviest term carries. The fit caps
// this deliberately: a term's fixed features fold into the coefficients
// once per `SunSky`, so only the view part costs anything per
// evaluation, and the cap is what sets how many monomials there are to
// evaluate.
[[nodiscard]] constexpr size_t skyViewDegree() {
  size_t degree{};
  for (size_t t = 0; t < rural::SKY_TERM_COUNT; t++) {
    size_t width{};
    for (size_t i = 0; i < 3; i++)
      if (skyViewSlot(rural::SKY_TERM_FEATURES[t][i]) >= 0) width++;
    if (width > degree) degree = width;
  }
  return degree;
}
constexpr size_t SKY_VIEW_DEGREE = skyViewDegree();

// Every monomial up to that degree in the five view features, in a
// canonical order: the constant, then the features, then the pairs, then
// the triples, each in ascending index order. A sixth slot standing at
// one pads the shorter ones, so a monomial is always a product of three
// entries and the expansion carries no branches.
//
// Specializing against the fixed features maps the fit's terms onto
// these, several terms to a monomial; monomials the fit never reaches
// keep weights of zero. Evaluating them all rather than only the ones
// with weight costs a few vector multiply-accumulates and buys
// straight-line code with no term table to walk at runtime.
constexpr size_t SKY_MONOMIAL_ONE = SKY_VIEW_FEATURE_COUNT;
// The build-time capacity of the table below, which is every monomial up
// to the cubic; the fit's own degree decides how many of them it holds.
constexpr size_t SKY_MONOMIAL_MAX = 56;
struct SkyMonomials final {
  uint8_t features[SKY_MONOMIAL_MAX][3]{};
  size_t count{};
};
[[nodiscard]] constexpr SkyMonomials makeSkyMonomials() {
  SkyMonomials monomials{};
  for (auto &monomial : monomials.features)
    for (auto &slot : monomial) slot = uint8_t(SKY_MONOMIAL_ONE);
  size_t m{1};
  for (size_t i = 0; i < SKY_VIEW_FEATURE_COUNT; i++)
    monomials.features[m++][0] = uint8_t(i);
  if (SKY_VIEW_DEGREE >= 2)
    for (size_t i = 0; i < SKY_VIEW_FEATURE_COUNT; i++)
      for (size_t j = i; j < SKY_VIEW_FEATURE_COUNT; j++) {
        monomials.features[m][0] = uint8_t(i);
        monomials.features[m++][1] = uint8_t(j);
      }
  if (SKY_VIEW_DEGREE >= 3)
    for (size_t i = 0; i < SKY_VIEW_FEATURE_COUNT; i++)
      for (size_t j = i; j < SKY_VIEW_FEATURE_COUNT; j++)
        for (size_t k = j; k < SKY_VIEW_FEATURE_COUNT; k++) {
          monomials.features[m][0] = uint8_t(i);
          monomials.features[m][1] = uint8_t(j);
          monomials.features[m++][2] = uint8_t(k);
        }
  monomials.count = m;
  return monomials;
}
constexpr SkyMonomials SKY_MONOMIALS = makeSkyMonomials();
constexpr size_t SKY_MONOMIAL_COUNT = SKY_MONOMIALS.count;

// The canonical monomial one of the fit's terms belongs to: its view
// features, in the order above, with its fixed features factored out.
// Terms that share a monomial differ only by a factor the options fix,
// so their weights sum once per `SunSky` instead of once per evaluation.
[[nodiscard]] constexpr uint8_t skyMonomialOf(size_t t) {
  uint8_t view[3]{uint8_t(SKY_MONOMIAL_ONE), uint8_t(SKY_MONOMIAL_ONE),
                  uint8_t(SKY_MONOMIAL_ONE)};
  size_t width{};
  for (size_t i = 0; i < 3; i++)
    if (const int slot = skyViewSlot(rural::SKY_TERM_FEATURES[t][i]); slot >= 0)
      view[width++] = uint8_t(slot);
  for (size_t m = 0; m < SKY_MONOMIAL_COUNT; m++) {
    bool isSame{true};
    for (size_t i = 0; i < 3; i++)
      isSame = isSame && SKY_MONOMIALS.features[m][i] == view[i];
    if (isSame) return uint8_t(m);
  }
  return 0; // unreachable: the monomials above are every one of degree 3
}

// The monomial count rounded up to the accumulator count, so the matvec
// loop needs no scalar tail. The rows past it are the slack in the
// class's fixed bound, which the constructor leaves at zero and the
// kernel never reads.
constexpr size_t SKY_TERM_STRIDE = (SKY_MONOMIAL_COUNT + 3) / 4 * 4;

// Expand the features into terms and accumulate the sparse coefficient dot
// products. The direct-beam fit runs this once per construction, where the
// dense form above would only trade a smaller table for more arithmetic.
//
// Four accumulators, as in the matvec above and for the same reason.
template <size_t NumFeatures, size_t NumTerms, size_t NumOutputs,
          size_t TermWidth>
void evalSparsePolynomial(const float (&z)[NumFeatures],
                          const int8_t (&termFeatures)[NumTerms][TermWidth],
                          const rural::Coeff *coeffs,
                          const int (&coeffOffsets)[NumOutputs + 1],
                          float (&outputs)[NumOutputs]) {
  // No initializer: `expandTerms` writes every entry before this reads
  // any of them.
  float terms[NumTerms];
  expandTerms(z, termFeatures, terms);
  for (size_t q = 0; q < NumOutputs; ++q) {
    float sum0{}, sum1{}, sum2{}, sum3{};
    int c = coeffOffsets[q];
    const int end = coeffOffsets[q + 1];
    for (; c + 4 <= end; c += 4) {
      sum0 += coeffs[c + 0].weight * terms[coeffs[c + 0].term];
      sum1 += coeffs[c + 1].weight * terms[coeffs[c + 1].term];
      sum2 += coeffs[c + 2].weight * terms[coeffs[c + 2].term];
      sum3 += coeffs[c + 3].weight * terms[coeffs[c + 3].term];
    }
    for (; c < end; ++c) sum0 += coeffs[c].weight * terms[coeffs[c].term];
    outputs[q] = (sum0 + sum1) + (sum2 + sum3);
  }
}

// Evaluate the direct-beam-fit outputs. The zenith angle must already
// be clamped to the trained range.
void evalSunOutputs(float sunZenithDeg, float visibility, float waterVapor,
                    float (&outputs)[rural::SUN_OUTPUT_COUNT]) {
  const float cosSun = std::cos(radians(sunZenithDeg));
  const float air = airmass(sunZenithDeg, cosSun);
  const float raw[rural::SUN_FEATURE_COUNT] = {
      cosSun, std::log(air), std::log(visibility), std::log(waterVapor), air};
  float z[rural::SUN_FEATURE_COUNT]{};
  for (size_t i = 0; i < rural::SUN_FEATURE_COUNT; ++i)
    z[i] = (raw[i] - rural::SUN_FEATURE_MEAN[i]) * SUN_FEATURE_INV_STD[i];
  evalSparsePolynomial(z, rural::SUN_TERM_FEATURES, rural::SUN_COEFFS,
                       rural::SUN_COEFF_OFFSETS, outputs);
}

// The continuous channel coordinate of the given wavelength in
// nanometers, clamped to the grid. Non-finite wavelengths clamp to the
// first channel. The indexes are signed because converting a float to
// an unsigned integer costs a range fixup and a branch on x86, once
// each way, and every table address in the per-wavelength loops waits
// behind it.
struct ChannelLerp final {
  int i0;
  int i1;
  float frac;
};
[[nodiscard]] ChannelLerp channelOf(float wavelenNm) {
  float t = (wavelenNm - rural::WAVELENGTH_MIN) * INV_WAVELENGTH_DELTA;
  if (!(t > 0.0f)) t = 0.0f;
  if (t > float(rural::WAVELENGTH_COUNT - 1))
    t = float(rural::WAVELENGTH_COUNT - 1);
  ChannelLerp lerp;
  lerp.i0 = int(t);
  lerp.i1 = std::min(lerp.i0 + 1, int(rural::WAVELENGTH_COUNT) - 1);
  lerp.frac = t - float(lerp.i0);
  return lerp;
}

// The clamped view zenith angle in degrees, its cosine, and the unit
// horizontal projection of the given unit direction (or +X if
// degenerate). The cosine comes straight off the direction rather than
// from a cosine of the angle just taken out of it.
void viewGeometry(const float3 &dir, float &cosView, float &viewZenithDeg,
                  float2 &horz) {
  const float cosZ = std::clamp(dir.z, -1.0f, 1.0f);
  cosView = std::max(cosZ, SunSky::COS_VIEW_ZENITH_MAX);
  viewZenithDeg =
      std::min(degrees(fastAcos(cosZ)), SunSky::VIEW_ZENITH_MAX_DEG);
  horz = float2(dir.x, dir.y);
  const float len = length(horz);
  horz = len > 1.0e-12f ? horz * (1.0f / len) : float2(1.0f, 0.0f);
}

} // namespace

void SunSky::evalSkyFit(float cosView, float viewZenithDeg,
                        float cosRelativeAzimuth,
                        float (&outputs)[SKY_FIT_OUTPUT_COUNT]) const noexcept {
  // The header cannot name the fit tables, so the grid and the counts it
  // declares are checked against them here.
  static_assert(WAVELENGTH_MIN_NM == rural::WAVELENGTH_MIN &&
                WAVELENGTH_MAX_NM == rural::WAVELENGTH_MAX);
  static_assert(SKY_FIT_OUTPUT_COUNT == int(rural::SKY_OUTPUT_COUNT));
  static_assert(SKY_FIT_TERM_COUNT == int(SKY_TERM_STRIDE));
  static_assert(SKY_FIT_OUTPUT_STRIDE == int(SKY_PACK_WIDTH));
  // As (1 - c)(1 + c) rather than 1 - c*c, which cancels away most of the
  // significand looking near the zenith, where c is close to one.
  const float sinView{
      std::sqrt(std::max((1.0f - cosView) * (1.0f + cosView), 0.0f))};
  const float cosPsi{std::clamp(
      mCosSunZenith * cosView + mSinSunZenith * sinView * cosRelativeAzimuth,
      -1.0f, 1.0f)};
  const float psiDeg{degrees(fastAcos(cosPsi))};
  // The five features the view supplies, in the order the specialized
  // monomials index them. The logarithms, exponentials, and arccosines
  // here are the inline ones from FastMath.h: libm's out-of-line calls
  // cost more than the polynomial they feed, and the fit's own residual
  // dwarfs their error.
  const float zz[SKY_VIEW_FEATURE_COUNT + 1]{
      standardizeSky(1, cosView),
      standardizeSky(3, -fastLog(airmassReciprocal(viewZenithDeg, cosView))),
      standardizeSky(4, cosPsi),
      standardizeSky(5, fastExp(psiDeg * (-1.0f / 15.0f))),
      standardizeSky(6, fastLog(psiDeg + 3.0f)),
      1.0f};
  // The monomials in the canonical order of `SKY_MONOMIALS`, built by
  // the same nested loops so the two cannot drift apart. Every bound is
  // a constant, so this unrolls to straight-line multiplies with no term
  // table to read.
  //
  // Zero-initialized because the stride rounds the monomial count up to
  // the accumulator count and the contraction reads all of it: the pad
  // lanes multiply all-zero rows of the matrix, so they contribute
  // nothing, but only as long as they are finite.
  alignas(32) float terms[SKY_TERM_STRIDE]{};
  terms[0] = 1.0f;
  for (size_t i = 0; i < SKY_VIEW_FEATURE_COUNT; i++) terms[1 + i] = zz[i];
  size_t m{1 + SKY_VIEW_FEATURE_COUNT};
  if constexpr (SKY_VIEW_DEGREE >= 2)
    for (size_t i = 0; i < SKY_VIEW_FEATURE_COUNT; i++)
      for (size_t j = i; j < SKY_VIEW_FEATURE_COUNT; j++)
        terms[m++] = zz[i] * zz[j];
  if constexpr (SKY_VIEW_DEGREE >= 3)
    for (size_t i = 0; i < SKY_VIEW_FEATURE_COUNT; i++)
      for (size_t j = i; j < SKY_VIEW_FEATURE_COUNT; j++)
        for (size_t k = j; k < SKY_VIEW_FEATURE_COUNT; k++)
          terms[m++] = zz[i] * zz[j] * zz[k];
  // Contract the terms against the specialized coefficients.
  //
  // Four accumulators, for the same reason the sparse kernel above carries
  // four: one running sum would make the loop a single chain of dependent
  // adds, and the multiplies and loads have throughput to spare while each
  // add waits on the last. Four is measurably the right number at every
  // instruction set level; eight is no better with AVX2 and worse without.
  using Pack = simd::Pack<float, SKY_PACK_WIDTH>;
  Pack acc0{}, acc1{}, acc2{}, acc3{};
  for (size_t t = 0; t < SKY_TERM_STRIDE; t += 4) {
    acc0 = acc0 + Pack(terms[t + 0]) * Pack::load(mSkyMatrix[t + 0]);
    acc1 = acc1 + Pack(terms[t + 1]) * Pack::load(mSkyMatrix[t + 1]);
    acc2 = acc2 + Pack(terms[t + 2]) * Pack::load(mSkyMatrix[t + 2]);
    acc3 = acc3 + Pack(terms[t + 3]) * Pack::load(mSkyMatrix[t + 3]);
  }
  alignas(32) float packed[SKY_PACK_WIDTH];
  ((acc0 + acc1) + (acc2 + acc3)).store(packed);
  for (size_t q = 0; q < rural::SKY_OUTPUT_COUNT; q++) outputs[q] = packed[q];
}

SunSky::SunSky(const SunSkyOptions &options) {
  // The fit tables are already in the library's radiance units, so the
  // user's scale is the whole of it.
  mScaleFactor = options.scaleFactor;
  mSunDiskScale = mScaleFactor / SUN_SOLID_ANGLE;
  mIsSunEnabled = options.isSunEnabled;
  const float visibility{
      std::clamp(options.visibility, VISIBILITY_MIN_KM, VISIBILITY_MAX_KM)};
  const float waterVapor{
      std::clamp(options.waterVaporScale, WATER_VAPOR_MIN, WATER_VAPOR_MAX)};

  // The effective sun direction: the given azimuth at the clamped
  // zenith, so the disk stays centered on the aureole the sky fit
  // produces.
  const float3 given{normalize(options.sunDirection)};
  const float sunZenithDeg{
      std::clamp(degrees(std::acos(std::clamp(given.z, -1.0f, 1.0f))),
                 SUN_ZENITH_MIN_DEG, SUN_ZENITH_MAX_DEG)};
  mSunDirHorz = float2(given.x, given.y);
  const float lenHorz{length(mSunDirHorz)};
  mSunDirHorz =
      lenHorz > 1.0e-12f ? mSunDirHorz * (1.0f / lenHorz) : float2(1.0f, 0.0f);
  mCosSunZenith = std::cos(radians(sunZenithDeg));
  mSinSunZenith = std::sin(radians(sunZenithDeg));
  mSunDir = float3(mSunDirHorz.x * mSinSunZenith, mSunDirHorz.y * mSinSunZenith,
                   mCosSunZenith);

  // Specialize the polynomial to the four standardized features no view
  // direction can change: with them fixed, every term is a constant
  // times a monomial in the five features the view supplies, so fold the
  // constant in and sum the terms that share a monomial.
  {
    // The fixed features by their index in the fit's feature vector; the
    // view features stand at one, so a term's product over all of them is
    // exactly its fixed part.
    float zFixed[rural::SKY_FEATURE_COUNT];
    for (auto &value : zFixed) value = 1.0f;
    zFixed[0] = standardizeSky(0, mCosSunZenith);
    zFixed[2] =
        standardizeSky(2, std::log(airmass(sunZenithDeg, mCosSunZenith)));
    zFixed[7] = standardizeSky(7, std::log(visibility));
    zFixed[8] = standardizeSky(8, std::log(waterVapor));
    for (size_t q = 0; q < rural::SKY_OUTPUT_COUNT; q++) {
      for (int c = rural::SKY_COEFF_OFFSETS[q];
           c < rural::SKY_COEFF_OFFSETS[q + 1]; c++) {
        const size_t t = size_t(rural::SKY_COEFFS[c].term);
        float fixed = rural::SKY_COEFFS[c].weight;
        for (const int8_t f : rural::SKY_TERM_FEATURES[t])
          if (f >= 0) fixed *= zFixed[size_t(f)];
        mSkyMatrix[skyMonomialOf(t)][q] += fixed;
      }
    }
  }

  // Moonlight mode: the per-channel lunar multiplier rides on top of
  // both fits, which otherwise run identically with the source at the
  // moon's position. Scattered radiance is linear in the source
  // irradiance, so this is exact for the atmosphere.
  if (options.isMoon) {
    const auto multiplier = roloMoon::evaluateMoonMultiplier(
        std::clamp(double(options.moonPhase), -180.0, 180.0),
        std::max(double(options.moonDistanceScale), 0.0));
    mChannelScale.assign(multiplier.begin(), multiplier.end());
  }

  // The direct solar irradiance is independent of the view direction,
  // so evaluate the whole spectrum once, capped channel-wise at the
  // TOA irradiance exactly as in the fit.
  {
    float outputs[rural::SUN_OUTPUT_COUNT]{};
    evalSunOutputs(sunZenithDeg, visibility, waterVapor, outputs);
    const float brightness = std::exp(outputs[0]);
    mSunIrradiance.resize(rural::WAVELENGTH_COUNT);
    for (size_t i = 0; i < rural::WAVELENGTH_COUNT; ++i) {
      float shape = rural::SUN_MEAN_SHAPE[i];
      for (size_t m = 0; m < rural::SUN_MODE_COUNT; ++m)
        shape += outputs[1 + m] * rural::SUN_MODES[m][i];
      mSunIrradiance[i] = std::min(brightness * std::max(shape, 0.0f),
                                   rural::SOLAR_IRRADIANCE[i]);
      if (!mChannelScale.empty()) mSunIrradiance[i] *= mChannelScale[i];
    }
  }

  // Channel sums of the mean shape and modes let each texel of the
  // sampling grid evaluate its broadband (channel-mean) radiance in
  // O(1) instead of summing 421 channels. In moonlight mode the sums
  // carry the per-channel lunar multiplier, so the sampling weights
  // follow the moonlit spectrum. The per-channel clamp at zero is
  // skipped here, which only perturbs the sampling weights, never the
  // reported pdf, so the estimator is unaffected.
  float sumMeanShape = 0.0f;
  float sumModes[rural::SKY_MODE_COUNT]{};
  for (size_t i = 0; i < rural::WAVELENGTH_COUNT; ++i) {
    const float scale = mChannelScale.empty() ? 1.0f : mChannelScale[i];
    sumMeanShape += scale * rural::SKY_MEAN_SHAPE[i];
    for (size_t m = 0; m < rural::SKY_MODE_COUNT; ++m)
      sumModes[m] += scale * rural::SKY_MODES[m][i];
  }

  // Tabulate broadband sky radiance on the lat-long grid. Every sum over
  // the grid runs a row at a time and adds the row subtotals, which is a
  // two-level pairwise sum over the loop nest that is already here and
  // holds the error near a part in a million rather than the part in a
  // hundred thousand a flat sum over 32768 texels would carry.
  std::vector<float> weights{};
  weights.reserve(size_t(SKY_DISTR_SIZE_X) * SKY_DISTR_SIZE_Y);
  float radianceSum{};
  float sinThetaSum{};
  float skyIntegral{};
  const float dTheta = PI / SKY_DISTR_SIZE_Y;
  const float dPhi = TWO_PI / SKY_DISTR_SIZE_X;
  for (int iY = 0; iY < SKY_DISTR_SIZE_Y; iY++) {
    const float theta = dTheta * (iY + 0.5f);
    const float sinTheta = std::sin(theta);
    const float viewZenithDeg =
        std::min(degrees(theta), SunSky::VIEW_ZENITH_MAX_DEG);
    const float cosView = std::max(std::cos(theta), COS_VIEW_ZENITH_MAX);
    float rowSum{};
    for (int iX = 0; iX < SKY_DISTR_SIZE_X; iX++) {
      const float phi = dPhi * (iX + 0.5f);
      const float cosRelAz =
          mSunDirHorz.x * std::cos(phi) + mSunDirHorz.y * std::sin(phi);
      float outputs[SKY_FIT_OUTPUT_COUNT]{};
      evalSkyFit(cosView, viewZenithDeg, cosRelAz, outputs);
      float shapeSum = sumMeanShape;
      for (size_t m = 0; m < rural::SKY_MODE_COUNT; ++m)
        shapeSum += outputs[1 + m] * sumModes[m];
      const float broadband = fastExp(outputs[0]) * std::max(shapeSum, 0.0f) /
                              rural::WAVELENGTH_COUNT;
      weights.push_back(sinTheta * broadband);
      rowSum += broadband;
    }
    radianceSum += sinTheta * rowSum;
    sinThetaSum += sinTheta * SKY_DISTR_SIZE_X;
    skyIntegral += rowSum * sinTheta * dTheta * dPhi;
  }
  const float meanSkyRadiance =
      sinThetaSum > 0 ? radianceSum / sinThetaSum : 0.0f;

  // MIS compensation, matching `EnvLight` in smdl-toy: subtract the
  // mean radiance from the tabulated density and clamp at zero,
  // falling back to the uncompensated weights if compensation removes
  // everything.
  if (options.isMISCompensationEnabled) {
    std::vector<float> compensated{weights};
    float compensatedSum{};
    size_t texel{};
    for (int iY = 0; iY < SKY_DISTR_SIZE_Y; iY++) {
      const float sinTheta = std::sin(dTheta * (iY + 0.5f));
      float rowSum{};
      for (int iX = 0; iX < SKY_DISTR_SIZE_X; iX++, texel++) {
        const float value = sinTheta > 0 ? weights[texel] / sinTheta : 0.0f;
        compensated[texel] = sinTheta * std::max(value - meanSkyRadiance, 0.0f);
        rowSum += compensated[texel];
      }
      compensatedSum += rowSum;
    }
    if (compensatedSum > 0) weights = std::move(compensated);
  }
  mSkyDistr = Distribution2D(SKY_DISTR_SIZE_X, SKY_DISTR_SIZE_Y, weights);

  // Select the sun against the sky by broadband power. The sun-disk
  // radiance integrated over its solid angle is just the broadband
  // direct irradiance, in the same channel-mean convention as the sky
  // integral.
  float sunIntegral = 0.0f;
  for (size_t i = 0; i < rural::WAVELENGTH_COUNT; ++i)
    sunIntegral += mSunIrradiance[i];
  sunIntegral /= rural::WAVELENGTH_COUNT;
  if (!mIsSunEnabled) sunIntegral = 0.0f;
  mSunSelectionChance =
      sunIntegral + skyIntegral > 0
          ? std::min(sunIntegral / (sunIntegral + skyIntegral), 0.999f)
          : 0.0f;
  mMeanRadiance = (skyIntegral + sunIntegral) * (0.25f * INV_PI) * mScaleFactor;
}

void SunSky::resolve(Span<const float> wavelens, SkyBasis &basis) const {
  const size_t numBands{wavelens.size()};
  basis.mNumBands = int(numBands);
  basis.mRows.assign((1 + rural::SKY_MODE_COUNT) * numBands, 0.0f);
  basis.mSunIrradiance.assign(numBands, 0.0f);
  if (mSunIrradiance.empty()) return; // default-constructed
  for (size_t j = 0; j < numBands; ++j) {
    const ChannelLerp lerp{channelOf(wavelens[j])};
    const float scale0{mChannelScale.empty() ? 1.0f : mChannelScale[lerp.i0]};
    const float scale1{mChannelScale.empty() ? 1.0f : mChannelScale[lerp.i1]};
    const auto mix{
        [frac = lerp.frac](float a, float b) { return a + frac * (b - a); }};
    basis.mRows[j] = mix(scale0 * rural::SKY_MEAN_SHAPE[lerp.i0],
                         scale1 * rural::SKY_MEAN_SHAPE[lerp.i1]);
    for (size_t m = 0; m < rural::SKY_MODE_COUNT; ++m)
      basis.mRows[(1 + m) * numBands + j] =
          mix(scale0 * rural::SKY_MODES[m][lerp.i0],
              scale1 * rural::SKY_MODES[m][lerp.i1]);
    basis.mSunIrradiance[j] =
        mix(mSunIrradiance[lerp.i0], mSunIrradiance[lerp.i1]);
  }
}

namespace {
// One contraction over the sky modes, band by band: the mean shape row
// plus each mode row weighted by its fit output, floored at zero and
// scaled. The rows are `restrict` against the output because a caller
// passing a band buffer that overlaps them would otherwise cost a runtime
// overlap check per row in front of a loop this short. They are
// parameters rather than locals because clang derives no-alias metadata
// only from the former.
SMDL_ALWAYS_INLINE void contractSkyModes(float *SMDL_RESTRICT out,
                                         const float *SMDL_RESTRICT rows,
                                         const float *SMDL_RESTRICT outputs,
                                         int numBands,
                                         float brightnessScale) noexcept {
  for (int j = 0; j < numBands; j++) {
    float shape{rows[j]};
    for (size_t m = 0; m < rural::SKY_MODE_COUNT; ++m)
      shape += outputs[1 + m] * rows[(1 + m) * size_t(numBands) + j];
    out[j] = std::max(shape, 0.0f) * brightnessScale;
  }
}
} // namespace

void SunSky::skyRadiance(const float3 &direction, const SkyBasis &basis,
                         float *radiance) const {
  const int numBands{basis.mNumBands};
  if (mSunIrradiance.empty()) { // default-constructed
    std::fill(radiance, radiance + numBands, 0.0f);
    return;
  }
  float cosView{};
  float viewZenithDeg{};
  float2 horizontal{};
  viewGeometry(direction, cosView, viewZenithDeg, horizontal);
  const float cosRelAz = dot(horizontal, mSunDirHorz);
  float outputs[SKY_FIT_OUTPUT_COUNT]{};
  evalSkyFit(cosView, viewZenithDeg, cosRelAz, outputs);
  const float brightnessScale = fastExp(outputs[0]) * mScaleFactor;
  contractSkyModes(radiance, basis.mRows.data(), outputs, numBands,
                   brightnessScale);
}

namespace {

// The sun disk added onto the sky, see `contractSkyModes` for why the
// pointers are parameters.
SMDL_ALWAYS_INLINE void addSunDisk(float *SMDL_RESTRICT out,
                                   const float *SMDL_RESTRICT disk,
                                   int numBands, float scale) noexcept {
  for (int j = 0; j < numBands; j++) out[j] += disk[j] * scale;
}

} // namespace

void SunSky::radiance(const float3 &direction, const SkyBasis &basis,
                      float *radiance) const {
  skyRadiance(direction, basis, radiance);
  if (mIsSunEnabled && !mSunIrradiance.empty() &&
      dot(direction, mSunDir) >= COS_SUN_ANGULAR_RADIUS) {
    addSunDisk(radiance, basis.mSunIrradiance.data(), basis.mNumBands,
               mSunDiskScale);
  }
}

void SunSky::skyRadiance(const float3 &direction, int numWavelens,
                         const float *wavelens, float *radiance) const {
  // Resolved on the spot, so that the two forms cannot drift apart: this
  // one is for a caller with no grid to hold onto, not for a hot loop.
  SkyBasis basis{};
  resolve(Span<const float>(wavelens, size_t(numWavelens)), basis);
  skyRadiance(direction, basis, radiance);
}

void SunSky::sunRadiance(int numWavelens, const float *wavelens,
                         float *radiance) const {
  if (!mIsSunEnabled || mSunIrradiance.empty()) {
    std::fill(radiance, radiance + numWavelens, 0.0f);
    return;
  }
  for (int j = 0; j < numWavelens; j++) {
    const ChannelLerp lerp{channelOf(wavelens[j])};
    const float value0 = mSunIrradiance[lerp.i0];
    const float value1 = mSunIrradiance[lerp.i1];
    radiance[j] = (value0 + lerp.frac * (value1 - value0)) * mSunDiskScale;
  }
}

void SunSky::radiance(const float3 &direction, int numWavelens,
                      const float *wavelens, float *radiance) const {
  skyRadiance(direction, numWavelens, wavelens, radiance);
  if (mIsSunEnabled && !mSunIrradiance.empty() &&
      dot(direction, mSunDir) >= COS_SUN_ANGULAR_RADIUS) {
    for (int j = 0; j < numWavelens; j++) {
      const ChannelLerp lerp{channelOf(wavelens[j])};
      const float value0 = mSunIrradiance[lerp.i0];
      const float value1 = mSunIrradiance[lerp.i1];
      radiance[j] += (value0 + lerp.frac * (value1 - value0)) * mSunDiskScale;
    }
  }
}

double SunSky::moonMultiplier(double wavelenNm, double phaseDeg,
                              double distanceScale) {
  return roloMoon::moonMultiplier(wavelenNm, phaseDeg, distanceScale);
}

float3 SunSky::sample(float2 xi, float *pdf) const noexcept {
  if (mSkyDistr.getNumTexelsX() == 0) {
    if (pdf) *pdf = 0.0f;
    return {0.0f, 0.0f, 1.0f};
  }
  const float pmfSun = mSunSelectionChance;
  if (xi.x < pmfSun) {
    xi.x /= pmfSun; // remap
    const float3 wi = coordinateSystem(mSunDir) *
                      uniformConeSample(COS_SUN_ANGULAR_RADIUS, xi);
    if (pdf) *pdf = this->pdf(wi);
    return wi;
  }
  xi.x = (xi.x - pmfSun) / (1.0f - pmfSun); // remap
  float pdfSky{};
  const float3 wi = mSkyDistr.directionSample(xi, nullptr, &pdfSky);
  if (pdf) {
    *pdf = (1.0f - pmfSun) * pdfSky;
    if (pmfSun > 0 && dot(wi, mSunDir) >= COS_SUN_ANGULAR_RADIUS)
      *pdf += pmfSun * uniformConePDF(COS_SUN_ANGULAR_RADIUS);
  }
  return wi;
}

float SunSky::pdf(const float3 &direction) const noexcept {
  if (mSkyDistr.getNumTexelsX() == 0) return 0.0f;
  const float pmfSun = mSunSelectionChance;
  float result = (1.0f - pmfSun) * mSkyDistr.directionPDF(direction);
  if (pmfSun > 0 && dot(direction, mSunDir) >= COS_SUN_ANGULAR_RADIUS)
    result += pmfSun * uniformConePDF(COS_SUN_ANGULAR_RADIUS);
  return result;
}

} // namespace smdl
