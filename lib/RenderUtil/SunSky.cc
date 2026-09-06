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

namespace smdl {

namespace {

// The lunar multiplier is generated on the same 421-channel grid as
// the sun-sky fit, so the channels line up one-to-one.
static_assert(rolo_moon::WAVELENGTH_COUNT == rural::WAVELENGTH_COUNT);

constexpr double DEG_TO_RAD = 0.017453292519943295;
constexpr float RAD_TO_DEG_F = 57.29578f;

// The trained parameter ranges
constexpr double SUN_ZENITH_MIN_DEG = 5.0;
constexpr double SUN_ZENITH_MAX_DEG = 88.0;
constexpr double VIEW_ZENITH_MAX_DEG = 88.0;
// The cosine of the view zenith clamp, so that clamping the angle and
// clamping its cosine agree.
constexpr double COS_VIEW_ZENITH_MAX = 0.03489949670250108;
constexpr double VISIBILITY_MIN_KM = 5.0;
constexpr double VISIBILITY_MAX_KM = 100.0;
constexpr double WATER_VAPOR_MIN = 0.3;
constexpr double WATER_VAPOR_MAX = 3.0;

// The cosine of the solar angular radius of 0.2665 degrees, and the
// resulting disk solid angle \f$ 2\pi(1 - \cos\theta) \f$.
constexpr double COS_SUN_ANGULAR_RADIUS = 0.99998918271223114;
constexpr double SUN_SOLID_ANGLE = 6.7967023572838338e-05;

// The conversion from the fit tables' native MODTRAN units of
// W/(cm^2 sr um) to the library-wide spectral radiance convention of
// W/(m^2 sr nm): 1e4 cm^2/m^2 divided by 1e3 nm/um.
constexpr double NATIVE_TO_W_M2_SR_NM = 10.0;

// The resolution of the tabulated sky sampling distribution.
constexpr int SKY_DISTR_SIZE_X = 256;
constexpr int SKY_DISTR_SIZE_Y = 128;

// The reciprocal of the channel spacing, so locating a wavelength on the
// grid is a multiply. The per-wavelength loops below run this once per
// wavelength, where a divide is the most expensive thing in them.
constexpr float INV_WAVELENGTH_DELTA = 1.0f / float(rural::WAVELENGTH_DELTA);

// The reciprocals of a fit's feature standard deviations, so that
// standardizing is a multiply. Every evaluation standardizes every
// feature.
template <size_t N>
[[nodiscard]] constexpr std::array<double, N>
reciprocals(const double (&values)[N]) {
  std::array<double, N> result{};
  for (size_t i = 0; i < N; i++) result[i] = 1.0 / values[i];
  return result;
}
constexpr auto SKY_FEATURE_INV_STD = reciprocals(rural::SKY_FEATURE_STD);
constexpr auto SUN_FEATURE_INV_STD = reciprocals(rural::SUN_FEATURE_STD);

template <size_t N>
[[nodiscard]] constexpr std::array<float, N>
narrow(const std::array<double, N> &values) {
  std::array<float, N> result{};
  for (size_t i = 0; i < N; i++) result[i] = float(values[i]);
  return result;
}
template <size_t N>
[[nodiscard]] constexpr std::array<float, N> narrow(const double (&values)[N]) {
  std::array<float, N> result{};
  for (size_t i = 0; i < N; i++) result[i] = float(values[i]);
  return result;
}
constexpr auto SKY_FEATURE_MEAN_F = narrow(rural::SKY_FEATURE_MEAN);
constexpr auto SKY_FEATURE_INV_STD_F = narrow(SKY_FEATURE_INV_STD);

[[nodiscard]] SMDL_ALWAYS_INLINE double standardizeSky(size_t i, double raw) {
  return (raw - rural::SKY_FEATURE_MEAN[i]) * SKY_FEATURE_INV_STD[i];
}

[[nodiscard]] SMDL_ALWAYS_INLINE float standardizeSky(size_t i, float raw) {
  return (raw - SKY_FEATURE_MEAN_F[i]) * SKY_FEATURE_INV_STD_F[i];
}

// Kasten-Young relative airmass. The cosine of the zenith angle is a
// separate argument because every caller already has it.
[[nodiscard]]
SMDL_ALWAYS_INLINE double airmass(double zenithDeg, double cosZenith) {
  return 1.0 / (cosZenith +
                0.50572 * fastExp(-1.6364 * fastLog(96.07995 - zenithDeg)));
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
template <size_t NumFeatures, size_t NumTerms, size_t TermWidth, typename Term>
void expandTerms(const double (&z)[NumFeatures],
                 const int8_t (&termFeatures)[NumTerms][TermWidth], Term *terms,
                 size_t count = NumTerms) {
  for (size_t t = 0; t < count; ++t) {
    double value = 1.0;
    for (size_t slot = 0; slot < TermWidth; ++slot) {
      if (const int8_t f = termFeatures[t][slot]; f >= 0) value *= z[size_t(f)];
    }
    terms[t] = Term(value);
  }
}

// The number of outputs rounded up to a vector, so one row of the matrix
// below is one whole pack.
constexpr size_t SKY_PACK_WIDTH = 8;
static_assert(rural::SKY_OUTPUT_COUNT <= SKY_PACK_WIDTH);

// Which of the fit's nine standardized features the options fix and which
// the view direction supplies. The split is what lets the constructor
// specialize the polynomial; see `SkyPolynomial`.
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
// evaluate. See sunsky-plan.md.
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
    bool same{true};
    for (size_t i = 0; i < 3; i++)
      same = same && SKY_MONOMIALS.features[m][i] == view[i];
    if (same) return uint8_t(m);
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
void evalSparsePolynomial(const double (&z)[NumFeatures],
                          const int8_t (&termFeatures)[NumTerms][TermWidth],
                          const rural::Coeff *coefs,
                          const uint16_t (&coefOffsets)[NumOutputs + 1],
                          double (&outputs)[NumOutputs]) {
  double terms[NumTerms]{};
  expandTerms(z, termFeatures, terms);
  for (size_t q = 0; q < NumOutputs; ++q) {
    double sum0{}, sum1{}, sum2{}, sum3{};
    uint16_t c = coefOffsets[q];
    const uint16_t end = coefOffsets[q + 1];
    for (; c + 4 <= end; c += 4) {
      sum0 += double(coefs[c + 0].weight) * terms[coefs[c + 0].term];
      sum1 += double(coefs[c + 1].weight) * terms[coefs[c + 1].term];
      sum2 += double(coefs[c + 2].weight) * terms[coefs[c + 2].term];
      sum3 += double(coefs[c + 3].weight) * terms[coefs[c + 3].term];
    }
    for (; c < end; ++c) sum0 += double(coefs[c].weight) * terms[coefs[c].term];
    outputs[q] = (sum0 + sum1) + (sum2 + sum3);
  }
}

// Evaluate the direct-beam-fit outputs. The zenith angle must already
// be clamped to the trained range.
void evalSunOutputs(double sunZenithDeg, double visibility, double waterVapor,
                    double (&outputs)[rural::SUN_OUTPUT_COUNT]) {
  const double cosSun = std::cos(sunZenithDeg * DEG_TO_RAD);
  const double m = airmass(sunZenithDeg, cosSun);
  const double raw[rural::SUN_FEATURE_COUNT] = {
      cosSun, std::log(m), std::log(visibility), std::log(waterVapor), m,
  };
  double z[rural::SUN_FEATURE_COUNT]{};
  for (size_t i = 0; i < rural::SUN_FEATURE_COUNT; ++i)
    z[i] = (raw[i] - rural::SUN_FEATURE_MEAN[i]) * SUN_FEATURE_INV_STD[i];
  evalSparsePolynomial(z, rural::SUN_TERM_FEATURES, rural::SUN_COEFFS,
                       rural::SUN_COEFF_OFFSETS, outputs);
}

// The sky spectral shape of channel `i` for the given spectral mode
// coefficients, clamped nonnegative. The caller applies the broadband
// brightness `exp(outputs[0])`, hoisted out of the per-wavelength loop.
//
// Single precision throughout: the modes it reads are float, the result
// is stored as float, and six terms of accumulation cannot use more than
// that, so widening to double only buys conversions in the innermost
// loop the model has.
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
  float t = (wavelenNm - float(rural::WAVELENGTH_MIN)) * INV_WAVELENGTH_DELTA;
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
  cosView = std::max(cosZ, float(COS_VIEW_ZENITH_MAX));
  viewZenithDeg =
      std::min(fastAcos(cosZ) * RAD_TO_DEG_F, float(VIEW_ZENITH_MAX_DEG));
  horz = float2(dir.x, dir.y);
  const float len = length(horz);
  horz = len > 1.0e-12f ? horz * (1.0f / len) : float2(1.0f, 0.0f);
}

} // namespace

void SunSky::evalSkyFit(float cosView, float viewZenithDeg,
                        float cosRelativeAzimuth,
                        float (&outputs)[SKY_FIT_OUTPUT_COUNT]) const noexcept {
  // The header cannot name the fit tables, so the counts it declares are
  // checked against them here.
  static_assert(SKY_FIT_OUTPUT_COUNT == int(rural::SKY_OUTPUT_COUNT));
  static_assert(SKY_FIT_TERM_COUNT >= int(SKY_TERM_STRIDE));
  static_assert(SKY_FIT_OUTPUT_STRIDE == int(SKY_PACK_WIDTH));
  // As (1 - c)(1 + c) rather than 1 - c*c, which cancels away most of the
  // significand looking near the zenith, where c is close to one.
  const float sinView{
      std::sqrt(std::max((1.0f - cosView) * (1.0f + cosView), 0.0f))};
  const float cosPsi{std::clamp(
      mCosSunZenith * cosView + mSinSunZenith * sinView * cosRelativeAzimuth,
      -1.0f, 1.0f)};
  const float psiDeg{fastAcos(cosPsi) * RAD_TO_DEG_F};
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
      1.0f,
  };
  // The monomials in the canonical order of `SKY_MONOMIALS`, built by
  // the same nested loops so the two cannot drift apart. Every bound is
  // a constant, so this unrolls to straight-line multiplies with no term
  // table to read.
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
  // Everything downstream of the fit tables stays in their native
  // units; the unit conversion rides along with the user's scale on
  // every radiance output.
  mScaleFactor = float(NATIVE_TO_W_M2_SR_NM * double(options.scaleFactor));
  mSunEnabled = options.enableSun;
  mVisibility = std::clamp(double(options.visibility), VISIBILITY_MIN_KM,
                           VISIBILITY_MAX_KM);
  mWaterVapor = std::clamp(double(options.waterVaporScale), WATER_VAPOR_MIN,
                           WATER_VAPOR_MAX);

  // The effective sun direction: the given azimuth at the clamped
  // zenith, so the disk stays centered on the aureole the sky fit
  // produces.
  {
    const float3 given = normalize(options.sunDirection);
    const double cosZ = std::clamp(double(given.z), -1.0, 1.0);
    mSunZenithDeg = std::clamp(std::acos(cosZ) / DEG_TO_RAD, SUN_ZENITH_MIN_DEG,
                               SUN_ZENITH_MAX_DEG);
    mSunDirHorz = float2(given.x, given.y);
    const float len = length(mSunDirHorz);
    mSunDirHorz =
        len > 1.0e-12f ? mSunDirHorz * (1.0f / len) : float2(1.0f, 0.0f);
    const double sinZ = std::sin(mSunZenithDeg * DEG_TO_RAD);
    mSunDir = float3(float(mSunDirHorz.x * sinZ), float(mSunDirHorz.y * sinZ),
                     float(std::cos(mSunZenithDeg * DEG_TO_RAD)));
  }

  // The sky fit's standardized features split into a part the options
  // fix and a part the view direction supplies. Everything fixed is
  // computed here, so no evaluation repeats it.
  const double cosSunZenithD{std::cos(mSunZenithDeg * DEG_TO_RAD)};
  mCosSunZenith = float(cosSunZenithD);
  mSinSunZenith = float(std::sin(mSunZenithDeg * DEG_TO_RAD));
  mZSunZenith = standardizeSky(0, cosSunZenithD);
  mZSunAirmass =
      standardizeSky(2, std::log(airmass(mSunZenithDeg, cosSunZenithD)));
  mZVisibility = standardizeSky(7, std::log(mVisibility));
  mZWaterVapor = standardizeSky(8, std::log(mWaterVapor));

  // Specialize the polynomial to those four: with them fixed, every term
  // is a constant times a monomial in the five features the view
  // supplies, so fold the constant in and sum the terms that share a
  // monomial. The sum runs in double and lands in the float kernel the
  // evaluation had anyway, so this trades no precision for the terms it
  // removes.
  {
    // The fixed features by their index in the fit's feature vector; the
    // view features stand at one, so a term's product over all of them is
    // exactly its fixed part.
    double zFixed[rural::SKY_FEATURE_COUNT];
    for (auto &value : zFixed) value = 1.0;
    zFixed[0] = mZSunZenith;
    zFixed[2] = mZSunAirmass;
    zFixed[7] = mZVisibility;
    zFixed[8] = mZWaterVapor;
    double folded[SKY_TERM_STRIDE][SKY_PACK_WIDTH]{};
    for (size_t q = 0; q < rural::SKY_OUTPUT_COUNT; q++) {
      for (uint16_t c = rural::SKY_COEFF_OFFSETS[q];
           c < rural::SKY_COEFF_OFFSETS[q + 1]; c++) {
        const size_t t = rural::SKY_COEFFS[c].term;
        double fixed = double(rural::SKY_COEFFS[c].weight);
        for (const int8_t f : rural::SKY_TERM_FEATURES[t])
          if (f >= 0) fixed *= zFixed[size_t(f)];
        folded[skyMonomialOf(t)][q] += fixed;
      }
    }
    for (size_t t = 0; t < SKY_TERM_STRIDE; t++)
      for (size_t q = 0; q < SKY_PACK_WIDTH; q++)
        mSkyMatrix[t][q] = float(folded[t][q]);
  }

  // Moonlight mode: the per-channel lunar multiplier rides on top of
  // both fits, which otherwise run identically with the source at the
  // moon's position. Scattered radiance is linear in the source
  // irradiance, so this is exact for the atmosphere.
  if (options.moon) {
    const double phaseDeg =
        std::clamp(double(options.moonPhase), -180.0, 180.0);
    const double distanceScale =
        std::max(double(options.moonDistanceScale), 0.0);
    const auto multiplier =
        rolo_moon::evaluateMoonMultiplier(phaseDeg, distanceScale);
    mChannelScale.assign(multiplier.begin(), multiplier.end());
  }

  // The direct solar irradiance is independent of the view direction,
  // so evaluate the whole spectrum once, capped channel-wise at the
  // TOA irradiance exactly as in the fit.
  {
    double outputs[rural::SUN_OUTPUT_COUNT]{};
    evalSunOutputs(mSunZenithDeg, mVisibility, mWaterVapor, outputs);
    const double brightness = std::exp(outputs[0]);
    mSunIrradiance.resize(rural::WAVELENGTH_COUNT);
    for (size_t i = 0; i < rural::WAVELENGTH_COUNT; ++i) {
      double shape = double(rural::SUN_MEAN_SHAPE[i]);
      for (size_t m = 0; m < rural::SUN_MODE_COUNT; ++m)
        shape += outputs[1 + m] * double(rural::SUN_MODES[m][i]);
      mSunIrradiance[i] = std::min(float(brightness * std::max(shape, 0.0)),
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
  double sumMeanShape = 0.0;
  double sumModes[rural::SKY_MODE_COUNT]{};
  for (size_t i = 0; i < rural::WAVELENGTH_COUNT; ++i) {
    const double scale = mChannelScale.empty() ? 1.0 : double(mChannelScale[i]);
    sumMeanShape += scale * double(rural::SKY_MEAN_SHAPE[i]);
    for (size_t m = 0; m < rural::SKY_MODE_COUNT; ++m)
      sumModes[m] += scale * double(rural::SKY_MODES[m][i]);
  }

  // Tabulate broadband sky radiance on the lat-long grid.
  auto weights{std::vector<float>{}};
  weights.reserve(size_t(SKY_DISTR_SIZE_X) * SKY_DISTR_SIZE_Y);
  double radianceSum{};
  double sinThetaSum{};
  double skyIntegral{};
  const double dTheta = double(PI) / SKY_DISTR_SIZE_Y;
  const double dPhi = 2.0 * double(PI) / SKY_DISTR_SIZE_X;
  for (int iY = 0; iY < SKY_DISTR_SIZE_Y; iY++) {
    const double theta = dTheta * (iY + 0.5);
    const double sinTheta = std::sin(theta);
    const double viewZenithDeg =
        std::min(theta / DEG_TO_RAD, VIEW_ZENITH_MAX_DEG);
    const double cosView = std::max(std::cos(theta), COS_VIEW_ZENITH_MAX);
    for (int iX = 0; iX < SKY_DISTR_SIZE_X; iX++) {
      const double phi = dPhi * (iX + 0.5);
      const double cosRelAz = double(mSunDirHorz.x) * std::cos(phi) +
                              double(mSunDirHorz.y) * std::sin(phi);
      float outputs[SKY_FIT_OUTPUT_COUNT]{};
      evalSkyFit(float(cosView), float(viewZenithDeg), float(cosRelAz),
                 outputs);
      double shapeSum = sumMeanShape;
      for (size_t m = 0; m < rural::SKY_MODE_COUNT; ++m)
        shapeSum += double(outputs[1 + m]) * sumModes[m];
      const double broadband = fastExp(double(outputs[0])) *
                               std::max(shapeSum, 0.0) /
                               rural::WAVELENGTH_COUNT;
      weights.push_back(float(sinTheta * broadband));
      radianceSum += sinTheta * broadband;
      sinThetaSum += sinTheta;
      skyIntegral += broadband * sinTheta * dTheta * dPhi;
    }
  }
  const double meanSkyRadiance =
      sinThetaSum > 0 ? radianceSum / sinThetaSum : 0.0;

  // MIS compensation, matching `EnvLight` in smdl-toy: subtract the
  // mean radiance from the tabulated density and clamp at zero,
  // falling back to the uncompensated weights if compensation removes
  // everything.
  if (options.enableMISCompensation) {
    auto compensated{weights};
    double compensatedSum{};
    size_t texel{};
    for (int iY = 0; iY < SKY_DISTR_SIZE_Y; iY++) {
      const double sinTheta = std::sin(dTheta * (iY + 0.5));
      for (int iX = 0; iX < SKY_DISTR_SIZE_X; iX++, texel++) {
        const double value =
            sinTheta > 0 ? double(weights[texel]) / sinTheta : 0.0;
        compensated[texel] =
            float(sinTheta * std::max(value - meanSkyRadiance, 0.0));
        compensatedSum += compensated[texel];
      }
    }
    if (compensatedSum > 0) weights = std::move(compensated);
  }
  mSkyDistr = Distribution2D(SKY_DISTR_SIZE_X, SKY_DISTR_SIZE_Y, weights);

  // Select the sun against the sky by broadband power. The sun-disk
  // radiance integrated over its solid angle is just the broadband
  // direct irradiance, in the same channel-mean convention as the sky
  // integral.
  double sunIntegral = 0.0;
  for (size_t i = 0; i < rural::WAVELENGTH_COUNT; ++i)
    sunIntegral += double(mSunIrradiance[i]);
  sunIntegral /= rural::WAVELENGTH_COUNT;
  if (!mSunEnabled) sunIntegral = 0.0;
  mSunSelectionChance =
      sunIntegral + skyIntegral > 0
          ? float(std::min(sunIntegral / (sunIntegral + skyIntegral), 0.999))
          : 0.0f;
  mMeanRadiance =
      float((skyIntegral + sunIntegral) / (4.0 * double(PI)) * mScaleFactor);
}

void SunSky::resolve(Span<const float> wavelens, SkyBasis &basis) const {
  const size_t numBands{wavelens.size()};
  basis.mNumBands = int(numBands);
  basis.mRows.assign((1 + rural::SKY_MODE_COUNT) * numBands, 0.0f);
  basis.mSunIrradiance.assign(numBands, 0.0f);
  if (mSunIrradiance.empty()) return; // default-constructed
  for (size_t j = 0; j < numBands; ++j) {
    const auto lerp = channelOf(wavelens[j]);
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

void SunSky::skyRadiance(const float3 &direction, const SkyBasis &basis,
                         float *radiance) const {
  const int numBands{basis.mNumBands};
  if (mSunIrradiance.empty()) { // default-constructed
    std::fill(radiance, radiance + numBands, 0.0f);
    return;
  }
  const float3 wi = normalize(direction);
  float cosView{};
  float viewZenithDeg{};
  float2 horizontal{};
  viewGeometry(wi, cosView, viewZenithDeg, horizontal);
  const float cosRelAz = dot(horizontal, mSunDirHorz);
  float outputs[SKY_FIT_OUTPUT_COUNT]{};
  evalSkyFit(cosView, viewZenithDeg, cosRelAz, outputs);
  const float brightnessScale = fastExp(outputs[0]) * mScaleFactor;
  // One contraction over the modes, band by band. The rows are
  // `restrict` against the output because a caller passing a band buffer
  // that overlaps them would otherwise cost a runtime overlap check per
  // row in front of a loop this short.
  const float *SMDL_RESTRICT rows[1 + rural::SKY_MODE_COUNT];
  for (size_t m = 0; m < 1 + rural::SKY_MODE_COUNT; ++m)
    rows[m] = basis.mRows.data() + m * size_t(numBands);
  float *SMDL_RESTRICT out{radiance};
  for (int j = 0; j < numBands; j++) {
    float shape{rows[0][j]};
    for (size_t m = 0; m < rural::SKY_MODE_COUNT; ++m)
      shape += outputs[1 + m] * rows[1 + m][j];
    out[j] = std::max(shape, 0.0f) * brightnessScale;
  }
}

void SunSky::radiance(const float3 &direction, const SkyBasis &basis,
                      float *radiance) const {
  skyRadiance(direction, basis, radiance);
  if (mSunEnabled && !mSunIrradiance.empty() &&
      dot(normalize(direction), mSunDir) >= float(COS_SUN_ANGULAR_RADIUS)) {
    const float diskScale = float(double(mScaleFactor) / SUN_SOLID_ANGLE);
    const float *SMDL_RESTRICT disk{basis.mSunIrradiance.data()};
    float *SMDL_RESTRICT out{radiance};
    for (int j = 0; j < basis.mNumBands; j++) out[j] += disk[j] * diskScale;
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
  if (!mSunEnabled || mSunIrradiance.empty()) {
    std::fill(radiance, radiance + numWavelens, 0.0f);
    return;
  }
  const auto diskScale = float(double(mScaleFactor) / SUN_SOLID_ANGLE);
  for (int j = 0; j < numWavelens; j++) {
    const auto lerp = channelOf(wavelens[j]);
    const float value0 = mSunIrradiance[lerp.i0];
    const float value1 = mSunIrradiance[lerp.i1];
    radiance[j] = (value0 + lerp.frac * (value1 - value0)) * diskScale;
  }
}

void SunSky::radiance(const float3 &direction, int numWavelens,
                      const float *wavelens, float *radiance) const {
  skyRadiance(direction, numWavelens, wavelens, radiance);
  if (mSunEnabled && !mSunIrradiance.empty() &&
      dot(normalize(direction), mSunDir) >= float(COS_SUN_ANGULAR_RADIUS)) {
    const auto diskScale = float(double(mScaleFactor) / SUN_SOLID_ANGLE);
    for (int j = 0; j < numWavelens; j++) {
      const auto lerp = channelOf(wavelens[j]);
      const float value0 = mSunIrradiance[lerp.i0];
      const float value1 = mSunIrradiance[lerp.i1];
      radiance[j] += (value0 + lerp.frac * (value1 - value0)) * diskScale;
    }
  }
}

double SunSky::moonMultiplier(double wavelenNm, double phaseDeg,
                              double distanceScale) {
  return rolo_moon::moonMultiplier(wavelenNm * 1.0e-3, phaseDeg, distanceScale);
}

float SunSky::cosSunAngularRadius() const noexcept {
  // TODO Move constant into header and inline this
  return float(COS_SUN_ANGULAR_RADIUS);
}

float SunSky::sunSolidAngle() const noexcept {
  // TODO Move constant into header and inline this
  return float(SUN_SOLID_ANGLE);
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
                      uniformConeSample(float(COS_SUN_ANGULAR_RADIUS), xi);
    if (pdf) *pdf = this->pdf(wi);
    return wi;
  }
  xi.x = (xi.x - pmfSun) / (1.0f - pmfSun); // remap
  float pdfSky{};
  const float3 wi = mSkyDistr.directionSample(xi, nullptr, &pdfSky);
  if (pdf) {
    *pdf = (1.0f - pmfSun) * pdfSky;
    if (pmfSun > 0 && dot(wi, mSunDir) >= float(COS_SUN_ANGULAR_RADIUS))
      *pdf += pmfSun * uniformConePDF(float(COS_SUN_ANGULAR_RADIUS));
  }
  return wi;
}

float SunSky::pdf(const float3 &direction) const noexcept {
  if (mSkyDistr.getNumTexelsX() == 0) return 0.0f;
  const float3 wi = normalize(direction);
  const float pSun = mSunSelectionChance;
  float result = (1.0f - pSun) * mSkyDistr.directionPDF(wi);
  if (pSun > 0 && dot(wi, mSunDir) >= float(COS_SUN_ANGULAR_RADIUS))
    result += pSun * uniformConePDF(float(COS_SUN_ANGULAR_RADIUS));
  return result;
}

} // namespace smdl
