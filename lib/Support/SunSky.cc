#include "smdl/Support/SunSky.h"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdint>

#include "SunSkyRoloMoon.h"
#include "SunSkyRural.h"

namespace smdl {

namespace {

// The lunar multiplier is generated on the same 421-channel grid as
// the sun-sky fit, so the channels line up one-to-one.
static_assert(rolo_moon::kSpectrumSize == rural_sun_sky::WAVELENGTH_COUNT);

constexpr double DEG_TO_RAD = 0.017453292519943295;

// The trained parameter ranges
constexpr double SUN_ZENITH_MIN_DEG = 5.0;
constexpr double SUN_ZENITH_MAX_DEG = 88.0;
constexpr double VIEW_ZENITH_MAX_DEG = 88.0;
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

// Kasten-Young relative airmass.
[[nodiscard]] double airmass(double zenithDeg) {
  return 1.0 / (std::cos(zenithDeg * DEG_TO_RAD) +
                0.50572 * std::pow(96.07995 - zenithDeg, -1.6364));
}

// Expand standardized features into sparse polynomial terms and
// accumulate the coefficient dot products. This is the shared kernel
// of both fits.
template <std::size_t NumFeatures, std::size_t NumTerms, std::size_t NumOutputs,
          std::size_t TermWidth>
void evalSparsePolynomial(
    const double (&raw)[NumFeatures], const double (&featureMean)[NumFeatures],
    const double (&featureStd)[NumFeatures],
    const std::int8_t (&termFeatures)[NumTerms][TermWidth],
    const rural_sun_sky::Coeff *coefs,
    const std::uint16_t (&coefOffsets)[NumOutputs + 1],
    double (&outputs)[NumOutputs]) {
  double z[NumFeatures]{};
  for (std::size_t i = 0; i < NumFeatures; ++i)
    z[i] = (raw[i] - featureMean[i]) / featureStd[i];
  double terms[NumTerms]{};
  for (std::size_t t = 0; t < NumTerms; ++t) {
    double value = 1.0;
    for (std::size_t slot = 0; slot < TermWidth; ++slot) {
      if (const std::int8_t f = termFeatures[t][slot]; f >= 0)
        value *= z[std::size_t(f)];
    }
    terms[t] = value;
  }
  for (std::size_t q = 0; q < NumOutputs; ++q) {
    double sum = 0.0;
    for (std::uint16_t c = coefOffsets[q]; c < coefOffsets[q + 1]; ++c)
      sum += double(coefs[c].weight) * terms[coefs[c].term];
    outputs[q] = sum;
  }
}

// Evaluate the sky-fit outputs: log broadband brightness, then the
// spectral mode coefficients. The zenith angles must already be
// clamped to the trained ranges; the relative azimuth enters only
// through the scattering angle.
void evalSkyOutputs(double sunZenithDeg, double viewZenithDeg,
                    double cosRelativeAzimuth, double visibility,
                    double waterVapor,
                    double (&outputs)[rural_sun_sky::SKY_OUTPUT_COUNT]) {
  const double cosS = std::cos(sunZenithDeg * DEG_TO_RAD);
  const double sinS = std::sin(sunZenithDeg * DEG_TO_RAD);
  const double cosV = std::cos(viewZenithDeg * DEG_TO_RAD);
  const double sinV = std::sin(viewZenithDeg * DEG_TO_RAD);
  const double cosPsi = std::min(
      std::max(cosS * cosV + sinS * sinV * cosRelativeAzimuth, -1.0), 1.0);
  const double psiDeg = std::acos(cosPsi) / DEG_TO_RAD;
  const double raw[rural_sun_sky::SKY_FEATURE_COUNT] = {
      cosS,
      cosV,
      std::log(airmass(sunZenithDeg)),
      std::log(airmass(viewZenithDeg)),
      cosPsi,
      std::exp(-psiDeg / 15.0),
      std::log(psiDeg + 3.0),
      std::log(visibility),
      std::log(waterVapor),
  };
  evalSparsePolynomial(
      raw, rural_sun_sky::SKY_FEATURE_MEAN, rural_sun_sky::SKY_FEATURE_STD,
      rural_sun_sky::SKY_TERM_FEATURES, rural_sun_sky::SKY_COEFFS,
      rural_sun_sky::SKY_COEFF_OFFSETS, outputs);
}

// Evaluate the direct-beam-fit outputs, same conventions.
void evalSunOutputs(double sunZenithDeg, double visibility, double waterVapor,
                    double (&outputs)[rural_sun_sky::SUN_OUTPUT_COUNT]) {
  const double m = airmass(sunZenithDeg);
  const double raw[rural_sun_sky::SUN_FEATURE_COUNT] = {
      std::cos(sunZenithDeg * DEG_TO_RAD),
      std::log(m),
      std::log(visibility),
      std::log(waterVapor),
      m,
  };
  evalSparsePolynomial(
      raw, rural_sun_sky::SUN_FEATURE_MEAN, rural_sun_sky::SUN_FEATURE_STD,
      rural_sun_sky::SUN_TERM_FEATURES, rural_sun_sky::SUN_COEFFS,
      rural_sun_sky::SUN_COEFF_OFFSETS, outputs);
}

// The sky radiance of channel `i` for the given fit outputs.
[[nodiscard]] double
skyChannel(const double (&outputs)[rural_sun_sky::SKY_OUTPUT_COUNT],
           std::size_t i) {
  double shape = double(rural_sun_sky::SKY_MEAN_SHAPE[i]);
  for (std::size_t m = 0; m < rural_sun_sky::SKY_MODE_COUNT; ++m)
    shape += outputs[m + 1] * double(rural_sun_sky::SKY_MODES[m][i]);
  return std::exp(outputs[0]) * std::max(shape, 0.0);
}

// The continuous channel coordinate of the given wavelength in
// nanometers, clamped to the grid. Non-finite wavelengths clamp to
// the first channel.
struct ChannelLerp final {
  std::size_t i0{}, i1{};
  double frac{};
};
[[nodiscard]] ChannelLerp channelOf(double wavelenNm) {
  double t = (wavelenNm - rural_sun_sky::WAVELENGTH_MIN) /
             rural_sun_sky::WAVELENGTH_DELTA;
  if (!(t > 0.0)) t = 0.0;
  if (t > double(rural_sun_sky::WAVELENGTH_COUNT - 1))
    t = double(rural_sun_sky::WAVELENGTH_COUNT - 1);
  ChannelLerp lerp{};
  lerp.i0 = std::size_t(t);
  lerp.i1 =
      std::min(lerp.i0 + 1, std::size_t(rural_sun_sky::WAVELENGTH_COUNT - 1));
  lerp.frac = t - double(lerp.i0);
  return lerp;
}

// The clamped view zenith angle in degrees and the unit horizontal
// projection of the given unit direction (or +X if degenerate).
void viewGeometry(const float3 &direction, double &viewZenithDeg,
                  float2 &horizontal) {
  const double cosV = std::min(std::max(double(direction.z), -1.0), 1.0);
  viewZenithDeg = std::min(std::acos(cosV) / DEG_TO_RAD, VIEW_ZENITH_MAX_DEG);
  horizontal = float2(direction.x, direction.y);
  const float len = std::sqrt(lengthSquared(horizontal));
  horizontal = len > 1.0e-12f ? horizontal * (1.0f / len) : float2(1.0f, 0.0f);
}

} // namespace

SunSky::SunSky(const SunSkyOptions &options) {
  // Everything downstream of the fit tables stays in their native
  // units; the unit conversion rides along with the user's scale on
  // every radiance output.
  scaleFactor = float(NATIVE_TO_W_M2_SR_NM * double(options.scaleFactor));
  sunEnabled = options.enableSun;
  visibility = std::min(std::max(double(options.visibility), VISIBILITY_MIN_KM),
                        VISIBILITY_MAX_KM);
  waterVapor =
      std::min(std::max(double(options.waterVaporScale), WATER_VAPOR_MIN),
               WATER_VAPOR_MAX);

  // The effective sun direction: the given azimuth at the clamped
  // zenith, so the disk stays centered on the aureole the sky fit
  // produces.
  {
    const float3 given = normalize(options.sunDirection);
    const double cosZ = std::min(std::max(double(given.z), -1.0), 1.0);
    sunZenithDeg =
        std::min(std::max(std::acos(cosZ) / DEG_TO_RAD, SUN_ZENITH_MIN_DEG),
                 SUN_ZENITH_MAX_DEG);
    sunDirHorizontal = float2(given.x, given.y);
    const float len = std::sqrt(lengthSquared(sunDirHorizontal));
    sunDirHorizontal =
        len > 1.0e-12f ? sunDirHorizontal * (1.0f / len) : float2(1.0f, 0.0f);
    const double sinZ = std::sin(sunZenithDeg * DEG_TO_RAD);
    sunDir = float3(float(sunDirHorizontal.x * sinZ),
                    float(sunDirHorizontal.y * sinZ),
                    float(std::cos(sunZenithDeg * DEG_TO_RAD)));
  }

  // Moonlight mode: the per-channel lunar multiplier rides on top of
  // both fits, which otherwise run identically with the source at the
  // moon's position. Scattered radiance is linear in the source
  // irradiance, so this is exact for the atmosphere.
  if (options.moon) {
    const double phaseDeg =
        std::min(std::max(double(options.moonPhase), -180.0), 180.0);
    const double distanceScale =
        std::max(double(options.moonDistanceScale), 0.0);
    const auto multiplier =
        rolo_moon::EvaluateMoonMultiplier(phaseDeg, distanceScale);
    channelScale.assign(multiplier.begin(), multiplier.end());
  }

  // The direct solar irradiance is independent of the view direction,
  // so evaluate the whole spectrum once, capped channel-wise at the
  // TOA irradiance exactly as in the fit.
  {
    double outputs[rural_sun_sky::SUN_OUTPUT_COUNT]{};
    evalSunOutputs(sunZenithDeg, visibility, waterVapor, outputs);
    const double brightness = std::exp(outputs[0]);
    sunIrradiance.resize(rural_sun_sky::WAVELENGTH_COUNT);
    for (std::size_t i = 0; i < rural_sun_sky::WAVELENGTH_COUNT; ++i) {
      double shape = double(rural_sun_sky::SUN_MEAN_SHAPE[i]);
      for (std::size_t m = 0; m < rural_sun_sky::SUN_MODE_COUNT; ++m)
        shape += outputs[1 + m] * double(rural_sun_sky::SUN_MODES[m][i]);
      sunIrradiance[i] = std::min(float(brightness * std::max(shape, 0.0)),
                                  rural_sun_sky::SOLAR_IRRADIANCE[i]);
      if (!channelScale.empty()) sunIrradiance[i] *= channelScale[i];
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
  double sumModes[rural_sun_sky::SKY_MODE_COUNT]{};
  for (std::size_t i = 0; i < rural_sun_sky::WAVELENGTH_COUNT; ++i) {
    const double scale = channelScale.empty() ? 1.0 : double(channelScale[i]);
    sumMeanShape += scale * double(rural_sun_sky::SKY_MEAN_SHAPE[i]);
    for (std::size_t m = 0; m < rural_sun_sky::SKY_MODE_COUNT; ++m)
      sumModes[m] += scale * double(rural_sun_sky::SKY_MODES[m][i]);
  }

  // Tabulate broadband sky radiance on the lat-long grid.
  auto weights{std::vector<float>{}};
  weights.reserve(std::size_t(SKY_DISTR_SIZE_X) * SKY_DISTR_SIZE_Y);
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
    for (int iX = 0; iX < SKY_DISTR_SIZE_X; iX++) {
      const double phi = dPhi * (iX + 0.5);
      const double cosRelAz = double(sunDirHorizontal.x) * std::cos(phi) +
                              double(sunDirHorizontal.y) * std::sin(phi);
      double outputs[rural_sun_sky::SKY_OUTPUT_COUNT]{};
      evalSkyOutputs(sunZenithDeg, viewZenithDeg, cosRelAz, visibility,
                     waterVapor, outputs);
      double shapeSum = sumMeanShape;
      for (std::size_t m = 0; m < rural_sun_sky::SKY_MODE_COUNT; ++m)
        shapeSum += outputs[1 + m] * sumModes[m];
      const double broadband = std::exp(outputs[0]) * std::max(shapeSum, 0.0) /
                               rural_sun_sky::WAVELENGTH_COUNT;
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
    std::size_t texel{};
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
  skyDistr = Distribution2D(SKY_DISTR_SIZE_X, SKY_DISTR_SIZE_Y, weights);

  // Select the sun against the sky by broadband power. The sun-disk
  // radiance integrated over its solid angle is just the broadband
  // direct irradiance, in the same channel-mean convention as the sky
  // integral.
  double sunIntegral = 0.0;
  for (std::size_t i = 0; i < rural_sun_sky::WAVELENGTH_COUNT; ++i)
    sunIntegral += double(sunIrradiance[i]);
  sunIntegral /= rural_sun_sky::WAVELENGTH_COUNT;
  if (!sunEnabled) sunIntegral = 0.0;
  sunSelectionProbability =
      sunIntegral + skyIntegral > 0
          ? float(std::min(sunIntegral / (sunIntegral + skyIntegral), 0.999))
          : 0.0f;
  meanRadiance =
      float((skyIntegral + sunIntegral) / (4.0 * double(PI)) * scaleFactor);
}

void SunSky::skyRadiance(const float3 &direction, int numWavelens,
                         const float *wavelens, float *radiance) const {
  if (sunIrradiance.empty()) { // default-constructed
    std::fill(radiance, radiance + numWavelens, 0.0f);
    return;
  }
  const float3 wi = normalize(direction);
  double viewZenithDeg{};
  float2 horizontal{};
  viewGeometry(wi, viewZenithDeg, horizontal);
  const double cosRelAz = double(dot(horizontal, sunDirHorizontal));
  double outputs[rural_sun_sky::SKY_OUTPUT_COUNT]{};
  evalSkyOutputs(sunZenithDeg, viewZenithDeg, cosRelAz, visibility, waterVapor,
                 outputs);
  for (int j = 0; j < numWavelens; j++) {
    const auto lerp = channelOf(double(wavelens[j]));
    double value0 = skyChannel(outputs, lerp.i0);
    double value1 = skyChannel(outputs, lerp.i1);
    if (!channelScale.empty()) {
      value0 *= double(channelScale[lerp.i0]);
      value1 *= double(channelScale[lerp.i1]);
    }
    radiance[j] = float((value0 + lerp.frac * (value1 - value0)) * scaleFactor);
  }
}

void SunSky::sunRadiance(int numWavelens, const float *wavelens,
                         float *radiance) const {
  if (!sunEnabled || sunIrradiance.empty()) {
    std::fill(radiance, radiance + numWavelens, 0.0f);
    return;
  }
  for (int j = 0; j < numWavelens; j++) {
    const auto lerp = channelOf(double(wavelens[j]));
    const auto value0 = double(sunIrradiance[lerp.i0]);
    const auto value1 = double(sunIrradiance[lerp.i1]);
    radiance[j] = float((value0 + lerp.frac * (value1 - value0)) /
                        SUN_SOLID_ANGLE * scaleFactor);
  }
}

void SunSky::radiance(const float3 &direction, int numWavelens,
                      const float *wavelens, float *radiance) const {
  skyRadiance(direction, numWavelens, wavelens, radiance);
  if (sunEnabled && !sunIrradiance.empty() &&
      dot(normalize(direction), sunDir) >= float(COS_SUN_ANGULAR_RADIUS)) {
    for (int j = 0; j < numWavelens; j++) {
      const auto lerp = channelOf(double(wavelens[j]));
      const auto value0 = double(sunIrradiance[lerp.i0]);
      const auto value1 = double(sunIrradiance[lerp.i1]);
      radiance[j] += float((value0 + lerp.frac * (value1 - value0)) /
                           SUN_SOLID_ANGLE * scaleFactor);
    }
  }
}

double SunSky::moonMultiplier(double wavelenNm, double phaseDeg,
                              double distanceScale) {
  return rolo_moon::MoonMultiplier(wavelenNm * 1.0e-3, phaseDeg, distanceScale);
}

float SunSky::cosSunAngularRadius() const noexcept {
  return float(COS_SUN_ANGULAR_RADIUS);
}

float SunSky::sunSolidAngle() const noexcept { return float(SUN_SOLID_ANGLE); }

float3 SunSky::sample(float2 xi, float *pdf) const noexcept {
  if (skyDistr.getNumTexelsX() == 0) {
    if (pdf) *pdf = 0.0f;
    return {0.0f, 0.0f, 1.0f};
  }
  const float pSun = sunSelectionProbability;
  if (xi.x < pSun) {
    xi.x /= pSun; // remap
    const float3 wi = coordinateSystem(sunDir) *
                      uniformConeSample(float(COS_SUN_ANGULAR_RADIUS), xi);
    if (pdf) *pdf = this->pdf(wi);
    return wi;
  }
  xi.x = (xi.x - pSun) / (1.0f - pSun); // remap
  float skyPDF{};
  const float3 wi = skyDistr.directionSample(xi, nullptr, &skyPDF);
  if (pdf) {
    *pdf = (1.0f - pSun) * skyPDF;
    if (pSun > 0 && dot(wi, sunDir) >= float(COS_SUN_ANGULAR_RADIUS))
      *pdf += pSun * uniformConePDF(float(COS_SUN_ANGULAR_RADIUS));
  }
  return wi;
}

float SunSky::pdf(const float3 &direction) const noexcept {
  if (skyDistr.getNumTexelsX() == 0) return 0.0f;
  const float3 wi = normalize(direction);
  const float pSun = sunSelectionProbability;
  float result = (1.0f - pSun) * skyDistr.directionPDF(wi);
  if (pSun > 0 && dot(wi, sunDir) >= float(COS_SUN_ANGULAR_RADIUS))
    result += pSun * uniformConePDF(float(COS_SUN_ANGULAR_RADIUS));
  return result;
}

} // namespace smdl
