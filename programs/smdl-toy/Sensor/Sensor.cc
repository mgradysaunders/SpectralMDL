#include <algorithm>
#include <array>
#include <cmath>
#include <cstddef>

#include "smdl/RenderUtil/Colorimetry.h"
#include "smdl/RenderUtil/Illuminant.h"
#include "smdl/Support/Parallel.h"

#include "Sensor/Response.h"
#include "Sensor/Sensor.h"

namespace {

#include "Sensor/TrainingReflectances.inl"

// A spectrum on the grid from one of the library's illuminant tables,
// which evaluate in single precision: `evaluate(count, wavelengths,
// values)`.
template <typename F> [[nodiscard]] SensorSpectrum tabulated(F &&evaluate) {
  auto wavelengths{std::vector<float>(SENSOR_WAVELENGTH_COUNT)};
  auto values{std::vector<float>(SENSOR_WAVELENGTH_COUNT)};
  for (size_t i = 0; i < SENSOR_WAVELENGTH_COUNT; i++)
    wavelengths[i] = float(sensorWavelength(i));
  evaluate(int(SENSOR_WAVELENGTH_COUNT), wavelengths.data(), values.data());
  return {values.begin(), values.end()};
}

// Photons per joule at `lambda` nanometers.
[[nodiscard]] double photonsPerJoule(double lambda) noexcept {
  return lambda * 1e-9 / (PLANCK * SPEED_OF_LIGHT);
}

// The mean of `V` over `[lo, hi]` by the midpoint rule, fine enough that
// the error is nothing against the band's own width.
[[nodiscard]] double meanLuminousEfficiency(double lo, double hi) noexcept {
  constexpr int NUM_STEPS{64};
  double total{};
  for (int k = 0; k < NUM_STEPS; k++)
    total +=
        smdl::wymanY(lo + (hi - lo) * (double(k) + 0.5) / double(NUM_STEPS));
  return total / double(NUM_STEPS);
}

} // namespace

SensorSpectrum daylightSpectrum(double kelvin) {
  auto xy{float2()};
  smdlKelvinToChromaticity(float(kelvin), &xy);
  return tabulated([&](int count, const float *wavelengths, float *values) {
    smdlEvalIlluminantD(count, wavelengths, values, xy);
  });
}

SensorSpectrum planckSpectrum(double kelvin) {
  // The second radiation constant h c / k in meter kelvins.
  constexpr double SECOND_RADIATION_CONSTANT{1.438776877e-2};
  const auto radiance{[kelvin](double lambda) {
    const double meters{lambda * 1e-9};
    return 1.0 / (std::pow(meters, 5.0) *
                  std::expm1(SECOND_RADIATION_CONSTANT / (meters * kelvin)));
  }};
  const double at560{radiance(560.0)};
  auto spectrum{SensorSpectrum(SENSOR_WAVELENGTH_COUNT)};
  for (size_t i = 0; i < SENSOR_WAVELENGTH_COUNT; i++)
    spectrum[i] = radiance(sensorWavelength(i)) / at560;
  return spectrum;
}

SensorSpectrum kelvinSpectrum(double kelvin) {
  return kelvin >= 4000.0 ? daylightSpectrum(kelvin) : planckSpectrum(kelvin);
}

SensorSpectrum whiteBalanceSpectrum(const WhiteBalance &whiteBalance) {
  switch (whiteBalance.kind) {
  case WhiteBalanceKind::DAYLIGHT:
    return daylightSpectrum(D55_KELVIN);
  case WhiteBalanceKind::SHADE:
    return daylightSpectrum(D75_KELVIN);
  case WhiteBalanceKind::TUNGSTEN:
    return planckSpectrum(ILLUMINANT_A_KELVIN);
  case WhiteBalanceKind::FLUORESCENT:
    return tabulated([](int count, const float *wavelengths, float *values) {
      smdl::smdlEvalIlluminantF(count, wavelengths, values, 2);
    });
  case WhiteBalanceKind::KELVIN:
    return kelvinSpectrum(double(whiteBalance.kelvin));
  default:
    return daylightSpectrum(D65_KELVIN);
  }
}

smdl::double3 illuminantWhite(const SensorSpectrum &illuminant) {
  SMDL_SANITY_CHECK(illuminant.size() == SENSOR_WAVELENGTH_COUNT);
  auto white{smdl::double3()};
  for (size_t i = 0; i < SENSOR_WAVELENGTH_COUNT; i++)
    white += illuminant[i] * smdl::wymanXYZ(sensorWavelength(i));
  return white.y > 0 ? white / white.y : white;
}

const std::vector<SensorSpectrum> &trainingReflectances() {
  static const auto patches{[] {
    auto result{std::vector<SensorSpectrum>(TRAINING_PATCH_COUNT)};
    for (size_t j = 0; j < TRAINING_PATCH_COUNT; j++) {
      const auto &table{TRAINING_REFLECTANCES[j]};
      auto &patch{result[j]};
      patch.resize(SENSOR_WAVELENGTH_COUNT);
      for (size_t i = 0; i < SENSOR_WAVELENGTH_COUNT; i++) {
        const double t{
            std::clamp((sensorWavelength(i) - TRAINING_WAVELENGTH_MIN) /
                           TRAINING_WAVELENGTH_STEP,
                       0.0, double(TRAINING_WAVELENGTH_COUNT - 1))};
        const size_t k{std::min(size_t(t), TRAINING_WAVELENGTH_COUNT - 2)};
        const double f{t - double(k)};
        patch[i] = (1.0 - f) * double(table[k]) + f * double(table[k + 1]);
      }
    }
    return result;
  }()};
  return patches;
}

Sensor::Sensor(const SensorSettings &settings)
    : mSettings(settings), mMaxISO(settings.detector.maxISO) {
  const auto d55{daylightSpectrum(D55_KELVIN)};
  const auto &bands{settings.response.bands};
  for (size_t b = 0; b < bands.size(); b++) {
    const double value{electronsPerLuxSecond(b, d55)};
    if (b == 0 || value > mPeakElectronsPerLuxSecond) {
      mPeakBand = b;
      mPeakElectronsPerLuxSecond = value;
    }
  }
  const auto &detector{settings.detector};
  const double pixelAreaUM2{pixelArea() * 1e12};
  if (detector.fullWell) {
    mFullWell = double(*detector.fullWell);
    mWellSource = WellSource::STATED;
  } else if (detector.baseISO && mPeakElectronsPerLuxSecond > 0) {
    mFullWell = ISO_SATURATION_LUX_SECONDS * mPeakElectronsPerLuxSecond /
                double(*detector.baseISO);
    mWellSource = WellSource::FROM_BASE_ISO;
  } else {
    mFullWell = GENERIC_ELECTRONS_PER_SQUARE_MICROMETER * pixelAreaUM2;
    mWellSource = WellSource::FROM_PITCH;
  }
  // A response with no weight at all counts nothing, so its ISO is
  // whatever it states, and a well the pitch gave it.
  mBaseISO =
      detector.baseISO ? double(*detector.baseISO)
      : mPeakElectronsPerLuxSecond > 0
          ? ISO_SATURATION_LUX_SECONDS * mPeakElectronsPerLuxSecond / mFullWell
          : 100.0;
  mMaxISO = std::max(mMaxISO, mBaseISO);
}

double Sensor::electronRate(size_t band,
                            const SensorSpectrum &illuminant) const {
  SMDL_SANITY_CHECK(illuminant.size() == SENSOR_WAVELENGTH_COUNT);
  const auto &curve{mSettings.response.bands[band]};
  const double scale{mSettings.response.qeScale()};
  double total{};
  for (size_t i = 0; i < SENSOR_WAVELENGTH_COUNT; i++) {
    const double lambda{sensorWavelength(i)};
    total += illuminant[i] * scale * curve.at(lambda) * photonsPerJoule(lambda);
  }
  return total;
}

double Sensor::illuminance(const SensorSpectrum &illuminant) {
  SMDL_SANITY_CHECK(illuminant.size() == SENSOR_WAVELENGTH_COUNT);
  double total{};
  for (size_t i = 0; i < SENSOR_WAVELENGTH_COUNT; i++)
    total += illuminant[i] * smdl::wymanY(sensorWavelength(i));
  return LUMENS_PER_WATT * total;
}

double Sensor::electronsPerLuxSecond(size_t band,
                                     const SensorSpectrum &illuminant) const {
  const double lux{illuminance(illuminant)};
  return lux > 0 ? pixelArea() * electronRate(band, illuminant) / lux : 0.0;
}

double Sensor::gain(double iso) const noexcept {
  const auto &detector{mSettings.detector};
  if (detector.gain) return double(*detector.gain);
  const double codes{double(detector.topCode()) - double(detector.blackLevel)};
  return codes / topCodeElectrons(iso);
}

double Sensor::fixedGainISO() const noexcept {
  const auto &detector{mSettings.detector};
  const double codes{double(detector.topCode()) - double(detector.blackLevel)};
  return mPeakElectronsPerLuxSecond > 0 ? double(detector.gain.value_or(1.0f)) *
                                              ISO_SATURATION_LUX_SECONDS *
                                              mPeakElectronsPerLuxSecond / codes
                                        : mBaseISO;
}

double Sensor::topCodeElectrons(double iso) const noexcept {
  // The saturation exposure at the ISO in the most sensitive band, which
  // at the base ISO is the well by construction, spelled through the
  // well so that a body with no weight in its curves still fills it.
  return mFullWell * mBaseISO / iso;
}

std::vector<double> Sensor::luminanceWeights(const Color &wavelengths) {
  const size_t numBands{wavelengths.size()};
  auto weights{std::vector<double>(numBands)};
  const auto &edges{gRenderGrid.bandEdges};
  if (edges.empty()) {
    const auto widths{wavelengthTrapezoidWidths(wavelengths)};
    for (size_t i = 0; i < numBands; i++)
      weights[i] = smdl::wymanY(double(wavelengths[i])) * widths[i];
  } else {
    for (size_t i = 0; i < numBands; i++) {
      const double lo{double(edges[i])};
      const double hi{double(edges[i + 1])};
      weights[i] = meanLuminousEfficiency(lo, hi) * (hi - lo);
    }
  }
  return weights;
}

MeteredExposure Sensor::meter(const smdl::SpectralFilm &film,
                              const Color &wavelengths, int4 window,
                              double seconds) const {
  const auto weights{luminanceWeights(wavelengths)};
  const size_t numBands{film.getNumBands()};
  SMDL_SANITY_CHECK(numBands == weights.size());
  // Each row sums for itself and the rows fold in order, so the mean
  // never depends on which thread took which row.
  const size_t numRows{size_t(std::max(window[3] - window[1], 0))};
  auto rows{std::vector<double>(numRows)};
  smdl::parallelFor(0, numRows, [&](size_t row) {
    const auto y{size_t(window[1]) + row};
    double total{};
    for (size_t x = size_t(window[0]); x < size_t(window[2]); x++) {
      for (size_t i = 0; i < numBands; i++) {
        // A pixel some material poisoned reads as black, as the
        // tonemap and the readout read it.
        const double value{film.mean(x, y, i)};
        if (std::isfinite(value)) total += value * weights[i];
      }
    }
    rows[row] = total;
  });
  double total{};
  for (const auto value : rows) total += value;
  const size_t numPixels{numRows * size_t(std::max(window[2] - window[0], 0))};
  auto metered{MeteredExposure{}};
  metered.luxSeconds =
      numPixels > 0 ? seconds * LUMENS_PER_WATT * total / double(numPixels)
                    : 0.0;
  metered.wantedISO = metered.luxSeconds > 0
                          ? METER_Q * METER_K / metered.luxSeconds
                          : double(INF);
  metered.iso = std::clamp(metered.wantedISO, mBaseISO, mMaxISO);
  metered.stopsOff =
      metered.wantedISO < mBaseISO  ? std::log2(mBaseISO / metered.wantedISO)
      : metered.wantedISO > mMaxISO ? -std::log2(metered.wantedISO / mMaxISO)
                                    : 0.0;
  return metered;
}

ColorFit Sensor::fitColor(const std::array<size_t, 3> &bands,
                          const SensorSpectrum &illuminant) const {
  SMDL_SANITY_CHECK(illuminant.size() == SENSOR_WAVELENGTH_COUNT);
  auto fit{ColorFit{}};
  fit.bands = bands;
  fit.white = illuminantWhite(illuminant);
  // What each band counts of the illuminant, wavelength by wavelength,
  // and of its white altogether, which normalizes the responses so that
  // the white reads (1, 1, 1).
  const double scale{mSettings.response.qeScale()};
  auto counts{std::array<std::vector<double>, 3>()};
  auto whiteCounts{smdl::double3()};
  for (size_t k = 0; k < 3; k++) {
    const auto &curve{mSettings.response.bands[bands[k]]};
    counts[k].resize(SENSOR_WAVELENGTH_COUNT);
    for (size_t i = 0; i < SENSOR_WAVELENGTH_COUNT; i++) {
      const double lambda{sensorWavelength(i)};
      counts[k][i] =
          illuminant[i] * scale * curve.at(lambda) * photonsPerJoule(lambda);
      whiteCounts[k] += counts[k][i];
    }
  }
  // The observer under the illuminant, which the targets are taken per
  // unit of, so that the white's Y is 1.
  auto observer{std::vector<smdl::double3>(SENSOR_WAVELENGTH_COUNT)};
  double whiteY{};
  for (size_t i = 0; i < SENSOR_WAVELENGTH_COUNT; i++) {
    observer[i] = illuminant[i] * smdl::wymanXYZ(sensorWavelength(i));
    whiteY += observer[i].y;
  }
  if (!(whiteCounts.x > 0 && whiteCounts.y > 0 && whiteCounts.z > 0 &&
        whiteY > 0)) {
    fit.isSingular = true;
    fit.multipliers = smdl::double3(1.0);
    return fit;
  }
  for (size_t k = 0; k < 3; k++)
    fit.multipliers[k] = whiteCounts.y / whiteCounts[k];
  // The responses C and the targets X, and the normal matrices C C^T and
  // X C^T, a patch at a time.
  const auto &patches{trainingReflectances()};
  auto responses{std::vector<smdl::double3>(patches.size())};
  auto targets{std::vector<smdl::double3>(patches.size())};
  auto normal{smdl::double3x3()};
  auto crossed{smdl::double3x3()};
  for (size_t j = 0; j < patches.size(); j++) {
    const auto &patch{patches[j]};
    auto response{smdl::double3()};
    auto target{smdl::double3()};
    for (size_t i = 0; i < SENSOR_WAVELENGTH_COUNT; i++) {
      for (size_t k = 0; k < 3; k++) response[k] += patch[i] * counts[k][i];
      target += patch[i] * observer[i];
    }
    for (size_t k = 0; k < 3; k++) response[k] /= whiteCounts[k];
    target /= whiteY;
    for (size_t k = 0; k < 3; k++) {
      normal[k] += response * response[k];
      crossed[k] += target * response[k];
    }
    responses[j] = response;
    targets[j] = target;
  }
  auto normalInverse{normal};
  if (!smdl::tryInvert(normalInverse)) {
    fit.isSingular = true;
    return fit;
  }
  // The least-squares matrix, then the rank-one correction that moves
  // the white onto its target along the direction least squares cares
  // least about.
  const auto leastSquares{crossed * normalInverse};
  const auto ones{smdl::double3(1.0)};
  const auto direction{normalInverse * ones};
  const auto miss{fit.white - leastSquares * ones};
  const double weight{smdl::dot(ones, direction)};
  fit.cameraToXYZ = leastSquares;
  for (size_t k = 0; k < 3; k++)
    fit.cameraToXYZ[k] += miss * (direction[k] / weight);
  for (size_t j = 0; j < patches.size(); j++) {
    const auto truth{smdl::xyzToLab(targets[j], fit.white)};
    const auto fitted{
        smdl::xyzToLab(fit.cameraToXYZ * responses[j], fit.white)};
    const double difference{smdl::deltaE00(truth, fitted)};
    fit.meanDeltaE00 += difference;
    fit.maxDeltaE00 = std::max(fit.maxDeltaE00, difference);
    fit.meanDeltaEab += smdl::deltaEab(truth, fitted);
  }
  fit.meanDeltaE00 /= double(patches.size());
  fit.meanDeltaEab /= double(patches.size());
  return fit;
}
