#include <algorithm>
#include <cmath>

#include "smdl/RenderUtil/Illuminant.h"
#include "smdl/Support/Parallel.h"

#include "Sensor/Colorimetry.h"
#include "Sensor/Response.h"
#include "Sensor/Sensor.h"

namespace {

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
    total += wymanY(lo + (hi - lo) * (double(k) + 0.5) / double(NUM_STEPS));
  return total / double(NUM_STEPS);
}

} // namespace

SensorSpectrum daylightSpectrum(double kelvin) {
  auto wavelengths{std::vector<float>(SENSOR_WAVELENGTH_COUNT)};
  auto values{std::vector<float>(SENSOR_WAVELENGTH_COUNT)};
  for (size_t i = 0; i < SENSOR_WAVELENGTH_COUNT; i++)
    wavelengths[i] = float(sensorWavelength(i));
  auto xy{float2()};
  smdlKelvinToChromaticity(float(kelvin), &xy);
  smdlEvalIlluminantD(int(SENSOR_WAVELENGTH_COUNT), wavelengths.data(),
                      values.data(), xy);
  return {values.begin(), values.end()};
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
    total += illuminant[i] * wymanY(sensorWavelength(i));
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
      weights[i] = wymanY(double(wavelengths[i])) * widths[i];
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
