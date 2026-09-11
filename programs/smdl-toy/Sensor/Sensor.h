/// \file
/// The physical sensor as physics: what its curves, its pixel, and its
/// detector imply before any picture is taken, and the meter that reads
/// a taken one. Independent of the render grid: the integrals here run
/// at 1 nm over the range the CIE tables span, so a body's numbers are
/// the body's alone.
#pragma once

#include <vector>

#include "smdl/RenderUtil/SpectralFilm.h"

#include "Color.h"
#include "Layout/SensorFile.h"

/// The wavelengths every integral of the sensor's physics runs over: 1
/// nm steps from 300 to 830 nm, the range the CIE daylight table spans,
/// past both ends of any camera's curve and of the observer.
///
/// \{
constexpr double SENSOR_WAVELENGTH_MIN{300.0};
constexpr double SENSOR_WAVELENGTH_MAX{830.0};
constexpr size_t SENSOR_WAVELENGTH_COUNT{531};
/// \}

/// The wavelength of sample `i` of that grid.
[[nodiscard]] constexpr double sensorWavelength(size_t i) noexcept {
  return SENSOR_WAVELENGTH_MIN + double(i);
}

/// A spectral distribution sampled on that grid, `SENSOR_WAVELENGTH_COUNT`
/// values: an illuminant's relative power per nanometer, which every
/// integral is taken per unit of.
using SensorSpectrum = std::vector<double>;

/// The CIE daylight illuminant of correlated color temperature `kelvin`
/// on the grid, relative, 1 at 560 nm: the daylight locus of CIE 15
/// through `smdlKelvinToChromaticity()`, which holds the temperature
/// within 4000 to 25000 K, and its three components through
/// `smdlEvalIlluminantD()`.
[[nodiscard]] SensorSpectrum daylightSpectrum(double kelvin);

/// The correlated color temperature of D55, the illuminant ISO 12232
/// rates a sensor's speed under, and of D65.
///
/// \{
constexpr double D55_KELVIN{5503.0};
constexpr double D65_KELVIN{6504.0};
/// \}

/// Lumens per watt at the peak of the observer, which turns an integral
/// against `V` into lux.
constexpr double LUMENS_PER_WATT{683.0};

/// The saturation exposure at ISO `S` is `78 / S` lux-seconds: ISO
/// 12232's saturation-based speed, the focal-plane exposure of D55 a
/// sensor at that speed clips at. Every ISO figure here is this line
/// read one way or the other.
constexpr double ISO_SATURATION_LUX_SECONDS{78.0};

/// The meter: the calibration constant `K` of a reflected-light meter,
/// and the fraction `q` of the metered luminance that reaches the focal
/// plane through a lens, so that the exposure a meter sets lands the
/// frame's mean at `q K / S` lux-seconds, which is `q K / 78`, 10.4%, of
/// saturation. The Frostbite and Filament calibration.
///
/// \{
constexpr double METER_K{12.5};
constexpr double METER_Q{0.65};
/// \}

/// The generic well, electrons per square micrometer of pixel, for a
/// body that states neither its well nor its base ISO.
constexpr double GENERIC_ELECTRONS_PER_SQUARE_MICROMETER{1000.0};

/// Where a sensor's well came from.
enum class WellSource {
  /// `full_well` as written.
  STATED,

  /// Read backward from `base_iso`: the exposure the well fills at is
  /// the saturation exposure at the base ISO.
  FROM_BASE_ISO,

  /// The generic well over the pixel's area.
  FROM_PITCH
};

/// What the meter read of a film.
struct MeteredExposure final {
  /// The frame's mean photometric exposure over the window in
  /// lux-seconds: the shutter times the illuminance the irradiance
  /// film holds, weighed by the observer.
  double luxSeconds{};

  /// The ISO the meter asks for, `q K` over that exposure, before the
  /// instrument's range: infinite for a dark frame.
  double wantedISO{};

  /// The ISO chosen: the wanted one, held within the base and the top.
  double iso{};

  /// How far the frame sits from where the meter wanted it at the ISO
  /// chosen, in stops: positive is overexposed, the meter having wanted
  /// less than the base; negative underexposed, more than the top; 0
  /// within the range.
  double stopsOff{};

  /// Is the frame outside the instrument's range?
  [[nodiscard]] bool isOverexposed() const noexcept { return stopsOff > 0; }
  [[nodiscard]] bool isUnderexposed() const noexcept { return stopsOff < 0; }
};

/// The sensor: its settings resolved into the numbers the readout and
/// the meter need.
///
/// The chain is ISO 12232's. Band `b` counts `n_b(S) = integral(S QE_b
/// lambda / (h c))` electrons per square meter and second under an
/// illuminant `S`, whose illuminance is `683 integral(S V)` lux, so the
/// pixel counts `A n_b / (683 integral(S V))` electrons per lux-second.
/// Under D55 the most sensitive band is the first to saturate: the well
/// fills at `H_sat = well / max_b(A eta_b)` lux-seconds, and the base ISO
/// is `78 / H_sat`. A stated base ISO gives the well by the same line
/// read backward. At an ISO `S` the ADC clips at `78 / S` lux-seconds,
/// so the gain is `(top - black) S / (78 max_b(A eta_b))` digital numbers
/// per electron: at the base it fills the well to the top code, and above
/// the base the ADC clips before the well does.
class Sensor final {
public:
  /// Resolve `settings`: integrate the bands under D55 for the well, the
  /// base ISO, and the gain. The settings were validated at parse, so
  /// nothing here throws.
  explicit Sensor(const SensorSettings &settings);

  [[nodiscard]] const SensorSettings &settings() const noexcept {
    return mSettings;
  }

  /// The pixel's area in square meters.
  [[nodiscard]] double pixelArea() const noexcept {
    return mSettings.pixelArea();
  }

  /// The electrons band `band` counts per square meter and second under
  /// `illuminant` at unit scale: `integral(S QE_b lambda / (h c))`, per
  /// W/(m^2 nm) of `S`.
  [[nodiscard]] double electronRate(size_t band,
                                    const SensorSpectrum &illuminant) const;

  /// The illuminance of `illuminant` at unit scale, `683 integral(S V)`,
  /// in lux per W/(m^2 nm) of `S`, under the observer of `wymanY()`.
  [[nodiscard]] static double illuminance(const SensorSpectrum &illuminant);

  /// The electrons the pixel's band `band` counts per lux-second of
  /// `illuminant`: the pixel area times the rate over the illuminance,
  /// which is where the illuminant's scale cancels.
  [[nodiscard]] double
  electronsPerLuxSecond(size_t band, const SensorSpectrum &illuminant) const;

  /// The most sensitive band under D55, and its electrons per
  /// lux-second, which every ISO figure here is taken over.
  ///
  /// \{
  [[nodiscard]] size_t peakBand() const noexcept { return mPeakBand; }
  [[nodiscard]] double peakElectronsPerLuxSecond() const noexcept {
    return mPeakElectronsPerLuxSecond;
  }
  /// \}

  /// The well in electrons, and where it came from.
  ///
  /// \{
  [[nodiscard]] double fullWell() const noexcept { return mFullWell; }
  [[nodiscard]] WellSource wellSource() const noexcept { return mWellSource; }
  /// \}

  /// The base ISO: stated, else what the well implies.
  ///
  /// \{
  [[nodiscard]] double baseISO() const noexcept { return mBaseISO; }
  [[nodiscard]] bool isBaseISOStated() const noexcept {
    return mSettings.detector.baseISO.has_value();
  }
  /// \}

  /// The top of the instrument's range, `max_iso`, never below the base.
  [[nodiscard]] double maxISO() const noexcept { return mMaxISO; }

  /// The gain in digital numbers per electron at ISO `iso`: the stated
  /// gain whatever the ISO, else the one that puts the top code at the
  /// saturation exposure `78 / iso`.
  [[nodiscard]] double gain(double iso) const noexcept;

  /// Is the gain stated, so that the instrument is a fixed-gain one
  /// whose speed is `fixedGainISO()` and nothing chooses an ISO?
  [[nodiscard]] bool hasFixedGain() const noexcept {
    return mSettings.detector.gain.has_value();
  }

  /// The saturation speed of the stated gain: the ISO whose derived gain
  /// it is. Meaningless without one.
  [[nodiscard]] double fixedGainISO() const noexcept;

  /// The electrons the ADC's top code stands for at ISO `iso`, which is
  /// the well at the base and less above it.
  [[nodiscard]] double topCodeElectrons(double iso) const noexcept;

  /// The observer's weights on the render grid `wavelengths`: `V_i w_i`
  /// such that `683 sum_i(E_i V_i w_i)` is the illuminance of a spectral
  /// irradiance sampled on it, under the grid's own rule: the trapezoid
  /// over a grid held still, and under the jitter each band's rectangle
  /// of `gRenderGrid.bandEdges`, with `V` averaged over the rectangle,
  /// since the film's band holds the irradiance averaged over it.
  [[nodiscard]] static std::vector<double>
  luminanceWeights(const Color &wavelengths);

  /// Meter `film`, the spectral irradiance at the sensor, over `window`
  /// at an exposure of `seconds`: the ISO a reflected-light meter would
  /// have set, held within the instrument's range. See `MeteredExposure`.
  [[nodiscard]] MeteredExposure meter(const smdl::SpectralFilm &film,
                                      const Color &wavelengths, int4 window,
                                      double seconds) const;

private:
  SensorSettings mSettings{};

  size_t mPeakBand{};

  double mPeakElectronsPerLuxSecond{};

  double mFullWell{};

  WellSource mWellSource{};

  double mBaseISO{};

  double mMaxISO{};
};
