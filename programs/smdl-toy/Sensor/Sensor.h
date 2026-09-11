/// \file
/// The physical sensor as physics: what its curves, its pixel, and its
/// detector imply before any picture is taken, and the meter that reads
/// a taken one. Independent of the render grid: the integrals here run
/// at 1 nm over the range the CIE tables span, so a body's numbers are
/// the body's alone.
#pragma once

#include <array>
#include <vector>

#include "smdl/RenderUtil/SpectralFilm.h"

#include "Color.h"
#include "Layout/CameraFile.h"
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

/// The correlated color temperatures of D55, the illuminant ISO 12232
/// rates a sensor's speed under and the `daylight` white balance; of
/// D65, the white of sRGB; of D75, the `shade` white balance; and of CIE
/// illuminant A, the `tungsten` one.
///
/// \{
constexpr double D55_KELVIN{5503.0};
constexpr double D65_KELVIN{6504.0};
constexpr double D75_KELVIN{7504.0};
constexpr double ILLUMINANT_A_KELVIN{2856.0};
/// \}

/// The Planckian radiator of temperature `kelvin` on the grid, relative,
/// 1 at 560 nm, with the second radiation constant of CODATA 2018, so
/// that at `ILLUMINANT_A_KELVIN` it is CIE illuminant A.
[[nodiscard]] SensorSpectrum planckSpectrum(double kelvin);

/// The illuminant of correlated color temperature `kelvin`: CIE daylight
/// from 4000 K up, and below it, where the daylight locus stops, the
/// Planckian radiator.
[[nodiscard]] SensorSpectrum kelvinSpectrum(double kelvin);

/// The illuminant a white balance names: D65 for `D65` and `cloudy`, D55
/// for `daylight`, D75 for `shade`, illuminant A for `tungsten`, CIE
/// fluorescent F2 for `fluorescent`, and `kelvinSpectrum()` of a stated
/// temperature. `auto` names none until the frame is measured, and
/// reads as D65.
[[nodiscard]] SensorSpectrum
whiteBalanceSpectrum(const WhiteBalance &whiteBalance);

/// The white of `illuminant` through the builtin's observer,
/// `builtinWymanXYZ()`, scaled so that Y is 1.
[[nodiscard]] smdl::double3 illuminantWhite(const SensorSpectrum &illuminant);

/// The reflectances a color fit trains on: the 190 patches of
/// rawtoaces-data on the grid, linear between the table's 5 nm steps and
/// held at its end values past 380 and 780 nm, where a surface does not
/// stop reflecting.
[[nodiscard]] const std::vector<SensorSpectrum> &trainingReflectances();

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

/// The mean CIEDE2000 over the training reflectances past which a fit
/// no longer sees color as the observer does, and a develop maps the
/// bands to R, G, and B as they are: several times any camera's, which
/// sit between 1 and 2, and far below what a sensor blind to part of
/// the visible makes of the set.
constexpr double FAITHFUL_FIT_DELTA_E00{10.0};

/// How three of a sensor's bands see color against the observer under
/// one illuminant. See `Sensor::fitColor()`.
struct ColorFit final {
  /// The bands mapped to R, G, and B, as indices into the response's.
  std::array<size_t, 3> bands{};

  /// The white balance: what each band is multiplied by so that the
  /// illuminant's white reads the same in all three as in the second,
  /// green, `n_G(S) / n_b(S)`.
  smdl::double3 multipliers{};

  /// The matrix from white-balanced camera RGB, in units where the
  /// illuminant's white reads (1, 1, 1), to XYZ under the illuminant,
  /// taking (1, 1, 1) to `white` exactly.
  smdl::double3x3 cameraToXYZ{};

  /// The illuminant's white through the builtin's observer, Y = 1.
  smdl::double3 white{};

  /// The fit's error over the training reflectances in CIELAB about
  /// `white`: the mean and the largest CIEDE2000, and the mean CIE 1976
  /// difference.
  ///
  /// \{
  double meanDeltaE00{};
  double maxDeltaE00{};
  double meanDeltaEab{};
  /// \}

  /// Did the bands respond too much alike to the training reflectances,
  /// or not at all to the illuminant, to tell three colors apart? Then
  /// there is no matrix, and the errors are meaningless.
  bool isSingular{};

  /// The sensor metamerism index over the training reflectances, `100 -
  /// 5.5 mean dE*ab`: ISO 17321's formula, which the standard applies to
  /// the 18 chromatic ColorChecker patches. Over this set it is a
  /// training error, optimistic, and not comparable to a published one.
  [[nodiscard]] double index() const noexcept {
    return 100.0 - 5.5 * meanDeltaEab;
  }

  /// Does the fit see color the way the observer does? See
  /// `FAITHFUL_FIT_DELTA_E00`.
  [[nodiscard]] bool isFaithful() const noexcept {
    return !isSingular && meanDeltaE00 <= FAITHFUL_FIT_DELTA_E00;
  }
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

  /// Fit the color matrix of `bands` under `illuminant`: white-preserving
  /// least squares (Finlayson and Drew 1997) from the bands' responses to
  /// the training reflectances, white-balanced, to the observer's XYZ of
  /// the same reflectances, `M = M_LS + (x_w - M_LS c_w) (c_w^T A^-1
  /// c_w)^-1 c_w^T A^-1` with `A = C C^T` and `c_w = (1, 1, 1)`: the
  /// white lands exactly, and the rest as closely as three by three
  /// numbers can put it. The render grid has no part in it, so the fit
  /// is the body's alone.
  [[nodiscard]] ColorFit fitColor(const std::array<size_t, 3> &bands,
                                  const SensorSpectrum &illuminant) const;

private:
  SensorSettings mSettings{};

  size_t mPeakBand{};

  double mPeakElectronsPerLuxSecond{};

  double mFullWell{};

  WellSource mWellSource{};

  double mBaseISO{};

  double mMaxISO{};
};
