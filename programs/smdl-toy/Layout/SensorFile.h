/// \file
/// The sensor format: what a picture lands on, as a fact shared by
/// every shot taken with it.
///
/// A `.sensor` file is the fourth format of the layout family and is
/// parsed by the same syntax core (`TextParser.h`). The split from
/// `.camera` is the separation the family is built on: a `.camera` says
/// where the picture is taken from and what the photographer turned, and
/// a `.sensor` says what it landed on: the pixel array and its pitch, the
/// spectral response of each band and the tile that lays them over the
/// pixels, the detector that turns electrons into digital numbers, and
/// how long a readout sweeps the frame. One sensor then serves as many
/// cameras as name it, and a lens swapped against it changes the field
/// exactly as a real one does.
///
/// The file is self-contained: no key in it takes a path, so a sensor is
/// one file and never two.
#pragma once

#include <algorithm>
#include <array>
#include <cmath>
#include <optional>
#include <string>
#include <string_view>
#include <type_traits>
#include <vector>

#include "Common.h"

#include "Layout/LayoutDiagnostics.h"

/// The extension that marks a sensor file.
constexpr std::string_view SENSOR_EXTENSION = ".sensor";

/// The peak quantum efficiency a `relative` response is scaled to when
/// its file states none: a plausible modern CMOS figure, and generic.
constexpr float DEFAULT_PEAK_QE = 0.5f;

/// The largest leak a band may state, where a pixel would keep half its
/// charge and the mixing of `tileMixing()` goes singular over a Bayer
/// tile. Far above anything a measured curve set carries: the limit that
/// binds in practice is `crosstalkFloor()`.
constexpr float CROSSTALK_MAX_LEAK = 0.125f;

/// How far a crosstalk-free curve may fall below zero, as a fraction of
/// the largest stated value, before the leak that implies it is refused.
/// See `crosstalkFloor()`.
constexpr double CROSSTALK_NEGATIVE_TOLERANCE = 0.002;

/// The direction a rolling readout sweeps the picture, named for where
/// the sweep travels to: `DOWN` reads the top line first.
enum class ReadoutDirection { DOWN, UP, LEFT, RIGHT };

/// What a response curve's values are.
enum class ResponseKind {
  /// A unitless weighting, the convention of every published camera
  /// curve. The curves are scaled so that the largest value across every
  /// band is `peak_qe`, which keeps the ratios the measurement carries.
  RELATIVE,

  /// Electrons per photon, an absolute measurement, taken as it is.
  QE
};

/// The curve through `knots` and `values` at `lambda`, linear between
/// the knots and zero outside them: the one rule a response curve is
/// read by, over the knots as a file states them and over the scaled
/// copies the render resolves them into, which is why it is written
/// once over whatever holds them.
///
/// The knots ascend and there is at least one of them, which is what a
/// parsed band guarantees.
template <typename Curve>
[[nodiscard]] inline double curveAt(const Curve &knots, const Curve &values,
                                    double lambda) noexcept {
  using Knot = std::decay_t<decltype(knots[0])>;
  SMDL_SANITY_CHECK(knots.size() == values.size() && knots.size() > 0);
  if (!(lambda >= knots.front() && lambda <= knots.back())) return 0.0;
  const auto itr{std::upper_bound(knots.begin(), knots.end(), Knot(lambda))};
  const size_t i{size_t(itr - knots.begin())};
  if (i == 0) return double(values.front());
  if (i == knots.size()) return double(values.back());
  const double w0{double(knots[i - 1])};
  const double w1{double(knots[i])};
  const double t{(lambda - w0) / (w1 - w0)};
  return double(values[i - 1]) +
         t * (double(values[i]) - double(values[i - 1]));
}

/// One response band: a named piecewise-linear curve over wavelength.
class ResponseBand final {
public:
  /// The name, an identifier, carried verbatim into the ENVI header's
  /// `band names`.
  std::string name{};

  /// The knots' wavelengths in nanometers, positive and strictly
  /// ascending, at least two of them.
  std::vector<float> wavelengths{};

  /// The curve's value at each knot, finite and nonnegative as a file
  /// states them, though a crosstalk-free curve derived from a set of
  /// them may dip slightly below zero; see `crosstalkFreeBands()`. The
  /// curve is zero outside its knots.
  std::vector<float> values{};

  /// The curve at `lambda` nanometers; see `curveAt()`.
  [[nodiscard]] double at(double lambda) const noexcept {
    return curveAt(wavelengths, values, lambda);
  }
};

/// The distinct bands the tile `cfa` lays down, in the order it first
/// names them, which is the order anything that speaks about them once
/// each walks them in. Empty without a tile.
[[nodiscard]] inline std::vector<size_t>
tileBands(const std::vector<size_t> &cfa) {
  std::vector<size_t> bands{};
  for (const auto index : cfa)
    if (std::find(bands.begin(), bands.end(), index) == bands.end())
      bands.push_back(index);
  return bands;
}

/// The `response` a sensor reads through: its named bands as curves over
/// wavelength, and the tile that lays them over the pixels when there is
/// one. As parsed; what the render makes of it against the wavelength
/// grid is decided after the frame, where the grid is.
class ResponseSettings final {
public:
  /// `kind`: what the values are, `relative` unless stated. One kind per
  /// response, so its bands share their scale.
  ResponseKind kind{ResponseKind::RELATIVE};

  /// `peak_qe`: with `relative`, the quantum efficiency the largest value
  /// across every band is scaled to, in (0, 1]; unset takes
  /// `DEFAULT_PEAK_QE`, which is generic and is logged as such.
  std::optional<float> peakQE{};

  /// `band`: the bands in file order, at least one.
  std::vector<ResponseBand> bands{};

  /// `cfa`: the tile's width in pixels, or 0 without a tile.
  size_t cfaColumns{};

  /// `cfa`: the tile row by row, each entry an index into `bands`, or
  /// empty without a tile. Pixel `(x, y)` of the frame reads through
  /// `cfa[(y % rows) * cfaColumns + x % cfaColumns]`.
  std::vector<size_t> cfa{};

  /// `rgb`: the three bands a develop maps to R, G, and B, as indices
  /// into `bands`, when the file stated them. See `rgbBands()`.
  std::optional<std::array<size_t, 3>> rgb{};

  /// `crosstalk`: the fraction of a pixel's charge each one of its four
  /// neighbors collects, one value per band in band order, so the total
  /// a pixel loses is four times it. Empty is no cross-talk, which is
  /// the default and what every shipped sensor states; a scalar in the
  /// file fills every band alike.
  ///
  /// The leak is the source pixel's rather than the destination's: the
  /// charge is generated under the source's filter and travels as far as
  /// the depth that filter's wavelengths convert at, which is why red
  /// leaks several times as far as blue. Each value is in
  /// `[0, CROSSTALK_MAX_LEAK)`, and the set as a whole must be one the
  /// curves can carry; see `crosstalkFloor()`.
  std::vector<float> crosstalk{};

  [[nodiscard]] bool hasCFA() const noexcept { return cfaColumns > 0; }

  /// Does any band leak?
  [[nodiscard]] bool hasCrosstalk() const noexcept {
    return std::any_of(crosstalk.begin(), crosstalk.end(),
                       [](float leak) { return leak > 0; });
  }

  [[nodiscard]] size_t cfaRows() const noexcept {
    return cfaColumns > 0 ? cfa.size() / cfaColumns : 0;
  }

  /// The band frame pixel `(x, y)` reads through under the tile; see
  /// `tileIndexAt()`. Without a tile, 0.
  [[nodiscard]] size_t bandAt(size_t x, size_t y) const noexcept {
    return hasCFA() ? cfa[tileIndexAt(cfaColumns, cfaRows(), x, y)] : 0;
  }

  /// The index of the band named `name`, or nothing.
  [[nodiscard]] std::optional<size_t>
  bandIndex(std::string_view name) const noexcept;

  /// The bands a develop maps to R, G, and B: `rgb` as stated, else the
  /// bands named `R`, `G`, and `B`, else the first three in file order,
  /// else nothing.
  [[nodiscard]] std::optional<std::array<size_t, 3>> rgbBands() const noexcept;

  /// What turns a curve's values into electrons per photon: 1 for `qe`,
  /// and for `relative` the peak quantum efficiency over the largest
  /// value across every band, so the ratios the measurement carries
  /// hold. Zero for a response whose every value is zero.
  [[nodiscard]] double qeScale() const noexcept;
};

/// The flat-field band mixing the tile and the leaks imply: what a
/// uniformly illuminated array reads through each band, as a row-major
/// square matrix over the crosstalk-free responsivities,
///
///     M[b][b'] = (1 - 4 leak[b]) delta[b][b'] + neighbors[b][b'] leak[b']
///
/// with `neighbors[b][b']` the mean count of band `b'` pixels among the
/// four neighbors of a band `b` pixel, over the tile with its own
/// periodic wrap. Diagonals are left out because the measurement finds
/// them negligible at the pitch where cross-talk matters at all.
///
/// A published camera curve is already this applied to the curves the
/// silicon would have without transport, because the measurement
/// illuminates the whole array at once and a uniform field cannot show
/// anything but the mixing. So this is what a response undoes before the
/// readout gathers over the same kernel, and undoing it is what makes the
/// two compose back to the stated curve on any flat field.
///
/// Charge conservation is the invariant it always has: the tile's band
/// multiplicities carried through it, `sum_b mult[b] M[b][b'] = mult[b']`.
/// The rows sum to one only when every band leaks alike. Under a per-band
/// leak a band that leaks less than its neighbors keeps more than it
/// started with, which is the whole point of stating one, so the rows are
/// deliberately not normalized. The identity without a tile and without a
/// leak.
[[nodiscard]] std::vector<double> tileMixing(const ResponseSettings &settings);

/// The inverse of `tileMixing()`, row-major, or empty if the mixing is
/// singular. A leak under `CROSSTALK_MAX_LEAK` does not reach the
/// singularity over a Bayer tile, but an arbitrary tile might.
[[nodiscard]] std::vector<double>
tileMixingInverse(const ResponseSettings &settings);

/// The most negative value any crosstalk-free curve takes under the
/// response's own leaks, as a fraction of the largest stated value: zero
/// or above is a curve set the leaks fit, and below
/// `-CROSSTALK_NEGATIVE_TOLERANCE` is one they do not.
///
/// Pushing the leak past what a curve set carries makes the de-mixed
/// curves dip below zero, which says the band overlap the measurement
/// shows is the color filter's own transmission rather than charge
/// crossing between pixels, and no amount of transport can explain it.
/// That is the one bound on a leak that comes from data rather than from
/// a guess, since no per-pitch, per-wavelength cross-talk table exists in
/// public.
///
/// Exact: a de-mixed curve is a linear combination of piecewise-linear
/// curves, so it is piecewise linear on the union of their knots and
/// takes its minimum at one of them. Zero for a response with no leak,
/// with no tile, or whose mixing is singular.
[[nodiscard]] double crosstalkFloor(const ResponseSettings &settings);

/// The crosstalk-free curves the stated ones imply: `tileMixingInverse()`
/// applied across the bands, each on the union of every band's knots,
/// which is where a combination of piecewise-linear curves is itself
/// piecewise linear and so is carried exactly by its values.
///
/// This is what a response projects samples onto, since a stated curve
/// is what a uniformly illuminated array reads and already carries the
/// mixing the readout's gather puts back. Everything else a sensor
/// derives stays on the stated curves, because the well, the speed and
/// the color fit are all flat-field facts; only spatial detail moves.
///
/// The stated bands verbatim without a leak, without a tile, or under a
/// singular mixing, so a response with no cross-talk takes the path it
/// always took. A curve may dip below zero, by no more than the parse
/// allowed it to; see `crosstalkFloor()`.
[[nodiscard]] std::vector<ResponseBand>
crosstalkFreeBands(const ResponseSettings &settings);

/// The `detector` a sensor reads out with: what turns the electrons the
/// response counts into the digital numbers the instrument writes. Every
/// field has a generic default from a modern CMOS sensor, so an empty
/// block, or none at all, is a usable detector and a block names what
/// differs; a vendor's EMVA 1288 data sheet carries the whole vector in
/// exactly these units.
class DetectorSettings final {
public:
  /// `base_iso`: the rated base ISO, positive. One fact with `full_well`:
  /// the base ISO is the lowest at which the ADC clips before the well
  /// does, so either may be stated and the other follows, and stating
  /// both is refused at parse.
  std::optional<float> baseISO{};

  /// `full_well`: the electrons a pixel holds before it clips, positive.
  /// Unset, and with no `base_iso`, is 1000 electrons per square
  /// micrometer of pixel, which the readout derives from the pitch, so
  /// that a phone pixel and a full-frame one both get a plausible well.
  std::optional<float> fullWell{};

  /// `read_noise`: the read noise in electrons rms, nonnegative.
  float readNoise{1.5f};

  /// `dark_current`: the dark current in electrons per second at
  /// `reference_temperature`, nonnegative.
  float darkCurrent{0.1f};

  /// `reference_temperature`: the degrees Celsius `dark_current` is
  /// stated at.
  float referenceTemperature{25.0f};

  /// `doubling_temperature`: the degrees Celsius per doubling of the dark
  /// current, positive. A field rather than a constant: the textbook 7
  /// and the 12.7 measured on current back-illuminated parts differ by
  /// a factor of four in dark current extrapolated to room temperature.
  float doublingTemperature{7.0f};

  /// `black_level`: the digital number a pixel with no electrons reads,
  /// so that the noise around zero is not clipped away, nonnegative and
  /// below the top code. Unset is `2^(bits - 5)`, which the parser
  /// resolves once `bits` is known; the value here is that for 12 bits.
  float blackLevel{128.0f};

  /// `bits`: the ADC's depth, 1 to 16, so the top code is `2^bits - 1`.
  int bits{12};

  /// `gain`: digital numbers per electron, positive. Unset fills the
  /// well to the top code: `(2^bits - 1 - black_level) / full_well`.
  std::optional<float> gain{};

  /// `max_iso`: the highest ISO the instrument offers, at least the base.
  float maxISO{102400.0f};

  /// The ADC's top code, `2^bits - 1`.
  [[nodiscard]] uint32_t topCode() const noexcept {
    return (uint32_t(1) << bits) - 1;
  }

  /// The digital numbers between the black level and the top code, which
  /// is what the well fills at the base ISO and what the speed of a
  /// stated gain is read against.
  [[nodiscard]] double codeRange() const noexcept {
    return double(topCode()) - double(blackLevel);
  }
};

/// The sensor a file's `sensor` directive describes.
class SensorSettings final {
public:
  /// `name`: the sensor's name, free text, or empty.
  std::string name{};

  /// `pixels`: the columns and rows of the array, positive.
  int2 pixels{};

  /// `pitch`: the pixel pitch across and down in micrometers, positive.
  /// `size` states the frame in millimeters instead, and the pitch
  /// follows from it over `pixels`.
  float2 pitchUM{};

  /// `response { ... }`: the bands, see `ResponseSettings`. Required.
  ResponseSettings response{};

  /// `detector { ... }`: the readout chain, see `DetectorSettings`. The
  /// generic detector when the file has no block.
  DetectorSettings detector{};

  /// Was a `detector` block written, or is `detector` the generic one?
  bool hasDetectorBlock{};

  /// `readout`: the seconds the sensor takes to read the frame out,
  /// nonnegative; 0 is a global shutter. The camera's `readout`
  /// overrides it.
  float readout{};

  /// `readout_direction`: the way the readout sweeps the picture, `down`
  /// unless stated. The camera's overrides it.
  ReadoutDirection readoutDirection{ReadoutDirection::DOWN};

  /// The frame in millimeters, the pitch over the pixels.
  [[nodiscard]] float2 sizeMM() const noexcept {
    return 1e-3f *
           float2(pitchUM.x * float(pixels.x), pitchUM.y * float(pixels.y));
  }

  /// The pixel's area in square meters.
  [[nodiscard]] double pixelArea() const noexcept {
    return double(pitchUM.x) * 1e-6 * double(pitchUM.y) * 1e-6;
  }
};

/// A parsed sensor file, as written.
class SensorDocument final {
public:
  /// The source the document was parsed from, owned by the
  /// `LayoutDiagnostics` that loaded it.
  const LayoutSource *source{};

  /// The sensor the file's `sensor` directive described.
  SensorSettings sensor{};
  LayoutLocation sensorLoc{};
};

/// Parse one sensor source into a document.
///
/// Pure: no filesystem access. Errors and warnings accumulate in
/// `diags`, and the returned document is the best effort regardless. A
/// file with no `sensor` block is an error, since a camera named it for
/// one.
[[nodiscard]] SensorDocument parseSensor(LayoutDiagnostics &diags,
                                         const LayoutSource &source);

/// Read a sensor file: parse, print every diagnostic to standard error
/// (colored when stderr is a terminal), and throw if any were errors.
///
/// The document's locations point into `diags`, which the caller owns so
/// that they outlive the read: a caller that keeps the document rather
/// than the sensor alone can point a refusal at the key the file stated.
///
/// \throws smdl::Error  If the file cannot be read, or on any parse
///                      error after printing the diagnostics.
///
[[nodiscard]] SensorDocument readSensor(LayoutDiagnostics &diags,
                                        const std::string &fileName);

/// The sensor file a camera file names, or empty for none: `stated` as
/// the camera file wrote it, resolved relative to `cameraFileName` so
/// that a scene directory stays self-contained. `human`, the observer, is
/// no file and is the caller's to recognize before asking.
///
/// \throws smdl::Error  If it names a file that does not exist, which
///                      may not be quietly ignored; or a `.response`
///                      file, whose meaning moved into this format.
///
[[nodiscard]] std::string
resolveSensorFileName(const std::string &stated,
                      const std::string &cameraFileName);
