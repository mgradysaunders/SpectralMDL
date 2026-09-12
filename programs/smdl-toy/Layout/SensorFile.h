/// \file
/// The sensor format: the body a picture lands on, as a fact shared by
/// every shot taken with it.
///
/// A `.sensor` file is the fourth format of the layout family and is
/// parsed by the same syntax core (`TextParser.h`). The split from
/// `.camera` is the separation the family is built on: a `.camera` says
/// where the picture is taken from and what the photographer turned, and
/// a `.sensor` says what it landed on: the pixel array and its pitch, the
/// spectral response of each band and the tile that lays them over the
/// pixels, the detector that turns electrons into digital numbers, and
/// how long a readout sweeps the frame. One body then serves as many
/// cameras as name it, and a lens swapped against it changes the field
/// exactly as a real one does.
///
/// The file is self-contained: no key in it takes a path, so a body is
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

  /// The curve's value at each knot, finite and nonnegative. The curve
  /// is zero outside its knots.
  std::vector<float> values{};

  /// The curve at `lambda` nanometers; see `curveAt()`.
  [[nodiscard]] double at(double lambda) const noexcept {
    return curveAt(wavelengths, values, lambda);
  }
};

/// The index into a tile `columns` wide and `rows` tall that frame pixel
/// `(x, y)` reads through, anchored at the frame's origin so that a crop
/// window changes nothing about which band a pixel sees.
[[nodiscard]] inline size_t tileIndexAt(size_t columns, size_t rows, size_t x,
                                        size_t y) noexcept {
  return (y % rows) * columns + x % columns;
}

/// The distinct bands the tile `cfa` lays down, in the order it first
/// names them, which is the order anything that speaks about them once
/// each walks them in. Empty without a tile.
[[nodiscard]] inline std::vector<size_t>
tileBands(const std::vector<size_t> &cfa) {
  auto bands{std::vector<size_t>()};
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

  [[nodiscard]] bool hasCFA() const noexcept { return cfaColumns > 0; }

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

/// The body a file's `sensor` directive describes.
class SensorSettings final {
public:
  /// `name`: the body's name, free text, or empty.
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

  /// The body the file's `sensor` directive described.
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
/// than the body alone can point a refusal at the key the file stated.
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
