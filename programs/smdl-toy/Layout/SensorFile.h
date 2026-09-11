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

#include <array>
#include <cmath>
#include <optional>
#include <string>
#include <string_view>
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

  /// The curve at `lambda` nanometers, linear between the knots and zero
  /// outside them.
  [[nodiscard]] double at(double lambda) const noexcept;
};

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
  /// nonnegative; 0 is a global shutter. The camera's `readout` and
  /// `-readout` override it.
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
/// \throws smdl::Error  If the file cannot be read, or on any parse
///                      error after printing the diagnostics.
///
[[nodiscard]] SensorDocument readSensor(const std::string &fileName);

/// The sensor file a camera should land its picture on, or empty for
/// none: `given` if the command line named one, else `stated` if the
/// camera file did, resolved relative to `cameraFileName` so that a scene
/// directory stays self-contained. `human`, the observer, is no file and
/// is the caller's to recognize before asking.
///
/// \throws smdl::Error  If either names a file that does not exist,
///                      since neither may be quietly ignored; or names a
///                      `.response` file, whose meaning moved into this
///                      format.
///
[[nodiscard]] std::string
resolveSensorFileName(const std::string &given,
                      const std::string &cameraFileName,
                      const std::string &stated);
