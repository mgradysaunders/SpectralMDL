/// \file
/// The camera format: where the picture is taken from, with what lens,
/// and when.
///
/// A `.camera` file is the second format of the layout family and is
/// parsed by the same syntax core (`TextParser.h`). The split is a
/// separation of concerns: a `.layout` says what is in the world and how
/// it moves, on an absolute clock, and a `.camera` says what it is
/// photographed with. One scene then takes as many viewpoints as there
/// are files, and neither file has to restate the other.
///
/// What the file deliberately does not carry: which instant to
/// photograph (`-time`, so one camera renders every frame of a shot) and
/// how big the picture is (`-resolution` and `-crop-window`, which are
/// facts about this render rather than about the camera).
///
/// The detector's response is the one thing the file may hold in either
/// of two places: a `response { ... }` block inline, or a `.response`
/// file holding that block alone, named by `response "path"` and
/// resolved relative to the camera. The sidecar is for curves that run
/// to hundreds of points and for a sensor several cameras share; it is
/// parsed by this same vocabulary with `response` as its one top-level
/// directive, so the two forms cannot drift apart.
#pragma once

#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "Common.h"

#include "Layout/LayoutDiagnostics.h"

/// The extension that marks a camera file, which is how the render finds
/// the one beside a layout.
constexpr std::string_view CAMERA_EXTENSION = ".camera";

/// The extension that marks a response file: a `response` block on its
/// own, which a camera names in place of writing the block inline.
constexpr std::string_view RESPONSE_EXTENSION = ".response";

/// The direction a rolling readout sweeps the picture, named for where
/// the sweep travels to: `DOWN` reads the top line first.
enum class ReadoutDirection { DOWN, UP, LEFT, RIGHT };

/// What a response curve's values are, which decides what its band
/// integrates to.
enum class ResponseKind {
  /// A unitless weighting, the convention of every published camera
  /// curve. The band is the normalized energy integral: the radiance
  /// averaged over the curve, in the film's own units.
  RELATIVE,

  /// Electrons per photon. The band is the photon integral weighted by
  /// the curve, in electrons per square meter, steradian, and second,
  /// which is what a detector readout counts from.
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
};

/// The `response` a camera reads through: the detector's named bands as
/// curves over wavelength, and the tile that lays them over the pixels
/// when there is one. As parsed; what the render makes of it against
/// the wavelength grid is decided after the frame, where the grid is.
class ResponseSettings final {
public:
  /// `name`: the sensor's name, free text, or empty.
  std::string name{};

  /// `kind`: what the values are, `relative` unless stated. One kind per
  /// response, so its bands share their units.
  ResponseKind kind{ResponseKind::RELATIVE};

  /// `band`: the bands in file order, at least one.
  std::vector<ResponseBand> bands{};

  /// `cfa`: the tile's width in pixels, or 0 without a tile.
  size_t cfaColumns{};

  /// `cfa`: the tile row by row, each entry an index into `bands`, or
  /// empty without a tile. Pixel `(x, y)` of the frame reads through
  /// `cfa[(y % rows) * cfaColumns + x % cfaColumns]`.
  std::vector<size_t> cfa{};

  [[nodiscard]] bool hasCFA() const noexcept { return cfaColumns > 0; }

  [[nodiscard]] size_t cfaRows() const noexcept {
    return cfaColumns > 0 ? cfa.size() / cfaColumns : 0;
  }

  /// The index of the band named `name`, or nothing.
  [[nodiscard]] std::optional<size_t>
  bandIndex(std::string_view name) const noexcept;
};

/// The `detector` a camera reads out with: what turns the electrons a
/// `qe` response counts into the digital numbers a stated instrument
/// writes. Every field has a generic default from a modern CMOS sensor,
/// so an empty block, or none at all, is a usable detector and a block
/// names what differs; a vendor's EMVA 1288 data sheet carries the whole
/// vector in exactly these units.
class DetectorSettings final {
public:
  /// `full_well`: the electrons a pixel holds before it clips, positive.
  /// Unset is 1000 electrons per square micrometer of pixel, which the
  /// readout derives from the pitch, so that a phone pixel and a
  /// full-frame one both get a plausible well.
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

  /// `temperature`: the sensor's degrees Celsius at the exposure.
  float temperature{25.0f};

  /// `black_level`: the electrons added before the ADC so that the noise
  /// around zero is not clipped away, nonnegative.
  float blackLevel{};

  /// `bits`: the ADC's depth, 1 to 16, so the top code is `2^bits - 1`.
  int bits{12};

  /// `gain`: digital numbers per electron, positive. Unset fills the
  /// well to the top code: `(2^bits - 1) / (full_well + black_level)`.
  std::optional<float> gain{};
};

/// The camera settings a `motion` key may restate, which is every one
/// that is a quantity to interpolate over the life of a shot.
///
/// The ones left out are left out because they are not: `blades` counts
/// the aperture's edges, `distortion_fit` is a bare flag, `lens`,
/// `sensor`, `response`, and `detector` are the instrument, and
/// `shutter`, `readout`, and `readout_direction` describe the interval a
/// key is sampled over rather than something sampled within it.
///
class CameraKeyable {
public:
  std::optional<float3> lookFrom{};
  std::optional<float3> lookTo{};
  std::optional<float3> lookUp{};
  std::optional<float> fovYDeg{};
  std::optional<float> fStop{};
  std::optional<float> aperture{};
  std::optional<float> focus{};
  std::optional<float> bladeAngleDeg{};
  std::optional<float> distortionK1{};
  std::optional<float> distortionK2{};
  std::optional<float> vignetting{};
  std::optional<float> catEye{};
  std::optional<float> catEyeRadius{};
};

/// One key of the camera's motion: the absolute time in seconds and
/// whatever settings the key restates.
///
/// A setting no key states holds the value the enclosing block states,
/// and a setting some keys state is interpolated between the two that
/// surround the time and clamped outside them, so a block that keys
/// only `look_to` is a pan and one that keys only `focus` is a focus
/// pull.
///
class CameraKey final : public CameraKeyable {
public:
  float time{};
};

/// The camera a file's `camera` directive describes.
///
/// Everything is optional and unset by default. The built-in defaults
/// are the base, the file overrides those, and explicit command-line
/// flags override the file.
///
class CameraSettings final : public CameraKeyable {
public:
  std::optional<int> blades{};
  std::optional<bool> shouldFitDistortion{};

  /// `lens`: the '.lens' file the camera looks through, as written, to be
  /// resolved relative to the camera file that names it. Not keyable: it
  /// is the lens, not a quantity to interpolate.
  ///
  /// With one, the frame is the sensor and the prescription together, so
  /// `fovy` and every setting that stands in for what a real lens does on
  /// its own are refused rather than ignored.
  std::optional<std::string> lens{};

  /// `sensor`: the sensor width and height in millimeters, full frame
  /// when unset. With a lens it decides the field of view; with the thin
  /// lens it sizes the frame `fovy` spans, which sets the pixel pitch a
  /// readout counts over and the focal length `fstop` is a fraction of.
  std::optional<float2> sensorMM{};

  /// `shutter`: the seconds from shutter open to shutter shut,
  /// nonnegative, which `-shutter` overrides. Zero or unset is a shut
  /// shutter, and every path then renders the one instant `-time`
  /// names, whatever motion the scene carries.
  ///
  /// This is the one exposure quantity the file carries, because it is
  /// a fact about the camera: how long it stays open. When it opens is
  /// not, which is why `-time` alone says that.
  std::optional<float> shutter{};

  /// `readout`: the seconds the sensor takes to read the frame out,
  /// nonnegative, which `-readout` overrides. Zero or unset is a global
  /// shutter, where every line exposes over the same interval; with one,
  /// the lines expose one after another, the frame spans `shutter` plus
  /// `readout`, and motion during the sweep skews the picture.
  std::optional<float> readout{};

  /// `readout_direction`: the way the readout sweeps the picture, `down`
  /// unless stated, so that `down` reads the top line first. Not keyable
  /// and not a flag: which way a sensor reads is a fact about the camera
  /// nobody changes per render.
  std::optional<ReadoutDirection> readoutDirection{};

  /// `response { ... }`: the detector's bands, written inline. Not
  /// keyable, and not merged: a second `response` in either form is an
  /// error, since two sets of bands have no meaningful union. See
  /// `ResponseSettings`.
  std::optional<ResponseSettings> response{};

  /// `response "path"`: the '.response' file holding the block instead,
  /// as written, to be resolved relative to the camera file that names
  /// it. `-response` overrides it as `-lens` overrides `lens`.
  std::optional<std::string> responseFile{};

  /// `detector { ... }`: what the camera reads out with, see
  /// `DetectorSettings`. Not keyable, and not merged: a second `detector`
  /// is an error, as a second `response` is.
  std::optional<DetectorSettings> detector{};

  /// The keys the `motion` block wrote, in ascending time, or empty for
  /// a still camera. See `CameraKey`.
  std::vector<CameraKey> motion{};

  /// These settings at `seconds`: every keyable value resolved through
  /// the keys that state it, over the value the block itself states,
  /// with `motion` cleared.
  ///
  /// The renderer interpolates only the framing within one shutter and
  /// holds the rest, so the caller resolves the whole bag at shutter
  /// open and asks again at shutter shut for the framing alone.
  [[nodiscard]] CameraSettings at(float seconds) const;

  /// Does any key sit strictly inside the open interval
  /// `(open, shut)`? Such a key is not represented by the two samples
  /// the renderer takes, and the caller says so.
  [[nodiscard]] bool hasKeyBetween(float open, float shut) const;

  /// The settings, other than the framing, whose value at `shut`
  /// differs from their value at `open`: what the renderer cannot vary
  /// within one shutter and holds at its open value instead. Named as
  /// the grammar spells them, for one log line.
  [[nodiscard]] std::vector<std::string_view> heldOverShutter(float open,
                                                              float shut) const;
};

/// A parsed camera file, as written.
class CameraDocument final {
public:
  /// The source the document was parsed from, owned by the
  /// `LayoutDiagnostics` that loaded it.
  const LayoutSource *source{};

  /// Whatever the file's `camera` directives named, merged.
  CameraSettings camera{};
  LayoutLocation cameraLoc{};
};

/// Parse one camera source into a document.
///
/// Pure: no filesystem access. Errors and warnings accumulate in
/// `diags`, and the returned document is the best effort regardless, so
/// one bad statement does not hide the diagnostics of the statements
/// after it.
[[nodiscard]] CameraDocument parseCamera(LayoutDiagnostics &diags,
                                         const LayoutSource &source);

/// Read a camera file: parse, print every diagnostic to standard error
/// (colored when stderr is a terminal), and throw if any were errors.
///
/// \throws smdl::Error  If the file cannot be read, or on any parse
///                      error after printing the diagnostics.
///
[[nodiscard]] CameraDocument readCamera(const std::string &fileName);

/// The camera file a render should use, or empty for none: `given` if
/// the command line named one, else the `.camera` file beside
/// `sceneFileName` when that names a `.layout` and the sidecar exists.
///
/// The sidecar convention is what keeps a scene directory
/// self-contained: an exporter writes `shot.layout` and `shot.camera`
/// together and a bare `smdl-toy shot.layout` renders the shot it meant.
///
/// \throws smdl::Error  If `given` names a file that does not exist,
///                      since an explicit flag must never be ignored.
///
[[nodiscard]] std::string
resolveCameraFileName(const std::string &given,
                      const std::string &sceneFileName);

/// A parsed response file, as written: the camera's `response` block on
/// its own.
class ResponseDocument final {
public:
  /// The source the document was parsed from, owned by the
  /// `LayoutDiagnostics` that loaded it.
  const LayoutSource *source{};

  /// What the file's `response` block described.
  ResponseSettings response{};
  LayoutLocation responseLoc{};
};

/// Parse one response source into a document, as `parseCamera()` does a
/// camera: pure, best effort, every diagnostic in `diags`. A file with
/// no `response` block is an error, since a camera named it for one.
[[nodiscard]] ResponseDocument parseResponse(LayoutDiagnostics &diags,
                                             const LayoutSource &source);

/// Read a response file, as `readCamera()` reads a camera.
///
/// \throws smdl::Error  If the file cannot be read, or on any parse
///                      error after printing the diagnostics.
///
[[nodiscard]] ResponseDocument readResponse(const std::string &fileName);

/// The response file a camera should read through, or empty for none:
/// `given` if the command line named one, else `stated` if the camera
/// file did, resolved relative to `cameraFileName` so that a scene
/// directory stays self-contained.
///
/// \throws smdl::Error  If either names a file that does not exist,
///                      since neither may be quietly ignored.
///
[[nodiscard]] std::string
resolveResponseFileName(const std::string &given,
                        const std::string &cameraFileName,
                        const std::string &stated);
