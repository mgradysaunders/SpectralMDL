/// \file
/// The camera format: the shot. Where the picture is taken from, on what
/// body and through what lens, and what the photographer turned.
///
/// A `.camera` file is the second format of the layout family and is
/// parsed by the same syntax core (`TextParser.h`). The split is a
/// separation of concerns: a `.layout` says what is in the world and how
/// it moves, on an absolute clock; a `.camera` says what it is
/// photographed with; a `.sensor` says what the picture lands on; and a
/// `.lens` says what the light came through. One scene then takes as
/// many viewpoints as there are camera files, one body serves every shot
/// taken with it, and no file restates another.
///
/// The camera names its body and its lens, each a file beside it or the
/// ideal stand-in: `sensor human`, the CIE observer, and `lens ideal`,
/// the thin lens, which are both the defaults. It never writes a body
/// inline: a body is a fact shared by every shot, so it gets a file.
///
/// What the file deliberately does not carry: which instant to
/// photograph (`-time`, so one camera renders every frame of a shot) and
/// how big the picture is (`-resolution` and `-crop-window`, which are
/// facts about this render rather than about the camera, and which a
/// physical body decides for itself).
#pragma once

#include <map>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "Common.h"

#include "Layout/LayoutDiagnostics.h"
#include "Layout/SensorFile.h"

/// The extension that marks a camera file, which is how the render finds
/// the one beside a layout.
constexpr std::string_view CAMERA_EXTENSION = ".camera";

/// The word that names the observer in place of a body, and the word
/// that names the thin lens in place of a prescription.
///
/// \{
constexpr std::string_view SENSOR_HUMAN = "human";
constexpr std::string_view LENS_IDEAL = "ideal";
/// \}

/// The camera settings a `motion` key may restate, which is every one
/// that is a quantity to interpolate over the life of a shot.
///
/// The ones left out are left out because they are not: `blades` counts
/// the aperture's edges, `distortion_fit` is a bare flag, `lens` and
/// `sensor` are the instrument, `temperature` is the body's condition
/// over the shot, `iso` is applied after the render, `shutter`,
/// `readout`, and `readout_direction` describe the interval a key is
/// sampled over rather than something sampled within it, and `focus
/// auto` is a measurement rather than a value, as `focus infinity` is a
/// value with no distance to interpolate toward.
///
class CameraKeyable {
public:
  std::optional<float3> lookFrom{};
  std::optional<float3> lookTo{};
  std::optional<float3> lookUp{};

  /// `fovy` and `focal_length`: two ways of stating the thin lens's
  /// field. Alone, either one implies the other over a frame 24 mm
  /// tall, or over the body's frame when the camera names one; together
  /// they state the frame height of the observer's camera, and are
  /// refused as two statements of one fact over a body. Both are
  /// refused with a lens, whose field is the frame and the glass.
  ///
  /// \{
  std::optional<float> fovYDeg{};
  std::optional<float> focalLengthMM{};
  /// \}

  std::optional<float> fStop{};
  std::optional<float> aperture{};

  /// `focus`: the distance along the view axis in scene units, measured
  /// from the camera origin, which is the entrance pupil of both optics;
  /// or `INF` for `focus infinity`, which a `motion` key may not state,
  /// there being no distance to interpolate toward. `focus auto` is
  /// `CameraSettings::shouldAutofocus` instead, and clears this.
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
/// are the base, the sensor file overrides those where it speaks, the
/// camera file overrides both, and explicit command-line flags override
/// everything.
///
class CameraSettings final : public CameraKeyable {
public:
  std::optional<int> blades{};
  std::optional<bool> shouldFitDistortion{};

  /// `focus auto`: focus on whatever the center of the frame sees,
  /// measured once the scene is built, the way `-autolook` measures.
  /// Not keyable, being a measurement rather than a value, and refused
  /// beside a `motion` key that states `focus`, since the two would be
  /// two statements of the focus. Stating a distance after it turns it
  /// back off, as the last statement of a setting wins.
  bool shouldAutofocus{};

  /// `lens`: the '.lens' file the camera looks through, as written, to be
  /// resolved relative to the camera file that names it; or `ideal`, the
  /// thin lens, which unset means too. Not keyable: it is the lens, not
  /// a quantity to interpolate.
  ///
  /// With a prescription, the field of view is the body and the glass
  /// together, so `fovy` and every setting that stands in for what a
  /// real lens does on its own are refused rather than ignored.
  std::optional<std::string> lens{};

  /// `sensor`: the '.sensor' file the picture lands on, as written, to be
  /// resolved relative to the camera file that names it; or `human`, the
  /// CIE observer, which unset means too. Not keyable, and never a
  /// block: a body is a file.
  std::optional<std::string> sensor{};

  /// `temperature`: the body's degrees Celsius over the shot, which its
  /// dark current follows; 25 unless stated. A condition of the shot
  /// rather than a fact about the body, which is why it is here and not
  /// in the sensor file. Meaningless to the observer, and refused with
  /// it.
  std::optional<float> temperature{};

  /// `iso`: the ISO a physical sensor is read out at, positive; or
  /// `auto`, which unset means too: the ISO is metered from the rendered
  /// film as ISO 12232's saturation speed, never below the body's base.
  /// The last statement wins, so a number clears `auto` and `auto`
  /// clears a number. Not keyable, being applied after the render, and
  /// refused with the observer, whose film holds radiance, and with a
  /// body whose detector states its `gain`, which fixes the speed.
  ///
  /// \{
  std::optional<float> iso{};
  bool shouldMeterISO{};
  /// \}

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
  /// nonnegative, overriding the body's own and overridden by
  /// `-readout`. Zero is a global shutter, where every line exposes over
  /// the same interval; with one, the lines expose one after another,
  /// the frame spans `shutter` plus `readout`, and motion during the
  /// sweep skews the picture.
  std::optional<float> readout{};

  /// `readout_direction`: the way the readout sweeps the picture,
  /// overriding the body's own, which is `down` unless stated, so that
  /// `down` reads the top line first. Not keyable and not a flag: which
  /// way a sensor reads is a fact nobody changes per render.
  std::optional<ReadoutDirection> readoutDirection{};

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

  /// Where each setting was last stated inside a `camera` block, by the
  /// key as the grammar spells it, so that a refusal made after the
  /// parse (a setting with no meaning beside the lens or the body the
  /// camera names) can point a caret at the key rather than at the
  /// block. A setting stated only in a `motion` key is not here.
  std::map<std::string, LayoutLocation> keyLocs{};
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
/// The document's locations point into `diags`, which the caller owns
/// so that they outlive the read: a refusal made once the lens and the
/// body are known points at the key the file stated.
///
/// \throws smdl::Error  If the file cannot be read, or on any parse
///                      error after printing the diagnostics.
///
[[nodiscard]] CameraDocument readCamera(LayoutDiagnostics &diags,
                                        const std::string &fileName);

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
