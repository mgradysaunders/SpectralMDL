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

/// The camera settings a `motion` key may restate, which is every one
/// that is a quantity to interpolate over the life of a shot.
///
/// The three left out are left out because they are not: `blades`
/// counts the aperture's edges, `distortion_fit` is a bare flag, and
/// `shutter` is the interval a key is sampled over rather than
/// something sampled within it.
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

  /// `shutter`: the seconds from shutter open to shutter shut,
  /// nonnegative, which `-shutter` overrides. Zero or unset is a shut
  /// shutter, and every path then renders the one instant `-time`
  /// names, whatever motion the scene carries.
  ///
  /// This is the one exposure quantity the file carries, because it is
  /// a fact about the camera: how long it stays open. When it opens is
  /// not, which is why `-time` alone says that.
  std::optional<float> shutter{};

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
