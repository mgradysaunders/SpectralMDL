/// \file
/// The camera resolved: what it is, where it looks, and how this render
/// runs it, from the camera file, the sensor and lens files it names,
/// and the command line, in one place. It lowers into `CameraOptions`
/// for `Render/Camera`, which stays free of files.
#pragma once

#include <optional>
#include <string>

#include "Layout/CameraFile.h"
#include "Layout/SensorFile.h"
#include "Render/Camera.h"

struct Options;

/// The resolved camera.
struct CameraModel final {
  /// Where each part came from, for the log and the report: the camera
  /// file, or empty for none; the sensor file, which under `-ideal` holds
  /// the body the preview stands in for, or empty for the observer; the
  /// lens file, or empty for the thin lens.
  ///
  /// \{
  std::string cameraFileName{};
  std::string sensorFileName{};
  std::string lensFileName{};
  /// \}

  /// The body, or nothing for the observer.
  std::optional<SensorSettings> sensor{};

  /// Is this `-ideal`'s preview of the camera? See `CameraFlags::isIdeal`.
  bool isPreview{};

  /// Under `-ideal`, the body whose exposure the preview simulates: the
  /// sensor the camera names, which the observer stands in for. Always
  /// empty when `sensor` is not.
  std::optional<SensorSettings> previewedSensor{};

  /// Does the render look through the thin lens fitted to `options.lens`
  /// rather than through the lens? Under `-ideal`, and under `-lens
  /// ideal` over a camera file's lens. The prescription stays in the
  /// options for `buildCamera()` to fit, at the focus the autofocus may
  /// have yet to measure.
  bool shouldApproximateLens{};

  /// Under `-ideal` with a body, what the preview's film is multiplied by
  /// to be the irradiance the body's own optics would have put on it: the
  /// pupil integral at the middle of the frame, per unit of the scene
  /// radiance the preview reads there. `buildCamera()` sets it, since it
  /// depends on the focus.
  double previewIrradianceScale{};

  /// The body's degrees Celsius over the shot, the camera file's
  /// `temperature`, 25 unless stated. Meaningful to a physical sensor
  /// alone.
  float temperature{25.0f};

  /// The ISO the body is read out at, or nothing for auto: metered from
  /// the rendered film, see `Sensor::meter()`, or the stated gain's own
  /// speed when the body's detector fixes it. Meaningful to a physical
  /// sensor alone.
  std::optional<float> iso{};

  /// The white the body's develop balances to, `D65` unless the camera
  /// file or `-white-balance` states another. Meaningful to a physical
  /// sensor alone.
  WhiteBalance whiteBalance{};

  /// Is the focus `auto`, from either source? Then `options.focus` is
  /// not final: `solveAutofocus()` measures the committed scene and
  /// writes the distance, and the camera is built after it, as it is
  /// after `-autolook`.
  bool shouldAutofocus{};

  /// What `Render/Camera` is built from: the framing and its motion,
  /// the optics, the frame, the picture's size in pixels, and the film
  /// quantity.
  CameraOptions options{};

  /// The camera document the settings came from, kept for the
  /// locations of its keys, and the sink that owns the text they point
  /// into: a refusal made once the scene is built points at the key the
  /// file stated. Both empty when no camera file was read.
  ///
  /// \{
  CameraDocument document{};
  LayoutDiagnostics cameraDiags{};
  /// \}

  [[nodiscard]] bool hasPhysicalSensor() const noexcept {
    return sensor.has_value();
  }

  /// What the film holds. See `CameraOptions::filmQuantity`.
  [[nodiscard]] FilmQuantity filmQuantity() const noexcept {
    return options.filmQuantity;
  }

  /// The picture's size in pixels: the body's own, or `-resolution`.
  [[nodiscard]] int2 resolution() const noexcept { return options.resolution; }
};

/// The name of a film quantity as the headers spell it.
[[nodiscard]] const char *filmQuantityName(FilmQuantity quantity) noexcept;

/// The effective focal length of the model's thin lens in scene units:
/// the frame height over twice the tangent of half the field of view,
/// which is what `-fstop` is a fraction of and what the depth of field
/// is taken at. Meaningless with a lens, whose focal length is its own.
[[nodiscard]] float thinLensFocalLength(const CameraOptions &options) noexcept;

/// Resolve the camera from the command line and the files it names, in
/// the order defaults, sensor file, camera file, flags, each later source
/// winning wherever it speaks. Nothing here needs the scene, so it runs
/// before anything slow loads and a typo fails fast.
///
/// This also settles the camera's half of the clock: `gRenderShutter`'s
/// exposure, readout, and sweep, which what a layout's motion means
/// depends on, so it runs before the layout is read.
///
/// \throws smdl::Error  If a file cannot be read, a setting has no
///                      meaning with the instrument, or the picture's
///                      size disagrees with the body's.
///
[[nodiscard]] CameraModel resolveCameraModel(const Options &opts);

/// Build the camera the render looks through from the model's options,
/// as `-autolook` and the autofocus left them: the model's own, or the
/// thin lens fitted to its lens when `shouldApproximateLens` says so,
/// logging the fit. Under `-ideal` with a body this settles
/// `previewIrradianceScale`.
///
/// \throws smdl::Error  If the camera cannot be built.
///
[[nodiscard]] Camera buildCamera(CameraModel &model);

/// Refuse what a render needs of the camera and a report does not: a
/// physical sensor counts the electrons of an exposure, and the preview
/// of one exposes the picture as it would, so a shut shutter renders
/// nothing either can use. Pointed at the `sensor` key when the camera
/// file named the body.
///
/// \throws smdl::Error  If the camera cannot render.
///
void refuseUnrenderable(const CameraModel &model, const Options &opts);

/// The report `-describe-camera` prints: the frame and the field, the
/// pixels and the pitch, the film quantity, the focus and the depth of
/// field, the bands and the tile, the detector, and the well, the base
/// ISO, and the ISO the body reads out at, as `resolveCameraModel()`
/// resolved them. Builds the lens to trace its field, which is the one
/// slow thing in it.
[[nodiscard]] std::string describeCamera(const CameraModel &model);
