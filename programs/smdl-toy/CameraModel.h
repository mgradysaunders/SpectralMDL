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
#include "Sensor/Sensor.h"

struct Options;

/// The resolved camera.
struct CameraModel final {
  /// What the camera is built from: the framing and its motion,
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
  LayoutDiagnostics documentDiags{};
  /// \}

  /// The sensor the camera names, resolved into the physics its curves
  /// imply, which nothing about this render varies, or nothing for the
  /// observer. Present under `-ideal` too, where the observer stands in
  /// for the sensor on its frame and pixels and `isPreview` says so, so
  /// that the meter, the readout, the develop and the report all read
  /// this one rather than each integrating the curves again.
  /// `Sensor::settings()` is what the `.sensor` file stated.
  std::optional<Sensor> sensor{};

  /// Is this `-ideal`'s preview of the camera? See `CameraFlags::isIdeal`.
  bool isPreview{};

  /// Under `-ideal` with a sensor, what the preview's film is multiplied by
  /// to be the irradiance the sensor's own optics would have put on it: the
  /// pupil integral at the middle of the frame, per unit of the scene
  /// radiance the preview reads there. `buildCamera()` sets it, since it
  /// depends on the focus.
  double previewIrradianceScale{};

  /// The white the sensor's develop balances to, `D65` unless the camera
  /// file or `-white-balance` states another. Meaningful to a physical
  /// sensor alone.
  WhiteBalance whiteBalance{};

  /// Is the camera file's focus `auto`? Then `options.focus` is not
  /// final: `solveAutofocus()` measures the committed scene and
  /// writes the distance, and the camera is built after it, as it is
  /// after `-autolook`.
  bool shouldAutofocus{};

  /// The sensor's degrees Celsius over the shot, the camera file's
  /// `temperature`, 25 unless stated. Meaningful to a physical sensor
  /// alone.
  float temperature{25.0f};

  /// The ISO the sensor is read out at, or nothing for auto: metered from
  /// the scene before the first sample and held for the sequence, see
  /// `Sensor/Meter.h`, or the stated gain's own speed when the sensor's
  /// detector fixes it. Meaningful to a physical sensor alone.
  std::optional<float> iso{};

  /// For the log and report: The associated camera file name, if applicable.
  std::string cameraFileName{};

  /// For the log and report: The associated sensor file name, if applicable.
  std::string sensorFileName{};

  /// For the log and report: The associated lens file name, if applicable.
  std::string lensFileName{};

  /// The picture's size in pixels: the sensor's own, or `-resolution`.
  [[nodiscard]] int2 resolution() const noexcept { return options.resolution; }

  /// What the film holds. See `CameraOptions::filmQuantity`.
  [[nodiscard]] FilmQuantity filmQuantity() const noexcept {
    return options.filmQuantity;
  }

  // TODO Rename `hasSensor`
  /// Does the render expose as the sensor, rather than as the observer?
  [[nodiscard]] bool hasPhysicalSensor() const noexcept {
    return sensor.has_value() && !isPreview;
  }

  /// Does `-ideal` preview the sensor through the observer, exposing the
  /// picture as the sensor would? See `isPreview`.
  [[nodiscard]] bool hasPreviewedSensor() const noexcept {
    return sensor.has_value() && isPreview;
  }

  /// Does the render look through the thin lens fitted to `options.lens`
  /// rather than through the lens? Under `-ideal`: the prescription
  /// stays in the options for `buildCamera()` to fit, at the focus the
  /// autofocus may have yet to measure.
  [[nodiscard]] bool shouldApproximateLens() const noexcept {
    return isPreview && options.lens.has_value();
  }
};

/// The name of a film quantity as the headers spell it.
[[nodiscard]] const char *filmQuantityName(FilmQuantity quantity) noexcept;

/// The effective focal length of the model's thin lens in scene units:
/// the frame height over twice the tangent of half the field of view,
/// which is what `fstop` is a fraction of and what the depth of field
/// is taken at. Meaningless with a lens, whose focal length is its own.
[[nodiscard]] float thinLensFocalLength(const CameraOptions &options) noexcept;

/// Resolve the camera from the files and the command line: the defaults,
/// the sensor file over them, and the camera file over both, which is
/// where the lens, the sensor, and the shot are stated. The command line
/// then frames the picture over the file's framing, sizes it, previews
/// it with `-ideal`, and states the ISO and the white balance over the
/// file's. Nothing here needs the scene, so it runs before anything slow
/// loads and a typo fails fast.
///
/// This also settles the camera's half of the clock: `gRenderShutter`'s
/// exposure, readout, and sweep, which what a layout's motion means
/// depends on, so it runs before the layout is read.
///
/// \throws smdl::Error  If a file cannot be read, a setting has no
///                      meaning with the instrument, or the picture's
///                      size disagrees with the sensor's.
///
[[nodiscard]] CameraModel resolveCameraModel(const Options &opts);

/// Build the camera the render looks through from the model's options,
/// as `-autolook` and the autofocus left them: the model's own, or the
/// thin lens fitted to its lens when `shouldApproximateLens()` says so,
/// logging the fit. Under `-ideal` with a sensor this settles
/// `previewIrradianceScale`.
///
/// \throws smdl::Error  If the camera cannot be built.
///
[[nodiscard]] Camera buildCamera(CameraModel &model);

/// Refuse what a render needs of the camera and a report does not: a
/// physical sensor counts the electrons of an exposure, and the preview
/// of one exposes the picture as it would, so a shut shutter renders
/// nothing either can use. Pointed at the camera file's `sensor` key.
///
/// \throws smdl::Error  If the camera cannot render.
///
void refuseUnrenderable(const CameraModel &model);

/// The report `-describe-camera` prints, as `resolveCameraModel()`
/// resolved the camera: the frame and the field, the pixels and the
/// pitch, the film quantity, the focus and the depth of field; a lens's
/// image circle, the share of the frame it leaves dark, and the thin lens
/// `-ideal` fits to it with how closely it lands; the bands and the tile,
/// the detector, the well, the base ISO, the ISO the sensor reads out at,
/// the exposure in a photographer's terms, the dynamic range, and the
/// color fit. Builds the lens to trace its field and fit it, which are the
/// slow things in it.
[[nodiscard]] std::string describeCamera(const CameraModel &model);
