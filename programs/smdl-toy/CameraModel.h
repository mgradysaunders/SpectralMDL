/// \file
/// The camera resolved: what it is, where it looks, and how this render
/// runs it, from the camera file, the sensor and lens files it names,
/// and the command line, in one place. It lowers into `CameraOptions`
/// for `Render/Camera`, which stays free of files.
#pragma once

#include <optional>
#include <string>

#include "Layout/SensorFile.h"
#include "Render/Camera.h"

struct Options;

/// The resolved camera.
struct CameraModel final {
  /// Where each part came from, for the log and the report: the camera
  /// file, or empty for none; the sensor file, or empty for the
  /// observer; the lens file, or empty for the thin lens.
  ///
  /// \{
  std::string cameraFileName{};
  std::string sensorFileName{};
  std::string lensFileName{};
  /// \}

  /// The body, or nothing for the observer.
  std::optional<SensorSettings> sensor{};

  /// The body's degrees Celsius over the shot, the camera file's
  /// `temperature`, 25 unless stated. Meaningful to a physical sensor
  /// alone.
  float temperature{25.0f};

  /// What `Render/Camera` is built from: the framing and its motion,
  /// the optics, the frame, the picture's size in pixels, and the film
  /// quantity.
  CameraOptions options{};

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

/// The report `-describe-camera` prints: the frame and the field, the
/// pixels and the pitch, the film quantity, the bands and the tile, and
/// the detector, as `resolveCameraModel()` resolved them. Builds the lens
/// to trace its field, which is the one slow thing in it.
[[nodiscard]] std::string describeCamera(const CameraModel &model);
