#include <cmath>
#include <string>

#include "smdl/Support/Error.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Strings.h"

#include "../CommandLine.h"

#include "CameraModel.h"
#include "Layout/CameraFile.h"
#include "Layout/LensFile.h"
#include "Options.h"

namespace {

// The observer's frame: 24 mm tall, which is what the thin lens's
// f-number has always been a fraction of, and as wide as the picture is
// for its height, so that the pixels are square. In millimeters first,
// so that a 3:2 picture gets the 36 by 24 of full frame to the bit.
[[nodiscard]] float2 observerFrameSize(int2 resolution) {
  const float aspect{float(resolution.x) / float(resolution.y)};
  return 1e-3f * float2(24.0f * aspect, 24.0f);
}

// With a lens the frame is the body and the prescription together, and
// every setting the thin lens uses to stand in for a real one is either
// meaningless or now emergent. Naming one anyway is a mistake worth
// reporting rather than a value to quietly drop, and this is the one
// place both sources have had their say: an explicit flag and whatever
// the camera file resolved to at shutter open.
void refuseThinLensSettings(const Options &opts,
                            const CameraSettings &fileCamera) {
  struct Refusal final {
    const char *name;
    bool wasStated;
    const char *why;
  };
  const Refusal refusals[]{
      {"fovy", opts.camera.fovYDeg.wasGiven || fileCamera.fovYDeg.has_value(),
       "the field of view is the frame and the glass together, and the log "
       "reports it"},
      {"aperture",
       opts.camera.aperture.wasGiven || fileCamera.aperture.has_value(),
       "the aperture is the lens's own stop, which 'fstop' still narrows"},
      {"distortion_k1",
       opts.camera.distortionK1.wasGiven || fileCamera.distortionK1.has_value(),
       "the distortion is whatever the surfaces do"},
      {"distortion_k2",
       opts.camera.distortionK2.wasGiven || fileCamera.distortionK2.has_value(),
       "the distortion is whatever the surfaces do"},
      {"distortion_fit",
       opts.camera.shouldFitDistortion.wasGiven ||
           fileCamera.shouldFitDistortion.has_value(),
       "there is no distortion polynomial to refit"},
      {"vignetting",
       opts.camera.vignetting.wasGiven || fileCamera.vignetting.has_value(),
       "the cos^4 falloff comes out of the pupil integral, and is not "
       "optional there"},
      {"cat_eye", opts.camera.catEye.wasGiven || fileCamera.catEye.has_value(),
       "the barrel vignette is whatever the clear apertures do"},
      {"cat_eye_radius",
       opts.camera.catEyeRadius.wasGiven || fileCamera.catEyeRadius.has_value(),
       "the barrel vignette is whatever the clear apertures do"},
  };
  for (const auto &refusal : refusals)
    if (refusal.wasStated)
      throw smdl::Error(smdl::concat(
          "'", refusal.name, "' has no meaning with a lens: ", refusal.why));
}

// The readout's sweep over the picture, which is where the direction
// and the resolution meet.
void settleReadoutLines(ReadoutDirection direction, int2 resolution) {
  gRenderShutter.isReadoutAlongX = direction == ReadoutDirection::LEFT ||
                                   direction == ReadoutDirection::RIGHT;
  gRenderShutter.isReadoutReversed =
      direction == ReadoutDirection::UP || direction == ReadoutDirection::LEFT;
  gRenderShutter.numReadoutLines =
      size_t(gRenderShutter.isReadoutAlongX ? resolution.x : resolution.y);
  // One line has nothing to spread a readout over, and a readout that
  // stayed would still lengthen the frame for no line to reach.
  if (gRenderShutter.readout > 0 && gRenderShutter.numReadoutLines <= 1) {
    SMDL_LOG_INFO("Rolling shutter: one line along the sweep, so the "
                  "frame reads out at once");
    gRenderShutter.readout = 0;
  }
  if (gRenderShutter.readout > 0) {
    const char *sweep{direction == ReadoutDirection::DOWN   ? "top to bottom"
                      : direction == ReadoutDirection::UP   ? "bottom to top"
                      : direction == ReadoutDirection::LEFT ? "right to left"
                                                            : "left to right"};
    SMDL_LOG_INFO(
        "Rolling shutter: ", smdl::Brief(1000.0f * gRenderShutter.readout, 4),
        " ms readout ", sweep, " over ",
        smdl::Brief(1000.0f * gRenderShutter.exposure, 4),
        " ms exposure, so the frame spans ",
        smdl::Brief(1000.0f * gRenderShutter.length(), 4), " ms");
  }
}

// One line naming the body's bands and their tile.
[[nodiscard]] std::string describeResponse(const ResponseSettings &response) {
  auto line{smdl::concat(response.bands.size(), " band(s)")};
  for (size_t i = 0; i < response.bands.size(); i++)
    line += smdl::concat(i == 0 ? " " : ", ", response.bands[i].name);
  if (response.kind == ResponseKind::QE) {
    line += " in electrons per photon";
  } else {
    line += smdl::concat(
        " as relative weights scaled to a peak quantum efficiency of ",
        smdl::Brief(response.peakQE.value_or(DEFAULT_PEAK_QE), 4),
        response.peakQE ? "" : " (generic)");
  }
  if (response.hasCFA()) {
    line += smdl::concat(", tiled ", response.cfaColumns, "x",
                         response.cfaRows(), " as");
    for (size_t i = 0; i < response.cfa.size(); i++)
      line += smdl::concat(i > 0 && i % response.cfaColumns == 0 ? " /" : "",
                           " ", response.bands[response.cfa[i]].name);
  }
  return line;
}

// One line naming the detector's chain.
[[nodiscard]] std::string describeDetector(const DetectorSettings &detector) {
  return smdl::concat(
      "well ",
      detector.fullWell
          ? smdl::concat(smdl::Brief(*detector.fullWell, 6), " e-")
      : detector.baseISO
          ? smdl::concat("from base ISO ", smdl::Brief(*detector.baseISO, 6))
          : std::string("from the pitch"),
      ", read noise ", smdl::Brief(detector.readNoise, 4), " e-, dark ",
      smdl::Brief(detector.darkCurrent, 4), " e-/s at ",
      smdl::Brief(detector.referenceTemperature, 4), " C doubling every ",
      smdl::Brief(detector.doublingTemperature, 4), " C; ", detector.bits,
      " bits at ",
      detector.gain ? smdl::concat(smdl::Brief(*detector.gain, 6), " DN/e-")
                    : std::string("a gain filling the well"),
      ", black level ", smdl::Brief(detector.blackLevel, 6), " DN");
}

// The lens the model names, built as the camera builds it, for the
// report.
[[nodiscard]] Lens buildLens(const CameraModel &model) {
  const auto &options{model.options};
  const float focus{options.focus > 0
                        ? options.focus
                        : length(options.lookTo - options.lookFrom)};
  return Lens{*options.lens, LensOptions{focus, options.fStop, options.blades,
                                         smdl::radians(options.bladeAngleDeg)}};
}

} // namespace

const char *filmQuantityName(FilmQuantity quantity) noexcept {
  return quantity == FilmQuantity::IRRADIANCE ? "irradiance" : "radiance";
}

CameraModel resolveCameraModel(const Options &opts) {
  auto model{CameraModel{}};
  auto &options{model.options};
  // The camera file first: '-camera' if it was given, else the '.camera'
  // beside the layout. Its settings are resolved at shutter open, which
  // is where everything but the framing is read: the renderer varies the
  // framing within one shutter and holds the rest.
  model.cameraFileName =
      resolveCameraFileName(opts.camera.file, opts.scene.inputSceneFile);
  const auto document{model.cameraFileName.empty()
                          ? CameraDocument()
                          : readCamera(model.cameraFileName)};
  const auto fileCamera{document.camera.at(opts.scene.time)};
  // The body: the flag if it spoke, else the file, else the observer. A
  // body the flag replaces with the observer still decides the frame and
  // the pixels, so the framing never changes between the two.
  const auto sensorWord{
      opts.camera.sensor.wasGiven
          ? opts.camera.sensor.value
          : fileCamera.sensor.value_or(std::string(SENSOR_HUMAN))};
  auto body{std::optional<SensorSettings>()};
  bool isBodyReplaced{false};
  if (sensorWord != SENSOR_HUMAN) {
    model.sensorFileName = resolveSensorFileName(
        opts.camera.sensor.wasGiven ? sensorWord : std::string(),
        model.cameraFileName,
        opts.camera.sensor.wasGiven ? std::string() : sensorWord);
    body = readSensor(model.sensorFileName).sensor;
    model.sensor = body;
  } else if (opts.camera.sensor.wasGiven && fileCamera.sensor &&
             *fileCamera.sensor != SENSOR_HUMAN) {
    body = readSensor(resolveSensorFileName(std::string(), model.cameraFileName,
                                            *fileCamera.sensor))
               .sensor;
    isBodyReplaced = true;
    SMDL_LOG_INFO(
        "Sensor: -sensor human replaces the camera file's ",
        smdl::Quoted(body->name.empty() ? *fileCamera.sensor : body->name),
        " with the observer, on that body's frame and pixels");
  }
  // The picture's size and the frame: a body's own, or -resolution's
  // over the observer's frame.
  if (body) {
    if (opts.image.resolution.wasGiven &&
        !smdl::isAllTrue(opts.image.resolution.value == body->pixels))
      throw smdl::Error(smdl::concat(
          "-resolution ", spellVector(opts.image.resolution.value),
          " is not the sensor's ", body->pixels.x, ",", body->pixels.y,
          ": a body renders exactly its own pixels; leave -resolution "
          "out, or use -crop-window to render part of the frame"));
    options.resolution = body->pixels;
    options.frameSize = 1e-6f * float2(body->pitchUM.x * float(body->pixels.x),
                                       body->pitchUM.y * float(body->pixels.y));
  } else {
    options.resolution = opts.image.resolution.value;
    options.frameSize = observerFrameSize(options.resolution);
  }
  options.filmQuantity =
      model.sensor ? FilmQuantity::IRRADIANCE : FilmQuantity::RADIANCE;
  // The two clocks. Which instant to photograph is the command line's
  // alone, so one camera file renders every frame of a shot; how long
  // the shutter stays open is the camera's, which the file may state
  // and '-shutter' overrides; how long the readout takes to sweep the
  // frame and which way is the body's, which the camera file overrides
  // and '-readout' overrides again.
  gRenderShutter.time = opts.scene.time;
  gRenderShutter.exposure = pick(opts.camera.shutter, fileCamera.shutter);
  gRenderShutter.readout = opts.camera.readout.wasGiven
                               ? opts.camera.readout.value
                           : fileCamera.readout ? *fileCamera.readout
                           : body               ? body->readout
                                                : 0.0f;
  settleReadoutLines(fileCamera.readoutDirection ? *fileCamera.readoutDirection
                     : body                      ? body->readoutDirection
                                                 : ReadoutDirection::DOWN,
                     options.resolution);
  // The lens: the flag if it spoke, else the file, else the thin lens.
  const auto lensWord{opts.camera.lens.wasGiven
                          ? opts.camera.lens.value
                          : fileCamera.lens.value_or(std::string(LENS_IDEAL))};
  if (lensWord != LENS_IDEAL) {
    model.lensFileName = resolveLensFileName(
        opts.camera.lens.wasGiven ? lensWord : std::string(),
        model.cameraFileName,
        opts.camera.lens.wasGiven ? std::string() : lensWord);
    options.lens = readLens(model.lensFileName).lens;
    refuseThinLensSettings(opts, fileCamera);
  } else if (opts.camera.lens.wasGiven && fileCamera.lens &&
             *fileCamera.lens != LENS_IDEAL) {
    SMDL_LOG_INFO("Lens: -lens ideal replaces the camera file's ",
                  smdl::Quoted(*fileCamera.lens), " with the thin lens");
  }
  // The camera, merged from three sources in increasing order of
  // priority: the defaults in `CameraOptions`, whatever the camera file's
  // 'camera' directive named, and whatever the command line explicitly
  // gave. A flag that was not given must not override the file, so what
  // decides is the occurrence count rather than the value.
  options.lookFrom = pick(opts.camera.lookFrom, fileCamera.lookFrom);
  options.lookTo = pick(opts.camera.lookTo, fileCamera.lookTo);
  options.lookUp = pick(opts.camera.lookUp, fileCamera.lookUp);
  options.fovYDeg = pick(opts.camera.fovYDeg, fileCamera.fovYDeg);
  options.fStop = pick(opts.camera.fStop, fileCamera.fStop);
  options.aperture = pick(opts.camera.aperture, fileCamera.aperture);
  options.focus = pick(opts.camera.focus, fileCamera.focus);
  options.blades = pick(opts.camera.blades, fileCamera.blades);
  options.bladeAngleDeg =
      pick(opts.camera.bladeAngleDeg, fileCamera.bladeAngleDeg);
  options.distortionK1 =
      pick(opts.camera.distortionK1, fileCamera.distortionK1);
  options.distortionK2 =
      pick(opts.camera.distortionK2, fileCamera.distortionK2);
  options.shouldFitDistortion =
      pick(opts.camera.shouldFitDistortion, fileCamera.shouldFitDistortion);
  options.vignetting = pick(opts.camera.vignetting, fileCamera.vignetting);
  options.catEye = pick(opts.camera.catEye, fileCamera.catEye);
  options.catEyeRadius =
      pick(opts.camera.catEyeRadius, fileCamera.catEyeRadius);
  options.noLOD = opts.render.sampling.noLOD;
  // The same exclusivity the command line checks, now that the file has
  // had its say: either source can supply either spelling, so only the
  // merged pair can be checked for naming both.
  if (options.fStop > 0 && options.aperture > 0)
    throw smdl::Error("expected at most one of -fstop and -aperture between "
                      "the command line and the camera file's 'camera' "
                      "directive (they are two spellings of the same "
                      "quantity)");
  // What a physical sensor needs of the optics, refused here so that it
  // fails before anything slow loads, since under -autolook the camera
  // is built after the scene.
  if (model.sensor && !options.lens && !(options.fStop > 0) &&
      !(options.aperture > 0))
    throw smdl::Error("a physical sensor integrates the irradiance over a "
                      "pupil, and a pinhole has none: state 'fstop' or "
                      "'aperture'");
  // The body's condition over the shot, which only a body has.
  if (fileCamera.temperature) {
    if (model.sensor) {
      model.temperature = *fileCamera.temperature;
    } else if (isBodyReplaced) {
      SMDL_LOG_INFO("Sensor: 'temperature' is ignored, since -sensor human "
                    "replaced the body it was stated for");
    } else {
      throw smdl::Error("'temperature' is a physical sensor's condition, "
                        "and this camera's sensor is 'human'");
    }
  }
  // What a readout needs, refused here for the same reason.
  if (!opts.image.outputDN.empty()) {
    if (!model.sensor)
      throw smdl::Error("-output-dn reads a body out, and this camera's "
                        "sensor is 'human': name a '.sensor' file with "
                        "'sensor' in the camera file or with -sensor");
    if (!gRenderShutter.hasExposure())
      throw smdl::Error("-output-dn needs an exposure: state 'shutter'");
  }
  // The camera's framing at shutter shut. The keys are absolute readings
  // of the clock, so a flag that replaces the framing drops the track
  // rather than moving a camera the file never described.
  if (!document.camera.motion.empty()) {
    const auto shutSeconds{gRenderShutter.secondsAt(1.0f)};
    const char *framingFlag{opts.camera.autolook.isEnabled  ? "-autolook"
                            : opts.camera.lookFrom.wasGiven ? "-look-from"
                            : opts.camera.lookTo.wasGiven   ? "-look-to"
                            : opts.camera.lookUp.wasGiven   ? "-look-up"
                                                            : nullptr};
    if (framingFlag) {
      SMDL_LOG_INFO("Camera motion: dropped, since ", framingFlag,
                    " replaces the framing the camera file's 'motion' "
                    "was written against");
    } else if (!gRenderShutter.spansTime()) {
      SMDL_LOG_INFO("Camera motion: the shutter is shut, so the camera "
                    "holds its framing at ",
                    gRenderShutter.time, " s");
    } else {
      const auto shutCamera{document.camera.at(shutSeconds)};
      options.hasMotion = true;
      options.lookFromShut = pick(opts.camera.lookFrom, shutCamera.lookFrom);
      options.lookToShut = pick(opts.camera.lookTo, shutCamera.lookTo);
      options.lookUpShut = pick(opts.camera.lookUp, shutCamera.lookUp);
    }
    // What the shutter cannot carry: a lens setting the track varies
    // over the shutter is read once, at open, and held.
    if (gRenderShutter.spansTime()) {
      if (const auto held{document.camera.heldOverShutter(gRenderShutter.time,
                                                          shutSeconds)};
          !held.empty()) {
        auto names{std::string()};
        for (const auto &name : held)
          names += (names.empty() ? "" : ", ") + std::string(name);
        SMDL_LOG_INFO("Camera motion: ", names,
                      " vary over the shutter, which only the framing does; "
                      "they hold the value at shutter open");
      }
      if (document.camera.hasKeyBetween(gRenderShutter.time, shutSeconds))
        SMDL_LOG_INFO("Camera motion: a key sits inside the shutter, so the "
                      "camera moves along the chord of its two ends");
    }
  }
  // What was resolved, one line per part.
  if (model.sensor) {
    const auto &sensor{*model.sensor};
    const auto sizeMM{sensor.sizeMM()};
    SMDL_LOG_INFO("Sensor: ",
                  sensor.name.empty() ? std::string("(unnamed)")
                                      : smdl::concat(smdl::Quoted(sensor.name)),
                  " from ", smdl::QuotedPath(model.sensorFileName), ": ",
                  sensor.pixels.x, " by ", sensor.pixels.y, " pixels at ",
                  smdl::Brief(sensor.pitchUM.x, 4),
                  sensor.pitchUM.x != sensor.pitchUM.y
                      ? smdl::concat(" by ", smdl::Brief(sensor.pitchUM.y, 4))
                      : std::string(),
                  " um, a ", smdl::Brief(sizeMM.x, 4), " by ",
                  smdl::Brief(sizeMM.y, 4),
                  " mm frame; the film holds the irradiance at it");
    SMDL_LOG_INFO("Response: ", describeResponse(sensor.response));
    SMDL_LOG_INFO("Detector: ", sensor.hasDetectorBlock ? "" : "generic, ",
                  describeDetector(sensor.detector));
  } else if (body) {
    const auto frameMM{1e3f * options.frameSize};
    SMDL_LOG_INFO("Sensor: the observer on a ", smdl::Brief(frameMM.x, 4),
                  " by ", smdl::Brief(frameMM.y, 4), " mm frame of ",
                  options.resolution.x, " by ", options.resolution.y,
                  " pixels; the film holds radiance");
  }
  return model;
}

std::string describeCamera(const CameraModel &model) {
  const auto &options{model.options};
  auto text{std::string()};
  const auto line{[&](auto &&...parts) {
    text += smdl::concat(parts...);
    text += '\n';
  }};
  const auto spell3{[](float3 v) {
    return smdl::concat(smdl::Brief(v.x, 5), " ", smdl::Brief(v.y, 5), " ",
                        smdl::Brief(v.z, 5));
  }};
  const auto frameMM{1e3f * options.frameSize};
  // The body's own pitch, or the observer's square one off the frame's
  // height, which is what its width was made from.
  const auto pitchUM{model.sensor ? model.sensor->pitchUM
                                  : float2(1e6f * options.frameSize.y /
                                           float(options.resolution.y))};
  line("camera: ", model.cameraFileName.empty()
                       ? std::string("the defaults and the command line")
                       : smdl::concat(smdl::QuotedPath(model.cameraFileName)));
  line("  looks from ", spell3(options.lookFrom), " to ",
       spell3(options.lookTo), ", up ", spell3(options.lookUp),
       options.hasMotion ? ", and moves over the shutter" : "");
  line("  frame: ", smdl::Brief(frameMM.x, 5), " by ",
       smdl::Brief(frameMM.y, 5), " mm, ", options.resolution.x, " by ",
       options.resolution.y, " pixels at ", smdl::Brief(pitchUM.x, 4),
       pitchUM.x != pitchUM.y ? smdl::concat(" by ", smdl::Brief(pitchUM.y, 4))
                              : std::string(),
       " um");
  line("  film: spectral ", filmQuantityName(options.filmQuantity),
       options.filmQuantity == FilmQuantity::IRRADIANCE
           ? ", W/(m^2 nm) at the sensor"
           : ", W/(m^2 sr nm) of the scene");
  line("  shutter: ",
       gRenderShutter.hasExposure()
           ? smdl::concat(smdl::Brief(1e3f * gRenderShutter.exposure, 5), " ms")
           : std::string("shut"),
       gRenderShutter.readout > 0
           ? smdl::concat(
                 ", read out over ",
                 smdl::Brief(1e3f * gRenderShutter.readout, 5), " ms ",
                 gRenderShutter.isReadoutAlongX
                     ? (gRenderShutter.isReadoutReversed ? "right to left"
                                                         : "left to right")
                     : (gRenderShutter.isReadoutReversed ? "bottom to top"
                                                         : "top to bottom"))
           : std::string(", global"));
  const float focus{options.focus > 0
                        ? options.focus
                        : length(options.lookTo - options.lookFrom)};
  if (options.lens) {
    const auto lens{buildLens(model)};
    const float halfHeight{0.5f * options.frameSize.y};
    const float halfDiagonal{
        0.5f * std::hypot(options.frameSize.x, options.frameSize.y)};
    const float vertical{lens.fieldAngleAt(halfHeight)};
    const float diagonal{lens.fieldAngleAt(halfDiagonal)};
    const float circle{lens.imageCircleRadius()};
    line("lens: ",
         options.lens->name.empty()
             ? std::string("(unnamed)")
             : smdl::concat(smdl::Quoted(options.lens->name)),
         " from ", smdl::QuotedPath(model.lensFileName));
    line("  focal length ", smdl::Brief(1e3f * lens.focalLength(), 5),
         " mm, f/", smdl::Brief(lens.fNumberWideOpen(), 4), " wide open",
         lens.fNumber() != lens.fNumberWideOpen()
             ? smdl::concat(", stopped down to f/",
                            smdl::Brief(lens.fNumber(), 4))
             : std::string(),
         ", focused at ", smdl::Brief(focus, 5), " scene units");
    line("  field: ",
         vertical > 0
             ? smdl::concat(smdl::Brief(2 * smdl::degrees(vertical), 4),
                            " degrees top to bottom")
             : std::string("dark at the top and bottom"),
         diagonal > 0
             ? smdl::concat(", ", smdl::Brief(2 * smdl::degrees(diagonal), 4),
                            " degrees across the diagonal")
             : std::string(", dark in the corners"));
    line("  image circle: ", smdl::Brief(2e3f * circle, 4),
         " mm across, against a frame diagonal of ",
         smdl::Brief(2e3f * halfDiagonal, 4), " mm",
         circle >= halfDiagonal ? ", which it covers"
                                : ", which reaches past it");
  } else {
    const float focalLength{0.5f /
                            std::tan(smdl::radians(options.fovYDeg / 2)) *
                            options.frameSize.y};
    const float fNumber{options.aperture > 0
                            ? focalLength / (2 * options.aperture)
                            : options.fStop};
    line("lens: the thin lens");
    line("  field ", smdl::Brief(options.fovYDeg, 4),
         " degrees top to bottom, a focal length of ",
         smdl::Brief(1e3f * focalLength, 5), " mm over the frame");
    line("  ", fNumber > 0 ? smdl::concat("f/", smdl::Brief(fNumber, 4),
                                          ", focused at ",
                                          smdl::Brief(focus, 5), " scene units")
                           : std::string("a pinhole"));
  }
  if (model.sensor) {
    const auto &sensor{*model.sensor};
    line("sensor: ",
         sensor.name.empty() ? std::string("(unnamed)")
                             : smdl::concat(smdl::Quoted(sensor.name)),
         " from ", smdl::QuotedPath(model.sensorFileName));
    line("  response: ", describeResponse(sensor.response));
    line("  detector: ", sensor.hasDetectorBlock ? "" : "generic, ",
         describeDetector(sensor.detector));
    line("  temperature: ", smdl::Brief(model.temperature, 4), " C");
  } else {
    line("sensor: the observer, so no bands and no readout");
  }
  return text;
}
