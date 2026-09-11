#include <cmath>
#include <string>

#include "smdl/Support/Error.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Strings.h"

#include "../CommandLine.h"

#include "CameraModel.h"
#include "Layout/LensFile.h"
#include "Options.h"
#include "Sensor/Sensor.h"

namespace {

// The observer's frame: 24 mm tall, which is what the thin lens's
// f-number has always been a fraction of, and as wide as the picture is
// for its height, so that the pixels are square. In millimeters first,
// so that a 3:2 picture gets the 36 by 24 of full frame to the bit.
[[nodiscard]] float2 observerFrameSize(int2 resolution, float heightMM) {
  const float aspect{float(resolution.x) / float(resolution.y)};
  return 1e-3f * float2(heightMM * aspect, heightMM);
}

// A setting as the source that stated it spells it: the flag, or the
// key in quotes. Every flag is its key with the underscores replaced.
[[nodiscard]] std::string spellSetting(std::string_view key, bool asFlag) {
  if (!asFlag) return smdl::concat("'", key, "'");
  auto flag{"-" + std::string(key)};
  for (auto &c : flag)
    if (c == '_') c = '-';
  return flag;
}

// A refusal of a setting the camera file or the command line stated,
// once the lens and the body are known: pointed at the key with a caret
// when the file stated it and the command line did not override it,
// and a plain error naming the flag otherwise. The pointed form reads
// as the compiler's own located errors do, the file, line, and column
// ahead of the message and the excerpt beneath it.
class Refuser final {
public:
  explicit Refuser(const CameraDocument &document) : mDocument(document) {}

  [[noreturn]] void refuse(std::string_view key, bool wasFlagGiven,
                           const std::string &message) const {
    if (!wasFlagGiven) {
      if (auto itr{mDocument.keyLocs.find(std::string(key))};
          itr != mDocument.keyLocs.end())
        throw smdl::Error(
            smdl::concat(LayoutDiagnostics::where(itr->second), ": ", message),
            "\n" + LayoutDiagnostics::excerpt(itr->second));
    }
    throw smdl::Error(message);
  }

private:
  const CameraDocument &mDocument;
};

// With a lens the frame is the body and the prescription together, and
// every setting the thin lens uses to stand in for a real one is either
// meaningless or now emergent. Naming one anyway is a mistake worth
// reporting rather than a value to quietly drop, and this is the one
// place both sources have had their say: an explicit flag and whatever
// the camera file resolved to at shutter open.
void refuseThinLensSettings(const Options &opts,
                            const CameraSettings &fileCamera,
                            const Refuser &refuser) {
  struct Refusal final {
    const char *key;
    bool wasFlagGiven;
    bool wasFileStated;
    const char *why;
  };
  const Refusal refusals[]{
      {"fovy", opts.camera.fovYDeg.wasGiven, fileCamera.fovYDeg.has_value(),
       "the field of view is the frame and the glass together, and the log "
       "reports it"},
      {"focal_length", opts.camera.focalLengthMM.wasGiven,
       fileCamera.focalLengthMM.has_value(),
       "the focal length is the prescription's own, and the log reports it"},
      {"aperture", opts.camera.aperture.wasGiven,
       fileCamera.aperture.has_value(),
       "the aperture is the lens's own stop, which 'fstop' still narrows"},
      {"distortion_k1", opts.camera.distortionK1.wasGiven,
       fileCamera.distortionK1.has_value(),
       "the distortion is whatever the surfaces do"},
      {"distortion_k2", opts.camera.distortionK2.wasGiven,
       fileCamera.distortionK2.has_value(),
       "the distortion is whatever the surfaces do"},
      {"distortion_fit", opts.camera.shouldFitDistortion.wasGiven,
       fileCamera.shouldFitDistortion.has_value(),
       "there is no distortion polynomial to refit"},
      {"vignetting", opts.camera.vignetting.wasGiven,
       fileCamera.vignetting.has_value(),
       "the cos^4 falloff comes out of the pupil integral, and is not "
       "optional there"},
      {"cat_eye", opts.camera.catEye.wasGiven, fileCamera.catEye.has_value(),
       "the barrel vignette is whatever the clear apertures do"},
      {"cat_eye_radius", opts.camera.catEyeRadius.wasGiven,
       fileCamera.catEyeRadius.has_value(),
       "the barrel vignette is whatever the clear apertures do"},
  };
  for (const auto &refusal : refusals)
    if (refusal.wasFlagGiven || refusal.wasFileStated)
      refuser.refuse(
          refusal.key, refusal.wasFlagGiven,
          smdl::concat(spellSetting(refusal.key, refusal.wasFlagGiven),
                       " has no meaning with a lens: ", refusal.why));
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

// One line naming the detector's chain, with the well and the gain the
// sensor's physics resolved.
[[nodiscard]] std::string describeDetector(const Sensor &sensor) {
  const auto &detector{sensor.settings().detector};
  return smdl::concat(
      "read noise ", smdl::Brief(detector.readNoise, 4), " e-, dark ",
      smdl::Brief(detector.darkCurrent, 4), " e-/s at ",
      smdl::Brief(detector.referenceTemperature, 4), " C doubling every ",
      smdl::Brief(detector.doublingTemperature, 4), " C; ", detector.bits,
      " bits, black level ", smdl::Brief(detector.blackLevel, 6), " DN, ",
      sensor.hasFixedGain()
          ? smdl::concat("a stated gain of ", smdl::Brief(*detector.gain, 6),
                         " DN/e-")
          : smdl::concat(smdl::Brief(sensor.gain(sensor.baseISO()), 6),
                         " DN/e- at the base ISO"));
}

// One line on the well and the base ISO, which are one fact, and what
// they rest on: the most sensitive band's count under D55.
[[nodiscard]] std::string describeWell(const Sensor &sensor) {
  const auto &bands{sensor.settings().response.bands};
  return smdl::concat(
      smdl::Brief(sensor.fullWell(), 6), " e- ",
      sensor.wellSource() == WellSource::STATED ? "stated"
      : sensor.wellSource() == WellSource::FROM_BASE_ISO
          ? "from the base ISO"
          : "from the pitch, the generic well",
      "; base ISO ", smdl::Brief(sensor.baseISO(), 5),
      sensor.isBaseISOStated() ? " stated" : " from the well", ", ",
      bands.empty() ? std::string("no band")
                    : smdl::concat(smdl::Quoted(bands[sensor.peakBand()].name)),
      " counting ", smdl::Brief(sensor.peakElectronsPerLuxSecond(), 4),
      " e- per lux-second under D55");
}

// One line on the ISO the body reads out at: stated, the fixed gain's
// own, or metered from the film, which cannot be known before it.
[[nodiscard]] std::string describeISO(const Sensor &sensor,
                                      const std::optional<float> &iso) {
  if (sensor.hasFixedGain())
    return smdl::concat(smdl::Brief(sensor.fixedGainISO(), 5),
                        ", the saturation speed of the stated gain, so "
                        "nothing is metered");
  if (iso)
    return smdl::concat(
        smdl::Brief(*iso, 6), " stated, ", smdl::Brief(sensor.gain(*iso), 6),
        " DN/e-",
        *iso < sensor.baseISO()
            ? ", below the base, so the well clips before the ADC does"
            : "");
  return smdl::concat("auto, metered from the film once it is rendered, "
                      "from the base ",
                      smdl::Brief(sensor.baseISO(), 5), " up to ",
                      smdl::Brief(sensor.maxISO(), 6));
}

// The focus distance the camera will be built with, as the camera
// itself resolves an unstated one.
[[nodiscard]] float focusDistanceOf(const CameraOptions &options) {
  return options.focus > 0 ? options.focus
                           : length(options.lookTo - options.lookFrom);
}

// A distance for the report, which may be infinite.
[[nodiscard]] std::string spellDistance(float distance) {
  return distance < INF ? smdl::concat(smdl::Brief(distance, 5), " scene units")
                        : std::string("infinity");
}

// The depth of field for the report, one line, or none for a pinhole.
[[nodiscard]] std::string describeDepthOfField(const DepthOfField &dof) {
  if (!dof.hasLimits()) return {};
  return smdl::concat("  depth of field: ", spellDistance(dof.nearLimit),
                      " to ", spellDistance(dof.farLimit), ", hyperfocal ",
                      spellDistance(dof.hyperfocal),
                      ", at a circle of confusion of ",
                      smdl::Brief(1e3f * dof.circleOfConfusion, 4), " mm\n");
}

// The lens the model names, built as the camera builds it, for the
// report.
[[nodiscard]] Lens buildLens(const CameraModel &model) {
  const auto &options{model.options};
  const float focus{focusDistanceOf(options)};
  return Lens{*options.lens, LensOptions{std::isinf(focus) ? 0.0f : focus,
                                         options.fStop, options.blades,
                                         smdl::radians(options.bladeAngleDeg)}};
}

} // namespace

const char *filmQuantityName(FilmQuantity quantity) noexcept {
  return quantity == FilmQuantity::IRRADIANCE ? "irradiance" : "radiance";
}

float thinLensFocalLength(const CameraOptions &options) noexcept {
  return 0.5f / std::tan(smdl::radians(options.fovYDeg / 2)) *
         options.frameSize.y;
}

CameraModel resolveCameraModel(const Options &opts) {
  auto model{CameraModel{}};
  auto &options{model.options};
  // The camera file first: '-camera' if it was given, else the '.camera'
  // beside the layout. Its settings are resolved at shutter open, which
  // is where everything but the framing is read: the renderer varies the
  // framing within one shutter and holds the rest. The document stays
  // with the model for the locations of its keys.
  model.cameraFileName =
      resolveCameraFileName(opts.camera.file, opts.scene.inputSceneFile);
  if (!model.cameraFileName.empty())
    model.document = readCamera(model.cameraDiags, model.cameraFileName);
  const auto &document{model.document};
  const auto fileCamera{document.camera.at(opts.scene.time)};
  const Refuser refuser{document};
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
  // over the observer's frame, 24 mm tall unless the field of view and
  // the focal length together say otherwise below.
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
    options.frameSize = observerFrameSize(options.resolution, 24.0f);
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
    refuseThinLensSettings(opts, fileCamera, refuser);
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
  // The thin lens's field, from the two ways of stating it. Over a body
  // the frame is the body's, so either one states the focal length and
  // both is two statements of one fact. Over the observer's frame,
  // either one alone implies the other on 24 mm, and both together size
  // the frame itself.
  if (!options.lens) {
    const bool wasFovYGiven{opts.camera.fovYDeg.wasGiven ||
                            fileCamera.fovYDeg.has_value()};
    const bool wasFocalLengthGiven{opts.camera.focalLengthMM.wasGiven ||
                                   fileCamera.focalLengthMM.has_value()};
    const float focalLength{
        1e-3f * pick(opts.camera.focalLengthMM, fileCamera.focalLengthMM)};
    if (body && wasFovYGiven && wasFocalLengthGiven) {
      const bool asFlag{opts.camera.focalLengthMM.wasGiven};
      refuser.refuse(
          "focal_length", asFlag,
          smdl::concat(spellSetting("fovy", opts.camera.fovYDeg.wasGiven),
                       " and ", spellSetting("focal_length", asFlag),
                       " are two statements of the thin lens's "
                       "field over a body, whose frame is its own: "
                       "state one"));
    }
    if (wasFocalLengthGiven && !wasFovYGiven) {
      options.fovYDeg =
          2 *
          smdl::degrees(std::atan(0.5f * options.frameSize.y / focalLength));
      SMDL_LOG_INFO("Field of view: ", smdl::Brief(options.fovYDeg, 4),
                    " degrees top to bottom, from a focal length of ",
                    smdl::Brief(1e3f * focalLength, 4), " mm over the ",
                    body ? "body's " : "observer's ",
                    smdl::Brief(1e3f * options.frameSize.y, 4), " mm frame");
    } else if (wasFocalLengthGiven && wasFovYGiven) {
      const float heightMM{2e3f * focalLength *
                           std::tan(smdl::radians(options.fovYDeg / 2))};
      options.frameSize = observerFrameSize(options.resolution, heightMM);
      SMDL_LOG_INFO("Frame: ", smdl::Brief(1e3f * options.frameSize.x, 4),
                    " by ", smdl::Brief(heightMM, 4),
                    " mm, from a focal length of ",
                    smdl::Brief(1e3f * focalLength, 4), " mm spanning ",
                    smdl::Brief(options.fovYDeg, 4), " degrees");
    }
  }
  // The same exclusivity the command line checks, now that the file has
  // had its say: either source can supply either spelling, so only the
  // merged pair can be checked for naming both.
  if (options.fStop > 0 && options.aperture > 0)
    refuser.refuse(
        fileCamera.aperture ? "aperture" : "fstop",
        opts.camera.fStop.wasGiven && opts.camera.aperture.wasGiven,
        "expected at most one of -fstop and -aperture between the command "
        "line and the camera file's 'camera' directive (they are two "
        "spellings of the same quantity)");
  // What a physical sensor needs of the optics, refused here so that it
  // fails before anything slow loads, since under -autolook the camera
  // is built after the scene.
  if (model.sensor && !options.lens && !(options.fStop > 0) &&
      !(options.aperture > 0))
    refuser.refuse("sensor", opts.camera.sensor.wasGiven,
                   "a physical sensor integrates the irradiance over a "
                   "pupil, and a pinhole has none: state 'fstop' or "
                   "'aperture'");
  // The focus: a distance, infinity, or the autofocus, the flag over the
  // file. The autofocus is a measurement of the committed scene, so it
  // leaves the distance to the stage.
  if (opts.camera.shouldAutofocus) {
    model.shouldAutofocus = true;
  } else if (opts.camera.focus.wasGiven) {
    options.focus = opts.camera.focus.value;
    if (fileCamera.shouldAutofocus)
      SMDL_LOG_INFO("Focus: -focus replaces the camera file's 'focus auto'");
  } else if (fileCamera.shouldAutofocus) {
    model.shouldAutofocus = true;
  } else {
    options.focus = fileCamera.focus.value_or(0.0f);
  }
  // The body's condition over the shot, which only a body has.
  if (fileCamera.temperature) {
    if (model.sensor) {
      model.temperature = *fileCamera.temperature;
    } else if (isBodyReplaced) {
      SMDL_LOG_INFO("Sensor: 'temperature' is ignored, since -sensor human "
                    "replaced the body it was stated for");
    } else {
      refuser.refuse("temperature", false,
                     "'temperature' is a physical sensor's condition, and "
                     "this camera's sensor is 'human'");
    }
  }
  // The ISO, which only a body reads out at: the flag over the file,
  // and the meter when neither states a number. A body whose detector
  // states its gain has one speed, which a number would contradict.
  const auto physics{model.sensor ? std::optional<Sensor>(*model.sensor)
                                  : std::optional<Sensor>()};
  {
    const bool wasFlagGiven{opts.camera.iso.wasGiven ||
                            opts.camera.shouldMeterISO};
    const bool wasStated{wasFlagGiven || fileCamera.iso.has_value() ||
                         fileCamera.shouldMeterISO};
    if (wasStated && !model.sensor) {
      if (isBodyReplaced) {
        SMDL_LOG_INFO("Sensor: 'iso' is ignored, since -sensor human "
                      "replaced the body it was stated for");
      } else {
        refuser.refuse("iso", wasFlagGiven,
                       smdl::concat(spellSetting("iso", wasFlagGiven),
                                    " is a physical sensor's setting, and "
                                    "this camera's sensor is 'human'"));
      }
    } else if (model.sensor) {
      if (opts.camera.shouldMeterISO) {
        if (fileCamera.iso)
          SMDL_LOG_INFO("ISO: -iso auto replaces the camera file's 'iso ",
                        smdl::Brief(*fileCamera.iso, 6), "'");
      } else if (opts.camera.iso.wasGiven) {
        model.iso = opts.camera.iso.value;
      } else if (fileCamera.iso) {
        model.iso = *fileCamera.iso;
      }
      if (model.iso && physics->hasFixedGain())
        refuser.refuse(
            "iso", opts.camera.iso.wasGiven,
            smdl::concat(spellSetting("iso", opts.camera.iso.wasGiven),
                         " has no meaning with a fixed gain: the body's "
                         "detector states 'gain', whose saturation speed "
                         "is ISO ",
                         smdl::Brief(physics->fixedGainISO(), 5)));
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
                  describeDetector(*physics));
    SMDL_LOG_INFO("Well: ", describeWell(*physics));
    SMDL_LOG_INFO("ISO: ", describeISO(*physics, model.iso));
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
  // The focus as stated. The autofocus cannot know its distance until
  // the scene is built, so it has no depth of field to report here.
  const float focus{focusDistanceOf(options)};
  const auto focusText{model.shouldAutofocus
                           ? std::string("auto, measured from the scene once "
                                         "it is built")
                           : spellDistance(focus)};
  const auto dofText{[&](float focalLength, float fNumber) {
    return model.shouldAutofocus
               ? std::string()
               : describeDepthOfField(depthOfField(focalLength, fNumber, focus,
                                                   options.frameSize));
  }};
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
             : std::string());
    line("  focus: ", focusText);
    text += dofText(lens.focalLength(), lens.fNumber());
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
    const float focalLength{thinLensFocalLength(options)};
    const float fNumber{options.aperture > 0
                            ? focalLength / (2 * options.aperture)
                            : options.fStop};
    line("lens: the thin lens");
    line("  field ", smdl::Brief(options.fovYDeg, 4),
         " degrees top to bottom, a focal length of ",
         smdl::Brief(1e3f * focalLength, 5), " mm over the frame");
    if (fNumber > 0) {
      line("  f/", smdl::Brief(fNumber, 4), ", focus: ", focusText);
      text += dofText(focalLength, fNumber);
    } else {
      line("  a pinhole, so everything is in focus");
    }
  }
  if (model.sensor) {
    const auto &sensor{*model.sensor};
    line("sensor: ",
         sensor.name.empty() ? std::string("(unnamed)")
                             : smdl::concat(smdl::Quoted(sensor.name)),
         " from ", smdl::QuotedPath(model.sensorFileName));
    const Sensor physics{sensor};
    line("  response: ", describeResponse(sensor.response));
    line("  detector: ", sensor.hasDetectorBlock ? "" : "generic, ",
         describeDetector(physics));
    line("  well: ", describeWell(physics));
    line("  iso: ", describeISO(physics, model.iso));
    line("  temperature: ", smdl::Brief(model.temperature, 4), " C");
  } else {
    line("sensor: the observer, so no bands and no readout");
  }
  return text;
}
