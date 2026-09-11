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

// A refusal of a setting the camera file stated, once the lens and the
// body are known: pointed at the key with a caret, reading as the
// compiler's own located errors do, the file, line, and column ahead of
// the message and the excerpt beneath it. A setting stated only in a
// 'motion' key has no location of its own, and is refused plainly.
[[noreturn]] void refuse(const CameraDocument &document, std::string_view key,
                         const std::string &message) {
  if (auto itr{document.keyLocs.find(std::string(key))};
      itr != document.keyLocs.end())
    throw smdl::Error(
        smdl::concat(LayoutDiagnostics::where(itr->second), ": ", message),
        "\n" + LayoutDiagnostics::excerpt(itr->second));
  throw smdl::Error(message);
}

// With a lens the frame is the body and the prescription together, and
// every setting the thin lens uses to stand in for a real one is either
// meaningless or emergent. Naming one anyway is a mistake worth
// reporting rather than a value to quietly drop.
void refuseThinLensSettings(const CameraDocument &document,
                            const CameraSettings &fileCamera) {
  struct Refusal final {
    const char *key;
    bool wasStated;
    const char *why;
  };
  const Refusal refusals[]{
      {"fovy", fileCamera.fovYDeg.has_value(),
       "the field of view is the frame and the glass together, and the log "
       "reports it"},
      {"focal_length", fileCamera.focalLengthMM.has_value(),
       "the focal length is the prescription's own, and the log reports it"},
      {"aperture", fileCamera.aperture.has_value(),
       "the aperture is the lens's own stop, which 'fstop' still narrows"},
      {"distortion_k1", fileCamera.distortionK1.has_value(),
       "the distortion is whatever the surfaces do"},
      {"distortion_k2", fileCamera.distortionK2.has_value(),
       "the distortion is whatever the surfaces do"},
      {"distortion_fit", fileCamera.shouldFitDistortion.has_value(),
       "there is no distortion polynomial to refit"},
      {"vignetting", fileCamera.vignetting.has_value(),
       "the cos^4 falloff comes out of the pupil integral, and is not "
       "optional there"},
      {"cat_eye", fileCamera.catEye.has_value(),
       "the barrel vignette is whatever the clear apertures do"},
      {"cat_eye_radius", fileCamera.catEyeRadius.has_value(),
       "the barrel vignette is whatever the clear apertures do"},
  };
  for (const auto &refusal : refusals)
    if (refusal.wasStated)
      refuse(document, refusal.key,
             smdl::concat("'", refusal.key,
                          "' has no meaning with a lens: ", refusal.why));
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

// One line on how the body sees color under its white balance: the fit
// over the training reflectances, its index, and the multipliers; or
// why the develop takes the bands as they are. An `auto` white balance
// is read off the frame once it is rendered, so the fit here is the one
// it starts from, under D65.
[[nodiscard]] std::string describeColor(const Sensor &sensor,
                                        const WhiteBalance &whiteBalance) {
  const auto &response{sensor.settings().response};
  const auto balance{
      smdl::concat("white balance ", whiteBalanceName(whiteBalance),
                   whiteBalance.kind == WhiteBalanceKind::AUTO
                       ? ", the frame's gray world, fitted here under D65"
                       : "")};
  const auto rgb{response.rgbBands()};
  if (!rgb)
    return smdl::concat(response.bands.size(),
                        " band(s) cannot carry color, so the develop is "
                        "gray; ",
                        balance);
  const auto &r{response.bands[(*rgb)[0]].name};
  const auto &g{response.bands[(*rgb)[1]].name};
  const auto &b{response.bands[(*rgb)[2]].name};
  const auto fit{sensor.fitColor(*rgb, whiteBalanceSpectrum(whiteBalance))};
  if (fit.isSingular)
    return smdl::concat(r, ", ", g, ", and ", b,
                        " respond too much alike to tell colors apart, so "
                        "the develop is false color; ",
                        balance);
  return smdl::concat(
      r, ", ", g, ", ", b, " fitted over ", trainingReflectances().size(),
      " training reflectances to a mean of ", smdl::Brief(fit.meanDeltaE00, 3),
      " dE00 and a largest of ", smdl::Brief(fit.maxDeltaE00, 3),
      ", an index of ", smdl::Brief(fit.index(), 3), " over this set",
      fit.isFaithful()
          ? std::string()
          : smdl::concat(", past ", smdl::Brief(FAITHFUL_FIT_DELTA_E00, 3),
                         " dE00, so the develop is false color"),
      "; ", balance, ", ", r, " ", smdl::Brief(fit.multipliers.x, 4), " and ",
      b, " ", smdl::Brief(fit.multipliers.z, 4), " against ", g);
}

// The thin lens fitted to a lens, and how closely it fits, for the log
// and the report: its focal length, stop, and distortion, and where its
// chief rays land against the lens's in the picture's pixels.
[[nodiscard]] std::string describeFit(const LensApproximation &fit,
                                      const CameraOptions &options) {
  const float focalLength{thinLensFocalLength(fit.options)};
  const float pitch{options.frameSize.y / float(options.resolution.y)};
  return smdl::concat(
      "a focal length of ", smdl::Brief(1e3f * focalLength, 5), " mm at f/",
      smdl::Brief(focalLength / (2 * fit.options.aperture), 4),
      fit.doesFold
          ? std::string(" with no distortion, since the fit folds")
          : smdl::concat(", distortion ",
                         smdl::Brief(fit.options.distortionK1, 3), " and ",
                         smdl::Brief(fit.options.distortionK2, 3)),
      ", and no vignetting; its chief rays land within ",
      smdl::Brief(fit.maxChiefRayError / pitch, 3), " pixels of the lens's at ",
      fit.numFittedRadii, " radii",
      fit.numDroppedRadii > 0 ? smdl::concat(", ", fit.numDroppedRadii,
                                             " past the image circle dropped")
                              : std::string());
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
  // The body the camera file names, else the observer. Under -ideal the
  // observer stands in for the body on its frame and pixels, so the
  // framing never changes between the preview and the render.
  model.isPreview = opts.camera.isIdeal;
  auto body{std::optional<SensorSettings>()};
  if (fileCamera.sensor && *fileCamera.sensor != SENSOR_HUMAN) {
    model.sensorFileName =
        resolveSensorFileName(*fileCamera.sensor, model.cameraFileName);
    body = readSensor(model.sensorFileName).sensor;
    if (model.isPreview) {
      model.previewedSensor = body;
      SMDL_LOG_INFO("Sensor: -ideal previews ",
                    body->name.empty()
                        ? smdl::concat(smdl::QuotedPath(model.sensorFileName))
                        : smdl::concat(smdl::Quoted(body->name)),
                    " through the observer, on its frame and pixels, and "
                    "exposes the picture as it would");
    } else {
      model.sensor = body;
    }
  }
  // The body the shot is exposed for: the physical sensor, or the one the
  // preview stands in for.
  const auto &shotBody{model.sensor ? model.sensor : model.previewedSensor};
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
  // The picture the frame was sized for, which -resolution-scale renders
  // fewer pixels of: a smaller picture of the same frame. A body renders
  // exactly its own pixels.
  const int2 picture{options.resolution};
  if (opts.image.resolutionScale.wasGiven) {
    if (model.sensor)
      throw smdl::Error("-resolution-scale renders a smaller picture of the "
                        "frame, and a body renders exactly its own pixels: "
                        "preview it with -ideal");
    const float scale{opts.image.resolutionScale.value};
    options.resolution =
        int2(std::max(1, int(std::lround(scale * float(picture.x)))),
             std::max(1, int(std::lround(scale * float(picture.y)))));
    SMDL_LOG_INFO("Resolution: -resolution-scale ", smdl::Brief(scale, 4),
                  " renders ", options.resolution.x, " by ",
                  options.resolution.y, " of the frame's ", picture.x, " by ",
                  picture.y, " pixels");
  }
  options.filmQuantity =
      model.sensor ? FilmQuantity::IRRADIANCE : FilmQuantity::RADIANCE;
  // The two clocks. Which instant to photograph is the command line's
  // alone, so one camera file renders every frame of a shot; how long
  // the shutter stays open is the camera's; how long the readout takes
  // to sweep the frame and which way is the body's, which the camera
  // file overrides.
  gRenderShutter.time = opts.scene.time;
  gRenderShutter.exposure = fileCamera.shutter.value_or(0.0f);
  gRenderShutter.readout = fileCamera.readout ? *fileCamera.readout
                           : body             ? body->readout
                                              : 0.0f;
  settleReadoutLines(fileCamera.readoutDirection ? *fileCamera.readoutDirection
                     : body                      ? body->readoutDirection
                                                 : ReadoutDirection::DOWN,
                     options.resolution);
  // The lens the camera file names, else the thin lens. Under -ideal
  // the prescription stays in the options for the thin lens fitted to
  // it.
  if (fileCamera.lens && *fileCamera.lens != LENS_IDEAL) {
    model.lensFileName =
        resolveLensFileName(*fileCamera.lens, model.cameraFileName);
    options.lens = readLens(model.lensFileName).lens;
    refuseThinLensSettings(document, fileCamera);
    if (model.isPreview)
      SMDL_LOG_INFO("Lens: -ideal previews ",
                    options.lens->name.empty()
                        ? smdl::concat(smdl::QuotedPath(model.lensFileName))
                        : smdl::concat(smdl::Quoted(options.lens->name)),
                    " through the thin lens fitted to it");
  }
  // The camera: the defaults in `CameraOptions`, whatever the camera
  // file's 'camera' directive named over them, and the framing flags
  // over both. A framing flag that was not given must not override the
  // file, so what decides is the occurrence count rather than the value.
  options.lookFrom = pick(opts.camera.lookFrom, fileCamera.lookFrom);
  options.lookTo = pick(opts.camera.lookTo, fileCamera.lookTo);
  options.lookUp = pick(opts.camera.lookUp, fileCamera.lookUp);
  options.fovYDeg = fileCamera.fovYDeg.value_or(options.fovYDeg);
  options.fStop = fileCamera.fStop.value_or(options.fStop);
  options.aperture = fileCamera.aperture.value_or(options.aperture);
  options.focus = fileCamera.focus.value_or(options.focus);
  options.blades = fileCamera.blades.value_or(options.blades);
  options.bladeAngleDeg =
      fileCamera.bladeAngleDeg.value_or(options.bladeAngleDeg);
  options.distortionK1 = fileCamera.distortionK1.value_or(options.distortionK1);
  options.distortionK2 = fileCamera.distortionK2.value_or(options.distortionK2);
  options.shouldFitDistortion =
      fileCamera.shouldFitDistortion.value_or(options.shouldFitDistortion);
  options.vignetting = fileCamera.vignetting.value_or(options.vignetting);
  options.catEye = fileCamera.catEye.value_or(options.catEye);
  options.catEyeRadius = fileCamera.catEyeRadius.value_or(options.catEyeRadius);
  options.noLOD = opts.render.sampling.noLOD;
  // The autofocus is a measurement of the committed scene, so it leaves
  // the distance to the stage.
  model.shouldAutofocus = fileCamera.shouldAutofocus;
  // The thin lens's field, from the two ways of stating it. The focal
  // length alone implies the field of view over the frame. Both together
  // are two statements of one fact over a body, whose frame is its own,
  // and size the frame itself over the observer's.
  if (!options.lens && fileCamera.focalLengthMM) {
    const float focalLength{1e-3f * *fileCamera.focalLengthMM};
    if (!fileCamera.fovYDeg) {
      options.fovYDeg =
          2 *
          smdl::degrees(std::atan(0.5f * options.frameSize.y / focalLength));
      SMDL_LOG_INFO("Field of view: ", smdl::Brief(options.fovYDeg, 4),
                    " degrees top to bottom, from a focal length of ",
                    smdl::Brief(1e3f * focalLength, 4), " mm over the ",
                    body ? "body's " : "observer's ",
                    smdl::Brief(1e3f * options.frameSize.y, 4), " mm frame");
    } else if (body) {
      refuse(document, "focal_length",
             "'fovy' and 'focal_length' are two statements of the thin "
             "lens's field over a body, whose frame is its own: state one");
    } else {
      const float heightMM{2e3f * focalLength *
                           std::tan(smdl::radians(options.fovYDeg / 2))};
      options.frameSize = observerFrameSize(picture, heightMM);
      SMDL_LOG_INFO("Frame: ", smdl::Brief(1e3f * options.frameSize.x, 4),
                    " by ", smdl::Brief(heightMM, 4),
                    " mm, from a focal length of ",
                    smdl::Brief(1e3f * focalLength, 4), " mm spanning ",
                    smdl::Brief(options.fovYDeg, 4), " degrees");
    }
  }
  // Either spelling of the aperture may come from the block or from a key
  // of its motion, so only the settings at shutter open can be checked
  // for naming both.
  if (options.fStop > 0 && options.aperture > 0)
    refuse(document, "aperture",
           "expected at most one of 'fstop' and 'aperture' (they are two "
           "spellings of the same quantity)");
  // What a physical sensor needs of the optics, refused here so that it
  // fails before anything slow loads, since under -autolook the camera
  // is built after the scene.
  if (shotBody && !options.lens && !(options.fStop > 0) &&
      !(options.aperture > 0))
    refuse(document, "sensor",
           "a physical sensor integrates the irradiance over a pupil, and a "
           "pinhole has none: state 'fstop' or 'aperture'");
  // The body's condition over the shot, which only a body has, and which
  // the preview, drawing no noise, has no use for.
  if (fileCamera.temperature) {
    if (model.sensor) {
      model.temperature = *fileCamera.temperature;
    } else if (model.previewedSensor) {
      SMDL_LOG_INFO("Sensor: 'temperature' is ignored, since -ideal "
                    "previews the body through the observer");
    } else {
      refuse(document, "temperature",
             "'temperature' is a physical sensor's condition, and this "
             "camera's sensor is 'human'");
    }
  }
  // The ISO, which only a body reads out at: -iso over the file, and the
  // meter when neither states a number. A body whose detector states its
  // gain has one speed, which a number would contradict.
  const auto physics{shotBody ? std::optional<Sensor>(*shotBody)
                              : std::optional<Sensor>()};
  if (!shotBody) {
    if (opts.camera.iso.wasGiven || opts.camera.shouldMeterISO)
      throw smdl::Error("-iso is a physical sensor's setting, and this "
                        "camera's sensor is 'human'");
    if (fileCamera.iso || fileCamera.shouldMeterISO)
      refuse(document, "iso",
             "'iso' is a physical sensor's setting, and this camera's "
             "sensor is 'human'");
  } else {
    if (opts.camera.iso.wasGiven)
      model.iso = opts.camera.iso.value;
    else if (!opts.camera.shouldMeterISO)
      model.iso = fileCamera.iso;
    if (model.iso && physics->hasFixedGain()) {
      const auto why{smdl::concat(
          " has no meaning with a fixed gain: the body's detector states "
          "'gain', whose saturation speed is ISO ",
          smdl::Brief(physics->fixedGainISO(), 5))};
      if (opts.camera.iso.wasGiven) throw smdl::Error("-iso" + why);
      refuse(document, "iso", "'iso'" + why);
    }
  }
  // The white balance, which only a body's develop has: -white-balance
  // over the file.
  if (model.sensor) {
    model.whiteBalance =
        pick(opts.camera.whiteBalance, fileCamera.whiteBalance);
  } else if (opts.camera.whiteBalance.wasGiven || fileCamera.whiteBalance) {
    if (model.previewedSensor)
      SMDL_LOG_INFO("Sensor: 'white_balance' is ignored, since -ideal "
                    "previews the body through the observer");
    else if (opts.camera.whiteBalance.wasGiven)
      throw smdl::Error("-white-balance is a physical sensor's setting, and "
                        "this camera's sensor is 'human'");
    else
      refuse(document, "white_balance",
             "'white_balance' is a physical sensor's setting, and this "
             "camera's sensor is 'human'");
  }
  // What a readout needs, refused here for the same reason.
  if (!opts.image.outputDN.empty()) {
    if (model.previewedSensor)
      throw smdl::Error("-output-dn reads a body out, and -ideal previews it "
                        "through the observer");
    if (!model.sensor)
      throw smdl::Error("-output-dn reads a body out, and this camera's "
                        "sensor is 'human': name a '.sensor' file with "
                        "'sensor' in the camera file");
    if (!gRenderShutter.hasExposure())
      throw smdl::Error("-output-dn needs an exposure: state 'shutter'");
  }
  // What the observer's develop does and a body's does not.
  if (model.sensor) {
    if (opts.image.tonemap.isNight)
      throw smdl::Error("-tonemap night models the observer's eyes at the "
                        "scene's own luminance, and a physical sensor's "
                        "film holds the irradiance at the sensor: render "
                        "the scene through a camera whose sensor is "
                        "'human'");
    if (opts.image.rgbPolicy.shouldForceFalseColor)
      throw smdl::Error(smdl::concat(
          opts.image.rgbPolicy.falseColorWaves.empty() ? "-false-color"
                                                       : "-rgb-wavelengths",
          " maps the spectral film's bands to RGB, and a physical sensor "
          "develops its own: map the spectrum through a camera whose "
          "sensor is 'human'"));
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
    SMDL_LOG_INFO("Color: ", describeColor(*physics, model.whiteBalance));
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
  line("camera: ",
       model.cameraFileName.empty()
           ? std::string("the defaults and the command line")
           : smdl::concat(smdl::QuotedPath(model.cameraFileName)),
       model.isPreview ? ", previewed with -ideal" : "");
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
           : std::string(", global"),
       model.sensor && !gRenderShutter.hasExposure()
           ? "; a physical sensor cannot render with a shut shutter: state "
             "'shutter'"
           : "");
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
    if (model.shouldApproximateLens())
      line("  preview: the thin lens fitted to it, ",
           describeFit(approximateLens(options), options));
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
    line("  color: ", describeColor(physics, model.whiteBalance));
    line("  temperature: ", smdl::Brief(model.temperature, 4), " C");
  } else if (model.previewedSensor) {
    const auto &sensor{*model.previewedSensor};
    line("sensor: the observer, previewing ",
         sensor.name.empty() ? std::string("(unnamed)")
                             : smdl::concat(smdl::Quoted(sensor.name)),
         " from ", smdl::QuotedPath(model.sensorFileName),
         " and exposing the picture as its develop would");
    line("  iso: ", describeISO(Sensor{sensor}, model.iso));
  } else {
    line("sensor: the observer, so no bands and no readout");
  }
  return text;
}

Camera buildCamera(CameraModel &model) {
  auto options{model.options};
  if (model.shouldApproximateLens()) {
    const auto fit{approximateLens(options)};
    const auto lensName{options.lens->name.empty()
                            ? smdl::concat(smdl::QuotedPath(model.lensFileName))
                            : smdl::concat(smdl::Quoted(options.lens->name))};
    SMDL_LOG_INFO("Lens: the thin lens fitted to ", lensName, " is ",
                  describeFit(fit, options));
    const float pitch{options.frameSize.y / float(options.resolution.y)};
    if (fit.doesFold)
      SMDL_LOG_WARN("Lens: the projection fitted to ", lensName,
                    " folds over the frame, so the thin lens is the pinhole "
                    "at its paraxial focal length, and the corners look out "
                    "elsewhere than the lens's do");
    else if (fit.maxChiefRayError > pitch)
      SMDL_LOG_WARN("Lens: the thin lens misses ", lensName, " by up to ",
                    smdl::Brief(fit.maxChiefRayError / pitch, 3),
                    " pixels, so what the preview frames near the edges "
                    "sits elsewhere in the render");
    options = fit.options;
  }
  if (model.previewedSensor) {
    // What the body's optics would put on the middle of the frame per unit
    // of scene radiance, where the preview, which has no vignetting of its
    // own to lose, reads the radiance itself. The thin lens images at
    // `z = f s / (s - f)` behind an aperture of radius `R`, and its flat
    // pupil loses the `cos^4` of its own cone: `pi R^2 / (R^2 + z^2)`. A
    // lens's fitted focal length is already the scale its film sits at,
    // the focus included, so over its entrance pupil it puts `pi R^2 /
    // f^2` there, whatever its pupil magnification, taking it for free of
    // pupil aberration as the observer's normalization does.
    const double f{thinLensFocalLength(options)};
    const double R{options.aperture > 0 ? double(options.aperture)
                                        : f / (2 * double(options.fStop))};
    if (model.shouldApproximateLens()) {
      model.previewIrradianceScale = PI * R * R / (f * f);
    } else {
      const double s{focusDistanceOf(options)};
      if (!(s > f))
        throw smdl::Error(smdl::concat("the thin lens cannot focus at ", s,
                                       " scene units, inside its focal "
                                       "length of ",
                                       f));
      const double z{std::isinf(s) ? f : f * s / (s - f)};
      model.previewIrradianceScale = PI * R * R / (R * R + z * z);
    }
  }
  return Camera{options};
}

void refuseUnrenderable(const CameraModel &model) {
  if (gRenderShutter.hasExposure()) return;
  if (model.sensor)
    refuse(model.document, "sensor",
           "a physical sensor counts the electrons of an exposure, and the "
           "shutter is shut: state 'shutter' in the camera file");
  if (model.previewedSensor)
    refuse(model.document, "sensor",
           "-ideal exposes the picture as the body would, and the shutter "
           "is shut: state 'shutter' in the camera file");
}
