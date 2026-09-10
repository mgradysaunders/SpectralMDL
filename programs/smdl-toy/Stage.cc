#include <optional>
#include <string>
#include <utility>

#include "smdl/Support/Error.h"
#include "smdl/Support/Filesystem.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Profiler.h"
#include "smdl/Support/Strings.h"

#include "../CommandLine.h"

#include "Layout/CameraFile.h"
#include "Layout/LayoutTables.h"
#include "Layout/LensFile.h"
#include "Options.h"
#include "Render/Autolook.h"
#include "Resume.h"
#include "Scene/Scene.h"
#include "Stage.h"

// The material a scene falls back to when it has none of its own: a
// plain 20 percent Lambertian, so a layout can be looked at before any
// material has been written for it. Deliberately the dullest thing that
// can be shaded, so nothing about it can be mistaken for a shading
// decision the scene has not made yet.
constexpr const char *DEFAULT_MATERIAL_SOURCE = R"(#smdl
import ::df::*;

export material default_object() = material(
  surface: material_surface(
    scattering: df::diffuse_reflection_bsdf(tint: color(0.2))));

// Half the default gray, so that an unshaded object on the -ground plane
// reads against it instead of dissolving into it.
export material default_ground() = material(
  surface: material_surface(
    scattering: df::diffuse_reflection_bsdf(tint: color(0.1))));
)";

// What the compiler calls the module above, which is what qualifies the
// names of the two materials in it.
constexpr const char *DEFAULT_MATERIAL_MODULE = "::smdl_toy_default";

// With a lens the frame is the sensor and the prescription together, and
// every setting the thin lens uses to stand in for a real one is either
// meaningless or now emergent. Naming one anyway is a mistake worth
// reporting rather than a value to quietly drop, and this is the one
// place both sources have had their say: an explicit flag and whatever
// the camera file resolved to at shutter open.
void refuseThinLensSettings(const Options &opts,
                            const CameraSettings &fileCamera, bool hasLens) {
  struct Refusal final {
    const char *name;
    bool wasStated;
    const char *why;
  };
  const Refusal refusals[]{
      {"fovy", opts.camera.fovYDeg.wasGiven || fileCamera.fovYDeg.has_value(),
       "the field of view is what the sensor and the prescription come to "
       "between them; state the sensor with 'sensor'"},
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
  if (hasLens) {
    for (const auto &refusal : refusals)
      if (refusal.wasStated)
        throw smdl::Error(smdl::concat(
            "'", refusal.name, "' has no meaning with a lens: ", refusal.why));
  } else if (opts.camera.sensorMM.wasGiven || fileCamera.sensorMM) {
    throw smdl::Error("'sensor' needs a lens to mean anything: without one "
                      "the frame is 'fovy' and the sensor has no size");
  } else if (opts.camera.shouldNormalizeLensExposure.wasGiven) {
    throw smdl::Error("'-normalize-lens-exposure' needs a lens to mean "
                      "anything: the thin lens already holds its brightness, "
                      "'fstop' there buying depth of field and costing no "
                      "light");
  }
}

Frame resolveFrame(const Options &opts) {
  // Everything the command line asks for, lowered into one layout: the
  // positional argument first, then each -mesh. Either may name a mesh
  // file or a '.layout'. Read before the camera because a layout may
  // describe one; it is only a text parse, so fail-fast ordering costs
  // nothing. Tilde is expanded here because a search path is typed by a
  // person.
  auto assetSearchPath{AssetSearchPath()};
  for (const auto &directory : opts.scene.assetDirs)
    assetSearchPath.push_back(smdl::makePathCanonical(directory));
  // The camera file first: '-camera' if it was given, else the '.camera'
  // beside the layout. It carries the clock, and the clock has to be
  // settled before the scene is read, since what a layout's motion means
  // is the transform at these two instants.
  const auto cameraFileName{
      resolveCameraFileName(opts.camera.file, opts.scene.inputSceneFile)};
  const auto cameraDocument{
      cameraFileName.empty() ? CameraDocument() : readCamera(cameraFileName)};
  // The two clocks. Which instant to photograph is the command line's
  // alone, so one camera file renders every frame of a shot; how long
  // the shutter stays open is a fact about the camera, which the file
  // may state and '-shutter' overrides.
  gRenderShutter.time = opts.scene.time;
  gRenderShutter.length =
      pick(opts.camera.shutter, cameraDocument.camera.shutter);
  // The file's own settings resolved at shutter open, which is where
  // everything but the framing is read: the renderer varies the framing
  // within one shutter and holds the rest. Resolved here, ahead of the
  // scene, because the lens it may name is read here too and a typo in
  // either should not wait on a layout.
  const auto fileCamera{cameraDocument.camera.at(gRenderShutter.time)};
  const auto lensFileName{
      resolveLensFileName(opts.camera.lens.value, cameraFileName,
                          fileCamera.lens ? *fileCamera.lens : std::string())};
  refuseThinLensSettings(opts, fileCamera, !lensFileName.empty());
  auto lensPrescription{std::optional<LensPrescription>()};
  if (!lensFileName.empty()) lensPrescription = readLens(lensFileName).lens;
  // What every 'motion' track is evaluated at. A shut shutter lands both
  // samples on one instant, so every track lowers static and the render
  // takes the path it takes with no motion at all.
  const MotionSampling sampling{gRenderShutter.time,
                                gRenderShutter.secondsAt(1.0f)};
  auto profReadLayout{smdl::profilerEntryBegin("Read layout")};
  auto layout{resolveLayoutArgument(opts.scene.inputSceneFile, assetSearchPath,
                                    sampling)};
  for (const auto &fileName : opts.scene.inputMeshFiles) {
    auto more{resolveLayoutArgument(fileName, assetSearchPath, sampling)};
    layout.items.insert(layout.items.end(), more.items.begin(),
                        more.items.end());
    layout.lights.insert(layout.lights.end(), more.lights.begin(),
                         more.lights.end());
    layout.entryMaterialAliases.insert(more.entryMaterialAliases.begin(),
                                       more.entryMaterialAliases.end());
  }
  smdl::profilerEntryEnd(profReadLayout);
  // The camera, merged from three sources in increasing order of priority:
  // the defaults in `CameraOptions`, whatever the camera file's 'camera'
  // directive named, and whatever the command line explicitly gave. A flag
  // that was not given must not override the file, so what decides is the
  // occurrence count rather than the value.
  auto cameraOptions{CameraOptions{}};
  cameraOptions.resolution = opts.image.resolution;
  cameraOptions.lens = std::move(lensPrescription);
  cameraOptions.sensorMM = pick(opts.camera.sensorMM, fileCamera.sensorMM);
  cameraOptions.lookFrom = pick(opts.camera.lookFrom, fileCamera.lookFrom);
  cameraOptions.lookTo = pick(opts.camera.lookTo, fileCamera.lookTo);
  cameraOptions.lookUp = pick(opts.camera.lookUp, fileCamera.lookUp);
  cameraOptions.fovYDeg = pick(opts.camera.fovYDeg, fileCamera.fovYDeg);
  cameraOptions.fStop = pick(opts.camera.fStop, fileCamera.fStop);
  cameraOptions.aperture = pick(opts.camera.aperture, fileCamera.aperture);
  cameraOptions.focus = pick(opts.camera.focus, fileCamera.focus);
  cameraOptions.blades = pick(opts.camera.blades, fileCamera.blades);
  cameraOptions.bladeAngleDeg =
      pick(opts.camera.bladeAngleDeg, fileCamera.bladeAngleDeg);
  cameraOptions.distortionK1 =
      pick(opts.camera.distortionK1, fileCamera.distortionK1);
  cameraOptions.distortionK2 =
      pick(opts.camera.distortionK2, fileCamera.distortionK2);
  cameraOptions.shouldFitDistortion =
      pick(opts.camera.shouldFitDistortion, fileCamera.shouldFitDistortion);
  cameraOptions.shouldNormalizeLensExposure =
      opts.camera.shouldNormalizeLensExposure.value;
  cameraOptions.vignetting =
      pick(opts.camera.vignetting, fileCamera.vignetting);
  cameraOptions.catEye = pick(opts.camera.catEye, fileCamera.catEye);
  cameraOptions.catEyeRadius =
      pick(opts.camera.catEyeRadius, fileCamera.catEyeRadius);
  cameraOptions.noLOD = opts.render.sampling.noLOD;
  // The same exclusivity the command line checks above, now that the file
  // has had its say: either source can supply either spelling, so only the
  // merged pair can be checked for naming both.
  if (cameraOptions.fStop > 0 && cameraOptions.aperture > 0)
    throw smdl::Error("expected at most one of -fstop and -aperture between "
                      "the command line and the camera file's 'camera' "
                      "directive (they are two spellings of the same "
                      "quantity)");
  // The camera's framing at shutter shut. The keys are absolute readings
  // of the clock, so a flag that replaces the framing drops the track
  // rather than moving a camera the file never described.
  if (!cameraDocument.camera.motion.empty()) {
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
    } else if (!gRenderShutter.isOpen()) {
      SMDL_LOG_INFO("Camera motion: the shutter is shut, so the camera "
                    "holds its framing at ",
                    gRenderShutter.time, " s");
    } else {
      const auto shutCamera{cameraDocument.camera.at(shutSeconds)};
      cameraOptions.hasMotion = true;
      cameraOptions.lookFromShut =
          pick(opts.camera.lookFrom, shutCamera.lookFrom);
      cameraOptions.lookToShut = pick(opts.camera.lookTo, shutCamera.lookTo);
      cameraOptions.lookUpShut = pick(opts.camera.lookUp, shutCamera.lookUp);
    }
    // What the shutter cannot carry: a lens setting the track varies
    // over the shutter is read once, at open, and held.
    if (gRenderShutter.isOpen()) {
      if (const auto held{cameraDocument.camera.heldOverShutter(
              gRenderShutter.time, shutSeconds)};
          !held.empty()) {
        auto names{std::string()};
        for (const auto &name : held)
          names += (names.empty() ? "" : ", ") + std::string(name);
        SMDL_LOG_INFO("Camera motion: ", names,
                      " vary over the shutter, which only the framing does; "
                      "they hold the value at shutter open");
      }
      if (cameraDocument.camera.hasKeyBetween(gRenderShutter.time, shutSeconds))
        SMDL_LOG_INFO("Camera motion: a key sits inside the shutter, so the "
                      "camera moves along the chord of its two ends");
    }
  }
  // What actually moved. A shut shutter needs no clearing: both samples
  // land on one instant, so every track lowered to one key already.
  {
    size_t numMovingItems{};
    size_t numMovingLights{};
    for (const auto &item : layout.items)
      numMovingItems += item.objectToWorldShut || !item.batchXfsShut.empty();
    for (const auto &light : layout.lights)
      numMovingLights += light.lightToWorldShut.has_value();
    if (numMovingItems + numMovingLights > 0)
      SMDL_LOG_INFO("Instance motion: ", numMovingItems, " placement(s) and ",
                    numMovingLights, " light(s) move over the shutter");
  }
  // Under -autolook the position comes from measuring the committed scene,
  // so construction (with the lens validation and the summary it logs)
  // waits until the solve below. Every other path keeps constructing
  // here, before anything slow loads, so a lens typo still fails fast.
  auto camera{std::optional<Camera>()};
  if (!opts.camera.autolook.isEnabled) camera.emplace(cameraOptions);
  const auto resolution{cameraOptions.resolution};
  const auto numPixelsX{size_t(resolution.x)};
  const auto numPixelsY{size_t(resolution.y)};
  const auto spp{size_t(opts.render.sampling.spp)};
  // The pixel window to render, the whole frame unless -crop-window
  // narrows it.
  int4 window{0, 0, resolution.x, resolution.y};
  if (opts.image.cropWindow.wasGiven) {
    window = opts.image.cropWindow.value;
    if (!(0 <= window[0] && window[0] < window[2] &&
          window[2] <= resolution.x && 0 <= window[1] &&
          window[1] < window[3] && window[3] <= resolution.y))
      throw smdl::Error(
          smdl::concat("-crop-window ", spellVector(window),
                       " is not a non-empty sub-rectangle of -resolution ",
                       resolution.x, ",", resolution.y));
  }
  const auto numWindowPixels{size_t(window[2] - window[0]) *
                             size_t(window[3] - window[1])};
  auto frame{Frame{}};
  frame.layout = std::move(layout);
  frame.cameraOptions = cameraOptions;
  frame.camera = std::move(camera);
  frame.resolution = resolution;
  frame.numPixelsX = numPixelsX;
  frame.numPixelsY = numPixelsY;
  frame.spp = spp;
  frame.window = window;
  frame.numWindowPixels = numWindowPixels;
  return frame;
}
ResolvedGrid resolveWavelengthGrid(const Options &opts, const Frame &frame,
                                   const ResumedSequence &resumed) {
  // The wavelength grid, in priority order: explicit '-wavelengths',
  // '-wavelength-range' uniform bands (endpoint-inclusive), or, when
  // resuming with no grid flags at all, the grid recorded in the resumed
  // file, so a resumed render needs no grid retyping. The band count
  // seeds every 'Color' constructed from here on.
  const bool shouldAdoptResumedGrid{!opts.render.grid.wasGiven &&
                                    resumed.wasLoaded};
  auto gridSpec{opts.render.grid.explicitWavelengths};
  if (shouldAdoptResumedGrid) {
    if (resumed.info.wavelengths.empty())
      throw smdl::Error(
          "cannot resume: the file carries no wavelengths to adopt, give "
          "the grid explicitly with -wavelength-range or -wavelengths");
    gridSpec = resumed.info.wavelengths;
  }
  if (gridSpec.empty()) {
    const auto &range{opts.render.grid.range};
    gridSpec.resize(size_t(range.bandCount));
    for (size_t i = 0; i < gridSpec.size(); i++) {
      const float t{float(i) / float(gridSpec.size() - 1)};
      gridSpec[i] = (1 - t) * range.range.x + t * range.range.y;
    }
  }
  // The band count has to land before the first `Color` is built, since
  // that is what sizes it.
  gRenderGrid.reset(smdl::Span<const float>(gridSpec.data(), gridSpec.size()),
                    opts.render.grid.shouldJitter);
  if (opts.render.grid.shouldJitter && gRenderGrid.bandEdges.empty())
    SMDL_LOG_WARN("-wavelength-jitter needs at least 2 bands to have a "
                  "band width to jitter within, so it does nothing here");
  const auto wavelengths{
      Color(smdl::Span<const float>(gridSpec.data(), gridSpec.size()))};
  if (resumed.wasLoaded) {
    if (resumed.film.getNumBands() != wavelengths.size())
      throw smdl::Error(smdl::concat(
          "cannot resume: the file has ", resumed.film.getNumBands(),
          " bands against the renderer's ", wavelengths.size()));
    for (size_t i = 0; i < wavelengths.size(); i++)
      if (i >= resumed.info.wavelengths.size() ||
          !(std::abs(resumed.info.wavelengths[i] - wavelengths[i]) < 0.5f))
        throw smdl::Error(
            "cannot resume: the wavelength grid does not match the "
            "renderer's");
  }
  if (opts.render.grid.wasGiven || shouldAdoptResumedGrid)
    SMDL_LOG_INFO(
        "Wavelength grid: ", wavelengths.size(),
        shouldAdoptResumedGrid ? " bands adopted from the resumed file, "
                               : " bands, ",
        wavelengths[0], "-", wavelengths[wavelengths.size() - 1], " nm");
  // The spectral extent the render actually reaches, which the jitter
  // widens to the outermost band edges.
  const auto &bandEdges{gRenderGrid.bandEdges};
  const float gridLower{bandEdges.empty() ? wavelengths[0] : bandEdges.front()};
  const float gridUpper{bandEdges.empty() ? wavelengths[wavelengths.size() - 1]
                                          : bandEdges.back()};
  if (!bandEdges.empty())
    SMDL_LOG_INFO("Wavelength jitter: bands tile ", gridLower, "-", gridUpper,
                  " nm, each holding its own mean");
  if (wavelengths.size() > 256)
    SMDL_LOG_WARN(wavelengths.size(),
                  " bands: JIT compile time and per-sample cost both grow "
                  "with the band count, expect a slow start and a slow "
                  "render");
  // Everything RGB-sourced degrades outside the visible; say so once
  // rather than rendering a mysteriously dark image.
  const bool isBeyondVisible{gridLower < 379.0f || gridUpper > 781.0f};
  if (isBeyondVisible)
    SMDL_LOG_WARN(
        "the wavelength grid leaves the visible (380-780nm): colored RGB "
        "textures and images contribute nothing outside it (gray extends "
        "flat), metal IOR tables clamp to their measured ranges, and the "
        "RGB outputs project through CIE color matching, so they darken "
        "wherever the grid misses the visible; the ENVI output is the "
        "radiometric record");
  // The accumulation buffers scale as bands times pixels; say so before
  // allocating gigabytes.
  if (const double gib{double(frame.numPixelsX * frame.numPixelsY) *
                       (8.0 + 8.0 * double(wavelengths.size()) +
                        (opts.render.guide.isEnabled
                             ? 16.0 * double(wavelengths.size()) + 24.0
                             : 0.0)) /
                       (1024.0 * 1024.0 * 1024.0)};
      gib > 1.0)
    SMDL_LOG_INFO("Accumulation buffers: ", gib, " GiB");
  return ResolvedGrid{wavelengths, isBeyondVisible};
}

void setUpCompiler(const Options &opts, const Frame &frame,
                   const ResolvedGrid &grid, smdl::Compiler &compiler) {
  compiler.wavelengthBaseMax = uint32_t(grid.wavelengths.size());
  compiler.isDebugEnabled = opts.compile.isDebugEnabled;
  compiler.shouldEmitUnitTests = false;
  registerSceneData(compiler);
  // The normal distribution entry points are what a glossy manifold
  // crossing draws its half vector from, and nothing else here asks for
  // them, so they are emitted only when that is on.
  bool anyCaster{false};
  for (const auto &item : frame.layout.items) anyCaster |= item.isCaster;
  compiler.shouldEmitScatterNormal = (opts.render.useMNEE && anyCaster) ||
                                     opts.render.shouldTestMNEENormalHook;
  // The built-in stand-in, always available: a scene whose materials
  // have not been written yet still renders, and a name that does not
  // resolve has somewhere to fall back to. It is added even when MDL
  // modules are given, so that '-fallback-material default_object' works
  // alongside them.
  if (auto error{
          compiler.addCode(DEFAULT_MATERIAL_MODULE, DEFAULT_MATERIAL_SOURCE)})
    error->printAndExit();
  for (auto &inputMDLFile : opts.scene.inputMDLFiles)
    if (auto error{compiler.add(std::string(inputMDLFile))})
      error->printAndExit();
}

StagedScene::StagedScene(const Options &opts, Frame &frame,
                         const ResolvedGrid &grid, smdl::Compiler &compiler) {
  const auto &layout{frame.layout};
  const auto &wavelengths{grid.wavelengths};
  const bool isGridBeyondVisible{grid.isBeyondVisible};
  auto &cameraOptions{frame.cameraOptions};
  auto &camera{frame.camera};
  const auto resolution{frame.resolution};
  // A scene given no MDL at all is a layout that has not been shaded yet,
  // so it falls back to the built-in material rather than refusing to
  // render. Given MDL, an unresolved name stays an error, since there it
  // means a name that was meant to resolve and did not.
  auto fallbackMaterial{opts.scene.fallbackMaterial};
  if (fallbackMaterial.empty() && opts.scene.inputMDLFiles.empty())
    fallbackMaterial = DEFAULT_OBJECT_MATERIAL_NAME;
  // The lowering folds every alias and override into the items
  // themselves, which is what keeps an imported layout's names closed;
  // see `MaterialAssignment::renames`.
  scene.emplace(compiler, fallbackMaterial, !opts.render.noRobustIntersection);
  for (const auto &item : layout.items) {
    SMDL_PROFILER_ENTRY("Scene::add()", item.fileName.c_str());
    scene->add(item);
  }
  // The ground plane goes in before commit() because commit() builds the
  // acceleration structure, and is sized from the pre-commit bounds
  // because the Embree bounds do not exist yet. Displacement can still
  // push geometry slightly below the pre-displacement minimum; -ground-z
  // is the override, and thumbnails do not care.
  auto groundInstance{INVALID_INDEX};
  // The pre-ground geometry bounds, remembered because the SD-tree below
  // must NOT be sized by the ground plane: the plane is a backdrop three
  // orders of magnitude wider than the subject, and cubifying over it
  // spends every spatial refinement level zooming back in.
  if (opts.scene.hasGround || opts.scene.groundZ.wasGiven) {
    guideBound = scene->preCommitBounds();
    hasValidGuideBounds = true;
    if (guideBound.isEmpty())
      throw smdl::Error("cannot -ground: the scene has no geometry to "
                        "put a plane under");
    const float z{opts.scene.groundZ.wasGiven ? opts.scene.groundZ.value
                                              : guideBound.lower.z};
    // Large enough that at autolook elevations the plane's edge lands at
    // the visual horizon, small enough to stay in float precision.
    const float halfExtent{std::clamp(
        1000.0f * 0.5f * smdl::length(guideBound.extent()), 100.0f, 20000.0f)};
    auto groundMaterial{opts.scene.groundMaterial};
    if (groundMaterial.empty()) groundMaterial = DEFAULT_GROUND_MATERIAL_NAME;
    // The one command-line-facing name the entry file's aliases still
    // reach, now that the aliases themselves are folded into the items.
    if (auto alias{layout.entryMaterialAliases.find(groundMaterial)};
        alias != layout.entryMaterialAliases.end())
      groundMaterial = alias->second;
    groundInstance = scene->addGroundPlane(z, halfExtent, groundMaterial);
    SMDL_LOG_INFO("Ground plane: z = ", z, ", half extent ", halfExtent,
                  ", material ", smdl::Quoted(groundMaterial));
  }
  // The imports above interned every name the scene can shade with, so
  // narrow the compile to those materials; the fallback and the exterior
  // medium are looked up by name later, so they join the list. With no
  // MDL modules there is only the built-in default module, nothing worth
  // filtering, and the unshaded-scene workflow would warn for every name.
  if (!opts.utility.allMaterials && !opts.scene.inputMDLFiles.empty()) {
    auto desiredMaterials{scene->usedMaterialNames()};
    if (!fallbackMaterial.empty()) desiredMaterials.push_back(fallbackMaterial);
    if (!layout.exteriorMediumName.empty())
      desiredMaterials.push_back(layout.exteriorMediumName);
    // The empty name (an unnamed primitive or groom) can only ever
    // resolve through the fallback.
    desiredMaterials.erase(std::remove(desiredMaterials.begin(),
                                       desiredMaterials.end(), std::string()),
                           desiredMaterials.end());
    compiler.setDesiredMaterials(std::move(desiredMaterials));
  }
  if (auto error{compiler.compile(opts.compile.optLevel)})
    error->printAndExit();
  if (auto error{compiler.jitCompile()}) error->printAndExit();
  {
    SMDL_PROFILER_ENTRY("Scene::commit()");
    scene->commit(wavelengths);
  }
  // The autolook solve, and the deferred camera construction it exists
  // for. The solved azimuth also becomes the default sun azimuth below,
  // so a batch of thumbnails is consistently lit however each one is
  // framed.
  auto autolookSunAzimuth{std::optional<float>()};
  if (opts.camera.autolook.isEnabled) {
    auto autolookOptions{AutolookOptions{}};
    autolookOptions.fovYDeg = cameraOptions.fovYDeg;
    autolookOptions.aspectRatio = float(resolution.x) / float(resolution.y);
    if (cameraOptions.lens) {
      // A lens has no field of view to be given; it has one that follows
      // from the sensor and the focal length, which is what the fit
      // needs. The prescription is solved once here for its focal length
      // alone, focused at infinity, since the focus the real lens takes
      // is the distance this very solve is about to choose.
      const auto sensorMM{cameraOptions.sensorMM.x > 0 &&
                                  cameraOptions.sensorMM.y > 0
                              ? cameraOptions.sensorMM
                              : float2(36.0f, 24.0f)};
      const Lens probe{*cameraOptions.lens, LensOptions{}};
      autolookOptions.fovYDeg = smdl::degrees(
          2 * std::atan(0.5e-3f * sensorMM.y / probe.focalLength()));
      autolookOptions.aspectRatio = sensorMM.x / sensorMM.y;
    }
    autolookOptions.zenithDeg = opts.camera.autolook.zenithDeg;
    if (opts.camera.autolook.azimuthDeg.wasGiven) {
      autolookOptions.azimuthDeg = opts.camera.autolook.azimuthDeg.value;
    } else if (layout.frontAzimuth) {
      autolookOptions.azimuthDeg = layout.frontAzimuth;
      SMDL_LOG_INFO("Autolook: locked to the manifest's front azimuth ",
                    *layout.frontAzimuth, " degrees");
    }
    autolookOptions.margin = opts.camera.autolook.margin;
    autolookOptions.ignoreBackfaces = opts.camera.autolook.ignoreBackfaces;
    autolookOptions.skipInstance = groundInstance;
    const auto autolook{solveAutolook(*scene, autolookOptions)};
    cameraOptions.lookFrom = autolook.lookFrom;
    cameraOptions.lookTo = autolook.lookTo;
    camera.emplace(cameraOptions);
    // The key light over the camera's right shoulder.
    autolookSunAzimuth = autolook.azimuthDeg - 35.0f;
  }

  // The environment, merged from the same three sources as the camera and
  // in the same order: the defaults, the layout's 'sky' directive, and
  // whatever the command line explicitly gave.
  const auto &fileSky{layout.sky};
  const auto iblFileName{pick(opts.light.sky.iblFileName, fileSky.iblFileName)};
  const auto moonGiven{opts.light.sky.moonPhase.wasGiven ||
                       bool(fileSky.moonPhase)};
  if (!iblFileName.empty()) {
    envLight = std::make_unique<EnvLight>(
        iblFileName, pick(opts.light.sky.iblScale, fileSky.iblScale));
    if (isGridBeyondVisible)
      SMDL_LOG_WARN("-ibl is an RGB image: on this wavelength grid it "
                    "contributes only inside the visible");
  } else if (!pick(opts.light.sky.none, fileSky.none)) {
    auto options{smdl::SunSkyOptions{}};
    float zenith{
        smdl::radians(pick(opts.light.sky.sunZenithDeg, fileSky.sunZenith))};
    float azimuthDeg{pick(opts.light.sky.sunAzimuthDeg, fileSky.sunAzimuth)};
    // Under -autolook with no stated sun azimuth, the key light follows the
    // solved camera: a perfectly framed thumbnail lit from behind is as
    // unreadable as one framed end-on, and this keeps a whole library
    // consistently lit however each asset was framed.
    if (autolookSunAzimuth && !opts.light.sky.sunAzimuthDeg.wasGiven &&
        !fileSky.sunAzimuth) {
      azimuthDeg = *autolookSunAzimuth;
      SMDL_LOG_INFO("Sun azimuth follows the framed camera: ", azimuthDeg,
                    " degrees");
    }
    const float azimuth{smdl::radians(azimuthDeg)};
    options.sunDirection =
        float3(std::sin(zenith) * std::cos(azimuth),
               std::sin(zenith) * std::sin(azimuth), std::cos(zenith));
    options.visibility = pick(opts.light.sky.visibility, fileSky.visibility);
    options.waterVaporScale =
        pick(opts.light.sky.waterVapor, fileSky.waterVapor);
    options.scaleFactor = pick(opts.light.sky.scale, fileSky.scale);
    if (moonGiven) {
      options.isMoon = true;
      options.moonPhase = pick(opts.light.sky.moonPhase, fileSky.moonPhase);
      options.moonDistanceScale =
          pick(opts.light.sky.moonDistance, fileSky.moonDistance);
    }
    envLight = std::make_unique<EnvLight>(options);
  }

  // The exterior medium the layout's 'medium' directive names, if
  // any: one material instance evaluated up front in an allocator that
  // outlives the render, seeding every camera path's medium stack. The
  // instance has no geometry, so heterogeneous coefficient queries run
  // in world space directly. Evaluated once at the base animation time,
  // so an open shutter does not vary its captured homogeneous
  // coefficients (per-point heterogeneous queries still see the path
  // time).
  if (!layout.exteriorMediumName.empty()) {
    const auto *materialDef{compiler.findMaterial(layout.exteriorMediumName)};
    if (!materialDef)
      throw smdl::Error(smdl::concat(
          "cannot resolve 'medium' directive material ",
          smdl::Quoted(layout.exteriorMediumName),
          opts.scene.inputMDLFiles.empty() ? " (no MDL modules were given)"
                                           : ""));
    if (!materialDef->hasVolume())
      throw smdl::Error(smdl::concat("'medium' directive material ",
                                     smdl::Quoted(layout.exteriorMediumName),
                                     " has no 'volume'"));
    auto state{makeRenderState(wavelengths, &mMediumAllocator)};
    state.finalize();
    exteriorMedium = new (mMediumAllocator) MediumStack{
        nullptr,
        mMediumAllocator.allocate<smdl::JIT::Material>(state, materialDef),
        nullptr};
    SMDL_LOG_INFO("Exterior medium: ", smdl::Quoted(layout.exteriorMediumName));
  }

  // The exterior haze: the analytic exponential-height atmosphere that
  // produces aerial perspective, whose extinction, transmittance and
  // free-flight distance are all closed form, so it costs no tracking
  // and no majorant. It is the medium of everything outside all
  // geometry, which is where the 'medium' directive puts its material
  // too, so the two cannot both be asked for.
  const auto &fileHaze{layout.haze};
  bool isHazeEnabled{opts.light.haze.isOn || layout.hasHaze};
  if (pick(opts.light.haze.none, fileHaze.none)) isHazeEnabled = false;
  if (isHazeEnabled) {
    if (exteriorMedium)
      throw smdl::Error("the exterior haze and the 'medium' directive both "
                        "describe the medium outside all geometry; keep one");
    auto options{smdl::HazeOptions{}};
    // An unwritten visibility follows the sky's, so that distant
    // terrain does not read hazier or clearer than the horizon sky
    // immediately behind it. The two models overlap toward the sky; see
    // `LayoutHaze`.
    options.visibility = pick(opts.light.haze.visibility, fileHaze.visibility);
    if (!(options.visibility > 0.0f))
      options.visibility = pick(opts.light.sky.visibility, fileSky.visibility);
    options.scaleHeight =
        pick(opts.light.haze.scaleHeight, fileHaze.scaleHeight);
    if (fileHaze.baseHeight) options.baseHeight = *fileHaze.baseHeight;
    if (fileHaze.droplet) options.dropletSize = *fileHaze.droplet;
    haze = std::make_unique<smdl::Haze>(
        options,
        smdl::Span<const float>(wavelengths.data(), wavelengths.size()),
        makeRenderState(wavelengths).metersPerSceneUnit);
    SMDL_LOG_INFO("Exterior haze: visibility ", options.visibility,
                  " km, scale height ", options.scaleHeight, " m");
  }
  // Every light in one selection path: each emissive mesh instance plus
  // the environment, weighted by power.
  auto profLightSampler{smdl::profilerEntryBegin("Build light sampler")};
  lights.emplace(compiler, *scene, envLight.get(), layout.lights, wavelengths,
                 opts.render.allLights, !opts.render.noLightTree);
  smdl::profilerEntryEnd(profLightSampler);
}
