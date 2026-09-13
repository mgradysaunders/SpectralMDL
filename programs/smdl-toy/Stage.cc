#include <algorithm>
#include <cmath>
#include <optional>
#include <string>
#include <utility>

#include "smdl/Support/Error.h"
#include "smdl/Support/Filesystem.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Profiler.h"
#include "smdl/Support/Strings.h"

#include "../CommandLine.h"

#include "Layout/LayoutTables.h"
#include "Options.h"
#include "Render/Autolook.h"
#include "Resume.h"
#include "Scene/Scene.h"
#include "Sensor/Response.h"
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

Frame resolveFrame(const Options &opts) {
  // Everything the command line asks for, lowered into one layout: the
  // positional argument first, then each -mesh. Either may name a mesh
  // file or a '.layout'. Read before the camera because a layout may
  // describe one; it is only a text parse, so fail-fast ordering costs
  // nothing. Tilde is expanded here because a search path is typed by a
  // person.
  AssetSearchPath assetSearchPath{};
  for (const auto &directory : opts.scene.assetDirs)
    assetSearchPath.push_back(smdl::makePathCanonical(directory));
  // The camera first, and with it the clock: how long the shutter stays
  // open and how long the readout sweeps are the camera's and the sensor's,
  // and what a layout's motion means is the transform at those two
  // instants, so they are settled before the scene is read. The camera
  // itself is a text parse and a paraxial solve, so fail-fast ordering
  // costs nothing.
  CameraModel model{resolveCameraModel(opts)};
  refuseUnrenderable(model);
  // What every 'motion' track is evaluated at. A shut shutter lands both
  // samples on one instant, so every track lowers static and the render
  // takes the path it takes with no motion at all.
  const MotionSampling sampling{gRenderShutter.time,
                                gRenderShutter.secondsAt(1.0f)};
  smdl::ProfilerEntry *profReadLayout{smdl::profilerEntryBegin("Read layout")};
  Layout layout{resolveLayoutArgument(opts.scene.inputSceneFile,
                                      assetSearchPath, sampling)};
  for (const auto &fileName : opts.scene.inputMeshFiles) {
    Layout more{resolveLayoutArgument(fileName, assetSearchPath, sampling)};
    layout.items.insert(layout.items.end(), more.items.begin(),
                        more.items.end());
    layout.lights.insert(layout.lights.end(), more.lights.begin(),
                         more.lights.end());
    layout.entryMaterialAliases.insert(more.entryMaterialAliases.begin(),
                                       more.entryMaterialAliases.end());
  }
  smdl::profilerEntryEnd(profReadLayout);
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
      SMDL_LOG_INFO(
          "Instance motion: ", smdl::Counted(numMovingItems, "placement"),
          " and ", smdl::Counted(numMovingLights, "light"),
          " move over the shutter");
  }
  // Under -autolook the position, and under 'focus auto' the focus, come
  // from measuring the committed scene, so construction (with the lens
  // validation and the summary it logs) waits until the solves in the
  // stage. Every other path keeps constructing here, before anything
  // slow loads, so a lens typo still fails fast.
  std::optional<Camera> camera{};
  if (!opts.camera.autolook.isEnabled && !model.shouldAutofocus)
    camera.emplace(buildCamera(model));
  const int2 resolution{model.cameraOptions.resolution};
  const size_t numPixelsX{size_t(resolution.x)};
  const size_t numPixelsY{size_t(resolution.y)};
  const size_t spp{size_t(opts.render.sampling.spp)};
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
  const size_t numWindowPixels{size_t(window[2] - window[0]) *
                               size_t(window[3] - window[1])};
  Frame frame{};
  frame.layout = std::move(layout);
  frame.shouldJitterWavelength =
      opts.render.grid.shouldJitter.wasGiven || !model.hasSensor()
          ? opts.render.grid.shouldJitter.value
          : true;
  frame.model = std::move(model);
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
  // The wavelength grid, in priority order: explicit '-wavelengths';
  // '-wavelength-range' uniform bands (endpoint-inclusive); when resuming
  // with no grid flags at all, the grid recorded in the resumed file, so
  // a resumed render needs no grid retyping; else the grid the renderer
  // places itself, '-wavelength-count' bands: by a physical sensor's
  // curves when every sample projects onto every band, uniform over
  // their span under a tile, whose per-band grids are the render loop's,
  // or uniform over the visible default. The band count seeds every
  // 'Color' constructed from here on.
  const GridOptions &gridOptions{opts.render.grid};
  const bool shouldAdoptResumedGrid{!gridOptions.wasGiven && resumed.wasLoaded};
  const ResponseSettings *response{
      frame.model.hasSensor() ? &frame.model.sensor->settings().response
                                      : nullptr};
  const auto uniform{[](const WavelengthRange &range) {
    std::vector<float> grid(size_t(range.bandCount));
    for (size_t i = 0; i < grid.size(); i++) {
      const float t{float(i) / float(grid.size() - 1)};
      grid[i] = (1 - t) * range.range.x + t * range.range.y;
    }
    return WavelengthCells::fromWavelengths(
        smdl::Span<const float>(grid.data(), grid.size()));
  }};
  const auto asSpan{[](const std::vector<float> &list) {
    return smdl::Span<const float>(list.data(), list.size());
  }};
  // The grids: one, or under a tile one per band the tile lays down, and
  // then the tile as grid indices, which is the sensor's tile with each
  // band replaced by the index of its grid.
  std::vector<WavelengthCells> family{};
  size_t tileColumns{0};
  std::vector<size_t> tile{};
  const auto placeTile{[&]() {
    const std::vector<size_t> bands{tileBands(response->cfa)};
    tileColumns = response->cfaColumns;
    for (const auto index : response->cfa)
      tile.push_back(
          size_t(std::find(bands.begin(), bands.end(), index) - bands.begin()));
  }};
  const std::vector<GridHeader::Grid> &records{resumed.grids.grids};
  const bool isFilePerBand{!records.empty() && !records.front().name.empty()};
  // The one grid's wavelengths as the file states them: the format's
  // list, which the observer's film carries, or the record's, which a
  // band film states in its place, its own bands being the sensor's.
  const std::vector<float> &fileWavelengths{resumed.info.wavelengths.empty() &&
                                                    !records.empty()
                                                ? records.front().wavelengths
                                                : resumed.info.wavelengths};
  bool wasLogged{false};
  if (!gridOptions.explicitWavelengths.empty()) {
    family.push_back(WavelengthCells::fromWavelengths(
        asSpan(gridOptions.explicitWavelengths)));
  } else if (gridOptions.range.wasGiven) {
    family.push_back(uniform(gridOptions.range.value));
  } else if (shouldAdoptResumedGrid) {
    if (isFilePerBand) {
      if (!response || !response->hasCFA())
        throw smdl::Error(
            "Cannot resume: the file holds a grid per tile band, and this "
            "camera's sensor has no tile; render through the sensor the "
            "file was rendered through");
      for (const auto index : tileBands(response->cfa)) {
        const std::string &name{response->bands[index].name};
        const auto itr{std::find_if(records.begin(), records.end(),
                                    [&](const GridHeader::Grid &record) {
                                      return record.name == name;
                                    })};
        if (itr == records.end())
          throw smdl::Error(
              smdl::concat("Cannot resume: the file states no grid for the "
                           "tile band ",
                           smdl::Quoted(name)));
        family.push_back(WavelengthCells{itr->bandEdges, itr->wavelengths});
      }
      placeTile();
    } else {
      if (fileWavelengths.empty())
        throw smdl::Error(
            "Cannot resume: the file carries no wavelengths to adopt; give "
            "the grid explicitly with -wavelength-range or -wavelengths, or "
            "for a band film written before its grid was recorded, start a "
            "fresh -output-bands");
      // The file's own cells when it states them, else the ones its list
      // implies, which is what a file written before the cells were
      // recorded meant.
      if (records.empty() || records.front().bandEdges.empty())
        family.push_back(
            WavelengthCells::fromWavelengths(asSpan(fileWavelengths)));
      else
        family.push_back(
            WavelengthCells{records.front().bandEdges, fileWavelengths});
    }
    for (const auto &cells : family)
      if (!cells.isValid())
        throw smdl::Error("Cannot resume: the file's band edges do not "
                          "describe its wavelengths");
  } else {
    WavelengthRange range{gridOptions.range.value};
    if (gridOptions.count.wasGiven) range.bandCount = gridOptions.count.value;
    const size_t count{size_t(range.bandCount)};
    const auto describeCells{[](const WavelengthCells &cells) {
      double narrowest{INF};
      double widest{0};
      for (size_t i = 0; i + 1 < cells.edges.size(); i++) {
        narrowest = std::min(narrowest, cells.edges[i + 1] - cells.edges[i]);
        widest = std::max(widest, cells.edges[i + 1] - cells.edges[i]);
      }
      return smdl::concat(float(cells.edges.front()), "-",
                          float(cells.edges.back()), " nm, from ",
                          smdl::Brief(narrowest, 3), " to ",
                          smdl::Brief(widest, 3), " nm wide");
    }};
    if (response && response->hasCFA()) {
      // Under a tile each band the tile lays down places its own grid,
      // which its pixels alone evaluate on.
      for (const auto index : tileBands(response->cfa)) {
        const ResponseBand &band{response->bands[index]};
        family.push_back(bandWavelengthCells(band, count));
        SMDL_LOG_INFO("Wavelength grid: ", count,
                      " bands placed by the "
                      "sensor's ",
                      smdl::Quoted(band.name), " curve for its pixels over ",
                      describeCells(family.back()));
      }
      placeTile();
      wasLogged = true;
    } else if (response) {
      family.push_back(responseWavelengthCells(*response, count));
      SMDL_LOG_INFO("Wavelength grid: ", count,
                    " bands placed by the sensor's curves over ",
                    describeCells(family.back()));
      wasLogged = true;
    } else {
      family.push_back(uniform(range));
    }
  }
  // The band count has to land before the first `Color` is built, since
  // that is what sizes it.
  const bool shouldJitter{frame.shouldJitterWavelength};
  if (shouldJitter && !gridOptions.shouldJitter.wasGiven)
    SMDL_LOG_INFO("Wavelength jitter: on for a physical sensor, so a band "
                  "narrower than the grid's spacing integrates without "
                  "aliasing; -wavelength-jitter=false turns it off");
  gRenderGrid.reset(std::move(family), tileColumns, std::move(tile),
                    shouldJitter);
  if (shouldJitter && !gRenderGrid.isJittering)
    SMDL_LOG_WARN("-wavelength-jitter needs at least 2 bands to have a "
                  "band width to jitter within, so it does nothing here");
  const Color wavelengths{gRenderGrid.first().wavelengths};
  if (resumed.wasLoaded) {
    // The observer's film holds the grid's bands; a band film holds the
    // sensor's, which the resume already held to the response's.
    if (!response && resumed.film.getNumBands() != gRenderGrid.numBands)
      throw smdl::Error(smdl::concat(
          "Cannot resume: the file has ", resumed.film.getNumBands(),
          " bands against the renderer's ", gRenderGrid.numBands));
    // Each grid against what the file states for it: the one grid
    // against the format's list and the edges when stated, a tile's
    // grids against their records. Adoption took them from there; a
    // session with grid flags of its own has to land on the same ones.
    if (isFilePerBand != gRenderGrid.hasTile() ||
        (isFilePerBand && records.size() != gRenderGrid.grids.size()))
      throw smdl::Error(smdl::concat(
          "Cannot resume: the file was rendered on ",
          isFilePerBand ? "a grid per tile band" : "one grid",
          " and this session renders on ",
          gRenderGrid.hasTile() ? "a grid per tile band" : "one grid",
          "; give the grid flags the first session was given, or none"));
    const auto check{[](const WavelengthGrid &grid,
                        const std::vector<float> &fileWavelengths,
                        const std::vector<double> &fileEdges) {
      for (size_t i = 0; i < grid.size(); i++)
        if (i >= fileWavelengths.size() ||
            !(std::abs(fileWavelengths[i] - grid.wavelengths[i]) < 0.5f))
          throw smdl::Error(
              "Cannot resume: the wavelength grid does not match the "
              "renderer's");
      for (size_t i = 0; i < fileEdges.size(); i++)
        if (i >= grid.bandEdges.size() ||
            !(std::abs(fileEdges[i] - grid.bandEdges[i]) < 0.5))
          throw smdl::Error("Cannot resume: the file's band edges do not "
                            "match the renderer's");
    }};
    if (isFilePerBand) {
      const std::vector<size_t> bands{tileBands(response->cfa)};
      for (size_t k = 0; k < bands.size(); k++) {
        const std::string &name{response->bands[bands[k]].name};
        const auto itr{std::find_if(records.begin(), records.end(),
                                    [&](const GridHeader::Grid &record) {
                                      return record.name == name;
                                    })};
        if (itr == records.end())
          throw smdl::Error(
              smdl::concat("Cannot resume: the file states no grid for the "
                           "tile band ",
                           smdl::Quoted(name)));
        check(gRenderGrid.grids[k], itr->wavelengths, itr->bandEdges);
      }
    } else {
      check(gRenderGrid.first(), fileWavelengths,
            records.empty() ? std::vector<double>{}
                            : records.front().bandEdges);
    }
  }
  if ((gridOptions.wasGiven || shouldAdoptResumedGrid) && !wasLogged) {
    if (gRenderGrid.hasTile())
      SMDL_LOG_INFO("Wavelength grid: ", gRenderGrid.numBands,
                    " bands adopted from the resumed file, one grid per "
                    "tile band");
    else
      SMDL_LOG_INFO(
          "Wavelength grid: ", wavelengths.size(),
          shouldAdoptResumedGrid ? " bands adopted from the resumed file, "
                                 : " bands, ",
          wavelengths[0], "-", wavelengths[wavelengths.size() - 1], " nm");
  }
  if (wavelengths.size() > 256)
    SMDL_LOG_WARN(wavelengths.size(),
                  " bands: JIT compile time and per-sample cost both grow "
                  "with the band count, expect a slow start and a slow "
                  "render");
  // Outside the visible, RGB-sourced spectra are extrapolated and the RGB
  // outputs see little; say so once rather than rendering a mysteriously
  // dark image.
  const bool isBeyondVisible{gRenderGrid.minWavelength() < 379.0f ||
                             gRenderGrid.maxWavelength() > 781.0f};
  if (isBeyondVisible)
    SMDL_LOG_WARN(
        "The wavelength grid leaves the visible (380-780nm): RGB colors, "
        "textures, and images extend flat from their 380 and 780nm values "
        "(a convention, not data), metal IOR tables clamp to their measured "
        "ranges, and the RGB outputs project through CIE color matching, so "
        "they darken wherever the grid misses the visible; the ENVI output "
        "is the radiometric record");
  // The accumulation buffers scale as bands times pixels, the film's
  // bands being the grid's for the observer and the sensor's through a
  // sensor; say so before allocating gigabytes.
  const SensorSettings *sensor{frame.model.hasSensor()
                                   ? &frame.model.sensor->settings()
                                   : nullptr};
  const double filmBytes{sensor
                             ? 8.0 * double(sensor->response.hasCFA()
                                                ? 1
                                                : sensor->response.bands.size())
                             : 8.0 * double(wavelengths.size())};
  if (const double gib{double(frame.numPixelsX * frame.numPixelsY) *
                       (8.0 + filmBytes +
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
  if (std::optional<smdl::Error> error{
          compiler.addCode(DEFAULT_MATERIAL_MODULE, DEFAULT_MATERIAL_SOURCE)})
    error->printAndExit();
  for (auto &inputMDLFile : opts.scene.inputMDLFiles)
    if (std::optional<smdl::Error> error{
            compiler.add(std::string(inputMDLFile))})
      error->printAndExit();
}

StagedScene::StagedScene(const Options &opts, Frame &frame,
                         const ResolvedGrid &grid, smdl::Compiler &compiler) {
  const Layout &layout{frame.layout};
  const Color &wavelengths{grid.wavelengths};
  const bool isGridBeyondVisible{grid.isBeyondVisible};
  CameraOptions &cameraOptions{frame.model.cameraOptions};
  std::optional<Camera> &camera{frame.camera};
  const int2 resolution{frame.resolution};
  // A scene given no MDL at all is a layout that has not been shaded yet,
  // so it falls back to the built-in material rather than refusing to
  // render. Given MDL, an unresolved name stays an error, since there it
  // means a name that was meant to resolve and did not.
  std::string fallbackMaterial{opts.scene.fallbackMaterial};
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
  uint32_t groundInstance{INVALID_INDEX};
  // The pre-ground geometry bounds, remembered because the SD-tree below
  // must NOT be sized by the ground plane: the plane is a backdrop three
  // orders of magnitude wider than the subject, and cubifying over it
  // spends every spatial refinement level zooming back in.
  if (opts.scene.hasGround || opts.scene.groundZ.wasGiven) {
    guideBound = scene->preCommitBounds();
    hasValidGuideBounds = true;
    if (guideBound.isEmpty())
      throw smdl::Error("Cannot -ground: the scene has no geometry to "
                        "put a plane under");
    const float z{opts.scene.groundZ.wasGiven ? opts.scene.groundZ.value
                                              : guideBound.lower.z};
    // Large enough that at autolook elevations the plane's edge lands at
    // the visual horizon, small enough to stay in float precision.
    const float halfExtent{std::clamp(
        1000.0f * 0.5f * smdl::length(guideBound.extent()), 100.0f, 20000.0f)};
    std::string groundMaterial{opts.scene.groundMaterial};
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
    std::vector<std::string> desiredMaterials{scene->usedMaterialNames()};
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
  if (std::optional<smdl::Error> error{compiler.compile(opts.compile.optLevel)})
    error->printAndExit();
  if (std::optional<smdl::Error> error{compiler.jitCompile()})
    error->printAndExit();
  {
    SMDL_PROFILER_ENTRY("Scene::commit()");
    scene->commit(wavelengths);
  }
  // The two measurements the camera may wait on: the autolook solve,
  // whose azimuth also becomes the default sun azimuth below so that a
  // batch of thumbnails is consistently lit however each one is framed,
  // and then the autofocus, which measures the framing the autolook
  // chose. The camera is built once both have had their say.
  // What both ask of a prescription is what it does focused at infinity,
  // since the focus a real lens takes is the distance these very
  // measurements are about to choose, so one solve answers both.
  std::optional<Lens> probe{};
  if (cameraOptions.lens &&
      (opts.camera.autolook.isEnabled || frame.model.shouldAutofocus))
    probe.emplace(*cameraOptions.lens, LensOptions{});
  std::optional<float> autolookSunAzimuth{};
  if (opts.camera.autolook.isEnabled) {
    AutolookOptions autolookOptions{};
    autolookOptions.fovYDeg = cameraOptions.fovYDeg;
    autolookOptions.aspectRatio = float(resolution.x) / float(resolution.y);
    if (probe) {
      // A lens's field follows from the frame and the prescription,
      // which is what the fit needs.
      std::optional<float> angle{
          probe->fieldAngleAt(0.5f * cameraOptions.frameSize.y)};
      if (!angle) {
        // A frame taller than the lens's image circle is framed to the
        // circle, which is where the picture ends at the sides as much as
        // at the top and bottom. A lens nothing gets out of at all is the
        // camera's to refuse, once it is built.
        angle = probe->fieldAngleAt(probe->imageCircleRadius());
        if (angle)
          autolookOptions.aspectRatio =
              std::min(autolookOptions.aspectRatio, 1.0f);
      }
      if (angle) autolookOptions.fovYDeg = 2 * smdl::degrees(*angle);
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
    const AutolookResult autolook{solveAutolook(*scene, autolookOptions)};
    cameraOptions.lookFrom = autolook.lookFrom;
    cameraOptions.lookTo = autolook.lookTo;
    // The key light over the camera's right shoulder.
    autolookSunAzimuth = autolook.azimuthDeg - 35.0f;
  }
  if (frame.model.shouldAutofocus) {
    AutofocusOptions autofocusOptions{};
    autofocusOptions.lookFrom = cameraOptions.lookFrom;
    autofocusOptions.lookTo = cameraOptions.lookTo;
    autofocusOptions.lookUp = cameraOptions.lookUp;
    if (probe) {
      // The prescription's paraxial focal length.
      autofocusOptions.focalLengthOverHeight =
          probe->focalLength() / cameraOptions.frameSize.y;
    } else {
      autofocusOptions.focalLengthOverHeight =
          0.5f / std::tan(smdl::radians(cameraOptions.fovYDeg / 2));
    }
    cameraOptions.focus = solveAutofocus(*scene, autofocusOptions).distance;
  }
  if (!camera) camera.emplace(buildCamera(frame.model));

  // The environment, merged from the same three sources as the camera and
  // in the same order: the defaults, the layout's 'sky' directive, and
  // whatever the command line explicitly gave.
  const LayoutSky &fileSky{layout.sky};
  const std::string iblFileName{
      pick(opts.light.sky.iblFileName, fileSky.iblFileName)};
  const bool moonGiven{opts.light.sky.moonPhase.wasGiven ||
                       bool(fileSky.moonPhase)};
  if (!iblFileName.empty()) {
    envLight = std::make_unique<EnvLight>(
        iblFileName, pick(opts.light.sky.iblScale, fileSky.iblScale));
    if (isGridBeyondVisible)
      SMDL_LOG_WARN("-ibl is an RGB image: on this wavelength grid it "
                    "contributes only inside the visible");
  } else if (!pick(opts.light.sky.none, fileSky.none)) {
    smdl::SunSkyOptions options{};
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

  // The exterior medium the layout's 'medium' directive names, if any.
  // Only the definition is resolved here: every camera path evaluates
  // its own instance at its head, at the path's wavelengths and time
  // (see 'PathWalk::trace'), which is what the homogeneity proof's
  // contract asks of an instance the closed forms read.
  if (!layout.exteriorMediumName.empty()) {
    const smdl::JIT::MaterialDef *materialDef{
        compiler.findMaterial(layout.exteriorMediumName)};
    if (!materialDef)
      throw smdl::Error(smdl::concat(
          "Cannot resolve the material of the 'medium' directive: ",
          compiler.explainMaterialLookup(layout.exteriorMediumName),
          opts.scene.inputMDLFiles.empty() ? " (no MDL modules were given)"
                                           : ""));
    if (!materialDef->hasVolume())
      throw smdl::Error(smdl::concat("The 'medium' directive material ",
                                     smdl::Quoted(layout.exteriorMediumName),
                                     " has no 'volume'"));
    exteriorMediumDef = materialDef;
    SMDL_LOG_INFO("Exterior medium: ", smdl::Quoted(layout.exteriorMediumName));
  }

  // The exterior haze: the analytic exponential-height atmosphere that
  // produces aerial perspective, whose extinction, transmittance and
  // free-flight distance are all closed form, so it costs no tracking
  // and no majorant. It is the medium of everything outside all
  // geometry, which is where the 'medium' directive puts its material
  // too, so the two cannot both be asked for.
  const LayoutHaze &fileHaze{layout.haze};
  bool isHazeEnabled{opts.light.haze.isOn || layout.hasHaze};
  if (pick(opts.light.haze.none, fileHaze.none)) isHazeEnabled = false;
  if (isHazeEnabled) {
    if (exteriorMediumDef)
      throw smdl::Error("The exterior haze and the 'medium' directive both "
                        "describe the medium outside all geometry; keep one");
    smdl::HazeOptions options{};
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
    for (const auto &grid : gRenderGrid.grids)
      hazes.emplace_back(options,
                         smdl::Span<const float>(grid.wavelengths.data(),
                                                 grid.wavelengths.size()),
                         makeRenderState(wavelengths).metersPerSceneUnit);
    SMDL_LOG_INFO("Exterior haze: visibility ", options.visibility,
                  " km, scale height ", options.scaleHeight, " m");
  }
  // Every light in one selection path: each emissive mesh instance plus
  // the environment, weighted by power.
  smdl::ProfilerEntry *profLightSampler{
      smdl::profilerEntryBegin("Build light sampler")};
  lights.emplace(compiler, *scene, envLight.get(), layout.lights, wavelengths,
                 opts.render.allLights, !opts.render.noLightTree);
  smdl::profilerEntryEnd(profLightSampler);
}
