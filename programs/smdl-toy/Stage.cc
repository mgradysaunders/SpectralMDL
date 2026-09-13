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

namespace {

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

// What the ground plane added.
struct GroundPlane final {
  // The instance the plane became, or `INVALID_INDEX` for no plane.
  uint32_t instance{INVALID_INDEX};

  // The geometry bounds as they were before the plane went in, which is
  // what the SD-tree is sized by; nothing without a plane, where the
  // scene's own bounds serve. The plane is a backdrop three orders of
  // magnitude wider than the subject, and cubifying over it spends every
  // spatial refinement level zooming back in.
  std::optional<BoundBox3> guideBound{};
};

// Put the ground plane under the scene, or nothing when neither -ground
// nor -ground-z asked for one.
//
// It goes in before commit(), which builds the acceleration structure,
// and is sized from the pre-commit bounds because the Embree bounds do
// not exist yet. Displacement can still push geometry slightly below the
// pre-displacement minimum; -ground-z is the override, and thumbnails do
// not care.
[[nodiscard]] GroundPlane addGround(const Options &opts, const Layout &layout,
                                    Scene &scene) {
  if (!opts.scene.hasGround && !opts.scene.groundZ.wasGiven) return {};
  GroundPlane ground{};
  const BoundBox3 bound{scene.preCommitBounds()};
  if (bound.isEmpty())
    throw smdl::Error("Cannot -ground: the scene has no geometry to "
                      "put a plane under");
  ground.guideBound = bound;
  const float z{opts.scene.groundZ.wasGiven ? opts.scene.groundZ.value
                                            : bound.lower.z};
  // Large enough that at autolook elevations the plane's edge lands at
  // the visual horizon, small enough to stay in float precision.
  const float halfExtent{std::clamp(
      1000.0f * 0.5f * smdl::length(bound.extent()), 100.0f, 20000.0f)};
  std::string groundMaterial{opts.scene.groundMaterial};
  if (groundMaterial.empty()) groundMaterial = DEFAULT_GROUND_MATERIAL_NAME;
  // The one command-line-facing name the entry file's aliases still
  // reach, now that the aliases themselves are folded into the items.
  if (auto alias{layout.entryMaterialAliases.find(groundMaterial)};
      alias != layout.entryMaterialAliases.end())
    groundMaterial = alias->second;
  ground.instance = scene.addGroundPlane(z, halfExtent, groundMaterial);
  SMDL_LOG_INFO("Ground plane: z = ", z, ", half extent ", halfExtent,
                ", material ", smdl::Quoted(groundMaterial));
  return ground;
}

// Narrow the compile to the materials the scene can actually shade with.
//
// The imports interned every such name, so the list is the scene's; the
// fallback and the exterior medium are looked up by name later, so they
// join it. With no MDL modules there is only the built-in default module,
// nothing worth filtering, and the unshaded-scene workflow would warn for
// every name.
void narrowDesiredMaterials(const Options &opts, const Layout &layout,
                            const Scene &scene,
                            const std::string &fallbackMaterial,
                            smdl::Compiler &compiler) {
  if (opts.utility.useAllMaterials || opts.scene.inputMDLFiles.empty()) return;
  std::vector<std::string> desiredMaterials{scene.usedMaterialNames()};
  if (!fallbackMaterial.empty()) desiredMaterials.push_back(fallbackMaterial);
  if (!layout.exteriorMediumName.empty())
    desiredMaterials.push_back(layout.exteriorMediumName);
  // The empty name (an unnamed primitive or groom) can only ever resolve
  // through the fallback.
  desiredMaterials.erase(std::remove(desiredMaterials.begin(),
                                     desiredMaterials.end(), std::string()),
                         desiredMaterials.end());
  compiler.setDesiredMaterials(std::move(desiredMaterials));
}

// Frame the camera on the committed scene, and answer the sun azimuth the
// solve implies, or nothing when nothing framed it.
//
// `probe` is the prescription focused at infinity, or null without a lens.
// Writes `lookFrom` and `lookTo` into the model's options, which the
// camera is built from afterward.
[[nodiscard]] std::optional<float>
solveAutolookInto(const Options &opts, Frame &frame, const Scene &scene,
                  uint32_t groundInstance, const Lens *probe) {
  CameraOptions &cameraOptions{frame.model.cameraOptions};
  const int2 resolution{frame.resolution};
  AutolookOptions autolookOptions{};
  autolookOptions.fovYDeg = cameraOptions.fovYDeg;
  autolookOptions.aspectRatio = float(resolution.x) / float(resolution.y);
  if (probe) {
    // A lens's field follows from the frame and the prescription, which
    // is what the fit needs.
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
  } else if (frame.layout.frontAzimuth) {
    autolookOptions.azimuthDeg = frame.layout.frontAzimuth;
    SMDL_LOG_INFO("Autolook: locked to the manifest's front azimuth ",
                  *frame.layout.frontAzimuth, " degrees");
  }
  autolookOptions.margin = opts.camera.autolook.margin;
  autolookOptions.shouldIgnoreBackfaces =
      opts.camera.autolook.shouldIgnoreBackfaces;
  autolookOptions.skipInstance = groundInstance;
  const AutolookResult autolook{solveAutolook(scene, autolookOptions)};
  cameraOptions.lookFrom = autolook.lookFrom;
  cameraOptions.lookTo = autolook.lookTo;
  // The key light over the camera's right shoulder.
  return autolook.azimuthDeg - 35.0f;
}

// Focus on whatever the center of the framed picture sees, writing the
// distance into the model's options. `probe` is as above.
void solveAutofocusInto(Frame &frame, const Scene &scene, const Lens *probe) {
  CameraOptions &cameraOptions{frame.model.cameraOptions};
  AutofocusOptions autofocusOptions{};
  autofocusOptions.lookFrom = cameraOptions.lookFrom;
  autofocusOptions.lookTo = cameraOptions.lookTo;
  autofocusOptions.lookUp = cameraOptions.lookUp;
  // The prescription's paraxial focal length, or the thin lens's field.
  autofocusOptions.focalLengthOverHeight =
      probe ? probe->focalLength() / cameraOptions.frameSize.y
            : 0.5f / std::tan(smdl::radians(cameraOptions.fovYDeg / 2));
  cameraOptions.focus = solveAutofocus(scene, autofocusOptions).distance;
}

// The environment, merged from the same three sources as the camera and
// in the same order: the defaults, the layout's 'sky' directive, and
// whatever the command line explicitly gave. Null when the scene has
// none.
//
// `autolookSunAzimuth` is what the autolook solve implied, or nothing.
// With no stated azimuth the key light follows the solved camera: a
// perfectly framed thumbnail lit from behind is as unreadable as one
// framed end-on, and this keeps a whole library consistently lit however
// each asset was framed.
[[nodiscard]] std::unique_ptr<EnvLight>
buildEnvLight(const Options &opts, const Layout &layout,
              std::optional<float> autolookSunAzimuth) {
  const LayoutSky &fileSky{layout.sky};
  const std::string iblFileName{
      pick(opts.light.sky.iblFileName, fileSky.iblFileName)};
  if (!iblFileName.empty()) {
    auto envLight{std::make_unique<EnvLight>(
        iblFileName, pick(opts.light.sky.iblScale, fileSky.iblScale))};
    if (gRenderGrid.isBeyondVisible)
      SMDL_LOG_WARN("-ibl is an RGB image: on this wavelength grid it "
                    "contributes only inside the visible");
    return envLight;
  }
  if (pick(opts.light.sky.none, fileSky.none)) return {};
  smdl::SunSkyOptions options{};
  const float zenith{
      smdl::radians(pick(opts.light.sky.sunZenithDeg, fileSky.sunZenith))};
  float azimuthDeg{pick(opts.light.sky.sunAzimuthDeg, fileSky.sunAzimuth)};
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
  options.waterVaporScale = pick(opts.light.sky.waterVapor, fileSky.waterVapor);
  options.scaleFactor = pick(opts.light.sky.scale, fileSky.scale);
  // Either source giving the phase, or the distance, asks for moonlight.
  if (opts.light.sky.moonPhase.wasGiven || bool(fileSky.moonPhase)) {
    options.isMoon = true;
    options.moonPhase = pick(opts.light.sky.moonPhase, fileSky.moonPhase);
    options.moonDistanceScale =
        pick(opts.light.sky.moonDistance, fileSky.moonDistance);
  }
  return std::make_unique<EnvLight>(options);
}

// The material the layout's 'medium' directive names, or null when it
// names none.
//
// Only the definition is resolved here: every camera path evaluates its
// own instance at its head, at the path's wavelengths and time (see
// 'PathWalk::trace'), which is what the homogeneity proof's contract asks
// of an instance the closed forms read.
[[nodiscard]] const smdl::JIT::MaterialDef *
resolveExteriorMedium(const Options &opts, const Layout &layout,
                      smdl::Compiler &compiler) {
  if (layout.exteriorMediumName.empty()) return nullptr;
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
  SMDL_LOG_INFO("Exterior medium: ", smdl::Quoted(layout.exteriorMediumName));
  return materialDef;
}

// The exterior haze on each render grid, empty when the scene has none.
//
// The analytic exponential-height atmosphere that produces aerial
// perspective, whose extinction, transmittance and free-flight distance
// are all closed form, so it costs no tracking and no majorant. It is the
// medium of everything outside all geometry, which is where the 'medium'
// directive puts its material too, so the two cannot both be asked for.
[[nodiscard]] std::vector<smdl::Haze>
buildHazes(const Options &opts, const Layout &layout,
           const smdl::JIT::MaterialDef *exteriorMediumDef,
           const Color &wavelengths) {
  const LayoutHaze &fileHaze{layout.haze};
  bool isEnabled{opts.light.haze.isEnabled || layout.hasHaze};
  if (pick(opts.light.haze.none, fileHaze.none)) isEnabled = false;
  if (!isEnabled) return {};
  if (exteriorMediumDef)
    throw smdl::Error("The exterior haze and the 'medium' directive both "
                      "describe the medium outside all geometry; keep one");
  smdl::HazeOptions options{};
  // An unwritten visibility follows the sky's, so that distant terrain
  // does not read hazier or clearer than the horizon sky immediately
  // behind it. The two models overlap toward the sky; see `LayoutHaze`.
  options.visibility = pick(opts.light.haze.visibility, fileHaze.visibility);
  if (!(options.visibility > 0.0f))
    options.visibility = pick(opts.light.sky.visibility, layout.sky.visibility);
  options.scaleHeight = pick(opts.light.haze.scaleHeight, fileHaze.scaleHeight);
  if (fileHaze.baseHeight) options.baseHeight = *fileHaze.baseHeight;
  if (fileHaze.droplet) options.dropletSize = *fileHaze.droplet;
  std::vector<smdl::Haze> hazes{};
  for (const auto &grid : gRenderGrid.grids)
    hazes.emplace_back(options,
                       smdl::Span<const float>(grid.wavelengths.data(),
                                               grid.wavelengths.size()),
                       makeRenderState(wavelengths).metersPerSceneUnit);
  SMDL_LOG_INFO("Exterior haze: visibility ", options.visibility,
                " km, scale height ", options.scaleHeight, " m");
  return hazes;
}

// The sensor's response, or null for the observer.
[[nodiscard]] const ResponseSettings *responseSettingsOf(const Frame &frame) {
  return frame.model.hasSensor() ? &frame.model.sensor->settings().response
                                 : nullptr;
}

// The grid `name` names among a resumed file's records.
//
// \throws smdl::Error  If the file states no grid for it, which is a file
//                      rendered through a different tile.
[[nodiscard]] const GridHeader::Grid &
findResumedGrid(const std::vector<GridHeader::Grid> &records,
                const std::string &name) {
  const auto itr{std::find_if(
      records.begin(), records.end(),
      [&](const GridHeader::Grid &record) { return record.name == name; })};
  if (itr == records.end())
    throw smdl::Error(smdl::concat(
        "Cannot resume: the file states no grid for the tile band ",
        smdl::Quoted(name)));
  return *itr;
}

// The one grid's wavelengths as a resumed file states them: the format's
// list, which the observer's film carries, or the record's, which a band
// film states in its place, its own bands being the sensor's.
[[nodiscard]] const std::vector<float> &
resumedFileWavelengths(const ResumedSequence &resumed) {
  const std::vector<GridHeader::Grid> &records{resumed.grids.grids};
  return resumed.info.wavelengths.empty() && !records.empty()
             ? records.front().wavelengths
             : resumed.info.wavelengths;
}

// The grids a render runs on, before they are installed: one, or under a
// tile one per band the tile lays down, plus the tile as grid indices,
// which is the sensor's tile with each band replaced by the index of its
// grid.
struct GridFamily final {
  std::vector<WavelengthCells> cells{};
  size_t tileColumns{};
  std::vector<size_t> tile{};

  // Did placing them already say what they are? A grid a sensor's curves
  // place describes itself band by band as it is placed; every other
  // choice is announced afterward.
  bool wasLogged{};
};

// Choose the grids, in priority order: explicit '-wavelengths';
// '-wavelength-range' uniform bands (endpoint-inclusive); when resuming
// with no grid flags at all, the grid recorded in the resumed file, so a
// resumed render needs no grid retyping; else the grid the renderer places
// itself, '-wavelength-count' bands: by a physical sensor's curves when
// every sample projects onto every band, uniform over their span under a
// tile, whose per-band grids are the render loop's, or uniform over the
// visible default.
//
// \throws smdl::Error  If the resumed file's grid cannot be adopted.
[[nodiscard]] GridFamily chooseGridFamily(const Options &opts,
                                          const Frame &frame,
                                          const ResumedSequence &resumed) {
  const GridFlags &gridFlags{opts.render.grid};
  const ResponseSettings *response{responseSettingsOf(frame)};
  const std::vector<GridHeader::Grid> &records{resumed.grids.grids};
  const bool isFilePerBand{!records.empty() && !records.front().name.empty()};
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
  GridFamily result{};
  const auto placeTile{[&]() {
    const std::vector<size_t> bands{tileBands(response->cfa)};
    result.tileColumns = response->cfaColumns;
    for (const auto index : response->cfa)
      result.tile.push_back(
          size_t(std::find(bands.begin(), bands.end(), index) - bands.begin()));
  }};
  if (!gridFlags.explicitWavelengths.empty()) {
    result.cells.push_back(WavelengthCells::fromWavelengths(
        asSpan(gridFlags.explicitWavelengths)));
    return result;
  }
  if (gridFlags.range.wasGiven) {
    result.cells.push_back(uniform(gridFlags.range.value));
    return result;
  }
  if (!gridFlags.wasGiven && resumed.wasLoaded) {
    if (isFilePerBand) {
      if (!response || !response->hasCFA())
        throw smdl::Error(
            "Cannot resume: the file holds a grid per tile band, and this "
            "camera's sensor has no tile; render through the sensor the "
            "file was rendered through");
      for (const auto index : tileBands(response->cfa)) {
        const GridHeader::Grid &record{
            findResumedGrid(records, response->bands[index].name)};
        result.cells.push_back(
            WavelengthCells{record.bandEdges, record.wavelengths});
      }
      placeTile();
    } else {
      const std::vector<float> &fileWavelengths{
          resumedFileWavelengths(resumed)};
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
        result.cells.push_back(
            WavelengthCells::fromWavelengths(asSpan(fileWavelengths)));
      else
        result.cells.push_back(
            WavelengthCells{records.front().bandEdges, fileWavelengths});
    }
    for (const auto &cells : result.cells)
      if (!cells.isValid())
        throw smdl::Error("Cannot resume: the file's band edges do not "
                          "describe its wavelengths");
    return result;
  }
  WavelengthRange range{gridFlags.range.value};
  if (gridFlags.count.wasGiven) range.bandCount = gridFlags.count.value;
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
      result.cells.push_back(bandWavelengthCells(band, count));
      SMDL_LOG_INFO("Wavelength grid: ", count,
                    " bands placed by the sensor's ", smdl::Quoted(band.name),
                    " curve for its pixels over ",
                    describeCells(result.cells.back()));
    }
    placeTile();
    result.wasLogged = true;
  } else if (response) {
    result.cells.push_back(responseWavelengthCells(*response, count));
    SMDL_LOG_INFO("Wavelength grid: ", count,
                  " bands placed by the sensor's curves over ",
                  describeCells(result.cells.back()));
    result.wasLogged = true;
  } else {
    result.cells.push_back(uniform(range));
  }
  return result;
}

// Refuse a resumed file whose grid is not the one `gRenderGrid` now holds.
//
// Adoption took the grid from the file, so this can only fail for a
// session that stated grid flags of its own; it has to land on the same
// grids the samples already in the film were drawn on.
//
// \throws smdl::Error  On any disagreement.
void refuseResumedGridMismatch(const Frame &frame,
                               const ResumedSequence &resumed) {
  if (!resumed.wasLoaded) return;
  const ResponseSettings *response{responseSettingsOf(frame)};
  const std::vector<GridHeader::Grid> &records{resumed.grids.grids};
  const bool isFilePerBand{!records.empty() && !records.front().name.empty()};
  // The observer's film holds the grid's bands; a band film holds the
  // sensor's, which the resume already held to the response's.
  if (!response && resumed.film.getNumBands() != gRenderGrid.numBands)
    throw smdl::Error(
        smdl::concat("Cannot resume: the file has ", resumed.film.getNumBands(),
                     " bands against the renderer's ", gRenderGrid.numBands));
  if (isFilePerBand != gRenderGrid.hasTile() ||
      (isFilePerBand && records.size() != gRenderGrid.grids.size()))
    throw smdl::Error(smdl::concat(
        "Cannot resume: the file was rendered on ",
        isFilePerBand ? "a grid per tile band" : "one grid",
        " and this session renders on ",
        gRenderGrid.hasTile() ? "a grid per tile band" : "one grid",
        "; give the grid flags the first session was given, or none"));
  // Half a nanometer of slack on both, which is finer than any grid the
  // renderer places and coarser than the round trip through the header's
  // decimal text.
  const auto check{[](const WavelengthGrid &grid,
                      const std::vector<float> &fileWavelengths,
                      const std::vector<double> &fileEdges) {
    for (size_t i = 0; i < grid.size(); i++)
      if (i >= fileWavelengths.size() ||
          !(std::abs(fileWavelengths[i] - grid.wavelengths[i]) < 0.5f))
        throw smdl::Error("Cannot resume: the wavelength grid does not match "
                          "the renderer's");
    for (size_t i = 0; i < fileEdges.size(); i++)
      if (i >= grid.bandEdges.size() ||
          !(std::abs(fileEdges[i] - grid.bandEdges[i]) < 0.5))
        throw smdl::Error("Cannot resume: the file's band edges do not match "
                          "the renderer's");
  }};
  if (!isFilePerBand) {
    check(gRenderGrid.first(), resumedFileWavelengths(resumed),
          records.empty() ? std::vector<double>{} : records.front().bandEdges);
    return;
  }
  const std::vector<size_t> bands{tileBands(response->cfa)};
  for (size_t k = 0; k < bands.size(); k++) {
    const GridHeader::Grid &record{
        findResumedGrid(records, response->bands[bands[k]].name)};
    check(gRenderGrid.grids[k], record.wavelengths, record.bandEdges);
  }
}

// What the installed grid is worth saying about it, once: what it is when
// placing it did not already say, that a very wide grid is slow, that it
// leaves the visible, and what the accumulation buffers will cost.
void logGridAdvisories(const Options &opts, const Frame &frame,
                       const ResumedSequence &resumed, bool wasLogged) {
  const bool shouldAdoptResumedGrid{!opts.render.grid.wasGiven &&
                                    resumed.wasLoaded};
  const Color wavelengths{gRenderGrid.wavelengths()};
  if ((opts.render.grid.wasGiven || shouldAdoptResumedGrid) && !wasLogged) {
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
  // dark image. Every later stage reads the same flag off the grid.
  if (gRenderGrid.isBeyondVisible)
    SMDL_LOG_WARN(
        "The wavelength grid leaves the visible (380-780nm): RGB colors, "
        "textures, and images extend flat from their 380 and 780nm values "
        "(a convention, not data), metal IOR tables clamp to their measured "
        "ranges, and the RGB outputs project through CIE color matching, so "
        "they darken wherever the grid misses the visible; the ENVI output "
        "is the radiometric record");
  // The accumulation buffers scale as bands times pixels, the film's bands
  // being the grid's for the observer and the sensor's through a sensor;
  // say so before allocating gigabytes.
  const ResponseSettings *response{responseSettingsOf(frame)};
  const double filmBytes{
      response ? 8.0 * double(response->hasCFA() ? 1 : response->bands.size())
               : 8.0 * double(wavelengths.size())};
  if (const double gib{double(frame.numPixelsX * frame.numPixelsY) *
                       (8.0 + filmBytes +
                        (opts.render.guide.isEnabled
                             ? 16.0 * double(wavelengths.size()) + 24.0
                             : 0.0)) /
                       (1024.0 * 1024.0 * 1024.0)};
      gib > 1.0)
    SMDL_LOG_INFO("Accumulation buffers: ", gib, " GiB");
}

} // namespace

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
void resolveWavelengthGrid(const Options &opts, const Frame &frame,
                           const ResumedSequence &resumed) {
  GridFamily family{chooseGridFamily(opts, frame, resumed)};
  const bool wasGridLogged{family.wasLogged};
  // Whether every sample draws its own grid within the cells: on for a
  // physical sensor unless told otherwise, since its bands may be
  // narrower than the grid's spacing.
  const bool shouldJitter{frame.shouldJitterWavelength};
  if (shouldJitter && !opts.render.grid.shouldJitter.wasGiven)
    SMDL_LOG_INFO("Wavelength jitter: on for a physical sensor, so a band "
                  "narrower than the grid's spacing integrates without "
                  "aliasing; -wavelength-jitter=false turns it off");
  // The band count has to land before the first `Color` is built, since
  // that is what sizes it.
  gRenderGrid.reset(std::move(family.cells), family.tileColumns,
                    std::move(family.tile), shouldJitter);
  if (shouldJitter && !gRenderGrid.isJittering)
    SMDL_LOG_WARN("-wavelength-jitter needs at least 2 bands to have a "
                  "band width to jitter within, so it does nothing here");
  refuseResumedGridMismatch(frame, resumed);
  logGridAdvisories(opts, frame, resumed, wasGridLogged);
}

void setUpCompiler(const Options &opts, const Frame &frame,
                   smdl::Compiler &compiler) {
  compiler.wavelengthBaseMax = uint32_t(gRenderGrid.numBands);
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
                         smdl::Compiler &compiler) {
  const Layout &layout{frame.layout};
  const Color wavelengths{gRenderGrid.wavelengths()};
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
  const GroundPlane ground{addGround(opts, layout, *scene)};
  if (ground.guideBound) {
    guideBound = *ground.guideBound;
    hasValidGuideBounds = true;
  }
  narrowDesiredMaterials(opts, layout, *scene, fallbackMaterial, compiler);
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
  //
  // What both ask of a prescription is what it does focused at infinity,
  // since the focus a real lens takes is the distance these very
  // measurements are about to choose, so one solve answers both.
  std::optional<Lens> probe{};
  if (frame.model.cameraOptions.lens &&
      (opts.camera.autolook.isEnabled || frame.model.shouldAutofocus))
    probe.emplace(*frame.model.cameraOptions.lens, LensOptions{});
  std::optional<float> autolookSunAzimuth{};
  if (opts.camera.autolook.isEnabled)
    autolookSunAzimuth = solveAutolookInto(opts, frame, *scene, ground.instance,
                                           probe ? &*probe : nullptr);
  if (frame.model.shouldAutofocus)
    solveAutofocusInto(frame, *scene, probe ? &*probe : nullptr);
  if (!frame.camera) frame.camera.emplace(buildCamera(frame.model));
  envLight = buildEnvLight(opts, layout, autolookSunAzimuth);
  exteriorMediumDef = resolveExteriorMedium(opts, layout, compiler);
  hazes = buildHazes(opts, layout, exteriorMediumDef, wavelengths);
  // Every light in one selection path: each emissive mesh instance plus
  // the environment, weighted by power.
  smdl::ProfilerEntry *profLightSampler{
      smdl::profilerEntryBegin("Build light sampler")};
  lights.emplace(compiler, *scene, envLight.get(), layout.lights, wavelengths,
                 opts.render.useAllLights, !opts.render.noLightTree);
  smdl::profilerEntryEnd(profLightSampler);
}
