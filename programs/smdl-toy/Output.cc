#include <algorithm>
#include <cmath>
#include <optional>
#include <string>

#include "smdl/Support/Filesystem.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Strings.h"

#include "CameraModel.h"
#include "IO/DNG.h"
#include "MedianFilter.h"
#include "Options.h"
#include "Output.h"
#include "Progress.h"
#include "Render/Guiding.h"
#include "Render/Light.h"
#include "Render/Sampler.h"
#include "RenderFilm.h"
#include "Resume.h"
#include "Sensor/Detector.h"
#include "Sensor/Develop.h"
#include "Sensor/Meter.h"
#include "Sensor/Response.h"
#include "Sensor/Sensor.h"
#include "Stage.h"
#include "Tonemap.h"

namespace {

/// The units of what the film holds, by `FilmQuantity`: the observer's
/// spectral radiance, the library-wide convention (see `smdl::SunSky`),
/// or a physical sensor's spectral irradiance at the focal plane.
///
/// \{
constexpr const char *SPECTRAL_RADIANCE_UNITS{"W/(m^2 sr nm)"};
constexpr const char *SPECTRAL_IRRADIANCE_UNITS{"W/(m^2 nm)"};
/// \}

/// The header fields written for a reader rather than for a resume.
/// `radiometric units` is not an ENVI standard field; the three solar
/// ones are, and carry the units ENVI states them in.
constexpr const char *ENVI_RADIOMETRIC_UNITS{"radiometric units"};
constexpr const char *ENVI_SUN_AZIMUTH{"sun azimuth"};
constexpr const char *ENVI_SUN_ELEVATION{"sun elevation"};
constexpr const char *ENVI_SOLAR_IRRADIANCE{"solar irradiance"};

/// The band film's reader-only fields: what its numbers are, and whose
/// sensor they came from. The readout's units are the same field.
constexpr const char *ENVI_BAND_UNITS{"band units"};
constexpr const char *ENVI_SENSOR_NAME{"render sensor name"};
constexpr const char *DIGITAL_NUMBER_UNITS{"DN"};

/// The shot the sensor takes of the film: the exposure, the temperature,
/// the f-number, and the ISO, stated, the fixed gain's own, or the rung
/// the meter chose before the first sample, which `header` carries. The
/// model established the sensor and the exposure; the f-number comes off
/// the camera, which under -autolook exists only once the scene does.
[[nodiscard]] DetectorShot takeShot(const Frame &frame, const Sensor &sensor,
                                    const RenderHeader &header) {
  SMDL_SANITY_CHECK(frame.camera && gRenderShutter.hasExposure());
  const CameraModel &model{frame.model};
  DetectorShot shot{};
  shot.exposure = gRenderShutter.exposure;
  shot.temperature = model.temperature;
  shot.fNumber = frame.camera->fNumber();
  const ShotISO iso{resolveShotISO(sensor, model.iso, header)};
  shot.iso = iso.iso;
  shot.wasISOMetered = iso.wasMetered;
  return shot;
}

/// The observer's picture under -ideal, exposed as the sensor it stands in
/// for would expose it: the ISO the sensor would take, and the gain that
/// puts a unit of the film where `developedLuminance()` puts the
/// focal-plane exposure the sensor's optics would have made of it. The
/// observer's develop is measured rather than assumed: the gain is taken
/// against what the JIT gives a flat spectrum on this grid, whose
/// luminance the meter's weights state, so whatever separates the two
/// integrations (the observer taken at each wavelength or averaged over
/// each band) cancels.
void exposePreview(const Frame &frame, const smdl::Compiler &compiler,
                   const RenderHeader &header, const Color &wavelengths,
                   std::vector<float> &rgbImage, DevelopLogging logging) {
  const CameraModel &model{frame.model};
  const Sensor &sensor{*model.sensor};
  const DetectorShot shot{takeShot(frame, sensor, header)};
  Color flat{};
  for (size_t i = 0; i < flat.size(); i++) flat[i] = 1;
  const float3 rgb{
      compiler.convertColorToRGB(makeRenderState(wavelengths), flat.data())};
  const double developed{0.2126 * double(rgb[0]) + 0.7152 * double(rgb[1]) +
                         0.0722 * double(rgb[2])};
  double nits{};
  for (const auto weight :
       Sensor::luminanceWeights(gRenderGrid.first(), gRenderGrid.isJittering))
    nits += LUMENS_PER_WATT * weight;
  const double gain{
      developed > 0
          ? developedLuminance(
                shot.iso, shot.exposure * model.previewIrradianceScale * nits) /
                developed
          : 0.0};
  for (auto &value : rgbImage) value = float(double(value) * gain);
  if (logging == DevelopLogging::VERBOSE)
    SMDL_LOG_INFO("Preview: the observer's picture exposed as the sensor's "
                  "develop would expose it, times ",
                  SpellFloat(gain, 4), " (", SpellFloat(std::log2(gain), 3),
                  " EV), so that a neutral at the meter's aim lands on ",
                  SpellFloat(DEVELOP_MIDDLE_GRAY, 3));
}

/// What the sensor makes of the films before there is a picture: the
/// detector at the shot's ISO and the readout it takes of the band
/// film. The checkpoint's preview and the final write both go through
/// here, so that the two read the sensor the same way and differ only in
/// what the caller says about it.
struct SensorReadout final {
  Detector detector;
  Readout readout;
  DetectorShot shot;
};

[[nodiscard]] SensorReadout
readOutSensor(const Frame &frame, const RenderHeader &header,
              const smdl::SpectralFilm &bandFilm,
              const DetectorReadoutOptions &options) {
  const Sensor &sensor{*frame.model.sensor};
  const DetectorShot shot{takeShot(frame, sensor, header)};
  const Detector detector{sensor, shot};
  Readout readout{detector.readOut(bandFilm, options, frame.window)};
  return SensorReadout{detector, std::move(readout), shot};
}

/// The response's fingerprint, which the band film and the readout both
/// carry so that a resume can check them, and the sensor's name for the
/// reader. Empty without a response.
[[nodiscard]] std::vector<std::string>
responseHeaderLines(const CameraModel &model, const Response *response) {
  std::vector<std::string> lines{};
  if (!response) return lines;
  ResponseHeader responseHeader{};
  responseHeader.hash = response->hash();
  responseHeader.cfaColumns = response->tileColumns();
  responseHeader.cfa = response->tileNames();
  responseHeader.crosstalk = response->crosstalk();
  lines = responseHeader.headerLines();
  if (model.hasSensor() && !model.sensor->settings().name.empty())
    lines.push_back(
        smdl::concat(ENVI_SENSOR_NAME, " = ", model.sensor->settings().name));
  return lines;
}

/// What the readout made of the film, for the line that reports it.
void logReadout(const Readout &readout) {
  SMDL_LOG_INFO(
      "Readout: mean ", SpellFloat(readout.meanElectrons, 4),
      " e- over the window, ",
      SpellPercent(double(readout.wellCount) /
                   double(std::max<uint64_t>(readout.windowCount, 1))),
      " of pixel bands at the well");
}

/// Write the readout as the extension `-output-raw` carries: the ENVI
/// pair any sensor writes, or the DNG a body writes. Both go through a
/// temporary and a rename, so that an interrupted write cannot destroy a
/// file already there.
void writeReadoutFile(const Options &opts, const Frame &frame,
                      const RenderFilm &target, const RenderHeader &header,
                      const SensorReadout &sensorReadout,
                      const std::vector<std::string> &responseLines) {
  const CameraModel &model{frame.model};
  const Detector &detector{sensorReadout.detector};
  const Readout &readout{sensorReadout.readout};
  const std::string &rawName{opts.image.outputRaw};
  const std::string partName{rawName + ".part"};
  if (smdl::hasExtension(rawName, DNG_EXTENSION)) {
    // The same white balance the develop resolves, which is what makes
    // the file develop elsewhere the way the picture beside it developed
    // here.
    const DevelopFit develop{resolveDevelopFit(
        *model.sensor, detector, readout, model.whiteBalance, frame.window)};
    std::vector<uint16_t> planes{};
    DNGImage image{makeDNGImage(*model.sensor, detector, readout, develop,
                                sensorReadout.shot, frame.window, planes)};
    image.description = smdl::concat(
        "smdl-toy readout, ", detectorNoiseName(opts.image.readout.noise),
        " noise, seed ", opts.image.readout.seed);
    writeDNGFile(partName, image);
    smdl::renameOnto(partName, rawName);
    SMDL_LOG_INFO("Wrote the readout: ", SpellQuoted(rawName),
                  ", a DNG of digital numbers up to ", detector.whiteLevel());
    if (develop.mode != DevelopMode::TRUE_COLOR)
      SMDL_LOG_WARN("The DNG states the fitted matrix all the same, so a "
                    "raw developer will make color of the file where the "
                    "picture beside it is not");
    return;
  }
  std::vector<std::string> rawLines{header.headerLines()};
  for (const auto &line : responseLines) rawLines.push_back(line);
  for (auto &line : detector.header(opts.image.readout).headerLines())
    rawLines.push_back(std::move(line));
  rawLines.push_back(
      smdl::concat(ENVI_BAND_UNITS, " = ", DIGITAL_NUMBER_UNITS));
  const std::vector<std::string> &bandNames{target.response()->filmBandNames()};
  smdl::writeENVIFileUInt16(
      smdl::Span<const uint16_t>(readout.digitalNumbers.data(),
                                 readout.digitalNumbers.size()),
      readout.bandCount, readout.pixelCountX, readout.pixelCountY, partName,
      smdl::Span<const std::string>(bandNames.data(), bandNames.size()),
      smdl::Span<const std::string>(rawLines.data(), rawLines.size()),
      frame.window, target.bandFilm()->getNumSamples());
  smdl::renameOnto(partName, rawName);
  smdl::renameOnto(partName + ".hdr", rawName + ".hdr");
  SMDL_LOG_INFO("Wrote the readout: ", SpellQuoted(rawName), ", ",
                SpellCounted(bandNames.size(), "band"),
                " of digital numbers up to ", detector.topCode());
}

/// The firefly filter over the picture, and the line that reports what it
/// did. The RGB pictures see the filtered pixels, and neither the film
/// written beside them nor the one it comes from sees any of it.
void filterRGB(const MedianFilterOptions &options, std::vector<float> &rgbImage,
               size_t numPixelsX, int4 window) {
  const MedianFilterReport report{
      medianFilterRGB(options, rgbImage, numPixelsX, window)};
  if (report.replacedCount == 0) return;
  const double sharePixels{double(report.replacedCount) /
                           double(report.examinedCount)};
  const double shareEnergy{
      report.energyTotal > 0 ? report.energyRemoved / report.energyTotal : 0.0};
  SMDL_LOG_INFO("Median filter replaced ", report.replacedCount, " of ",
                report.examinedCount, " pixels (", SpellPercent(sharePixels),
                ") and removed ", SpellPercent(shareEnergy), " of the energy");
}

/// The grids' cells, which a resumed session adopts with the wavelengths.
///
/// For a grid the sensor placed, the wavelength list alone does not imply
/// the cells, and under a tile each pixel holds its own band's grid, so
/// the format's one list is left out and the grids are stated by band. A
/// band film has no wavelength list at all, its bands being the sensor's,
/// so its one grid states its wavelengths beside its edges.
[[nodiscard]] GridHeader gridHeaderOf(const RenderFilm &target,
                                      const Color &wavelengths) {
  GridHeader gridHeader{};
  if (gRenderGrid.hasTile()) {
    const Response *response{target.response()};
    SMDL_SANITY_CHECK(response && response->tileBandNames().size() ==
                                      gRenderGrid.grids.size());
    for (size_t k = 0; k < gRenderGrid.grids.size(); k++) {
      const WavelengthGrid &grid{gRenderGrid.grids[k]};
      GridHeader::Grid &record{gridHeader.grids.emplace_back()};
      record.name = response->tileBandNames()[k];
      record.wavelengths.assign(grid.wavelengths.data(),
                                grid.wavelengths.data() + grid.size());
      record.bandEdges = grid.bandEdges;
    }
    return gridHeader;
  }
  GridHeader::Grid &record{gridHeader.grids.emplace_back()};
  record.bandEdges = gRenderGrid.first().bandEdges;
  if (target.bandFilm())
    record.wavelengths.assign(wavelengths.data(),
                              wavelengths.data() + wavelengths.size());
  return gridHeader;
}

/// The reader-only lines of the observer's film: what its numbers are,
/// and where the light came from. Only the procedural sun says any of the
/// latter: an image environment has no sun to place, and moonlight's
/// source is not the sun.
void appendSpectralReaderLines(const CameraModel &model,
                               const EnvLight *envLight,
                               const Color &wavelengths,
                               std::vector<std::string> &headerLines) {
  headerLines.push_back(
      smdl::concat(ENVI_RADIOMETRIC_UNITS, " = ",
                   model.filmQuantity() == FilmQuantity::IRRADIANCE
                       ? SPECTRAL_IRRADIANCE_UNITS
                       : SPECTRAL_RADIANCE_UNITS));
  float azimuthDeg{};
  float elevationDeg{};
  std::vector<float> irradiance{};
  if (!envLight ||
      !envLight->sunMetadata(wavelengths, azimuthDeg, elevationDeg, irradiance))
    return;
  headerLines.push_back(
      smdl::concat(ENVI_SUN_AZIMUTH, " = ", SpellExact(azimuthDeg)));
  headerLines.push_back(
      smdl::concat(ENVI_SUN_ELEVATION, " = ", SpellExact(elevationDeg)));
  std::string line{smdl::concat(ENVI_SOLAR_IRRADIANCE, " = {")};
  for (size_t i = 0; i < irradiance.size(); i++)
    line += smdl::concat(i > 0 ? ", " : "", SpellExact(irradiance[i]));
  headerLines.push_back(line + "}");
}

/// Write the film to `outputBands`, through a temporary and a rename so
/// that an interrupted write cannot destroy the file a resumed session
/// reads from, which may be this very path.
///
/// The sequence's own record goes first, then the grids, then what the
/// numbers mean: the observer's film states its radiometric units and
/// where the sun stood, and a band film states the response's fingerprint
/// and names its bands in place of a wavelength list. Everything past the
/// record is written for whoever opens the file next and never read back,
/// so none of it joins the fingerprint a resumed session compares.
void writeFilmFile(const Options &opts, const Frame &frame,
                   const EnvLight *envLight, const RenderFilm &target,
                   const RenderHeader &header, const std::string &outputBands,
                   const std::vector<std::string> &responseLines) {
  const CameraModel &model{frame.model};
  const Color wavelengths{gRenderGrid.wavelengths()};
  const std::string partName{outputBands + ".part"};
  std::vector<std::string> headerLines{header.headerLines()};
  for (const auto &line : gridHeaderOf(target, wavelengths).headerLines())
    headerLines.push_back(line);
  if (const smdl::SpectralFilm *film{target.spectralFilm()}) {
    SMDL_SANITY_CHECK(!gRenderGrid.hasTile());
    appendSpectralReaderLines(model, envLight, wavelengths, headerLines);
    film->writeENVIFile(smdl::Span<const float>(wavelengths), partName,
                        headerLines, frame.window, {},
                        opts.image.shouldWriteDouble);
    smdl::renameOnto(partName, outputBands);
    smdl::renameOnto(partName + ".hdr", outputBands + ".hdr");
    SMDL_LOG_INFO("Wrote the film: ", SpellQuoted(outputBands), ", ",
                  SpellCounted(wavelengths.size(), "band"), " in ",
                  model.filmQuantity() == FilmQuantity::IRRADIANCE
                      ? SPECTRAL_IRRADIANCE_UNITS
                      : SPECTRAL_RADIANCE_UNITS);
    return;
  }
  for (const auto &line : responseLines) headerLines.push_back(line);
  headerLines.push_back(smdl::concat(ENVI_BAND_UNITS, " = ", BAND_UNITS));
  const std::vector<std::string> &bandNames{target.response()->filmBandNames()};
  target.bandFilm()->writeENVIFile({}, partName, headerLines, frame.window,
                                   bandNames, opts.image.shouldWriteDouble);
  smdl::renameOnto(partName, outputBands);
  smdl::renameOnto(partName + ".hdr", outputBands + ".hdr");
  SMDL_LOG_INFO("Wrote the band film: ", SpellQuoted(outputBands), ", ",
                SpellCounted(bandNames.size(), "band"), " in ", BAND_UNITS);
}

/// Write the guide tree beside the film, with the same
/// temporary-and-rename discipline, stamped with the merged sample count
/// so a resumed session can tell how far behind a stale tree is.
void writeGuideTreeFile(const STree &sdtree, const std::string &outputBands,
                        uint64_t samplesPerPixel) {
  const std::string treeName{outputBands + std::string(GUIDE_TREE_EXTENSION)};
  const std::string treePartName{treeName + ".part"};
  sdtree.writeFile(treePartName, samplesPerPixel);
  smdl::renameOnto(treePartName, treeName);
  SMDL_LOG_INFO("Wrote guide tree: ", SpellQuoted(treeName), ", ",
                sdtree.leafCount(), " spatial leaves");
}

} // namespace

std::vector<float> developPreview(const Options &opts, const Frame &frame,
                                  smdl::Compiler &compiler,
                                  const RenderHeader &header,
                                  const RenderFilm &target) {
  const Color wavelengths{gRenderGrid.wavelengths()};
  if (const smdl::SpectralFilm *film{target.spectralFilm()}) {
    std::vector<float> rgbImage{
        resolveRGB(compiler, *film, wavelengths, opts.image.rgbPolicy)};
    if (frame.model.hasPreviewedSensor())
      exposePreview(frame, compiler, header, wavelengths, rgbImage,
                    DevelopLogging::SILENT);
    return rgbImage;
  }
  const smdl::SpectralFilm *bandFilm{target.bandFilm()};
  DetectorReadoutOptions noiseless{opts.image.readout};
  noiseless.noise = DetectorNoise::NONE;
  const SensorReadout sensorReadout{
      readOutSensor(frame, header, *bandFilm, noiseless)};
  return developReadout(*frame.model.sensor, sensorReadout.detector,
                        sensorReadout.readout, frame.model.whiteBalance,
                        frame.window, DevelopLogging::SILENT);
}

bool hasFloatImageExtension(const std::string &fileName) {
  return smdl::hasExtension(fileName, ".exr") ||
         smdl::hasExtension(fileName, ".hdr");
}

std::optional<smdl::Error> writeRGBImage(const Options &opts,
                                         const RenderFilm &target,
                                         const std::string &fileName,
                                         const std::vector<float> &rgb,
                                         size_t numPixelsX, size_t numPixelsY) {
  if (hasFloatImageExtension(fileName))
    return smdl::writeFloatImage(fileName, int(numPixelsX), int(numPixelsY), 3,
                                 rgb.data());
  const std::vector<uint8_t> ldrImage{
      tonemap(opts.image.tonemap, rgb, numPixelsX, numPixelsY,
              target.spectralFilm(), gRenderGrid.wavelengths())};
  return smdl::write8bitImage(fileName, int(numPixelsX), int(numPixelsY), 3,
                              ldrImage.data());
}

void writeOutputs(const Options &opts, const Frame &frame,
                  smdl::Compiler &compiler, const EnvLight *envLight,
                  const RenderFilm &target, ResumedSequence &resumed,
                  const std::string &outputBands, const STree *sdtree) {
  const CameraModel &model{frame.model};
  // The tally accumulated above is the sequence's, but the fingerprint is
  // this session's: the settings a later resume compares itself against
  // are the ones the samples now in the film were drawn under.
  resumed.header.sampler = SAMPLER_VERSION;
  resumed.header.hasWavelengthJitter = gRenderGrid.isJittering;
  resumed.header.args = opts.argsEcho;
  resumed.header.quantity = filmQuantityName(model.filmQuantity());
  const std::vector<std::string> responseLines{
      responseHeaderLines(model, target.response())};
  // The picture as linear sRGB: a sensor's develop of its readout, which
  // runs whether or not the digital numbers are written since the picture
  // is made from them, or the observer's develop of the spectral film,
  // exposed under -ideal as the sensor it stands in for would expose it.
  std::vector<float> rgbImage{};
  if (target.hasResponse()) {
    const SensorReadout sensorReadout{readOutSensor(
        frame, resumed.header, *target.bandFilm(), opts.image.readout)};
    sensorReadout.detector.logSummary();
    logReadout(sensorReadout.readout);
    if (!opts.image.outputRaw.empty())
      writeReadoutFile(opts, frame, target, resumed.header, sensorReadout,
                       responseLines);
    rgbImage = developReadout(*model.sensor, sensorReadout.detector,
                              sensorReadout.readout, model.whiteBalance,
                              frame.window, DevelopLogging::VERBOSE);
  } else {
    const Color wavelengths{gRenderGrid.wavelengths()};
    rgbImage = resolveRGB(compiler, *target.spectralFilm(), wavelengths,
                          opts.image.rgbPolicy);
    if (model.hasPreviewedSensor())
      exposePreview(frame, compiler, resumed.header, wavelengths, rgbImage,
                    DevelopLogging::VERBOSE);
  }
  filterRGB(opts.image.medianFilter, rgbImage, frame.numPixelsX, frame.window);
  if (!outputBands.empty()) {
    writeFilmFile(opts, frame, envLight, target, resumed.header, outputBands,
                  responseLines);
    if (sdtree)
      writeGuideTreeFile(*sdtree, outputBands,
                         resumed.info.samplesPerPixel + frame.spp);
    SMDL_LOG_INFO(
        "Cumulative render time: ", formatDuration(resumed.header.seconds),
        " wall, ", formatDuration(resumed.header.cpuSeconds), " compute over ",
        SpellCounted(resumed.header.sessions, "session"));
  }
  for (const auto &fileName : opts.image.outputRGB)
    if (std::optional<smdl::Error> error{
            writeRGBImage(opts, target, fileName, rgbImage, frame.numPixelsX,
                          frame.numPixelsY)})
      error->print();
}
