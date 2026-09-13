#include <algorithm>
#include <cmath>
#include <optional>
#include <string>

#include "smdl/Support/Filesystem.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Strings.h"

#include "CameraModel.h"
#include "MedianFilter.h"
#include "Options.h"
#include "Output.h"
#include "Progress.h"
#include "Render/Guiding.h"
#include "Render/Light.h"
#include "Render/Sampler.h"
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
                   std::vector<float> &rgbImage, bool shouldLog) {
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
  if (shouldLog)
    SMDL_LOG_INFO("Preview: the observer's picture exposed as the sensor's "
                  "develop would expose it, times ",
                  smdl::Brief(gain, 4), " (", smdl::Brief(std::log2(gain), 3),
                  " EV), so that a neutral at the meter's aim lands on ",
                  smdl::Brief(DEVELOP_MIDDLE_GRAY, 3));
}

/// What the sensor makes of the films before there is a picture: the
/// detector at the shot's ISO and the readout it takes of the band
/// film. The checkpoint's preview and the final write both go through
/// here, so that the two read the sensor the same way and differ only in
/// what they say about it.
struct SensorReadout final {
  Detector detector;
  Readout readout;
};

[[nodiscard]] SensorReadout readOutSensor(const Frame &frame,
                                          const RenderHeader &header,
                                          const smdl::SpectralFilm &bandFilm,
                                          const DetectorReadoutOptions &options,
                                          bool shouldLog) {
  const Sensor &sensor{*frame.model.sensor};
  const Detector detector{sensor, takeShot(frame, sensor, header)};
  if (shouldLog) detector.logSummary();
  Readout readout{detector.readOut(bandFilm, options, frame.window)};
  return SensorReadout{detector, std::move(readout)};
}

} // namespace

std::vector<float> developPreview(const Options &opts, const Frame &frame,
                                  const ResolvedGrid &grid,
                                  smdl::Compiler &compiler,
                                  const RenderHeader &header,
                                  const smdl::SpectralFilm *film,
                                  const smdl::SpectralFilm *bandFilm) {
  SMDL_SANITY_CHECK((film != nullptr) != (bandFilm != nullptr));
  if (!frame.model.hasPhysicalSensor()) {
    SMDL_SANITY_CHECK(film);
    std::vector<float> rgbImage{
        resolveRGB(compiler, *film, grid.wavelengths, opts.image.rgbPolicy)};
    if (frame.model.hasPreviewedSensor())
      exposePreview(frame, compiler, header, grid.wavelengths, rgbImage, false);
    return rgbImage;
  }
  SMDL_SANITY_CHECK(bandFilm);
  DetectorReadoutOptions noiseless{opts.image.readout};
  noiseless.noise = DetectorNoise::NONE;
  const SensorReadout sensorReadout{
      readOutSensor(frame, header, *bandFilm, noiseless, false)};
  return developReadout(*frame.model.sensor, sensorReadout.detector,
                        sensorReadout.readout, frame.model.whiteBalance,
                        frame.window, false);
}

bool hasFloatImageExtension(const std::string &fileName) {
  return smdl::hasExtension(fileName, ".exr") ||
         smdl::hasExtension(fileName, ".hdr");
}

std::optional<smdl::Error> writeRGBImage(const Options &opts,
                                         const ResolvedGrid &grid,
                                         const smdl::SpectralFilm *film,
                                         const std::string &fileName,
                                         const std::vector<float> &rgb,
                                         size_t numPixelsX, size_t numPixelsY) {
  if (hasFloatImageExtension(fileName))
    return smdl::writeFloatImage(fileName, int(numPixelsX), int(numPixelsY), 3,
                                 rgb.data());
  const std::vector<uint8_t> ldrImage{tonemap(
      opts.image.tonemap, rgb, numPixelsX, numPixelsY, film, grid.wavelengths)};
  return smdl::write8bitImage(fileName, int(numPixelsX), int(numPixelsY), 3,
                              ldrImage.data());
}

void writeOutputs(const Options &opts, const Frame &frame,
                  const ResolvedGrid &grid, smdl::Compiler &compiler,
                  const EnvLight *envLight, const smdl::SpectralFilm *film,
                  const Response *response, const smdl::SpectralFilm *bandFilm,
                  ResumedSequence &resumed, const std::string &outputBands,
                  const STree *sdtree) {
  SMDL_SANITY_CHECK((film != nullptr) != (bandFilm != nullptr));
  SMDL_SANITY_CHECK(!bandFilm || response);
  const Color &wavelengths{grid.wavelengths};
  const CameraModel &model{frame.model};
  const size_t numPixelsX{frame.numPixelsX};
  const size_t numPixelsY{frame.numPixelsY};
  const int4 window{frame.window};
  const size_t spp{frame.spp};
  // Whether every sample drew its own wavelength grid, which a resumed
  // session compares against its own.
  const bool shouldJitterWavelength{gRenderGrid.isJittering};
  // The tally accumulated above is the sequence's, but the fingerprint
  // is this session's: the settings a later resume compares itself
  // against are the ones the samples now in the film were drawn under.
  resumed.header.sampler = SAMPLER_VERSION;
  resumed.header.hasWavelengthJitter = shouldJitterWavelength;
  resumed.header.args = opts.argsEcho;
  resumed.header.quantity = filmQuantityName(model.filmQuantity());
  // The response's fingerprint, which the band film and the readout
  // carry, and the sensor's name for the reader.
  std::vector<std::string> responseLines{};
  if (response) {
    ResponseHeader responseHeader{};
    responseHeader.hash = response->hash();
    responseHeader.cfaColumns = response->tileColumns();
    responseHeader.cfa = response->tileNames();
    responseLines = responseHeader.headerLines();
    if (model.hasPhysicalSensor() && !model.sensor->settings().name.empty())
      responseLines.push_back(
          smdl::concat(ENVI_SENSOR_NAME, " = ", model.sensor->settings().name));
  }
  // The picture as linear sRGB: the observer's develop of the spectral
  // film, exposed under -ideal as the sensor it stands in for would expose
  // it, or the sensor's develop of its readout. The ISO is the shot's
  // (stated, the rung the meter chose before the first sample, or the
  // fixed gain's own), the readout reads the band film out at it, onto
  // its own pair under the usual discipline when asked for, and the
  // develop makes the picture of it.
  std::vector<float> rgbImage{};
  if (model.hasPhysicalSensor()) {
    SMDL_SANITY_CHECK(bandFilm);
    const SensorReadout sensorReadout{readOutSensor(
        frame, resumed.header, *bandFilm, opts.image.readout, true)};
    const Detector &detector{sensorReadout.detector};
    const Readout &readout{sensorReadout.readout};
    SMDL_LOG_INFO(
        "Readout: mean ", smdl::Brief(readout.meanElectrons, 4),
        " e- over the window, ",
        smdl::Brief(100.0 * double(readout.wellCount) /
                        double(std::max<uint64_t>(readout.windowCount, 1)),
                    3),
        "% of pixel bands at the well");
    if (!opts.image.outputDN.empty()) {
      const std::string &dnName{opts.image.outputDN};
      const std::string dnPartName{dnName + ".part"};
      std::vector<std::string> dnLines{resumed.header.headerLines()};
      for (const auto &line : responseLines) dnLines.push_back(line);
      for (auto &line : detector.header(opts.image.readout).headerLines())
        dnLines.push_back(std::move(line));
      dnLines.push_back(
          smdl::concat(ENVI_BAND_UNITS, " = ", DIGITAL_NUMBER_UNITS));
      const std::vector<std::string> &bandNames{response->filmBandNames()};
      smdl::writeENVIFileUInt16(
          smdl::Span<const uint16_t>(readout.digitalNumbers.data(),
                                     readout.digitalNumbers.size()),
          readout.bandCount, readout.pixelCountX, readout.pixelCountY,
          dnPartName,
          smdl::Span<const std::string>(bandNames.data(), bandNames.size()),
          smdl::Span<const std::string>(dnLines.data(), dnLines.size()), window,
          bandFilm->getNumSamples());
      smdl::renameOnto(dnPartName, dnName);
      smdl::renameOnto(dnPartName + ".hdr", dnName + ".hdr");
      SMDL_LOG_INFO("Wrote the readout: ", smdl::Quoted(dnName), ", ",
                    smdl::Counted(bandNames.size(), "band"),
                    " of digital numbers up to ", detector.topCode());
    }
    rgbImage = developReadout(*model.sensor, detector, readout,
                              model.whiteBalance, window, true);
  } else {
    SMDL_SANITY_CHECK(film);
    rgbImage = resolveRGB(compiler, *film, wavelengths, opts.image.rgbPolicy);
    if (model.hasPreviewedSensor())
      exposePreview(frame, compiler, resumed.header, wavelengths, rgbImage,
                    true);
  }
  {
    // The RGB pictures see the filtered pixels, and neither the film
    // written below nor the one it comes from sees any of it.
    const MedianFilterReport report{
        medianFilterRGB(opts.image.medianFilter, rgbImage, numPixelsX, window)};
    if (report.replacedCount > 0) {
      const double sharePixels{100.0 * double(report.replacedCount) /
                               double(report.examinedCount)};
      const double shareEnergy{report.energyTotal > 0
                                   ? 100.0 * report.energyRemoved /
                                         report.energyTotal
                                   : 0.0};
      SMDL_LOG_INFO("Median filter replaced ", report.replacedCount, " of ",
                    report.examinedCount, " pixels (",
                    smdl::Brief(sharePixels, 3), "%) and removed ",
                    smdl::Brief(shareEnergy, 3), "% of the energy");
    }
  }
  if (!outputBands.empty()) {
    // Write through a temporary and rename, so an interrupted write
    // cannot destroy the file a resumed session reads from, which may
    // be this very path.
    const std::string partName{outputBands + ".part"};
    // The sequence's own record first, then what the numbers mean and
    // where the light came from: written for whoever opens the file
    // next, and never read back, so none of it joins the fingerprint a
    // resumed session compares.
    std::vector<std::string> headerLines{resumed.header.headerLines()};
    // The grids' cells, which a resumed session adopts with the
    // wavelengths: for a grid the sensor placed, the list alone does not
    // imply them, and under a tile each pixel holds its own band's grid,
    // so the format's one list is left out and the grids are stated by
    // band. A band film has no wavelength list at all, its bands being
    // the sensor's, so its one grid states its wavelengths beside its
    // edges.
    GridHeader gridHeader{};
    if (gRenderGrid.hasTile()) {
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
    } else {
      GridHeader::Grid &record{gridHeader.grids.emplace_back()};
      record.bandEdges = gRenderGrid.first().bandEdges;
      if (bandFilm)
        record.wavelengths.assign(wavelengths.data(),
                                  wavelengths.data() + wavelengths.size());
    }
    for (const auto &line : gridHeader.headerLines())
      headerLines.push_back(line);
    // The window the recorded count belongs to, which the film itself
    // does not know: a windowed render still carries a full frame of
    // pixels, and the header must not describe the untouched ones as
    // samples.
    if (film) {
      SMDL_SANITY_CHECK(!gRenderGrid.hasTile());
      headerLines.push_back(
          smdl::concat(ENVI_RADIOMETRIC_UNITS, " = ",
                       model.filmQuantity() == FilmQuantity::IRRADIANCE
                           ? SPECTRAL_IRRADIANCE_UNITS
                           : SPECTRAL_RADIANCE_UNITS));
      float azimuthDeg{};
      float elevationDeg{};
      std::vector<float> irradiance{};
      // Only the procedural sun says any of this: an image environment
      // has no sun to place, and moonlight's source is not the sun.
      if (envLight && envLight->sunMetadata(wavelengths, azimuthDeg,
                                            elevationDeg, irradiance)) {
        headerLines.push_back(
            smdl::concat(ENVI_SUN_AZIMUTH, " = ", azimuthDeg));
        headerLines.push_back(
            smdl::concat(ENVI_SUN_ELEVATION, " = ", elevationDeg));
        std::string line{smdl::concat(ENVI_SOLAR_IRRADIANCE, " = {")};
        for (size_t i = 0; i < irradiance.size(); i++)
          line += smdl::concat(i > 0 ? ", " : "", irradiance[i]);
        headerLines.push_back(line + "}");
      }
      film->writeENVIFile(smdl::Span<const float>(wavelengths), partName,
                          headerLines, window, {},
                          opts.image.shouldWriteDouble);
      smdl::renameOnto(partName, outputBands);
      smdl::renameOnto(partName + ".hdr", outputBands + ".hdr");
      SMDL_LOG_INFO("Wrote the film: ", smdl::Quoted(outputBands), ", ",
                    smdl::Counted(wavelengths.size(), "band"), " in ",
                    model.filmQuantity() == FilmQuantity::IRRADIANCE
                        ? SPECTRAL_IRRADIANCE_UNITS
                        : SPECTRAL_RADIANCE_UNITS);
    } else {
      // The band film: the response's fingerprint, which a resume must
      // match, then the reader-only lines, and the bands named in place
      // of a wavelength list.
      for (const auto &line : responseLines) headerLines.push_back(line);
      headerLines.push_back(smdl::concat(ENVI_BAND_UNITS, " = ", BAND_UNITS));
      const std::vector<std::string> &bandNames{response->filmBandNames()};
      bandFilm->writeENVIFile({}, partName, headerLines, window, bandNames,
                              opts.image.shouldWriteDouble);
      smdl::renameOnto(partName, outputBands);
      smdl::renameOnto(partName + ".hdr", outputBands + ".hdr");
      SMDL_LOG_INFO("Wrote the band film: ", smdl::Quoted(outputBands), ", ",
                    smdl::Counted(bandNames.size(), "band"), " in ",
                    BAND_UNITS);
    }
    if (sdtree) {
      // The guide tree rides beside the film with the same
      // temporary-and-rename discipline, stamped with the merged sample
      // count so a resumed session can tell how far behind a stale tree
      // is.
      const std::string treeName{outputBands +
                                 std::string(GUIDE_TREE_EXTENSION)};
      const std::string treePartName{treeName + ".part"};
      sdtree->writeFile(treePartName, resumed.info.samplesPerPixel + spp);
      smdl::renameOnto(treePartName, treeName);
      SMDL_LOG_INFO("Wrote guide tree: ", smdl::Quoted(treeName), ", ",
                    sdtree->leafCount(), " spatial leaves");
    }
    SMDL_LOG_INFO(
        "Cumulative render time: ", formatDuration(resumed.header.seconds),
        " wall, ", formatDuration(resumed.header.cpuSeconds), " compute over ",
        smdl::Counted(resumed.header.sessions, "session"));
  }
  for (const auto &fileName : opts.image.outputRGB) {
    if (std::optional<smdl::Error> error{writeRGBImage(
            opts, grid, film, fileName, rgbImage, numPixelsX, numPixelsY)}) {
      error->print();
    }
  }
}
