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
/// body they came from. The readout's units are the same field.
constexpr const char *ENVI_BAND_UNITS{"band units"};
constexpr const char *ENVI_SENSOR_NAME{"render sensor name"};
constexpr const char *DIGITAL_NUMBER_UNITS{"DN"};

/// A shutter for the log, in seconds and as a reciprocal.
[[nodiscard]] std::string spellShutter(double seconds) {
  return seconds > 0 && seconds < 1
             ? smdl::concat(smdl::Brief(seconds, 4), " s (1/",
                            smdl::Brief(1.0 / seconds, 4), ")")
             : smdl::concat(smdl::Brief(seconds, 4), " s");
}

/// The log line that says why the shot reads out at its ISO: the meter's
/// reading against the base and the top, the shutter or the stop that
/// would meter into range when the frame is outside it, and where a
/// stated ISO or a fixed gain's speed sits against the meter.
void logISO(const Sensor &sensor, const std::optional<float> &stated,
            const MeteredExposure &metered, const DetectorShot &shot) {
  const bool isDark{!(metered.luxSeconds > 0)};
  const std::string reading{
      isDark
          ? std::string("the film is dark, so the meter reads nothing")
          : smdl::concat("the meter reads ", smdl::Brief(metered.luxSeconds, 4),
                         " lux-seconds over the window and asks for ISO ",
                         smdl::Brief(metered.wantedISO, 5))};
  // Where a given ISO leaves the frame against the meter's wish.
  const auto against{[&](double iso) {
    if (isDark) return std::string();
    const double stops{std::log2(double(iso) / metered.wantedISO)};
    return smdl::concat(", so the frame comes out ",
                        smdl::Brief(std::abs(stops), 3), " stops ",
                        stops >= 0 ? "brighter" : "darker", " than metered");
  }};
  const double iso{shot.iso};
  if (sensor.hasFixedGain()) {
    SMDL_LOG_INFO("ISO: ", smdl::Brief(iso, 5),
                  ", the saturation speed of the stated gain; ", reading,
                  against(iso));
    return;
  }
  if (stated) {
    SMDL_LOG_INFO("ISO: ", smdl::Brief(iso, 6), " stated; ", reading,
                  against(iso));
    if (iso < sensor.baseISO())
      SMDL_LOG_WARN("ISO ", smdl::Brief(iso, 6), " is below the base ISO of ",
                    smdl::Brief(sensor.baseISO(), 5),
                    ", so the well clips before the ADC does");
    else if (iso > sensor.maxISO())
      SMDL_LOG_WARN("ISO ", smdl::Brief(iso, 6), " is above the top ISO of ",
                    smdl::Brief(sensor.maxISO(), 6));
    return;
  }
  if (metered.stopsOff != 0) {
    // The exposure that would meter to the end of the range the frame
    // ran past: the shutter scaled by the ISO ratio, or the stop by its
    // square root.
    const bool isOver{metered.isOverexposed()};
    const double end{isOver ? sensor.baseISO() : sensor.maxISO()};
    const std::string fix{
        isDark ? std::string()
               : smdl::concat(
                     ": a shutter of ",
                     spellShutter(shot.exposure * metered.wantedISO / end),
                     ", or f/",
                     smdl::Brief(
                         shot.fNumber * std::sqrt(end / metered.wantedISO), 4),
                     ", would meter to it")};
    SMDL_LOG_WARN(
        "ISO ", smdl::Brief(metered.iso, 6),
        isOver ? ", the base; " : ", the top; ", reading, ", so the frame is ",
        isDark ? std::string("dark")
               : smdl::concat(smdl::Brief(std::abs(metered.stopsOff), 3),
                              " stops ", isOver ? "over" : "under", "exposed"),
        " at the ", isOver ? "base" : "top", " ISO", fix);
    return;
  }
  SMDL_LOG_INFO("ISO: ", smdl::Brief(metered.iso, 5), " metered, from ",
                smdl::Brief(metered.luxSeconds, 4),
                " lux-seconds over the window");
}

/// The shot the body takes of the film: the exposure, the temperature,
/// the f-number, and the ISO, stated, the fixed gain's own, or what the
/// film meters to. A body's film is the irradiance it meters; under
/// -ideal the film is the scene radiance, which the model's preview scale
/// turns into the irradiance the body would have metered. The model
/// established the body and the exposure; the f-number comes off the
/// camera, which under -autolook exists only once the scene does. The
/// final picture logs where the ISO came from, and a checkpoint does not.
[[nodiscard]] DetectorShot takeShot(const Frame &frame, const Sensor &sensor,
                                    const smdl::SpectralFilm &film,
                                    const Color &wavelengths, bool shouldLog) {
  SMDL_SANITY_CHECK(frame.camera && gRenderShutter.hasExposure());
  const CameraModel &model{frame.model};
  DetectorShot shot{};
  shot.exposure = gRenderShutter.exposure;
  shot.temperature = model.temperature;
  shot.fNumber = frame.camera->fNumber();
  const double irradianceScale{model.sensor ? 1.0
                                            : model.previewIrradianceScale};
  const MeteredExposure metered{sensor.meter(film, wavelengths, frame.window,
                                             shot.exposure * irradianceScale)};
  shot.wasISOMetered = !model.iso && !sensor.hasFixedGain();
  shot.iso = sensor.hasFixedGain() ? sensor.fixedGainISO()
             : model.iso           ? double(*model.iso)
                                   : metered.iso;
  if (shouldLog) logISO(sensor, model.iso, metered, shot);
  return shot;
}

/// The observer's picture under -ideal, exposed as the body it stands in
/// for would expose it: the ISO the body would take, and the gain that
/// puts a unit of the film where `developedLuminance()` puts the
/// focal-plane exposure the body's optics would have made of it. The
/// observer's develop is measured rather than assumed: the gain is taken
/// against what the JIT gives a flat spectrum on this grid, whose
/// luminance the meter's weights state, so whatever separates the two
/// integrations (the observer taken at each wavelength or averaged over
/// each band) cancels.
void exposePreview(const Frame &frame, const smdl::Compiler &compiler,
                   const smdl::SpectralFilm &film, const Color &wavelengths,
                   std::vector<float> &rgbImage, bool shouldLog) {
  const CameraModel &model{frame.model};
  const Sensor &sensor{*model.physics};
  const DetectorShot shot{
      takeShot(frame, sensor, film, wavelengths, shouldLog)};
  Color flat{};
  for (size_t i = 0; i < flat.size(); i++) flat[i] = 1;
  const float3 rgb{
      compiler.convertColorToRGB(makeRenderState(wavelengths), flat.data())};
  const double developed{0.2126 * double(rgb[0]) + 0.7152 * double(rgb[1]) +
                         0.0722 * double(rgb[2])};
  double nits{};
  for (const auto weight : Sensor::luminanceWeights(wavelengths))
    nits += LUMENS_PER_WATT * weight;
  const double gain{
      developed > 0
          ? developedLuminance(
                shot.iso, shot.exposure * model.previewIrradianceScale * nits) /
                developed
          : 0.0};
  for (auto &value : rgbImage) value = float(double(value) * gain);
  if (shouldLog)
    SMDL_LOG_INFO("Preview: the observer's picture exposed as the body's "
                  "develop would expose it, times ",
                  smdl::Brief(gain, 4), " (", smdl::Brief(std::log2(gain), 3),
                  " EV), so that a neutral at the meter's aim lands on ",
                  smdl::Brief(DEVELOP_MIDDLE_GRAY, 3));
}

/// What the body makes of the films before there is a picture: the
/// detector at the shot's ISO and the readout it takes of the band
/// film. The checkpoint's preview and the final write both go through
/// here, so that the two read the body the same way and differ only in
/// what they say about it.
struct BodyReadout final {
  Detector detector;
  Readout readout;
};

[[nodiscard]] BodyReadout
readOutBody(const Frame &frame, const smdl::SpectralFilm &film,
            const smdl::SpectralFilm &bandFilm, const Color &wavelengths,
            const DetectorReadoutOptions &options, bool shouldLog) {
  const Sensor &sensor{*frame.model.physics};
  const Detector detector{
      sensor, takeShot(frame, sensor, film, wavelengths, shouldLog)};
  if (shouldLog) detector.logSummary();
  Readout readout{detector.readOut(bandFilm, options, frame.window)};
  return BodyReadout{detector, std::move(readout)};
}

} // namespace

std::vector<float> developPreview(const Options &opts, const Frame &frame,
                                  const ResolvedGrid &grid,
                                  smdl::Compiler &compiler,
                                  const smdl::SpectralFilm &film,
                                  const smdl::SpectralFilm *bandFilm) {
  if (!frame.model.sensor) {
    std::vector<float> rgbImage{
        resolveRGB(compiler, film, grid.wavelengths, opts.image.rgbPolicy)};
    if (frame.model.previewedSensor)
      exposePreview(frame, compiler, film, grid.wavelengths, rgbImage, false);
    return rgbImage;
  }
  SMDL_SANITY_CHECK(bandFilm);
  DetectorReadoutOptions noiseless{opts.image.readout};
  noiseless.noise = DetectorNoise::NONE;
  const BodyReadout body{
      readOutBody(frame, film, *bandFilm, grid.wavelengths, noiseless, false)};
  return developReadout(*frame.model.physics, body.detector, body.readout,
                        frame.model.whiteBalance, frame.window, false);
}

void writeOutputs(const Options &opts, const Frame &frame,
                  const ResolvedGrid &grid, smdl::Compiler &compiler,
                  const EnvLight *envLight, const smdl::SpectralFilm &film,
                  const Response *response, const smdl::SpectralFilm *bandFilm,
                  ResumedSequence &resumed, const std::string &outputSpectrum,
                  const STree *sdtree) {
  SMDL_SANITY_CHECK(!bandFilm || response);
  const Color &wavelengths{grid.wavelengths};
  const CameraModel &model{frame.model};
  const size_t numPixelsX{frame.numPixelsX};
  const size_t numPixelsY{frame.numPixelsY};
  const int4 window{frame.window};
  const size_t spp{frame.spp};
  // Whether every sample drew its own wavelength grid, which a resumed
  // session compares against its own.
  const bool shouldJitterWavelength{!gRenderGrid.bandEdges.empty()};
  // The tally accumulated above is the sequence's, but the fingerprint
  // is this session's: the settings a later resume compares itself
  // against are the ones the samples now in the film were drawn under.
  resumed.header.sampler = SAMPLER_VERSION;
  resumed.header.hasWavelengthJitter = shouldJitterWavelength;
  resumed.header.args = opts.argsEcho;
  resumed.header.quantity = filmQuantityName(model.filmQuantity());
  // The response's fingerprint, which every film beside the spectral one
  // carries, the readout included, and the body's name for the reader.
  std::vector<std::string> responseLines{};
  if (response) {
    ResponseHeader responseHeader{};
    responseHeader.hash = response->hash();
    responseHeader.cfaColumns = response->tileColumns();
    responseHeader.cfa = response->tileNames();
    responseLines = responseHeader.headerLines();
    if (model.sensor && !model.sensor->name.empty())
      responseLines.push_back(
          smdl::concat(ENVI_SENSOR_NAME, " = ", model.sensor->name));
  }
  // The picture as linear sRGB: the observer's develop of the spectral
  // film, exposed under -ideal as the body it stands in for would expose
  // it, or the body's develop of its readout. The meter reads the film, the ISO
  // follows (stated, metered, or the fixed gain's own), the readout
  // reads the band film out at it, onto its own pair under the usual
  // discipline when asked for, and the develop makes the picture of it.
  std::vector<float> rgbImage{};
  if (model.sensor) {
    SMDL_SANITY_CHECK(bandFilm);
    const BodyReadout body{readOutBody(frame, film, *bandFilm, wavelengths,
                                       opts.image.readout, true)};
    const Detector &detector{body.detector};
    const Readout &readout{body.readout};
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
    rgbImage = developReadout(*model.physics, detector, readout,
                              model.whiteBalance, window, true);
  } else {
    rgbImage = resolveRGB(compiler, film, wavelengths, opts.image.rgbPolicy);
    if (model.previewedSensor)
      exposePreview(frame, compiler, film, wavelengths, rgbImage, true);
  }
  {
    // Both RGB outputs see the same filtered pixels, and neither the
    // spectral file below nor the film it comes from sees any of it.
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
  if (!opts.image.outputRGBFloat.empty()) {
    if (std::optional<smdl::Error> error{
            smdl::writeFloatImage(opts.image.outputRGBFloat, int(numPixelsX),
                                  int(numPixelsY), 3, rgbImage.data())}) {
      error->print();
    }
  }
  if (!outputSpectrum.empty()) {
    // Write through a temporary and rename, so an interrupted write
    // cannot destroy the file a resumed session reads from, which may
    // be this very path.
    const std::string partName{outputSpectrum + ".part"};
    // What the numbers mean, and where the light came from: written for
    // whoever opens the file next, and never read back, so none of it
    // joins the fingerprint a resumed session compares.
    std::vector<std::string> headerLines{resumed.header.headerLines()};
    headerLines.push_back(
        smdl::concat(ENVI_RADIOMETRIC_UNITS, " = ",
                     model.filmQuantity() == FilmQuantity::IRRADIANCE
                         ? SPECTRAL_IRRADIANCE_UNITS
                         : SPECTRAL_RADIANCE_UNITS));
    {
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
    }
    // The window the recorded count belongs to, which the film itself
    // does not know: a windowed render still carries a full frame of
    // pixels, and the header must not describe the untouched ones as
    // samples.
    film.writeENVIFile(wavelengths, partName, headerLines, window, {},
                       opts.image.shouldWriteDouble);
    // Both members of the ENVI pair; `writeENVIFile()` wrote them under
    // the temporary name and its own '.hdr' suffix.
    smdl::renameOnto(partName, outputSpectrum);
    smdl::renameOnto(partName + ".hdr", outputSpectrum + ".hdr");
    if (sdtree) {
      // The guide tree rides beside the accumulation with the same
      // temporary-and-rename discipline, stamped with the merged sample
      // count so a resumed session can tell how far behind a stale tree
      // is.
      const std::string treeName{outputSpectrum +
                                 std::string(GUIDE_TREE_EXTENSION)};
      const std::string treePartName{treeName + ".part"};
      sdtree->writeFile(treePartName, resumed.info.samplesPerPixel + spp);
      smdl::renameOnto(treePartName, treeName);
      SMDL_LOG_INFO("Wrote guide tree: ", smdl::Quoted(treeName), ", ",
                    sdtree->leafCount(), " spatial leaves");
    }
    if (bandFilm) {
      // The band film beside the spectral one, under the same
      // discipline: the sequence's own fingerprint, so the pair stands
      // on its own, then the response's, which a resume must match, and
      // the reader-only lines.
      const std::string bandName{bandFilmFileName(outputSpectrum)};
      const std::string bandPartName{bandName + ".part"};
      std::vector<std::string> bandLines{resumed.header.headerLines()};
      for (const auto &line : responseLines) bandLines.push_back(line);
      bandLines.push_back(smdl::concat(ENVI_BAND_UNITS, " = ", BAND_UNITS));
      const std::vector<std::string> &bandNames{response->filmBandNames()};
      bandFilm->writeENVIFile({}, bandPartName, bandLines, window, bandNames,
                              opts.image.shouldWriteDouble);
      smdl::renameOnto(bandPartName, bandName);
      smdl::renameOnto(bandPartName + ".hdr", bandName + ".hdr");
      SMDL_LOG_INFO("Wrote the band film: ", smdl::Quoted(bandName), ", ",
                    smdl::Counted(bandNames.size(), "band"), " in ",
                    BAND_UNITS);
    }
    SMDL_LOG_INFO(
        "Cumulative render time: ", formatDuration(resumed.header.seconds),
        " wall, ", formatDuration(resumed.header.cpuSeconds), " compute over ",
        smdl::Counted(resumed.header.sessions, "session"));
  }
  {
    const std::vector<uint8_t> ldrImage{
        tonemap(opts.image.tonemap, rgbImage, film, wavelengths)};
    if (std::optional<smdl::Error> error{
            smdl::write8bitImage(opts.image.outputRGB, int(numPixelsX),
                                 int(numPixelsY), 3, ldrImage.data())}) {
      error->print();
    }
  }
}
