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
  const auto reading{
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
    const auto fix{
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
/// film meters to. The model established the body and the exposure; the
/// f-number comes off the camera, which under -autolook exists only
/// once the scene does. The final picture logs where the ISO came from,
/// and a preview does not.
[[nodiscard]] DetectorShot takeShot(const Frame &frame, const Sensor &sensor,
                                    const smdl::SpectralFilm &film,
                                    const Color &wavelengths, bool shouldLog) {
  SMDL_SANITY_CHECK(frame.camera && gRenderShutter.hasExposure());
  const auto &model{frame.model};
  auto shot{DetectorShot{}};
  shot.exposure = gRenderShutter.exposure;
  shot.temperature = model.temperature;
  shot.fNumber = frame.camera->fNumber();
  const auto metered{
      sensor.meter(film, wavelengths, frame.window, shot.exposure)};
  shot.wasISOMetered = !model.iso && !sensor.hasFixedGain();
  shot.iso = sensor.hasFixedGain() ? sensor.fixedGainISO()
             : model.iso           ? double(*model.iso)
                                   : metered.iso;
  if (shouldLog) logISO(sensor, model.iso, metered, shot);
  return shot;
}

} // namespace

std::vector<float> developPreview(const Options &opts, const Frame &frame,
                                  const ResolvedGrid &grid,
                                  smdl::Compiler &compiler,
                                  const smdl::SpectralFilm &film,
                                  const smdl::SpectralFilm *bandFilm) {
  if (!frame.model.sensor)
    return resolveRGB(compiler, film, grid.wavelengths, opts.image.rgbPolicy);
  SMDL_SANITY_CHECK(bandFilm);
  const Sensor sensor{*frame.model.sensor};
  const Detector detector{
      sensor, takeShot(frame, sensor, film, grid.wavelengths, false)};
  auto noiseless{opts.image.readout};
  noiseless.noise = DetectorNoise::NONE;
  const auto readout{detector.readOut(*bandFilm, noiseless, frame.window)};
  return developReadout(sensor, detector, readout, frame.model.whiteBalance,
                        frame.window, false);
}

void writeOutputs(const Options &opts, const Frame &frame,
                  const ResolvedGrid &grid, smdl::Compiler &compiler,
                  const EnvLight *envLight, const smdl::SpectralFilm &film,
                  const Response *response, const smdl::SpectralFilm *bandFilm,
                  ResumedSequence &resumed, const std::string &outputSpectrum,
                  const STree *sdtree) {
  SMDL_SANITY_CHECK(!bandFilm || response);
  const auto &wavelengths{grid.wavelengths};
  const auto &model{frame.model};
  const auto numPixelsX{frame.numPixelsX};
  const auto numPixelsY{frame.numPixelsY};
  const auto window{frame.window};
  const auto spp{frame.spp};
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
  auto responseLines{std::vector<std::string>()};
  if (response) {
    auto responseHeader{ResponseHeader{}};
    responseHeader.hash = response->hash();
    responseHeader.cfaColumns = response->tileColumns();
    responseHeader.cfa = response->tileNames();
    responseLines = responseHeader.headerLines();
    if (model.sensor && !model.sensor->name.empty())
      responseLines.push_back(
          smdl::concat(ENVI_SENSOR_NAME, " = ", model.sensor->name));
  }
  // The picture as linear sRGB: the observer's develop of the spectral
  // film, or the body's of its readout. The meter reads the film, the ISO
  // follows (stated, metered, or the fixed gain's own), the readout
  // reads the band film out at it, onto its own pair under the usual
  // discipline when asked for, and the develop makes the picture of it.
  auto rgbImage{std::vector<float>()};
  if (model.sensor) {
    SMDL_SANITY_CHECK(bandFilm);
    const Sensor sensor{*model.sensor};
    const Detector detector{sensor,
                            takeShot(frame, sensor, film, wavelengths, true)};
    detector.logSummary();
    const auto readout{detector.readOut(*bandFilm, opts.image.readout, window)};
    SMDL_LOG_INFO(
        "Readout: mean ", smdl::Brief(readout.meanElectrons, 4),
        " e- over the window, ",
        smdl::Brief(100.0 * double(readout.wellCount) /
                        double(std::max<uint64_t>(readout.windowCount, 1)),
                    3),
        "% of pixel bands at the well");
    if (!opts.image.outputDN.empty()) {
      const auto &dnName{opts.image.outputDN};
      const auto dnPartName{dnName + ".part"};
      auto dnLines{resumed.header.headerLines()};
      for (const auto &line : responseLines) dnLines.push_back(line);
      for (auto &line : detector.header(opts.image.readout).headerLines())
        dnLines.push_back(std::move(line));
      dnLines.push_back(
          smdl::concat(ENVI_BAND_UNITS, " = ", DIGITAL_NUMBER_UNITS));
      const auto &bandNames{response->filmBandNames()};
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
                    bandNames.size(), " band(s) of digital numbers up to ",
                    detector.topCode());
    }
    rgbImage = developReadout(sensor, detector, readout, model.whiteBalance,
                              window, true);
  } else {
    rgbImage = resolveRGB(compiler, film, wavelengths, opts.image.rgbPolicy);
  }
  {
    // Both RGB outputs see the same filtered pixels, and neither the
    // spectral file below nor the film it comes from sees any of it.
    const auto report{
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
    if (auto error{smdl::writeFloatImage(opts.image.outputRGBFloat,
                                         int(numPixelsX), int(numPixelsY), 3,
                                         rgbImage.data())}) {
      error->print();
    }
  }
  if (!outputSpectrum.empty()) {
    // Write through a temporary and rename, so an interrupted write
    // cannot destroy the file a resumed session reads from, which may
    // be this very path.
    const auto partName{outputSpectrum + ".part"};
    // What the numbers mean, and where the light came from: written for
    // whoever opens the file next, and never read back, so none of it
    // joins the fingerprint a resumed session compares.
    auto headerLines{resumed.header.headerLines()};
    headerLines.push_back(
        smdl::concat(ENVI_RADIOMETRIC_UNITS, " = ",
                     model.filmQuantity() == FilmQuantity::IRRADIANCE
                         ? SPECTRAL_IRRADIANCE_UNITS
                         : SPECTRAL_RADIANCE_UNITS));
    {
      float azimuthDeg{};
      float elevationDeg{};
      auto irradiance{std::vector<float>()};
      // Only the procedural sun says any of this: an image environment
      // has no sun to place, and moonlight's source is not the sun.
      if (envLight && envLight->sunMetadata(wavelengths, azimuthDeg,
                                            elevationDeg, irradiance)) {
        headerLines.push_back(
            smdl::concat(ENVI_SUN_AZIMUTH, " = ", azimuthDeg));
        headerLines.push_back(
            smdl::concat(ENVI_SUN_ELEVATION, " = ", elevationDeg));
        auto line{smdl::concat(ENVI_SOLAR_IRRADIANCE, " = {")};
        for (size_t i = 0; i < irradiance.size(); i++)
          line += smdl::concat(i > 0 ? ", " : "", irradiance[i]);
        headerLines.push_back(line + "}");
      }
    }
    // The window the recorded count belongs to, which the film itself
    // does not know: a windowed render still carries a full frame of
    // pixels, and the header must not describe the untouched ones as
    // samples.
    film.writeENVIFile(wavelengths, partName, headerLines, window);
    // Both members of the ENVI pair; `writeENVIFile()` wrote them under
    // the temporary name and its own '.hdr' suffix.
    smdl::renameOnto(partName, outputSpectrum);
    smdl::renameOnto(partName + ".hdr", outputSpectrum + ".hdr");
    if (sdtree) {
      // The guide tree rides beside the accumulation with the same
      // temporary-and-rename discipline, stamped with the merged sample
      // count so a resumed session can tell how far behind a stale tree
      // is.
      const auto treeName{outputSpectrum + std::string(GUIDE_TREE_EXTENSION)};
      const auto treePartName{treeName + ".part"};
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
      const auto bandName{bandFilmFileName(outputSpectrum)};
      const auto bandPartName{bandName + ".part"};
      auto bandLines{resumed.header.headerLines()};
      for (const auto &line : responseLines) bandLines.push_back(line);
      bandLines.push_back(smdl::concat(ENVI_BAND_UNITS, " = ", BAND_UNITS));
      const auto &bandNames{response->filmBandNames()};
      bandFilm->writeENVIFile({}, bandPartName, bandLines, window, bandNames);
      smdl::renameOnto(bandPartName, bandName);
      smdl::renameOnto(bandPartName + ".hdr", bandName + ".hdr");
      SMDL_LOG_INFO("Wrote the band film: ", smdl::Quoted(bandName), ", ",
                    bandNames.size(), " band(s) in ", BAND_UNITS);
    }
    SMDL_LOG_INFO(
        "Cumulative render time: ", formatDuration(resumed.header.seconds),
        " wall, ", formatDuration(resumed.header.cpuSeconds), " compute over ",
        resumed.header.sessions, " session(s)");
  }
  {
    const auto ldrImage{
        tonemap(opts.image.tonemap, rgbImage, film, wavelengths)};
    if (auto error{smdl::write8bitImage(opts.image.outputRGB, int(numPixelsX),
                                        int(numPixelsY), 3, ldrImage.data())}) {
      error->print();
    }
  }
}
