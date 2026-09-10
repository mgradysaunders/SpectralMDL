#include <string>

#include "smdl/Support/Filesystem.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Strings.h"

#include "Detector.h"
#include "MedianFilter.h"
#include "Options.h"
#include "Output.h"
#include "Progress.h"
#include "Render/Guiding.h"
#include "Render/Light.h"
#include "Render/Sampler.h"
#include "Response.h"
#include "Resume.h"
#include "Stage.h"
#include "Tonemap.h"

namespace {

/// The spectral radiance the film holds, which is the library-wide
/// convention; see `smdl::SunSky`.
constexpr const char *SPECTRAL_RADIANCE_UNITS{"W/(m^2 sr nm)"};

/// The header fields written for a reader rather than for a resume.
/// `radiance units` is not an ENVI standard field; the three solar ones
/// are, and carry the units ENVI states them in.
constexpr const char *ENVI_RADIANCE_UNITS{"radiance units"};
constexpr const char *ENVI_SUN_AZIMUTH{"sun azimuth"};
constexpr const char *ENVI_SUN_ELEVATION{"sun elevation"};
constexpr const char *ENVI_SOLAR_IRRADIANCE{"solar irradiance"};

/// The band film's reader-only fields: what its numbers are, and whose
/// response they came from. The readout's units are the same field.
constexpr const char *ENVI_BAND_UNITS{"band units"};
constexpr const char *ENVI_RESPONSE_NAME{"render response name"};
constexpr const char *DIGITAL_NUMBER_UNITS{"DN"};

} // namespace

void writeOutputs(const Options &opts, const Frame &frame,
                  const ResolvedGrid &grid, smdl::Compiler &compiler,
                  const EnvLight *envLight, const smdl::SpectralFilm &film,
                  const Response *response, const smdl::SpectralFilm *bandFilm,
                  const smdl::SpectralFilm *bandSquares,
                  ResumedSequence &resumed, const std::string &outputSpectrum,
                  const STree *sdtree) {
  SMDL_SANITY_CHECK(!bandFilm || response);
  SMDL_SANITY_CHECK(!bandFilm == !bandSquares);
  const auto &wavelengths{grid.wavelengths};
  const auto numPixelsX{frame.numPixelsX};
  const auto numPixelsY{frame.numPixelsY};
  const auto window{frame.window};
  const auto spp{frame.spp};
  // Whether every sample drew its own wavelength grid, which a resumed
  // session compares against its own.
  const bool shouldJitterWavelength{!gRenderGrid.bandEdges.empty()};
  auto rgbImage{resolveRGB(compiler, film, wavelengths, opts.image.rgbPolicy)};
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
  // The tally accumulated above is the sequence's, but the fingerprint
  // is this session's: the settings a later resume compares itself
  // against are the ones the samples now in the film were drawn under.
  resumed.header.sampler = SAMPLER_VERSION;
  resumed.header.hasWavelengthJitter = shouldJitterWavelength;
  resumed.header.args = opts.argsEcho;
  // The response's fingerprint, which every film beside the spectral one
  // carries, the readout included.
  auto responseLines{std::vector<std::string>()};
  if (response) {
    auto responseHeader{ResponseHeader{}};
    responseHeader.kind = response->kindName();
    responseHeader.hash = response->hash();
    responseHeader.cfaColumns = response->tileColumns();
    responseHeader.cfa = response->tileNames();
    responseLines = responseHeader.headerLines();
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
        smdl::concat(ENVI_RADIANCE_UNITS, " = ", SPECTRAL_RADIANCE_UNITS));
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
      // the reader-only lines. The squares film follows with the same
      // header but for its units.
      auto bandLines{resumed.header.headerLines()};
      for (const auto &line : responseLines) bandLines.push_back(line);
      const auto &bandNames{response->filmBandNames()};
      const auto writeCompanion{[&](const smdl::SpectralFilm &companion,
                                    const std::string &name, const char *units,
                                    const char *what) {
        const auto companionPartName{name + ".part"};
        auto lines{bandLines};
        lines.push_back(smdl::concat(ENVI_BAND_UNITS, " = ", units));
        if (!response->name().empty())
          lines.push_back(
              smdl::concat(ENVI_RESPONSE_NAME, " = ", response->name()));
        companion.writeENVIFile({}, companionPartName, lines, window,
                                bandNames);
        smdl::renameOnto(companionPartName, name);
        smdl::renameOnto(companionPartName + ".hdr", name + ".hdr");
        SMDL_LOG_INFO("Wrote the ", what, ": ", smdl::Quoted(name), ", ",
                      bandNames.size(), " band(s) in ", units);
      }};
      writeCompanion(*bandFilm, bandFilmFileName(outputSpectrum),
                     response->units(), "band film");
      writeCompanion(*bandSquares, bandSquaresFileName(outputSpectrum),
                     response->squaredUnits(), "squares film");
    }
    SMDL_LOG_INFO(
        "Cumulative render time: ", formatDuration(resumed.header.seconds),
        " wall, ", formatDuration(resumed.header.cpuSeconds), " compute over ",
        resumed.header.sessions, " session(s)");
  }
  if (!opts.image.outputDN.empty()) {
    // The readout: the band film through the detector, on its own pair
    // under the same discipline. Staging established the response, the
    // exposure, and the f-number; the pitch comes off the camera here,
    // which under -autolook exists only now.
    SMDL_SANITY_CHECK(bandFilm && bandSquares && frame.camera);
    const auto &camera{*frame.camera};
    const auto sensor{camera.sensorSize()};
    auto geometry{DetectorGeometry{}};
    geometry.pitchUM = float2(1e6f * sensor.x / float(numPixelsX),
                              1e6f * sensor.y / float(numPixelsY));
    geometry.pixelArea = (double(sensor.x) / double(numPixelsX)) *
                         (double(sensor.y) / double(numPixelsY));
    geometry.exposure = gRenderShutter.exposure;
    geometry.irradianceScale = camera.irradianceScale();
    geometry.fNumber = camera.fNumber();
    const Detector detector{frame.detector.value_or(DetectorSettings{}),
                            geometry};
    const auto readout{
        detector.readOut(*bandFilm, *bandSquares, opts.image.readout, window)};
    const auto &dnName{opts.image.outputDN};
    const auto dnPartName{dnName + ".part"};
    auto dnLines{resumed.header.headerLines()};
    for (const auto &line : responseLines) dnLines.push_back(line);
    for (auto &line :
         detector.header(readout, opts.image.readout).headerLines())
      dnLines.push_back(std::move(line));
    dnLines.push_back(
        smdl::concat(ENVI_BAND_UNITS, " = ", DIGITAL_NUMBER_UNITS));
    if (!response->name().empty())
      dnLines.push_back(
          smdl::concat(ENVI_RESPONSE_NAME, " = ", response->name()));
    const auto &bandNames{response->filmBandNames()};
    smdl::writeENVIFileUInt16(
        smdl::Span<const uint16_t>(readout.digitalNumbers.data(),
                                   readout.digitalNumbers.size()),
        readout.bandCount, readout.pixelCountX, readout.pixelCountY, dnPartName,
        smdl::Span<const std::string>(bandNames.data(), bandNames.size()),
        smdl::Span<const std::string>(dnLines.data(), dnLines.size()), window,
        bandFilm->getNumSamples());
    smdl::renameOnto(dnPartName, dnName);
    smdl::renameOnto(dnPartName + ".hdr", dnName + ".hdr");
    SMDL_LOG_INFO("Wrote the readout: ", smdl::Quoted(dnName), ", ",
                  bandNames.size(), " band(s) of digital numbers up to ",
                  detector.topCode());
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
