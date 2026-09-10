#include <string>

#include "smdl/Support/Filesystem.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Strings.h"

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
/// response they came from.
constexpr const char *ENVI_BAND_UNITS{"band units"};
constexpr const char *ENVI_RESPONSE_NAME{"render response name"};

} // namespace

void writeOutputs(const Options &opts, const Frame &frame,
                  const ResolvedGrid &grid, smdl::Compiler &compiler,
                  const EnvLight *envLight, const smdl::SpectralFilm &film,
                  const Response *response, const smdl::SpectralFilm *bandFilm,
                  ResumedSequence &resumed, const std::string &outputSpectrum,
                  const STree *sdtree) {
  SMDL_SANITY_CHECK(!bandFilm || response);
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
  if (!outputSpectrum.empty()) {

    // Write through a temporary and rename, so an interrupted write
    // cannot destroy the file a resumed session reads from, which may
    // be this very path.
    const auto partName{outputSpectrum + ".part"};
    // The tally accumulated above is the sequence's, but the fingerprint
    // is this session's: the settings a later resume compares itself
    // against are the ones the samples now in the film were drawn under.
    resumed.header.sampler = SAMPLER_VERSION;
    resumed.header.hasWavelengthJitter = shouldJitterWavelength;
    resumed.header.args = opts.argsEcho;
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
      // the reader-only lines.
      const auto bandName{bandFilmFileName(outputSpectrum)};
      const auto bandPartName{bandName + ".part"};
      auto bandLines{resumed.header.headerLines()};
      auto responseHeader{ResponseHeader{}};
      responseHeader.kind = response->kindName();
      responseHeader.hash = response->hash();
      responseHeader.cfaColumns = response->tileColumns();
      responseHeader.cfa = response->tileNames();
      for (auto &line : responseHeader.headerLines())
        bandLines.push_back(std::move(line));
      bandLines.push_back(
          smdl::concat(ENVI_BAND_UNITS, " = ", response->units()));
      if (!response->name().empty())
        bandLines.push_back(
            smdl::concat(ENVI_RESPONSE_NAME, " = ", response->name()));
      const auto &bandNames{response->filmBandNames()};
      bandFilm->writeENVIFile({}, bandPartName, bandLines, window, bandNames);
      smdl::renameOnto(bandPartName, bandName);
      smdl::renameOnto(bandPartName + ".hdr", bandName + ".hdr");
      SMDL_LOG_INFO("Wrote the band film: ", smdl::Quoted(bandName), ", ",
                    bandNames.size(), " band(s) in ", response->units());
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
