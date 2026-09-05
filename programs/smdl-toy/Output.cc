#include <string>

#include "smdl/Support/Filesystem.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Strings.h"

#include "Options.h"
#include "Output.h"
#include "Progress.h"
#include "Render/Guiding.h"
#include "Render/Sampler.h"
#include "Resume.h"
#include "Stage.h"
#include "Tonemap.h"

void writeOutputs(const Options &opts, const Frame &frame,
                  const ResolvedGrid &grid, smdl::Compiler &compiler,
                  const smdl::SpectralFilm &film, ResumedSequence &resumed,
                  const std::string &outputSpectrum, const STree *sdtree) {
  const auto &wavelengths{grid.wavelengths};
  const auto numPixelsX{frame.numPixelsX};
  const auto numPixelsY{frame.numPixelsY};
  const auto window{frame.window};
  const auto spp{frame.spp};
  // Whether every sample drew its own wavelength grid, which a resumed
  // session compares against its own.
  const bool jitterWavelength{!renderGrid().bandEdges.empty()};
  const auto rgbImage{resolveRGB(compiler, film, wavelengths, opts.rgbPolicy)};
  if (!opts.output.rgbFloat.empty()) {
    if (auto error{smdl::writeFloatImage(opts.output.rgbFloat, int(numPixelsX),
                                         int(numPixelsY), 3,
                                         rgbImage.data())}) {
      error->print();
    }
  }
  if (!outputSpectrum.empty()) {
    // TODO If using procedural SunSky (and not in moonlight mode), add
    // the standard ENVI header lines:
    //   sun azimuth = (degrees)
    //   sun elevation = (degrees)
    //   solar irradiance = {...} (W/m2/um)

    // Write through a temporary and rename, so an interrupted write
    // cannot destroy the file a resumed session reads from, which may
    // be this very path.
    const auto partName{outputSpectrum + ".part"};
    // The tally accumulated above is the sequence's, but the fingerprint
    // is this session's: the settings a later resume compares itself
    // against are the ones the samples now in the film were drawn under.
    resumed.header.sampler = SAMPLER_VERSION;
    resumed.header.wavelengthJitter = jitterWavelength;
    resumed.header.args = opts.argsEcho;
    // The window the recorded count belongs to, which the film itself
    // does not know: a windowed render still carries a full frame of
    // pixels, and the header must not describe the untouched ones as
    // samples.
    film.writeENVIFile(
        smdl::Span<const float>(wavelengths.data(), wavelengths.size()),
        partName, resumed.header.headerLines(), window);
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
    SMDL_LOG_INFO(
        "Cumulative render time: ", formatDuration(resumed.header.seconds),
        " wall, ", formatDuration(resumed.header.cpuSeconds), " compute over ",
        resumed.header.sessions, " session(s)");
  }
  {
    const auto ldrImage{tonemap(opts.tonemap, rgbImage, film, wavelengths)};
    if (auto error{smdl::write8bitImage(opts.output.rgb, int(numPixelsX),
                                        int(numPixelsY), 3, ldrImage.data())}) {
      error->print();
    }
  }
}
