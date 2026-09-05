#include <array>
#include <string>
#include <vector>

#include "CommandLine.h"

#include "smdl/Support/Error.h"
#include "smdl/Support/Filesystem.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Strings.h"

#include "Options.h"
#include "Render/Sampler.h"
#include "Resume.h"

namespace {

// The command line, joined for the `smdl args` metadata field, with the
// session-only flags stripped: outputs, display transforms, the sample
// budget, the guiding strategy, the thread count, and -resume itself
// legitimately change between the sessions of one render, while anything
// else that differs likely changes the radiance being estimated and
// earns a warning. The wavelength and window flags are stripped too: a
// genuine grid or window mismatch already has its own hard error, so
// warning here would double-report. Tokenizes on whitespace, so a path
// containing spaces can misalign the comparison; the result only feeds a
// warning, never behavior.
[[nodiscard]]
std::vector<std::string> stripSessionOnlyArgs(const std::string &args) {
  // Split by whether the flag's value arrives as a separate token, so
  // that token is stripped with it; the boolean guiding flags carry no
  // value and must not eat the token after them.
  static constexpr auto SESSION_ONLY_VALUES = std::array{"resume",
                                                         "spp",
                                                         "output-rgb",
                                                         "output-rgbf",
                                                         "output-spectrum",
                                                         "exposure",
                                                         "tonemap",
                                                         "tonemap-decades",
                                                         "curve",
                                                         "local",
                                                         "local-strength",
                                                         "local-range",
                                                         "local-clamp",
                                                         "wavelength-range",
                                                         "wavelengths",
                                                         "crop-window",
                                                         "guide-bsdf-fraction",
                                                         "guide-split",
                                                         "mnee-depth",
                                                         "mnee-max-trials",
                                                         "mnee-biased",
                                                         "mnee-max-roughness",
                                                         "mnee-receiver-alpha",
                                                         "sample-offset",
                                                         "threads"};
  static constexpr auto SESSION_ONLY_FLAGS =
      std::array{"guide",         "guide-adrrs", "mnee",
                 "mnee-sun-only", "mnee-report", "mnee-test-normalhook"};
  auto tokens{std::vector<std::string>()};
  for (size_t pos{}; pos < args.size();) {
    size_t end{args.find_first_of(" \t", pos)};
    if (end == std::string::npos) end = args.size();
    if (end > pos) tokens.push_back(args.substr(pos, end - pos));
    pos = end + 1;
  }
  auto result{std::vector<std::string>()};
  for (size_t i = 0; i < tokens.size(); i++) {
    const auto &token{tokens[i]};
    bool isSessionOnly{false};
    bool takesValue{false};
    bool hasAttachedValue{false};
    if (!token.empty() && token[0] == '-') {
      auto name{token.substr(token.find_first_not_of('-'))};
      auto equals{name.find('=')};
      hasAttachedValue = equals != std::string::npos;
      name = name.substr(0, equals);
      for (const auto *sessionOnlyName : SESSION_ONLY_VALUES)
        if (name == sessionOnlyName) {
          isSessionOnly = true;
          takesValue = true;
          break;
        }
      if (!isSessionOnly)
        for (const auto *sessionOnlyName : SESSION_ONLY_FLAGS)
          if (name == sessionOnlyName) {
            isSessionOnly = true;
            break;
          }
    }
    if (isSessionOnly) {
      if (takesValue && !hasAttachedValue && i + 1 < tokens.size()) i++;
      continue;
    }
    result.push_back(token);
  }
  return result;
}

} // namespace

ResumedSequence resumeSequence(const Options &opts, int2 resolution,
                               int4 window) {
  auto result{ResumedSequence{}};
  result.requested = !opts.output.resume.empty();
  result.sampleIndexBase = opts.sampling.sampleOffset;
  // A fresh sequence begins where `-sample-offset` says with an empty
  // tally; a resumed one takes both off the file below.
  result.header.sampleOffset = opts.sampling.sampleOffset;
  if (result.requested) {
    // A wholly missing data-plus-header pair is not an error: it makes
    // this run the first session of an intended sequence, rendering
    // from scratch and writing the file for the next -resume. Half a
    // pair is a damaged prior session, and starting fresh over it
    // would clobber what is left, so that stays fatal.
    const auto &resumeName{opts.output.resume};
    const bool haveData{smdl::exists(resumeName)};
    const bool haveHeader{smdl::exists(resumeName + ".hdr")};
    if (haveData != haveHeader)
      throw smdl::Error(smdl::concat(
          "cannot resume: ",
          smdl::Quoted(haveData ? resumeName : resumeName + ".hdr"),
          " exists but ",
          smdl::Quoted(haveData ? resumeName + ".hdr" : resumeName),
          " does not; refusing to start fresh over a damaged session"));
    if (!haveData) {
      // -spp 0 re-runs the output stage, which is meaningless with
      // nothing to load; worse, the 0-sample file it would write has
      // no 'render spp' field and could not itself be resumed.
      if (opts.sampling.spp == 0)
        throw smdl::Error(smdl::concat(
            "cannot resume with '-spp 0': ", smdl::Quoted(resumeName),
            " does not exist, so there is no output stage to re-run"));
      SMDL_LOG_INFO(
          "Starting a new render sequence: ", smdl::Quoted(resumeName),
          " does not exist yet, this session writes it");
    }
    result.loaded = haveData;
  }
  if (!result.loaded) return result;
  auto &film{result.film};
  auto &info{result.info};
  auto &header{result.header};
  info = film.readENVIFile(opts.output.resume);
  if (film.getNumPixelsX() != size_t(resolution.x) ||
      film.getNumPixelsY() != size_t(resolution.y))
    throw smdl::Error(
        smdl::concat("cannot resume: the file is ", film.getNumPixelsX(), "x",
                     film.getNumPixelsY(), " against -resolution ",
                     resolution.x, ",", resolution.y));
  if (info.samplesPerPixel == 0)
    throw smdl::Error("cannot resume: the header has no 'render spp' count "
                      "(the file was not written by -output-spectrum)");
  // The window is what the recorded count applies to, so a session
  // that moved it would accumulate over a different set of pixels and
  // the film would stop having a single samples per pixel. Both
  // directions land here: the file's window defaults to the whole
  // frame, and so does this session's.
  if (!smdl::isAllTrue(info.cropWindow == window))
    throw smdl::Error(smdl::concat(
        "cannot resume: the file was rendered with -crop-window ",
        spellVector(info.cropWindow), " against this session's ",
        spellVector(window),
        "; the window must be held constant across a resumed sequence, "
        "otherwise the samples per pixel stop being uniform"));
  // The record continues rather than restarts, so the tally comes off
  // the file and so does the sample offset: the flag names where a
  // sequence begins, and only its first session gets to say. The
  // fingerprint fields come off the file too, to be compared against
  // this session here and replaced by it when it is written back.
  header.sampleOffset = 0;
  header.readFrom(info.fields);
  if (header.sampler != SAMPLER_VERSION)
    SMDL_LOG_WARN("resuming a file from a different sampler: the continuation "
                  "samples are independent of the first session's rather than "
                  "jointly stratified (still unbiased, noise just improves "
                  "more slowly)");
  if (header.wavelengthJitter != opts.grid.jitter)
    SMDL_LOG_WARN(
        "resuming across a -wavelength-jitter change: a jittered band "
        "holds the mean radiance over the band and an unjittered one holds "
        "the radiance at one wavelength, so the merged image mixes two "
        "different quantities");
  if (!header.args.empty() &&
      stripSessionOnlyArgs(header.args) != stripSessionOnlyArgs(opts.argsEcho))
    SMDL_LOG_WARN("resuming with different flags: the file records ",
                  smdl::Quoted(header.args),
                  "; if the scene or camera changed, the merged image "
                  "mixes two different renders");
  result.sampleIndexBase = header.sampleOffset + info.samplesPerPixel;
  SMDL_LOG_INFO("Resuming: ", info.samplesPerPixel, " samples per pixel from ",
                smdl::Quoted(opts.output.resume), " (sample offset ",
                header.sampleOffset, ")");
  return result;
}
