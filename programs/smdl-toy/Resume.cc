#include <array>
#include <string>
#include <vector>

#include "../CommandLine.h"

#include "smdl/Support/Error.h"
#include "smdl/Support/Filesystem.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Strings.h"

#include "CameraModel.h"
#include "Options.h"
#include "Render/Sampler.h"
#include "Resume.h"
#include "Sensor/Response.h"
#include "Stage.h"

namespace {

// The command line, joined for the `smdl args` metadata field, with the
// session-only flags stripped: outputs, display transforms, the firefly
// filter, the sample budget, the guiding strategy, the thread count, and
// -resume itself legitimately change between the sessions of one render,
// while anything else that differs likely changes the radiance being
// estimated and earns a warning. The wavelength and window flags are stripped
// too: a genuine grid or window mismatch already has its own hard error, so
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
                                                         "median-filter-factor",
                                                         "median-filter-radius",
                                                         "wavelength-range",
                                                         "wavelengths",
                                                         "crop-window",
                                                         "resolution-scale",
                                                         "guide-bsdf-fraction",
                                                         "guide-split",
                                                         "mnee-depth",
                                                         "mnee-max-trials",
                                                         "mnee-biased",
                                                         "mnee-max-roughness",
                                                         "mnee-receiver-alpha",
                                                         "sample-offset",
                                                         "threads",
                                                         "output-dn",
                                                         "detector-seed",
                                                         "detector-noise",
                                                         "iso",
                                                         "white-balance"};
  static constexpr auto SESSION_ONLY_FLAGS =
      std::array{"guide",
                 "guide-adrrs",
                 "mnee",
                 "mnee-sun-only",
                 "mnee-test-normalhook",
                 "median-filter",
                 "output-spectrum-double",
                 "report",
                 "json"};
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
    bool doesTakeValue{false};
    bool hasAttachedValue{false};
    if (!token.empty() && token[0] == '-') {
      auto name{token.substr(token.find_first_not_of('-'))};
      auto equals{name.find('=')};
      hasAttachedValue = equals != std::string::npos;
      name = name.substr(0, equals);
      for (const auto *sessionOnlyName : SESSION_ONLY_VALUES)
        if (name == sessionOnlyName) {
          isSessionOnly = true;
          doesTakeValue = true;
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
      if (doesTakeValue && !hasAttachedValue && i + 1 < tokens.size()) i++;
      continue;
    }
    result.push_back(token);
  }
  return result;
}

} // namespace

namespace {

// The names of a band list, for a message.
[[nodiscard]] std::string spellNames(const std::vector<std::string> &names) {
  auto text{std::string("{")};
  for (size_t i = 0; i < names.size(); i++)
    text += (i > 0 ? ", " : "") + names[i];
  return text + "}";
}

// The band film beside the accumulation, with what its header carried.
struct BandFilm final {
  smdl::SpectralFilm film{};
  smdl::SpectralFilm::ENVIFileInfo info{};
};

// Load the band film at `name` and hold it to the accumulation: present
// as a pair, the same resolution and window, the same sample count, and
// the bands `names`. Everything is a hard error; see `resumeSequence()`.
[[nodiscard]] BandFilm loadBandFilm(const std::string &name, int2 resolution,
                                    int4 window, uint64_t samplesPerPixel,
                                    const std::vector<std::string> &names) {
  const bool hasData{smdl::exists(name)};
  const bool hasHeader{smdl::exists(name + ".hdr")};
  if (!hasData || !hasHeader)
    throw smdl::Error(smdl::concat(
        "cannot resume: the band film ", smdl::Quoted(name),
        " beside the accumulation ",
        hasData || hasHeader ? "is half a pair" : "does not exist",
        "; the sequence was rendered without a response, or its last "
        "session was interrupted between the files"));
  auto result{BandFilm{}};
  result.info = result.film.readENVIFile(name);
  if (result.film.getNumPixelsX() != size_t(resolution.x) ||
      result.film.getNumPixelsY() != size_t(resolution.y))
    throw smdl::Error(smdl::concat(
        "cannot resume: the band film is ", result.film.getNumPixelsX(), "x",
        result.film.getNumPixelsY(), " against -resolution ", resolution.x, ",",
        resolution.y));
  if (result.info.samplesPerPixel != samplesPerPixel)
    throw smdl::Error(smdl::concat(
        "cannot resume: the band film holds ", result.info.samplesPerPixel,
        " samples per pixel against the accumulation's ", samplesPerPixel,
        "; the last session was interrupted between the files, or rendered "
        "without the response"));
  if (!smdl::isAllTrue(result.info.cropWindow == window))
    throw smdl::Error(smdl::concat(
        "cannot resume: the band film was rendered with -crop-window ",
        spellVector(result.info.cropWindow), " against this session's ",
        spellVector(window)));
  if (result.info.bandNames != names)
    throw smdl::Error(smdl::concat("cannot resume: the band film's bands are ",
                                   spellNames(result.info.bandNames),
                                   " against this response's ",
                                   spellNames(names)));
  return result;
}

} // namespace

ResumedSequence resumeSequence(const Options &opts, const Frame &frame,
                               const ResponseSettings *response) {
  const auto resolution{frame.resolution};
  const auto window{frame.window};
  auto result{ResumedSequence{}};
  result.wasRequested = !opts.image.resume.empty();
  result.sampleIndexBase = opts.render.sampling.sampleOffset;
  // A fresh sequence begins where `-sample-offset` says with an empty
  // tally; a resumed one takes both off the file below.
  result.header.sampleOffset = opts.render.sampling.sampleOffset;
  if (result.wasRequested) {
    // A wholly missing data-plus-header pair is not an error: it makes
    // this run the first session of an intended sequence, rendering
    // from scratch and writing the file for the next -resume. Half a
    // pair is a damaged prior session, and starting fresh over it
    // would clobber what is left, so that stays fatal.
    const auto &resumeName{opts.image.resume};
    const bool hasData{smdl::exists(resumeName)};
    const bool hasHeader{smdl::exists(resumeName + ".hdr")};
    if (hasData != hasHeader)
      throw smdl::Error(smdl::concat(
          "cannot resume: ",
          smdl::Quoted(hasData ? resumeName : resumeName + ".hdr"),
          " exists but ",
          smdl::Quoted(hasData ? resumeName + ".hdr" : resumeName),
          " does not; refusing to start fresh over a damaged session"));
    if (!hasData) {
      // -spp 0 re-runs the output stage, which is meaningless with
      // nothing to load; worse, the 0-sample file it would write has
      // no 'render spp' field and could not itself be resumed.
      if (opts.render.sampling.spp == 0)
        throw smdl::Error(smdl::concat(
            "cannot resume with '-spp 0': ", smdl::Quoted(resumeName),
            " does not exist, so there is no output stage to re-run"));
      SMDL_LOG_INFO(
          "Starting a new render sequence: ", smdl::Quoted(resumeName),
          " does not exist yet, this session writes it");
    }
    result.wasLoaded = hasData;
  }
  if (!result.wasLoaded) return result;
  auto &film{result.film};
  auto &info{result.info};
  auto &header{result.header};
  info = film.readENVIFile(opts.image.resume);
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
  // A file written before the film could hold anything but radiance
  // says nothing, and means radiance.
  const std::string fileQuantity{header.quantity.empty() ? "radiance"
                                                         : header.quantity};
  if (fileQuantity != filmQuantityName(frame.model.filmQuantity()))
    throw smdl::Error(smdl::concat(
        "cannot resume: the file holds ", fileQuantity,
        " and this camera's film holds ",
        filmQuantityName(frame.model.filmQuantity()),
        " (a physical sensor's film and the observer's are different "
        "quantities); render with the same sensor, or start a fresh "
        "-output-spectrum"));
  if (header.sampler != SAMPLER_VERSION)
    SMDL_LOG_WARN("resuming a file from a different sampler: the continuation "
                  "samples are independent of the first session's rather than "
                  "jointly stratified (still unbiased, noise just improves "
                  "more slowly)");
  if (header.hasWavelengthJitter != frame.shouldJitterWavelength)
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
                smdl::Quoted(opts.image.resume), " (sample offset ",
                header.sampleOffset, ")");
  // The band film beside the accumulation. Without a response it is left
  // where it is, and said so, since it falls behind from here on.
  const auto bandName{bandFilmFileName(opts.image.resume)};
  if (!response) {
    if (smdl::exists(bandName) || smdl::exists(bandName + ".hdr"))
      SMDL_LOG_WARN("the band film ", smdl::Quoted(bandName),
                    " beside the accumulation is not continued: this "
                    "session has no response, so it falls behind");
    return result;
  }
  const auto names{responseFilmBandNames(*response)};
  auto bands{
      loadBandFilm(bandName, resolution, window, info.samplesPerPixel, names)};
  result.responseHeader.readFrom(bands.info.fields);
  if (result.responseHeader.hash != responseHash(*response))
    throw smdl::Error(
        "cannot resume: the response's curves differ from the ones the band "
        "film was rendered with; start a fresh -output-spectrum, or render "
        "without the response");
  result.bandFilm = std::move(bands.film);
  result.bandInfo = std::move(bands.info);
  SMDL_LOG_INFO("Resuming the band film: ", smdl::Quoted(bandName), ", ",
                smdl::Counted(names.size(), "band"));
  return result;
}
