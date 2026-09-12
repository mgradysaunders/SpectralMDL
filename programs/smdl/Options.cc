// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#include <algorithm>

#include "../CommandLine.h"

#include "smdl/Common.h"
#include "smdl/RenderUtil/OpticalGlass.h"
#include "smdl/Support/Error.h"
#include "smdl/Support/Strings.h"

#include "Options.h"

namespace {

cl::SubCommand subDump{"dump", "Dump as LLVM-IR or native assembly"};
cl::SubCommand subList{"list", "List all materials"};
cl::SubCommand subRun{"run", "Run execs"};
cl::SubCommand subTest{"test", "Run execs and unit tests"};
cl::SubCommand subFormat{"format", "Format source code"};
cl::SubCommand subDoc{"doc", "Show documentation"};
cl::SubCommand subVolume{"volume",
                         "Convert voxel grids between '.vol' and '.nvdb'"};
cl::SubCommandGroup subsWithCompileOptions{&subDump, &subList, &subRun,
                                           &subTest};
cl::SubCommandGroup subsWithOutputFile{&subDump, &subDoc};
cl::SubCommandGroup allSubs{&subDump,   &subList, &subRun,   &subTest,
                            &subFormat, &subDoc,  &subVolume};

// NOTE: This is `ZeroOrMore` only so that `smdl doc --builtins` works
// with no inputs; every other subcommand requires at least one input,
// enforced in `parseCommandLine()`.
cl::list<std::string> optInputs{cl::Positional, cl::desc("<input>"),
                                cl::ZeroOrMore, cl::sub(allSubs)};

cl::OptionCategory catCompile{"Compile Options"};
//--{ Compile Options
cl::opt<unsigned> optOptLevel{"O",
                              cl::desc("The optimization level (default: 2)"),
                              cl::Prefix,
                              cl::init(2U),
                              cl::sub(subsWithCompileOptions),
                              cl::cat(catCompile)};
cl::opt<bool> optDebug{"g", cl::desc("Enable debugging"), cl::init(false),
                       cl::sub(subsWithCompileOptions), cl::cat(catCompile)};
cl::opt<std::string> optWavelengthRange{
    "wavelength-range",
    cl::desc("Wavelengths spanning A to B nm with N bands, "
             "format 'A,B:N' where ':N' is optional (default: 380,720:16)\n"
             "* N is the number of wavelengths the material code is "
             "compiled for, and A and B are what 'state::wavelength_min()' "
             "and 'wavelength_max()' report"),
    cl::sub(subsWithCompileOptions), cl::cat(catCompile)};
cl::opt<std::string> optWavelengths{
    "wavelengths",
    cl::desc("Wavelengths in nm, comma-separated or a text file of "
             "whitespace-separated values (mutually exclusive with "
             "-wavelength-range)"),
    cl::sub(subsWithCompileOptions), cl::cat(catCompile)};
//--}

cl::OptionCategory catDoc{"Doc Options"};
//--{ Doc Options
cl::opt<DocFormat> optDocFormat{
    "f",
    cl::desc("The output format:"),
    cl::init(DocFormat::TEXT),
    cl::values(
        cl::OptionEnumValue{"text", int(DocFormat::TEXT),
                            "Plain text for symbol queries (default); "
                            "whole-database output falls back to Markdown"},
        cl::OptionEnumValue{"json", int(DocFormat::JSON), "JSON database"},
        cl::OptionEnumValue{"md", int(DocFormat::MARKDOWN), "Markdown"}),
    cl::sub(subDoc),
    cl::cat(catDoc)};
cl::opt<bool> optDocAll{
    "all",
    cl::desc("Include hidden declarations, i.e., declarations not marked "
             "'export' or named with a leading underscore"),
    cl::init(false), cl::sub(subDoc), cl::cat(catDoc)};
cl::opt<bool> optDocBuiltins{"builtins",
                             cl::desc("Include all builtin modules"),
                             cl::init(false), cl::sub(subDoc), cl::cat(catDoc)};
//--}

cl::OptionCategory catFormat{"Format Options"};
//--{ Format Options
cl::opt<bool> optFormatInPlace{"i", cl::desc("Format in place"),
                               cl::init(false), cl::sub(subFormat),
                               cl::cat(catFormat)};
cl::opt<bool> optFormatCompact{
    "c", cl::desc("Format the output more compactly"), cl::init(false),
    cl::sub(subFormat), cl::cat(catFormat)};
cl::opt<bool> optFormatNoComments{"no-comments", cl::desc("Remove comments"),
                                  cl::init(false), cl::sub(subFormat),
                                  cl::cat(catFormat)};
cl::opt<bool> optFormatKeepDocComments{
    "keep-doc-comments",
    cl::desc("With -no-comments, keep the '///' doc comments"), cl::init(false),
    cl::sub(subFormat), cl::cat(catFormat)};
cl::opt<bool> optFormatNoAnnotations{
    "no-annotations", cl::desc("Remove annotations"), cl::init(false),
    cl::sub(subFormat), cl::cat(catFormat)};
cl::opt<int> optFormatColumns{
    "columns",
    cl::desc("The column past which to prefer breaking a line, or 0 to "
             "never break for width"),
    cl::init(80), cl::sub(subFormat), cl::cat(catFormat)};
//--}

cl::OptionCategory catOutput{"Output Options"};
//--{ Output Options
cl::opt<smdl::DumpFormat> optDumpFormat{
    "f",
    cl::desc("The dump format:"),
    cl::init(smdl::DUMP_FORMAT_IR),
    cl::values(
        cl::OptionEnumValue{"llvm-ir", int(smdl::DUMP_FORMAT_IR), "LLVM-IR"},
        cl::OptionEnumValue{"asm", int(smdl::DUMP_FORMAT_ASM),
                            "Native assembly"},
        cl::OptionEnumValue{"obj", int(smdl::DUMP_FORMAT_OBJ),
                            "Native object file"}),
    cl::sub(subDump),
    cl::cat(catOutput)};
cl::opt<std::string> optOutput{
    "output", cl::desc("The output filename (default: standard output)"),
    cl::Optional, cl::sub(subsWithOutputFile), cl::cat(catOutput)};
//--}

cl::OptionCategory catState{"State Options"};
//--{ State Options
cl::opt<float> optTime{"time",
                       cl::desc("The animation time in seconds (default: 0)"),
                       cl::init(0.0f), cl::sub(subTest), cl::cat(catState)};
cl::opt<float> optWavelengthHero{
    "wavelength-hero",
    cl::desc("The hero wavelength in nanometers (default: 587.5618, the d "
             "line)"),
    cl::init(smdl::FRAUNHOFER_D_LINE), cl::sub(subTest), cl::cat(catState)};
cl::opt<int> optObjectID{"object-id", cl::desc("The object ID (default: 0)"),
                         cl::init(0), cl::sub(subTest), cl::cat(catState)};
cl::opt<smdl::float3> optTexCoord{
    "texcoord", cl::desc("The texture coordinate (default: 0,0,0)"),
    cl::init(smdl::float3{}), cl::sub(subTest), cl::cat(catState)};
cl::opt<int> optPtexFaceID{"ptex-face-id",
                           cl::desc("The Ptex face ID (default: 0)"),
                           cl::init(0), cl::sub(subTest), cl::cat(catState)};
cl::opt<smdl::float2> optPtexFaceUV{
    "ptex-face-uv", cl::desc("The Ptex face coordinate (default: 0,0)"),
    cl::init(smdl::float2{}), cl::sub(subTest), cl::cat(catState)};
//--}

cl::OptionCategory catUtility{"Utility Options"};
//--{ Utility Options
// NOTE: LLVM registers a '--color' of its own on the top-level
// subcommand, in a hidden category that `HideUnrelatedOptions` filters
// out, so it never appears in any '--help'. This option shadows it in
// every subcommand, since an option is looked up in the subcommand ahead
// of the top level (`CommandLine.cpp`, `LookupLongOption`), and drives
// the coloring explicitly, which keeps the behavior independent of that
// LLVM-internal option. Keep it scoped to subcommands: registering a
// '--color' at the top level would land in the same option map as
// LLVM's, and `cl` aborts on a duplicate name. That is also why the
// renderer, which has no subcommands, cannot have one at all.
cl::opt<cl::boolOrDefault> optColor{
    "color",
    cl::desc("Colorize log messages, the unit test report, and the "
             "documentation text (default: autodetect)\n"
             "* autodetect colors a terminal, unless NO_COLOR is set or "
             "TERM is 'dumb'"),
    cl::init(cl::boolOrDefault::BOU_UNSET), cl::sub(allSubs),
    cl::cat(catUtility)};
cl::opt<std::string> optLogLevel{
    "log-level",
    cl::desc("The log level to filter output verbosity, must be "
             "'debug', 'info', 'warn', or 'error' (default: 'info')"),
    cl::init(std::string("info")), cl::sub(allSubs), cl::cat(catUtility)};
cl::opt<cl::boolOrDefault> optUnicode{
    "unicode",
    cl::desc("Label log messages with Unicode symbols rather than bracketed "
             "words (default: autodetect)"),
    cl::init(cl::boolOrDefault::BOU_UNSET), cl::sub(allSubs),
    cl::cat(catUtility)};
cl::opt<std::string> optProfile{
    "profile",
    cl::desc("Write a time-trace JSON of the work this subcommand does "
             "(default: smdl.trace.json)\n"
             "* open in chrome://tracing or https://ui.perfetto.dev"),
    cl::ValueOptional,
    cl::init(std::string{}),
    cl::sub(allSubs),
    cl::cat(catUtility)};
cl::opt<unsigned> optThreads{
    "threads",
    cl::desc("Set the thread limit for image loading and albedo tabulation, "
             "or 0 for the maximum (default: 0)\n"
             "* '-threads 1' runs them inline with no pool at all, for a "
             "debugger"),
    cl::init(0U), cl::sub(subsWithCompileOptions), cl::cat(catUtility)};
//--}

cl::OptionCategory catVolume{"Volume Options"};
//--{ Volume Options
cl::list<std::string> optVolumeGrids{
    "grid",
    cl::desc("The grid to read from a NanoVDB input, and to name in a "
             "NanoVDB output. Repeat once per input, or give none: an "
             "unnamed input reads its first grid, and an unnamed output "
             "grid is named after the input file's stem"),
    cl::sub(subVolume), cl::cat(catVolume)};
cl::opt<std::string> optVolumeOutput{
    "output",
    cl::desc("The output filename, whose extension ('.vol' or '.nvdb') "
             "selects the format. Without one, describe the inputs instead"),
    cl::sub(subVolume), cl::cat(catVolume)};
//--}

// Which subcommand `cl` matched. Exactly one is ever active.
[[nodiscard]] Subcommand activeSubcommand() {
  if (subDump) return Subcommand::DUMP;
  if (subList) return Subcommand::LIST;
  if (subRun) return Subcommand::RUN;
  if (subTest) return Subcommand::TEST;
  if (subFormat) return Subcommand::FORMAT;
  if (subDoc) return Subcommand::DOC;
  if (subVolume) return Subcommand::VOLUME;
  throw smdl::Error("Expected a subcommand");
}

// The wavelength grid the material code compiles for. An explicit
// `-wavelengths` list is the grid; otherwise it is the endpoint
// inclusive uniform grid `-wavelength-range` describes, whose single
// band sits at the midpoint (the general formula is 0/0 there).
[[nodiscard]] std::vector<float>
resolveWavelengths(const WavelengthRange &range, std::vector<float> given) {
  if (!given.empty()) return given;
  std::vector<float> wavelengths(range.bandCount);
  for (unsigned i = 0; i < range.bandCount; i++) {
    const float fac{range.bandCount > 1 ? float(i) / float(range.bandCount - 1)
                                        : 0.5f};
    wavelengths[i] = (1 - fac) * range.range.x + fac * range.range.y;
  }
  return wavelengths;
}

} // namespace

Options parseCommandLine(int argc, char **argv) {
  cl::SetVersionPrinter(
      [](llvm::raw_ostream &os) { os << smdl::BuildInfo::get().toString(); });
  cl::HideUnrelatedOptions({&catCompile, &catDoc, &catFormat, &catOutput,
                            &catState, &catUtility, &catVolume});
  cl::ParseCommandLineOptions(argc, argv, "SpectralMDL compiler");
  // Honors '-print-options' and '-print-all-options', which LLVM
  // registers but leaves to the tool to act on; it prints nothing unless
  // one of them was given.
  cl::PrintOptionValues();

  Options opts{};
  opts.subcommand = activeSubcommand();

  // The '::'-prefixed positionals are queries rather than file names,
  // and only `doc` reads them as such.
  for (const auto &input : optInputs) {
    if (opts.subcommand == Subcommand::DOC && smdl::startsWith(input, "::")) {
      opts.docQueries.push_back(input);
    } else {
      opts.inputs.push_back(input);
    }
  }
  // `doc` is the exception: '--builtins' and the queries above give it
  // something to document without any input at all.
  if (opts.inputs.empty() && opts.subcommand != Subcommand::DOC)
    throw smdl::Error("Expected at least one input");
  if (optWavelengths.getNumOccurrences() > 0 &&
      optWavelengthRange.getNumOccurrences() > 0)
    throw smdl::Error("Expected at most one of -wavelengths and "
                      "-wavelength-range (they are two spellings of the "
                      "wavelength grid)");
  if (!optVolumeGrids.empty() && optVolumeGrids.size() != opts.inputs.size())
    throw smdl::Error("Expected one -grid per input, or none at all");

  // Parsed here so a typo fails before anything loads.
  const WavelengthRange range{
      parseWavelengthRange(std::string(optWavelengthRange))};
  opts.compile.optLevel = smdl::OptLevel(std::min(unsigned(optOptLevel), 3U));
  opts.compile.isDebugEnabled = bool(optDebug);
  opts.compile.wavelengths =
      resolveWavelengths(range, parseWavelengths(std::string(optWavelengths)));
  opts.compile.wavelengthRange =
      optWavelengths.getNumOccurrences() > 0
          ? smdl::float2{opts.compile.wavelengths.front(),
                         opts.compile.wavelengths.back()}
          : range.range;

  opts.doc.format = DocFormat(optDocFormat);
  opts.doc.shouldIncludeHidden = bool(optDocAll);
  opts.doc.allBuiltins = bool(optDocBuiltins);

  opts.format.isInPlace = bool(optFormatInPlace);
  opts.format.isCompact = bool(optFormatCompact);
  opts.format.shouldDropComments = bool(optFormatNoComments);
  opts.format.shouldKeepDocComments = bool(optFormatKeepDocComments);
  opts.format.shouldDropAnnotations = bool(optFormatNoAnnotations);
  opts.format.softColumnLimit = int(optFormatColumns);

  opts.output.dumpFormat = smdl::DumpFormat(optDumpFormat);
  opts.output.fileName = flag(optOutput);

  opts.state.time = float(optTime);
  opts.state.wavelengthHero = float(optWavelengthHero);
  opts.state.objectID = int(optObjectID);
  opts.state.texCoord = smdl::float3(optTexCoord);
  opts.state.ptexFaceID = int(optPtexFaceID);
  opts.state.ptexFaceUV = smdl::float2(optPtexFaceUV);

  opts.utility.threads = unsigned(optThreads);
  opts.utility.logLevel = parseLogLevel(std::string(optLogLevel));
  opts.utility.ansiColorMode =
      optColor == cl::boolOrDefault::BOU_TRUE    ? smdl::ANSI_COLOR_MODE_ALWAYS
      : optColor == cl::boolOrDefault::BOU_FALSE ? smdl::ANSI_COLOR_MODE_NEVER
                                                 : smdl::ANSI_COLOR_MODE_AUTO;
  opts.utility.unicodeMode = lowerUnicodeMode(optUnicode);
  opts.utility.profile = std::string(optProfile).empty()
                             ? std::string("smdl.trace.json")
                             : std::string(optProfile);
  opts.utility.isProfiling = optProfile.getNumOccurrences() > 0;

  opts.volume.gridNames.assign(optVolumeGrids.begin(), optVolumeGrids.end());
  opts.volume.fileName = std::string(optVolumeOutput);
  return opts;
}
