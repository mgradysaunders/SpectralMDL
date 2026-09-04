#include "smdl/Common.h"
#include "smdl/Compiler.h"
#include "smdl/Resource/VoxelGrid.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Parallel.h"
#include <filesystem>
#include <fstream>
#include <iostream>
#include <optional>
#include <system_error>

#include "llvm/Support/CommandLine.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/WithColor.h"
#include "llvm/Support/raw_ostream.h"

namespace cl = llvm::cl;
static cl::OptionCategory catOptions{"Options"};
static cl::SubCommand subDump{"dump", "Dump as LLVM-IR or native assembly"};
static cl::SubCommand subList{"list", "List all materials"};
static cl::SubCommand subRun{"run", "Run execs"};
static cl::SubCommand subTest{"test", "Run execs and unit tests"};
static cl::SubCommand subFormat{"format", "Format source code"};
static cl::SubCommand subDoc{"doc", "Show documentation"};
static cl::SubCommand subVolume{
    "volume", "Convert voxel grid files between '.vol' and '.nvdb', or "
              "describe what one holds"};
static cl::SubCommandGroup subsWithCompileOptions{&subDump, &subList, &subRun,
                                                  &subTest};
static cl::SubCommandGroup subsWithOutputFile{&subDump, &subDoc};
static cl::SubCommandGroup subsWithColor{&subTest, &subDoc};
static cl::SubCommandGroup allSubs{&subDump,   &subList, &subRun,   &subTest,
                                   &subFormat, &subDoc,  &subVolume};

// NOTE: This is `ZeroOrMore` only so that `smdl doc --builtins` works
// with no inputs; every other subcommand requires at least one input,
// enforced in `main()`.
static cl::list<std::string> inputFiles{cl::Positional, cl::desc("<input>"),
                                        cl::ZeroOrMore, cl::sub(allSubs),
                                        cl::cat(catOptions)};

static cl::opt<unsigned> optLevel{"O",
                                  cl::desc("Optimization level (default 2)"),
                                  cl::Prefix,
                                  cl::init(2U),
                                  cl::sub(subsWithCompileOptions),
                                  cl::cat(catOptions)};
static cl::opt<bool> enableDebug{"g", cl::desc("Enable debugging"),
                                 cl::sub(subsWithCompileOptions),
                                 cl::cat(catOptions)};
static cl::opt<unsigned> optThreads{
    "threads",
    cl::desc("Use at most this many threads for image loading and albedo "
             "tabulation, or 0 for every hardware thread (default 0); 1 runs "
             "them inline on this thread"),
    cl::init(0U), cl::sub(subsWithCompileOptions), cl::cat(catOptions)};
static cl::opt<smdl::DumpFormat> dumpFormat{
    "f", cl::desc("Dump format:"),
    cl::values(
        cl::OptionEnumValue{"llvm-ir", int(smdl::DUMP_FORMAT_IR), "LLVM-IR"},
        cl::OptionEnumValue{"asm", int(smdl::DUMP_FORMAT_ASM),
                            "Native assembly"},
        cl::OptionEnumValue{"obj", int(smdl::DUMP_FORMAT_OBJ),
                            "Native object file"}),
    cl::sub(subDump), cl::cat(catOptions)};
static cl::opt<std::string> outputFilename{
    "output", cl::desc("Output filename (default stdout)"), cl::Optional,
    cl::sub(subsWithOutputFile), cl::cat(catOptions)};

// NOTE: LLVM registers a `--color` of its own on the top-level
// subcommand, in a hidden category that `HideUnrelatedOptions` filters
// out, so it never appears in any `--help`. It is nonetheless accepted
// everywhere, because an option a subcommand does not recognize falls
// back to the top-level lookup (`CommandLine.cpp`, `LookupLongOption`).
// That is why `smdl list --color` is quietly tolerated and does
// nothing.
//
// This option shadows it for `doc` and `test`, since the subcommand is
// searched first, and drives the coloring explicitly, which keeps the
// behavior independent of that LLVM-internal option. Keep it scoped to
// subcommands: registering a `--color` at the top level would land in
// the same option map as LLVM's, and `cl` aborts on a duplicate name.
static cl::opt<cl::boolOrDefault> colorOption{
    "color", cl::desc("Colorize the output (default autodetect)"),
    cl::init(cl::boolOrDefault::BOU_UNSET), cl::sub(subsWithColor),
    cl::cat(catOptions)};

static cl::opt<bool> formatInPlace{"i", cl::desc("Format in place"),
                                   cl::sub(subFormat), cl::cat(catOptions)};
static cl::opt<bool> formatNoComments{"no-comments",
                                      cl::desc("Remove comments"),
                                      cl::sub(subFormat), cl::cat(catOptions)};
static cl::opt<bool> formatKeepDocComments{
    "keep-doc-comments",
    cl::desc("Keep '///' doc comments despite '--no-comments'"),
    cl::sub(subFormat), cl::cat(catOptions)};
static cl::opt<bool> formatNoAnnotations{
    "no-annotations", cl::desc("Remove annotations"), cl::sub(subFormat),
    cl::cat(catOptions)};
static cl::opt<bool> formatCompact{"c",
                                   cl::desc("Format output more compactly"),
                                   cl::sub(subFormat), cl::cat(catOptions)};

enum DocOutputFormat : int {
  DOC_FORMAT_TEXT,
  DOC_FORMAT_JSON,
  DOC_FORMAT_MD,
};
static cl::opt<DocOutputFormat> docFormat{
    "f",
    cl::desc("Output format:"),
    cl::init(DOC_FORMAT_TEXT),
    cl::values(
        cl::OptionEnumValue{"text", int(DOC_FORMAT_TEXT),
                            "Plain text for symbol queries (default); "
                            "whole-database output falls back to Markdown"},
        cl::OptionEnumValue{"json", int(DOC_FORMAT_JSON), "JSON database"},
        cl::OptionEnumValue{"md", int(DOC_FORMAT_MD), "Markdown"}),
    cl::sub(subDoc),
    cl::cat(catOptions)};
static cl::opt<bool> docIncludeHidden{
    "all",
    cl::desc("Include hidden declarations, i.e., declarations not marked "
             "'export' or named with a leading underscore"),
    cl::sub(subDoc), cl::cat(catOptions)};
static cl::opt<bool> docAllBuiltins{"builtins",
                                    cl::desc("Include all builtin modules"),
                                    cl::sub(subDoc), cl::cat(catOptions)};

static cl::list<std::string> volumeGridNames{
    "grid",
    cl::desc("The grid to read from a NanoVDB input, and to name in a "
             "NanoVDB output. Repeat once per input, or give none: an "
             "unnamed input reads its first grid, and an unnamed output "
             "grid is named after the input file's stem"),
    cl::sub(subVolume), cl::cat(catOptions)};
static cl::opt<std::string> volumeOutput{
    "output",
    cl::desc("Output filename, whose extension ('.vol' or '.nvdb') selects "
             "the format. Without one, describe the inputs instead"),
    cl::sub(subVolume), cl::cat(catOptions)};

static cl::OptionCategory catState{"State Options"};
static cl::opt<unsigned> wavelengthBaseMax{
    "wavelength-base-max", cl::desc("Number of wavelengths (default 16)"),
    cl::init(16U), cl::sub(subsWithCompileOptions), cl::cat(catState)};
static cl::opt<float> minWavelen{
    "wavelength-min",
    cl::desc("Wavelength minimum in nanometers (default 380)"),
    cl::init(380.0f), cl::sub(subTest), cl::cat(catState)};
static cl::opt<float> maxWavelen{
    "wavelength-max",
    cl::desc("Wavelength maximum in nanometers (default 720)"),
    cl::init(720.0f), cl::sub(subTest), cl::cat(catState)};
static cl::opt<float> animationTime{
    "animation-time", cl::desc("Animation time (default 0)"), cl::init(0.0f),
    cl::sub(subTest), cl::cat(catState)};
static cl::opt<int> objectID{"object-id", cl::desc("Object ID (default 0)"),
                             cl::init(0), cl::sub(subTest), cl::cat(catState)};
static cl::opt<float> texCoordU{
    "texcoord-u", cl::desc("Texture coordinate U (default 0)"), cl::init(0.0f),
    cl::sub(subTest), cl::cat(catState)};
static cl::opt<float> texCoordV{
    "texcoord-v", cl::desc("Texture coordinate V (default 0)"), cl::init(0.0f),
    cl::sub(subTest), cl::cat(catState)};
static cl::opt<float> texCoordW{
    "texcoord-w", cl::desc("Texture coordinate W (default 0)"), cl::init(0.0f),
    cl::sub(subTest), cl::cat(catState)};
static cl::opt<int> ptexFaceID{
    "ptex-face-id", cl::desc("Ptex face ID (default 0)"), cl::init(0),
    cl::sub(subTest), cl::cat(catState)};
static cl::opt<float> ptexFaceU{
    "ptex-face-u", cl::desc("Ptex face coordinate U (default 0)"),
    cl::init(0.0f), cl::sub(subTest), cl::cat(catState)};
static cl::opt<float> ptexFaceV{
    "ptex-face-v", cl::desc("Ptex face coordinate V (default 0)"),
    cl::init(0.0f), cl::sub(subTest), cl::cat(catState)};

// The color scheme of the `doc` subcommand's plain text output:
// identity in blue, structure in cyan, and metadata in grey, with the
// documentation text left unstyled so that the prose stays the easiest
// thing to read.
static constexpr auto docColorName{llvm::HighlightColor::Tag};
static constexpr auto docColorSignature{llvm::HighlightColor::Attribute};
static constexpr auto docColorMetadata{llvm::HighlightColor::Note};

// The `doc` subcommand's plain text printer, for symbol and module
// queries. The Markdown and JSON printers live in the library, but this
// one colors as it goes, which a `std::string` cannot carry.
//
// NOTE: All coloring must go through `WithColor`, which is what detects
// the terminal. On POSIX, `raw_ostream::changeColor()` writes escape
// codes whether or not the stream is a terminal, so calling it directly
// would corrupt piped and redirected output.
class DocTextPrinter final {
public:
  DocTextPrinter(llvm::raw_ostream &os, llvm::ColorMode colorMode,
                 bool includeHidden)
      : mOS(os), mColorMode(colorMode), mIncludeHidden(includeHidden) {}

  // Print a module as its documentation text plus a listing of the
  // declarations in it.
  void printModule(const smdl::DocModule &mod) {
    mOS << "module ";
    emit(mod.qualifiedName, docColorName);
    mOS << '\n';
    if (!mod.docText.empty()) {
      mOS << '\n';
      emitIndented(mod.docText, 2, std::nullopt);
    }
    mOS << '\n';
    for (const auto &entry : mod.entries) {
      if (isHidden(entry)) continue;
      mOS.indent(2);
      emit(entry.qualifiedName, docColorName);
      emit(" (" + entry.kind + ")", docColorMetadata);
      mOS << '\n';
    }
    mOS << '\n';
  }

  // Print one declaration in full: signature, documentation text,
  // documented parameters, and visible members.
  void printEntry(const smdl::DocEntry &entry) {
    emit(entry.qualifiedName, docColorName);
    emit(" (" + entry.kind + ", line " + std::to_string(entry.lineNo) + ")",
         docColorMetadata);
    mOS << '\n';
    emitSignature(entry, 2);
    if (!entry.docText.empty()) {
      mOS << '\n';
      emitIndented(entry.docText, 2, std::nullopt);
    }
    printParams(entry);
    printMembers(entry);
    mOS << '\n';
  }

private:
  [[nodiscard]] bool isHidden(const smdl::DocEntry &entry) const {
    return entry.isHidden() && !mIncludeHidden;
  }

  void emit(std::string_view text, std::optional<llvm::HighlightColor> color) {
    if (text.empty()) return; // Do not wrap nothing in escape codes
    auto str{llvm::StringRef(text.data(), text.size())};
    if (color) {
      llvm::WithColor(mOS, *color, mColorMode) << str;
    } else {
      mOS << str;
    }
  }

  // Emit indented text line by line. The indentation and the newlines
  // stay outside the colored span so that no escape code lands on a
  // blank line or stretches across the width of the terminal.
  void emitIndented(std::string_view text, size_t indent,
                    std::optional<llvm::HighlightColor> color) {
    size_t i{0};
    while (i < text.size()) {
      auto j{text.find('\n', i)};
      if (j == std::string_view::npos) j = text.size();
      // Leave blank lines truly blank instead of indenting them.
      if (j > i) {
        mOS.indent(indent);
        emit(text.substr(i, j - i), color);
      }
      mOS << '\n';
      i = j + 1;
    }
  }

  // Signatures are always a single line, so the declared name inside one
  // can be split out and colored to give the eye something to land on.
  void emitSignature(const smdl::DocEntry &item, size_t indent) {
    mOS.indent(indent);
    // NOTE: `nameOffset` is `NO_NAME_OFFSET` when the name does not
    // appear in the signature, which fails the bounds test below and
    // falls through to the unsplit signature.
    const auto begin{size_t(item.nameOffset)};
    const auto end{begin + item.name.size()};
    if (end <= item.signature.size()) {
      emit(std::string_view(item.signature).substr(0, begin),
           docColorSignature);
      emit(std::string_view(item.signature).substr(begin, item.name.size()),
           docColorName);
      emit(std::string_view(item.signature).substr(end), docColorSignature);
    } else {
      emit(item.signature, docColorSignature);
    }
    mOS << '\n';
  }

  // Print the documented parameters, skipping the undocumented ones,
  // which the signature already shows.
  void printParams(const smdl::DocEntry &entry) {
    auto anyParamDocs{false};
    for (const auto &param : entry.params)
      anyParamDocs |= !param.docText.empty();
    if (!anyParamDocs) return;
    mOS << '\n';
    auto afterDocText{false};
    for (const auto &param : entry.params) {
      if (param.docText.empty()) continue;
      if (afterDocText) mOS << '\n';
      emitIndented(param.name + ":", 2, docColorName);
      emitIndented(param.docText, 4, std::nullopt);
      afterDocText = true;
    }
  }

  // Print the visible members. NOTE: A member whose documentation text
  // ends the previous member is separated from it by a blank line, so
  // that multi-line texts do not run into the next signature.
  void printMembers(const smdl::DocEntry &entry) {
    auto anyMembers{false};
    auto afterDocText{false};
    for (const auto &member : entry.members) {
      if (isHidden(member)) continue;
      if (!anyMembers) {
        mOS << '\n';
        anyMembers = true;
      } else if (afterDocText) {
        mOS << '\n';
      }
      emitSignature(member, 2);
      if (!member.docText.empty())
        emitIndented(member.docText, 4, std::nullopt);
      afterDocText = !member.docText.empty();
    }
  }

  llvm::raw_ostream &mOS;

  llvm::ColorMode mColorMode;

  bool mIncludeHidden;
};

// Add the builtin modules named by the queries, or all of them, to the
// database. A module already added from an input file wins, so that
// documenting a local copy of a builtin shows the local copy.
static void loadBuiltinDocModules(smdl::DocDatabase &docs,
                                  const std::vector<std::string> &queries) {
  const auto builtinNames{smdl::getBuiltinModuleNames()};
  auto loadBuiltin{[&](std::string_view name) {
    for (const auto &mod : docs.modules)
      if (mod.name == name) return;
    if (auto mod{smdl::extractBuiltinDocModule(name)})
      docs.modules.push_back(std::move(*mod));
  }};
  if (docAllBuiltins) {
    for (const auto &name : builtinNames) loadBuiltin(name);
    return;
  }
  for (const auto &query : queries) {
    for (const auto &name : builtinNames) {
      auto prefix{"::" + std::string(name)};
      if (query == prefix || smdl::startsWith(query, prefix + "::"))
        loadBuiltin(name);
    }
  }
}

// Run the `doc` subcommand: `queries` holds the `::`-prefixed
// positional arguments, everything else was added to the compiler.
static void runDocSubcommand(smdl::Compiler &compiler,
                             const std::vector<std::string> &queries) {
  auto docs{smdl::DocDatabase{}};
  if (auto error{compiler.extractDocs(docs)}) {
    error->printAndExit();
  }
  loadBuiltinDocModules(docs, queries);
  if (docs.modules.empty()) {
    std::cerr << "nothing to document: pass input files, '::'-prefixed "
                 "queries, or '--builtins'\n";
    std::exit(EXIT_FAILURE);
  }
  // Open the destination up front: the text printer colors as it goes,
  // which a `std::string` cannot carry.
  auto errorCode{std::error_code{}};
  auto outputFile{std::optional<llvm::raw_fd_ostream>{}};
  if (outputFilename.getNumOccurrences()) {
    outputFile.emplace(outputFilename.getValue(), errorCode);
    if (errorCode) {
      std::cerr << "cannot open '" << outputFilename.getValue()
                << "': " << errorCode.message() << '\n';
      std::exit(EXIT_FAILURE);
    }
  }
  auto &os{outputFile ? static_cast<llvm::raw_ostream &>(*outputFile)
                      : llvm::outs()};
  // Colors are for a human reading a terminal: `--output` captures the
  // documentation into a file, and JSON and Markdown are machine and
  // document formats. Otherwise honor `--color`, and without it let
  // `WithColor` detect the terminal itself.
  const auto colorArg{colorOption.getValue()};
  const auto colorMode{
      outputFile || docFormat != DOC_FORMAT_TEXT ? llvm::ColorMode::Disable
      : colorArg == cl::boolOrDefault::BOU_TRUE  ? llvm::ColorMode::Enable
      : colorArg == cl::boolOrDefault::BOU_FALSE ? llvm::ColorMode::Disable
                                                 : llvm::ColorMode::Auto};
  if (queries.empty() || docFormat != DOC_FORMAT_TEXT) {
    // Whole-database output. Symbol queries only participate by loading
    // the builtin modules they name.
    if (!docIncludeHidden) docs.removeHidden();
    os << (docFormat == DOC_FORMAT_JSON ? docs.printJSON()
                                        : docs.printMarkdown());
  } else {
    auto printer{DocTextPrinter(os, colorMode, docIncludeHidden)};
    for (const auto &query : queries) {
      const smdl::DocModule *moduleMatch{};
      for (const auto &mod : docs.modules)
        if (mod.qualifiedName == query) moduleMatch = &mod;
      if (moduleMatch) {
        printer.printModule(*moduleMatch);
        continue;
      }
      auto found{docs.findSymbol(query)};
      if (found.empty()) {
        std::cerr << "no documentation found for '" << query << "'\n";
        std::exit(EXIT_FAILURE);
      }
      for (const auto *entry : found) printer.printEntry(*entry);
    }
  }
  os.flush();
}

[[nodiscard]] static bool isNanoVDBFileName(llvm::StringRef fileName) {
  return fileName.ends_with_insensitive(".nvdb");
}

// Describe what one voxel grid file holds. This mode earns its place
// beside the conversion: the maximum is what `tex::max_value()` returns
// for the grid and the world bounds are what `density_bound_min` and
// `density_bound_max` want, so it is where a hand-written volume
// material gets its numbers.
static void printVolumeInfo(const std::string &fileName,
                            const std::string &gridName,
                            const smdl::VoxelGrid &grid) {
  const auto extent{grid.getExtent()};
  const auto brickCount{grid.getBrickCount()};
  const auto boundMin{grid.getWorldBoundMin()};
  const auto boundMax{grid.getWorldBoundMax()};
  llvm::outs() << smdl::concat(
      smdl::bestPathForPrinting(fileName),
      gridName.empty() ? std::string()
                       : smdl::concat(": grid ", smdl::Quoted(gridName)),
      "\n  extent ", extent.x, " x ", extent.y, " x ", extent.z, " (",
      brickCount.x, " x ", brickCount.y, " x ", brickCount.z, " bricks)",
      "\n  background ", grid.getBackground(), //
      "\n  values ", grid.getMinValue(), " to ", grid.getMaxValue(),
      "\n  bounds [", boundMin.x, ", ", boundMin.y, ", ", boundMin.z, "] to [",
      boundMax.x, ", ", boundMax.y, ", ", boundMax.z, "]\n");
  llvm::outs().flush();
}

// The `volume` subcommand, which needs no `Compiler` at all: a voxel
// grid is a resource that stands on its own.
//
// `-grid` names the grid to READ from a NanoVDB input and the grid to
// WRITE into a NanoVDB output, which is what lets one flag carry every
// combination of the two formats. A Mitsuba volume holds one anonymous
// grid, so the name reaches it in neither direction.
static int runVolumeSubcommand() {
  if (!volumeGridNames.empty() && volumeGridNames.size() != inputFiles.size()) {
    std::cerr << "expected one -grid per input, or none at all\n";
    return EXIT_FAILURE;
  }
  const auto explicitName{[](size_t i) {
    return i < volumeGridNames.size() ? volumeGridNames[i] : std::string();
  }};
  auto grids{std::vector<std::unique_ptr<smdl::VoxelGrid>>()};
  auto writeNames{std::vector<std::string>()};
  for (size_t i = 0; i < inputFiles.size(); i++) {
    const auto &fileName{inputFiles[i]};
    auto grid{std::make_unique<smdl::VoxelGrid>()};
    // An unnamed NanoVDB input reads its first grid, so the name only
    // travels when it was actually asked for.
    const auto readName{isNanoVDBFileName(fileName) ? explicitName(i)
                                                    : std::string()};
    if (auto error{grid->loadFromFile(fileName, readName)})
      error->printAndExit();
    auto writeName{explicitName(i)};
    if (writeName.empty())
      writeName = std::filesystem::path(fileName).stem().string();
    grids.push_back(std::move(grid));
    writeNames.push_back(std::move(writeName));
  }
  if (volumeOutput.empty()) {
    for (size_t i = 0; i < grids.size(); i++)
      printVolumeInfo(inputFiles[i], explicitName(i), *grids[i]);
    return EXIT_SUCCESS;
  }
  if (grids.size() == 1) {
    if (auto error{grids[0]->saveToFile(
            volumeOutput,
            isNanoVDBFileName(volumeOutput) ? writeNames[0] : std::string())})
      error->printAndExit();
    return EXIT_SUCCESS;
  }
  auto pointers{std::vector<const smdl::VoxelGrid *>()};
  for (const auto &grid : grids) pointers.push_back(grid.get());
  if (auto error{
          smdl::VoxelGrid::saveToFile(volumeOutput, pointers, writeNames)})
    error->printAndExit();
  return EXIT_SUCCESS;
}

int main(int argc, char **argv) {
  llvm::InitLLVM X(argc, argv);
  smdl::Logger::get().addSink<smdl::LogSinks::print_to_cerr>();
  cl::SetVersionPrinter(
      [](llvm::raw_ostream &os) { os << smdl::BuildInfo::get().toString(); });
  cl::HideUnrelatedOptions({&catOptions});
  cl::ParseCommandLineOptions(argc, argv, "SpectralMDL compiler");
  // Before 'compile()', which is where the parallel work is and which
  // builds the thread pool that cannot be resized afterward.
  smdl::setThreadCount(unsigned(optThreads));
  auto compiler{smdl::Compiler{}};
  compiler.enableDebug = enableDebug;
  compiler.enableUnitTests = true;
  compiler.wavelengthBaseMax = wavelengthBaseMax;
  const auto colorArg{colorOption.getValue()};
  compiler.colorMode =
      colorArg == cl::boolOrDefault::BOU_TRUE    ? smdl::COLOR_MODE_ALWAYS
      : colorArg == cl::boolOrDefault::BOU_FALSE ? smdl::COLOR_MODE_NEVER
                                                 : smdl::COLOR_MODE_AUTO;
  if (inputFiles.empty() && !subDoc) {
    std::cerr << "expected at least one input\n";
    return EXIT_FAILURE;
  }
  // Before the loop below, which would reject a voxel grid as a source
  // file, and before the compiler this subcommand has no use for.
  if (subVolume) return runVolumeSubcommand();
  auto docQueries{std::vector<std::string>{}};
  for (const auto &inputFile : inputFiles) {
    if (subDoc && smdl::startsWith(inputFile, "::")) {
      docQueries.push_back(inputFile);
      continue;
    }
    if (auto error{compiler.add(std::string(inputFile))}) {
      error->printAndExit();
    }
  }
  if (subDoc) {
    runDocSubcommand(compiler, docQueries);
  } else if (subFormat) {
    smdl::FormatOptions options{};
    options.inPlace = formatInPlace;
    options.noComments = formatNoComments;
    options.keepDocComments = formatKeepDocComments;
    options.noAnnotations = formatNoAnnotations;
    options.compact = formatCompact;
    if (auto error{compiler.formatSourceFiles(options)}) {
      error->printAndExit();
    }
  } else {
    if (auto error{compiler.compile(smdl::OptLevel(unsigned(optLevel)))}) {
      error->printAndExit();
    }
    if (subDump) {
      auto dumped{std::string{}};
      if (auto error{compiler.dump(dumpFormat, dumped)}) {
        error->printAndExit();
      }
      if (outputFilename.getNumOccurrences()) {
        auto ofs{std::ofstream(outputFilename.getValue())};
        if (!ofs.is_open()) std::exit(EXIT_FAILURE);
        ofs << dumped;
      } else {
        std::cout << dumped;
        std::cout.flush();
      }
    } else if (subList) {
      std::cout << compiler.printMaterialSummary();
      std::cout.flush();
    } else if (subRun || subTest) {
      if (auto error{compiler.jitCompile()}) error->printAndExit();
      if (auto error{compiler.runExecs()}) error->printAndExit();
      if (subTest) {
        auto wavelengths{
            std::vector<float>(size_t(compiler.wavelengthBaseMax))};
        smdl::BumpPtrAllocator allocator{};
        smdl::State state{};
        state.allocator = &allocator;
        state.texture_coordinate[0][0] = texCoordU;
        state.texture_coordinate[0][1] = texCoordV;
        state.texture_coordinate[0][2] = texCoordW;
        state.animation_time = animationTime;
        state.object_id = objectID;
        state.ptex_face_id = ptexFaceID;
        state.ptex_face_uv[0] = ptexFaceU;
        state.ptex_face_uv[1] = ptexFaceV;
        state.wavelength_min = minWavelen;
        state.wavelength_max = maxWavelen;
        state.wavelength_base = &wavelengths[0];
        // Endpoint-inclusive uniform grid; a single band sits at the
        // midpoint (the general formula is 0/0 there).
        for (unsigned i = 0; i < compiler.wavelengthBaseMax; i++) {
          float fac = compiler.wavelengthBaseMax > 1
                          ? float(i) / float(compiler.wavelengthBaseMax - 1)
                          : 0.5f;
          wavelengths[i] =
              (1 - fac) * state.wavelength_min + fac * state.wavelength_max;
        }
        if (auto error{compiler.runUnitTests(state)}) {
          std::cerr << '\n';
          error->printAndExit();
        }
      }
    }
  }
  return 0;
}
