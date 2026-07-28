#include "smdl/Compiler.h"
#include "smdl/Support/Logger.h"
#include <fstream>
#include <iostream>
#include <optional>
#include <system_error>

#include "llvm/Support/CommandLine.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/WithColor.h"
#include "llvm/Support/raw_ostream.h"

namespace cl = llvm::cl;
static cl::OptionCategory optionsCat{"Options"};
static cl::SubCommand subDump{"dump", "Dump as LLVM-IR or native assembly"};
static cl::SubCommand subList{"list", "List all materials"};
static cl::SubCommand subRun{"run", "Run execs"};
static cl::SubCommand subTest{"test", "Run execs and unit tests"};
static cl::SubCommand subFormat{"format", "Format source code"};
static cl::SubCommand subDoc{"doc", "Show documentation"};
static cl::SubCommandGroup subsWithCompileOptions{&subDump, &subList, &subRun,
                                                  &subTest};
static cl::SubCommandGroup allSubs{&subDump, &subList,   &subRun,
                                   &subTest, &subFormat, &subDoc};

// NOTE: This is `ZeroOrMore` only so that `smdl doc --builtins` works
// with no inputs; every other subcommand requires at least one input,
// enforced in `main()`.
static cl::list<std::string> inputFiles{cl::Positional, cl::desc("<input>"),
                                        cl::ZeroOrMore, cl::sub(allSubs),
                                        cl::cat(optionsCat)};

static cl::opt<unsigned> optLevel{"O",
                                  cl::desc("Optimization level (default 2)"),
                                  cl::Prefix,
                                  cl::init(2U),
                                  cl::sub(subsWithCompileOptions),
                                  cl::cat(optionsCat)};
static cl::opt<bool> enableDebug{"g", cl::desc("Enable debugging"),
                                 cl::sub(subsWithCompileOptions),
                                 cl::cat(optionsCat)};
static cl::opt<smdl::DumpFormat> dumpFormat{
    "f", cl::desc("Dump format:"),
    cl::values(
        cl::OptionEnumValue{"llvm-ir", int(smdl::DUMP_FORMAT_IR), "LLVM-IR"},
        cl::OptionEnumValue{"asm", int(smdl::DUMP_FORMAT_ASM),
                            "Native assembly"},
        cl::OptionEnumValue{"obj", int(smdl::DUMP_FORMAT_OBJ),
                            "Native object file"}),
    cl::sub(subDump), cl::cat(optionsCat)};
static cl::opt<std::string> outputFilename{
    "output", cl::desc("Output filename (default stdout)"), cl::Optional,
    cl::sub(subDump), cl::cat(optionsCat)};

static cl::opt<bool> formatInPlace{"i", cl::desc("Format in place"),
                                   cl::sub(subFormat), cl::cat(optionsCat)};
static cl::opt<bool> formatNoComments{"no-comments",
                                      cl::desc("Remove comments"),
                                      cl::sub(subFormat), cl::cat(optionsCat)};
static cl::opt<bool> formatKeepDocComments{
    "keep-doc-comments",
    cl::desc("Keep '///' doc comments despite '--no-comments'"),
    cl::sub(subFormat), cl::cat(optionsCat)};
static cl::opt<bool> formatNoAnnotations{
    "no-annotations", cl::desc("Remove annotations"), cl::sub(subFormat),
    cl::cat(optionsCat)};
static cl::opt<bool> formatCompact{"c",
                                   cl::desc("Format output more compactly"),
                                   cl::sub(subFormat), cl::cat(optionsCat)};

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
    cl::cat(optionsCat)};
static cl::opt<std::string> docOutputFilename{
    "output", cl::desc("Output filename (default stdout)"), cl::Optional,
    cl::sub(subDoc), cl::cat(optionsCat)};
static cl::opt<bool> docIncludeHidden{
    "all",
    cl::desc("Include hidden declarations, i.e., declarations not marked "
             "'export' or named with a leading underscore"),
    cl::sub(subDoc), cl::cat(optionsCat)};
static cl::opt<bool> docAllBuiltins{"builtins",
                                    cl::desc("Include all builtin modules"),
                                    cl::sub(subDoc), cl::cat(optionsCat)};
// NOTE: LLVM registers a `--color` of its own, but only on the top-level
// subcommand, so it never appears in `smdl doc --help`. This shadows it
// for the `doc` subcommand and drives `WithColor` explicitly, which also
// keeps the behavior independent of that LLVM-internal option.
static cl::opt<cl::boolOrDefault> docColor{
    "color", cl::desc("Colorize the text output (default autodetect)"),
    cl::init(cl::BOU_UNSET), cl::sub(subDoc), cl::cat(optionsCat)};

static cl::OptionCategory catState{"State Options"};
static cl::opt<unsigned> wavelengthBaseMax{
    "wavelength_base_max", cl::desc("Number of wavelengths (default 16)"),
    cl::init(16U), cl::sub(subsWithCompileOptions), cl::cat(catState)};
static cl::opt<float> minWavelen{
    "wavelength_min",
    cl::desc("Wavelength minimum in nanometers (default 380)"),
    cl::init(380.0f), cl::sub(subTest), cl::cat(catState)};
static cl::opt<float> maxWavelen{
    "wavelength_max",
    cl::desc("Wavelength maximum in nanometers (default 720)"),
    cl::init(720.0f), cl::sub(subTest), cl::cat(catState)};
static cl::opt<float> animationTime{
    "animation_time", cl::desc("Animation time (default 0)"), cl::init(0.0f),
    cl::sub(subTest), cl::cat(catState)};
static cl::opt<int> objectID{"object_id", cl::desc("Object ID (default 0)"),
                             cl::init(0), cl::sub(subTest), cl::cat(catState)};
static cl::opt<float> texCoordU{
    "texcoord_u", cl::desc("Texture coordinate U (default 0)"), cl::init(0.0f),
    cl::sub(subTest), cl::cat(catState)};
static cl::opt<float> texCoordV{
    "texcoord_v", cl::desc("Texture coordinate V (default 0)"), cl::init(0.0f),
    cl::sub(subTest), cl::cat(catState)};
static cl::opt<float> texCoordW{
    "texcoord_w", cl::desc("Texture coordinate W (default 0)"), cl::init(0.0f),
    cl::sub(subTest), cl::cat(catState)};
static cl::opt<int> ptexFaceID{
    "ptex_face_id", cl::desc("Ptex face ID (default 0)"), cl::init(0),
    cl::sub(subTest), cl::cat(catState)};
static cl::opt<float> ptexFaceU{
    "ptex_face_u", cl::desc("Ptex face coordinate U (default 0)"),
    cl::init(0.0f), cl::sub(subTest), cl::cat(catState)};
static cl::opt<float> ptexFaceV{
    "ptex_face_v", cl::desc("Ptex face coordinate V (default 0)"),
    cl::init(0.0f), cl::sub(subTest), cl::cat(catState)};

/// The color scheme of the `doc` subcommand's plain text output:
/// identity in blue, structure in cyan, and metadata in grey, with the
/// documentation text left unstyled so that the prose stays the easiest
/// thing to read.
static constexpr auto docColorName{llvm::HighlightColor::Tag};
static constexpr auto docColorSignature{llvm::HighlightColor::Attribute};
static constexpr auto docColorMetadata{llvm::HighlightColor::Note};

/// Run the `doc` subcommand: `queries` holds the `::`-prefixed
/// positional arguments, everything else was added to the compiler.
static void runDocSubcommand(smdl::Compiler &compiler,
                             const std::vector<std::string> &queries) {
  auto docs{smdl::DocDatabase{}};
  if (auto error{compiler.extractDocs(docs)}) {
    error->printAndExit();
  }
  // Pull in the builtin modules named by the queries, or all of them.
  auto loadBuiltin{[&](std::string_view name) {
    for (const auto &mod : docs.modules)
      if (mod.name == name) return;
    if (auto mod{smdl::extractBuiltinDocModule(name)})
      docs.modules.push_back(std::move(*mod));
  }};
  if (docAllBuiltins) {
    for (const auto &name : smdl::getBuiltinModuleNames()) loadBuiltin(name);
  } else {
    for (const auto &query : queries) {
      for (const auto &name : smdl::getBuiltinModuleNames()) {
        auto prefix{"::" + std::string(name)};
        if (query == prefix || smdl::startsWith(query, prefix + "::"))
          loadBuiltin(name);
      }
    }
  }
  if (docs.modules.empty()) {
    std::cerr << "nothing to document: pass input files, '::'-prefixed "
                 "queries, or '--builtins'\n";
    std::exit(EXIT_FAILURE);
  }
  // Open the destination up front: the text printer colors as it goes,
  // which a `std::string` cannot carry.
  auto errorCode{std::error_code{}};
  auto outputFile{std::optional<llvm::raw_fd_ostream>{}};
  if (docOutputFilename.getNumOccurrences()) {
    outputFile.emplace(docOutputFilename.getValue(), errorCode);
    if (errorCode) {
      std::cerr << "cannot open '" << docOutputFilename.getValue()
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
  const auto colorMode{
      outputFile || docFormat != DOC_FORMAT_TEXT ? llvm::ColorMode::Disable
      : docColor.getValue() == cl::BOU_TRUE      ? llvm::ColorMode::Enable
      : docColor.getValue() == cl::BOU_FALSE     ? llvm::ColorMode::Disable
                                                 : llvm::ColorMode::Auto};
  if (queries.empty() || docFormat != DOC_FORMAT_TEXT) {
    // Whole-database output. Symbol queries only participate by loading
    // the builtin modules they name.
    if (!docIncludeHidden) docs.removeHidden();
    os << (docFormat == DOC_FORMAT_JSON ? docs.printJSON()
                                        : docs.printMarkdown());
  } else {
    // Plain text symbol and module queries.
    //
    // NOTE: All coloring must go through `WithColor`, which is what
    // detects the terminal. On POSIX, `raw_ostream::changeColor()` writes
    // escape codes whether or not the stream is a terminal, so calling it
    // directly would corrupt piped and redirected output.
    auto emit{
        [&](std::string_view text, std::optional<llvm::HighlightColor> color) {
          if (text.empty()) return; // Do not wrap nothing in escape codes
          auto str{llvm::StringRef(text.data(), text.size())};
          if (color) {
            llvm::WithColor(os, *color, colorMode) << str;
          } else {
            os << str;
          }
        }};
    // Emit indented text line by line. The indentation and the newlines
    // stay outside the colored span so that no escape code lands on a
    // blank line or stretches across the width of the terminal.
    auto emitIndented{[&](std::string_view text, size_t indent,
                          std::optional<llvm::HighlightColor> color) {
      size_t i{0};
      while (i < text.size()) {
        auto j{text.find('\n', i)};
        if (j == std::string_view::npos) j = text.size();
        // Leave blank lines truly blank instead of indenting them.
        if (j > i) {
          os.indent(indent);
          emit(text.substr(i, j - i), color);
        }
        os << '\n';
        i = j + 1;
      }
    }};
    // Signatures are always a single line, so the declared name inside one
    // can be split out and colored to give the eye something to land on.
    auto emitSignature{[&](const smdl::DocEntry &item, size_t indent) {
      os.indent(indent);
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
      os << '\n';
    }};
    auto printTextEntry{[&](const smdl::DocEntry &entry) {
      emit(entry.qualifiedName, docColorName);
      emit(" (" + entry.kind + ", line " + std::to_string(entry.lineNo) + ")",
           docColorMetadata);
      os << '\n';
      emitSignature(entry, 2);
      if (!entry.docText.empty()) {
        os << '\n';
        emitIndented(entry.docText, 2, std::nullopt);
      }
      // NOTE: A field whose documentation text ends the previous field is
      // separated from it by a blank line, so that multi-line texts do not
      // run into the next signature.
      auto anyParamDocs{false};
      for (const auto &param : entry.params)
        anyParamDocs |= !param.docText.empty();
      if (anyParamDocs) {
        os << '\n';
        auto afterDocText{false};
        for (const auto &param : entry.params) {
          if (param.docText.empty()) continue;
          if (afterDocText) os << '\n';
          emitIndented(param.name + ":", 2, docColorName);
          emitIndented(param.docText, 4, std::nullopt);
          afterDocText = true;
        }
      }
      auto anyMembers{false};
      auto afterDocText{false};
      for (const auto &member : entry.members) {
        if (member.isHidden() && !docIncludeHidden) continue;
        if (!anyMembers) {
          os << '\n';
          anyMembers = true;
        } else if (afterDocText) {
          os << '\n';
        }
        emitSignature(member, 2);
        if (!member.docText.empty())
          emitIndented(member.docText, 4, std::nullopt);
        afterDocText = !member.docText.empty();
      }
      os << '\n';
    }};
    for (const auto &query : queries) {
      const smdl::DocModule *moduleMatch{};
      for (const auto &mod : docs.modules)
        if (mod.qualifiedName == query) moduleMatch = &mod;
      if (moduleMatch) {
        os << "module ";
        emit(moduleMatch->qualifiedName, docColorName);
        os << '\n';
        if (!moduleMatch->docText.empty()) {
          os << '\n';
          emitIndented(moduleMatch->docText, 2, std::nullopt);
        }
        os << '\n';
        for (const auto &entry : moduleMatch->entries) {
          if (entry.isHidden() && !docIncludeHidden) continue;
          os.indent(2);
          emit(entry.qualifiedName, docColorName);
          emit(" (" + entry.kind + ")", docColorMetadata);
          os << '\n';
        }
        os << '\n';
        continue;
      }
      auto found{docs.findSymbol(query)};
      if (found.empty()) {
        std::cerr << "no documentation found for '" << query << "'\n";
        std::exit(EXIT_FAILURE);
      }
      for (const auto *entry : found) printTextEntry(*entry);
    }
  }
  os.flush();
}

int main(int argc, char **argv) {
  llvm::InitLLVM X(argc, argv);
  smdl::Logger::get().addSink<smdl::LogSinks::print_to_cerr>();
  cl::HideUnrelatedOptions({&optionsCat});
  cl::ParseCommandLineOptions(argc, argv, "SpectralMDL compiler");

  auto compiler{smdl::Compiler{}};
  compiler.enableDebug = enableDebug;
  compiler.enableUnitTests = true;
  compiler.wavelengthBaseMax = wavelengthBaseMax;
  if (inputFiles.empty() && !subDoc) {
    std::cerr << "expected at least one input\n";
    return EXIT_FAILURE;
  }
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
    if (auto error{compiler.formatSourceCode(options)}) {
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
        for (unsigned i = 0; i < compiler.wavelengthBaseMax; i++) {
          float fac = float(i) / float(compiler.wavelengthBaseMax - 1);
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
