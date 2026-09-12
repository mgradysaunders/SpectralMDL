#include <optional>
#include <string>
#include <system_error>

#include "llvm/Support/WithColor.h"
#include "llvm/Support/raw_ostream.h"

#include "smdl/Support/Error.h"
#include "smdl/Support/Strings.h"

#include "Doc.h"
#include "Options.h"

namespace {

// The color scheme of the plain text output: identity in blue, structure
// in cyan, and metadata in grey, with the documentation text left
// unstyled so that the prose stays the easiest thing to read.
constexpr llvm::HighlightColor docColorName{llvm::HighlightColor::Tag};
constexpr llvm::HighlightColor docColorSignature{
    llvm::HighlightColor::Attribute};
constexpr llvm::HighlightColor docColorMetadata{llvm::HighlightColor::Note};

// The plain text printer, for symbol and module queries. The Markdown
// and JSON printers live in the library, but this one colors as it goes,
// which a `std::string` cannot carry.
//
// NOTE: All coloring must go through `WithColor` and the resolved color
// mode. On POSIX, `raw_ostream::changeColor()` writes escape codes
// whether or not the stream is a terminal, so calling it directly would
// corrupt piped and redirected output.
class DocTextPrinter final {
public:
  DocTextPrinter(llvm::raw_ostream &os, llvm::ColorMode colorMode,
                 bool shouldIncludeHidden)
      : mOS(os), mColorMode(colorMode),
        mShouldIncludeHidden(shouldIncludeHidden) {}

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
    return entry.isHidden() && !mShouldIncludeHidden;
  }

  void emit(std::string_view text, std::optional<llvm::HighlightColor> color) {
    if (text.empty()) return; // Do not wrap nothing in escape codes
    llvm::StringRef str{text.data(), text.size()};
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
      size_t j{text.find('\n', i)};
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
    const size_t begin{size_t(item.nameOffset)};
    const size_t end{begin + item.name.size()};
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
    bool anyParamDocs{false};
    for (const auto &param : entry.params)
      anyParamDocs |= !param.docText.empty();
    if (!anyParamDocs) return;
    mOS << '\n';
    bool isAfterDocText{false};
    for (const auto &param : entry.params) {
      if (param.docText.empty()) continue;
      if (isAfterDocText) mOS << '\n';
      emitIndented(param.name + ":", 2, docColorName);
      emitIndented(param.docText, 4, std::nullopt);
      isAfterDocText = true;
    }
  }

  // Print the visible members. NOTE: A member whose documentation text
  // ends the previous member is separated from it by a blank line, so
  // that multi-line texts do not run into the next signature.
  void printMembers(const smdl::DocEntry &entry) {
    bool anyMembers{false};
    bool isAfterDocText{false};
    for (const auto &member : entry.members) {
      if (isHidden(member)) continue;
      if (!anyMembers) {
        mOS << '\n';
        anyMembers = true;
      } else if (isAfterDocText) {
        mOS << '\n';
      }
      emitSignature(member, 2);
      if (!member.docText.empty())
        emitIndented(member.docText, 4, std::nullopt);
      isAfterDocText = !member.docText.empty();
    }
  }

  llvm::raw_ostream &mOS;

  llvm::ColorMode mColorMode;

  bool mShouldIncludeHidden;
};

// Add the builtin modules named by the queries, or all of them, to the
// database. A module already added from an input file wins, so that
// documenting a local copy of a builtin shows the local copy.
void loadBuiltinDocModules(const Options &opts, smdl::DocDatabase &docs) {
  const std::vector<std::string_view> builtinNames{
      smdl::getBuiltinModuleNames()};
  auto loadBuiltin{[&](std::string_view name) {
    for (const auto &mod : docs.modules)
      if (mod.name == name) return;
    if (std::optional<smdl::DocModule> mod{smdl::extractBuiltinDocModule(name)})
      docs.modules.push_back(std::move(*mod));
  }};
  if (opts.doc.allBuiltins) {
    for (const auto &name : builtinNames) loadBuiltin(name);
    return;
  }
  for (const auto &query : opts.docQueries) {
    for (const auto &name : builtinNames) {
      std::string prefix{"::" + std::string(name)};
      if (query == prefix || smdl::startsWith(query, prefix + "::"))
        loadBuiltin(name);
    }
  }
}

} // namespace

void runDoc(const Options &opts, smdl::Compiler &compiler) {
  smdl::DocDatabase docs{};
  if (std::optional<smdl::Error> error{compiler.extractDocs(docs)})
    error->printAndExit();
  loadBuiltinDocModules(opts, docs);
  if (docs.modules.empty())
    throw smdl::Error("Nothing to document: pass input files, '::'-prefixed "
                      "queries, or '-builtins'");
  // Open the destination up front: the text printer colors as it goes,
  // which a `std::string` cannot carry.
  std::error_code errorCode{};
  std::optional<llvm::raw_fd_ostream> outputFile{};
  if (opts.output.fileName.wasGiven) {
    outputFile.emplace(opts.output.fileName.value, errorCode);
    if (errorCode)
      throw smdl::Error(smdl::concat("Cannot open ",
                                     smdl::Quoted(opts.output.fileName.value),
                                     ": ", errorCode.message()));
  }
  llvm::raw_ostream &os{outputFile
                            ? static_cast<llvm::raw_ostream &>(*outputFile)
                            : llvm::outs()};
  // Colors are for a human reading a terminal: '-output' captures the
  // documentation into a file, and JSON and Markdown are machine and
  // document formats. Otherwise '-color' resolves for standard output as
  // it does for the log on standard error.
  const llvm::ColorMode colorMode{
      !outputFile && opts.doc.format == DocFormat::TEXT &&
              smdl::shouldUseColors(opts.utility.ansiColorMode,
                                    smdl::coutSupportsANSIColors())
          ? llvm::ColorMode::Enable
          : llvm::ColorMode::Disable};
  if (opts.docQueries.empty() || opts.doc.format != DocFormat::TEXT) {
    // Whole-database output. Symbol queries only participate by loading
    // the builtin modules they name.
    if (!opts.doc.shouldIncludeHidden) docs.removeHidden();
    os << (opts.doc.format == DocFormat::JSON ? docs.printJSON()
                                              : docs.printMarkdown());
  } else {
    DocTextPrinter printer{os, colorMode, opts.doc.shouldIncludeHidden};
    for (const auto &query : opts.docQueries) {
      const smdl::DocModule *moduleMatch{};
      for (const auto &mod : docs.modules)
        if (mod.qualifiedName == query) moduleMatch = &mod;
      if (moduleMatch) {
        printer.printModule(*moduleMatch);
        continue;
      }
      std::vector<const smdl::DocEntry *> found{docs.findSymbol(query)};
      if (found.empty())
        throw smdl::Error(
            smdl::concat("No documentation found for ", smdl::Quoted(query)));
      for (const auto *entry : found) printer.printEntry(*entry);
    }
  }
  os.flush();
}
