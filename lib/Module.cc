#include "smdl/Module.h"
#include "smdl/Parser.h"
#include "smdl/Support/Profiler.h"
#include "smdl/Support/QualifiedName.h"

#include "Compiler/Emitter.h"
#include "Formatter.h"

#include <filesystem>
#include <iostream>

namespace smdl {

Module::Module(std::string name, std::string sourceCode)
    : sourceCode_(std::move(sourceCode)) {
  origin_ = ORIGIN_BUILTIN;
  // The name is the builtin lookup key, e.g., `models::prospect`: the
  // last component is the module name and the components together form
  // the qualified name.
  std::vector<std::string_view> components{splitQualifiedName(name)};
  if (components.empty()) {
    name_ = std::move(name);
  } else {
    qualifiedName_ = joinQualifiedName(components);
    name_ = std::string(components.back());
  }
  displayName_ =
      concat("<builtin ", qualifiedName_.empty() ? name_ : qualifiedName_, ">");
}

Module::~Module() {}

namespace {
// Derive the qualified module name, e.g., `::vendor::metals::steel`
// for `<searchRoot>/vendor/metals/steel.mdl`. Falls back to the bare
// `::stem` if the file name is not lexically under the search root.
[[nodiscard]] std::string deriveQualifiedName(const std::string &fileName,
                                              const std::string &searchRoot) {
  std::filesystem::path filePath{fileName};
  std::filesystem::path relative{filePath.lexically_relative(searchRoot)};
  if (relative.empty() || *relative.begin() == "..") {
    return "::" + filePath.stem().string();
  }
  std::string name{};
  for (auto itr{relative.begin()}; itr != relative.end(); ++itr) {
    name += "::";
    name +=
        std::next(itr) == relative.end() ? itr->stem().string() : itr->string();
  }
  return name;
}
} // namespace

std::unique_ptr<Module> Module::loadFromFile(const std::string &fileName,
                                             const std::string &searchRoot,
                                             const std::string &qualifiedName) {
  std::unique_ptr<Module> module{std::make_unique<Module>()};
  module->origin_ = ORIGIN_FILE;
  module->fileName_ = fileName;
  module->displayName_ = fileName;
  module->name_ = std::filesystem::path(fileName).stem().string();
  module->searchRoot_ =
      searchRoot.empty() ? parentPathOf(fileName) : searchRoot;
  module->qualifiedName_ =
      qualifiedName.empty() ? deriveQualifiedName(fileName, module->searchRoot_)
                            : qualifiedName;
  module->sourceCode_ = readOrThrow(fileName);
  return module;
}

std::unique_ptr<Module> Module::loadFromFileExtractedFromArchive(
    const std::string &archiveFileName, const std::string &entryName,
    const std::string &file, const std::string &searchRoot) {
  std::unique_ptr<Module> module{std::make_unique<Module>()};
  module->origin_ = ORIGIN_ARCHIVE;
  module->fileName_ = joinPaths(archiveFileName, entryName);
  module->displayName_ = module->fileName_;
  module->name_ = std::filesystem::path(entryName).stem().string();
  module->searchRoot_ =
      searchRoot.empty() ? parentPathOf(archiveFileName) : searchRoot;
  // The entry path encodes the package structure, so derive the
  // qualified name as if the archive were extracted in place at the
  // top level of the search root.
  module->qualifiedName_ =
      deriveQualifiedName(module->fileName_, archiveFileName);
  module->sourceCode_ = file;
  return module;
}

std::unique_ptr<Module>
Module::loadFromMDLE(const std::string &mdleFileName, const std::string &file,
                     const std::string &qualifiedName,
                     const std::string &resourceDirectory) {
  std::unique_ptr<Module> module{std::make_unique<Module>()};
  module->origin_ = ORIGIN_ARCHIVE;
  module->fileName_ = joinPaths(mdleFileName, "main.mdl");
  module->displayName_ = module->fileName_;
  module->name_ = std::filesystem::path(mdleFileName).stem().string();
  module->searchRoot_ = parentPathOf(mdleFileName);
  module->qualifiedName_ = qualifiedName;
  module->anchorDirectory_ = resourceDirectory;
  module->sourceCode_ = file;
  return module;
}

std::unique_ptr<Module>
Module::loadFromSourceCode(const std::string &qualifiedName,
                           std::string sourceCode,
                           const std::string &anchorDirectory) {
  std::unique_ptr<Module> module{std::make_unique<Module>()};
  std::vector<std::string_view> components{splitQualifiedName(qualifiedName)};
  module->origin_ = ORIGIN_SOURCE_CODE;
  module->displayName_ = concat("<string ", qualifiedName, ">");
  module->name_ =
      components.empty() ? qualifiedName : std::string(components.back());
  module->qualifiedName_ = qualifiedName;
  module->anchorDirectory_ = anchorDirectory;
  module->sourceCode_ = std::move(sourceCode);
  return module;
}

std::optional<Error> Module::parse(BumpPtrAllocator &allocator) noexcept {
  return catchAndReturnError([&] {
    if (!root_) {
      SMDL_PROFILER_ENTRY("Module::parse()", displayName_.c_str());
      root_ = Parser(allocator, *this).parse();
      computeSearchDirs_();
    }
  });
}

void Module::computeSearchDirs_() {
  searchDirs_.clear();
  for (const auto &searchDir : root_->searchDirs) {
    const SourceLocation &srcLoc{searchDir.path->srcLoc};
    if (searchDir.path->value.empty())
      srcLoc.throwError("The '#search_dir' path must not be empty");
    std::string dir{};
    try {
      dir = expandPathVariables(searchDir.path->value);
    } catch (const Error &error) {
      srcLoc.throwError(error.message);
    }
    // NOTE: 'makePathCanonical' expands a leading tilde, which must not
    // be mistaken for a path relative to the module directory.
    if (dir[0] != '~' && std::filesystem::path(dir).is_relative())
      dir = joinPaths(getDirectory(), dir);
    dir = makePathCanonical(std::move(dir));
    if (!isDirectory(dir))
      srcLoc.logWarn(concat("The '#search_dir' path ", SpellFilePath(dir),
                            " is not an existing directory"));
    searchDirs_.push_back(std::move(dir));
  }
}

std::optional<Error> Module::compile(Context &context) noexcept {
  if (!isParsed()) {
    return Error("Module not yet parsed");
  }
  return catchAndReturnError([&] {
    if (compileStatus_ == COMPILE_STATUS_IN_PROGRESS)
      throw Error(
          concat("Detected cyclic import of module ", SpellQuoted(name_)));
    if (compileStatus_ == COMPILE_STATUS_FAILED)
      throw Error(compileErrorMessage_);
    if (compileStatus_ == COMPILE_STATUS_NOT_STARTED) {
      // On failure, mark FAILED and remember the original error: the
      // module must not be re-emitted (that would duplicate symbols), and
      // a later reference must reproduce the original diagnostic instead
      // of misreporting a cyclic import.
      compileStatus_ = COMPILE_STATUS_IN_PROGRESS;
      try {
        SMDL_PROFILER_ENTRY("Module::compile()", displayName_.c_str());
        SMDL_PRESERVE(context.currentModule, context.currentNamespacePath);
        context.currentModule = this;
        context.modulesInProgress.push_back(this);
        SMDL_DEFER([&] { context.modulesInProgress.pop_back(); });
        // Always start from an empty namespace path: this module may be
        // compiled recursively from the middle of another module's
        // namespace, which must not leak into our material names.
        context.currentNamespacePath.clear();
        Emitter emitter{context};
        emitter.emit(root_);
        emitter.compileMaterials();
        emitter.checkOpenImplementations();
        rootScope_ = emitter.scope;
        compileStatus_ = COMPILE_STATUS_FINISHED;
      } catch (const Error &error) {
        compileStatus_ = COMPILE_STATUS_FAILED;
        compileErrorMessage_ = error.message;
        throw;
      } catch (...) {
        compileStatus_ = COMPILE_STATUS_FAILED;
        compileErrorMessage_ = concat("module ", SpellQuoted(name_),
                                      " previously failed to compile");
        throw;
      }
    }
  });
}

std::optional<Error>
Module::formatSourceFiles(const FormatOptions &formatOptions) noexcept {
  if (!isFileBacked()) {
    return Error(concat("Cannot format ", SpellQuoted(displayName_),
                        " because the module has no file"));
  }
  if (!isParsed()) {
    BumpPtrAllocator allocator{};
    if (std::optional<Error> error{parse(allocator)}) return error;
    std::optional<Error> error{formatSourceFiles(formatOptions)};
    root_ = {};
    return error;
  }
  return catchAndReturnError([&] {
    SMDL_PROFILER_ENTRY("Module::formatSourceFiles()", displayName_.c_str());
    Formatter formatter{formatOptions};
    std::string formatted{formatter.format(sourceCode_, *root_)};
    if (formatOptions.isInPlace) {
      if (isExtractedFromArchive()) {
        throw Error(
            concat("Cannot format module extracted from archive in-place ",
                   SpellFilePath(fileName_)));
      }
      std::basic_fstream<char> stream{openOrThrow(fileName_, std::ios::out)};
      stream << formatted;
    } else {
      std::cout << formatted;
      std::cout.flush();
    }
  });
}

bool Module::isSMDLSyntax() const noexcept {
  return root_ && root_->isSMDLSyntax();
}

void Module::reset() noexcept {
  root_.reset();
  searchDirs_.clear();
  compileStatus_ = COMPILE_STATUS_NOT_STARTED;
  compileErrorMessage_.clear();
  rootScope_ = nullptr;
}

} // namespace smdl
