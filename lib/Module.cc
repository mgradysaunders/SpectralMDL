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
    : mSourceCode(std::move(sourceCode)) {
  // The name is the builtin lookup key, e.g., `models::prospect`: the
  // last component is the module name and the components together form
  // the qualified name.
  auto components{splitQualifiedName(name)};
  if (components.empty()) {
    mName = std::move(name);
  } else {
    mQualifiedName = joinQualifiedName(components);
    mName = std::string(components.back());
  }
}

Module::~Module() {}

/// Derive the qualified module name, e.g., `::vendor::metals::steel`
/// for `<searchRoot>/vendor/metals/steel.mdl`. Falls back to the bare
/// `::stem` if the file name is not lexically under the search root.
[[nodiscard]] static std::string
deriveQualifiedName(const std::string &fileName,
                    const std::string &searchRoot) {
  auto filePath{std::filesystem::path(fileName)};
  auto relative{filePath.lexically_relative(searchRoot)};
  if (relative.empty() || *relative.begin() == "..") {
    return "::" + filePath.stem().string();
  }
  auto name{std::string()};
  for (auto itr{relative.begin()}; itr != relative.end(); ++itr) {
    name += "::";
    name += std::next(itr) == relative.end() ? itr->stem().string()
                                             : itr->string();
  }
  return name;
}

std::unique_ptr<Module> Module::loadFromFile(const std::string &fileName,
                                             const std::string &searchRoot) {
  auto module_{std::make_unique<Module>()};
  module_->mFileName = fileName;
  module_->mName = std::filesystem::path(fileName).stem().string();
  module_->mSearchRoot =
      searchRoot.empty() ? parentPathOf(fileName) : searchRoot;
  module_->mQualifiedName = deriveQualifiedName(fileName, module_->mSearchRoot);
  module_->mSourceCode = readOrThrow(fileName);
  return module_;
}

std::unique_ptr<Module>
Module::loadFromFileExtractedFromArchive(const std::string &archiveFileName,
                                         const std::string &entryName,
                                         const std::string &file,
                                         const std::string &searchRoot) {
  auto module_{std::make_unique<Module>()};
  module_->mIsExtractedFromArchive = true;
  module_->mFileName = joinPaths(archiveFileName, entryName);
  module_->mName = std::filesystem::path(entryName).stem().string();
  module_->mSearchRoot =
      searchRoot.empty() ? parentPathOf(archiveFileName) : searchRoot;
  // The entry path encodes the package structure, so derive the
  // qualified name as if the archive were extracted in place at the
  // top level of the search root.
  module_->mQualifiedName =
      deriveQualifiedName(module_->mFileName, archiveFileName);
  module_->mSourceCode = file;
  return module_;
}

std::unique_ptr<Module> Module::loadFromMDLE(const std::string &mdleFileName,
                                             const std::string &file,
                                             const std::string &qualifiedName,
                                             const std::string &resourceDirectory) {
  auto module_{std::make_unique<Module>()};
  module_->mIsExtractedFromArchive = true;
  module_->mFileName = joinPaths(mdleFileName, "main.mdl");
  module_->mName = std::filesystem::path(mdleFileName).stem().string();
  module_->mSearchRoot = parentPathOf(mdleFileName);
  module_->mQualifiedName = qualifiedName;
  module_->mResourceDirectory = resourceDirectory;
  module_->mSourceCode = file;
  return module_;
}

std::optional<Error> Module::parse(BumpPtrAllocator &allocator) noexcept {
  return catchAndReturnError([&] {
    if (!mRoot) {
      SMDL_PROFILER_ENTRY("Module::parse()",
                          isBuiltin() ? mName.c_str() : mFileName.c_str());
      mRoot = Parser(allocator, *this).parse();
    }
  });
}

std::optional<Error> Module::compile(Context &context) noexcept {
  if (!isParsed()) {
    return Error("module not yet parsed");
  }
  return catchAndReturnError([&] {
    if (mCompileStatus == COMPILE_STATUS_IN_PROGRESS)
      throw Error(concat("detected cyclic import of module ", Quoted(mName)));
    if (mCompileStatus == COMPILE_STATUS_FAILED)
      throw Error(mCompileErrorMessage);
    if (mCompileStatus == COMPILE_STATUS_NOT_STARTED) {
      // On failure, mark FAILED and remember the original error: the
      // module must not be re-emitted (that would duplicate symbols), and
      // a later reference must reproduce the original diagnostic instead
      // of misreporting a cyclic import.
      mCompileStatus = COMPILE_STATUS_IN_PROGRESS;
      try {
        SMDL_PROFILER_ENTRY("Module::compile()",
                            isBuiltin() ? mName.c_str() : mFileName.c_str());
        SMDL_PRESERVE(context.currentModule, context.currentNamespacePath);
        context.currentModule = this;
        // Always start from an empty namespace path: this module may be
        // compiled recursively from the middle of another module's
        // namespace, which must not leak into our material names.
        context.currentNamespacePath.clear();
        Emitter emitter{context};
        emitter.emit(mRoot);
        mRootScope = emitter.scope;
        mCompileStatus = COMPILE_STATUS_FINISHED;
      } catch (const Error &error) {
        mCompileStatus = COMPILE_STATUS_FAILED;
        mCompileErrorMessage = error.message;
        throw;
      } catch (...) {
        mCompileStatus = COMPILE_STATUS_FAILED;
        mCompileErrorMessage =
            concat("module ", Quoted(mName), " previously failed to compile");
        throw;
      }
    }
  });
}

std::optional<Error>
Module::formatSourceCode(const FormatOptions &formatOptions) noexcept {
  if (isBuiltin()) {
    return Error(concat("cannot format builtin module ", Quoted(mName)));
  }
  if (!isParsed()) {
    auto allocator{BumpPtrAllocator{}};
    if (auto error{parse(allocator)})
      return error;
    auto error{formatSourceCode(formatOptions)};
    mRoot = {};
    return error;
  }
  return catchAndReturnError([&] {
    SMDL_PROFILER_ENTRY("Module::formatSourceCode()",
                        isBuiltin() ? mName.c_str() : mFileName.c_str());
    auto formatter{Formatter{formatOptions}};
    auto formatted{formatter.format(mSourceCode, *mRoot)};
    if (formatOptions.inPlace) {
      if (mIsExtractedFromArchive) {
        throw Error(
            concat("cannot format module extracted from archive in-place ",
                   Quoted(mFileName)));
      }
      auto stream{openOrThrow(mFileName, std::ios::out)};
      stream << formatted;
    } else {
      std::cout << formatted;
      std::cout.flush();
    }
  });
}

bool Module::isSMDLSyntax() const noexcept {
  return mRoot && mRoot->isSMDLSyntax();
}

void Module::reset() noexcept {
  mRoot.reset();
  mCompileStatus = COMPILE_STATUS_NOT_STARTED;
  mCompileErrorMessage.clear();
  mRootScope = nullptr;
}

} // namespace smdl
