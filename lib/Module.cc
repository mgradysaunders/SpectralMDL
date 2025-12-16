#include "smdl/Module.h"
#include "smdl/Parser.h"
#include "smdl/Support/Profiler.h"

#include "Compiler/Emitter.h"
#include "Formatter.h"

#include <filesystem>
#include <iostream>

namespace smdl {

Module::Module(std::string name, std::string sourceCode)
    : mName(std::move(name)), mSourceCode(std::move(sourceCode)) {}

Module::~Module() {}

std::unique_ptr<Module> Module::loadFromFile(const std::string &fileName) {
  auto module_{std::make_unique<Module>()};
  module_->mFileName = fileName;
  module_->mName = std::filesystem::path(fileName).stem().string();
  module_->mSourceCode = readOrThrow(fileName);
  return module_;
}

std::unique_ptr<Module>
Module::loadFromFileExtractedFromArchive(const std::string &fileName,
                                         const std::string &file) {
  auto module_{std::make_unique<Module>()};
  module_->mIsExtractedFromArchive = true;
  module_->mFileName = fileName;
  module_->mName = std::filesystem::path(fileName).stem().string();
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
    if (mCompileStatus == COMPILE_STATUS_NOT_STARTED) {
      mCompileStatus = COMPILE_STATUS_IN_PROGRESS;
      SMDL_PROFILER_ENTRY("Module::compile()",
                          isBuiltin() ? mName.c_str() : mFileName.c_str());
      SMDL_PRESERVE(context.currentModule);
      context.currentModule = this;
      Emitter emitter{context};
      emitter.emit(mRoot);
      mLastCrumb = emitter.crumb;
      mCompileStatus = COMPILE_STATUS_FINISHED;
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
    SMDL_PROFILER_ENTRY("Module::format_source_code()",
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
  return mRoot && mRoot->is_smdl_syntax();
}

void Module::reset() noexcept {
  mRoot.reset();
  mCompileStatus = COMPILE_STATUS_NOT_STARTED;
  mLastCrumb = nullptr;
}

} // namespace smdl
