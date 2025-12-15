#include "smdl/Module.h"
#include "smdl/Parser.h"
#include "smdl/Support/Profiler.h"

#include "Compiler/Emitter.h"
#include "Formatter.h"

#include <filesystem>
#include <iostream>

namespace smdl {

Module::Module(std::string name, std::string sourceCode)
    : name(std::move(name)), sourceCode(std::move(sourceCode)) {}

Module::~Module() {}

std::unique_ptr<Module> Module::loadFromFile(const std::string &fileName) {
  auto module_{std::make_unique<Module>()};
  module_->fileName = fileName;
  module_->name = std::filesystem::path(fileName).stem().string();
  module_->sourceCode = readOrThrow(fileName);
  return module_;
}

std::unique_ptr<Module>
Module::loadFromFileExtractedFromArchive(const std::string &fileName,
                                         const std::string &file) {
  auto module_{std::make_unique<Module>()};
  module_->isExtractedFromArchive = true;
  module_->fileName = fileName;
  module_->name = std::filesystem::path(fileName).stem().string();
  module_->sourceCode = file;
  return module_;
}

std::optional<Error> Module::parse(BumpPtrAllocator &allocator) noexcept {
  return catchAndReturnError([&] {
    if (!root) {
      SMDL_PROFILER_ENTRY("Module::parse()",
                          isBuiltin() ? name.c_str() : fileName.c_str());
      root = Parser(allocator, *this).parse();
    }
  });
}

std::optional<Error> Module::compile(Context &context) noexcept {
  if (!isParsed()) {
    return Error("module not yet parsed");
  }
  return catchAndReturnError([&] {
    if (compileStatus == COMPILE_STATUS_IN_PROGRESS)
      throw Error(concat("detected cyclic import of module ", Quoted(name)));
    if (compileStatus == COMPILE_STATUS_NOT_STARTED) {
      compileStatus = COMPILE_STATUS_IN_PROGRESS;
      SMDL_PROFILER_ENTRY("Module::compile()",
                          isBuiltin() ? name.c_str() : fileName.c_str());
      SMDL_PRESERVE(context.currentModule);
      context.currentModule = this;
      Emitter emitter{context};
      emitter.emit(root);
      lastCrumb = emitter.crumb;
      compileStatus = COMPILE_STATUS_FINISHED;
    }
  });
}

std::optional<Error>
Module::formatSourceCode(const FormatOptions &formatOptions) noexcept {
  if (isBuiltin()) {
    return Error(concat("cannot format builtin module ", Quoted(name)));
  }
  if (!isParsed()) {
    auto allocator{BumpPtrAllocator{}};
    if (auto error{parse(allocator)})
      return error;
    auto error{formatSourceCode(formatOptions)};
    root = {};
    return error;
  }
  return catchAndReturnError([&] {
    SMDL_PROFILER_ENTRY("Module::format_source_code()",
                        isBuiltin() ? name.c_str() : fileName.c_str());
    auto formatter{Formatter{formatOptions}};
    auto formatted{formatter.format(sourceCode, *root)};
    if (formatOptions.inPlace) {
      if (isExtractedFromArchive) {
        throw Error(
            concat("cannot format module extracted from archive in-place ",
                   Quoted(fileName)));
      }
      auto stream{openOrThrow(fileName, std::ios::out)};
      stream << formatted;
    } else {
      std::cout << formatted;
      std::cout.flush();
    }
  });
}

bool Module::isSMDLSyntax() const noexcept {
  return root && root->is_smdl_syntax();
}

void Module::reset() noexcept {
  root.reset();
  compileStatus = COMPILE_STATUS_NOT_STARTED;
  lastCrumb = nullptr;
}

} // namespace smdl
