#include "smdl/Module.h"
#include "smdl/Parser.h"

#include "Compiler/Emitter.h"
#include "Formatter.h"
#include "filesystem.h"

#include <iostream>

namespace smdl {

Module::Module(std::string name, std::string sourceCode)
    : name(std::move(name)), sourceCode(std::move(sourceCode)) {}

Module::~Module() {}

std::unique_ptr<Module> Module::load_from_file(const std::string &fileName) {
  auto module_{std::make_unique<Module>()};
  module_->fileName = fileName;
  module_->name = fs::path(fileName).stem().string();
  module_->sourceCode = read_or_throw(fileName);
  return module_;
}

std::unique_ptr<Module>
Module::load_from_file_extracted_from_archive(const std::string &fileName,
                                              const std::string &file) {
  auto module_{std::make_unique<Module>()};
  module_->isExtractedFromArchive = true;
  module_->fileName = fileName;
  module_->name = fs::path(fileName).stem().string();
  module_->sourceCode = file;
  return module_;
}

std::optional<Error> Module::parse(BumpPtrAllocator &allocator) noexcept {
  return catch_and_return_error([&] {
    if (!root)
      root = Parser(allocator, *this).parse();
  });
}

std::optional<Error> Module::compile(Context &context) noexcept {
  if (!is_parsed()) {
    return Error("module not yet parsed");
  }
  return catch_and_return_error([&] {
    if (compileStatus == COMPILE_STATUS_IN_PROGRESS)
      throw Error(concat("detected cyclic import of module ", quoted(name)));
    if (compileStatus == COMPILE_STATUS_NOT_STARTED) {
      compileStatus = COMPILE_STATUS_IN_PROGRESS;
      auto preserve{Preserve(context.currentModule)};
      context.currentModule = this;
      Emitter emitter{context};
      emitter.emit(root);
      lastCrumb = emitter.crumb;
      compileStatus = COMPILE_STATUS_FINISHED;
    }
  });
}

std::optional<Error>
Module::format_source_code(const FormatOptions &formatOptions) noexcept {
  if (is_builtin()) {
    return Error(concat("cannot format builtin module ", quoted(name)));
  }

  if (!is_parsed()) {
    auto allocator{BumpPtrAllocator{}};
    if (auto error{parse(allocator)})
      return error;
    auto error{format_source_code(formatOptions)};
    root = {};
    return error;
  }
  return catch_and_return_error([&] {
    auto formatter{Formatter{formatOptions}};
    auto formatted{formatter.format(sourceCode, *root)};
    if (formatOptions.inPlace) {
      if (is_extracted_from_archive()) {
        throw Error(
            concat("cannot format module extracted from archive in-place ",
                   quoted(fileName)));
      }
      auto stream{open_or_throw(fileName, std::ios::out)};
      stream << formatted;
    } else {
      std::cout << formatted;
      std::cout.flush();
    }
  });
}

void Module::reset() noexcept {
  root.reset();
  compileStatus = COMPILE_STATUS_NOT_STARTED;
  lastCrumb = nullptr;
}

} // namespace smdl
