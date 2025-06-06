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
  module_->name = fs_make_path(fileName).stem().string();
  module_->sourceCode = fs_read_file(fileName);
  return module_;
}

std::optional<Error> Module::parse(BumpPtrAllocator &allocator) {
  return catch_and_return_error([&] {
    if (!root)
      root = Parser(allocator, *this).parse();
  });
}

std::optional<Error> Module::compile(Context &context) {
  if (!is_parsed())
    return Error("module not yet parsed");
  return catch_and_return_error([&] {
    if (compileStatus == CompileStatus::InProgress)
      throw Error(concat("detected cyclic import of module ", quoted(name)));
    if (compileStatus == CompileStatus::NotStarted) {
      compileStatus = CompileStatus::InProgress;
      Emitter emitter{context};
      emitter.emit(root);
      lastCrumb = emitter.crumb;
      compileStatus = CompileStatus::Finished;
    }
  });
}

std::optional<Error> Module::format_source_code(const FormatOptions &options) {
  if (is_builtin())
    return Error(concat("cannot format builtin module ", quoted(name)));
  if (!is_parsed()) {
    auto allocator{BumpPtrAllocator{}};
    if (auto error{parse(allocator)})
      return error;
    auto error{format_source_code(options)};
    root = {};
    return error;
  } else {
    auto formatter{Formatter{options}};
    auto formatted{formatter.format(sourceCode, *root)};
    if (options.inPlace) {
      auto outStream{std::ofstream(fileName, std::ios::out)};
      if (!outStream.is_open())
        return Error(concat("cannot open ", quoted(fileName)));
      outStream << formatted;
    } else {
      std::cout << formatted;
      std::cout.flush();
    }
    return std::nullopt;
  }
}

void Module::reset() {
  root.reset();
  compileStatus = CompileStatus::NotStarted;
  lastCrumb = nullptr;
}

} // namespace smdl
