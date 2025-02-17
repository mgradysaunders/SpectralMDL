#include "smdl/Module.h"
#include "smdl/Parser.h"

#include "Compiler/Emitter.h"

#include "filesystem.h"

namespace smdl {

std::unique_ptr<Module> Module::load_from_file(const std::string &fileName) {
  auto ifs{std::ifstream(fileName, std::ios::in)};
  if (!ifs.is_open())
    throw std::runtime_error(concat("cannot open file ", quoted(fileName)));
  auto mod{std::make_unique<Module>()};
  mod->fileName = fileName;
  mod->name = fs_make_path(fileName).stem().string();
  mod->sourceCode = std::string((std::istreambuf_iterator<char>(ifs)),
                                std::istreambuf_iterator<char>());
  return mod;
}

std::optional<Error> Module::parse(BumpPtrAllocator &allocator) {
  return catch_and_return_error([&] {
    if (!root)
      root = Parser(allocator, *this).parse();
  });
}

std::optional<Error> Module::compile(Context &context) {
  SMDL_SANITY_CHECK(is_parsed(), "tried to compile before parse");
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

} // namespace smdl
