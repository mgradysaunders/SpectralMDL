#include "Module.h"

#include "Context.h"
#include "Emitter.h"
#include "Parser.h"

namespace smdl::Compiler {

void Module::parse(Context &context) {
  sanity_check(text != nullptr);
  root = Parser(context.bumpAllocator, path, text->getBuffer()).parse();
}

void Module::emit(Context &context) {
  if (status != Status::Finished) {
    if (status == Status::InProgress)
      throw Error(std::format("module '{}' reached through cyclic 'import' sequence", name));
    status = Status::InProgress;
    Emitter emitter{context, this, /*crumb=*/nullptr};
    emitter.emit(*root);
    lastCrumb = emitter.crumb;
    status = Status::Finished;
  }
}

} // namespace smdl::Compiler
