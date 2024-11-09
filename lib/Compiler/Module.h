#pragma once

#include "Function.h"

namespace smdl::Compiler {

class Module final {
public:
  enum class Status : uint32_t { NotStarted, InProgress, Finished };

  explicit Module(std::filesystem::path path)
      : path(path.string()),        //
        name(path.stem().string()), //
        text(llvm_throw_if_error(llvm::MemoryBuffer::getFile(this->path, /*isText=*/true))) {}

  explicit Module(llvm::StringRef name, llvm::StringRef text)
      : path("(builtin)"), name(name.str()), text(llvm::MemoryBuffer::getMemBuffer(text)) {}

  Module(const Module &) = delete;

  Module(Module &&) = delete;

  void parse(Context &context);

  void emit(Context &context);

public:
  /// The file path. This is empty if the module is builtin.
  std::string path{};

  /// The logical name of the module.
  std::string name{};

  /// The memory buffer containing the source text.
  std::unique_ptr<llvm::MemoryBuffer> text{};

  /// The root of the Abstract Syntax Tree.
  unique_bump_ptr<AST::Node> root{};

  /// The compile status.
  Status status{Status::NotStarted};

  /// The last crumb in the module. (This is the starting point to search for exported declarations!)
  Crumb *lastCrumb{};
};

} // namespace smdl::Compiler
