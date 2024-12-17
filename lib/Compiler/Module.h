#pragma once

#include "Function.h"

namespace smdl::Compiler {

class Module final {
public:
  enum class Status : uint32_t { NotStarted, InProgress, Finished };

  explicit Module(std::filesystem::path fname)
      : filename(std::move(fname)), filenameStr(filename.string()), name(filename.stem().string()),
        text(load_file_to_string(filenameStr)) {}

  explicit Module(llvm::StringRef name, llvm::StringRef text) : name(name.str()), text(text.str()) {}

  Module(const Module &) = delete;

  Module(Module &&) = delete;

public:
  [[nodiscard]] std::filesystem::path directory_path() const { return filename.empty() ? filename : filename.parent_path(); }

  [[nodiscard]] bool is_builtin() const { return filename.empty(); }

  [[nodiscard]] bool is_parse_finished() const { return root != nullptr; }

  [[nodiscard]] bool is_emit_finished() const { return status == Status::Finished; }

public:
  void parse(Context &context);

  void emit(Context &context);

  void format_source();

public:
  /// The filename as `std::filesystem::path`. This is empty if the module is builtin.
  const std::filesystem::path filename{};

  /// The filename as `std::string`. This is empty if the module is builtin.
  const std::string filenameStr{};

  /// The logical name of the module.
  const std::string name{};

  /// The source text.
  const std::string text{};

  /// The root of the Abstract Syntax Tree.
  unique_bump_ptr<AST::Node> root{};

  /// The compile status. (This is necessary to detect cyclic imports, which would cause infinite loops)
  Status status{Status::NotStarted};

  /// The last declaration in the module. (This is the starting point to search for exported declarations!)
  Breadcrumb *lastBreadcrumb{};

  /// The last import declaration in the module. (All imports must be at the top of the file)
  Breadcrumb *lastImportDeclaration{};

  /// Does this module use the SDML extended syntax?
  bool isSmdlSyntax{};
};

} // namespace smdl::Compiler
