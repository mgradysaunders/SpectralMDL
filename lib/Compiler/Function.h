#pragma once

#include "Type.h"

namespace smdl::Compiler {

/// A concrete LLVM function instance.
class FunctionInstance final {
public:
  FunctionInstance() = default;

  explicit FunctionInstance(Emitter &emitter0, AST::Function &decl, const ParamList &params, llvm::ArrayRef<Type *> argTypes);

  explicit FunctionInstance(Emitter &emitter0, AST::UnitTest &decl);

  explicit FunctionInstance(Emitter &emitter0, AST::Expr &expr);

  explicit FunctionInstance(Emitter &emitter0, EnumType *enumType);

  /// If the return type is abstract and cannot be inferred until the function code is generated, this
  /// allows us to patch in the concrete return type at the end.
  void patch_return_type(Type *returnType);

  /// Force inline specific calls (or give the user warnings).
  void force_inline(llvm::ArrayRef<Inline> inlines);

  /// Force inline flatten as much as possible.
  void force_inline_flatten() { llvm_force_inline_flatten(*sanity_check_nonnull(llvmFunc)); }

  /// Optimize.
  void optimize(llvm::OptimizationLevel level = llvm::OptimizationLevel::O2) {
    LLVMOptimizer llvmOptimizer{};
    llvmOptimizer.run(*sanity_check_nonnull(llvmFunc), level);
  }

  /// Eliminate unreachable blocks.
  void eliminate_unreachable() { llvm::EliminateUnreachableBlocks(*sanity_check_nonnull(llvmFunc)); }

  /// Verify the LLVM IR.
  void verify() {
    std::string message{};
    llvm::raw_string_ostream OS{message};
    if (llvm::verifyFunction(*sanity_check_nonnull(llvmFunc), &OS)) // Returns true on error
      srcLoc.report_error(std::format("function '{}' LLVM-IR verification failed: {}", name, message));
  }

  [[nodiscard]] Value call(Emitter &emitter, llvm::ArrayRef<Value> argValues, const AST::SourceLocation &srcLoc = {});

  [[nodiscard]] operator bool() const { return type && llvmFunc; }

public:
  /// The function name (for error messages).
  llvm::StringRef name{};

  /// The function type.
  FunctionType *type{};

  /// The LLVM function.
  llvm::Function *llvmFunc{};

  /// The source location if applicable.
  AST::SourceLocation srcLoc{};
};

/// This is the representation of a function, which necessarily corresponds to 1 specific
/// 'AST::Function' in the source code. Note however that a function in the source code may
/// not necessarily correspond to 1 concrete LLVM function. It may be a '@(macro)', in which
/// case it is always expanded inline, or it may have 'auto' parameters in which case more
/// than 1 LLVM version of the function may end up being compiled.
class Function final {
public:
  Function(Emitter &emitter0, AST::Function &decl);

  [[nodiscard]] llvm::StringRef get_name() const { return decl.name->name; }

  [[nodiscard]] bool is_visible() const { return decl.attrs.isVisible; }

  [[nodiscard]] bool is_foreign() const { return decl.attrs.isForeign; }

  [[nodiscard]] bool is_pure() const { return decl.attrs.isPure; }

  [[nodiscard]] bool is_macro() const { return decl.attrs.isMacro; }

  [[nodiscard]] bool is_variant() const { return decl.isVariant; }

  [[nodiscard]] bool has_definition() const { return decl.definition != nullptr; }

  [[nodiscard]] bool has_abstract_parameters() const { return params.has_any_abstract(); }

  [[nodiscard]] bool has_unique_concrete_instance() const {
    return has_definition() && !has_abstract_parameters() && !is_macro() && !is_variant();
  }

  [[nodiscard]] Value call(Emitter &emitter0, const ArgList &args, const AST::SourceLocation &srcLoc = {});

public:
  /// The most recent overload somewhere above this function in the source code.
  Function *prev{};

  /// The AST function.
  AST::Function &decl;

  /// The AST let and call expressions if this is a function variant.
  AST::Function::LetAndCall letAndCall{};

  /// The parameter list derived from the AST function.
  ParamList params{};

  /// The concrete function instances keyed by concrete parameter types.
  std::map<llvm::SmallVector<Type *>, FunctionInstance> instances{};

public:
  [[nodiscard]] static Value compile_time_evaluate(Emitter &emitter0, AST::Expr &expr);

private:
  void validate_attributes();
};

} // namespace smdl::Compiler
