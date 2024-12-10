#pragma once

#include "Type.h"

namespace smdl::Compiler {

/// A concrete LLVM function instance.
class FunctionInstance final {
public:
  FunctionInstance() = default;

  explicit FunctionInstance(Emitter &emitter0, AST::Function &decl, const ParamList &params, llvm::ArrayRef<Type *> argTypes);

  /// Construct a concrete function instance from an 'AST::UnitTest' declaration.
  explicit FunctionInstance(Emitter &emitter0, AST::UnitTest &decl);

  /// Construct a concrete function instance from an 'AST::Expr' that returns the result of the expression.
  explicit FunctionInstance(Emitter &emitter0, AST::Expr &expr);

  /// Construct a concrete function instance from an 'EnumType' that returns the string name of an enum value.
  explicit FunctionInstance(Emitter &emitter0, EnumType *enumType);

  /// Get the link name for the function (to look it up later in the JIT module).
  [[nodiscard]] llvm::StringRef get_link_name() const { return llvmFunc->getName(); }

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
  explicit Function(Emitter &emitter0, AST::Function &decl);

  /// Get the function name.
  [[nodiscard]] llvm::StringRef get_name() const { return decl.name.srcName; }

  /// Get the function return type.
  [[nodiscard]] Type *get_return_type() const { return decl.returnType->type; }

  /// Is the function exported by the module?
  [[nodiscard]] bool is_exported() const { return decl.isExport; }

  /// Is the function meant to be visible to the C++ host program?
  [[nodiscard]] bool is_visible() const { return decl.has_attr("visible"); }

  /// Is the function linked in from the C++ host program?
  [[nodiscard]] bool is_foreign() const { return decl.has_attr("foreign"); }

  /// Is the function pure? (meaning there is no '$state' pointer)
  [[nodiscard]] bool is_pure() const { return decl.has_attr("pure"); }

  /// Is the function always macro-inlined?
  [[nodiscard]] bool is_macro() const { return decl.has_attr("macro"); }

  /// Is the function actually a function variant?
  ///
  /// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  /// int foo(*) = bar(baz: "Hello, world");
  /// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  [[nodiscard]] bool is_variant() const { return decl.isVariant; }

  /// Does the function have a definition?
  [[nodiscard]] bool has_definition() const { return decl.definition != nullptr; }

  /// Does the function have any abstract parameters?
  [[nodiscard]] bool has_abstract_parameters() const { return params.has_any_abstract(); }

  /// Does the function have a unique concrete 'FunctionInstance'?
  [[nodiscard]] bool has_unique_concrete_instance() const {
    return has_definition() && !has_abstract_parameters() && !is_macro() && !is_variant();
  }

  /// If the function has a unique concrete 'FunctionInstance', return it. Else return null.
  [[nodiscard]] const FunctionInstance *get_unique_concrete_instance() {
    if (has_unique_concrete_instance() && !instances.empty()) {
      return &instances.begin()->second;
    }
    return nullptr;
  }

  /// Does this function represent a material?
  ///
  /// This is true if:
  /// 1. The function has a unique concrete instance, AND
  /// 2. The function takes no arguments, AND
  /// 3. The function declaration has the abstract return type 'material'.
  [[nodiscard]] bool represents_material() const;

  [[nodiscard]] Value call(Emitter &emitter0, const ArgList &args, const AST::SourceLocation &srcLoc = {});

public:
  /// The AST function.
  AST::Function &decl;

  /// The most recent overload somewhere above this function in the source code.
  Function *prev{};

  /// The next overload somewhere below this function in the source code.
  Function *next{};

  /// Get the bottom overload.
  [[nodiscard]] Function *get_bottom_overload() {
    auto func{this};
    while (func->next)
      func = func->next;
    return func;
  }

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
