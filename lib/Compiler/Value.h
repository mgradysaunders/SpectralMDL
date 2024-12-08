// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#pragma once

#include "AST.h"

namespace smdl::Compiler {

class Function;
class FunctionInstance;
class Module;
class Type;

//--{ Value
class Value final {
public:
  enum class Kind { Invalid = 0, LValue = 1, RValue = 2 };

  Value() = default;

  Value(Kind kind, Type *type, llvm::Value *llvmValue) : kind(kind), type(type), llvmValue(llvmValue) {}

  [[nodiscard]] static Value zero(Type *type);

  [[nodiscard]] bool is_void() const;

  /// Is an lvalue?
  [[nodiscard]] bool is_lvalue() const { return kind == Kind::LValue; }

  /// Is an rvalue?
  [[nodiscard]] bool is_rvalue() const { return kind == Kind::RValue; }

  /// Is this an LLVM instruction?
  [[nodiscard]] bool is_llvm_instruction() const { return llvmValue && llvm::isa<llvm::Instruction>(llvmValue); }

  /// Is this an LLVM constant?
  [[nodiscard]] bool is_llvm_constant() const { return llvmValue && llvm::isa<llvm::Constant>(llvmValue); }

  /// Is this an LLVM global? Note: Every LLVM global is also technically an LLVM constant.
  [[nodiscard]] bool is_llvm_global() const { return llvmValue && llvm::isa<llvm::GlobalValue>(llvmValue); }

  /// Is known and usable at compile-time? This is an rvalue that is also an LLVM constant.
  [[nodiscard]] bool is_compile_time() const { return is_rvalue() && is_llvm_constant(); }

  /// Is compile-time integer?
  [[nodiscard]] bool is_compile_time_int() const { return llvmValue && llvm::isa<llvm::ConstantInt>(llvmValue); }

  /// Is compile-time string?
  [[nodiscard]] bool is_compile_time_string() const;

  /// Is compile-time pointer to compiler function?
  [[nodiscard]] bool is_compile_time_function() const;

  /// Is compile-time pointer to compiler intrinsic?
  [[nodiscard]] bool is_compile_time_intrinsic() const;

  /// Is compile-time pointer to compiler module?
  [[nodiscard]] bool is_compile_time_module() const;

  /// Is compile-time pointer to compiler type?
  [[nodiscard]] bool is_compile_time_type() const;

  /// Get compile-time integer or crash the program.
  [[nodiscard]] int_t get_compile_time_int() const {
    sanity_check(is_compile_time_int());
    return static_cast<llvm::ConstantInt *>(llvmValue)->getValue().getLimitedValue(
        std::numeric_limits<std::make_unsigned_t<int_t>>::max());
  }

  /// Get compile-time string or crash the program.
  [[nodiscard]] llvm::StringRef get_compile_time_string() const;

  /// Get compile-time pointer to compiler function or crash the program.
  [[nodiscard]] Function *get_compile_time_function() const {
    sanity_check(is_compile_time_function());
    return llvm_constant_int_as_ptr<Function>(llvmValue);
  }

  /// Get compile-time pointer to compiler intrinsic or crash the program.
  [[nodiscard]] AST::Intrinsic *get_compile_time_intrinsic() const {
    sanity_check(is_compile_time_intrinsic());
    return llvm_constant_int_as_ptr<AST::Intrinsic>(llvmValue);
  }

  /// Get compile-time pointer to compiler module or crash the program.
  [[nodiscard]] Module *get_compile_time_module() const {
    sanity_check(is_compile_time_module());
    return llvm_constant_int_as_ptr<Module>(llvmValue);
  }

  /// Get compile-time pointer to compiler type or crash the program.
  [[nodiscard]] Type *get_compile_time_type() const {
    sanity_check(is_compile_time_type());
    return llvm_constant_int_as_ptr<Type>(llvmValue);
  }

  /// Is usable in the given LLVM function?
  /// - If this is an LLVM constant or global, it is usable.
  /// - If this is an LLVM instruction, it is only usable if it belongs to the same LLVM function.
  [[nodiscard]] bool is_usable_in_llvm_function(llvm::Function *llvmFunc) const {
    if (is_llvm_instruction())
      return static_cast<llvm::Instruction *>(llvmValue)->getFunction() == llvmFunc;
    return true;
  }

public:
  /// Implicit conversion to bool.
  [[nodiscard]] operator bool() const { return type && llvmValue; }

  /// Implicit conversion to LLVM value.
  [[nodiscard]] operator llvm::Value *() const { return llvmValue; }

  /// Explicit conversion to _any_ derived LLVM value with sanity checking.
  template <std::derived_from<llvm::Value> ValueT> requires(!std::same_as<llvm::Value, ValueT>)
  [[nodiscard]] explicit operator ValueT *() const {
    sanity_check(llvmValue != nullptr);
    sanity_check(llvm::isa<ValueT>(llvmValue));
    return static_cast<ValueT *>(llvmValue);
  }

public:
  Kind kind{};

  Type *type{};

  llvm::Value *llvmValue{};
};
//--}

/// Construct an lvalue.
[[nodiscard]] inline Value LValue(Type *type, llvm::Value *llvmValue) { return Value(Value::Kind::LValue, type, llvmValue); }

/// Construct an rvalue.
[[nodiscard]] inline Value RValue(Type *type, llvm::Value *llvmValue) { return Value(Value::Kind::RValue, type, llvmValue); }

//--{ Crumb
/// This is a compiler crumb, which is the fundamental unit of name and scope resolution.
class Crumb final {
public:
  /// Is this an exported AST declaration?
  [[nodiscard]] bool is_exported_ast_decl() const;

  /// Is this an AST import declaration?
  [[nodiscard]] bool is_ast_import() const { return node && llvm::isa<AST::Import>(node); }

  /// Is this an AST using import declaration?
  [[nodiscard]] bool is_ast_using_import() const { return node && llvm::isa<AST::UsingImport>(node); }

  /// Is this an AST using alias declaration?
  [[nodiscard]] bool is_ast_using_alias() const { return node && llvm::isa<AST::UsingAlias>(node); }

  /// Is this an AST defer statement?
  [[nodiscard]] bool is_ast_defer() const { return node && llvm::isa<AST::Defer>(node); }

  /// Is this an AST preserve statement?
  [[nodiscard]] bool is_ast_preserve() const { return node && llvm::isa<AST::Preserve>(node); }

  /// Is this named?
  [[nodiscard]] bool is_named() const { return !name.empty(); }

  /// Matches the given name?
  [[nodiscard]] bool matches_name(llvm::StringRef name0) const;

  /// Matches the given name?
  [[nodiscard]] bool matches_name(llvm::ArrayRef<llvm::StringRef> path) const;

public:
  /// The previous crumb.
  Crumb *prev{};

  /// The value.
  Value value{};

  /// The name or qualified name. Here, a qualified name is just a name sequence with more than 1 name.
  llvm::ArrayRef<llvm::StringRef> name{};

  /// The AST node, if applicable.
  AST::Node *node{};

  /// The AST source location, if applicable.
  AST::SourceLocation srcLoc{};

  Value valueToPreserve{};

public:
  /// Find the first crumb with the given name sequence that is usable in the given LLVM function.
  [[nodiscard]] static Crumb *find(Crumb *crumb, llvm::ArrayRef<llvm::StringRef> name, llvm::Function *llvmFunc, int depth = 0);

  /// Find the first crumb with the given name that is usable in the given LLVM function.
  [[nodiscard]] static Crumb *find(Crumb *crumb, llvm::StringRef name, llvm::Function *llvmFunc, int depth = 0) {
    return find(crumb, llvm::ArrayRef(name), llvmFunc, depth);
  }
};
//--}

//--{ Param
class ParamList;

/// This represents a named parameter with an optional default initializer. This may be understood as an
/// ordinary function parameter OR as a field in a struct (which is effectively the same idea as being a
/// parameter to the constructor of the struct).
class Param final {
public:
  Param() = default;

  /// Construct from name and type.
  Param(Type *type, llvm::StringRef name, AST::Expr *init = {}) : type(sanity_check_nonnull(type)), name(name), init(init) {
    sanity_check(!name.empty());
  }

  /// Construct from an AST parameter.
  explicit Param(Context &context, const AST::Param &astParam);

  /// Construct from an AST structure field.
  explicit Param(Context &context, const AST::Struct::Field &astField);

  [[nodiscard]] const ParamList *get_inline_parameters() const;

public:
  /// The type.
  Type *type{};

  /// Is void? (Needed for API compatibility, but discarded by compiler?)
  bool isVoid{};

  /// Is const?
  bool isConst{};

  /// Is inline?
  bool isInline{};

  /// The name. This must be non-empty!
  llvm::StringRef name{};

  /// The default initializer expression, if applicable.
  AST::Expr *init{};

  /// The source location, if applicable.
  AST::SourceLocation srcLoc{};
};
//--}

//--{ ParamList
class ParamList final {
public:
  ParamList() = default;

  explicit ParamList(Context &context, const AST::Struct &decl);

  explicit ParamList(Context &context, const AST::Function &decl);

public:
  [[nodiscard]] bool empty() const { return params.empty(); }

  [[nodiscard]] auto begin() const { return params.begin(); }

  [[nodiscard]] auto begin() { return params.begin(); }

  [[nodiscard]] auto end() const { return params.end(); }

  [[nodiscard]] auto end() { return params.end(); }

  [[nodiscard]] auto size() const { return params.size(); }

  [[nodiscard]] auto &operator[](unsigned i) const { return params[i]; }

  [[nodiscard]] auto &operator[](unsigned i) { return params[i]; }

  void push_back(auto &&...more) { params.push_back(std::forward<decltype(more)>(more)...); }

  auto &emplace_back(auto &&...more) { return params.emplace_back(std::forward<decltype(more)>(more)...); }

public:
  /// Is the predicate true for all parameters?
  [[nodiscard]] bool is_all_true(auto &&pred) const { return detail::is_all_true(params, std::forward<decltype(pred)>(pred)); }

  /// Is the predicate true for any parameter?
  [[nodiscard]] bool is_any_true(auto &&pred) const { return detail::is_any_true(params, std::forward<decltype(pred)>(pred)); }

  /// Has any abstract types?
  [[nodiscard]] bool has_any_abstract() const;

  /// Has any inline parameters?
  [[nodiscard]] bool has_any_inline() const;

  /// Get the parameter names (without inlining!)
  [[nodiscard]] llvm::SmallVector<llvm::StringRef> get_names() const;

  /// Get the parameter types (without inlining!)
  [[nodiscard]] llvm::SmallVector<Type *> get_types() const;

  /// Get the parameter LLVM types (without inlining!)
  [[nodiscard]] llvm::SmallVector<llvm::Type *> get_llvm_types() const;

  using InlinePath = llvm::SmallVector<std::pair<const Param *, unsigned>>;

  /// Get the inline path or name sequence to access the given parameter name. Returns false on failure.
  [[nodiscard]] bool get_inline_path(llvm::StringRef name, InlinePath &path) const;

  /// Guarantee no ambiguous names after considering inlining.
  void guarantee_no_ambiguous_inlining(const AST::SourceLocation &srcLoc) const;

public:
  llvm::SmallVector<Param> params{};

  Module *module{};

  Crumb *crumb{};
};
//--}

//--{ Arg
class Arg final {
public:
  Arg() = default;

  Arg(llvm::StringRef name, Value value, const AST::SourceLocation &srcLoc = {}, llvm::StringRef src = {})
      : name(name), value(value), srcLoc(srcLoc), src(src) {}

  Arg(Value value) : value(value) {}

  [[nodiscard]] bool is_named() const { return !name.empty(); }

  [[nodiscard]] bool is_positional() const { return name.empty(); }

  /// The name. This may be empty.
  llvm::StringRef name{};

  /// The value.
  Value value{};

  /// The source-location if applicable.
  AST::SourceLocation srcLoc{};

  /// The source-code range if applicable. This is useful for implementing the default '#assert(...)' failure message.
  llvm::StringRef src{};

  /// Is marked with the 'visit' keyword? AND Is a union that can actually be visited?
  bool isVisit{};
};
//--}

//--{ ArgList
class ArgList final {
public:
  ArgList() = default;

  ArgList(Value value) : args{Arg(value)} {}

  ArgList(Arg arg) : args{arg} {}

  ArgList(llvm::SmallVector<Arg> args) : args(std::move(args)) {}

  ArgList(std::initializer_list<Value> values) : args(values.begin(), values.end()) {}

public:
  [[nodiscard]] bool empty() const { return args.empty(); }

  [[nodiscard]] auto begin() const { return args.begin(); }

  [[nodiscard]] auto end() const { return args.end(); }

  [[nodiscard]] auto size() const { return args.size(); }

  [[nodiscard]] auto &operator[](unsigned i) { return args[i]; }

  [[nodiscard]] auto &operator[](unsigned i) const { return args[i]; }

  void push_back(auto &&...more) { args.push_back(std::forward<decltype(more)>(more)...); }

  auto &emplace_back(auto &&...more) { return args.emplace_back(std::forward<decltype(more)>(more)...); }

public:
  /// Is the predicate true for all arguments?
  [[nodiscard]] bool is_all_true(auto &&pred) const { return detail::is_all_true(args, std::forward<decltype(pred)>(pred)); }

  /// Is the predicate true for any argument?
  [[nodiscard]] bool is_any_true(auto &&pred) const { return detail::is_any_true(args, std::forward<decltype(pred)>(pred)); }

  /// Is exactly 1 positional argument that is equivalent to 'null'?
  [[nodiscard]] bool is_one_positional_null() const { return is_one_positional() && args[0].value.is_void(); }

  /// Is exactly 1 positional argument?
  [[nodiscard]] bool is_one_positional() const { return size() == 1 && args[0].is_positional(); }

  /// Is all positional arguments?
  [[nodiscard]] bool is_all_positional() const {
    return is_all_true([](auto &arg) { return arg.is_positional(); });
  }

  /// Has an argument with the given name?
  [[nodiscard]] bool has_name(llvm::StringRef name) const {
    return is_any_true([=](auto &arg) { return arg.name == name; });
  }

  /// Has an argument for any of the given names?
  [[nodiscard]] bool has_any_of_these_names(llvm::ArrayRef<llvm::StringRef> names) const {
    return detail::is_any_true(names, [&](auto name) { return has_name(name); });
  }

  /// Has an argument for all of the given names?
  [[nodiscard]] bool has_all_of_these_names(llvm::ArrayRef<llvm::StringRef> names) const {
    return detail::is_all_true(names, [&](auto name) { return has_name(name); });
  }

  /// Has arguments with only the given names?
  [[nodiscard]] bool has_only_these_names(llvm::ArrayRef<llvm::StringRef> names) const {
    return is_all_true(
        [&](auto &arg) { return !arg.is_named() || detail::is_any_true(names, [&](auto name) { return arg.name == name; }); });
  }

  [[nodiscard]] bool has_any_visited() const {
    return is_any_true([](auto &arg) { return arg.isVisit; });
  }

  [[nodiscard]] uint32_t index_of_first_visited() const {
    for (uint32_t i{}; i < args.size(); i++)
      if (args[i].isVisit)
        return i;
    return uint32_t(-1);
  }

  /// Get the argument types
  [[nodiscard]] llvm::SmallVector<Type *> get_types() const;

  /// Get the argument LLVM types
  [[nodiscard]] llvm::SmallVector<llvm::Type *> get_llvm_types() const;

  void guarantee_valid_names(const AST::SourceLocation &srcLoc) const;

  [[nodiscard]] explicit operator std::string() const;

public:
  llvm::SmallVector<Arg> args{};
};
//--}

/// A pending return value.
class Return final {
public:
  Value value{};

  llvm::BasicBlock *block{};

  AST::SourceLocation srcLoc{};
};

/// A pending explicit '#inline(...)' request.
class Inline final {
public:
  /// The value to inline. This should be some instance of 'llvm::CallBase'.
  Value value{};

  /// The source location of the '#inline(...)' request.
  AST::SourceLocation srcLoc{};

  /// Is meant to inline recursively?
  bool isRecursive{};
};

} // namespace smdl::Compiler
