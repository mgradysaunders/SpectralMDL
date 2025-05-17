#pragma once

#include "llvm.h"

namespace smdl {

/// \addtogroup Compiler
/// \{

class Emitter;
class Type;

/// A value, the fundamental unit of computation.
class Value final {
public:
  enum class Kind { Invalid = 0, LValue = 1, RValue = 2 };

  Value() = default;

  explicit Value(Kind kind, Type *type, llvm::Value *llvmValue)
      : kind(kind), type(type), llvmValue(llvmValue) {}

  /// The zero-value for the given type.
  [[nodiscard]] static Value zero(Type *type);

  /// Is void?
  [[nodiscard]] bool is_void() const;

  /// Is an lvalue?
  [[nodiscard]] bool is_lvalue() const { return kind == Kind::LValue; }

  /// Is an rvalue?
  [[nodiscard]] bool is_rvalue() const { return kind == Kind::RValue; }

  /// Is this an LLVM instruction?
  [[nodiscard]] bool is_llvm_instruction() const {
    return llvm::isa_and_present<llvm::Instruction>(llvmValue);
  }

  /// Is this an LLVM constant?
  [[nodiscard]] bool is_llvm_constant() const {
    return llvm::isa_and_present<llvm::Constant>(llvmValue);
  }

  /// Is this an LLVM global? Note: Every LLVM global is also technically an
  /// LLVM constant.
  [[nodiscard]] bool is_llvm_global() const {
    return llvm::isa_and_present<llvm::GlobalValue>(llvmValue);
  }

  /// Is usable in the given LLVM function?
  /// - If this is an LLVM constant or global, it is usable.
  /// - If this is an LLVM instruction, it is only usable if it belongs to the
  /// same LLVM function.
  [[nodiscard]] bool
  is_usable_in_llvm_function(llvm::Function *llvmFunc) const {
    if (auto llvmInst{llvm::dyn_cast_if_present<llvm::Instruction>(llvmValue)}; llvmInst && llvmInst->getParent())
      return llvmInst->getFunction() == llvmFunc;
    return true;
  }

  /// Is known at compile time?
  [[nodiscard]] bool is_comptime() const {
    return is_rvalue() && is_llvm_constant();
  }

  /// Is compile-time int?
  [[nodiscard]] bool is_comptime_int() const {
    return llvm::dyn_cast_if_present<llvm::ConstantInt>(llvmValue) != nullptr;
  }

  /// Is compile-time string?
  [[nodiscard]] bool is_comptime_string() const;

  /// Get value as compile-time int or `unsigned(-1)` on failure.
  [[nodiscard]] unsigned get_comptime_int() const {
    if (auto llvmConst{llvm::dyn_cast_if_present<llvm::ConstantInt>(llvmValue)})
      return llvmConst->getValue().getLimitedValue(
          std::numeric_limits<unsigned>::max());
    return unsigned(-1);
  }

  /// Get value as compile-time string or the `std::string_view()` on failure.
  [[nodiscard]] std::string_view get_comptime_string() const;

  /// Is compile-time meta `Module`?
  [[nodiscard]] bool is_comptime_meta_module(Context &context) const;

  /// Is compile-time meta `Type`?
  [[nodiscard]] bool is_comptime_meta_type(Context &context) const;

  /// Is compile-time meta `AST::Intrinsic`?
  [[nodiscard]] bool is_comptime_meta_intrinsic(Context &context) const;

  /// Is compile-time meta `AST::Namespace`?
  [[nodiscard]] bool is_comptime_meta_namespace(Context &context) const;

  /// Get the compile-time `Module` or throw an error.
  [[nodiscard]] Module *
  get_comptime_meta_module(Context &context,
                           const SourceLocation &srcLoc) const {
    if (!is_comptime_meta_module(context))
      srcLoc.throw_error("expected compile-time module");
    return llvm_constant_int_as_ptr<Module>(llvmValue);
  }

  /// Get the compile-time `Type` or throw an error.
  [[nodiscard]] Type *
  get_comptime_meta_type(Context &context, const SourceLocation &srcLoc) const {
    if (!is_comptime_meta_type(context))
      srcLoc.throw_error("expected compile-time type");
    return llvm_constant_int_as_ptr<Type>(llvmValue);
  }

  /// Get the compile-time `AST::Intrinsic` or throw an error.
  [[nodiscard]] AST::Intrinsic *
  get_comptime_meta_intrinsic(Context &context,
                              const SourceLocation &srcLoc) const {
    if (!is_comptime_meta_intrinsic(context))
      srcLoc.throw_error("expected compile-time intrinsic");
    return llvm_constant_int_as_ptr<AST::Intrinsic>(llvmValue);
  }

  /// Get the compile-time `AST::Namespace` or throw an error.
  [[nodiscard]] AST::Namespace *
  get_comptime_meta_namespace(Context &context,
                              const SourceLocation &srcLoc) const {
    if (!is_comptime_meta_namespace(context))
      srcLoc.throw_error("expected compile-time intrinsic");
    return llvm_constant_int_as_ptr<AST::Namespace>(llvmValue);
  }

  /// Implicit conversion to bool.
  [[nodiscard]] operator bool() const { return type && llvmValue; }

  /// Implicit conversion to LLVM value.
  [[nodiscard]] operator llvm::Value *() const { return llvmValue; }

public:
  /// The kind.
  Kind kind{};

  /// The type.
  Type *type{};

  /// The LLVM value.
  llvm::Value *llvmValue{};
};

/// Make an lvalue.
[[nodiscard]] inline Value LValue(Type *type, llvm::Value *llvmValue) {
  return Value(Value::Kind::LValue, type, llvmValue);
}

/// Make an rvalue.
[[nodiscard]] inline Value RValue(Type *type, llvm::Value *llvmValue) {
  return Value(Value::Kind::RValue, type, llvmValue);
}

/// A crumb, the fundamental unit of name resolution and scope.
class Crumb final {
public:
  /// Find by name starting from the given crumb and walking upwards.
  [[nodiscard]] static Crumb *find(Context &context,
                                   Span<std::string_view> name,
                                   llvm::Function *llvmFunc, Crumb *crumb,
                                   Crumb *stopCrumb = nullptr,
                                   bool ignoreIfNotExported = false);

  /// Get the source location if applicable.
  [[nodiscard]] SourceLocation get_source_location() const {
    return node ? node->srcLoc : SourceLocation();
  }

  /// Is exported?
  [[nodiscard]] bool is_exported() const {
    if (auto decl{llvm::dyn_cast_if_present<AST::Decl>(node)})
      return decl->is_exported();
    if (auto declarator{llvm::dyn_cast_if_present<AST::Enum::Declarator>(node)})
      return declarator->decl->is_exported();
    if (auto declarator{
            llvm::dyn_cast_if_present<AST::Variable::Declarator>(node)})
      return declarator->decl->is_exported();
    return false;
  }

  /// Is named?
  [[nodiscard]] bool is_named() const { return !name.empty(); }

  /// Has simple name?
  [[nodiscard]] bool has_simple_name() const { return name.size() == 1; }

  /// Has qualified name?
  [[nodiscard]] bool has_qualified_name() const { return name.size() > 1; }

  /// Is this an AST import declaration?
  [[nodiscard]] bool is_ast_import() const {
    return llvm::isa_and_present<AST::Import>(node);
  }

  /// Is this an AST using import declaration?
  [[nodiscard]] bool is_ast_using_import() const {
    return llvm::isa_and_present<AST::UsingImport>(node);
  }

  /// Is this an AST using alias declaration?
  [[nodiscard]] bool is_ast_using_alias() const {
    return llvm::isa_and_present<AST::UsingAlias>(node);
  }

  /// Is this an AST defer statement?
  [[nodiscard]] bool is_ast_defer() const {
    return llvm::isa_and_present<AST::Defer>(node);
  }

  /// Is this an AST preserve statement?
  [[nodiscard]] bool is_ast_preserve() const {
    return llvm::isa_and_present<AST::Preserve>(node);
  }

  /// Maybe issue warning about an unused value.
  void maybe_warn_about_unused_value() const {
    if (isUsed == 0 && name.size() == 1) {
      if (llvm::isa_and_present<AST::Parameter>(node)) {
        auto astParam{static_cast<AST::Parameter *>(node)};
        if (!astParam->warningIssued &&
            !astParam->type->has_qualifier("inline") &&
            !(astParam->annotations &&
              astParam->annotations->is_marked_unused())) {
          astParam->warningIssued = true;
          get_source_location().log_warn(
              concat("unused parameter ", quoted(name[0])));
        }
      }
      if (llvm::isa_and_present<AST::Variable::Declarator>(node)) {
        auto declarator{static_cast<AST::Variable::Declarator *>(node)};
        if (!declarator->warningIssued &&
            !(declarator->annotations &&
              declarator->annotations->is_marked_unused())) {
          declarator->warningIssued = true;
          get_source_location().log_warn(
              concat("unused variable ", quoted(name[0])));
        }
      }
    }
  }

public:
  /// The previous crumb.
  Crumb *prev{};

  /// The name. This may be empty!
  Span<std::string_view> name{};

  /// The AST node if applicable.
  AST::Node *node{};

  /// The value.
  Value value{};

  /// If this is an AST preserve statement, the value to preserve.
  Value valueToPreserve{};

  uint8_t isUsed{};
};

/// A parameter.
class Parameter final {
public:
  /// Get the source location. Returns the empty source location if this
  /// has no associated `astParam` or `astField`.
  [[nodiscard]] auto get_source_location() const {
    if (astParam)
      return astParam->name.srcLoc;
    if (astField)
      return astField->name.srcLoc;
    return SourceLocation();
  }

  /// Is this an AST function parameter?
  [[nodiscard]] bool is_ast_parameter() const { return astParam != nullptr; }

  /// Is this an AST struct field?
  [[nodiscard]] bool is_ast_field() const { return astField != nullptr; }

  /// Get the AST type. This may be null!
  [[nodiscard]] AST::Type *get_ast_type() const {
    if (astParam)
      return astParam->type.get();
    if (astField)
      return astField->type.get();
    return nullptr;
  }

  /// Is marked with the keyword `const`?
  [[nodiscard]] bool is_const() const {
    if (auto astType{get_ast_type()})
      return astType->has_qualifier("const") || builtinConst;
    return builtinConst;
  }

  /// Is marked with the keyword `inline`?
  [[nodiscard]] bool is_inline() const {
    if (auto astType{get_ast_type()})
      return astType->has_qualifier("inline");
    return false;
  }

  /// Get the default AST initializer expression. This may be null!
  [[nodiscard]] AST::Expr *get_ast_initializer() const {
    if (astParam)
      return astParam->exprInit.get();
    if (astField)
      return astField->exprInit.get();
    return nullptr;
  }

public:
  /// The type.
  Type *type{};

  /// The name. This must be non-empty!
  std::string_view name{};

  /// The AST parameter if applicable.
  AST::Parameter *astParam{};

  /// The AST field if applicable.
  AST::Struct::Field *astField{};

  /// The default value if not an AST parameter or AST field.
  std::optional<Value> builtinDefaultValue{};

  /// Force const-ness?
  bool builtinConst{};
};

/// A parameter list.
class ParameterList final : public SmallVectorOf<Parameter> {
public:
  ParameterList() = default;

  ParameterList(std::initializer_list<Parameter> params) {
    elems.resize(params.size());
    std::copy(params.begin(), params.end(), elems.begin());
  }

  /// Is abstract? i.e., is any type abstract?
  [[nodiscard]] bool is_abstract() const;

  /// Is concrete? i.e., is every type concrete?
  [[nodiscard]] bool is_concrete() const { return !is_abstract(); }

  /// Get the parameter names.
  [[nodiscard]] std::vector<std::string_view> get_names() const;

  /// Get the parameter types.
  [[nodiscard]] std::vector<Type *> get_types() const;

  /// Get the paramter LLVM types.
  [[nodiscard]] std::vector<llvm::Type *> get_llvm_types() const;

  /// The lookup sequence.
  using LookupSeq = std::vector<std::pair<const Parameter *, unsigned>>;

  /// Get the lookup sequence to access the given parameter name. Returns false
  /// on failure.
  [[nodiscard]] bool get_lookup_sequence(std::string_view name,
                                         LookupSeq &seq) const;

  /// The last crumb before the parameter list.
  Crumb *lastCrumb{};
};

/// An argument.
class Argument final {
public:
  /// Get the source location. Returns the empty source location if this
  /// has no associated `astArg`.
  [[nodiscard]] auto get_source_location() const {
    return astArg ? astArg->srcLoc : SourceLocation();
  }

  /// Get the source. Returns the empty string view if this has no
  /// associated `astArg`.
  [[nodiscard]] auto get_source() const {
    return astArg ? astArg->src : std::string_view();
  }

  /// Is positional or unnamed?
  [[nodiscard]] bool is_positional() const { return name.empty(); }

  /// Is named?
  [[nodiscard]] bool is_named() const { return !name.empty(); }

  /// Is marked with the keyword `visit` and is actually visitable?
  [[nodiscard]] bool is_visited() const;

  /// Implicit conversion to `Value`.
  [[nodiscard]] operator Value() const { return value; }

public:
  /// The name. This may be empty!
  std::string_view name{};

  /// The value.
  Value value{};

  /// The associated AST argument if applicable. This may be null!
  AST::Argument *astArg{};

  /// Is implied visit? This is determined by `Emitter::ResolvedArguments`.
  bool impliedVisit{};
};

/// An argument list.
class ArgumentList final : public SmallVectorOf<Argument> {
public:
  ArgumentList() = default;

  ArgumentList(Value arg) { elems.push_back(Argument{{}, arg, {}}); }

  ArgumentList(Argument arg) { elems.push_back(std::move(arg)); }

  ArgumentList(llvm::ArrayRef<Value> args) {
    elems.resize(args.size());
    for (size_t i = 0; i < elems.size(); i++) {
      elems[i] = Argument{{}, args[i], {}};
    }
  }

public:
  /// Get the source location. Returns the empty source location if this
  /// has no associated `astArgs`.
  [[nodiscard]] auto get_source_location() const {
    return astArgs ? astArgs->srcLoc : SourceLocation();
  }

  /// Is one positional argument?
  [[nodiscard]] bool is_one_positional() const {
    return elems.size() == 1 && elems[0].is_positional();
  }

  /// Is one positional argument with the given type?
  [[nodiscard]] bool is_one_positional(Type *type) const {
    return is_one_positional() && elems[0].value.type == type;
  }

  /// Is one positional `null` argument?
  [[nodiscard]] bool is_null() const {
    return is_one_positional() && !elems[0].value;
  }

  /// Is all positional arguments?
  [[nodiscard]] bool is_all_positional() const {
    return is_all_true([](auto &arg) { return arg.is_positional(); });
  }

  /// Is all named arguments?
  [[nodiscard]] bool is_all_named() const {
    return is_all_true([](auto &arg) { return arg.is_named(); });
  }

  /// Is any argument named?
  [[nodiscard]] bool is_any_named() const {
    return is_any_true([](auto &arg) { return arg.is_named(); });
  }

  /// Is only arguments with the given names?
  [[nodiscard]] bool is_only_these_names(Span<std::string_view> names) const {
    return is_all_true([&](auto &arg) {
      return arg.name.empty() || names.contains(arg.name);
    });
  }

  /// Is any argument visited?
  [[nodiscard]] bool is_any_visited() const {
    return is_any_true([](auto &arg) { return arg.is_visited(); });
  }

  /// Get index of first argument marked with the keyword `visit`.
  [[nodiscard]] size_t index_of_first_visited() const {
    for (size_t i = 0; i < elems.size(); i++) {
      if (elems[i].is_visited())
        return i;
    }
    return size_t(-1);
  }

  /// Get the argument names.
  [[nodiscard]] std::vector<std::string_view> get_names() const;

  /// Get the argument types.
  [[nodiscard]] std::vector<Type *> get_types() const;

  /// Get the argument LLVM types.
  [[nodiscard]] std::vector<llvm::Type *> get_llvm_types() const;

  /// Get the argument values.
  [[nodiscard]] std::vector<Value> get_values() const;

  /// Get the argument LLVM values for creating a call instruction.
  [[nodiscard]] std::vector<llvm::Value *>
  get_llvm_values_for_call(Emitter &emitter, bool isPure) const;

  /// Validate names or throw an `Error` on failure.
  void validate_names();

  /// Convert to string for debugging or error messages.
  [[nodiscard]] operator std::string() const;

public:
  /// The associated AST args if applicable. This may be null!
  AST::ArgumentList *astArgs{};
};

/// \}

} // namespace smdl
