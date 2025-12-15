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
  [[nodiscard]] bool isVoid() const;

  /// Is an lvalue?
  [[nodiscard]] bool isLValue() const { return kind == Kind::LValue; }

  /// Is an rvalue?
  [[nodiscard]] bool isRValue() const { return kind == Kind::RValue; }

  /// Is this an LLVM instruction?
  [[nodiscard]] bool isLLVMInstruction() const {
    return llvm::isa_and_present<llvm::Instruction>(llvmValue);
  }

  /// Is this an LLVM constant?
  [[nodiscard]] bool isLLVMConstant() const {
    return llvm::isa_and_present<llvm::Constant>(llvmValue);
  }

  /// Is this an LLVM global? Note: Every LLVM global is also technically an
  /// LLVM constant.
  [[nodiscard]] bool isLLVMGlobal() const {
    return llvm::isa_and_present<llvm::GlobalValue>(llvmValue);
  }

  /// Is usable in the given LLVM function?
  /// - If this is an LLVM constant or global, it is usable.
  /// - If this is an LLVM instruction, it is only usable if it belongs to the
  /// same LLVM function.
  [[nodiscard]] bool isUsableInLLVMFunction(llvm::Function *llvmFunc) const {
    if (auto llvmInst{llvm::dyn_cast_if_present<llvm::Instruction>(llvmValue)};
        llvmInst && llvmInst->getParent())
      return llvmInst->getFunction() == llvmFunc;
    return true;
  }

  /// Is known at compile time?
  [[nodiscard]] bool isComptime() const {
    return isRValue() && isLLVMConstant();
  }

  /// Is compile-time int?
  [[nodiscard]] bool isComptimeInt() const {
    return llvm::dyn_cast_if_present<llvm::ConstantInt>(llvmValue) != nullptr;
  }

  /// Is compile-time string?
  [[nodiscard]] bool isComptimeString() const;

  /// Get value as compile-time int or `unsigned(-1)` on failure.
  [[nodiscard]] unsigned getComptimeInt() const {
    if (auto llvmConst{llvm::dyn_cast_if_present<llvm::ConstantInt>(llvmValue)})
      return llvmConst->getValue().getLimitedValue(
          std::numeric_limits<unsigned>::max());
    return unsigned(-1);
  }

  /// Get value as compile-time string or the `std::string_view()` on failure.
  [[nodiscard]] std::string_view getComptimeString() const;

  /// Is compile-time meta `Module`?
  [[nodiscard]] bool isComptimeMetaModule(Context &context) const;

  /// Is compile-time meta `Type`?
  [[nodiscard]] bool isComptimeMetaType(Context &context) const;

  /// Is compile-time meta `AST::Intrinsic`?
  [[nodiscard]] bool isComptimeMetaIntrinsic(Context &context) const;

  /// Is compile-time meta `AST::Namespace`?
  [[nodiscard]] bool isComptimeMetaNamespace(Context &context) const;

  /// Get the compile-time `Module` or throw an error.
  [[nodiscard]] Module *
  getComptimeMetaModule(Context &context, const SourceLocation &srcLoc) const {
    if (!isComptimeMetaModule(context))
      srcLoc.throw_error("expected compile-time module");
    return llvmConstantIntAsPtr<Module>(llvmValue);
  }

  /// Get the compile-time `Type` or throw an error.
  [[nodiscard]] Type *getComptimeMetaType(Context &context,
                                          const SourceLocation &srcLoc) const {
    if (!isComptimeMetaType(context))
      srcLoc.throw_error("expected compile-time type");
    return llvmConstantIntAsPtr<Type>(llvmValue);
  }

  /// Get the compile-time `AST::Intrinsic` or throw an error.
  [[nodiscard]] AST::Intrinsic *
  getComptimeMetaIntrinsic(Context &context,
                           const SourceLocation &srcLoc) const {
    if (!isComptimeMetaIntrinsic(context))
      srcLoc.throw_error("expected compile-time intrinsic");
    return llvmConstantIntAsPtr<AST::Intrinsic>(llvmValue);
  }

  /// Get the compile-time `AST::Namespace` or throw an error.
  [[nodiscard]] AST::Namespace *
  getComptimeMetaNamespace(Context &context,
                           const SourceLocation &srcLoc) const {
    if (!isComptimeMetaNamespace(context))
      srcLoc.throw_error("expected compile-time intrinsic");
    return llvmConstantIntAsPtr<AST::Namespace>(llvmValue);
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
                                   Span<const std::string_view> name,
                                   llvm::Function *llvmFunc, Crumb *crumb,
                                   Crumb *stopCrumb = nullptr,
                                   bool ignoreIfNotExported = false);

  /// Get the source location if applicable.
  [[nodiscard]] SourceLocation getSourceLocation() const {
    return node ? node->srcLoc : SourceLocation();
  }

  /// Is exported?
  [[nodiscard]] bool isExported() const {
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
  [[nodiscard]] bool isNamed() const { return !name.empty(); }

  /// Has simple name?
  [[nodiscard]] bool hasSimpleName() const { return name.size() == 1; }

  /// Has qualified name?
  [[nodiscard]] bool hasQualifiedName() const { return name.size() > 1; }

  /// Is this an AST import declaration?
  [[nodiscard]] bool isASTImport() const {
    return llvm::isa_and_present<AST::Import>(node);
  }

  /// Is this an AST using import declaration?
  [[nodiscard]] bool isASTUsingImport() const {
    return llvm::isa_and_present<AST::UsingImport>(node);
  }

  /// Is this an AST using alias declaration?
  [[nodiscard]] bool isASTUsingAlias() const {
    return llvm::isa_and_present<AST::UsingAlias>(node);
  }

  /// Is this an AST defer statement?
  [[nodiscard]] bool isASTDefer() const {
    return llvm::isa_and_present<AST::Defer>(node);
  }

  /// Is this an AST preserve statement?
  [[nodiscard]] bool isASTPreserve() const {
    return llvm::isa_and_present<AST::Preserve>(node);
  }

  /// Maybe issue warning about an unused value.
  void maybeWarnAboutUnusedValue() const {
    if (isUsed == 0 && name.size() == 1) {
      if (llvm::isa_and_present<AST::Parameter>(node)) {
        auto astParam{static_cast<AST::Parameter *>(node)};
        if (!astParam->warningIssued &&
            !astParam->type->has_qualifier("inline") &&
            !(astParam->annotations &&
              astParam->annotations->is_marked_unused())) {
          astParam->warningIssued = true;
          getSourceLocation().log_warn(
              concat("unused parameter ", quoted(name[0])));
        }
      }
      if (llvm::isa_and_present<AST::Variable::Declarator>(node)) {
        auto declarator{static_cast<AST::Variable::Declarator *>(node)};
        if (!declarator->warningIssued &&
            !(declarator->annotations &&
              declarator->annotations->is_marked_unused())) {
          declarator->warningIssued = true;
          getSourceLocation().log_warn(
              concat("unused variable ", quoted(name[0])));
        }
      }
    }
  }

public:
  /// The previous crumb.
  Crumb *prev{};

  /// The name. This may be empty!
  Span<const std::string_view> name{};

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
  [[nodiscard]] auto getSourceLocation() const {
    if (astParam)
      return astParam->name.srcLoc;
    if (astField)
      return astField->name.srcLoc;
    return SourceLocation();
  }

  /// Is this an AST function parameter?
  [[nodiscard]] bool isASTParameter() const { return astParam != nullptr; }

  /// Is this an AST struct field?
  [[nodiscard]] bool isASTField() const { return astField != nullptr; }

  /// Get the AST type. This may be null!
  [[nodiscard]] AST::Type *getASTType() const {
    if (astParam)
      return astParam->type.get();
    if (astField)
      return astField->type.get();
    return nullptr;
  }

  /// Is marked with the keyword `const`?
  [[nodiscard]] bool isConst() const {
    if (auto astType{getASTType()})
      return astType->has_qualifier("const") || builtinConst;
    return builtinConst;
  }

  /// Is marked with the keyword `inline`?
  [[nodiscard]] bool isInline() const {
    if (auto astType{getASTType()})
      return astType->has_qualifier("inline");
    return false;
  }

  /// Get the default AST initializer expression. This may be null!
  [[nodiscard]] AST::Expr *getASTInitializer() const {
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
  [[nodiscard]] bool isAbstract() const;

  /// Is concrete? i.e., is every type concrete?
  [[nodiscard]] bool isConcrete() const { return !isAbstract(); }

  /// Do all parameters have default initializers?
  [[nodiscard]] bool allDefaultInitializers() const {
    return isAllTrue(
        [](auto &param) { return param.getASTInitializer() != nullptr; });
  }

  /// Get the parameter names.
  [[nodiscard]] std::vector<std::string_view> getNames() const;

  /// Get the parameter types.
  [[nodiscard]] std::vector<Type *> getTypes() const;

  /// Get the paramter LLVM types.
  [[nodiscard]] std::vector<llvm::Type *> getLLVMTypes() const;

  /// The lookup sequence.
  using LookupSeq = std::vector<std::pair<const Parameter *, unsigned>>;

  /// Get the lookup sequence to access the given parameter name. Returns false
  /// on failure.
  [[nodiscard]] bool getLookupSequence(std::string_view name,
                                       LookupSeq &seq) const;

  /// The last crumb before the parameter list.
  Crumb *lastCrumb{};
};

/// An argument.
class Argument final {
public:
  Argument() = default;

  Argument(std::string_view name, Value value = {},
           AST::Argument *astArg = nullptr)
      : name(name), value(value), astArg(astArg) {}

  Argument(Value value) : value(value) {}

  /// Get the source location. Returns the empty source location if this
  /// has no associated `astArg`.
  [[nodiscard]] auto getSourceLocation() const {
    return astArg ? astArg->srcLoc : SourceLocation();
  }

  /// Get the source. Returns the empty string view if this has no
  /// associated `astArg`.
  [[nodiscard]] auto getSource() const {
    return astArg ? astArg->src : std::string_view();
  }

  /// Is positional or unnamed?
  [[nodiscard]] bool isPositional() const { return name.empty(); }

  /// Is named?
  [[nodiscard]] bool isNamed() const { return !name.empty(); }

  /// Is marked with the keyword `visit` and is actually visitable?
  [[nodiscard]] bool isVisited() const;

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
  using Base = SmallVectorOf<Argument>;

  ArgumentList() = default;

  ArgumentList(const Argument &arg) { elems.push_back(arg); }

  ArgumentList(const Value &arg) { elems.push_back(Argument(arg)); }

  ArgumentList(llvm::ArrayRef<Value> args) : Base(args.begin(), args.end()) {}

  ArgumentList(std::initializer_list<Argument> args)
      : Base(args.begin(), args.end()) {}

public:
  /// Get the source location. Returns the empty source location if this
  /// has no associated `astArgs`.
  [[nodiscard]] auto getSourceLocation() const {
    return astArgs ? astArgs->srcLoc : SourceLocation();
  }

  /// Is one positional argument?
  [[nodiscard]] bool isOnePositional() const {
    return elems.size() == 1 && elems[0].isPositional();
  }

  /// Is one positional argument with the given type?
  [[nodiscard]] bool isOnePositional(Type *type) const {
    return isOnePositional() && elems[0].value.type == type;
  }

  /// Is one positional `null` argument?
  [[nodiscard]] bool isNull() const {
    return isOnePositional() && elems[0].value.isVoid();
  }

  /// Is all positional arguments?
  [[nodiscard]] bool isAllPositional() const {
    return isAllTrue([](auto &arg) { return arg.isPositional(); });
  }

  /// Is all named arguments?
  [[nodiscard]] bool isAllNamed() const {
    return isAllTrue([](auto &arg) { return arg.isNamed(); });
  }

  /// Is any argument named?
  [[nodiscard]] bool isAnyNamed() const {
    return isAnyTrue([](auto &arg) { return arg.isNamed(); });
  }

  /// Is only arguments with the given names?
  [[nodiscard]] bool
  isOnlyTheseNames(Span<const std::string_view> names) const {
    return isAllTrue([&](auto &arg) {
      return arg.name.empty() || names.contains(arg.name);
    });
  }

  [[nodiscard]] bool hasName(std::string_view name) const {
    return isAnyTrue(
        [&](auto &arg) { return arg.isNamed() && arg.name == name; });
  }

  /// Is any argument visited?
  [[nodiscard]] bool isAnyVisited() const {
    return isAnyTrue([](auto &arg) { return arg.isVisited(); });
  }

  /// Get index of first argument marked with the keyword `visit`.
  [[nodiscard]] size_t indexOfFirstVisited() const {
    for (size_t i = 0; i < elems.size(); i++) {
      if (elems[i].isVisited())
        return i;
    }
    return size_t(-1);
  }

  /// Get the argument names.
  [[nodiscard]] std::vector<std::string_view> getNames() const;

  /// Get the argument types.
  [[nodiscard]] std::vector<Type *> getTypes() const;

  /// Get the argument LLVM types.
  [[nodiscard]] std::vector<llvm::Type *> getLLVMTypes() const;

  /// Get the argument values.
  [[nodiscard]] std::vector<Value> getValues() const;

  /// Get the argument LLVM values for creating a call instruction.
  [[nodiscard]] std::vector<llvm::Value *>
  getLLVMValuesForCall(Emitter &emitter, bool isPure) const;

  /// Validate names or throw an `Error` on failure.
  void validateNames();

  /// Convert to string for debugging or error messages.
  [[nodiscard]] operator std::string() const;

public:
  /// The associated AST args if applicable. This may be null!
  AST::ArgumentList *astArgs{};
};

/// \}

} // namespace smdl
