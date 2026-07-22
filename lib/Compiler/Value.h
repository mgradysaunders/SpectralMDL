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

  /// Get value as sign-extended compile-time int, or `std::nullopt` if this
  /// is not a compile-time int or does not fit in 64 bits. Prefer this over
  /// `getComptimeInt()` wherever the value must be range-checked: the
  /// unsigned accessor silently clamps, so e.g. `-1` becomes `4294967295`.
  [[nodiscard]] std::optional<int64_t> getComptimeSignedInt() const {
    if (auto llvmConst{llvm::dyn_cast_if_present<llvm::ConstantInt>(llvmValue)})
      if (llvmConst->getValue().getSignificantBits() <= 64)
        return llvmConst->getValue().getSExtValue();
    return std::nullopt;
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
      srcLoc.throwError("expected compile-time module");
    return llvmConstantIntAsPtr<Module>(llvmValue);
  }

  /// Get the compile-time `Type` or throw an error.
  [[nodiscard]] Type *getComptimeMetaType(Context &context,
                                          const SourceLocation &srcLoc) const {
    if (!isComptimeMetaType(context))
      srcLoc.throwError("expected compile-time type");
    return llvmConstantIntAsPtr<Type>(llvmValue);
  }

  /// Get the compile-time `AST::Intrinsic` or throw an error.
  [[nodiscard]] AST::Intrinsic *
  getComptimeMetaIntrinsic(Context &context,
                           const SourceLocation &srcLoc) const {
    if (!isComptimeMetaIntrinsic(context))
      srcLoc.throwError("expected compile-time intrinsic");
    return llvmConstantIntAsPtr<AST::Intrinsic>(llvmValue);
  }

  /// Get the compile-time `AST::Namespace` or throw an error.
  [[nodiscard]] AST::Namespace *
  getComptimeMetaNamespace(Context &context,
                           const SourceLocation &srcLoc) const {
    if (!isComptimeMetaNamespace(context))
      srcLoc.throwError("expected compile-time intrinsic");
    return llvmConstantIntAsPtr<AST::Namespace>(llvmValue);
  }

  /// Get the name of the LLVM value.
  [[nodiscard]] llvm::StringRef getName() const {
    return llvmValue ? llvmValue->getName() : "";
  }

  /// Is completely null?
  [[nodiscard]] bool operator!() const { return !type; }

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

/// A declaration, the fundamental unit of name resolution and scope.
class Declaration final {
public:
  /// Resolve the given name through a single import (or import-like)
  /// declaration: the see-through searches of universal imports, the
  /// unqualified match of a specific `using` import, and the exact-name
  /// match of the declaration itself. Returns null if the name does not
  /// resolve through this
  /// declaration.
  [[nodiscard]] static Declaration *
  findThroughImport(Context &context, Span<const std::string_view> name,
                    llvm::Function *llvmFunc, Declaration *declaration);

  /// Resolve the given (possibly qualified) name against the contents of a
  /// single scope, with no parent walk: named declarations newest-first
  /// descending into namespaces for qualified names, then imports
  /// newest-first via `findThroughImport`. The name is interned on entry.
  /// `seqLimit` bounds declaration visibility for re-anchored resolution
  /// (see `Emitter::anchors`).
  ///
  /// If `unusableMatch` is non-null, it receives the nearest exact-name
  /// match skipped only because its run-time value lives in a different
  /// LLVM function than `llvmFunc`. Callers that resolve user identifiers
  /// should treat a recorded skip as an impossible-capture error:
  /// lexically the skipped declaration is the match, so binding to
  /// anything farther away would be silent misbinding.
  [[nodiscard]] static Declaration *
  resolveInScope(Context &context, Span<const std::string_view> name,
                 llvm::Function *llvmFunc, Scope *scope,
                 bool ignoreIfNotExported, uint64_t seqLimit,
                 Declaration **unusableMatch);

  /// Resolve the given name against a module's root scope (see
  /// `Module::mRootScope`).
  [[nodiscard]] static Declaration *
  findInModule(Context &context, Span<const std::string_view> name,
               llvm::Function *llvmFunc, Module *module_,
               bool ignoreIfNotExported = true);

  /// Get the source location if applicable.
  [[nodiscard]] SourceLocation getSourceLocation() const {
    return node ? node->srcLoc : SourceLocation();
  }

  /// Is exported?
  [[nodiscard]] bool isExported() const {
    if (auto decl{llvm::dyn_cast_if_present<AST::Decl>(node)})
      return decl->isExported();
    if (auto declarator{llvm::dyn_cast_if_present<AST::Enum::Declarator>(node)})
      return declarator->decl->isExported();
    if (auto declarator{
            llvm::dyn_cast_if_present<AST::Variable::Declarator>(node)})
      return declarator->decl->isExported();
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

  /// Is exempt from same-scope shadow rejection? Imports bring in foreign
  /// names: declaring over them is cross-module shadowing, which is
  /// allowed, and namespaces may be re-opened.
  [[nodiscard]] bool isSameScopeShadowExempt() const {
    return isASTImport() || isASTUsingImport() ||
           llvm::isa_and_present<AST::Namespace>(node);
  }

  /// Maybe issue warning about an unused value.
  void maybeWarnAboutUnusedValue() const {
    if (isUsed == 0 && name.size() == 1) {
      if (llvm::isa_and_present<AST::Parameter>(node)) {
        auto astParam{static_cast<AST::Parameter *>(node)};
        if (!astParam->warningIssued &&
            !astParam->type->hasQualifier("inline") &&
            !(astParam->annotations &&
              astParam->annotations->isMarkedUnused())) {
          astParam->warningIssued = true;
          getSourceLocation().logWarn(
              concat("unused parameter ", Quoted(name[0])));
        }
      }
      if (llvm::isa_and_present<AST::Variable::Declarator>(node)) {
        auto declarator{static_cast<AST::Variable::Declarator *>(node)};
        if (!declarator->warningIssued &&
            !(declarator->annotations &&
              declarator->annotations->isMarkedUnused())) {
          declarator->warningIssued = true;
          getSourceLocation().logWarn(
              concat("unused variable ", Quoted(name[0])));
        }
      }
    }
  }

public:
  /// The name, interned by `Context::internName` (see `declare`), so
  /// the span and its characters are context-owned, never a borrow of
  /// caller storage, and full-name equality is pointer equality. This may
  /// be empty!
  Span<const std::string_view> name{};

  /// The AST node if applicable.
  AST::Node *node{};

  /// The value.
  Value value{};

  /// Has this declaration been resolved at least once? Drives the
  /// unused-value warnings.
  bool isUsed{};

  /// The globally monotonic declaration sequence number (see
  /// `Context::nextDeclSeq`), used by the scope index to reproduce
  /// declaration-order visibility for re-anchored resolution.
  uint64_t seq{};

  /// The previous declaration with the same name in the same scope (e.g.,
  /// a declaration over an import shadows it but leaves it reachable).
  Declaration *prevSameNameInScope{};
};

/// A scope: the unit of the resolution index. Scopes carry all name resolution
/// (the `Emitter` walks from the current scope outward) and the same-scope
/// shadow probe.
class Scope final {
public:
  /// The parent scope.
  Scope *parent{};

  /// Is transparent? A transparent scope holds declarations that the chain
  /// pops before the enclosing scope ends (two-arm merge arms, variable
  /// initializers, namespace and struct-initialize interiors). It bounds
  /// the lifetime of its map entries without acting as a shadow boundary:
  /// the same-scope probe continues into the parent.
  bool transparent{};

  /// The named declarations in this scope, keyed by the interned name
  /// array (see `Context::internName`), newest first through
  /// `Declaration::prevSameNameInScope`. Import declarations live in `imports`
  /// instead.
  llvm::SmallDenseMap<const void *, Declaration *, 4> decls{};

  /// The import declarations in this scope in declaration order. Kept
  /// separate from `decls` because imports resolve by see-through searches
  /// (see `Declaration::findThroughImport`) and always precede every other
  /// declaration in their scope, so probing `decls` first preserves
  /// newest-first declaration order.
  llvm::SmallVector<Declaration *, 4> imports{};

  /// The `using` alias declarations in this scope, in declaration order
  /// with their sequence numbers. Aliases participate only in import-path
  /// substitution (see `Emitter::resolveImportUsingAliases`), never in
  /// name resolution.
  llvm::SmallVector<std::pair<AST::UsingAlias *, uint64_t>, 2> usingAliases{};
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
      return astType->hasQualifier("const") || builtinConst;
    return builtinConst;
  }

  /// Is marked with the keyword `inline`?
  [[nodiscard]] bool isInline() const {
    if (auto astType{getASTType()})
      return astType->hasQualifier("inline");
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
  [[nodiscard]] bool hasAllDefaultInitializers() const {
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
  using LookupSequence = std::vector<std::pair<const Parameter *, unsigned>>;

  /// Get the lookup sequence to access the given parameter name. Returns false
  /// on failure.
  [[nodiscard]] bool getLookupSequence(std::string_view name,
                                       LookupSequence &seq) const;

public:
  /// Is variadic? i.e., ends with `...`?
  bool isVariadic{};

  /// The scope at the parameter list's declaration site — together with
  /// `lastSeq` this is the resolution anchor for lazily emitted bodies,
  /// and it is null iff no anchor was captured. See
  /// `Emitter::captureResolutionAnchor`/`restoreResolutionAnchor`.
  Scope *lastScope{};

  /// The declaration sequence visibility limit paired with `lastScope`.
  uint64_t lastSeq{};
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
