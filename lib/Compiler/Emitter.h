// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#pragma once

#include "Context.h"

namespace smdl {

/// \addtogroup Compiler
/// \{

/// An emitter, responsible for the most important code generation!
class Emitter final {
public:
  /// A pending result value.
  class Result final {
  public:
    /// The value.
    Value value{};

    /// The block.
    llvm::BasicBlock *block{};

    /// The source location of the `return` statement.
    SourceLocation srcLoc{};
  };

  /// A pending explicit '#inline(...)' request.
  class Inline final {
  public:
    /// The value. This should be some instance of 'llvm::CallBase'.
    Value value{};

    /// The source location of the '#inline(...)' request.
    SourceLocation srcLoc{};

    /// Is meant to inline recursively?
    bool isRecursive{};
  };

  /// A label representing where to branch to eventually.
  class Label final {
  public:
    /// The crumb to unwind to.
    Crumb *crumb{};

    /// The block to branch to.
    llvm::BasicBlock *block{};

    /// Implicit conversion to bool.
    [[nodiscard]] operator bool() const { return block != nullptr; }
  };

  explicit Emitter(Context &context) : context(context), builder(context) {
    auto fmf{llvm::FastMathFlags::getFast()};
    fmf.setNoNaNs(false); // Don't assume no NaNs!
    fmf.setNoInfs(false); // Don't assume no Infs!
    builder.setFastMathFlags(fmf);
  }

public:
  /// Get the active insert block.
  [[nodiscard]] llvm::BasicBlock *getInsertBlock() {
    return builder.GetInsertBlock();
  }

  /// Get the LLVM function of the active insert block.
  [[nodiscard]] llvm::Function *getLLVMFunction() {
    return getInsertBlock() ? getInsertBlock()->getParent() : nullptr;
  }

  /// Does the insert block have a terminator instruction? (e.g.,
  /// unconditional branch, unreachable, no-return function call)
  [[nodiscard]] bool hasTerminator() {
    return getInsertBlock() && getInsertBlock()->getTerminator() != nullptr;
  }

public:
  /// \name Fundamental operations
  /// \{

  /// Create function implementation.
  ///
  /// \param[in] name
  /// The function name.
  ///
  /// \param[in] isPure
  /// Is pure? If false, `state` must be available.
  ///
  /// \param[in] returnType
  /// The return type. Must be non-null! Used to construct
  /// the result with `create_result()`.
  ///
  /// \param[in] params
  /// The parameters.
  ///
  /// \param[in] paramValues
  /// The values to use for the parameters. Must be the same size as `params`.
  ///
  /// \param[in] srcLoc
  /// The source location if applicable.
  ///
  /// \param[in] callback
  /// The callback to populate the LLVM function body.
  ///
  Value createFunctionImplementation(std::string_view name, bool isPure,
                                     Type *returnType,
                                     const ParameterList &params,
                                     llvm::ArrayRef<Value> paramValues,
                                     const SourceLocation &srcLoc,
                                     const std::function<void()> &callback);

  /// Create function.
  ///
  /// \param[out] llvmFunc
  /// The LLVM function. This is a reference in order to support concrete
  /// recursion, such that the pointer is initialized with a placeholder LLVM
  /// function before invoking the callback.
  ///
  /// \param[in] name
  /// The function name. This is the name ultimately given to the returned
  /// LLVM function with `llvm::Function::setName()`.
  ///
  /// \param[in] isPure
  /// Is pure? If false, the LLVM function is initialized with an extra `State`
  /// pointer at the beginning of the parameter list.
  ///
  /// \param[inout] returnType
  /// The return type. This may be an abstract type like `auto`, in
  /// which case it is overwritten with the inferred concrete type.
  ///
  /// \param[in] paramTypes
  /// The parameter types, must be resolved to concrete types compatible with
  /// the types implied by the `params`!
  ///
  /// \param[in] params
  /// The parameters.
  ///
  /// \param[in] srcLoc
  /// The source location if applicable.
  ///
  /// \param[in] callback
  /// The callback to populate the LLVM function body.
  ///
  void createFunction(llvm::Function *&llvmFunc, std::string_view name,
                      bool isPure, Type *&returnType,
                      llvm::ArrayRef<Type *> paramTypes,
                      const ParameterList &params, const SourceLocation &srcLoc,
                      const std::function<void()> &callback);

  [[nodiscard]]
  llvm::Function *createFunction(std::string_view name, bool isPure,
                                 Type *&returnType, const ParameterList &params,
                                 const SourceLocation &srcLoc,
                                 const std::function<void()> &callback) {
    llvm::Function *llvmFunc{};
    createFunction(llvmFunc, name, isPure, returnType, params.getTypes(),
                   params, srcLoc, callback);
    return llvmFunc;
  }

  /// Create block with the given name.
  [[nodiscard]] llvm::BasicBlock *createBlock(const llvm::Twine &name = {}) {
    return llvm::BasicBlock::Create(context, name, getLLVMFunction());
  }

  /// Create 1 or more blocks at once with a `baseName + extension` naming
  /// convention.
  ///
  /// The intended usage is like this:
  /// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  /// auto [blockCond, blockThen, blockElse] =
  ///     create_blocks<3>("select", {".cond", ".then", ".else"});
  /// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ///
  /// \note
  /// The `baseName` is uniqued by `Context::get_unique_name()` with respect
  /// to the current LLVM function before being concatenated with the
  /// extensions. This makes for most readable LLVM IR.
  ///
  template <size_t N>
  [[nodiscard]] std::array<llvm::BasicBlock *, N>
  createBlocks(llvm::StringRef baseName,
               std::array<llvm::StringRef, N> extensions) {
    auto name{context.getUniqueName(baseName, getLLVMFunction())};
    auto blocks{std::array<llvm::BasicBlock *, N>{}};
    for (size_t i = 0; i < N; i++)
      blocks[i] = createBlock(name + extensions[i]);
    return blocks;
  }

  /// Create an alloca as an uninitialized lvalue.
  ///
  /// \note
  /// Each alloca is created in the LLVM function entry block. If there is
  /// no LLVM function (i.e., `create_function()` was never called), an
  /// `SMDL_SANITY_CHECK` crashes the program!
  ///
  [[nodiscard]] Value createAlloca(Type *type, const llvm::Twine &name = {});

  /// Create lifetime start intrinsic for optimizing alloca usage.
  void createLifetimeStart(Value value) {
    if (value.isLValue() && llvm::isa<llvm::AllocaInst>(value.llvmValue)) {
      builder.CreateLifetimeStart(
          value, builder.getInt64(context.getSizeOf(value.type)));
    }
  }

  /// Create lifetime end intrinsic for optimizing alloca usage.
  void createLifetimeEnd(Value value) {
    if (value.isLValue() && llvm::isa<llvm::AllocaInst>(value.llvmValue)) {
      builder.CreateLifetimeEnd(
          value, builder.getInt64(context.getSizeOf(value.type)));
    }
  }

  /// Declare crumb.
  auto declareCrumb(Span<const std::string_view> name, AST::Node *node,
                    Value value, Value valueToPreserve = {}) {
    return (crumb = context.allocator.allocate<Crumb>(crumb, name, node, value,
                                                      valueToPreserve));
  }

  /// Declare parameter.
  void declareParameter(const Parameter &param, Value value);

  /// Declare parameter as inline.
  void declareParameterInline(Value value);

  /// Declare import.
  void declareImport(Span<const std::string_view> importPath, bool isAbs,
                     AST::Decl &decl);

  /// Guarantee that the given value is an lvalue.
  ///
  /// - If the value is already an lvalue, return it.
  /// - If the value is an rvalue, copy it into an alloca and return the lvalue.
  ///
  [[nodiscard]] Value lvalue(Value value) {
    if (value.isRValue() && !value.isVoid()) {
      auto lv{createAlloca(value.type, value.llvmValue->hasName()
                                           ? value.llvmValue->getName() + ".lv"
                                           : "")};
      createLifetimeStart(lv);
      builder.CreateStore(value, lv);
      return lv;
    }
    return value;
  }

  /// Guarantee that the given value is an rvalue.
  ///
  /// - If the value is already an rvalue, return it.
  /// - If the value is an lvalue, load it into a register and return the
  ///   rvalue.
  ///
  [[nodiscard]] Value rvalue(Value value) {
    if (value.isLValue())
      return RValue(
          value.type,
          value.type->isVoid()
              ? nullptr
              : builder.CreateLoad(value.type->llvmType, value,
                                   value.llvmValue->hasName()
                                       ? value.llvmValue->getName() + ".rv"
                                       : ""));
    return value;
  }

  /// Unwind to the given `lastCrumb`.
  void unwind(Crumb *lastCrumb);

  /// This is the primary mechanism for handling scope.
  ///
  /// \param[in] blockStart
  /// The basic block to start the scope. This is optional! If present, the
  /// implementation assumes that it is empty, moves it to the end of the LLVM
  /// function for readability, then sets it as the insert point of the
  /// IR builder.
  ///
  /// \param[in] blockEnd
  /// The basic block to end the scope. This is optional! If present, it
  /// represents where the IR builder should branch to at the end of the
  /// scope.
  ///
  /// \param[in] func
  /// The function to emit code inside the scope.
  ///
  /// \note
  /// - The implementation _does not_ implicitly branch to `blockStart` before
  ///   setting the insert point. It assumes the branch or conditional branch
  ///   has already been emitted.
  /// - The implementation _does_ implicitly branch to `blockEnd`, but only if
  ///   the intermediate code generation does not add an explicit terminator
  ///   (i.e., `return`, `break`, or `continue`).
  /// - The implementation also preserves the crumb, `$state` value, and
  ///   all labels. These may be modified by `func` with the expectation that
  ///   they will be restored at the end of the scope.
  ///
  template <typename Func>
  void handleScope(llvm::BasicBlock *blockStart, llvm::BasicBlock *blockEnd,
                   Func &&func) {
    SMDL_PRESERVE(crumb, state, labelReturn, labelBreak, labelContinue, inDefer,
                  currentModule);
    auto crumb0{crumb};
    if (blockStart) {
      llvmMoveBlockToEnd(blockStart);
      builder.SetInsertPoint(blockStart);
    }
    std::invoke(std::forward<Func>(func));
    if (!hasTerminator()) {
      unwind(crumb0);
      if (blockEnd)
        builder.CreateBr(blockEnd);
    }
    if (blockEnd && !blockEnd->getTerminator())
      llvmMoveBlockToEnd(blockEnd);
  }

  /// Move the insert point to the beginning of the given block if it has been
  /// branched to. If the block has no predecessors, erase it.
  void handleBlockEnd(llvm::BasicBlock *block) {
    if (block->hasNPredecessors(0)) {
      block->eraseFromParent();
    } else {
      llvmMoveBlockToEnd(block);
      builder.SetInsertPoint(block);
    }
  }

  /// Create result PHI instruction.
  Value createResult(Type *resultType, llvm::ArrayRef<Result> results,
                     const SourceLocation &srcLoc);

  /// \}

public:
  /// \name Type operations
  /// \{

  /// Wraps `Type::invoke()`
  [[nodiscard]] Value invoke(Type *type, const ArgumentList &args,
                             const SourceLocation &srcLoc) {
    return type->invoke(*this, args, srcLoc);
  }

  /// Wraps `Type::invoke()` after looking up `keyword` in the context.
  [[nodiscard]] Value invoke(const char *keyword, const ArgumentList &args,
                             const SourceLocation &srcLoc) {
    return invoke(context.getKeywordAsType(keyword, srcLoc), args, srcLoc);
  }

  /// Wraps `Type::accessField()`
  [[nodiscard]] Value accessField(Value value, std::string_view key,
                                  const SourceLocation &srcLoc) {
    return value.type->accessField(*this, value, key, srcLoc);
  }

  /// Wraps `Type::accessIndex()`
  [[nodiscard]] Value accessIndex(Value value, Value i,
                                  const SourceLocation &srcLoc) {
    return value.type->accessIndex(*this, value, i, srcLoc);
  }

  /// Wraps `Type::accessIndex()`
  [[nodiscard]] Value accessIndex(Value value, unsigned i,
                                  const SourceLocation &srcLoc = {}) {
    return accessIndex(value, context.getComptimeInt(int(i)), srcLoc);
  }

  /// Wraps `Type::access_index()` for every index.
  [[nodiscard]] std::vector<Value>
  accessEveryIndex(Value value, unsigned n, const SourceLocation &srcLoc,
                   const std::function<Value(unsigned, Value)> &pred = {}) {
    std::vector<Value> elems{};
    for (unsigned i = 0; i < n; i++) {
      auto &elem{elems.emplace_back(accessIndex(value, i, srcLoc))};
      if (pred)
        elem = pred(i, elem);
    }
    return elems;
  }

  /// Wraps `Type::insert()`
  [[nodiscard]] Value insert(Value value, Value elem, unsigned i,
                             const SourceLocation &srcLoc = {}) {
    return value.type->insert(*this, value, elem, i, srcLoc);
  }

  /// \}

public:
  /// \name AST Emit
  /// \{

  /// Emit node.
  Value emit(AST::Node &node);

  /// Emit file.
  Value emit(AST::File &file) {
    for (auto &decl : file.importDecls)
      emit(decl);
    for (auto &decl : file.globalDecls)
      emit(decl);
    return Value();
  }

  /// Emit declaration.
  Value emit(AST::Decl &decl);

  /// Emit annotation declaration.
  Value emit(AST::AnnotationDecl & /*decl*/) {
    // TODO
    return Value();
  }

  /// Emit enum declaration.
  Value emit(AST::Enum &decl) {
    context.getEnumType(&decl)->initialize(*this);
    return Value();
  }

  /// Emit exec declaration.
  Value emit(AST::Exec &decl);

  /// Emit function declaration.
  Value emit(AST::Function &decl) {
    context.getFunctionType(&decl)->initialize(*this);
    return Value();
  }

  /// Emit import declaration.
  Value emit(AST::Import &decl) {
    if (decl.isExported())
      decl.srcLoc.throwError("cannot re-export qualified 'import'");
    for (auto &[importPath, srcComma] : decl.importPathWrappers)
      declareImport(importPath, importPath.isAbsolute(), decl);
    return Value();
  }

  /// Emit namespace declaration.
  Value emit(AST::Namespace &decl) {
    decl.firstCrumb = declareCrumb(*decl.identifier, &decl,
                                   context.getComptimeMetaNamespace(&decl));
    SMDL_PRESERVE(crumb);
    for (auto &each : decl.decls)
      emit(each);
    decl.lastCrumb = crumb;
    return Value();
  }

  /// Emit struct declaration.
  Value emit(AST::Struct &decl) {
    context.getStructType(&decl)->initialize(*this);
    return Value();
  }

  /// Emit tag declaration.
  Value emit(AST::Tag &decl) {
    declareCrumb(decl.name, &decl,
                 context.getComptimeMetaType(context.getTagType(&decl)));
    return Value();
  }

  /// Emit typedef declaration.
  Value emit(AST::Typedef &decl) {
    declareCrumb(decl.name, &decl, emit(decl.type));
    return Value();
  }

  /// Emit unit-test declaration.
  Value emit(AST::UnitTest &decl);

  /// Emit using alias declaration.
  Value emit(AST::UsingAlias &decl) {
    declareCrumb(/*name=*/{}, &decl, /*value=*/{});
    return Value();
  }

  /// Emit using import declaration.
  Value emit(AST::UsingImport &decl);

  /// Emit variable declaration.
  Value emit(AST::Variable &decl);

  /// Emit expression.
  Value emit(AST::Expr &expr);

  /// Emit access-field expression.
  Value emit(AST::AccessField &expr) {
    return accessField(emit(expr.expr), expr.name.srcName, expr.srcLoc);
  }

  /// Emit access-index expression.
  Value emit(AST::AccessIndex &expr);

  /// Emit binary expression.
  Value emit(AST::Binary &expr);

  /// Emit call expression.
  Value emit(AST::Call &expr) {
    return emitCall(emit(expr.expr), emit(expr.args), expr.srcLoc);
  }

  /// Emit identifier expression.
  Value emit(AST::Identifier &expr) {
    return resolveIdentifier(expr, expr.srcLoc);
  }

  /// Emit intrinsic expression.
  Value emit(AST::Intrinsic &expr) {
    return context.getComptimeMetaIntrinsic(&expr);
  }

  /// Emit let expression.
  Value emit(AST::Let &expr) {
    auto crumb0{crumb};
    for (auto &decl : expr.decls)
      emit(decl);
    auto value{rvalue(emit(expr.expr))};
    unwind(crumb0);
    return value;
  }

  /// Emit literal bool expression.
  Value emit(AST::LiteralBool &expr) {
    return context.getComptimeBool(expr.value);
  }

  /// Emit literal float expression.
  Value emit(AST::LiteralFloat &expr) {
    auto src{llvm::StringRef(expr.srcValue)};
    if (src.ends_with_insensitive("jd")) {
      return invoke("complex",
                    {context.getComptimeDouble(0.0),
                     context.getComptimeDouble(expr.value)},
                    expr.srcLoc);
    } else if (src.ends_with_insensitive("jf") ||
               src.ends_with_insensitive("j")) {
      return invoke("complex",
                    {context.getComptimeFloat(0.0f),
                     context.getComptimeFloat(float(expr.value))},
                    expr.srcLoc);
    } else if (src.ends_with_insensitive("d")) {
      return context.getComptimeDouble(expr.value);
    } else {
      return context.getComptimeFloat(float(expr.value));
    }
  }

  /// Emit literal int expression.
  Value emit(AST::LiteralInt &expr) {
    // If the literal value is greater than the maximum `int` promote
    // it to `int64`.
    if (expr.value > uint64_t(std::numeric_limits<int>::max())) {
      auto intType{context.getArithmeticType(Scalar::getInt(64), Extent(1))};
      return RValue(intType,
                    llvm::ConstantInt::get(intType->llvmType,
                                           llvm::APInt(64, expr.value)));
    }
    return context.getComptimeInt(int(expr.value));
  }

  /// Emit literal string expression.
  Value emit(AST::LiteralString &expr) {
    return context.getComptimeString(expr.value);
  }

  /// Emit parenthesized expression.
  Value emit(AST::Parens &expr);

  /// Emit return-from expression.
  Value emit(AST::ReturnFrom &expr);

  /// Emit select expression.
  Value emit(AST::Select &expr);

  /// Emit type expression.
  Value emit(AST::Type &expr) {
    auto value{emit(expr.expr)};
    if (value.type != context.getMetaTypeType())
      expr.srcLoc.throwError("expected expression to resolve to a type");
    expr.type = value.getComptimeMetaType(context, expr.srcLoc);
    return value;
  }

  /// Emit type-cast expression.
  Value emit(AST::TypeCast &expr) {
    return invoke(emit(expr.type).getComptimeMetaType(context, expr.srcLoc),
                  emit(expr.expr), expr.srcLoc);
  }

  /// Emit unary expression.
  Value emit(AST::Unary &expr) {
    return emitOp(expr.op, emit(expr.expr), expr.srcLoc);
  }

  /// Emit late-if helper.
  template <typename Func>
  void emitLateIf(std::optional<AST::LateIf> &lateIf, Func &&func) {
    if (!lateIf) {
      std::invoke(std::forward<Func>(func));
    } else {
      auto [blockThen, blockElse] =
          createBlocks<2>("late_if", {".then", ".else"});
      builder.CreateCondBr(invoke(context.getBoolType(), emit(lateIf->expr),
                                  lateIf->expr->srcLoc),
                           blockThen, blockElse);
      handleScope(blockThen, blockElse, std::forward<Func>(func));
      builder.SetInsertPoint(blockElse);
    }
  }

  /// Emit statement.
  Value emit(AST::Stmt &stmt);

  /// Emit break statement.
  Value emit(AST::Break &stmt) {
    SMDL_SANITY_CHECK(!hasTerminator());
    if (!labelBreak)
      stmt.srcLoc.throwError(inDefer ? "cannot 'break' from 'defer'"
                                     : "nowhere to 'break'");
    emitLateIf(stmt.lateIf, [&] {
      unwind(labelBreak.crumb);
      builder.CreateBr(labelBreak.block);
    });
    return Value();
  }

  /// Emit compound statement.
  Value emit(AST::Compound &stmt);

  /// Emit continue statement.
  Value emit(AST::Continue &stmt) {
    SMDL_SANITY_CHECK(!hasTerminator());
    if (!labelContinue)
      stmt.srcLoc.throwError(inDefer ? "cannot 'continue' from 'defer'"
                                     : "nowhere to 'continue'");
    emitLateIf(stmt.lateIf, [&] {
      unwind(labelContinue.crumb);
      builder.CreateBr(labelContinue.block);
    });
    return Value();
  }

  /// Emit declaration statement.
  Value emit(AST::DeclStmt &stmt) { return emit(stmt.decl); }

  /// Emit defer statement.
  Value emit(AST::Defer &stmt) {
    declareCrumb(/*name=*/{}, &stmt, /*value=*/{});
    return Value();
  }

  /// Emit do-while statement.
  Value emit(AST::DoWhile &stmt);

  /// Emit expression statement.
  Value emit(AST::ExprStmt &stmt) {
    if (stmt.expr)
      emitLateIf(stmt.lateIf, [&] { emit(stmt.expr); });
    return Value();
  }

  /// Emit for statement.
  Value emit(AST::For &stmt);

  /// Emit if statement.
  Value emit(AST::If &stmt);

  /// Emit preserve statement.
  Value emit(AST::Preserve &stmt) {
    for (auto &[expr, srcComma] : stmt.exprWrappers) {
      auto value{emit(expr)};
      if (!value.isLValue())
        stmt.srcLoc.throwError("cannot 'preserve' rvalue");
      declareCrumb({}, &stmt, value, rvalue(value));
    }
    return Value();
  }

  /// Emit return statement.
  Value emit(AST::Return &stmt) {
    SMDL_SANITY_CHECK(!hasTerminator());
    if (!labelReturn)
      stmt.srcLoc.throwError(inDefer ? "cannot 'return' from 'defer'"
                                     : "nowhere to 'return'");
    emitLateIf(stmt.lateIf, [&] {
      Value value{};
      if (stmt.expr)
        value = rvalue(emit(stmt.expr));
      returns.push_back(Result{value, getInsertBlock(), stmt.srcLoc});
      unwind(labelReturn.crumb);
      builder.CreateBr(labelReturn.block);
    });
    return Value();
  }

  /// Emit switch statement.
  Value emit(AST::Switch &stmt);

  /// Emit unreachable statement.
  Value emit(AST::Unreachable &stmt) {
    if (!getLLVMFunction())
      stmt.srcLoc.throwError(
          "'unreachable' must be within function definition");
    builder.CreateUnreachable();
    return Value();
  }

  /// Emit visit statement.
  Value emit(AST::Visit &stmt) {
    return emitVisit(emit(stmt.expr), stmt.srcLoc, [&](Value value) {
      declareCrumb(stmt.name, &stmt, value);
      if (!value.type->isVoid())
        emit(stmt.stmt);
      return Value();
    });
  }

  /// Emit while statement.
  Value emit(AST::While &stmt);

  /// Emit argument list.
  ArgumentList emit(AST::ArgumentList &astArgs) {
    ArgumentList args{};
    for (auto &astArg : astArgs)
      args.push_back(Argument{astArg.name.srcName, emit(astArg.expr), &astArg});
    args.astArgs = &astArgs;
    args.validateNames();
    return args;
  }

  /// Emit pointer if non-null.
  template <typename T> Value emit(T *ptr) {
    return ptr ? emit(*ptr) : Value();
  }

  /// Emit pointer if non-null.
  template <typename T> Value emit(const BumpPtr<T> &ptr) {
    return emit(ptr.get());
  }

  /// Emit optional if non-null.
  template <typename T> Value emit(std::optional<T> &opt) {
    return opt ? emit(*opt) : Value();
  }

  /// Emit type switch, useful to implement `Node`, `Decl`, `Expr`, and `Stmt`
  /// sub-class dispatch.
  template <typename... Ts, typename T> Value emitTypeSwitch(T &node) {
    return llvm::TypeSwitch<T *, Value>(&node).template Case<Ts...>(
        [&](auto each) { return emit(*each); });
  }

  /// \}

public:
  /// \name Other Emit
  /// \{

  /// Emit unary operation.
  Value emitOp(AST::UnaryOp op, Value value, const SourceLocation &srcLoc);

  /// Emit binary operation.
  Value emitOp(AST::BinaryOp op, Value lhs, Value rhs,
               const SourceLocation &srcLoc);

  /// Helper to emit unary or binary operation columnwise, assuming
  /// `Type` is an arithmetic matrix type.
  template <typename Func>
  [[nodiscard]] Value emitOpColumnwise(Type *type, Func &&func) {
    SMDL_SANITY_CHECK(type);
    SMDL_SANITY_CHECK(type->isArithmeticMatrix());
    auto result{Value::zero(type)};
    for (unsigned j = 0;
         j < static_cast<ArithmeticType *>(type)->extent.numCols; j++)
      result = insert(result, std::invoke(func, j), j, /*srcLoc=*/{});
    return result;
  }

  /// Emit intrinsic.
  Value emitIntrinsic(std::string_view name, const ArgumentList &args,
                      const SourceLocation &srcLoc);

  /// Emit call.
  Value emitCall(Value callee, const ArgumentList &args,
                 const SourceLocation &srcLoc);

  /// Emit visit.
  Value emitVisit(Value value, const SourceLocation &srcLoc,
                  const std::function<Value(Value)> &visitor);

  /// Emit panic.
  void emitPanic(Value message, const SourceLocation &srcLoc);

  /// Emit print.
  void emitPrint(Value file, Value value, const SourceLocation &srcLoc,
                 bool quoteStrings = false);

  void emitPrint(Value file, std::string_view value,
                 const SourceLocation &srcLoc) {
    emitPrint(file, context.getComptimeString(value), srcLoc);
  }

  void emitReturn(Value value, const SourceLocation &srcLoc) {
    SMDL_SANITY_CHECK(!hasTerminator());
    SMDL_SANITY_CHECK(labelReturn.block);
    returns.push_back(Result{value, getInsertBlock(), srcLoc});
    unwind(labelReturn.crumb);
    builder.CreateBr(labelReturn.block);
  }

  [[nodiscard]] Parameter emitParameter(AST::Parameter &astParam) {
    return Parameter{
        emit(astParam.type).getComptimeMetaType(context, astParam.name.srcLoc),
        astParam.name, &astParam};
  }

  [[nodiscard]] ParameterList emitParameterList(AST::ParameterList &astParams) {
    ParameterList params{};
    for (auto &astParam : astParams)
      params.emplace_back(emitParameter(astParam));
    params.isVariadic = astParams.hasTrailingEllipsis();
    params.lastCrumb = crumb;
    return params;
  }

  /// \}

public:
  /// \name Resolve
  /// \{

  /// Resolve identifier to value.
  [[nodiscard]] Value resolveIdentifier(Span<const std::string_view> names,
                                        const SourceLocation &srcLoc,
                                        bool voidByDefault = false);

  /// The return structure of `resolveArguments()`.
  struct ResolvedArguments final {
  public:
    ResolvedArguments(const ParameterList &params, const ArgumentList &args)
        : params(params), args(args) {
      argParams.resize(args.size());
      values.resize(params.size());
    }

    [[nodiscard]] bool isResolved(size_t iArg) const {
      return iArg < argParams.size() && argParams[iArg] != nullptr;
    }

    [[nodiscard]] llvm::SmallVector<Type *> getNonVariadicTypes() const {
      auto valueTypes{llvm::SmallVector<Type *>(values.size())};
      for (size_t i = 0; i < values.size(); i++)
        valueTypes[i] = values[i].type;
      // For crappy support of C-style variadic functions, if the
      // parameters have an ellipsis, then the resolveArguments() routine
      // allows for more arguments than parameters, but we use this function
      // for initializing the LLVM function type, which should only depend on
      // the non-variadic value types
      if (params.isVariadic)
        valueTypes.resize(params.size());
      return valueTypes;
    }

    [[nodiscard]] std::optional<ArgumentList> getImpliedVisitArguments() const {
      bool impliedVisit{false};
      auto impliedVisitArgs{args};
      for (size_t i = 0; i < args.size(); i++) {
        if (argParams[i] != nullptr && //
            argParams[i]->type->isAbstract() &&
            args[i].value.type->isUnionOrPointerToUnion()) {
          impliedVisitArgs[i].impliedVisit = impliedVisit = true;
        }
      }
      if (!impliedVisit)
        return std::nullopt;
      return std::move(impliedVisitArgs);
    }

  public:
    /// The parameters passed to `resolveArguments()`.
    const ParameterList &params;

    /// The arguments passed to `resolveArguments()`.
    const ArgumentList &args;

    /// The parameters matched to the arguments, in correspondence with `args`.
    llvm::SmallVector<const Parameter *> argParams;

    /// The values, in correspondence with `params`.
    llvm::SmallVector<Value> values;
  };

  /// Resolve the given arguments or throw an error on failure.
  ///
  /// \param[in] params
  /// The parameters.
  ///
  /// \param[in] args
  /// The arguments to resolve against the parameters.
  ///
  /// \param[in] srcLoc
  /// The source location if applicable.
  ///
  /// \param[in] dontEmit
  /// Do not emit anything? If true and resolution is successful,
  /// do not actually emit the code for converting the arguments
  /// into the types and order of the parameters.
  ///
  [[nodiscard]] ResolvedArguments resolveArguments(const ParameterList &params,
                                                   const ArgumentList &args,
                                                   const SourceLocation &srcLoc,
                                                   bool dontEmit = false);

  /// Can resolve arguments?
  ///
  /// This simply wraps `resolveArguments()` with a `try` block and returns
  /// false if an error is thrown.
  ///
  [[nodiscard]] bool canResolveArguments(const ParameterList &params,
                                         const ArgumentList &args,
                                         const SourceLocation &srcLoc) try {
    auto res{resolveArguments(params, args, srcLoc, /*dontEmit=*/true)};
    return true;
  } catch (...) {
    return false;
  }

  /// Resolve module.
  [[nodiscard]] Module *resolveModule(Span<const std::string_view> importPath,
                                      bool isAbs, Module *thisModule);

  /// Resolve import using aliases in import path.
  void resolveImportUsingAliases(
      Crumb *crumbStart, Span<const std::string_view> importPath,
      llvm::SmallVector<std::string_view> &finalImportPath);

  /// \}

public:
  /// The context.
  Context &context;

  /// The pointer to the last crumb. See `declare_crumb()`.
  Crumb *crumb{};

  /// The `$state` value.
  Value state{};

  /// The label to branch to after `return` statement.
  Label labelReturn{};

  /// The label to branch to after `break` statement.
  Label labelBreak{};

  /// The label to branch to after `continue` statement.
  Label labelContinue{};

  /// Is currently in defer statement? This allows for more specific error
  /// messages if the user tries to defer a `break`, `continue`, or `return`
  /// statement.
  bool inDefer{};

  /// The pending results.
  llvm::SmallVector<Result> returns{};

  /// The explicit `#inline(...)` requests.
  llvm::SmallVector<Inline> inlines{};

  /// The LLVM-IR builder.
  llvm::IRBuilder<> builder;

  /// The intermediate crumbs to potentially warn about later.
  llvm::SmallVector<Crumb *> crumbsToWarnAbout{};

  Module *currentModule{context.currentModule};
};

/// \}

} // namespace smdl
