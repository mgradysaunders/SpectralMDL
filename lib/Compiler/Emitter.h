// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#pragma once

#include "Context.h"

namespace smdl::Compiler {

class Emitter final {
public:
  class Label final {
  public:
    Breadcrumb *lastBreadcrumb{};

    llvm::BasicBlock *block{};

    [[nodiscard]] operator bool() const { return block; }

    [[nodiscard]] operator llvm::BasicBlock *() const { return block; }
  };

  class Semantics final {
  public:
    /// The last breadcrumb.
    Breadcrumb *lastBreadcrumb{};

    /// The `$state` value.
    Value state{};

    /// The pending returns.
    llvm::SmallVector<Return> *returns{};

    /// The explicit inlines via `#inline(...)` requests.
    llvm::SmallVector<Inline> *inlines{};

    /// Where to go after `break` statement.
    Label breakTo{};

    /// Where to go after `continue` statement.
    Label continueTo{};

    /// Where to go after `return` statement.
    Label returnTo{};

    /// Where to go after expiring at the natural end-of-scope.
    Label expireTo{};

    /// The current macro recursion depth.
    uint32_t macroRecursionDepth{};
  };

  explicit Emitter(
      Context &context, Breadcrumb *lastBreadcrumb = nullptr, //
      llvm::SmallVector<Return> *returns = nullptr,           //
      llvm::SmallVector<Inline> *inlines = nullptr,           //
      llvm::Function *llvmFunc = nullptr)
      : context(context), builder(context.llvmContext) {
    semantics->lastBreadcrumb = lastBreadcrumb;
    semantics->returns = returns;
    semantics->inlines = inlines;
    auto fmf{llvm::FastMathFlags::getFast()};
    fmf.setNoNaNs(false); // Don't assume no NaNs!
    fmf.setNoInfs(false); // Don't assume no Infs!
    builder.setFastMathFlags(fmf);
    if (llvmFunc) {
      auto blockEntry{llvm::BasicBlock::Create(context.llvmContext, "entry", llvmFunc)};
      auto blockReturn{llvm::BasicBlock::Create(context.llvmContext, "return", llvmFunc)};
      semantics->returnTo = {lastBreadcrumb, blockReturn};
      builder.SetInsertPoint(blockEntry);
    }
  }

public:
  //--{ Helpers
  [[nodiscard]] llvm::BasicBlock *get_insert_block() { return builder.GetInsertBlock(); }

  [[nodiscard]] bool has_terminator() { return get_insert_block() && get_insert_block()->getTerminator() != nullptr; }

  void move_to(llvm::BasicBlock *block) { builder.SetInsertPoint(block); }

  void move_to_or_erase(llvm::BasicBlock *block) {
    if (block->hasNPredecessors(0)) {
      block->eraseFromParent();
    } else {
      move_to(block);
    }
  }

  [[nodiscard]] llvm::Function *get_llvm_function() { return get_insert_block() ? get_insert_block()->getParent() : nullptr; }

  [[nodiscard]] llvm::BasicBlock *get_llvm_function_entry_block() {
    if (auto llvmFunc{get_llvm_function()})
      return &llvmFunc->getEntryBlock();
    return nullptr;
  }

  [[nodiscard]] llvm::BasicBlock *create_block(const llvm::Twine &name) {
    return llvm::BasicBlock::Create(context.llvmContext, name, sanity_check_nonnull(get_llvm_function()));
  }
  //--}

public:
  //--{ Fundamental operations
  void push(
      Value value, llvm::ArrayRef<llvm::StringRef> name, AST::Node *node, const AST::SourceLocation &srcLoc,
      Value valueToPreserve = {}) {
    semantics->lastBreadcrumb =
        context.bump_allocate<Breadcrumb>(Breadcrumb{semantics->lastBreadcrumb, value, name, node, srcLoc, valueToPreserve});
  }

  void declare(const AST::Name &name, Value value, AST::Decl *decl = {}) {
    // We assume `name` is a reference to a persistent name in the AST, so we can directly
    // construct an `llvm::ArrayRef` wrapping a pointer to `name.srcName` without needing
    // to `context.bump_duplicate()` it.
    push(value, llvm::ArrayRef(name.srcName), decl, name.srcLoc);
  }

  void declare_function_parameter(const Param &param, Value value);

  void declare_function_parameter_inline(Value value);

  void declare_import(bool isAbs, llvm::ArrayRef<llvm::StringRef> path, AST::Decl &decl);

  [[nodiscard]] Value insert(Value value, Value elem, unsigned i, const AST::SourceLocation &srcLoc = {}) {
    return value.type->insert(*this, value, elem, i, srcLoc);
  }

  [[nodiscard]] Value access(Value value, llvm::StringRef fieldName, const AST::SourceLocation &srcLoc = {}) {
    return value.type->access(*this, value, fieldName, srcLoc);
  }

  [[nodiscard]] Value access(Value value, Value i, const AST::SourceLocation &srcLoc = {}) {
    return value.type->access(*this, value, i, srcLoc);
  }

  [[nodiscard]] Value access(Value value, unsigned i, const AST::SourceLocation &srcLoc = {}) {
    return value.type->access(*this, value, context.get_compile_time_int(i), srcLoc);
  }

  [[nodiscard]] Value construct(Type *type, const ArgList &args, const AST::SourceLocation &srcLoc = {}) {
    if (args.is_one_positional() && args[0].value.type == type)
      return rvalue(args[0].value);
    return type->construct(*this, args, srcLoc);
  }

  [[nodiscard]] Value lvalue(Value value, bool manageLifetime = true);

  [[nodiscard]] Value rvalue(Value value);
  //--}

public:
  template <typename T, typename Deleter> auto emit(const std::unique_ptr<T, Deleter> &ptr) {
    return emit(*sanity_check_nonnull(ptr.get()));
  }

  template <typename T> auto emit(const std::optional<T> &opt) {
    sanity_check(opt.has_value());
    return emit(*opt);
  }

  Value emit(AST::Node &astNode);

  Value emit(AST::File &astFile) {
    for (auto &astDecl : astFile.imports)
      emit(astDecl);
    for (auto &astDecl : astFile.globals)
      emit(astDecl);
    return {};
  }

  //--{ Emit: Decls
  Value emit(AST::Decl &astDecl);

  Value emit(AST::Enum &astEnum);

  Value emit(AST::Function &astFunction);

  Value emit(AST::Import &astImport) {
    if (astImport.isExport)
      astImport.srcLoc.report_error("can't re-export qualified 'import'");
    for (auto &path : astImport.paths)
      declare_import(path.identifier->is_absolute(), path.identifier->get_string_refs(), astImport);
    return {};
  }

  Value emit(AST::Struct &astStruct);

  Value emit(AST::Tag &astTag) {
    context.validate_decl_name("tag", astTag.name);
    declare(astTag.name, context.get_compile_time_type(context.get_tag_type(&astTag, get_llvm_function())), &astTag);
    return {};
  }

  Value emit(AST::Typedef &astTypedef) {
    context.validate_decl_name("typedef", astTypedef.name);
    declare(astTypedef.name, emit(astTypedef.type), &astTypedef);
    return {};
  }

  Value emit(AST::UnitTest &astUnitTest);

  Value emit(AST::UsingAlias &astUsingAlias) {
    push({}, {}, &astUsingAlias, astUsingAlias.srcLoc);
    return {};
  }

  Value emit(AST::UsingImport &astUsingImport) {
    auto path{astUsingImport.path->get_string_refs()};
    for (auto &name : astUsingImport.names) {
      path.push_back(name.srcName);
      declare_import(astUsingImport.path->is_absolute(), path, astUsingImport);
      path.pop_back();
    }
    return {};
  }

  Value emit(AST::Variable &astVariable);
  //--}

  //--{ Emit: Exprs
  Value emit(AST::Expr &astExpr);

  Value emit(AST::Binary &astBinary);

  Value emit(AST::Call &astCall) { return emit_call(emit(astCall.expr), emit(astCall.args), astCall.srcLoc); }

  Value emit(AST::Cast &astCast) {
    return construct(emit(astCast.type).get_compile_time_type(), emit(astCast.expr), astCast.srcLoc);
  }

  Value emit(AST::Conditional &astConditional);

  Value emit(AST::GetField &astGet) { return access(emit(astGet.expr), astGet.name.srcName, astGet.name.srcLoc); }

  Value emit(AST::GetIndex &astGet);

  Value emit(AST::Identifier &astIdentifier) { return context.resolve(*this, astIdentifier); }

  Value emit(AST::Intrinsic &astIntrinsic) { return context.get_compile_time_intrinsic(&astIntrinsic); }

  Value emit(AST::Let &astLet);

  Value emit(AST::LiteralBool &astLiteral) { return context.get_compile_time_bool(astLiteral.value); }

  Value emit(AST::LiteralFloat &astLiteral) { return context.get_compile_time_FP(astLiteral.value, astLiteral.precision); }

  Value emit(AST::LiteralInt &astLiteral) { return context.get_compile_time_int(astLiteral.value); }

  Value emit(AST::LiteralString &astLiteral) { return context.get_compile_time_string(astLiteral.value); }

  Value emit(AST::Parens &astParens);

  Value emit(AST::ReturnFrom &astReturnFrom);

  Value emit(AST::Type &astType);

  Value emit(AST::Unary &astUnary) { return emit_op(astUnary.op, emit(astUnary.expr), astUnary.srcLoc); }
  //--}

  //--{ Emit: Stmts
  Value emit(AST::Stmt &astStmt);

  Value emit(AST::Break &astBreak) {
    if (!semantics->breakTo)
      astBreak.srcLoc.report_error("unexpected 'break'");
    emit_late_if(astBreak.lateIf, [&] { emit_unwind_and_br(semantics->breakTo); });
    return {};
  }

  Value emit(AST::Compound &astCompound);

  Value emit(AST::Continue &astContinue) {
    if (!semantics->continueTo)
      astContinue.srcLoc.report_error("unexpected 'continue'");
    emit_late_if(astContinue.lateIf, [&] { emit_unwind_and_br(semantics->continueTo); });
    return {};
  }

  Value emit(AST::DeclStmt &astDeclStmt) { return emit(astDeclStmt.decl); }

  Value emit(AST::Defer &astDefer) {
    push({}, {}, &astDefer, astDefer.srcLoc);
    return {};
  }

  Value emit(AST::DoWhile &astDoWhile);

  Value emit(AST::ExprStmt &astExprStmt) {
    if (astExprStmt.expr)
      emit_late_if(astExprStmt.lateIf, [&] { emit(astExprStmt.expr); });
    return {};
  }

  Value emit(AST::For &astFor);

  Value emit(AST::If &astIf);

  Value emit(AST::Preserve &astPreserve);

  Value emit(AST::Return &astReturn) {
    if (!semantics->returnTo)
      astReturn.srcLoc.report_error("unexpected 'return'");
    emit_late_if(astReturn.lateIf, [&] { emit_return(astReturn.expr.get() ? emit(astReturn.expr) : Value(), astReturn.srcLoc); });
    return {};
  }

  Value emit(AST::Switch &astSwitch);

  Value emit(AST::Unreachable &astUnreachable) {
    sanity_check(get_llvm_function());
    builder.CreateUnreachable();
    return {};
  }

  Value emit(AST::Visit &astVisit) {
    emit_visit(emit(astVisit.expr), [&](Emitter &emitter, Value value) -> Value {
      emitter.declare(astVisit.name, value);
      if (!value.is_void())
        emitter.emit(astVisit.body);
      return {};
    });
    return {};
  }

  Value emit(AST::While &astWhile);
  //--}

  ArgList emit(const AST::ArgList &astArgs);

public:
  [[nodiscard]] Value emit_alloca(const llvm::Twine &name, Type *type);

  void emit_end_lifetime(Value value);

  void emit_scope(llvm::BasicBlock *blockStart, llvm::BasicBlock *blockEnd, std::invocable<> auto &&func) {
    semantics.push();
    semantics->expireTo = {semantics->lastBreadcrumb, blockEnd};

    move_to(blockStart);
    std::invoke(std::forward<decltype(func)>(func));
    emit_unwind_and_br(semantics->expireTo);

    semantics.pop();
  }

  void emit_scope(std::invocable<> auto &&func) { // "Thin scope"
    auto lastBreadcrumb{semantics->lastBreadcrumb};
    std::invoke(std::forward<decltype(func)>(func));
    emit_unwind(lastBreadcrumb);
  }

  Value emit_scope(AST::Expr &astExpr) {
    Value value{};
    emit_scope([&] { value = emit(astExpr); });
    return value;
  }

  void emit_unwind_and_br(Label label) {
    if (!has_terminator()) {
      emit_unwind(label.lastBreadcrumb);
      emit_br(label.block);
    }
  }

  void emit_unwind(Breadcrumb *lastBreadcrumb);

  Value emit_cond(AST::Expr &expr, bool insideScope = true) {
    Value cond{};
    if (insideScope)
      emit_scope([&] { cond = construct(context.get_bool_type(), emit(expr), expr.srcLoc); });
    else
      cond = construct(context.get_bool_type(), emit(expr), expr.srcLoc);
    return cond;
  }

  void emit_br_and_move_to(llvm::BasicBlock *block) {
    emit_br(block);
    move_to(block);
  }

  void emit_br(llvm::BasicBlock *block) {
    sanity_check(!has_terminator());
    builder.CreateBr(block);
  }

  void emit_br(Value cond, llvm::BasicBlock *blockPass, llvm::BasicBlock *blockFail) {
    sanity_check(!has_terminator());
    builder.CreateCondBr(construct(context.get_bool_type(), cond), blockPass, blockFail);
  }

  void emit_late_if(std::optional<AST::LateIf> &astLateIf, std::invocable<> auto &&func) {
    if (!astLateIf) {
      std::invoke(func);
    } else {
      auto lastBreadcrumb{semantics->lastBreadcrumb};
      auto cond{emit_cond(*astLateIf->cond, /*insideScope=*/false)};
      if (cond.is_compile_time_int()) {
        auto pass{cond.get_compile_time_int() != 0};
        if (!pass)
          return;
      }
      auto name{context.get_unique_name("late-if", get_llvm_function())};
      auto blockPass{create_block(llvm_twine(name, ".pass"))};
      auto blockFail{create_block(llvm_twine(name, ".fail"))};
      emit_br(cond, blockPass, blockFail);
      emit_scope(blockPass, blockFail, std::forward<decltype(func)>(func));
      llvm_move_block_to_end(blockFail);
      move_to(blockFail);
      emit_unwind(lastBreadcrumb);
    }
  }

  Value emit_phi(
      Type *type, llvm::ArrayRef<std::pair<llvm::Value *, llvm::BasicBlock *>> inputs, Value::Kind kind = Value::Kind::RValue);

  Value emit_op(AST::UnaryOp op, Value val, const AST::SourceLocation &srcLoc = {});

  Value emit_op(AST::BinaryOp op, Value lhs, Value rhs, const AST::SourceLocation &srcLoc = {});

  Value emit_intrinsic(llvm::StringRef name, const ArgList &args, const AST::SourceLocation &srcLoc = {});

  Value emit_intrinsic(const AST::Intrinsic &intr, const ArgList &args) {
    return emit_intrinsic(intr.srcName.drop_front(1), args, intr.srcLoc);
  }

  Value emit_call(Value value, const ArgList &args, const AST::SourceLocation &srcLoc = {});

  Value emit_visit(Value value, const std::function<Value(Emitter &, Value)> &visitor);

  void emit_return(Value value, const AST::SourceLocation &srcLoc = {});

  Value emit_final_return_phi(Type *type, llvm::ArrayRef<Return> returns, const AST::SourceLocation &srcLoc = {});

  Type *emit_final_return(Type *type, llvm::ArrayRef<Return> returns, const AST::SourceLocation &srcLoc = {});

  void emit_print(Value os, Value value, bool quoteStrings = false);

  void emit_panic(Value message, const AST::SourceLocation &srcLoc);

  void emit_panic(llvm::StringRef message, const AST::SourceLocation &srcLoc) {
    emit_panic(context.get_compile_time_string(message), srcLoc);
  }

  template <typename... Ts> Value emit_type_switch(auto &node) {
    return llvm::TypeSwitch<decltype(&node), Value>(&node)
        .template Case<Ts...>([&](auto *each) { return emit(*each); })
        .Default([&](auto *each) {
          each->srcLoc.report_error("unimplemented");
          return Value();
        });
  }

public:
  /// The context.
  Context &context;

  /// The Intermediate Representation (IR) builder.
  llvm::IRBuilder<> builder;

  Stacked<Semantics> semantics{};
};

} // namespace smdl::Compiler
