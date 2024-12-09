// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#pragma once

#include "Context.h"

namespace smdl::Compiler {

class Emitter final {
public:
  class Label final {
  public:
    Crumb *crumb{};

    llvm::BasicBlock *block{};

    [[nodiscard]] operator bool() const { return block; }

    [[nodiscard]] operator llvm::BasicBlock *() const { return block; }
  };

  Emitter(
      Context &context, Module *module, Crumb *crumb, //
      llvm::SmallVector<Return> *returns = nullptr,   //
      llvm::SmallVector<Inline> *inlines = nullptr,   //
      llvm::Function *llvmFunc = nullptr)
      : context(context), module(module), crumb(crumb), returns(returns), inlines(inlines), builder(context.llvmContext) {
    auto fmf{llvm::FastMathFlags::getFast()};
    fmf.setNoNaNs(false); // Don't assume no NaNs!
    fmf.setNoInfs(false); // Don't assume no Infs!
    builder.setFastMathFlags(fmf);
    if (llvmFunc) {
      auto blockEntry{llvm::BasicBlock::Create(context.llvmContext, "entry", llvmFunc)};
      auto blockReturn{llvm::BasicBlock::Create(context.llvmContext, "return", llvmFunc)};
      afterReturn = {crumb, blockReturn};
      builder.SetInsertPoint(blockEntry);
    }
  }

  Emitter(Emitter *parent)
      : parent(parent), context(parent->context), module(parent->module), crumb(parent->crumb), state(parent->state),
        returns(parent->returns), inlines(parent->inlines), afterBreak(parent->afterBreak),
        afterContinue(parent->afterContinue), afterReturn(parent->afterReturn), afterEndScope(parent->afterEndScope),
        builder(parent->context.llvmContext), macroRecursionDepth(parent->macroRecursionDepth) {
    auto fmf{llvm::FastMathFlags::getFast()};
    fmf.setNoNaNs(false); // Don't assume no NaNs!
    fmf.setNoInfs(false); // Don't assume no Infs!
    builder.setFastMathFlags(fmf);
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
    crumb = context.bump_allocate<Crumb>(Crumb{crumb, value, name, node, srcLoc, valueToPreserve});
  }

  void declare(const AST::Name &astName, Value value, AST::Decl *decl = {}) {
    // We assume 'astName' is a reference to a persistent bump-allocated name, so we can directly
    // construct an 'llvm::ArrayRef' without needing to 'context.bump_duplicate()'.
    push(value, astName.srcName, decl, astName.srcLoc);
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

  Value emit(AST::Node &node);

  Value emit(AST::File &file) {
    for (auto &decl : file.imports)
      emit(decl);
    for (auto &decl : file.globals)
      emit(decl);
    return {};
  }

  //--{ Emit: Decls
  Value emit(AST::Decl &decl);

  Value emit(AST::Enum &decl);

  Value emit(AST::Function &decl);

  Value emit(AST::Import &decl);

  Value emit(AST::Struct &decl);

  Value emit(AST::Tag &decl);

  Value emit(AST::Typedef &decl);

  Value emit(AST::UnitTest &decl);

  Value emit(AST::UsingAlias &decl);

  Value emit(AST::UsingImport &decl);

  Value emit(AST::Variable &decl);
  //--}

  //--{ Emit: Exprs
  Value emit(AST::Expr &expr);

  Value emit(AST::Binary &expr);

  Value emit(AST::Call &expr) { return emit_call(emit(expr.expr), emit(expr.args), expr.srcLoc); }

  Value emit(AST::Cast &expr) { return construct(emit(expr.type).get_compile_time_type(), emit(expr.expr), expr.srcLoc); }

  Value emit(AST::Conditional &expr);

  Value emit(AST::GetField &expr) { return access(emit(expr.expr), expr.name->srcName, expr.name->srcLoc); }

  Value emit(AST::GetIndex &expr);

  Value emit(AST::Identifier &expr) { return context.resolve(*this, expr); }

  Value emit(AST::Intrinsic &expr) { return context.get_compile_time_intrinsic(&expr); }

  Value emit(AST::Let &expr);

  Value emit(AST::LiteralBool &expr) { return context.get_compile_time_bool(expr.value); }

  Value emit(AST::LiteralFloat &expr) { return context.get_compile_time_FP(expr.value, expr.precision); }

  Value emit(AST::LiteralInt &expr) { return context.get_compile_time_int(expr.value); }

  Value emit(AST::LiteralString &expr) { return context.get_compile_time_string(expr.value); }

  Value emit(AST::Parens &expr);

  Value emit(AST::ReturnFrom &expr);

  Value emit(AST::Type &expr);

  Value emit(AST::Unary &expr) { return emit_op(expr.op, emit(expr.expr), expr.srcLoc); }
  //--}

  //--{ Emit: Stmts
  Value emit(AST::Stmt &stmt);

  Value emit(AST::Break &stmt) {
    if (!afterBreak)
      stmt.srcLoc.report_error("unexpected 'break'");
    emit_late_if(stmt.lateIf, [&](auto &emitter) { emitter.emit_unwind_and_br(afterBreak); });
    return {};
  }

  Value emit(AST::Compound &stmt);

  Value emit(AST::Continue &stmt) {
    if (!afterContinue)
      stmt.srcLoc.report_error("unexpected 'continue'");
    emit_late_if(stmt.lateIf, [&](auto &emitter) { emitter.emit_unwind_and_br(afterContinue); });
    return {};
  }

  Value emit(AST::DeclStmt &stmt) { return emit(stmt.decl); }

  Value emit(AST::Defer &stmt) {
    push({}, {}, &stmt, stmt.srcLoc);
    stmt.crumb = crumb;
    return {};
  }

  Value emit(AST::DoWhile &stmt);

  Value emit(AST::ExprStmt &stmt) {
    if (stmt.expr)
      emit_late_if(stmt.lateIf, [&](auto &emitter) { emitter.emit_scope([&](auto &emitter) { emitter.emit(stmt.expr); }); });
    return {};
  }

  Value emit(AST::For &stmt);

  Value emit(AST::If &stmt);

  Value emit(AST::Preserve &stmt);

  Value emit(AST::Return &stmt) {
    if (!afterReturn)
      stmt.srcLoc.report_error("unexpected 'return'");
    emit_late_if(stmt.lateIf, [&](auto &emitter) {
      emitter.emit_return(stmt.expr.get() ? emitter.emit(stmt.expr) : Value(), stmt.srcLoc);
    });
    return {};
  }

  Value emit(AST::Switch &stmt);

  Value emit(AST::Unreachable &stmt) {
    sanity_check(get_llvm_function());
    builder.CreateUnreachable();
    return {};
  }

  Value emit(AST::Visit &stmt) {
    emit_visit(emit(stmt.expr), [&](Emitter &emitter, Value value) -> Value {
      emitter.declare(*stmt.name, value);
      if (!value.is_void())
        emitter.emit(stmt.body);
      return {};
    });
    return {};
  }

  Value emit(AST::While &stmt);
  //--}

  ArgList emit(const AST::ArgList &astArgs);

public:
  [[nodiscard]] Value emit_alloca(const llvm::Twine &name, Type *type);

  void emit_end_lifetime(Value value);

  void emit_scope(llvm::BasicBlock *blockStart, llvm::BasicBlock *blockEnd, std::invocable<Emitter &> auto &&func) {
    Emitter emitter{this};
    emitter.move_to(blockStart);
    emitter.afterEndScope = {crumb, blockEnd};
    std::invoke(std::forward<decltype(func)>(func), emitter);
    emitter.emit_unwind_and_br(emitter.afterEndScope);
  }

  void emit_scope(std::invocable<Emitter &> auto &&func) { // "Thin scope"
    Crumb *crumb0{crumb};
    std::invoke(std::forward<decltype(func)>(func), *this);
    emit_unwind(crumb0);
  }

  Value emit_scope(AST::Expr &expr) {
    Value value{};
    emit_scope([&](Emitter &emitter) { value = emitter.emit(expr); });
    return value;
  }

  void emit_unwind_and_br(Label label);

  void emit_unwind(Crumb *crumb0);

  Value emit_cond(AST::Expr &expr, bool insideScope = true) {
    Value cond{};
    if (insideScope)
      emit_scope([&](Emitter &emitter) { cond = emitter.construct(context.get_bool_type(), emitter.emit(expr), expr.srcLoc); });
    else
      cond = construct(context.get_bool_type(), emit(expr), expr.srcLoc);
    return cond;
  }

  void emit_br_and_move_to(llvm::BasicBlock *block) { emit_br(block), move_to(block); }

  void emit_br(llvm::BasicBlock *block);

  void emit_br(Value cond, llvm::BasicBlock *blockPass, llvm::BasicBlock *blockFail);

  void emit_late_if(std::optional<AST::LateIf> &lateIf, std::invocable<Emitter &> auto &&func) {
    if (!lateIf) {
      std::invoke(func, *this);
    } else {
      auto crumb0{crumb};
      auto cond{emit_cond(*lateIf->cond, /*insideScope=*/false)};
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
      emit_unwind(crumb0);
    }
  }

  Value emit_phi(
      Type *type, llvm::ArrayRef<std::pair<llvm::Value *, llvm::BasicBlock *>> inputs, Value::Kind kind = Value::Kind::RValue);

  Value emit_op(AST::UnaryOp op, Value val, const AST::SourceLocation &srcLoc = {});

  Value emit_op(AST::BinaryOp op, Value lhs, Value rhs, const AST::SourceLocation &srcLoc = {});

  Value emit_logical_op(
      AST::BinaryOp op, const std::function<Value(Emitter &)> &lhs, const std::function<Value(Emitter &)> &rhs,
      const AST::SourceLocation &srcLoc = {});

  Value emit_intrinsic(llvm::StringRef name, const ArgList &args, const AST::SourceLocation &srcLoc = {});

  Value emit_intrinsic(const AST::Intrinsic &intr, const ArgList &args) { return emit_intrinsic(intr.name, args, intr.srcLoc); }

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

public:
  /// The parent emitter, if applicable.
  Emitter *const parent{};

  /// The context.
  Context &context;

  /// The associated module.
  Module *module{};

  /// The current crumb.
  Crumb *crumb{};

  /// The current state.
  Value state{};

  /// The pending returns.
  llvm::SmallVector<Return> *returns{};

  /// The explicit inlines via '#inline(...)' requests.
  llvm::SmallVector<Inline> *inlines{};

  /// Where to go after 'break' statement.
  Label afterBreak{};

  /// Where to go after 'continue' statement.
  Label afterContinue{};

  /// Where to go after 'return' statement.
  Label afterReturn{};

  /// Where to go after end-of-scope.
  Label afterEndScope{};

  /// The Intermediate Representation (IR) builder.
  llvm::IRBuilder<> builder;

  uint32_t macroRecursionDepth{};
};

} // namespace smdl::Compiler
