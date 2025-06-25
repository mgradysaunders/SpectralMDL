/// \file
#pragma once

#include "smdl/AST/Decl.h"

namespace smdl::AST {

/// A late if condition attached to some statements.
class SMDL_EXPORT LateIf final {
public:
  explicit LateIf(std::string_view srcKwIf, BumpPtr<Expr> expr)
      : srcKwIf(srcKwIf), expr(std::move(expr)) {}

  /// The keyword `if`.
  std::string_view srcKwIf{};

  /// The condition expression.
  BumpPtr<Expr> expr{};
};

/// A `break` statement.
class SMDL_EXPORT Break final : public StmtSubclass<StmtKind::Break> {
public:
  explicit Break(std::string_view srcKwBreak, std::optional<LateIf> lateIf,
                 std::string_view srcSemicolon)
      : srcKwBreak(srcKwBreak), lateIf(std::move(lateIf)),
        srcSemicolon(srcSemicolon) {}

  /// The keyword `break`.
  std::string_view srcKwBreak{};

  /// The late if.
  std::optional<LateIf> lateIf{};

  /// The semicolon `;`.
  std::string_view srcSemicolon{};
};

/// A compound statement (between braces `{ ... }`).
class SMDL_EXPORT Compound final : public StmtSubclass<StmtKind::Compound> {
public:
  explicit Compound(std::string_view srcBraceL,
                    std::vector<BumpPtr<Stmt>> stmts,
                    std::string_view srcBraceR)
      : srcBraceL(srcBraceL), stmts(std::move(stmts)), srcBraceR(srcBraceR) {}

  /// The brace `{`.
  std::string_view srcBraceL{};

  /// The statements.
  std::vector<BumpPtr<Stmt>> stmts{};

  /// The brace `}`.
  std::string_view srcBraceR{};
};

/// A `continue` statement.
class SMDL_EXPORT Continue final : public StmtSubclass<StmtKind::Continue> {
public:
  explicit Continue(std::string_view srcKwContinue,
                    std::optional<LateIf> lateIf, std::string_view srcSemicolon)
      : srcKwContinue(srcKwContinue), lateIf(std::move(lateIf)),
        srcSemicolon(srcSemicolon) {}

  /// The keyword `continue`.
  std::string_view srcKwContinue{};

  /// The late if.
  std::optional<LateIf> lateIf{};

  /// The semicolon `;`.
  std::string_view srcSemicolon{};
};

/// A declaration as a statement.
class SMDL_EXPORT DeclStmt final : public StmtSubclass<StmtKind::DeclStmt> {
public:
  explicit DeclStmt(BumpPtr<Decl> decl) : decl(std::move(decl)) {}

  /// The declaration.
  BumpPtr<Decl> decl{};
};

/// A `defer` statement.
class SMDL_EXPORT Defer final : public StmtSubclass<StmtKind::Defer> {
public:
  explicit Defer(std::string_view srcKwDefer, BumpPtr<Stmt> stmt)
      : srcKwDefer(srcKwDefer), stmt(std::move(stmt)) {}

  /// The keyword `defer`.
  std::string_view srcKwDefer{};

  /// The statement.
  BumpPtr<Stmt> stmt{};
};

/// A `do ... while (...)` statement.
class SMDL_EXPORT DoWhile final : public StmtSubclass<StmtKind::DoWhile> {
public:
  explicit DoWhile(std::string_view srcKwDo, BumpPtr<Stmt> stmt,
                   std::string_view srcKwWhile, BumpPtr<Expr> expr,
                   std::string_view srcSemicolon)
      : srcKwDo(srcKwDo), stmt(std::move(stmt)), srcKwWhile(srcKwWhile),
        expr(std::move(expr)), srcSemicolon(srcSemicolon) {}

  /// The keyword `do`.
  std::string_view srcKwDo{};

  /// The body statement.
  BumpPtr<Stmt> stmt{};

  /// The keyword `while`.
  std::string_view srcKwWhile{};

  /// The condition expression.
  BumpPtr<Expr> expr{};

  /// The semicolon `;`.
  std::string_view srcSemicolon{};
};

/// An expression as a statement.
class SMDL_EXPORT ExprStmt final : public StmtSubclass<StmtKind::ExprStmt> {
public:
  ExprStmt() = default; // Empty statement

  explicit ExprStmt(BumpPtr<Expr> expr, std::optional<LateIf> lateIf,
                    std::string_view srcSemicolon)
      : expr(std::move(expr)), lateIf(std::move(lateIf)),
        srcSemicolon(srcSemicolon) {}

  /// The expression.
  BumpPtr<Expr> expr{};

  /// The late if.
  std::optional<LateIf> lateIf{};

  /// The semicolon `;`.
  std::string_view srcSemicolon{};
};

/// A `for` statement.
class SMDL_EXPORT For final : public StmtSubclass<StmtKind::For> {
public:
  explicit For(std::string_view srcKwFor, std::string_view srcParenL,
               BumpPtr<Stmt> stmtInit, BumpPtr<Expr> exprCond,
               std::string_view srcSemicolonAfterCond, BumpPtr<Expr> exprNext,
               std::string_view srcParenR, BumpPtr<Stmt> stmtLoop)
      : srcKwFor(srcKwFor), srcParenL(srcParenL), stmtInit(std::move(stmtInit)),
        exprCond(std::move(exprCond)),
        srcSemicolonAfterCond(srcSemicolonAfterCond),
        exprNext(std::move(exprNext)), srcParenR(srcParenR),
        stmtLoop(std::move(stmtLoop)) {}

  /// The keyword `for`.
  std::string_view srcKwFor{};

  /// The parenthesis `(`.
  std::string_view srcParenL{};

  /// The initializer.
  BumpPtr<Stmt> stmtInit{};

  /// The condition expression.
  BumpPtr<Expr> exprCond{};

  /// The semicolon `;` after the condition expression.
  std::string_view srcSemicolonAfterCond{};

  /// The next expression.
  BumpPtr<Expr> exprNext{};

  /// The parenthesis `)`.
  std::string_view srcParenR{};

  /// The body statement.
  BumpPtr<Stmt> stmtLoop{};
};

/// An `if` statement.
class SMDL_EXPORT If final : public StmtSubclass<StmtKind::If> {
public:
  explicit If(std::string_view srcKwIf, BumpPtr<Expr> expr,
              BumpPtr<Stmt> stmtThen, std::string_view srcKwElse,
              BumpPtr<Stmt> stmtElse)
      : srcKwIf(srcKwIf), expr(std::move(expr)), stmtThen(std::move(stmtThen)),
        srcKwElse(srcKwElse), stmtElse(std::move(stmtElse)) {}

  /// The keyword `if`.
  std::string_view srcKwIf{};

  /// The condition expression.
  BumpPtr<Expr> expr{};

  /// The then statement.
  BumpPtr<Stmt> stmtThen{};

  /// The keyword `else` (may be empty).
  std::string_view srcKwElse{};

  /// The else statement (may be null).
  BumpPtr<Stmt> stmtElse{};
};

/// A `preserve` statement.
class SMDL_EXPORT Preserve final : public StmtSubclass<StmtKind::Preserve> {
public:
  class ExprWrapper final {
  public:
    /// The expression.
    BumpPtr<Expr> expr{};

    /// The next comma `,`. This may be empty!
    std::string_view srcComma{};
  };

  explicit Preserve(std::string_view srcKwPreserve,
                    std::vector<ExprWrapper> exprWrappers,
                    std::string_view srcSemicolon)
      : srcKwPreserve(srcKwPreserve), exprWrappers(std::move(exprWrappers)),
        srcSemicolon(srcSemicolon) {}

  /// The keyword `preserve`.
  std::string_view srcKwPreserve{};

  /// The expression wrappers.
  std::vector<ExprWrapper> exprWrappers{};

  /// The semicolon `;`.
  std::string_view srcSemicolon{};

  /// Has comma `,` after the last expression?
  [[nodiscard]] bool has_trailing_comma() const {
    return !exprWrappers.empty() && !exprWrappers.back().srcComma.empty();
  }
};

/// A `return` statement.
class SMDL_EXPORT Return final : public StmtSubclass<StmtKind::Return> {
public:
  explicit Return(std::string_view srcKwReturn, BumpPtr<Expr> expr,
                  std::optional<LateIf> lateIf, std::string_view srcSemicolon)
      : srcKwReturn(srcKwReturn), expr(std::move(expr)),
        lateIf(std::move(lateIf)), srcSemicolon(srcSemicolon) {}

  /// The keyword `return`.
  std::string_view srcKwReturn{};

  /// The expression.
  BumpPtr<Expr> expr{};

  /// The late if.
  std::optional<LateIf> lateIf{};

  /// The semicolon `;`.
  std::string_view srcSemicolon{};
};

/// A `switch` statement.
class SMDL_EXPORT Switch final : public StmtSubclass<StmtKind::Switch> {
public:
  struct Case final {
    /// Is default?
    [[nodiscard]] bool is_default() const { return !expr; }

    /// The keyword `case` or `default`.
    std::string_view srcKwCaseOrDefault{};

    /// The expression. This may be null! (if `default`)
    BumpPtr<Expr> expr{};

    /// The colon `:`.
    std::string_view srcColon{};

    /// The statements.
    std::vector<BumpPtr<Stmt>> stmts{};
  };

  explicit Switch(std::string_view srcKwSwitch, BumpPtr<Expr> expr,
                  std::string_view srcBraceL, std::vector<Case> cases,
                  std::string_view srcBraceR)
      : srcKwSwitch(srcKwSwitch), expr(std::move(expr)), srcBraceL(srcBraceL),
        cases(std::move(cases)), srcBraceR(srcBraceR) {}

  /// The keyword `switch`.
  std::string_view srcKwSwitch{};

  /// The expression.
  BumpPtr<Expr> expr{};

  /// The brace `{`.
  std::string_view srcBraceL{};

  /// The cases.
  std::vector<Case> cases{};

  /// The brace `}`.
  std::string_view srcBraceR{};
};

/// An `unreachable` statement.
class SMDL_EXPORT Unreachable final
    : public StmtSubclass<StmtKind::Unreachable> {
public:
  explicit Unreachable(std::string_view srcKwUnreachable,
                       std::string_view srcSemicolon)
      : srcKwUnreachable(srcKwUnreachable), srcSemicolon(srcSemicolon) {}

  /// The keyword `unreachable`.
  std::string_view srcKwUnreachable{};

  /// The semicolon `;`.
  std::string_view srcSemicolon{};
};

/// A `visit` statement.
class SMDL_EXPORT Visit final : public StmtSubclass<StmtKind::Visit> {
public:
  explicit Visit(std::string_view srcKwVisit, Name name,
                 std::string_view srcKwIn, BumpPtr<Expr> expr,
                 BumpPtr<Stmt> stmt)
      : srcKwVisit(srcKwVisit), name(std::move(name)), srcKwIn(srcKwIn),
        expr(std::move(expr)), stmt(std::move(stmt)) {}

  /// The keyword `visit`.
  std::string_view srcKwVisit{};

  /// The name.
  Name name{};

  /// The keyword `in`.
  std::string_view srcKwIn{};

  /// The expression.
  BumpPtr<Expr> expr{};

  /// The body statement.
  BumpPtr<Stmt> stmt{};
};

/// A `while` statement.
class SMDL_EXPORT While final : public StmtSubclass<StmtKind::While> {
public:
  explicit While(std::string_view srcKwWhile, BumpPtr<Expr> expr,
                 BumpPtr<Stmt> stmt)
      : srcKwWhile(srcKwWhile), expr(std::move(expr)), stmt(std::move(stmt)) {}

  /// The keyword `while`.
  std::string_view srcKwWhile{};

  /// The condition expression.
  BumpPtr<Expr> expr{};

  /// The body statement.
  BumpPtr<Stmt> stmt{};
};

} // namespace smdl::AST
