// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#pragma once

#include "AST.h"

namespace smdl {

class Formatter final {
public:
  struct want_space final {};

  struct want_newline final {};

  explicit Formatter(llvm::StringRef inSrc) : inSrc(inSrc), inSrcPos(inSrc.data()) {}

  void write_indent_if_necessary();

  void write_char(char ch);

  void write_in_between(const char *inSrcPos0);

  void write(AST::SourceRef src);

  void write(want_space) { wantSpace = true; }

  void write(want_newline) { wantNewLine = true; }

  template <typename Func> void adjust_indent(size_t level, Func &&func) {
    size_t prevIndent = indent;
    indent += level;
    std::invoke(std::forward<Func>(func));
    indent = prevIndent;
  }

public:
  template <typename T, typename Deleter> auto write(const std::unique_ptr<T, Deleter> &ptr) {
    if (ptr)
      write(*ptr);
  }

  template <typename T> void write(const std::optional<T> &value) {
    if (value)
      write(*value);
  }

  template <typename T, typename... Ts> void write(T &&arg, Ts &&...args) requires(sizeof...(Ts) > 0) {
    size_t prevIndent = indent;
    write(std::forward<T>(arg));
    (write(std::forward<Ts>(args)), ...);
    indent = prevIndent;
  }

  void write(AST::Node &node);

  void write(AST::File &file);

  //--{ Write: Decls
  void write(AST::Decl &decl);

  void write(AST::Enum &decl);

  void write(AST::Function &decl);

  void write(AST::Import &decl);

  void write(AST::Struct &decl);

  void write(AST::Tag &decl) { write(decl.srcKwTag, want_space(), decl.name, decl.srcSemicolon); }

  void write(AST::Typedef &decl) {
    write(decl.srcKwTypedef, want_space(), decl.type, want_space(), decl.name, decl.srcSemicolon);
  }

  void write(AST::UnitTest &decl) { write(decl.srcKwUnitTest, want_space(), decl.name, want_space(), decl.body); }

  void write(AST::UsingAlias &decl) {
    write(decl.srcKwUsing, want_space(), decl.name, want_space(), decl.srcEq, want_space(), decl.path, decl.srcSemicolon);
  }

  void write(AST::UsingImport &decl);

  void write(AST::Variable &decl);
  //--}

  //--{ Write: Exprs
  void write(AST::Expr &expr);

  void write(AST::Binary &expr) {
    if (expr.op == AST::BinaryOp::Comma) {
      write(expr.lhs, expr.srcOp, want_space(), expr.rhs);
    } else {
      write(expr.lhs, want_space(), expr.srcOp, want_space(), expr.rhs);
    }
  }

  void write(AST::Call &expr) { write(expr.expr, expr.args); }

  void write(AST::Cast &expr) { write(expr.srcKwCast, expr.srcAngleL, expr.type, expr.srcAngleR, expr.expr); }

  void write(AST::Conditional &expr);

  void write(AST::GetField &expr) { write(expr.expr, expr.srcDot, expr.name); }

  void write(AST::GetIndex &expr);

  void write(AST::Identifier &expr) {
    for (auto &name : expr.names)
      write(name.srcDoubleColon, name.name, name.literalString);
  }

  void write(AST::Intrinsic &expr) { write(expr.srcName); }

  void write(AST::Let &expr);

  void write(AST::LiteralBool &expr) { write(expr.srcValue); }

  void write(AST::LiteralFloat &expr) { write(expr.srcValue); }

  void write(AST::LiteralInt &expr) { write(expr.srcValue); }

  void write(AST::LiteralString &expr) { write(expr.srcValue); }

  void write(AST::Parens &expr) { write(expr.srcDollar, expr.srcParenL, expr.expr, expr.srcParenR); }

  void write(AST::ReturnFrom &expr) { write(expr.srcKwReturnFrom, want_space(), expr.stmt); }

  void write(AST::SizeName &expr) { write(expr.srcAngleL, expr.name, expr.srcAngleR); }

  void write(AST::Type &expr);

  void write(AST::Unary &expr) {
    if (expr.is_postfix())
      write(expr.expr, expr.srcOp);
    else
      write(expr.srcOp, expr.expr);
  }
  //--}

  //--{ Write: Stmts
  void write(AST::Stmt &stmt);

  void write(AST::Break &stmt) { write(stmt.srcKwBreak, stmt.lateIf, stmt.srcSemicolon); }

  void write(AST::Compound &stmt);

  void write(AST::Continue &stmt) { write(stmt.srcKwContinue, stmt.lateIf, stmt.srcSemicolon); }

  void write(AST::DeclStmt &stmt) { write(stmt.decl); }

  void write(AST::Defer &stmt) { write(stmt.srcKwDefer, want_space(), stmt.stmt); }

  void write(AST::DoWhile &stmt) {
    write(stmt.srcKwDo, want_space(), stmt.body, want_space(), stmt.srcKwWhile, want_space(), stmt.cond, stmt.srcSemicolon);
  }

  void write(AST::ExprStmt &stmt) {
    if (stmt.expr)
      write(stmt.expr, stmt.lateIf);
    write(stmt.srcSemicolon);
  }

  void write(AST::For &stmt);

  void write(AST::If &stmt);

  void write(AST::Preserve &stmt);

  void write(AST::Return &stmt) {
    write(stmt.srcKwReturn);
    if (stmt.expr != nullptr)
      write(want_space(), stmt.expr);
    write(stmt.lateIf, stmt.srcSemicolon);
  }

  void write(AST::Switch &stmt);

  void write(AST::Unreachable &stmt) { write(stmt.srcKwUnreachable, stmt.srcSemicolon); }

  void write(AST::Visit &stmt) {
    write(
        stmt.srcKwVisit, want_space(), stmt.name, want_space(), stmt.srcKwIn, want_space(), stmt.expr, want_space(), stmt.body);
  }

  void write(AST::While &stmt) { write(stmt.srcKwWhile, want_space(), stmt.cond, want_space(), stmt.body); }
  //--}

  void write(const AST::AnnotationBlock &annotations);

  void write(const AST::Arg &arg);

  void write(const AST::ArgList &args);

  void write(const AST::Param &param);

  void write(const AST::ParamList &params);

  void write(const AST::LateIf &lateIf) { write(want_space(), lateIf.srcKwIf, want_space(), lateIf.cond); }

  void write(const AST::Name &name) { write(name.srcName); }

  template <typename... Ts> void write_type_switch(auto &node) {
    llvm::TypeSwitch<decltype(&node), void>(&node).template Case<Ts...>([&]<typename T>(T *each) { write(*each); });
  }

  std::string outSrc{};

private:
  llvm::StringRef inSrc{};

  const char *inSrcPos{};

  bool wantSpace{};

  bool wantNewLine{true};

  size_t lineNo{1};

  size_t charNo{1};

  size_t indent{};

  struct FormatOffLocation final {
    const char *inSrcPos{};

    size_t outSrcPos{};
  };
  
  std::optional<FormatOffLocation> formatOff{};
};

} // namespace smdl
