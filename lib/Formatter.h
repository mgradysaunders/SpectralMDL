#pragma once

#include "AST.h"

namespace smdl {

class Formatter final {
public:
  struct want_space final {};

  struct want_newline final {};

  explicit Formatter(llvm::StringRef src) : src(src) { srcPos = src.data(); }

  void write_char(char ch);

  void write_indent();

  void write_in_between(const char *newSrcPos);

  void write(AST::SourceRef srcRef);

  void write(want_space) { wantSpace = true; }

  void write(want_newline) { wantNewLine = true; }

  template <typename Func> void preserve_indent(Func &&func) {
    int prevIndent = indent;
    std::invoke(std::forward<Func>(func));
    indent = prevIndent;
  }

  template <typename Func> void increment_indent(int level, Func &&func) {
    int prevIndent = indent;
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
    int prevIndent = indent;
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

  void write(AST::Conditional &expr) {
    write(
        expr.cond, want_space(), expr.srcQuestion, want_space(), expr.ifPass, want_space(), expr.srcColon, want_space(),
        expr.ifFail);
  }

  void write(AST::GetField &expr) { write(expr.expr, expr.srcDot, expr.name); }

  void write(AST::GetIndex &expr) {
    write(expr.expr);
    for (auto &index : expr.indexes) {
      write(index.srcBrackL);
      if (index.expr != nullptr)
        write(index.expr);
      write(index.srcBrackR);
    }
  }

  void write(AST::Identifier &expr);

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

  void write(AST::Compound &stmt) {
    write(stmt.srcBraceL);
    increment_indent(2, [&]() {
      for (auto &subStmt : stmt.stmts)
        write(want_newline(), subStmt);
    });
    write(want_newline(), stmt.srcBraceR);
  }

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

  void write(AST::For &stmt) {
    write(
        stmt.srcKwFor, want_space(), stmt.srcParenL, stmt.init, want_space(), stmt.cond, stmt.srcSemicolonAfterCond,
        want_space(), stmt.incr, stmt.srcParenR, want_space(), stmt.body);
  }

  void write(AST::If &stmt) {
    if (stmt.ifFail) {
      write(stmt.srcKwIf, want_space(), stmt.cond, want_space(), stmt.ifPass, want_space(), stmt.srcKwElse, want_space(), stmt.ifFail);
    } else {
      write(stmt.srcKwIf, want_space(), stmt.cond, want_space(), stmt.ifPass);
    }
  }

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

  void write(const AST::AnnotationBlock &annotations) {
    write(want_space(), annotations.srcDoubleBrackL);
    for (auto &annotation : annotations.annotations) {
      write(annotation.identifier, annotation.args);
      if (!annotation.srcComma.empty())
        write(annotation.srcComma, want_space());
    }
    write(annotations.srcDoubleBrackR);
  }

  void write(const AST::Arg &arg);

  void write(const AST::ArgList &args);

  void write(const AST::Param &param);

  void write(const AST::ParamList &params);

  void write(const AST::LateIf &lateIf) { write(want_space(), lateIf.srcKwIf, want_space(), lateIf.cond); }

  void write(const AST::Name &name) { write(name.srcName); }

  template <typename... Ts> void write_type_switch(auto &node) {
    llvm::TypeSwitch<decltype(&node), void>(&node).template Case<Ts...>([&]<typename T>(T *each) { write(*each); });
  }

  std::string str{};

private:
  llvm::StringRef src{};

  const char *srcPos{};

  bool wantSpace{};

  bool wantNewLine{true};

  int lineNo{1};

  int charNo{1};

  int maxCharNo{1};

  int indent{};
};

} // namespace smdl
