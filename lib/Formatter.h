#pragma once

#include "AST.h"

namespace smdl {

class Formatter final {
public:
  Formatter(llvm::StringRef src) : src(src) {}

  struct SepSpace {};

  static constexpr auto SEP_SPACE = SepSpace{};

  void write(SepSpace);

  void write(char ch);

  void write(llvm::StringRef str);

  void write_literal_string(llvm::StringRef str);

  void write_separated(const auto &values, const char *delim) {
    for (size_t i = 0; i < values.size(); i++) {
      write(values[i]);
      if (i + 1 < values.size())
        write(delim);
    }
  }

  void write_as_compound(AST::Node &node);

  void guarantee_newlines(size_t n);

  void try_to_preserve_comments(llvm::StringRef src0, llvm::StringRef src1);

  void try_to_preserve_comments(llvm::StringRef s);

  template <typename Func> void increment_indent(int level, Func &&func) {
    int prevIndent = indent;
    indent += level;
    std::invoke(std::forward<Func>(func));
    indent = prevIndent;
  }

public:
  template <typename T, typename Deleter> auto write(const std::unique_ptr<T, Deleter> &ptr) {
    write(*sanity_check_nonnull(ptr.get()));
  }

  template <typename T> void write(const std::optional<T> &value) {
    if (value)
      write(*value);
  }

  template <typename T, typename... Ts> void write(T &&arg, Ts &&...args) requires(sizeof...(Ts) > 0) {
    write(std::forward<T>(arg));
    write(std::forward<Ts>(args)...);
  }

  void write(AST::Node &node);

  void write(AST::File &file);

  //--{ Write: Decls
  void write(AST::Decl &decl);

  void write(AST::Enum &decl);

  void write(AST::Function &decl);

  void write(AST::Import &decl);

  void write(AST::Struct &decl);

  void write(AST::Tag &decl) { write(decl.srcKwTag, SEP_SPACE, decl.name, decl.srcSemicolon); }

  void write(AST::Typedef &decl) { write(decl.srcKwTypedef, SEP_SPACE, decl.type, SEP_SPACE, decl.name, decl.srcSemicolon); }

  void write(AST::UnitTest &decl) { write(decl.srcKwUnitTest, SEP_SPACE, decl.name, SEP_SPACE, decl.body); }

  void write(AST::UsingAlias &decl) {
    write(decl.srcKwUsing, SEP_SPACE, decl.name, SEP_SPACE, decl.srcEq, SEP_SPACE, decl.path, decl.srcSemicolon);
  }

  void write(AST::UsingImport &decl);

  void write(AST::Variable &decl);
  //--}

  //--{ Write: Exprs
  void write(AST::Expr &expr);

  void write(AST::Binary &expr) {
    if (expr.op == AST::BinaryOp::Comma) {
      write(expr.lhs, AST::to_string(expr.op), SEP_SPACE, expr.rhs);
    } else {
      write(expr.lhs, SEP_SPACE, AST::to_string(expr.op), SEP_SPACE, expr.rhs);
    }
  }

  void write(AST::Call &expr) { write(expr.expr, expr.args); }

  void write(AST::Cast &expr) { write("cast<", expr.type, ">(", expr.expr, ')'); }

  void write(AST::Conditional &expr) {
    write(expr.cond, SEP_SPACE, expr.srcQuestion, SEP_SPACE, expr.ifPass, SEP_SPACE, expr.srcColon, SEP_SPACE, expr.ifFail);
  }

  void write(AST::GetField &expr) { write(expr.expr, expr.srcDot, expr.name); }

  void write(AST::GetIndex &expr);

  void write(AST::Identifier &expr);

  void write(AST::Intrinsic &expr) { write('#', expr.name); }

  void write(AST::Name &expr) { write(expr.srcName); }

  void write(AST::Let &expr);

  void write(AST::LiteralBool &expr) { write(expr.src); }

  void write(AST::LiteralFloat &expr) { write(expr.src); }

  void write(AST::LiteralInt &expr) { write(expr.src); }

  void write(AST::LiteralString &expr) { write(expr.src); }

  void write(AST::Parens &expr) { write(expr.srcDollar, expr.srcParenL, expr.expr, expr.srcParenR); }

  void write(AST::ReturnFrom &expr) { write(expr.srcKwReturnFrom, SEP_SPACE, expr.stmt); }

  void write(AST::SizeName &expr) {
    if (expr.name)
      write('<', expr.name, '>');
  }

  void write(AST::Type &expr);

  void write(AST::Unary &expr) {
    if ((expr.op & AST::UnaryOp::Postfix) == AST::UnaryOp::Postfix)
      write(expr.expr, AST::to_string(expr.op));
    else
      write(AST::to_string(expr.op), expr.expr);
  }
  //--}

  //--{ Write: Stmts
  void write(AST::Stmt &stmt);

  void write(AST::Break &stmt) { write(stmt.srcKwBreak, stmt.lateIf, stmt.srcSemicolon); }

  void write(AST::Compound &stmt);

  void write(AST::Continue &stmt) { write(stmt.srcKwContinue, stmt.lateIf, stmt.srcSemicolon); }

  void write(AST::DeclStmt &stmt) { write(stmt.decl); }

  void write(AST::Defer &stmt) { write(stmt.srcKwDefer, SEP_SPACE, stmt.stmt); }

  void write(AST::DoWhile &stmt) {
    write(stmt.srcKwDo, SEP_SPACE, stmt.body, SEP_SPACE, stmt.srcKwWhile, SEP_SPACE, stmt.cond, stmt.srcSemicolon);
  }

  void write(AST::ExprStmt &stmt) {
    if (stmt.expr)
      write(stmt.expr, stmt.lateIf);
    write(stmt.srcSemicolon);
  }

  void write(AST::For &stmt) {
    write(
        stmt.srcKwFor, SEP_SPACE, stmt.srcParenL, stmt.init, SEP_SPACE, stmt.cond, stmt.srcSemicolonAfterCond, SEP_SPACE,
        stmt.incr, stmt.srcParenR, SEP_SPACE, stmt.body);
  }

  void write(AST::If &stmt) {
    write(stmt.srcKwIf, SEP_SPACE, stmt.cond, SEP_SPACE, stmt.ifPass);
    if (stmt.ifFail)
      write(SEP_SPACE, stmt.srcKwElse, SEP_SPACE, stmt.ifFail);
  }

  void write(AST::Preserve &stmt);

  void write(AST::Return &stmt) {
    write(stmt.srcKwReturn);
    if (stmt.expr != nullptr)
      write(SEP_SPACE, stmt.expr);
    write(stmt.lateIf, stmt.srcSemicolon);
  }

  void write(AST::Switch &stmt);

  void write(AST::Unreachable &stmt) { write(stmt.srcKwUnreachable, stmt.srcSemicolon); }

  void write(AST::Visit &stmt) {
    write(stmt.srcKwVisit, SEP_SPACE, stmt.name, SEP_SPACE, stmt.srcKwIn, SEP_SPACE, stmt.expr, SEP_SPACE, stmt.body);
  }

  void write(AST::While &stmt) { write(stmt.srcKwWhile, SEP_SPACE, stmt.cond, SEP_SPACE, stmt.body); }
  //--}

  void write(AST::FrequencyQualifier freq) { write(freq == AST::FrequencyQualifier::Uniform ? "uniform" : "varying"); }

  void write(const AST::Annotation &annotation);

  void write(const AST::AnnotationBlock &annotations);

  void write(const AST::Arg &arg);

  void write(const AST::ArgList &args);

  void write(const AST::Param &param);

  void write(const AST::ParamList &params);

  void write(const AST::Enum::Declarator &declarator);

  bool write(const AST::Function::Attrs &attrs);

  void write(const AST::LateIf &lateIf) { write(SEP_SPACE, lateIf.srcKwIf, SEP_SPACE, lateIf.cond); }

  template <typename... Ts> void write_type_switch(auto &node) {
    llvm::TypeSwitch<decltype(&node), void>(&node).template Case<Ts...>([&]<typename T>(T *each) { write(*each); });
  }

  llvm::StringRef src{};

  int indent{};

  std::string str{};
};

} // namespace smdl
