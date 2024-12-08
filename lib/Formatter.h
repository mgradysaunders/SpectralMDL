#pragma once

#include "AST.h"

namespace smdl {

class Formatter final {
public:
  Formatter(llvm::StringRef src) : src(src) {}

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

  void write(AST::Tag &decl);

  void write(AST::Typedef &decl);

  void write(AST::UnitTest &decl);

  void write(AST::UsingAlias &decl);

  void write(AST::UsingImport &decl);

  void write(AST::Variable &decl);
  //--}

  //--{ Write: Exprs
  void write(AST::Expr &expr);

  void write(AST::Binary &expr) {
    if (expr.op == AST::BinaryOp::Comma) {
      write(expr.lhs, ", ", expr.rhs);
    } else {
      write(expr.lhs, ' ');
      try_to_preserve_comments(expr.lhs->src, expr.src);
      write(AST::to_string(expr.op), ' ');
      try_to_preserve_comments(expr.src, expr.rhs->src);
      write(expr.rhs);
    }
  }

  void write(AST::Call &expr) { 
    write(expr.expr);
    try_to_preserve_comments(expr.expr->src, expr.args.src);
    write('(', expr.args, ')'); 
  }

  void write(AST::Cast &expr) { write("cast<", expr.type, ">(", expr.expr, ')'); }

  void write(AST::Conditional &expr) { write(expr.cond, " ? ", expr.ifPass, " : ", expr.ifFail); }

  void write(AST::GetField &expr) { write(expr.what, '.', expr.name); }

  void write(AST::GetIndex &expr);

  void write(AST::Identifier &expr);

  void write(AST::Intrinsic &expr) { write('#', expr.name); }

  void write(AST::Name &expr) { write(expr.name); }

  void write(AST::Let &expr);

  void write(AST::LiteralBool &expr) { write(expr.src); }

  void write(AST::LiteralFloat &expr) { write(expr.src); }

  void write(AST::LiteralInt &expr) { write(expr.src); }

  void write(AST::LiteralString &expr) { write(expr.src); }

  void write(AST::Parens &expr);

  void write(AST::ReturnFrom &expr) { write("return_from ", expr.stmt); }

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

  void write(AST::Break &stmt) {
    write("break");
    if (stmt.cond != nullptr)
      write(" if ", stmt.cond);
    write(';');
  }

  void write(AST::Compound &stmt);

  void write(AST::Continue &stmt) {
    write("continue");
    if (stmt.cond != nullptr)
      write(" if ", stmt.cond);
    write(';');
  }

  void write(AST::DeclStmt &stmt) { write(stmt.decl); }

  void write(AST::Defer &stmt);

  void write(AST::DoWhile &stmt);

  void write(AST::ExprStmt &stmt);

  void write(AST::For &stmt);

  void write(AST::If &stmt);

  void write(AST::Preserve &stmt);

  void write(AST::Return &stmt) {
    write("return");
    if (stmt.expr != nullptr)
      write(' ', stmt.expr);
    if (stmt.cond != nullptr)
      write(" if ", stmt.cond);
    write(';');
  }

  void write(AST::Switch &stmt);

  void write(AST::Unreachable &) { write("unreachable"); }

  void write(AST::Visit &stmt);

  void write(AST::While &stmt);
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

  void write(const AST::Struct::Field &field);

  void write(const AST::Struct::Tag &tag);

  void write(const AST::Variable::Declarator &declarator);

  template <typename... Ts> void write_type_switch(auto &node) {
    llvm::TypeSwitch<decltype(&node), void>(&node).template Case<Ts...>([&]<typename T>(T *each) { write(*each); });
  }

  llvm::StringRef src{};

  int indent{};

  std::string str{};
};

} // namespace smdl
