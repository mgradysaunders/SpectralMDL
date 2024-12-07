// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#pragma once

#include "AST.h"

namespace smdl {

class Formatter final {
public:
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

public:
  template <typename T, typename Deleter> auto write(const std::unique_ptr<T, Deleter> &ptr) {
    write(*sanity_check_nonnull(ptr.get()));
  }

  template <typename T> void write(const std::optional<T> &value) {
    if (value)
      write(*value);
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

  void write(AST::Binary &expr);

  void write(AST::Call &expr);

  void write(AST::Cast &expr);

  void write(AST::Conditional &expr);

  void write(AST::GetField &expr);

  void write(AST::GetIndex &expr);

  void write(AST::Identifier &expr);

  void write(AST::Intrinsic &expr);

  void write(AST::Name &expr);

  void write(AST::Let &expr);

  void write(AST::LiteralBool &expr);

  void write(AST::LiteralFloat &expr);

  void write(AST::LiteralInt &expr);

  void write(AST::LiteralString &expr);

  void write(AST::Parens &expr);

  void write(AST::ReturnFrom &expr);

  void write(AST::SizeName &expr);

  void write(AST::Type &expr);

  void write(AST::Unary &expr);
  //--}

  //--{ Write: Stmts
  void write(AST::Stmt &stmt);

  void write(AST::Break &stmt);

  void write(AST::Compound &stmt);

  void write(AST::Continue &stmt);

  void write(AST::DeclStmt &stmt);

  void write(AST::Defer &stmt);

  void write(AST::DoWhile &stmt);

  void write(AST::ExprStmt &stmt);

  void write(AST::For &stmt);

  void write(AST::If &stmt);

  void write(AST::Preserve &stmt);

  void write(AST::Return &stmt);

  void write(AST::Switch &stmt);

  void write(AST::Unreachable &stmt);

  void write(AST::Visit &stmt);

  void write(AST::While &stmt);
  //--}

  void write(AST::FrequencyQualifier freq);

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

  template <typename Arg, typename... Args> void write(Arg &&arg, Args &&...args) requires(sizeof...(Args) > 0) {
    write(std::forward<Arg>(arg));
    write(std::forward<Args>(args)...);
  }

  template <typename... Ts> void write_type_switch(auto &node) {
    llvm::TypeSwitch<decltype(&node), void>(&node).template Case<Ts...>([&]<typename T>(T *each) { write(*each); });
  }

  int indentLevel{};

  std::string str{};
};

} // namespace smdl
