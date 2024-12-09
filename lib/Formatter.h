#pragma once

#include "AST.h"

namespace smdl {

class Formatter final {
public:
  Formatter(llvm::StringRef src) : src(src) {}

  void write(char ch);

  void write(llvm::StringRef str);

  struct guarantee_space final {};

  struct guarantee_newline final {};

  void write(guarantee_space);

  void write(guarantee_newline);

  void write_separated(const auto &values, const char *delim) {
    for (size_t i = 0; i < values.size(); i++) {
      write(values[i]);
      if (i + 1 < values.size())
        write(delim);
    }
  }

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

  void write(AST::Enum &decl) {
    write(decl.srcKwEnum, guarantee_space(), decl.name, decl.annotations, guarantee_space(), decl.srcBraceL);
    increment_indent(1, [&]() {
      for (auto &declarator : decl.declarators) {
        write(guarantee_newline(), declarator.name);
        if (declarator.init)
          write(guarantee_space(), declarator.srcEq, guarantee_space(), declarator.init);
        write(declarator.annotations, declarator.srcComma);
      }
    });
    write(guarantee_newline(), decl.srcBraceR, decl.srcSemicolon);
  }

  void write(AST::Function &decl);

  void write(AST::Import &decl) {
    write(decl.srcKwImport);
    for (auto &path : decl.paths)
      write(guarantee_space(), path.identifier, path.srcComma);
    write(decl.srcSemicolon);
  }

  void write(AST::Struct &decl) {
    write(decl.srcKwStruct, guarantee_space(), decl.name);
    if (!decl.tags.empty()) {
      write(decl.srcColonBeforeTags);
      for (auto &tag : decl.tags)
        write(guarantee_space(), tag.srcKwDefault, guarantee_space(), tag.type, tag.srcComma);
    }
    if (decl.fields.empty()) {
      write(guarantee_space(), decl.srcBraceL, decl.srcBraceR, decl.srcSemicolon);
    } else {
      write(guarantee_space(), decl.srcBraceL);
      increment_indent(1, [&]() {
        for (auto &field : decl.fields) {
          write(guarantee_newline(), field.srcKwVoid, guarantee_space(), field.type, guarantee_space(), field.name);
          if (field.init)
            write(guarantee_space(), field.srcEq, guarantee_space(), field.init);
          write(field.annotations, field.srcSemicolon);
        }
      });
      write(guarantee_newline(), decl.srcBraceR, decl.srcSemicolon);
    }
  }

  void write(AST::Tag &decl) { write(decl.srcKwTag, guarantee_space(), decl.name, decl.srcSemicolon); }

  void write(AST::Typedef &decl) {
    write(decl.srcKwTypedef, guarantee_space(), decl.type, guarantee_space(), decl.name, decl.srcSemicolon);
  }

  void write(AST::UnitTest &decl) { write(decl.srcKwUnitTest, guarantee_space(), decl.name, guarantee_space(), decl.body); }

  void write(AST::UsingAlias &decl) {
    write(
        decl.srcKwUsing, guarantee_space(), decl.name, guarantee_space(), decl.srcEq, guarantee_space(), decl.path,
        decl.srcSemicolon);
  }

  void write(AST::UsingImport &decl) {
    write(
        decl.srcKwExport, guarantee_space(), decl.srcKwUsing, guarantee_space(), decl.path, guarantee_space(), decl.srcKwImport,
        guarantee_space());
    for (auto &name : decl.names) {
      write(name.srcName);
      if (!name.srcComma.empty())
        write(name.srcComma, guarantee_space());
    }
    write(decl.srcSemicolon);
  }

  void write(AST::Variable &decl) {
    write(decl.type);
    for (auto &declarator : decl.declarators) {
      write(guarantee_space(), declarator.name);
      if (declarator.init != nullptr)
        write(guarantee_space(), declarator.srcEq, guarantee_space(), declarator.init);
      else if (declarator.args)
        write(declarator.args);
      write(declarator.annotations, declarator.srcComma);
    }
    write(decl.srcSemicolon);
  }
  //--}

  //--{ Write: Exprs
  void write(AST::Expr &expr);

  void write(AST::Binary &expr) {
    if (expr.op == AST::BinaryOp::Comma) {
      write(expr.lhs, expr.srcOp, guarantee_space(), expr.rhs);
    } else {
      write(expr.lhs, guarantee_space(), expr.srcOp, guarantee_space(), expr.rhs);
    }
  }

  void write(AST::Call &expr) { write(expr.expr, expr.args); }

  void write(AST::Cast &expr) { write(expr.srcKwCast, expr.srcAngleL, expr.type, expr.srcAngleR, expr.expr); }

  void write(AST::Conditional &expr) {
    write(
        expr.cond, guarantee_space(), expr.srcQuestion, guarantee_space(), expr.ifPass, guarantee_space(), expr.srcColon,
        guarantee_space(), expr.ifFail);
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

  void write(AST::Intrinsic &expr) { write('#', expr.name); }

  void write(AST::Name &expr) { write(expr.srcName); }

  void write(AST::Let &expr) {
    write(expr.srcKwLet, guarantee_space());
    if (!expr.srcBraceL.empty()) {
      write(expr.srcBraceL);
      increment_indent(1, [&]() {
        for (auto &var : expr.vars)
          write(guarantee_newline(), var);
      });
      write(guarantee_newline(), expr.srcBraceR, guarantee_space(), expr.srcKwIn, guarantee_space(), expr.expr);
    } else {
      sanity_check(expr.vars.size() == 1);
      write(expr.vars[0], guarantee_space(), expr.srcKwIn, guarantee_space(), expr.expr);
    }
  }

  void write(AST::LiteralBool &expr) { write(expr.src); } // TODO

  void write(AST::LiteralFloat &expr) { write(expr.src); } // TODO

  void write(AST::LiteralInt &expr) { write(expr.src); } // TODO

  void write(AST::LiteralString &expr) { write(expr.src); } // TODO

  void write(AST::Parens &expr) { write(expr.srcDollar, expr.srcParenL, expr.expr, expr.srcParenR); }

  void write(AST::ReturnFrom &expr) { write(expr.srcKwReturnFrom, guarantee_space(), expr.stmt); }

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
    increment_indent(1, [&]() {
      for (auto &subStmt : stmt.stmts)
        write(guarantee_newline(), subStmt);
    });
    write(guarantee_newline(), stmt.srcBraceR);
  }

  void write(AST::Continue &stmt) { write(stmt.srcKwContinue, stmt.lateIf, stmt.srcSemicolon); }

  void write(AST::DeclStmt &stmt) { write(stmt.decl); }

  void write(AST::Defer &stmt) { write(stmt.srcKwDefer, guarantee_space(), stmt.stmt); }

  void write(AST::DoWhile &stmt) {
    write(
        stmt.srcKwDo, guarantee_space(), stmt.body, guarantee_space(), stmt.srcKwWhile, guarantee_space(), stmt.cond,
        stmt.srcSemicolon);
  }

  void write(AST::ExprStmt &stmt) {
    if (stmt.expr)
      write(stmt.expr, stmt.lateIf);
    write(stmt.srcSemicolon);
  }

  void write(AST::For &stmt) {
    write(
        stmt.srcKwFor, guarantee_space(), stmt.srcParenL, stmt.init, guarantee_space(), stmt.cond, stmt.srcSemicolonAfterCond,
        guarantee_space(), stmt.incr, stmt.srcParenR, guarantee_space(), stmt.body);
  }

  void write(AST::If &stmt) {
    write(stmt.srcKwIf, guarantee_space(), stmt.cond, guarantee_space(), stmt.ifPass);
    if (stmt.ifFail)
      write(guarantee_space(), stmt.srcKwElse, guarantee_space(), stmt.ifFail);
  }

  void write(AST::Preserve &stmt);

  void write(AST::Return &stmt) {
    write(stmt.srcKwReturn);
    if (stmt.expr != nullptr)
      write(guarantee_space(), stmt.expr);
    write(stmt.lateIf, stmt.srcSemicolon);
  }

  void write(AST::Switch &stmt) {
    write(stmt.srcKwSwitch, guarantee_space(), stmt.expr, guarantee_space(), stmt.srcBraceL);
    for (auto &switchCase : stmt.cases) {
      write(guarantee_newline(), switchCase.srcKwCaseOrDefault);
      if (!switchCase.is_default())
        write(guarantee_space(), switchCase.cond);
      write(switchCase.srcColon);
      increment_indent(1, [&]() {
        for (auto &subStmt : switchCase.stmts) {
          write(guarantee_newline(), subStmt);
        }
      });
    }
    write(guarantee_newline(), stmt.srcBraceR);
  }

  void write(AST::Unreachable &stmt) { write(stmt.srcKwUnreachable, stmt.srcSemicolon); }

  void write(AST::Visit &stmt) {
    write(
        stmt.srcKwVisit, guarantee_space(), stmt.name, guarantee_space(), stmt.srcKwIn, guarantee_space(), stmt.expr,
        guarantee_space(), stmt.body);
  }

  void write(AST::While &stmt) { write(stmt.srcKwWhile, guarantee_space(), stmt.cond, guarantee_space(), stmt.body); }
  //--}

  void write(AST::FrequencyQualifier freq) { write(freq == AST::FrequencyQualifier::Uniform ? "uniform" : "varying"); }

  void write(const AST::AnnotationBlock &annotations);

  void write(const AST::Arg &arg);

  void write(const AST::ArgList &args);

  void write(const AST::Param &param);

  void write(const AST::ParamList &params);

  bool write(const AST::Function::Attrs &attrs);

  void write(const AST::LateIf &lateIf) { write(guarantee_space(), lateIf.srcKwIf, guarantee_space(), lateIf.cond); }

  template <typename... Ts> void write_type_switch(auto &node) {
    llvm::TypeSwitch<decltype(&node), void>(&node).template Case<Ts...>([&]<typename T>(T *each) { write(*each); });
  }

  llvm::StringRef src{};

  int indent{};

  std::string str{};
};

} // namespace smdl
