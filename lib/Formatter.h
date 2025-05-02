// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#pragma once

#include "smdl/AST.h"

namespace smdl {

/// \addtogroup Main
/// \{

class SMDL_EXPORT Formatter final {
public:
  Formatter() = default;

  [[nodiscard]] std::string format(std::string_view sourceCode,
                                   const AST::Node &node) {
    outputSrc.clear();
    inputSrc = sourceCode;
    inputSrcPos = sourceCode.data();
    wantSep = '\0';
    write(node);
    return outputSrc;
  }

private:
  void write(std::string_view inSrc);

  enum Command : int {
    ADD_SPACE,
    ADD_NEWLINE,
    BLOCK_BEGIN,
    BLOCK_END,
    ALIGN_BEGIN,
    ALIGN_END,
    BREAK_HERE
  };

  void write(Command command) {
    switch (command) {
    case ADD_SPACE:
      if (wantSep == '\0')
        wantSep = ' ';
      break;
    case ADD_NEWLINE:
      wantSep = '\n';
      break;
    case BLOCK_BEGIN:
      indent += 2;
      break;
    case BLOCK_END:
      indent -= 2;
      break;
    case ALIGN_BEGIN:
      wantAlign.push_back(line_width());
      if (wantSep == ' ' && last_output() != ' ')
        ++wantAlign.back();
      break;
    case ALIGN_END:
      wantAlign.pop_back();
      break;
    case BREAK_HERE:
      wantBreak = outputSrc.size();
      if (wantSep == ' ' && last_output() != ' ')
        ++*wantBreak;
      break;
    default:
      break;
    }
  }

  template <typename T> void write(const BumpPtr<T> &ptr) {
    if (ptr)
      write(*ptr);
  }

  template <typename T> void write(const std::optional<T> &opt) {
    if (opt)
      write(*opt);
  }

  template <typename T0, typename T1, typename... Ts>
  void write(T0 &&arg0, T1 &&arg1, Ts &&...args) {
    write(std::forward<T0>(arg0));
    write(std::forward<T1>(arg1));
    if constexpr (sizeof...(args) > 0)
      (write(std::forward<Ts>(args)), ...);
  }

  void write(const AST::Node &node);

  void write(const AST::File &file);

  //--{ Write: Decls
  void write(const AST::Decl &decl);

  void write(const AST::Enum &decl);

  void write(const AST::Function &decl);

  void write(const AST::Import &decl);

  void write(const AST::Struct &decl);

  void write(const AST::Tag &decl) {
    write(decl.srcKwTag, ADD_SPACE, decl.name, decl.srcSemicolon);
  }

  void write(const AST::Typedef &decl) {
    write(decl.srcKwTypedef, ADD_SPACE, decl.type, ADD_SPACE, decl.name,
          decl.srcSemicolon);
  }

  void write(const AST::UnitTest &decl) {
    write(decl.srcKwUnitTest, ADD_SPACE, decl.name, ADD_SPACE, decl.stmt);
  }

  void write(const AST::UsingAlias &decl);

  void write(const AST::UsingImport &decl);

  void write(const AST::Variable &decl);
  //--}

  //--{ Write: Exprs
  void write(const AST::Expr &expr);

  void write(const AST::AccessField &expr) {
    write(expr.expr, expr.srcDot, expr.name);
  }

  void write(const AST::AccessIndex &expr) {
    write(expr.expr);
    for (const auto &index : expr.indexes)
      write(index.srcBrackL, ALIGN_BEGIN, index.expr, ALIGN_END,
            index.srcBrackR);
  }

  void write(const AST::Binary &expr) {
    if (expr.op == AST::BINOP_COMMA) {
      write(expr.exprLhs, expr.srcOp, ADD_SPACE, expr.exprRhs);
    } else {
      write(expr.exprLhs, ADD_SPACE, expr.srcOp, ADD_SPACE, expr.exprRhs);
    }
  }

  void write(const AST::Call &expr) { write(expr.expr, expr.args); }

  void write(const AST::Identifier &expr) {
    for (const auto &[srcDoubleColon, name] : expr.elements)
      write(srcDoubleColon, name);
  }

  void write(const AST::Intrinsic &expr) { write(expr.srcName); }

  void write(const AST::Let &expr) {
    write(expr.srcKwLet, ADD_SPACE);
    if (!expr.srcBraceL.empty()) {
      write(expr.srcBraceL, ADD_NEWLINE, BLOCK_BEGIN);
      for (const auto &decl : expr.decls)
        write(decl, ADD_NEWLINE);
      write(BLOCK_END, expr.srcBraceR, ADD_SPACE, expr.srcKwIn, ADD_SPACE,
            expr.expr);
    } else {
      SMDL_SANITY_CHECK(expr.decls.size() == 1);
      write(expr.decls[0], ADD_SPACE, expr.srcKwIn, ADD_SPACE, expr.expr);
    }
  }

  void write(const AST::LiteralBool &expr) { write(expr.srcValue); }

  void write(const AST::LiteralFloat &expr) { write(expr.srcValue); }

  void write(const AST::LiteralInt &expr) { write(expr.srcValue); }

  void write(const AST::LiteralString &expr) { write(expr.srcValue); }

  void write(const AST::Parens &expr) {
    write(expr.srcDollar, expr.srcParenL, ALIGN_BEGIN, //
          expr.expr, ALIGN_END, expr.srcParenR);
  }

  void write(const AST::ReturnFrom &expr) {
    write(expr.srcKwReturnFrom, ADD_SPACE, expr.stmt);
  }

  void write(const AST::Select &expr) {
    write(expr.exprCond, ADD_SPACE, expr.srcQuestion, ADD_SPACE, //
          expr.exprThen, ADD_SPACE, expr.srcColon, ADD_SPACE, expr.exprElse);
  }

  void write(const AST::SizeName &expr) {
    write(expr.srcAngleL, expr.name, expr.srcAngleR);
  }

  void write(const AST::Type &expr) {
    for (const auto &src : expr.srcQuals)
      write(src, ADD_SPACE);
    write(expr.expr);
  }

  void write(const AST::TypeCast &expr) {
    write(expr.srcKwCast, expr.srcAngleL, expr.type, expr.srcAngleR, expr.expr);
  }

  void write(const AST::Unary &expr) {
    if (expr.is_postfix())
      write(expr.expr, expr.srcOp);
    else
      write(expr.srcOp, expr.expr);
  }
  //--}

  //--{ Write: Stmts
  void write(const AST::Stmt &stmt);

  void write(const AST::Break &stmt) {
    write(stmt.srcKwBreak, stmt.lateIf, stmt.srcSemicolon);
  }

  void write(const AST::Compound &stmt);

  void write(const AST::Continue &stmt) {
    write(stmt.srcKwContinue, stmt.lateIf, stmt.srcSemicolon);
  }

  void write(const AST::DeclStmt &stmt) { write(stmt.decl); }

  void write(const AST::Defer &stmt) {
    write(stmt.srcKwDefer, ADD_SPACE, stmt.stmt);
  }

  void write(const AST::DoWhile &stmt) {
    write(stmt.srcKwDo, ADD_SPACE, stmt.stmt, ADD_SPACE, //
          stmt.srcKwWhile, ADD_SPACE, stmt.expr, stmt.srcSemicolon);
  }

  void write(const AST::ExprStmt &stmt) { write(stmt.expr, stmt.srcSemicolon); }

  void write(const AST::For &stmt);

  void write(const AST::If &stmt);

  void write(const AST::Preserve &stmt);

  void write(const AST::Return &stmt);

  void write(const AST::Switch &stmt);

  void write(const AST::Unreachable &stmt) {
    write(stmt.srcKwUnreachable, stmt.srcSemicolon);
  }

  void write(const AST::Visit &stmt) {
    write(stmt.srcKwVisit, ADD_SPACE, stmt.name, ADD_SPACE, stmt.srcKwIn,
          ADD_SPACE, stmt.expr, ADD_SPACE, stmt.stmt);
  }

  void write(const AST::While &stmt) {
    write(stmt.srcKwWhile, ADD_SPACE, stmt.expr, ADD_SPACE, stmt.stmt);
  }
  //--}

  void write(const AST::Name &name) { write(name.srcName); }

  void write(const AST::Annotation &anno);

  void write(const AST::AnnotationBlock &annos);

  void write(const AST::Argument &arg);

  void write(const AST::ArgumentList &args);

  void write(const AST::Parameter &param);

  void write(const AST::ParameterList &params);

  void write(const AST::ImportPath &importPath);

  void write(const AST::LateIf &lateIf) {
    write(ADD_SPACE, lateIf.srcKwIf, ADD_SPACE, lateIf.expr);
  }

private:
  std::string outputSrc{};

  std::string_view inputSrc{};

  const char *inputSrcPos{};

  size_t indent{};

  std::vector<size_t> wantAlign{};

  std::optional<size_t> wantBreak{};

  char wantSep{'\0'};

  [[nodiscard]] bool is_beginning_of_line() const {
    return outputSrc.empty() || outputSrc.back() == '\n';
  }

  [[nodiscard]] char last_output() const {
    return outputSrc.empty() ? '\0' : outputSrc.back();
  }

  [[nodiscard]] size_t line_width() const {
    size_t num{};
    for (auto pos{outputSrc.rbegin()}; pos != outputSrc.rend() && *pos != '\n';
         ++pos)
      ++num;
    return num;
  }

  [[nodiscard]] size_t leading_spaces() const {
    return wantSep == ' ' && !wantAlign.empty() ? wantAlign.back() : indent;
  }

  void explicit_space() {
    if (!outputSrc.empty() && outputSrc.back() != ' ' &&
        outputSrc.back() != '\n')
      outputSrc += ' ';
  }

  void explicit_newline() {
    outputSrc += '\n';
    for (size_t i = 0; i < leading_spaces(); i++)
      outputSrc += ' ';
    // Forget any wanted break point
    wantBreak = std::nullopt;
  }
};

/// \}

} // namespace smdl
