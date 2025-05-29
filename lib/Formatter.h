// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#pragma once

#include "smdl/AST.h"

#include "llvm.h"

namespace smdl {

class Formatter final {
public:
  Formatter(const FormatOptions &options) : options(options) {}

  [[nodiscard]] std::string format(llvm::StringRef inSrc,
                                   const AST::Node &node) {
    outputSrc.clear();
    inputSrc = inSrc;
    indent = 0;
    // Write!
    write(node);
    // If there was a dangling `// smdl format off` that was
    // never turned back on, then it hasn't been processed yet!
    apply_format_off(inSrc.data() + inSrc.size());
    // Align line comments
    align_line_comments();
    return outputSrc;
  }

  void align_line_comments();

private:
  enum Delim {
    DELIM_NONE,
    DELIM_SPACE,
    DELIM_UNNECESSARY_SPACE,
    DELIM_NEWLINE
  };

  enum Command { INCREMENT_INDENT, ALIGN_INDENT, PUSH_INDENT, POP_INDENT };

  llvm::StringRef consume_input(size_t numChars) {
    auto inSrc{inputSrc.take_front(numChars)};
    inputSrc = inputSrc.drop_front(numChars);
    return inSrc;
  }

  llvm::StringRef consume_input_space() {
    auto inSrc{inputSrc.take_while(is_space)};
    inputSrc = inputSrc.drop_front(inSrc.size());
    return inSrc;
  }

  [[nodiscard]] llvm::StringRef consume_input_comment() {
    if (inputSrc.starts_with("//")) {
      auto pos{inputSrc.find('\n', 1)};
      return consume_input(pos == inputSrc.npos ? pos : pos + 1);
    }
    if (inputSrc.starts_with("/*")) {
      auto pos{inputSrc.find("*/", 2)};
      return consume_input(pos == inputSrc.npos ? pos : pos + 2);
    }
    return {};
  }

  [[nodiscard]] char last_output() const {
    return outputSrc.empty() ? '\0' : outputSrc.back();
  }

  [[nodiscard]] char last_output(int i) const {
    if (i += outputSrc.size(); 0 <= i && i < int(outputSrc.size()))
      return outputSrc[i];
    return '\0';
  }

  [[nodiscard]] size_t current_column() const {
    size_t column{};
    for (auto itr{outputSrc.rbegin()}; itr != outputSrc.rend() && *itr != '\n';
         ++itr)
      ++column;
    return column;
  }

  [[nodiscard]] bool next_comment_forces_newline() const;

  void write_delim_none();

  void write_delim_space();

  void write_delim_newline();

  void write_indent_if_newline() {
    if (last_output() == '\n')
      for (int i = 0; i < indent; i++)
        outputSrc += ' ';
  }

  void write_comment(llvm::StringRef inSrc);

  void write_more_comments() {
    while (true) {
      auto nextComment{consume_input_comment()};
      if (!nextComment.empty())
        write_comment(nextComment);
      else
        break;
    }
  }

  void write_token(llvm::StringRef inSrc);

  [[nodiscard]] Delim write_start_list(size_t size, bool forceNewLines,
                                       bool alignIndent = true) {
    if (options.compact && size < 4)
      forceNewLines = false;
    bool initialNewLine{forceNewLines || next_comment_forces_newline()};
    if (initialNewLine) {
      write(INCREMENT_INDENT, DELIM_NEWLINE);
    } else {
      if (alignIndent)
        write(ALIGN_INDENT);
      write(DELIM_NONE);
    }
    return forceNewLines ? DELIM_NEWLINE : DELIM_UNNECESSARY_SPACE;
  }

private:
  void write(std::string_view inSrc) { write_token(inSrc); }

  void write(Delim delim) {
    switch (delim) {
    case DELIM_NONE:
      write_delim_none();
      break;
    case DELIM_SPACE:
      write_delim_space();
      break;
    case DELIM_UNNECESSARY_SPACE:
      if (options.compact)
        write_delim_none();
      else
        write_delim_space();
      break;
    case DELIM_NEWLINE:
      write_delim_newline();
      break;
    default:
      break;
    }
  }

  void write(Command command) {
    switch (command) {
    case INCREMENT_INDENT:
      indent += 2;
      break;
    case ALIGN_INDENT:
      if (last_output() != '\n')
        indent = current_column();
      break;
    case PUSH_INDENT:
      indentStack.push_back(indent);
      break;
    case POP_INDENT:
      SMDL_SANITY_CHECK(!indentStack.empty());
      indent = indentStack.back();
      indentStack.pop_back();
      break;
    default:
      break;
    }
  }

  void write(const AST::Node &node) {
    write_type_switch<AST::Decl, AST::Expr, AST::File, AST::Stmt>(node);
  }

  void write(const AST::File &file);

  //--{ Write: Decls
  void write(const AST::Decl &decl);

  void write(const AST::Enum &decl);

  void write(const AST::Function &decl);

  void write(const AST::Import &decl) {
    write(decl.srcKwImport, DELIM_SPACE, PUSH_INDENT);
    auto delim{write_start_list(decl.importPathWrappers.size(),
                                decl.has_trailing_comma())};
    for (const auto &[importPath, srcComma] : decl.importPathWrappers) {
      write(importPath, srcComma, srcComma.empty() ? DELIM_NONE : delim);
    }
    write(decl.srcSemicolon, POP_INDENT);
  }

  void write(const AST::Namespace &decl) {
    write(decl.srcKwNamespace, DELIM_SPACE, decl.identifier, DELIM_SPACE,
          decl.srcBraceL, DELIM_NEWLINE);
    for (const auto &subDecl : decl.decls)
      write(subDecl->srcKwExport, DELIM_SPACE, subDecl, DELIM_NEWLINE);
    write(decl.srcBraceR, DELIM_NEWLINE);
  }

  void write(const AST::Struct &decl);

  void write(const AST::Tag &decl) {
    write(decl.srcKwTag, DELIM_SPACE, decl.name, decl.srcSemicolon);
  }

  void write(const AST::Typedef &decl) {
    write(decl.srcKwTypedef, DELIM_SPACE, decl.type, DELIM_SPACE, decl.name,
          decl.srcSemicolon);
  }

  void write(const AST::UnitTest &decl) {
    write(decl.srcKwUnitTest, DELIM_SPACE, decl.name, DELIM_SPACE, decl.stmt);
  }

  void write(const AST::UsingAlias &decl) {
    write(decl.srcKwUsing, DELIM_SPACE, decl.name, DELIM_SPACE, decl.srcEqual,
          DELIM_SPACE, decl.importPath, decl.srcSemicolon);
  }

  void write(const AST::UsingImport &decl) {
    write(decl.srcKwUsing, DELIM_SPACE, decl.importPath, DELIM_SPACE,
          decl.srcKwImport, DELIM_SPACE, PUSH_INDENT);
    auto delim{write_start_list(decl.names.size(), decl.has_trailing_comma())};
    for (const auto &[srcName, srcComma] : decl.names)
      write(srcName, srcComma, srcComma.empty() ? DELIM_NONE : delim);
    write(decl.srcSemicolon, POP_INDENT);
  }

  void write(const AST::Variable &decl);
  //--}

  //--{ Write: Exprs
  void write(const AST::Expr &expr);

  void write(const AST::AccessField &expr) {
    write(expr.expr, expr.srcDot, expr.name);
  }

  void write(const AST::AccessIndex &expr) {
    write(expr.expr);
    for (const auto &index : expr.indexes) {
      write(index.srcBrackL, PUSH_INDENT, ALIGN_INDENT, index.expr, POP_INDENT,
            index.srcBrackR);
    }
  }

  void write(const AST::Binary &expr) {
    if (expr.op == AST::BINOP_ELSE) {
      write(expr.exprLhs, DELIM_SPACE, expr.srcOp, DELIM_SPACE, expr.exprRhs);
    } else {
      bool mustHaveSpaceBefore{
          (expr.op == AST::BINOP_ADD && last_output() == '+') ||
          (expr.op == AST::BINOP_SUB && last_output() == '-')};
      write(expr.exprLhs,
            expr.op == AST::BINOP_COMMA ? DELIM_NONE
            : mustHaveSpaceBefore       ? DELIM_SPACE
                                        : DELIM_UNNECESSARY_SPACE,
            expr.srcOp, DELIM_UNNECESSARY_SPACE, expr.exprRhs);
    }
  }

  void write(const AST::Call &expr) { write(expr.expr, expr.args); }

  void write(const AST::Identifier &expr) {
    for (const auto &[srcDoubleColon, name] : expr.elements) {
      write(srcDoubleColon, name);
    }
  }

  void write(const AST::Intrinsic &expr) { write(expr.srcName); }

  void write(const AST::Let &expr);

  void write(const AST::LiteralBool &expr) { write(expr.srcValue); }

  void write(const AST::LiteralFloat &expr) { write(expr.srcValue); }

  void write(const AST::LiteralInt &expr) { write(expr.srcValue); }

  void write(const AST::LiteralString &expr) { write(expr.srcValue); }

  void write(const AST::Parens &expr) {
    write(expr.srcDollar, expr.srcParenL, PUSH_INDENT, ALIGN_INDENT, expr.expr,
          POP_INDENT, expr.srcParenR);
  }

  void write(const AST::ReturnFrom &expr) {
    write(expr.srcKwReturnFrom, DELIM_UNNECESSARY_SPACE, expr.stmt);
  }

  void write(const AST::Select &expr) {
    write(PUSH_INDENT, ALIGN_INDENT,                 //
          expr.exprCond, DELIM_UNNECESSARY_SPACE,    //
          expr.srcQuestion, DELIM_UNNECESSARY_SPACE, //
          expr.exprThen, DELIM_UNNECESSARY_SPACE,    //
          expr.srcColon, DELIM_UNNECESSARY_SPACE, expr.exprElse, POP_INDENT);
  }

  void write(const AST::SizeName &expr) {
    write(expr.srcAngleL, PUSH_INDENT, ALIGN_INDENT, expr.name, POP_INDENT,
          expr.srcAngleR);
  }

  void write(const AST::Type &expr) {
    for (const auto &srcQual : expr.srcQuals)
      write(srcQual, DELIM_SPACE);
    write(expr.expr);
  }

  void write(const AST::TypeCast &expr) {
    write(expr.srcKwCast, expr.srcAngleL, PUSH_INDENT, ALIGN_INDENT, expr.type,
          POP_INDENT, expr.srcAngleR, expr.expr);
  }

  void write(const AST::Unary &expr) {
    if (expr.is_postfix()) {
      write(expr.expr, expr.srcOp);
    } else {
      // Don't write unnecessary plus in compact mode
      if (expr.op == AST::UNOP_POS && options.compact) {
        write(DELIM_NONE);
        consume_input(expr.srcOp.size());
        write(expr.expr);
      } else {
        if (((expr.op == AST::UNOP_INC || expr.op == AST::UNOP_POS) &&
             last_output() == '+') ||
            ((expr.op == AST::UNOP_DEC || expr.op == AST::UNOP_NEG) &&
             last_output() == '-'))
          write(DELIM_SPACE);
        write(expr.srcOp, expr.expr);
      }
    }
  }
  //--}

  //--{ Write: Stmts
  void write(const AST::Stmt &stmt);

  void write(const AST::Break &stmt) {
    write(stmt.srcKwBreak, stmt.lateIf, stmt.srcSemicolon);
  }

  void write(const AST::Compound &stmt) {
    write(stmt.srcBraceL, PUSH_INDENT, INCREMENT_INDENT, DELIM_NEWLINE);
    for (const auto &subStmt : stmt.stmts) {
      write(subStmt, DELIM_NEWLINE);
    }
    write(POP_INDENT, stmt.srcBraceR);
  }

  void write(const AST::Continue &stmt) {
    write(stmt.srcKwContinue, stmt.lateIf, stmt.srcSemicolon);
  }

  void write(const AST::DeclStmt &stmt) { write(stmt.decl); }

  void write(const AST::Defer &stmt) {
    write(stmt.srcKwDefer, DELIM_SPACE, stmt.stmt);
  }

  void write(const AST::DoWhile &stmt) {
    write(stmt.srcKwDo, DELIM_SPACE, stmt.stmt, DELIM_UNNECESSARY_SPACE, //
          stmt.srcKwWhile, DELIM_UNNECESSARY_SPACE, stmt.expr,
          stmt.srcSemicolon);
  }

  void write(const AST::ExprStmt &stmt) {
    write(stmt.expr, stmt.lateIf, stmt.srcSemicolon);
  }

  void write(const AST::For &stmt);

  void write(const AST::If &stmt);

  void write(const AST::Preserve &stmt) {
    write(stmt.srcKwPreserve, DELIM_SPACE, PUSH_INDENT);
    auto delim{
        write_start_list(stmt.exprWrappers.size(), stmt.has_trailing_comma())};
    for (const auto &[expr, srcComma] : stmt.exprWrappers) {
      write(expr, srcComma, srcComma.empty() ? DELIM_NONE : delim);
    }
    write(stmt.srcSemicolon, POP_INDENT);
  }

  void write(const AST::Return &stmt) {
    // This may be empty in abbreviated function definitions!
    if (!stmt.srcKwReturn.empty())
      write(stmt.srcKwReturn, DELIM_SPACE);
    if (stmt.expr)
      write(stmt.expr);
    write(stmt.lateIf, stmt.srcSemicolon);
  }

  void write(const AST::Switch &stmt);

  void write(const AST::Unreachable &stmt) {
    write(stmt.srcKwUnreachable, stmt.srcSemicolon);
  }

  void write(const AST::Visit &stmt) {
    write(stmt.srcKwVisit, DELIM_SPACE, stmt.name, DELIM_SPACE, //
          stmt.srcKwIn, DELIM_SPACE, stmt.expr, DELIM_UNNECESSARY_SPACE,
          stmt.stmt);
  }

  void write(const AST::While &stmt) {
    write(stmt.srcKwWhile, DELIM_UNNECESSARY_SPACE, stmt.expr,
          DELIM_UNNECESSARY_SPACE, stmt.stmt);
  }
  //--}

  void write(const AST::Name &name) { write(name.srcName); }

  void write(const AST::AnnotationBlock &annos);

  void write(const AST::ArgumentList &args);

  void write(const AST::ParameterList &params);

  void write(const AST::ImportPath &importPath) {
    for (const auto &[srcDoubleColon, srcName, literalString] :
         importPath.elements)
      write(srcDoubleColon, srcName, literalString);
  }

  void write(const AST::LateIf &lateIf) {
    write(PUSH_INDENT, INCREMENT_INDENT, DELIM_SPACE, lateIf.srcKwIf,
          DELIM_UNNECESSARY_SPACE, lateIf.expr, POP_INDENT);
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
  void write(const T0 &arg0, const T1 &arg1, const Ts &...args) {
    write(arg0);
    write(arg1);
    if constexpr (sizeof...(args) > 0)
      (write(args), ...);
  }

  template <typename... Ts, typename T> void write_type_switch(T &node) {
    llvm::TypeSwitch<T *, void>(&node).template Case<Ts...>(
        [&](auto each) { write(*each); });
  }

private:
  FormatOptions options{};

  std::string outputSrc{};

  llvm::StringRef inputSrc{};

  int indent{};

  std::vector<int> indentStack{};

  struct LineCommentInfo final {
    size_t i{};

    size_t column{};
  };

  std::vector<LineCommentInfo> lineCommentsToAlign{};

  struct FormatOffInfo final {
    const char *inputSrcPos{};

    size_t outputSrcPos{};
  };

  std::optional<FormatOffInfo> formatOff{};

  void apply_format_off(const char *inputSrcPos) {
    if (formatOff) {
      SMDL_SANITY_CHECK(formatOff->inputSrcPos < inputSrcPos);
      outputSrc.resize(formatOff->outputSrcPos);
      outputSrc += std::string_view(formatOff->inputSrcPos,
                                    inputSrcPos - formatOff->inputSrcPos);
      formatOff.reset();
    }
  }
};

} // namespace smdl
