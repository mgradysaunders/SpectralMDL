// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#pragma once

#include "smdl/AST.h"

#include "llvm.h"

namespace smdl {

class Formatter final {
public:
  Formatter(const FormatOptions &options) : mOptions(options) {}

  [[nodiscard]] std::string format(llvm::StringRef inSrc,
                                   const AST::Node &node) {
    mOutputSrc.clear();
    mInputSrc = inSrc;
    mIndent = 0;
    // Write!
    write(node);
    // If there was a dangling `// smdl format off` that was
    // never turned back on, then it hasn't been processed yet!
    applyFormatOff(inSrc.data() + inSrc.size());
    // Align line comments
    alignLineComments();
    return mOutputSrc;
  }

  void alignLineComments();

private:
  enum Delim {
    DELIM_NONE,
    DELIM_SPACE,
    DELIM_UNNECESSARY_SPACE,
    DELIM_NEWLINE
  };

  enum Command { INCREMENT_INDENT, ALIGN_INDENT, PUSH_INDENT, POP_INDENT };

  llvm::StringRef consumeInput(size_t numChars) {
    auto inSrc{mInputSrc.take_front(numChars)};
    mInputSrc = mInputSrc.drop_front(numChars);
    return inSrc;
  }

  llvm::StringRef consumeInputSpace() {
    auto inSrc{mInputSrc.take_while(isSpace)};
    mInputSrc = mInputSrc.drop_front(inSrc.size());
    return inSrc;
  }

  [[nodiscard]] llvm::StringRef consumeInputComment() {
    if (mInputSrc.starts_with("//")) {
      auto pos{mInputSrc.find('\n', 1)};
      return consumeInput(pos == llvm::StringRef::npos ? pos : pos + 1);
    }
    if (mInputSrc.starts_with("/*")) {
      auto pos{mInputSrc.find("*/", 2)};
      return consumeInput(pos == llvm::StringRef::npos ? pos : pos + 2);
    }
    return {};
  }

  [[nodiscard]] char lastOutput() const {
    return mOutputSrc.empty() ? '\0' : mOutputSrc.back();
  }

  [[nodiscard]] char lastOutput(int i) const {
    auto outputSrcSize{int(mOutputSrc.size())};
    if (i += outputSrcSize; 0 <= i && i < outputSrcSize)
      return mOutputSrc[i];
    return '\0';
  }

  [[nodiscard]] int currentColumn() const {
    auto column{int(0)};
    auto itr{mOutputSrc.rbegin()};
    while (itr != mOutputSrc.rend() && *itr != '\n') {
      ++column;
      ++itr;
    }
    return column;
  }

  [[nodiscard]] bool nextCommentForcesNewLine() const;

  void writeDelimNone();

  void writeDelimSpace();

  void writeDelimNewLine();

  void writeIndentIfNewLine() {
    if (lastOutput() == '\n')
      for (int i = 0; i < mIndent; i++)
        mOutputSrc += ' ';
  }

  void writeComment(llvm::StringRef inSrc);

  void writeMoreComments() {
    while (true) {
      auto nextComment{consumeInputComment()};
      if (!nextComment.empty())
        writeComment(nextComment);
      else
        break;
    }
  }

  void writeToken(llvm::StringRef inSrc);

  [[nodiscard]] Delim writeStartList(size_t size, bool forceNewLines,
                                     bool alignIndent = true) {
    if (mOptions.compact && size < 4)
      forceNewLines = false;
    bool initialNewLine{forceNewLines || nextCommentForcesNewLine()};
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
  void write(std::string_view inSrc) { writeToken(inSrc); }

  void write(Delim delim) {
    switch (delim) {
    case DELIM_NONE:
      writeDelimNone();
      break;
    case DELIM_SPACE:
      writeDelimSpace();
      break;
    case DELIM_UNNECESSARY_SPACE:
      if (mOptions.compact)
        writeDelimNone();
      else
        writeDelimSpace();
      break;
    case DELIM_NEWLINE:
      writeDelimNewLine();
      break;
    default:
      break;
    }
  }

  void write(Command command) {
    switch (command) {
    case INCREMENT_INDENT:
      mIndent += 2;
      break;
    case ALIGN_INDENT:
      if (lastOutput() != '\n')
        mIndent = currentColumn();
      break;
    case PUSH_INDENT:
      mIndentStack.push_back(mIndent);
      break;
    case POP_INDENT:
      SMDL_SANITY_CHECK(!mIndentStack.empty());
      mIndent = mIndentStack.back();
      mIndentStack.pop_back();
      break;
    default:
      break;
    }
  }

  void write(const AST::Node &node) {
    writeTypeSwitch<AST::Decl, AST::Expr, AST::File, AST::Stmt>(node);
  }

  void write(const AST::File &file);

  //--{ Write: Decls
  void write(const AST::Decl &decl);

  void write(const AST::AnnotationDecl &decl) {
    write(decl.srcKwAnnotation, DELIM_SPACE, decl.name, decl.params,
          decl.annotations, decl.srcSemicolon);
  }

  void write(const AST::Enum &decl);

  void write(const AST::Exec &decl) {
    write(decl.srcKwExec, DELIM_UNNECESSARY_SPACE, decl.stmt);
  }

  void write(const AST::Function &decl);

  void write(const AST::Import &decl) {
    write(decl.srcKwImport, DELIM_SPACE, PUSH_INDENT);
    auto delim{writeStartList(decl.importPathWrappers.size(),
                              decl.hasTrailingComma())};
    for (const auto &[importPath, srcComma] : decl.importPathWrappers) {
      write(importPath, srcComma, srcComma.empty() ? DELIM_NONE : delim);
    }
    write(decl.srcSemicolon, POP_INDENT);
  }

  void write(const AST::Namespace &decl) {
    write(decl.srcKwNamespace, DELIM_SPACE, decl.identifier, DELIM_SPACE,
          decl.srcBraceL, DELIM_NEWLINE);
    for (const auto &subDecl : decl.decls)
      write(subDecl->attributes, subDecl->srcKwExport, DELIM_SPACE, subDecl,
            DELIM_NEWLINE);
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
    auto delim{writeStartList(decl.names.size(), decl.hasTrailingComma())};
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
    if (expr.op == AST::BINOP_APPROX_CMP_EQ ||
        expr.op == AST::BINOP_APPROX_CMP_NE) {
      // Format approximate comparison syntax
      // `lhs ~== [eps] rhs`
      // `lhs ~!= [eps] rhs`
      write(expr.exprLhs,                        //
            DELIM_UNNECESSARY_SPACE, expr.srcOp, //
            DELIM_UNNECESSARY_SPACE, expr.srcBrackL, expr.exprEps,
            expr.srcBrackR, DELIM_UNNECESSARY_SPACE, expr.exprRhs);
    } else if (expr.op == AST::BINOP_ELSE) {
      write(expr.exprLhs, DELIM_SPACE, expr.srcOp, DELIM_SPACE, expr.exprRhs);
    } else {
      bool mustHaveSpaceBefore{
          (expr.op == AST::BINOP_ADD && lastOutput() == '+') ||
          (expr.op == AST::BINOP_SUB && lastOutput() == '-')};
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

  void write(const AST::LiteralString &expr) {
    for (const auto &srcValue : expr.srcValues)
      write(srcValue);
  }

  void write(const AST::Parens &expr) {
    write(expr.srcDollar, PUSH_INDENT, ALIGN_INDENT, expr.srcParenL,
          PUSH_INDENT, ALIGN_INDENT, expr.expr, POP_INDENT, expr.srcParenR,
          POP_INDENT);
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
    if (expr.isPostfix()) {
      write(expr.expr, expr.srcOp);
    } else {
      // Don't write unnecessary plus in compact mode
      if (expr.op == AST::UNOP_POS && mOptions.compact) {
        write(DELIM_NONE);
        consumeInput(expr.srcOp.size());
        write(expr.expr);
      } else {
        // Avoid `+++`, `---`, and `/*`
        if (((expr.op == AST::UNOP_INC || expr.op == AST::UNOP_POS) &&
             lastOutput() == '+') ||
            ((expr.op == AST::UNOP_DEC || expr.op == AST::UNOP_NEG) &&
             lastOutput() == '-') ||
            ((expr.op == AST::UNOP_DEREF) && lastOutput() == '/'))
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
        writeStartList(stmt.exprWrappers.size(), stmt.hasTrailingComma())};
    for (const auto &[expr, srcComma] : stmt.exprWrappers) {
      write(expr, srcComma, srcComma.empty() ? DELIM_NONE : delim);
    }
    write(stmt.srcSemicolon, POP_INDENT);
  }

  void write(const AST::Return &stmt) {
    // This may be empty in abbreviated function definitions!
    write(PUSH_INDENT);
    if (!stmt.srcKwReturn.empty()) {
      write(stmt.srcKwReturn);
      if (stmt.expr || stmt.lateIf)
        write(DELIM_SPACE);
    }
    write(ALIGN_INDENT);
    if (stmt.expr) {
      write(stmt.expr);
    }
    write(stmt.lateIf, stmt.srcSemicolon);
    write(POP_INDENT);
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

  void write(const AST::Decl::Attributes &attributes) {
    write(attributes.srcAt, attributes.srcParenL, PUSH_INDENT, ALIGN_INDENT);
    for (size_t i = 0; i < attributes.attrs.size(); i++) {
      write(attributes.attrs[i]);
      if (i + 1 < attributes.attrs.size())
        write(DELIM_SPACE);
    }
    write(attributes.srcParenR, POP_INDENT, DELIM_NEWLINE);
  }

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

  template <typename... Ts, typename T> void writeTypeSwitch(T &node) {
    llvm::TypeSwitch<T *, void>(&node).template Case<Ts...>(
        [&](auto each) { write(*each); });
  }

private:
  FormatOptions mOptions;

  std::string mOutputSrc;

  llvm::StringRef mInputSrc;

  int mIndent{};

  std::vector<int> mIndentStack;

  struct LineCommentInfo final {
    size_t i{};

    size_t column{};
  };

  std::vector<LineCommentInfo> mLineCommentsToAlign;

  struct FormatOff final {
    const char *inputSrcPos{};

    size_t outputSrcPos{};
  };

  std::optional<FormatOff> mFormatOff;

  void applyFormatOff(const char *inputSrcPos) {
    if (mFormatOff) {
      SMDL_SANITY_CHECK(mFormatOff->inputSrcPos < inputSrcPos);
      mOutputSrc.resize(mFormatOff->outputSrcPos);
      mOutputSrc += std::string_view(mFormatOff->inputSrcPos,
                                     inputSrcPos - mFormatOff->inputSrcPos);
      mFormatOff.reset();
    }
  }
};

} // namespace smdl
