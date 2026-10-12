// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
/// \file
#pragma once

#include <utility>

#include "smdl/AST.h"

#include "llvm.h"

namespace smdl {

class Formatter final {
public:
  Formatter(const FormatOptions &options) : options_(options) {}

  [[nodiscard]] std::string format(llvm::StringRef inSrc,
                                   const ast::Node &node) {
    outputSrc_.clear();
    inputSrc_ = inSrc;
    indent_ = 0;
    indentStack_.clear();
    lineCommentsToAlign_.clear();
    numTrials_ = 0;
    hasMoreOnLine_ = false;
    formatOff_.reset();
    // Write!
    write_(node);
    // If there was a dangling `// smdl format off` that was
    // never turned back on, then it hasn't been processed yet!
    applyFormatOff_(inSrc.data() + inSrc.size());
    // Align line comments
    alignLineComments_();
    return outputSrc_;
  }

private:
  void alignLineComments_();

  enum Delim {
    DELIM_NONE,
    DELIM_SPACE,
    DELIM_UNNECESSARY_SPACE,
    DELIM_NEWLINE
  };

  enum Command { INCREMENT_INDENT, ALIGN_INDENT, PUSH_INDENT, POP_INDENT };

  llvm::StringRef consumeInput_(size_t numChars) {
    llvm::StringRef inSrc{inputSrc_.take_front(numChars)};
    inputSrc_ = inputSrc_.drop_front(numChars);
    return inSrc;
  }

  llvm::StringRef consumeInputSpace_() {
    llvm::StringRef inSrc{inputSrc_.take_while(isSpace)};
    inputSrc_ = inputSrc_.drop_front(inSrc.size());
    return inSrc;
  }

  [[nodiscard]] llvm::StringRef consumeInputComment_() {
    if (inputSrc_.starts_with("//")) {
      size_t pos{inputSrc_.find('\n', 1)};
      return consumeInput_(pos == llvm::StringRef::npos ? inputSrc_.size()
                                                        : pos + 1);
    }
    if (inputSrc_.starts_with("/*")) {
      size_t pos{inputSrc_.find("*/", 2)};
      return consumeInput_(pos == llvm::StringRef::npos ? inputSrc_.size()
                                                        : pos + 2);
    }
    return {};
  }

  [[nodiscard]] char lastOutput_() const {
    return outputSrc_.empty() ? '\0' : outputSrc_.back();
  }

  [[nodiscard]] char lastOutput_(int i) const {
    int outputSrcSize{int(outputSrc_.size())};
    if (i += outputSrcSize; 0 <= i && i < outputSrcSize) return outputSrc_[i];
    return '\0';
  }

  [[nodiscard]] int currentColumn_() const {
    int column{0};
    auto itr{outputSrc_.rbegin()};
    while (itr != outputSrc_.rend() && *itr != '\n') {
      ++column;
      ++itr;
    }
    return column;
  }

  /// The indentation of the line currently being written, which is where
  /// a block opened part way along that line belongs.
  [[nodiscard]] int currentLineIndent_() const {
    size_t i{outputSrc_.size()};
    while (i > 0 && outputSrc_[i - 1] != '\n') i--;
    int column{0};
    while (i < outputSrc_.size() && outputSrc_[i] == ' ') i++, column++;
    return column;
  }

  /// Is the layout allowed to depend on how wide the line is?
  [[nodiscard]] bool isColumnAware_() const {
    return options_.softColumnLimit > 0 && !options_.isCompact;
  }

  /// May `ALIGN_INDENT` align to the given column? Aligning further right
  /// than half the line leaves too little of it to be worth anything, which
  /// is how continuations end up crammed against the right margin.
  [[nodiscard]] bool canAlignTo_(int column) const {
    return !isColumnAware_() || column <= options_.softColumnLimit / 2;
  }

  /// May the layout be decided by trial? A trial may nest one deep, so
  /// that measuring a broken form accounts for the constructs inside it
  /// breaking in turn; below that a trial measures its subject as if
  /// nothing in it broke, which bounds the cost. Also false inside
  /// `// smdl format off`, where the output is about to be replaced by the
  /// verbatim input anyway, and false wherever the rest of the line is
  /// already spoken for.
  [[nodiscard]] bool canTrial_() const {
    return isColumnAware_() && numTrials_ < 2 && !formatOff_ && !hasMoreOnLine_;
  }

  /// Keep the given comment in the output? False only when removing
  /// comments, which may still preserve `///` and `///<` documentation
  /// comments.
  [[nodiscard]] bool keepComment_(llvm::StringRef inSrc) const {
    return !options_.shouldDropComments ||
           (options_.shouldKeepDocComments && inSrc.starts_with("///"));
  }

  [[nodiscard]] bool nextCommentForcesNewLine_() const;

  void writeDelimNone_();

  void writeDelimSpace_();

  void writeDelimNewLine_();

  void writeIndentIfNewLine_() {
    if (lastOutput_() == '\n' && !options_.isCompact)
      for (int i = 0; i < indent_; i++) outputSrc_ += ' ';
  }

  void writeComment_(llvm::StringRef inSrc);

  void writeMoreComments_() {
    while (true) {
      llvm::StringRef nextComment{consumeInputComment_()};
      if (!nextComment.empty())
        writeComment_(nextComment);
      else
        break;
    }
  }

  void writeToken_(llvm::StringRef inSrc) { writeToken_(inSrc, inSrc); }

  // Consume `inSrc` from the input but write `outSrc` to the output. This
  // is how the formatter is allowed to rewrite a token instead of echoing
  // it verbatim.
  void writeToken_(llvm::StringRef inSrc, llvm::StringRef outSrc);

  void writeMinifiedFloat_(const ast::LiteralFloat &expr);

  /// Write a comma-separated list, given `writeItems(delim)` which writes
  /// every item and follows each comma it writes with `delim`. The list
  /// goes where it is, or on the next line at the incremented indent, or
  /// one item per line there, whichever is the narrowest layout that is
  /// worth taking. A trailing comma or an intervening comment forces one
  /// item per line. `canBreak` is false for a list that must not move at
  /// all: one nothing encloses, where a break would leave the keyword
  /// that introduces it dangling, and one that is pure data.
  ///
  /// The caller must have written the opening delimiter and pushed the
  /// indent the broken forms are relative to.
  template <typename Items>
  void writeList_(size_t size, bool shouldForceNewLines, bool canBreak,
                  Items &&writeItems) {
    if (options_.isCompact && size < 4) shouldForceNewLines = false;
    // Everything inside a list of several items written on one line has
    // text after it on that line, so nothing in there may rearrange itself
    // over several: the text that follows would land in the middle of the
    // result.
    auto writeItemsOnOneLine{[&] {
      bool hasMoreOnLine{
          std::exchange(hasMoreOnLine_, hasMoreOnLine_ || size > 1)};
      writeItems(DELIM_UNNECESSARY_SPACE);
      hasMoreOnLine_ = hasMoreOnLine;
    }};
    auto writeHere{[&] { write_(DELIM_NONE), writeItemsOnOneLine(); }};
    auto writeOnNextLine{[&] {
      write_(INCREMENT_INDENT, DELIM_NEWLINE), writeItemsOnOneLine();
    }};
    auto writeOnePerLine{[&] {
      write_(INCREMENT_INDENT, DELIM_NEWLINE);
      writeItems(DELIM_NEWLINE);
    }};
    if (!shouldForceNewLines && !nextCommentForcesNewLine_()) {
      if (canBreak && canTrial_()) {
        Measure here{measureInPlace_(writeHere)};
        Measure onNextLine{measureInPlace_(writeOnNextLine)};
        // Give the list the next line to itself when it fits there, the
        // opening bracket is still on the page, and the list is wide
        // enough to be worth a line of its own. Otherwise the break this
        // list wants belongs further out, and moving it alone only
        // dribbles it down the right margin.
        if (!fits_(here) && fits_(onNextLine) &&
            currentColumn_() <= options_.softColumnLimit &&
            here.maxColumn - currentColumn_() >= options_.softColumnLimit / 4) {
          writeOnNextLine();
          return;
        }
        // Otherwise spread it out, but only if that comes out narrower.
        if (!fits_(here) && !fits_(onNextLine) &&
            measureInPlace_(writeOnePerLine).maxColumn < here.maxColumn) {
          writeOnePerLine();
          return;
        }
      }
      writeHere();
      return;
    }
    writeOnePerLine();
  }

  /// Write the right-hand side of a `=`, breaking after the `=` and
  /// continuing at the incremented indent when the right-hand side comes
  /// out better there. Anything that already fits where it is, a `let`
  /// block say, stays where it is.
  template <typename... Ts> void writeAfterEqual_(Ts &&...args) {
    if (canTrial_()) {
      Measure here{
          measureInPlace_([&] { write_(DELIM_UNNECESSARY_SPACE, args...); })};
      Measure onNextLine{measureInPlace_(
          [&] { write_(INCREMENT_INDENT, DELIM_NEWLINE, args...); })};
      // The trial on the next line opens with the line break itself, so
      // one line break is what one line looks like there. Move the
      // right-hand side when that makes it one line that fits, or when it
      // fits there and does not here.
      bool isOneLineHere{fits_(here) && here.numNewLines == 0};
      bool isOneLineThere{fits_(onNextLine) && onNextLine.numNewLines == 1};
      if ((isOneLineThere && !isOneLineHere) ||
          (fits_(onNextLine) && !fits_(here))) {
        write_(PUSH_INDENT, INCREMENT_INDENT, DELIM_NEWLINE, args...,
               POP_INDENT);
        return;
      }
    }
    write_(DELIM_UNNECESSARY_SPACE, args...);
  }

  /// What a trial layout would look like.
  struct Measure final {
    /// The rightmost column any line of it reaches. The first line counts
    /// the columns already written before it; an empty line counts for
    /// nothing, so a trial that opens with a line break is not charged for
    /// the column it started at.
    int maxColumn{};

    /// The line breaks it takes.
    int numNewLines{};
  };

  /// Write `args` knowing that more of the line comes after them, so that
  /// nothing in them rearranges itself over several lines and leaves that
  /// text stranded in the middle of the result.
  template <typename... Ts> void writeWithMoreOnLine_(const Ts &...args) {
    bool hasMoreOnLine{std::exchange(hasMoreOnLine_, true)};
    write_(args...);
    hasMoreOnLine_ = hasMoreOnLine;
  }

  /// Write whatever `writeTrial` writes, measure it, and undo it.
  template <typename Trial>
  [[nodiscard]] Measure measureInPlace_(Trial &&writeTrial) {
    State state{saveState_()};
    int startColumn{currentColumn_()};
    ++numTrials_;
    writeTrial();
    --numTrials_;
    llvm::StringRef outSrc{
        llvm::StringRef(outputSrc_).drop_front(state.outputSrcSize)};
    Measure measure{0, int(outSrc.count('\n'))};
    for (auto column{startColumn}; !outSrc.empty();) {
      auto [line, rest] = outSrc.split('\n');
      if (!line.empty())
        measure.maxColumn =
            std::max(measure.maxColumn, column + int(line.size()));
      outSrc = outSrc.size() == line.size() ? llvm::StringRef() : rest;
      column = 0;
    }
    restoreState_(state);
    return measure;
  }

  /// Does the trial layout stay within the soft column limit?
  [[nodiscard]] bool fits_(const Measure &measure) const {
    return measure.maxColumn <= options_.softColumnLimit;
  }

private:
  void write_(std::string_view inSrc) { writeToken_(inSrc); }

  void write_(Delim delim) {
    switch (delim) {
    case DELIM_NONE:
      writeDelimNone_();
      break;
    case DELIM_SPACE:
      writeDelimSpace_();
      break;
    case DELIM_UNNECESSARY_SPACE:
      if (options_.isCompact)
        writeDelimNone_();
      else
        writeDelimSpace_();
      break;
    case DELIM_NEWLINE:
      writeDelimNewLine_();
      break;
    default:
      break;
    }
  }

  void write_(Command command) {
    switch (command) {
    case INCREMENT_INDENT:
      indent_ += 2;
      break;
    case ALIGN_INDENT:
      if (lastOutput_() != '\n')
        if (int column{currentColumn_()}; canAlignTo_(column)) indent_ = column;
      break;
    case PUSH_INDENT:
      indentStack_.push_back(indent_);
      break;
    case POP_INDENT:
      SMDL_SANITY_CHECK(!indentStack_.empty());
      indent_ = indentStack_.back();
      indentStack_.pop_back();
      break;
    default:
      break;
    }
  }

  void write_(const ast::Node &node) {
    writeTypeSwitch_<ast::Decl, ast::Expr, ast::File, ast::Stmt>(node);
  }

  void write_(const ast::File &file);

  //--{ Write: Decls
  void write_(const ast::Decl &decl);

  void write_(const ast::AnnotationDecl &decl) {
    write_(decl.srcKwAnnotation, DELIM_SPACE, decl.name, decl.params,
           decl.annotations, decl.srcSemicolon);
  }

  void write_(const ast::Enum &decl);

  void write_(const ast::Exec &decl) {
    write_(decl.srcKwExec, DELIM_UNNECESSARY_SPACE, decl.stmt);
  }

  void write_(const ast::Function &decl);

  void write_(const ast::Import &decl) {
    write_(decl.srcKwImport, DELIM_SPACE, PUSH_INDENT);
    writeList_(
        decl.importPathWrappers.size(), decl.hasTrailingComma(),
        /*canBreak=*/false, [&](Delim delim) {
          for (const auto &[importPath, srcComma] : decl.importPathWrappers)
            write_(importPath, srcComma, srcComma.empty() ? DELIM_NONE : delim);
        });
    write_(decl.srcSemicolon, POP_INDENT);
  }

  void write_(const ast::Namespace &decl) {
    write_(decl.srcKwNamespace, DELIM_SPACE, decl.identifier, DELIM_SPACE,
           decl.srcBraceL, DELIM_NEWLINE);
    for (const auto &subDecl : decl.decls)
      write_(subDecl->attributes, subDecl->srcKwExport, DELIM_SPACE, subDecl,
             DELIM_NEWLINE);
    write_(decl.srcBraceR, DELIM_NEWLINE);
  }

  void write_(const ast::Struct &decl);

  void write_(const ast::Tag &decl) {
    write_(decl.srcKwTag, DELIM_SPACE, decl.name, decl.srcSemicolon);
  }

  void write_(const ast::Typedef &decl) {
    write_(decl.srcKwTypedef, DELIM_SPACE, decl.type, DELIM_SPACE, decl.name,
           decl.srcSemicolon);
  }

  void write_(const ast::UnitTest &decl) {
    write_(decl.srcKwUnitTest, DELIM_SPACE, decl.name, DELIM_SPACE, decl.stmt);
  }

  void write_(const ast::UsingAlias &decl) {
    write_(decl.srcKwUsing, DELIM_SPACE, decl.name, DELIM_SPACE, decl.srcEqual,
           DELIM_SPACE, decl.importPath, decl.srcSemicolon);
  }

  void write_(const ast::UsingImport &decl) {
    write_(decl.srcKwUsing, DELIM_SPACE, decl.importPath, DELIM_SPACE,
           decl.srcKwImport, DELIM_SPACE, PUSH_INDENT);
    writeList_(decl.names.size(), decl.hasTrailingComma(),
               /*canBreak=*/false, [&](Delim delim) {
                 for (const auto &[srcName, srcComma] : decl.names)
                   write_(srcName, srcComma,
                          srcComma.empty() ? DELIM_NONE : delim);
               });
    write_(decl.srcSemicolon, POP_INDENT);
  }

  void write_(const ast::Variable &decl);
  //--}

  //--{ Write: Exprs
  void write_(const ast::Expr &expr);

  void write_(const ast::AccessField &expr) {
    write_(expr.expr, expr.srcDot, expr.name);
  }

  void write_(const ast::AccessIndex &expr) {
    write_(expr.expr);
    for (const auto &index : expr.indexes) {
      write_(index.srcBrackL, PUSH_INDENT, ALIGN_INDENT, index.expr, POP_INDENT,
             index.srcBrackR);
    }
  }

  void write_(const ast::Binary &expr) {
    if (expr.op == ast::BINOP_APPROX_CMP_EQ ||
        expr.op == ast::BINOP_APPROX_CMP_NE) {
      // Format approximate comparison syntax
      // `lhs ~== |eps| rhs`, `lhs ~== (eps) rhs`
      // `lhs ~!= |eps| rhs`, `lhs ~!= (eps) rhs`
      writeWithMoreOnLine_(expr.exprLHS,                        //
                           DELIM_UNNECESSARY_SPACE, expr.srcOp, //
                           DELIM_UNNECESSARY_SPACE, expr.srcDelimL,
                           expr.exprEps, expr.srcDelimR);
      write_(DELIM_UNNECESSARY_SPACE, expr.exprRHS);
    } else if (expr.op == ast::BINOP_ELSE) {
      writeWithMoreOnLine_(expr.exprLHS, DELIM_SPACE, expr.srcOp);
      write_(DELIM_SPACE, expr.exprRHS);
    } else {
      writeWithMoreOnLine_(expr.exprLHS);
      // Avoid `+++` and `---` when the left operand ends with `++` or `--`
      bool needsSpaceBefore{
          (expr.op == ast::BINOP_ADD && lastOutput_() == '+') ||
          (expr.op == ast::BINOP_SUB && lastOutput_() == '-')};
      write_(expr.op == ast::BINOP_COMMA ? DELIM_NONE
             : needsSpaceBefore          ? DELIM_SPACE
                                         : DELIM_UNNECESSARY_SPACE,
             expr.srcOp, DELIM_UNNECESSARY_SPACE, expr.exprRHS);
    }
  }

  void write_(const ast::Call &expr) { write_(expr.expr, expr.args); }

  void write_(const ast::Identifier &expr) {
    for (const auto &[srcDoubleColon, name] : expr.elements) {
      write_(srcDoubleColon, name);
    }
  }

  void write_(const ast::Intrinsic &expr) { write_(expr.srcName); }

  void write_(const ast::Lambda &expr);

  void write_(const ast::Let &expr);

  void write_(const ast::LiteralBool &expr) { write_(expr.srcValue); }

  void write_(const ast::LiteralFloat &expr) {
    // Only minify the spelling in compact mode! Otherwise preserve
    // however the author wrote it.
    if (options_.isCompact) {
      writeMinifiedFloat_(expr);
    } else {
      write_(expr.srcValue);
    }
  }

  void write_(const ast::LiteralInt &expr) { write_(expr.srcValue); }

  void write_(const ast::LiteralString &expr) {
    for (size_t i = 0; i < expr.srcValues.size(); i++) {
      if (i > 0) write_(DELIM_UNNECESSARY_SPACE);
      write_(expr.srcValues[i]);
    }
  }

  void write_(const ast::Parens &expr) {
    write_(expr.srcDollar, PUSH_INDENT, ALIGN_INDENT, expr.srcParenL,
           PUSH_INDENT, ALIGN_INDENT, expr.expr, POP_INDENT, expr.srcParenR,
           POP_INDENT);
  }

  void write_(const ast::ReturnFrom &expr) {
    write_(expr.srcKwReturnFrom, DELIM_UNNECESSARY_SPACE, expr.stmt);
  }

  void write_(const ast::Select &expr) {
    auto writeOnOneLine{[&] {
      writeWithMoreOnLine_(expr.exprCond, DELIM_UNNECESSARY_SPACE,    //
                           expr.srcQuestion, DELIM_UNNECESSARY_SPACE, //
                           expr.exprThen, DELIM_UNNECESSARY_SPACE,    //
                           expr.srcColon);
      write_(DELIM_UNNECESSARY_SPACE, expr.exprElse);
    }};
    // A conditional that does not fit breaks before both `?` and `:`, which
    // is the one place an expression is allowed to rearrange itself, because
    // there the branches line up under one another instead of wrapping
    // wherever the width happens to run out.
    auto writeOnThreeLines{[&] {
      write_(expr.exprCond, DELIM_NEWLINE, expr.srcQuestion, DELIM_SPACE,
             expr.exprThen, DELIM_NEWLINE, expr.srcColon, DELIM_SPACE,
             expr.exprElse);
    }};
    // The branches line up under the condition, or one step in from
    // whatever the conditional follows when that is too far right. At the
    // start of a line there is nothing to line up under and the enclosing
    // construct has already chosen the indent.
    auto indent{[&] {
      if (lastOutput_() != '\n') write_(INCREMENT_INDENT, ALIGN_INDENT);
    }};
    write_(PUSH_INDENT);
    if (canTrial_() && !fits_(measureInPlace_(writeOnOneLine)) &&
        fits_(measureInPlace_([&] { indent(), writeOnThreeLines(); }))) {
      indent(), writeOnThreeLines();
    } else {
      writeOnOneLine();
    }
    write_(POP_INDENT);
  }

  void write_(const ast::SizeName &expr) {
    write_(expr.srcAngleL, PUSH_INDENT, ALIGN_INDENT, expr.name, POP_INDENT,
           expr.srcAngleR);
  }

  void write_(const ast::Type &expr) {
    for (const auto &srcQual : expr.srcQuals) write_(srcQual, DELIM_SPACE);
    write_(expr.expr);
  }

  void write_(const ast::TypeCast &expr) {
    write_(expr.srcKwCast, expr.srcAngleL, PUSH_INDENT, ALIGN_INDENT, expr.type,
           POP_INDENT, expr.srcAngleR, expr.expr);
  }

  void write_(const ast::Unary &expr) {
    if (expr.isPostfix()) {
      write_(expr.expr, expr.srcOp);
    } else {
      // Don't write unnecessary plus in compact mode
      if (expr.op == ast::UNOP_POS && options_.isCompact) {
        write_(DELIM_NONE);
        consumeInput_(expr.srcOp.size());
        write_(expr.expr);
      } else {
        // Avoid `+++`, `---`, and `/*`
        if (((expr.op == ast::UNOP_INC || expr.op == ast::UNOP_POS) &&
             lastOutput_() == '+') ||
            ((expr.op == ast::UNOP_DEC || expr.op == ast::UNOP_NEG) &&
             lastOutput_() == '-') ||
            ((expr.op == ast::UNOP_DEREF) && lastOutput_() == '/'))
          write_(DELIM_SPACE);
        write_(expr.srcOp, expr.expr);
      }
    }
  }
  //--}

  //--{ Write: Stmts
  void write_(const ast::Stmt &stmt);

  void write_(const ast::Break &stmt) {
    write_(stmt.srcKwBreak, stmt.lateIf, stmt.srcSemicolon);
  }

  void write_(const ast::Compound &stmt) {
    write_(stmt.srcBraceL, PUSH_INDENT, INCREMENT_INDENT, DELIM_NEWLINE);
    for (const auto &subStmt : stmt.stmts) {
      write_(subStmt, DELIM_NEWLINE);
    }
    write_(POP_INDENT, stmt.srcBraceR);
  }

  void write_(const ast::Continue &stmt) {
    write_(stmt.srcKwContinue, stmt.lateIf, stmt.srcSemicolon);
  }

  void write_(const ast::DeclStmt &stmt) { write_(stmt.decl); }

  void write_(const ast::Defer &stmt) {
    write_(stmt.srcKwDefer, DELIM_SPACE, stmt.stmt);
  }

  void write_(const ast::DoWhile &stmt) {
    write_(stmt.srcKwDo, DELIM_SPACE, stmt.stmt, DELIM_UNNECESSARY_SPACE, //
           stmt.srcKwWhile, DELIM_UNNECESSARY_SPACE, stmt.expr,
           stmt.srcSemicolon);
  }

  void write_(const ast::ExprStmt &stmt) {
    write_(stmt.expr, stmt.lateIf, stmt.srcSemicolon);
  }

  void write_(const ast::For &stmt);

  void write_(const ast::If &stmt);

  void write_(const ast::Preserve &stmt) {
    write_(stmt.srcKwPreserve, DELIM_SPACE, PUSH_INDENT);
    writeList_(stmt.exprWrappers.size(), stmt.hasTrailingComma(),
               /*canBreak=*/false, [&](Delim delim) {
                 for (const auto &[expr, srcComma] : stmt.exprWrappers)
                   write_(expr, srcComma,
                          srcComma.empty() ? DELIM_NONE : delim);
               });
    write_(stmt.srcSemicolon, POP_INDENT);
  }

  void write_(const ast::Return &stmt) {
    // The keyword may be empty in abbreviated function definitions, where
    // there is nothing to align under and the indent belongs to whatever
    // wrote the `=`.
    write_(PUSH_INDENT);
    if (!stmt.srcKwReturn.empty()) {
      write_(stmt.srcKwReturn);
      if (stmt.expr || stmt.lateIf) write_(DELIM_SPACE);
      write_(ALIGN_INDENT);
    }
    if (stmt.expr) {
      write_(stmt.expr);
    }
    write_(stmt.lateIf, stmt.srcSemicolon);
    write_(POP_INDENT);
  }

  void write_(const ast::Switch &stmt);

  void write_(const ast::Unreachable &stmt) {
    write_(stmt.srcKwUnreachable, stmt.srcSemicolon);
  }

  void write_(const ast::Visit &stmt) {
    write_(stmt.srcKwVisit, DELIM_SPACE, stmt.name, DELIM_SPACE, //
           stmt.srcKwIn, DELIM_SPACE, stmt.expr, DELIM_UNNECESSARY_SPACE,
           stmt.stmt);
  }

  void write_(const ast::While &stmt) {
    write_(stmt.srcKwWhile, DELIM_UNNECESSARY_SPACE, stmt.expr,
           DELIM_UNNECESSARY_SPACE, stmt.stmt);
  }
  //--}

  void write_(const ast::Name &name) { write_(name.srcName); }

  void write_(const ast::Decl::Attributes &attributes) {
    write_(attributes.srcAt, attributes.srcParenL, PUSH_INDENT, ALIGN_INDENT);
    for (size_t i = 0; i < attributes.attrs.size(); i++) {
      write_(attributes.attrs[i]);
      if (i + 1 < attributes.attrs.size()) write_(DELIM_SPACE);
    }
    write_(attributes.srcParenR, POP_INDENT, DELIM_NEWLINE);
  }

  void write_(const ast::AnnotationBlock &annos);

  void write_(const ast::ArgumentList &args);

  void write_(const ast::ParameterList &params);

  void write_(const ast::ImportPath &importPath) {
    for (const auto &[srcDoubleColon, srcName, literalString] :
         importPath.elements)
      write_(srcDoubleColon, srcName, literalString);
  }

  void write_(const ast::LateIf &lateIf) {
    write_(PUSH_INDENT, INCREMENT_INDENT, DELIM_SPACE, lateIf.srcKwIf,
           DELIM_UNNECESSARY_SPACE, lateIf.expr, POP_INDENT);
  }

  template <typename T> void write_(const BumpPtr<T> &ptr) {
    if (ptr) write_(*ptr);
  }

  template <typename T> void write_(const std::optional<T> &opt) {
    if (opt) write_(*opt);
  }

  template <typename T0, typename T1, typename... Ts>
  void write_(const T0 &arg0, const T1 &arg1, const Ts &...args) {
    write_(arg0);
    write_(arg1);
    if constexpr (sizeof...(args) > 0) (write_(args), ...);
  }

  template <typename... Ts, typename T> void writeTypeSwitch_(T &node) {
    llvm::TypeSwitch<T *, void>(&node)
        .template Case<Ts...>([&](auto each) { write_(*each); })
        .Default([](T *) {
          SMDL_SANITY_CHECK_MSG(false, "unhandled AST node in Formatter");
        });
  }

private:
  FormatOptions options_;

  std::string outputSrc_;

  llvm::StringRef inputSrc_;

  int indent_{};

  std::vector<int> indentStack_;

  struct LineCommentInfo final {
    size_t i{};

    size_t column{};
  };

  std::vector<LineCommentInfo> lineCommentsToAlign_;

  struct FormatOff final {
    const char *inputSrcPos{};

    size_t outputSrcPos{};
  };

  std::optional<FormatOff> formatOff_;

  /// The nesting depth of `measureInPlace_()`.
  int numTrials_{};

  /// Is the rest of the current line already spoken for?
  bool hasMoreOnLine_{};

  /// Everything `measureInPlace_()` has to put back afterwards.
  struct State final {
    size_t outputSrcSize{};

    /// Trailing spaces removed before the trial, so that the trial cannot
    /// erase output written before it (`writeDelimNewLine_()` would).
    size_t numTrailingSpaces{};

    llvm::StringRef inputSrc{};

    bool hasMoreOnLine{};

    int indent{};

    std::vector<int> indentStack{};

    size_t numLineCommentsToAlign{};

    std::optional<FormatOff> formatOff{};
  };

  [[nodiscard]] State saveState_() {
    State state{};
    while (lastOutput_() == ' ') {
      outputSrc_.pop_back();
      state.numTrailingSpaces++;
    }
    state.outputSrcSize = outputSrc_.size();
    state.inputSrc = inputSrc_;
    state.hasMoreOnLine = hasMoreOnLine_;
    state.indent = indent_;
    state.indentStack = indentStack_;
    state.numLineCommentsToAlign = lineCommentsToAlign_.size();
    state.formatOff = formatOff_;
    return state;
  }

  void restoreState_(const State &state) {
    SMDL_SANITY_CHECK(outputSrc_.size() >= state.outputSrcSize);
    outputSrc_.resize(state.outputSrcSize);
    outputSrc_.append(state.numTrailingSpaces, ' ');
    inputSrc_ = state.inputSrc;
    hasMoreOnLine_ = state.hasMoreOnLine;
    indent_ = state.indent;
    indentStack_ = state.indentStack;
    lineCommentsToAlign_.resize(state.numLineCommentsToAlign);
    formatOff_ = state.formatOff;
  }

  void applyFormatOff_(const char *inputSrcPos) {
    if (formatOff_) {
      SMDL_SANITY_CHECK(formatOff_->inputSrcPos < inputSrcPos);
      outputSrc_.resize(formatOff_->outputSrcPos);
      outputSrc_ += std::string_view(formatOff_->inputSrcPos,
                                     inputSrcPos - formatOff_->inputSrcPos);
      formatOff_.reset();
    }
  }
};

} // namespace smdl
