#include "Formatter.h"

#include "llvm.h"

namespace smdl {

// The idea here is to parse whitespace or comments into adjacent string
// references. Keep in mind that there may be more than one comment between any
// two grammatically relevant tokens in the source code.
//
// So this:
// ~~~~~~~~~~~~~~~~~~~
//  // A comment
//  /* Another comment
//   */
//
// ~~~~~~~~~~~~~~~~~~~
//
// Should be parsed into 5 source regions like this:
// - srcRegions[0] = "\n  "
// - srcRegions[1] = "// A comment"
// - srcRegions[2] = "\n  "
// - srcRegions[3] = "/* Another comment\n   */"
// - srcRegions[4] = "\n\n"
class WhitespaceOrComments final {
public:
  explicit WhitespaceOrComments(llvm::StringRef src) {
    while (!src.empty()) {
      if (src.starts_with("//")) {
        auto pos{src.find('\n')};
        auto src0{src.take_front(pos)};
        auto src1{src.drop_front(pos)};
        src = src1;
        srcRegions.push_back(src0);
        numLineComments++;
      } else if (src.starts_with("/*")) {
        if (auto pos{src.find("*/")}; pos != src.npos) {
          auto src0{src.take_front(pos + 2)};
          auto src1{src.drop_front(pos + 2)};
          src = src1;
          srcRegions.push_back(src0);
          numMultilineComments++;
        } else {
          // Shouldn't ever get here?
          srcRegions.push_back({src});
          numMultilineComments++;
          break;
        }
      } else {
        auto pos{src.find_first_of('/')};
        auto src0{src.take_front(pos)};
        auto src1{src.drop_front(pos)};
        src = src1;
        srcRegions.push_back(src0);
      }
    }
  }

  /// The number or regions.
  [[nodiscard]] size_t size() const { return srcRegions.size(); }

  /// Is only whitespace with no comments?
  [[nodiscard]] bool is_only_whitespace() const { return num_comments() == 0; }

  /// The number of comments.
  [[nodiscard]] size_t num_comments() const {
    return numLineComments + numMultilineComments;
  }

  /// The number of specifically line comments.
  [[nodiscard]] size_t num_line_comments() const { return numLineComments; }

  /// The number of specifically multiline comments.
  [[nodiscard]] size_t num_multiline_comments() const {
    return numMultilineComments;
  }

  /// Is the given region whitespace?
  [[nodiscard]] bool is_whitespace(size_t i) const {
    return !srcRegions[i].starts_with('/');
  }

  /// Is the given region a comment?
  [[nodiscard]] bool is_comment(size_t i) const {
    return srcRegions[i].starts_with('/');
  }

  /// Is the given region specifically a line comment?
  [[nodiscard]] bool is_line_comment(size_t i) const {
    return srcRegions[i].starts_with("//");
  }

  /// Is the given region specifically a line comment with the given tokens?
  [[nodiscard]] bool
  is_line_comment_with_tokens(size_t i,
                              llvm::ArrayRef<llvm::StringRef> tokens) const {
    if (is_line_comment(i)) {
      auto text{srcRegions[i].drop_front(2).trim()};
      auto textTokens{llvm::SmallVector<llvm::StringRef>{}};
      text.split(textTokens, " ", /*MaxSplit=*/-1, /*KeepEmpty=*/false);
      if (textTokens.size() == tokens.size()) {
        for (size_t i = 0; i < tokens.size(); i++)
          if (textTokens[i] != tokens[i])
            return false;
        return true;
      }
    }
    return false;
  }

  /// Is the given region an `// smdl format off` directive?
  [[nodiscard]] bool is_smdl_format_off(size_t i) const {
    return is_line_comment_with_tokens(i, {"smdl", "format", "off"});
  }

  /// Is the given region an `// smdl format on` directive?
  [[nodiscard]] bool is_smdl_format_on(size_t i) const {
    return is_line_comment_with_tokens(i, {"smdl", "format", "on"});
  }

  /// The number of new lines in the given source region.
  [[nodiscard]] size_t num_newlines(size_t i) const {
    return srcRegions[i].count('\n');
  }

  /// The number of trailing newlines.
  [[nodiscard]] size_t num_trailing_newlines() const {
    return size() > 0 && is_whitespace(size() - 1) ? num_newlines(size() - 1)
                                                   : 0;
  }

  /// Is the given region a comment that starts on its own line?
  [[nodiscard]] size_t num_newlines_before_comment(size_t i) const {
    return is_comment(i) && i > 0 && is_whitespace(i - 1) ? num_newlines(i - 1)
                                                          : 0;
  }

  [[nodiscard]] llvm::StringRef operator[](size_t i) const {
    return srcRegions[i];
  }

public:
  llvm::SmallVector<llvm::StringRef> srcRegions{};

  size_t numLineComments{};

  size_t numMultilineComments{};
};

void Formatter::write(std::string_view inSrc) {
  if (inSrc.empty())
    return;
  const char *inSrcPos0{inSrc.data()};
  const char *inSrcPos1{inSrc.data() + inSrc.size()};
  SMDL_SANITY_CHECK(inputSrc.data() <= inSrcPos0 &&
                    inSrcPos1 <= inputSrc.data() + inputSrc.size());
  // TODO This is disgusting and only halfway works
  auto wsOrComments{WhitespaceOrComments(
      llvm::StringRef(inputSrcPos, inSrcPos0 - inputSrcPos))};
  bool firstComment = true;
  for (size_t i = 0; i < wsOrComments.size(); i++) {
    if (wsOrComments.is_line_comment(i)) {
      if (wsOrComments.num_newlines_before_comment(i) > 0 && firstComment)
        explicit_newline();
      explicit_space();
      outputSrc += wsOrComments[i];
      explicit_newline();
      firstComment = false;
    }
  }
  if (wsOrComments.num_line_comments() > 0)
    wantSep = '\0';
  if (wantSep == '\n') {
    explicit_newline();
  } else if (wantSep == ' ') {
    explicit_space();
    if (line_width() + inSrc.size() > 80) {
      auto lineBrSrc{std::string()};
      if (wantBreak) {
        lineBrSrc = outputSrc.substr(*wantBreak);
        outputSrc.resize(*wantBreak);
      }
      // Only newline if it actually enforces the column limit,
      // otherwise just let it overflow!
      if (!(leading_spaces() + lineBrSrc.size() + inSrc.size() > 80)) {
        explicit_newline();
        outputSrc += lineBrSrc;
      }
    }
  }
  outputSrc += inSrc;
  inputSrcPos = inSrcPos1;
  wantSep = '\0'; // Reset
}

void Formatter::write(const AST::Node &node) {
  llvm::TypeSwitch<AST::Node *, void>(const_cast<AST::Node *>(&node))
      .template Case<AST::Decl, AST::Expr, AST::File, AST::Stmt>(
          [&](auto *each) { write(*each); });
}

void Formatter::write(const AST::File &file) {
  if (file.is_smdl_syntax())
    write(file.srcKwSmdlSyntax, ADD_NEWLINE);
  if (file.version)
    write(file.version->srcKwMdl, ADD_SPACE, file.version->srcVersion,
          file.version->srcSemicolon, ADD_NEWLINE);
  for (auto &decl : file.importDecls)
    write(ADD_NEWLINE, decl);
  if (!file.srcKwModule.empty())
    write(ADD_NEWLINE, file.srcKwModule, file.moduleAnnotations,
          file.srcSemicolonAfterModule);
  for (auto &decl : file.globalDecls)
    write(ADD_NEWLINE, decl->srcKwExport, ADD_SPACE, decl);

#if 0
  wantSep = '\n';
  // Catch up to the end of the file (there could be comments we haven't printed
  // yet). If there was an unmatched `// smdl format off`, then overwrite the
  // remainder of the file with the unformatted source.
  write_in_between(inSrc.data() + inSrc.size());
  if (formatOff) {
    outSrc.resize(formatOff->outSrcPos);
    outSrc.insert(outSrc.end(), formatOff->inSrcPos,
                  inSrc.data() + inSrc.size());
    formatOff = std::nullopt;
  }
#endif
}

//--{ Write: Decls
void Formatter::write(const AST::Decl &decl) {
  llvm::TypeSwitch<AST::Decl *, void>(const_cast<AST::Decl *>(&decl))
      .template Case<AST::Enum, AST::Function, AST::Import, AST::Struct,
                     AST::Tag, AST::Typedef, AST::UnitTest, AST::UsingAlias,
                     AST::UsingImport, AST::Variable>(
          [&](auto *each) { write(*each); });
}

void Formatter::write(const AST::Enum &decl) {
  write(decl.srcKwEnum, ADD_SPACE, decl.name, decl.annotations, ADD_SPACE,
        decl.srcBraceL, ADD_NEWLINE, BLOCK_BEGIN);
  for (const auto &each : decl.declarators) {
    write(each.name);
    if (each.exprInit)
      write(ADD_SPACE, each.srcEqual, ADD_SPACE, ALIGN_BEGIN, each.exprInit,
            ALIGN_END);
    write(each.annotations, each.srcComma, ADD_NEWLINE);
  }
  write(BLOCK_END, decl.srcBraceR, decl.srcSemicolon);
}

void Formatter::write(const AST::Function &decl) {
  if (decl.attributes) {
    auto &attributes{*decl.attributes};
    write(attributes.srcAt, attributes.srcParenL);
    for (size_t i = 0; i < attributes.attrs.size(); i++) {
      write(attributes.attrs[i]);
      if (i + 1 < attributes.attrs.size())
        write(ADD_SPACE);
    }
    write(attributes.srcParenR, ADD_SPACE);
  }
  write(decl.returnType, ADD_SPACE, decl.earlyAnnotations, ADD_SPACE, decl.name,
        decl.params, ADD_SPACE, decl.srcFrequency, ADD_SPACE,
        decl.lateAnnotations, ADD_SPACE, decl.srcEqual, ADD_SPACE,
        decl.definition, decl.srcSemicolon);
}

void Formatter::write(const AST::Import &decl) {
  write(decl.srcKwImport, ADD_SPACE, ALIGN_BEGIN);
  for (const auto &[importPath, srcComma] : decl.importPathWrappers) {
    write(importPath);
    if (!srcComma.empty())
      write(srcComma, ADD_SPACE);
  }
  write(decl.srcSemicolon, ALIGN_END);
}

void Formatter::write(const AST::Struct &decl) {
  write(decl.srcKwStruct, ADD_SPACE, decl.name);
  if (!decl.srcColonBeforeTags.empty()) {
    write(decl.srcColonBeforeTags, ADD_SPACE, ALIGN_BEGIN);
    for (const auto &tag : decl.tags)
      write(BREAK_HERE, tag.srcKwDefault, ADD_SPACE, tag.type, tag.srcComma,
            ADD_SPACE);
    write(ALIGN_END);
  }
  write(decl.annotations, ADD_SPACE, decl.srcBraceL, ADD_NEWLINE, BLOCK_BEGIN);
  for (const auto &field : decl.fields) {
    write(field.type, ADD_SPACE, field.name);
    if (field.exprInit)
      write(ADD_SPACE, field.srcEqual, ADD_SPACE, ALIGN_BEGIN, field.exprInit,
            ALIGN_END);
    write(field.annotations, field.srcSemicolon, ADD_NEWLINE);
  }
  if (decl.stmtFinalize)
    write(decl.srcKwFinalize, ADD_SPACE, decl.stmtFinalize, ADD_NEWLINE);
  write(BLOCK_END, decl.srcBraceR, decl.srcSemicolon);
}

void Formatter::write(const AST::UsingAlias &decl) {
  write(decl.srcKwUsing, ADD_SPACE, decl.name, ADD_SPACE, decl.srcEqual,
        ADD_SPACE, decl.importPath, decl.srcSemicolon);
}

void Formatter::write(const AST::UsingImport &decl) {
  write(decl.srcKwUsing, ADD_SPACE, decl.importPath, ADD_SPACE,
        decl.srcKwImport, ADD_SPACE, ALIGN_BEGIN);
  for (const auto &[srcName, srcComma] : decl.names) {
    write(srcName);
    if (!srcComma.empty())
      write(srcComma, ADD_SPACE);
  }
  write(decl.srcSemicolon, ALIGN_END);
}

void Formatter::write(const AST::Variable &decl) {
  write(decl.type, ADD_SPACE, ALIGN_BEGIN);
  for (const auto &each : decl.declarators) {
    write(BREAK_HERE);
    write(each.name);
    if (each.exprInit) {
      write(ADD_SPACE, each.srcEqual, ADD_SPACE, each.exprInit);
    } else if (each.argsInit) {
      write(each.argsInit);
    }
    write(each.annotations);
    if (!each.srcComma.empty())
      write(each.srcComma, ADD_SPACE);
  }
  write(decl.srcSemicolon, ALIGN_END);
}
//--}

//--{ Write: Exprs
void Formatter::write(const AST::Expr &expr) {
  llvm::TypeSwitch<AST::Expr *, void>(const_cast<AST::Expr *>(&expr))
      .template Case<AST::AccessField, AST::AccessIndex, AST::Binary, AST::Call,
                     AST::Identifier, AST::Intrinsic, AST::Let,
                     AST::LiteralBool, AST::LiteralFloat, AST::LiteralInt,
                     AST::LiteralString, AST::Parens, AST::ReturnFrom,
                     AST::Select, AST::SizeName, AST::Type, AST::TypeCast,
                     AST::Unary>([&](auto *each) { write(*each); });
}
//--}

//--{ Write: Stmts
void Formatter::write(const AST::Stmt &stmt) {
  llvm::TypeSwitch<AST::Stmt *, void>(const_cast<AST::Stmt *>(&stmt))
      .template Case<AST::Break, AST::Compound, AST::Continue, AST::DeclStmt,
                     AST::Defer, AST::DoWhile, AST::ExprStmt, AST::For, AST::If,
                     AST::Preserve, AST::Return, AST::Switch, AST::Unreachable,
                     AST::Visit, AST::While>([&](auto *each) { write(*each); });
}

void Formatter::write(const AST::Compound &stmt) {
  write(stmt.srcBraceL, ADD_NEWLINE, BLOCK_BEGIN);
  for (auto &subStmt : stmt.stmts)
    write(subStmt, ADD_NEWLINE);
  write(BLOCK_END, stmt.srcBraceR);
}

void Formatter::write(const AST::For &stmt) {
  write(stmt.srcKwFor, ADD_SPACE, stmt.srcParenL, stmt.stmtInit, ADD_SPACE,
        stmt.exprCond, stmt.srcSemicolonAfterCond, ADD_SPACE, stmt.exprNext,
        stmt.srcParenR);
  if (llvm::isa<AST::Compound>(stmt.stmtLoop)) {
    write(ADD_SPACE, stmt.stmtLoop);
  } else {
    write(ADD_NEWLINE, BLOCK_BEGIN, stmt.stmtLoop, BLOCK_END, ADD_NEWLINE);
  }
}

void Formatter::write(const AST::If &stmt) {
  write(stmt.srcKwIf, ADD_SPACE, stmt.expr);
  if (llvm::isa<AST::Compound>(stmt.stmtThen)) {
    write(ADD_SPACE, stmt.stmtThen);
  } else {
    write(ADD_NEWLINE, BLOCK_BEGIN, stmt.stmtThen, BLOCK_END, ADD_NEWLINE);
  }
  if (stmt.stmtElse) {
    if (llvm::isa<AST::Compound>(stmt.stmtThen)) {
      write(ADD_SPACE);
    } else {
      write(ADD_NEWLINE);
    }
    write(stmt.srcKwElse);
    if (llvm::isa<AST::Compound>(stmt.stmtElse) ||
        llvm::isa<AST::If>(stmt.stmtElse)) {
      write(ADD_SPACE, stmt.stmtElse);
    } else {
      write(ADD_NEWLINE, BLOCK_BEGIN, stmt.stmtElse, BLOCK_END, ADD_NEWLINE);
    }
  }
}

void Formatter::write(const AST::Preserve &stmt) {
  write(stmt.srcKwPreserve);
  for (auto &[expr, srcComma] : stmt.exprWrappers)
    write(ADD_SPACE, expr, srcComma);
  write(stmt.srcSemicolon);
}

void Formatter::write(const AST::Return &stmt) {
  write(stmt.srcKwReturn);
  if (stmt.expr)
    write(ADD_SPACE, stmt.expr);
  write(stmt.lateIf, stmt.srcSemicolon);
}

void Formatter::write(const AST::Switch &stmt) {
  write(stmt.srcKwSwitch, ADD_SPACE, stmt.expr, ADD_SPACE, stmt.srcBraceL,
        ADD_NEWLINE);
  for (const auto &each : stmt.cases) {
    write(each.srcKwCaseOrDefault);
    if (!each.is_default())
      write(ADD_SPACE, ALIGN_BEGIN, each.expr, ALIGN_END);
    write(each.srcColon);
    if (each.stmts.size() == 1 &&
        llvm::isa<AST::Compound>(each.stmts.front())) {
      write(ADD_SPACE, each.stmts.front(), ADD_NEWLINE);
    } else {
      write(ADD_NEWLINE, BLOCK_BEGIN);
      for (const auto &subStmt : each.stmts) {
        write(subStmt, ADD_NEWLINE);
      }
      write(BLOCK_END);
    }
  }
  write(stmt.srcBraceR);
}
//--}

void Formatter::write(const AST::Annotation &anno) {
  write(anno.identifier, anno.args);
  if (!anno.srcComma.empty())
    write(anno.srcComma, ADD_SPACE);
}

void Formatter::write(const AST::AnnotationBlock &annos) {
  write(ADD_SPACE);
  write(annos.srcDoubleBrackL);
  for (const auto &anno : annos.annotations)
    write(BREAK_HERE, anno);
  write(annos.srcDoubleBrackR);
}

void Formatter::write(const AST::Argument &arg) {
  if (arg.is_visited())
    write(arg.srcKwVisit, ADD_SPACE);
  if (arg.name)
    write(arg.name, arg.srcColonAfterName, ADD_SPACE);
  write(arg.expr);
  if (!arg.srcComma.empty())
    write(arg.srcComma, ADD_SPACE);
}

void Formatter::write(const AST::ArgumentList &args) {
  write(args.srcParenL, ALIGN_BEGIN);
  for (const auto &arg : args)
    write(BREAK_HERE, arg);
  write(ALIGN_END, args.srcParenR);
}

void Formatter::write(const AST::Parameter &param) {
  write(param.type, ADD_SPACE, param.name);
  if (param.exprInit)
    write(ADD_SPACE, param.srcEqual, ADD_SPACE, param.exprInit);
  write(param.annotations);
  if (!param.srcComma.empty())
    write(param.srcComma, ADD_SPACE);
}

void Formatter::write(const AST::ParameterList &params) {
  write(params.srcParenL, ALIGN_BEGIN);
  for (const auto &param : params)
    write(param);
  write(ALIGN_END, params.srcParenR);
}

void Formatter::write(const AST::ImportPath &importPath) {
  for (const auto &elem : importPath.elements)
    write(elem.srcDoubleColon, elem.srcName, elem.literalString);
}

} // namespace smdl
