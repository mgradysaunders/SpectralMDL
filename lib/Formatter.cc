// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#include "Formatter.h"

namespace smdl {

void Formatter::write_indent_if_necessary() {
  if (!outSrc.empty() && outSrc.back() == '\n')
    for (size_t i = 0; i < indent; i++)
      write_char(' ');
}

void Formatter::write_char(char ch) {
  if (outSrc.empty() && (ch == '\n' || ch == ' '))
    return;
  if (ch == '\n') {
    lineNo++;
    charNo = 1;
  } else {
    charNo++;
  }
  outSrc.push_back(ch);
}

[[nodiscard]] static size_t length_with_reduced_whitespace(llvm::StringRef str) {
  size_t num{};
  for (auto itr = str.begin(); itr < str.end();) {
    ++num;
    ++itr;
    while (itr < str.end() && std::isspace(static_cast<unsigned char>(*itr)))
      ++itr;
  }
  return num;
}

// The idea here is to parse whitespace or comments into adjacent string references. Keep
// in mind that there may be more than one comment between any two grammatically relevant
// tokens in the source code.
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
// - srcregions[3] = "/* Another comment\n   */"
// - srcRegions[4] = "\n\n"
struct WhitespaceOrComments final {
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
  [[nodiscard]] size_t num_comments() const { return numLineComments + numMultilineComments; }

  /// The number of specifically line comments.
  [[nodiscard]] size_t num_line_comments() const { return numLineComments; }

  /// The number of specifically multiline comments.
  [[nodiscard]] size_t num_multiline_comments() const { return numMultilineComments; }

  /// Is the given region whitespace?
  [[nodiscard]] bool is_whitespace(size_t i) const { return !srcRegions[i].starts_with('/'); }

  /// Is the given region a comment?
  [[nodiscard]] bool is_comment(size_t i) const { return srcRegions[i].starts_with('/'); }

  /// Is the given region specifically a line comment?
  [[nodiscard]] bool is_line_comment(size_t i) const { return srcRegions[i].starts_with("//"); }

  /// Is the given region specifically a line comment with the given tokens?
  [[nodiscard]] bool is_line_comment_with_tokens(size_t i, llvm::ArrayRef<llvm::StringRef> tokens) const {
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
  [[nodiscard]] bool is_smdl_format_off(size_t i) const { return is_line_comment_with_tokens(i, {"smdl", "format", "off"}); }

  /// Is the given region an `// smdl format on` directive?
  [[nodiscard]] bool is_smdl_format_on(size_t i) const { return is_line_comment_with_tokens(i, {"smdl", "format", "on"}); }

  /// The number of new lines in the given source region.
  [[nodiscard]] size_t num_newlines(size_t i) const { return srcRegions[i].count('\n'); }

  /// The number of trailing newlines.
  [[nodiscard]] size_t num_trailing_newlines() const {
    return size() > 0 && is_whitespace(size() - 1) ? num_newlines(size() - 1) : 0;
  }

  /// Is the given region a comment that starts on its own line?
  [[nodiscard]] size_t num_newlines_before_comment(size_t i) const {
    return is_comment(i) && i > 0 && is_whitespace(i - 1) ? num_newlines(i - 1) : 0;
  }

  [[nodiscard]] llvm::StringRef operator[](size_t i) const { return srcRegions[i]; }

public:
  llvm::SmallVector<llvm::StringRef> srcRegions{};

  size_t numLineComments{};

  size_t numMultilineComments{};
};

void Formatter::write(AST::SourceRef src) {
  if (!src.empty()) {
    const char *inSrcPos0{src.data()};
    const char *inSrcPos1{src.data() + src.size()};
    sanity_check(inSrc.data() <= inSrcPos0 && inSrcPos1 <= inSrc.data() + inSrc.size());
    sanity_check(inSrcPos <= inSrcPos0);
    {
      auto whitespaceOrComments{WhitespaceOrComments(llvm::StringRef(inSrcPos, inSrcPos0 - inSrcPos))};
      bool forcesNewLine = false;
      for (size_t i = 0; i < whitespaceOrComments.size(); i++) {
        if (whitespaceOrComments.is_comment(i)) {
          size_t numNewLines{whitespaceOrComments.num_newlines_before_comment(i)};
          if (numNewLines == 0) {
            write_char(' ');
          } else {
            write_char('\n');
            if (numNewLines > 1 && wantNewLine)
              write_char('\n');
            write_indent_if_necessary();
            if (!wantNewLine) {
              write_char(' ');
              write_char(' ');
            }
          }
          if (whitespaceOrComments.is_smdl_format_off(i)) {
            if (!formatOff)
              formatOff = FormatOffLocation{whitespaceOrComments[i].data(), outSrc.size()};
          } else if (whitespaceOrComments.is_smdl_format_on(i)) {
            if (formatOff) {
              outSrc.resize(formatOff->outSrcPos);
              outSrc.insert(outSrc.end(), formatOff->inSrcPos, whitespaceOrComments[i].data());
              formatOff = std::nullopt;
            }
          }
          for (char ch : whitespaceOrComments[i])
            write_char(ch);
          forcesNewLine = whitespaceOrComments.is_line_comment(i);
        }
      }
      if (!outSrc.empty()) {
        if (wantNewLine) {
          write_char('\n');
          if (whitespaceOrComments.num_trailing_newlines() > 1)
            write_char('\n');
        } else if (forcesNewLine) {
          write_char('\n');
          indent += 2;
        } else if (wantSpace) {
          write_char(' ');
        }
      }
    }
    write_indent_if_necessary();
    for (char ch : src)
      write_char(ch);
    wantNewLine = wantSpace = false; // Reset
    inSrcPos = inSrcPos1;
  }
}

void Formatter::write(AST::Node &node) { write_type_switch<AST::Decl, AST::Expr, AST::File, AST::Stmt>(node); }

void Formatter::write(AST::File &file) {
  if (file.isSmdlSyntax)
    write(file.srcKwSmdlSyntax, want_newline());
  if (file.version)
    write(file.version->srcKwMdl, want_space(), file.version->srcVersion, file.version->srcSemicolon, want_newline());
  for (auto &decl : file.imports)
    write(want_newline(), decl);
  if (!file.srcKwModule.empty())
    write(want_newline(), file.srcKwModule, file.annotations, file.srcSemicolonAfterModule);
  for (auto &decl : file.globals)
    write(want_newline(), decl->srcKwExport, want_space(), decl);
}

//--{ Write: Decls
void Formatter::write(AST::Decl &decl) {
  write_type_switch<
      AST::Enum, AST::Function, AST::Import, AST::Struct, AST::Tag, AST::Typedef, AST::UnitTest, AST::UsingAlias,
      AST::UsingImport, AST::Variable>(decl);
}

void Formatter::write(AST::Enum &decl) {
  write(decl.srcKwEnum, want_space(), decl.name, decl.annotations, want_space(), decl.srcBraceL);
  adjust_indent(2, [&]() {
    for (auto &declarator : decl.declarators) {
      write(want_newline(), declarator.name);
      if (declarator.init)
        write(want_space(), declarator.srcEq, want_space(), declarator.init);
      write(declarator.annotations, declarator.srcComma);
    }
  });
  write(want_newline(), decl.srcBraceR, decl.srcSemicolon);
}

void Formatter::write(AST::Function &decl) {
  if (!decl.srcAttrsAt.empty()) {
    write(decl.srcAttrsAt, decl.srcAttrsParenL);
    for (size_t i = 0; i < decl.srcAttrs.size(); i++) {
      write(decl.srcAttrs[i]);
      if (i + 1 < decl.srcAttrs.size())
        write(want_space());
    }
    write(decl.srcAttrsParenR, want_space());
  }
  write(decl.returnType, decl.earlyAnnotations, want_space(), decl.name);
  if (decl.isVariant) {
    write(decl.srcVariantParenL, decl.srcVariantStar, decl.srcVariantParenR);
  } else {
    write(decl.params);
  }
  if (!decl.srcFrequency.empty())
    write(want_space(), decl.srcFrequency);
  write(decl.lateAnnotations);
  if (decl.definition == nullptr) {
    write(decl.srcSemicolon);
  } else {
    if (auto stmt{llvm::dyn_cast<AST::Return>(decl.definition.get())}) {
      adjust_indent(0, [&]() {
        write(want_space(), decl.srcEq);
        if (charNo > 60) {
          write(want_newline());
          indent += 2;
        } else {
          write(want_space());
        }
        write(stmt->expr, decl.srcSemicolon);
      });
    } else {
      write(want_space(), decl.definition);
    }
  }
}

void Formatter::write(AST::Import &decl) {
  write(decl.srcKwImport);
  for (auto &path : decl.paths)
    write(want_space(), path.identifier, path.srcComma);
  write(decl.srcSemicolon);
}

void Formatter::write(AST::Struct &decl) {
  write(decl.srcKwStruct, want_space(), decl.name);
  if (!decl.tags.empty()) {
    write(decl.srcColonBeforeTags);
    for (auto &tag : decl.tags)
      write(want_space(), tag.srcKwDefault, want_space(), tag.type, tag.srcComma);
  }
  if (decl.fields.empty()) {
    write(want_space(), decl.srcBraceL, decl.srcBraceR, decl.srcSemicolon);
  } else {
    write(want_space(), decl.srcBraceL);
    adjust_indent(2, [&]() {
      for (auto &field : decl.fields) {
        write(want_newline(), field.srcKwVoid, want_space(), field.type, want_space(), field.name);
        if (field.init)
          write(want_space(), field.srcEq, want_space(), field.init);
        write(field.annotations, field.srcSemicolon);
      }
    });
    write(want_newline(), decl.srcBraceR, decl.srcSemicolon);
  }
}

void Formatter::write(AST::UsingImport &decl) {
  write(decl.srcKwExport, want_space(), decl.srcKwUsing, want_space(), decl.path, want_space(), decl.srcKwImport);
  for (auto &name : decl.names)
    write(want_space(), name.srcName, name.srcComma);
  write(decl.srcSemicolon);
}

void Formatter::write(AST::Variable &decl) {
  write(decl.type);
  for (auto &declarator : decl.declarators) {
    write(want_space(), declarator.name);
    if (declarator.init != nullptr) {
      size_t prevIndent = indent;
      write(want_space(), declarator.srcEq, want_space(), declarator.init);
      indent = prevIndent;
    } else if (declarator.args)
      write(declarator.args);
    write(declarator.annotations, declarator.srcComma);
  }
  write(decl.srcSemicolon);
}
//--}

//--{ Write: Exprs
void Formatter::write(AST::Expr &expr) {
  write_type_switch<
      AST::Binary, AST::Call, AST::Cast, AST::Conditional, AST::GetField, AST::GetIndex, AST::Identifier, AST::Intrinsic,
      AST::Let, AST::LiteralBool, AST::LiteralFloat, AST::LiteralInt, AST::LiteralString, AST::Parens, AST::ReturnFrom,
      AST::SizeName, AST::Type, AST::Unary>(expr);
}

void Formatter::write(AST::Conditional &expr) {
  write(
      expr.cond, want_space(), expr.srcQuestion, want_space(), expr.ifPass, want_space(), expr.srcColon, want_space(),
      expr.ifFail);
}

void Formatter::write(AST::GetIndex &expr) {
  write(expr.expr);
  for (auto &index : expr.indexes) {
    write(index.srcBrackL);
    if (index.expr != nullptr)
      write(index.expr);
    write(index.srcBrackR);
  }
}

void Formatter::write(AST::Let &expr) {
  write(expr.srcKwLet, want_space());
  if (!expr.srcBraceL.empty()) {
    write(expr.srcBraceL);
    adjust_indent(2, [&]() {
      for (auto &var : expr.vars)
        write(want_newline(), var);
    });
    write(want_newline(), expr.srcBraceR, want_space(), expr.srcKwIn, want_space(), expr.expr);
  } else {
    sanity_check(expr.vars.size() == 1);
    write(expr.vars[0], want_space(), expr.srcKwIn, want_space(), expr.expr);
  }
}

void Formatter::write(AST::Type &expr) {
  for (auto &srcAttr : expr.srcAttrs)
    write(srcAttr, want_space());
  write(expr.expr);
}
//--}

//--{ Write: Stmts
void Formatter::write(AST::Stmt &stmt) {
  write_type_switch<
      AST::Break, AST::Compound, AST::Continue, AST::DeclStmt, AST::Defer, AST::DoWhile, AST::ExprStmt, AST::For, AST::If,
      AST::Preserve, AST::Return, AST::Switch, AST::Unreachable, AST::Visit, AST::While>(stmt);
}

void Formatter::write(AST::Compound &stmt) {
  wantNewLine = false;
  wantSpace = true;
  write(stmt.srcBraceL);
  adjust_indent(2, [&]() {
    for (auto &subStmt : stmt.stmts)
      write(want_newline(), subStmt);
  });
  write(want_newline(), stmt.srcBraceR);
}

void Formatter::write(AST::For &stmt) {
  write(
      stmt.srcKwFor, want_space(), stmt.srcParenL, stmt.init, want_space(), stmt.cond, stmt.srcSemicolonAfterCond, want_space(),
      stmt.incr, stmt.srcParenR, want_space(), stmt.body);
}

void Formatter::write(AST::If &stmt) {
  if (stmt.ifFail) {
    write(
        stmt.srcKwIf, want_space(), stmt.cond, want_space(), stmt.ifPass, want_space(), stmt.srcKwElse, want_space(),
        stmt.ifFail);
  } else {
    write(stmt.srcKwIf, want_space(), stmt.cond, want_space(), stmt.ifPass);
  }
}

void Formatter::write(AST::Preserve &stmt) {
  write(stmt.srcKwPreserve);
  for (auto &preservee : stmt.preservees)
    write(want_space(), preservee.expr, preservee.srcComma);
  write(stmt.srcSemicolon);
}

void Formatter::write(AST::Switch &stmt) {
  write(stmt.srcKwSwitch, want_space(), stmt.expr, want_space(), stmt.srcBraceL);
  for (auto &switchCase : stmt.cases) {
    write(want_newline(), switchCase.srcKwCaseOrDefault);
    if (!switchCase.is_default())
      write(want_space(), switchCase.cond);
    write(switchCase.srcColon);
    adjust_indent(2, [&]() {
      for (auto &subStmt : switchCase.stmts) {
        write(want_newline(), subStmt);
      }
    });
  }
  write(want_newline(), stmt.srcBraceR);
}
//--}

void Formatter::write(const AST::AnnotationBlock &annotations) {
  write(want_space(), annotations.srcDoubleBrackL);
  for (auto &annotation : annotations.annotations) {
    write(annotation.identifier, annotation.args);
    if (!annotation.srcComma.empty())
      write(annotation.srcComma, want_space());
  }
  write(annotations.srcDoubleBrackR);
}

void Formatter::write(const AST::Arg &arg) {
  adjust_indent(0, [&]() {
    if (arg.isVisit)
      write(arg.srcKwVisit, want_space());
    if (arg.name)
      write(arg.name, arg.srcColonAfterName, want_space());
    write(arg.expr);
  });
}

void Formatter::write(const AST::ArgList &args) {
  write(args.srcParenL);
  if (args.args.size() > 1 && length_with_reduced_whitespace(args.src) > 80) {
    adjust_indent(2, [&]() {
      write(want_newline());
      for (auto &arg : args.args)
        write(want_newline(), arg, arg.srcComma);
    });
    write(want_newline());
  } else {
    for (auto &arg : args.args) {
      write(arg, arg.srcComma);
      if (!arg.srcComma.empty())
        write(want_space());
    }
  }
  write(args.srcParenR);
}

void Formatter::write(const AST::Param &param) {
  adjust_indent(0, [&]() {
    write(param.type, want_space(), param.name);
    if (param.init)
      write(want_space(), param.srcEq, want_space(), param.init);
    write(param.annotations);
  });
}

void Formatter::write(const AST::ParamList &params) {
  write(params.srcParenL);
  if (params.params.size() > 1 && length_with_reduced_whitespace(params.src) > 80) {
    adjust_indent(4, [&]() {
      for (auto &param : params.params) {
        write(want_newline(), param, param.srcComma);
      }
    });
  } else {
    for (auto &param : params.params) {
      write(param);
      if (!param.srcComma.empty())
        write(param.srcComma, want_space());
    }
  }
  write(params.srcParenR);
}

} // namespace smdl
