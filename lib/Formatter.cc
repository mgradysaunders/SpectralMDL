// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#include "Formatter.h"

namespace smdl {

void Formatter::alignLineComments() {
  auto insertSpacesBeforeComment{[&](auto &comment, size_t numSpaces) {
    for (size_t i = 0; i < numSpaces; i++) {
      mOutputSrc.insert(mOutputSrc.begin() + comment.i, ' ');
    }
    for (auto itr{&comment};
         itr < mLineCommentsToAlign.data() + mLineCommentsToAlign.size();
         ++itr) {
      itr->i += numSpaces;
      itr->column += numSpaces;
    }
  }};
  for (auto itr{mLineCommentsToAlign.rbegin()};
       itr != mLineCommentsToAlign.rend();) {
    auto maxColumn{itr->column};
    auto itrPrev{itr};
    auto itrNext{itr};
    itrNext++;
    while (itrNext != mLineCommentsToAlign.rend()) {
      const auto &iPrev{itrPrev->i};
      const auto &iNext{itrNext->i};
      if (auto src{llvm::StringRef(mOutputSrc.data() + iNext, iPrev - iNext)};
          src.count('\n') > 1) {
        break;
      }
      maxColumn = std::max(maxColumn, itrNext->column);
      itrPrev = itrNext++;
    }
    while (itr != itrNext) {
      insertSpacesBeforeComment(*itr, maxColumn - itr->column);
      ++itr;
    }
  }
}

bool Formatter::nextCommentForcesNewLine() const {
  if (mOptions.noComments)
    return false;
  auto inSrc{mInputSrc};
  while (!inSrc.empty()) {
    auto ws{inSrc.take_while(isSpace)};
    inSrc = inSrc.drop_front(ws.size());
    if (inSrc.starts_with("//")) {
      return true;
    } else if (inSrc.starts_with("/*")) {
      if (ws.count('\n') > 0) {
        return true;
      }
      auto pos{inSrc.find("*/", 2)};
      if (pos == inSrc.npos)
        break; // Shouldn't happen?
      inSrc = inSrc.drop_front(pos + 2);
      if (inSrc.take_while(isSpace).count('\n') > 0) {
        return true;
      }
    } else {
      break; // Shouldn't happen?
    }
  }
  return false;
}

void Formatter::writeDelimNone() {
  auto numNewLines{consumeInputSpace().count('\n')};
  if (auto comment{consumeInputComment()}; !comment.empty()) {
    // Preserve up to 1 extra newline
    if (!mOptions.noComments && numNewLines == 1 && lastOutput() != '\n') {
      mOutputSrc += '\n';
    }
    writeComment(comment), writeMoreComments();
  }
}

void Formatter::writeDelimSpace() {
  writeDelimNone();
  if (!mOutputSrc.empty() && lastOutput() != ' ' && lastOutput() != '\n') {
    mOutputSrc += ' ';
  }
}

void Formatter::writeDelimNewLine() {
  while (lastOutput() == ' ') // Remove spaces
    mOutputSrc.pop_back();
  if (auto numNewLines{consumeInputSpace().count('\n')}; numNewLines <= 1) {
    if (auto comment{consumeInputComment()}; !comment.empty()) {
      // Preserve up to 1 extra newline
      if (!mOptions.noComments && numNewLines == 1 && lastOutput() != '\n') {
        mOutputSrc += '\n';
      }
      writeComment(comment), writeMoreComments();
    }
    if (!mOutputSrc.empty() && lastOutput() != '\n') {
      mOutputSrc += '\n';
    }
  } else {
    // Preserve 1 extra newline
    while (lastOutput(-1) != '\n' || lastOutput(-2) != '\n') {
      mOutputSrc += '\n';
    }
    if (auto comment{consumeInputComment()}; !comment.empty()) {
      writeComment(comment), writeMoreComments();
    }
  }
  if (mOptions.compact && lastOutput(-1) == '\n' && lastOutput(-2) == '\n') {
    mOutputSrc.pop_back();
  }
}

void Formatter::writeComment(llvm::StringRef inSrc) {
  if (!inSrc.empty() && !mOptions.noComments) {
    // This better be a line comment or a multiline comment!
    SMDL_SANITY_CHECK((inSrc.starts_with("//") && inSrc.ends_with("\n")) ||
                      (inSrc.starts_with("/*") && inSrc.ends_with("*/")));
    if (!mOutputSrc.empty() && lastOutput() != ' ' && lastOutput() != '\n') {
      mOutputSrc += ' ';
    }
    // Parse format on/off directives
    if (inSrc.starts_with("//") || !inSrc.contains('\n')) {
      auto text{inSrc.starts_with("//")
                    ? inSrc.drop_front(2).trim()
                    : inSrc.drop_front(2).drop_back(2).trim()};
      auto tokens{llvm::SmallVector<llvm::StringRef>{}};
      while (!text.empty() && tokens.size() < 3) {
        text = text.drop_while(isSpace);
        auto token{text.take_while([&](char ch) { return !isSpace(ch); })};
        if (!token.empty()) {
          tokens.push_back(token);
          text = text.drop_front(token.size());
        }
      }
      if (tokens.size() == 3 && tokens[0] == "smdl" && tokens[1] == "format") {
        if (tokens[2] == "off" && !mFormatOff)
          mFormatOff = FormatOff{inSrc.data(), mOutputSrc.size()};
        if (tokens[2] == "on" && mFormatOff)
          applyFormatOff(inSrc.data());
      }
    }
    bool isNewLine{mOutputSrc.empty() || lastOutput() == '\n'};
    writeIndentIfNewLine();
    // Remember line comments to align later, but only remember if not
    // disabled by `// smdl format off`!
    if (inSrc.starts_with("//") && !isNewLine && !mFormatOff) {
      mLineCommentsToAlign.push_back(
          {mOutputSrc.size(), static_cast<size_t>(currentColumn())});
    }
    mOutputSrc += inSrc;
    if (!mOptions.compact && consumeInputSpace().count('\n') > 0) {
      // Preserve up to 1 extra newline
      mOutputSrc += '\n';
    } else if (inSrc.starts_with("/*")) {
      // Guarantee a space after multi-line comment
      mOutputSrc += ' ';
    }
  }
}

void Formatter::writeToken(llvm::StringRef inSrc) {
  if (!inSrc.empty()) {
    SMDL_SANITY_CHECK(mInputSrc.begin() <= inSrc.begin() &&
                      inSrc.end() <= mInputSrc.end());
    SMDL_SANITY_CHECK(inSrc.count('\n') == 0);
    writeDelimNone();
    writeIndentIfNewLine();
    consumeInput(inSrc.begin() + inSrc.size() - mInputSrc.begin());
    mOutputSrc += inSrc;
  }
}

void Formatter::write(const AST::File &file) {
  write(DELIM_NONE);
  if (file.isSMDLSyntax()) {
    write(file.srcKwSmdlSyntax, DELIM_NEWLINE);
  }
  if (file.version) {
    auto &version{*file.version};
    write(version.srcKwMdl, DELIM_SPACE, version.srcVersion,
          version.srcSemicolon, DELIM_NEWLINE);
  }
  for (const auto &decl : file.importDecls) {
    write(decl->attributes, decl->srcKwExport, DELIM_SPACE, decl,
          DELIM_NEWLINE);
  }
  if (!file.srcKwModule.empty()) {
    write(file.srcKwModule, file.moduleAnnotations,
          file.srcSemicolonAfterModule, DELIM_NEWLINE);
  }
  for (const auto &decl : file.globalDecls) {
    write(decl->attributes, decl->srcKwExport, DELIM_SPACE, decl,
          DELIM_NEWLINE);
  }
  write(DELIM_NEWLINE);
}

//--{ Write: Decls
void Formatter::write(const AST::Decl &decl) {
  writeTypeSwitch<AST::AnnotationDecl, AST::Enum, AST::Exec, AST::Function,
                  AST::Import, AST::Namespace, AST::Struct, AST::Tag,
                  AST::Typedef, AST::UnitTest, AST::UsingAlias,
                  AST::UsingImport, AST::Variable>(decl);
}

void Formatter::write(const AST::Enum &decl) {
  write(decl.srcKwEnum, DELIM_SPACE, decl.name, decl.annotations,
        DELIM_UNNECESSARY_SPACE, decl.srcBraceL, DELIM_UNNECESSARY_SPACE,
        PUSH_INDENT);
  auto delim{writeStartList(decl.declarators.size(), decl.hasTrailingComma())};
  for (const auto &each : decl.declarators) {
    if (!mOptions.noAnnotations && each.annotations)
      write(DELIM_NEWLINE);
    write(each.name);
    if (each.exprInit) {
      write(DELIM_UNNECESSARY_SPACE, each.srcEqual, DELIM_UNNECESSARY_SPACE,
            PUSH_INDENT);
      if (delim == DELIM_NEWLINE)
        write(ALIGN_INDENT);
      write(each.exprInit, POP_INDENT);
    }
    write(each.annotations, each.srcComma,
          each.srcComma.empty() ? DELIM_NONE : delim);
    if (!mOptions.noAnnotations && each.annotations)
      write(DELIM_NEWLINE);
  }
  write(delim, POP_INDENT, decl.srcBraceR, decl.srcSemicolon);
}

void Formatter::write(const AST::Function &decl) {

  write(decl.returnType, decl.earlyAnnotations, DELIM_SPACE, decl.name,
        decl.params);
  if (!decl.srcFrequency.empty())
    write(DELIM_UNNECESSARY_SPACE, decl.srcFrequency);
  if (decl.lateAnnotations)
    write(decl.lateAnnotations);
  if (!decl.srcEqual.empty()) {
    write(PUSH_INDENT, INCREMENT_INDENT, DELIM_UNNECESSARY_SPACE, decl.srcEqual,
          DELIM_UNNECESSARY_SPACE, PUSH_INDENT, ALIGN_INDENT, decl.definition,
          decl.srcSemicolon, POP_INDENT, POP_INDENT);
  } else if (decl.definition) {
    write(DELIM_UNNECESSARY_SPACE, decl.definition);
  } else {
    write(decl.srcSemicolon);
  }
}

void Formatter::write(const AST::Struct &decl) {
  write(decl.srcKwStruct, DELIM_SPACE, decl.name);
  if (!decl.srcColonBeforeTags.empty()) {
    write(decl.srcColonBeforeTags, DELIM_UNNECESSARY_SPACE, PUSH_INDENT);
    auto delim{writeStartList(decl.tags.size(), decl.hasTrailingCommaOnTags())};
    for (const auto &tag : decl.tags) {
      write(tag.srcKwDefault,
            tag.srcKwDefault.empty() ? DELIM_UNNECESSARY_SPACE : DELIM_SPACE,
            tag.type, tag.srcComma, tag.srcComma.empty() ? DELIM_NONE : delim);
    }
    write(POP_INDENT);
  }
  write(decl.annotations, DELIM_UNNECESSARY_SPACE, decl.srcBraceL, PUSH_INDENT,
        INCREMENT_INDENT, decl.fields.empty() ? DELIM_NONE : DELIM_NEWLINE);
  for (const auto &constructor : decl.constructors) {
    write(constructor.name.srcName, constructor.params, DELIM_UNNECESSARY_SPACE,
          constructor.srcEqual, DELIM_UNNECESSARY_SPACE, constructor.expr,
          constructor.srcSemicolon, DELIM_NEWLINE);
  }
  for (const auto &field : decl.fields) {
    write(field.type, DELIM_SPACE, field.name);
    if (field.exprInit)
      write(DELIM_UNNECESSARY_SPACE, field.srcEqual, //
            DELIM_UNNECESSARY_SPACE, PUSH_INDENT, ALIGN_INDENT, field.exprInit,
            POP_INDENT);
    write(field.annotations, field.srcSemicolon, DELIM_NEWLINE);
  }
  if (decl.stmtFinalize) {
    write(DELIM_NEWLINE, decl.srcKwFinalize, DELIM_SPACE, decl.stmtFinalize,
          DELIM_NEWLINE);
  }
  write(POP_INDENT, decl.srcBraceR, decl.srcSemicolon);
}

void Formatter::write(const AST::Variable &decl) {
  write(decl.type, DELIM_SPACE, PUSH_INDENT);
  auto moreThanOne{decl.declarators.size() > 1};
  auto delim{writeStartList(decl.declarators.size(), decl.hasTrailingComma(),
                            /*alignIndent=*/moreThanOne)};
  for (const auto &each : decl.declarators) {
    if (!mOptions.noAnnotations && each.annotations && moreThanOne)
      write(DELIM_NEWLINE);
    if (!each.srcBraceL.empty())
      write(DELIM_UNNECESSARY_SPACE);
    write(each.srcBraceL);
    for (const auto &[name, srcComma] : each.names) {
      write(name, srcComma);
      if (!srcComma.empty())
        write(DELIM_UNNECESSARY_SPACE);
    }
    write(each.srcBraceR);
    if (each.exprInit) {
      write(DELIM_UNNECESSARY_SPACE, each.srcEqual, DELIM_UNNECESSARY_SPACE,
            PUSH_INDENT);
      if (delim == DELIM_NEWLINE)
        write(ALIGN_INDENT);
      write(each.exprInit, POP_INDENT);
    } else if (each.argsInit) {
      write(each.argsInit);
    }
    write(each.annotations, each.srcComma,
          each.srcComma.empty() ? DELIM_NONE : delim);
    if (!mOptions.noAnnotations && each.annotations && moreThanOne)
      write(DELIM_NEWLINE);
  }
  write(decl.srcSemicolon, POP_INDENT);
}
//--}

//--{ Write: Exprs
void Formatter::write(const AST::Expr &expr) {
  writeTypeSwitch<AST::AccessField, AST::AccessIndex, AST::Binary, AST::Call,
                  AST::Identifier, AST::Intrinsic, AST::Let, AST::LiteralBool,
                  AST::LiteralFloat, AST::LiteralInt, AST::LiteralString,
                  AST::Parens, AST::ReturnFrom, AST::Select, AST::SizeName,
                  AST::Type, AST::TypeCast, AST::Unary>(expr);
}

void Formatter::write(const AST::Let &expr) {
  write(expr.srcKwLet, DELIM_SPACE);
  if (!expr.srcBraceL.empty()) {
    write(expr.srcBraceL, DELIM_NEWLINE, PUSH_INDENT, INCREMENT_INDENT);
    for (const auto &decl : expr.decls)
      write(decl, DELIM_NEWLINE);
    write(POP_INDENT, expr.srcBraceR);
  } else {
    SMDL_SANITY_CHECK(expr.decls.size() == 1);
    write(expr.decls[0]);
  }
  write(DELIM_SPACE, expr.srcKwIn, DELIM_SPACE, expr.expr);
}
//--}

//--{ Write: Stmts
void Formatter::write(const AST::Stmt &stmt) {
  writeTypeSwitch<AST::Break, AST::Compound, AST::Continue, AST::DeclStmt,
                  AST::Defer, AST::DoWhile, AST::ExprStmt, AST::For, AST::If,
                  AST::Preserve, AST::Return, AST::Switch, AST::Unreachable,
                  AST::Visit, AST::While>(stmt);
}

void Formatter::write(const AST::For &stmt) {
  write(stmt.srcKwFor, DELIM_UNNECESSARY_SPACE, stmt.srcParenL, PUSH_INDENT,
        ALIGN_INDENT, stmt.stmtInit, DELIM_UNNECESSARY_SPACE, stmt.exprCond,
        stmt.srcSemicolonAfterCond, DELIM_UNNECESSARY_SPACE, stmt.exprNext,
        POP_INDENT, stmt.srcParenR);
  if (llvm::isa<AST::Compound>(stmt.stmtLoop)) {
    write(DELIM_UNNECESSARY_SPACE, stmt.stmtLoop);
  } else {
    write(PUSH_INDENT, INCREMENT_INDENT, DELIM_NEWLINE, stmt.stmtLoop,
          POP_INDENT, DELIM_NEWLINE);
  }
}

void Formatter::write(const AST::If &stmt) {
  write(stmt.srcKwIf, DELIM_UNNECESSARY_SPACE, stmt.expr);
  if (llvm::isa<AST::Compound>(stmt.stmtThen)) {
    write(DELIM_UNNECESSARY_SPACE, stmt.stmtThen);
  } else {
    write(PUSH_INDENT, INCREMENT_INDENT, DELIM_NEWLINE, stmt.stmtThen,
          POP_INDENT, DELIM_NEWLINE);
  }
  if (stmt.stmtElse) {
    write(llvm::isa<AST::Compound>(stmt.stmtThen) ? DELIM_SPACE : DELIM_NEWLINE,
          stmt.srcKwElse);
    if (llvm::isa<AST::Compound>(stmt.stmtElse) ||
        llvm::isa<AST::If>(stmt.stmtElse)) {
      write(DELIM_SPACE, stmt.stmtElse);
    } else {
      write(PUSH_INDENT, INCREMENT_INDENT, DELIM_NEWLINE, stmt.stmtElse,
            POP_INDENT, DELIM_NEWLINE);
    }
  }
}

void Formatter::write(const AST::Switch &stmt) {
  write(stmt.srcKwSwitch, DELIM_UNNECESSARY_SPACE, stmt.expr,
        DELIM_UNNECESSARY_SPACE, stmt.srcBraceL, DELIM_NEWLINE);
  for (const auto &each : stmt.cases) {
    write(each.srcKwCaseOrDefault);
    if (!each.isDefault())
      write(DELIM_SPACE, each.expr);
    write(each.srcColon);
    if (each.stmts.size() == 1 && !nextCommentForcesNewLine()) {
      write(DELIM_SPACE, each.stmts.front(), DELIM_NEWLINE);
    } else {
      write(PUSH_INDENT, INCREMENT_INDENT, DELIM_NEWLINE);
      for (const auto &subStmt : each.stmts)
        write(subStmt, DELIM_NEWLINE);
      write(POP_INDENT);
    }
  }
  write(stmt.srcBraceR);
}
//--}

void Formatter::write(const AST::AnnotationBlock &annos) {
  if (!mOptions.noAnnotations) {
    write(PUSH_INDENT, INCREMENT_INDENT, DELIM_UNNECESSARY_SPACE,
          annos.srcDoubleBrackL, PUSH_INDENT);
    auto delim{writeStartList(annos.size(), annos.hasTrailingComma())};
    for (const auto &[identifier, args, srcComma] : annos) {
      write(identifier, args, srcComma, srcComma.empty() ? DELIM_NONE : delim);
    }
    write(POP_INDENT, annos.srcDoubleBrackR, POP_INDENT);
  }
}

void Formatter::write(const AST::ArgumentList &args) {
  write(args.srcParenL, PUSH_INDENT);
  auto delim{writeStartList(args.size(), args.hasTrailingComma())};
  for (const auto &arg : args) {
    if (arg.isVisited())
      write(arg.srcKwVisit, DELIM_SPACE);
    if (arg.isNamed())
      write(arg.name, arg.srcColonAfterName, DELIM_SPACE);
    write(arg.expr, arg.srcComma, arg.srcComma.empty() ? DELIM_NONE : delim);
  }
  write(POP_INDENT, args.srcParenR);
}

void Formatter::write(const AST::ParameterList &params) {
  write(params.srcParenL, PUSH_INDENT);
  if (params.isVariant()) {
    write(params.srcStar);
  } else {
    auto delim{writeStartList(params.size(), params.hasTrailingComma())};
    for (const auto &param : params) {
      write(param.type, DELIM_SPACE, param.name);
      if (param.exprInit) {
        write(DELIM_UNNECESSARY_SPACE, param.srcEqual, DELIM_UNNECESSARY_SPACE,
              PUSH_INDENT);
        if (delim == DELIM_NEWLINE) {
          write(ALIGN_INDENT);
        }
        write(param.exprInit, POP_INDENT);
      }
      write(param.annotations, param.srcComma,
            param.srcComma.empty() ? DELIM_NONE : delim);
    }
    write(params.srcEllipsis);
  }
  write(POP_INDENT, params.srcParenR);
}

} // namespace smdl
