// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#include "Formatter.h"

namespace smdl {

void Formatter::align_line_comments() {
  auto insertSpacesBeforeComment{[&](auto &comment, size_t numSpaces) {
    for (size_t i = 0; i < numSpaces; i++) {
      outputSrc.insert(outputSrc.begin() + comment.i, ' ');
    }
    for (auto itr{&comment};
         itr < lineCommentsToAlign.data() + lineCommentsToAlign.size(); ++itr) {
      itr->i += numSpaces;
      itr->column += numSpaces;
    }
  }};
  for (auto itr{lineCommentsToAlign.rbegin()};
       itr != lineCommentsToAlign.rend();) {
    auto maxColumn{itr->column};
    auto itrPrev{itr};
    auto itrNext{itr};
    itrNext++;
    while (itrNext != lineCommentsToAlign.rend()) {
      const auto &iPrev{itrPrev->i};
      const auto &iNext{itrNext->i};
      if (auto src{llvm::StringRef(outputSrc.data() + iNext, iPrev - iNext)};
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

bool Formatter::next_comment_forces_newline() const {
  if (options.noComments)
    return false;
  auto inSrc{inputSrc};
  while (!inSrc.empty()) {
    auto ws{inSrc.take_while(is_space)};
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
      if (inSrc.take_while(is_space).count('\n') > 0) {
        return true;
      }
    } else {
      break; // Shouldn't happen?
    }
  }
  return false;
}

void Formatter::write_delim_none() {
  auto numNewLines{consume_input_space().count('\n')};
  if (auto comment{consume_input_comment()}; !comment.empty()) {
    // Preserve up to 1 extra newline
    if (!options.noComments && numNewLines == 1 && last_output() != '\n') {
      outputSrc += '\n';
    }
    write_comment(comment), write_more_comments();
  }
}

void Formatter::write_delim_space() {
  write_delim_none();
  if (!outputSrc.empty() && last_output() != ' ' && last_output() != '\n') {
    outputSrc += ' ';
  }
}

void Formatter::write_delim_newline() {
  while (last_output() == ' ') // Remove spaces
    outputSrc.pop_back();
  if (auto numNewLines{consume_input_space().count('\n')}; numNewLines <= 1) {
    if (auto comment{consume_input_comment()}; !comment.empty()) {
      // Preserve up to 1 extra newline
      if (!options.noComments && numNewLines == 1 && last_output() != '\n') {
        outputSrc += '\n';
      }
      write_comment(comment), write_more_comments();
    }
    if (!outputSrc.empty() && last_output() != '\n') {
      outputSrc += '\n';
    }
  } else {
    // Preserve 1 extra newline
    while (last_output(-1) != '\n' || last_output(-2) != '\n') {
      outputSrc += '\n';
    }
    if (auto comment{consume_input_comment()}; !comment.empty()) {
      write_comment(comment), write_more_comments();
    }
  }
  if (options.compact && last_output(-1) == '\n' && last_output(-2) == '\n') {
    outputSrc.pop_back();
  }
}

void Formatter::write_comment(llvm::StringRef inSrc) {
  if (!inSrc.empty() && !options.noComments) {
    // This better be a line comment or a multiline comment!
    SMDL_SANITY_CHECK((inSrc.starts_with("//") && inSrc.ends_with("\n")) ||
                      (inSrc.starts_with("/*") && inSrc.ends_with("*/")));
    if (!outputSrc.empty() && last_output() != ' ' && last_output() != '\n') {
      outputSrc += ' ';
    }
    // Parse format on/off directives
    if (inSrc.starts_with("//") || !inSrc.contains('\n')) {
      auto text{inSrc.starts_with("//")
                    ? inSrc.drop_front(2).trim()
                    : inSrc.drop_front(2).drop_back(2).trim()};
      auto tokens{llvm::SmallVector<llvm::StringRef>{}};
      while (!text.empty() && tokens.size() < 3) {
        text = text.drop_while(is_space);
        auto token{text.take_while([&](char ch) { return !is_space(ch); })};
        if (!token.empty()) {
          tokens.push_back(token);
          text = text.drop_front(token.size());
        }
      }
      if (tokens.size() == 3 && tokens[0] == "smdl" && tokens[1] == "format") {
        if (tokens[2] == "off" && !formatOff)
          formatOff = FormatOffInfo{inSrc.data(), outputSrc.size()};
        if (tokens[2] == "on" && formatOff)
          apply_format_off(inSrc.data());
      }
    }
    bool isNewLine{outputSrc.empty() || last_output() == '\n'};
    write_indent_if_newline();
    // Remember line comments to align later, but only remember if not
    // disabled by `// smdl format off`!
    if (inSrc.starts_with("//") && !isNewLine && !formatOff) {
      lineCommentsToAlign.push_back({outputSrc.size(), current_column()});
    }
    outputSrc += inSrc;
    if (!options.compact && consume_input_space().count('\n') > 0) {
      // Preserve up to 1 extra newline
      outputSrc += '\n';
    } else if (inSrc.starts_with("/*")) {
      // Guarantee a space after multi-line comment
      outputSrc += ' ';
    }
  }
}

void Formatter::write_token(llvm::StringRef inSrc) {
  if (!inSrc.empty()) {
    SMDL_SANITY_CHECK(
        (inputSrc.begin() <= inSrc.begin() && inSrc.end() <= inputSrc.end()) &&
        inSrc.count('\n') == 0);
    write_delim_none();
    write_indent_if_newline();
    consume_input(inSrc.begin() + inSrc.size() - inputSrc.begin());
    outputSrc += inSrc;
  }
}

void Formatter::write(const AST::File &file) {
  write(DELIM_NONE);
  if (file.is_smdl_syntax()) {
    write(file.srcKwSmdlSyntax, DELIM_NEWLINE);
  }
  if (file.version) {
    auto &version{*file.version};
    write(version.srcKwMdl, DELIM_SPACE, version.srcVersion,
          version.srcSemicolon, DELIM_NEWLINE);
  }
  for (const auto &decl : file.importDecls) {
    write(decl->srcKwExport, DELIM_SPACE, decl, DELIM_NEWLINE);
  }
  if (!file.srcKwModule.empty()) {
    write(file.srcKwModule, file.moduleAnnotations,
          file.srcSemicolonAfterModule, DELIM_NEWLINE);
  }
  for (const auto &decl : file.globalDecls) {
    write(decl->srcKwExport, DELIM_SPACE, decl, DELIM_NEWLINE);
  }
  write(DELIM_NEWLINE);
}

//--{ Write: Decls
void Formatter::write(const AST::Decl &decl) {
  write_type_switch<AST::Enum, AST::Function, AST::Import, AST::Namespace,
                    AST::Struct, AST::Tag, AST::Typedef, AST::UnitTest,
                    AST::UsingAlias, AST::UsingImport, AST::Variable>(decl);
}

void Formatter::write(const AST::Enum &decl) {
  write(decl.srcKwEnum, DELIM_SPACE, decl.name, decl.annotations,
        DELIM_UNNECESSARY_SPACE, decl.srcBraceL, DELIM_UNNECESSARY_SPACE,
        PUSH_INDENT);
  auto delim{
      write_start_list(decl.declarators.size(), decl.has_trailing_comma())};
  for (const auto &each : decl.declarators) {
    if (!options.noAnnotations && each.annotations)
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
    if (!options.noAnnotations && each.annotations)
      write(DELIM_NEWLINE);
  }
  write(delim, POP_INDENT, decl.srcBraceR, decl.srcSemicolon);
}

void Formatter::write(const AST::Function &decl) {
  if (decl.attributes) {
    auto &attributes{*decl.attributes};
    write(attributes.srcAt, attributes.srcParenL, PUSH_INDENT, ALIGN_INDENT);
    for (size_t i = 0; i < attributes.attrs.size(); i++) {
      write(attributes.attrs[i]);
      if (i + 1 < attributes.attrs.size())
        write(DELIM_SPACE);
    }
    write(attributes.srcParenR, POP_INDENT, DELIM_UNNECESSARY_SPACE);
  }
  write(decl.returnType, decl.earlyAnnotations, DELIM_SPACE, decl.name,
        decl.params, DELIM_UNNECESSARY_SPACE, decl.srcFrequency,
        decl.lateAnnotations);
  if (!decl.srcEqual.empty()) {
    write(PUSH_INDENT, INCREMENT_INDENT, DELIM_UNNECESSARY_SPACE, decl.srcEqual,
          DELIM_UNNECESSARY_SPACE, PUSH_INDENT, ALIGN_INDENT, decl.definition,
          decl.srcSemicolon, POP_INDENT, POP_INDENT);
  } else {
    write(DELIM_UNNECESSARY_SPACE, decl.definition);
  }
}

void Formatter::write(const AST::Struct &decl) {
  write(decl.srcKwStruct, DELIM_SPACE, decl.name);
  if (!decl.srcColonBeforeTags.empty()) {
    write(decl.srcColonBeforeTags, DELIM_UNNECESSARY_SPACE, PUSH_INDENT);
    auto delim{
        write_start_list(decl.tags.size(), decl.has_trailing_comma_on_tags())};
    for (const auto &tag : decl.tags) {
      write(tag.srcKwDefault,
            tag.srcKwDefault.empty() ? DELIM_UNNECESSARY_SPACE : DELIM_SPACE,
            tag.type, tag.srcComma, tag.srcComma.empty() ? DELIM_NONE : delim);
    }
    write(POP_INDENT);
  }
  write(decl.annotations, DELIM_UNNECESSARY_SPACE, decl.srcBraceL, PUSH_INDENT,
        INCREMENT_INDENT, decl.fields.empty() ? DELIM_NONE : DELIM_NEWLINE);
  for (const auto &field : decl.fields) {
    write(field.type, DELIM_SPACE, field.name);
    if (field.exprInit)
      write(DELIM_UNNECESSARY_SPACE, field.srcEqual, //
            DELIM_UNNECESSARY_SPACE, PUSH_INDENT, ALIGN_INDENT, field.exprInit,
            POP_INDENT);
    write(field.annotations, field.srcSemicolon, DELIM_NEWLINE);
  }
  if (decl.stmtFinalize) {
    write(DELIM_NEWLINE, decl.srcKwFinalize, DELIM_SPACE, decl.stmtFinalize, DELIM_NEWLINE);
  }
  write(POP_INDENT, decl.srcBraceR, decl.srcSemicolon);
}

void Formatter::write(const AST::Variable &decl) {
  write(decl.type, DELIM_SPACE, PUSH_INDENT);
  auto moreThanOne{decl.declarators.size() > 1};
  auto delim{write_start_list(decl.declarators.size(),
                              decl.has_trailing_comma(),
                              /*alignIndent=*/moreThanOne)};
  for (const auto &each : decl.declarators) {
    if (!options.noAnnotations && each.annotations && moreThanOne)
      write(DELIM_NEWLINE);
    write(each.name);
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
    if (!options.noAnnotations && each.annotations && moreThanOne)
      write(DELIM_NEWLINE);
  }
  write(decl.srcSemicolon, POP_INDENT);
}
//--}

//--{ Write: Exprs
void Formatter::write(const AST::Expr &expr) {
  write_type_switch<AST::AccessField, AST::AccessIndex, AST::Binary, AST::Call,
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
  write_type_switch<AST::Break, AST::Compound, AST::Continue, AST::DeclStmt,
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
    if (!each.is_default())
      write(DELIM_SPACE, each.expr);
    write(each.srcColon);
    if (each.stmts.size() == 1 && !next_comment_forces_newline()) {
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
  if (!options.noAnnotations) {
    write(PUSH_INDENT, INCREMENT_INDENT, DELIM_UNNECESSARY_SPACE,
          annos.srcDoubleBrackL, PUSH_INDENT);
    auto delim{write_start_list(annos.size(), annos.has_trailing_comma())};
    for (const auto &[identifier, args, srcComma] : annos) {
      write(identifier, args, srcComma, srcComma.empty() ? DELIM_NONE : delim);
    }
    write(POP_INDENT, annos.srcDoubleBrackR, POP_INDENT);
  }
}

void Formatter::write(const AST::ArgumentList &args) {
  write(args.srcParenL, PUSH_INDENT);
  auto delim{write_start_list(args.size(), args.has_trailing_comma())};
  for (const auto &arg : args) {
    if (arg.is_visited())
      write(arg.srcKwVisit, DELIM_SPACE);
    if (arg.is_named())
      write(arg.name, arg.srcColonAfterName, DELIM_SPACE);
    write(arg.expr, arg.srcComma, arg.srcComma.empty() ? DELIM_NONE : delim);
  }
  write(POP_INDENT, args.srcParenR);
}

void Formatter::write(const AST::ParameterList &params) {
  write(params.srcParenL, PUSH_INDENT);
  if (params.is_variant()) {
    write(params.srcStar);
  } else {
    auto delim{write_start_list(params.size(), params.has_trailing_comma())};
    for (const auto &param : params) {
      if (!options.noAnnotations && param.annotations && &param != &params[0]) {
        write(DELIM_NEWLINE);
      }
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
      if (!options.noAnnotations && param.annotations) {
        write(DELIM_NEWLINE);
      }
    }
  }
  write(POP_INDENT, params.srcParenR);
}

} // namespace smdl
