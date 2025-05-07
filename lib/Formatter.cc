// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#include "Formatter.h"

namespace smdl {

void Formatter::realign_end_of_line_comments() {
  auto insertSpacesBeforeComment{[&](auto &comment, size_t n) {
    for (size_t i = 0; i < n; i++)
      outputSrc.insert(outputSrc.begin() + comment.first, ' ');
    for (auto itr = &comment;
         itr < outputLineComments.data() + outputLineComments.size(); ++itr) {
      itr->first += n;
      itr->second += n;
    }
  }};
  for (auto itr1{outputLineComments.rbegin()};
       itr1 != outputLineComments.rend();) {
    auto column{itr1->second};
    auto itrPrev{itr1};
    auto itr2{itr1};
    itr2++;
    while (itr2 != outputLineComments.rend()) {
      auto src{llvm::StringRef(outputSrc.data() + itr2->first,
                               itrPrev->first - itr2->first)};
      if (src.count('\n') > 1)
        break;
      column = std::max(column, itr2->second);
      itrPrev = itr2;
      ++itr2;
    }
    while (itr1 != itr2) {
      insertSpacesBeforeComment(*itr1, column - itr1->second);
      ++itr1;
    }
  }
}

void Formatter::write_comments() {
  auto firstWhitespace{consume_input_whitespace()};
  auto firstComment{consume_input_comment()};
  if (!firstComment.empty()) {
    if (firstWhitespace.count('\n') > 0) {
      delim_newline();
    } else {
      delim_space();
    }
    bool isNewLine{last_output() == '\n'};
    write_indent_if_newline();
    if (!isNewLine && firstComment.starts_with("//"))
      outputLineComments.emplace_back(outputSrc.size(),
                                      current_output_column());
    outputSrc += std::string_view(firstComment);
    if (firstComment.starts_with("/*"))
      delim_space();
    while (true) {
      auto nextWhitespace{consume_input_whitespace()};
      auto nextComment{consume_input_comment()};
      if (nextComment.empty())
        return;
      isNewLine = last_output() == '\n';
      write_indent_if_newline();
      if (!isNewLine && nextComment.starts_with("//"))
        outputLineComments.emplace_back(outputSrc.size(),
                                        current_output_column());
      outputSrc += std::string_view(nextComment);
    }
  }
}

void Formatter::write_token(llvm::StringRef inSrc) {
  if (!inSrc.empty()) {
    write_comments();
    SMDL_SANITY_CHECK(inputSrc.begin() <= inSrc.begin() &&
                      inSrc.end() <= inputSrc.end());
    SMDL_SANITY_CHECK(inSrc.count('\n') == 0);
    write_indent_if_newline();
    consume_input(inSrc.begin() + inSrc.size() - inputSrc.begin());
    outputSrc += std::string_view(inSrc);
  }
}

void Formatter::write(const AST::File &file) {
  if (file.is_smdl_syntax()) {
    write(file.srcKwSmdlSyntax, DELIM_NEWLINE);
  }
  if (file.version) {
    auto &version{*file.version};
    write(version.srcKwMdl, DELIM_SPACE, version.srcVersion,
          version.srcSemicolon, DELIM_NEWLINE);
  }
  for (const auto &decl : file.importDecls) {
    write(decl, DELIM_NEWLINE);
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

void Formatter::write(const AST::Decl &decl) {
  write_type_switch<AST::Enum, AST::Function, AST::Import, AST::Struct,
                    AST::Tag, AST::Typedef, AST::UnitTest, AST::UsingAlias,
                    AST::UsingImport, AST::Variable>(decl);
}

void Formatter::write(const AST::Enum &decl) {
  write(decl.srcKwEnum, DELIM_SPACE, decl.name, decl.annotations, DELIM_SPACE,
        decl.srcBraceL, DELIM_SPACE, PUSH_INDENT);
  auto delim{start_list(decl.has_trailing_comma())};
  for (const auto &each : decl.declarators) {
    write(each.name);
    if (each.exprInit) {
      write(DELIM_SPACE, each.srcEqual, DELIM_SPACE, PUSH_INDENT);
      if (delim == DELIM_NEWLINE)
        write(ALIGN_INDENT);
      write(each.exprInit, POP_INDENT);
    }
    write(each.annotations, each.srcComma,
          each.srcComma.empty() ? DELIM_NONE : delim);
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
    write(attributes.srcParenR, POP_INDENT, DELIM_SPACE);
  }
  write(decl.returnType, DELIM_SPACE, decl.earlyAnnotations, decl.name,
        decl.params, DELIM_SPACE, decl.srcFrequency, decl.lateAnnotations);
  if (!decl.srcEqual.empty()) {
    write(PUSH_INDENT, INCREMENT_INDENT, DELIM_SPACE, decl.srcEqual,
          DELIM_SPACE, PUSH_INDENT, ALIGN_INDENT, decl.definition,
          decl.srcSemicolon, POP_INDENT, POP_INDENT);
  } else {
    write(DELIM_SPACE, decl.definition);
  }
}

void Formatter::write(const AST::Struct &decl) {
  write(decl.srcKwStruct, DELIM_SPACE, decl.name);
  if (!decl.srcColonBeforeTags.empty()) {
    write(decl.srcColonBeforeTags, DELIM_SPACE, PUSH_INDENT);
    auto delim{start_list(decl.has_trailing_comma_on_tags())};
    for (const auto &[srcKwDefault, type, srcComma] : decl.tags) {
      write(srcKwDefault, DELIM_SPACE, type, srcComma,
            srcComma.empty() ? DELIM_NONE : delim);
    }
    write(POP_INDENT);
  }
  write(decl.annotations, DELIM_SPACE, decl.srcBraceL, PUSH_INDENT,
        INCREMENT_INDENT, DELIM_NEWLINE);
  for (const auto &field : decl.fields) {
    write(field.type, DELIM_SPACE, field.name);
    if (field.exprInit)
      write(DELIM_SPACE, field.srcEqual, //
            DELIM_SPACE, PUSH_INDENT, ALIGN_INDENT, field.exprInit, POP_INDENT);
    write(field.annotations, field.srcSemicolon, DELIM_NEWLINE);
  }
  if (decl.stmtFinalize)
    write(decl.srcKwFinalize, DELIM_SPACE, decl.stmtFinalize, DELIM_NEWLINE);
  write(POP_INDENT, decl.srcBraceR, decl.srcSemicolon);
}

void Formatter::write(const AST::Variable &decl) {
  write(decl.type, DELIM_SPACE, PUSH_INDENT);
  auto delim{start_list(decl.has_trailing_comma())};
  for (const auto &each : decl.declarators) {
    write(each.name);
    if (each.exprInit) {
      write(DELIM_SPACE, each.srcEqual, DELIM_SPACE, PUSH_INDENT);
      if (delim == DELIM_NEWLINE)
        write(ALIGN_INDENT);
      write(each.exprInit, POP_INDENT);
    } else if (each.argsInit) {
      write(each.argsInit);
    }
    write(each.annotations, each.srcComma,
          each.srcComma.empty() ? DELIM_NONE : delim);
  }
  write(decl.srcSemicolon, POP_INDENT);
}

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

void Formatter::write(const AST::Stmt &stmt) {
  write_type_switch<AST::Break, AST::Compound, AST::Continue, AST::DeclStmt,
                    AST::Defer, AST::DoWhile, AST::ExprStmt, AST::For, AST::If,
                    AST::Preserve, AST::Return, AST::Switch, AST::Unreachable,
                    AST::Visit, AST::While>(stmt);
}

void Formatter::write(const AST::For &stmt) {
  write(stmt.srcKwFor, DELIM_SPACE, stmt.srcParenL, PUSH_INDENT, ALIGN_INDENT,
        stmt.stmtInit, DELIM_SPACE, stmt.exprCond, stmt.srcSemicolonAfterCond,
        DELIM_SPACE, stmt.exprNext, POP_INDENT, stmt.srcParenR);
  if (llvm::isa<AST::Compound>(stmt.stmtLoop)) {
    write(DELIM_SPACE, stmt.stmtLoop);
  } else {
    write(PUSH_INDENT, INCREMENT_INDENT, DELIM_NEWLINE, stmt.stmtLoop,
          POP_INDENT, DELIM_NEWLINE);
  }
}

void Formatter::write(const AST::If &stmt) {
  write(stmt.srcKwIf, DELIM_SPACE, stmt.expr);
  if (llvm::isa<AST::Compound>(stmt.stmtThen)) {
    write(DELIM_SPACE, stmt.stmtThen);
  } else {
    write(PUSH_INDENT, INCREMENT_INDENT, DELIM_NEWLINE, stmt.stmtThen,
          POP_INDENT, DELIM_NEWLINE);
  }
  if (stmt.stmtElse) {
    if (llvm::isa<AST::Compound>(stmt.stmtThen)) {
      write(DELIM_SPACE);
    } else {
      write(DELIM_NEWLINE);
    }
    write(stmt.srcKwElse);
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
  write(stmt.srcKwSwitch, DELIM_SPACE, stmt.expr, DELIM_SPACE, stmt.srcBraceL,
        DELIM_NEWLINE);
  for (const auto &each : stmt.cases) {
    write(each.srcKwCaseOrDefault);
    if (!each.is_default())
      write(DELIM_SPACE, each.expr);
    write(each.srcColon);
    if (each.stmts.size() == 1 &&
        llvm::isa<AST::Compound>(each.stmts.front())) {
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

void Formatter::write(const AST::AnnotationBlock &annos) {
  write(PUSH_INDENT, INCREMENT_INDENT, DELIM_SPACE, annos.srcDoubleBrackL,
        PUSH_INDENT);
  auto delim{start_list(annos.has_trailing_comma())};
  for (const auto &[identifier, args, srcComma] : annos.annotations)
    write(identifier, args, srcComma, srcComma.empty() ? DELIM_NONE : delim);
  write(POP_INDENT, annos.srcDoubleBrackR, POP_INDENT);
}

void Formatter::write(const AST::ArgumentList &args) {
  write(args.srcParenL, PUSH_INDENT);
  auto delim{start_list(args.has_trailing_comma())};
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
  auto delim{start_list(params.has_trailing_comma())};
  for (const auto &param : params) {
    if (param.annotations)
      write(DELIM_NEWLINE);
    write(param.type, DELIM_SPACE, param.name);
    if (param.exprInit) {
      write(DELIM_SPACE, param.srcEqual, DELIM_SPACE, PUSH_INDENT);
      if (delim == DELIM_NEWLINE)
        write(ALIGN_INDENT);
      write(param.exprInit, POP_INDENT);
    }
    write(param.annotations, param.srcComma,
          param.srcComma.empty() ? DELIM_NONE : delim);
    if (param.annotations)
      write(DELIM_NEWLINE);
  }
  write(POP_INDENT, params.srcParenR);
}

} // namespace smdl
