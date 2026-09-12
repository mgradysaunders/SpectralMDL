// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#include "Formatter.h"

#include <cmath>
#include <cstdio>

#include "llvm/ADT/APFloat.h"

namespace smdl {

void Formatter::alignLineComments() {
  auto insertSpacesBeforeComment{[&](auto &comment, size_t numSpaces) {
    mOutputSrc.insert(comment.i, numSpaces, ' ');
    for (auto itr{&comment};
         itr < mLineCommentsToAlign.data() + mLineCommentsToAlign.size();
         ++itr) {
      itr->i += numSpaces;
      itr->column += numSpaces;
    }
  }};
  for (auto itr{mLineCommentsToAlign.rbegin()};
       itr != mLineCommentsToAlign.rend();) {
    size_t maxColumn{itr->column};
    auto itrPrev{itr};
    auto itrNext{itr};
    itrNext++;
    while (itrNext != mLineCommentsToAlign.rend()) {
      const size_t &iPrev{itrPrev->i};
      const size_t &iNext{itrNext->i};
      if (llvm::StringRef src{
              llvm::StringRef(mOutputSrc.data() + iNext, iPrev - iNext)};
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
  llvm::StringRef inSrc{mInputSrc};
  while (!inSrc.empty()) {
    llvm::StringRef ws{inSrc.take_while(isSpace)};
    inSrc = inSrc.drop_front(ws.size());
    if (inSrc.starts_with("//")) {
      size_t pos{inSrc.find('\n')};
      if (keepComment(pos == inSrc.npos ? inSrc : inSrc.take_front(pos))) {
        return true;
      }
      // Step over the dropped line comment.
      if (pos == inSrc.npos) break;
      inSrc = inSrc.drop_front(pos);
    } else if (inSrc.starts_with("/*")) {
      size_t pos{inSrc.find("*/", 2)};
      if (pos == inSrc.npos) break; // Shouldn't happen?
      if (keepComment(inSrc.take_front(pos + 2))) {
        if (ws.count('\n') > 0) {
          return true;
        }
        inSrc = inSrc.drop_front(pos + 2);
        if (inSrc.take_while(isSpace).count('\n') > 0) {
          return true;
        }
      } else {
        // Step over the dropped multiline comment.
        inSrc = inSrc.drop_front(pos + 2);
      }
    } else {
      break; // Shouldn't happen?
    }
  }
  return false;
}

void Formatter::writeDelimNone() {
  size_t numNewLines{consumeInputSpace().count('\n')};
  if (llvm::StringRef comment{consumeInputComment()}; !comment.empty()) {
    // Preserve up to 1 extra newline
    if (keepComment(comment) && numNewLines == 1 && lastOutput() != '\n') {
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
  if (size_t numNewLines{consumeInputSpace().count('\n')}; numNewLines <= 1) {
    if (llvm::StringRef comment{consumeInputComment()}; !comment.empty()) {
      // Preserve up to 1 extra newline
      if (keepComment(comment) && numNewLines == 1 && lastOutput() != '\n') {
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
    if (llvm::StringRef comment{consumeInputComment()}; !comment.empty()) {
      writeComment(comment), writeMoreComments();
    }
  }
  if (mOptions.isCompact && lastOutput(-1) == '\n' && lastOutput(-2) == '\n') {
    mOutputSrc.pop_back();
  }
}

void Formatter::writeComment(llvm::StringRef inSrc) {
  if (!inSrc.empty() && !keepComment(inSrc)) {
    // Still consume the trailing space, or else `writeMoreComments()`
    // cannot see past the dropped comment to a doc comment that
    // follows it in the same gap, and the next `writeToken()` would
    // silently discard it!
    (void)consumeInputSpace();
    return;
  }
  if (!inSrc.empty()) {
    // This better be a line comment or a multiline comment! A line comment
    // only lacks the terminating newline if it ends the file.
    SMDL_SANITY_CHECK((inSrc.starts_with("//") &&
                       (inSrc.ends_with("\n") || mInputSrc.empty())) ||
                      (inSrc.starts_with("/*") && inSrc.ends_with("*/")));
    if (!mOutputSrc.empty() && lastOutput() != ' ' && lastOutput() != '\n') {
      mOutputSrc += ' ';
    }
    bool isNewLine{mOutputSrc.empty() || lastOutput() == '\n'};
    writeIndentIfNewLine();
    // Parse format on/off directives. This must happen after
    // `writeIndentIfNewLine()` so that `FormatOff::outputSrcPos` includes
    // the indentation of the `// smdl format off` comment itself.
    if (inSrc.starts_with("//") || !inSrc.contains('\n')) {
      llvm::StringRef text{inSrc.starts_with("//")
                               ? inSrc.drop_front(2).trim()
                               : inSrc.drop_front(2).drop_back(2).trim()};
      llvm::SmallVector<llvm::StringRef> tokens{};
      while (!text.empty() && tokens.size() < 3) {
        text = text.drop_while(isSpace);
        llvm::StringRef token{
            text.take_while([&](char ch) { return !isSpace(ch); })};
        if (!token.empty()) {
          tokens.push_back(token);
          text = text.drop_front(token.size());
        }
      }
      if (tokens.size() == 3 && tokens[0] == "smdl" && tokens[1] == "format") {
        if (tokens[2] == "off" && !mFormatOff)
          mFormatOff = FormatOff{inSrc.data(), mOutputSrc.size()};
        if (tokens[2] == "on" && mFormatOff) applyFormatOff(inSrc.data());
      }
    }
    // Remember line comments to align later, but only remember if not
    // disabled by `// smdl format off`!
    if (inSrc.starts_with("//") && !isNewLine && !mFormatOff) {
      mLineCommentsToAlign.push_back(
          {mOutputSrc.size(), static_cast<size_t>(currentColumn())});
    }
    mOutputSrc += inSrc;
    // Always consume the trailing space, or else `writeMoreComments()`
    // cannot see past it and the next `writeToken()` silently discards
    // whatever comments remain in the gap!
    size_t numNewLines{consumeInputSpace().count('\n')};
    if (!mOptions.isCompact && numNewLines > 0) {
      // Preserve up to 1 extra newline
      mOutputSrc += '\n';
    } else if (inSrc.starts_with("/*")) {
      // Guarantee a space after multi-line comment
      mOutputSrc += ' ';
    }
  }
}

void Formatter::writeToken(llvm::StringRef inSrc, llvm::StringRef outSrc) {
  if (!inSrc.empty()) {
    SMDL_SANITY_CHECK(mInputSrc.begin() <= inSrc.begin() &&
                      inSrc.end() <= mInputSrc.end());
    SMDL_SANITY_CHECK(inSrc.count('\n') == 0);
    SMDL_SANITY_CHECK(outSrc.count('\n') == 0);
    writeDelimNone();
    writeIndentIfNewLine();
    consumeInput(inSrc.begin() + inSrc.size() - mInputSrc.begin());
    mOutputSrc += outSrc;
  }
}

namespace {
[[nodiscard]] bool isFloatSuffix(char ch) {
  return ch == 'j' || ch == 'J' || ch == 'd' || ch == 'D' || //
         ch == 'f' || ch == 'F';
}

// Does the spelling parse back to exactly the given value? NOTE: This must use
// the same conversion as the parser, or else the guarantee is worthless!
[[nodiscard]] bool roundTripsTo(llvm::StringRef spelling, double value) {
  llvm::APFloat parsedValue(llvm::APFloat::IEEEdouble());
  llvm::Expected<llvm::APFloat::opStatus> opStatus{
      parsedValue.convertFromString(spelling,
                                    llvm::APFloat::rmNearestTiesToEven)};
  if (!opStatus) {
    llvm::consumeError(opStatus.takeError());
    return false;
  }
  return parsedValue.convertToDouble() == value;
}

// Find the shortest spelling of the floating point literal `srcValue` that
// still parses to exactly `value`. If there is no shorter spelling, return
// `srcValue` itself.
[[nodiscard]] std::string minifyFloatSpelling(llvm::StringRef srcValue,
                                              double value) {
  // Give up on anything that has no spelling as a literal at all. Infinity
  // is reachable because the parser only warns about overflow, and negative
  // values should be unreachable because negation is a unary expression,
  // but check for both anyway.
  if (!std::isfinite(value) || std::signbit(value)) return srcValue.str();
  // Split off the type suffix.
  size_t numChars{srcValue.size()};
  while (numChars > 0 && isFloatSuffix(srcValue[numChars - 1])) numChars--;
  llvm::StringRef suffix{srcValue.drop_front(numChars)};
  if (suffix.ends_with("f") || suffix.ends_with("F")) // Drop the default!
    suffix = suffix.drop_back(1);
  // Without a suffix, the spelling must contain a decimal point or an
  // exponent, or else it lexes as an integer literal instead!
  bool needsFloatMarker{suffix.empty()};
  // Find the fewest significant digits that round trip, then take the
  // digits and the decimal exponent of the leading digit.
  std::string digits{};
  int exponent{};
  if (value == 0.0) {
    digits = "0";
  } else {
    char buffer[32]{};
    for (int precision{1}; precision <= 17; precision++) {
      std::snprintf(buffer, sizeof(buffer), "%.*e", precision - 1, value);
      if (roundTripsTo(buffer, value)) break;
    }
    auto [srcMantissa, srcExponent] = llvm::StringRef(buffer).split('e');
    srcExponent.consume_front("+"); // Must remove! `getAsInteger()` chokes
    if (srcExponent.getAsInteger(10, exponent))
      return srcValue.str(); // Shouldn't happen!
    // Keep only the digits. Note that `%e` always normalizes to exactly
    // 1 digit before the decimal point, so throwing the decimal point away
    // does not lose anything, and doing it this way means it does not
    // matter what the current locale considers a decimal point to be
    for (char ch : srcMantissa)
      if (isDigit(ch)) digits += ch;
    while (digits.size() > 1 && digits.back() == '0') digits.pop_back();
  }
  int numDigits{int(digits.size())};
  // Spell it in fixed notation. Remember that the lexer requires a literal
  // to begin with a digit, so `0.5` cannot shrink to `.5`.
  std::string fixed{};
  if (exponent >= numDigits - 1) {
    fixed = digits;
    fixed.append(exponent - numDigits + 1, '0');
    if (needsFloatMarker) fixed += '.';
  } else if (exponent >= 0) {
    fixed = digits.substr(0, exponent + 1) + '.' + digits.substr(exponent + 1);
  } else {
    fixed = "0.";
    fixed.append(-exponent - 1, '0');
    fixed += digits;
  }
  // Spell it in scientific notation, where the exponent never needs a
  // plus sign and never needs leading zeros. Consider both putting the
  // decimal point after the leading digit and omitting it entirely, e.g.,
  // both `1.024e3` and `1024e0`, because which is shorter depends on how
  // many digits the exponent takes.
  std::string sci{
      numDigits == 1 ? digits : digits.substr(0, 1) + '.' + digits.substr(1)};
  sci += 'e';
  sci += std::to_string(exponent);
  std::string sciNoPoint{digits + 'e' +
                         std::to_string(exponent - numDigits + 1)};
  // Take the shortest, preferring fixed notation because it is easier to
  // read, then put the suffix back.
  std::string result{fixed};
  if (sci.size() < result.size()) result = sci;
  if (sciNoPoint.size() < result.size()) result = sciNoPoint;
  result += suffix;
  // Never write anything longer than what the author wrote, and never
  // write anything that has not been verified to parse back correctly!
  if (result.size() >= srcValue.size() || !isDigit(result[0]) ||
      !roundTripsTo(llvm::StringRef(result).drop_back(suffix.size()), value))
    return srcValue.str();
  return result;
}
} // namespace

void Formatter::writeMinifiedFloat(const AST::LiteralFloat &expr) {
  writeToken(expr.srcValue, minifyFloatSpelling(expr.srcValue, expr.value));
}

void Formatter::write(const AST::File &file) {
  write(DELIM_NONE);
  if (file.isSMDLSyntax()) {
    write(file.srcKwSmdlSyntax, DELIM_NEWLINE);
  }
  for (const auto &searchDir : file.searchDirs) {
    write(searchDir.srcKwSearchDir, DELIM_SPACE, searchDir.path, DELIM_NEWLINE);
  }
  if (file.version) {
    const AST::File::Version &version{*file.version};
    write(version.srcKwMdl, DELIM_SPACE, version.srcVersion,
          version.srcSemicolon, DELIM_NEWLINE);
  }
  for (const auto &decl : file.importDecls) {
    write(decl->attributes, decl->srcKwExport, DELIM_SPACE, decl,
          DELIM_NEWLINE);
  }
  if (!file.srcKwModule.empty() && !mOptions.shouldDropAnnotations) {
    // If removing annotations, skip the entire `module [[ ... ]];` because
    // the parser rejects `module;` without an annotation block!
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
  writeList(decl.declarators.size(), decl.hasTrailingComma(), /*canBreak=*/true,
            [&](Delim delim) {
              for (const auto &each : decl.declarators) {
                if (!mOptions.shouldDropAnnotations && each.annotations)
                  write(DELIM_NEWLINE);
                write(each.name);
                if (each.exprInit) {
                  write(DELIM_UNNECESSARY_SPACE, each.srcEqual,
                        DELIM_UNNECESSARY_SPACE, PUSH_INDENT);
                  if (delim == DELIM_NEWLINE) write(ALIGN_INDENT);
                  write(each.exprInit, POP_INDENT);
                }
                write(each.annotations, each.srcComma,
                      each.srcComma.empty() ? DELIM_NONE : delim);
                if (!mOptions.shouldDropAnnotations && each.annotations)
                  write(DELIM_NEWLINE);
              }
              write(delim);
            });
  write(POP_INDENT, decl.srcBraceR, decl.srcSemicolon);
}

void Formatter::write(const AST::Function &decl) {

  write(decl.returnType, decl.earlyAnnotations, DELIM_SPACE, decl.name,
        decl.params);
  if (!decl.srcFrequency.empty())
    write(DELIM_UNNECESSARY_SPACE, decl.srcFrequency);
  if (decl.lateAnnotations) write(decl.lateAnnotations);
  if (!decl.srcEqual.empty()) {
    write(DELIM_UNNECESSARY_SPACE, decl.srcEqual);
    writeAfterEqual(decl.definition, decl.srcSemicolon);
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
    writeList(decl.tags.size(), decl.hasTrailingCommaOnTags(),
              /*canBreak=*/false, [&](Delim delim) {
                for (const auto &tag : decl.tags)
                  write(tag.srcKwDefault,
                        tag.srcKwDefault.empty() ? DELIM_UNNECESSARY_SPACE
                                                 : DELIM_SPACE,
                        tag.type, tag.srcComma,
                        tag.srcComma.empty() ? DELIM_NONE : delim);
              });
    write(POP_INDENT);
  }
  write(decl.annotations, DELIM_UNNECESSARY_SPACE, decl.srcBraceL, PUSH_INDENT,
        INCREMENT_INDENT,
        decl.constructors.empty() && decl.fields.empty() ? DELIM_NONE
                                                         : DELIM_NEWLINE);
  for (const auto &constructor : decl.constructors) {
    write(constructor.name.srcName, constructor.params, DELIM_UNNECESSARY_SPACE,
          constructor.srcEqual);
    writeAfterEqual(constructor.expr, constructor.srcSemicolon);
    write(DELIM_NEWLINE);
  }
  for (const auto &field : decl.fields) {
    write(field.type, DELIM_SPACE, field.name);
    if (field.exprInit) {
      write(DELIM_UNNECESSARY_SPACE, field.srcEqual);
      writeAfterEqual(field.exprInit);
    }
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
  bool moreThanOne{decl.declarators.size() > 1};
  writeList(decl.declarators.size(), decl.hasTrailingComma(),
            /*canBreak=*/false, [&](Delim delim) {
              for (const auto &each : decl.declarators) {
                if (!mOptions.shouldDropAnnotations && each.annotations &&
                    moreThanOne)
                  write(DELIM_NEWLINE);
                if (!each.srcBraceL.empty()) write(DELIM_UNNECESSARY_SPACE);
                write(each.srcBraceL);
                for (const auto &[name, srcComma] : each.names) {
                  write(name, srcComma);
                  if (!srcComma.empty()) write(DELIM_UNNECESSARY_SPACE);
                }
                write(each.srcBraceR);
                if (each.exprInit) {
                  write(DELIM_UNNECESSARY_SPACE, each.srcEqual);
                  writeAfterEqual(each.exprInit);
                } else if (each.argsInit) {
                  write(each.argsInit);
                }
                write(each.annotations, each.srcComma,
                      each.srcComma.empty() ? DELIM_NONE : delim);
                if (!mOptions.shouldDropAnnotations && each.annotations &&
                    moreThanOne)
                  write(DELIM_NEWLINE);
              }
            });
  write(decl.srcSemicolon, POP_INDENT);
}
//--}

//--{ Write: Exprs
void Formatter::write(const AST::Expr &expr) {
  writeTypeSwitch<AST::AccessField, AST::AccessIndex, AST::Binary, AST::Call,
                  AST::Identifier, AST::Intrinsic, AST::Lambda, AST::Let,
                  AST::LiteralBool, AST::LiteralFloat, AST::LiteralInt,
                  AST::LiteralString, AST::Parens, AST::ReturnFrom, AST::Select,
                  AST::SizeName, AST::Type, AST::TypeCast, AST::Unary>(expr);
}

void Formatter::write(const AST::Lambda &expr) {
  // This mirrors `write(const AST::Function &)` minus the name,
  // annotations, frequency qualifier, semicolon, and return type, which
  // is implicitly `auto`.
  const AST::Function &decl{*expr.func};
  write(expr.srcBackslash, decl.params);
  if (!decl.srcEqual.empty()) {
    write(DELIM_UNNECESSARY_SPACE, decl.srcEqual);
    writeAfterEqual(decl.definition);
  } else {
    write(DELIM_UNNECESSARY_SPACE, decl.definition);
  }
}

void Formatter::write(const AST::Let &expr) {
  write(expr.srcKwLet, DELIM_SPACE);
  if (!expr.srcBraceL.empty()) {
    // The block, and the `in` expression after it, indent from the line the
    // `let` starts on, exactly like a function body, instead of trailing off
    // to the right of whatever the `let` follows.
    write(PUSH_INDENT);
    mIndent = currentLineIndent();
    write(expr.srcBraceL, PUSH_INDENT, INCREMENT_INDENT, DELIM_NEWLINE);
    for (const auto &decl : expr.decls) write(decl, DELIM_NEWLINE);
    write(POP_INDENT, expr.srcBraceR, DELIM_SPACE, expr.srcKwIn, DELIM_SPACE,
          expr.expr, POP_INDENT);
  } else {
    SMDL_SANITY_CHECK(expr.decls.size() == 1);
    write(expr.decls[0], DELIM_SPACE, expr.srcKwIn, DELIM_SPACE, expr.expr);
  }
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
    if (!each.isDefault()) write(DELIM_SPACE, each.expr);
    write(each.srcColon);
    if (each.stmts.size() == 1 && !nextCommentForcesNewLine()) {
      write(DELIM_SPACE, each.stmts.front(), DELIM_NEWLINE);
    } else {
      write(PUSH_INDENT, INCREMENT_INDENT, DELIM_NEWLINE);
      for (const auto &subStmt : each.stmts) write(subStmt, DELIM_NEWLINE);
      write(POP_INDENT);
    }
  }
  write(stmt.srcBraceR);
}
//--}

void Formatter::write(const AST::AnnotationBlock &annos) {
  if (!mOptions.shouldDropAnnotations) {
    write(PUSH_INDENT, INCREMENT_INDENT, DELIM_UNNECESSARY_SPACE,
          annos.srcDoubleBrackL, PUSH_INDENT);
    writeList(annos.size(), annos.hasTrailingComma(), /*canBreak=*/true,
              [&](Delim delim) {
                for (const auto &[identifier, args, srcComma] : annos)
                  write(identifier, args, srcComma,
                        srcComma.empty() ? DELIM_NONE : delim);
              });
    write(POP_INDENT, annos.srcDoubleBrackR, POP_INDENT);
  }
}

namespace {
// Is the expression a literal constant, possibly signed?
[[nodiscard]] bool isLiteral(const AST::Expr &expr) {
  if (const AST::Unary *unary{llvm::dyn_cast<AST::Unary>(&expr)};
      unary && (unary->op == AST::UNOP_POS || unary->op == AST::UNOP_NEG))
    return isLiteral(*unary->expr);
  return llvm::isa<AST::LiteralBool, AST::LiteralFloat, AST::LiteralInt,
                   AST::LiteralString>(expr);
}

// Is the argument list nothing but literal constants? Such a list is data,
// and how data is laid out belongs to whoever wrote it, so the formatter
// never breaks one to fit. A table of rows stays a table of rows.
[[nodiscard]] bool isDataList(const AST::ArgumentList &args) {
  return args.size() > 0 && llvm::all_of(args, [](const AST::Argument &arg) {
           return arg.isPositional() && arg.expr && isLiteral(*arg.expr);
         });
}
} // namespace

void Formatter::write(const AST::ArgumentList &args) {
  write(args.srcParenL, PUSH_INDENT);
  writeList(args.size(), args.hasTrailingComma(),
            /*canBreak=*/!isDataList(args), [&](Delim delim) {
              for (const auto &arg : args) {
                if (arg.isVisited()) write(arg.srcKwVisit, DELIM_SPACE);
                if (arg.isInlined()) write(arg.srcKwInline, DELIM_SPACE);
                if (arg.isNamed())
                  write(arg.name, arg.srcColonAfterName, DELIM_SPACE);
                write(arg.expr, arg.srcComma,
                      arg.srcComma.empty() ? DELIM_NONE : delim);
              }
            });
  write(POP_INDENT, args.srcParenR);
}

void Formatter::write(const AST::ParameterList &params) {
  write(params.srcParenL, PUSH_INDENT);
  if (params.isVariant()) {
    write(params.srcStar);
  } else {
    writeList(params.size(), params.hasTrailingComma(), /*canBreak=*/true,
              [&](Delim delim) {
                for (const auto &param : params) {
                  write(param.type, DELIM_SPACE, param.name);
                  // A default is never worth a line break of its own: it
                  // would leave the parameter it belongs to dangling.
                  if (param.exprInit) {
                    write(DELIM_UNNECESSARY_SPACE, param.srcEqual,
                          DELIM_UNNECESSARY_SPACE, PUSH_INDENT);
                    if (delim == DELIM_NEWLINE) write(ALIGN_INDENT);
                    write(param.exprInit, POP_INDENT);
                  }
                  write(param.annotations, param.srcComma,
                        param.srcComma.empty() ? DELIM_NONE : delim);
                }
                write(params.srcEllipsis);
              });
  }
  write(POP_INDENT, params.srcParenR);
}

} // namespace smdl
