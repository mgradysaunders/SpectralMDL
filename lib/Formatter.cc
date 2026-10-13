// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#include "Formatter.h"

#include <cmath>
#include <cstdio>

#include "llvm/ADT/APFloat.h"

namespace smdl {

void Formatter::alignLineComments_() {
  auto insertSpacesBeforeComment{[&](auto &comment, size_t numSpaces) {
    outputSrc_.insert(comment.i, numSpaces, ' ');
    for (auto itr{&comment};
         itr < lineCommentsToAlign_.data() + lineCommentsToAlign_.size();
         ++itr) {
      itr->i += numSpaces;
      itr->column += numSpaces;
    }
  }};
  for (auto itr{lineCommentsToAlign_.rbegin()};
       itr != lineCommentsToAlign_.rend();) {
    size_t maxColumn{itr->column};
    auto itrPrev{itr};
    auto itrNext{itr};
    itrNext++;
    while (itrNext != lineCommentsToAlign_.rend()) {
      const size_t &iPrev{itrPrev->i};
      const size_t &iNext{itrNext->i};
      if (llvm::StringRef src{
              llvm::StringRef(outputSrc_.data() + iNext, iPrev - iNext)};
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

bool Formatter::nextCommentForcesNewLine_() const {
  llvm::StringRef inSrc{inputSrc_};
  while (!inSrc.empty()) {
    llvm::StringRef ws{inSrc.take_while(isSpace)};
    inSrc = inSrc.drop_front(ws.size());
    if (inSrc.starts_with("//")) {
      size_t pos{inSrc.find('\n')};
      if (keepComment_(pos == inSrc.npos ? inSrc : inSrc.take_front(pos))) {
        return true;
      }
      // Step over the dropped line comment.
      if (pos == inSrc.npos) break;
      inSrc = inSrc.drop_front(pos);
    } else if (inSrc.starts_with("/*")) {
      size_t length{blockCommentLength(inSrc)};
      if (length == std::string_view::npos) break; // Shouldn't happen?
      if (keepComment_(inSrc.take_front(length))) {
        if (ws.count('\n') > 0) {
          return true;
        }
        inSrc = inSrc.drop_front(length);
        if (inSrc.take_while(isSpace).count('\n') > 0) {
          return true;
        }
      } else {
        // Step over the dropped multiline comment.
        inSrc = inSrc.drop_front(length);
      }
    } else {
      break; // Shouldn't happen?
    }
  }
  return false;
}

void Formatter::writeDelimNone_() {
  size_t numNewLines{consumeInputSpace_().count('\n')};
  if (llvm::StringRef comment{consumeInputComment_()}; !comment.empty()) {
    // Preserve up to 1 extra newline
    if (keepComment_(comment) && numNewLines == 1 && lastOutput_() != '\n') {
      outputSrc_ += '\n';
    }
    writeComment_(comment), writeMoreComments_();
  }
}

void Formatter::writeDelimSpace_() {
  writeDelimNone_();
  if (!outputSrc_.empty() && lastOutput_() != ' ' && lastOutput_() != '\n') {
    outputSrc_ += ' ';
  }
}

void Formatter::writeDelimNewLine_() {
  while (lastOutput_() == ' ') // Remove spaces
    outputSrc_.pop_back();
  if (size_t numNewLines{consumeInputSpace_().count('\n')}; numNewLines <= 1) {
    if (llvm::StringRef comment{consumeInputComment_()}; !comment.empty()) {
      // Preserve up to 1 extra newline
      if (keepComment_(comment) && numNewLines == 1 && lastOutput_() != '\n') {
        outputSrc_ += '\n';
      }
      writeComment_(comment), writeMoreComments_();
    }
    if (!outputSrc_.empty() && lastOutput_() != '\n') {
      outputSrc_ += '\n';
    }
  } else {
    // Preserve 1 extra newline
    while (lastOutput_(-1) != '\n' || lastOutput_(-2) != '\n') {
      outputSrc_ += '\n';
    }
    if (llvm::StringRef comment{consumeInputComment_()}; !comment.empty()) {
      writeComment_(comment), writeMoreComments_();
    }
  }
  if (options_.isCompact && lastOutput_(-1) == '\n' &&
      lastOutput_(-2) == '\n') {
    outputSrc_.pop_back();
  }
}

void Formatter::writeComment_(llvm::StringRef inSrc) {
  if (!inSrc.empty() && !keepComment_(inSrc)) {
    // Still consume the trailing space, or else `writeMoreComments_()`
    // cannot see past the dropped comment to a doc comment that
    // follows it in the same gap, and the next `writeToken_()` would
    // silently discard it!
    (void)consumeInputSpace_();
    return;
  }
  if (!inSrc.empty()) {
    // This better be a line comment or a multiline comment! A line comment
    // only lacks the terminating newline if it ends the file.
    SMDL_SANITY_CHECK((inSrc.starts_with("//") &&
                       (inSrc.ends_with("\n") || inputSrc_.empty())) ||
                      (inSrc.starts_with("/*") && inSrc.ends_with("*/")));
    if (!outputSrc_.empty() && lastOutput_() != ' ' && lastOutput_() != '\n') {
      outputSrc_ += ' ';
    }
    bool isNewLine{outputSrc_.empty() || lastOutput_() == '\n'};
    writeIndentIfNewLine_();
    // Parse format on/off directives. This must happen after
    // `writeIndentIfNewLine_()` so that `FormatOff::outputSrcPos` includes
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
        if (tokens[2] == "off" && !formatOff_)
          formatOff_ = FormatOff{inSrc.data(), outputSrc_.size()};
        if (tokens[2] == "on" && formatOff_) applyFormatOff_(inSrc.data());
      }
    }
    // Remember line comments to align later, but only remember if not
    // disabled by `// smdl format off`!
    if (inSrc.starts_with("//") && !isNewLine && !formatOff_) {
      lineCommentsToAlign_.push_back(
          {outputSrc_.size(), static_cast<size_t>(currentColumn_())});
    }
    outputSrc_ += inSrc;
    // Always consume the trailing space, or else `writeMoreComments_()`
    // cannot see past it and the next `writeToken_()` silently discards
    // whatever comments remain in the gap!
    size_t numNewLines{consumeInputSpace_().count('\n')};
    if (!options_.isCompact && numNewLines > 0) {
      // Preserve up to 1 extra newline
      outputSrc_ += '\n';
    } else if (inSrc.starts_with("/*")) {
      // Guarantee a space after multi-line comment
      outputSrc_ += ' ';
    }
  }
}

void Formatter::writeToken_(llvm::StringRef inSrc, llvm::StringRef outSrc) {
  if (!inSrc.empty()) {
    SMDL_SANITY_CHECK(inputSrc_.begin() <= inSrc.begin() &&
                      inSrc.end() <= inputSrc_.end());
    SMDL_SANITY_CHECK(inSrc.count('\n') == 0);
    SMDL_SANITY_CHECK(outSrc.count('\n') == 0);
    writeDelimNone_();
    writeIndentIfNewLine_();
    consumeInput_(inSrc.begin() + inSrc.size() - inputSrc_.begin());
    outputSrc_ += outSrc;
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
  // Spell it in fixed notation, keeping the leading digit. A literal may
  // begin with its decimal point, but nothing that begins otherwise than
  // with a digit is written (see below), so `0.5` stays `0.5`.
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

void Formatter::writeMinifiedFloat_(const ast::LiteralFloat &expr) {
  writeToken_(expr.srcValue, minifyFloatSpelling(expr.srcValue, expr.value));
}

void Formatter::write_(const ast::File &file) {
  write_(DELIM_NONE);
  if (file.isSMDLSyntax()) {
    write_(file.srcKwSMDLSyntax, DELIM_NEWLINE);
  }
  for (const auto &searchDir : file.searchDirs) {
    write_(searchDir.srcKwSearchDir, DELIM_SPACE, searchDir.path,
           DELIM_NEWLINE);
  }
  if (file.version) {
    const ast::File::Version &version{*file.version};
    write_(version.srcKwMDL, DELIM_SPACE, version.srcVersion,
           version.srcSemicolon, DELIM_NEWLINE);
  }
  for (const auto &decl : file.importDecls) {
    write_(decl->attributes, decl->srcKwExport, DELIM_SPACE, decl,
           DELIM_NEWLINE);
  }
  if (!file.srcKwModule.empty() && !options_.shouldDropAnnotations) {
    // If removing annotations, skip the entire `module [[ ... ]];` because
    // the parser rejects `module;` without an annotation block!
    write_(file.srcKwModule, file.moduleAnnotations,
           file.srcSemicolonAfterModule, DELIM_NEWLINE);
  }
  for (const auto &decl : file.globalDecls) {
    write_(decl->attributes, decl->srcKwExport, DELIM_SPACE,
           decl->srcKwDeclarative, DELIM_SPACE, decl, DELIM_NEWLINE);
  }
  write_(DELIM_NEWLINE);
}

//--{ Write: Decls
void Formatter::write_(const ast::Decl &decl) {
  writeTypeSwitch_<ast::AnnotationDecl, ast::Enum, ast::Exec, ast::Function,
                   ast::Import, ast::Namespace, ast::Struct,
                   ast::StructCategory, ast::Tag, ast::Typedef, ast::UnitTest,
                   ast::UsingAlias, ast::UsingImport, ast::Variable>(decl);
}

void Formatter::write_(const ast::Enum &decl) {
  write_(decl.srcKwEnum, DELIM_SPACE, decl.name, decl.annotations,
         DELIM_UNNECESSARY_SPACE, decl.srcBraceL, DELIM_UNNECESSARY_SPACE,
         PUSH_INDENT);
  writeList_(decl.declarators.size(), decl.hasTrailingComma(),
             /*canBreak=*/true, [&](Delim delim) {
               for (const auto &each : decl.declarators) {
                 if (!options_.shouldDropAnnotations && each.annotations)
                   write_(DELIM_NEWLINE);
                 write_(each.name);
                 if (each.exprInit) {
                   write_(DELIM_UNNECESSARY_SPACE, each.srcEqual,
                          DELIM_UNNECESSARY_SPACE, PUSH_INDENT);
                   if (delim == DELIM_NEWLINE) write_(ALIGN_INDENT);
                   write_(each.exprInit, POP_INDENT);
                 }
                 write_(each.annotations, each.srcComma,
                        each.srcComma.empty() ? DELIM_NONE : delim);
                 if (!options_.shouldDropAnnotations && each.annotations)
                   write_(DELIM_NEWLINE);
               }
               write_(delim);
             });
  write_(POP_INDENT, decl.srcBraceR, decl.srcSemicolon);
}

void Formatter::write_(const ast::Function &decl) {

  write_(decl.returnType, decl.earlyAnnotations, DELIM_SPACE, decl.name,
         decl.params);
  if (!decl.srcFrequency.empty())
    write_(DELIM_UNNECESSARY_SPACE, decl.srcFrequency);
  if (decl.lateAnnotations) write_(decl.lateAnnotations);
  if (!decl.srcEqual.empty()) {
    write_(DELIM_UNNECESSARY_SPACE, decl.srcEqual);
    writeAfterEqual_(decl.definition, decl.srcSemicolon);
  } else if (decl.definition) {
    write_(DELIM_UNNECESSARY_SPACE, decl.definition);
  } else {
    write_(decl.srcSemicolon);
  }
}

void Formatter::write_(const ast::Struct &decl) {
  write_(decl.srcKwStruct, DELIM_SPACE, decl.name);
  if (!decl.srcKwIn.empty())
    write_(DELIM_SPACE, decl.srcKwIn, DELIM_SPACE, decl.category);
  if (!decl.srcColonBeforeTags.empty()) {
    write_(decl.srcColonBeforeTags, DELIM_UNNECESSARY_SPACE, PUSH_INDENT);
    writeList_(decl.tags.size(), decl.hasTrailingCommaOnTags(),
               /*canBreak=*/false, [&](Delim delim) {
                 for (const auto &tag : decl.tags)
                   write_(tag.srcKwDefault,
                          tag.srcKwDefault.empty() ? DELIM_UNNECESSARY_SPACE
                                                   : DELIM_SPACE,
                          tag.type, tag.srcComma,
                          tag.srcComma.empty() ? DELIM_NONE : delim);
               });
    write_(POP_INDENT);
  }
  write_(decl.annotations, DELIM_UNNECESSARY_SPACE, decl.srcBraceL, PUSH_INDENT,
         INCREMENT_INDENT,
         decl.constructors.empty() && decl.fields.empty() ? DELIM_NONE
                                                          : DELIM_NEWLINE);
  for (const auto &constructor : decl.constructors) {
    write_(constructor.name.srcName, constructor.params,
           DELIM_UNNECESSARY_SPACE, constructor.srcEqual);
    writeAfterEqual_(constructor.expr, constructor.srcSemicolon);
    write_(DELIM_NEWLINE);
  }
  for (const auto &field : decl.fields) {
    write_(field.type, DELIM_SPACE, field.name);
    if (field.exprInit) {
      write_(DELIM_UNNECESSARY_SPACE, field.srcEqual);
      writeAfterEqual_(field.exprInit);
    }
    write_(field.annotations, field.srcSemicolon, DELIM_NEWLINE);
  }
  if (decl.stmtFinalize) {
    write_(DELIM_NEWLINE, decl.srcKwFinalize, DELIM_SPACE, decl.stmtFinalize,
           DELIM_NEWLINE);
  }
  write_(POP_INDENT, decl.srcBraceR, decl.srcSemicolon);
}

void Formatter::write_(const ast::Variable &decl) {
  write_(decl.type, DELIM_SPACE, PUSH_INDENT);
  bool moreThanOne{decl.declarators.size() > 1};
  writeList_(decl.declarators.size(), decl.hasTrailingComma(),
             /*canBreak=*/false, [&](Delim delim) {
               for (const auto &each : decl.declarators) {
                 if (!options_.shouldDropAnnotations && each.annotations &&
                     moreThanOne)
                   write_(DELIM_NEWLINE);
                 if (!each.srcBraceL.empty()) write_(DELIM_UNNECESSARY_SPACE);
                 write_(each.srcBraceL);
                 for (const auto &[name, srcComma] : each.names) {
                   write_(name, srcComma);
                   if (!srcComma.empty()) write_(DELIM_UNNECESSARY_SPACE);
                 }
                 write_(each.srcBraceR);
                 if (each.exprInit) {
                   write_(DELIM_UNNECESSARY_SPACE, each.srcEqual);
                   writeAfterEqual_(each.exprInit);
                 } else if (each.argsInit) {
                   write_(each.argsInit);
                 }
                 write_(each.annotations, each.srcComma,
                        each.srcComma.empty() ? DELIM_NONE : delim);
                 if (!options_.shouldDropAnnotations && each.annotations &&
                     moreThanOne)
                   write_(DELIM_NEWLINE);
               }
             });
  write_(decl.srcSemicolon, POP_INDENT);
}
//--}

//--{ Write: Exprs
void Formatter::write_(const ast::Expr &expr) {
  writeTypeSwitch_<ast::AccessField, ast::AccessIndex, ast::Binary, ast::Call,
                   ast::Identifier, ast::Intrinsic, ast::Lambda, ast::Let,
                   ast::LiteralBool, ast::LiteralFloat, ast::LiteralInt,
                   ast::LiteralString, ast::OperatorCall, ast::Parens,
                   ast::ReturnFrom, ast::Select, ast::SizeName, ast::Type,
                   ast::TypeCast, ast::Unary>(expr);
}

void Formatter::write_(const ast::Lambda &expr) {
  // This mirrors `write_(const ast::Function &)` minus the name,
  // annotations, frequency qualifier, semicolon, and return type, which
  // is implicitly `auto`.
  const ast::Function &decl{*expr.func};
  write_(expr.srcBackslash, decl.params);
  if (!decl.srcEqual.empty()) {
    write_(DELIM_UNNECESSARY_SPACE, decl.srcEqual);
    writeAfterEqual_(decl.definition);
  } else {
    write_(DELIM_UNNECESSARY_SPACE, decl.definition);
  }
}

void Formatter::write_(const ast::Let &expr) {
  write_(expr.srcKwLet, DELIM_SPACE);
  if (!expr.srcBraceL.empty()) {
    // The block, and the `in` expression after it, indent from the line the
    // `let` starts on, exactly like a function body, instead of trailing off
    // to the right of whatever the `let` follows.
    write_(PUSH_INDENT);
    indent_ = currentLineIndent_();
    write_(expr.srcBraceL, PUSH_INDENT, INCREMENT_INDENT, DELIM_NEWLINE);
    for (const auto &decl : expr.decls) write_(decl, DELIM_NEWLINE);
    write_(POP_INDENT, expr.srcBraceR, DELIM_SPACE, expr.srcKwIn, DELIM_SPACE,
           expr.expr, POP_INDENT);
  } else {
    SMDL_SANITY_CHECK(expr.decls.size() == 1);
    write_(expr.decls[0], DELIM_SPACE, expr.srcKwIn, DELIM_SPACE, expr.expr);
  }
}
//--}

//--{ Write: Stmts
void Formatter::write_(const ast::Stmt &stmt) {
  writeTypeSwitch_<ast::Break, ast::Compound, ast::Continue, ast::DeclStmt,
                   ast::Defer, ast::DoWhile, ast::ExprStmt, ast::For, ast::If,
                   ast::Preserve, ast::Return, ast::Switch, ast::Unreachable,
                   ast::Visit, ast::While>(stmt);
}

void Formatter::write_(const ast::For &stmt) {
  write_(stmt.srcKwFor, DELIM_UNNECESSARY_SPACE, stmt.srcParenL, PUSH_INDENT,
         ALIGN_INDENT, stmt.stmtInit, DELIM_UNNECESSARY_SPACE, stmt.exprCond,
         stmt.srcSemicolonAfterCond, DELIM_UNNECESSARY_SPACE, stmt.exprNext,
         POP_INDENT, stmt.srcParenR);
  if (llvm::isa<ast::Compound>(stmt.stmtLoop)) {
    write_(DELIM_UNNECESSARY_SPACE, stmt.stmtLoop);
  } else {
    write_(PUSH_INDENT, INCREMENT_INDENT, DELIM_NEWLINE, stmt.stmtLoop,
           POP_INDENT, DELIM_NEWLINE);
  }
}

void Formatter::write_(const ast::If &stmt) {
  write_(stmt.srcKwIf, DELIM_UNNECESSARY_SPACE, stmt.expr);
  if (llvm::isa<ast::Compound>(stmt.stmtThen)) {
    write_(DELIM_UNNECESSARY_SPACE, stmt.stmtThen);
  } else {
    write_(PUSH_INDENT, INCREMENT_INDENT, DELIM_NEWLINE, stmt.stmtThen,
           POP_INDENT, DELIM_NEWLINE);
  }
  if (stmt.stmtElse) {
    write_(llvm::isa<ast::Compound>(stmt.stmtThen) ? DELIM_SPACE
                                                   : DELIM_NEWLINE,
           stmt.srcKwElse);
    if (llvm::isa<ast::Compound>(stmt.stmtElse) ||
        llvm::isa<ast::If>(stmt.stmtElse)) {
      write_(DELIM_SPACE, stmt.stmtElse);
    } else {
      write_(PUSH_INDENT, INCREMENT_INDENT, DELIM_NEWLINE, stmt.stmtElse,
             POP_INDENT, DELIM_NEWLINE);
    }
  }
}

void Formatter::write_(const ast::Switch &stmt) {
  write_(stmt.srcKwSwitch, DELIM_UNNECESSARY_SPACE, stmt.expr,
         DELIM_UNNECESSARY_SPACE, stmt.srcBraceL, DELIM_NEWLINE);
  for (const auto &each : stmt.cases) {
    write_(each.srcKwCaseOrDefault);
    if (!each.isDefault()) write_(DELIM_SPACE, each.expr);
    write_(each.srcColon);
    if (each.stmts.size() == 1 && !nextCommentForcesNewLine_()) {
      write_(DELIM_SPACE, each.stmts.front(), DELIM_NEWLINE);
    } else {
      write_(PUSH_INDENT, INCREMENT_INDENT, DELIM_NEWLINE);
      for (const auto &subStmt : each.stmts) write_(subStmt, DELIM_NEWLINE);
      write_(POP_INDENT);
    }
  }
  write_(stmt.srcBraceR);
}
//--}

void Formatter::write_(const ast::AnnotationBlock &annos) {
  if (!options_.shouldDropAnnotations) {
    write_(PUSH_INDENT, INCREMENT_INDENT, DELIM_UNNECESSARY_SPACE,
           annos.srcDoubleBrackL, PUSH_INDENT);
    writeList_(annos.size(), annos.hasTrailingComma(), /*canBreak=*/true,
               [&](Delim delim) {
                 for (const auto &[identifier, args, srcComma] : annos)
                   write_(identifier, args, srcComma,
                          srcComma.empty() ? DELIM_NONE : delim);
               });
    write_(POP_INDENT, annos.srcDoubleBrackR, POP_INDENT);
  }
}

namespace {
// Is the expression a literal constant, possibly signed?
[[nodiscard]] bool isLiteral(const ast::Expr &expr) {
  if (const ast::Unary *unary{llvm::dyn_cast<ast::Unary>(&expr)};
      unary && (unary->op == ast::UNOP_POS || unary->op == ast::UNOP_NEG))
    return isLiteral(*unary->expr);
  return llvm::isa<ast::LiteralBool, ast::LiteralFloat, ast::LiteralInt,
                   ast::LiteralString>(expr);
}

// Is the argument list nothing but literal constants? Such a list is data,
// and how data is laid out belongs to whoever wrote it, so the formatter
// never breaks one to fit. A table of rows stays a table of rows.
[[nodiscard]] bool isDataList(const ast::ArgumentList &args) {
  return args.size() > 0 && llvm::all_of(args, [](const ast::Argument &arg) {
           return arg.isPositional() && arg.expr && isLiteral(*arg.expr);
         });
}
} // namespace

void Formatter::write_(const ast::ArgumentList &args) {
  write_(args.srcParenL, PUSH_INDENT);
  writeList_(args.size(), args.hasTrailingComma(),
             /*canBreak=*/!isDataList(args), [&](Delim delim) {
               for (const auto &arg : args) {
                 if (arg.isVisited()) write_(arg.srcKwVisit, DELIM_SPACE);
                 if (arg.isInlined()) write_(arg.srcKwInline, DELIM_SPACE);
                 if (arg.isNamed())
                   write_(arg.name, arg.srcColonAfterName, DELIM_SPACE);
                 write_(arg.expr, arg.srcComma,
                        arg.srcComma.empty() ? DELIM_NONE : delim);
               }
             });
  write_(POP_INDENT, args.srcParenR);
}

void Formatter::write_(const ast::ParameterList &params) {
  write_(params.srcParenL, PUSH_INDENT);
  if (params.isVariant()) {
    write_(params.srcStar);
  } else {
    writeList_(params.size(), params.hasTrailingComma(), /*canBreak=*/true,
               [&](Delim delim) {
                 for (const auto &param : params) {
                   write_(param.type, DELIM_SPACE, param.name);
                   // A default is never worth a line break of its own: it
                   // would leave the parameter it belongs to dangling.
                   if (param.exprInit) {
                     write_(DELIM_UNNECESSARY_SPACE, param.srcEqual,
                            DELIM_UNNECESSARY_SPACE, PUSH_INDENT);
                     if (delim == DELIM_NEWLINE) write_(ALIGN_INDENT);
                     write_(param.exprInit, POP_INDENT);
                   }
                   write_(param.annotations, param.srcComma,
                          param.srcComma.empty() ? DELIM_NONE : delim);
                 }
                 write_(params.srcEllipsis);
               });
  }
  write_(POP_INDENT, params.srcParenR);
}

} // namespace smdl
