// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#include "smdl/Parser.h"

#include <algorithm>
#include <charconv>

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ConvertUTF.h"
#include "llvm/Support/Error.h"

namespace smdl {

//--{ Basics
char Parser::next_() {
  if (isEOF_()) return '\0';
  char ch{peek_()};
  if (ch == '\n') {
    srcLoc_.lineNo++;
    srcLoc_.charNo = 1;
  } else {
    srcLoc_.charNo++;
  }
  srcLoc_.i++;
  return ch;
}

std::string_view Parser::next_(size_t n) {
  std::string_view result{getRemainingSourceCode_().substr(0, n)};
  for (size_t i = 0; i < n && !isEOF_(); i++) next_();
  return result;
}

std::optional<std::string_view> Parser::next_(std::string_view str) {
  if (startsWith(getRemainingSourceCode_(), str)) return next_(str.size());
  return std::nullopt;
}

std::optional<std::string_view> Parser::nextKeyword_(std::string_view str) {
  checkpoint_();
  std::optional<std::string_view> result{next_(str)};
  if (!result || isWord(peek_())) {
    reject_();
    return std::nullopt;
  } else {
    accept_();
    return result;
  }
}

std::string_view Parser::tokenAt_(uint64_t i) const {
  std::string_view src{getSourceCode_()};
  if (i >= src.size()) return {};
  src = src.substr(i);
  // '#' and '$' lead words rather than standing alone, so that a message
  // can name '#include' or '$state' instead of a lone sigil.
  if (isAlpha(src[0]) || src[0] == '_' || src[0] == '#' || src[0] == '$') {
    size_t n{1};
    while (n < src.size() && isWord(src[n])) n++;
    return src.substr(0, n);
  }
  return src.substr(0, 1);
}

std::string_view Parser::peekToken_() {
  skip_();
  return tokenAt_(srcLoc_.i);
}

std::string Parser::explainToken_(std::string_view token) const {
  // The spellings people reach for out of habit, where the generic parse
  // failure says nothing about the actual mistake. Each entry names its own
  // token, because the token that stops the parse is often not the one that
  // needs explaining.
  static constexpr std::pair<std::string_view, std::string_view> foreign[]{
      {"and", "there is no 'and'; use '&&'"},
      {"class", "there is no 'class'; use 'struct'"},
      {"const_cast", "conversions are written 'T(x)' or 'cast<T>(x)'"},
      {"delete", "there is no 'delete'; there is no dynamic allocation"},
      {"dynamic_cast", "conversions are written 'T(x)' or 'cast<T>(x)'"},
      {"elif", "there is no 'elif'; use 'else if'"},
      {"is", "there is no 'is'; the type test operator is '<:'"},
      {"new", "there is no 'new'; there is no dynamic allocation"},
      {"not", "there is no 'not'; use '!'"},
      {"or", "there is no 'or'; use '||'"},
      {"private", "there are no access specifiers"},
      {"protected", "there are no access specifiers"},
      {"public", "there are no access specifiers"},
      {"reinterpret_cast", "use '#bitCast(T, x)'"},
      {"static_cast", "conversions are written 'T(x)' or 'cast<T>(x)'"},
      {"template", "there are no templates; parameters may be declared 'auto'"},
      {"union", "'union' is not a keyword; union types are written '(A | B)'"},
      {"#define", "there is no preprocessor; use 'const' or 'typedef'"},
      {"#include", "there is no '#include'; use 'import'"},
      {"'", "there are no character literals; strings use double quotes"},
  };
  for (const auto &[spelling, advice] : foreign)
    if (token == spelling) return concat("; ", advice);
  // A file that never opted in cannot use the extensions, and naming the
  // one it reached for beats reporting that a declaration was expected.
  if (!isSMDL_) {
    static constexpr std::string_view extensions[]{
        "defer", "exec",      "namespace",   "return_from", "static",
        "tag",   "unit_test", "unreachable", "visit",
    };
    bool isExtension{token.size() > 1 && (token[0] == '#' || token[0] == '$')};
    for (auto extension : extensions) isExtension |= token == extension;
    if (isExtension)
      return concat("; ", SpellQuoted(token),
                    " is a SpectralMDL extension, so the file must begin "
                    "with '#smdl'");
  }
  return {};
}

void Parser::throwUnexpectedToken_(const SourceLocation &srcLoc,
                                   std::string_view message,
                                   const SourceLocation *srcLocStart) {
  std::string_view token{peekToken_()};
  std::string explanation{explainToken_(token)};
  // Neither end explains it, so look through the construct itself: a
  // borrowed keyword often sits in the middle, as 'new' does in
  // 'auto p = new int(3);', which fails at 'int'.
  if (explanation.empty() && srcLocStart) {
    const std::string_view src{getSourceCode_()};
    // Out to the end of the statement, not merely to where the parse gave
    // up: an infix word like 'and' stops the parse at the token before it.
    size_t scanEnd{src.find_first_of(";\n", srcLoc.i)};
    if (scanEnd == std::string_view::npos) scanEnd = src.size();
    for (auto i{srcLocStart->i}; i < scanEnd && i < src.size();) {
      // Step over anything whose contents are not code, so that the word
      // 'class' in a comment or a string never becomes advice.
      if (src[i] == '"') {
        for (i++; i < src.size() && src[i] != '"'; i++)
          if (src[i] == '\\') i++;
        i++;
        continue;
      }
      if (src.compare(i, 2, "//") == 0) break;
      if (src.compare(i, 2, "/*") == 0) {
        size_t close{src.find("*/", i + 2)};
        if (close == std::string_view::npos) break;
        i = close + 2;
        continue;
      }
      std::string_view tokenInside{tokenAt_(i)};
      if (tokenInside.empty()) break;
      if (std::string hit{explainToken_(tokenInside)}; !hit.empty()) {
        explanation = hit;
        break;
      }
      i += tokenInside.size();
      while (i < src.size() && isSpace(src[i])) i++;
    }
  }
  if (token.empty())
    srcLoc.throwError(
        concat(message, ", but reached the end of the file", explanation));
  srcLoc.throwError(
      concat(message, ", but found ", SpellQuoted(token), explanation));
}

std::optional<std::string_view> Parser::nextWord_() {
  checkpoint_();
  uint64_t i{srcLoc_.i};
  if (isSMDL_ && peek_() == '$') // The `$` prefix is extended syntax!
    next_();
  if (isAlpha(peek_()) || peek_() == '_') {
    next_();
    while (isWord(peek_())) next_();
    accept_();
    return getSourceCode_().substr(i, srcLoc_.i - i);
  } else {
    reject_();
    return std::nullopt;
  }
}

std::optional<std::string_view> Parser::nextInteger_() {
  uint64_t i{srcLoc_.i};
  while (isDigit(peek_())) next_();
  if (srcLoc_.i > i) {
    return getSourceCode_().substr(i, srcLoc_.i - i);
  } else {
    return std::nullopt;
  }
}

namespace {
// Is the source between a documentation comment and what follows it
// close enough to attach? I.e., only whitespace with at most one
// newline, so that a blank line breaks attachment.
[[nodiscard]] bool isDocCommentAdjacent(std::string_view src) {
  int numNewLines{};
  for (char ch : src) {
    if (!isSpace(ch)) return false;
    if (ch == '\n' && ++numNewLines > 1) return false;
  }
  return true;
}
} // namespace

void Parser::skip_() {
  auto skipSome{[&] {
    if (startsWith(getRemainingSourceCode_(), "//")) {
      uint64_t iComment{srcLoc_.i};
      next_(2);
      while (!isEOF_() && peek_() != '\n') next_(1);
      // A `///` comment (but not a `///<` trailing comment) is a
      // documentation line: remember it so that declaration parsers can
      // pick it up with `getDocCommentBefore_()`, merging consecutive
      // lines into one block. Every other comment breaks the pending
      // block. A rewind by `reject_()` may re-scan the block, in which
      // case `iComment <= pendingDocCommentEnd_` restarts it in place.
      std::string_view comment{
          getSourceCode_().substr(iComment, srcLoc_.i - iComment)};
      if (startsWith(comment, "///<")) {
        // A `///<` comment is a trailing documentation line for the
        // item it follows, picked up by
        // `attachPendingTrailingDocComment_()`. It also breaks any
        // pending leading block.
        pendingTrailingDocCommentBegin_ = iComment;
        pendingTrailingDocCommentEnd_ = srcLoc_.i;
        pendingDocCommentBegin_ = pendingDocCommentEnd_ = 0;
      } else if (startsWith(comment, "///")) {
        if (pendingDocCommentBegin_ == pendingDocCommentEnd_ ||
            iComment <= pendingDocCommentEnd_ ||
            !isDocCommentAdjacent(getSourceCode_().substr(
                pendingDocCommentEnd_, iComment - pendingDocCommentEnd_)))
          pendingDocCommentBegin_ = iComment;
        pendingDocCommentEnd_ = srcLoc_.i;
      } else {
        pendingDocCommentBegin_ = pendingDocCommentEnd_ = 0;
      }
      return true;
    } else if (startsWith(getRemainingSourceCode_(), "/*")) {
      SourceLocation srcLocComment{srcLoc_};
      next_(2);
      while (!isEOF_() && !startsWith(getRemainingSourceCode_(), "*/"))
        next_(1);
      if (isEOF_()) srcLocComment.throwError("Unterminated multiline comment");
      next_(2);
      pendingDocCommentBegin_ = pendingDocCommentEnd_ = 0;
      return true;
    } else if (isSpace(peek_())) {
      next_(1);
      return true;
    } else {
      return false;
    }
  }};
  while (!isEOF_() && skipSome()) continue;
}

std::string_view Parser::getDocCommentBefore_(size_t srcIndex) const {
  if (pendingDocCommentBegin_ == pendingDocCommentEnd_ ||
      srcIndex < pendingDocCommentEnd_ ||
      !isDocCommentAdjacent(getSourceCode_().substr(
          pendingDocCommentEnd_, srcIndex - pendingDocCommentEnd_)))
    return {};
  return getPendingDocComment_();
}

bool Parser::pendingTrailingDocCommentTrailsCode_() const {
  for (auto i{pendingTrailingDocCommentBegin_};
       i > 0 && getSourceCode_()[i - 1] != '\n'; i--) {
    if (!isSpace(getSourceCode_()[i - 1])) return true;
  }
  return false;
}
//--}

//--{ Parse: Expr
auto Parser::parseSimpleName_() -> std::optional<ast::Name> {
  SourceLocation srcLoc0{checkpoint_()};
  if (std::optional<std::string_view> name{nextWord_()}) {
    // NOTE: These must remain sorted for `std::binary_search`!
    static constexpr std::string_view keywords[]{
        "break",   "case",   "cast", "const",   "continue", "default",
        "do",      "else",   "enum", "export",  "false",    "for",
        "if",      "import", "let",  "module",  "package",  "return",
        "struct",  "switch", "true", "typedef", "uniform",  "using",
        "varying", "while",
    };
    static constexpr std::string_view keywordsSMDLSyntax[]{
        "defer",  "inline", "namespace",   "return_from",
        "static", "tag",    "unreachable", "visit",
    };
    bool isKeyword{
        std::binary_search(std::begin(keywords), std::end(keywords), *name) ||
        (isSMDL_ && std::binary_search(std::begin(keywordsSMDLSyntax),
                                       std::end(keywordsSMDLSyntax), *name))};
    if (!isKeyword) {
      accept_();
      return ast::Name{srcLoc0, *name};
    }
  }
  reject_();
  return std::nullopt;
}

auto Parser::parseIdentifier_() -> BumpPtr<ast::Identifier> {
  SourceLocation srcLoc0{checkpoint_()};
  std::vector<ast::Identifier::Element> elements{};
  std::optional<std::string_view> srcDoubleColon{next_("::")};
  if (std::optional<ast::Name> name{parseSimpleName_()}) {
    elements.push_back(
        ast::Identifier::Element{orEmpty_(srcDoubleColon), *name});
  } else {
    if (srcDoubleColon) {
      srcLoc0.throwError("Expected name after '::'");
    } else {
      reject_();
      return nullptr;
    }
  }
  while (true) {
    checkpoint_();
    if (srcDoubleColon = next_("::"); srcDoubleColon) {
      if (std::optional<ast::Name> name{parseSimpleName_()}) {
        elements.push_back(ast::Identifier::Element{*srcDoubleColon, *name});
        accept_();
        continue;
      }
    }
    reject_();
    break;
  }
  if (srcLoc_.i > srcLoc0.i) {
    accept_();
    return allocate_<ast::Identifier>(srcLoc0, std::in_place,
                                      std::move(elements));
  } else {
    reject_();
    return nullptr;
  }
}

auto Parser::parseType_() -> BumpPtr<ast::Type> {
  SourceLocation srcLoc0{checkpoint_()};
  std::vector<std::string_view> srcQuals{};
  while (true) {
    checkpoint_();
    // The `inline` and `static` qualifiers are extended syntax!
    if (std::optional<std::string_view> srcQual{
            isSMDL_ ? nextKeyword_(
                          {"const", "inline", "static", "uniform", "varying"})
                    : nextKeyword_({"const", "uniform", "varying"})}) {
      accept_();
      srcQuals.push_back(*srcQual);
    } else {
      reject_();
      break;
    }
  }
  BumpPtr<ast::Expr> expr{parseUnaryExpression_()};
  if (!expr) {
    reject_();
    return nullptr;
  }
  accept_();
  return allocate_<ast::Type>(srcLoc0, std::in_place, std::move(srcQuals),
                              std::move(expr));
}

auto Parser::parseParameter_() -> std::optional<ast::Parameter> {
  SourceLocation srcLoc0{checkpoint_()};
  // Capture before parsing: comments inside the parameter must not
  // clobber the pending block first.
  std::string_view srcDocComment{getDocCommentBefore_(srcLoc0.i)};
  BumpPtr<ast::Type> type{parseType_()};
  if (!type) {
    reject_();
    return std::nullopt;
  }
  std::optional<ast::Name> name{parseSimpleName_()};
  if (!name) {
    reject_();
    return std::nullopt;
  }
  ast::Parameter param{};
  param.srcLoc = srcLoc0;
  param.srcDocComment = srcDocComment;
  param.type = std::move(type);
  param.name = *name;
  if (std::optional<std::string_view> srcEqual{nextDelimiter_("=")}) {
    BumpPtr<ast::Expr> exprInit{parseAssignmentExpression_()};
    if (!exprInit)
      throwUnexpectedToken_(srcLoc_, "expected an initializer after '='");
    param.srcEqual = *srcEqual;
    param.exprInit = std::move(exprInit);
  }
  param.annotations = parseAnnotationBlock_();
  accept_();
  return std::move(param);
}

auto Parser::parseParameterList_() -> std::optional<ast::ParameterList> {
  checkpoint_();
  ast::ParameterList params{};
  std::optional<std::string_view> srcParenL{nextDelimiter_("(")};
  if (!srcParenL) {
    reject_();
    return std::nullopt;
  }
  params.srcParenL = *srcParenL;
  skip_();
  if (std::optional<std::string_view> srcStar{nextDelimiter_("*")}) {
    params.srcStar = *srcStar;
  } else {
    params.params.reserve(4);
    parseCommaSeparated_(params.params, [&] { return parseParameter_(); });
    std::optional<std::string_view> srcEllipsis{nextDelimiter_("...")};
    if (srcEllipsis) {
      // The last parameter must have a trailing comma if the parameter list
      // features a variadic ellipsis.
      if (!params.params.empty() && params.params.back().srcComma.empty()) {
        reject_();
        return std::nullopt;
      }
      params.srcEllipsis = *srcEllipsis;
    }
  }
  std::optional<std::string_view> srcParenR{nextDelimiter_(")")};
  if (!srcParenR) {
    reject_();
    return std::nullopt;
  }
  params.srcParenR = *srcParenR;
  accept_();
  return std::move(params);
}

auto Parser::parseArgument_() -> std::optional<ast::Argument> {
  SourceLocation srcLoc0{checkpoint_()};
  ast::Argument argument{};
  argument.srcLoc = srcLoc0;
  if (isSMDL_) {
    if (std::optional<std::string_view> srcKwVisit{nextKeyword_("visit")}) {
      argument.srcKwVisit = *srcKwVisit;
    }
    if (std::optional<std::string_view> srcKwInline{nextKeyword_("inline")}) {
      argument.srcKwInline = *srcKwInline;
      if (!argument.srcKwVisit.empty())
        srcLoc0.throwError(
            "Cannot combine 'visit' and 'inline' on an argument");
    }
  }
  argument.name = [&]() -> ast::Name {
    checkpoint_();
    if (std::optional<ast::Name> name{parseSimpleName_()}) {
      if (std::optional<std::string_view> srcColon{nextDelimiter_(":")};
          srcColon && peek_() != ':' && peek_() != '=') {
        argument.srcColonAfterName = *srcColon;
        accept_();
        return *name;
      }
    }
    reject_();
    return {};
  }();
  argument.expr = parseAssignmentExpression_();
  if (!argument.expr) {
    reject_();
    return std::nullopt;
  }
  argument.src = getSourceCodeBetween_(srcLoc0, srcLoc_);
  accept_();
  return std::move(argument);
}

auto Parser::parseArgumentList_() -> std::optional<ast::ArgumentList> {
  SourceLocation srcLoc0{checkpoint_()};
  ast::ArgumentList args{};
  args.srcLoc = srcLoc0;
  std::optional<std::string_view> srcParenL{nextDelimiter_("(")};
  if (!srcParenL) {
    reject_();
    return std::nullopt;
  }
  args.srcParenL = *srcParenL;
  parseCommaSeparated_(args.args, [&] { return parseArgument_(); });
  std::optional<std::string_view> srcParenR{nextDelimiter_(")")};
  if (!srcParenR) {
    reject_();
    return std::nullopt;
  }
  args.srcParenR = *srcParenR;
  accept_();
  return std::move(args);
}

auto Parser::parseAnnotation_() -> std::optional<ast::Annotation> {
  checkpoint_();
  BumpPtr<ast::Identifier> identifier{parseIdentifier_()};
  if (!identifier) {
    reject_();
    return std::nullopt;
  }
  std::optional<ast::ArgumentList> args{parseArgumentList_()};
  if (!args) {
    reject_();
    return std::nullopt;
  }
  accept_();
  return ast::Annotation{std::move(identifier), std::move(*args)};
}

auto Parser::parseAnnotationBlock_() -> BumpPtr<ast::AnnotationBlock> {
  std::optional<Parser::ParsedToken> brackL{nextDelimiterAndLocation_("[[")};
  if (!brackL) return nullptr;
  std::vector<ast::Annotation> annos{};
  parseCommaSeparated_(annos, [&] { return parseAnnotation_(); }, "]]");
  std::optional<std::string_view> srcDoubleBrackR{nextDelimiter_("]]")};
  if (!srcDoubleBrackR)
    srcLoc_.throwError("Expected annotation, ',', or ']]' in annotation block");
  return allocate_<ast::AnnotationBlock>(brackL->srcLoc, std::in_place,
                                         brackL->src, std::move(annos),
                                         *srcDoubleBrackR);
}

auto Parser::parseExpressionInParentheses_() -> BumpPtr<ast::Expr> {
  SourceLocation srcLoc0{checkpoint_()};
  std::optional<std::string_view> srcDollar{nextDelimiter_("$")};
  std::optional<std::string_view> srcParenL{nextDelimiter_("(")};
  if (!srcParenL) {
    reject_();
    return nullptr;
  }
  BumpPtr<ast::Expr> expr{parseExpression_()};
  if (!expr) {
    reject_();
    return nullptr;
  }
  std::optional<std::string_view> srcParenR{nextDelimiter_(")")};
  if (!srcParenR)
    throwUnexpectedToken_(
        srcLoc_,
        concat("expected ')' to close the '(' opened at line ", srcLoc0.lineNo),
        &srcLoc0);
  accept_();
  return allocate_<ast::Parens>(srcLoc0, std::in_place, orEmpty_(srcDollar),
                                *srcParenL, std::move(expr), *srcParenR);
}

auto Parser::parseExpression_() -> BumpPtr<ast::Expr> {
  return parseBinaryLeftAssociative_(
      {BINOP_COMMA}, [&] { return parseAssignmentExpression_(); });
}

auto Parser::parseAssignmentExpression_() -> BumpPtr<ast::Expr> {
  return parseBinaryRightAssociative_(
      {BINOP_LET, //
       BINOP_EQ_LSHR, BINOP_EQ_ADD, BINOP_EQ_SUB, BINOP_EQ_MUL, BINOP_EQ_DIV,
       BINOP_EQ_REM, BINOP_EQ_SHL, BINOP_EQ_ASHR, BINOP_EQ_AND, BINOP_EQ_OR,
       BINOP_EQ_XOR, BINOP_EQ},
      [&] { return parseElseExpression_(); });
}

auto Parser::parseElseExpression_() -> BumpPtr<ast::Expr> {
  return parseBinaryRightAssociative_(
      {BINOP_ELSE}, [&] { return parseConditionalExpression_(); });
}

auto Parser::parseConditionalExpression_() -> BumpPtr<ast::Expr> {
  BumpPtr<ast::Expr> expr{parseLogicalOrExpression_()};
  if (!expr) return nullptr;
  skip_();
  SourceLocation srcLoc0{srcLoc_};
  if (std::optional<std::string_view> srcQuestion{next_("?")}) {
    BumpPtr<ast::Expr> exprThen{parseExpression_()};
    if (!exprThen)
      srcLoc0.throwError("Expected then clause in conditional expression");
    skip_();
    std::optional<std::string_view> srcColon{next_(":")};
    if (!srcColon)
      srcLoc_.throwError("Expected ':' after then clause in conditional "
                         "expression");
    BumpPtr<ast::Expr> exprElse{parseAssignmentExpression_()};
    if (!exprElse)
      srcLoc0.throwError("Expected else clause in conditional expression");
    expr = allocate_<ast::Select>(srcLoc0, std::in_place, std::move(expr),
                                  *srcQuestion, std::move(exprThen), *srcColon,
                                  std::move(exprElse));
  }
  return expr;
}

auto Parser::parseLogicalOrExpression_() -> BumpPtr<ast::Expr> {
  return parseBinaryLeftAssociative_(
      {BINOP_LOGIC_OR}, [&] { return parseLogicalAndExpression_(); });
}

auto Parser::parseLogicalAndExpression_() -> BumpPtr<ast::Expr> {
  return parseBinaryLeftAssociative_(
      {BINOP_LOGIC_AND}, [&] { return parseInclusiveOrExpression_(); });
}

auto Parser::parseInclusiveOrExpression_() -> BumpPtr<ast::Expr> {
  return parseBinaryLeftAssociative_(
      {BINOP_OR}, [&] { return parseExclusiveOrExpression_(); });
}

auto Parser::parseExclusiveOrExpression_() -> BumpPtr<ast::Expr> {
  return parseBinaryLeftAssociative_({BINOP_XOR},
                                     [&] { return parseAndExpression_(); });
}

auto Parser::parseAndExpression_() -> BumpPtr<ast::Expr> {
  return parseBinaryLeftAssociative_(
      {BINOP_AND}, [&] { return parseEqualityExpression_(); });
}

auto Parser::parseEqualityExpression_() -> BumpPtr<ast::Expr> {
  return parseBinaryLeftAssociative_(
      {BINOP_CMP_EQ, BINOP_CMP_NE, BINOP_APPROX_CMP_EQ, BINOP_APPROX_CMP_NE},
      [&] { return parseRelationalExpression_(); });
}

auto Parser::parseRelationalExpression_() -> BumpPtr<ast::Expr> {
  return parseBinaryLeftAssociative_(
      {BINOP_SUBSET, BINOP_CMP_LE, BINOP_CMP_GE, BINOP_CMP_LT, BINOP_CMP_GT},
      [&] { return parseShiftExpression_(); });
}

auto Parser::parseShiftExpression_() -> BumpPtr<ast::Expr> {
  return parseBinaryLeftAssociative_({BINOP_LSHR, BINOP_SHL, BINOP_ASHR}, [&] {
    return parseAdditiveExpression_();
  });
}

auto Parser::parseAdditiveExpression_() -> BumpPtr<ast::Expr> {
  return parseBinaryLeftAssociative_(
      {BINOP_ADD, BINOP_SUB}, [&] { return parseMultiplicativeExpression_(); });
}

auto Parser::parseMultiplicativeExpression_() -> BumpPtr<ast::Expr> {
  return parseBinaryLeftAssociative_({BINOP_MUL, BINOP_DIV, BINOP_REM},
                                     [&] { return parseUnaryExpression_(); });
}

auto Parser::parseUnaryExpression_() -> BumpPtr<ast::Expr> {
  ParseDepthGuard depthGuard{*this};
  // Lambdas, introduced unambiguously by a backslash. This is extended
  // syntax!
  if (isSMDL_) {
    if (BumpPtr<ast::Expr> expr{parseLambdaExpression_()}) return expr;
  }
  if (BumpPtr<ast::Expr> expr{parsePostfixExpression_()}) return expr;
  auto parsePrefixExpression{[&]() -> BumpPtr<ast::Expr> {
    SourceLocation srcLoc0{checkpoint_()};
    std::optional<Parser::ParsedUnaryOp> op{parseUnaryOp_()};
    if (!op) {
      reject_();
      return nullptr;
    }
    BumpPtr<ast::Expr> expr{parseUnaryExpression_()};
    if (!expr) {
      reject_();
      return nullptr;
    }
    accept_();
    expr = allocate_<ast::Unary>(srcLoc0, std::in_place, op->srcOp, op->op,
                                 std::move(expr));
    return expr;
  }};
  if (BumpPtr<ast::Expr> expr{parsePrefixExpression()}) return expr;
  if (BumpPtr<ast::Expr> expr{parseLetExpression_()}) return expr;
  if (isSMDL_) {
    if (BumpPtr<ast::Expr> expr{parseReturnFromExpression_()}) return expr;
  }
  return nullptr;
}

auto Parser::parsePostfixExpression_() -> BumpPtr<ast::Expr> {
  BumpPtr<ast::Expr> expr{parsePrimaryExpression_()};
  if (!expr) return nullptr;
  auto withPostfix{[&]() -> BumpPtr<ast::Expr> {
    SourceLocation srcLoc0{srcLoc_};
    if (std::optional<std::string_view> srcDot{nextDelimiter_(".")}) {
      std::optional<ast::Name> name{parseSimpleName_()};
      if (!name) srcLoc0.throwError("Expected name after '.'");
      return allocate_<ast::AccessField>(srcLoc0, std::in_place,
                                         std::move(expr), *srcDot, *name);
    }
    if (std::optional<std::string_view> srcOp{nextDelimiter_("++")})
      return allocate_<ast::Unary>(srcLoc0, std::in_place, *srcOp,
                                   UNOP_POSTFIX_INC, std::move(expr));
    if (std::optional<std::string_view> srcOp{nextDelimiter_("--")})
      return allocate_<ast::Unary>(srcLoc0, std::in_place, *srcOp,
                                   UNOP_POSTFIX_DEC, std::move(expr));
    if (std::optional<ast::ArgumentList> args{parseArgumentList_()})
      return allocate_<ast::Call>(srcLoc0, std::in_place, std::move(expr),
                                  std::move(*args));
    std::vector<ast::AccessIndex::Index> indexes{};
    while (!startsWith(getRemainingSourceCode_(), "[[")) {
      ast::AccessIndex::Index index{};
      std::optional<std::string_view> srcBrackL{nextDelimiter_("[")};
      if (!srcBrackL) break;
      if (std::optional<std::string_view> srcAngleL{nextDelimiter_("<")}) {
        std::optional<ast::Name> name{parseSimpleName_()};
        if (!name) srcLoc0.throwError("Expected name after '[<'");
        std::optional<std::string_view> srcAngleR{nextDelimiter_(">")};
        if (!srcAngleR) srcLoc0.throwError("Expected '>]'");
        index.expr = allocate_<ast::SizeName>(srcLoc0, std::in_place,
                                              *srcAngleL, *name, *srcAngleR);
      } else {
        index.expr = parseExpression_(); // This may be null to represent `[]`
      }
      std::optional<std::string_view> srcBrackR{nextDelimiter_("]")};
      if (!srcBrackR)
        throwUnexpectedToken_(srcLoc_,
                              concat("expected ']' to close the '[' opened at "
                                     "line ",
                                     srcLoc0.lineNo),
                              &srcLoc0);
      index.srcBrackL = *srcBrackL;
      index.srcBrackR = *srcBrackR;
      indexes.push_back(std::move(index));
      skip_();
    }
    if (!indexes.empty())
      return allocate_<ast::AccessIndex>(srcLoc0, std::in_place,
                                         std::move(expr), std::move(indexes));
    return nullptr;
  }};
  while (true) {
    BumpPtr<ast::Expr> nextExpr{withPostfix()};
    if (!nextExpr) break;
    expr = std::move(nextExpr);
  }
  return expr;
}

auto Parser::parseLetExpression_() -> BumpPtr<ast::Expr> {
  std::optional<Parser::ParsedToken> kwLet{nextKeywordAndLocation_("let")};
  if (!kwLet) return nullptr;
  SourceLocation srcLoc0{kwLet->srcLoc};
  std::vector<BumpPtr<ast::Decl>> decls{};
  std::optional<std::string_view> srcBraceL{};
  std::optional<std::string_view> srcBraceR{};
  if (srcBraceL = nextDelimiter_("{"); srcBraceL) {
    while (true) {
      BumpPtr<ast::Variable> decl{parseVariableDeclaration_()};
      if (!decl) break;
      decls.push_back(std::move(decl));
      skip_();
      if (peek_() == '}') break;
    }
    if (srcBraceR = nextDelimiter_("}"); !srcBraceR) {
      // Anything that is not a declaration stops the loop above, so the
      // block is far more often wrong at the offending token than it is
      // missing its closing brace. Only blame the brace at EOF.
      if (isEOF_()) srcLoc0.throwError("Expected closing '}' after 'let'");
      srcLoc_.throwError("Expected variable declaration or closing '}' in "
                         "'let' block, which must contain only declarations");
    }
  } else {
    BumpPtr<ast::Variable> decl{parseVariableDeclaration_()};
    if (!decl) srcLoc0.throwError("Expected variable declaration after 'let'");
    decls.push_back(std::move(decl));
  }
  std::optional<std::string_view> srcKwIn{nextKeyword_("in")};
  if (!srcKwIn) srcLoc0.throwError("Expected 'in' after 'let ...'");
  BumpPtr<ast::Expr> expr{parseConditionalExpression_()};
  if (!expr) srcLoc0.throwError("Expected expression after 'let ... in'");
  return allocate_<ast::Let>(srcLoc0, std::in_place, kwLet->src,
                             orEmpty_(srcBraceL), std::move(decls),
                             orEmpty_(srcBraceR), *srcKwIn, std::move(expr));
}

auto Parser::parseReturnFromExpression_() -> BumpPtr<ast::Expr> {
  std::optional<Parser::ParsedToken> kwReturnFrom{
      nextKeywordAndLocation_("return_from")};
  if (!kwReturnFrom) return nullptr;
  BumpPtr<ast::Compound> stmt{parseCompoundStatement_()};
  if (!stmt)
    kwReturnFrom->srcLoc.throwError(
        "Expected compound statement after 'return_from'");
  return allocate_<ast::ReturnFrom>(kwReturnFrom->srcLoc, std::in_place,
                                    kwReturnFrom->src, std::move(stmt));
}

auto Parser::parseLambdaExpression_() -> BumpPtr<ast::Expr> {
  std::optional<Parser::ParsedToken> backslash{nextDelimiterAndLocation_("\\")};
  if (!backslash) return nullptr;
  // The backslash unambiguously introduces a lambda, so everything from
  // here on is a committed parse. There is no return type syntax; the
  // return type is always implicitly `auto`.
  SourceLocation srcLoc0{backslash->srcLoc};
  std::optional<ast::ParameterList> params{parseParameterList_()};
  if (!params) srcLoc0.throwError("Expected parameter list after '\\'");
  if (params->isVariant())
    srcLoc0.throwError("Lambda must not be a function variant");
  if (params->hasTrailingEllipsis())
    srcLoc0.throwError("Lambda must not be variadic");
  std::optional<std::string_view> srcEqual{};
  BumpPtr<ast::Node> definition{};
  if (srcEqual = nextDelimiter_("="); srcEqual) {
    skip_();
    SourceLocation srcLoc1{srcLoc_};
    // The body is an assignment expression, not a full expression: the
    // comma operator must not swallow subsequent arguments when the lambda
    // appears in an argument list.
    BumpPtr<ast::Expr> def{parseAssignmentExpression_()};
    if (!def) srcLoc0.throwError("Expected lambda expression after '='");
    definition = allocate_<ast::Return>(srcLoc1, std::in_place,
                                        std::string_view(), std::move(def),
                                        std::nullopt, std::string_view());
  } else {
    BumpPtr<ast::Compound> def{parseCompoundStatement_()};
    if (!def)
      srcLoc0.throwError("Expected '=' or compound statement after lambda "
                         "parameter list");
    definition = std::move(def);
  }
  BumpPtr<ast::Function> func{allocate_<ast::Function>(
      srcLoc0, std::in_place, BumpPtr<ast::Type>{},
      BumpPtr<ast::AnnotationBlock>{}, ast::Name{}, std::move(*params),
      std::string_view(), BumpPtr<ast::AnnotationBlock>{}, orEmpty_(srcEqual),
      std::move(definition), std::string_view())};
  return allocate_<ast::Lambda>(srcLoc0, std::in_place, backslash->src,
                                std::move(func));
}

auto Parser::parsePrimaryExpression_() -> BumpPtr<ast::Expr> {
  if (BumpPtr<ast::Expr> expr{parseExpressionInParentheses_()}) return expr;
  if (BumpPtr<ast::Expr> expr{parseLiteralExpression_()}) return expr;
  if (BumpPtr<ast::Identifier> expr{parseIdentifier_()}) return expr;
  SourceLocation srcLoc0{srcLoc_};
  if (std::optional<std::string_view> srcKwCast{nextKeyword_("cast")}) {
    std::optional<std::string_view> srcAngleL{nextDelimiter_("<")};
    if (!srcAngleL) srcLoc0.throwError("Expected opening '<' after 'cast'");
    BumpPtr<ast::Type> type{parseType_()};
    if (!type) srcLoc0.throwError("Expected type after 'cast'");
    std::optional<std::string_view> srcAngleR{nextDelimiter_(">")};
    if (!srcAngleR) srcLoc0.throwError("Expected closing '>' after 'cast'");
    BumpPtr<ast::Expr> expr{parseExpressionInParentheses_()};
    if (!expr)
      srcLoc0.throwError("Expected parenthesized expression after 'cast<...>'");
    return allocate_<ast::TypeCast>(srcLoc0, std::in_place, *srcKwCast,
                                    *srcAngleL, std::move(type), *srcAngleR,
                                    std::move(expr));
  }
  return nullptr;
}

auto Parser::parseLiteralExpression_() -> BumpPtr<ast::Expr> {
  if (BumpPtr<ast::LiteralBool> expr{parseLiteralBoolExpression_()})
    return expr;
  if (BumpPtr<ast::LiteralString> expr{parseLiteralStringExpression_()})
    return expr;
  if (BumpPtr<ast::Expr> expr{parseLiteralNumberExpression_()}) return expr;
  if (isSMDL_) {
    skip_();
    SourceLocation srcLoc0{srcLoc_};
    if (next_("#")) {
      std::optional<std::string_view> word{nextWord_()};
      if (!word) srcLoc0.throwError("Expected intrinsic name after '#'");
      if (*word == "search_dir")
        srcLoc0.throwError("A '#search_dir' is only allowed at the top of the "
                           "file immediately after '#smdl'");
      return allocate_<ast::Intrinsic>(
          srcLoc0, std::in_place,
          getSourceCode_().substr(srcLoc0.i, srcLoc_.i - srcLoc0.i));
    }
  }
  return nullptr;
}

auto Parser::parseLiteralBoolExpression_() -> BumpPtr<ast::LiteralBool> {
  skip_();
  SourceLocation srcLoc0{srcLoc_};
  if (std::optional<std::string_view> srcValue{nextKeyword_("true")})
    return allocate_<ast::LiteralBool>(srcLoc0, std::in_place, *srcValue, true);
  if (std::optional<std::string_view> srcValue{nextKeyword_("false")})
    return allocate_<ast::LiteralBool>(srcLoc0, std::in_place, *srcValue,
                                       false);
  return nullptr;
}

auto Parser::parseLiteralStringExpression_() -> BumpPtr<ast::LiteralString> {
  skip_();
  if (peek_() != '"') return nullptr;
  std::string str{};
  SourceLocation srcLoc0{srcLoc_};
  auto appendCodepointAsUTF8{[&](uint32_t codepoint) {
    char result[4]{};
    char *resultPtr{&result[0]};
    if (!llvm::ConvertCodePointToUTF8(codepoint, resultPtr)) return false;
    str.insert(str.end(), &result[0], resultPtr);
    return true;
  }};
  std::vector<std::string_view> srcValues{};
  // The start of the current string segment, which advances past `srcLoc0`
  // when adjacent string literals are concatenated.
  SourceLocation srcLocSeg{srcLoc0};
  while (nextDelimiter_("\"")) {
    while (true) {
      if (isEOF_()) srcLocSeg.throwError("Unexpected EOF in literal string");
      if (peek_() == '\n')
        srcLocSeg.throwError("Unexpected EOL in literal string");
      if (peek_() == '"') break;
      if (char ch{next_()}; ch != '\\') {
        str += ch;
      } else {
        ch = next_();
        if (ch == 'a') { // alert
          str += '\a';
        } else if (ch == 'b') { // backspace
          str += '\b';
        } else if (ch == 'f') { // form feed
          str += '\f';
        } else if (ch == 'n') { // new line
          str += '\n';
        } else if (ch == 'r') { // carriage return
          str += '\r';
        } else if (ch == 't') { // horizontal tab
          str += '\t';
        } else if (ch == 'v') { // vertical tab
          str += '\v';
        } else if (isDigit8(ch)) { // octal
          uint32_t byte{uint32_t(octToInt(ch))};
          for (int i{}; i < 2; i++) {
            ch = next_();
            if (!isDigit8(ch))
              srcLocSeg.throwError("Expected 3 octal digits after '\\'");
            byte = (byte << 3) | uint32_t(octToInt(ch));
          }
          if (byte > 255)
            srcLocSeg.throwError("Octal escape sequence out of range");
          str += static_cast<char>(byte);
        } else if (ch == 'x') { // hexadecimal
          uint8_t byte{};
          for (int i{}; i < 2; i++) {
            ch = next_();
            if (!isDigit16(ch))
              srcLocSeg.throwError("Expected 2 hexadecimal digits after '\\x'");
            byte = (byte << 4) | uint8_t(hexToInt(ch));
          }
          str += static_cast<char>(byte);
        } else if (ch == 'u') { // unicode 16-bit
          uint32_t codepoint{};
          for (int i{}; i < 4; i++) {
            ch = next_();
            if (!isDigit16(ch))
              srcLocSeg.throwError("Expected 4 hexadecimal digits after '\\u'");
            codepoint = (codepoint << 4) | uint32_t(hexToInt(ch));
          }
          if (!appendCodepointAsUTF8(codepoint))
            srcLocSeg.throwError("UTF-8 encoding of '\\u' sequence failed");
        } else if (ch == 'U') { // unicode 32-bit
          uint32_t codepoint{};
          for (int i{}; i < 8; i++) {
            ch = next_();
            if (!isDigit16(ch))
              srcLocSeg.throwError("Expected 8 hexadecimal digits after '\\U'");
            codepoint = (codepoint << 4) | uint32_t(hexToInt(ch));
          }
          if (!appendCodepointAsUTF8(codepoint))
            srcLocSeg.throwError("UTF-8 encoding of '\\U' sequence failed");
        } else {
          str += ch;
        }
      }
    }
    if (!nextDelimiter_("\""))
      srcLocSeg.throwError("Expected '\"' to close literal string");
    srcValues.push_back(getSourceCodeBetween_(srcLocSeg, srcLoc_));
    skip_();
    srcLocSeg = srcLoc_;
  }
  return allocate_<ast::LiteralString>(srcLoc0, std::in_place,
                                       std::move(srcValues), std::move(str));
}

auto Parser::parseLiteralNumberExpression_() -> BumpPtr<ast::Expr> {
  skip_();
  if (!isDigit(peek_())) return nullptr;
  SourceLocation srcLoc0{srcLoc_};
  auto parseDigits{[&](auto &&isDigit) {
    std::string digits{};
    while (isDigit(peek_())) {
      digits.push_back(peek_());
      next_();
      if (next_("'")) { // Maybe consume single-quote separator
        if (peek_() == '\'')
          srcLoc0.throwError("Numeric literal must not contain adjacent "
                             "single-quote separators");
        if (!isDigit(peek_()))
          srcLoc0.throwError("Numeric literal must not be terminated by "
                             "single-quote separator");
      }
    }
    return digits;
  }};
  auto parseIntWithPrefix{[&](auto &&isDigit, int radix, const char *prefix,
                              const char *info, std::string &digitsStr) {
    if (!isDigit(peek_()))
      srcLoc0.throwError("Expected literal prefix ", SpellQuoted(prefix),
                         " to be followed by ", info);
    std::string digits{parseDigits(isDigit)};
    unsigned bits{llvm::APInt::getBitsNeeded(digits, radix)};
    if (bits > 64) srcLoc0.logWarn("Integer literal exceeds 64 bits");
    digitsStr = prefix;
    digitsStr += std::string(digits);
    return llvm::APInt(bits, digits, radix);
  }};
  // Is the remaining source code `0` followed by any of the given
  // characters? A `0` followed by `.`, an exponent, or a suffix begins an
  // ordinary decimal literal, not an octal/binary/hexadecimal literal.
  auto zeroFollowedByAny{[&](std::string_view chars) {
    std::string_view remaining{getRemainingSourceCode_()};
    return remaining.size() >= 2 && remaining[0] == '0' &&
           chars.find(remaining[1]) != std::string_view::npos;
  }};
  if (!zeroFollowedByAny(".eEfFdD") && !(isSMDL_ && zeroFollowedByAny("j")) &&
      next_("0")) {
    llvm::APInt value{64, 0};
    std::string digits{};
    if (isDigit8(peek_())) {
      value = parseIntWithPrefix(isDigit8, 8, "0", "[0-7]", digits);
    } else if (next_("b") || next_("B")) {
      value = parseIntWithPrefix(isDigit2, 2, "0b", "[0-1]", digits);
    } else if (next_("x") || next_("X")) {
      value = parseIntWithPrefix(isDigit16, 16, "0x", "[0-9a-fA-F]", digits);
    } else {
      digits = "0";
    }
    if (isDigit(peek_()))
      srcLoc_.throwError("Invalid digit in integer literal");
    return allocate_<ast::LiteralInt>(srcLoc0, std::in_place,
                                      getSourceCodeBetween_(srcLoc0, srcLoc_),
                                      value.getLimitedValue());
  } else {
    bool isInt{true};
    std::string digits{parseDigits(isDigit)};
    if (next_(".")) {
      digits += '.';
      digits += parseDigits(isDigit);
      isInt = false;
    }
    if (next_("e") || next_("E")) {
      digits += 'e';
      if (next_("+"))
        digits += '+';
      else if (next_("-"))
        digits += '-';
      if (!isDigit(peek_()))
        srcLoc0.throwError(
            "Expected exponent after 'e' in floating point literal");
      digits += parseDigits(isDigit);
      isInt = false;
    }
    if (isSMDL_ && next_("j")) { // Imaginary unit
      isInt = false;
    }
    if (next_("d") || next_("D") || next_("f") || next_("F")) {
      isInt = false;
    }
    if (isInt) {
      unsigned bits{llvm::APInt::getBitsNeeded(digits, 10)};
      if (bits > 64) srcLoc0.logWarn("Integer literal exceeds 64 bits");
      return allocate_<ast::LiteralInt>(
          srcLoc0, std::in_place, getSourceCodeBetween_(srcLoc0, srcLoc_),
          llvm::APInt(bits, digits, 10).getLimitedValue());
    } else {
      llvm::APFloat value(llvm::APFloat::IEEEdouble());
      llvm::Expected<llvm::APFloat::opStatus> opStatus{
          value.convertFromString(digits, llvm::APFloat::rmNearestTiesToEven)};
      if (!opStatus) {
        llvm::consumeError(opStatus.takeError());
        srcLoc0.throwError("Failed to parse floating point literal");
      }
      if (*opStatus & llvm::APFloat::opOverflow)
        srcLoc0.logWarn("Floating point literal exceeds range of 'double'");
      return allocate_<ast::LiteralFloat>(
          srcLoc0, std::in_place, getSourceCodeBetween_(srcLoc0, srcLoc_),
          value.convertToDouble());
    }
  }
  return nullptr;
}

auto Parser::parseUnaryOp_() -> std::optional<ParsedUnaryOp> {
  for (auto op : std::array{UNOP_INC, UNOP_DEC, UNOP_POS, UNOP_NEG, UNOP_NOT,
                            UNOP_LOGIC_NOT})
    if (std::optional<std::string_view> srcOp{next_(to_string(op))})
      return ParsedUnaryOp{*srcOp, op};
  if (isSMDL_) {
    for (auto op : std::array{UNOP_ADDR, UNOP_DEREF, UNOP_MAYBE})
      if (std::optional<std::string_view> srcOp{next_(to_string(op))})
        return ParsedUnaryOp{*srcOp, op};
  }
  return std::nullopt;
}

auto Parser::parseBinaryOp_(Span<const ast::BinaryOp> ops)
    -> std::optional<ParsedBinaryOp> {
  for (auto op : ops) {
    if (!isSMDL_ && isExtendedSyntax(op)) continue;
    if (op == BINOP_ELSE) {
      if (std::optional<std::string_view> srcOp{nextKeyword_(to_string(op))})
        return ParsedBinaryOp{*srcOp, op};
    } else {
      // Don't mistake bit and for logical and.
      if (op == BINOP_AND && startsWith(getRemainingSourceCode_(), "&&"))
        continue;
      if (std::optional<std::string_view> srcOp{next_(to_string(op))})
        return ParsedBinaryOp{*srcOp, op};
    }
  }
  return std::nullopt;
}
//--}

//--{ Parse: Decl
auto Parser::parseFile_() -> BumpPtr<ast::File> {
  skip_();
  // Any documentation comment before the `#smdl` marker or the
  // `mdl X.Y` version is module-level documentation.
  std::string_view srcDocComment{getPendingDocComment_()};
  SourceLocation srcLoc0{srcLoc_};
  std::optional<std::string_view> srcKwSMDLSyntax{nextKeyword_("#smdl")};
  if (srcKwSMDLSyntax) isSMDL_ = true;
  std::vector<ast::File::SearchDir> searchDirs{parseFileSearchDirs_()};
  std::optional<ast::File::Version> version{parseFileVersion_()};
  if (!version && !isSMDL_) srcLoc0.throwError("Expected MDL version");
  std::vector<BumpPtr<ast::Decl>> importDecls{};
  while (true) {
    auto parseAnyImport{[&]() -> BumpPtr<ast::Decl> {
      if (BumpPtr<ast::UsingAlias> decl{parseUsingAlias_()}) return decl;
      if (BumpPtr<ast::UsingImport> decl{parseUsingImport_()}) return decl;
      if (BumpPtr<ast::Import> decl{parseImport_()}) return decl;
      return nullptr;
    }};
    BumpPtr<ast::Decl> decl{parseAnyImport()};
    if (!decl) break;
    importDecls.push_back(std::move(decl));
  }
  std::optional<std::string_view> srcKwModule{nextKeyword_("module")};
  BumpPtr<ast::AnnotationBlock> moduleAnnotations{};
  std::optional<std::string_view> srcSemicolonAfterModule{};
  if (srcKwModule) {
    moduleAnnotations = parseAnnotationBlock_();
    if (!moduleAnnotations)
      srcLoc0.throwError("Expected annotation block after 'module'");
    srcSemicolonAfterModule = nextDelimiter_(";");
    if (!srcSemicolonAfterModule)
      srcLoc0.throwError("Expected ';' after 'module [[ ... ]]'");
  }
  std::vector<BumpPtr<ast::Decl>> globalDecls{};
  while (true) {
    BumpPtr<ast::Decl> decl{parseGlobalDeclaration_()};
    if (!decl) break;
    globalDecls.push_back(std::move(decl));
    skip_();
    if (isEOF_()) break;
  }
  if (!isEOF_()) {
    if (startsWith(getRemainingSourceCode_(), "#search_dir"))
      srcLoc_.throwError("A '#search_dir' is only allowed at the top of the "
                         "file immediately after '#smdl'");
    throwUnexpectedToken_(srcLoc_, "expected a declaration");
  }
  BumpPtr<ast::File> file{allocate_<ast::File>(
      srcLoc0, std::in_place, orEmpty_(srcKwSMDLSyntax), std::move(searchDirs),
      std::move(version), std::move(importDecls), orEmpty_(srcKwModule),
      std::move(moduleAnnotations), orEmpty_(srcSemicolonAfterModule),
      std::move(globalDecls))};
  file->srcDocComment = srcDocComment;
  return file;
}

auto Parser::parseFileSearchDirs_() -> std::vector<ast::File::SearchDir> {
  std::vector<ast::File::SearchDir> searchDirs{};
  while (true) {
    skip_();
    SourceLocation srcLoc0{srcLoc_};
    std::optional<std::string_view> srcKwSearchDir{nextKeyword_("#search_dir")};
    if (!srcKwSearchDir) break;
    if (!isSMDL_)
      srcLoc0.throwError("A '#search_dir' requires the file to begin with "
                         "'#smdl'");
    BumpPtr<ast::LiteralString> path{parseLiteralStringExpression_()};
    if (!path)
      srcLoc0.throwError("Expected literal string path after '#search_dir'");
    searchDirs.push_back(
        ast::File::SearchDir{*srcKwSearchDir, std::move(path)});
  }
  return searchDirs;
}

auto Parser::parseFileVersion_() -> std::optional<ast::File::Version> {
  std::optional<Parser::ParsedToken> kwMDL{nextKeywordAndLocation_("mdl")};
  if (!kwMDL) return std::nullopt;
  SourceLocation srcLoc0{kwMDL->srcLoc};
  skip_();
  SourceLocation srcLoc1{srcLoc_};
  std::optional<std::string_view> srcMajor{nextInteger_()};
  std::optional<std::string_view> srcDot{next_(".")};
  std::optional<std::string_view> srcMinor{nextInteger_()};
  if (!srcMajor || !srcDot || !srcMinor)
    srcLoc0.throwError("Expected 'X.Y' version after 'mdl'");
  auto parseVersionNumber{[&](std::string_view srcNumber) {
    uint32_t number{};
    if (std::from_chars(srcNumber.data(), srcNumber.data() + srcNumber.size(),
                        number)
            .ec != std::errc())
      srcLoc0.throwError("Version number ", SpellQuoted(srcNumber),
                         " is out of range");
    return number;
  }};
  ast::File::Version version{};
  version.srcKwMDL = kwMDL->src;
  version.srcVersion =
      getSourceCode_().substr(srcLoc1.i, srcLoc_.i - srcLoc1.i);
  version.major = parseVersionNumber(*srcMajor);
  version.minor = parseVersionNumber(*srcMinor);
  std::optional<std::string_view> srcSemicolon{nextDelimiter_(";")};
  if (!srcSemicolon) srcLoc0.throwError("Expected ';' after 'mdl ...'");
  version.srcSemicolon = *srcSemicolon;
  return version;
}

auto Parser::parseImportPath_() -> std::optional<ast::ImportPath> {
  checkpoint_();
  std::vector<ast::ImportPath::Element> elements{};
  while (true) {
    checkpoint_();
    std::optional<std::string_view> srcDoubleColon{nextDelimiter_("::")};
    if (!srcDoubleColon && !elements.empty()) {
      reject_();
      break;
    }
    ast::ImportPath::Element element{};
    if (srcDoubleColon) {
      element.srcDoubleColon = *srcDoubleColon;
    }
    if (std::optional<std::string_view> srcName{nextDelimiter_("..")}) {
      element.srcName = *srcName;
    } else if (std::optional<std::string_view> srcName{nextDelimiter_(".")}) {
      element.srcName = *srcName;
    } else if (std::optional<std::string_view> srcName{nextDelimiter_("*")}) {
      element.srcName = *srcName;
    } else if (std::optional<ast::Name> name{parseSimpleName_()}) {
      element.srcName = name->srcName;
    } else if (BumpPtr<ast::LiteralString> literalString{
                   parseLiteralStringExpression_()}) {
      element.literalString = std::move(literalString);
    } else {
      reject_();
      break;
    }
    accept_();
    elements.push_back(std::move(element));
  }
  if (elements.empty()) {
    reject_();
    return std::nullopt;
  }
  accept_();
  return ast::ImportPath(std::move(elements));
}

auto Parser::parseUsingAlias_() -> BumpPtr<ast::UsingAlias> {
  SourceLocation srcLoc0{checkpoint_()};
  std::optional<std::string_view> srcKwUsing{nextKeyword_("using")};
  if (!srcKwUsing) {
    reject_();
    return nullptr;
  }
  std::optional<ast::Name> name{parseSimpleName_()};
  if (!name) {
    reject_();
    return nullptr;
  }
  std::optional<std::string_view> srcEqual{nextDelimiter_("=")};
  if (!srcEqual) {
    reject_();
    return nullptr;
  }
  std::optional<ast::ImportPath> importPath{parseImportPath_()};
  if (!importPath)
    srcLoc0.throwError("Expected import path after 'using ... ='");
  std::optional<std::string_view> srcSemicolon{nextDelimiter_(";")};
  if (!srcSemicolon) srcLoc0.throwError("Expected ';' after 'using ... = ...'");
  accept_();
  return allocate_<ast::UsingAlias>(srcLoc0, std::in_place, *srcKwUsing, *name,
                                    *srcEqual, std::move(*importPath),
                                    *srcSemicolon);
}

auto Parser::parseUsingImport_() -> BumpPtr<ast::UsingImport> {
  SourceLocation srcLoc0{checkpoint_()};
  std::optional<std::string_view> srcKwExport{nextKeyword_("export")};
  std::optional<std::string_view> srcKwUsing{nextKeyword_("using")};
  if (!srcKwUsing) {
    reject_();
    return nullptr;
  }
  std::optional<ast::ImportPath> importPath{parseImportPath_()};
  if (!importPath) {
    reject_();
    return nullptr;
  }
  if (importPath->isImportAll())
    srcLoc0.throwError(
        "Import path after '[export] using' must not end with '::*'");
  std::optional<std::string_view> srcKwImport{nextKeyword_("import")};
  if (!srcKwImport)
    srcLoc0.throwError("Expected 'import' after '[export] using ...'");
  std::vector<ast::UsingImport::Name> names{};
  if (std::optional<std::string_view> srcStar{nextDelimiter_("*")}) {
    names.push_back(ast::UsingImport::Name{*srcStar, {}});
  } else {
    parseCommaSeparated_(names, [&]() -> std::optional<ast::UsingImport::Name> {
      std::optional<ast::Name> name{parseSimpleName_()};
      if (!name) return std::nullopt;
      return ast::UsingImport::Name{name->srcName, {}};
    });
  }
  std::optional<std::string_view> srcSemicolon{nextDelimiter_(";")};
  if (!srcSemicolon)
    srcLoc0.throwError("Expected ';' after '[export] using ... import ...'");
  accept_();
  BumpPtr<ast::UsingImport> result{allocate_<ast::UsingImport>(
      srcLoc0, std::in_place, *srcKwUsing, std::move(*importPath), *srcKwImport,
      std::move(names), *srcSemicolon)};
  if (srcKwExport) result->srcKwExport = *srcKwExport;
  return result;
}

auto Parser::parseImport_() -> BumpPtr<ast::Import> {
  std::optional<Parser::ParsedToken> kwImport{
      nextKeywordAndLocation_("import")};
  if (!kwImport) return nullptr;
  std::vector<ast::Import::ImportPathWrapper> importPathWrappers{};
  parseCommaSeparated_(
      importPathWrappers,
      [&]() -> std::optional<ast::Import::ImportPathWrapper> {
        std::optional<ast::ImportPath> importPath{parseImportPath_()};
        if (!importPath) return std::nullopt;
        return ast::Import::ImportPathWrapper{std::move(*importPath), {}};
      });
  std::optional<std::string_view> srcSemicolon{nextDelimiter_(";")};
  if (!srcSemicolon)
    kwImport->srcLoc.throwError("Expected ';' after 'import ...'");
  return allocate_<ast::Import>(kwImport->srcLoc, std::in_place, kwImport->src,
                                std::move(importPathWrappers), *srcSemicolon);
}

auto Parser::parseAttributes_() -> std::optional<ast::Decl::Attributes> {
  std::optional<Parser::ParsedToken> srcAt{nextDelimiterAndLocation_("@")};
  if (!srcAt) return std::nullopt;
  SourceLocation srcLoc0{srcAt->srcLoc};
  ast::Function::Attributes attributes{};
  attributes.srcAt = srcAt->src;
  std::optional<std::string_view> srcParenL{nextDelimiter_("(")};
  if (!srcParenL)
    srcLoc0.throwError("Expected '@(...)' syntax for function attributes");
  attributes.srcParenL = *srcParenL;
  static constexpr std::array<std::string_view, 12> attrNames{
      "alwaysinline", "cold", "fastmath", "foreign", "hot",  "macro",
      "noinline",     "open", "optnone",  "optsize", "pure", "visible"};
  while (true) {
    std::optional<std::string_view> attr{
        [&]() -> std::optional<std::string_view> {
          for (auto attrName : attrNames)
            if (std::optional<std::string_view> srcAttr{nextKeyword_(attrName)})
              return srcAttr;
          return std::nullopt;
        }()};
    if (attr) {
      attributes.attrs.push_back(*attr);
    } else {
      skip_();
      if (peek_() != ')') {
        SourceLocation srcLocAttr{srcLoc_};
        if (std::optional<std::string_view> word{nextWord_()})
          srcLocAttr.throwError("Unrecognized attribute ", SpellQuoted(*word),
                                ", expected one of ", join(attrNames, ", "));
        srcLocAttr.throwError("Expected attribute name or ')' after '@('");
      }
      break;
    }
  }
  std::optional<std::string_view> srcParenR{nextDelimiter_(")")};
  if (!srcParenR) srcLoc0.throwError("Expected '@(...)' syntax for attributes");
  attributes.srcParenR = *srcParenR;
  return std::move(attributes);
}

auto Parser::parseGlobalDeclaration_() -> BumpPtr<ast::Decl> {
  SourceLocation srcLoc0{checkpoint_()};
  // Capture before parsing: comments inside the declaration must not
  // clobber the pending block first.
  std::string_view srcDocComment{getDocCommentBefore_(srcLoc0.i)};
  std::optional<ast::Decl::Attributes> attributes{parseAttributes_()};
  std::optional<std::string_view> srcKwExport{nextKeyword_("export")};
  BumpPtr<ast::Decl> decl{[&]() -> BumpPtr<ast::Decl> {
    if (BumpPtr<ast::Decl> decl{parseAnnotationDeclaration_()}) return decl;
    if (BumpPtr<ast::Function> decl{parseFunctionDeclaration_()}) return decl;
    if (BumpPtr<ast::Decl> decl{parseTypeDeclaration_()}) return decl;
    if (BumpPtr<ast::Variable> decl{parseVariableDeclaration_()}) return decl;
    if (isSMDL_) {
      if (BumpPtr<ast::Exec> decl{parseExecDeclaration_()}) return decl;
      if (BumpPtr<ast::UnitTest> decl{parseUnitTestDeclaration_()}) return decl;
      if (BumpPtr<ast::Namespace> decl{parseNamespaceDeclaration_()})
        return decl;
    }
    return nullptr;
  }()};
  if (!decl) {
    reject_();
    if (nextKeyword_("using") || nextKeyword_("import"))
      srcLoc0.throwError("All 'using' and 'import' declarations must appear "
                         "at the top of the file");
    return nullptr;
  }
  decl->isGlobal = true;
  decl->srcDocComment = srcDocComment;
  if (attributes) decl->attributes = std::move(attributes);
  if (srcKwExport) decl->srcKwExport = *srcKwExport;
  accept_();
  return decl;
}

auto Parser::parseAnnotationDeclaration_() -> BumpPtr<ast::Decl> {
  std::optional<Parser::ParsedToken> kwAnnotation{
      nextKeywordAndLocation_("annotation")};
  if (!kwAnnotation) return nullptr;
  SourceLocation srcLoc0{kwAnnotation->srcLoc};
  std::optional<ast::Name> name{parseSimpleName_()};
  if (!name) srcLoc0.throwError("Expected simple name after 'annotation'");
  std::optional<ast::ParameterList> params{parseParameterList_()};
  if (!params)
    srcLoc0.throwError(
        "Expected parameter list after 'annotation' declaration");
  BumpPtr<ast::AnnotationBlock> annotations{parseAnnotationBlock_()};
  std::optional<std::string_view> srcSemicolon{nextDelimiter_(";")};
  if (!srcSemicolon)
    srcLoc0.throwError("Expected ';' after 'annotation' declaration");
  return allocate_<ast::AnnotationDecl>(
      srcLoc0, std::in_place, kwAnnotation->src, std::move(*name),
      std::move(*params), std::move(annotations), *srcSemicolon);
}

auto Parser::parseTypeDeclaration_() -> BumpPtr<ast::Decl> {
  if (BumpPtr<ast::Typedef> decl{parseAliasTypeDeclaration_()}) return decl;
  if (BumpPtr<ast::Struct> decl{parseStructTypeDeclaration_()}) return decl;
  if (BumpPtr<ast::Enum> decl{parseEnumTypeDeclaration_()}) return decl;
  if (isSMDL_) {
    if (BumpPtr<ast::Tag> decl{parseTagDeclaration_()}) return decl;
  }
  return nullptr;
}

auto Parser::parseAliasTypeDeclaration_() -> BumpPtr<ast::Typedef> {
  std::optional<Parser::ParsedToken> kwTypedef{
      nextKeywordAndLocation_("typedef")};
  if (!kwTypedef) return nullptr;
  SourceLocation srcLoc0{kwTypedef->srcLoc};
  BumpPtr<ast::Type> type{parseType_()};
  if (!type) srcLoc0.throwError("Expected type after 'typedef'");
  std::optional<ast::Name> name{parseSimpleName_()};
  if (!name) srcLoc0.throwError("Expected name after 'typedef ...'");
  std::optional<std::string_view> srcSemicolon{nextDelimiter_(";")};
  if (!srcSemicolon) srcLoc0.throwError("Expected ';' after 'typedef ...'");
  return allocate_<ast::Typedef>(srcLoc0, std::in_place, kwTypedef->src,
                                 std::move(type), *name, *srcSemicolon);
}

auto Parser::parseStructTypeDeclaration_() -> BumpPtr<ast::Struct> {
  SourceLocation srcLoc0{checkpoint_()};
  std::optional<std::string_view> srcKwStruct{nextKeyword_("struct")};
  if (!srcKwStruct) {
    reject_();
    return nullptr;
  }
  std::optional<ast::Name> name{parseSimpleName_()};
  if (!name) srcLoc0.throwError("Expected name after 'struct'");
  std::vector<ast::Struct::Tag> tags{};
  std::optional<std::string_view> srcColonBeforeTags{nextDelimiter_(":")};
  if (srcColonBeforeTags) {
    parseCommaSeparated_(tags, [&]() -> std::optional<ast::Struct::Tag> {
      SourceLocation srcLoc1{srcLoc_};
      std::optional<std::string_view> srcKwDefault{nextKeyword_("default")};
      BumpPtr<ast::Identifier> tagName{parseIdentifier_()};
      if (!tagName) return std::nullopt;
      ast::Struct::Tag tag{};
      tag.srcKwDefault = orEmpty_(srcKwDefault);
      tag.type = allocate_<ast::Type>(srcLoc1, std::in_place,
                                      std::vector<std::string_view>(),
                                      std::move(tagName));
      return std::move(tag);
    });
  }
  BumpPtr<ast::AnnotationBlock> annotations{parseAnnotationBlock_()};
  std::optional<std::string_view> srcBraceL{nextDelimiter_("{")};
  if (!srcBraceL) srcLoc0.throwError("Expected '{' after 'struct ...'");
  std::vector<ast::Struct::Constructor> constructors{};
  std::vector<ast::Struct::Field> fields{};
  std::optional<std::string_view> srcKwFinalize{};
  BumpPtr<ast::Stmt> stmtFinalize{};
  // Parse constructors, which must appear at the top of the
  // struct declaration. This is an extension!
  while (true) {
    std::optional<ast::Struct::Constructor> constructor{
        parseStructConstructor_()};
    if (!constructor) break;
    if (constructor->name.srcName != name->srcName)
      constructor->name.srcLoc.throwError(
          "Constructor must name the containing struct ", SpellQuoted(*name));
    constructors.push_back(std::move(*constructor));
    skip_();
    if (peek_() == '}') break;
  }
  // Parse fields
  while (true) {
    std::optional<ast::Struct::Field> field{parseStructFieldDeclarator_()};
    if (!field) {
      // Parse finalize block, which must appear at the bottom of the
      // struct declaration if it appears at all. This is an extension!
      if (srcKwFinalize = nextKeyword_("finalize"); srcKwFinalize) {
        if (stmtFinalize = parseCompoundStatement_(); !stmtFinalize) {
          srcLoc0.throwError("Expected '{ ... }' after 'finalize'");
        }
      }
      break;
    }
    fields.push_back(std::move(*field));
    skip_();
    attachPendingTrailingDocComment_(fields);
    if (peek_() == '}') break;
  }
  std::optional<std::string_view> srcBraceR{nextDelimiter_("}")};
  if (!srcBraceR)
    throwUnexpectedToken_(srcLoc_,
                          "expected a field declarator or '}' in 'struct ...'");
  std::optional<std::string_view> srcSemicolon{nextDelimiter_(";")};
  if (!srcSemicolon)
    srcLoc0.throwError("Expected ';' after 'struct ... { ... }'");
  accept_();
  return allocate_<ast::Struct>(
      srcLoc0, std::in_place, *srcKwStruct, *name, orEmpty_(srcColonBeforeTags),
      std::move(tags), std::move(annotations), *srcBraceL,
      std::move(constructors), std::move(fields), orEmpty_(srcKwFinalize),
      std::move(stmtFinalize), *srcBraceR, *srcSemicolon);
}

auto Parser::parseStructConstructor_()
    -> std::optional<ast::Struct::Constructor> {
  SourceLocation srcLoc0{checkpoint_()};
  std::optional<ast::Name> name{parseSimpleName_()};
  if (!name) {
    reject_();
    return std::nullopt;
  }
  skip_();
  std::optional<ast::ParameterList> params{parseParameterList_()};
  if (!params) {
    reject_();
    return std::nullopt;
  }
  std::optional<std::string_view> srcEqual{nextDelimiter_("=")};
  if (!srcEqual) {
    reject_();
    return std::nullopt;
  }
  BumpPtr<ast::Expr> expr{parseExpression_()};
  if (!expr) {
    srcLoc0.throwError("Expected expression after '='");
  }
  std::optional<std::string_view> srcSemicolon{nextDelimiter_(";")};
  if (!srcSemicolon) {
    srcLoc0.throwError("Expected ';' after constructor expression");
  }
  accept_();
  return ast::Struct::Constructor{std::move(*name), std::move(*params),
                                  *srcEqual, std::move(expr), *srcSemicolon};
}

auto Parser::parseStructFieldDeclarator_()
    -> std::optional<ast::Struct::Field> {
  SourceLocation srcLoc0{checkpoint_()};
  std::string_view srcDocComment{getDocCommentBefore_(srcLoc0.i)};
  ast::Struct::Field field{};
  BumpPtr<ast::Type> type{parseType_()};
  if (!type) {
    reject_();
    return std::nullopt;
  }
  field.srcLoc = srcLoc0;
  field.srcDocComment = srcDocComment;
  field.type = std::move(type);
  std::optional<ast::Name> name{parseSimpleName_()};
  if (!name) {
    reject_();
    return std::nullopt;
  }
  field.name = *name;
  if (std::optional<std::string_view> srcEqual{nextDelimiter_("=")}) {
    BumpPtr<ast::Expr> exprInit{parseExpression_()};
    if (!exprInit)
      throwUnexpectedToken_(srcLoc_, "expected an initializer after '='");
    field.srcEqual = *srcEqual;
    field.exprInit = std::move(exprInit);
  }
  field.annotations = parseAnnotationBlock_();
  std::optional<std::string_view> srcSemicolon{nextDelimiter_(";")};
  if (!srcSemicolon) srcLoc_.throwError("Expected ';' after field declarator");
  field.srcSemicolon = *srcSemicolon;
  accept_();
  return std::move(field);
}

auto Parser::parseEnumTypeDeclaration_() -> BumpPtr<ast::Enum> {
  std::optional<Parser::ParsedToken> kwEnum{nextKeywordAndLocation_("enum")};
  if (!kwEnum) return nullptr;
  SourceLocation srcLoc0{kwEnum->srcLoc};
  std::optional<ast::Name> name{parseSimpleName_()};
  if (!name) srcLoc0.throwError("Expected name after 'enum'");
  BumpPtr<ast::AnnotationBlock> annotations{parseAnnotationBlock_()};
  std::optional<std::string_view> srcBraceL{nextDelimiter_("{")};
  if (!srcBraceL) srcLoc0.throwError("Expected '{' after 'enum ...'");
  std::vector<ast::Enum::Declarator> declarators{};
  parseCommaSeparated_(declarators,
                       [&] { return parseEnumValueDeclarator_(); });
  std::optional<std::string_view> srcBraceR{nextDelimiter_("}")};
  if (!srcBraceR)
    throwUnexpectedToken_(srcLoc_,
                          "expected a value declarator or '}' in 'enum ...'");
  std::optional<std::string_view> srcSemicolon{nextDelimiter_(";")};
  if (!srcSemicolon) srcLoc0.throwError("Expected ';' after 'enum ...'");
  return allocate_<ast::Enum>(
      srcLoc0, std::in_place, kwEnum->src, *name, std::move(annotations),
      *srcBraceL, std::move(declarators), *srcBraceR, *srcSemicolon);
}

auto Parser::parseEnumValueDeclarator_()
    -> std::optional<ast::Enum::Declarator> {
  SourceLocation srcLoc0{checkpoint_()};
  std::string_view srcDocComment{getDocCommentBefore_(srcLoc0.i)};
  std::optional<ast::Name> name{parseSimpleName_()};
  if (!name) {
    reject_();
    return std::nullopt;
  }
  ast::Enum::Declarator declarator{};
  declarator.srcLoc = srcLoc0;
  declarator.srcDocComment = srcDocComment;
  declarator.name = *name;
  if (std::optional<std::string_view> srcEqual{nextDelimiter_("=")}) {
    BumpPtr<ast::Expr> exprInit{parseAssignmentExpression_()};
    if (!exprInit)
      throwUnexpectedToken_(srcLoc_, "expected an initializer after '='");
    declarator.srcEqual = *srcEqual;
    declarator.exprInit = std::move(exprInit);
  }
  declarator.annotations = parseAnnotationBlock_();
  accept_();
  return std::move(declarator);
}

auto Parser::parseVariableDeclaration_() -> BumpPtr<ast::Variable> {
  SourceLocation srcLoc0{checkpoint_()};
  BumpPtr<ast::Type> type{parseType_()};
  if (!type) {
    reject_();
    return nullptr;
  }
  std::vector<ast::Variable::Declarator> declarators{};
  parseCommaSeparated_(declarators, [&] { return parseVariableDeclarator_(); });
  if (declarators.empty()) {
    reject_();
    return nullptr;
  }
  std::optional<std::string_view> srcSemicolon{nextDelimiter_(";")};
  if (!srcSemicolon)
    throwUnexpectedToken_(srcLoc_,
                          concat("expected ';' after the variable declaration "
                                 "starting at line ",
                                 srcLoc0.lineNo),
                          &srcLoc0);
  // Pick up a trailing documentation comment after the semicolon, e.g.,
  // `const int X = 0; ///< doc`.
  skip_();
  attachPendingTrailingDocComment_(declarators);
  accept_();
  return allocate_<ast::Variable>(srcLoc0, std::in_place, std::move(type),
                                  std::move(declarators), *srcSemicolon);
}

auto Parser::parseVariableDeclarator_()
    -> std::optional<ast::Variable::Declarator> {
  SourceLocation srcLoc0{checkpoint_()};
  ast::Variable::Declarator declarator{};
  declarator.srcLoc = srcLoc0;
  declarator.srcDocComment = getDocCommentBefore_(srcLoc0.i);
  if (std::optional<ast::Name> name{parseSimpleName_()}) {
    declarator.names.push_back(
        ast::Variable::Declarator::DeclaratorName{*name});
  } else if (std::optional<std::string_view> srcBraceL{nextDelimiter_("{")};
             srcBraceL && isSMDL_) {
    // Parse destructure syntax `{foo, bar, baz}`
    declarator.srcBraceL = *srcBraceL;
    parseCommaSeparated_(
        declarator.names,
        [&]() -> std::optional<ast::Variable::Declarator::DeclaratorName> {
          std::optional<ast::Name> name{parseSimpleName_()};
          if (!name) return std::nullopt;
          return ast::Variable::Declarator::DeclaratorName{*name};
        });
    std::optional<std::string_view> srcBraceR{nextDelimiter_("}")};
    // An empty destructure binds nothing, and accepting it would swallow
    // the '{}' of any declaration that merely begins with a name, e.g.
    // 'exec {}'.
    if (!srcBraceR || declarator.names.empty()) {
      reject_();
      return std::nullopt;
    }
    declarator.srcBraceR = *srcBraceR;
  } else {
    reject_();
    return std::nullopt;
  }
  if (std::optional<std::string_view> srcEqual{nextDelimiter_("=")}) {
    BumpPtr<ast::Expr> exprInit{parseAssignmentExpression_()};
    if (!exprInit)
      throwUnexpectedToken_(srcLoc_, "expected an initializer after '='");
    declarator.srcEqual = *srcEqual;
    declarator.exprInit = std::move(exprInit);
  } else if (std::optional<ast::ArgumentList> argsInit{parseArgumentList_()}) {
    declarator.argsInit = std::move(argsInit);
  }
  declarator.annotations = parseAnnotationBlock_();
  accept_();
  return std::move(declarator);
}

auto Parser::parseFunctionDeclaration_() -> BumpPtr<ast::Function> {
  SourceLocation srcLoc0{checkpoint_()};
  BumpPtr<ast::Type> type{parseType_()};
  if (!type) {
    reject_();
    return nullptr;
  }
  BumpPtr<ast::AnnotationBlock> earlyAnnotations{parseAnnotationBlock_()};
  std::optional<ast::Name> name{parseSimpleName_()};
  if (!name) {
    reject_();
    return nullptr;
  }
  std::optional<ast::ParameterList> params{parseParameterList_()};
  if (!params) {
    reject_();
    return nullptr;
  }
  std::optional<std::string_view> srcFrequency{
      nextKeyword_({"uniform", "varying"})};
  BumpPtr<ast::AnnotationBlock> lateAnnotations{parseAnnotationBlock_()};
  std::optional<std::string_view> srcEqual{};
  BumpPtr<ast::Node> definition{};
  std::optional<std::string_view> srcSemicolon{};
  skip_();
  if (params->isVariant() && peek_() != '=')
    srcLoc0.throwError(
        "Function variant must be defined by 'let' or call expression");
  if (srcSemicolon = nextDelimiter_(";"); srcSemicolon) {
    // Nothing
  } else if (srcEqual = nextDelimiter_("="); srcEqual) {
    skip_();
    SourceLocation srcLoc1{srcLoc_};
    BumpPtr<ast::Expr> def{parseExpression_()};
    if (!def) srcLoc0.throwError("Expected function expression after '='");
    if (srcSemicolon = nextDelimiter_(";"); !srcSemicolon)
      srcLoc0.throwError("Expected ';' after function expression");
    if (params->isVariant() && !llvm::isa<ast::Let>(def.get()) &&
        !llvm::isa<ast::Call>(def.get()))
      srcLoc0.throwError(
          "Function variant definition must be 'let' or call expression");
    definition = allocate_<ast::Return>(srcLoc1, std::in_place,
                                        std::string_view(), std::move(def),
                                        std::nullopt, std::string_view());
  } else {
    BumpPtr<ast::Compound> def{parseCompoundStatement_()};
    if (!def) srcLoc0.throwError("Expected ';' or function definition");
    definition = std::move(def);
  }
  accept_();
  return allocate_<ast::Function>(
      srcLoc0, std::in_place, std::move(type), std::move(earlyAnnotations),
      *name, std::move(*params), orEmpty_(srcFrequency),
      std::move(lateAnnotations), orEmpty_(srcEqual), std::move(definition),
      orEmpty_(srcSemicolon));
}

auto Parser::parseTagDeclaration_() -> BumpPtr<ast::Tag> {
  std::optional<Parser::ParsedToken> kwTag{nextKeywordAndLocation_("tag")};
  if (!kwTag) return nullptr;
  SourceLocation srcLoc0{kwTag->srcLoc};
  std::optional<ast::Name> name{parseSimpleName_()};
  if (!name) srcLoc0.throwError("Expected name after 'tag'");
  std::optional<std::string_view> srcSemicolon{nextDelimiter_(";")};
  if (!srcSemicolon) srcLoc0.throwError("Expected ';' after 'tag ...'");
  return allocate_<ast::Tag>(srcLoc0, std::in_place, kwTag->src, *name,
                             *srcSemicolon);
}

auto Parser::parseExecDeclaration_() -> BumpPtr<ast::Exec> {
  std::optional<Parser::ParsedToken> kwExec{nextKeywordAndLocation_("exec")};
  if (!kwExec) return nullptr;
  BumpPtr<ast::Compound> stmt{parseCompoundStatement_()};
  if (!stmt)
    kwExec->srcLoc.throwError("Expected compound statement after 'exec'");
  return allocate_<ast::Exec>(kwExec->srcLoc, std::in_place, kwExec->src,
                              std::move(stmt));
}

auto Parser::parseUnitTestDeclaration_() -> BumpPtr<ast::UnitTest> {
  std::optional<Parser::ParsedToken> kwUnitTest{
      nextKeywordAndLocation_("unit_test")};
  if (!kwUnitTest) return nullptr;
  SourceLocation srcLoc0{kwUnitTest->srcLoc};
  BumpPtr<ast::LiteralString> name{parseLiteralStringExpression_()};
  if (!name) srcLoc0.throwError("Expected literal string after 'unit_test'");
  BumpPtr<ast::Compound> stmt{parseCompoundStatement_()};
  if (!stmt)
    srcLoc0.throwError("Expected compound statement after 'unit_test ...'");
  return allocate_<ast::UnitTest>(srcLoc0, std::in_place, kwUnitTest->src,
                                  std::move(name), std::move(stmt));
}

auto Parser::parseNamespaceDeclaration_() -> BumpPtr<ast::Namespace> {
  std::optional<Parser::ParsedToken> kwNamespace{
      nextKeywordAndLocation_("namespace")};
  if (!kwNamespace) return nullptr;
  SourceLocation srcLoc0{kwNamespace->srcLoc};
  BumpPtr<ast::Identifier> identifier{parseIdentifier_()};
  if (!identifier) srcLoc0.throwError("Expected identifier after 'namespace'");
  std::optional<std::string_view> srcBraceL{nextDelimiter_("{")};
  if (!srcBraceL) srcLoc0.throwError("Expected '{' after 'namespace ...'");
  std::vector<BumpPtr<ast::Decl>> decls{};
  while (true) {
    BumpPtr<ast::Decl> decl{parseGlobalDeclaration_()};
    if (!decl) break;
    decls.push_back(std::move(decl));
    skip_();
    if (isEOF_()) break;
  }
  std::optional<std::string_view> srcBraceR{nextDelimiter_("}")};
  if (!srcBraceR)
    throwUnexpectedToken_(srcLoc_,
                          "expected a declaration or '}' in 'namespace ...'");
  return allocate_<ast::Namespace>(srcLoc0, std::in_place, kwNamespace->src,
                                   std::move(identifier), *srcBraceL,
                                   std::move(decls), *srcBraceR);
}
//--}

//--{ Parse: Stmt
auto Parser::parseStatement_() -> BumpPtr<ast::Stmt> {
  ParseDepthGuard depthGuard{*this};
  skip_();
  SourceLocation srcLoc0{srcLoc_};
  if (BumpPtr<ast::Compound> stmt{parseCompoundStatement_()}) return stmt;
  if (BumpPtr<ast::If> stmt{parseIfStatement_()}) return stmt;
  if (BumpPtr<ast::Switch> stmt{parseSwitchStatement_()}) return stmt;
  if (BumpPtr<ast::While> stmt{parseWhileStatement_()}) return stmt;
  if (BumpPtr<ast::DoWhile> stmt{parseDoStatement_()}) return stmt;
  if (BumpPtr<ast::For> stmt{parseForStatement_()}) return stmt;
  if (BumpPtr<ast::Break> stmt{parseBreakStatement_()}) return stmt;
  if (BumpPtr<ast::Continue> stmt{parseContinueStatement_()}) return stmt;
  if (BumpPtr<ast::Return> stmt{parseReturnStatement_()}) return stmt;
  if (isSMDL_) {
    if (BumpPtr<ast::Unreachable> stmt{parseUnreachableStatement_()})
      return stmt;
    if (BumpPtr<ast::Preserve> stmt{parsePreserveStatement_()}) return stmt;
    if (BumpPtr<ast::Defer> stmt{parseDeferStatement_()}) return stmt;
    if (BumpPtr<ast::Visit> stmt{parseVisitStatement_()}) return stmt;
  }
  if (BumpPtr<ast::Decl> decl{parseTypeDeclaration_()})
    return allocate_<ast::DeclStmt>(srcLoc0, std::in_place, std::move(decl));
  if (BumpPtr<ast::Variable> decl{parseVariableDeclaration_()})
    return allocate_<ast::DeclStmt>(srcLoc0, std::in_place, std::move(decl));
  if (std::optional<std::string_view> srcSemicolon{nextDelimiter_(";")})
    return allocate_<ast::ExprStmt>(srcLoc0, std::in_place, nullptr,
                                    std::nullopt, *srcSemicolon);
  if (BumpPtr<ast::Expr> expr{parseExpression_()}) {
    std::optional<ast::LateIf> lateIf{parseLateIf_()};
    std::optional<std::string_view> srcSemicolon{nextDelimiter_(";")};
    if (!srcSemicolon)
      throwUnexpectedToken_(srcLoc_, "expected ';' after expression", &srcLoc0);
    return allocate_<ast::ExprStmt>(srcLoc0, std::in_place, std::move(expr),
                                    std::move(lateIf), *srcSemicolon);
  }
  return nullptr;
}

auto Parser::parseCompoundStatement_() -> BumpPtr<ast::Compound> {
  std::optional<Parser::ParsedToken> braceL{nextDelimiterAndLocation_("{")};
  if (!braceL) return nullptr;
  std::vector<BumpPtr<ast::Stmt>> stmts{};
  while (true) {
    BumpPtr<ast::Stmt> stmt{parseStatement_()};
    if (!stmt) break;
    stmts.push_back(std::move(stmt));
    skip_();
    if (peek_() == '}') break;
  }
  std::optional<std::string_view> srcBraceR{nextDelimiter_("}")};
  if (!srcBraceR)
    throwUnexpectedToken_(
        srcLoc_, concat("expected a statement or '}' to close the block "
                        "starting at line ",
                        braceL->srcLoc.lineNo));
  return allocate_<ast::Compound>(braceL->srcLoc, std::in_place, braceL->src,
                                  std::move(stmts), *srcBraceR);
}

auto Parser::parseIfStatement_() -> BumpPtr<ast::If> {
  std::optional<Parser::ParsedToken> kwIf{nextKeywordAndLocation_("if")};
  if (!kwIf) return nullptr;
  SourceLocation srcLoc0{kwIf->srcLoc};
  BumpPtr<ast::Expr> exprCond{parseExpressionInParentheses_()};
  if (!exprCond)
    srcLoc0.throwError("Expected parenthesized condition after 'if'");
  BumpPtr<ast::Stmt> ifPass{parseStatement_()};
  if (!ifPass) srcLoc0.throwError("Expected statement after 'if (...)'");
  if (std::optional<std::string_view> srcKwElse{nextKeyword_("else")}) {
    BumpPtr<ast::Stmt> ifFail{parseStatement_()};
    if (!ifFail) srcLoc0.throwError("Expected statement after 'else'");
    return allocate_<ast::If>(srcLoc0, std::in_place, kwIf->src,
                              std::move(exprCond), std::move(ifPass),
                              *srcKwElse, std::move(ifFail));
  } else {
    return allocate_<ast::If>(srcLoc0, std::in_place, kwIf->src,
                              std::move(exprCond), std::move(ifPass),
                              std::string_view(), nullptr);
  }
}

auto Parser::parseSwitchStatement_() -> BumpPtr<ast::Switch> {
  std::optional<Parser::ParsedToken> kwSwitch{
      nextKeywordAndLocation_("switch")};
  if (!kwSwitch) return nullptr;
  SourceLocation srcLoc0{kwSwitch->srcLoc};
  BumpPtr<ast::Expr> expr{parseExpressionInParentheses_()};
  if (!expr)
    srcLoc0.throwError("Expected parenthesized expression after 'switch'");
  std::optional<std::string_view> srcBraceL{nextDelimiter_("{")};
  if (!srcBraceL) srcLoc0.throwError("Expected opening '{' after 'switch'");
  std::vector<ast::Switch::Case> switchCases{};
  while (true) {
    std::optional<ast::Switch::Case> switchCase{parseSwitchCase_()};
    if (!switchCase) break;
    switchCases.push_back(std::move(*switchCase));
    skip_();
    if (peek_() == '}') break;
  }
  std::optional<std::string_view> srcBraceR{nextDelimiter_("}")};
  if (!srcBraceR)
    throwUnexpectedToken_(srcLoc_,
                          "expected 'case', 'default', or '}' in 'switch'");
  return allocate_<ast::Switch>(srcLoc0, std::in_place, kwSwitch->src,
                                std::move(expr), *srcBraceL,
                                std::move(switchCases), *srcBraceR);
}

auto Parser::parseSwitchCase_() -> std::optional<ast::Switch::Case> {
  skip_();
  SourceLocation srcLoc0{srcLoc_};
  ast::Switch::Case switchCase{};
  if (std::optional<std::string_view> srcKwCase{nextKeyword_("case")}) {
    BumpPtr<ast::Expr> expr{parseExpression_()};
    if (!expr) srcLoc0.throwError("Expected expression after 'case'");
    std::optional<std::string_view> srcColon{nextDelimiter_(":")};
    if (!srcColon) srcLoc0.throwError("Expected ':' after 'case ...'");
    switchCase.srcKwCaseOrDefault = *srcKwCase;
    switchCase.expr = std::move(expr);
    switchCase.srcColon = *srcColon;
  } else if (std::optional<std::string_view> srcKwDefault{
                 nextKeyword_("default")}) {
    std::optional<std::string_view> srcColon{nextDelimiter_(":")};
    if (!srcColon) srcLoc0.throwError("Expected ':' after 'default'");
    switchCase.srcKwCaseOrDefault = *srcKwDefault;
    switchCase.srcColon = *srcColon;
  } else {
    return std::nullopt;
  }
  while (true) {
    BumpPtr<ast::Stmt> stmt{parseStatement_()};
    if (!stmt) break;
    switchCase.stmts.push_back(std::move(stmt));
    skip_();
  }
  return std::move(switchCase);
}

auto Parser::parseWhileStatement_() -> BumpPtr<ast::While> {
  std::optional<Parser::ParsedToken> kwWhile{nextKeywordAndLocation_("while")};
  if (!kwWhile) return nullptr;
  SourceLocation srcLoc0{kwWhile->srcLoc};
  BumpPtr<ast::Expr> expr{parseExpressionInParentheses_()};
  if (!expr)
    srcLoc0.throwError("Expected parenthesized expression after 'while'");
  BumpPtr<ast::Stmt> stmt{parseStatement_()};
  if (!stmt) srcLoc0.throwError("Expected statement after 'while (...)'");
  return allocate_<ast::While>(srcLoc0, std::in_place, kwWhile->src,
                               std::move(expr), std::move(stmt));
}

auto Parser::parseDoStatement_() -> BumpPtr<ast::DoWhile> {
  std::optional<Parser::ParsedToken> kwDo{nextKeywordAndLocation_("do")};
  if (!kwDo) return nullptr;
  SourceLocation srcLoc0{kwDo->srcLoc};
  BumpPtr<ast::Stmt> stmt{parseStatement_()};
  if (!stmt) srcLoc0.throwError("Expected statement after 'do'");
  std::optional<std::string_view> srcKwWhile{nextKeyword_("while")};
  if (!srcKwWhile) srcLoc0.throwError("Expected 'while' after 'do ...'");
  BumpPtr<ast::Expr> expr{parseExpressionInParentheses_()};
  if (!expr)
    srcLoc0.throwError(
        "Expected parenthesized expression after 'do ... while'");
  std::optional<std::string_view> srcSemicolon{nextDelimiter_(";")};
  if (!srcSemicolon)
    srcLoc0.throwError("Expected ';' after 'do ... while (...)'");
  return allocate_<ast::DoWhile>(srcLoc0, std::in_place, kwDo->src,
                                 std::move(stmt), *srcKwWhile, std::move(expr),
                                 *srcSemicolon);
}

auto Parser::parseForStatement_() -> BumpPtr<ast::For> {
  std::optional<Parser::ParsedToken> kwFor{nextKeywordAndLocation_("for")};
  if (!kwFor) return nullptr;
  SourceLocation srcLoc0{kwFor->srcLoc};
  std::optional<std::string_view> srcParenL{nextDelimiter_("(")};
  if (!srcParenL) srcLoc0.throwError("Expected '(' after 'for'");
  BumpPtr<ast::Stmt> stmtInit{};
  if (BumpPtr<ast::Variable> decl{parseVariableDeclaration_()}) {
    stmtInit =
        allocate_<ast::DeclStmt>(decl->srcLoc, std::in_place, std::move(decl));
  } else if (BumpPtr<ast::Expr> expr{parseExpression_()}) {
    std::optional<std::string_view> srcSemicolon{nextDelimiter_(";")};
    if (!srcSemicolon)
      throwUnexpectedToken_(srcLoc_, "expected ';' after expression", &srcLoc0);
    stmtInit =
        allocate_<ast::ExprStmt>(expr->srcLoc, std::in_place, std::move(expr),
                                 std::nullopt, *srcSemicolon);
  } else {
    srcLoc0.throwError(
        "Expected variable declaration or expression after 'for ('");
  }
  BumpPtr<ast::Expr> exprCond{parseExpression_()};
  std::optional<std::string_view> srcSemicolonAfterCond{nextDelimiter_(";")};
  if (!srcSemicolonAfterCond)
    srcLoc0.throwError("Expected ';' after 'for (... ; ...'");
  BumpPtr<ast::Expr> exprIncr{parseExpression_()};
  std::optional<std::string_view> srcParenR{nextDelimiter_(")")};
  if (!srcParenR) srcLoc0.throwError("Expected ')' after 'for (...'");
  BumpPtr<ast::Stmt> stmt{parseStatement_()};
  if (!stmt) srcLoc0.throwError("Expected statement after 'for (...)'");
  return allocate_<ast::For>(srcLoc0, std::in_place, kwFor->src, *srcParenL,
                             std::move(stmtInit), std::move(exprCond),
                             *srcSemicolonAfterCond, std::move(exprIncr),
                             *srcParenR, std::move(stmt));
}

auto Parser::parseBreakStatement_() -> BumpPtr<ast::Break> {
  return parseJumpStatement_<ast::Break>("break");
}

auto Parser::parseContinueStatement_() -> BumpPtr<ast::Continue> {
  return parseJumpStatement_<ast::Continue>("continue");
}

auto Parser::parseReturnStatement_() -> BumpPtr<ast::Return> {
  std::optional<Parser::ParsedToken> kwReturn{
      nextKeywordAndLocation_("return")};
  if (!kwReturn) return nullptr;
  BumpPtr<ast::Expr> expr{parseExpression_()}; // Allow this to be null!
  std::optional<ast::LateIf> lateIf{parseLateIf_()};
  std::optional<std::string_view> srcSemicolon{nextDelimiter_(";")};
  if (!srcSemicolon)
    throwUnexpectedToken_(srcLoc_, "expected ';' after 'return ...'",
                          &kwReturn->srcLoc);
  return allocate_<ast::Return>(kwReturn->srcLoc, std::in_place, kwReturn->src,
                                std::move(expr), std::move(lateIf),
                                *srcSemicolon);
}

auto Parser::parseUnreachableStatement_() -> BumpPtr<ast::Unreachable> {
  std::optional<Parser::ParsedToken> kwUnreachable{
      nextKeywordAndLocation_("unreachable")};
  if (!kwUnreachable) return nullptr;
  std::optional<std::string_view> srcSemicolon{nextDelimiter_(";")};
  if (!srcSemicolon)
    kwUnreachable->srcLoc.throwError("Expected ';' after 'unreachable'");
  return allocate_<ast::Unreachable>(kwUnreachable->srcLoc, std::in_place,
                                     kwUnreachable->src, *srcSemicolon);
}

auto Parser::parsePreserveStatement_() -> BumpPtr<ast::Preserve> {
  std::optional<Parser::ParsedToken> kwPreserve{
      nextKeywordAndLocation_("preserve")};
  if (!kwPreserve) return nullptr;
  std::vector<ast::Preserve::ExprWrapper> exprs{};
  parseCommaSeparated_(exprs,
                       [&]() -> std::optional<ast::Preserve::ExprWrapper> {
                         BumpPtr<ast::Expr> expr{parseUnaryExpression_()};
                         if (!expr) return std::nullopt;
                         return ast::Preserve::ExprWrapper{std::move(expr), {}};
                       });
  std::optional<std::string_view> srcSemicolon{nextDelimiter_(";")};
  if (!srcSemicolon)
    kwPreserve->srcLoc.throwError("Expected ';' after 'preserve ...'");
  return allocate_<ast::Preserve>(kwPreserve->srcLoc, std::in_place,
                                  kwPreserve->src, std::move(exprs),
                                  *srcSemicolon);
}

auto Parser::parseDeferStatement_() -> BumpPtr<ast::Defer> {
  std::optional<Parser::ParsedToken> kwDefer{nextKeywordAndLocation_("defer")};
  if (!kwDefer) return nullptr;
  BumpPtr<ast::Stmt> stmt{parseStatement_()};
  if (!stmt) kwDefer->srcLoc.throwError("Expected statement after 'defer'");
  return allocate_<ast::Defer>(kwDefer->srcLoc, std::in_place, kwDefer->src,
                               std::move(stmt));
}

auto Parser::parseVisitStatement_() -> BumpPtr<ast::Visit> {
  std::optional<Parser::ParsedToken> kwVisit{nextKeywordAndLocation_("visit")};
  if (!kwVisit) return nullptr;
  SourceLocation srcLoc0{kwVisit->srcLoc};
  std::optional<ast::Name> name{parseSimpleName_()};
  if (!name) srcLoc0.throwError("Expected name after 'visit'");
  std::optional<std::string_view> srcKwIn{nextKeyword_("in")};
  if (!srcKwIn) srcLoc0.throwError("Expected 'in' after 'visit ...'");
  BumpPtr<ast::Expr> expr{parseExpression_()};
  if (!expr) srcLoc0.throwError("Expected expression after 'visit ... in'");
  BumpPtr<ast::Compound> stmt{parseCompoundStatement_()};
  if (!stmt)
    srcLoc0.throwError("Expected compound statement after 'visit ... in ...'");
  return allocate_<ast::Visit>(srcLoc0, std::in_place, kwVisit->src, *name,
                               *srcKwIn, std::move(expr), std::move(stmt));
}

auto Parser::parseLateIf_() -> std::optional<ast::LateIf> {
  if (!isSMDL_) return std::nullopt;
  std::optional<Parser::ParsedToken> kwIf{nextKeywordAndLocation_("if")};
  if (!kwIf) return std::nullopt;
  BumpPtr<ast::Expr> expr{parseExpressionInParentheses_()};
  if (!expr)
    kwIf->srcLoc.throwError(
        "Expected expression in parentheses after '... if'");
  return ast::LateIf(kwIf->src, std::move(expr));
}
//--}

} // namespace smdl
