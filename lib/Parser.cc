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
char Parser::next() {
  if (isEOF()) return '\0';
  char ch{peek()};
  if (ch == '\n') {
    mSrcLoc.lineNo++;
    mSrcLoc.charNo = 1;
  } else {
    mSrcLoc.charNo++;
  }
  mSrcLoc.i++;
  return ch;
}

std::string_view Parser::next(size_t n) {
  std::string_view result{getRemainingSourceCode().substr(0, n)};
  for (size_t i = 0; i < n && !isEOF(); i++) next();
  return result;
}

std::optional<std::string_view> Parser::next(std::string_view str) {
  if (startsWith(getRemainingSourceCode(), str)) return next(str.size());
  return std::nullopt;
}

std::optional<std::string_view> Parser::nextKeyword(std::string_view str) {
  checkpoint();
  std::optional<std::string_view> result{next(str)};
  if (!result || isWord(peek())) {
    reject();
    return std::nullopt;
  } else {
    accept();
    return result;
  }
}

std::string_view Parser::tokenAt(uint64_t i) const {
  std::string_view src{getSourceCode()};
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

std::string_view Parser::peekToken() {
  skip();
  return tokenAt(mSrcLoc.i);
}

std::string Parser::explainToken(std::string_view token) const {
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
  if (!mIsSMDL) {
    static constexpr std::string_view extensions[]{
        "defer", "exec",      "namespace",   "return_from", "static",
        "tag",   "unit_test", "unreachable", "visit",
    };
    bool isExtension{token.size() > 1 && (token[0] == '#' || token[0] == '$')};
    for (auto extension : extensions) isExtension |= token == extension;
    if (isExtension)
      return concat("; ", Quoted(token),
                    " is a SpectralMDL extension, so the file must begin "
                    "with '#smdl'");
  }
  return {};
}

void Parser::throwUnexpectedToken(const SourceLocation &srcLoc,
                                  std::string_view message,
                                  const SourceLocation *srcLocStart) {
  std::string_view token{peekToken()};
  std::string explanation{explainToken(token)};
  // Neither end explains it, so look through the construct itself: a
  // borrowed keyword often sits in the middle, as 'new' does in
  // 'auto p = new int(3);', which fails at 'int'.
  if (explanation.empty() && srcLocStart) {
    const std::string_view src{getSourceCode()};
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
      std::string_view tokenInside{tokenAt(i)};
      if (tokenInside.empty()) break;
      if (std::string hit{explainToken(tokenInside)}; !hit.empty()) {
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
      concat(message, ", but found ", Quoted(token), explanation));
}

std::optional<std::string_view> Parser::nextWord() {
  checkpoint();
  uint64_t i{mSrcLoc.i};
  if (mIsSMDL && peek() == '$') // The `$` prefix is extended syntax!
    next();
  if (isAlpha(peek()) || peek() == '_') {
    next();
    while (isWord(peek())) next();
    accept();
    return getSourceCode().substr(i, mSrcLoc.i - i);
  } else {
    reject();
    return std::nullopt;
  }
}

std::optional<std::string_view> Parser::nextInteger() {
  uint64_t i{mSrcLoc.i};
  while (isDigit(peek())) next();
  if (mSrcLoc.i > i) {
    return getSourceCode().substr(i, mSrcLoc.i - i);
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

void Parser::skip() {
  auto skipSome{[&] {
    if (startsWith(getRemainingSourceCode(), "//")) {
      uint64_t iComment{mSrcLoc.i};
      next(2);
      while (!isEOF() && peek() != '\n') next(1);
      // A `///` comment (but not a `///<` trailing comment) is a
      // documentation line: remember it so that declaration parsers can
      // pick it up with `getDocCommentBefore()`, merging consecutive
      // lines into one block. Every other comment breaks the pending
      // block. A rewind by `reject()` may re-scan the block, in which
      // case `iComment <= mPendingDocCommentEnd` restarts it in place.
      std::string_view comment{
          getSourceCode().substr(iComment, mSrcLoc.i - iComment)};
      if (startsWith(comment, "///<")) {
        // A `///<` comment is a trailing documentation line for the
        // item it follows, picked up by
        // `attachPendingTrailingDocComment()`. It also breaks any
        // pending leading block.
        mPendingTrailingDocCommentBegin = iComment;
        mPendingTrailingDocCommentEnd = mSrcLoc.i;
        mPendingDocCommentBegin = mPendingDocCommentEnd = 0;
      } else if (startsWith(comment, "///")) {
        if (mPendingDocCommentBegin == mPendingDocCommentEnd ||
            iComment <= mPendingDocCommentEnd ||
            !isDocCommentAdjacent(getSourceCode().substr(
                mPendingDocCommentEnd, iComment - mPendingDocCommentEnd)))
          mPendingDocCommentBegin = iComment;
        mPendingDocCommentEnd = mSrcLoc.i;
      } else {
        mPendingDocCommentBegin = mPendingDocCommentEnd = 0;
      }
      return true;
    } else if (startsWith(getRemainingSourceCode(), "/*")) {
      SourceLocation srcLocComment{mSrcLoc};
      next(2);
      while (!isEOF() && !startsWith(getRemainingSourceCode(), "*/")) next(1);
      if (isEOF()) srcLocComment.throwError("Unterminated multiline comment");
      next(2);
      mPendingDocCommentBegin = mPendingDocCommentEnd = 0;
      return true;
    } else if (isSpace(peek())) {
      next(1);
      return true;
    } else {
      return false;
    }
  }};
  while (!isEOF() && skipSome()) continue;
}

std::string_view Parser::getDocCommentBefore(size_t srcIndex) const {
  if (mPendingDocCommentBegin == mPendingDocCommentEnd ||
      srcIndex < mPendingDocCommentEnd ||
      !isDocCommentAdjacent(getSourceCode().substr(
          mPendingDocCommentEnd, srcIndex - mPendingDocCommentEnd)))
    return {};
  return getPendingDocComment();
}

bool Parser::pendingTrailingDocCommentTrailsCode() const {
  for (auto i{mPendingTrailingDocCommentBegin};
       i > 0 && getSourceCode()[i - 1] != '\n'; i--) {
    if (!isSpace(getSourceCode()[i - 1])) return true;
  }
  return false;
}
//--}

//--{ Parse: Expr
auto Parser::parseSimpleName() -> std::optional<AST::Name> {
  SourceLocation srcLoc0{checkpoint()};
  if (std::optional<std::string_view> name{nextWord()}) {
    // NOTE: These must remain sorted for `std::binary_search`!
    static constexpr std::string_view keywords[]{
        "break",   "case",   "cast", "const",   "continue", "default",
        "do",      "else",   "enum", "export",  "false",    "for",
        "if",      "import", "let",  "module",  "package",  "return",
        "struct",  "switch", "true", "typedef", "uniform",  "using",
        "varying", "while",
    };
    static constexpr std::string_view keywordsSmdlSyntax[]{
        "defer",  "inline", "namespace",   "return_from",
        "static", "tag",    "unreachable", "visit",
    };
    bool isKeyword{
        std::binary_search(std::begin(keywords), std::end(keywords), *name) ||
        (mIsSMDL && std::binary_search(std::begin(keywordsSmdlSyntax),
                                       std::end(keywordsSmdlSyntax), *name))};
    if (!isKeyword) {
      accept();
      return AST::Name{srcLoc0, *name};
    }
  }
  reject();
  return std::nullopt;
}

auto Parser::parseIdentifier() -> BumpPtr<AST::Identifier> {
  SourceLocation srcLoc0{checkpoint()};
  std::vector<AST::Identifier::Element> elements{};
  std::optional<std::string_view> srcDoubleColon{next("::")};
  if (std::optional<AST::Name> name{parseSimpleName()}) {
    elements.push_back(
        AST::Identifier::Element{orEmpty(srcDoubleColon), *name});
  } else {
    if (srcDoubleColon) {
      srcLoc0.throwError("Expected name after '::'");
    } else {
      reject();
      return nullptr;
    }
  }
  while (true) {
    checkpoint();
    if (srcDoubleColon = next("::"); srcDoubleColon) {
      if (std::optional<AST::Name> name{parseSimpleName()}) {
        elements.push_back(AST::Identifier::Element{*srcDoubleColon, *name});
        accept();
        continue;
      }
    }
    reject();
    break;
  }
  if (mSrcLoc.i > srcLoc0.i) {
    accept();
    return allocate<AST::Identifier>(srcLoc0, std::in_place,
                                     std::move(elements));
  } else {
    reject();
    return nullptr;
  }
}

auto Parser::parseType() -> BumpPtr<AST::Type> {
  SourceLocation srcLoc0{checkpoint()};
  std::vector<std::string_view> srcQuals{};
  while (true) {
    checkpoint();
    // The `inline` and `static` qualifiers are extended syntax!
    if (std::optional<std::string_view> srcQual{
            mIsSMDL ? nextKeyword(
                          {"const", "inline", "static", "uniform", "varying"})
                    : nextKeyword({"const", "uniform", "varying"})}) {
      accept();
      srcQuals.push_back(*srcQual);
    } else {
      reject();
      break;
    }
  }
  BumpPtr<AST::Expr> expr{parseUnaryExpression()};
  if (!expr) {
    reject();
    return nullptr;
  }
  accept();
  return allocate<AST::Type>(srcLoc0, std::in_place, std::move(srcQuals),
                             std::move(expr));
}

auto Parser::parseParameter() -> std::optional<AST::Parameter> {
  SourceLocation srcLoc0{checkpoint()};
  // Capture before parsing: comments inside the parameter must not
  // clobber the pending block first.
  std::string_view srcDocComment{getDocCommentBefore(srcLoc0.i)};
  BumpPtr<AST::Type> type{parseType()};
  if (!type) {
    reject();
    return std::nullopt;
  }
  std::optional<AST::Name> name{parseSimpleName()};
  if (!name) {
    reject();
    return std::nullopt;
  }
  AST::Parameter param{};
  param.srcLoc = srcLoc0;
  param.srcDocComment = srcDocComment;
  param.type = std::move(type);
  param.name = *name;
  if (std::optional<std::string_view> srcEqual{nextDelimiter("=")}) {
    BumpPtr<AST::Expr> exprInit{parseAssignmentExpression()};
    if (!exprInit)
      throwUnexpectedToken(mSrcLoc, "expected an initializer after '='");
    param.srcEqual = *srcEqual;
    param.exprInit = std::move(exprInit);
  }
  param.annotations = parseAnnotationBlock();
  accept();
  return std::move(param);
}

auto Parser::parseParameterList() -> std::optional<AST::ParameterList> {
  checkpoint();
  AST::ParameterList params{};
  std::optional<std::string_view> srcParenL{nextDelimiter("(")};
  if (!srcParenL) {
    reject();
    return std::nullopt;
  }
  params.srcParenL = *srcParenL;
  skip();
  if (std::optional<std::string_view> srcStar{nextDelimiter("*")}) {
    params.srcStar = *srcStar;
  } else {
    params.params.reserve(4);
    parseCommaSeparated(params.params, [&] { return parseParameter(); });
    std::optional<std::string_view> srcEllipsis{nextDelimiter("...")};
    if (srcEllipsis) {
      // The last parameter must have a trailing comma if the parameter list
      // features a variadic ellipsis.
      if (!params.params.empty() && params.params.back().srcComma.empty()) {
        reject();
        return std::nullopt;
      }
      params.srcEllipsis = *srcEllipsis;
    }
  }
  std::optional<std::string_view> srcParenR{nextDelimiter(")")};
  if (!srcParenR) {
    reject();
    return std::nullopt;
  }
  params.srcParenR = *srcParenR;
  accept();
  return std::move(params);
}

auto Parser::parseArgument() -> std::optional<AST::Argument> {
  SourceLocation srcLoc0{checkpoint()};
  AST::Argument argument{};
  argument.srcLoc = srcLoc0;
  if (mIsSMDL) {
    if (std::optional<std::string_view> srcKwVisit{nextKeyword("visit")}) {
      argument.srcKwVisit = *srcKwVisit;
    }
    if (std::optional<std::string_view> srcKwInline{nextKeyword("inline")}) {
      argument.srcKwInline = *srcKwInline;
      if (!argument.srcKwVisit.empty())
        srcLoc0.throwError(
            "Cannot combine 'visit' and 'inline' on an argument");
    }
  }
  argument.name = [&]() -> AST::Name {
    checkpoint();
    if (std::optional<AST::Name> name{parseSimpleName()}) {
      if (std::optional<std::string_view> srcColon{nextDelimiter(":")};
          srcColon && peek() != ':' && peek() != '=') {
        argument.srcColonAfterName = *srcColon;
        accept();
        return *name;
      }
    }
    reject();
    return {};
  }();
  argument.expr = parseAssignmentExpression();
  if (!argument.expr) {
    reject();
    return std::nullopt;
  }
  argument.src = getSourceCodeBetween(srcLoc0, mSrcLoc);
  accept();
  return std::move(argument);
}

auto Parser::parseArgumentList() -> std::optional<AST::ArgumentList> {
  SourceLocation srcLoc0{checkpoint()};
  AST::ArgumentList args{};
  args.srcLoc = srcLoc0;
  std::optional<std::string_view> srcParenL{nextDelimiter("(")};
  if (!srcParenL) {
    reject();
    return std::nullopt;
  }
  args.srcParenL = *srcParenL;
  parseCommaSeparated(args.args, [&] { return parseArgument(); });
  std::optional<std::string_view> srcParenR{nextDelimiter(")")};
  if (!srcParenR) {
    reject();
    return std::nullopt;
  }
  args.srcParenR = *srcParenR;
  accept();
  return std::move(args);
}

auto Parser::parseAnnotation() -> std::optional<AST::Annotation> {
  checkpoint();
  BumpPtr<AST::Identifier> identifier{parseIdentifier()};
  if (!identifier) {
    reject();
    return std::nullopt;
  }
  std::optional<AST::ArgumentList> args{parseArgumentList()};
  if (!args) {
    reject();
    return std::nullopt;
  }
  accept();
  return AST::Annotation{std::move(identifier), std::move(*args)};
}

auto Parser::parseAnnotationBlock() -> BumpPtr<AST::AnnotationBlock> {
  std::optional<Parser::ParsedToken> brackL{nextDelimiterAndLocation("[[")};
  if (!brackL) return nullptr;
  std::vector<AST::Annotation> annos{};
  parseCommaSeparated(annos, [&] { return parseAnnotation(); }, "]]");
  std::optional<std::string_view> srcDoubleBrackR{nextDelimiter("]]")};
  if (!srcDoubleBrackR)
    mSrcLoc.throwError("Expected annotation, ',', or ']]' in annotation block");
  return allocate<AST::AnnotationBlock>(brackL->srcLoc, std::in_place,
                                        brackL->src, std::move(annos),
                                        *srcDoubleBrackR);
}

auto Parser::parseExpressionInParentheses() -> BumpPtr<AST::Expr> {
  SourceLocation srcLoc0{checkpoint()};
  std::optional<std::string_view> srcDollar{nextDelimiter("$")};
  std::optional<std::string_view> srcParenL{nextDelimiter("(")};
  if (!srcParenL) {
    reject();
    return nullptr;
  }
  BumpPtr<AST::Expr> expr{parseExpression()};
  if (!expr) {
    reject();
    return nullptr;
  }
  std::optional<std::string_view> srcParenR{nextDelimiter(")")};
  if (!srcParenR)
    throwUnexpectedToken(
        mSrcLoc,
        concat("expected ')' to close the '(' opened at line ", srcLoc0.lineNo),
        &srcLoc0);
  accept();
  return allocate<AST::Parens>(srcLoc0, std::in_place, orEmpty(srcDollar),
                               *srcParenL, std::move(expr), *srcParenR);
}

auto Parser::parseExpression() -> BumpPtr<AST::Expr> {
  return parseBinaryLeftAssociative(
      {BINOP_COMMA}, [&] { return parseAssignmentExpression(); });
}

auto Parser::parseAssignmentExpression() -> BumpPtr<AST::Expr> {
  return parseBinaryRightAssociative({BINOP_LET, //
                                      BINOP_EQ_LSHR, BINOP_EQ_ADD, BINOP_EQ_SUB,
                                      BINOP_EQ_MUL, BINOP_EQ_DIV, BINOP_EQ_REM,
                                      BINOP_EQ_SHL, BINOP_EQ_ASHR, BINOP_EQ_AND,
                                      BINOP_EQ_OR, BINOP_EQ_XOR, BINOP_EQ},
                                     [&] { return parseElseExpression(); });
}

auto Parser::parseElseExpression() -> BumpPtr<AST::Expr> {
  return parseBinaryRightAssociative(
      {BINOP_ELSE}, [&] { return parseConditionalExpression(); });
}

auto Parser::parseConditionalExpression() -> BumpPtr<AST::Expr> {
  BumpPtr<AST::Expr> expr{parseLogicalOrExpression()};
  if (!expr) return nullptr;
  skip();
  SourceLocation srcLoc0{mSrcLoc};
  if (std::optional<std::string_view> srcQuestion{next("?")}) {
    BumpPtr<AST::Expr> exprThen{parseExpression()};
    if (!exprThen)
      srcLoc0.throwError("Expected then clause in conditional expression");
    skip();
    std::optional<std::string_view> srcColon{next(":")};
    if (!srcColon)
      mSrcLoc.throwError("Expected ':' after then clause in conditional "
                         "expression");
    BumpPtr<AST::Expr> exprElse{parseAssignmentExpression()};
    if (!exprElse)
      srcLoc0.throwError("Expected else clause in conditional expression");
    expr = allocate<AST::Select>(srcLoc0, std::in_place, std::move(expr),
                                 *srcQuestion, std::move(exprThen), *srcColon,
                                 std::move(exprElse));
  }
  return expr;
}

auto Parser::parseLogicalOrExpression() -> BumpPtr<AST::Expr> {
  return parseBinaryLeftAssociative(
      {BINOP_LOGIC_OR}, [&] { return parseLogicalAndExpression(); });
}

auto Parser::parseLogicalAndExpression() -> BumpPtr<AST::Expr> {
  return parseBinaryLeftAssociative(
      {BINOP_LOGIC_AND}, [&] { return parseInclusiveOrExpression(); });
}

auto Parser::parseInclusiveOrExpression() -> BumpPtr<AST::Expr> {
  return parseBinaryLeftAssociative(
      {BINOP_OR}, [&] { return parseExclusiveOrExpression(); });
}

auto Parser::parseExclusiveOrExpression() -> BumpPtr<AST::Expr> {
  return parseBinaryLeftAssociative({BINOP_XOR},
                                    [&] { return parseAndExpression(); });
}

auto Parser::parseAndExpression() -> BumpPtr<AST::Expr> {
  return parseBinaryLeftAssociative({BINOP_AND},
                                    [&] { return parseEqualityExpression(); });
}

auto Parser::parseEqualityExpression() -> BumpPtr<AST::Expr> {
  return parseBinaryLeftAssociative(
      {BINOP_CMP_EQ, BINOP_CMP_NE, BINOP_APPROX_CMP_EQ, BINOP_APPROX_CMP_NE},
      [&] { return parseRelationalExpression(); });
}

auto Parser::parseRelationalExpression() -> BumpPtr<AST::Expr> {
  return parseBinaryLeftAssociative(
      {BINOP_SUBSET, BINOP_CMP_LE, BINOP_CMP_GE, BINOP_CMP_LT, BINOP_CMP_GT},
      [&] { return parseShiftExpression(); });
}

auto Parser::parseShiftExpression() -> BumpPtr<AST::Expr> {
  return parseBinaryLeftAssociative({BINOP_LSHR, BINOP_SHL, BINOP_ASHR},
                                    [&] { return parseAdditiveExpression(); });
}

auto Parser::parseAdditiveExpression() -> BumpPtr<AST::Expr> {
  return parseBinaryLeftAssociative(
      {BINOP_ADD, BINOP_SUB}, [&] { return parseMultiplicativeExpression(); });
}

auto Parser::parseMultiplicativeExpression() -> BumpPtr<AST::Expr> {
  return parseBinaryLeftAssociative({BINOP_MUL, BINOP_DIV, BINOP_REM},
                                    [&] { return parseUnaryExpression(); });
}

auto Parser::parseUnaryExpression() -> BumpPtr<AST::Expr> {
  ParseDepthGuard depthGuard{*this};
  // Lambdas, introduced unambiguously by a backslash. This is extended
  // syntax!
  if (mIsSMDL) {
    if (BumpPtr<AST::Expr> expr{parseLambdaExpression()}) return expr;
  }
  if (BumpPtr<AST::Expr> expr{parsePostfixExpression()}) return expr;
  auto parsePrefixExpression{[&]() -> BumpPtr<AST::Expr> {
    SourceLocation srcLoc0{checkpoint()};
    std::optional<Parser::ParsedUnaryOp> op{parseUnaryOp()};
    if (!op) {
      reject();
      return nullptr;
    }
    BumpPtr<AST::Expr> expr{parseUnaryExpression()};
    if (!expr) {
      reject();
      return nullptr;
    }
    accept();
    expr = allocate<AST::Unary>(srcLoc0, std::in_place, op->srcOp, op->op,
                                std::move(expr));
    return expr;
  }};
  if (BumpPtr<AST::Expr> expr{parsePrefixExpression()}) return expr;
  if (BumpPtr<AST::Expr> expr{parseLetExpression()}) return expr;
  if (mIsSMDL) {
    if (BumpPtr<AST::Expr> expr{parseReturnFromExpression()}) return expr;
  }
  return nullptr;
}

auto Parser::parsePostfixExpression() -> BumpPtr<AST::Expr> {
  BumpPtr<AST::Expr> expr{parsePrimaryExpression()};
  if (!expr) return nullptr;
  auto withPostfix{[&]() -> BumpPtr<AST::Expr> {
    SourceLocation srcLoc0{mSrcLoc};
    if (std::optional<std::string_view> srcDot{nextDelimiter(".")}) {
      std::optional<AST::Name> name{parseSimpleName()};
      if (!name) srcLoc0.throwError("Expected name after '.'");
      return allocate<AST::AccessField>(srcLoc0, std::in_place, std::move(expr),
                                        *srcDot, *name);
    }
    if (std::optional<std::string_view> srcOp{nextDelimiter("++")})
      return allocate<AST::Unary>(srcLoc0, std::in_place, *srcOp,
                                  UNOP_POSTFIX_INC, std::move(expr));
    if (std::optional<std::string_view> srcOp{nextDelimiter("--")})
      return allocate<AST::Unary>(srcLoc0, std::in_place, *srcOp,
                                  UNOP_POSTFIX_DEC, std::move(expr));
    if (std::optional<AST::ArgumentList> args{parseArgumentList()})
      return allocate<AST::Call>(srcLoc0, std::in_place, std::move(expr),
                                 std::move(*args));
    std::vector<AST::AccessIndex::Index> indexes{};
    while (!startsWith(getRemainingSourceCode(), "[[")) {
      AST::AccessIndex::Index index{};
      std::optional<std::string_view> srcBrackL{nextDelimiter("[")};
      if (!srcBrackL) break;
      if (std::optional<std::string_view> srcAngleL{nextDelimiter("<")}) {
        std::optional<AST::Name> name{parseSimpleName()};
        if (!name) srcLoc0.throwError("Expected name after '[<'");
        std::optional<std::string_view> srcAngleR{nextDelimiter(">")};
        if (!srcAngleR) srcLoc0.throwError("Expected '>]'");
        index.expr = allocate<AST::SizeName>(srcLoc0, std::in_place, *srcAngleL,
                                             *name, *srcAngleR);
      } else {
        index.expr = parseExpression(); // This may be null to represent `[]`
      }
      std::optional<std::string_view> srcBrackR{nextDelimiter("]")};
      if (!srcBrackR)
        throwUnexpectedToken(mSrcLoc,
                             concat("expected ']' to close the '[' opened at "
                                    "line ",
                                    srcLoc0.lineNo),
                             &srcLoc0);
      index.srcBrackL = *srcBrackL;
      index.srcBrackR = *srcBrackR;
      indexes.push_back(std::move(index));
      skip();
    }
    if (!indexes.empty())
      return allocate<AST::AccessIndex>(srcLoc0, std::in_place, std::move(expr),
                                        std::move(indexes));
    return nullptr;
  }};
  while (true) {
    BumpPtr<AST::Expr> nextExpr{withPostfix()};
    if (!nextExpr) break;
    expr = std::move(nextExpr);
  }
  return expr;
}

auto Parser::parseLetExpression() -> BumpPtr<AST::Expr> {
  std::optional<Parser::ParsedToken> kwLet{nextKeywordAndLocation("let")};
  if (!kwLet) return nullptr;
  SourceLocation srcLoc0{kwLet->srcLoc};
  std::vector<BumpPtr<AST::Decl>> decls{};
  std::optional<std::string_view> srcBraceL{};
  std::optional<std::string_view> srcBraceR{};
  if (srcBraceL = nextDelimiter("{"); srcBraceL) {
    while (true) {
      BumpPtr<AST::Variable> decl{parseVariableDeclaration()};
      if (!decl) break;
      decls.push_back(std::move(decl));
      skip();
      if (peek() == '}') break;
    }
    if (srcBraceR = nextDelimiter("}"); !srcBraceR) {
      // Anything that is not a declaration stops the loop above, so the
      // block is far more often wrong at the offending token than it is
      // missing its closing brace. Only blame the brace at EOF.
      if (isEOF()) srcLoc0.throwError("Expected closing '}' after 'let'");
      mSrcLoc.throwError("Expected variable declaration or closing '}' in "
                         "'let' block, which must contain only declarations");
    }
  } else {
    BumpPtr<AST::Variable> decl{parseVariableDeclaration()};
    if (!decl) srcLoc0.throwError("Expected variable declaration after 'let'");
    decls.push_back(std::move(decl));
  }
  std::optional<std::string_view> srcKwIn{nextKeyword("in")};
  if (!srcKwIn) srcLoc0.throwError("Expected 'in' after 'let ...'");
  BumpPtr<AST::Expr> expr{parseConditionalExpression()};
  if (!expr) srcLoc0.throwError("Expected expression after 'let ... in'");
  return allocate<AST::Let>(srcLoc0, std::in_place, kwLet->src,
                            orEmpty(srcBraceL), std::move(decls),
                            orEmpty(srcBraceR), *srcKwIn, std::move(expr));
}

auto Parser::parseReturnFromExpression() -> BumpPtr<AST::Expr> {
  std::optional<Parser::ParsedToken> kwReturnFrom{
      nextKeywordAndLocation("return_from")};
  if (!kwReturnFrom) return nullptr;
  BumpPtr<AST::Compound> stmt{parseCompoundStatement()};
  if (!stmt)
    kwReturnFrom->srcLoc.throwError(
        "Expected compound statement after 'return_from'");
  return allocate<AST::ReturnFrom>(kwReturnFrom->srcLoc, std::in_place,
                                   kwReturnFrom->src, std::move(stmt));
}

auto Parser::parseLambdaExpression() -> BumpPtr<AST::Expr> {
  std::optional<Parser::ParsedToken> backslash{nextDelimiterAndLocation("\\")};
  if (!backslash) return nullptr;
  // The backslash unambiguously introduces a lambda, so everything from
  // here on is a committed parse. There is no return type syntax; the
  // return type is always implicitly `auto`.
  SourceLocation srcLoc0{backslash->srcLoc};
  std::optional<AST::ParameterList> params{parseParameterList()};
  if (!params) srcLoc0.throwError("Expected parameter list after '\\'");
  if (params->isVariant())
    srcLoc0.throwError("Lambda must not be a function variant");
  if (params->hasTrailingEllipsis())
    srcLoc0.throwError("Lambda must not be variadic");
  std::optional<std::string_view> srcEqual{};
  BumpPtr<AST::Node> definition{};
  if (srcEqual = nextDelimiter("="); srcEqual) {
    skip();
    SourceLocation srcLoc1{mSrcLoc};
    // The body is an assignment expression, not a full expression: the
    // comma operator must not swallow subsequent arguments when the lambda
    // appears in an argument list.
    BumpPtr<AST::Expr> def{parseAssignmentExpression()};
    if (!def) srcLoc0.throwError("Expected lambda expression after '='");
    definition =
        allocate<AST::Return>(srcLoc1, std::in_place, std::string_view(),
                              std::move(def), std::nullopt, std::string_view());
  } else {
    BumpPtr<AST::Compound> def{parseCompoundStatement()};
    if (!def)
      srcLoc0.throwError("Expected '=' or compound statement after lambda "
                         "parameter list");
    definition = std::move(def);
  }
  BumpPtr<AST::Function> func{allocate<AST::Function>(
      srcLoc0, std::in_place, BumpPtr<AST::Type>{},
      BumpPtr<AST::AnnotationBlock>{}, AST::Name{}, std::move(*params),
      std::string_view(), BumpPtr<AST::AnnotationBlock>{}, orEmpty(srcEqual),
      std::move(definition), std::string_view())};
  return allocate<AST::Lambda>(srcLoc0, std::in_place, backslash->src,
                               std::move(func));
}

auto Parser::parsePrimaryExpression() -> BumpPtr<AST::Expr> {
  if (BumpPtr<AST::Expr> expr{parseExpressionInParentheses()}) return expr;
  if (BumpPtr<AST::Expr> expr{parseLiteralExpression()}) return expr;
  if (BumpPtr<AST::Identifier> expr{parseIdentifier()}) return expr;
  SourceLocation srcLoc0{mSrcLoc};
  if (std::optional<std::string_view> srcKwCast{nextKeyword("cast")}) {
    std::optional<std::string_view> srcAngleL{nextDelimiter("<")};
    if (!srcAngleL) srcLoc0.throwError("Expected opening '<' after 'cast'");
    BumpPtr<AST::Type> type{parseType()};
    if (!type) srcLoc0.throwError("Expected type after 'cast'");
    std::optional<std::string_view> srcAngleR{nextDelimiter(">")};
    if (!srcAngleR) srcLoc0.throwError("Expected closing '>' after 'cast'");
    BumpPtr<AST::Expr> expr{parseExpressionInParentheses()};
    if (!expr)
      srcLoc0.throwError("Expected parenthesized expression after 'cast<...>'");
    return allocate<AST::TypeCast>(srcLoc0, std::in_place, *srcKwCast,
                                   *srcAngleL, std::move(type), *srcAngleR,
                                   std::move(expr));
  }
  return nullptr;
}

auto Parser::parseLiteralExpression() -> BumpPtr<AST::Expr> {
  if (BumpPtr<AST::LiteralBool> expr{parseLiteralBoolExpression()}) return expr;
  if (BumpPtr<AST::LiteralString> expr{parseLiteralStringExpression()})
    return expr;
  if (BumpPtr<AST::Expr> expr{parseLiteralNumberExpression()}) return expr;
  if (mIsSMDL) {
    skip();
    SourceLocation srcLoc0{mSrcLoc};
    if (next("#")) {
      std::optional<std::string_view> word{nextWord()};
      if (!word) srcLoc0.throwError("Expected intrinsic name after '#'");
      if (*word == "search_dir")
        srcLoc0.throwError("A '#search_dir' is only allowed at the top of the "
                           "file immediately after '#smdl'");
      return allocate<AST::Intrinsic>(
          srcLoc0, std::in_place,
          getSourceCode().substr(srcLoc0.i, mSrcLoc.i - srcLoc0.i));
    }
  }
  return nullptr;
}

auto Parser::parseLiteralBoolExpression() -> BumpPtr<AST::LiteralBool> {
  skip();
  SourceLocation srcLoc0{mSrcLoc};
  if (std::optional<std::string_view> srcValue{nextKeyword("true")})
    return allocate<AST::LiteralBool>(srcLoc0, std::in_place, *srcValue, true);
  if (std::optional<std::string_view> srcValue{nextKeyword("false")})
    return allocate<AST::LiteralBool>(srcLoc0, std::in_place, *srcValue, false);
  return nullptr;
}

auto Parser::parseLiteralStringExpression() -> BumpPtr<AST::LiteralString> {
  skip();
  if (peek() != '"') return nullptr;
  std::string str{};
  SourceLocation srcLoc0{mSrcLoc};
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
  while (nextDelimiter("\"")) {
    while (true) {
      if (isEOF()) srcLocSeg.throwError("Unexpected EOF in literal string");
      if (peek() == '\n')
        srcLocSeg.throwError("Unexpected EOL in literal string");
      if (peek() == '"') break;
      if (char ch{next()}; ch != '\\') {
        str += ch;
      } else {
        ch = next();
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
            ch = next();
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
            ch = next();
            if (!isDigit16(ch))
              srcLocSeg.throwError("Expected 2 hexadecimal digits after '\\x'");
            byte = (byte << 4) | uint8_t(hexToInt(ch));
          }
          str += static_cast<char>(byte);
        } else if (ch == 'u') { // unicode 16-bit
          uint32_t codepoint{};
          for (int i{}; i < 4; i++) {
            ch = next();
            if (!isDigit16(ch))
              srcLocSeg.throwError("Expected 4 hexadecimal digits after '\\u'");
            codepoint = (codepoint << 4) | uint32_t(hexToInt(ch));
          }
          if (!appendCodepointAsUTF8(codepoint))
            srcLocSeg.throwError("UTF-8 encoding of '\\u' sequence failed");
        } else if (ch == 'U') { // unicode 32-bit
          uint32_t codepoint{};
          for (int i{}; i < 8; i++) {
            ch = next();
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
    if (!nextDelimiter("\""))
      srcLocSeg.throwError("Expected '\"' to close literal string");
    srcValues.push_back(getSourceCodeBetween(srcLocSeg, mSrcLoc));
    skip();
    srcLocSeg = mSrcLoc;
  }
  return allocate<AST::LiteralString>(srcLoc0, std::in_place,
                                      std::move(srcValues), std::move(str));
}

auto Parser::parseLiteralNumberExpression() -> BumpPtr<AST::Expr> {
  skip();
  if (!isDigit(peek())) return nullptr;
  SourceLocation srcLoc0{mSrcLoc};
  auto parseDigits{[&](auto &&isDigit) {
    std::string digits{};
    while (isDigit(peek())) {
      digits.push_back(peek());
      next();
      if (next("'")) { // Maybe consume single-quote separator
        if (peek() == '\'')
          srcLoc0.throwError("Numeric literal must not contain adjacent "
                             "single-quote separators");
        if (!isDigit(peek()))
          srcLoc0.throwError("Numeric literal must not be terminated by "
                             "single-quote separator");
      }
    }
    return digits;
  }};
  auto parseIntWithPrefix{[&](auto &&isDigit, int radix, const char *prefix,
                              const char *info, std::string &digitsStr) {
    if (!isDigit(peek()))
      srcLoc0.throwError("Expected literal prefix ", Quoted(prefix),
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
    std::string_view remaining{getRemainingSourceCode()};
    return remaining.size() >= 2 && remaining[0] == '0' &&
           chars.find(remaining[1]) != std::string_view::npos;
  }};
  if (!zeroFollowedByAny(".eEfFdD") && !(mIsSMDL && zeroFollowedByAny("j")) &&
      next("0")) {
    llvm::APInt value{64, 0};
    std::string digits{};
    if (isDigit8(peek())) {
      value = parseIntWithPrefix(isDigit8, 8, "0", "[0-7]", digits);
    } else if (next("b") || next("B")) {
      value = parseIntWithPrefix(isDigit2, 2, "0b", "[0-1]", digits);
    } else if (next("x") || next("X")) {
      value = parseIntWithPrefix(isDigit16, 16, "0x", "[0-9a-fA-F]", digits);
    } else {
      digits = "0";
    }
    if (isDigit(peek())) mSrcLoc.throwError("Invalid digit in integer literal");
    return allocate<AST::LiteralInt>(srcLoc0, std::in_place,
                                     getSourceCodeBetween(srcLoc0, mSrcLoc),
                                     value.getLimitedValue());
  } else {
    bool isInt{true};
    std::string digits{parseDigits(isDigit)};
    if (next(".")) {
      digits += '.';
      digits += parseDigits(isDigit);
      isInt = false;
    }
    if (next("e") || next("E")) {
      digits += 'e';
      if (next("+"))
        digits += '+';
      else if (next("-"))
        digits += '-';
      if (!isDigit(peek()))
        srcLoc0.throwError(
            "Expected exponent after 'e' in floating point literal");
      digits += parseDigits(isDigit);
      isInt = false;
    }
    if (mIsSMDL && next("j")) { // Imaginary unit
      isInt = false;
    }
    if (next("d") || next("D") || next("f") || next("F")) {
      isInt = false;
    }
    if (isInt) {
      unsigned bits{llvm::APInt::getBitsNeeded(digits, 10)};
      if (bits > 64) srcLoc0.logWarn("Integer literal exceeds 64 bits");
      return allocate<AST::LiteralInt>(
          srcLoc0, std::in_place, getSourceCodeBetween(srcLoc0, mSrcLoc),
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
      return allocate<AST::LiteralFloat>(srcLoc0, std::in_place,
                                         getSourceCodeBetween(srcLoc0, mSrcLoc),
                                         value.convertToDouble());
    }
  }
  return nullptr;
}

auto Parser::parseUnaryOp() -> std::optional<ParsedUnaryOp> {
  for (auto op : std::array{UNOP_INC, UNOP_DEC, UNOP_POS, UNOP_NEG, UNOP_NOT,
                            UNOP_LOGIC_NOT})
    if (std::optional<std::string_view> srcOp{next(to_string(op))})
      return ParsedUnaryOp{*srcOp, op};
  if (mIsSMDL) {
    for (auto op : std::array{UNOP_ADDR, UNOP_DEREF, UNOP_MAYBE})
      if (std::optional<std::string_view> srcOp{next(to_string(op))})
        return ParsedUnaryOp{*srcOp, op};
  }
  return std::nullopt;
}

auto Parser::parseBinaryOp(Span<const AST::BinaryOp> ops)
    -> std::optional<ParsedBinaryOp> {
  for (auto op : ops) {
    if (!mIsSMDL && isExtendedSyntax(op)) continue;
    if (op == BINOP_ELSE) {
      if (std::optional<std::string_view> srcOp{nextKeyword(to_string(op))})
        return ParsedBinaryOp{*srcOp, op};
    } else {
      // Don't mistake bit and for logical and.
      if (op == BINOP_AND && startsWith(getRemainingSourceCode(), "&&"))
        continue;
      if (std::optional<std::string_view> srcOp{next(to_string(op))})
        return ParsedBinaryOp{*srcOp, op};
    }
  }
  return std::nullopt;
}
//--}

//--{ Parse: Decl
auto Parser::parseFile() -> BumpPtr<AST::File> {
  skip();
  // Any documentation comment before the `#smdl` marker or the
  // `mdl X.Y` version is module-level documentation.
  std::string_view srcDocComment{getPendingDocComment()};
  SourceLocation srcLoc0{mSrcLoc};
  std::optional<std::string_view> srcKwSmdlSyntax{nextKeyword("#smdl")};
  if (srcKwSmdlSyntax) mIsSMDL = true;
  std::vector<AST::File::SearchDir> searchDirs{parseFileSearchDirs()};
  std::optional<AST::File::Version> version{parseFileVersion()};
  if (!version && !mIsSMDL) srcLoc0.throwError("Expected MDL version");
  std::vector<BumpPtr<AST::Decl>> importDecls{};
  while (true) {
    auto parseAnyImport{[&]() -> BumpPtr<AST::Decl> {
      if (BumpPtr<AST::UsingAlias> decl{parseUsingAlias()}) return decl;
      if (BumpPtr<AST::UsingImport> decl{parseUsingImport()}) return decl;
      if (BumpPtr<AST::Import> decl{parseImport()}) return decl;
      return nullptr;
    }};
    BumpPtr<AST::Decl> decl{parseAnyImport()};
    if (!decl) break;
    importDecls.push_back(std::move(decl));
  }
  std::optional<std::string_view> srcKwModule{nextKeyword("module")};
  BumpPtr<AST::AnnotationBlock> moduleAnnotations{};
  std::optional<std::string_view> srcSemicolonAfterModule{};
  if (srcKwModule) {
    moduleAnnotations = parseAnnotationBlock();
    if (!moduleAnnotations)
      srcLoc0.throwError("Expected annotation block after 'module'");
    srcSemicolonAfterModule = nextDelimiter(";");
    if (!srcSemicolonAfterModule)
      srcLoc0.throwError("Expected ';' after 'module [[ ... ]]'");
  }
  std::vector<BumpPtr<AST::Decl>> globalDecls{};
  while (true) {
    BumpPtr<AST::Decl> decl{parseGlobalDeclaration()};
    if (!decl) break;
    globalDecls.push_back(std::move(decl));
    skip();
    if (isEOF()) break;
  }
  if (!isEOF()) {
    if (startsWith(getRemainingSourceCode(), "#search_dir"))
      mSrcLoc.throwError("A '#search_dir' is only allowed at the top of the "
                         "file immediately after '#smdl'");
    throwUnexpectedToken(mSrcLoc, "expected a declaration");
  }
  BumpPtr<AST::File> file{allocate<AST::File>(
      srcLoc0, std::in_place, orEmpty(srcKwSmdlSyntax), std::move(searchDirs),
      std::move(version), std::move(importDecls), orEmpty(srcKwModule),
      std::move(moduleAnnotations), orEmpty(srcSemicolonAfterModule),
      std::move(globalDecls))};
  file->srcDocComment = srcDocComment;
  return file;
}

auto Parser::parseFileSearchDirs() -> std::vector<AST::File::SearchDir> {
  std::vector<AST::File::SearchDir> searchDirs{};
  while (true) {
    skip();
    SourceLocation srcLoc0{mSrcLoc};
    std::optional<std::string_view> srcKwSearchDir{nextKeyword("#search_dir")};
    if (!srcKwSearchDir) break;
    if (!mIsSMDL)
      srcLoc0.throwError("A '#search_dir' requires the file to begin with "
                         "'#smdl'");
    BumpPtr<AST::LiteralString> path{parseLiteralStringExpression()};
    if (!path)
      srcLoc0.throwError("Expected literal string path after '#search_dir'");
    searchDirs.push_back(
        AST::File::SearchDir{*srcKwSearchDir, std::move(path)});
  }
  return searchDirs;
}

auto Parser::parseFileVersion() -> std::optional<AST::File::Version> {
  std::optional<Parser::ParsedToken> kwMdl{nextKeywordAndLocation("mdl")};
  if (!kwMdl) return std::nullopt;
  SourceLocation srcLoc0{kwMdl->srcLoc};
  skip();
  SourceLocation srcLoc1{mSrcLoc};
  std::optional<std::string_view> srcMajor{nextInteger()};
  std::optional<std::string_view> srcDot{next(".")};
  std::optional<std::string_view> srcMinor{nextInteger()};
  if (!srcMajor || !srcDot || !srcMinor)
    srcLoc0.throwError("Expected 'X.Y' version after 'mdl'");
  auto parseVersionNumber{[&](std::string_view srcNumber) {
    uint32_t number{};
    if (std::from_chars(srcNumber.data(), srcNumber.data() + srcNumber.size(),
                        number)
            .ec != std::errc())
      srcLoc0.throwError("Version number ", Quoted(srcNumber),
                         " is out of range");
    return number;
  }};
  AST::File::Version version{};
  version.srcKwMdl = kwMdl->src;
  version.srcVersion = getSourceCode().substr(srcLoc1.i, mSrcLoc.i - srcLoc1.i);
  version.major = parseVersionNumber(*srcMajor);
  version.minor = parseVersionNumber(*srcMinor);
  std::optional<std::string_view> srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon) srcLoc0.throwError("Expected ';' after 'mdl ...'");
  version.srcSemicolon = *srcSemicolon;
  return version;
}

auto Parser::parseImportPath() -> std::optional<AST::ImportPath> {
  checkpoint();
  std::vector<AST::ImportPath::Element> elements{};
  while (true) {
    checkpoint();
    std::optional<std::string_view> srcDoubleColon{nextDelimiter("::")};
    if (!srcDoubleColon && !elements.empty()) {
      reject();
      break;
    }
    AST::ImportPath::Element element{};
    if (srcDoubleColon) {
      element.srcDoubleColon = *srcDoubleColon;
    }
    if (std::optional<std::string_view> srcName{nextDelimiter("..")}) {
      element.srcName = *srcName;
    } else if (std::optional<std::string_view> srcName{nextDelimiter(".")}) {
      element.srcName = *srcName;
    } else if (std::optional<std::string_view> srcName{nextDelimiter("*")}) {
      element.srcName = *srcName;
    } else if (std::optional<AST::Name> name{parseSimpleName()}) {
      element.srcName = name->srcName;
    } else if (BumpPtr<AST::LiteralString> literalString{
                   parseLiteralStringExpression()}) {
      element.literalString = std::move(literalString);
    } else {
      reject();
      break;
    }
    accept();
    elements.push_back(std::move(element));
  }
  if (elements.empty()) {
    reject();
    return std::nullopt;
  }
  accept();
  return AST::ImportPath(std::move(elements));
}

auto Parser::parseUsingAlias() -> BumpPtr<AST::UsingAlias> {
  SourceLocation srcLoc0{checkpoint()};
  std::optional<std::string_view> srcKwUsing{nextKeyword("using")};
  if (!srcKwUsing) {
    reject();
    return nullptr;
  }
  std::optional<AST::Name> name{parseSimpleName()};
  if (!name) {
    reject();
    return nullptr;
  }
  std::optional<std::string_view> srcEqual{nextDelimiter("=")};
  if (!srcEqual) {
    reject();
    return nullptr;
  }
  std::optional<AST::ImportPath> importPath{parseImportPath()};
  if (!importPath)
    srcLoc0.throwError("Expected import path after 'using ... ='");
  std::optional<std::string_view> srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon) srcLoc0.throwError("Expected ';' after 'using ... = ...'");
  accept();
  return allocate<AST::UsingAlias>(srcLoc0, std::in_place, *srcKwUsing, *name,
                                   *srcEqual, std::move(*importPath),
                                   *srcSemicolon);
}

auto Parser::parseUsingImport() -> BumpPtr<AST::UsingImport> {
  SourceLocation srcLoc0{checkpoint()};
  std::optional<std::string_view> srcKwExport{nextKeyword("export")};
  std::optional<std::string_view> srcKwUsing{nextKeyword("using")};
  if (!srcKwUsing) {
    reject();
    return nullptr;
  }
  std::optional<AST::ImportPath> importPath{parseImportPath()};
  if (!importPath) {
    reject();
    return nullptr;
  }
  if (importPath->isImportAll())
    srcLoc0.throwError(
        "Import path after '[export] using' must not end with '::*'");
  std::optional<std::string_view> srcKwImport{nextKeyword("import")};
  if (!srcKwImport)
    srcLoc0.throwError("Expected 'import' after '[export] using ...'");
  std::vector<AST::UsingImport::Name> names{};
  if (std::optional<std::string_view> srcStar{nextDelimiter("*")}) {
    names.push_back(AST::UsingImport::Name{*srcStar, {}});
  } else {
    parseCommaSeparated(names, [&]() -> std::optional<AST::UsingImport::Name> {
      std::optional<AST::Name> name{parseSimpleName()};
      if (!name) return std::nullopt;
      return AST::UsingImport::Name{name->srcName, {}};
    });
  }
  std::optional<std::string_view> srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon)
    srcLoc0.throwError("Expected ';' after '[export] using ... import ...'");
  accept();
  BumpPtr<AST::UsingImport> result{allocate<AST::UsingImport>(
      srcLoc0, std::in_place, *srcKwUsing, std::move(*importPath), *srcKwImport,
      std::move(names), *srcSemicolon)};
  if (srcKwExport) result->srcKwExport = *srcKwExport;
  return result;
}

auto Parser::parseImport() -> BumpPtr<AST::Import> {
  std::optional<Parser::ParsedToken> kwImport{nextKeywordAndLocation("import")};
  if (!kwImport) return nullptr;
  std::vector<AST::Import::ImportPathWrapper> importPathWrappers{};
  parseCommaSeparated(
      importPathWrappers,
      [&]() -> std::optional<AST::Import::ImportPathWrapper> {
        std::optional<AST::ImportPath> importPath{parseImportPath()};
        if (!importPath) return std::nullopt;
        return AST::Import::ImportPathWrapper{std::move(*importPath), {}};
      });
  std::optional<std::string_view> srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon)
    kwImport->srcLoc.throwError("Expected ';' after 'import ...'");
  return allocate<AST::Import>(kwImport->srcLoc, std::in_place, kwImport->src,
                               std::move(importPathWrappers), *srcSemicolon);
}

auto Parser::parseAttributes() -> std::optional<AST::Decl::Attributes> {
  std::optional<Parser::ParsedToken> srcAt{nextDelimiterAndLocation("@")};
  if (!srcAt) return std::nullopt;
  SourceLocation srcLoc0{srcAt->srcLoc};
  AST::Function::Attributes attributes{};
  attributes.srcAt = srcAt->src;
  std::optional<std::string_view> srcParenL{nextDelimiter("(")};
  if (!srcParenL)
    srcLoc0.throwError("Expected '@(...)' syntax for function attributes");
  attributes.srcParenL = *srcParenL;
  static constexpr std::array<std::string_view, 11> attrNames{
      "alwaysinline", "cold",    "fastmath", "foreign", "hot",    "macro",
      "noinline",     "optnone", "optsize",  "pure",    "visible"};
  while (true) {
    std::optional<std::string_view> attr{
        [&]() -> std::optional<std::string_view> {
          for (auto attrName : attrNames)
            if (std::optional<std::string_view> srcAttr{nextKeyword(attrName)})
              return srcAttr;
          return std::nullopt;
        }()};
    if (attr) {
      attributes.attrs.push_back(*attr);
    } else {
      skip();
      if (peek() != ')') {
        SourceLocation srcLocAttr{mSrcLoc};
        if (std::optional<std::string_view> word{nextWord()})
          srcLocAttr.throwError("Unrecognized attribute ", Quoted(*word),
                                ", expected one of ", join(attrNames, ", "));
        srcLocAttr.throwError("Expected attribute name or ')' after '@('");
      }
      break;
    }
  }
  std::optional<std::string_view> srcParenR{nextDelimiter(")")};
  if (!srcParenR) srcLoc0.throwError("Expected '@(...)' syntax for attributes");
  attributes.srcParenR = *srcParenR;
  return std::move(attributes);
}

auto Parser::parseGlobalDeclaration() -> BumpPtr<AST::Decl> {
  SourceLocation srcLoc0{checkpoint()};
  // Capture before parsing: comments inside the declaration must not
  // clobber the pending block first.
  std::string_view srcDocComment{getDocCommentBefore(srcLoc0.i)};
  std::optional<AST::Decl::Attributes> attributes{parseAttributes()};
  std::optional<std::string_view> srcKwExport{nextKeyword("export")};
  BumpPtr<AST::Decl> decl{[&]() -> BumpPtr<AST::Decl> {
    if (BumpPtr<AST::Decl> decl{parseAnnotationDeclaration()}) return decl;
    if (BumpPtr<AST::Function> decl{parseFunctionDeclaration()}) return decl;
    if (BumpPtr<AST::Decl> decl{parseTypeDeclaration()}) return decl;
    if (BumpPtr<AST::Variable> decl{parseVariableDeclaration()}) return decl;
    if (mIsSMDL) {
      if (BumpPtr<AST::Exec> decl{parseExecDeclaration()}) return decl;
      if (BumpPtr<AST::UnitTest> decl{parseUnitTestDeclaration()}) return decl;
      if (BumpPtr<AST::Namespace> decl{parseNamespaceDeclaration()})
        return decl;
    }
    return nullptr;
  }()};
  if (!decl) {
    reject();
    if (nextKeyword("using") || nextKeyword("import"))
      srcLoc0.throwError("All 'using' and 'import' declarations must appear "
                         "at the top of the file");
    return nullptr;
  }
  decl->isGlobal = true;
  decl->srcDocComment = srcDocComment;
  if (attributes) decl->attributes = std::move(attributes);
  if (srcKwExport) decl->srcKwExport = *srcKwExport;
  accept();
  return decl;
}

auto Parser::parseAnnotationDeclaration() -> BumpPtr<AST::Decl> {
  std::optional<Parser::ParsedToken> kwAnnotation{
      nextKeywordAndLocation("annotation")};
  if (!kwAnnotation) return nullptr;
  SourceLocation srcLoc0{kwAnnotation->srcLoc};
  std::optional<AST::Name> name{parseSimpleName()};
  if (!name) srcLoc0.throwError("Expected simple name after 'annotation'");
  std::optional<AST::ParameterList> params{parseParameterList()};
  if (!params)
    srcLoc0.throwError(
        "Expected parameter list after 'annotation' declaration");
  BumpPtr<AST::AnnotationBlock> annotations{parseAnnotationBlock()};
  std::optional<std::string_view> srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon)
    srcLoc0.throwError("Expected ';' after 'annotation' declaration");
  return allocate<AST::AnnotationDecl>(
      srcLoc0, std::in_place, kwAnnotation->src, std::move(*name),
      std::move(*params), std::move(annotations), *srcSemicolon);
}

auto Parser::parseTypeDeclaration() -> BumpPtr<AST::Decl> {
  if (BumpPtr<AST::Typedef> decl{parseAliasTypeDeclaration()}) return decl;
  if (BumpPtr<AST::Struct> decl{parseStructTypeDeclaration()}) return decl;
  if (BumpPtr<AST::Enum> decl{parseEnumTypeDeclaration()}) return decl;
  if (mIsSMDL) {
    if (BumpPtr<AST::Tag> decl{parseTagDeclaration()}) return decl;
  }
  return nullptr;
}

auto Parser::parseAliasTypeDeclaration() -> BumpPtr<AST::Typedef> {
  std::optional<Parser::ParsedToken> kwTypedef{
      nextKeywordAndLocation("typedef")};
  if (!kwTypedef) return nullptr;
  SourceLocation srcLoc0{kwTypedef->srcLoc};
  BumpPtr<AST::Type> type{parseType()};
  if (!type) srcLoc0.throwError("Expected type after 'typedef'");
  std::optional<AST::Name> name{parseSimpleName()};
  if (!name) srcLoc0.throwError("Expected name after 'typedef ...'");
  std::optional<std::string_view> srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon) srcLoc0.throwError("Expected ';' after 'typedef ...'");
  return allocate<AST::Typedef>(srcLoc0, std::in_place, kwTypedef->src,
                                std::move(type), *name, *srcSemicolon);
}

auto Parser::parseStructTypeDeclaration() -> BumpPtr<AST::Struct> {
  SourceLocation srcLoc0{checkpoint()};
  std::optional<std::string_view> srcKwStruct{nextKeyword("struct")};
  if (!srcKwStruct) {
    reject();
    return nullptr;
  }
  std::optional<AST::Name> name{parseSimpleName()};
  if (!name) srcLoc0.throwError("Expected name after 'struct'");
  std::vector<AST::Struct::Tag> tags{};
  std::optional<std::string_view> srcColonBeforeTags{nextDelimiter(":")};
  if (srcColonBeforeTags) {
    parseCommaSeparated(tags, [&]() -> std::optional<AST::Struct::Tag> {
      SourceLocation srcLoc1{mSrcLoc};
      std::optional<std::string_view> srcKwDefault{nextKeyword("default")};
      BumpPtr<AST::Identifier> tagName{parseIdentifier()};
      if (!tagName) return std::nullopt;
      AST::Struct::Tag tag{};
      tag.srcKwDefault = orEmpty(srcKwDefault);
      tag.type = allocate<AST::Type>(srcLoc1, std::in_place,
                                     std::vector<std::string_view>(),
                                     std::move(tagName));
      return std::move(tag);
    });
  }
  BumpPtr<AST::AnnotationBlock> annotations{parseAnnotationBlock()};
  std::optional<std::string_view> srcBraceL{nextDelimiter("{")};
  if (!srcBraceL) srcLoc0.throwError("Expected '{' after 'struct ...'");
  std::vector<AST::Struct::Constructor> constructors{};
  std::vector<AST::Struct::Field> fields{};
  std::optional<std::string_view> srcKwFinalize{};
  BumpPtr<AST::Stmt> stmtFinalize{};
  // Parse constructors, which must appear at the top of the
  // struct declaration. This is an extension!
  while (true) {
    std::optional<AST::Struct::Constructor> constructor{
        parseStructConstructor()};
    if (!constructor) break;
    if (constructor->name.srcName != name->srcName)
      constructor->name.srcLoc.throwError(
          "Constructor must name the containing struct ", Quoted(*name));
    constructors.push_back(std::move(*constructor));
    skip();
    if (peek() == '}') break;
  }
  // Parse fields
  while (true) {
    std::optional<AST::Struct::Field> field{parseStructFieldDeclarator()};
    if (!field) {
      // Parse finalize block, which must appear at the bottom of the
      // struct declaration if it appears at all. This is an extension!
      if (srcKwFinalize = nextKeyword("finalize"); srcKwFinalize) {
        if (stmtFinalize = parseCompoundStatement(); !stmtFinalize) {
          srcLoc0.throwError("Expected '{ ... }' after 'finalize'");
        }
      }
      break;
    }
    fields.push_back(std::move(*field));
    skip();
    attachPendingTrailingDocComment(fields);
    if (peek() == '}') break;
  }
  std::optional<std::string_view> srcBraceR{nextDelimiter("}")};
  if (!srcBraceR)
    throwUnexpectedToken(mSrcLoc,
                         "expected a field declarator or '}' in 'struct ...'");
  std::optional<std::string_view> srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon)
    srcLoc0.throwError("Expected ';' after 'struct ... { ... }'");
  accept();
  return allocate<AST::Struct>(
      srcLoc0, std::in_place, *srcKwStruct, *name, orEmpty(srcColonBeforeTags),
      std::move(tags), std::move(annotations), *srcBraceL,
      std::move(constructors), std::move(fields), orEmpty(srcKwFinalize),
      std::move(stmtFinalize), *srcBraceR, *srcSemicolon);
}

auto Parser::parseStructConstructor()
    -> std::optional<AST::Struct::Constructor> {
  SourceLocation srcLoc0{checkpoint()};
  std::optional<AST::Name> name{parseSimpleName()};
  if (!name) {
    reject();
    return std::nullopt;
  }
  skip();
  std::optional<AST::ParameterList> params{parseParameterList()};
  if (!params) {
    reject();
    return std::nullopt;
  }
  std::optional<std::string_view> srcEqual{nextDelimiter("=")};
  if (!srcEqual) {
    reject();
    return std::nullopt;
  }
  BumpPtr<AST::Expr> expr{parseExpression()};
  if (!expr) {
    srcLoc0.throwError("Expected expression after '='");
  }
  std::optional<std::string_view> srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon) {
    srcLoc0.throwError("Expected ';' after constructor expression");
  }
  accept();
  return AST::Struct::Constructor{std::move(*name), std::move(*params),
                                  *srcEqual, std::move(expr), *srcSemicolon};
}

auto Parser::parseStructFieldDeclarator() -> std::optional<AST::Struct::Field> {
  SourceLocation srcLoc0{checkpoint()};
  std::string_view srcDocComment{getDocCommentBefore(srcLoc0.i)};
  AST::Struct::Field field{};
  BumpPtr<AST::Type> type{parseType()};
  if (!type) {
    reject();
    return std::nullopt;
  }
  field.srcLoc = srcLoc0;
  field.srcDocComment = srcDocComment;
  field.type = std::move(type);
  std::optional<AST::Name> name{parseSimpleName()};
  if (!name) {
    reject();
    return std::nullopt;
  }
  field.name = *name;
  if (std::optional<std::string_view> srcEqual{nextDelimiter("=")}) {
    BumpPtr<AST::Expr> exprInit{parseExpression()};
    if (!exprInit)
      throwUnexpectedToken(mSrcLoc, "expected an initializer after '='");
    field.srcEqual = *srcEqual;
    field.exprInit = std::move(exprInit);
  }
  field.annotations = parseAnnotationBlock();
  std::optional<std::string_view> srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon) mSrcLoc.throwError("Expected ';' after field declarator");
  field.srcSemicolon = *srcSemicolon;
  accept();
  return std::move(field);
}

auto Parser::parseEnumTypeDeclaration() -> BumpPtr<AST::Enum> {
  std::optional<Parser::ParsedToken> kwEnum{nextKeywordAndLocation("enum")};
  if (!kwEnum) return nullptr;
  SourceLocation srcLoc0{kwEnum->srcLoc};
  std::optional<AST::Name> name{parseSimpleName()};
  if (!name) srcLoc0.throwError("Expected name after 'enum'");
  BumpPtr<AST::AnnotationBlock> annotations{parseAnnotationBlock()};
  std::optional<std::string_view> srcBraceL{nextDelimiter("{")};
  if (!srcBraceL) srcLoc0.throwError("Expected '{' after 'enum ...'");
  std::vector<AST::Enum::Declarator> declarators{};
  parseCommaSeparated(declarators, [&] { return parseEnumValueDeclarator(); });
  std::optional<std::string_view> srcBraceR{nextDelimiter("}")};
  if (!srcBraceR)
    throwUnexpectedToken(mSrcLoc,
                         "expected a value declarator or '}' in 'enum ...'");
  std::optional<std::string_view> srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon) srcLoc0.throwError("Expected ';' after 'enum ...'");
  return allocate<AST::Enum>(srcLoc0, std::in_place, kwEnum->src, *name,
                             std::move(annotations), *srcBraceL,
                             std::move(declarators), *srcBraceR, *srcSemicolon);
}

auto Parser::parseEnumValueDeclarator()
    -> std::optional<AST::Enum::Declarator> {
  SourceLocation srcLoc0{checkpoint()};
  std::string_view srcDocComment{getDocCommentBefore(srcLoc0.i)};
  std::optional<AST::Name> name{parseSimpleName()};
  if (!name) {
    reject();
    return std::nullopt;
  }
  AST::Enum::Declarator declarator{};
  declarator.srcLoc = srcLoc0;
  declarator.srcDocComment = srcDocComment;
  declarator.name = *name;
  if (std::optional<std::string_view> srcEqual{nextDelimiter("=")}) {
    BumpPtr<AST::Expr> exprInit{parseAssignmentExpression()};
    if (!exprInit)
      throwUnexpectedToken(mSrcLoc, "expected an initializer after '='");
    declarator.srcEqual = *srcEqual;
    declarator.exprInit = std::move(exprInit);
  }
  declarator.annotations = parseAnnotationBlock();
  accept();
  return std::move(declarator);
}

auto Parser::parseVariableDeclaration() -> BumpPtr<AST::Variable> {
  SourceLocation srcLoc0{checkpoint()};
  BumpPtr<AST::Type> type{parseType()};
  if (!type) {
    reject();
    return nullptr;
  }
  std::vector<AST::Variable::Declarator> declarators{};
  parseCommaSeparated(declarators, [&] { return parseVariableDeclarator(); });
  if (declarators.empty()) {
    reject();
    return nullptr;
  }
  std::optional<std::string_view> srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon)
    throwUnexpectedToken(mSrcLoc,
                         concat("expected ';' after the variable declaration "
                                "starting at line ",
                                srcLoc0.lineNo),
                         &srcLoc0);
  // Pick up a trailing documentation comment after the semicolon, e.g.,
  // `const int X = 0; ///< doc`.
  skip();
  attachPendingTrailingDocComment(declarators);
  accept();
  return allocate<AST::Variable>(srcLoc0, std::in_place, std::move(type),
                                 std::move(declarators), *srcSemicolon);
}

auto Parser::parseVariableDeclarator()
    -> std::optional<AST::Variable::Declarator> {
  SourceLocation srcLoc0{checkpoint()};
  AST::Variable::Declarator declarator{};
  declarator.srcLoc = srcLoc0;
  declarator.srcDocComment = getDocCommentBefore(srcLoc0.i);
  if (std::optional<AST::Name> name{parseSimpleName()}) {
    declarator.names.push_back(
        AST::Variable::Declarator::DeclaratorName{*name});
  } else if (std::optional<std::string_view> srcBraceL{nextDelimiter("{")};
             srcBraceL && mIsSMDL) {
    // Parse destructure syntax `{foo, bar, baz}`
    declarator.srcBraceL = *srcBraceL;
    parseCommaSeparated(
        declarator.names,
        [&]() -> std::optional<AST::Variable::Declarator::DeclaratorName> {
          std::optional<AST::Name> name{parseSimpleName()};
          if (!name) return std::nullopt;
          return AST::Variable::Declarator::DeclaratorName{*name};
        });
    std::optional<std::string_view> srcBraceR{nextDelimiter("}")};
    // An empty destructure binds nothing, and accepting it would swallow
    // the '{}' of any declaration that merely begins with a name, e.g.
    // 'exec {}'.
    if (!srcBraceR || declarator.names.empty()) {
      reject();
      return std::nullopt;
    }
    declarator.srcBraceR = *srcBraceR;
  } else {
    reject();
    return std::nullopt;
  }
  if (std::optional<std::string_view> srcEqual{nextDelimiter("=")}) {
    BumpPtr<AST::Expr> exprInit{parseAssignmentExpression()};
    if (!exprInit)
      throwUnexpectedToken(mSrcLoc, "expected an initializer after '='");
    declarator.srcEqual = *srcEqual;
    declarator.exprInit = std::move(exprInit);
  } else if (std::optional<AST::ArgumentList> argsInit{parseArgumentList()}) {
    declarator.argsInit = std::move(argsInit);
  }
  declarator.annotations = parseAnnotationBlock();
  accept();
  return std::move(declarator);
}

auto Parser::parseFunctionDeclaration() -> BumpPtr<AST::Function> {
  SourceLocation srcLoc0{checkpoint()};
  BumpPtr<AST::Type> type{parseType()};
  if (!type) {
    reject();
    return nullptr;
  }
  BumpPtr<AST::AnnotationBlock> earlyAnnotations{parseAnnotationBlock()};
  std::optional<AST::Name> name{parseSimpleName()};
  if (!name) {
    reject();
    return nullptr;
  }
  std::optional<AST::ParameterList> params{parseParameterList()};
  if (!params) {
    reject();
    return nullptr;
  }
  std::optional<std::string_view> srcFrequency{
      nextKeyword({"uniform", "varying"})};
  BumpPtr<AST::AnnotationBlock> lateAnnotations{parseAnnotationBlock()};
  std::optional<std::string_view> srcEqual{};
  BumpPtr<AST::Node> definition{};
  std::optional<std::string_view> srcSemicolon{};
  skip();
  if (params->isVariant() && peek() != '=')
    srcLoc0.throwError(
        "Function variant must be defined by 'let' or call expression");
  if (srcSemicolon = nextDelimiter(";"); srcSemicolon) {
    // Nothing
  } else if (srcEqual = nextDelimiter("="); srcEqual) {
    skip();
    SourceLocation srcLoc1{mSrcLoc};
    BumpPtr<AST::Expr> def{parseExpression()};
    if (!def) srcLoc0.throwError("Expected function expression after '='");
    if (srcSemicolon = nextDelimiter(";"); !srcSemicolon)
      srcLoc0.throwError("Expected ';' after function expression");
    if (params->isVariant() && !llvm::isa<AST::Let>(def.get()) &&
        !llvm::isa<AST::Call>(def.get()))
      srcLoc0.throwError(
          "Function variant definition must be 'let' or call expression");
    definition =
        allocate<AST::Return>(srcLoc1, std::in_place, std::string_view(),
                              std::move(def), std::nullopt, std::string_view());
  } else {
    BumpPtr<AST::Compound> def{parseCompoundStatement()};
    if (!def) srcLoc0.throwError("Expected ';' or function definition");
    definition = std::move(def);
  }
  accept();
  return allocate<AST::Function>(srcLoc0, std::in_place, std::move(type),
                                 std::move(earlyAnnotations), *name,
                                 std::move(*params), orEmpty(srcFrequency),
                                 std::move(lateAnnotations), orEmpty(srcEqual),
                                 std::move(definition), orEmpty(srcSemicolon));
}

auto Parser::parseTagDeclaration() -> BumpPtr<AST::Tag> {
  std::optional<Parser::ParsedToken> kwTag{nextKeywordAndLocation("tag")};
  if (!kwTag) return nullptr;
  SourceLocation srcLoc0{kwTag->srcLoc};
  std::optional<AST::Name> name{parseSimpleName()};
  if (!name) srcLoc0.throwError("Expected name after 'tag'");
  std::optional<std::string_view> srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon) srcLoc0.throwError("Expected ';' after 'tag ...'");
  return allocate<AST::Tag>(srcLoc0, std::in_place, kwTag->src, *name,
                            *srcSemicolon);
}

auto Parser::parseExecDeclaration() -> BumpPtr<AST::Exec> {
  std::optional<Parser::ParsedToken> kwExec{nextKeywordAndLocation("exec")};
  if (!kwExec) return nullptr;
  BumpPtr<AST::Compound> stmt{parseCompoundStatement()};
  if (!stmt)
    kwExec->srcLoc.throwError("Expected compound statement after 'exec'");
  return allocate<AST::Exec>(kwExec->srcLoc, std::in_place, kwExec->src,
                             std::move(stmt));
}

auto Parser::parseUnitTestDeclaration() -> BumpPtr<AST::UnitTest> {
  std::optional<Parser::ParsedToken> kwUnitTest{
      nextKeywordAndLocation("unit_test")};
  if (!kwUnitTest) return nullptr;
  SourceLocation srcLoc0{kwUnitTest->srcLoc};
  BumpPtr<AST::LiteralString> name{parseLiteralStringExpression()};
  if (!name) srcLoc0.throwError("Expected literal string after 'unit_test'");
  BumpPtr<AST::Compound> stmt{parseCompoundStatement()};
  if (!stmt)
    srcLoc0.throwError("Expected compound statement after 'unit_test ...'");
  return allocate<AST::UnitTest>(srcLoc0, std::in_place, kwUnitTest->src,
                                 std::move(name), std::move(stmt));
}

auto Parser::parseNamespaceDeclaration() -> BumpPtr<AST::Namespace> {
  std::optional<Parser::ParsedToken> kwNamespace{
      nextKeywordAndLocation("namespace")};
  if (!kwNamespace) return nullptr;
  SourceLocation srcLoc0{kwNamespace->srcLoc};
  BumpPtr<AST::Identifier> identifier{parseIdentifier()};
  if (!identifier) srcLoc0.throwError("Expected identifier after 'namespace'");
  std::optional<std::string_view> srcBraceL{nextDelimiter("{")};
  if (!srcBraceL) srcLoc0.throwError("Expected '{' after 'namespace ...'");
  std::vector<BumpPtr<AST::Decl>> decls{};
  while (true) {
    BumpPtr<AST::Decl> decl{parseGlobalDeclaration()};
    if (!decl) break;
    decls.push_back(std::move(decl));
    skip();
    if (isEOF()) break;
  }
  std::optional<std::string_view> srcBraceR{nextDelimiter("}")};
  if (!srcBraceR)
    throwUnexpectedToken(mSrcLoc,
                         "expected a declaration or '}' in 'namespace ...'");
  return allocate<AST::Namespace>(srcLoc0, std::in_place, kwNamespace->src,
                                  std::move(identifier), *srcBraceL,
                                  std::move(decls), *srcBraceR);
}
//--}

//--{ Parse: Stmt
auto Parser::parseStatement() -> BumpPtr<AST::Stmt> {
  ParseDepthGuard depthGuard{*this};
  skip();
  SourceLocation srcLoc0{mSrcLoc};
  if (BumpPtr<AST::Compound> stmt{parseCompoundStatement()}) return stmt;
  if (BumpPtr<AST::If> stmt{parseIfStatement()}) return stmt;
  if (BumpPtr<AST::Switch> stmt{parseSwitchStatement()}) return stmt;
  if (BumpPtr<AST::While> stmt{parseWhileStatement()}) return stmt;
  if (BumpPtr<AST::DoWhile> stmt{parseDoStatement()}) return stmt;
  if (BumpPtr<AST::For> stmt{parseForStatement()}) return stmt;
  if (BumpPtr<AST::Break> stmt{parseBreakStatement()}) return stmt;
  if (BumpPtr<AST::Continue> stmt{parseContinueStatement()}) return stmt;
  if (BumpPtr<AST::Return> stmt{parseReturnStatement()}) return stmt;
  if (mIsSMDL) {
    if (BumpPtr<AST::Unreachable> stmt{parseUnreachableStatement()})
      return stmt;
    if (BumpPtr<AST::Preserve> stmt{parsePreserveStatement()}) return stmt;
    if (BumpPtr<AST::Defer> stmt{parseDeferStatement()}) return stmt;
    if (BumpPtr<AST::Visit> stmt{parseVisitStatement()}) return stmt;
  }
  if (BumpPtr<AST::Decl> decl{parseTypeDeclaration()})
    return allocate<AST::DeclStmt>(srcLoc0, std::in_place, std::move(decl));
  if (BumpPtr<AST::Variable> decl{parseVariableDeclaration()})
    return allocate<AST::DeclStmt>(srcLoc0, std::in_place, std::move(decl));
  if (std::optional<std::string_view> srcSemicolon{nextDelimiter(";")})
    return allocate<AST::ExprStmt>(srcLoc0, std::in_place, nullptr,
                                   std::nullopt, *srcSemicolon);
  if (BumpPtr<AST::Expr> expr{parseExpression()}) {
    std::optional<AST::LateIf> lateIf{parseLateIf()};
    std::optional<std::string_view> srcSemicolon{nextDelimiter(";")};
    if (!srcSemicolon)
      throwUnexpectedToken(mSrcLoc, "expected ';' after expression", &srcLoc0);
    return allocate<AST::ExprStmt>(srcLoc0, std::in_place, std::move(expr),
                                   std::move(lateIf), *srcSemicolon);
  }
  return nullptr;
}

auto Parser::parseCompoundStatement() -> BumpPtr<AST::Compound> {
  std::optional<Parser::ParsedToken> braceL{nextDelimiterAndLocation("{")};
  if (!braceL) return nullptr;
  std::vector<BumpPtr<AST::Stmt>> stmts{};
  while (true) {
    BumpPtr<AST::Stmt> stmt{parseStatement()};
    if (!stmt) break;
    stmts.push_back(std::move(stmt));
    skip();
    if (peek() == '}') break;
  }
  std::optional<std::string_view> srcBraceR{nextDelimiter("}")};
  if (!srcBraceR)
    throwUnexpectedToken(
        mSrcLoc, concat("expected a statement or '}' to close the block "
                        "starting at line ",
                        braceL->srcLoc.lineNo));
  return allocate<AST::Compound>(braceL->srcLoc, std::in_place, braceL->src,
                                 std::move(stmts), *srcBraceR);
}

auto Parser::parseIfStatement() -> BumpPtr<AST::If> {
  std::optional<Parser::ParsedToken> kwIf{nextKeywordAndLocation("if")};
  if (!kwIf) return nullptr;
  SourceLocation srcLoc0{kwIf->srcLoc};
  BumpPtr<AST::Expr> exprCond{parseExpressionInParentheses()};
  if (!exprCond)
    srcLoc0.throwError("Expected parenthesized condition after 'if'");
  BumpPtr<AST::Stmt> ifPass{parseStatement()};
  if (!ifPass) srcLoc0.throwError("Expected statement after 'if (...)'");
  if (std::optional<std::string_view> srcKwElse{nextKeyword("else")}) {
    BumpPtr<AST::Stmt> ifFail{parseStatement()};
    if (!ifFail) srcLoc0.throwError("Expected statement after 'else'");
    return allocate<AST::If>(srcLoc0, std::in_place, kwIf->src,
                             std::move(exprCond), std::move(ifPass), *srcKwElse,
                             std::move(ifFail));
  } else {
    return allocate<AST::If>(srcLoc0, std::in_place, kwIf->src,
                             std::move(exprCond), std::move(ifPass),
                             std::string_view(), nullptr);
  }
}

auto Parser::parseSwitchStatement() -> BumpPtr<AST::Switch> {
  std::optional<Parser::ParsedToken> kwSwitch{nextKeywordAndLocation("switch")};
  if (!kwSwitch) return nullptr;
  SourceLocation srcLoc0{kwSwitch->srcLoc};
  BumpPtr<AST::Expr> expr{parseExpressionInParentheses()};
  if (!expr)
    srcLoc0.throwError("Expected parenthesized expression after 'switch'");
  std::optional<std::string_view> srcBraceL{nextDelimiter("{")};
  if (!srcBraceL) srcLoc0.throwError("Expected opening '{' after 'switch'");
  std::vector<AST::Switch::Case> switchCases{};
  while (true) {
    std::optional<AST::Switch::Case> switchCase{parseSwitchCase()};
    if (!switchCase) break;
    switchCases.push_back(std::move(*switchCase));
    skip();
    if (peek() == '}') break;
  }
  std::optional<std::string_view> srcBraceR{nextDelimiter("}")};
  if (!srcBraceR)
    throwUnexpectedToken(mSrcLoc,
                         "expected 'case', 'default', or '}' in 'switch'");
  return allocate<AST::Switch>(srcLoc0, std::in_place, kwSwitch->src,
                               std::move(expr), *srcBraceL,
                               std::move(switchCases), *srcBraceR);
}

auto Parser::parseSwitchCase() -> std::optional<AST::Switch::Case> {
  skip();
  SourceLocation srcLoc0{mSrcLoc};
  AST::Switch::Case switchCase{};
  if (std::optional<std::string_view> srcKwCase{nextKeyword("case")}) {
    BumpPtr<AST::Expr> expr{parseExpression()};
    if (!expr) srcLoc0.throwError("Expected expression after 'case'");
    std::optional<std::string_view> srcColon{nextDelimiter(":")};
    if (!srcColon) srcLoc0.throwError("Expected ':' after 'case ...'");
    switchCase.srcKwCaseOrDefault = *srcKwCase;
    switchCase.expr = std::move(expr);
    switchCase.srcColon = *srcColon;
  } else if (std::optional<std::string_view> srcKwDefault{
                 nextKeyword("default")}) {
    std::optional<std::string_view> srcColon{nextDelimiter(":")};
    if (!srcColon) srcLoc0.throwError("Expected ':' after 'default'");
    switchCase.srcKwCaseOrDefault = *srcKwDefault;
    switchCase.srcColon = *srcColon;
  } else {
    return std::nullopt;
  }
  while (true) {
    BumpPtr<AST::Stmt> stmt{parseStatement()};
    if (!stmt) break;
    switchCase.stmts.push_back(std::move(stmt));
    skip();
  }
  return std::move(switchCase);
}

auto Parser::parseWhileStatement() -> BumpPtr<AST::While> {
  std::optional<Parser::ParsedToken> kwWhile{nextKeywordAndLocation("while")};
  if (!kwWhile) return nullptr;
  SourceLocation srcLoc0{kwWhile->srcLoc};
  BumpPtr<AST::Expr> expr{parseExpressionInParentheses()};
  if (!expr)
    srcLoc0.throwError("Expected parenthesized expression after 'while'");
  BumpPtr<AST::Stmt> stmt{parseStatement()};
  if (!stmt) srcLoc0.throwError("Expected statement after 'while (...)'");
  return allocate<AST::While>(srcLoc0, std::in_place, kwWhile->src,
                              std::move(expr), std::move(stmt));
}

auto Parser::parseDoStatement() -> BumpPtr<AST::DoWhile> {
  std::optional<Parser::ParsedToken> kwDo{nextKeywordAndLocation("do")};
  if (!kwDo) return nullptr;
  SourceLocation srcLoc0{kwDo->srcLoc};
  BumpPtr<AST::Stmt> stmt{parseStatement()};
  if (!stmt) srcLoc0.throwError("Expected statement after 'do'");
  std::optional<std::string_view> srcKwWhile{nextKeyword("while")};
  if (!srcKwWhile) srcLoc0.throwError("Expected 'while' after 'do ...'");
  BumpPtr<AST::Expr> expr{parseExpressionInParentheses()};
  if (!expr)
    srcLoc0.throwError(
        "Expected parenthesized expression after 'do ... while'");
  std::optional<std::string_view> srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon)
    srcLoc0.throwError("Expected ';' after 'do ... while (...)'");
  return allocate<AST::DoWhile>(srcLoc0, std::in_place, kwDo->src,
                                std::move(stmt), *srcKwWhile, std::move(expr),
                                *srcSemicolon);
}

auto Parser::parseForStatement() -> BumpPtr<AST::For> {
  std::optional<Parser::ParsedToken> kwFor{nextKeywordAndLocation("for")};
  if (!kwFor) return nullptr;
  SourceLocation srcLoc0{kwFor->srcLoc};
  std::optional<std::string_view> srcParenL{nextDelimiter("(")};
  if (!srcParenL) srcLoc0.throwError("Expected '(' after 'for'");
  BumpPtr<AST::Stmt> stmtInit{};
  if (BumpPtr<AST::Variable> decl{parseVariableDeclaration()}) {
    stmtInit =
        allocate<AST::DeclStmt>(decl->srcLoc, std::in_place, std::move(decl));
  } else if (BumpPtr<AST::Expr> expr{parseExpression()}) {
    std::optional<std::string_view> srcSemicolon{nextDelimiter(";")};
    if (!srcSemicolon)
      throwUnexpectedToken(mSrcLoc, "expected ';' after expression", &srcLoc0);
    stmtInit =
        allocate<AST::ExprStmt>(expr->srcLoc, std::in_place, std::move(expr),
                                std::nullopt, *srcSemicolon);
  } else {
    srcLoc0.throwError(
        "Expected variable declaration or expression after 'for ('");
  }
  BumpPtr<AST::Expr> exprCond{parseExpression()};
  std::optional<std::string_view> srcSemicolonAfterCond{nextDelimiter(";")};
  if (!srcSemicolonAfterCond)
    srcLoc0.throwError("Expected ';' after 'for (... ; ...'");
  BumpPtr<AST::Expr> exprIncr{parseExpression()};
  std::optional<std::string_view> srcParenR{nextDelimiter(")")};
  if (!srcParenR) srcLoc0.throwError("Expected ')' after 'for (...'");
  BumpPtr<AST::Stmt> stmt{parseStatement()};
  if (!stmt) srcLoc0.throwError("Expected statement after 'for (...)'");
  return allocate<AST::For>(srcLoc0, std::in_place, kwFor->src, *srcParenL,
                            std::move(stmtInit), std::move(exprCond),
                            *srcSemicolonAfterCond, std::move(exprIncr),
                            *srcParenR, std::move(stmt));
}

auto Parser::parseBreakStatement() -> BumpPtr<AST::Break> {
  return parseJumpStatement<AST::Break>("break");
}

auto Parser::parseContinueStatement() -> BumpPtr<AST::Continue> {
  return parseJumpStatement<AST::Continue>("continue");
}

auto Parser::parseReturnStatement() -> BumpPtr<AST::Return> {
  std::optional<Parser::ParsedToken> kwReturn{nextKeywordAndLocation("return")};
  if (!kwReturn) return nullptr;
  BumpPtr<AST::Expr> expr{parseExpression()}; // Allow this to be null!
  std::optional<AST::LateIf> lateIf{parseLateIf()};
  std::optional<std::string_view> srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon)
    throwUnexpectedToken(mSrcLoc, "expected ';' after 'return ...'",
                         &kwReturn->srcLoc);
  return allocate<AST::Return>(kwReturn->srcLoc, std::in_place, kwReturn->src,
                               std::move(expr), std::move(lateIf),
                               *srcSemicolon);
}

auto Parser::parseUnreachableStatement() -> BumpPtr<AST::Unreachable> {
  std::optional<Parser::ParsedToken> kwUnreachable{
      nextKeywordAndLocation("unreachable")};
  if (!kwUnreachable) return nullptr;
  std::optional<std::string_view> srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon)
    kwUnreachable->srcLoc.throwError("Expected ';' after 'unreachable'");
  return allocate<AST::Unreachable>(kwUnreachable->srcLoc, std::in_place,
                                    kwUnreachable->src, *srcSemicolon);
}

auto Parser::parsePreserveStatement() -> BumpPtr<AST::Preserve> {
  std::optional<Parser::ParsedToken> kwPreserve{
      nextKeywordAndLocation("preserve")};
  if (!kwPreserve) return nullptr;
  std::vector<AST::Preserve::ExprWrapper> exprs{};
  parseCommaSeparated(exprs,
                      [&]() -> std::optional<AST::Preserve::ExprWrapper> {
                        BumpPtr<AST::Expr> expr{parseUnaryExpression()};
                        if (!expr) return std::nullopt;
                        return AST::Preserve::ExprWrapper{std::move(expr), {}};
                      });
  std::optional<std::string_view> srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon)
    kwPreserve->srcLoc.throwError("Expected ';' after 'preserve ...'");
  return allocate<AST::Preserve>(kwPreserve->srcLoc, std::in_place,
                                 kwPreserve->src, std::move(exprs),
                                 *srcSemicolon);
}

auto Parser::parseDeferStatement() -> BumpPtr<AST::Defer> {
  std::optional<Parser::ParsedToken> kwDefer{nextKeywordAndLocation("defer")};
  if (!kwDefer) return nullptr;
  BumpPtr<AST::Stmt> stmt{parseStatement()};
  if (!stmt) kwDefer->srcLoc.throwError("Expected statement after 'defer'");
  return allocate<AST::Defer>(kwDefer->srcLoc, std::in_place, kwDefer->src,
                              std::move(stmt));
}

auto Parser::parseVisitStatement() -> BumpPtr<AST::Visit> {
  std::optional<Parser::ParsedToken> kwVisit{nextKeywordAndLocation("visit")};
  if (!kwVisit) return nullptr;
  SourceLocation srcLoc0{kwVisit->srcLoc};
  std::optional<AST::Name> name{parseSimpleName()};
  if (!name) srcLoc0.throwError("Expected name after 'visit'");
  std::optional<std::string_view> srcKwIn{nextKeyword("in")};
  if (!srcKwIn) srcLoc0.throwError("Expected 'in' after 'visit ...'");
  BumpPtr<AST::Expr> expr{parseExpression()};
  if (!expr) srcLoc0.throwError("Expected expression after 'visit ... in'");
  BumpPtr<AST::Compound> stmt{parseCompoundStatement()};
  if (!stmt)
    srcLoc0.throwError("Expected compound statement after 'visit ... in ...'");
  return allocate<AST::Visit>(srcLoc0, std::in_place, kwVisit->src, *name,
                              *srcKwIn, std::move(expr), std::move(stmt));
}

auto Parser::parseLateIf() -> std::optional<AST::LateIf> {
  if (!mIsSMDL) return std::nullopt;
  std::optional<Parser::ParsedToken> kwIf{nextKeywordAndLocation("if")};
  if (!kwIf) return std::nullopt;
  BumpPtr<AST::Expr> expr{parseExpressionInParentheses()};
  if (!expr)
    kwIf->srcLoc.throwError(
        "Expected expression in parentheses after '... if'");
  return AST::LateIf(kwIf->src, std::move(expr));
}
//--}

} // namespace smdl
