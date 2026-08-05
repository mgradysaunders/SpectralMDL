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
  auto ch{peek()};
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
  auto result{getRemainingSourceCode().substr(0, n)};
  for (size_t i = 0; i < n && !isEOF(); i++) next();
  return result;
}

std::optional<std::string_view> Parser::next(std::string_view str) {
  if (startsWith(getRemainingSourceCode(), str)) return next(str.size());
  return std::nullopt;
}

std::optional<std::string_view> Parser::nextKeyword(std::string_view str) {
  checkpoint();
  auto result{next(str)};
  if (!result || isWord(peek())) {
    reject();
    return std::nullopt;
  } else {
    accept();
    return result;
  }
}

std::optional<std::string_view> Parser::nextWord() {
  checkpoint();
  auto i{mSrcLoc.i};
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
  auto i{mSrcLoc.i};
  while (isDigit(peek())) next();
  if (mSrcLoc.i > i) {
    return getSourceCode().substr(i, mSrcLoc.i - i);
  } else {
    return std::nullopt;
  }
}

/// Is the source between a documentation comment and what follows it
/// close enough to attach? I.e., only whitespace with at most one
/// newline, so that a blank line breaks attachment.
[[nodiscard]] static bool isDocCommentAdjacent(std::string_view src) {
  int numNewLines{};
  for (char ch : src) {
    if (!isSpace(ch)) return false;
    if (ch == '\n' && ++numNewLines > 1) return false;
  }
  return true;
}

void Parser::skip() {
  auto skipSome{[&] {
    if (startsWith(getRemainingSourceCode(), "//")) {
      auto iComment{mSrcLoc.i};
      next(2);
      while (!isEOF() && peek() != '\n') next(1);
      // A `///` comment (but not a `///<` trailing comment) is a
      // documentation line: remember it so that declaration parsers can
      // pick it up with `getDocCommentBefore()`, merging consecutive
      // lines into one block. Every other comment breaks the pending
      // block. A rewind by `reject()` may re-scan the block, in which
      // case `iComment <= mPendingDocCommentEnd` restarts it in place.
      auto comment{getSourceCode().substr(iComment, mSrcLoc.i - iComment)};
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
      auto srcLocComment{mSrcLoc};
      next(2);
      while (!isEOF() && !startsWith(getRemainingSourceCode(), "*/")) next(1);
      if (isEOF()) srcLocComment.throwError("unterminated multiline comment");
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
  auto srcLoc0{checkpoint()};
  if (auto name{nextWord()}) {
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
  auto srcLoc0{checkpoint()};
  auto elements{std::vector<AST::Identifier::Element>{}};
  auto srcDoubleColon{next("::")};
  if (auto name{parseSimpleName()}) {
    elements.push_back(
        AST::Identifier::Element{orEmpty(srcDoubleColon), *name});
  } else {
    if (srcDoubleColon) {
      srcLoc0.throwError("expected name after '::'");
    } else {
      reject();
      return nullptr;
    }
  }
  while (true) {
    checkpoint();
    if (srcDoubleColon = next("::"); srcDoubleColon) {
      if (auto name{parseSimpleName()}) {
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
  auto srcLoc0{checkpoint()};
  auto srcQuals{std::vector<std::string_view>()};
  while (true) {
    checkpoint();
    // The `inline` and `static` qualifiers are extended syntax!
    if (auto srcQual{mIsSMDL ? nextKeyword({"const", "inline", "static",
                                            "uniform", "varying"})
                             : nextKeyword({"const", "uniform", "varying"})}) {
      accept();
      srcQuals.push_back(*srcQual);
    } else {
      reject();
      break;
    }
  }
  auto expr{parseUnaryExpression()};
  if (!expr) {
    reject();
    return nullptr;
  }
  accept();
  return allocate<AST::Type>(srcLoc0, std::in_place, std::move(srcQuals),
                             std::move(expr));
}

auto Parser::parseParameter() -> std::optional<AST::Parameter> {
  auto srcLoc0{checkpoint()};
  // Capture before parsing: comments inside the parameter must not
  // clobber the pending block first.
  auto srcDocComment{getDocCommentBefore(srcLoc0.i)};
  auto type{parseType()};
  if (!type) {
    reject();
    return std::nullopt;
  }
  auto name{parseSimpleName()};
  if (!name) {
    reject();
    return std::nullopt;
  }
  auto param{AST::Parameter{}};
  param.srcLoc = srcLoc0;
  param.srcDocComment = srcDocComment;
  param.type = std::move(type);
  param.name = *name;
  if (auto srcEqual{nextDelimiter("=")}) {
    auto exprInit{parseAssignmentExpression()};
    if (!exprInit) srcLoc0.throwError("expected initializer after '='");
    param.srcEqual = *srcEqual;
    param.exprInit = std::move(exprInit);
  }
  param.annotations = parseAnnotationBlock();
  accept();
  return std::move(param);
}

auto Parser::parseParameterList() -> std::optional<AST::ParameterList> {
  checkpoint();
  auto params{AST::ParameterList{}};
  auto srcParenL{nextDelimiter("(")};
  if (!srcParenL) {
    reject();
    return std::nullopt;
  }
  params.srcParenL = *srcParenL;
  skip();
  if (auto srcStar{nextDelimiter("*")}) {
    params.srcStar = *srcStar;
  } else {
    params.params.reserve(4);
    parseCommaSeparated(params.params, [&] { return parseParameter(); });
    auto srcEllipsis{nextDelimiter("...")};
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
  auto srcParenR{nextDelimiter(")")};
  if (!srcParenR) {
    reject();
    return std::nullopt;
  }
  params.srcParenR = *srcParenR;
  accept();
  return std::move(params);
}

auto Parser::parseArgument() -> std::optional<AST::Argument> {
  auto srcLoc0{checkpoint()};
  auto argument{AST::Argument{}};
  argument.srcLoc = srcLoc0;
  if (mIsSMDL) {
    if (auto srcKwVisit{nextKeyword("visit")}) {
      argument.srcKwVisit = *srcKwVisit;
    }
  }
  argument.name = [&]() -> AST::Name {
    checkpoint();
    if (auto name{parseSimpleName()}) {
      if (auto srcColon{nextDelimiter(":")};
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
  auto srcLoc0{checkpoint()};
  auto args{AST::ArgumentList{}};
  args.srcLoc = srcLoc0;
  auto srcParenL{nextDelimiter("(")};
  if (!srcParenL) {
    reject();
    return std::nullopt;
  }
  args.srcParenL = *srcParenL;
  parseCommaSeparated(args.args, [&] { return parseArgument(); });
  auto srcParenR{nextDelimiter(")")};
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
  auto identifier{parseIdentifier()};
  if (!identifier) {
    reject();
    return std::nullopt;
  }
  auto args{parseArgumentList()};
  if (!args) {
    reject();
    return std::nullopt;
  }
  accept();
  return AST::Annotation{std::move(identifier), std::move(*args)};
}

auto Parser::parseAnnotationBlock() -> BumpPtr<AST::AnnotationBlock> {
  auto brackL{nextDelimiterAndLocation("[[")};
  if (!brackL) return nullptr;
  auto annos{std::vector<AST::Annotation>{}};
  parseCommaSeparated(annos, [&] { return parseAnnotation(); }, "]]");
  auto srcDoubleBrackR{nextDelimiter("]]")};
  if (!srcDoubleBrackR)
    mSrcLoc.throwError("expected annotation, ',', or ']]' in annotation block");
  return allocate<AST::AnnotationBlock>(brackL->srcLoc, std::in_place,
                                        brackL->src, std::move(annos),
                                        *srcDoubleBrackR);
}

auto Parser::parseExpressionInParentheses() -> BumpPtr<AST::Expr> {
  auto srcLoc0{checkpoint()};
  auto srcDollar{nextDelimiter("$")};
  auto srcParenL{nextDelimiter("(")};
  if (!srcParenL) {
    reject();
    return nullptr;
  }
  auto expr{parseExpression()};
  if (!expr) {
    reject();
    return nullptr;
  }
  auto srcParenR{nextDelimiter(")")};
  if (!srcParenR) srcLoc0.throwError("expected closing ')'");
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
  auto expr{parseLogicalOrExpression()};
  if (!expr) return nullptr;
  skip();
  auto srcLoc0{mSrcLoc};
  if (auto srcQuestion{next("?")}) {
    auto exprThen{parseExpression()};
    if (!exprThen)
      srcLoc0.throwError("expected then clause in conditional expression");
    skip();
    auto srcColon{next(":")};
    if (!srcColon)
      mSrcLoc.throwError("expected ':' after then clause in conditional "
                         "expression");
    auto exprElse{parseAssignmentExpression()};
    if (!exprElse)
      srcLoc0.throwError("expected else clause in conditional expression");
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
    if (auto expr{parseLambdaExpression()}) return expr;
  }
  if (auto expr{parsePostfixExpression()}) return expr;
  auto parsePrefixExpression{[&]() -> BumpPtr<AST::Expr> {
    auto srcLoc0{checkpoint()};
    auto op{parseUnaryOp()};
    if (!op) {
      reject();
      return nullptr;
    }
    auto expr{parseUnaryExpression()};
    if (!expr) {
      reject();
      return nullptr;
    }
    accept();
    expr = allocate<AST::Unary>(srcLoc0, std::in_place, op->srcOp, op->op,
                                std::move(expr));
    return expr;
  }};
  if (auto expr{parsePrefixExpression()}) return expr;
  if (auto expr{parseLetExpression()}) return expr;
  if (mIsSMDL) {
    if (auto expr{parseReturnFromExpression()}) return expr;
  }
  return nullptr;
}

auto Parser::parsePostfixExpression() -> BumpPtr<AST::Expr> {
  auto expr{parsePrimaryExpression()};
  if (!expr) return nullptr;
  auto withPostfix{[&]() -> BumpPtr<AST::Expr> {
    auto srcLoc0{mSrcLoc};
    if (auto srcDot{nextDelimiter(".")}) {
      auto name{parseSimpleName()};
      if (!name) srcLoc0.throwError("expected name after '.'");
      return allocate<AST::AccessField>(srcLoc0, std::in_place, std::move(expr),
                                        *srcDot, *name);
    }
    if (auto srcOp{nextDelimiter("++")})
      return allocate<AST::Unary>(srcLoc0, std::in_place, *srcOp,
                                  UNOP_POSTFIX_INC, std::move(expr));
    if (auto srcOp{nextDelimiter("--")})
      return allocate<AST::Unary>(srcLoc0, std::in_place, *srcOp,
                                  UNOP_POSTFIX_DEC, std::move(expr));
    if (auto args{parseArgumentList()})
      return allocate<AST::Call>(srcLoc0, std::in_place, std::move(expr),
                                 std::move(*args));
    auto indexes{std::vector<AST::AccessIndex::Index>{}};
    while (!startsWith(getRemainingSourceCode(), "[[")) {
      auto index{AST::AccessIndex::Index{}};
      auto srcBrackL{nextDelimiter("[")};
      if (!srcBrackL) break;
      if (auto srcAngleL{nextDelimiter("<")}) {
        auto name{parseSimpleName()};
        if (!name) srcLoc0.throwError("expected name after '[<'");
        auto srcAngleR{nextDelimiter(">")};
        if (!srcAngleR) srcLoc0.throwError("expected '>]'");
        index.expr = allocate<AST::SizeName>(srcLoc0, std::in_place, *srcAngleL,
                                             *name, *srcAngleR);
      } else {
        index.expr = parseExpression(); // This may be null to represent `[]`
      }
      auto srcBrackR{nextDelimiter("]")};
      if (!srcBrackR) srcLoc0.throwError("expected ']'");
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
    auto nextExpr{withPostfix()};
    if (!nextExpr) break;
    expr = std::move(nextExpr);
  }
  return expr;
}

auto Parser::parseLetExpression() -> BumpPtr<AST::Expr> {
  auto kwLet{nextKeywordAndLocation("let")};
  if (!kwLet) return nullptr;
  auto srcLoc0{kwLet->srcLoc};
  auto decls{std::vector<BumpPtr<AST::Decl>>{}};
  auto srcBraceL{std::optional<std::string_view>()};
  auto srcBraceR{std::optional<std::string_view>()};
  if (srcBraceL = nextDelimiter("{"); srcBraceL) {
    while (true) {
      auto decl{parseVariableDeclaration()};
      if (!decl) break;
      decls.push_back(std::move(decl));
      skip();
      if (peek() == '}') break;
    }
    if (srcBraceR = nextDelimiter("}"); !srcBraceR)
      srcLoc0.throwError("expected closing '}' after 'let'");
  } else {
    auto decl{parseVariableDeclaration()};
    if (!decl) srcLoc0.throwError("expected variable declaration after 'let'");
    decls.push_back(std::move(decl));
  }
  auto srcKwIn{nextKeyword("in")};
  if (!srcKwIn) srcLoc0.throwError("expected 'in' after 'let ...'");
  auto expr{parseConditionalExpression()};
  if (!expr) srcLoc0.throwError("expected expression after 'let ... in'");
  return allocate<AST::Let>(srcLoc0, std::in_place, kwLet->src,
                            orEmpty(srcBraceL), std::move(decls),
                            orEmpty(srcBraceR), *srcKwIn, std::move(expr));
}

auto Parser::parseReturnFromExpression() -> BumpPtr<AST::Expr> {
  auto kwReturnFrom{nextKeywordAndLocation("return_from")};
  if (!kwReturnFrom) return nullptr;
  auto stmt{parseCompoundStatement()};
  if (!stmt)
    kwReturnFrom->srcLoc.throwError(
        "expected compound statement after 'return_from'");
  return allocate<AST::ReturnFrom>(kwReturnFrom->srcLoc, std::in_place,
                                   kwReturnFrom->src, std::move(stmt));
}

auto Parser::parseLambdaExpression() -> BumpPtr<AST::Expr> {
  auto backslash{nextDelimiterAndLocation("\\")};
  if (!backslash) return nullptr;
  // The backslash unambiguously introduces a lambda, so everything from
  // here on is a committed parse. There is no return type syntax; the
  // return type is always implicitly `auto`.
  auto srcLoc0{backslash->srcLoc};
  auto params{parseParameterList()};
  if (!params) srcLoc0.throwError("expected parameter list after '\\'");
  if (params->isVariant())
    srcLoc0.throwError("lambda must not be a function variant");
  if (params->hasTrailingEllipsis())
    srcLoc0.throwError("lambda must not be variadic");
  auto srcEqual{std::optional<std::string_view>()};
  auto definition{BumpPtr<AST::Node>{}};
  if (srcEqual = nextDelimiter("="); srcEqual) {
    skip();
    auto srcLoc1{mSrcLoc};
    // The body is an assignment expression, not a full expression: the
    // comma operator must not swallow subsequent arguments when the lambda
    // appears in an argument list.
    auto def{parseAssignmentExpression()};
    if (!def) srcLoc0.throwError("expected lambda expression after '='");
    definition =
        allocate<AST::Return>(srcLoc1, std::in_place, std::string_view(),
                              std::move(def), std::nullopt, std::string_view());
  } else {
    auto def{parseCompoundStatement()};
    if (!def)
      srcLoc0.throwError("expected '=' or compound statement after lambda "
                         "parameter list");
    definition = std::move(def);
  }
  auto func{allocate<AST::Function>(
      srcLoc0, std::in_place, BumpPtr<AST::Type>{},
      BumpPtr<AST::AnnotationBlock>{}, AST::Name{}, std::move(*params),
      std::string_view(), BumpPtr<AST::AnnotationBlock>{}, orEmpty(srcEqual),
      std::move(definition), std::string_view())};
  return allocate<AST::Lambda>(srcLoc0, std::in_place, backslash->src,
                               std::move(func));
}

auto Parser::parsePrimaryExpression() -> BumpPtr<AST::Expr> {
  if (auto expr{parseExpressionInParentheses()}) return expr;
  if (auto expr{parseLiteralExpression()}) return expr;
  if (auto expr{parseIdentifier()}) return expr;
  auto srcLoc0{mSrcLoc};
  if (auto srcKwCast{nextKeyword("cast")}) {
    auto srcAngleL{nextDelimiter("<")};
    if (!srcAngleL) srcLoc0.throwError("expected opening '<' after 'cast'");
    auto type{parseType()};
    if (!type) srcLoc0.throwError("expected type after 'cast'");
    auto srcAngleR{nextDelimiter(">")};
    if (!srcAngleR) srcLoc0.throwError("expected closing '>' after 'cast'");
    auto expr{parseExpressionInParentheses()};
    if (!expr)
      srcLoc0.throwError("expected parenthesized expression after 'cast<...>'");
    return allocate<AST::TypeCast>(srcLoc0, std::in_place, *srcKwCast,
                                   *srcAngleL, std::move(type), *srcAngleR,
                                   std::move(expr));
  }
  return nullptr;
}

auto Parser::parseLiteralExpression() -> BumpPtr<AST::Expr> {
  if (auto expr{parseLiteralBoolExpression()}) return expr;
  if (auto expr{parseLiteralStringExpression()}) return expr;
  if (auto expr{parseLiteralNumberExpression()}) return expr;
  if (mIsSMDL) {
    skip();
    auto srcLoc0{mSrcLoc};
    if (next("#")) {
      auto word{nextWord()};
      if (!word) srcLoc0.throwError("expected intrinsic name after '#'");
      if (*word == "search_dir")
        srcLoc0.throwError("'#search_dir' is only allowed at the top of the "
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
  auto srcLoc0{mSrcLoc};
  if (auto srcValue{nextKeyword("true")})
    return allocate<AST::LiteralBool>(srcLoc0, std::in_place, *srcValue, true);
  if (auto srcValue{nextKeyword("false")})
    return allocate<AST::LiteralBool>(srcLoc0, std::in_place, *srcValue, false);
  return nullptr;
}

auto Parser::parseLiteralStringExpression() -> BumpPtr<AST::LiteralString> {
  skip();
  if (peek() != '"') return nullptr;
  auto str{std::string()};
  auto srcLoc0{mSrcLoc};
  auto appendCodepointAsUTF8{[&](uint32_t codepoint) {
    char result[4]{};
    char *resultPtr{&result[0]};
    if (!llvm::ConvertCodePointToUTF8(codepoint, resultPtr)) return false;
    str.insert(str.end(), &result[0], resultPtr);
    return true;
  }};
  auto srcValues{std::vector<std::string_view>{}};
  // The start of the current string segment, which advances past `srcLoc0`
  // when adjacent string literals are concatenated.
  auto srcLocSeg{srcLoc0};
  while (nextDelimiter("\"")) {
    while (true) {
      if (isEOF()) srcLocSeg.throwError("unexpected EOF in literal string");
      if (peek() == '\n')
        srcLocSeg.throwError("unexpected EOL in literal string");
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
              srcLocSeg.throwError("expected 3 octal digits after '\\'");
            byte = (byte << 3) | uint32_t(octToInt(ch));
          }
          if (byte > 255)
            srcLocSeg.throwError("octal escape sequence out of range");
          str += static_cast<char>(byte);
        } else if (ch == 'x') { // hexadecimal
          uint8_t byte{};
          for (int i{}; i < 2; i++) {
            ch = next();
            if (!isDigit16(ch))
              srcLocSeg.throwError("expected 2 hexadecimal digits after '\\x'");
            byte = (byte << 4) | uint8_t(hexToInt(ch));
          }
          str += static_cast<char>(byte);
        } else if (ch == 'u') { // unicode 16-bit
          uint32_t codepoint{};
          for (int i{}; i < 4; i++) {
            ch = next();
            if (!isDigit16(ch))
              srcLocSeg.throwError("expected 4 hexadecimal digits after '\\u'");
            codepoint = (codepoint << 4) | uint32_t(hexToInt(ch));
          }
          if (!appendCodepointAsUTF8(codepoint))
            srcLocSeg.throwError("UTF-8 encoding of '\\u' sequence failed");
        } else if (ch == 'U') { // unicode 32-bit
          uint32_t codepoint{};
          for (int i{}; i < 8; i++) {
            ch = next();
            if (!isDigit16(ch))
              srcLocSeg.throwError("expected 8 hexadecimal digits after '\\U'");
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
      srcLocSeg.throwError("expected '\"' to close literal string");
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
  auto srcLoc0{mSrcLoc};
  auto parseDigits{[&](auto &&isDigit) {
    std::string digits{};
    while (isDigit(peek())) {
      digits.push_back(peek());
      next();
      if (next("'")) { // Maybe consume single-quote separator
        if (peek() == '\'')
          srcLoc0.throwError("numeric literal must not contain adjacent "
                             "single-quote separators");
        if (!isDigit(peek()))
          srcLoc0.throwError("numeric literal must not be terminated by "
                             "single-quote separator");
      }
    }
    return digits;
  }};
  auto parseIntWithPrefix{[&](auto &&isDigit, int radix, const char *prefix,
                              const char *info, std::string &digitsStr) {
    if (!isDigit(peek()))
      srcLoc0.throwError("expected literal prefix ", Quoted(prefix),
                         " to be followed by ", info);
    auto digits{parseDigits(isDigit)};
    auto bits{llvm::APInt::getBitsNeeded(digits, radix)};
    if (bits > 64) srcLoc0.logWarn("integer literal exceeds 64 bits");
    digitsStr = prefix;
    digitsStr += std::string(digits);
    return llvm::APInt(bits, digits, radix);
  }};
  // Is the remaining source code `0` followed by any of the given
  // characters? A `0` followed by `.`, an exponent, or a suffix begins an
  // ordinary decimal literal, not an octal/binary/hexadecimal literal.
  auto zeroFollowedByAny{[&](std::string_view chars) {
    auto remaining{getRemainingSourceCode()};
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
    if (isDigit(peek())) mSrcLoc.throwError("invalid digit in integer literal");
    return allocate<AST::LiteralInt>(srcLoc0, std::in_place,
                                     getSourceCodeBetween(srcLoc0, mSrcLoc),
                                     value.getLimitedValue());
  } else {
    bool isInt{true};
    auto digits{parseDigits(isDigit)};
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
            "expected exponent after 'e' in floating point literal");
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
      auto bits{llvm::APInt::getBitsNeeded(digits, 10)};
      if (bits > 64) srcLoc0.logWarn("integer literal exceeds 64 bits");
      return allocate<AST::LiteralInt>(
          srcLoc0, std::in_place, getSourceCodeBetween(srcLoc0, mSrcLoc),
          llvm::APInt(bits, digits, 10).getLimitedValue());
    } else {
      llvm::APFloat value(llvm::APFloat::IEEEdouble());
      auto opStatus{
          value.convertFromString(digits, llvm::APFloat::rmNearestTiesToEven)};
      if (!opStatus) {
        llvm::consumeError(opStatus.takeError());
        srcLoc0.throwError("failed to parse floating point literal");
      }
      if (*opStatus & llvm::APFloat::opOverflow)
        srcLoc0.logWarn("floating point literal exceeds range of 'double'");
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
    if (auto srcOp{next(to_string(op))}) return ParsedUnaryOp{*srcOp, op};
  if (mIsSMDL) {
    for (auto op : std::array{UNOP_ADDR, UNOP_DEREF, UNOP_MAYBE})
      if (auto srcOp{next(to_string(op))}) return ParsedUnaryOp{*srcOp, op};
  }
  return std::nullopt;
}

auto Parser::parseBinaryOp(Span<const AST::BinaryOp> ops)
    -> std::optional<ParsedBinaryOp> {
  for (auto op : ops) {
    if (!mIsSMDL && isExtendedSyntax(op)) continue;
    if (op == BINOP_ELSE) {
      if (auto srcOp{nextKeyword(to_string(op))})
        return ParsedBinaryOp{*srcOp, op};
    } else {
      // Don't mistake bit and for logical and.
      if (op == BINOP_AND && startsWith(getRemainingSourceCode(), "&&"))
        continue;
      if (auto srcOp{next(to_string(op))}) return ParsedBinaryOp{*srcOp, op};
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
  auto srcDocComment{getPendingDocComment()};
  auto srcLoc0{mSrcLoc};
  auto srcKwSmdlSyntax{nextKeyword("#smdl")};
  if (srcKwSmdlSyntax) mIsSMDL = true;
  auto searchDirs{parseFileSearchDirs()};
  auto version{parseFileVersion()};
  if (!version && !mIsSMDL) srcLoc0.throwError("expected MDL version");
  auto importDecls{std::vector<BumpPtr<AST::Decl>>{}};
  while (true) {
    auto parseAnyImport{[&]() -> BumpPtr<AST::Decl> {
      if (auto decl{parseUsingAlias()}) return decl;
      if (auto decl{parseUsingImport()}) return decl;
      if (auto decl{parseImport()}) return decl;
      return nullptr;
    }};
    auto decl{parseAnyImport()};
    if (!decl) break;
    importDecls.push_back(std::move(decl));
  }
  auto srcKwModule{nextKeyword("module")};
  auto moduleAnnotations{BumpPtr<AST::AnnotationBlock>{}};
  auto srcSemicolonAfterModule{std::optional<std::string_view>()};
  if (srcKwModule) {
    moduleAnnotations = parseAnnotationBlock();
    if (!moduleAnnotations)
      srcLoc0.throwError("expected annotation block after 'module'");
    srcSemicolonAfterModule = nextDelimiter(";");
    if (!srcSemicolonAfterModule)
      srcLoc0.throwError("expected ';' after 'module [[ ... ]]'");
  }
  auto globalDecls{std::vector<BumpPtr<AST::Decl>>{}};
  while (true) {
    auto decl{parseGlobalDeclaration()};
    if (!decl) break;
    globalDecls.push_back(std::move(decl));
    skip();
    if (isEOF()) break;
  }
  if (!isEOF()) {
    if (startsWith(getRemainingSourceCode(), "#search_dir"))
      mSrcLoc.throwError("'#search_dir' is only allowed at the top of the "
                         "file immediately after '#smdl'");
    mSrcLoc.throwError("unexpected token, expected a declaration");
  }
  auto file{allocate<AST::File>(
      srcLoc0, std::in_place, orEmpty(srcKwSmdlSyntax), std::move(searchDirs),
      std::move(version), std::move(importDecls), orEmpty(srcKwModule),
      std::move(moduleAnnotations), orEmpty(srcSemicolonAfterModule),
      std::move(globalDecls))};
  file->srcDocComment = srcDocComment;
  return file;
}

auto Parser::parseFileSearchDirs() -> std::vector<AST::File::SearchDir> {
  auto searchDirs{std::vector<AST::File::SearchDir>{}};
  while (true) {
    skip();
    auto srcLoc0{mSrcLoc};
    auto srcKwSearchDir{nextKeyword("#search_dir")};
    if (!srcKwSearchDir) break;
    if (!mIsSMDL)
      srcLoc0.throwError("'#search_dir' requires the file to begin with "
                         "'#smdl'");
    auto path{parseLiteralStringExpression()};
    if (!path)
      srcLoc0.throwError("expected literal string path after '#search_dir'");
    searchDirs.push_back(
        AST::File::SearchDir{*srcKwSearchDir, std::move(path)});
  }
  return searchDirs;
}

auto Parser::parseFileVersion() -> std::optional<AST::File::Version> {
  auto kwMdl{nextKeywordAndLocation("mdl")};
  if (!kwMdl) return std::nullopt;
  auto srcLoc0{kwMdl->srcLoc};
  skip();
  auto srcLoc1{mSrcLoc};
  auto srcMajor{nextInteger()};
  auto srcDot{next(".")};
  auto srcMinor{nextInteger()};
  if (!srcMajor || !srcDot || !srcMinor)
    srcLoc0.throwError("expected 'X.Y' version after 'mdl'");
  auto parseVersionNumber{[&](std::string_view srcNumber) {
    uint32_t number{};
    if (std::from_chars(srcNumber.data(), srcNumber.data() + srcNumber.size(),
                        number)
            .ec != std::errc())
      srcLoc0.throwError("version number ", Quoted(srcNumber),
                         " is out of range");
    return number;
  }};
  AST::File::Version version{};
  version.srcKwMdl = kwMdl->src;
  version.srcVersion = getSourceCode().substr(srcLoc1.i, mSrcLoc.i - srcLoc1.i);
  version.major = parseVersionNumber(*srcMajor);
  version.minor = parseVersionNumber(*srcMinor);
  auto srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon) srcLoc0.throwError("expected ';' after 'mdl ...'");
  version.srcSemicolon = *srcSemicolon;
  return version;
}

auto Parser::parseImportPath() -> std::optional<AST::ImportPath> {
  checkpoint();
  auto elements{std::vector<AST::ImportPath::Element>{}};
  while (true) {
    checkpoint();
    auto srcDoubleColon{nextDelimiter("::")};
    if (!srcDoubleColon && !elements.empty()) {
      reject();
      break;
    }
    auto element{AST::ImportPath::Element{}};
    if (srcDoubleColon) {
      element.srcDoubleColon = *srcDoubleColon;
    }
    if (auto srcName{nextDelimiter("..")}) {
      element.srcName = *srcName;
    } else if (auto srcName{nextDelimiter(".")}) {
      element.srcName = *srcName;
    } else if (auto srcName{nextDelimiter("*")}) {
      element.srcName = *srcName;
    } else if (auto name{parseSimpleName()}) {
      element.srcName = name->srcName;
    } else if (auto literalString{parseLiteralStringExpression()}) {
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
  auto srcLoc0{checkpoint()};
  auto srcKwUsing{nextKeyword("using")};
  if (!srcKwUsing) {
    reject();
    return nullptr;
  }
  auto name{parseSimpleName()};
  if (!name) {
    reject();
    return nullptr;
  }
  auto srcEqual{nextDelimiter("=")};
  if (!srcEqual) {
    reject();
    return nullptr;
  }
  auto importPath{parseImportPath()};
  if (!importPath)
    srcLoc0.throwError("expected import path after 'using ... ='");
  auto srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon) srcLoc0.throwError("expected ';' after 'using ... = ...'");
  accept();
  return allocate<AST::UsingAlias>(srcLoc0, std::in_place, *srcKwUsing, *name,
                                   *srcEqual, std::move(*importPath),
                                   *srcSemicolon);
}

auto Parser::parseUsingImport() -> BumpPtr<AST::UsingImport> {
  auto srcLoc0{checkpoint()};
  auto srcKwExport{nextKeyword("export")};
  auto srcKwUsing{nextKeyword("using")};
  if (!srcKwUsing) {
    reject();
    return nullptr;
  }
  auto importPath{parseImportPath()};
  if (!importPath) {
    reject();
    return nullptr;
  }
  if (importPath->isImportAll())
    srcLoc0.throwError(
        "import path after '[export] using' must not end with '::*'");
  auto srcKwImport{nextKeyword("import")};
  if (!srcKwImport)
    srcLoc0.throwError("expected 'import' after '[export] using ...'");
  auto names{std::vector<AST::UsingImport::Name>{}};
  if (auto srcStar{nextDelimiter("*")}) {
    names.push_back(AST::UsingImport::Name{*srcStar, {}});
  } else {
    parseCommaSeparated(names, [&]() -> std::optional<AST::UsingImport::Name> {
      auto name{parseSimpleName()};
      if (!name) return std::nullopt;
      return AST::UsingImport::Name{name->srcName, {}};
    });
  }
  auto srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon)
    srcLoc0.throwError("expected ';' after '[export] using ... import ...'");
  accept();
  auto result{allocate<AST::UsingImport>(srcLoc0, std::in_place, *srcKwUsing,
                                         std::move(*importPath), *srcKwImport,
                                         std::move(names), *srcSemicolon)};
  if (srcKwExport) result->srcKwExport = *srcKwExport;
  return result;
}

auto Parser::parseImport() -> BumpPtr<AST::Import> {
  auto kwImport{nextKeywordAndLocation("import")};
  if (!kwImport) return nullptr;
  auto importPathWrappers{std::vector<AST::Import::ImportPathWrapper>{}};
  parseCommaSeparated(importPathWrappers,
                      [&]() -> std::optional<AST::Import::ImportPathWrapper> {
                        auto importPath{parseImportPath()};
                        if (!importPath) return std::nullopt;
                        return AST::Import::ImportPathWrapper{
                            std::move(*importPath), {}};
                      });
  auto srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon)
    kwImport->srcLoc.throwError("expected ';' after 'import ...'");
  return allocate<AST::Import>(kwImport->srcLoc, std::in_place, kwImport->src,
                               std::move(importPathWrappers), *srcSemicolon);
}

auto Parser::parseAttributes() -> std::optional<AST::Decl::Attributes> {
  auto srcAt{nextDelimiterAndLocation("@")};
  if (!srcAt) return std::nullopt;
  auto srcLoc0{srcAt->srcLoc};
  auto attributes{AST::Function::Attributes{}};
  attributes.srcAt = srcAt->src;
  auto srcParenL{nextDelimiter("(")};
  if (!srcParenL)
    srcLoc0.throwError("expected '@(...)' syntax for function attributes");
  attributes.srcParenL = *srcParenL;
  static constexpr std::array<std::string_view, 11> attrNames{
      "alwaysinline", "cold",    "fastmath", "foreign", "hot",    "macro",
      "noinline",     "optnone", "optsize",  "pure",    "visible"};
  while (true) {
    auto attr{[&]() -> std::optional<std::string_view> {
      for (auto attrName : attrNames)
        if (auto srcAttr{nextKeyword(attrName)}) return srcAttr;
      return std::nullopt;
    }()};
    if (attr) {
      attributes.attrs.push_back(*attr);
    } else {
      skip();
      if (peek() != ')') {
        auto srcLocAttr{mSrcLoc};
        if (auto word{nextWord()})
          srcLocAttr.throwError("unrecognized attribute ", Quoted(*word),
                                ", expected one of ", join(attrNames, ", "));
        srcLocAttr.throwError("expected attribute name or ')' after '@('");
      }
      break;
    }
  }
  auto srcParenR{nextDelimiter(")")};
  if (!srcParenR) srcLoc0.throwError("expected '@(...)' syntax for attributes");
  attributes.srcParenR = *srcParenR;
  return std::move(attributes);
}

auto Parser::parseGlobalDeclaration() -> BumpPtr<AST::Decl> {
  auto srcLoc0{checkpoint()};
  // Capture before parsing: comments inside the declaration must not
  // clobber the pending block first.
  auto srcDocComment{getDocCommentBefore(srcLoc0.i)};
  auto attributes{parseAttributes()};
  auto srcKwExport{nextKeyword("export")};
  auto decl{[&]() -> BumpPtr<AST::Decl> {
    if (auto decl{parseAnnotationDeclaration()}) return decl;
    if (auto decl{parseFunctionDeclaration()}) return decl;
    if (auto decl{parseTypeDeclaration()}) return decl;
    if (auto decl{parseVariableDeclaration()}) return decl;
    if (mIsSMDL) {
      if (auto decl{parseExecDeclaration()}) return decl;
      if (auto decl{parseUnitTestDeclaration()}) return decl;
      if (auto decl{parseNamespaceDeclaration()}) return decl;
    }
    return nullptr;
  }()};
  if (!decl) {
    reject();
    if (nextKeyword("using") || nextKeyword("import"))
      srcLoc0.throwError("'using' and 'import' declarations must appear at "
                         "the top of the file");
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
  auto kwAnnotation{nextKeywordAndLocation("annotation")};
  if (!kwAnnotation) return nullptr;
  auto srcLoc0{kwAnnotation->srcLoc};
  auto name{parseSimpleName()};
  if (!name) srcLoc0.throwError("expected simple name after 'annotation'");
  auto params{parseParameterList()};
  if (!params)
    srcLoc0.throwError(
        "expected parameter list after 'annotation' declaration");
  auto annotations{parseAnnotationBlock()};
  auto srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon)
    srcLoc0.throwError("expected ';' after 'annotation' declaration");
  return allocate<AST::AnnotationDecl>(
      srcLoc0, std::in_place, kwAnnotation->src, std::move(*name),
      std::move(*params), std::move(annotations), *srcSemicolon);
}

auto Parser::parseTypeDeclaration() -> BumpPtr<AST::Decl> {
  if (auto decl{parseAliasTypeDeclaration()}) return decl;
  if (auto decl{parseStructTypeDeclaration()}) return decl;
  if (auto decl{parseEnumTypeDeclaration()}) return decl;
  if (mIsSMDL) {
    if (auto decl{parseTagDeclaration()}) return decl;
  }
  return nullptr;
}

auto Parser::parseAliasTypeDeclaration() -> BumpPtr<AST::Typedef> {
  auto kwTypedef{nextKeywordAndLocation("typedef")};
  if (!kwTypedef) return nullptr;
  auto srcLoc0{kwTypedef->srcLoc};
  auto type{parseType()};
  if (!type) srcLoc0.throwError("expected type after 'typedef'");
  auto name{parseSimpleName()};
  if (!name) srcLoc0.throwError("expected name after 'typedef ...'");
  auto srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon) srcLoc0.throwError("expected ';' after 'typedef ...'");
  return allocate<AST::Typedef>(srcLoc0, std::in_place, kwTypedef->src,
                                std::move(type), *name, *srcSemicolon);
}

auto Parser::parseStructTypeDeclaration() -> BumpPtr<AST::Struct> {
  auto srcLoc0{checkpoint()};
  auto srcKwStruct{nextKeyword("struct")};
  if (!srcKwStruct) {
    reject();
    return nullptr;
  }
  auto name{parseSimpleName()};
  if (!name) srcLoc0.throwError("expected name after 'struct'");
  auto tags{std::vector<AST::Struct::Tag>{}};
  auto srcColonBeforeTags{nextDelimiter(":")};
  if (srcColonBeforeTags) {
    parseCommaSeparated(tags, [&]() -> std::optional<AST::Struct::Tag> {
      auto srcLoc1{mSrcLoc};
      auto srcKwDefault{nextKeyword("default")};
      auto tagName{parseIdentifier()};
      if (!tagName) return std::nullopt;
      auto tag{AST::Struct::Tag{}};
      tag.srcKwDefault = orEmpty(srcKwDefault);
      tag.type = allocate<AST::Type>(srcLoc1, std::in_place,
                                     std::vector<std::string_view>(),
                                     std::move(tagName));
      return std::move(tag);
    });
  }
  auto annotations{parseAnnotationBlock()};
  auto srcBraceL{nextDelimiter("{")};
  if (!srcBraceL) srcLoc0.throwError("expected '{' after 'struct ...'");
  auto constructors{std::vector<AST::Struct::Constructor>{}};
  auto fields{std::vector<AST::Struct::Field>{}};
  auto srcKwFinalize{std::optional<std::string_view>{}};
  auto stmtFinalize{BumpPtr<AST::Stmt>{}};
  // Parse constructors, which must appear at the top of the
  // struct declaration. This is an extension!
  while (true) {
    auto constructor{parseStructConstructor()};
    if (!constructor) break;
    if (constructor->name.srcName != name->srcName)
      constructor->name.srcLoc.throwError(
          "constructor must name the containing struct ", Quoted(*name));
    constructors.push_back(std::move(*constructor));
    skip();
    if (peek() == '}') break;
  }
  // Parse fields
  while (true) {
    auto field{parseStructFieldDeclarator()};
    if (!field) {
      // Parse finalize block, which must appear at the bottom of the
      // struct declaration if it appears at all. This is an extension!
      if (srcKwFinalize = nextKeyword("finalize"); srcKwFinalize) {
        if (stmtFinalize = parseCompoundStatement(); !stmtFinalize) {
          srcLoc0.throwError("expected '{ ... }' after 'finalize'");
        }
      }
      break;
    }
    fields.push_back(std::move(*field));
    skip();
    attachPendingTrailingDocComment(fields);
    if (peek() == '}') break;
  }
  auto srcBraceR{nextDelimiter("}")};
  if (!srcBraceR)
    mSrcLoc.throwError("expected field declarator or '}' in 'struct ...'");
  auto srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon)
    srcLoc0.throwError("expected ';' after 'struct ... { ... }'");
  accept();
  return allocate<AST::Struct>(
      srcLoc0, std::in_place, *srcKwStruct, *name, orEmpty(srcColonBeforeTags),
      std::move(tags), std::move(annotations), *srcBraceL,
      std::move(constructors), std::move(fields), orEmpty(srcKwFinalize),
      std::move(stmtFinalize), *srcBraceR, *srcSemicolon);
}

auto Parser::parseStructConstructor()
    -> std::optional<AST::Struct::Constructor> {
  auto srcLoc0{checkpoint()};
  auto name{parseSimpleName()};
  if (!name) {
    reject();
    return std::nullopt;
  }
  skip();
  auto params{parseParameterList()};
  if (!params) {
    reject();
    return std::nullopt;
  }
  auto srcEqual{nextDelimiter("=")};
  if (!srcEqual) {
    reject();
    return std::nullopt;
  }
  auto expr{parseExpression()};
  if (!expr) {
    srcLoc0.throwError("expected expression after '='");
  }
  auto srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon) {
    srcLoc0.throwError("expected ';' after constructor expression");
  }
  accept();
  return AST::Struct::Constructor{std::move(*name), std::move(*params),
                                  *srcEqual, std::move(expr), *srcSemicolon};
}

auto Parser::parseStructFieldDeclarator() -> std::optional<AST::Struct::Field> {
  auto srcLoc0{checkpoint()};
  auto srcDocComment{getDocCommentBefore(srcLoc0.i)};
  auto field{AST::Struct::Field{}};
  auto type{parseType()};
  if (!type) {
    reject();
    return std::nullopt;
  }
  field.srcLoc = srcLoc0;
  field.srcDocComment = srcDocComment;
  field.type = std::move(type);
  auto name{parseSimpleName()};
  if (!name) {
    reject();
    return std::nullopt;
  }
  field.name = *name;
  if (auto srcEqual{nextDelimiter("=")}) {
    auto exprInit{parseExpression()};
    if (!exprInit) mSrcLoc.throwError("expected initializer after '='");
    field.srcEqual = *srcEqual;
    field.exprInit = std::move(exprInit);
  }
  field.annotations = parseAnnotationBlock();
  auto srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon) mSrcLoc.throwError("expected ';' after field declarator");
  field.srcSemicolon = *srcSemicolon;
  accept();
  return std::move(field);
}

auto Parser::parseEnumTypeDeclaration() -> BumpPtr<AST::Enum> {
  auto kwEnum{nextKeywordAndLocation("enum")};
  if (!kwEnum) return nullptr;
  auto srcLoc0{kwEnum->srcLoc};
  auto name{parseSimpleName()};
  if (!name) srcLoc0.throwError("expected name after 'enum'");
  auto annotations{parseAnnotationBlock()};
  auto srcBraceL{nextDelimiter("{")};
  if (!srcBraceL) srcLoc0.throwError("expected '{' after 'enum ...'");
  auto declarators{std::vector<AST::Enum::Declarator>{}};
  parseCommaSeparated(declarators, [&] { return parseEnumValueDeclarator(); });
  auto srcBraceR{nextDelimiter("}")};
  if (!srcBraceR)
    mSrcLoc.throwError("expected value declarator or '}' in 'enum ...'");
  auto srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon) srcLoc0.throwError("expected ';' after 'enum ...'");
  return allocate<AST::Enum>(srcLoc0, std::in_place, kwEnum->src, *name,
                             std::move(annotations), *srcBraceL,
                             std::move(declarators), *srcBraceR, *srcSemicolon);
}

auto Parser::parseEnumValueDeclarator()
    -> std::optional<AST::Enum::Declarator> {
  auto srcLoc0{checkpoint()};
  auto srcDocComment{getDocCommentBefore(srcLoc0.i)};
  auto name{parseSimpleName()};
  if (!name) {
    reject();
    return std::nullopt;
  }
  auto declarator{AST::Enum::Declarator{}};
  declarator.srcLoc = srcLoc0;
  declarator.srcDocComment = srcDocComment;
  declarator.name = *name;
  if (auto srcEqual{nextDelimiter("=")}) {
    auto exprInit{parseAssignmentExpression()};
    if (!exprInit) srcLoc0.throwError("expected initializer after '='");
    declarator.srcEqual = *srcEqual;
    declarator.exprInit = std::move(exprInit);
  }
  declarator.annotations = parseAnnotationBlock();
  accept();
  return std::move(declarator);
}

auto Parser::parseVariableDeclaration() -> BumpPtr<AST::Variable> {
  auto srcLoc0{checkpoint()};
  auto type{parseType()};
  if (!type) {
    reject();
    return nullptr;
  }
  auto declarators{std::vector<AST::Variable::Declarator>{}};
  parseCommaSeparated(declarators, [&] { return parseVariableDeclarator(); });
  if (declarators.empty()) {
    reject();
    return nullptr;
  }
  auto srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon)
    srcLoc0.throwError("expected ';' after variable declaration");
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
  auto srcLoc0{checkpoint()};
  auto declarator{AST::Variable::Declarator{}};
  declarator.srcLoc = srcLoc0;
  declarator.srcDocComment = getDocCommentBefore(srcLoc0.i);
  if (auto name{parseSimpleName()}) {
    declarator.names.push_back(
        AST::Variable::Declarator::DeclaratorName{*name});
  } else if (auto srcBraceL{nextDelimiter("{")}; srcBraceL && mIsSMDL) {
    // Parse destructure syntax `{foo, bar, baz}`
    declarator.srcBraceL = *srcBraceL;
    parseCommaSeparated(
        declarator.names,
        [&]() -> std::optional<AST::Variable::Declarator::DeclaratorName> {
          auto name{parseSimpleName()};
          if (!name) return std::nullopt;
          return AST::Variable::Declarator::DeclaratorName{*name};
        });
    auto srcBraceR{nextDelimiter("}")};
    if (!srcBraceR) {
      reject();
      return std::nullopt;
    }
    declarator.srcBraceR = *srcBraceR;
  } else {
    reject();
    return std::nullopt;
  }
  if (auto srcEqual{nextDelimiter("=")}) {
    auto exprInit{parseAssignmentExpression()};
    if (!exprInit) srcLoc0.throwError("expected initializer after '='");
    declarator.srcEqual = *srcEqual;
    declarator.exprInit = std::move(exprInit);
  } else if (auto argsInit{parseArgumentList()}) {
    declarator.argsInit = std::move(argsInit);
  }
  declarator.annotations = parseAnnotationBlock();
  accept();
  return std::move(declarator);
}

auto Parser::parseFunctionDeclaration() -> BumpPtr<AST::Function> {
  auto srcLoc0{checkpoint()};
  auto type{parseType()};
  if (!type) {
    reject();
    return nullptr;
  }
  auto earlyAnnotations{parseAnnotationBlock()};
  auto name{parseSimpleName()};
  if (!name) {
    reject();
    return nullptr;
  }
  auto params{parseParameterList()};
  if (!params) {
    reject();
    return nullptr;
  }
  auto srcFrequency{nextKeyword({"uniform", "varying"})};
  auto lateAnnotations{parseAnnotationBlock()};
  auto srcEqual{std::optional<std::string_view>()};
  auto definition{BumpPtr<AST::Node>{}};
  auto srcSemicolon{std::optional<std::string_view>()};
  skip();
  if (params->isVariant() && peek() != '=')
    srcLoc0.throwError(
        "function variant must be defined by 'let' or call expression");
  if (srcSemicolon = nextDelimiter(";"); srcSemicolon) {
    // Nothing
  } else if (srcEqual = nextDelimiter("="); srcEqual) {
    skip();
    auto srcLoc1{mSrcLoc};
    auto def{parseExpression()};
    if (!def) srcLoc0.throwError("expected function expression after '='");
    if (srcSemicolon = nextDelimiter(";"); !srcSemicolon)
      srcLoc0.throwError("expected ';' after function expression");
    if (params->isVariant() && !llvm::isa<AST::Let>(def.get()) &&
        !llvm::isa<AST::Call>(def.get()))
      srcLoc0.throwError(
          "function variant definition must be 'let' or call expression");
    definition =
        allocate<AST::Return>(srcLoc1, std::in_place, std::string_view(),
                              std::move(def), std::nullopt, std::string_view());
  } else {
    auto def{parseCompoundStatement()};
    if (!def) srcLoc0.throwError("expected ';' or function definition");
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
  auto kwTag{nextKeywordAndLocation("tag")};
  if (!kwTag) return nullptr;
  auto srcLoc0{kwTag->srcLoc};
  auto name{parseSimpleName()};
  if (!name) srcLoc0.throwError("expected name after 'tag'");
  auto srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon) srcLoc0.throwError("expected ';' after 'tag ...'");
  return allocate<AST::Tag>(srcLoc0, std::in_place, kwTag->src, *name,
                            *srcSemicolon);
}

auto Parser::parseExecDeclaration() -> BumpPtr<AST::Exec> {
  auto kwExec{nextKeywordAndLocation("exec")};
  if (!kwExec) return nullptr;
  auto stmt{parseCompoundStatement()};
  if (!stmt)
    kwExec->srcLoc.throwError("expected compound statement after 'exec'");
  return allocate<AST::Exec>(kwExec->srcLoc, std::in_place, kwExec->src,
                             std::move(stmt));
}

auto Parser::parseUnitTestDeclaration() -> BumpPtr<AST::UnitTest> {
  auto kwUnitTest{nextKeywordAndLocation("unit_test")};
  if (!kwUnitTest) return nullptr;
  auto srcLoc0{kwUnitTest->srcLoc};
  auto name{parseLiteralStringExpression()};
  if (!name) srcLoc0.throwError("expected literal string after 'unit_test'");
  auto stmt{parseCompoundStatement()};
  if (!stmt)
    srcLoc0.throwError("expected compound statement after 'unit_test ...'");
  return allocate<AST::UnitTest>(srcLoc0, std::in_place, kwUnitTest->src,
                                 std::move(name), std::move(stmt));
}

auto Parser::parseNamespaceDeclaration() -> BumpPtr<AST::Namespace> {
  auto kwNamespace{nextKeywordAndLocation("namespace")};
  if (!kwNamespace) return nullptr;
  auto srcLoc0{kwNamespace->srcLoc};
  auto identifier{parseIdentifier()};
  if (!identifier) srcLoc0.throwError("expected identifier after 'namespace'");
  auto srcBraceL{nextDelimiter("{")};
  if (!srcBraceL) srcLoc0.throwError("expected '{' after 'namespace ...'");
  auto decls{std::vector<BumpPtr<AST::Decl>>{}};
  while (true) {
    auto decl{parseGlobalDeclaration()};
    if (!decl) break;
    decls.push_back(std::move(decl));
    skip();
    if (isEOF()) break;
  }
  auto srcBraceR{nextDelimiter("}")};
  if (!srcBraceR)
    mSrcLoc.throwError("expected declaration or '}' in 'namespace ...'");
  return allocate<AST::Namespace>(srcLoc0, std::in_place, kwNamespace->src,
                                  std::move(identifier), *srcBraceL,
                                  std::move(decls), *srcBraceR);
}
//--}

//--{ Parse: Stmt
auto Parser::parseStatement() -> BumpPtr<AST::Stmt> {
  ParseDepthGuard depthGuard{*this};
  skip();
  auto srcLoc0{mSrcLoc};
  if (auto stmt{parseCompoundStatement()}) return stmt;
  if (auto stmt{parseIfStatement()}) return stmt;
  if (auto stmt{parseSwitchStatement()}) return stmt;
  if (auto stmt{parseWhileStatement()}) return stmt;
  if (auto stmt{parseDoStatement()}) return stmt;
  if (auto stmt{parseForStatement()}) return stmt;
  if (auto stmt{parseBreakStatement()}) return stmt;
  if (auto stmt{parseContinueStatement()}) return stmt;
  if (auto stmt{parseReturnStatement()}) return stmt;
  if (mIsSMDL) {
    if (auto stmt{parseUnreachableStatement()}) return stmt;
    if (auto stmt{parsePreserveStatement()}) return stmt;
    if (auto stmt{parseDeferStatement()}) return stmt;
    if (auto stmt{parseVisitStatement()}) return stmt;
  }
  if (auto decl{parseTypeDeclaration()})
    return allocate<AST::DeclStmt>(srcLoc0, std::in_place, std::move(decl));
  if (auto decl{parseVariableDeclaration()})
    return allocate<AST::DeclStmt>(srcLoc0, std::in_place, std::move(decl));
  if (auto srcSemicolon{nextDelimiter(";")})
    return allocate<AST::ExprStmt>(srcLoc0, std::in_place, nullptr,
                                   std::nullopt, *srcSemicolon);
  if (auto expr{parseExpression()}) {
    auto lateIf{parseLateIf()};
    auto srcSemicolon{nextDelimiter(";")};
    if (!srcSemicolon) srcLoc0.throwError("expected ';' after expression");
    return allocate<AST::ExprStmt>(srcLoc0, std::in_place, std::move(expr),
                                   std::move(lateIf), *srcSemicolon);
  }
  return nullptr;
}

auto Parser::parseCompoundStatement() -> BumpPtr<AST::Compound> {
  auto braceL{nextDelimiterAndLocation("{")};
  if (!braceL) return nullptr;
  auto stmts{std::vector<BumpPtr<AST::Stmt>>{}};
  while (true) {
    auto stmt{parseStatement()};
    if (!stmt) break;
    stmts.push_back(std::move(stmt));
    skip();
    if (peek() == '}') break;
  }
  auto srcBraceR{nextDelimiter("}")};
  if (!srcBraceR)
    mSrcLoc.throwError("expected statement or '}' in compound statement "
                       "starting at line ",
                       braceL->srcLoc.lineNo);
  return allocate<AST::Compound>(braceL->srcLoc, std::in_place, braceL->src,
                                 std::move(stmts), *srcBraceR);
}

auto Parser::parseIfStatement() -> BumpPtr<AST::If> {
  auto kwIf{nextKeywordAndLocation("if")};
  if (!kwIf) return nullptr;
  auto srcLoc0{kwIf->srcLoc};
  auto exprCond{parseExpressionInParentheses()};
  if (!exprCond)
    srcLoc0.throwError("expected parenthesized condition after 'if'");
  auto ifPass{parseStatement()};
  if (!ifPass) srcLoc0.throwError("expected statement after 'if (...)'");
  if (auto srcKwElse{nextKeyword("else")}) {
    auto ifFail{parseStatement()};
    if (!ifFail) srcLoc0.throwError("expected statement after 'else'");
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
  auto kwSwitch{nextKeywordAndLocation("switch")};
  if (!kwSwitch) return nullptr;
  auto srcLoc0{kwSwitch->srcLoc};
  auto expr{parseExpressionInParentheses()};
  if (!expr)
    srcLoc0.throwError("expected parenthesized expression after 'switch'");
  auto srcBraceL{nextDelimiter("{")};
  if (!srcBraceL) srcLoc0.throwError("expected opening '{' after 'switch'");
  auto switchCases{std::vector<AST::Switch::Case>{}};
  while (true) {
    auto switchCase{parseSwitchCase()};
    if (!switchCase) break;
    switchCases.push_back(std::move(*switchCase));
    skip();
    if (peek() == '}') break;
  }
  auto srcBraceR{nextDelimiter("}")};
  if (!srcBraceR)
    mSrcLoc.throwError("expected 'case', 'default', or '}' in 'switch'");
  return allocate<AST::Switch>(srcLoc0, std::in_place, kwSwitch->src,
                               std::move(expr), *srcBraceL,
                               std::move(switchCases), *srcBraceR);
}

auto Parser::parseSwitchCase() -> std::optional<AST::Switch::Case> {
  skip();
  auto srcLoc0{mSrcLoc};
  auto switchCase{AST::Switch::Case{}};
  if (auto srcKwCase{nextKeyword("case")}) {
    auto expr{parseExpression()};
    if (!expr) srcLoc0.throwError("expected expression after 'case'");
    auto srcColon{nextDelimiter(":")};
    if (!srcColon) srcLoc0.throwError("expected ':' after 'case ...'");
    switchCase.srcKwCaseOrDefault = *srcKwCase;
    switchCase.expr = std::move(expr);
    switchCase.srcColon = *srcColon;
  } else if (auto srcKwDefault{nextKeyword("default")}) {
    auto srcColon{nextDelimiter(":")};
    if (!srcColon) srcLoc0.throwError("expected ':' after 'default'");
    switchCase.srcKwCaseOrDefault = *srcKwDefault;
    switchCase.srcColon = *srcColon;
  } else {
    return std::nullopt;
  }
  while (true) {
    auto stmt{parseStatement()};
    if (!stmt) break;
    switchCase.stmts.push_back(std::move(stmt));
    skip();
  }
  return std::move(switchCase);
}

auto Parser::parseWhileStatement() -> BumpPtr<AST::While> {
  auto kwWhile{nextKeywordAndLocation("while")};
  if (!kwWhile) return nullptr;
  auto srcLoc0{kwWhile->srcLoc};
  auto expr{parseExpressionInParentheses()};
  if (!expr)
    srcLoc0.throwError("expected parenthesized expression after 'while'");
  auto stmt{parseStatement()};
  if (!stmt) srcLoc0.throwError("expected statement after 'while (...)'");
  return allocate<AST::While>(srcLoc0, std::in_place, kwWhile->src,
                              std::move(expr), std::move(stmt));
}

auto Parser::parseDoStatement() -> BumpPtr<AST::DoWhile> {
  auto kwDo{nextKeywordAndLocation("do")};
  if (!kwDo) return nullptr;
  auto srcLoc0{kwDo->srcLoc};
  auto stmt{parseStatement()};
  if (!stmt) srcLoc0.throwError("expected statement after 'do'");
  auto srcKwWhile{nextKeyword("while")};
  if (!srcKwWhile) srcLoc0.throwError("expected 'while' after 'do ...'");
  auto expr{parseExpressionInParentheses()};
  if (!expr)
    srcLoc0.throwError(
        "expected parenthesized expression after 'do ... while'");
  auto srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon)
    srcLoc0.throwError("expected ';' after 'do ... while (...)'");
  return allocate<AST::DoWhile>(srcLoc0, std::in_place, kwDo->src,
                                std::move(stmt), *srcKwWhile, std::move(expr),
                                *srcSemicolon);
}

auto Parser::parseForStatement() -> BumpPtr<AST::For> {
  auto kwFor{nextKeywordAndLocation("for")};
  if (!kwFor) return nullptr;
  auto srcLoc0{kwFor->srcLoc};
  auto srcParenL{nextDelimiter("(")};
  if (!srcParenL) srcLoc0.throwError("expected '(' after 'for'");
  auto stmtInit{BumpPtr<AST::Stmt>{}};
  if (auto decl{parseVariableDeclaration()}) {
    stmtInit =
        allocate<AST::DeclStmt>(decl->srcLoc, std::in_place, std::move(decl));
  } else if (auto expr{parseExpression()}) {
    auto srcSemicolon{nextDelimiter(";")};
    if (!srcSemicolon) srcLoc0.throwError("expected ';' after expression");
    stmtInit =
        allocate<AST::ExprStmt>(expr->srcLoc, std::in_place, std::move(expr),
                                std::nullopt, *srcSemicolon);
  } else {
    srcLoc0.throwError(
        "expected variable declaration or expression after 'for ('");
  }
  auto exprCond{parseExpression()};
  auto srcSemicolonAfterCond{nextDelimiter(";")};
  if (!srcSemicolonAfterCond)
    srcLoc0.throwError("expected ';' after 'for (... ; ...'");
  auto exprIncr{parseExpression()};
  auto srcParenR{nextDelimiter(")")};
  if (!srcParenR) srcLoc0.throwError("expected ')' after 'for (...'");
  auto stmt{parseStatement()};
  if (!stmt) srcLoc0.throwError("expected statement after 'for (...)'");
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
  auto kwReturn{nextKeywordAndLocation("return")};
  if (!kwReturn) return nullptr;
  auto expr{parseExpression()}; // Allow this to be null!
  auto lateIf{parseLateIf()};
  auto srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon)
    kwReturn->srcLoc.throwError("expected ';' after 'return ...'");
  return allocate<AST::Return>(kwReturn->srcLoc, std::in_place, kwReturn->src,
                               std::move(expr), std::move(lateIf),
                               *srcSemicolon);
}

auto Parser::parseUnreachableStatement() -> BumpPtr<AST::Unreachable> {
  auto kwUnreachable{nextKeywordAndLocation("unreachable")};
  if (!kwUnreachable) return nullptr;
  auto srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon)
    kwUnreachable->srcLoc.throwError("expected ';' after 'unreachable'");
  return allocate<AST::Unreachable>(kwUnreachable->srcLoc, std::in_place,
                                    kwUnreachable->src, *srcSemicolon);
}

auto Parser::parsePreserveStatement() -> BumpPtr<AST::Preserve> {
  auto kwPreserve{nextKeywordAndLocation("preserve")};
  if (!kwPreserve) return nullptr;
  auto exprs{std::vector<AST::Preserve::ExprWrapper>{}};
  parseCommaSeparated(exprs,
                      [&]() -> std::optional<AST::Preserve::ExprWrapper> {
                        auto expr{parseUnaryExpression()};
                        if (!expr) return std::nullopt;
                        return AST::Preserve::ExprWrapper{std::move(expr), {}};
                      });
  auto srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon)
    kwPreserve->srcLoc.throwError("expected ';' after 'preserve ...'");
  return allocate<AST::Preserve>(kwPreserve->srcLoc, std::in_place,
                                 kwPreserve->src, std::move(exprs),
                                 *srcSemicolon);
}

auto Parser::parseDeferStatement() -> BumpPtr<AST::Defer> {
  auto kwDefer{nextKeywordAndLocation("defer")};
  if (!kwDefer) return nullptr;
  auto stmt{parseStatement()};
  if (!stmt) kwDefer->srcLoc.throwError("expected statement after 'defer'");
  return allocate<AST::Defer>(kwDefer->srcLoc, std::in_place, kwDefer->src,
                              std::move(stmt));
}

auto Parser::parseVisitStatement() -> BumpPtr<AST::Visit> {
  auto kwVisit{nextKeywordAndLocation("visit")};
  if (!kwVisit) return nullptr;
  auto srcLoc0{kwVisit->srcLoc};
  auto name{parseSimpleName()};
  if (!name) srcLoc0.throwError("expected name after 'visit'");
  auto srcKwIn{nextKeyword("in")};
  if (!srcKwIn) srcLoc0.throwError("expected 'in' after 'visit ...'");
  auto expr{parseExpression()};
  if (!expr) srcLoc0.throwError("expected expression after 'visit ... in'");
  auto stmt{parseCompoundStatement()};
  if (!stmt)
    srcLoc0.throwError("expected compound statement after 'visit ... in ...'");
  return allocate<AST::Visit>(srcLoc0, std::in_place, kwVisit->src, *name,
                              *srcKwIn, std::move(expr), std::move(stmt));
}

auto Parser::parseLateIf() -> std::optional<AST::LateIf> {
  if (!mIsSMDL) return std::nullopt;
  auto kwIf{nextKeywordAndLocation("if")};
  if (!kwIf) return std::nullopt;
  auto expr{parseExpressionInParentheses()};
  if (!expr)
    kwIf->srcLoc.throwError(
        "expected expression in parentheses after '... if'");
  return AST::LateIf(kwIf->src, std::move(expr));
}
//--}

} // namespace smdl
