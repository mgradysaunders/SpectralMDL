// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#include "smdl/Parser.h"

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ConvertUTF.h"
#include "llvm/Support/Error.h"

namespace smdl {

//--{ Basics
char Parser::peek() const {
  if (isEOF())
    return '\0';
  return getSourceCode()[mSrcLoc.i];
}

char Parser::next() {
  if (isEOF())
    return '\0';
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
  for (size_t i = 0; i < n && !isEOF(); i++)
    next();
  return result;
}

std::optional<std::string_view> Parser::next(std::string_view str) {
  if (startsWith(getRemainingSourceCode(), str))
    return next(str.size());
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
  if (peek() == '$')
    next();
  if (isAlpha(peek()) || peek() == '_') {
    next();
    while (isWord(peek()))
      next();
    accept();
    return getSourceCode().substr(i, mSrcLoc.i - i);
  } else {
    reject();
    return std::nullopt;
  }
}

std::optional<std::string_view> Parser::nextInteger() {
  auto i{mSrcLoc.i};
  while (isDigit(peek()))
    next();
  if (mSrcLoc.i > i) {
    return getSourceCode().substr(i, mSrcLoc.i - i);
  } else {
    return std::nullopt;
  }
}

void Parser::skip() {
  auto skipSome{[&] {
    if (startsWith(getRemainingSourceCode(), "//")) {
      next(2);
      while (!isEOF() && peek() != '\n')
        next(1);
      return true;
    } else if (startsWith(getRemainingSourceCode(), "/*")) {
      next(2);
      while (!isEOF() && !startsWith(getRemainingSourceCode(), "*/"))
        next(1);
      if (isEOF())
        mSrcLoc.throwError("unexpected EOF in multiline comment");
      next(2);
      return true;
    } else if (isSpace(peek())) {
      next(1);
      return true;
    } else {
      return false;
    }
  }};
  while (!isEOF() && skipSome())
    continue;
}
//--}

//--{ Parse: Expr
auto Parser::parseSimpleName() -> std::optional<AST::Name> {
  auto srcLoc0{checkpoint()};
  if (auto name{nextWord()}) {
    static const std::string_view keywords[]{
        "break",   "case",   "cast", "const",   "continue", "default",
        "do",      "else",   "enum", "export",  "false",    "for",
        "if",      "import", "let",  "module",  "package",  "return",
        "struct",  "switch", "true", "typedef", "uniform",  "using",
        "varying", "while",
    };
    for (const auto &keyword : keywords) {
      if (*name == keyword) {
        reject();
        return std::nullopt;
      }
    }
    if (mIsSMDL) {
      static const std::string_view keywordsSmdlSyntax[]{
          "defer",  "inline", "namespace",   "return_from",
          "static", "tag",    "unreachable", "visit",
      };
      for (const auto &keyword : keywordsSmdlSyntax) {
        if (*name == keyword) {
          reject();
          return std::nullopt;
        }
      }
    }
    accept();
    return AST::Name{srcLoc0, *name};
  } else {
    reject();
    return std::nullopt;
  }
}

auto Parser::parseIdentifier() -> BumpPtr<AST::Identifier> {
  auto srcLoc0{checkpoint()};
  auto elements{std::vector<AST::Identifier::Element>{}};
  auto srcDoubleColon{next("::")};
  if (auto name{parseSimpleName()}) {
    elements.push_back(AST::Identifier::Element{
        srcDoubleColon ? *srcDoubleColon : std::string_view(), *name});
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
    if (auto srcQual{
            nextKeyword({"const", "inline", "static", "uniform", "varying"})}) {
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
  param.type = std::move(type);
  param.name = *name;
  if (auto srcEqual{nextDelimiter("=")}) {
    auto exprInit{parseAssignmentExpression()};
    if (!exprInit)
      srcLoc0.throwError("expected initializer after '='");
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
    while (true) {
      skip();
      auto param{parseParameter()};
      if (!param)
        break;
      params.params.push_back(std::move(*param));
      auto srcComma{nextDelimiter(",")};
      if (!srcComma)
        break;
      params.params.back().srcComma = *srcComma;
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
  while (true) {
    skip();
    auto argument{parseArgument()};
    if (!argument)
      break;
    args.args.push_back(std::move(*argument));
    auto srcComma{nextDelimiter(",")};
    if (!srcComma)
      break;
    args.args.back().srcComma = *srcComma;
  }
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
  skip();
  auto srcLoc0{mSrcLoc};
  auto srcDoubleBrackL{nextDelimiter("[[")};
  if (!srcDoubleBrackL)
    return nullptr;
  auto annos{std::vector<AST::Annotation>{}};
  while (true) {
    auto anno{parseAnnotation()};
    if (!anno)
      break;
    annos.push_back(std::move(*anno));
    auto srcComma{nextDelimiter(",")};
    if (!srcComma)
      break;
    annos.back().srcComma = *srcComma;
    skip();
    if (startsWith(getRemainingSourceCode(), "]]"))
      break;
  }
  auto srcDoubleBrackR{nextDelimiter("]]")};
  if (!srcDoubleBrackR)
    srcLoc0.throwError("expected ']]' to close annotation block");
  return allocate<AST::AnnotationBlock>(srcLoc0, std::in_place,
                                        *srcDoubleBrackL, std::move(annos),
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
  if (!srcParenR)
    srcLoc0.throwError("expected closing ')'");
  accept();
  return allocate<AST::Parens>(srcLoc0, std::in_place,
                               srcDollar ? *srcDollar : std::string_view(),
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
  if (!expr)
    return nullptr;
  skip();
  auto srcLoc0{mSrcLoc};
  if (auto srcQuestion{next("?")}) {
    auto exprThen{parseExpression()};
    if (!exprThen)
      srcLoc0.throwError("expected then clause in conditional expression");
    skip();
    auto srcColon{next(":")};
    auto exprElse{srcColon ? parseAssignmentExpression()
                           : BumpPtr<AST::Expr>(nullptr)};
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
  if (auto expr{parsePostfixExpression()})
    return expr;
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
  if (auto expr{parsePrefixExpression()})
    return expr;
  if (auto expr{parseLetExpression()})
    return expr;
  if (mIsSMDL) {
    if (auto expr{parseReturnFromExpression()})
      return expr;
  }
  return nullptr;
}

auto Parser::parsePostfixExpression() -> BumpPtr<AST::Expr> {
  auto expr{parsePrimaryExpression()};
  if (!expr)
    return nullptr;
  auto withPostfix{[&]() -> BumpPtr<AST::Expr> {
    auto srcLoc0{mSrcLoc};
    if (auto srcDot{nextDelimiter(".")}) {
      auto name{parseSimpleName()};
      if (!name)
        srcLoc0.throwError("expected name after '.'");
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
      if (!srcBrackL)
        break;
      if (auto srcAngleL{nextDelimiter("<")}) {
        auto name{parseSimpleName()};
        if (!name)
          srcLoc0.throwError("expected name after '[<'");
        auto srcAngleR{nextDelimiter(">")};
        if (!srcAngleR)
          srcLoc0.throwError("expected '>]'");
        index.expr = allocate<AST::SizeName>(srcLoc0, std::in_place, *srcAngleL,
                                             *name, *srcAngleR);
      } else {
        index.expr = parseExpression(); // This may be null to represent `[]`
      }
      auto srcBrackR{nextDelimiter("]")};
      if (!srcBrackR)
        srcLoc0.throwError("expected ']'");
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
    if (!nextExpr)
      break;
    expr = std::move(nextExpr);
  }
  return expr;
}

auto Parser::parseLetExpression() -> BumpPtr<AST::Expr> {
  skip();
  auto srcLoc0{mSrcLoc};
  auto srcKwLet{nextKeyword("let")};
  if (!srcKwLet)
    return nullptr;
  auto decls{std::vector<BumpPtr<AST::Decl>>{}};
  auto srcBraceL{std::optional<std::string_view>()};
  auto srcBraceR{std::optional<std::string_view>()};
  if (srcBraceL = nextDelimiter("{"); srcBraceL) {
    while (true) {
      auto decl{parseVariableDeclaration()};
      if (!decl)
        break;
      decls.push_back(std::move(decl));
      skip();
      if (peek() == '}')
        break;
    }
    if (srcBraceR = nextDelimiter("}"); !srcBraceR)
      srcLoc0.throwError("expected closing '}' after 'let'");
  } else {
    auto decl{parseVariableDeclaration()};
    if (!decl)
      srcLoc0.throwError("expected variable declaration after 'let'");
    decls.push_back(std::move(decl));
  }
  auto srcKwIn{nextKeyword("in")};
  if (!srcKwIn)
    srcLoc0.throwError("expected 'in' after 'let ...'");
  auto expr{parseConditionalExpression()};
  if (!expr)
    srcLoc0.throwError("expected expression after 'let ... in'");
  return allocate<AST::Let>(
      srcLoc0, std::in_place, *srcKwLet,
      srcBraceL ? *srcBraceL : std::string_view(), std::move(decls),
      srcBraceR ? *srcBraceR : std::string_view(), *srcKwIn, std::move(expr));
}

auto Parser::parseReturnFromExpression() -> BumpPtr<AST::Expr> {
  skip();
  auto srcLoc0{mSrcLoc};
  auto srcKwReturnFrom{nextKeyword("return_from")};
  if (!srcKwReturnFrom)
    return nullptr;
  auto stmt{parseCompoundStatement()};
  if (!stmt)
    srcLoc0.throwError("expected compound statement after 'return_from'");
  return allocate<AST::ReturnFrom>(srcLoc0, std::in_place, *srcKwReturnFrom,
                                   std::move(stmt));
}

auto Parser::parsePrimaryExpression() -> BumpPtr<AST::Expr> {
  if (auto expr{parseExpressionInParentheses()})
    return expr;
  if (auto expr{parseLiteralExpression()})
    return expr;
  if (auto expr{parseIdentifier()})
    return expr;
  auto srcLoc0{mSrcLoc};
  if (auto srcKwCast{nextKeyword("cast")}) {
    auto srcAngleL{nextDelimiter("<")};
    if (!srcAngleL)
      srcLoc0.throwError("expected opening '<' after 'cast'");
    auto type{parseType()};
    if (!type)
      srcLoc0.throwError("expected type after 'cast'");
    auto srcAngleR{nextDelimiter(">")};
    if (!srcAngleR)
      srcLoc0.throwError("expected closing '>' after 'cast'");
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
  if (auto expr{parseLiteralBoolExpression()})
    return expr;
  if (auto expr{parseLiteralStringExpression()})
    return expr;
  if (auto expr{parseLiteralNumberExpression()})
    return expr;
  if (mIsSMDL) {
    skip();
    auto srcLoc0{mSrcLoc};
    if (next("#")) {
      if (!nextWord())
        srcLoc0.throwError("expected intrinsic name after '#'");
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
  if (peek() != '"')
    return nullptr;
  auto str{std::string()};
  auto srcLoc0{mSrcLoc};
  auto appendCodepointAsUTF8{[&](uint32_t codepoint) {
    char result[4]{};
    char *resultPtr{&result[0]};
    if (!llvm::ConvertCodePointToUTF8(codepoint, resultPtr))
      return false;
    str.insert(str.end(), &result[0], resultPtr);
    return true;
  }};
  auto srcValues{std::vector<std::string_view>{}};
  while (nextDelimiter("\"")) {
    while (true) {
      if (isEOF())
        srcLoc0.throwError("unexpected EOF in literal string");
      if (peek() == '\n')
        srcLoc0.throwError("unexpected EOL in literal string");
      if (peek() == '"')
        break;
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
          uint8_t byte = uint8_t(octToInt(ch));
          for (int i{}; i < 2; i++) {
            ch = next();
            if (!isDigit8(ch))
              srcLoc0.throwError("expected 3 octal digits after '\\'");
            byte = (byte << 3) | uint8_t(octToInt(ch));
          }
          str += static_cast<char>(byte); // Could overflow?
        } else if (ch == 'x') {           // hexadecimal
          uint8_t byte{};
          for (int i{}; i < 2; i++) {
            ch = next();
            if (!isDigit16(ch))
              srcLoc0.throwError("expected 2 hexadecimal digits after '\\x'");
            byte = (byte << 4) | uint8_t(hexToInt(ch));
          }
          str += static_cast<char>(byte);
        } else if (ch == 'u') { // unicode 16-bit
          uint32_t codepoint{};
          for (int i{}; i < 4; i++) {
            ch = next();
            if (!isDigit16(ch))
              srcLoc0.throwError("expected 4 hexadecimal digits after '\\u'");
            codepoint = (codepoint << 4) | uint32_t(hexToInt(ch));
          }
          if (!appendCodepointAsUTF8(codepoint))
            srcLoc0.throwError("UTF-8 encoding of '\\u' sequence failed");
        } else if (ch == 'U') { // unicode 32-bit
          uint32_t codepoint{};
          for (int i{}; i < 8; i++) {
            ch = next();
            if (!isDigit16(ch))
              srcLoc0.throwError("expected 8 hexadecimal digits after '\\U'");
            codepoint = (codepoint << 4) | uint32_t(hexToInt(ch));
          }
          if (!appendCodepointAsUTF8(codepoint))
            srcLoc0.throwError("UTF-8 encoding of '\\U' sequence failed");
        } else {
          str += ch;
        }
      }
    }
    if (!nextDelimiter("\""))
      srcLoc0.throwError("expected '\"' to close literal string");
    srcValues.push_back(getSourceCodeBetween(srcLoc0, mSrcLoc));
    skip();
    srcLoc0 = mSrcLoc;
  }
  return allocate<AST::LiteralString>(srcLoc0, std::in_place,
                                      std::move(srcValues), std::move(str));
}

auto Parser::parseLiteralNumberExpression() -> BumpPtr<AST::Expr> {
  skip();
  if (!isDigit(peek()))
    return nullptr;
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
    if (bits > 64)
      srcLoc0.logWarn("integer literal exceeds 64 bits");
    digitsStr = prefix;
    digitsStr += std::string(digits);
    return llvm::APInt(bits, digits, radix);
  }};
  if (auto remaining{getRemainingSourceCode()};
      !startsWith(remaining, "0.") && !startsWith(remaining, "0e") &&
      !startsWith(remaining, "0E") && next("0")) {
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
      return allocate<AST::LiteralInt>(
          srcLoc0, std::in_place, getSourceCodeBetween(srcLoc0, mSrcLoc),
          llvm::APInt(llvm::APInt::getBitsNeeded(digits, 10), digits, 10)
              .getLimitedValue());
    } else {
      llvm::APFloat value(llvm::APFloat::IEEEdouble());
      auto opStatus{
          value.convertFromString(digits, llvm::APFloat::rmNearestTiesToEven)};
      if (!opStatus)
        srcLoc0.throwError("failed to parse floating point literal");
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
    if (auto srcOp{next(to_string(op))})
      return ParsedUnaryOp{*srcOp, op};
  if (mIsSMDL) {
    if (auto srcOp{next(to_string(UNOP_ADDR))})
      return ParsedUnaryOp{*srcOp, UNOP_ADDR};
    if (auto srcOp{next(to_string(UNOP_DEREF))})
      return ParsedUnaryOp{*srcOp, UNOP_DEREF};
    if (auto srcOp{next(to_string(UNOP_MAYBE))})
      return ParsedUnaryOp{*srcOp, UNOP_MAYBE};
  }
  return std::nullopt;
}

auto Parser::parseBinaryOp(Span<const AST::BinaryOp> ops)
    -> std::optional<ParsedBinaryOp> {
  for (auto op : ops) {
    if (!mIsSMDL && isExtendedSyntax(op))
      continue;
    if (op == BINOP_ELSE) {
      if (auto srcOp{nextKeyword(to_string(op))})
        return ParsedBinaryOp{*srcOp, op};
    } else {
      // Don't mistake bit and for logical and.
      if (op == BINOP_AND && startsWith(getRemainingSourceCode(), "&&"))
        continue;
      if (auto srcOp{next(to_string(op))})
        return ParsedBinaryOp{*srcOp, op};
    }
  }
  return std::nullopt;
}
//--}

//--{ Parse: Decl
auto Parser::parseFile() -> BumpPtr<AST::File> {
  skip();
  auto srcLoc0{mSrcLoc};
  auto srcKwSmdlSyntax{nextKeyword("#smdl")};
  if (srcKwSmdlSyntax)
    mIsSMDL = true;
  auto version{parseFileVersion()};
  if (!version && !mIsSMDL)
    srcLoc0.throwError("expected MDL version");
  auto importDecls{std::vector<BumpPtr<AST::Decl>>{}};
  while (true) {
    auto parseAnyImport{[&]() -> BumpPtr<AST::Decl> {
      if (auto decl{parseUsingAlias()})
        return decl;
      if (auto decl{parseUsingImport()})
        return decl;
      if (auto decl{parseImport()})
        return decl;
      return nullptr;
    }};
    auto decl{parseAnyImport()};
    if (!decl)
      break;
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
    if (!decl)
      break;
    globalDecls.push_back(std::move(decl));
    skip();
    if (isEOF())
      break;
  }
  if (!isEOF())
    srcLoc0.throwError("expected EOF (apparently failed to parse everything!)");
  return allocate<AST::File>(
      srcLoc0, std::in_place,
      srcKwSmdlSyntax ? *srcKwSmdlSyntax : std::string_view(),
      std::move(version), std::move(importDecls),
      srcKwModule ? *srcKwModule : std::string_view(),
      std::move(moduleAnnotations),
      srcSemicolonAfterModule ? *srcSemicolonAfterModule : std::string_view(),
      std::move(globalDecls));
}

auto Parser::parseFileVersion() -> std::optional<AST::File::Version> {
  skip();
  auto srcLoc0{mSrcLoc};
  auto srcKwMdl{nextKeyword("mdl")};
  if (!srcKwMdl)
    return std::nullopt;
  skip();
  auto srcLoc1{mSrcLoc};
  auto srcMajor{nextInteger()};
  auto srcDot{next(".")};
  auto srcMinor{nextInteger()};
  if (!srcMajor || !srcDot || !srcMinor)
    srcLoc0.throwError("expected 'X.Y' version after 'mdl'");
  AST::File::Version version{};
  version.srcKwMdl = *srcKwMdl;
  version.srcVersion = getSourceCode().substr(srcLoc1.i, mSrcLoc.i - srcLoc1.i);
  version.major = llvm::APInt(32, *srcMajor, 10).getLimitedValue();
  version.minor = llvm::APInt(32, *srcMinor, 10).getLimitedValue();
  auto srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon)
    srcLoc0.throwError("expected ';' after 'mdl ...'");
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
  if (!srcSemicolon)
    srcLoc0.throwError("expected ';' after 'using ... = ...'");
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
    while (true) {
      auto name{parseSimpleName()};
      if (!name)
        break;
      // srcLoc0.throwError("expected import name");
      names.push_back(AST::UsingImport::Name{name->srcName, {}});
      auto srcComma{nextDelimiter(",")};
      if (!srcComma)
        break;
      names.back().srcComma = *srcComma;
    }
  }
  auto srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon)
    srcLoc0.throwError("expected ';' after '[export] using ... import ...'");
  accept();
  auto result{allocate<AST::UsingImport>(srcLoc0, std::in_place, *srcKwUsing,
                                         std::move(*importPath), *srcKwImport,
                                         std::move(names), *srcSemicolon)};
  if (srcKwExport)
    result->srcKwExport = *srcKwExport;
  return result;
}

auto Parser::parseImport() -> BumpPtr<AST::Import> {
  skip();
  auto srcLoc0{mSrcLoc};
  auto srcKwImport{nextKeyword("import")};
  if (!srcKwImport)
    return nullptr;
  auto importPathWrappers{std::vector<AST::Import::ImportPathWrapper>{}};
  while (true) {
    auto importPath{parseImportPath()};
    if (!importPath)
      break;
    // srcLoc0.throwError("expected import path");
    importPathWrappers.push_back(
        AST::Import::ImportPathWrapper{std::move(*importPath), {}});
    auto srcComma{nextDelimiter(",")};
    if (!srcComma)
      break;
    importPathWrappers.back().srcComma = *srcComma;
  }
  auto srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon)
    srcLoc0.throwError("expected ';' after 'import ...'");
  return allocate<AST::Import>(srcLoc0, std::in_place, *srcKwImport,
                               std::move(importPathWrappers), *srcSemicolon);
}

auto Parser::parseAttributes() -> std::optional<AST::Decl::Attributes> {
  skip();
  auto srcLoc0{mSrcLoc};
  auto srcAt{nextDelimiter("@")};
  if (!srcAt)
    return std::nullopt;
  auto attributes{AST::Function::Attributes{}};
  attributes.srcAt = *srcAt;
  auto srcParenL{nextDelimiter("(")};
  if (!srcParenL)
    srcLoc0.throwError("expected '@(...)' syntax for function attributes");
  attributes.srcParenL = *srcParenL;
  while (true) {
    checkpoint();
    if (auto attr{nextKeyword({"alwaysinline", "cold", "fastmath", "foreign",
                               "hot", "macro", "noinline", "optnone", "optsize",
                               "pure", "visible"})}) {
      accept();
      attributes.attrs.push_back(*attr);
    } else {
      skip();
      if (peek() != ')')
        srcLoc0.throwError("unrecognized attribute");
      reject();
      break;
    }
  }
  auto srcParenR{nextDelimiter(")")};
  if (!srcParenR)
    srcLoc0.throwError("expected '@(...)' syntax for attributes");
  attributes.srcParenR = *srcParenR;
  return std::move(attributes);
}

auto Parser::parseGlobalDeclaration() -> BumpPtr<AST::Decl> {
  auto srcLoc0{checkpoint()};
  auto attributes{parseAttributes()};
  auto srcKwExport{nextKeyword("export")};
  auto decl{[&]() -> BumpPtr<AST::Decl> {
    if (auto decl{parseAnnotationDeclaration()})
      return decl;
    if (auto decl{parseFunctionDeclaration()})
      return decl;
    if (auto decl{parseTypeDeclaration()})
      return decl;
    if (auto decl{parseVariableDeclaration()})
      return decl;
    if (mIsSMDL) {
      if (auto decl{parseExecDeclaration()})
        return decl;
      if (auto decl{parseUnitTestDeclaration()})
        return decl;
      if (auto decl{parseNamespaceDeclaration()})
        return decl;
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
  if (attributes)
    decl->attributes = std::move(attributes);
  if (srcKwExport)
    decl->srcKwExport = *srcKwExport;
  accept();
  return decl;
}

auto Parser::parseAnnotationDeclaration() -> BumpPtr<AST::Decl> {
  skip();
  auto srcLoc0{mSrcLoc};
  auto srcKwAnnotation{nextKeyword("annotation")};
  if (!srcKwAnnotation)
    return nullptr;
  auto name{parseSimpleName()};
  if (!name)
    srcLoc0.throwError("expected simple name after 'annotation'");
  auto params{parseParameterList()};
  if (!params)
    srcLoc0.throwError(
        "expected parameter list after 'annotation' declaration");
  auto annotations{parseAnnotationBlock()};
  auto srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon)
    srcLoc0.throwError("expected ';' after 'annotation' declaration");
  return allocate<AST::AnnotationDecl>(srcLoc0, std::in_place, *srcKwAnnotation,
                                       std::move(*name), std::move(*params),
                                       std::move(annotations), *srcSemicolon);
}

auto Parser::parseTypeDeclaration() -> BumpPtr<AST::Decl> {
  if (auto decl{parseAliasTypeDeclaration()})
    return decl;
  if (auto decl{parseStructTypeDeclaration()})
    return decl;
  if (auto decl{parseEnumTypeDeclaration()})
    return decl;
  if (mIsSMDL) {
    if (auto decl{parseTagDeclaration()})
      return decl;
  }
  return nullptr;
}

auto Parser::parseAliasTypeDeclaration() -> BumpPtr<AST::Typedef> {
  skip();
  auto srcLoc0{mSrcLoc};
  auto srcKwTypedef{nextKeyword("typedef")};
  if (!srcKwTypedef)
    return nullptr;
  auto type{parseType()};
  if (!type)
    srcLoc0.throwError("expected type after 'typedef'");
  auto name{parseSimpleName()};
  if (!name)
    srcLoc0.throwError("expected name after 'typedef ...'");
  auto srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon)
    srcLoc0.throwError("expected ';' after 'typedef ...'");
  return allocate<AST::Typedef>(srcLoc0, std::in_place, *srcKwTypedef,
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
  if (!name)
    srcLoc0.throwError("expected name after 'struct'");
  auto tags{std::vector<AST::Struct::Tag>{}};
  auto srcColonBeforeTags{nextDelimiter(":")};
  if (srcColonBeforeTags) {
    while (true) {
      skip();
      auto srcLoc1{mSrcLoc};
      auto srcKwDefault{nextKeyword("default")};
      auto tagName{parseIdentifier()};
      if (!tagName)
        break;
      auto &tag{tags.emplace_back()};
      if (srcKwDefault)
        tag.srcKwDefault = *srcKwDefault;
      tag.type = allocate<AST::Type>(srcLoc1, std::in_place,
                                     std::vector<std::string_view>(),
                                     std::move(tagName));
      auto srcComma{nextDelimiter(",")};
      if (!srcComma)
        break;
      tag.srcComma = *srcComma;
    }
  }
  auto annotations{parseAnnotationBlock()};
  auto srcBraceL{nextDelimiter("{")};
  if (!srcBraceL)
    srcLoc0.throwError("expected '{' after 'struct ...'");
  auto constructors{std::vector<AST::Struct::Constructor>{}};
  auto fields{std::vector<AST::Struct::Field>{}};
  auto srcKwFinalize{std::optional<std::string_view>{}};
  auto stmtFinalize{BumpPtr<AST::Stmt>{}};
  // Parse constructors, which must appear at the top of the
  // struct declaration. This is an extension!
  while (true) {
    auto constructor{parseStructConstructor()};
    if (!constructor)
      break;
    if (constructor->name.srcName != name->srcName)
      constructor->name.srcLoc.throwError(
          "constructor must name the containing struct ", Quoted(*name));
    constructors.push_back(std::move(*constructor));
    skip();
    if (peek() == '}')
      break;
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
    if (peek() == '}')
      break;
  }
  auto srcBraceR{nextDelimiter("}")};
  if (!srcBraceR)
    srcLoc0.throwError("expected '}' after 'struct ... { ...'");
  auto srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon)
    srcLoc0.throwError("expected ';' after 'struct ... { ... }'");
  accept();
  return allocate<AST::Struct>(
      srcLoc0, std::in_place, *srcKwStruct, *name,
      srcColonBeforeTags ? *srcColonBeforeTags : std::string_view(),
      std::move(tags), std::move(annotations), *srcBraceL,
      std::move(constructors), std::move(fields),
      srcKwFinalize ? *srcKwFinalize : std::string_view(),
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
  auto field{AST::Struct::Field{}};
  auto type{parseType()};
  if (!type) {
    reject();
    return std::nullopt;
  }
  field.srcLoc = srcLoc0;
  field.type = std::move(type);
  auto name{parseSimpleName()};
  if (!name) {
    reject();
    return std::nullopt;
  }
  field.name = *name;
  if (auto srcEqual{nextDelimiter("=")}) {
    auto exprInit{parseExpression()};
    if (!exprInit)
      mSrcLoc.throwError("expected initializer after '='");
    field.srcEqual = *srcEqual;
    field.exprInit = std::move(exprInit);
  }
  field.annotations = parseAnnotationBlock();
  auto srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon)
    mSrcLoc.throwError("expected ';' after field declarator");
  field.srcSemicolon = *srcSemicolon;
  accept();
  return std::move(field);
}

auto Parser::parseEnumTypeDeclaration() -> BumpPtr<AST::Enum> {
  skip();
  auto srcLoc0{mSrcLoc};
  auto srcKwEnum{nextKeyword("enum")};
  if (!srcKwEnum)
    return nullptr;
  auto name{parseSimpleName()};
  if (!name)
    srcLoc0.throwError("expected name after 'enum'");
  auto annotations{parseAnnotationBlock()};
  auto srcBraceL{nextDelimiter("{")};
  if (!srcBraceL)
    srcLoc0.throwError("expected '{' after 'enum ...'");
  auto declarators{std::vector<AST::Enum::Declarator>{}};
  while (true) {
    auto declarator{parseEnumValueDeclarator()};
    if (!declarator)
      break;
    declarators.push_back(std::move(*declarator));
    auto srcComma{nextDelimiter(",")};
    if (!srcComma)
      break;
    declarators.back().srcComma = *srcComma;
  }
  auto srcBraceR{nextDelimiter("}")};
  if (!srcBraceR)
    srcLoc0.throwError("expected '}' after 'enum ... { ...'");
  auto srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon)
    srcLoc0.throwError("expected ';' after 'enum ...'");
  return allocate<AST::Enum>(srcLoc0, std::in_place, *srcKwEnum, *name,
                             std::move(annotations), *srcBraceL,
                             std::move(declarators), *srcBraceR, *srcSemicolon);
}

auto Parser::parseEnumValueDeclarator()
    -> std::optional<AST::Enum::Declarator> {
  auto srcLoc0{checkpoint()};
  auto name{parseSimpleName()};
  if (!name) {
    reject();
    return std::nullopt;
  }
  auto declarator{AST::Enum::Declarator{}};
  declarator.srcLoc = srcLoc0;
  declarator.name = *name;
  if (auto srcEqual{nextDelimiter("=")}) {
    auto exprInit{parseAssignmentExpression()};
    if (!exprInit)
      srcLoc0.throwError("expected initializer after '='");
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
  while (true) {
    auto declarator{parseVariableDeclarator()};
    if (!declarator)
      break;
    declarators.push_back(std::move(*declarator));
    auto srcComma{nextDelimiter(",")};
    if (!srcComma)
      break;
    declarators.back().srcComma = *srcComma;
  }
  if (declarators.empty()) {
    reject();
    return nullptr;
  }
  auto srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon)
    srcLoc0.throwError("expected ';' after variable declaration");
  accept();
  return allocate<AST::Variable>(srcLoc0, std::in_place, std::move(type),
                                 std::move(declarators), *srcSemicolon);
}

auto Parser::parseVariableDeclarator()
    -> std::optional<AST::Variable::Declarator> {
  auto srcLoc0{checkpoint()};
  auto declarator{AST::Variable::Declarator{}};
  declarator.srcLoc = srcLoc0;
  if (auto name{parseSimpleName()}) {
    declarator.names.push_back(
        AST::Variable::Declarator::DeclaratorName{*name});
  } else if (auto srcBraceL{nextDelimiter("{")}; srcBraceL && mIsSMDL) {
    // Parse destructure syntax `{foo, bar, baz}`
    declarator.srcBraceL = *srcBraceL;
    while (true) {
      auto name{parseSimpleName()};
      if (!name)
        break;
      declarator.names.push_back(
          AST::Variable::Declarator::DeclaratorName{*name});
      auto srcComma{nextDelimiter(",")};
      if (!srcComma)
        break;
      declarator.names.back().srcComma = *srcComma;
    }
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
    if (!exprInit)
      srcLoc0.throwError("expected initializer after '='");
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
  checkpoint();
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
    if (!def)
      srcLoc0.throwError("expected function expression after '='");
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
    if (!def)
      srcLoc0.throwError("expected ';' or function definition");
    definition = std::move(def);
  }
  accept();
  return allocate<AST::Function>(
      srcLoc0, std::in_place, std::move(type), std::move(earlyAnnotations),
      *name, std::move(*params),
      srcFrequency ? *srcFrequency : std::string_view(),
      std::move(lateAnnotations), srcEqual ? *srcEqual : std::string_view(),
      std::move(definition), srcSemicolon ? *srcSemicolon : std::string_view());
}

auto Parser::parseTagDeclaration() -> BumpPtr<AST::Tag> {
  skip();
  auto srcLoc0{mSrcLoc};
  auto srcKwTag{nextKeyword("tag")};
  if (!srcKwTag)
    return nullptr;
  auto name{parseSimpleName()};
  if (!name)
    srcLoc0.throwError("expected name after 'tag'");
  auto srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon)
    srcLoc0.throwError("expected ';' after 'tag ...'");
  return allocate<AST::Tag>(srcLoc0, std::in_place, *srcKwTag, *name,
                            *srcSemicolon);
}

auto Parser::parseExecDeclaration() -> BumpPtr<AST::Exec> {
  skip();
  auto srcLoc0{mSrcLoc};
  auto srcKwExec{nextKeyword("exec")};
  if (!srcKwExec)
    return nullptr;
  auto stmt{parseCompoundStatement()};
  if (!stmt)
    srcLoc0.throwError("expected compound statement after 'exec'");
  return allocate<AST::Exec>(srcLoc0, std::in_place, *srcKwExec,
                             std::move(stmt));
}

auto Parser::parseUnitTestDeclaration() -> BumpPtr<AST::UnitTest> {
  skip();
  auto srcLoc0{mSrcLoc};
  auto srcKwUnitTest{nextKeyword("unit_test")};
  if (!srcKwUnitTest)
    return nullptr;
  auto name{parseLiteralStringExpression()};
  if (!name)
    srcLoc0.throwError("expected literal string after 'unit_test'");
  auto stmt{parseCompoundStatement()};
  if (!stmt)
    srcLoc0.throwError("expected compound statement after 'unit_test ...'");
  return allocate<AST::UnitTest>(srcLoc0, std::in_place, *srcKwUnitTest,
                                 std::move(name), std::move(stmt));
}

auto Parser::parseNamespaceDeclaration() -> BumpPtr<AST::Namespace> {
  skip();
  auto srcLoc0{mSrcLoc};
  auto srcKwNamespace{nextKeyword("namespace")};
  if (!srcKwNamespace)
    return nullptr;
  auto identifier{parseIdentifier()};
  if (!identifier)
    srcLoc0.throwError("expected identifier after 'namespace'");
  auto srcBraceL{nextDelimiter("{")};
  if (!srcBraceL)
    srcLoc0.throwError("expected '{' after 'namespace ...'");
  auto decls{std::vector<BumpPtr<AST::Decl>>{}};
  while (true) {
    auto decl{parseGlobalDeclaration()};
    if (!decl)
      break;
    decls.push_back(std::move(decl));
    skip();
    if (isEOF())
      break;
  }
  auto srcBraceR{nextDelimiter("}")};
  if (!srcBraceR)
    srcLoc0.throwError("expected '}' after 'namespace ... { ...'");
  return allocate<AST::Namespace>(srcLoc0, std::in_place, *srcKwNamespace,
                                  std::move(identifier), *srcBraceL,
                                  std::move(decls), *srcBraceR);
}
//--}

//--{ Parse: Stmt
auto Parser::parseStatement() -> BumpPtr<AST::Stmt> {
  skip();
  auto srcLoc0{mSrcLoc};
  if (auto stmt{parseCompoundStatement()})
    return stmt;
  if (auto stmt{parseIfStatement()})
    return stmt;
  if (auto stmt{parseSwitchStatement()})
    return stmt;
  if (auto stmt{parseWhileStatement()})
    return stmt;
  if (auto stmt{parseDoStatement()})
    return stmt;
  if (auto stmt{parseForStatement()})
    return stmt;
  if (auto stmt{parseBreakStatement()})
    return stmt;
  if (auto stmt{parseContinueStatement()})
    return stmt;
  if (auto stmt{parseReturnStatement()})
    return stmt;
  if (mIsSMDL) {
    if (auto stmt{parseUnreachableStatement()})
      return stmt;
    if (auto stmt{parsePreserveStatement()})
      return stmt;
    if (auto stmt{parseDeferStatement()})
      return stmt;
    if (auto stmt{parseVisitStatement()})
      return stmt;
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
    if (!srcSemicolon)
      srcLoc0.throwError("expected ';' after expression");
    return allocate<AST::ExprStmt>(srcLoc0, std::in_place, std::move(expr),
                                   std::move(lateIf), *srcSemicolon);
  }
  return nullptr;
}

auto Parser::parseCompoundStatement() -> BumpPtr<AST::Compound> {
  skip();
  auto srcLoc0{mSrcLoc};
  auto srcBraceL{nextDelimiter("{")};
  if (!srcBraceL)
    return nullptr;
  auto stmts{std::vector<BumpPtr<AST::Stmt>>{}};
  while (true) {
    auto stmt{parseStatement()};
    if (!stmt)
      break;
    stmts.push_back(std::move(stmt));
    skip();
    if (peek() == '}')
      break;
  }
  auto srcBraceR{nextDelimiter("}")};
  if (!srcBraceR)
    srcLoc0.throwError("expected '}' to close compound statement");
  return allocate<AST::Compound>(srcLoc0, std::in_place, *srcBraceL,
                                 std::move(stmts), *srcBraceR);
}

auto Parser::parseIfStatement() -> BumpPtr<AST::If> {
  skip();
  auto srcLoc0{mSrcLoc};
  auto srcKwIf{nextKeyword("if")};
  if (!srcKwIf)
    return nullptr;
  auto exprCond{parseExpressionInParentheses()};
  if (!exprCond)
    srcLoc0.throwError("expected parenthesized condition after 'if'");
  auto ifPass{parseStatement()};
  if (!ifPass)
    srcLoc0.throwError("expected statement after 'if (...)'");
  if (auto srcKwElse{nextKeyword("else")}) {
    auto ifFail{parseStatement()};
    if (!ifFail)
      srcLoc0.throwError("expected statement after 'else'");
    return allocate<AST::If>(srcLoc0, std::in_place, *srcKwIf,
                             std::move(exprCond), std::move(ifPass), *srcKwElse,
                             std::move(ifFail));
  } else {
    return allocate<AST::If>(srcLoc0, std::in_place, *srcKwIf,
                             std::move(exprCond), std::move(ifPass), *srcKwElse,
                             nullptr);
  }
}

auto Parser::parseSwitchStatement() -> BumpPtr<AST::Switch> {
  auto srcLoc0{mSrcLoc};
  auto srcKwSwitch{nextKeyword("switch")};
  if (!srcKwSwitch)
    return nullptr;
  auto expr{parseExpressionInParentheses()};
  if (!expr)
    srcLoc0.throwError("expected parenthesized expression after 'switch'");
  auto srcBraceL{nextDelimiter("{")};
  if (!srcBraceL)
    srcLoc0.throwError("expected opening '{' after 'switch'");
  auto switchCases{std::vector<AST::Switch::Case>{}};
  while (true) {
    auto switchCase{parseSwitchCase()};
    if (!switchCase)
      break;
    switchCases.push_back(std::move(*switchCase));
    skip();
    if (peek() == '}')
      break;
  }
  auto srcBraceR{nextDelimiter("}")};
  if (!srcBraceR)
    srcLoc0.throwError("expected closing '}' after 'switch'");
  return allocate<AST::Switch>(srcLoc0, std::in_place, *srcKwSwitch,
                               std::move(expr), *srcBraceL,
                               std::move(switchCases), *srcBraceR);
}

auto Parser::parseSwitchCase() -> std::optional<AST::Switch::Case> {
  skip();
  auto srcLoc0{mSrcLoc};
  auto switchCase{AST::Switch::Case{}};
  if (auto srcKwCase{nextKeyword("case")}) {
    auto expr{parseExpression()};
    if (!expr)
      srcLoc0.throwError("expected expression after 'case'");
    auto srcColon{nextDelimiter(":")};
    if (!srcColon)
      srcLoc0.throwError("expected ':' after 'case ...'");
    switchCase.srcKwCaseOrDefault = *srcKwCase;
    switchCase.expr = std::move(expr);
    switchCase.srcColon = *srcColon;
  } else if (auto srcKwDefault{nextKeyword("default")}) {
    auto srcColon{nextDelimiter(":")};
    if (!srcColon)
      srcLoc0.throwError("expected ':' after 'default'");
    switchCase.srcKwCaseOrDefault = *srcKwDefault;
    switchCase.srcColon = *srcColon;
  } else {
    return std::nullopt;
  }
  while (true) {
    auto stmt{parseStatement()};
    if (!stmt)
      break;
    switchCase.stmts.push_back(std::move(stmt));
    skip();
  }
  return std::move(switchCase);
}

auto Parser::parseWhileStatement() -> BumpPtr<AST::While> {
  skip();
  auto srcLoc0{mSrcLoc};
  auto srcKwWhile{nextKeyword("while")};
  if (!srcKwWhile)
    return nullptr;
  auto expr{parseExpressionInParentheses()};
  if (!expr)
    srcLoc0.throwError("expected parenthesized expression after 'while'");
  auto stmt{parseStatement()};
  if (!stmt)
    srcLoc0.throwError("expected statement after 'while (...)'");
  return allocate<AST::While>(srcLoc0, std::in_place, *srcKwWhile,
                              std::move(expr), std::move(stmt));
}

auto Parser::parseDoStatement() -> BumpPtr<AST::DoWhile> {
  skip();
  auto srcLoc0{mSrcLoc};
  auto srcKwDo{nextKeyword("do")};
  if (!srcKwDo)
    return nullptr;
  auto stmt{parseStatement()};
  if (!stmt)
    srcLoc0.throwError("expected statement after 'do'");
  auto srcKwWhile{nextKeyword("while")};
  if (!srcKwWhile)
    srcLoc0.throwError("expected 'while' after 'do ...'");
  auto expr{parseExpressionInParentheses()};
  if (!expr)
    srcLoc0.throwError(
        "expected parenthesized expression after 'do ... while'");
  auto srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon)
    srcLoc0.throwError("expected ';' after 'do ... while (...)'");
  return allocate<AST::DoWhile>(srcLoc0, std::in_place, *srcKwDo,
                                std::move(stmt), *srcKwWhile, std::move(expr),
                                *srcSemicolon);
}

auto Parser::parseForStatement() -> BumpPtr<AST::For> {
  skip();
  auto srcLoc0{mSrcLoc};
  auto srcKwFor{nextKeyword("for")};
  if (!srcKwFor)
    return nullptr;
  auto srcParenL{nextDelimiter("(")};
  if (!srcParenL)
    srcLoc0.throwError("expected '(' after 'for'");
  auto stmtInit{BumpPtr<AST::Stmt>{}};
  if (auto decl{parseVariableDeclaration()}) {
    stmtInit =
        allocate<AST::DeclStmt>(decl->srcLoc, std::in_place, std::move(decl));
  } else if (auto expr{parseExpression()}) {
    auto srcSemicolon{nextDelimiter(";")};
    if (!srcSemicolon)
      srcLoc0.throwError("expected ';' after expression");
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
  if (!srcParenR)
    srcLoc0.throwError("expected ')' after 'for (...'");
  auto stmt{parseStatement()};
  if (!stmt)
    srcLoc0.throwError("expected statement after 'for (...)'");
  return allocate<AST::For>(srcLoc0, std::in_place, *srcKwFor, *srcParenL,
                            std::move(stmtInit), std::move(exprCond),
                            *srcSemicolonAfterCond, std::move(exprIncr),
                            *srcParenR, std::move(stmt));
}

auto Parser::parseBreakStatement() -> BumpPtr<AST::Break> {
  skip();
  auto srcLoc0{mSrcLoc};
  auto srcKwBreak{nextKeyword("break")};
  if (!srcKwBreak)
    return nullptr;
  auto lateIf{parseLateIf()};
  auto srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon)
    srcLoc0.throwError("expected ';' after 'break'");
  return allocate<AST::Break>(srcLoc0, std::in_place, *srcKwBreak,
                              std::move(lateIf), *srcSemicolon);
}

auto Parser::parseContinueStatement() -> BumpPtr<AST::Continue> {
  skip();
  auto srcLoc0{mSrcLoc};
  auto srcKwContinue{nextKeyword("continue")};
  if (!srcKwContinue)
    return nullptr;
  auto lateIf{parseLateIf()};
  auto srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon)
    srcLoc0.throwError("expected ';' after 'continue'");
  return allocate<AST::Continue>(srcLoc0, std::in_place, *srcKwContinue,
                                 std::move(lateIf), *srcSemicolon);
}

auto Parser::parseReturnStatement() -> BumpPtr<AST::Return> {
  skip();
  auto srcLoc0{mSrcLoc};
  auto srcKwReturn{nextKeyword("return")};
  if (!srcKwReturn)
    return nullptr;
  auto expr{parseExpression()}; // Allow this to be null!
  auto lateIf{parseLateIf()};
  auto srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon)
    srcLoc0.throwError("expected ';' after 'return ...'");
  return allocate<AST::Return>(srcLoc0, std::in_place, *srcKwReturn,
                               std::move(expr), std::move(lateIf),
                               *srcSemicolon);
}

auto Parser::parseUnreachableStatement() -> BumpPtr<AST::Unreachable> {
  skip();
  auto srcLoc0{mSrcLoc};
  auto srcKwUnreachable{nextKeyword("unreachable")};
  if (!srcKwUnreachable)
    return nullptr;
  auto srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon)
    srcLoc0.throwError("expected ';' after 'unreachable'");
  return allocate<AST::Unreachable>(srcLoc0, std::in_place, *srcKwUnreachable,
                                    *srcSemicolon);
}

auto Parser::parsePreserveStatement() -> BumpPtr<AST::Preserve> {
  auto srcLoc0{mSrcLoc};
  auto srcKwPreserve{nextKeyword("preserve")};
  if (!srcKwPreserve)
    return nullptr;
  auto exprs{std::vector<AST::Preserve::ExprWrapper>{}};
  while (true) {
    auto expr{parseUnaryExpression()};
    if (!expr)
      break;
    exprs.push_back(AST::Preserve::ExprWrapper{std::move(expr), {}});
    auto srcComma{nextDelimiter(",")};
    if (!srcComma)
      break;
    exprs.back().srcComma = *srcComma;
  }
  auto srcSemicolon{nextDelimiter(";")};
  if (!srcSemicolon)
    srcLoc0.throwError("expected ';' after 'preserve ...'");
  return allocate<AST::Preserve>(srcLoc0, std::in_place, *srcKwPreserve,
                                 std::move(exprs), *srcSemicolon);
}

auto Parser::parseDeferStatement() -> BumpPtr<AST::Defer> {
  skip();
  auto srcLoc0{mSrcLoc};
  auto srcKwDefer{nextKeyword("defer")};
  if (!srcKwDefer)
    return nullptr;
  auto stmt{parseStatement()};
  if (!stmt)
    srcLoc0.throwError("expected statement after 'defer'");
  return allocate<AST::Defer>(srcLoc0, std::in_place, *srcKwDefer,
                              std::move(stmt));
}

auto Parser::parseVisitStatement() -> BumpPtr<AST::Visit> {
  skip();
  auto srcLoc0{mSrcLoc};
  auto srcKwVisit{nextKeyword("visit")};
  if (!srcKwVisit)
    return nullptr;
  auto name{parseSimpleName()};
  if (!name)
    srcLoc0.throwError("expected name after 'visit'");
  auto srcKwIn{nextKeyword("in")};
  if (!srcKwIn)
    srcLoc0.throwError("expected 'in' after 'visit ...'");
  auto expr{parseExpression()};
  if (!expr)
    srcLoc0.throwError("expected expression after 'visit ... in'");
  auto stmt{parseCompoundStatement()};
  if (!stmt)
    srcLoc0.throwError("expected compound statement after 'visit ... in ...'");
  return allocate<AST::Visit>(srcLoc0, std::in_place, *srcKwVisit, *name,
                              *srcKwIn, std::move(expr), std::move(stmt));
}

auto Parser::parseLateIf() -> std::optional<AST::LateIf> {
  if (!mIsSMDL)
    return std::nullopt;
  skip();
  auto srcLoc0{mSrcLoc};
  auto srcKwIf{nextKeyword("if")};
  if (!srcKwIf)
    return std::nullopt;
  auto expr{parseExpressionInParentheses()};
  if (!expr)
    srcLoc0.throwError("expected expression in parentheses after '... if'");
  return AST::LateIf(*srcKwIf, std::move(expr));
}
//--}

} // namespace smdl
