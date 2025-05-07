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
  if (is_eof())
    return '\0';
  return get_source_code()[srcLoc.i];
}

char Parser::next() {
  if (is_eof())
    return '\0';
  auto ch{peek()};
  if (ch == '\n') {
    srcLoc.lineNo++;
    srcLoc.charNo = 1;
  } else {
    srcLoc.charNo++;
  }
  srcLoc.i++;
  return ch;
}

std::string_view Parser::next(size_t n) {
  auto result{get_remaining_source_code().substr(0, n)};
  for (size_t i = 0; i < n && !is_eof(); i++)
    next();
  return result;
}

std::optional<std::string_view> Parser::next(std::string_view str) {
  if (starts_with(get_remaining_source_code(), str))
    return next(str.size());
  return std::nullopt;
}

std::optional<std::string_view> Parser::next_keyword(std::string_view str) {
  checkpoint();
  auto result{next(str)};
  if (!result || is_word(peek())) {
    reject();
    return std::nullopt;
  } else {
    accept();
    return result;
  }
}

std::optional<std::string_view> Parser::next_word() {
  checkpoint();
  auto i{srcLoc.i};
  if (peek() == '$')
    next();
  if (is_alpha(peek()) || peek() == '_') {
    next();
    while (is_word(peek()))
      next();
    accept();
    return get_source_code().substr(i, srcLoc.i - i);
  } else {
    reject();
    return std::nullopt;
  }
}

std::optional<std::string_view> Parser::next_integer() {
  auto i{srcLoc.i};
  while (is_digit(peek()))
    next();
  if (srcLoc.i > i) {
    return get_source_code().substr(i, srcLoc.i - i);
  } else {
    return std::nullopt;
  }
}

void Parser::skip() {
  auto skipSome{[&] {
    if (starts_with(get_remaining_source_code(), "//")) {
      next(2);
      while (!is_eof() && peek() != '\n')
        next(1);
      return true;
    } else if (starts_with(get_remaining_source_code(), "/*")) {
      next(2);
      while (!is_eof() && !starts_with(get_remaining_source_code(), "*/"))
        next(1);
      if (is_eof())
        srcLoc.throw_error("unexpected EOF in multiline comment");
      next(2);
      return true;
    } else if (is_space(peek())) {
      next(1);
      return true;
    } else {
      return false;
    }
  }};
  while (!is_eof() && skipSome())
    continue;
}
//--}

//--{ Parse: Expr
auto Parser::parse_simple_name() -> std::optional<AST::Name> {
  auto srcLoc0{checkpoint()};
  if (auto name{next_word()}) {
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
    if (isSmdl) {
      static const std::string_view keywordsSmdlSyntax[]{
          "defer", "inline",      "return_from", "static",
          "tag",   "unreachable", "visit",
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

auto Parser::parse_identifier() -> BumpPtr<AST::Identifier> {
  auto srcLoc0{checkpoint()};
  auto elements{std::vector<AST::Identifier::Element>{}};
  auto srcDoubleColon{next("::")};
  if (auto name{parse_simple_name()}) {
    elements.push_back(AST::Identifier::Element{
        srcDoubleColon ? *srcDoubleColon : std::string_view(), *name});
  } else {
    if (srcDoubleColon) {
      srcLoc0.throw_error("expected name after '::'");
    } else {
      reject();
      return nullptr;
    }
  }
  while (true) {
    checkpoint();
    if ((srcDoubleColon = next("::"))) {
      if (auto name{parse_simple_name()}) {
        elements.push_back(AST::Identifier::Element{*srcDoubleColon, *name});
        accept();
        continue;
      }
    }
    reject();
    break;
  }
  if (srcLoc.i > srcLoc0.i) {
    accept();
    return allocate<AST::Identifier>(srcLoc0, std::in_place,
                                     std::move(elements));
  } else {
    reject();
    return nullptr;
  }
}

auto Parser::parse_type() -> BumpPtr<AST::Type> {
  auto srcLoc0{checkpoint()};
  auto srcQuals{std::vector<std::string_view>()};
  while (true) {
    checkpoint();
    if (auto srcQual{next_keyword(
            {"const", "inline", "static", "uniform", "varying"})}) {
      accept();
      srcQuals.push_back(*srcQual);
    } else {
      reject();
      break;
    }
  }
  auto expr{parse_unary_expression()};
  if (!expr) {
    reject();
    return nullptr;
  }
  accept();
  return allocate<AST::Type>(srcLoc0, std::in_place, std::move(srcQuals),
                             std::move(expr));
}

auto Parser::parse_parameter() -> std::optional<AST::Parameter> {
  auto srcLoc0{checkpoint()};
  auto type{parse_type()};
  if (!type) {
    reject();
    return std::nullopt;
  }
  auto name{parse_simple_name()};
  if (!name) {
    reject();
    return std::nullopt;
  }
  auto param{AST::Parameter{}};
  param.type = std::move(type);
  param.name = *name;
  if (auto srcEqual{next_delimiter("=")}) {
    auto exprInit{parse_assignment_expression()};
    if (!exprInit)
      srcLoc0.throw_error("expected initializer after '='");
    param.srcEqual = *srcEqual;
    param.exprInit = std::move(exprInit);
  }
  param.annotations = parse_annotation_block();
  accept();
  return std::move(param);
}

auto Parser::parse_parameter_list() -> std::optional<AST::ParameterList> {
  auto srcLoc0{checkpoint()};
  auto params{AST::ParameterList{}};
  auto srcParenL{next_delimiter("(")};
  if (!srcParenL) {
    reject();
    return std::nullopt;
  }
  params.srcParenL = *srcParenL;
  skip();
  if (auto srcStar{next_delimiter("*")}) {
    params.srcStar = *srcStar;
  } else {
    while (true) {
      skip();
      auto param{parse_parameter()};
      if (!param)
        break;
      params.params.push_back(std::move(*param));
      auto srcComma{next_delimiter(",")};
      if (!srcComma)
        break;
      params.params.back().srcComma = *srcComma;
    }
  }
  auto srcParenR{next_delimiter(")")};
  if (!srcParenR) {
    reject();
    return std::nullopt;
  }
  params.srcParenR = *srcParenR;
  accept();
  return std::move(params);
}

auto Parser::parse_argument() -> std::optional<AST::Argument> {
  auto srcLoc0{checkpoint()};
  auto argument{AST::Argument{}};
  argument.srcLoc = srcLoc0;
  if (isSmdl) {
    if (auto srcKwVisit{next_keyword("visit")}) {
      argument.srcKwVisit = *srcKwVisit;
    }
  }
  argument.name = [&]() -> AST::Name {
    checkpoint();
    if (auto name{parse_simple_name()}) {
      if (auto srcColon{next_delimiter(":")};
          srcColon && peek() != ':' && peek() != '=') {
        argument.srcColonAfterName = *srcColon;
        accept();
        return *name;
      }
    }
    reject();
    return {};
  }();
  argument.expr = parse_assignment_expression();
  if (!argument.expr) {
    reject();
    return std::nullopt;
  }
  argument.src = get_source_code_between(srcLoc0, srcLoc);
  accept();
  return std::move(argument);
}

auto Parser::parse_argument_list() -> std::optional<AST::ArgumentList> {
  auto srcLoc0{checkpoint()};
  auto args{AST::ArgumentList{}};
  args.srcLoc = srcLoc0;
  auto srcParenL{next_delimiter("(")};
  if (!srcParenL) {
    reject();
    return std::nullopt;
  }
  args.srcParenL = *srcParenL;
  while (true) {
    skip();
    auto argument{parse_argument()};
    if (!argument)
      break;
    args.args.push_back(std::move(*argument));
    auto srcComma{next_delimiter(",")};
    if (!srcComma)
      break;
    args.args.back().srcComma = *srcComma;
  }
  auto srcParenR{next_delimiter(")")};
  if (!srcParenR) {
    reject();
    return std::nullopt;
  }
  args.srcParenR = *srcParenR;
  accept();
  return std::move(args);
}

auto Parser::parse_annotation() -> std::optional<AST::Annotation> {
  checkpoint();
  auto identifier{parse_identifier()};
  if (!identifier) {
    reject();
    return std::nullopt;
  }
  auto args{parse_argument_list()};
  if (!args) {
    reject();
    return std::nullopt;
  }
  accept();
  return AST::Annotation{std::move(identifier), std::move(*args)};
}

auto Parser::parse_annotation_block() -> BumpPtr<AST::AnnotationBlock> {
  skip();
  auto srcLoc0{srcLoc};
  auto srcDoubleBrackL{next_delimiter("[[")};
  if (!srcDoubleBrackL)
    return nullptr;
  auto annotations{std::vector<AST::Annotation>{}};
  while (true) {
    auto annotation{parse_annotation()};
    if (!annotation)
      break;
    annotations.push_back(std::move(*annotation));
    auto srcComma{next_delimiter(",")};
    if (!srcComma)
      break;
    annotations.back().srcComma = *srcComma;
    skip();
    if (starts_with(get_remaining_source_code(), "]]"))
      break;
  }
  auto srcDoubleBrackR{next_delimiter("]]")};
  if (!srcDoubleBrackR)
    srcLoc0.throw_error("expected ']]' to close annotation block");
  return allocate<AST::AnnotationBlock>(
      srcLoc0, std::in_place, *srcDoubleBrackL, std::move(annotations),
      *srcDoubleBrackR);
}

auto Parser::parse_expression_in_parentheses() -> BumpPtr<AST::Expr> {
  auto srcLoc0{checkpoint()};
  auto srcDollar{next_delimiter("$")};
  auto srcParenL{next_delimiter("(")};
  if (!srcParenL) {
    reject();
    return nullptr;
  }
  auto expr{parse_expression()};
  if (!expr) {
    reject();
    return nullptr;
  }
  auto srcParenR{next_delimiter(")")};
  if (!srcParenR)
    srcLoc0.throw_error("expected closing ')'");
  accept();
  return allocate<AST::Parens>(srcLoc0, std::in_place,
                               srcDollar ? *srcDollar : std::string_view(),
                               *srcParenL, std::move(expr), *srcParenR);
}

auto Parser::parse_expression() -> BumpPtr<AST::Expr> {
  return parse_binary_left_associative(
      {BINOP_COMMA}, [&] { return parse_assignment_expression(); });
}

auto Parser::parse_assignment_expression() -> BumpPtr<AST::Expr> {
  return parse_binary_right_associative(
      {BINOP_LET, //
       BINOP_EQ_LSHR, BINOP_EQ_ADD, BINOP_EQ_SUB, BINOP_EQ_MUL, BINOP_EQ_DIV,
       BINOP_EQ_REM, BINOP_EQ_SHL, BINOP_EQ_ASHR, BINOP_EQ_AND, BINOP_EQ_OR,
       BINOP_EQ_XOR, BINOP_EQ},
      [&] { return parse_conditional_expression(); });
}

auto Parser::parse_conditional_expression() -> BumpPtr<AST::Expr> {
  auto expr{parse_logical_or_expression()};
  if (!expr)
    return nullptr;
  skip();
  auto srcLoc0{srcLoc};
  if (auto srcQuestion{next("?")}) {
    auto exprThen{parse_expression()};
    if (!exprThen)
      srcLoc0.throw_error("expected then clause in conditional expression");
    skip();
    auto srcColon{next(":")};
    auto exprElse{srcColon ? parse_assignment_expression()
                           : BumpPtr<AST::Expr>(nullptr)};
    if (!exprElse)
      srcLoc0.throw_error("expected else clause in conditional expression");
    expr = allocate<AST::Select>(srcLoc0, std::in_place, std::move(expr),
                                 *srcQuestion, std::move(exprThen), *srcColon,
                                 std::move(exprElse));
  }
  return expr;
}

auto Parser::parse_logical_or_expression() -> BumpPtr<AST::Expr> {
  return parse_binary_left_associative(
      {BINOP_LOGIC_OR}, [&] { return parse_logical_and_expression(); });
}

auto Parser::parse_logical_and_expression() -> BumpPtr<AST::Expr> {
  return parse_binary_left_associative(
      {BINOP_LOGIC_AND}, [&] { return parse_inclusive_or_expression(); });
}

auto Parser::parse_inclusive_or_expression() -> BumpPtr<AST::Expr> {
  return parse_binary_left_associative(
      {BINOP_OR}, [&] { return parse_exclusive_or_expression(); });
}

auto Parser::parse_exclusive_or_expression() -> BumpPtr<AST::Expr> {
  return parse_binary_left_associative({BINOP_XOR},
                                       [&] { return parse_and_expression(); });
}

auto Parser::parse_and_expression() -> BumpPtr<AST::Expr> {
  return parse_binary_left_associative(
      {BINOP_AND}, [&] { return parse_equality_expression(); });
}

auto Parser::parse_equality_expression() -> BumpPtr<AST::Expr> {
  return parse_binary_left_associative({BINOP_CMP_EQ, BINOP_CMP_NE}, [&] {
    return parse_relational_expression();
  });
}

auto Parser::parse_relational_expression() -> BumpPtr<AST::Expr> {
  return parse_binary_left_associative(
      {BINOP_SUBSET, BINOP_CMP_LE, BINOP_CMP_GE, BINOP_CMP_LT, BINOP_CMP_GT},
      [&] { return parse_shift_expression(); });
}

auto Parser::parse_shift_expression() -> BumpPtr<AST::Expr> {
  return parse_binary_left_associative(
      {BINOP_LSHR, BINOP_SHL, BINOP_ASHR},
      [&] { return parse_additive_expression(); });
}

auto Parser::parse_additive_expression() -> BumpPtr<AST::Expr> {
  return parse_binary_left_associative({BINOP_ADD, BINOP_SUB}, [&] {
    return parse_multiplicative_expression();
  });
}

auto Parser::parse_multiplicative_expression() -> BumpPtr<AST::Expr> {
  return parse_binary_left_associative({BINOP_MUL, BINOP_DIV, BINOP_REM}, [&] {
    return parse_unary_expression();
  });
}

auto Parser::parse_unary_expression() -> BumpPtr<AST::Expr> {
  if (auto expr{parse_postfix_expression()})
    return expr;
  auto parse_prefix_expression{[&]() -> BumpPtr<AST::Expr> {
    auto srcLoc0{checkpoint()};
    auto op{parse_unary_op()};
    if (!op) {
      reject();
      return nullptr;
    }
    auto expr{parse_unary_expression()};
    if (!expr) {
      reject();
      return nullptr;
    }
    accept();
    expr = allocate<AST::Unary>(srcLoc0, std::in_place, op->srcOp, op->op,
                                std::move(expr));
    return expr;
  }};
  if (auto expr{parse_prefix_expression()})
    return expr;
  if (auto expr{parse_let_expression()})
    return expr;
  if (isSmdl) {
    if (auto expr{parse_return_from_expression()})
      return expr;
  }
  return nullptr;
}

auto Parser::parse_postfix_expression() -> BumpPtr<AST::Expr> {
  auto expr{parse_primary_expression()};
  if (!expr)
    return nullptr;
  auto withPostfix{[&]() -> BumpPtr<AST::Expr> {
    auto srcLoc0{srcLoc};
    if (auto srcDot{next_delimiter(".")}) {
      auto name{parse_simple_name()};
      if (!name)
        srcLoc0.throw_error("expected name after '.'");
      return allocate<AST::AccessField>(srcLoc0, std::in_place, std::move(expr),
                                        *srcDot, *name);
    }
    if (auto srcOp{next_delimiter("++")})
      return allocate<AST::Unary>(srcLoc0, std::in_place, *srcOp,
                                  UNOP_POSTFIX_INC, std::move(expr));
    if (auto srcOp{next_delimiter("--")})
      return allocate<AST::Unary>(srcLoc0, std::in_place, *srcOp,
                                  UNOP_POSTFIX_DEC, std::move(expr));
    if (auto args{parse_argument_list()})
      return allocate<AST::Call>(srcLoc0, std::in_place, std::move(expr),
                                 std::move(*args));
    auto indexes{std::vector<AST::AccessIndex::Index>{}};
    while (!starts_with(get_remaining_source_code(), "[[")) {
      auto index{AST::AccessIndex::Index{}};
      auto srcBrackL{next_delimiter("[")};
      if (!srcBrackL)
        break;
      if (auto srcAngleL{next_delimiter("<")}) {
        auto name{parse_simple_name()};
        if (!name)
          srcLoc0.throw_error("expected name after '[<'");
        auto srcAngleR{next_delimiter(">")};
        if (!srcAngleR)
          srcLoc0.throw_error("expected '>]'");
        index.expr = allocate<AST::SizeName>(srcLoc0, std::in_place, *srcAngleL,
                                             *name, *srcAngleR);
      } else {
        index.expr = parse_expression(); // This may be null to represent `[]`
      }
      auto srcBrackR{next_delimiter("]")};
      if (!srcBrackR)
        srcLoc0.throw_error("expected ']'");
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

auto Parser::parse_let_expression() -> BumpPtr<AST::Expr> {
  skip();
  auto srcLoc0{srcLoc};
  auto srcKwLet{next_keyword("let")};
  if (!srcKwLet)
    return nullptr;
  auto decls{std::vector<BumpPtr<AST::Decl>>{}};
  auto srcBraceL{std::optional<std::string_view>()};
  auto srcBraceR{std::optional<std::string_view>()};
  if ((srcBraceL = next_delimiter("{"))) {
    while (true) {
      auto decl{parse_variable_declaration()};
      if (!decl)
        break;
      decls.push_back(std::move(decl));
      skip();
      if (peek() == '}')
        break;
    }
    if (!(srcBraceR = next_delimiter("}")))
      srcLoc0.throw_error("expected closing '}' after 'let'");
  } else {
    auto decl{parse_variable_declaration()};
    if (!decl)
      srcLoc0.throw_error("expected variable declaration after 'let'");
    decls.push_back(std::move(decl));
  }
  auto srcKwIn{next_keyword("in")};
  if (!srcKwIn)
    srcLoc0.throw_error("expected 'in' after 'let ...'");
  auto expr{parse_conditional_expression()};
  if (!expr)
    srcLoc0.throw_error("expected expression after 'let ... in'");
  return allocate<AST::Let>(
      srcLoc0, std::in_place, *srcKwLet,
      srcBraceL ? *srcBraceL : std::string_view(), std::move(decls),
      srcBraceR ? *srcBraceR : std::string_view(), *srcKwIn, std::move(expr));
}

auto Parser::parse_return_from_expression() -> BumpPtr<AST::Expr> {
  skip();
  auto srcLoc0{srcLoc};
  auto srcKwReturnFrom{next_keyword("return_from")};
  if (!srcKwReturnFrom)
    return nullptr;
  auto stmt{parse_compound_statement()};
  if (!stmt)
    srcLoc0.throw_error("expected compound statement after 'return_from'");
  return allocate<AST::ReturnFrom>(srcLoc0, std::in_place, *srcKwReturnFrom,
                                   std::move(stmt));
}

auto Parser::parse_primary_expression() -> BumpPtr<AST::Expr> {
  if (auto expr{parse_expression_in_parentheses()})
    return expr;
  if (auto expr{parse_literal_expression()})
    return expr;
  if (auto expr{parse_identifier()})
    return expr;
  auto srcLoc0{srcLoc};
  if (auto srcKwCast{next_keyword("cast")}) {
    auto srcAngleL{next_delimiter("<")};
    if (!srcAngleL)
      srcLoc0.throw_error("expected opening '<' after 'cast'");
    auto type{parse_type()};
    if (!type)
      srcLoc0.throw_error("expected type after 'cast'");
    auto srcAngleR{next_delimiter(">")};
    if (!srcAngleR)
      srcLoc0.throw_error("expected closing '>' after 'cast'");
    auto expr{parse_expression_in_parentheses()};
    if (!expr)
      srcLoc0.throw_error(
          "expected parenthesized expression after 'cast<...>'");
    return allocate<AST::TypeCast>(srcLoc0, std::in_place, *srcKwCast,
                                   *srcAngleL, std::move(type), *srcAngleR,
                                   std::move(expr));
  }
  return nullptr;
}

auto Parser::parse_literal_expression() -> BumpPtr<AST::Expr> {
  if (auto expr{parse_literal_bool_expression()})
    return expr;
  if (auto expr{parse_literal_string_expression()})
    return expr;
  if (auto expr{parse_literal_number_expression()})
    return expr;
  if (isSmdl) {
    skip();
    auto srcLoc0{srcLoc};
    if (next("#")) {
      if (!next_word())
        srcLoc0.throw_error("expected intrinsic name after '#'");
      return allocate<AST::Intrinsic>(
          srcLoc0, std::in_place,
          get_source_code().substr(srcLoc0.i, srcLoc.i - srcLoc0.i));
    }
  }
  return nullptr;
}

auto Parser::parse_literal_bool_expression() -> BumpPtr<AST::LiteralBool> {
  skip();
  auto srcLoc0{srcLoc};
  if (auto srcValue{next_keyword("true")})
    return allocate<AST::LiteralBool>(srcLoc0, std::in_place, *srcValue, true);
  if (auto srcValue{next_keyword("false")})
    return allocate<AST::LiteralBool>(srcLoc0, std::in_place, *srcValue, false);
  return nullptr;
}

auto Parser::parse_literal_string_expression() -> BumpPtr<AST::LiteralString> {
  skip();
  if (peek() != '"')
    return nullptr;
  auto str{std::string()};
  auto srcLoc0{srcLoc};
  auto appendCodepointAsUTF8{[&](uint32_t codepoint) {
    char result[4]{};
    char *resultPtr{&result[0]};
    if (!llvm::ConvertCodePointToUTF8(codepoint, resultPtr))
      return false;
    str.insert(str.end(), &result[0], resultPtr);
    return true;
  }};
  while (next_delimiter("\"")) {
    while (true) {
      if (is_eof())
        srcLoc0.throw_error("unexpected EOF in literal string");
      if (peek() == '\n')
        srcLoc0.throw_error("unexpected EOL in literal string");
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
        } else if (is_digit_8(ch)) { // octal
          uint8_t byte = uint8_t(oct_to_int(ch));
          for (int i{}; i < 2; i++) {
            ch = next();
            if (!is_digit_8(ch))
              srcLoc0.throw_error("expected 3 octal digits after '\\'");
            byte = (byte << 3) | uint8_t(oct_to_int(ch));
          }
          str += static_cast<char>(byte); // Could overflow?
        } else if (ch == 'x') {           // hexadecimal
          uint8_t byte{};
          for (int i{}; i < 2; i++) {
            ch = next();
            if (!is_digit_16(ch))
              srcLoc0.throw_error("expected 2 hexadecimal digits after '\\x'");
            byte = (byte << 4) | uint8_t(hex_to_int(ch));
          }
          str += static_cast<char>(byte);
        } else if (ch == 'u') { // unicode 16-bit
          uint32_t codepoint{};
          for (int i{}; i < 4; i++) {
            ch = next();
            if (!is_digit_16(ch))
              srcLoc0.throw_error("expected 4 hexadecimal digits after '\\u'");
            codepoint = (codepoint << 4) | uint32_t(hex_to_int(ch));
          }
          if (!appendCodepointAsUTF8(codepoint))
            srcLoc0.throw_error("UTF-8 encoding of '\\u' sequence failed");
        } else if (ch == 'U') { // unicode 32-bit
          uint32_t codepoint{};
          for (int i{}; i < 8; i++) {
            ch = next();
            if (!is_digit_16(ch))
              srcLoc0.throw_error("expected 8 hexadecimal digits after '\\U'");
            codepoint = (codepoint << 4) | uint32_t(hex_to_int(ch));
          }
          if (!appendCodepointAsUTF8(codepoint))
            srcLoc0.throw_error("UTF-8 encoding of '\\U' sequence failed");
        } else {
          str += ch;
        }
      }
    }
    if (!next_delimiter("\""))
      srcLoc0.throw_error("expected '\"' to close literal string");
  }
  return allocate<AST::LiteralString>(srcLoc0, std::in_place,
                                      get_source_code_between(srcLoc0, srcLoc),
                                      std::move(str));
}

auto Parser::parse_literal_number_expression() -> BumpPtr<AST::Expr> {
  skip();
  if (!is_digit(peek()))
    return nullptr;
  auto srcLoc0{srcLoc};
  auto parseDigits{[&](auto &&isDigit) {
    std::string digits{};
    while (isDigit(peek())) {
      digits.push_back(peek());
      next();
      if (next("'")) { // UNOP_MAYBE consume single-quote separator
        if (peek() == '\'')
          srcLoc0.throw_error("numeric literal must not contain adjacent "
                              "single-quote separators");
        if (!is_digit(peek()))
          srcLoc0.throw_error("numeric literal must not be terminated by "
                              "single-quote separator");
      }
    }
    return digits;
  }};
  auto parseIntWithPrefix{[&](auto &&isDigit, int radix, const char *prefix,
                              const char *info, std::string &digitsStr) {
    if (!isDigit(peek()))
      srcLoc0.throw_error(concat("expected literal prefix '", prefix,
                                 "' to be followed by ", info));
    auto digits{parseDigits(is_digit)};
    auto bits{llvm::APInt::getBitsNeeded(digits, radix)};
    if (bits > 64)
      srcLoc0.log_warn("integer literal exceeds 64 bits");
    digitsStr = prefix;
    digitsStr += std::string(digits);
    return llvm::APInt(bits, digits, radix);
  }};
  if (auto remaining{get_remaining_source_code()};
      !starts_with(remaining, "0.") && !starts_with(remaining, "0e") &&
      !starts_with(remaining, "0E") && next("0")) {
    llvm::APInt value{64, 0};
    std::string digits{};
    if (is_digit_8(peek())) {
      value = parseIntWithPrefix(is_digit_8, 8, "0", "[0-7]", digits);
    } else if (next("b") || next("B")) {
      value = parseIntWithPrefix(is_digit_2, 2, "0b", "[0-1]", digits);
    } else if (next("x") || next("X")) {
      value = parseIntWithPrefix(is_digit_16, 16, "0x", "[0-9a-fA-F]", digits);
    } else {
      digits = "0";
    }
    return allocate<AST::LiteralInt>(srcLoc0, std::in_place,
                                     get_source_code_between(srcLoc0, srcLoc),
                                     value.getLimitedValue());
  } else {
    bool isInt{true};
    auto digits{parseDigits(is_digit)};
    if (next(".")) {
      digits += '.';
      digits += parseDigits(is_digit);
      isInt = false;
    }
    if (next("e") || next("E")) {
      digits += 'e';
      if (next("+"))
        digits += '+';
      else if (next("-"))
        digits += '-';
      if (!is_digit(peek()))
        srcLoc0.throw_error(
            "expected exponent after 'e' in floating point literal");
      digits += parseDigits(is_digit);
      isInt = false;
    }
    if (next("d") || next("D") || next("f") || next("F")) {
      isInt = false;
    }
    if (isInt) {
      return allocate<AST::LiteralInt>(
          srcLoc0, std::in_place, get_source_code_between(srcLoc0, srcLoc),
          llvm::APInt(llvm::APInt::getBitsNeeded(digits, 10), digits, 10)
              .getLimitedValue());
    } else {
      llvm::APFloat value(llvm::APFloat::IEEEdouble());
      auto opStatus{
          value.convertFromString(digits, llvm::APFloat::rmNearestTiesToEven)};
      if (!opStatus)
        srcLoc0.throw_error("failed to parse floating point literal");
      return allocate<AST::LiteralFloat>(
          srcLoc0, std::in_place, get_source_code_between(srcLoc0, srcLoc),
          value.convertToDouble());
    }
  }
  return nullptr;
}

auto Parser::parse_unary_op() -> std::optional<ParsedUnaryOp> {
  for (auto op : std::array{UNOP_INC, UNOP_DEC, UNOP_POS, UNOP_NEG, UNOP_NOT,
                            UNOP_LOGIC_NOT})
    if (auto srcOp{next(to_string(op))})
      return ParsedUnaryOp{*srcOp, op};
  if (isSmdl) {
    if (auto srcOp{next(to_string(UNOP_ADDR))})
      return ParsedUnaryOp{*srcOp, UNOP_ADDR};
    if (auto srcOp{next(to_string(UNOP_DEREF))})
      return ParsedUnaryOp{*srcOp, UNOP_DEREF};
    if (auto srcOp{next(to_string(UNOP_MAYBE))})
      return ParsedUnaryOp{*srcOp, UNOP_MAYBE};
  }
  return std::nullopt;
}

auto Parser::parse_binary_op(Span<AST::BinaryOp> ops)
    -> std::optional<ParsedBinaryOp> {
  for (auto op : ops) {
    // Don't process ":=" or "<:" unless in extended syntax mode.
    if ((op == BINOP_LET || op == BINOP_SUBSET) && !isSmdl)
      continue;
    // Don't mistake bit and for logical and.
    if (op == BINOP_AND && starts_with(get_remaining_source_code(), "&&"))
      continue;
    if (auto srcOp{next(to_string(op))})
      return ParsedBinaryOp{*srcOp, op};
  }
  return std::nullopt;
}
//--}

//--{ Parse: Decl
auto Parser::parse_file() -> BumpPtr<AST::File> {
  skip();
  auto srcLoc0{srcLoc};
  auto srcKwSmdlSyntax{next_keyword("#smdl")};
  if (srcKwSmdlSyntax)
    isSmdl = true;
  auto version{parse_file_version()};
  if (!version && !isSmdl)
    srcLoc0.throw_error("expected MDL version");
  auto importDecls{std::vector<BumpPtr<AST::Decl>>{}};
  while (true) {
    auto parse_any_import{[&]() -> BumpPtr<AST::Decl> {
      if (auto decl{parse_using_alias()})
        return decl;
      if (auto decl{parse_using_import()})
        return decl;
      if (auto decl{parse_import()})
        return decl;
      return nullptr;
    }};
    auto decl{parse_any_import()};
    if (!decl)
      break;
    importDecls.push_back(std::move(decl));
  }
  auto srcKwModule{next_keyword("module")};
  auto moduleAnnotations{BumpPtr<AST::AnnotationBlock>{}};
  auto srcSemicolonAfterModule{std::optional<std::string_view>()};
  if (srcKwModule) {
    moduleAnnotations = parse_annotation_block();
    if (!moduleAnnotations)
      srcLoc0.throw_error("expected annotation block after 'module'");
    srcSemicolonAfterModule = next_delimiter(";");
    if (!srcSemicolonAfterModule)
      srcLoc0.throw_error("expected ';' after 'module [[ ... ]]'");
  }
  auto globalDecls{std::vector<BumpPtr<AST::Decl>>{}};
  while (true) {
    auto decl{parse_global_declaration()};
    if (!decl)
      break;
    globalDecls.push_back(std::move(decl));
    skip();
    if (is_eof())
      break;
  }
  if (!is_eof())
    srcLoc0.throw_error(
        "expected EOF (apparently failed to parse everything!)");
  return allocate<AST::File>(
      srcLoc0, std::in_place,
      srcKwSmdlSyntax ? *srcKwSmdlSyntax : std::string_view(),
      std::move(version), std::move(importDecls),
      srcKwModule ? *srcKwModule : std::string_view(),
      std::move(moduleAnnotations),
      srcSemicolonAfterModule ? *srcSemicolonAfterModule : std::string_view(),
      std::move(globalDecls));
}

auto Parser::parse_file_version() -> std::optional<AST::File::Version> {
  skip();
  auto srcLoc0{srcLoc};
  auto srcKwMdl{next_keyword("mdl")};
  if (!srcKwMdl)
    return std::nullopt;
  skip();
  auto srcLoc1{srcLoc};
  auto srcMajor{next_integer()};
  auto srcDot{next(".")};
  auto srcMinor{next_integer()};
  if (!srcMajor || !srcDot || !srcMinor)
    srcLoc0.throw_error("expected 'X.Y' version after 'mdl'");
  AST::File::Version version{};
  version.srcKwMdl = *srcKwMdl;
  version.srcVersion =
      get_source_code().substr(srcLoc1.i, srcLoc.i - srcLoc1.i);
  version.major = llvm::APInt(32, *srcMajor, 10).getLimitedValue();
  version.minor = llvm::APInt(32, *srcMinor, 10).getLimitedValue();
  auto srcSemicolon{next_delimiter(";")};
  if (!srcSemicolon)
    srcLoc0.throw_error("expected ';' after 'mdl ...'");
  version.srcSemicolon = *srcSemicolon;
  return version;
}

auto Parser::parse_import_path() -> std::optional<AST::ImportPath> {
  auto srcLoc0{checkpoint()};
  auto elements{std::vector<AST::ImportPath::Element>{}};
  while (true) {
    checkpoint();
    auto srcDoubleColon{next_delimiter("::")};
    if (!srcDoubleColon && !elements.empty()) {
      reject();
      break;
    }
    auto element{AST::ImportPath::Element{}};
    if (srcDoubleColon) {
      element.srcDoubleColon = *srcDoubleColon;
    }
    if (auto srcName{next_delimiter("..")}) {
      element.srcName = *srcName;
    } else if (auto srcName{next_delimiter(".")}) {
      element.srcName = *srcName;
    } else if (auto srcName{next_delimiter("*")}) {
      element.srcName = *srcName;
    } else if (auto name{parse_simple_name()}) {
      element.srcName = name->srcName;
    } else if (auto literalString{parse_literal_string_expression()}) {
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

auto Parser::parse_using_alias() -> BumpPtr<AST::UsingAlias> {
  auto srcLoc0{checkpoint()};
  auto srcKwUsing{next_keyword("using")};
  if (!srcKwUsing) {
    reject();
    return nullptr;
  }
  auto name{parse_simple_name()};
  if (!name) {
    reject();
    return nullptr;
  }
  auto srcEqual{next_delimiter("=")};
  if (!srcEqual) {
    reject();
    return nullptr;
  }
  auto importPath{parse_import_path()};
  if (!importPath)
    srcLoc0.throw_error("expected import path after 'using ... ='");
  auto srcSemicolon{next_delimiter(";")};
  if (!srcSemicolon)
    srcLoc0.throw_error("expected ';' after 'using ... = ...'");
  accept();
  return allocate<AST::UsingAlias>(srcLoc0, std::in_place, *srcKwUsing, *name,
                                   *srcEqual, std::move(*importPath),
                                   *srcSemicolon);
}

auto Parser::parse_using_import() -> BumpPtr<AST::UsingImport> {
  auto srcLoc0{checkpoint()};
  auto srcKwExport{next_keyword("export")};
  auto srcKwUsing{next_keyword("using")};
  if (!srcKwUsing) {
    reject();
    return nullptr;
  }
  auto importPath{parse_import_path()};
  if (!importPath) {
    reject();
    return nullptr;
  }
  if (importPath->is_import_all())
    srcLoc0.throw_error(
        "import path after '[export] using' must not end with '::*'");
  auto srcKwImport{next_keyword("import")};
  if (!srcKwImport)
    srcLoc0.throw_error("expected 'import' after '[export] using ...'");
  auto names{std::vector<AST::UsingImport::Name>{}};
  if (auto srcStar{next_delimiter("*")}) {
    names.push_back(AST::UsingImport::Name{*srcStar, {}});
  } else {
    while (true) {
      auto name{parse_simple_name()};
      if (!name)
        break;
        // srcLoc0.throw_error("expected import name");
      names.push_back(AST::UsingImport::Name{name->srcName, {}});
      auto srcComma{next_delimiter(",")};
      if (!srcComma)
        break;
      names.back().srcComma = *srcComma;
    }
  }
  auto srcSemicolon{next_delimiter(";")};
  if (!srcSemicolon)
    srcLoc0.throw_error("expected ';' after '[export] using ... import ...'");
  accept();
  auto result{allocate<AST::UsingImport>(srcLoc0, std::in_place, *srcKwUsing,
                                         std::move(*importPath), *srcKwImport,
                                         std::move(names), *srcSemicolon)};
  if (srcKwExport)
    result->srcKwExport = *srcKwExport;
  return result;
}

auto Parser::parse_import() -> BumpPtr<AST::Import> {
  skip();
  auto srcLoc0{srcLoc};
  auto srcKwImport{next_keyword("import")};
  if (!srcKwImport)
    return nullptr;
  auto importPathWrappers{std::vector<AST::Import::ImportPathWrapper>{}};
  while (true) {
    auto importPath{parse_import_path()};
    if (!importPath)
      break;
      // srcLoc0.throw_error("expected import path");
    importPathWrappers.push_back(
        AST::Import::ImportPathWrapper{std::move(*importPath), {}});
    auto srcComma{next_delimiter(",")};
    if (!srcComma)
      break;
    importPathWrappers.back().srcComma = *srcComma;
  }
  auto srcSemicolon{next_delimiter(";")};
  if (!srcSemicolon)
    srcLoc0.throw_error("expected ';' after 'import ...'");
  return allocate<AST::Import>(srcLoc0, std::in_place, *srcKwImport,
                               std::move(importPathWrappers), *srcSemicolon);
}

auto Parser::parse_global_declaration() -> BumpPtr<AST::Decl> {
  auto srcLoc0{checkpoint()};
  auto srcKwExport{next_keyword("export")};
  auto decl{[&]() -> BumpPtr<AST::Decl> {
    if (auto decl{parse_function_declaration()})
      return decl;
    if (auto decl{parse_type_declaration()})
      return decl;
    if (auto decl{parse_variable_declaration()})
      return decl;
    if (isSmdl) {
      if (auto decl{parse_unit_test_declaration()})
        return decl;
    }
    return nullptr;
  }()};
  if (!decl) {
    reject();
    if (next_keyword("using") || next_keyword("import"))
      srcLoc0.throw_error("'using' and 'import' declarations must appear at "
                          "the top of the file");
    return nullptr;
  }
  decl->isGlobal = true;
  if (srcKwExport)
    decl->srcKwExport = *srcKwExport;
  accept();
  return decl;
}

auto Parser::parse_type_declaration() -> BumpPtr<AST::Decl> {
  if (auto decl{parse_alias_type_declaration()})
    return decl;
  if (auto decl{parse_struct_type_declaration()})
    return decl;
  if (auto decl{parse_enum_type_declaration()})
    return decl;
  if (isSmdl) {
    if (auto decl{parse_tag_declaration()})
      return decl;
  }
  return nullptr;
}

auto Parser::parse_alias_type_declaration() -> BumpPtr<AST::Typedef> {
  skip();
  auto srcLoc0{srcLoc};
  auto srcKwTypedef{next_keyword("typedef")};
  if (!srcKwTypedef)
    return nullptr;
  auto type{parse_type()};
  if (!type)
    srcLoc0.throw_error("expected type after 'typedef'");
  auto name{parse_simple_name()};
  if (!name)
    srcLoc0.throw_error("expected name after 'typedef ...'");
  auto srcSemicolon{next_delimiter(";")};
  if (!srcSemicolon)
    srcLoc0.throw_error("expected ';' after 'typedef ...'");
  return allocate<AST::Typedef>(srcLoc0, std::in_place, *srcKwTypedef,
                                std::move(type), *name, *srcSemicolon);
}

auto Parser::parse_struct_type_declaration() -> BumpPtr<AST::Struct> {
  auto srcLoc0{checkpoint()};
  auto srcKwStruct{next_keyword("struct")};
  if (!srcKwStruct) {
    reject();
    return nullptr;
  }
  auto name{parse_simple_name()};
  if (!name)
    srcLoc0.throw_error("expected name after 'struct'");
  auto tags{std::vector<AST::Struct::Tag>{}};
  auto srcColonBeforeTags{next_delimiter(":")};
  if (srcColonBeforeTags) {
    while (true) {
      skip();
      auto srcLoc1{srcLoc};
      auto srcKwDefault{next_keyword("default")};
      auto tagName{parse_identifier()};
      if (!tagName)
        break;
      auto &tag{tags.emplace_back()};
      if (srcKwDefault)
        tag.srcKwDefault = *srcKwDefault;
      tag.type = allocate<AST::Type>(srcLoc1, std::in_place,
                                     std::vector<std::string_view>(),
                                     std::move(tagName));
      auto srcComma{next_delimiter(",")};
      if (!srcComma)
        break;
      tag.srcComma = *srcComma;
    }
  }
  auto annotations{parse_annotation_block()};
  auto srcBraceL{next_delimiter("{")};
  if (!srcBraceL)
    srcLoc0.throw_error("expected '{' after 'struct ...'");
  auto fields{std::vector<AST::Struct::Field>{}};
  auto srcKwFinalize{std::optional<std::string_view>{}};
  auto stmtFinalize{BumpPtr<AST::Stmt>{}};
  while (true) {
    auto field{parse_struct_field_declarator()};
    if (!field) {
      if (srcKwFinalize = next_keyword("finalize"); srcKwFinalize) {
        if (stmtFinalize = parse_compound_statement(); !stmtFinalize) {
          srcLoc0.throw_error("expected '{ ... }' after 'finalize'");
        }
      }
      break;
    }
    fields.push_back(std::move(*field));
    skip();
    if (peek() == '}')
      break;
  }
  auto srcBraceR{next_delimiter("}")};
  if (!srcBraceR)
    srcLoc0.throw_error("expected '}' after 'struct ... { ...'");
  auto srcSemicolon{next_delimiter(";")};
  if (!srcSemicolon)
    srcLoc0.throw_error("expected ';' after 'struct ... { ... }'");
  accept();
  return allocate<AST::Struct>(
      srcLoc0, std::in_place, *srcKwStruct, *name,
      srcColonBeforeTags ? *srcColonBeforeTags : std::string_view(),
      std::move(tags), std::move(annotations), *srcBraceL, std::move(fields),
      srcKwFinalize ? *srcKwFinalize : std::string_view(),
      std::move(stmtFinalize), *srcBraceR, *srcSemicolon);
}

auto Parser::parse_struct_field_declarator()
    -> std::optional<AST::Struct::Field> {
  auto srcLoc0{checkpoint()};
  auto field{AST::Struct::Field{}};
  auto type{parse_type()};
  if (!type) {
    reject();
    return std::nullopt;
  }
  field.type = std::move(type);
  auto name{parse_simple_name()};
  if (!name) {
    reject();
    return std::nullopt;
  }
  field.name = *name;
  if (auto srcEqual{next_delimiter("=")}) {
    auto exprInit{parse_expression()};
    if (!exprInit)
      srcLoc.throw_error("expected initializer after '='");
    field.srcEqual = *srcEqual;
    field.exprInit = std::move(exprInit);
  }
  field.annotations = parse_annotation_block();
  auto srcSemicolon{next_delimiter(";")};
  if (!srcSemicolon)
    srcLoc.throw_error("expected ';' after field declarator");
  field.srcSemicolon = *srcSemicolon;
  accept();
  return std::move(field);
}

auto Parser::parse_enum_type_declaration() -> BumpPtr<AST::Enum> {
  skip();
  auto srcLoc0{srcLoc};
  auto srcKwEnum{next_keyword("enum")};
  if (!srcKwEnum)
    return nullptr;
  auto name{parse_simple_name()};
  if (!name)
    srcLoc0.throw_error("expected name after 'enum'");
  auto annotations{parse_annotation_block()};
  auto srcBraceL{next_delimiter("{")};
  if (!srcBraceL)
    srcLoc0.throw_error("expected '{' after 'enum ...'");
  auto declarators{std::vector<AST::Enum::Declarator>{}};
  while (true) {
    auto declarator{parse_enum_value_declarator()};
    if (!declarator)
      break;
    declarators.push_back(std::move(*declarator));
    auto srcComma{next_delimiter(",")};
    if (!srcComma)
      break;
    declarators.back().srcComma = *srcComma;
  }
  auto srcBraceR{next_delimiter("}")};
  if (!srcBraceR)
    srcLoc0.throw_error("expected '}' after 'enum ... { ...'");
  auto srcSemicolon{next_delimiter(";")};
  if (!srcSemicolon)
    srcLoc0.throw_error("expected ';' after 'enum ...'");
  return allocate<AST::Enum>(srcLoc0, std::in_place, *srcKwEnum, *name,
                             std::move(annotations), *srcBraceL,
                             std::move(declarators), *srcBraceR, *srcSemicolon);
}

auto Parser::parse_enum_value_declarator()
    -> std::optional<AST::Enum::Declarator> {
  auto srcLoc0{checkpoint()};
  auto name{parse_simple_name()};
  if (!name) {
    reject();
    return std::nullopt;
  }
  auto declarator{AST::Enum::Declarator{}};
  declarator.name = *name;
  if (auto srcEqual{next_delimiter("=")}) {
    auto exprInit{parse_assignment_expression()};
    if (!exprInit)
      srcLoc0.throw_error("expected initializer after '='");
    declarator.srcEqual = *srcEqual;
    declarator.exprInit = std::move(exprInit);
  }
  declarator.annotations = parse_annotation_block();
  accept();
  return std::move(declarator);
}

auto Parser::parse_variable_declaration() -> BumpPtr<AST::Variable> {
  auto srcLoc0{checkpoint()};
  auto type{parse_type()};
  if (!type) {
    reject();
    return nullptr;
  }
  auto declarators{std::vector<AST::Variable::Declarator>{}};
  while (true) {
    auto declarator{parse_variable_declarator()};
    if (!declarator)
      break;
    declarators.push_back(std::move(*declarator));
    auto srcComma{next_delimiter(",")};
    if (!srcComma)
      break;
    declarators.back().srcComma = *srcComma;
  }
  if (declarators.empty()) {
    reject();
    return nullptr;
  }
  auto srcSemicolon{next_delimiter(";")};
  if (!srcSemicolon)
    srcLoc0.throw_error("expected ';' after variable declaration");
  accept();
  return allocate<AST::Variable>(srcLoc0, std::in_place, std::move(type),
                                 std::move(declarators), *srcSemicolon);
}

auto Parser::parse_variable_declarator()
    -> std::optional<AST::Variable::Declarator> {
  auto srcLoc0{checkpoint()};
  auto name{parse_simple_name()};
  if (!name) {
    reject();
    return std::nullopt;
  }
  auto declarator{AST::Variable::Declarator{}};
  declarator.name = *name;
  if (auto srcEqual{next_delimiter("=")}) {
    auto exprInit{parse_assignment_expression()};
    if (!exprInit)
      srcLoc0.throw_error("expected initializer after '='");
    declarator.srcEqual = *srcEqual;
    declarator.exprInit = std::move(exprInit);
  } else if (auto argsInit{parse_argument_list()}) {
    declarator.argsInit = std::move(argsInit);
  }
  declarator.annotations = parse_annotation_block();
  accept();
  return std::move(declarator);
}

auto Parser::parse_function_declaration_attributes()
    -> std::optional<AST::Function::Attributes> {
  skip();
  auto srcLoc0{srcLoc};
  auto srcAt{next_delimiter("@")};
  if (!srcAt)
    return std::nullopt;
  auto attributes{AST::Function::Attributes{}};
  attributes.srcAt = *srcAt;
  auto srcParenL{next_delimiter("(")};
  if (!srcParenL)
    srcLoc0.throw_error("expected '@(...)' syntax for function attributes");
  attributes.srcParenL = *srcParenL;
  while (true) {
    checkpoint();
    if (auto attr{
            next_keyword({"alwaysinline", "cold", "hot", "macro", "noinline",
                          "optnone", "optsize", "pure", "visible"})}) {
      accept();
      attributes.attrs.push_back(*attr);
    } else {
      skip();
      if (peek() != ')')
        srcLoc0.throw_error("unrecognized function attribute");
      reject();
      break;
    }
  }
  auto srcParenR{next_delimiter(")")};
  if (!srcParenR)
    srcLoc0.throw_error("expected '@(...)' syntax for function attributes");
  attributes.srcParenR = *srcParenR;
  return std::move(attributes);
}

auto Parser::parse_function_declaration() -> BumpPtr<AST::Function> {
  checkpoint();
  auto srcLoc0{checkpoint()};
  auto attributes{parse_function_declaration_attributes()};
  auto type{parse_type()};
  if (!type) {
    reject();
    return nullptr;
  }
  auto earlyAnnotations{parse_annotation_block()};
  auto name{parse_simple_name()};
  if (!name) {
    reject();
    return nullptr;
  }
  auto params{parse_parameter_list()};
  if (!params) {
    reject();
    return nullptr;
  }
  auto srcFrequency{next_keyword({"uniform", "varying"})};
  auto lateAnnotations{parse_annotation_block()};
  auto srcEqual{std::optional<std::string_view>()};
  auto definition{BumpPtr<AST::Node>{}};
  auto srcSemicolon{std::optional<std::string_view>()};
  skip();
  if (params->is_variant() && peek() != '=')
    srcLoc0.throw_error(
        "function variant must be defined by 'let' or call expression");
  if ((srcSemicolon = next_delimiter(";"))) {
    // Nothing
  } else if ((srcEqual = next_delimiter("="))) {
    skip();
    auto srcLoc1{srcLoc};
    auto def{parse_expression()};
    if (!def)
      srcLoc0.throw_error("expected function expression after '='");
    if (!(srcSemicolon = next_delimiter(";")))
      srcLoc0.throw_error("expected ';' after function expression");
    if (params->is_variant() && !llvm::isa<AST::Let>(def.get()) &&
        !llvm::isa<AST::Call>(def.get()))
      srcLoc0.throw_error(
          "function variant definition must be 'let' or call expression");
    definition =
        allocate<AST::Return>(srcLoc1, std::in_place, std::string_view(),
                              std::move(def), std::nullopt, std::string_view());
  } else {
    auto def{parse_compound_statement()};
    if (!def)
      srcLoc0.throw_error("expected ';' or function definition");
    definition = std::move(def);
  }
  accept();
  return allocate<AST::Function>(
      srcLoc0, std::in_place, std::move(attributes), std::move(type),
      std::move(earlyAnnotations), *name, std::move(*params),
      srcFrequency ? *srcFrequency : std::string_view(),
      std::move(lateAnnotations), srcEqual ? *srcEqual : std::string_view(),
      std::move(definition), srcSemicolon ? *srcSemicolon : std::string_view());
}

auto Parser::parse_tag_declaration() -> BumpPtr<AST::Tag> {
  skip();
  auto srcLoc0{srcLoc};
  auto srcKwTag{next_keyword("tag")};
  if (!srcKwTag)
    return nullptr;
  auto name{parse_simple_name()};
  if (!name)
    srcLoc0.throw_error("expected name after 'tag'");
  auto srcSemicolon{next_delimiter(";")};
  if (!srcSemicolon)
    srcLoc0.throw_error("expected ';' after 'tag ...'");
  return allocate<AST::Tag>(srcLoc0, std::in_place, *srcKwTag, *name,
                            *srcSemicolon);
}

auto Parser::parse_unit_test_declaration() -> BumpPtr<AST::UnitTest> {
  skip();
  auto srcLoc0{srcLoc};
  auto srcKwUnitTest{next_keyword("unit_test")};
  if (!srcKwUnitTest)
    return nullptr;
  auto name{parse_literal_string_expression()};
  if (!name)
    srcLoc0.throw_error("expected literal string after 'unit_test'");
  auto stmt{parse_compound_statement()};
  if (!stmt)
    srcLoc0.throw_error("expected compound statement after 'unit_test ...'");
  return allocate<AST::UnitTest>(srcLoc0, std::in_place, *srcKwUnitTest,
                                 std::move(name), std::move(stmt));
}
//--}

//--{ Parse: Stmt
auto Parser::parse_statement() -> BumpPtr<AST::Stmt> {
  skip();
  auto srcLoc0{srcLoc};
  if (auto stmt{parse_compound_statement()})
    return stmt;
  if (auto stmt{parse_if_statement()})
    return stmt;
  if (auto stmt{parse_switch_statement()})
    return stmt;
  if (auto stmt{parse_while_statement()})
    return stmt;
  if (auto stmt{parse_do_statement()})
    return stmt;
  if (auto stmt{parse_for_statement()})
    return stmt;
  if (auto stmt{parse_break_statement()})
    return stmt;
  if (auto stmt{parse_continue_statement()})
    return stmt;
  if (auto stmt{parse_return_statement()})
    return stmt;
  if (isSmdl) {
    if (auto stmt{parse_unreachable_statement()})
      return stmt;
    if (auto stmt{parse_preserve_statement()})
      return stmt;
    if (auto stmt{parse_defer_statement()})
      return stmt;
    if (auto stmt{parse_visit_statement()})
      return stmt;
  }
  if (auto decl{parse_type_declaration()})
    return allocate<AST::DeclStmt>(srcLoc0, std::in_place, std::move(decl));
  if (auto decl{parse_variable_declaration()})
    return allocate<AST::DeclStmt>(srcLoc0, std::in_place, std::move(decl));
  if (auto srcSemicolon{next_delimiter(";")})
    return allocate<AST::ExprStmt>(srcLoc0, std::in_place, nullptr,
                                   std::nullopt, *srcSemicolon);
  if (auto expr{parse_expression()}) {
    auto lateIf{parse_late_if()};
    auto srcSemicolon{next_delimiter(";")};
    if (!srcSemicolon)
      srcLoc0.throw_error("expected ';' after expression");
    return allocate<AST::ExprStmt>(srcLoc0, std::in_place, std::move(expr),
                                   std::move(lateIf), *srcSemicolon);
  }
  return nullptr;
}

auto Parser::parse_compound_statement() -> BumpPtr<AST::Compound> {
  skip();
  auto srcLoc0{srcLoc};
  auto srcBraceL{next_delimiter("{")};
  if (!srcBraceL)
    return nullptr;
  auto stmts{std::vector<BumpPtr<AST::Stmt>>{}};
  while (true) {
    auto stmt{parse_statement()};
    if (!stmt)
      break;
    stmts.push_back(std::move(stmt));
    skip();
    if (peek() == '}')
      break;
  }
  auto srcBraceR{next_delimiter("}")};
  if (!srcBraceR)
    srcLoc0.throw_error("expected '}' to close compound statement");
  return allocate<AST::Compound>(srcLoc0, std::in_place, *srcBraceL,
                                 std::move(stmts), *srcBraceR);
}

auto Parser::parse_if_statement() -> BumpPtr<AST::If> {
  skip();
  auto srcLoc0{srcLoc};
  auto srcKwIf{next_keyword("if")};
  if (!srcKwIf)
    return nullptr;
  auto exprCond{parse_expression_in_parentheses()};
  if (!exprCond)
    srcLoc0.throw_error("expected parenthesized condition after 'if'");
  auto ifPass{parse_statement()};
  if (!ifPass)
    srcLoc0.throw_error("expected statement after 'if (...)'");
  if (auto srcKwElse{next_keyword("else")}) {
    auto ifFail{parse_statement()};
    if (!ifFail)
      srcLoc0.throw_error("expected statement after 'else'");
    return allocate<AST::If>(srcLoc0, std::in_place, *srcKwIf,
                             std::move(exprCond), std::move(ifPass), *srcKwElse,
                             std::move(ifFail));
  } else {
    return allocate<AST::If>(srcLoc0, std::in_place, *srcKwIf,
                             std::move(exprCond), std::move(ifPass), *srcKwElse,
                             nullptr);
  }
}

auto Parser::parse_switch_statement() -> BumpPtr<AST::Switch> {
  auto srcLoc0{srcLoc};
  auto srcKwSwitch{next_keyword("switch")};
  if (!srcKwSwitch)
    return nullptr;
  auto expr{parse_expression_in_parentheses()};
  if (!expr)
    srcLoc0.throw_error("expected parenthesized expression after 'switch'");
  auto srcBraceL{next_delimiter("{")};
  if (!srcBraceL)
    srcLoc0.throw_error("expected opening '{' after 'switch'");
  auto switchCases{std::vector<AST::Switch::Case>{}};
  while (true) {
    auto switchCase{parse_switch_case()};
    if (!switchCase)
      break;
    switchCases.push_back(std::move(*switchCase));
    skip();
    if (peek() == '}')
      break;
  }
  auto srcBraceR{next_delimiter("}")};
  if (!srcBraceR)
    srcLoc0.throw_error("expected closing '}' after 'switch'");
  return allocate<AST::Switch>(srcLoc0, std::in_place, *srcKwSwitch,
                               std::move(expr), *srcBraceL,
                               std::move(switchCases), *srcBraceR);
}

auto Parser::parse_switch_case() -> std::optional<AST::Switch::Case> {
  skip();
  auto srcLoc0{srcLoc};
  auto switchCase{AST::Switch::Case{}};
  if (auto srcKwCase{next_keyword("case")}) {
    auto expr{parse_expression()};
    if (!expr)
      srcLoc0.throw_error("expected expression after 'case'");
    auto srcColon{next_delimiter(":")};
    if (!srcColon)
      srcLoc0.throw_error("expected ':' after 'case ...'");
    switchCase.srcKwCaseOrDefault = *srcKwCase;
    switchCase.expr = std::move(expr);
    switchCase.srcColon = *srcColon;
  } else if (auto srcKwDefault{next_keyword("default")}) {
    auto srcColon{next_delimiter(":")};
    if (!srcColon)
      srcLoc0.throw_error("expected ':' after 'default'");
    switchCase.srcKwCaseOrDefault = *srcKwDefault;
    switchCase.srcColon = *srcColon;
  } else {
    return std::nullopt;
  }
  while (true) {
    auto stmt{parse_statement()};
    if (!stmt)
      break;
    switchCase.stmts.push_back(std::move(stmt));
    skip();
  }
  return std::move(switchCase);
}

auto Parser::parse_while_statement() -> BumpPtr<AST::While> {
  skip();
  auto srcLoc0{srcLoc};
  auto srcKwWhile{next_keyword("while")};
  if (!srcKwWhile)
    return nullptr;
  auto expr{parse_expression_in_parentheses()};
  if (!expr)
    srcLoc0.throw_error("expected parenthesized expression after 'while'");
  auto stmt{parse_statement()};
  if (!stmt)
    srcLoc0.throw_error("expected statement after 'while (...)'");
  return allocate<AST::While>(srcLoc0, std::in_place, *srcKwWhile,
                              std::move(expr), std::move(stmt));
}

auto Parser::parse_do_statement() -> BumpPtr<AST::DoWhile> {
  skip();
  auto srcLoc0{srcLoc};
  auto srcKwDo{next_keyword("do")};
  if (!srcKwDo)
    return nullptr;
  auto stmt{parse_statement()};
  if (!stmt)
    srcLoc0.throw_error("expected statement after 'do'");
  auto srcKwWhile{next_keyword("while")};
  if (!srcKwWhile)
    srcLoc0.throw_error("expected 'while' after 'do ...'");
  auto expr{parse_expression_in_parentheses()};
  if (!expr)
    srcLoc0.throw_error(
        "expected parenthesized expression after 'do ... while'");
  auto srcSemicolon{next_delimiter(";")};
  if (!srcSemicolon)
    srcLoc0.throw_error("expected ';' after 'do ... while (...)'");
  return allocate<AST::DoWhile>(srcLoc0, std::in_place, *srcKwDo,
                                std::move(stmt), *srcKwWhile, std::move(expr),
                                *srcSemicolon);
}

auto Parser::parse_for_statement() -> BumpPtr<AST::For> {
  skip();
  auto srcLoc0{srcLoc};
  auto srcKwFor{next_keyword("for")};
  if (!srcKwFor)
    return nullptr;
  auto srcParenL{next_delimiter("(")};
  if (!srcParenL)
    srcLoc0.throw_error("expected '(' after 'for'");
  auto stmtInit{BumpPtr<AST::Stmt>{}};
  if (auto decl{parse_variable_declaration()}) {
    stmtInit =
        allocate<AST::DeclStmt>(decl->srcLoc, std::in_place, std::move(decl));
  } else if (auto expr{parse_expression()}) {
    auto srcSemicolon{next_delimiter(";")};
    if (!srcSemicolon)
      srcLoc0.throw_error("expected ';' after expression");
    stmtInit =
        allocate<AST::ExprStmt>(expr->srcLoc, std::in_place, std::move(expr),
                                std::nullopt, *srcSemicolon);
  } else {
    srcLoc0.throw_error(
        "expected variable declaration or expression after 'for ('");
  }
  auto exprCond{parse_expression()};
  auto srcSemicolonAfterCond{next_delimiter(";")};
  if (!srcSemicolonAfterCond)
    srcLoc0.throw_error("expected ';' after 'for (... ; ...'");
  auto exprIncr{parse_expression()};
  auto srcParenR{next_delimiter(")")};
  if (!srcParenR)
    srcLoc0.throw_error("expected ')' after 'for (...'");
  auto stmt{parse_statement()};
  if (!stmt)
    srcLoc0.throw_error("expected statement after 'for (...)'");
  return allocate<AST::For>(srcLoc0, std::in_place, *srcKwFor, *srcParenL,
                            std::move(stmtInit), std::move(exprCond),
                            *srcSemicolonAfterCond, std::move(exprIncr),
                            *srcParenR, std::move(stmt));
}

auto Parser::parse_break_statement() -> BumpPtr<AST::Break> {
  skip();
  auto srcLoc0{srcLoc};
  auto srcKwBreak{next_keyword("break")};
  if (!srcKwBreak)
    return nullptr;
  auto lateIf{parse_late_if()};
  auto srcSemicolon{next_delimiter(";")};
  if (!srcSemicolon)
    srcLoc0.throw_error("expected ';' after 'break'");
  return allocate<AST::Break>(srcLoc0, std::in_place, *srcKwBreak,
                              std::move(lateIf), *srcSemicolon);
}

auto Parser::parse_continue_statement() -> BumpPtr<AST::Continue> {
  skip();
  auto srcLoc0{srcLoc};
  auto srcKwContinue{next_keyword("continue")};
  if (!srcKwContinue)
    return nullptr;
  auto lateIf{parse_late_if()};
  auto srcSemicolon{next_delimiter(";")};
  if (!srcSemicolon)
    srcLoc0.throw_error("expected ';' after 'continue'");
  return allocate<AST::Continue>(srcLoc0, std::in_place, *srcKwContinue,
                                 std::move(lateIf), *srcSemicolon);
}

auto Parser::parse_return_statement() -> BumpPtr<AST::Return> {
  skip();
  auto srcLoc0{srcLoc};
  auto srcKwReturn{next_keyword("return")};
  if (!srcKwReturn)
    return nullptr;
  auto expr{parse_expression()}; // Allow this to be null!
  auto lateIf{parse_late_if()};
  auto srcSemicolon{next_delimiter(";")};
  if (!srcSemicolon)
    srcLoc0.throw_error("expected ';' after 'return ...'");
  return allocate<AST::Return>(srcLoc0, std::in_place, *srcKwReturn,
                               std::move(expr), std::move(lateIf),
                               *srcSemicolon);
}

auto Parser::parse_unreachable_statement() -> BumpPtr<AST::Unreachable> {
  skip();
  auto srcLoc0{srcLoc};
  auto srcKwUnreachable{next_keyword("unreachable")};
  if (!srcKwUnreachable)
    return nullptr;
  auto srcSemicolon{next_delimiter(";")};
  if (!srcSemicolon)
    srcLoc0.throw_error("expected ';' after 'unreachable'");
  return allocate<AST::Unreachable>(srcLoc0, std::in_place, *srcKwUnreachable,
                                    *srcSemicolon);
}

auto Parser::parse_preserve_statement() -> BumpPtr<AST::Preserve> {
  auto srcLoc0{srcLoc};
  auto srcKwPreserve{next_keyword("preserve")};
  if (!srcKwPreserve)
    return nullptr;
  auto exprs{std::vector<AST::Preserve::ExprWrapper>{}};
  while (true) {
    auto expr{parse_unary_expression()};
    if (!expr)
      break;
    exprs.push_back(AST::Preserve::ExprWrapper{std::move(expr), {}});
    auto srcComma{next_delimiter(",")};
    if (!srcComma)
      break;
    exprs.back().srcComma = *srcComma;
  }
  auto srcSemicolon{next_delimiter(";")};
  if (!srcSemicolon)
    srcLoc0.throw_error("expected ';' after 'preserve ...'");
  return allocate<AST::Preserve>(srcLoc0, std::in_place, *srcKwPreserve,
                                 std::move(exprs), *srcSemicolon);
}

auto Parser::parse_defer_statement() -> BumpPtr<AST::Defer> {
  skip();
  auto srcLoc0{srcLoc};
  auto srcKwDefer{next_keyword("defer")};
  if (!srcKwDefer)
    return nullptr;
  auto stmt{parse_statement()};
  if (!stmt)
    srcLoc0.throw_error("expected statement after 'defer'");
  return allocate<AST::Defer>(srcLoc0, std::in_place, *srcKwDefer,
                              std::move(stmt));
}

auto Parser::parse_visit_statement() -> BumpPtr<AST::Visit> {
  skip();
  auto srcLoc0{srcLoc};
  auto srcKwVisit{next_keyword("visit")};
  if (!srcKwVisit)
    return nullptr;
  auto name{parse_simple_name()};
  if (!name)
    srcLoc0.throw_error("expected name after 'visit'");
  auto srcKwIn{next_keyword("in")};
  if (!srcKwIn)
    srcLoc0.throw_error("expected 'in' after 'visit ...'");
  auto expr{parse_expression()};
  if (!expr)
    srcLoc0.throw_error("expected expression after 'visit ... in'");
  auto stmt{parse_compound_statement()};
  if (!stmt)
    srcLoc0.throw_error("expected compound statement after 'visit ... in ...'");
  return allocate<AST::Visit>(srcLoc0, std::in_place, *srcKwVisit, *name,
                              *srcKwIn, std::move(expr), std::move(stmt));
}

auto Parser::parse_late_if() -> std::optional<AST::LateIf> {
  if (!isSmdl)
    return std::nullopt;
  skip();
  auto srcLoc0{srcLoc};
  auto srcKwIf{next_keyword("if")};
  if (!srcKwIf)
    return std::nullopt;
  auto expr{parse_expression_in_parentheses()};
  if (!expr)
    srcLoc0.throw_error("expected expression in parentheses after '... if'");
  return AST::LateIf(*srcKwIf, std::move(expr));
}
//--}

} // namespace smdl
