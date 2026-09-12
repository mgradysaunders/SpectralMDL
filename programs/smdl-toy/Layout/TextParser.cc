#include "Layout/TextParser.h"

#include "smdl/Support/Error.h"

#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <filesystem>

// The syntax core: characters to tokens, and the helpers every format of
// the layout family parses its settings and transforms with. Nothing
// here knows a directive by name; that is what the derived parsers are.

std::string resolveSiblingFile(const std::string &stated,
                               const std::string &cameraFileName,
                               std::string_view what) {
  if (stated.empty()) return {};
  auto path{std::filesystem::path(stated)};
  if (path.is_relative() && !cameraFileName.empty())
    path = std::filesystem::path(cameraFileName).parent_path() / path;
  if (!std::filesystem::exists(path))
    throw smdl::Error(smdl::concat("the camera file names the ", what, " ",
                                   smdl::QuotedPath(stated),
                                   ", which does not exist beside it"));
  return path.string();
}

Token Lexer::next() {
  const auto &text{mSource.text};
  for (;;) {
    while (mPos < text.size() && smdl::isSpace(text[mPos])) mPos++;
    if (mPos >= text.size()) return {Token::END, {}, position(), 1};
    if (text[mPos] != '#') break;
    while (mPos < text.size() && text[mPos] != '\n') mPos++;
  }
  const auto start{position()};
  const char ch{text[mPos]};
  if (ch == '{') return mPos++, Token{Token::OPEN, "{", start, 1};
  if (ch == '}') return mPos++, Token{Token::CLOSE, "}", start, 1};
  if (ch == '=') return mPos++, Token{Token::EQUALS, "=", start, 1};
  if (ch == '"') {
    mPos++;
    std::string content{};
    while (mPos < text.size() && text[mPos] != '"' && text[mPos] != '\n')
      content += text[mPos], mPos++;
    if (mPos < text.size() && text[mPos] == '"') {
      mPos++;
    } else {
      // Stopped at a newline or the end of the file: take what was
      // written as the string so parsing can continue past it.
      mDiags.error({&mSource, start, position() - start},
                   "unterminated string");
    }
    return {Token::STRING, std::move(content), start, position() - start};
  }
  std::string content{};
  while (mPos < text.size() && !smdl::isSpace(text[mPos]) &&
         text[mPos] != '{' && text[mPos] != '}' && text[mPos] != '=' &&
         text[mPos] != '#') {
    content += text[mPos], mPos++;
  }
  return {Token::WORD, std::move(content), start, position() - start};
}

void TextParser::synchronize() {
  size_t depth{mToken.kind == Token::OPEN ? size_t(1) : size_t(0)};
  if (depth == 0 && isTopLevelKeyword(mToken) &&
      lineOf(mToken) > lastDiagnosticLine())
    return;
  if (mToken.kind != Token::END) advance();
  while (mToken.kind != Token::END) {
    if (mToken.kind == Token::OPEN) depth++;
    if (mToken.kind == Token::CLOSE && depth > 0) depth--;
    if (depth == 0 && isTopLevelKeyword(mToken)) return;
    advance();
  }
}

bool TextParser::isTopLevelKeyword(const Token &token) const noexcept {
  return token.kind == Token::WORD &&
         std::find(mKeywords.begin(), mKeywords.end(), token.text) !=
             mKeywords.end();
}

uint32_t TextParser::lastDiagnosticLine() const noexcept {
  if (mDiags.all().empty()) return 0;
  const auto &location{mDiags.all().back().location};
  if (location.source != &mSource) return 0;
  return mSource.lineAndColumn(location.offset).lineNo;
}

float TextParser::positive(const LayoutLocation &keyLoc, std::string_view key,
                           float value) {
  if (!(value > 0)) {
    mDiags.error(keyLoc, smdl::concat("expected a positive number for ",
                                      smdl::Quoted(key),
                                      " (omit it to leave it unset)"));
    throw Recover();
  }
  return value;
}

float TextParser::finite(const LayoutLocation &keyLoc, std::string_view key,
                         float value) {
  if (!std::isfinite(value)) {
    mDiags.error(keyLoc, smdl::concat("expected a finite number for ",
                                      smdl::Quoted(key)));
    throw Recover();
  }
  return value;
}

bool TextParser::parseTransformOp(const std::string &op,
                                  const LayoutLocation &opLoc, float4x4 &xf) {
  if (op == "translate") {
    auto v{numbers<3>()};
    xf = translation(v[0], v[1], v[2]) * xf;
  } else if (op == "scale") {
    // One number scales uniformly and three scale per axis. Operation
    // names are never numbers, so looking at whether a number follows
    // tells the two apart with no ambiguity.
    auto scale{float3()};
    scale[0] = numbers<1>()[0];
    if (isNumber(mToken)) {
      scale[1] = numbers<1>()[0];
      if (!isNumber(mToken)) {
        mDiags.error(opLoc, "expected one number for a uniform 'scale' or "
                            "three for a non-uniform one");
        throw Recover();
      }
      scale[2] = numbers<1>()[0];
    } else {
      scale[1] = scale[2] = scale[0];
    }
    xf = float4x4{float4{scale[0], 0, 0, 0}, float4{0, scale[1], 0, 0},
                  float4{0, 0, scale[2], 0}, float4{0, 0, 0, 1}} *
         xf;
  } else if (op == "rotate_x" || op == "rotate_y" || op == "rotate_z") {
    auto axis{float3{}};
    axis[op.back() - 'x'] = 1.0f;
    xf = rotation(axis, numbers<1>()[0]) * xf;
  } else if (op == "rotate") {
    auto v{numbers<4>()};
    xf = rotation(float3(v[0], v[1], v[2]), v[3]) * xf;
  } else if (op == "matrix") {
    auto v{numbers<16>()};
    // Written row-major, which is how anyone lays a matrix out on the
    // page; `float4x4` stores columns.
    auto m{float4x4()};
    for (size_t i = 0; i < 4; i++)
      for (size_t j = 0; j < 4; j++) m[j][i] = v[4 * i + j];
    xf = m * xf;
  } else {
    return false;
  }
  return true;
}

bool TextParser::isNumber(const Token &token) {
  float ignored{};
  return tryNumber(token, ignored);
}

bool TextParser::tryNumber(const Token &token, float &value) {
  if (token.kind != Token::WORD || token.text.empty()) return false;
  const char *begin{token.text.c_str()};
  char *end{};
  const float parsed{std::strtof(begin, &end)};
  // A value out of float's range is a number the grammar accepts and
  // the range checks downstream reject, which is where saying so
  // belongs; `strtof` still returns the saturated value for it.
  if (end != begin + token.text.size()) return false;
  value = parsed;
  return true;
}

std::string TextParser::expect(Token::Kind kind, std::string_view what) {
  if (mToken.kind != kind) {
    mDiags.error(location(), smdl::concat("expected ", what));
    throw Recover();
  }
  auto text{mToken.text};
  advance();
  return text;
}

bool TextParser::isIdentifier(std::string_view name) {
  if (name.empty() || !(smdl::isAlpha(name[0]) || name[0] == '_')) return false;
  for (const char ch : name)
    if (!smdl::isWord(ch)) return false;
  return true;
}

uint32_t TextParser::lineOf(const Token &token) const noexcept {
  return mSource.lineAndColumn(token.offset).lineNo;
}

float4x4 TextParser::translation(float x, float y, float z) {
  return float4x4{float4{1, 0, 0, 0}, float4{0, 1, 0, 0}, float4{0, 0, 1, 0},
                  float4{x, y, z, 1}};
}

float4x4 TextParser::rotation(float3 axis, float degrees) {
  if (!smdl::tryNormalize(axis)) return float4x4(1.0f);
  const float radians{smdl::radians(degrees)};
  const float c{std::cos(radians)}, s{std::sin(radians)}, t{1.0f - c};
  return float4x4{
      float4{t * axis.x * axis.x + c, t * axis.x * axis.y + s * axis.z,
             t * axis.x * axis.z - s * axis.y, 0},
      float4{t * axis.x * axis.y - s * axis.z, t * axis.y * axis.y + c,
             t * axis.y * axis.z + s * axis.x, 0},
      float4{t * axis.x * axis.z + s * axis.y, t * axis.y * axis.z - s * axis.x,
             t * axis.z * axis.z + c, 0},
      float4{0, 0, 0, 1}};
}
