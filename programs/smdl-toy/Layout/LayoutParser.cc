#include "Layout/Layout.h"

#include "smdl/Support/Strings.h"

#include <algorithm>
#include <array>
#include <cmath>
#include <cstdlib>
#include <map>

// The parser half of the layout toolchain: characters to tokens to a
// `LayoutDocument`, with nothing else. Path resolution, imports, and
// lowering live in `Layout.cc`, and everything here stays free of the
// filesystem so that a document parses the same from disk or from a
// test's string.

namespace {

// One token, with the byte range it came from so that every diagnostic
// can point at it.
class Token final {
public:
  enum Kind { WORD, STRING, OPEN, CLOSE, EQUALS, END };
  Kind kind{END};
  std::string text{};
  uint32_t offset{};
  uint32_t length{};
};

// The whole file as tokens: quoted strings, bare words, and the three
// punctuation marks. A directive body can span one line or several
// without the parser caring which.
class Lexer final {
public:
  Lexer(LayoutDiagnostics &diags, const LayoutSource &source)
      : mDiags(diags), mSource(source) {}

  [[nodiscard]] Token next() {
    const auto &text{mSource.text};
    for (;;) {
      while (mPos < text.size() && isSpace(text[mPos])) mPos++;
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
    while (mPos < text.size() && !isSpace(text[mPos]) && text[mPos] != '{' &&
           text[mPos] != '}' && text[mPos] != '=' && text[mPos] != '#') {
      content += text[mPos], mPos++;
    }
    return {Token::WORD, std::move(content), start, position() - start};
  }

private:
  [[nodiscard]] static bool isSpace(char ch) noexcept {
    return ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n';
  }

  [[nodiscard]] uint32_t position() const noexcept { return uint32_t(mPos); }

  LayoutDiagnostics &mDiags;
  const LayoutSource &mSource;
  size_t mPos{};
};

// Thrown to abandon the statement being parsed after its diagnostic is
// emitted; caught by the statement loop, which synchronizes at the next
// top-level keyword. Never escapes `parseLayout()`.
class Recover final {};

// The top-level keywords, which are the synchronization points.
constexpr std::array<std::string_view, 11> TOP_LEVEL_KEYWORDS{
    "asset",  "group",  "place", "import", "light", "material",
    "medium", "camera", "sky",   "haze",   "time"};

// The transform operations, named here so an unknown top-level
// directive that is really a stray transform gets a pointed note.
constexpr std::array<std::string_view, 7> TRANSFORM_OPS{
    "translate", "scale",    "rotate", "rotate_x",
    "rotate_y",  "rotate_z", "matrix"};

class Parser final {
public:
  Parser(LayoutDiagnostics &diags, const LayoutSource &source,
         LayoutDocument &document)
      : mDiags(diags), mSource(source), mLexer(diags, source),
        mDocument(document) {
    mToken = mLexer.next();
  }

  void parse() {
    checkMagic();
    while (mToken.kind != Token::END) {
      try {
        parseStatement();
      } catch (const Recover &) {
        synchronize();
      }
    }
  }

private:
  // The `#smdl layout` first line, checked against the raw text rather
  // than the token stream, because to the grammar it is only a comment.
  void checkMagic() {
    const auto &text{mSource.text};
    const auto magic{LAYOUT_MAGIC};
    const bool present{
        smdl::startsWith(text, magic) &&
        (text.size() == magic.size() || text[magic.size()] == '\n' ||
         text[magic.size()] == '\r' || text[magic.size()] == ' ')};
    if (!present)
      mDiags.error({&mSource, 0, 1},
                   smdl::concat("expected ", smdl::Quoted(magic),
                                " on the first line of a layout file"));
  }

  void parseStatement() {
    if (mToken.kind != Token::WORD) {
      mDiags.error(location(), smdl::concat("expected a directive, got ",
                                            smdl::Quoted(mToken.text)));
      throw Recover();
    }
    if (mToken.text == "asset") {
      parseAsset();
    } else if (mToken.text == "group") {
      parseGroup();
    } else if (mToken.text == "place") {
      parsePlaceInto(mDocument.placements);
    } else if (mToken.text == "import") {
      parseImport();
    } else if (mToken.text == "light") {
      parseLight();
    } else if (mToken.text == "material") {
      parseAlias();
    } else if (mToken.text == "medium") {
      parseMedium();
    } else if (mToken.text == "camera") {
      parseCamera();
    } else if (mToken.text == "sky") {
      parseSky();
    } else if (mToken.text == "haze") {
      parseHaze();
    } else if (mToken.text == "time") {
      parseTime();
    } else {
      auto &error{
          mDiags.error(location(), smdl::concat("unknown directive ",
                                                smdl::Quoted(mToken.text)))};
      if (std::find(TRANSFORM_OPS.begin(), TRANSFORM_OPS.end(), mToken.text) !=
          TRANSFORM_OPS.end()) {
        error.note({}, "transform operations belong on a 'place' line or "
                       "inside its block");
      } else if (const auto nearest{
                     smdl::suggestNearest(mToken.text, TOP_LEVEL_KEYWORDS)};
                 !nearest.empty()) {
        error.note({},
                   smdl::concat("did you mean ", smdl::Quoted(nearest), "?"));
      }
      throw Recover();
    }
  }

  // Skip to the next top-level keyword, tracking brace depth so that a
  // keyword inside the abandoned statement's block does not fool the
  // loop into starting mid-block. An error raised after a line's last
  // token leaves the next statement's keyword current already; it is
  // not part of the abandoned statement, so it is kept rather than
  // skipped, and the line check is what tells it from a keyword that
  // is merely an operation word of the abandoned line.
  void synchronize() {
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

  [[nodiscard]] static bool isTopLevelKeyword(const Token &token) noexcept {
    return token.kind == Token::WORD &&
           std::find(TOP_LEVEL_KEYWORDS.begin(), TOP_LEVEL_KEYWORDS.end(),
                     token.text) != TOP_LEVEL_KEYWORDS.end();
  }

  // The line of the diagnostic just raised, or 0 if it points nowhere.
  [[nodiscard]] uint32_t lastDiagnosticLine() const noexcept {
    if (mDiags.all().empty()) return 0;
    const auto &location{mDiags.all().back().location};
    if (location.source != &mSource) return 0;
    return mSource.lineAndColumn(location.offset).lineNo;
  }

  void parseAsset() {
    advance();
    auto &decl{mDocument.assets.emplace_back()};
    if (mToken.kind != Token::WORD || !isIdentifier(mToken.text)) {
      mDiags.error(location(), "expected an asset name after 'asset'");
      mDocument.assets.pop_back();
      throw Recover();
    }
    decl.name = mToken.text;
    decl.nameLoc = location();
    if (const auto previous{findDeclaration(decl.name, &decl)}) {
      mDiags
          .error(decl.nameLoc, smdl::concat("redeclaration of asset ",
                                            smdl::Quoted(decl.name)))
          .note(previous, "previous declaration is here");
      mDocument.assets.pop_back();
      throw Recover();
    }
    advance();
    expect(Token::EQUALS, "'=' after the asset name");
    decl.pathLoc = location();
    // The source is a quoted path, or one of the built-in shape
    // keywords: paths are quoted because they are names out of the
    // filesystem, and shapes are bare because they are vocabulary.
    if (mToken.kind == Token::WORD) {
      if (mToken.text == "sphere") {
        decl.primitive.shape = PrimitiveSpec::Shape::SPHERE;
      } else if (mToken.text == "box") {
        decl.primitive.shape = PrimitiveSpec::Shape::BOX;
      } else if (mToken.text == "disk") {
        decl.primitive.shape = PrimitiveSpec::Shape::DISK;
      } else if (mToken.text == "cylinder") {
        decl.primitive.shape = PrimitiveSpec::Shape::CYLINDER;
      } else if (mToken.text == "cone") {
        decl.primitive.shape = PrimitiveSpec::Shape::CONE;
      } else {
        mDiags.error(location(),
                     smdl::concat("expected a quoted path, or one of the "
                                  "shapes 'sphere', 'box', 'disk', "
                                  "'cylinder', or 'cone', after '=', got ",
                                  smdl::Quoted(mToken.text)));
        throw Recover();
      }
      advance();
    } else {
      decl.path = expect(Token::STRING, "a quoted path after '='");
    }
    if (mToken.kind == Token::OPEN) parseAssetBody(decl);
    // A primitive has no mesh slots: whole-asset 'material <name>' is
    // required, and per-slot assignment is impossible.
    if (decl.primitive.active()) {
      if (decl.materials.all.empty())
        mDiags.error(decl.nameLoc,
                     smdl::concat("the ", decl.primitive.name(), " asset ",
                                  smdl::Quoted(decl.name),
                                  " needs 'material <name>' in its block"));
      if (!decl.materials.bySlot.empty())
        mDiags.error(decl.nameLoc, smdl::concat("a ", decl.primitive.name(),
                                                " has no material slots; use "
                                                "'material <name>' alone"));
    }
  }

  void parseAssetBody(LayoutAssetDecl &decl) {
    advance(); // '{'
    while (mToken.kind != Token::CLOSE) {
      if (mToken.kind != Token::END && mToken.kind != Token::WORD) {
        mDiags.error(location(), "expected an asset operation or '}'");
        throw Recover();
      }
      if (mToken.kind == Token::END) {
        mDiags.error(location(), "expected '}' before end of file");
        throw Recover();
      }
      const auto op{mToken.text};
      const auto opLoc{location()};
      advance();
      if (op == "select" || op == "recenter" || op == "subdivide" ||
          op == "displace") {
        // Analytic shapes have no objects to pick, no polygons to refine,
        // and displacement would need vertices to move.
        if (decl.primitive.active()) {
          mDiags.error(opLoc, smdl::concat(smdl::Quoted(op),
                                           " applies to a mesh file, but this "
                                           "asset is a ",
                                           decl.primitive.name()));
          throw Recover();
        }
        if (op == "select") {
          decl.selection.patterns.push_back(
              expect(Token::STRING, "a quoted object name after 'select'"));
        } else if (op == "recenter") {
          decl.selection.recenter = true;
        } else if (op == "subdivide") {
          parseSubdivide(decl.subdiv, opLoc);
        } else {
          decl.subdiv.displace = true;
        }
      } else if (op == "radius" || op == "height" || op == "size") {
        if (!decl.primitive.active()) {
          mDiags.error(opLoc,
                       smdl::concat(smdl::Quoted(op),
                                    " is a shape parameter, and this asset "
                                    "is a file"));
          throw Recover();
        }
        if ((op == "radius" && !decl.primitive.hasRadius()) ||
            (op == "height" && !decl.primitive.hasHeight()) ||
            (op == "size" && !decl.primitive.hasSize())) {
          mDiags.error(opLoc, smdl::concat("a ", decl.primitive.name(),
                                           " has no ", smdl::Quoted(op)));
          throw Recover();
        }
        if (op == "size") {
          const auto values{numbers<3>()};
          if (!(values[0] > 0 && values[1] > 0 && values[2] > 0)) {
            mDiags.error(opLoc, "expected three positive numbers for 'size'");
            throw Recover();
          }
          decl.primitive.size = float3(values[0], values[1], values[2]);
        } else {
          const auto value{numbers<1>()[0]};
          if (!(value > 0)) {
            mDiags.error(opLoc, smdl::concat("expected a positive number for ",
                                             smdl::Quoted(op)));
            throw Recover();
          }
          (op == "radius" ? decl.primitive.radius : decl.primitive.height) =
              value;
        }
      } else if (op == "tube" || op == "ribbon" || op == "radius_scale") {
        // Whether the path names a curves file is the lowering's to
        // discover; what the parser can already reject is a shape,
        // which is never one.
        if (decl.primitive.active()) {
          mDiags.error(opLoc,
                       smdl::concat(smdl::Quoted(op),
                                    " applies to a curves file, but this "
                                    "asset is a ",
                                    decl.primitive.name()));
          throw Recover();
        }
        if (!decl.curvesOpsLoc) decl.curvesOpsLoc = opLoc;
        if (op == "radius_scale") {
          const auto value{numbers<1>()[0]};
          if (!(value > 0)) {
            mDiags.error(opLoc,
                         "expected a positive number for 'radius_scale'");
            throw Recover();
          }
          decl.curves.radiusScale = value;
        } else {
          // One word decides the cross-section, so a second is either
          // a repeat or a contradiction, and both are conflicts.
          if (decl.curves.modeSet) {
            mDiags.error(opLoc, "'tube' or 'ribbon' appears twice in "
                                "one asset");
            throw Recover();
          }
          decl.curves.mode = op == "ribbon" ? CurvesSpec::Mode::RIBBON
                                            : CurvesSpec::Mode::TUBE;
          decl.curves.modeSet = true;
        }
      } else if (op == "material") {
        parseMaterialOps(decl.materials, "asset");
      } else if (op == "caster") {
        decl.caster = true;
        decl.casterLoc = opLoc;
      } else if (op == "light") {
        decl.light = true;
        decl.lightLoc = opLoc;
      } else if (op == "caustic") {
        decl.caustic = true;
      } else if (op == "at") {
        mDiags
            .error(opLoc, "'at' blocks were retired with the '.scene' "
                          "format")
            .note({}, "write one top-level 'place' per instance instead");
        throw Recover();
      } else if (!parseTransformOp(op, opLoc, decl.transform)) {
        mDiags.error(opLoc,
                     smdl::concat("unknown asset operation ", smdl::Quoted(op),
                                  decl.primitive.active()
                                      ? " (expected radius, height, size, "
                                        "material, caster, light, translate, "
                                        "scale, rotate, rotate_x, rotate_y, "
                                        "rotate_z, or matrix)"
                                      : " (expected select, recenter, "
                                        "subdivide, displace, tube, ribbon, "
                                        "radius_scale, material, caster, "
                                        "light, translate, scale, rotate, "
                                        "rotate_x, rotate_y, rotate_z, or "
                                        "matrix)"));
        throw Recover();
      }
    }
    advance(); // '}'
  }

  // A `light` declaration: `light <name> = point|spot|profile
  // "<path>"|rect|disk` with an optional block of settings and
  // transform operations. See `LayoutLightDecl` for the semantics of
  // each setting.
  void parseLight() {
    advance();
    auto &decl{mDocument.lights.emplace_back()};
    if (mToken.kind != Token::WORD || !isIdentifier(mToken.text)) {
      mDiags.error(location(), "expected a light name after 'light'");
      mDocument.lights.pop_back();
      throw Recover();
    }
    decl.name = mToken.text;
    decl.nameLoc = location();
    if (const auto previous{findDeclaration(decl.name, &decl)}) {
      mDiags
          .error(decl.nameLoc, smdl::concat("redeclaration of light ",
                                            smdl::Quoted(decl.name)))
          .note(previous, "previous declaration is here");
      mDocument.lights.pop_back();
      throw Recover();
    }
    advance();
    expect(Token::EQUALS, "'=' after the light name");
    if (mToken.kind != Token::WORD) {
      mDiags.error(location(), "expected 'point', 'spot', 'profile', 'rect', "
                               "or 'disk' after '='");
      throw Recover();
    }
    if (mToken.text == "point") {
      decl.kind = LayoutLightDecl::Kind::POINT;
      advance();
    } else if (mToken.text == "spot") {
      decl.kind = LayoutLightDecl::Kind::SPOT;
      advance();
    } else if (mToken.text == "profile") {
      decl.kind = LayoutLightDecl::Kind::PROFILE;
      advance();
      decl.profilePathLoc = location();
      decl.profilePath =
          expect(Token::STRING, "a quoted IES path after 'profile'");
    } else if (mToken.text == "rect") {
      decl.kind = LayoutLightDecl::Kind::RECT;
      advance();
    } else if (mToken.text == "disk") {
      decl.kind = LayoutLightDecl::Kind::DISK;
      advance();
    } else {
      mDiags.error(location(),
                   smdl::concat("expected 'point', 'spot', 'profile', 'rect', "
                                "or 'disk' after '=', got ",
                                smdl::Quoted(mToken.text)));
      throw Recover();
    }
    if (mToken.kind == Token::OPEN) parseLightBody(decl);
  }

  void parseLightBody(LayoutLightDecl &decl) {
    advance(); // '{'
    const auto isSpot{decl.kind == LayoutLightDecl::Kind::SPOT};
    const auto isProfile{decl.kind == LayoutLightDecl::Kind::PROFILE};
    const auto isRect{decl.kind == LayoutLightDecl::Kind::RECT};
    const auto isDisk{decl.kind == LayoutLightDecl::Kind::DISK};
    while (mToken.kind != Token::CLOSE) {
      if (mToken.kind == Token::END) {
        mDiags.error(location(), "expected '}' before end of file");
        throw Recover();
      }
      if (mToken.kind != Token::WORD) {
        mDiags.error(location(), "expected a light setting or '}'");
        throw Recover();
      }
      const auto op{mToken.text};
      const auto opLoc{location()};
      advance();
      if (op == "power") {
        const auto value{numbers<1>()[0]};
        if (!(value > 0)) {
          mDiags.error(opLoc, "expected a positive number for 'power'");
          throw Recover();
        }
        decl.power = value;
        decl.powerSet = true;
      } else if (op == "temperature") {
        const auto value{numbers<1>()[0]};
        if (!(value > 0)) {
          mDiags.error(opLoc, "expected a positive number for 'temperature'");
          throw Recover();
        }
        decl.temperature = value;
      } else if (op == "color") {
        const auto v{numbers<3>()};
        if (!(v[0] >= 0 && v[1] >= 0 && v[2] >= 0)) {
          mDiags.error(opLoc,
                       "expected three non-negative numbers for 'color'");
          throw Recover();
        }
        decl.color = float3(v[0], v[1], v[2]);
      } else if (op == "angle" || op == "blend") {
        if (!isSpot) {
          mDiags.error(opLoc,
                       smdl::concat(smdl::Quoted(op),
                                    " applies to a spot, and this light is "
                                    "a ",
                                    decl.kindName()));
          throw Recover();
        }
        const auto value{numbers<1>()[0]};
        if (op == "angle") {
          if (!(value > 0 && value <= 180)) {
            mDiags.error(opLoc, "expected an 'angle' between 0 and 180 "
                                "degrees");
            throw Recover();
          }
          decl.spotAngle = value;
        } else {
          if (!(value >= 0 && value <= 1)) {
            mDiags.error(opLoc, "expected a 'blend' between 0 and 1");
            throw Recover();
          }
          decl.spotBlend = value;
        }
      } else if (op == "scale") {
        if (!isProfile) {
          mDiags.error(opLoc,
                       smdl::concat("'scale' applies to a profile, and this "
                                    "light is a ",
                                    decl.kindName(),
                                    isRect || isDisk
                                        ? " (the place line's 'scale' "
                                          "stretches it)"
                                        : ""));
          throw Recover();
        }
        const auto value{numbers<1>()[0]};
        if (!(value > 0)) {
          mDiags.error(opLoc, "expected a positive number for 'scale'");
          throw Recover();
        }
        decl.scale = value;
      } else if (op == "size") {
        if (!isRect) {
          mDiags.error(opLoc, smdl::concat("'size' applies to a rect, and this "
                                           "light is a ",
                                           decl.kindName()));
          throw Recover();
        }
        const auto v{numbers<2>()};
        if (!(v[0] > 0 && v[1] > 0)) {
          mDiags.error(opLoc, "expected two positive numbers for 'size'");
          throw Recover();
        }
        decl.size = float2(v[0], v[1]);
      } else if (op == "radius") {
        if (!isDisk) {
          mDiags.error(opLoc,
                       smdl::concat("'radius' applies to a disk, and this "
                                    "light is a ",
                                    decl.kindName()));
          throw Recover();
        }
        const auto value{numbers<1>()[0]};
        if (!(value > 0)) {
          mDiags.error(opLoc, "expected a positive number for 'radius'");
          throw Recover();
        }
        decl.radius = value;
      } else if (op == "caustic") {
        decl.caustic = true;
      } else if (!parseTransformOp(op, opLoc, decl.transform)) {
        // Note 'scale' is taken by the profile multiplier, so unlike the
        // other blocks it is not offered as a transform here; a shape is
        // stretched by the place line's 'scale' instead.
        mDiags.error(opLoc,
                     smdl::concat("unknown light setting ", smdl::Quoted(op),
                                  " (expected power, temperature, color, ",
                                  isSpot      ? "angle, blend, "
                                  : isProfile ? "scale, "
                                  : isRect    ? "size, "
                                  : isDisk    ? "radius, "
                                              : "",
                                  "caustic, translate, rotate, rotate_x, "
                                  "rotate_y, rotate_z, or matrix)"));
        throw Recover();
      }
    }
    advance(); // '}'
  }

  // A `group` declaration: a named arrangement of `place` statements
  // and nothing else, so that a group stays an arrangement and never
  // becomes a scope.
  void parseGroup() {
    advance();
    auto &group{mDocument.groups.emplace_back()};
    if (mToken.kind != Token::WORD || !isIdentifier(mToken.text)) {
      mDiags.error(location(), "expected a group name after 'group'");
      mDocument.groups.pop_back();
      throw Recover();
    }
    group.name = mToken.text;
    group.nameLoc = location();
    if (const auto previous{findDeclaration(group.name, &group)}) {
      mDiags
          .error(group.nameLoc, smdl::concat("redeclaration of group ",
                                             smdl::Quoted(group.name)))
          .note(previous, "previous declaration is here");
      mDocument.groups.pop_back();
      throw Recover();
    }
    advance();
    if (mToken.kind != Token::OPEN) {
      mDiags.error(location(), "expected '{' after the group name");
      throw Recover();
    }
    advance(); // '{'
    while (mToken.kind != Token::CLOSE) {
      if (mToken.kind == Token::END) {
        mDiags.error(location(), "expected '}' before end of file");
        throw Recover();
      }
      if (mToken.kind == Token::WORD && mToken.text == "place") {
        parsePlaceInto(group.placements);
        continue;
      }
      auto &error{
          mDiags.error(location(), smdl::concat("expected 'place' or '}', got ",
                                                smdl::Quoted(mToken.text)))};
      if (mToken.kind == Token::WORD &&
          (mToken.text == "import" || mToken.text == "asset" ||
           mToken.text == "group" || mToken.text == "light"))
        error.note({}, "a group holds 'place' statements only; declare "
                       "assets, groups, and lights at the top level and "
                       "'place' them here");
      throw Recover();
    }
    advance(); // '}'
  }

  // A `place` statement, in any of its three spellings: bare, a block,
  // or everything on the `place` keyword's own line. The one-line form
  // exists for machines that write one instance per line; the line
  // boundary is what keeps the next top-level directive from reading as
  // more operations.
  void parsePlaceInto(std::vector<LayoutPlacement> &into) {
    const auto placeLine{lineOf(mToken)};
    advance();
    auto &placement{into.emplace_back()};
    placement.kind = LayoutPlacement::Kind::PLACE;
    if (mToken.kind != Token::WORD || !isIdentifier(mToken.text)) {
      mDiags.error(location(), "expected an asset or group name after "
                               "'place'");
      into.pop_back();
      throw Recover();
    }
    placement.assetName = mToken.text;
    placement.assetNameLoc = location();
    advance();
    // The bulk form: one instance per record of a '.places' buffer,
    // each record standing where a one-line place's operations would.
    if (mToken.kind == Token::WORD && mToken.text == "*") {
      advance();
      placement.placesPathLoc = location();
      placement.placesPath =
          expect(Token::STRING, "a quoted '.places' path after '*'");
    }
    if (mToken.kind == Token::WORD && mToken.text == "as") {
      if (!placement.placesPath.empty()) {
        mDiags
            .error(location(), "a bulk place has no single identity to "
                               "record, so 'as' does not apply")
            .note({}, "identity for scattered records is a future concern "
                      "of the buffer itself");
        throw Recover();
      }
      advance();
      if (mToken.kind != Token::WORD || !isIdentifier(mToken.text)) {
        mDiags.error(location(), "expected a name after 'as'");
        throw Recover();
      }
      placement.asName = mToken.text;
      placement.asNameLoc = location();
      // Identity wants uniqueness; nothing consumes it yet, so a repeat
      // is a warning rather than an error.
      if (auto [itr, isNew]{
              mAsNames.try_emplace(placement.asName, placement.asNameLoc)};
          !isNew)
        mDiags
            .warn(placement.asNameLoc,
                  smdl::concat("duplicate place name ",
                               smdl::Quoted(placement.asName)))
            .note(itr->second, "first placed here");
      advance();
    }
    if (mToken.kind == Token::OPEN) {
      advance(); // '{'
      while (mToken.kind != Token::CLOSE) {
        if (mToken.kind == Token::END) {
          mDiags.error(location(), "expected '}' before end of file");
          throw Recover();
        }
        if (mToken.kind != Token::WORD) {
          mDiags.error(location(), "expected a place operation or '}'");
          throw Recover();
        }
        parsePlaceOp(placement);
      }
      advance(); // '}'
      return;
    }
    // The one-line form: operations continue while they sit on the
    // `place` keyword's own line.
    while (mToken.kind == Token::WORD && lineOf(mToken) == placeLine)
      parsePlaceOp(placement);
  }

  // One operation of a `place`: a transform, or a `material <from> =
  // <to>` override. The operations an asset declaration takes are
  // pointed back at it.
  void parsePlaceOp(LayoutPlacement &placement) {
    const auto op{mToken.text};
    const auto opLoc{location()};
    advance();
    if (op == "select" || op == "recenter" || op == "subdivide" ||
        op == "displace" || op == "tube" || op == "ribbon" ||
        op == "radius_scale") {
      mDiags.error(opLoc,
                   smdl::concat(smdl::Quoted(op),
                                " is a property of what is loaded, so it "
                                "belongs on the 'asset' declaration"));
      throw Recover();
    }
    if (op == "at") {
      mDiags.error(opLoc, "'at' blocks were retired with the '.scene' format")
          .note({}, "write one top-level 'place' per instance instead");
      throw Recover();
    }
    if (op == "variant") {
      // One entry of a bulk place's override table, picked by the
      // buffer's per-record variant indices in order of appearance.
      if (placement.placesPath.empty()) {
        mDiags.error(opLoc,
                     "'variant' belongs to a bulk place: 'place <name> * "
                     "\"<file>\" { variant { ... } }'");
        throw Recover();
      }
      if (mToken.kind != Token::OPEN) {
        mDiags.error(location(), "expected '{' after 'variant'");
        throw Recover();
      }
      advance(); // '{'
      auto &variant{placement.variants.emplace_back()};
      while (mToken.kind != Token::CLOSE) {
        if (mToken.kind == Token::END) {
          mDiags.error(location(), "expected '}' before end of file");
          throw Recover();
        }
        if (mToken.kind != Token::WORD || mToken.text != "material") {
          mDiags.error(location(), "a variant holds 'material <from> = <to>' "
                                   "overrides and nothing else");
          throw Recover();
        }
        const auto pairLoc{location()};
        advance();
        std::string from{};
        if (mToken.kind == Token::WORD || mToken.kind == Token::STRING) {
          from = mToken.text;
          advance();
        } else {
          mDiags.error(pairLoc, "expected a material name after 'material'");
          throw Recover();
        }
        if (mToken.kind != Token::EQUALS) {
          mDiags.error(pairLoc,
                       "a variant override renames one material: write "
                       "'material <from> = <to>'");
          throw Recover();
        }
        advance(); // '='
        auto to{expect(Token::WORD, "an MDL material name after '='")};
        if (!variant.try_emplace(from, std::move(to)).second) {
          mDiags.error(pairLoc,
                       smdl::concat("the material ", smdl::Quoted(from),
                                    " is overridden twice in one variant"));
          throw Recover();
        }
      }
      advance(); // '}'
      return;
    }
    if (op == "caster") {
      parseMarkOverride(placement.casterOverride, placement.casterLoc, opLoc,
                        "caster", "place");
      return;
    }
    if (op == "light") {
      parseMarkOverride(placement.lightOverride, placement.lightLoc, opLoc,
                        "light", "place");
      return;
    }
    if (op == "material") {
      // Only the pair form: a place override renames one resolved name.
      // Whole-target assignment belongs to the asset declaration, where
      // it can speak about slots.
      std::string from{};
      if (mToken.kind == Token::WORD || mToken.kind == Token::STRING) {
        from = mToken.text;
        advance();
      } else {
        mDiags.error(opLoc, "expected a material name after "
                            "'material'");
        throw Recover();
      }
      if (mToken.kind != Token::EQUALS) {
        mDiags
            .error(opLoc, "a place override renames one material: write "
                          "'material <from> = <to>'")
            .note({}, "to shade every slot at once, use 'material <name>' "
                      "on the 'asset' declaration");
        throw Recover();
      }
      advance(); // '='
      auto to{expect(Token::WORD, "an MDL material name after '='")};
      if (!placement.overrides.try_emplace(from, std::move(to)).second) {
        mDiags.error(opLoc, smdl::concat("the material ", smdl::Quoted(from),
                                         " is overridden twice in one place"));
        throw Recover();
      }
      return;
    }
    if (!parseTransformOp(op, opLoc, placement.transform)) {
      mDiags.error(opLoc,
                   smdl::concat("unknown place operation ", smdl::Quoted(op),
                                " (expected material, variant, caster, "
                                "light, translate, scale, rotate, rotate_x, "
                                "rotate_y, rotate_z, or matrix)"));
      throw Recover();
    }
  }

  // The `<word>` / `<word> off` override of a place or an import, for
  // the `caster` and `light` marks alike. Operation names are never
  // `off`, so peeking for it is unambiguous.
  void parseMarkOverride(std::optional<bool> &mark, LayoutLocation &markLoc,
                         const LayoutLocation &opLoc, std::string_view word,
                         std::string_view where) {
    if (mark) {
      mDiags.error(opLoc,
                   smdl::concat("'", word, "' appears twice in one ", where));
      throw Recover();
    }
    markLoc = opLoc;
    if (mToken.kind == Token::WORD && mToken.text == "off") {
      mark = false;
      advance();
    } else {
      mark = true;
    }
  }

  void parseImport() {
    advance();
    auto &placement{mDocument.placements.emplace_back()};
    placement.kind = LayoutPlacement::Kind::IMPORT;
    placement.importPathLoc = location();
    placement.importPath =
        expect(Token::STRING, "a quoted path after 'import'");
    if (mToken.kind != Token::OPEN) return;
    advance(); // '{'
    while (mToken.kind != Token::CLOSE) {
      if (mToken.kind == Token::END) {
        mDiags.error(location(), "expected '}' before end of file");
        throw Recover();
      }
      if (mToken.kind != Token::WORD) {
        mDiags.error(location(), "expected an import operation or '}'");
        throw Recover();
      }
      const auto op{mToken.text};
      const auto opLoc{location()};
      advance();
      if (op == "select" || op == "recenter" || op == "subdivide" ||
          op == "displace" || op == "tube" || op == "ribbon" ||
          op == "radius_scale") {
        mDiags
            .error(opLoc,
                   smdl::concat(smdl::Quoted(op),
                                " is a property of what is loaded, so it "
                                "belongs on an 'asset' declaration"))
            .note({}, "declare 'asset <name> = \"<path>\" { ... }' and "
                      "'place' it instead");
        throw Recover();
      }
      if (op == "at") {
        mDiags
            .error(opLoc, "'at' blocks were retired with the '.scene' "
                          "format")
            .note({}, "write one top-level 'place' per instance instead");
        throw Recover();
      }
      if (op == "material") {
        parseMaterialOps(placement.importMaterials, "import");
      } else if (op == "caster") {
        parseMarkOverride(placement.casterOverride, placement.casterLoc, opLoc,
                          "caster", "import");
      } else if (op == "light") {
        parseMarkOverride(placement.lightOverride, placement.lightLoc, opLoc,
                          "light", "import");
      } else if (!parseTransformOp(op, opLoc, placement.transform)) {
        mDiags.error(opLoc,
                     smdl::concat("unknown import operation ", smdl::Quoted(op),
                                  " (expected material, caster, light, "
                                  "translate, scale, rotate, rotate_x, "
                                  "rotate_y, rotate_z, or matrix)"));
        throw Recover();
      }
    }
    advance(); // '}'
  }

  // The `material` operation of an asset or import block, in either of
  // its two forms: `material x` shades every slot, `material "Slot" = x`
  // shades one. Unambiguous by what follows: a slot is quoted because it
  // is a name out of the mesh file, an MDL material name is bare.
  void parseMaterialOps(MaterialAssignment &materials, std::string_view where) {
    const auto opLoc{location()};
    if (mToken.kind == Token::STRING) {
      auto slot{mToken.text};
      advance();
      expect(Token::EQUALS, "'=' after the material slot name");
      auto target{expect(Token::WORD, "an MDL material name after '='")};
      if (!materials.bySlot.try_emplace(slot, std::move(target)).second) {
        mDiags.error(opLoc,
                     smdl::concat("the material slot ", smdl::Quoted(slot),
                                  " is assigned twice in one ", where));
        throw Recover();
      }
      return;
    }
    if (mToken.kind != Token::WORD) {
      mDiags.error(opLoc, "expected an MDL material name, or a quoted material "
                          "slot name, after 'material'");
      throw Recover();
    }
    if (!materials.all.empty()) {
      mDiags.error(opLoc, smdl::concat("'material' assigns the whole ", where,
                                       " twice"));
      throw Recover();
    }
    materials.all = mToken.text;
    advance();
  }

  void parseSubdivide(SubdivSpec &subdiv, const LayoutLocation &opLoc) {
    // Once per asset: a second 'subdivide' is far more likely a
    // conflicting edit than an intentional override.
    if (subdiv.levels > 0) {
      mDiags.error(opLoc, "'subdivide' appears twice in one asset");
      throw Recover();
    }
    const float levels{numbers<1>()[0]};
    if (!(levels >= 1 && levels <= 8 && levels == float(int(levels)))) {
      mDiags.error(opLoc, "expected an integer subdivision level from 1 "
                          "to 8 after 'subdivide'");
      throw Recover();
    }
    subdiv.levels = uint32_t(levels);
    // Optional trailing words, in either order and independent of each
    // other: 'loop' selects the triangle split, and 'linear' turns
    // smoothing off. Operation names are never numbers, and no operation
    // is called 'loop' or 'linear', so peeking is unambiguous.
    auto sawLoop{false};
    auto sawLinear{false};
    while (mToken.kind == Token::WORD &&
           (mToken.text == "loop" || mToken.text == "linear")) {
      auto &saw{mToken.text == "loop" ? sawLoop : sawLinear};
      if (saw) {
        mDiags.error(location(),
                     smdl::concat(smdl::Quoted(mToken.text),
                                  " appears twice in one 'subdivide'"));
        throw Recover();
      }
      saw = true;
      advance();
    }
    if (sawLoop) subdiv.scheme = SubdivSpec::Scheme::LOOP;
    if (sawLinear) subdiv.smooth = false;
  }

  void parseAlias() {
    advance();
    auto name{expect(Token::STRING, "a quoted material name after "
                                    "'material'")};
    expect(Token::EQUALS, "'=' after the material name");
    auto target{expect(Token::WORD, "an MDL material name after '='")};
    // Last one wins within the file; the scope ends at the file.
    mDocument.materialAliases.insert_or_assign(std::move(name),
                                               std::move(target));
  }

  void parseMedium() {
    const auto opLoc{location()};
    advance();
    // Last one wins within the file, like the 'material' aliasing.
    mDocument.mediumName =
        expect(Token::WORD, "an MDL material name after 'medium'");
    mDocument.mediumLoc = opLoc;
  }

  // A `camera { ... }` block. Last one wins per field within the file; a
  // field no directive names is left unset for the command line, or
  // failing that the built-in default, to fill in.
  void parseCamera() {
    if (!mDocument.cameraLoc) mDocument.cameraLoc = location();
    advance();
    if (mToken.kind != Token::OPEN) {
      mDiags.error(location(), "expected '{' after 'camera'");
      throw Recover();
    }
    advance(); // '{'
    auto &camera{mDocument.camera};
    while (mToken.kind != Token::CLOSE) {
      if (mToken.kind == Token::END) {
        mDiags.error(location(), "expected '}' before end of file");
        throw Recover();
      }
      if (mToken.kind != Token::WORD) {
        mDiags.error(location(), "expected a camera setting or '}'");
        throw Recover();
      }
      const auto key{mToken.text};
      const auto keyLoc{location()};
      advance();
      if (key == "resolution") {
        auto v{numbers<2>()};
        if (!(v[0] >= 1 && v[1] >= 1)) {
          mDiags.error(keyLoc,
                       "expected two positive integers for 'resolution'");
          throw Recover();
        }
        camera.resolution = int2(int(v[0]), int(v[1]));
      } else if (key == "look_from") {
        auto v{numbers<3>()};
        camera.lookFrom = float3(v[0], v[1], v[2]);
      } else if (key == "look_to") {
        auto v{numbers<3>()};
        camera.lookTo = float3(v[0], v[1], v[2]);
      } else if (key == "look_up") {
        auto v{numbers<3>()};
        camera.lookUp = float3(v[0], v[1], v[2]);
      } else if (key == "fovy") {
        camera.fovYDeg = positive(keyLoc, key, numbers<1>()[0]);
      } else if (key == "fstop") {
        camera.fStop = positive(keyLoc, key, numbers<1>()[0]);
      } else if (key == "aperture") {
        camera.aperture = positive(keyLoc, key, numbers<1>()[0]);
      } else if (key == "focus") {
        camera.focus = positive(keyLoc, key, numbers<1>()[0]);
      } else if (key == "blades") {
        camera.blades = int(numbers<1>()[0]);
      } else if (key == "blade_angle") {
        camera.bladeAngleDeg = numbers<1>()[0];
      } else if (key == "distortion_k1") {
        camera.distortionK1 = numbers<1>()[0];
      } else if (key == "distortion_k2") {
        camera.distortionK2 = numbers<1>()[0];
      } else if (key == "distortion_fit") {
        // A bare keyword, like 'recenter', since the flag it mirrors
        // takes no value either.
        camera.distortionFit = true;
      } else if (key == "vignetting") {
        camera.vignetting = numbers<1>()[0];
      } else if (key == "cat_eye") {
        camera.catEye = numbers<1>()[0];
      } else if (key == "cat_eye_radius") {
        camera.catEyeRadius = positive(keyLoc, key, numbers<1>()[0]);
      } else if (key == "motion") {
        parseCameraMotion(camera);
      } else {
        mDiags.error(
            keyLoc,
            smdl::concat("unknown camera setting ", smdl::Quoted(key),
                         " (expected resolution, look_from, look_to, look_up, "
                         "fovy, fstop, aperture, focus, blades, blade_angle, "
                         "distortion_k1, distortion_k2, distortion_fit, "
                         "vignetting, cat_eye, cat_eye_radius, or motion)"));
        throw Recover();
      }
    }
    advance(); // '}'
  }

  // The `motion { ... }` block inside `camera`: the framing at shutter
  // close, merged per field like the block around it.
  void parseCameraMotion(LayoutCamera &camera) {
    if (mToken.kind != Token::OPEN) {
      mDiags.error(location(), "expected '{' after 'motion'");
      throw Recover();
    }
    advance(); // '{'
    if (!camera.motion) camera.motion.emplace();
    auto &motion{*camera.motion};
    while (mToken.kind != Token::CLOSE) {
      if (mToken.kind == Token::END) {
        mDiags.error(location(), "expected '}' before end of file");
        throw Recover();
      }
      if (mToken.kind != Token::WORD) {
        mDiags.error(location(), "expected a camera motion setting or '}'");
        throw Recover();
      }
      const auto key{mToken.text};
      const auto keyLoc{location()};
      advance();
      if (key == "look_from") {
        auto v{numbers<3>()};
        motion.lookFrom = float3(v[0], v[1], v[2]);
      } else if (key == "look_to") {
        auto v{numbers<3>()};
        motion.lookTo = float3(v[0], v[1], v[2]);
      } else if (key == "look_up") {
        auto v{numbers<3>()};
        motion.lookUp = float3(v[0], v[1], v[2]);
      } else {
        mDiags.error(keyLoc, smdl::concat("unknown camera motion setting ",
                                          smdl::Quoted(key),
                                          " (expected look_from, look_to, "
                                          "or look_up)"));
        throw Recover();
      }
    }
    advance(); // '}'
  }

  // A `sky { ... }` block, merged per field like `camera`.
  void parseSky() {
    if (!mDocument.skyLoc) mDocument.skyLoc = location();
    advance();
    if (mToken.kind != Token::OPEN) {
      mDiags.error(location(), "expected '{' after 'sky'");
      throw Recover();
    }
    advance(); // '{'
    auto &sky{mDocument.sky};
    while (mToken.kind != Token::CLOSE) {
      if (mToken.kind == Token::END) {
        mDiags.error(location(), "expected '}' before end of file");
        throw Recover();
      }
      if (mToken.kind != Token::WORD) {
        mDiags.error(location(), "expected a sky setting or '}'");
        throw Recover();
      }
      const auto key{mToken.text};
      const auto keyLoc{location()};
      advance();
      if (key == "none") {
        // A bare keyword, like 'recenter': there is nothing to say about
        // an environment that is not there.
        sky.none = true;
      } else if (key == "sun_zenith") {
        sky.sunZenith = numbers<1>()[0];
      } else if (key == "sun_azimuth") {
        sky.sunAzimuth = numbers<1>()[0];
      } else if (key == "visibility") {
        sky.visibility = positive(keyLoc, key, numbers<1>()[0]);
      } else if (key == "water_vapor") {
        sky.waterVapor = positive(keyLoc, key, numbers<1>()[0]);
      } else if (key == "scale") {
        sky.scale = positive(keyLoc, key, numbers<1>()[0]);
      } else if (key == "moon") {
        sky.moonPhase = numbers<1>()[0];
      } else if (key == "moon_distance") {
        sky.moonDistance = positive(keyLoc, key, numbers<1>()[0]);
      } else if (key == "ibl") {
        // Recorded as written; the lowering resolves it like any other
        // path the layout names, so an environment map can live in the
        // asset library too.
        sky.iblFileName.reset();
        mDocument.iblPathLoc = location();
        mDocument.iblPath =
            expect(Token::STRING, "a quoted file name after 'ibl'");
      } else if (key == "ibl_scale") {
        sky.iblScale = positive(keyLoc, key, numbers<1>()[0]);
      } else {
        mDiags.error(keyLoc,
                     smdl::concat("unknown sky setting ", smdl::Quoted(key),
                                  " (expected none, sun_zenith, sun_azimuth, "
                                  "visibility, water_vapor, scale, moon, "
                                  "moon_distance, ibl, or ibl_scale)"));
        throw Recover();
      }
    }
    advance(); // '}'
  }

  // A `haze { ... }` block, merged per field like `sky`. Writing the
  // block at all is what turns the haze on, so an empty one is the
  // default atmosphere rather than a statement of nothing.
  void parseHaze() {
    if (!mDocument.hazeLoc) mDocument.hazeLoc = location();
    advance();
    if (mToken.kind != Token::OPEN) {
      mDiags.error(location(), "expected '{' after 'haze'");
      throw Recover();
    }
    advance(); // '{'
    auto &haze{mDocument.haze};
    while (mToken.kind != Token::CLOSE) {
      if (mToken.kind == Token::END) {
        mDiags.error(location(), "expected '}' before end of file");
        throw Recover();
      }
      if (mToken.kind != Token::WORD) {
        mDiags.error(location(), "expected a haze setting or '}'");
        throw Recover();
      }
      const auto key{mToken.text};
      const auto keyLoc{location()};
      advance();
      if (key == "none") {
        haze.none = true;
      } else if (key == "visibility") {
        haze.visibility = positive(keyLoc, key, numbers<1>()[0]);
      } else if (key == "scale_height") {
        haze.scaleHeight = positive(keyLoc, key, numbers<1>()[0]);
      } else if (key == "base_height") {
        haze.baseHeight = numbers<1>()[0];
      } else if (key == "droplet") {
        haze.droplet = positive(keyLoc, key, numbers<1>()[0]);
      } else {
        mDiags.error(keyLoc,
                     smdl::concat("unknown haze setting ", smdl::Quoted(key),
                                  " (expected none, visibility, scale_height, "
                                  "base_height, or droplet)"));
        throw Recover();
      }
    }
    advance(); // '}'
  }

  // A `time { ... }` block, merged per field like `sky`.
  void parseTime() {
    if (!mDocument.timeLoc) mDocument.timeLoc = location();
    advance();
    if (mToken.kind != Token::OPEN) {
      mDiags.error(location(), "expected '{' after 'time'");
      throw Recover();
    }
    advance(); // '{'
    auto &time{mDocument.time};
    while (mToken.kind != Token::CLOSE) {
      if (mToken.kind == Token::END) {
        mDiags.error(location(), "expected '}' before end of file");
        throw Recover();
      }
      if (mToken.kind != Token::WORD) {
        mDiags.error(location(), "expected a time setting or '}'");
        throw Recover();
      }
      const auto key{mToken.text};
      const auto keyLoc{location()};
      advance();
      if (key == "base") {
        time.base = finite(keyLoc, key, numbers<1>()[0]);
      } else if (key == "shutter") {
        const auto value{finite(keyLoc, key, numbers<1>()[0])};
        if (!(value >= 0)) {
          mDiags.error(keyLoc, "expected a nonnegative number for 'shutter' "
                               "(0 or omitted is a closed shutter)");
          throw Recover();
        }
        time.shutter = value;
      } else {
        mDiags.error(keyLoc,
                     smdl::concat("unknown time setting ", smdl::Quoted(key),
                                  " (expected base or shutter)"));
        throw Recover();
      }
    }
    advance(); // '}'
  }

  // Check a setting whose zero means "unset" everywhere downstream, so
  // that writing it down has to mean something.
  [[nodiscard]] float positive(const LayoutLocation &keyLoc,
                               std::string_view key, float value) {
    if (!(value > 0)) {
      mDiags.error(keyLoc, smdl::concat("expected a positive number for ",
                                        smdl::Quoted(key),
                                        " (omit it to leave it unset)"));
      throw Recover();
    }
    return value;
  }

  // Check a setting that is a clock reading, which the number syntax
  // alone would let be infinite or not a number.
  [[nodiscard]] float finite(const LayoutLocation &keyLoc, std::string_view key,
                             float value) {
    if (!std::isfinite(value)) {
      mDiags.error(keyLoc, smdl::concat("expected a finite number for ",
                                        smdl::Quoted(key)));
      throw Recover();
    }
    return value;
  }

  // One transform operation, applied on the LEFT of `xf` so that it
  // takes effect after everything above it. Returns false if `op` names
  // no transform operation, leaving the caller to decide what that
  // means.
  [[nodiscard]] bool parseTransformOp(const std::string &op,
                                      const LayoutLocation &opLoc,
                                      float4x4 &xf) {
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
          mDiags.error(opLoc,
                       "expected one number for a uniform 'scale' or three "
                       "for a non-uniform one");
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

  // Does the token spell a number, in full? An operation name never
  // does, which is what lets a directive take a variable count of them.
  [[nodiscard]] static bool isNumber(const Token &token) {
    float ignored{};
    return tryNumber(token, ignored);
  }

  [[nodiscard]] static bool tryNumber(const Token &token, float &value) {
    if (token.kind != Token::WORD) return false;
    size_t used{};
    try {
      value = std::stof(token.text, &used);
    } catch (const std::exception &) {
      return false;
    }
    return used == token.text.size();
  }

  template <size_t N> [[nodiscard]] std::array<float, N> numbers() {
    std::array<float, N> values{};
    for (size_t i = 0; i < N; i++) {
      if (mToken.kind != Token::WORD) {
        mDiags.error(
            location(),
            smdl::concat("expected ", N, " number(s), got ", i, " of them"));
        throw Recover();
      }
      if (!tryNumber(mToken, values[i])) {
        mDiags.error(location(), smdl::concat("expected a number, got ",
                                              smdl::Quoted(mToken.text)));
        throw Recover();
      }
      advance();
    }
    return values;
  }

  std::string expect(Token::Kind kind, std::string_view what) {
    if (mToken.kind != kind) {
      mDiags.error(location(), smdl::concat("expected ", what));
      throw Recover();
    }
    auto text{mToken.text};
    advance();
    return text;
  }

  [[nodiscard]] LayoutLocation location() const noexcept {
    return {&mSource, mToken.offset, std::max(mToken.length, uint32_t(1))};
  }

  void advance() { mToken = mLexer.next(); }

  [[nodiscard]] static bool isIdentifier(std::string_view name) {
    if (name.empty() || !(smdl::isAlpha(name[0]) || name[0] == '_'))
      return false;
    for (const char ch : name)
      if (!smdl::isWord(ch)) return false;
    return true;
  }

  // The location of an existing declaration of `name` across the shared
  // asset/group/light namespace, or an invalid location. `skip` is the
  // entry being built, excluded from the search.
  [[nodiscard]] LayoutLocation findDeclaration(std::string_view name,
                                               const void *skip) const {
    for (const auto &asset : mDocument.assets)
      if (&asset != skip && asset.name == name) return asset.nameLoc;
    for (const auto &group : mDocument.groups)
      if (&group != skip && group.name == name) return group.nameLoc;
    for (const auto &light : mDocument.lights)
      if (&light != skip && light.name == name) return light.nameLoc;
    return {};
  }

  // The 1-based line a token sits on, which is what decides where a
  // one-line 'place' ends.
  [[nodiscard]] uint32_t lineOf(const Token &token) const noexcept {
    return mSource.lineAndColumn(token.offset).lineNo;
  }

  [[nodiscard]] static float4x4 translation(float x, float y, float z) {
    return float4x4{float4{1, 0, 0, 0}, float4{0, 1, 0, 0}, float4{0, 0, 1, 0},
                    float4{x, y, z, 1}};
  }

  [[nodiscard]] static float4x4 rotation(float3 axis, float degrees) {
    if (!smdl::tryNormalize(axis)) return float4x4(1.0f);
    const float radians{smdl::radians(degrees)};
    const float c{std::cos(radians)}, s{std::sin(radians)}, t{1.0f - c};
    return float4x4{
        float4{t * axis.x * axis.x + c, t * axis.x * axis.y + s * axis.z,
               t * axis.x * axis.z - s * axis.y, 0},
        float4{t * axis.x * axis.y - s * axis.z, t * axis.y * axis.y + c,
               t * axis.y * axis.z + s * axis.x, 0},
        float4{t * axis.x * axis.z + s * axis.y,
               t * axis.y * axis.z - s * axis.x, t * axis.z * axis.z + c, 0},
        float4{0, 0, 0, 1}};
  }

  LayoutDiagnostics &mDiags;
  const LayoutSource &mSource;
  Lexer mLexer;
  Token mToken{};
  LayoutDocument &mDocument;

  // Every `as` name seen, for the duplicate warning: identity is only
  // worth recording if it names one thing.
  std::map<std::string, LayoutLocation, std::less<>> mAsNames{};
};

} // namespace

LayoutDocument parseLayout(LayoutDiagnostics &diags, const LayoutSource &source,
                           std::string directory) {
  auto document{LayoutDocument()};
  document.source = &source;
  document.directory = std::move(directory);
  Parser(diags, source, document).parse();
  return document;
}
