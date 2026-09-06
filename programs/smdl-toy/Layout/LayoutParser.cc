#include "Layout/Layout.h"

#include "Layout/TextParser.h"

#include "smdl/Support/Strings.h"

#include <algorithm>
#include <array>
#include <map>

// The layout format's own vocabulary: the statements, the declarations,
// and the operations each block admits. The lexer and every helper that
// speaks about the language rather than about this format live in
// `TextParser.h`, which the `.camera` format derives from too. Path
// resolution, imports, and lowering live in `Layout.cc`, and everything
// here stays free of the filesystem so that a document parses the same
// from disk or from a test's string.

namespace {

// The top-level keywords, which are the synchronization points.
constexpr std::array<std::string_view, 9> TOP_LEVEL_KEYWORDS{
    "asset",    "group",  "place", "import", "light",
    "material", "medium", "sky",   "haze"};

// The directive a `.camera` file owns, kept here only so that writing
// one in a layout is answered with where it belongs rather than with a
// spelling suggestion.
constexpr std::string_view CAMERA_FILE_KEYWORD{"camera"};

class Parser final : public TextParser {
public:
  Parser(LayoutDiagnostics &diags, const LayoutSource &source,
         LayoutDocument &document)
      : TextParser(diags, source, LAYOUT_MAGIC, "layout file",
                   TOP_LEVEL_KEYWORDS),
        mDocument(document) {}

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
    } else if (mToken.text == "sky") {
      parseSky();
    } else if (mToken.text == "haze") {
      parseHaze();
    } else {
      auto &error{
          mDiags.error(location(), smdl::concat("unknown directive ",
                                                smdl::Quoted(mToken.text)))};
      if (mToken.text == CAMERA_FILE_KEYWORD) {
        error.note({}, "'camera' belongs in a '.camera' file, which the render "
                       "finds beside the layout or takes from '-camera'");
      } else if (mToken.text == "time") {
        error.note({}, "the clock is nobody's file: '-time' names the instant "
                       "to photograph, and 'shutter' inside a '.camera' file's "
                       "'camera' block says how long the shutter stays open");
      } else if (std::find(TRANSFORM_OPS.begin(), TRANSFORM_OPS.end(),
                           mToken.text) != TRANSFORM_OPS.end()) {
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
    parseSettings("an asset operation", [&](const std::string &op,
                                            const LayoutLocation &opLoc) {
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
          decl.subdiv.isDisplaced = true;
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
          (op == "radius" ? decl.primitive.radius : decl.primitive.height) =
              positive(opLoc, op, numbers<1>()[0]);
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
          decl.curves.radiusScale = positive(opLoc, op, numbers<1>()[0]);
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
      } else if (op == "animation") {
        parseAssetAnimation(decl, opLoc);
      } else if (op == "material") {
        parseMaterialOps(decl.materials, "asset");
      } else if (op == "caster") {
        decl.isCaster = true;
        decl.casterLoc = opLoc;
      } else if (op == "light") {
        decl.isLight = true;
        decl.lightLoc = opLoc;
      } else if (op == "caustic") {
        decl.isCaustic = true;
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
                                        "radius_scale, animation, material, "
                                        "caster, light, translate, scale, "
                                        "rotate, rotate_x, rotate_y, "
                                        "rotate_z, or matrix)"));
        throw Recover();
      }
    });
  }

  // The `animation` operation of an asset: a clip by quoted name or by
  // bare index, then `offset <seconds>`, `speed <factor>`, and `once` in
  // any order; or the single word `off`. The settings end at the first
  // word that is none of these, which is the next asset operation, so
  // `animation` alone is legal and means the file's only clip.
  void parseAssetAnimation(LayoutAssetDecl &decl, const LayoutLocation &opLoc) {
    if (decl.primitive.active()) {
      mDiags.error(opLoc, smdl::concat("'animation' applies to a mesh file, "
                                       "but this asset is a ",
                                       decl.primitive.name()));
      throw Recover();
    }
    if (decl.animationLoc) {
      mDiags.error(opLoc, "'animation' appears twice in one asset")
          .note(decl.animationLoc, "first written here");
      throw Recover();
    }
    decl.animationLoc = opLoc;
    if (mToken.kind == Token::OPEN) {
      mDiags
          .error(location(), "'animation' takes a clip and settings, not a "
                             "block")
          .note({}, "write 'animation \"<clip>\" offset <seconds> speed "
                    "<factor> once', or 'animation off'");
      throw Recover();
    }
    auto &spec{decl.animation};
    const auto isSetting{[&] {
      return mToken.kind == Token::STRING ||
             (mToken.kind == Token::WORD &&
              (mToken.text == "offset" || mToken.text == "speed" ||
               mToken.text == "once" || mToken.text == "off" ||
               isNumber(mToken)));
    }};
    auto anySetting{false};
    while (isSetting()) {
      const auto settingLoc{location()};
      if (mToken.kind == Token::STRING || isNumber(mToken)) {
        if (spec.hasClip()) {
          mDiags.error(settingLoc, "'animation' names two clips");
          throw Recover();
        }
        if (mToken.kind == Token::STRING) {
          spec.clipName = mToken.text;
        } else {
          float value{};
          (void)tryNumber(mToken, value);
          if (!(value >= 0 && value == std::floor(value))) {
            mDiags.error(settingLoc,
                         "expected a quoted clip name or an unsigned clip "
                         "index after 'animation'");
            throw Recover();
          }
          spec.clipIndex = uint32_t(value);
        }
        advance();
      } else if (mToken.text == "off") {
        advance();
        if (anySetting || isSetting()) {
          mDiags.error(settingLoc, "'animation off' takes no clip and no "
                                   "settings");
          throw Recover();
        }
        spec.off = true;
        return;
      } else if (mToken.text == "once") {
        spec.once = true;
        advance();
      } else {
        const auto setting{mToken.text};
        advance();
        const auto value{finite(settingLoc, setting, numbers<1>()[0])};
        if (setting == "speed" && value == 0) {
          mDiags.error(settingLoc, "'speed' must be nonzero");
          throw Recover();
        }
        (setting == "offset" ? spec.offset : spec.speed) = value;
      }
      anySetting = true;
    }
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
    const auto isSpot{decl.kind == LayoutLightDecl::Kind::SPOT};
    const auto isProfile{decl.kind == LayoutLightDecl::Kind::PROFILE};
    const auto isRect{decl.kind == LayoutLightDecl::Kind::RECT};
    const auto isDisk{decl.kind == LayoutLightDecl::Kind::DISK};
    parseSettings("a light setting", [&](const std::string &op,
                                         const LayoutLocation &opLoc) {
      if (op == "power") {
        decl.power = positive(opLoc, op, numbers<1>()[0]);
        decl.powerSet = true;
      } else if (op == "temperature") {
        decl.temperature = positive(opLoc, op, numbers<1>()[0]);
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
        decl.scale = positive(opLoc, op, numbers<1>()[0]);
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
        decl.radius = positive(opLoc, op, numbers<1>()[0]);
      } else if (op == "caustic") {
        decl.isCaustic = true;
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
    });
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
        op == "radius_scale" || op == "animation") {
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
    if (op == "motion") {
      parsePlaceMotion(placement, opLoc);
      return;
    }
    if (op == "offset") {
      parsePlaceOffset(placement, opLoc);
      return;
    }
    if (!parseTransformOp(op, opLoc, placement.transform)) {
      mDiags.error(opLoc,
                   smdl::concat("unknown place operation ", smdl::Quoted(op),
                                " (expected material, variant, caster, "
                                "light, motion, offset, translate, scale, "
                                "rotate, rotate_x, rotate_y, rotate_z, or "
                                "matrix)"));
      throw Recover();
    }
  }

  // The `offset <seconds>` operation of a place: seconds added to the
  // render clock of everything the placement places. One per place; a
  // second is an error rather than a sum, since the value is one number.
  void parsePlaceOffset(LayoutPlacement &placement,
                        const LayoutLocation &opLoc) {
    if (placement.animationOffset) {
      mDiags.error(opLoc, "'offset' appears twice in one place")
          .note(placement.animationOffsetLoc, "first written here");
      throw Recover();
    }
    const auto value{numbers<1>()[0]};
    if (!std::isfinite(value)) {
      mDiags.error(opLoc, "expected a finite number of seconds for 'offset'");
      throw Recover();
    }
    placement.animationOffset = value;
    placement.animationOffsetLoc = opLoc;
  }

  // The `motion { at <seconds> ... }` block of a place: a track of keys
  // at absolute times, each accumulating its own transform from identity
  // exactly as the place's own operations do. One block per place; a
  // second is an error rather than a merge, since the keys are one
  // track.
  void parsePlaceMotion(LayoutPlacement &placement,
                        const LayoutLocation &opLoc) {
    if (!placement.motion.empty()) {
      mDiags.error(opLoc, "'motion' appears twice in one place")
          .note(placement.motionLoc, "first written here");
      throw Recover();
    }
    if (mToken.kind != Token::OPEN) {
      mDiags.error(location(), "expected '{' after 'motion'");
      throw Recover();
    }
    advance(); // '{'
    auto &track{placement.motion};
    while (mToken.kind != Token::CLOSE) {
      if (mToken.kind == Token::END) {
        mDiags.error(location(), "expected '}' before end of file");
        throw Recover();
      }
      if (mToken.kind != Token::WORD) {
        mDiags.error(location(), "expected 'at' or a transform operation");
        throw Recover();
      }
      const auto word{mToken.text};
      const auto wordLoc{location()};
      advance();
      if (word == "at") {
        const auto time{finite(wordLoc, "at", numbers<1>()[0])};
        if (!track.keys.empty() && !(time > track.keys.back().time)) {
          mDiags.error(wordLoc, "the keys of a 'motion' block are written in "
                                "ascending time");
          throw Recover();
        }
        track.keys.emplace_back().time = time;
        continue;
      }
      if (track.keys.empty()) {
        mDiags
            .error(wordLoc, "a 'motion' block holds 'at <seconds>' keys, and "
                            "every operation belongs to the key above it")
            .note({}, "write 'motion { at 0 translate 0 0 0 at 1 translate "
                      "1 0 0 }'");
        throw Recover();
      }
      if (!parseTransformOp(word, wordLoc, track.keys.back().transform)) {
        mDiags.error(wordLoc,
                     smdl::concat("expected 'at' or a transform operation "
                                  "inside 'motion', got ",
                                  smdl::Quoted(word),
                                  " (translate, scale, rotate, rotate_x, "
                                  "rotate_y, rotate_z, or matrix)"));
        throw Recover();
      }
    }
    advance(); // '}'
    if (track.keys.empty()) {
      mDiags.error(opLoc, "a 'motion' block holds at least one 'at <seconds>' "
                          "key");
      throw Recover();
    }
    placement.motionLoc = opLoc;
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
          op == "radius_scale" || op == "animation") {
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
      if (op == "motion" || op == "offset") {
        mDiags
            .error(opLoc, smdl::concat(smdl::Quoted(op),
                                       " is a place operation, not an import "
                                       "operation"))
            .note({}, smdl::concat("declare the file as an asset and 'place' "
                                   "it with ",
                                   op == "motion" ? "a 'motion' block"
                                                  : "an 'offset'"));
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
    if (sawLinear) subdiv.isSmooth = false;
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

  // A `sky { ... }` block, merged per field, last one wins.
  void parseSky() {
    if (!mDocument.skyLoc) mDocument.skyLoc = location();
    advance();
    if (mToken.kind != Token::OPEN) {
      mDiags.error(location(), "expected '{' after 'sky'");
      throw Recover();
    }
    auto &sky{mDocument.sky};
    parseSettings("a sky setting", [&](const std::string &key,
                                       const LayoutLocation &keyLoc) {
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
    });
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
    auto &haze{mDocument.haze};
    parseSettings("a haze setting", [&](const std::string &key,
                                        const LayoutLocation &keyLoc) {
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
    });
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
