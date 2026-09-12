#include "Layout/LensFile.h"

#include "Layout/TextParser.h"

#include "smdl/Support/Error.h"
#include "smdl/Support/Strings.h"

#include <algorithm>
#include <array>
#include <cmath>
#include <optional>

// The lens format's own vocabulary, over the syntax core in
// `TextParser.h`. One directive and nothing else: what the light came
// through.

namespace {

// The top-level keywords, which are the synchronization points.
constexpr std::array<std::string_view, 1> TOP_LEVEL_KEYWORDS{"lens"};

// How far a printed 'ior' or 'abbe' beside 'sellmeier' may be from what the
// coefficients give before it reads as a transcription error. It is loose
// enough for any maker's fit against the values it prints beside it, to
// five decimals and to two, and tight enough for a coefficient typed wrong
// in a digit that matters.
constexpr float PRINTED_ND_TOLERANCE = 5e-5f;
constexpr float PRINTED_ABBE_TOLERANCE = 0.06f;

// Is `name` a medium name: a letter, then letters, digits, '-', and '_'?
// That is how makers spell a designation (N-BK7), and `isIdentifier()`
// refuses the hyphen.
[[nodiscard]] bool isMediumName(std::string_view name) {
  if (name.empty() || !smdl::isAlpha(name[0])) return false;
  for (const char ch : name)
    if (!(smdl::isWord(ch) || ch == '-')) return false;
  return true;
}

// A medium name in upper case, the case the catalog spells in, so that
// two names compare the way the catalog matches them: ignoring case.
[[nodiscard]] std::string foldMediumName(std::string_view name) {
  std::string folded{name};
  for (auto &ch : folded)
    if ('a' <= ch && ch <= 'z') ch = char(ch - 'a' + 'A');
  return folded;
}

class Parser final : public TextParser {
public:
  Parser(LayoutDiagnostics &diags, const LayoutSource &source,
         LensDocument &document)
      : TextParser(diags, source, TOP_LEVEL_KEYWORDS), mDocument(document) {}

  void parse() {
    parseStatements([this] { parseStatement(); });
  }

private:
  // A medium the `lens` block defines, kept until the block closes, when
  // the surfaces that name it are known.
  class MediumDefinition final {
  public:
    std::string name{};
    LayoutLocation nameLoc{};
    smdl::OpticalGlass medium{};

    // Does a surface name it?
    bool isUsed{};
  };

  void parseStatement() {
    if (mToken.kind != Token::WORD) {
      mDiags.error(location(), smdl::concat("expected a directive, got ",
                                            smdl::Quoted(mToken.text)));
      throw Recover();
    }
    if (mToken.text == "lens") {
      parseLensBlock();
    } else {
      LayoutDiagnostic &error{
          mDiags.error(location(), smdl::concat("unknown directive ",
                                                smdl::Quoted(mToken.text)))};
      if (mToken.text == "surface" || mToken.text == "stop") {
        error.note({}, "a surface belongs inside the 'lens' block, which "
                       "is what puts it in order with the others");
      } else if (mToken.text == "medium") {
        error.note({}, "a medium is defined inside the 'lens' block, beside "
                       "the surfaces that name it");
      } else if (mToken.text == "glass") {
        error.note({}, "the keyword is 'medium', since a molded polymer and "
                       "an immersion fluid are the same thing to the trace "
                       "as a glass is");
      } else if (mToken.text == "camera" || mToken.text == "sensor") {
        error.note({}, "a lens file describes the lens alone; where the "
                       "picture is taken from and on what sensor belongs "
                       "in the '.camera' file that names this one");
      } else {
        error.note({}, "a lens file holds one 'lens' block, whose "
                       "'surface' and 'stop' entries are the prescription");
      }
      throw Recover();
    }
  }

  // A `lens { ... }` block, which is the whole file. Unlike a camera's,
  // a second one does not merge: the surfaces are a sequence, and two
  // sequences have no meaningful union.
  void parseLensBlock() {
    if (mDocument.lensLoc) {
      mDiags
          .error(location(), "a lens file describes one lens, and this is "
                             "the second 'lens' block")
          .note(mDocument.lensLoc, "the first one is here");
      throw Recover();
    }
    mDocument.lensLoc = location();
    advance();
    if (mToken.kind != Token::OPEN) {
      mDiags.error(location(), "expected '{' after 'lens'");
      throw Recover();
    }
    LensPrescription &lens{mDocument.lens};
    // Where each surface wrote the name of its medium, or nowhere, in
    // step with `lens.surfaces`.
    std::vector<LayoutLocation> mediumLocs{};
    parseSettings("a lens setting", [&](const std::string &key,
                                        const LayoutLocation &keyLoc) {
      if (key == "name") {
        lens.name = expect(Token::STRING, "a quoted name after 'name'");
      } else if (key == "medium") {
        parseMediumDefinition();
      } else if (key == "surface" || key == "stop") {
        const bool isStop{key == "stop"};
        if (lens.surfaces.size() == LENS_MAX_SURFACES) {
          mDiags.error(keyLoc,
                       smdl::concat("expected at most ", LENS_MAX_SURFACES,
                                    " surfaces in a lens, the stop "
                                    "among them"));
          throw Recover();
        }
        if (isStop) {
          if (mStopLoc) {
            mDiags
                .error(keyLoc, "a lens has one aperture stop, and this is "
                               "the second 'stop'")
                .note(mStopLoc, "the first one is here");
            throw Recover();
          }
          mStopLoc = keyLoc;
        }
        LayoutLocation mediumLoc{};
        lens.surfaces.push_back(parseSurface(keyLoc, isStop, mediumLoc));
        mediumLocs.push_back(mediumLoc);
      } else {
        LayoutDiagnostic &error{mDiags.error(
            keyLoc, smdl::concat("unknown lens setting ", smdl::Quoted(key),
                                 " (expected name, medium, surface, or "
                                 "stop)"))};
        if (key == "glass")
          error.note({}, "'glass' is now 'medium', since not every "
                         "refractive medium is glass");
        throw Recover();
      }
    });
    if (!mStopLoc) {
      mDiags
          .error(mDocument.lensLoc, "a lens needs exactly one aperture stop, "
                                    "and this one has none")
          .note({}, "a patent usually leaves the diaphragm out of its "
                    "table; write 'stop { thickness <mm> diameter <mm> }' "
                    "where it sits, between the two surfaces it separates");
      throw Recover();
    }
    resolveMedia(mediumLocs);
  }

  // One `surface { ... }` or `stop { ... }` entry. The two share a body
  // because they are one thing to the trace, a plane or a quadric with a
  // clear aperture; what differs is which keys mean anything, and the
  // stop's are so few that spelling out why each of the others is absent
  // is worth more than a second parser. `mediumLoc` is set to where the
  // entry names its medium, if it does.
  [[nodiscard]] LensSurface parseSurface(const LayoutLocation &surfaceLoc,
                                         bool isStop,
                                         LayoutLocation &mediumLoc) {
    if (mToken.kind != Token::OPEN) {
      mDiags.error(location(),
                   smdl::concat("expected '{' after ",
                                smdl::Quoted(isStop ? "stop" : "surface")));
      throw Recover();
    }
    LensSurface surface{};
    surface.isStop = isStop;
    bool hasDiameter{false};
    LayoutLocation iorLoc{};
    LayoutLocation mediumKeyLoc{};
    parseSettings("a surface setting", [&](const std::string &key,
                                           const LayoutLocation &keyLoc) {
      if (isStop && (key == "radius" || key == "ior" || key == "medium" ||
                     key == "conic" || key == "aspheric")) {
        LayoutDiagnostic &error{mDiags.error(
            keyLoc, smdl::concat(smdl::Quoted(key),
                                 " has no meaning on the aperture stop"))};
        if (key == "ior" || key == "medium") {
          error.note({}, "the stop sits in the space the surface before it "
                         "names, and does not name one of its own");
        } else {
          error.note({}, "the stop is a flat opening, so it has no shape "
                         "to state");
        }
        throw Recover();
      }
      if (key == "radius") {
        surface.radius = finite(keyLoc, key, numbers<1>()[0]);
      } else if (key == "thickness") {
        const float value{finite(keyLoc, key, numbers<1>()[0])};
        if (!(value >= 0)) {
          mDiags
              .error(keyLoc, "expected a nonnegative number for 'thickness' "
                             "(the distance to the next surface)")
              .note({}, "surfaces run front to film, so every step along "
                        "the axis is forward");
          throw Recover();
        }
        surface.thickness = value;
      } else if (key == "ior") {
        const float value{finite(keyLoc, key, numbers<1>()[0])};
        if (!(value >= 1)) {
          mDiags
              .error(keyLoc, smdl::concat("expected 'ior' to be at least 1, "
                                          "got ",
                                          value))
              .note({}, "it is the index of the space after the surface, "
                        "which is 1 for air and is what omitting it means");
          throw Recover();
        }
        surface.medium = smdl::OpticalGlass::constant(value);
        iorLoc = keyLoc;
      } else if (key == "medium") {
        if (mToken.kind == Token::OPEN) {
          mDiags.error(location(), "expected a medium name after 'medium'")
              .note({}, "a surface names its medium; one the catalog lacks "
                        "is defined beside the surfaces, as 'medium NAME "
                        "{ ... }' in the 'lens' block");
          throw Recover();
        }
        mediumLoc = location();
        surface.mediumName = parseMediumName();
        mediumKeyLoc = keyLoc;
      } else if (key == "diameter") {
        const float value{finite(keyLoc, key, numbers<1>()[0])};
        if (!(value > 0)) {
          mDiags.error(keyLoc, "expected a positive number for 'diameter' "
                               "(the clear aperture, in millimeters)");
          throw Recover();
        }
        surface.diameter = value;
        hasDiameter = true;
      } else if (key == "conic") {
        surface.conic = finite(keyLoc, key, numbers<1>()[0]);
      } else if (key == "aspheric") {
        parseAspheric(surface);
      } else {
        LayoutDiagnostic &error{mDiags.error(
            keyLoc,
            smdl::concat("unknown surface setting ", smdl::Quoted(key),
                         isStop ? " (expected thickness or diameter)"
                                : " (expected radius, thickness, ior, "
                                  "medium, diameter, conic, or aspheric)"))};
        if (key == "glass")
          error.note({}, "'glass' is now 'medium', since not every "
                         "refractive medium is glass");
        throw Recover();
      }
    });
    if (iorLoc && mediumKeyLoc) {
      mDiags
          .error(mediumKeyLoc, "'medium' and 'ior' both state the space "
                               "after this surface, and a named medium "
                               "states its own index")
          .note(iorLoc, "'ior' is here");
      throw Recover();
    }
    if (!hasDiameter) {
      mDiags.error(surfaceLoc, "expected 'diameter', which every surface needs")
          .note({}, "it is the clear aperture in millimeters, and it is "
                    "what decides which rays the surface passes");
      throw Recover();
    }
    return surface;
  }

  // The `aspheric` coefficient list, the `r^4` term first, which takes as
  // many numbers as are written. A table that prints its zero terms is
  // common, so all-zero coefficients parse and mean a surface that is
  // exactly its conic.
  void parseAspheric(LensSurface &surface) {
    float value{0.0f};
    while (mToken.kind == Token::WORD && tryNumber(mToken, value)) {
      if (surface.aspheric.size() == LENS_MAX_ASPHERIC_TERMS) {
        mDiags.error(location(),
                     smdl::concat("expected at most ", LENS_MAX_ASPHERIC_TERMS,
                                  " coefficients after 'aspheric'"));
        throw Recover();
      }
      if (!std::isfinite(value)) {
        mDiags.error(location(),
                     "expected a finite number for an 'aspheric' coefficient");
        throw Recover();
      }
      surface.aspheric.push_back(value);
      advance();
    }
    if (surface.aspheric.empty()) {
      mDiags.error(location(), "expected at least one number after 'aspheric'");
      throw Recover();
    }
  }

  // The name after 'medium', on a surface or in a definition.
  [[nodiscard]] std::string parseMediumName() {
    if (mToken.kind != Token::WORD || !isMediumName(mToken.text)) {
      mDiags.error(location(), "expected a medium name after 'medium': a "
                               "letter, then letters, digits, '-', and '_'");
      throw Recover();
    }
    std::string name{mToken.text};
    advance();
    return name;
  }

  // One `medium NAME { ... }` in the `lens` block. The medium is built,
  // and so validated, when its block closes, and whatever it refuses is
  // pointed at its name. Every key takes one number, or one row each of
  // 'sellmeier', and the last one wins, as a surface's do.
  void parseMediumDefinition() {
    MediumDefinition definition{};
    definition.nameLoc = location();
    definition.name = parseMediumName();
    const std::string &name{definition.name};
    if (const smdl::OpticalGlassEntry *entry{smdl::findOpticalGlass(name)}) {
      mDiags
          .error(definition.nameLoc,
                 name == entry->name
                     ? smdl::concat(smdl::Quoted(name), " is a built-in glass")
                     : smdl::concat(smdl::Quoted(name),
                                    " names the built-in glass ",
                                    smdl::Quoted(entry->name)))
          .note({}, "a built-in glass is one definition shared by every "
                    "lens that names it, so a medium of this file's own "
                    "needs a name of its own");
      throw Recover();
    }
    if (const MediumDefinition *first{findDefinition(name)}) {
      mDiags
          .error(definition.nameLoc, smdl::concat("medium ", smdl::Quoted(name),
                                                  " is defined twice"))
          .note(first->nameLoc,
                first->name == name
                    ? "the first definition is here"
                    : smdl::concat("the first definition is here, as ",
                                   smdl::Quoted(first->name),
                                   ", and medium names match ignoring "
                                   "case"));
      throw Recover();
    }
    if (mToken.kind != Token::OPEN) {
      mDiags.error(location(),
                   smdl::concat("expected '{' after 'medium ", name, "'"));
      throw Recover();
    }
    std::optional<float> nd{};
    std::optional<float> abbeNumber{};
    std::optional<float> partialDispersion{};
    LayoutLocation ndLoc{};
    LayoutLocation abbeLoc{};
    LayoutLocation partialLoc{};
    LayoutLocation sellmeierLoc{};
    std::array<float, 3> b{};
    std::array<float, 3> c{};
    parseSettings("a medium setting", [&](const std::string &key,
                                          const LayoutLocation &keyLoc) {
      if (key == "ior") {
        ndLoc = keyLoc;
        nd = finite(keyLoc, key, numbers<1>()[0]);
      } else if (key == "abbe") {
        abbeLoc = keyLoc;
        abbeNumber = finite(keyLoc, key, numbers<1>()[0]);
      } else if (key == "partial_dispersion") {
        partialLoc = keyLoc;
        partialDispersion = finite(keyLoc, key, numbers<1>()[0]);
      } else if (key == "sellmeier") {
        sellmeierLoc = keyLoc;
        parseSellmeier(keyLoc, b, c);
      } else {
        mDiags.error(keyLoc,
                     smdl::concat("unknown medium setting ", smdl::Quoted(key),
                                  " (expected ior, abbe, partial_dispersion, "
                                  "or sellmeier)"));
        throw Recover();
      }
    });
    if (sellmeierLoc) {
      if (partialDispersion) {
        mDiags
            .error(partialLoc,
                   "'partial_dispersion' has no place beside 'sellmeier'")
            .note({}, "the coefficients state the whole dispersion, and "
                      "'ior' and 'abbe' beside them are the printed values "
                      "they are checked against");
        throw Recover();
      }
      if (buildMedium(definition,
                      [&] { return smdl::OpticalGlass::sellmeier(b, c); }))
        throw Recover();
      checkPrinted(definition, nd, ndLoc, abbeNumber, abbeLoc);
    } else if (abbeNumber) {
      if (!nd) {
        mDiags.error(abbeLoc, "'abbe' needs 'ior' beside it, the index at "
                              "the d line the Abbe number is taken against");
        throw Recover();
      }
      const float index{*nd};
      const float number{*abbeNumber};
      if (LayoutDiagnostic * refusal{buildMedium(definition, [&] {
            return smdl::OpticalGlass::abbe(index, number, partialDispersion);
          })}) {
        // When the fit holds on the normal line, the stated partial
        // dispersion is what was refused, and where the line puts it is
        // the number to read it against.
        smdl::OpticalGlass normal{};
        if (partialDispersion && !smdl::catchAndReturnError([&] {
              normal = smdl::OpticalGlass::abbe(index, number);
            }))
          refusal->note(
              partialLoc,
              smdl::concat("the normal line puts a medium of this "
                           "Abbe number at ",
                           smdl::Brief(normal.partialDispersion(), 4)));
        throw Recover();
      }
    } else if (partialDispersion) {
      mDiags.error(partialLoc, "'partial_dispersion' needs 'ior' and 'abbe' "
                               "beside it, since it is a part of the "
                               "dispersion they state");
      throw Recover();
    } else if (nd) {
      mDiags
          .error(definition.nameLoc, smdl::concat("medium ", smdl::Quoted(name),
                                                  " states only an index"))
          .note({}, "an index alone has no dispersion, so write it as 'ior' "
                    "on the surface");
      throw Recover();
    } else {
      mDiags.error(definition.nameLoc,
                   smdl::concat("medium ", smdl::Quoted(name),
                                " needs 'ior' and 'abbe', or 'sellmeier'"));
      throw Recover();
    }
    mMedia.push_back(std::move(definition));
  }

  // The `sellmeier { b B1 B2 B3 c C1 C2 C3 }` block, its rows in the order
  // a maker's datasheet prints them. `{` is checked here, since the block
  // is not optional once 'sellmeier' is written.
  void parseSellmeier(const LayoutLocation &sellmeierLoc,
                      std::array<float, 3> &b, std::array<float, 3> &c) {
    if (mToken.kind != Token::OPEN) {
      mDiags.error(location(), "expected '{' after 'sellmeier'");
      throw Recover();
    }
    const auto row{[&](const LayoutLocation &keyLoc, const std::string &key) {
      std::array<float, 3> values{numbers<3>()};
      for (auto &value : values) value = finite(keyLoc, key, value);
      return values;
    }};
    LayoutLocation bLoc{};
    LayoutLocation cLoc{};
    parseSettings("a row of coefficients", [&](const std::string &key,
                                               const LayoutLocation &keyLoc) {
      if (key == "b") {
        bLoc = keyLoc;
        b = row(keyLoc, key);
      } else if (key == "c") {
        cLoc = keyLoc;
        c = row(keyLoc, key);
      } else {
        mDiags.error(keyLoc,
                     smdl::concat("unknown Sellmeier row ", smdl::Quoted(key),
                                  " (expected b or c)"));
        throw Recover();
      }
    });
    if (!(bLoc && cLoc)) {
      mDiags.error(sellmeierLoc, "expected both rows in 'sellmeier', 'b' "
                                 "and 'c', three coefficients each");
      throw Recover();
    }
  }

  // Build a definition's medium by `factory`, reporting whatever
  // `OpticalGlass` refuses at the definition's name. Returns the refusal,
  // for notes, or null.
  template <typename Factory>
  [[nodiscard]] LayoutDiagnostic *buildMedium(MediumDefinition &definition,
                                              Factory &&factory) {
    const std::optional<smdl::Error> error{
        smdl::catchAndReturnError([&] { definition.medium = factory(); })};
    if (!error) return nullptr;
    return &mDiags.error(definition.nameLoc,
                         smdl::concat("medium ", smdl::Quoted(definition.name),
                                      ": ",
                                      smdl::decapitalized(error->message)));
  }

  // Warn where a printed value stated beside 'sellmeier' disagrees with
  // what the coefficients give, which is how a coefficient typed wrong
  // shows itself.
  void checkPrinted(const MediumDefinition &definition, std::optional<float> nd,
                    const LayoutLocation &ndLoc,
                    std::optional<float> abbeNumber,
                    const LayoutLocation &abbeLoc) {
    const auto warn{[&](const LayoutLocation &keyLoc, std::string_view key,
                        float printed, float computed, int digits) {
      mDiags
          .warn(keyLoc,
                smdl::concat(smdl::Quoted(key), " ",
                             smdl::Brief(printed, digits),
                             " disagrees with the Sellmeier coefficients of "
                             "medium ",
                             smdl::Quoted(definition.name), ", which give ",
                             smdl::Brief(computed, digits)))
          .note({}, "beside 'sellmeier', 'ior' and 'abbe' are the printed "
                    "values the coefficients are checked against, and a "
                    "disagreement is usually a coefficient typed wrong");
    }};
    const smdl::OpticalGlass &medium{definition.medium};
    if (nd && !(std::abs(medium.nd() - *nd) <= PRINTED_ND_TOLERANCE))
      warn(ndLoc, "ior", *nd, medium.nd(), 6);
    if (abbeNumber && !(std::abs(medium.abbeNumber() - *abbeNumber) <=
                        PRINTED_ABBE_TOLERANCE))
      warn(abbeLoc, "abbe", *abbeNumber, medium.abbeNumber(), 4);
  }

  // Give each surface that names a medium its own, once the block has
  // closed and every definition in it is known, so a definition may
  // follow the surface that names it. A file's own medium cannot take a
  // built-in name, so which of the two is searched first decides nothing.
  void resolveMedia(const std::vector<LayoutLocation> &mediumLocs) {
    std::vector<LensSurface> &surfaces{mDocument.lens.surfaces};
    for (size_t i = 0; i < surfaces.size(); i++) {
      LensSurface &surface{surfaces[i]};
      if (surface.mediumName.empty()) continue;
      if (const smdl::OpticalGlassEntry *entry{
              smdl::findOpticalGlass(surface.mediumName)}) {
        surface.medium = entry->glass;
        surface.mediumName = std::string(entry->name);
      } else if (MediumDefinition *
                 definition{findDefinition(surface.mediumName)}) {
        surface.medium = definition->medium;
        surface.mediumName = definition->name;
        definition->isUsed = true;
      } else {
        reportUnknownMedium(surface.mediumName, mediumLocs[i]);
        throw Recover();
      }
    }
    for (const auto &definition : mMedia)
      if (!definition.isUsed)
        mDiags.warn(definition.nameLoc,
                    smdl::concat("medium ", smdl::Quoted(definition.name),
                                 " is defined, and no surface names it"));
  }

  // The file's own definition of `name`, ignoring case, or null.
  [[nodiscard]] MediumDefinition *findDefinition(std::string_view name) {
    const std::string folded{foldMediumName(name)};
    for (auto &definition : mMedia)
      if (foldMediumName(definition.name) == folded) return &definition;
    return nullptr;
  }

  // An unknown medium name, with the nearest known one when a typo is
  // likely, and the catalog's names, which are few enough to list.
  void reportUnknownMedium(const std::string &name,
                           const LayoutLocation &nameLoc) {
    std::vector<std::string_view> names{};
    for (const auto &entry : smdl::opticalGlassCatalog())
      names.push_back(entry.name);
    const size_t numBuiltIn{names.size()};
    for (const auto &definition : mMedia) names.push_back(definition.name);
    LayoutDiagnostic &error{mDiags.error(
        nameLoc, smdl::concat("unknown medium ", smdl::Quoted(name)))};
    // The distance is taken between folded names, since the match ignores
    // case, and the suggestion is spelled as the catalog or the file
    // spells it.
    std::vector<std::string> folded{};
    for (const auto known : names) folded.push_back(foldMediumName(known));
    const std::vector<std::string_view> candidates(folded.begin(),
                                                   folded.end());
    if (const std::string_view nearest{
            smdl::suggestNearest(foldMediumName(name), candidates)};
        !nearest.empty()) {
      const size_t i{
          size_t(std::find(candidates.begin(), candidates.end(), nearest) -
                 candidates.begin())};
      error.note({},
                 smdl::concat("did you mean ", smdl::Quoted(names[i]), "?"));
    }
    error.note({}, smdl::concat("the built-in glasses are ",
                                smdl::join(smdl::Span<const std::string_view>(
                                               names.data(), numBuiltIn),
                                           ", "),
                                "; a 'medium' block in the lens defines any "
                                "other"));
  }

  LensDocument &mDocument;

  // Where the aperture stop was written, which is both the "have we seen
  // one" flag and the note a second one points at.
  LayoutLocation mStopLoc{};

  // The media the `lens` block defines, in file order.
  std::vector<MediumDefinition> mMedia{};
};

} // namespace

size_t LensPrescription::stopIndex() const noexcept {
  for (size_t i = 0; i < surfaces.size(); i++)
    if (surfaces[i].isStop) return i;
  return surfaces.size();
}

bool LensPrescription::isDispersive() const noexcept {
  for (const auto &surface : surfaces)
    if (!surface.isStop && surface.medium.isDispersive()) return true;
  return false;
}

LensDocument parseLens(LayoutDiagnostics &diags, const LayoutSource &source) {
  LensDocument document{};
  document.source = &source;
  Parser(diags, source, document).parse();
  return document;
}

LensDocument readLens(LayoutDiagnostics &diags, const std::string &fileName) {
  return readDocument(diags, fileName, parseLens);
}

std::string resolveLensFileName(const std::string &stated,
                                const std::string &cameraFileName) {
  return resolveSiblingFile(stated, cameraFileName, "lens");
}
