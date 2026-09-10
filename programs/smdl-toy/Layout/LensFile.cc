#include "Layout/LensFile.h"

#include "Layout/TextParser.h"

#include "smdl/Support/Error.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Strings.h"

#include <array>
#include <cmath>
#include <filesystem>

// The lens format's own vocabulary, over the syntax core in
// `TextParser.h`. One directive and nothing else: what the light came
// through.

namespace {

// The top-level keywords, which are the synchronization points.
constexpr std::array<std::string_view, 1> TOP_LEVEL_KEYWORDS{"lens"};

class Parser final : public TextParser {
public:
  Parser(LayoutDiagnostics &diags, const LayoutSource &source,
         LensDocument &document)
      : TextParser(diags, source, TOP_LEVEL_KEYWORDS), mDocument(document) {}

  void parse() {
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
    if (mToken.text == "lens") {
      parseLensBlock();
    } else {
      auto &error{
          mDiags.error(location(), smdl::concat("unknown directive ",
                                                smdl::Quoted(mToken.text)))};
      if (mToken.text == "surface" || mToken.text == "stop") {
        error.note({}, "a surface belongs inside the 'lens' block, which "
                       "is what puts it in order with the others");
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
    auto &lens{mDocument.lens};
    parseSettings("a lens setting", [&](const std::string &key,
                                        const LayoutLocation &keyLoc) {
      if (key == "name") {
        lens.name = expect(Token::STRING, "a quoted name after 'name'");
      } else if (key == "surface") {
        lens.surfaces.push_back(parseSurface(keyLoc, false));
      } else if (key == "stop") {
        if (mStopLoc) {
          mDiags
              .error(keyLoc, "a lens has one aperture stop, and this is the "
                             "second 'stop'")
              .note(mStopLoc, "the first one is here");
          throw Recover();
        }
        mStopLoc = keyLoc;
        lens.surfaces.push_back(parseSurface(keyLoc, true));
      } else {
        mDiags.error(keyLoc,
                     smdl::concat("unknown lens setting ", smdl::Quoted(key),
                                  " (expected name, surface, or "
                                  "stop)"));
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
  }

  // One `surface { ... }` or `stop { ... }` entry. The two share a body
  // because they are one thing to the trace, a plane or a quadric with a
  // clear aperture; what differs is which keys mean anything, and the
  // stop's are so few that spelling out why each of the others is absent
  // is worth more than a second parser.
  [[nodiscard]] LensSurface parseSurface(const LayoutLocation &surfaceLoc,
                                         bool isStop) {
    if (mToken.kind != Token::OPEN) {
      mDiags.error(location(),
                   smdl::concat("expected '{' after ",
                                smdl::Quoted(isStop ? "stop" : "surface")));
      throw Recover();
    }
    auto surface{LensSurface{}};
    surface.isStop = isStop;
    auto hasDiameter{false};
    parseSettings("a surface setting", [&](const std::string &key,
                                           const LayoutLocation &keyLoc) {
      if (isStop && (key == "radius" || key == "ior" || key == "conic" ||
                     key == "aspheric")) {
        auto &error{mDiags.error(
            keyLoc, smdl::concat(smdl::Quoted(key),
                                 " has no meaning on the aperture stop"))};
        if (key == "ior") {
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
        const auto value{finite(keyLoc, key, numbers<1>()[0])};
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
        const auto value{finite(keyLoc, key, numbers<1>()[0])};
        if (!(value >= 1)) {
          mDiags
              .error(keyLoc, smdl::concat("expected 'ior' to be at least 1, "
                                          "got ",
                                          value))
              .note({}, "it is the index of the space after the surface, "
                        "which is 1 for air and is what omitting it means");
          throw Recover();
        }
        surface.ior = value;
      } else if (key == "diameter") {
        const auto value{finite(keyLoc, key, numbers<1>()[0])};
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
        mDiags.error(keyLoc,
                     smdl::concat("unknown surface setting ", smdl::Quoted(key),
                                  isStop ? " (expected thickness or diameter)"
                                         : " (expected radius, thickness, ior, "
                                           "diameter, conic, or aspheric)"));
        throw Recover();
      }
    });
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
    auto value{0.0f};
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

  LensDocument &mDocument;

  // Where the aperture stop was written, which is both the "have we seen
  // one" flag and the note a second one points at.
  LayoutLocation mStopLoc{};
};

} // namespace

size_t LensPrescription::stopIndex() const noexcept {
  for (size_t i = 0; i < surfaces.size(); i++)
    if (surfaces[i].isStop) return i;
  return surfaces.size();
}

LensDocument parseLens(LayoutDiagnostics &diags, const LayoutSource &source) {
  auto document{LensDocument()};
  document.source = &source;
  Parser(diags, source, document).parse();
  return document;
}

LensDocument readLens(const std::string &fileName) {
  auto diags{LayoutDiagnostics()};
  const auto &source{diags.loadSource(fileName)};
  auto document{parseLens(diags, source)};
  if (!diags.empty()) diags.printAll(smdl::cerrSupportsANSIColors());
  if (diags.hasErrors())
    throw smdl::Error(smdl::concat("cannot read ", smdl::QuotedPath(fileName),
                                   ": ", diags.summary()));
  SMDL_LOG_DEBUG("Read ", smdl::QuotedPath(fileName));
  return document;
}

std::string resolveLensFileName(const std::string &given,
                                const std::string &cameraFileName,
                                const std::string &stated) {
  if (!given.empty()) {
    if (!std::filesystem::exists(given))
      throw smdl::Error(
          smdl::concat("-lens ", smdl::QuotedPath(given), " does not exist"));
    return given;
  }
  if (stated.empty()) return {};
  auto path{std::filesystem::path(stated)};
  if (path.is_relative() && !cameraFileName.empty())
    path = std::filesystem::path(cameraFileName).parent_path() / path;
  if (!std::filesystem::exists(path))
    throw smdl::Error(smdl::concat("the camera file names the lens ",
                                   smdl::QuotedPath(stated),
                                   ", which does not exist beside it"));
  return path.string();
}
