/// \file
/// The syntax core the layout toolchain's text formats share: the token,
/// the lexer, the parser base holding everything that is true of the
/// grammar rather than of one format, and the read every format's own
/// `read...()` is.
///
/// The `.layout` and `.camera` formats are one language with two
/// vocabularies. Everything here is the language; the vocabularies live
/// in `LayoutParser.cc` and `CameraFile.cc`, which derive from
/// `TextParser` and add the statements they know.
#pragma once

#include <array>
#include <cstdint>
#include <string>
#include <string_view>

#include "smdl/Support/Logger.h"
#include "smdl/Support/Span.h"
#include "smdl/Support/Strings.h"

#include "Common.h"

#include "Layout/LayoutDiagnostics.h"

/// Read `fileName` as one document: load it as a source of `diags`,
/// `parse(diags, source)` it, print every diagnostic, and refuse the
/// file if any of them was an error.
///
/// The document's locations point into `diags`, so a caller that holds
/// onto them past the read (a refusal made once the scene is known)
/// passes a sink that outlives the document.
///
/// \throws smdl::Error  If the file cannot be read, or on any parse
///                      error after printing the diagnostics.
///
template <typename Parse>
[[nodiscard]] auto readDocument(LayoutDiagnostics &diags,
                                const std::string &fileName, Parse &&parse) {
  const auto &source{diags.loadSource(fileName)};
  auto document{parse(diags, source)};
  diags.printAllAndRefuse(fileName);
  SMDL_LOG_DEBUG("Read ", smdl::QuotedPath(fileName));
  return document;
}

/// The file a camera file names, or empty for none: `stated` as the
/// camera file wrote it, resolved relative to `cameraFileName` so that a
/// scene directory stays self-contained. `what` is the noun a refusal
/// calls it by, e.g. `"sensor"`.
///
/// \throws smdl::Error  If it names a file that does not exist, which
///                      may not be quietly ignored.
///
[[nodiscard]] std::string resolveSiblingFile(const std::string &stated,
                                             const std::string &cameraFileName,
                                             std::string_view what);

/// One token, with the byte range it came from so that every diagnostic
/// can point at it.
class Token final {
public:
  enum Kind { WORD, STRING, OPEN, CLOSE, EQUALS, END };
  Kind kind{END};
  std::string text{};
  uint32_t offset{};
  uint32_t length{};
};

/// The whole file as tokens: quoted strings, bare words, and the three
/// punctuation marks. A directive body can span one line or several
/// without the parser caring which.
class Lexer final {
public:
  Lexer(LayoutDiagnostics &diags, const LayoutSource &source)
      : mDiags(diags), mSource(source) {}

  [[nodiscard]] Token next();

private:
  [[nodiscard]] uint32_t position() const noexcept { return uint32_t(mPos); }

  LayoutDiagnostics &mDiags;
  const LayoutSource &mSource;
  size_t mPos{};
};

/// Thrown to abandon the statement being parsed after its diagnostic is
/// emitted; caught by the statement loop, which synchronizes at the next
/// top-level keyword. Never escapes a `parse...()` entry point.
class Recover final {};

/// The transform operations, named here so an unknown directive that is
/// really a stray transform gets a pointed note.
constexpr std::array<std::string_view, 7> TRANSFORM_OPS{
    "translate", "scale",    "rotate", "rotate_x",
    "rotate_y",  "rotate_z", "matrix"};

/// The parser base: the token stream, the diagnostics, and every helper
/// that speaks about the language rather than about one format's
/// vocabulary.
///
/// A derived parser supplies its top-level keywords (the synchronization
/// points), then writes one `parseStatement()` over the helpers here.
///
class TextParser {
public:
  /// `keywords` are the statement keywords `synchronize()` resumes at,
  /// and must outlive the parser.
  TextParser(LayoutDiagnostics &diags, const LayoutSource &source,
             smdl::Span<const std::string_view> keywords)
      : mDiags(diags), mSource(source), mLexer(diags, source),
        mKeywords(keywords) {
    mToken = mLexer.next();
  }

protected:
  /// Read statements with `parseStatement` until the end of the file,
  /// synchronizing at the next top-level keyword after each one that
  /// threw, so that one bad statement costs its own diagnostic and not
  /// the diagnostics of the statements after it. This loop is what every
  /// format's `parse()` is.
  template <typename ParseStatement>
  void parseStatements(ParseStatement &&parseStatement) {
    while (mToken.kind != Token::END) {
      try {
        parseStatement();
      } catch (const Recover &) {
        synchronize();
      }
    }
  }

  /// Skip to the next top-level keyword, tracking brace depth so that a
  /// keyword inside the abandoned statement's block does not fool the
  /// loop into starting mid-block. An error raised after a line's last
  /// token leaves the next statement's keyword current already; it is
  /// not part of the abandoned statement, so it is kept rather than
  /// skipped, and the line check is what tells it from a keyword that
  /// is merely an operation word of the abandoned line.
  void synchronize();

  [[nodiscard]] bool isTopLevelKeyword(const Token &token) const noexcept;

  /// The line of the last diagnostic, or 0. See `synchronize()`.
  [[nodiscard]] uint32_t lastDiagnosticLine() const noexcept;

  /// Check a setting that has to be positive to mean anything: a size, a
  /// power, a scale, or one of the camera and sky quantities whose zero
  /// means "unset" everywhere downstream. Writing one down has to say
  /// something, and in every case leaving it out is what asks for the
  /// default, which is what the message points at.
  [[nodiscard]] float positive(const LayoutLocation &keyLoc,
                               std::string_view key, float value);

  /// Check a setting that is a clock reading, which the number syntax
  /// alone would let be infinite or not a number.
  [[nodiscard]] float finite(const LayoutLocation &keyLoc, std::string_view key,
                             float value);

  /// One transform operation, applied on the LEFT of `xf` so that it
  /// takes effect after everything above it. Returns false if `op` names
  /// no transform operation, leaving the caller to decide what that
  /// means.
  [[nodiscard]] bool parseTransformOp(const std::string &op,
                                      const LayoutLocation &opLoc,
                                      float4x4 &xf);

  /// Does the token spell a number, in full? An operation name never
  /// does, which is what lets a directive take a variable count of them.
  [[nodiscard]] static bool isNumber(const Token &token);

  /// Not `std::stof`, which reports "not a number" by throwing: this is
  /// asked of every token of a directive that takes a variable count of
  /// them, so the answer "no" has to be cheap.
  [[nodiscard]] static bool tryNumber(const Token &token, float &value);

  template <size_t N> [[nodiscard]] std::array<float, N> numbers() {
    std::array<float, N> values{};
    for (size_t i = 0; i < N; i++) {
      if (mToken.kind != Token::WORD) {
        mDiags.error(location(),
                     smdl::concat("expected ", smdl::Counted(N, "number"),
                                  ", got ", i, " of them"));
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

  std::string expect(Token::Kind kind, std::string_view what);

  /// The `{ ... }` body of a block whose contents are a run of settings:
  /// the loop, the two diagnostics every such block was spelling for
  /// itself, and the keyword and location handed to `body` with the
  /// keyword already consumed. `what` is the article and noun the
  /// messages use, e.g. `"a camera setting"`.
  ///
  /// The caller has already established that `{` is current, either by
  /// checking it (a top-level block, where a missing brace is its own
  /// error) or by calling this only when it is (an asset or light body,
  /// whose block is optional).
  template <typename Body>
  void parseSettings(std::string_view what, Body &&body) {
    advance(); // '{'
    while (mToken.kind != Token::CLOSE) {
      if (mToken.kind == Token::END) {
        mDiags.error(location(), "expected '}' before end of file");
        throw Recover();
      }
      if (mToken.kind != Token::WORD) {
        mDiags.error(location(), smdl::concat("expected ", what, " or '}'"));
        throw Recover();
      }
      const auto key{mToken.text};
      const auto keyLoc{location()};
      advance();
      body(key, keyLoc);
    }
    advance(); // '}'
  }

  [[nodiscard]] LayoutLocation location() const noexcept {
    return {&mSource, mToken.offset, std::max(mToken.length, uint32_t(1))};
  }

  void advance() { mToken = mLexer.next(); }

  [[nodiscard]] static bool isIdentifier(std::string_view name);

  /// The 1-based line a token sits on, which is what decides where a
  /// one-line 'place' ends.
  [[nodiscard]] uint32_t lineOf(const Token &token) const noexcept;

  [[nodiscard]] static float4x4 translation(float x, float y, float z);

  [[nodiscard]] static float4x4 rotation(float3 axis, float degrees);

  LayoutDiagnostics &mDiags;
  const LayoutSource &mSource;
  Lexer mLexer;
  Token mToken{};

private:
  smdl::Span<const std::string_view> mKeywords{};
};
