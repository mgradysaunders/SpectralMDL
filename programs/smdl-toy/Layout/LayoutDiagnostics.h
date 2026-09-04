/// \file
/// The diagnostics kit for the layout toolchain: loaded sources, located
/// diagnostics with attached notes, and caret rendering.
#pragma once

#include <cstdint>
#include <deque>
#include <string>
#include <string_view>
#include <vector>

/// One loaded source file: its name, its text, and the line table that
/// turns byte offsets into line and column numbers on demand.
///
/// Offsets are the unit of location everywhere in the toolchain, because
/// they are what a lexer naturally has; lines and columns exist only at
/// the moment a diagnostic is rendered.
///
class LayoutSource final {
public:
  LayoutSource(std::string fileName, std::string text);

  /// A 1-based line and column pair, spelled like `smdl::SourceLocation`
  /// spells them.
  class LineAndColumn final {
  public:
    uint32_t lineNo{1}; ///< The 1-based line number.
    uint32_t charNo{1}; ///< The 1-based column number, in bytes.
  };

  /// The line and column of a byte offset. An offset at or past the end
  /// of the text lands just past the last character, which is where an
  /// unexpected-end-of-file diagnostic wants its caret.
  [[nodiscard]] LineAndColumn lineAndColumn(uint32_t offset) const noexcept;

  /// The text of the 1-based line `lineNo`, without its newline and
  /// without a trailing carriage return, so that a CRLF file renders
  /// cleanly. Out-of-range lines are empty.
  [[nodiscard]] std::string_view lineText(uint32_t lineNo) const noexcept;

  /// The number of lines. Text ending in a newline has a final empty
  /// line, so that end-of-file locations have a line to land on.
  [[nodiscard]] uint32_t lineCount() const noexcept {
    return uint32_t(mLineStarts.size());
  }

public:
  /// The name diagnostics print, exactly as the file was referred to.
  std::string fileName{};

  /// The text, owned here so that every location into it stays valid for
  /// as long as the `LayoutDiagnostics` that loaded it lives.
  std::string text{};

private:
  /// The byte offset of the start of each line; front is always 0.
  std::vector<uint32_t> mLineStarts{};
};

/// A location: a source, a byte offset into it, and the length of the
/// marked range. A default-constructed location has no source and means
/// "nowhere", which is what a note that speaks about no particular text
/// carries.
class LayoutLocation final {
public:
  [[nodiscard]] explicit operator bool() const noexcept {
    return source != nullptr;
  }

public:
  const LayoutSource *source{};

  /// The byte offset of the first marked character.
  uint32_t offset{};

  /// The number of marked characters. Rendering clamps the marker to the
  /// end of the line it starts on, so a length spanning lines is safe to
  /// pass.
  uint32_t length{1};
};

/// One diagnostic: a kind, a location, a message, and any notes attached
/// to it. Notes are how a diagnostic points at more than one place: the
/// previous declaration a redefinition collides with, each file of an
/// import cycle, the suggestion for a misspelled name.
class LayoutDiagnostic final {
public:
  enum class Kind {
    ERROR,
    WARNING,
    NOTE,
  };

  /// Attach a note, returning `*this` so that notes chain:
  /// `error(loc, ...).note(loc2, ...).note(loc3, ...)`. A note with a
  /// default `LayoutLocation` renders with no location and no excerpt.
  LayoutDiagnostic &note(LayoutLocation noteLoc, std::string noteMessage);

public:
  Kind kind{Kind::ERROR};
  LayoutLocation location{};
  std::string message{};
  std::vector<LayoutDiagnostic> notes{};
};

/// The diagnostic sink: owns every loaded source and accumulates every
/// diagnostic, in emission order, which for a single-pass parser is
/// document order.
///
/// Sources live in a `std::deque` so that the `LayoutSource` references
/// and the `LayoutLocation` pointers into them stay valid as more files
/// load; diagnostics likewise, so that the reference `error()` returns
/// stays valid while notes are attached to it across later emissions.
///
class LayoutDiagnostics final {
public:
  /// Load a file as a source.
  ///
  /// \throws smdl::Error  If the file cannot be read. That failure has no
  ///                      source text to point a caret at, so it stays an
  ///                      exception rather than a diagnostic.
  const LayoutSource &loadSource(const std::string &fileName);

  /// Add a source from memory: tests, and any future tool that feeds
  /// text it already holds.
  const LayoutSource &addSource(std::string fileName, std::string text);

  /// Report an error. Returns the diagnostic so notes can chain.
  LayoutDiagnostic &error(LayoutLocation location, std::string message);

  /// Report a warning. Returns the diagnostic so notes can chain.
  LayoutDiagnostic &warn(LayoutLocation location, std::string message);

  /// Attach a note to every diagnostic reported at or after index
  /// `firstIndex`. This is how nested processing annotates in one pass:
  /// record `all().size()`, recurse, and every diagnostic the recursion
  /// produced gains a "imported from here" note pointing at the import
  /// site.
  void noteAllFrom(size_t firstIndex, LayoutLocation noteLoc,
                   std::string noteMessage);

  [[nodiscard]] bool empty() const noexcept { return mDiagnostics.empty(); }
  [[nodiscard]] bool hasErrors() const noexcept { return mErrorCount > 0; }
  [[nodiscard]] size_t errorCount() const noexcept { return mErrorCount; }
  [[nodiscard]] size_t warningCount() const noexcept { return mWarningCount; }

  [[nodiscard]] const std::deque<LayoutDiagnostic> &all() const noexcept {
    return mDiagnostics;
  }

  /// Render one diagnostic: `file:line:col: kind: message`, the source
  /// line, a caret marker under the marked range, then each note the
  /// same way. Tabs in the source line are preserved in the marker
  /// prefix so the caret stays aligned however tabs display.
  [[nodiscard]] static std::string render(const LayoutDiagnostic &diagnostic,
                                          bool colored);

  /// Render every accumulated diagnostic, followed by `summary()` on its
  /// own line if there is anything to summarize.
  [[nodiscard]] std::string renderAll(bool colored) const;

  /// The count line, e.g. `3 errors` or `1 error, 2 warnings`. Empty if
  /// nothing has been reported.
  [[nodiscard]] std::string summary() const;

  /// Print `renderAll()` to standard error.
  void printAll(bool colored) const;

private:
  std::deque<LayoutSource> mSources{};
  std::deque<LayoutDiagnostic> mDiagnostics{};
  size_t mErrorCount{};
  size_t mWarningCount{};
};
