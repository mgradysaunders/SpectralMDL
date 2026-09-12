#include "smdl/Support/Logger.h"

#include <algorithm>
#include <cctype>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <iostream>

// NOTE: Test for the header directly. Do not gate this on an OS list:
// Darwin defines neither '__linux__' nor '__unix__', and '_POSIX_VERSION'
// only exists after <unistd.h> has been included, so any such list silently
// turns off 'isatty' (and therefore colored output) on macOS.
#if __has_include(<unistd.h>)
#define SMDL_HAS_UNISTD 1
#include <unistd.h>
#endif // #if __has_include(<unistd.h>)

namespace {

// Does `str` contain `lowerNeedle`, ignoring the case of `str`?
[[nodiscard]] bool containsIgnoringCase(std::string_view str,
                                        std::string_view lowerNeedle) noexcept {
  return std::search(str.begin(), str.end(), lowerNeedle.begin(),
                     lowerNeedle.end(), [](char ch, char lower) {
                       return std::tolower(static_cast<unsigned char>(ch)) ==
                              lower;
                     }) != str.end();
}

// Does the environment allow colors on a terminal? See `shouldUseColors()`.
[[nodiscard]] bool environmentAllowsColors() noexcept {
  const char *noColor{std::getenv("NO_COLOR")};
  const char *term{std::getenv("TERM")};
  return !(noColor && *noColor) && term && *term &&
         std::string_view(term) != "dumb";
}

// The palette `formatLogMessage()` highlights with, which is smdl-toy's
// `LayoutDiagnostics` palette where the two overlap (the bold location,
// the bold green caret), so that the two programs' errors look alike.
constexpr std::string_view ANSI_RESET{"\033[0m"};
constexpr std::string_view ANSI_BOLD{"\033[1m"};
constexpr std::string_view ANSI_DIM{"\033[2m"};
constexpr std::string_view ANSI_CYAN{"\033[36m"};
constexpr std::string_view ANSI_CARET{"\033[1;32m"};

[[nodiscard]] bool isWordChar(char ch) noexcept {
  return std::isalnum(static_cast<unsigned char>(ch)) || ch == '_';
}

[[nodiscard]] bool isAllDigits(std::string_view str) noexcept {
  return !str.empty() && std::all_of(str.begin(), str.end(), [](char ch) {
    return std::isdigit(static_cast<unsigned char>(ch));
  });
}

// The length of the location `str` starts with, `[file:line]` or
// `[file:line:col]` as `LocationMarkup` writes it, or 0 if there is none.
// A name that is itself a number makes a clock time or a range, which
// is not a location.
[[nodiscard]] size_t locationLength(std::string_view str) noexcept {
  if (str.empty() || str[0] != '[') return 0;
  const size_t close{str.find(']')};
  if (close == std::string_view::npos) return 0;
  std::string_view name{str.substr(1, close - 1)};
  const auto dropNumber{[&] {
    const size_t colon{name.rfind(':')};
    if (colon == std::string_view::npos || !isAllDigits(name.substr(colon + 1)))
      return false;
    name = name.substr(0, colon);
    return true;
  }};
  if (!dropNumber()) return 0;
  (void)dropNumber(); // The column, if there is one
  return name.empty() || isAllDigits(name) ? 0 : close + 1;
}

// The length of the single-quoted string `str` starts with, quotes
// included, or 0 if it is never closed. A quote closes only where no
// word goes on after it, so that the apostrophe in 'bob's.png' does not.
[[nodiscard]] size_t quotedLength(std::string_view str) noexcept {
  if (str.empty() || str[0] != '\'') return 0;
  for (size_t i = 1; i < str.size(); i++)
    if (str[i] == '\'' && (i + 1 == str.size() || !isWordChar(str[i + 1])))
      return i + 1;
  return 0;
}

// The length of the gutter `line` starts with, through its bar, or 0 if
// there is none: `  12 |` on a quoted source line and `     |` on the
// caret line under it, as `SourceLocation::getSourceSnippet()` writes
// them.
[[nodiscard]] size_t gutterLength(std::string_view line) noexcept {
  const size_t bar{line.find(" | ")};
  if (bar == std::string_view::npos || bar < 3 || line.substr(0, 2) != "  ")
    return 0;
  const std::string_view number{line.substr(2, bar - 2)};
  const bool isBlank{number.find_first_not_of(' ') == std::string_view::npos};
  return isBlank || isAllDigits(number) ? bar + 2 : 0;
}

// The position of the caret if `line` is the marker under a quoted
// source line, a '^' extended by any number of '~' after only a gutter
// and whitespace, or `npos` if it is not.
[[nodiscard]] size_t caretPosition(std::string_view line) noexcept {
  const size_t caret{line.find_first_not_of(" \t", gutterLength(line))};
  if (caret == std::string_view::npos || line[caret] != '^' ||
      line.find_first_not_of('~', caret + 1) != std::string_view::npos)
    return std::string_view::npos;
  return caret;
}

void appendStyled(std::string &result, std::string_view style,
                  std::string_view text) {
  result += style;
  result += text;
  result += ANSI_RESET;
}

// Append a source line, or the caret line under one, with its gutter
// dimmed and the caret colored. The source itself stays as written,
// since its quotes and brackets are code rather than a message's.
void appendSnippetLine(std::string &result, std::string_view line,
                       size_t caret = std::string_view::npos) {
  const size_t gutter{gutterLength(line)};
  if (gutter > 0) appendStyled(result, ANSI_DIM, line.substr(0, gutter));
  if (caret == std::string_view::npos) {
    result += line.substr(gutter);
    return;
  }
  result += line.substr(gutter, caret - gutter);
  appendStyled(result, ANSI_CARET, line.substr(caret));
}

// Append a line of message text with its locations and quoted strings
// highlighted. Either begins only where no word runs into it.
void appendHighlighted(std::string &result, std::string_view line) {
  for (size_t i = 0; i < line.size();) {
    if (i == 0 || !isWordChar(line[i - 1])) {
      const std::string_view rest{line.substr(i)};
      if (const size_t n{locationLength(rest)}; n > 0) {
        appendStyled(result, ANSI_BOLD, rest.substr(0, n));
        i += n;
        continue;
      }
      if (const size_t n{quotedLength(rest)}; n > 0) {
        appendStyled(result, ANSI_CYAN, rest.substr(0, n));
        i += n;
        continue;
      }
    }
    result += line[i++];
  }
}

} // namespace

namespace smdl {

Logger &Logger::get() {
  static Logger logger{};
  return logger;
}

void Logger::reset() {
  std::scoped_lock guard{mMtx};
  for (auto &sink : mSinks) {
    sink->flush();
    sink->close();
  }
  mSinks.clear();
}

void Logger::flush() {
  std::scoped_lock guard{mMtx};
  for (auto &sink : mSinks) sink->flush();
}

void Logger::close() {
  std::scoped_lock guard{mMtx};
  for (auto &sink : mSinks) sink->close();
}

void Logger::logMessage(LogLevel level, std::string_view message) {
  if (isEnabled(level)) {
    std::scoped_lock guard{mMtx};
    for (auto &sink : mSinks) sink->logMessage(level, message);
  }
}

std::string_view logLevelLabel(LogLevel level, bool useColors,
                               bool useUnicode) noexcept {
  // The symbols are U+2699, U+2139, U+26A0 and U+2718, each one column
  // wide. The first three are deliberately not followed by U+FE0F, which
  // would ask for their double-width emoji forms, and terminals disagree
  // on how wide those are.
  // NOLINTNEXTLINE
  static constexpr const char *labels[2][2][4] = {
      {{"[debug] ", "[info] ", "[warn] ", "[error] "},
       {"\033[36m[debug]\033[0m ", "\033[32m[info]\033[0m ",
        "\033[33m[warn]\033[0m ", "\033[91m[error]\033[0m "}},
      {{"⚙ ", "ℹ ", "⚠ ", "✘ "},
       {"\033[36m⚙\033[0m ", "\033[32mℹ\033[0m ", "\033[33m⚠\033[0m ",
        "\033[91m✘\033[0m "}}};
  return labels[int(useUnicode)][int(useColors)][std::clamp(int(level), 0, 3)];
}

std::string formatLogMessage(LogLevel level, std::string_view message,
                             bool useColors, bool useUnicode) {
  std::string result{logLevelLabel(level, useColors, useUnicode)};
  if (!useColors || message.find('\033') != std::string_view::npos) {
    result += message;
    return result;
  }
  if (level <= LOG_LEVEL_DEBUG) {
    // The label resets its own color, which ends the first dim, so the
    // message needs a second.
    result.insert(0, ANSI_DIM);
    appendStyled(result, ANSI_DIM, message);
    return result;
  }
  std::vector<std::string_view> lines{};
  for (size_t pos = 0;;) {
    const size_t end{message.find('\n', pos)};
    lines.push_back(message.substr(pos, end - pos));
    if (end == std::string_view::npos) break;
    pos = end + 1;
  }
  for (size_t i = 0; i < lines.size(); i++) {
    if (i > 0) result += '\n';
    if (i + 1 < lines.size()) {
      if (const size_t caret{caretPosition(lines[i + 1])};
          caret != std::string_view::npos) {
        appendSnippetLine(result, lines[i]);
        result += '\n';
        appendSnippetLine(result, lines[++i], caret);
        continue;
      }
    }
    appendHighlighted(result, lines[i]);
  }
  return result;
}

bool cerrSupportsANSIColors() noexcept {
#if SMDL_HAS_UNISTD
  return ::isatty(STDERR_FILENO);
#else
  return false;
#endif // #if SMDL_HAS_UNISTD
}

bool coutSupportsANSIColors() noexcept {
#if SMDL_HAS_UNISTD
  return ::isatty(STDOUT_FILENO);
#else
  return false;
#endif // #if SMDL_HAS_UNISTD
}

bool localeIsUTF8() noexcept {
  for (const char *name : {"LC_ALL", "LC_CTYPE", "LANG"}) {
    const char *value{std::getenv(name)};
    if (!value || !*value) continue;
    return containsIgnoringCase(value, "utf-8") ||
           containsIgnoringCase(value, "utf8");
  }
  return false;
}

bool shouldUseUnicode(UnicodeMode mode, bool isTerminal) noexcept {
  return mode == UNICODE_MODE_ALWAYS ||
         (mode == UNICODE_MODE_AUTO && isTerminal && localeIsUTF8());
}

bool shouldUseColors(ANSIColorMode mode, bool isTerminal) noexcept {
  return mode == ANSI_COLOR_MODE_ALWAYS ||
         (mode == ANSI_COLOR_MODE_AUTO && isTerminal &&
          environmentAllowsColors());
}

namespace LogSinks {

PrintToCerr::PrintToCerr(UnicodeMode unicodeMode) noexcept {
  setUnicodeMode(unicodeMode);
  setColorMode(ANSI_COLOR_MODE_AUTO);
}

void PrintToCerr::logMessage(LogLevel level, std::string_view message) {
  std::cerr << formatLogMessage(level, message, mUseColors, mUseUnicode)
            << '\n';
}

void PrintToCerr::setUnicodeMode(UnicodeMode unicodeMode) noexcept {
  mUseUnicode = shouldUseUnicode(unicodeMode, cerrSupportsANSIColors());
}

void PrintToCerr::setColorMode(ANSIColorMode colorMode) noexcept {
  mUseColors = shouldUseColors(colorMode, cerrSupportsANSIColors());
}

PrintToCout::PrintToCout(UnicodeMode unicodeMode) noexcept {
  setUnicodeMode(unicodeMode);
  setColorMode(ANSI_COLOR_MODE_AUTO);
}

void PrintToCout::logMessage(LogLevel level, std::string_view message) {
  std::cout << formatLogMessage(level, message, mUseColors, mUseUnicode)
            << std::endl;
}

void PrintToCout::flush() { std::cout.flush(); }

void PrintToCout::setUnicodeMode(UnicodeMode unicodeMode) noexcept {
  mUseUnicode = shouldUseUnicode(unicodeMode, coutSupportsANSIColors());
}

void PrintToCout::setColorMode(ANSIColorMode colorMode) noexcept {
  mUseColors = shouldUseColors(colorMode, coutSupportsANSIColors());
}

} // namespace LogSinks

} // namespace smdl
