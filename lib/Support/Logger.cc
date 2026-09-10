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
  if (level >= mMinLevel) {
    std::scoped_lock guard{mMtx};
    for (auto &sink : mSinks) sink->logMessage(level, message);
  }
}

std::string_view logLevelLabel(LogLevel level, bool useColors,
                               bool useUnicode) noexcept {
  // The symbols are U+203A, U+2022, U+26A0 and U+2718, each one column
  // wide. The warning sign is deliberately not followed by U+FE0F, which
  // would ask for its double-width emoji form, and terminals disagree on
  // how wide that is.
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

namespace LogSinks {

PrintToCerr::PrintToCerr(UnicodeMode unicodeMode) noexcept
    : mUseColors(cerrSupportsANSIColors()) {
  setUnicodeMode(unicodeMode);
}

void PrintToCerr::logMessage(LogLevel level, std::string_view message) {
  std::cerr << logLevelLabel(level, mUseColors, mUseUnicode) << message << '\n';
}

void PrintToCerr::setUnicodeMode(UnicodeMode unicodeMode) noexcept {
  mUseUnicode = shouldUseUnicode(unicodeMode, cerrSupportsANSIColors());
}

PrintToCout::PrintToCout(UnicodeMode unicodeMode) noexcept
    : mUseColors(coutSupportsANSIColors()) {
  setUnicodeMode(unicodeMode);
}

void PrintToCout::logMessage(LogLevel level, std::string_view message) {
  std::cout << logLevelLabel(level, mUseColors, mUseUnicode) << message
            << std::endl;
}

void PrintToCout::flush() { std::cout.flush(); }

void PrintToCout::setUnicodeMode(UnicodeMode unicodeMode) noexcept {
  mUseUnicode = shouldUseUnicode(unicodeMode, coutSupportsANSIColors());
}

} // namespace LogSinks

} // namespace smdl
