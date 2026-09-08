#include "smdl/Support/Logger.h"

#include <cstddef>
#include <cstdio>
#include <iostream>

// NOTE: Test for the header directly. Do not gate this on an OS list:
// Darwin defines neither '__linux__' nor '__unix__', and '_POSIX_VERSION'
// only exists after <unistd.h> has been included, so any such list silently
// turns off 'isatty' (and therefore colored output) on macOS.
#if __has_include(<unistd.h>)
#define SMDL_HAS_UNISTD 1
#include <unistd.h>
#endif // #if __has_include(<unistd.h>)

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

std::string_view logLevelLabel(LogLevel level, bool useColors) noexcept {
  // NOLINTNEXTLINE
  static constexpr const char *labels[2][4] = {
      {"[debug] ", "", "[warn] ", "[error] "},
      {"\033[36m[debug]\033[0m ", "", "\033[33m[warn]\033[0m ",
       "\033[91m[error]\033[0m "}};
  return labels[int(useColors)][std::clamp(int(level), 0, 3)];
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

namespace LogSinks {

void PrintToCerr::logMessage(LogLevel level, std::string_view message) {
  static const bool useColors{cerrSupportsANSIColors()};
  std::cerr << logLevelLabel(level, useColors) << message << '\n';
}

void PrintToCout::logMessage(LogLevel level, std::string_view message) {
  static const bool useColors{coutSupportsANSIColors()};
  std::cout << logLevelLabel(level, useColors) << message << std::endl;
}

void PrintToCout::flush() { std::cout.flush(); }

} // namespace LogSinks

} // namespace smdl
