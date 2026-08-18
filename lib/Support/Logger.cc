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
  std::lock_guard guard{mtx};
  for (auto &sink : sinks) {
    sink->flush();
    sink->close();
  }
  sinks.clear();
}

void Logger::flush() {
  std::lock_guard guard{mtx};
  for (auto &sink : sinks) sink->flush();
}

void Logger::close() {
  std::lock_guard guard{mtx};
  for (auto &sink : sinks) sink->close();
}

void Logger::logMessage(LogLevel level, std::string_view message) {
  std::lock_guard guard{mtx};
  for (auto &sink : sinks) sink->logMessage(level, message);
}

static const char *LabelsWithColors[]{"\033[36m[debug]\033[0m ", "",
                                      "\033[33m[warn]\033[0m ",
                                      "\033[91m[error]\033[0m "};

static const char *LabelsWithoutColors[]{"[debug] ", "", "[warn] ", "[error] "};

std::string_view logLevelLabel(LogLevel level, bool withColors) {
  const auto labels{withColors ? &LabelsWithColors[0]
                               : &LabelsWithoutColors[0]};
  return labels[int(level)];
}

namespace LogSinks {

void print_to_cerr::logMessage(LogLevel level, std::string_view message) {
  static const bool WithColors{cerrSupportsANSIColors()};
  std::cerr << logLevelLabel(level, WithColors) << message << '\n';
}

void print_to_cout::logMessage(LogLevel level, std::string_view message) {
  static const bool WithColors{coutSupportsANSIColors()};
  std::cout << logLevelLabel(level, WithColors) << message << std::endl;
}

void print_to_cout::flush() { std::cout.flush(); }

} // namespace LogSinks

bool cerrSupportsANSIColors() {
#if SMDL_HAS_UNISTD
  return isatty(STDERR_FILENO);
#else
  return false;
#endif // #if SMDL_HAS_UNISTD
}

bool coutSupportsANSIColors() {
#if SMDL_HAS_UNISTD
  return isatty(STDOUT_FILENO);
#else
  return false;
#endif // #if SMDL_HAS_UNISTD
}

} // namespace smdl
