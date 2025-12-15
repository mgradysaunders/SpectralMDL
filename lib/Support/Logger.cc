#include "smdl/Support/Logger.h"

#include <cstddef>
#include <cstdio>
#include <iostream>

#if __linux__ || __unix__ || defined(_POSIX_VERSION)
#if __has_include(<unistd.h>)
#define SMDL_HAS_UNISTD 1
#include <unistd.h>
#endif // #if __has_include(<unistd.h>)
#endif // #if __linux__ || __unix__ || defined(_POSIX_VERSION)

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
  for (auto &sink : sinks)
    sink->flush();
}

void Logger::close() {
  std::lock_guard guard{mtx};
  for (auto &sink : sinks)
    sink->close();
}

void Logger::logMessage(LogLevel level, std::string_view message) {
  std::lock_guard guard{mtx};
  for (auto &sink : sinks)
    sink->logMessage(level, message);
}

namespace LogSinks {

static const char *LabelsWithColors[]{"\033[36m[debug]\033[0m ", "",
                                      "\033[33m[warn]\033[0m ",
                                      "\033[91m[error]\033[0m "};

static const char *LabelsWithoutColors[]{"[debug] ", "", "[warn] ", "[error] "};

void print_to_cerr::logMessage(LogLevel level, std::string_view message) {
  static const auto Labels{cerrSupportsANSIColors() ? &LabelsWithColors[0]
                                                    : &LabelsWithoutColors[0]};
  std::cerr << Labels[int(level)] << message << '\n';
}

void print_to_cout::logMessage(LogLevel level, std::string_view message) {
  static const auto Labels{coutSupportsANSIColors() ? &LabelsWithColors[0]
                                                    : &LabelsWithoutColors[0]};
  std::cout << Labels[int(level)] << message << std::endl;
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
