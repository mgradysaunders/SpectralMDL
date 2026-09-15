#include "smdl/Support/Logger.h"

#include <algorithm>
#include <iostream>

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

std::string_view logLevelLabel(LogLevel level) noexcept {
  // NOLINTNEXTLINE
  static constexpr const char *labels[4]{"[debug] ", "[info] ", "[warn] ",
                                         "[error] "};
  return labels[std::clamp(int(level), int(LOG_LEVEL_DEBUG),
                           int(LOG_LEVEL_ERROR))];
}

namespace LogSinks {

void PrintToCerr::logMessage(LogLevel level, std::string_view message) {
  std::cerr << logLevelLabel(level) << message << '\n';
}

void PrintToCout::logMessage(LogLevel level, std::string_view message) {
  std::cout << logLevelLabel(level) << message << std::endl;
}

void PrintToCout::flush() { std::cout.flush(); }

} // namespace LogSinks

} // namespace smdl
