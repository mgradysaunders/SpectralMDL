#include "smdl/Support/Logger.h"

#include <algorithm>
#include <iostream>

#include "Support/Environment.h"

namespace smdl {

Logger &Logger::get() {
  static Logger logger{};
  return logger;
}

// NOTE: This must not log, since it runs inside the first 'get()'. That
// is why the override is reported later, by 'reportMinLevelOverride()'.
Logger::Logger()
    : mMinLevel(
          Environment::get().logLevel.value().value_or(LOG_LEVEL_INFO)) {}

void Logger::setMinLevel(LogLevel minLevel) {
  mHostMinLevel = minLevel;
  mMinLevel = Environment::get().logLevel.value().value_or(minLevel);
  reportMinLevelOverride();
}

// The report is made as soon as the host sets its level, or else ahead of
// the first message, whichever comes first with a sink to hear it. It
// logs through 'logMessage()', so the lock has to be released by then.
void Logger::reportMinLevelOverride() {
  bool hasSinks{};
  {
    std::scoped_lock guard{mMtx};
    hasSinks = !mSinks.empty();
  }
  if (hasSinks) Environment::get().logLevel.report(mHostMinLevel);
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
    reportMinLevelOverride();
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

std::string_view logLevelName(LogLevel level) noexcept {
  // NOLINTNEXTLINE
  static constexpr const char *names[4]{"debug", "info", "warn", "error"};
  return names[std::clamp(int(level), int(LOG_LEVEL_DEBUG),
                          int(LOG_LEVEL_ERROR))];
}

std::optional<LogLevel> parseLogLevel(std::string_view name) noexcept {
  for (LogLevel level : {LOG_LEVEL_DEBUG, LOG_LEVEL_INFO, LOG_LEVEL_WARN,
                         LOG_LEVEL_ERROR})
    if (name == logLevelName(level)) return level;
  return std::nullopt;
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
