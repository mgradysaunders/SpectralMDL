/// \file
#pragma once

#include <memory>
#include <mutex>
#include <string_view>
#include <vector>

#include "smdl/Support/Strings.h"

namespace smdl {

/// \addtogroup support
/// \{

/// Log level.
enum LogLevel : int {
  LOG_LEVEL_DEBUG = 0, ///< Debug message.
  LOG_LEVEL_INFO,      ///< Informational message.
  LOG_LEVEL_WARN,      ///< Warning!
  LOG_LEVEL_ERROR,     ///< Error!
};

/// A log sink to receive log messages.
class SMDL_EXPORT LogSink {
public:
  LogSink() = default;

  virtual ~LogSink() = default;

  /// Write.
  virtual void logMessage(LogLevel level, std::string_view message) {}

  /// Flush the file or stream if applicable.
  virtual void flush() {}

  /// Close the file or stream if applicable.
  virtual void close() {}
};

/// The logger.
class SMDL_EXPORT Logger final {
private:
  Logger() = default;

  ~Logger() { reset(); }

public:
  Logger(const Logger &) = delete;

  /// Get the logger singleton.
  [[nodiscard]] static Logger &get();

  /// Set the minimum level. This is not mutex protected and is understood to be
  /// set once at program startup and rarely if ever changed. The default level
  /// is `LOG_LEVEL_INFO`.
  void setMinLevel(LogLevel minLevel) { mMinLevel = minLevel; }

  /// Would a message at `level` reach the sinks?
  [[nodiscard]] bool isEnabled(LogLevel level) const noexcept {
    return level >= mMinLevel;
  }

  /// Add a new sink.
  template <typename T, typename... Args> T &addSink(Args &&...args) {
    return static_cast<T &>(
        *mSinks.emplace_back(std::make_unique<T>(std::forward<Args>(args)...)));
  }

  /// Flush all sinks.
  void flush();

  /// Close all sinks.
  void close();

  /// Log message with the given level.
  void logMessage(LogLevel level, std::string_view message);

  /// Remove all sinks!
  void reset();

private:
  /// The mutex just to be safe.
  std::mutex mMtx{};

  /// The sinks.
  std::vector<std::unique_ptr<LogSink>> mSinks{};

  /// The minimum level, such that every message below this is ignored.
  LogLevel mMinLevel{LOG_LEVEL_INFO};
};

/// Log a message with `level`, which `concat` builds from the remaining
/// arguments. The arguments are only evaluated if the message reaches the
/// sinks, so a disabled message costs nothing to write, and an argument
/// must not have a side effect the program depends on.
///
/// The library words a message as a sentence whose first word is
/// capitalized and which does not end in a period.
#define SMDL_LOG(level, ...)                                            \
  do {                                                                  \
    const auto smdlLogLevel{level};                                     \
    if (::smdl::Logger &smdlLogger{::smdl::Logger::get()};              \
        smdlLogger.isEnabled(smdlLogLevel))                             \
      smdlLogger.logMessage(smdlLogLevel, ::smdl::concat(__VA_ARGS__)); \
  } while (false)

/// Log a message with `LOG_LEVEL_DEBUG`. See `SMDL_LOG`.
#define SMDL_LOG_DEBUG(...) SMDL_LOG(::smdl::LOG_LEVEL_DEBUG, __VA_ARGS__)

/// Log a message with `LOG_LEVEL_INFO`. See `SMDL_LOG`.
#define SMDL_LOG_INFO(...) SMDL_LOG(::smdl::LOG_LEVEL_INFO, __VA_ARGS__)

/// Log a message with `LOG_LEVEL_WARN`. See `SMDL_LOG`.
#define SMDL_LOG_WARN(...) SMDL_LOG(::smdl::LOG_LEVEL_WARN, __VA_ARGS__)

/// Log a message with `LOG_LEVEL_ERROR`. See `SMDL_LOG`.
#define SMDL_LOG_ERROR(...) SMDL_LOG(::smdl::LOG_LEVEL_ERROR, __VA_ARGS__)

/// The label prefix for the given log level, as the default log sinks
/// print it: a bracketed word, always followed by a space.
///
/// | Level   | Label     |
/// |---------|-----------|
/// | `Debug` | `[debug]` |
/// | `Info`  | `[info]`  |
/// | `Warn`  | `[warn]`  |
/// | `Error` | `[error]` |
///
/// This is public so that a host with its own sink can print the same
/// labels rather than restate them.
[[nodiscard]] SMDL_EXPORT std::string_view
logLevelLabel(LogLevel level) noexcept;

/// The default log sinks, for convenience. Each prints
/// `logLevelLabel()` and then the message, in plain ASCII with no
/// escape codes at all.
///
/// A program that wants more of its terminal, a palette, symbols, or a
/// message body picked apart and highlighted, installs a sink of its own
/// and renders it there. That is presentation, and it belongs to the
/// program that owns the terminal rather than to a middleware library
/// writing into one it does not.
namespace LogSinks {

/// A default log sink to print to `std::cerr`.
class SMDL_EXPORT PrintToCerr final : public LogSink {
public:
  void logMessage(LogLevel level, std::string_view message) final;
};

/// A default log sink to print to `std::cout`.
class SMDL_EXPORT PrintToCout final : public LogSink {
public:
  void logMessage(LogLevel level, std::string_view message) final;

  void flush() final;
};

} // namespace LogSinks

/// \}

} // namespace smdl
