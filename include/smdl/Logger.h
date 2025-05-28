#pragma once

#include <mutex>

#include "smdl/common.h"

namespace smdl {

/// \addtogroup Main
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
  virtual void log_message(LogLevel level, std::string_view message) {}

  /// Flush the file or stream if appliable.
  virtual void flush() {}

  /// Close the file or stream if applicable.
  virtual void close() {}
};

/// The logger.
class SMDL_EXPORT Logger final {
private:
  Logger() = default;

  Logger(const Logger &) = delete;

  ~Logger() { reset(); }

public:
  /// Get the logger singleton.
  [[nodiscard]] static Logger &get();

  /// Add a new sink.
  template <typename T, typename... Args> T &add_sink(Args &&...args) {
    return static_cast<T &>(
        *sinks.emplace_back(std::make_unique<T>(std::forward<Args>(args)...)));
  }

  /// Flush all sinks.
  void flush();

  /// Close all sinks.
  void close();

  /// Log message with the given level.
  void log_message(LogLevel level, std::string_view message);

  /// Remove all sinks!
  void reset();

private:
  /// The mutex just to be safe.
  std::mutex mtx{};

  /// The sinks.
  std::vector<std::unique_ptr<LogSink>> sinks{};
};

/// Log a message with `LOG_LEVEL_DEBUG`.
#define SMDL_LOG_DEBUG(...)                                                    \
  ::smdl::Logger::get().log_message(::smdl::LOG_LEVEL_DEBUG,                   \
                                    ::smdl::concat(__VA_ARGS__))

/// Log a message with `LOG_LEVEL_INFO`.
#define SMDL_LOG_INFO(...)                                                     \
  ::smdl::Logger::get().log_message(::smdl::LOG_LEVEL_INFO,                    \
                                    ::smdl::concat(__VA_ARGS__))

/// Log a message with `LOG_LEVEL_WARN`.
#define SMDL_LOG_WARN(...)                                                     \
  ::smdl::Logger::get().log_message(::smdl::LOG_LEVEL_WARN,                    \
                                    ::smdl::concat(__VA_ARGS__))

/// Log a message with `LOG_LEVEL_ERROR`.
#define SMDL_LOG_ERROR(...)                                                    \
  ::smdl::Logger::get().log_message(::smdl::LOG_LEVEL_ERROR,                   \
                                    ::smdl::concat(__VA_ARGS__))

/// The default log-sinks for convenience.
///
/// | Log level | Label     |
/// |-----------|-----------|
/// | `Debug`   | `[debug]` |
/// | `Info`    |  _none_   |
/// | `Warn`    | `[warn]`  |
/// | `Error`   | `[error]` |
///
namespace LogSinks {

/// A default log sink to print to `std::cerr`.
class SMDL_EXPORT print_to_cerr final : public LogSink {
public:
  void log_message(LogLevel level, std::string_view message) final;
};

/// A default log sink to print to `std::cout`.
class SMDL_EXPORT print_to_cout final : public LogSink {
public:
  void log_message(LogLevel level, std::string_view message) final;

  void flush() final;
};

} // namespace LogSinks

/// Use `<unistd.h>` on POSIX to test if cerr routes to a terminal.
[[nodiscard]] SMDL_EXPORT bool cerr_supports_ansi_colors();

/// Use `<unistd.h>` on POSIX to test if cout routes to a terminal.
[[nodiscard]] SMDL_EXPORT bool cout_supports_ansi_colors();

/// \}

} // namespace smdl
