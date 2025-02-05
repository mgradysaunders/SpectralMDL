#pragma once

#include <mutex>

#include "smdl/common.h"

namespace smdl {

/// \addtogroup Main
/// \{

/// Log level.
enum class LogLevel : int {
  Debug = 0, ///< Debug message.
  Info,      ///< Informational message.
  Warn,      ///< Warning!
  Error,     ///< Error!
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

  ~Logger();

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
  void remove_all_sinks();

private:
  /// The mutex just to be safe.
  std::mutex mtx{};

  /// The sinks.
  std::vector<std::unique_ptr<LogSink>> sinks{};
};

/// Log a message with `LogLevel::Debug`.
#define SMDL_LOG_DEBUG(message)                                                \
  ::smdl::Logger::get().log_message(::smdl::LogLevel::Debug, message)

/// Log a message with `LogLevel::Info`.
#define SMDL_LOG_INFO(message)                                                 \
  ::smdl::Logger::get().log_message(::smdl::LogLevel::Info, message)

/// Log a message with `LogLevel::Warn`.
#define SMDL_LOG_WARN(message)                                                 \
  ::smdl::Logger::get().log_message(::smdl::LogLevel::Warn, message)

/// Log a message with `LogLevel::Error`.
#define SMDL_LOG_ERROR(message)                                                \
  ::smdl::Logger::get().log_message(::smdl::LogLevel::Error, message)

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

/// \}

} // namespace smdl
