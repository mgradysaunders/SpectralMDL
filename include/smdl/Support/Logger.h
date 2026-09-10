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

/// Whether the default log sinks label a message with a Unicode symbol
/// or with a bracketed ASCII word. See `LogSinks`.
enum UnicodeMode : int {
  UNICODE_MODE_AUTO,   ///< Symbols only on a terminal in a UTF-8 locale.
  UNICODE_MODE_ALWAYS, ///< Symbols even if the stream is redirected.
  UNICODE_MODE_NEVER   ///< Always the bracketed words.
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

/// Log a message with `LOG_LEVEL_DEBUG`.
#define SMDL_LOG_DEBUG(...)                                 \
  ::smdl::Logger::get().logMessage(::smdl::LOG_LEVEL_DEBUG, \
                                   ::smdl::concat(__VA_ARGS__))

/// Log a message with `LOG_LEVEL_INFO`.
#define SMDL_LOG_INFO(...)                                 \
  ::smdl::Logger::get().logMessage(::smdl::LOG_LEVEL_INFO, \
                                   ::smdl::concat(__VA_ARGS__))

/// Log a message with `LOG_LEVEL_WARN`.
#define SMDL_LOG_WARN(...)                                 \
  ::smdl::Logger::get().logMessage(::smdl::LOG_LEVEL_WARN, \
                                   ::smdl::concat(__VA_ARGS__))

/// Log a message with `LOG_LEVEL_ERROR`.
#define SMDL_LOG_ERROR(...)                                 \
  ::smdl::Logger::get().logMessage(::smdl::LOG_LEVEL_ERROR, \
                                   ::smdl::concat(__VA_ARGS__))

/// The label prefix for the given log level, as the default log sinks
/// print it: a symbol or a bracketed word, with or without ANSI color
/// codes, and always followed by a space. See `LogSinks` for the table.
///
/// This is public so that a host with its own sink prints the same
/// labels as the default sinks without redefining them.
[[nodiscard]] SMDL_EXPORT std::string_view
logLevelLabel(LogLevel level, bool useColors, bool useUnicode) noexcept;

/// Use `<unistd.h>` on POSIX to test if cerr routes to a terminal.
[[nodiscard]] SMDL_EXPORT bool cerrSupportsANSIColors() noexcept;

/// Use `<unistd.h>` on POSIX to test if cout routes to a terminal.
[[nodiscard]] SMDL_EXPORT bool coutSupportsANSIColors() noexcept;

/// Does the environment claim UTF-8? Tests the locale variables in the
/// order the C library resolves them, `LC_ALL`, `LC_CTYPE`, then `LANG`,
/// and the first non-empty one decides, so that a specific `LC_CTYPE` is
/// not overruled by a stale `LANG`.
[[nodiscard]] SMDL_EXPORT bool localeIsUTF8() noexcept;

/// Resolve `mode` for a stream, given whether the stream is a terminal.
///
/// `UNICODE_MODE_AUTO` wants a terminal as well as a UTF-8 locale because
/// captured output is read back by tools, which match on the bracketed
/// words.
[[nodiscard]] SMDL_EXPORT bool shouldUseUnicode(UnicodeMode mode,
                                                bool isTerminal) noexcept;

/// The default log-sinks for convenience, which label each message as
/// `logLevelLabel()` does:
///
/// | Level   | ASCII     | Unicode                     | Color      |
/// |---------|-----------|-----------------------------|------------|
/// | `Debug` | `[debug]` | U+2699, a gear              | cyan       |
/// | `Info`  | `[info]`  | U+2139, an information sign | green      |
/// | `Warn`  | `[warn]`  | U+26A0, a warning sign      | yellow     |
/// | `Error` | `[error]` | U+2718, a heavy ballot X    | bright red |
///
/// The label is colored when the stream is a terminal, and is a symbol
/// when the sink's `UnicodeMode` resolves to one.
namespace LogSinks {

/// A default log sink to print to `std::cerr`.
class SMDL_EXPORT PrintToCerr final : public LogSink {
public:
  explicit PrintToCerr(UnicodeMode unicodeMode = UNICODE_MODE_AUTO) noexcept;

  void logMessage(LogLevel level, std::string_view message) final;

  /// Set whether messages are labeled with symbols. This is not mutex
  /// protected and, like `Logger::setMinLevel()`, is understood to be set
  /// once at program startup, before anything logs from another thread.
  void setUnicodeMode(UnicodeMode unicodeMode) noexcept;

private:
  bool mUseColors{};

  bool mUseUnicode{};
};

/// A default log sink to print to `std::cout`.
class SMDL_EXPORT PrintToCout final : public LogSink {
public:
  explicit PrintToCout(UnicodeMode unicodeMode = UNICODE_MODE_AUTO) noexcept;

  void logMessage(LogLevel level, std::string_view message) final;

  void flush() final;

  /// See `PrintToCerr::setUnicodeMode()`.
  void setUnicodeMode(UnicodeMode unicodeMode) noexcept;

private:
  bool mUseColors{};

  bool mUseUnicode{};
};

} // namespace LogSinks

/// \}

} // namespace smdl
