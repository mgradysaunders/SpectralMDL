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

/// Whether output is colored with ANSI escape codes: the default log
/// sinks' messages, and the report `Compiler::runUnitTests()` prints. See
/// `shouldUseColors()`.
enum ANSIColorMode : int {
  ANSI_COLOR_MODE_AUTO,   ///< Colorize a terminal, if the environment allows.
  ANSI_COLOR_MODE_ALWAYS, ///< Colorize even if the stream is redirected.
  ANSI_COLOR_MODE_NEVER   ///< Never colorize.
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
/// The library words a warning or an error like an `Error` message,
/// starting in lowercase unless its first word is a name, and a debug or
/// info message as a sentence starting in uppercase. Neither ends in a
/// period.
#define SMDL_LOG(level, ...)                                            \
  do {                                                                  \
    const auto smdlLogLevel{level};                                     \
    if (auto &smdlLogger{::smdl::Logger::get()};                        \
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
/// print it: a symbol or a bracketed word, with or without ANSI color
/// codes, and always followed by a space. See `LogSinks` for the table.
///
/// This is public so that a host with its own sink prints the same
/// labels as the default sinks without redefining them.
[[nodiscard]] SMDL_EXPORT std::string_view
logLevelLabel(LogLevel level, bool useColors, bool useUnicode) noexcept;

/// A message as the default log sinks print it, `logLevelLabel()` and
/// then the message, without the trailing newline.
///
/// Without colors the message is exactly as given. With them, a debug
/// message is dimmed whole, and any other has its locations (as
/// `LocationMarkup` writes them) in bold and its single-quoted strings in
/// cyan. A source line followed by a caret line is left as written,
/// except that its gutter is dimmed and the caret is green. A message
/// that already contains an escape code is left as it is.
///
/// This reads the finished text, so the message a sink is handed stays
/// plain, and a host with its own sink renders the same by calling this.
[[nodiscard]] SMDL_EXPORT std::string formatLogMessage(LogLevel level,
                                                       std::string_view message,
                                                       bool useColors,
                                                       bool useUnicode);

/// Use `<unistd.h>` on POSIX to test if cerr routes to a terminal. See
/// `shouldUseColors()` for whether it should be colored.
[[nodiscard]] SMDL_EXPORT bool cerrSupportsANSIColors() noexcept;

/// Use `<unistd.h>` on POSIX to test if cout routes to a terminal. See
/// `shouldUseColors()` for whether it should be colored.
[[nodiscard]] SMDL_EXPORT bool coutSupportsANSIColors() noexcept;

/// Resolve `mode` for a stream, given whether the stream is a terminal.
///
/// `ANSI_COLOR_MODE_AUTO` also wants the environment to allow colors:
/// `NO_COLOR` unset or empty (the no-color.org convention), and `TERM`
/// set to something other than `dumb`. The explicit modes override both,
/// as that convention asks.
[[nodiscard]] SMDL_EXPORT bool shouldUseColors(ANSIColorMode mode,
                                               bool isTerminal) noexcept;

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

/// The default log-sinks for convenience, which print each message as
/// `formatLogMessage()` does, labeled as `logLevelLabel()` does:
///
/// | Level   | ASCII     | Unicode                     | Color      |
/// |---------|-----------|-----------------------------|------------|
/// | `Debug` | `[debug]` | U+2699, a gear              | cyan, dim  |
/// | `Info`  | `[info]`  | U+2139, an information sign | green      |
/// | `Warn`  | `[warn]`  | U+26A0, a warning sign      | yellow     |
/// | `Error` | `[error]` | U+2718, a heavy ballot X    | bright red |
///
/// The message is colored when the sink's `ANSIColorMode` resolves to
/// colors, and labeled with a symbol when its `UnicodeMode` resolves to
/// one.
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

  /// Set whether messages are colored, which is `ANSI_COLOR_MODE_AUTO`
  /// until this is called. See `setUnicodeMode()` for when to call it.
  void setColorMode(ANSIColorMode colorMode) noexcept;

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

  /// See `PrintToCerr::setColorMode()`.
  void setColorMode(ANSIColorMode colorMode) noexcept;

private:
  bool mUseColors{};

  bool mUseUnicode{};
};

} // namespace LogSinks

/// \}

} // namespace smdl
