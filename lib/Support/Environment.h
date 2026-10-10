/// \file
/// The `SMDL_*` environment variables, which let a session be debugged
/// without the host exposing every setting. `README.md` lists them for
/// the user.
///
/// Every variable is read once per process, the first time any of them
/// is needed, so it holds still across recompiles. Each is one of two
/// kinds:
///
/// - An `EnvOverride` overrides a setting the host chooses through the
///   API. It never writes the host's field: the setting in effect is
///   resolved where it is used, so the host reads back what it set.
/// - A diagnostic asks for output the API does not offer, like the perf
///   map, so it has no host setting to override.
///
/// No variable may change what the host can look up, or the size of
/// anything the host and the JIT-compiled code share.
#pragma once

#include <atomic>
#include <optional>
#include <string>

#include "smdl/Compiler.h"
#include "smdl/Support/Logger.h"

namespace smdl {

/// An environment variable that overrides a setting the host chooses.
///
/// Unset or empty, it defers to the host. Set to something it cannot
/// parse, it defers to the host too, and says so.
template <typename T> class EnvOverride final {
public:
  /// Read the variable `name`, which overrides `what`, a noun phrase
  /// such as `"the optimization level"`.
  EnvOverride(const char *name, const char *what);

  EnvOverride(const EnvOverride &) = delete;

  /// The value, or none if the variable is unset, empty, or malformed.
  [[nodiscard]] const std::optional<T> &value() const noexcept {
    return mValue;
  }

  /// The setting in effect: the variable's value if it has one, else
  /// `hostValue`. Reports as `report()` does.
  [[nodiscard]] T resolve(T hostValue) const {
    report(hostValue);
    return mValue.value_or(hostValue);
  }

  /// Say at warning level that the variable is malformed, or that it
  /// overrides `hostValue` with something else. Only the first call that
  /// finds either says anything, once per process.
  void report(T hostValue) const;

private:
  /// The name, e.g., `SMDL_OPT_LEVEL`.
  const char *mName{};

  /// What the variable overrides, for the report.
  const char *mWhat{};

  /// The text the variable is set to, or empty if unset.
  std::string mText{};

  /// See `value()`.
  std::optional<T> mValue{};

  /// Has `report()` said anything?
  mutable std::atomic<bool> mIsReported{};
};

extern template class EnvOverride<bool>;
extern template class EnvOverride<OptLevel>;
extern template class EnvOverride<LogLevel>;

/// The `SMDL_*` environment variables.
class Environment final {
public:
  /// Get the variables, reading them on the first call.
  [[nodiscard]] static const Environment &get();

  Environment(const Environment &) = delete;

private:
  Environment();

public:
  /// `SMDL_DEBUG` overrides `Compiler::isDebugEnabled`, which is what
  /// `$DEBUG` is.
  EnvOverride<bool> debug{"SMDL_DEBUG", "$DEBUG"};

  /// `SMDL_OPT_LEVEL` overrides the level `Compiler::compile()` is given.
  EnvOverride<OptLevel> optLevel{"SMDL_OPT_LEVEL", "the optimization level"};

  /// `SMDL_LOG_LEVEL` overrides `Logger::setMinLevel()`.
  EnvOverride<LogLevel> logLevel{"SMDL_LOG_LEVEL", "the log level"};

  /// `SMDL_LLVM_ARGS`, options for LLVM itself as a command line would
  /// give them, or empty for none.
  std::string llvmArgs;

  /// `SMDL_DUMP_IR`, the directory `Compiler::compile()` writes LLVM-IR
  /// to, or empty for none.
  std::string dumpIRDir;

  /// `SMDL_PERF_MAP`: does `Compiler::jitCompile()` write a perf map?
  bool shouldWritePerfMap{};

  /// `SMDL_GDB_JIT`: does the JIT register the code it links with GDB's
  /// JIT interface?
  bool shouldRegisterWithGDB{};
};

} // namespace smdl
