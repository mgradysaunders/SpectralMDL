/// \file
#pragma once

#include <functional>
#include <optional>
#include <stdexcept>
#include <string>

#include "smdl/Export.h"

namespace smdl {

/// \addtogroup support
/// \{

/// The error representation.
class SMDL_EXPORT Error final : public std::exception {
public:
  explicit Error(std::string message) : message(std::move(message)) {}

  Error(std::string message, std::string snippet)
      : message(std::move(message)), snippet(std::move(snippet)) {}

  /// Print to standard error.
  void print() const;

  /// Print to standard error and exit with `EXIT_FAILURE`.
  [[noreturn]] void printAndExit() const;

  [[nodiscard]] const char *what() const noexcept final {
    return message.c_str();
  }

public:
  /// The message.
  std::string message{};

  /// The source context to show beneath the message, if any. This is kept
  /// apart from `message` so that an error quoted inside another error, as
  /// the overload and construction candidate notes do, contributes its
  /// wording without dragging a second caret into the output.
  std::string snippet{};
};

/// Use C++ ABI to demangle the given name.
[[nodiscard]] SMDL_EXPORT std::string abiDemangle(const char *name);

/// Use C++ ABI to retrieve and demangle the current exception name.
[[nodiscard]] SMDL_EXPORT std::string abiDemangleExceptionName();

/// Run the given function, catch whatever it might throw, and return it as
/// an `Error` value.
template <typename F>
[[nodiscard]] inline std::optional<Error> catchAndReturnError(F &&f) try {
  std::invoke(std::forward<F>(f));
  return std::nullopt;
} catch (Error error) {
  return std::move(error);
} catch (const std::exception &error) {
  return Error("converted from " + abiDemangleExceptionName() + ": " +
               error.what());
} catch (...) {
  return Error("converted from " + abiDemangleExceptionName());
}

/// \}

} // namespace smdl
