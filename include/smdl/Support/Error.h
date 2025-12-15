/// \file
#pragma once

#include <functional>
#include <optional>
#include <stdexcept>
#include <string>

#include "smdl/Export.h"

namespace smdl {

/// \addtogroup Support
/// \{

/// The error representation.
class SMDL_EXPORT Error final : public std::exception {
public:
  explicit Error(std::string message) : message(std::move(message)) {}

  /// Print to standard error.
  void print() const;

  /// Print to standard error and exit with `EXIT_FAILURE`.
  void printAndExit() const;

  const char *what() const noexcept final { return message.c_str(); }

public:
  /// The message.
  std::string message{};
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
