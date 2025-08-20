/// \file
#pragma once

#include <functional>
#include <type_traits>
#include <utility>

#include "smdl/Export.h"

namespace smdl {

/// \addtogroup Support
/// \{

/// Expand the correct sanity check macro.
#define SMDL_SANITY_CHECK__EXPAND(X, Y, Z, ...) Z

/// Sanity check a condition.
#define SMDL_SANITY_CHECK__1(cond)                                             \
  do {                                                                         \
    if (!(cond))                                                               \
      ::smdl::detail::sanity_check_failed(#cond, __FILE__, __LINE__);          \
  } while (false)

/// Sanity check a condition with a message.
#define SMDL_SANITY_CHECK__2(cond, message)                                    \
  do {                                                                         \
    if (!(cond))                                                               \
      ::smdl::detail::sanity_check_failed(#cond, __FILE__, __LINE__, message); \
  } while (false)

/// Sanity check a condition with or without a message.
#define SMDL_SANITY_CHECK(...)                                                 \
  SMDL_SANITY_CHECK__EXPAND(__VA_ARGS__, SMDL_SANITY_CHECK__2,                 \
                            SMDL_SANITY_CHECK__1)(__VA_ARGS__)

namespace detail {

[[noreturn]] SMDL_EXPORT void sanity_check_failed(const char *condition,
                                                  const char *file, int line,
                                                  const char *more = nullptr);

} // namespace detail

/// Helper to implement `SMDL_CAT` correctly (Yes this is necessary!)
#define SMDL_CAT__HELPER(X, Y) X##Y

/// Concatenate macros.
#define SMDL_CAT(X, Y) SMDL_CAT__HELPER(X, Y)

/// Defer until end of scope.
#define SMDL_DEFER(...)                                                        \
  const auto SMDL_CAT(__defer, __LINE__) = ::smdl::detail::Defer(__VA_ARGS__)

/// Preserve values, restoring at end of scope.
#define SMDL_PRESERVE(...)                                                     \
  const auto SMDL_CAT(__preserve, __LINE__) =                                  \
      ::smdl::detail::Preserve(__VA_ARGS__)

namespace detail {

template <typename Lambda> class Defer final {
public:
  explicit constexpr Defer(Lambda f) : f(std::move(f)) {}

  Defer(const Defer &) = delete;

  Defer(Defer &&) = delete;

  ~Defer() { std::invoke(f); }

  Lambda f;
};

template <typename... Ts> class Preserve final {
public:
  explicit constexpr Preserve(Ts &...values)
      : values(values...), backupValues(values...) {}

  Preserve(const Preserve &) = delete;

  Preserve(Preserve &&) = delete;

  ~Preserve() { values = backupValues; }

private:
  std::tuple<Ts &...> values;

  std::tuple<Ts...> backupValues;
};

} // namespace detail

/// \}

} // namespace smdl
