/// \file
#pragma once

#include <functional>
#include <type_traits>
#include <utility>

#include "smdl/Export.h"

namespace smdl {

/// \addtogroup support
/// \{

/// Force a function to be inlined into every caller.
///
/// This is a demand, not a hint: it overrides the inliner's cost model, so
/// it is only correct where the caller genuinely benefits from seeing the
/// body (constant folding across the boundary, or a hot leaf whose call
/// overhead rivals its work). Reserve it for measured cases.
#if defined(__GNUC__) || defined(__clang__)
#define SMDL_ALWAYS_INLINE inline __attribute__((always_inline))
#elif defined(_MSC_VER)
#define SMDL_ALWAYS_INLINE __forceinline
#else
#define SMDL_ALWAYS_INLINE inline
#endif

/// Forbid a function from being inlined into any caller.
///
/// Useful to keep a cold path (error reporting, a rare slow branch) out of
/// the instruction cache and out of its caller's register allocation, and
/// to keep a function addressable as a distinct symbol in a profile.
#if defined(__GNUC__) || defined(__clang__)
#define SMDL_NO_INLINE __attribute__((noinline))
#elif defined(_MSC_VER)
#define SMDL_NO_INLINE __declspec(noinline)
#else
#define SMDL_NO_INLINE
#endif

/// Sanity check a condition.
///
/// \note
/// This is two macros rather than one variadic macro that dispatches on the
/// argument count. The dispatching form is not portable: forwarding
/// `__VA_ARGS__` into a nested macro passes it as a *single* argument under
/// MSVC's traditional preprocessor, and supplying no argument at all for a
/// `...` parameter is only well-formed as of C++20.
///
#define SMDL_SANITY_CHECK(cond)                                                \
  do {                                                                         \
    if (!(cond)) ::smdl::detail::sanityCheckFailed(#cond, __FILE__, __LINE__); \
  } while (false)

/// Sanity check a condition, explaining what it means if it fails.
#define SMDL_SANITY_CHECK_MSG(cond, message)                                 \
  do {                                                                       \
    if (!(cond))                                                             \
      ::smdl::detail::sanityCheckFailed(#cond, __FILE__, __LINE__, message); \
  } while (false)

namespace detail {

[[noreturn]] SMDL_EXPORT void sanityCheckFailed(const char *condition,
                                                const char *file, int line,
                                                const char *more = nullptr);

} // namespace detail

/// Helper to implement `SMDL_CAT` correctly (Yes this is necessary!)
#define SMDL_CAT__HELPER(X, Y) X##Y

/// Concatenate macros.
#define SMDL_CAT(X, Y) SMDL_CAT__HELPER(X, Y)

/// Defer until end of scope.
#define SMDL_DEFER(...) \
  const auto SMDL_CAT(__defer, __LINE__) = ::smdl::detail::Defer(__VA_ARGS__)

/// Preserve values, restoring at end of scope.
#define SMDL_PRESERVE(...)                    \
  const auto SMDL_CAT(__preserve, __LINE__) = \
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
