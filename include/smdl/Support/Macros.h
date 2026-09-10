/// \file
#pragma once

#include <cstdint>
#include <cstring>
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

/// Promise that a pointer is the only access path to what it points at
/// for the lifetime of the declaration.
///
/// Its use is to keep the vectorizer from guarding a short loop with
/// runtime overlap checks, which for a loop of a few vector iterations
/// cost more than the loop: the checks are quadratic in the number of
/// distinct arrays the body touches. It is a promise the compiler cannot
/// verify, so the caller must genuinely pass distinct buffers.
#if defined(__GNUC__) || defined(__clang__) || defined(_MSC_VER)
#define SMDL_RESTRICT __restrict
#else
#define SMDL_RESTRICT
#endif

/// Promise that a pointer is aligned to the given power of two, and give
/// back the pointer with its type intact.
///
/// The promise travels with the returned pointer rather than with the
/// argument, so the result is what must be used (`p = SMDL_ASSUME_ALIGNED(p,
/// 32)`); as a bare statement this does nothing. Its use is to let the
/// vectorizer emit aligned moves and drop the scalar prologue that otherwise
/// walks a pointer of unknown alignment up to a vector boundary. It is a
/// promise the compiler cannot verify, and a pointer that is not actually so
/// aligned is undefined behavior. Pass a plain pointer variable: the argument
/// may be expanded more than once, and the unary plus is there to strip the
/// reference that `decltype` would otherwise deduce from a parenthesized one.
#if defined(__GNUC__) || defined(__clang__)
#define SMDL_ASSUME_ALIGNED(ptr, align) \
  ((decltype(+(ptr)))__builtin_assume_aligned((ptr), (align)))
#elif defined(_MSC_VER)
#define SMDL_ASSUME_ALIGNED(ptr, align)                                    \
  (__assume((reinterpret_cast<std::uintptr_t>(ptr) & ((align) - 1)) == 0), \
   (ptr))
#else
#define SMDL_ASSUME_ALIGNED(ptr, align) (ptr)
#endif

/// Mark a branch condition as almost always true (`SMDL_LIKELY`) or
/// almost always false (`SMDL_UNLIKELY`).
///
/// This is a hint to the optimizer's block layout, not to the hardware:
/// the branch predictor learns the direction on its own, so what the
/// hint moves is which successor falls through and which is placed out
/// of line, and how the optimizer weighs the two when it inlines,
/// spills, and if-converts. Reserve it for a hot path where the
/// condition is one-sided and the layout was measured to matter.
#if defined(__GNUC__) || defined(__clang__)
#define SMDL_LIKELY(cond) __builtin_expect(!!(cond), 1)
#define SMDL_UNLIKELY(cond) __builtin_expect(!!(cond), 0)
#else
#define SMDL_LIKELY(cond) (!!(cond))
#define SMDL_UNLIKELY(cond) (!!(cond))
#endif

template <typename To, typename From>
[[nodiscard]]
SMDL_ALWAYS_INLINE To bitCast(const From &from) noexcept {
  static_assert(sizeof(To) == sizeof(From) &&
                std::is_trivially_constructible_v<To>);
  To to;
  std::memcpy(&to, &from, sizeof(From));
  return to;
}

/// Sanity check a condition.
///
/// \note
/// This is two macros rather than one variadic macro that dispatches on the
/// argument count. The dispatching form is not portable: forwarding
/// `__VA_ARGS__` into a nested macro passes it as a *single* argument under
/// MSVC's traditional preprocessor, and supplying no argument at all for a
/// `...` parameter is only well-formed as of C++20.
///
#define SMDL_SANITY_CHECK(cond)                                     \
  do {                                                              \
    if (SMDL_UNLIKELY(!(cond)))                                     \
      ::smdl::detail::sanityCheckFailed(#cond, __FILE__, __LINE__); \
  } while (false)

/// Sanity check a condition, explaining what it means if it fails.
#define SMDL_SANITY_CHECK_MSG(cond, message)                                 \
  do {                                                                       \
    if (SMDL_UNLIKELY(!(cond)))                                              \
      ::smdl::detail::sanityCheckFailed(#cond, __FILE__, __LINE__, message); \
  } while (false)

#if !SMDL_DOXYGEN
namespace detail {

[[noreturn]] SMDL_EXPORT void sanityCheckFailed(const char *condition,
                                                const char *file, int line,
                                                const char *more = nullptr);

} // namespace detail
#endif // #if !SMDL_DOXYGEN

/// Sanity check a condition in a debug build only.
///
/// For an invariant on an operation small enough that the always-on
/// check would cost more than the work it guards: the size agreement of
/// two spectra about to be added band by band, say, which is three
/// instructions in front of six. Reserve it for invariants the library
/// establishes itself. A precondition a caller could get wrong, and
/// above all one that decides whether a load is in bounds, stays on the
/// always-on `SMDL_SANITY_CHECK`; where that one is too expensive for a
/// hot caller, give the caller an unchecked entry point instead of
/// weakening the checked one.
#ifdef NDEBUG
#define SMDL_DEBUG_CHECK(cond) ((void)0)
#define SMDL_DEBUG_CHECK_MSG(cond, message) ((void)0)
#else
#define SMDL_DEBUG_CHECK(cond) SMDL_SANITY_CHECK(cond)
#define SMDL_DEBUG_CHECK_MSG(cond, message) SMDL_SANITY_CHECK_MSG(cond, message)
#endif

/// Helper to implement `SMDL_CAT` correctly (Yes this is necessary!)
#define SMDL_CAT_HELPER(X, Y) X##Y

/// Concatenate macros.
#define SMDL_CAT(X, Y) SMDL_CAT_HELPER(X, Y)

/// Defer until end of scope.
#define SMDL_DEFER(...) \
  const auto SMDL_CAT(__defer, __LINE__) = ::smdl::detail::Defer(__VA_ARGS__)

/// Preserve values, restoring at end of scope.
#define SMDL_PRESERVE(...)                    \
  const auto SMDL_CAT(__preserve, __LINE__) = \
      ::smdl::detail::Preserve(__VA_ARGS__)

#if !SMDL_DOXYGEN
namespace detail {

template <typename T> class Defer final {
public:
  explicit constexpr Defer(T func) : mFunc(std::move(func)) {}
  Defer(const Defer &) = delete;
  Defer(Defer &&) = delete;
  ~Defer() { std::invoke(mFunc); }

private:
  T mFunc;
};

template <typename... Ts> class Preserve final {
public:
  explicit constexpr Preserve(Ts &...refs) : mRefs(refs...), mTmps(refs...) {}
  Preserve(const Preserve &) = delete;
  Preserve(Preserve &&) = delete;
  ~Preserve() { mRefs = mTmps; }

private:
  std::tuple<Ts &...> mRefs;
  std::tuple<Ts...> mTmps;
};

} // namespace detail
#endif // #if !SMDL_DOXYGEN

/// \}

} // namespace smdl
