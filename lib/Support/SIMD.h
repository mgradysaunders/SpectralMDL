/// \file
/// A minimal fixed-width vector type for the library's own hot loops.
///
/// Deliberately internal: this must never appear in a header under
/// `include/`. A GNU vector type is passed in vector registers only when
/// the target enables them, so a type like this crossing a translation
/// unit boundary would have a different calling convention on each side
/// whenever the library and its consumer disagree about
/// `SMDL_CXX_X86_ISA`. Inside `lib/`, which is one set of translation
/// units built with one set of flags, the question cannot arise.
///
/// Where the vector extension exists the same source is correct at every
/// instruction set level: an oversized vector is split into as many
/// registers as the target actually has, so raising the level makes this
/// faster and never makes it necessary.
///
/// The operation set is small on purpose: broadcast, load, store, add and
/// multiply. Shuffles, horizontal reductions, gathers and masks are all
/// absent because none of them has a portable spelling, and a kernel that
/// wants one should be restructured until it does not.
#pragma once

#include <cstddef>
#include <cstring>

// Predefine this to 0 to build the fallback on a compiler that does have
// the extension, which is how the fallback gets tested at all.
#ifndef SMDL_SIMD_VECTOR_EXTENSION
#if defined(__GNUC__) || defined(__clang__)
#define SMDL_SIMD_VECTOR_EXTENSION 1
#else
#define SMDL_SIMD_VECTOR_EXTENSION 0
#endif
#endif

namespace smdl::simd {

#if SMDL_SIMD_VECTOR_EXTENSION

/// The raw storage behind `Pack`, one specialization per supported width.
///
/// The widths are spelled out rather than computed because `vector_size`
/// is ignored on a dependent type: written as an alias inside the class
/// template it silently degrades to the scalar type on GCC.
template <typename T, std::size_t N> struct RawVector;

template <> struct RawVector<float, 4> {
  typedef float Type __attribute__((vector_size(16)));
};

template <> struct RawVector<float, 8> {
  typedef float Type __attribute__((vector_size(32)));
};

#else

/// The raw storage behind `Pack`: a plain array where the compiler has no
/// vector extension, leaving it to the auto-vectorizer.
template <typename T, std::size_t N> struct RawVector {
  using Type = T[N];
};

#endif

/// A pack of `N` values of `T`, operated on element-wise.
///
/// Loads and stores go through `memcpy` and so place no alignment
/// requirement on the caller, though an aligned address still lets the
/// target use its aligned instructions.
template <typename T, std::size_t N> struct Pack final {
  using Raw = typename RawVector<T, N>::Type;

  Pack() = default;

  /// Broadcast one value into every element.
  explicit Pack(T value) noexcept {
    for (std::size_t i = 0; i < N; i++) values[i] = value;
  }

  /// Load `N` consecutive values.
  [[nodiscard]] static Pack load(const T *from) noexcept {
    Pack pack;
    std::memcpy(&pack.values, from, sizeof(Raw));
    return pack;
  }

  /// Store `N` consecutive values.
  void store(T *to) const noexcept { std::memcpy(to, &values, sizeof(Raw)); }

  /// Add element-wise.
  [[nodiscard]] friend Pack operator+(const Pack &lhs,
                                      const Pack &rhs) noexcept {
    Pack pack;
#if SMDL_SIMD_VECTOR_EXTENSION
    pack.values = lhs.values + rhs.values;
#else
    for (std::size_t i = 0; i < N; i++)
      pack.values[i] = lhs.values[i] + rhs.values[i];
#endif
    return pack;
  }

  /// Multiply element-wise.
  [[nodiscard]] friend Pack operator*(const Pack &lhs,
                                      const Pack &rhs) noexcept {
    Pack pack;
#if SMDL_SIMD_VECTOR_EXTENSION
    pack.values = lhs.values * rhs.values;
#else
    for (std::size_t i = 0; i < N; i++)
      pack.values[i] = lhs.values[i] * rhs.values[i];
#endif
    return pack;
  }

  Raw values{};
};

/// A pack of 8 floats: one AVX register, or two SSE or NEON registers.
using float8 = Pack<float, 8>;

} // namespace smdl::simd
