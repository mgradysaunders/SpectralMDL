/// \file
/// A minimal fixed-width vector type for hot loops.
///
/// **A `Pack` must never appear in the signature of anything the library
/// exports, nor in any type it exports.** A GNU vector type is passed in
/// vector registers only when the target enables them, so a `Pack`
/// crossing the library boundary would have a different calling
/// convention on each side whenever the library and its consumer
/// disagree about `SMDL_CXX_X86_ISA`, which `lib` applies privately.
/// Within one translation unit, or across units a single project builds
/// with one set of flags, the question cannot arise. Take spans and
/// scalars at the boundary and form the packs behind it, which is what
/// every kernel here does anyway.
///
/// Where the vector extension exists the same source is correct at every
/// instruction set level: an oversized vector is split into as many
/// registers as the target actually has, so raising the level makes this
/// faster and never makes it necessary.
///
/// The operation set is what an element-wise kernel needs and no more:
/// broadcast, load, store, the four arithmetic operators, `sqrt`, `log`,
/// `min`, `max`, `abs`, and the comparisons, which yield a `Mask` that
/// `select` blends with. Lanes that must do different things are handled
/// by computing both sides and selecting, which is how a divergent loop is
/// written here. Everything is exact but `log`, which states its bound.
///
/// Shuffles and gathers remain absent for want of a portable spelling; a
/// kernel that wants one gets restructured until it does not. The one
/// horizontal operation is `anyTrue`/`allTrue`, which a masked loop needs
/// to know when to stop, and which is a store and a scalar scan rather
/// than a movemask, since that has no portable spelling either.
#pragma once

#include <cmath>
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

/// One float, so that a kernel written over a pack can be instantiated
/// at width one for a caller with a single item and stay the same code.
/// The compiler emits the scalar instruction for each operation, which
/// is what a hand-written scalar version would have been.
template <> struct RawVector<float, 1> {
  using Type = float __attribute__((vector_size(4)));
};

template <> struct RawVector<float, 4> {
  using Type = float __attribute__((vector_size(16)));
};

template <> struct RawVector<float, 8> {
  using Type = float __attribute__((vector_size(32)));
};

/// The raw storage behind `Mask`: the integer vector a comparison of
/// `RawVector<T, N>` yields, whose elements are all ones or all zeros.
template <typename T, std::size_t N> struct RawMask;

template <> struct RawMask<float, 1> {
  using Type = int __attribute__((vector_size(4)));
};

template <> struct RawMask<float, 4> {
  using Type = int __attribute__((vector_size(16)));
};

template <> struct RawMask<float, 8> {
  using Type = int __attribute__((vector_size(32)));
};

/// The unsigned integer vector the size of `RawVector<T, N>`, for a kernel
/// that works on the bit patterns of its lanes, where a borrow or a carry
/// out of a lane must wrap rather than overflow.
template <typename T, std::size_t N> struct RawBits;

template <> struct RawBits<float, 1> {
  using Type = unsigned __attribute__((vector_size(4)));
};

template <> struct RawBits<float, 4> {
  using Type = unsigned __attribute__((vector_size(16)));
};

template <> struct RawBits<float, 8> {
  using Type = unsigned __attribute__((vector_size(32)));
};

#else

/// The raw storage behind `Pack`: a plain array where the compiler has no
/// vector extension, leaving it to the auto-vectorizer.
template <typename T, std::size_t N> struct RawVector {
  using Type = T[N];
};

/// The raw storage behind `Mask` where there is no vector extension.
/// Elements are 0 or -1, matching what a vector comparison yields, so
/// that both arms of this header are written the same way.
template <typename T, std::size_t N> struct RawMask {
  using Type = int[N];
};

/// The raw bit patterns where there is no vector extension.
template <typename T, std::size_t N> struct RawBits {
  using Type = unsigned[N];
};

#endif

/// Reinterpret the bits of one trivially copyable type as another of the
/// same size. A vector-to-vector cast would do it where the extension
/// exists, but this spelling is the one that also compiles without it.
template <typename To, typename From>
[[nodiscard]] inline To bitCast(const From &from) noexcept {
  static_assert(sizeof(To) == sizeof(From));
  To to;
  std::memcpy(&to, &from, sizeof(To));
  return to;
}

/// Which lanes a comparison selected.
template <typename T, std::size_t N> struct Mask final {
  using Raw = typename RawMask<T, N>::Type;

  Mask() = default;

  /// Set every lane alike.
  explicit Mask(bool value) noexcept {
    for (std::size_t i = 0; i < N; i++) values[i] = value ? -1 : 0;
  }

  /// \name Lane-wise logic
  /// \{
  [[nodiscard]] friend Mask operator&(const Mask &lhs,
                                      const Mask &rhs) noexcept {
    Mask mask;
#if SMDL_SIMD_VECTOR_EXTENSION
    mask.values = lhs.values & rhs.values;
#else
    for (std::size_t i = 0; i < N; i++)
      mask.values[i] = lhs.values[i] & rhs.values[i];
#endif
    return mask;
  }

  [[nodiscard]] friend Mask operator|(const Mask &lhs,
                                      const Mask &rhs) noexcept {
    Mask mask;
#if SMDL_SIMD_VECTOR_EXTENSION
    mask.values = lhs.values | rhs.values;
#else
    for (std::size_t i = 0; i < N; i++)
      mask.values[i] = lhs.values[i] | rhs.values[i];
#endif
    return mask;
  }

  /// Lanes where the two differ, which is how a masked loop asks
  /// whether a sign changed.
  [[nodiscard]] friend Mask operator^(const Mask &lhs,
                                      const Mask &rhs) noexcept {
    Mask mask;
#if SMDL_SIMD_VECTOR_EXTENSION
    mask.values = lhs.values ^ rhs.values;
#else
    for (std::size_t i = 0; i < N; i++)
      mask.values[i] = lhs.values[i] ^ rhs.values[i];
#endif
    return mask;
  }

  [[nodiscard]] friend Mask operator~(const Mask &lhs) noexcept {
    Mask mask;
#if SMDL_SIMD_VECTOR_EXTENSION
    mask.values = ~lhs.values;
#else
    for (std::size_t i = 0; i < N; i++) mask.values[i] = ~lhs.values[i];
#endif
    return mask;
  }
  /// \}

  /// Is the lane set?
  [[nodiscard]] bool operator[](std::size_t i) const noexcept {
    return values[i] != 0;
  }

  Raw values{};
};

/// Is any lane set? The loop a masked kernel runs until every lane is
/// done needs this and nothing wider.
template <typename T, std::size_t N>
[[nodiscard]] inline bool anyTrue(const Mask<T, N> &mask) noexcept {
  for (std::size_t i = 0; i < N; i++)
    if (mask.values[i] != 0) return true;
  return false;
}

/// Is every lane set?
template <typename T, std::size_t N>
[[nodiscard]] inline bool allTrue(const Mask<T, N> &mask) noexcept {
  for (std::size_t i = 0; i < N; i++)
    if (mask.values[i] == 0) return false;
  return true;
}

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

  /// Subtract element-wise.
  [[nodiscard]] friend Pack operator-(const Pack &lhs,
                                      const Pack &rhs) noexcept {
    Pack pack;
#if SMDL_SIMD_VECTOR_EXTENSION
    pack.values = lhs.values - rhs.values;
#else
    for (std::size_t i = 0; i < N; i++)
      pack.values[i] = lhs.values[i] - rhs.values[i];
#endif
    return pack;
  }

  /// Divide element-wise.
  [[nodiscard]] friend Pack operator/(const Pack &lhs,
                                      const Pack &rhs) noexcept {
    Pack pack;
#if SMDL_SIMD_VECTOR_EXTENSION
    pack.values = lhs.values / rhs.values;
#else
    for (std::size_t i = 0; i < N; i++)
      pack.values[i] = lhs.values[i] / rhs.values[i];
#endif
    return pack;
  }

  /// Negate element-wise.
  [[nodiscard]] friend Pack operator-(const Pack &lhs) noexcept {
    Pack pack;
#if SMDL_SIMD_VECTOR_EXTENSION
    pack.values = -lhs.values;
#else
    for (std::size_t i = 0; i < N; i++) pack.values[i] = -lhs.values[i];
#endif
    return pack;
  }

  /// \name Comparisons
  ///
  /// Each yields the lanes on which it holds. A vector comparison already
  /// produces all ones or all zeros per lane, which is what `Mask` is.
  /// \{
#if SMDL_SIMD_VECTOR_EXTENSION
#define SMDL_SIMD_COMPARE(op)                                             \
  [[nodiscard]] friend Mask<T, N> operator op(const Pack &lhs,            \
                                              const Pack &rhs) noexcept { \
    Mask<T, N> mask;                                                      \
    mask.values = lhs.values op rhs.values;                               \
    return mask;                                                          \
  }
#else
#define SMDL_SIMD_COMPARE(op)                                             \
  [[nodiscard]] friend Mask<T, N> operator op(const Pack &lhs,            \
                                              const Pack &rhs) noexcept { \
    Mask<T, N> mask;                                                      \
    for (std::size_t i = 0; i < N; i++)                                   \
      mask.values[i] = (lhs.values[i] op rhs.values[i]) ? -1 : 0;         \
    return mask;                                                          \
  }
#endif
  SMDL_SIMD_COMPARE(<)
  SMDL_SIMD_COMPARE(<=)
  SMDL_SIMD_COMPARE(>)
  SMDL_SIMD_COMPARE(>=)
  SMDL_SIMD_COMPARE(==)
  SMDL_SIMD_COMPARE(!=)
#undef SMDL_SIMD_COMPARE
  /// \}

  /// Read one lane. Present for the edges of a kernel, never for its
  /// inner loop, where a scalar read defeats the point.
  [[nodiscard]] T operator[](std::size_t i) const noexcept { return values[i]; }

  Raw values{};
};

/// Take `lhs` where the mask is set and `rhs` where it is not.
template <typename T, std::size_t N>
[[nodiscard]] inline Pack<T, N> select(const Mask<T, N> &mask,
                                       const Pack<T, N> &lhs,
                                       const Pack<T, N> &rhs) noexcept {
  Pack<T, N> pack;
#if SMDL_SIMD_VECTOR_EXTENSION
  using RawMaskType = typename Mask<T, N>::Raw;
  const RawMaskType a{bitCast<RawMaskType>(lhs.values)};
  const RawMaskType b{bitCast<RawMaskType>(rhs.values)};
  pack.values =
      bitCast<typename Pack<T, N>::Raw>((mask.values & a) | (~mask.values & b));
#else
  for (std::size_t i = 0; i < N; i++)
    pack.values[i] = mask.values[i] ? lhs.values[i] : rhs.values[i];
#endif
  return pack;
}

/// The square root of every element.
template <typename T, std::size_t N>
[[nodiscard]] inline Pack<T, N> sqrt(const Pack<T, N> &pack) noexcept {
  Pack<T, N> result;
#if SMDL_SIMD_VECTOR_EXTENSION && defined(__clang__)
  result.values = __builtin_elementwise_sqrt(pack.values);
#else
  // The loop is what GCC and the fallback get. With '-fno-math-errno',
  // which this project sets, it vectorizes to the same instruction.
  for (std::size_t i = 0; i < N; i++)
    result.values[i] = std::sqrt(pack.values[i]);
#endif
  return result;
}

/// The natural logarithm of every element, each a positive normal float,
/// to 3e-7 relative to \f$ \ln x \f$ away from 1 and 1e-7 absolute within
/// [0.5, 2], exactly 0 at 1. The domain and the bounds are those of
/// `fastLog` (`RenderUtil/FastMath.h`), and a lane outside the domain,
/// which a kernel may compute and select away, holds an unspecified value.
///
/// The reduction is `fastLog`'s, with the fold of the mantissa into
/// [2/3, 4/3) done in integer lanes rather than by a branch: subtracting
/// the bits of 2/3 borrows out of the exponent field exactly when the
/// mantissa is below 4/3, so the arithmetic shift yields the exponent `n`,
/// and adding the bits back under the mantissa mask rebuilds the mantissa
/// `m` to go with it. With `r = m - 1` the result is
/// \f$ n \ln 2 + r + r^2 q(r) \f$, `q` the degree-6 polynomial minimizing
/// the absolute error over the fold, in Estrin's form so that no chain of
/// dependent multiplies is longer than four.
///
/// `fastLog`'s series in (m - 1) / (m + 1) needs fewer terms, but puts a
/// division on the critical path, and the table-driven reduction of a
/// scalar libm needs a gather.
template <std::size_t N>
[[nodiscard]] inline Pack<float, N> log(const Pack<float, N> &pack) noexcept {
  using P = Pack<float, N>;
  constexpr unsigned FOLD_BITS{0x3F2AAAABu}; // 2/3
  constexpr unsigned MANTISSA_MASK{0x007FFFFFu};
  P n;
  P m;
#if SMDL_SIMD_VECTOR_EXTENSION
  using Bits = typename RawBits<float, N>::Type;
  using Ints = typename Mask<float, N>::Raw;
  const Bits bits{bitCast<Bits>(pack.values) - FOLD_BITS};
  const Ints e{bitCast<Ints>(bits) >> 23};
  for (std::size_t i = 0; i < N; i++) n.values[i] = float(e[i]);
  m.values = bitCast<typename P::Raw>((bits & MANTISSA_MASK) + FOLD_BITS);
#else
  for (std::size_t i = 0; i < N; i++) {
    const unsigned bits{bitCast<unsigned>(pack.values[i]) - FOLD_BITS};
    n.values[i] = float(int(bits) >> 23);
    m.values[i] = bitCast<float>((bits & MANTISSA_MASK) + FOLD_BITS);
  }
#endif
  const P r{m - P(1.0f)};
  const P r2{r * r};
  const P r4{r2 * r2};
  const P q{
      (P(-0.499998331f) + P(0.333380431f) * r) +
      r2 * (P(-0.250143409f) + P(0.197843447f) * r) +
      r4 * ((P(-0.163074389f) + P(0.171943158f) * r) + r2 * P(-0.159186885f))};
  return (n * P(0.6931472f) + r) + r2 * q;
}

/// \name Element-wise minimum, maximum and magnitude
/// \{
template <typename T, std::size_t N>
[[nodiscard]] inline Pack<T, N> min(const Pack<T, N> &lhs,
                                    const Pack<T, N> &rhs) noexcept {
  return select(lhs < rhs, lhs, rhs);
}

template <typename T, std::size_t N>
[[nodiscard]] inline Pack<T, N> max(const Pack<T, N> &lhs,
                                    const Pack<T, N> &rhs) noexcept {
  return select(lhs > rhs, lhs, rhs);
}

template <typename T, std::size_t N>
[[nodiscard]] inline Pack<T, N> abs(const Pack<T, N> &pack) noexcept {
  return max(pack, -pack);
}
/// \}

/// A pack of one float, for instantiating a packed kernel at width one.
using float1 = Pack<float, 1>;

/// A pack of 4 floats: one SSE or NEON register.
using float4 = Pack<float, 4>;

/// A pack of 8 floats: one AVX register, or two SSE or NEON registers.
using float8 = Pack<float, 8>;

} // namespace smdl::simd
