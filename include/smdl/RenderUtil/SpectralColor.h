/// \file
#pragma once

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <cstring>

#include "smdl/Export.h"
#include "smdl/Support/Macros.h"
#include "smdl/Support/Span.h"

namespace smdl {

/// \addtogroup renderutil
/// \{

/// A color vector: a runtime-sized vector of per-band amplitudes.
///
/// The band count of a spectral render is a runtime constant, so this
/// carries its size instead of a template parameter. Up to
/// `INLINE_CAPACITY` bands live in the object itself, which is the
/// historical fixed size of 16, so a default 16-band render allocates
/// nothing; larger band counts spill to the heap.
///
/// Performance note: whenever the storage is inline, arithmetic runs
/// fixed-length loops over the full `INLINE_CAPACITY` and copies move
/// the whole inline buffer, so the compiler unrolls and vectorizes
/// exactly as it did when the size was a compile-time 16. This is
/// sound because the inline buffer is zero-initialized at construction
/// and only ever written by these same fixed-length operations, so
/// lanes at or beyond `size()` always hold initialized floats; they
/// may compute to NaN (for example a division's `0/0`), so the
/// predicates, which also test every inline lane to stay branch-free,
/// mask the lanes past the size out of their verdict. The reductions
/// that fold a running value keep the size-bounded loop: its back-edge
/// is predicted for free, and every masked fixed-length form measured
/// no leaner. The extrema split the fold across four accumulators,
/// which is the one place this class reassociates: a single running
/// `max` is a chain of sixteen dependent instructions, and the compiler
/// will not break it on its own because reassociating an extremum
/// changes what it does with a NaN.
///
/// Sizes must agree in mixed operations; this is sanity-checked, not
/// reconciled. The default constructor makes an empty vector, which is
/// a valid operand of nothing; give a size before doing arithmetic.
/// Deliberately not `final`: a renderer wants a subclass whose default
/// constructor supplies its render-wide band count.
class SpectralColor {
public:
  /// The number of bands stored inline.
  static constexpr size_t INLINE_CAPACITY = 16;

  /// Construct empty.
  SpectralColor() = default;

  /// Construct with `size` bands of `value`.
  explicit SpectralColor(size_t size, float value = 0.0f) {
    reallocate(size);
    if (SMDL_LIKELY(isInline())) {
      // NOLINTNEXTLINE
      for (size_t i = 0; i < INLINE_CAPACITY; i++) mBuf[i] = value;
    } else {
      for (size_t i = 0; i < mSize; i++) mPtr[i] = value;
    }
  }

  /// Construct with `values.size()` bands copied from `values`.
  explicit SpectralColor(Span<const float> values) {
    reallocate(values.size());
    if (mSize > 0) std::memcpy(mPtr, values.data(), mSize * sizeof(float));
  }

  SpectralColor(const SpectralColor &other) {
    if (SMDL_LIKELY(other.isInline())) {
      std::memcpy(mBuf, other.mBuf, sizeof(mBuf));
      mSize = other.mSize;
    } else {
      reallocate(other.mSize);
      std::memcpy(mPtr, other.mPtr, mSize * sizeof(float));
    }
  }

  SpectralColor(SpectralColor &&other) noexcept { stealOrCopy(other); }

  SpectralColor &operator=(const SpectralColor &other) {
    if (this != &other) {
      if (SMDL_LIKELY(other.isInline() && isInline())) {
        std::memcpy(mBuf, other.mBuf, sizeof(mBuf));
        mSize = other.mSize;
      } else {
        reallocate(other.mSize);
        if (mSize > 0) std::memcpy(mPtr, other.mPtr, mSize * sizeof(float));
      }
    }
    return *this;
  }

  SpectralColor &operator=(SpectralColor &&other) noexcept {
    if (this != &other) {
      if (SMDL_UNLIKELY(!isInline())) delete[] mPtr;
      stealOrCopy(other);
    }
    return *this;
  }

  ~SpectralColor() {
    if (SMDL_UNLIKELY(!isInline())) delete[] mPtr;
    mPtr = mBuf;
    mSize = 0;
  }

public:
  [[nodiscard]] size_t size() const noexcept { return mSize; }

  [[nodiscard]] float *data() noexcept { return mPtr; }

  [[nodiscard]] const float *data() const noexcept { return mPtr; }

  [[nodiscard]] float &operator[](size_t i) noexcept { return mPtr[i]; }

  [[nodiscard]] const float &operator[](size_t i) const noexcept {
    return mPtr[i];
  }

public:
  SpectralColor &operator+=(const SpectralColor &rhs) noexcept {
    SMDL_DEBUG_CHECK(mSize == rhs.mSize);
    if (SMDL_LIKELY(isInline())) {
      // Through a local rather than straight into `mBuf`, which reads as
      // the obvious form and does not vectorize: nothing rules out
      // `this == &rhs`, so a store to lane i may alias the load of lane
      // j, and the fixed trip count is short enough that the unroller
      // reaches it before the vectorizer does. The local cannot escape,
      // so its stores provably alias neither operand, the loads hoist
      // above them, and the copy back folds into the same vectors.
      // NOLINTNEXTLINE
      alignas(32) float tmp[INLINE_CAPACITY];
      for (size_t i = 0; i < INLINE_CAPACITY; i++)
        tmp[i] = mBuf[i] + rhs.mBuf[i];
      for (size_t i = 0; i < INLINE_CAPACITY; i++) mBuf[i] = tmp[i];
    } else {
      for (size_t i = 0; i < mSize; i++) mPtr[i] += rhs.mPtr[i];
    }
    return *this;
  }

  SpectralColor &operator-=(const SpectralColor &rhs) noexcept {
    SMDL_DEBUG_CHECK(mSize == rhs.mSize);
    if (SMDL_LIKELY(isInline())) {
      // NOLINTNEXTLINE
      alignas(32) float tmp[INLINE_CAPACITY];
      for (size_t i = 0; i < INLINE_CAPACITY; i++)
        tmp[i] = mBuf[i] - rhs.mBuf[i];
      for (size_t i = 0; i < INLINE_CAPACITY; i++) mBuf[i] = tmp[i];
    } else {
      for (size_t i = 0; i < mSize; i++) mPtr[i] -= rhs.mPtr[i];
    }
    return *this;
  }

  SpectralColor &operator*=(const SpectralColor &rhs) noexcept {
    SMDL_DEBUG_CHECK(mSize == rhs.mSize);
    if (SMDL_LIKELY(isInline())) {
      // NOLINTNEXTLINE
      alignas(32) float tmp[INLINE_CAPACITY];
      for (size_t i = 0; i < INLINE_CAPACITY; i++)
        tmp[i] = mBuf[i] * rhs.mBuf[i];
      for (size_t i = 0; i < INLINE_CAPACITY; i++) mBuf[i] = tmp[i];
    } else {
      for (size_t i = 0; i < mSize; i++) mPtr[i] *= rhs.mPtr[i];
    }
    return *this;
  }

  SpectralColor &operator/=(const SpectralColor &rhs) noexcept {
    SMDL_DEBUG_CHECK(mSize == rhs.mSize);
    if (SMDL_LIKELY(isInline())) {
      // NOLINTNEXTLINE
      alignas(32) float tmp[INLINE_CAPACITY];
      for (size_t i = 0; i < INLINE_CAPACITY; i++)
        tmp[i] = mBuf[i] / rhs.mBuf[i];
      for (size_t i = 0; i < INLINE_CAPACITY; i++) mBuf[i] = tmp[i];
    } else {
      for (size_t i = 0; i < mSize; i++) mPtr[i] /= rhs.mPtr[i];
    }
    return *this;
  }

  SpectralColor &operator+=(float rhs) noexcept {
    if (SMDL_LIKELY(isInline())) {
      // NOLINTNEXTLINE
      for (size_t i = 0; i < INLINE_CAPACITY; i++) mBuf[i] += rhs;
    } else {
      for (size_t i = 0; i < mSize; i++) mPtr[i] += rhs;
    }
    return *this;
  }

  SpectralColor &operator-=(float rhs) noexcept {
    if (SMDL_LIKELY(isInline())) {
      // NOLINTNEXTLINE
      for (size_t i = 0; i < INLINE_CAPACITY; i++) mBuf[i] -= rhs;
    } else {
      for (size_t i = 0; i < mSize; i++) mPtr[i] -= rhs;
    }
    return *this;
  }

  SpectralColor &operator*=(float rhs) noexcept {
    if (SMDL_LIKELY(isInline())) {
      // NOLINTNEXTLINE
      for (size_t i = 0; i < INLINE_CAPACITY; i++) mBuf[i] *= rhs;
    } else {
      for (size_t i = 0; i < mSize; i++) mPtr[i] *= rhs;
    }
    return *this;
  }

  SpectralColor &operator/=(float rhs) noexcept {
    if (SMDL_LIKELY(isInline())) {
      // NOLINTNEXTLINE
      for (size_t i = 0; i < INLINE_CAPACITY; i++) mBuf[i] /= rhs;
    } else {
      for (size_t i = 0; i < mSize; i++) mPtr[i] /= rhs;
    }
    return *this;
  }

  [[nodiscard]] SpectralColor operator+() const { return *this; }

  [[nodiscard]] SpectralColor operator-() const {
    SpectralColor result{*this};
    if (SMDL_LIKELY(result.isInline())) {
      // NOLINTNEXTLINE
      for (size_t i = 0; i < INLINE_CAPACITY; i++)
        result.mBuf[i] = -result.mBuf[i];
    } else {
      for (size_t i = 0; i < result.mSize; i++)
        result.mPtr[i] = -result.mPtr[i];
    }
    return result;
  }

  [[nodiscard]] SpectralColor operator+(const SpectralColor &rhs) const {
    SpectralColor result{*this};
    result += rhs;
    return result;
  }

  [[nodiscard]] SpectralColor operator-(const SpectralColor &rhs) const {
    SpectralColor result{*this};
    result -= rhs;
    return result;
  }

  [[nodiscard]] SpectralColor operator*(const SpectralColor &rhs) const {
    SpectralColor result{*this};
    result *= rhs;
    return result;
  }

  [[nodiscard]] SpectralColor operator/(const SpectralColor &rhs) const {
    SpectralColor result{*this};
    result /= rhs;
    return result;
  }

  [[nodiscard]] SpectralColor operator+(float rhs) const {
    SpectralColor result{*this};
    result += rhs;
    return result;
  }

  [[nodiscard]] SpectralColor operator-(float rhs) const {
    SpectralColor result{*this};
    result -= rhs;
    return result;
  }

  [[nodiscard]] SpectralColor operator*(float rhs) const {
    SpectralColor result{*this};
    result *= rhs;
    return result;
  }

  [[nodiscard]] SpectralColor operator/(float rhs) const {
    SpectralColor result{*this};
    result /= rhs;
    return result;
  }

  [[nodiscard]] friend SpectralColor operator+(float lhs,
                                               const SpectralColor &rhs) {
    return rhs + lhs;
  }

  [[nodiscard]] friend SpectralColor operator-(float lhs,
                                               const SpectralColor &rhs) {
    SpectralColor result{rhs.mSize, lhs};
    result -= rhs;
    return result;
  }

  [[nodiscard]] friend SpectralColor operator*(float lhs,
                                               const SpectralColor &rhs) {
    return rhs * lhs;
  }

  [[nodiscard]] friend SpectralColor operator/(float lhs,
                                               const SpectralColor &rhs) {
    SpectralColor result{rhs.mSize, lhs};
    result /= rhs;
    return result;
  }

public:
  /// Is all exactly or approximately zero?
  ///
  /// \param[in] thresh
  /// The threshold. This is zero by default so we only detect
  /// exactly black color spectra. If set to something small,
  /// the implementation also detects nearly black color
  /// spectra.
  ///
  [[nodiscard]] bool isAllZero(float thresh = 0.0f) const noexcept {
    if (SMDL_LIKELY(isInline())) {
      uint32_t bits{};
      for (size_t i = 0; i < INLINE_CAPACITY; i++)
        bits |= uint32_t(!(std::abs(mBuf[i]) <= thresh)) << i;
      return (bits & liveLanes()) == 0;
    }
    for (size_t i = 0; i < mSize; i++)
      if (!(std::abs(mPtr[i]) <= thresh)) return false;
    return true;
  }

  /// Is any component infinite?
  [[nodiscard]] bool isAnyInf() const noexcept {
    if (SMDL_LIKELY(isInline())) {
      uint32_t bits{};
      for (size_t i = 0; i < INLINE_CAPACITY; i++)
        bits |= uint32_t(std::isinf(mBuf[i])) << i;
      return (bits & liveLanes()) != 0;
    }
    for (size_t i = 0; i < mSize; i++)
      if (std::isinf(mPtr[i])) return true;
    return false;
  }

  /// Is any component not-a-number?
  [[nodiscard]] bool isAnyNan() const noexcept {
    if (SMDL_LIKELY(isInline())) {
      uint32_t bits{};
      for (size_t i = 0; i < INLINE_CAPACITY; i++)
        bits |= uint32_t(std::isnan(mBuf[i])) << i;
      return (bits & liveLanes()) != 0;
    }
    for (size_t i = 0; i < mSize; i++)
      if (std::isnan(mPtr[i])) return true;
    return false;
  }

  /// Is any component either infinite or not-a-number?
  [[nodiscard]] bool isAnyNonFinite() const noexcept {
    if (SMDL_LIKELY(isInline())) {
      uint32_t bits{};
      for (size_t i = 0; i < INLINE_CAPACITY; i++)
        bits |= uint32_t(!std::isfinite(mBuf[i])) << i;
      return (bits & liveLanes()) != 0;
    }
    for (size_t i = 0; i < mSize; i++)
      if (!std::isfinite(mPtr[i])) return true;
    return false;
  }

  /// Set all non-positive components to zero.
  void setNonPositiveToZero() noexcept {
    if (SMDL_LIKELY(isInline())) {
      // NOLINTNEXTLINE
      for (size_t i = 0; i < INLINE_CAPACITY; i++)
        mBuf[i] = std::max(mBuf[i], 0.0f);
    } else {
      for (size_t i = 0; i < mSize; i++) mPtr[i] = std::max(mPtr[i], 0.0f);
    }
  }

  /// Set all non-finite components to zero.
  void setNonFiniteToZero() noexcept {
    if (SMDL_LIKELY(isInline())) {
      // NOLINTNEXTLINE
      for (size_t i = 0; i < INLINE_CAPACITY; i++)
        mBuf[i] = maskedFloat(mBuf[i], std::isfinite(mBuf[i]));
    } else {
      for (size_t i = 0; i < mSize; i++)
        if (!std::isfinite(mPtr[i])) mPtr[i] = 0.0f;
    }
  }

  /// Calculate the average.
  [[nodiscard]] float average() const noexcept {
    SMDL_DEBUG_CHECK(mSize > 0);
    // Sequential, unlike the extrema below: the summation order is the
    // documented contract, and splitting the accumulator would reassociate
    // it. A band count this short cannot pay for that anyway.
    float result{};
    for (size_t i = 0; i < mSize; i++) result += mPtr[i];
    return result / float(mSize);
  }

  /// Find the maximum component.
  [[nodiscard]] float maxComponent() const noexcept {
    SMDL_DEBUG_CHECK(mSize > 0);
    float r0{mPtr[0]}, r1{r0}, r2{r0}, r3{r0};
    size_t i{1};
    for (; i + 3 < mSize; i += 4) {
      r0 = std::max(r0, mPtr[i + 0]);
      r1 = std::max(r1, mPtr[i + 1]);
      r2 = std::max(r2, mPtr[i + 2]);
      r3 = std::max(r3, mPtr[i + 3]);
    }
    for (; i < mSize; i++) r0 = std::max(r0, mPtr[i]);
    // NOLINTNEXTLINE
    return std::max(std::max(r0, r1), std::max(r2, r3));
  }

  /// Find the minimum component.
  [[nodiscard]] float minComponent() const noexcept {
    SMDL_DEBUG_CHECK(mSize > 0);
    float r0{mPtr[0]}, r1{r0}, r2{r0}, r3{r0};
    size_t i{1};
    for (; i + 3 < mSize; i += 4) {
      r0 = std::min(r0, mPtr[i + 0]);
      r1 = std::min(r1, mPtr[i + 1]);
      r2 = std::min(r2, mPtr[i + 2]);
      r3 = std::min(r3, mPtr[i + 3]);
    }
    for (; i < mSize; i++) r0 = std::min(r0, mPtr[i]);
    // NOLINTNEXTLINE
    return std::min(std::min(r0, r1), std::min(r2, r3));
  }

  [[nodiscard]] operator Span<float>() noexcept { return {mPtr, mSize}; }

  [[nodiscard]] operator Span<const float>() const noexcept {
    return {mPtr, mSize};
  }

private:
  /// Is the storage the inline buffer? True exactly when
  /// `mSize <= INLINE_CAPACITY`, which is what makes the fixed-length
  /// fast paths sound: an inline operand of an agreeing size is
  /// another inline buffer. Every branch on it is hinted: the optimizer's
  /// static heuristic reads a pointer equality as unlikely and laid the
  /// inline path, the one every operation takes at a band count within
  /// `INLINE_CAPACITY`, out of line at every site.
  [[nodiscard]] SMDL_ALWAYS_INLINE bool isInline() const noexcept {
    return mSize <= INLINE_CAPACITY;
  }

  /// One bit per inline lane within the size. The predicates test every
  /// inline lane into a bit each and mask with this, which is a handful
  /// of straight-line instructions where the size-bounded loop was
  /// sixteen trips with a back-edge each; a per-lane comparison against
  /// the size costs more than the loop did, because the optimizer widens
  /// it to 64-bit lanes.
  [[nodiscard]] SMDL_ALWAYS_INLINE uint32_t liveLanes() const noexcept {
    return (uint32_t(1) << mSize) - 1u;
  }

  /// `value` where `keep`, else zero, by an integer mask.
  [[nodiscard]] SMDL_ALWAYS_INLINE static float
  maskedFloat(float value, bool keep) noexcept {
    return bitCast<float>(bitCast<uint32_t>(value) & (0u - uint32_t(keep)));
  }

  /// Point `mData` at storage for `size` bands, uninitialized beyond
  /// the guarantee that inline lanes stay initialized. A size equal to
  /// the current one implies the storage is already right, because the
  /// heap is in use exactly when the size exceeds `INLINE_CAPACITY`.
  void reallocate(size_t size) {
    if (size == mSize) return;
    if (SMDL_UNLIKELY(!isInline())) delete[] mPtr;
    mPtr = SMDL_LIKELY(size <= INLINE_CAPACITY) ? mBuf : new float[size];
    mSize = size;
  }

  /// Move-construct the representation: steal a heap allocation, copy
  /// the whole inline buffer. The moved-from vector is left empty.
  void stealOrCopy(SpectralColor &other) noexcept {
    mSize = other.mSize;
    if (SMDL_UNLIKELY(!other.isInline())) {
      mPtr = other.mPtr;
    } else {
      mPtr = mBuf;
      std::memcpy(mBuf, other.mBuf, sizeof(mBuf));
    }
    other.mPtr = other.mBuf;
    other.mSize = 0;
  }

  /// The number of bands.
  size_t mSize{};

  /// The band values: `mLocal` when `mSize <= INLINE_CAPACITY`, a heap
  /// allocation otherwise.
  float *mPtr{mBuf};

  /// The inline storage. Zero-initialized in full so the fixed-length
  /// fast paths only ever read initialized lanes.
  alignas(32) float mBuf[INLINE_CAPACITY]{};
};

/// \}

} // namespace smdl
