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
/// sound because every constructor fills the inline buffer in full and
/// it is only ever written by these same fixed-length operations, so
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
/// The object holds no pointer into itself: the storage is the heap
/// pointer where there is one and the inline buffer otherwise, chosen
/// at every access, so a temporary is a plain value whose address
/// never escapes at construction, which is what lets the
/// optimizer keep an expression's intermediates in registers instead
/// of spilling each one through its own buffer. Every path that
/// touches the heap is out of line, so the inline body of an operation
/// is its fixed-length loop and one predicted branch; a caller that
/// inlines a dozen operations does not inline a dozen allocators. The
/// operators and predicates are forced inline besides: the inliner
/// prices a fixed-length loop as the sixteen scalar lanes it sees
/// before vectorization, above its threshold, and left them out of
/// line at a hundred call sites in the renderer, each a call around
/// what vectorizes to a handful of instructions.
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
  SpectralColor() noexcept {
    for (size_t i = 0; i < INLINE_CAPACITY; i++) mBuf[i] = 0.0f;
  }

  /// Construct with `size` bands of `value`.
  explicit SpectralColor(size_t size, float value = 0.0f) {
    // Whether or not the buffer ends up being the storage, so that the
    // common case writes it exactly once.
    // NOLINTNEXTLINE
    for (size_t i = 0; i < INLINE_CAPACITY; i++) mBuf[i] = value;
    mSize = uint32_t(size);
    if (SMDL_UNLIKELY(!isInline())) allocateHeapFilled(value);
  }

  /// Construct with `values.size()` bands copied from `values`.
  explicit SpectralColor(Span<const float> values) {
    for (size_t i = 0; i < INLINE_CAPACITY; i++) mBuf[i] = 0.0f;
    mSize = uint32_t(values.size());
    if (SMDL_UNLIKELY(!isInline())) allocateHeap();
    if (mSize > 0) std::memcpy(data(), values.data(), mSize * sizeof(float));
  }

  SpectralColor(const SpectralColor &other) {
    std::memcpy(mBuf, other.mBuf, sizeof(mBuf));
    mSize = other.mSize;
    if (SMDL_UNLIKELY(!isInline())) allocateHeapCopy(other.mHeap);
  }

  SpectralColor(SpectralColor &&other) noexcept { steal(other); }

  SpectralColor &operator=(const SpectralColor &other) {
    if (this != &other) {
      std::memcpy(mBuf, other.mBuf, sizeof(mBuf));
      if (SMDL_LIKELY(other.isInline() && isInline())) {
        mSize = other.mSize;
      } else {
        assignSlow(other);
      }
    }
    return *this;
  }

  SpectralColor &operator=(SpectralColor &&other) noexcept {
    if (this != &other) {
      if (SMDL_UNLIKELY(!isInline())) freeHeap();
      steal(other);
    }
    return *this;
  }

  ~SpectralColor() {
    if (SMDL_UNLIKELY(!isInline())) freeHeap();
  }

public:
  [[nodiscard]] size_t size() const noexcept { return mSize; }

  [[nodiscard]] float *data() noexcept { return mHeap ? mHeap : mBuf; }

  [[nodiscard]] const float *data() const noexcept {
    return mHeap ? mHeap : mBuf;
  }

  [[nodiscard]] float &operator[](size_t i) noexcept { return data()[i]; }

  [[nodiscard]] const float &operator[](size_t i) const noexcept {
    return data()[i];
  }

public:
  SMDL_ALWAYS_INLINE SpectralColor &
  operator+=(const SpectralColor &rhs) noexcept {
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
      applyHeap(rhs, [](float a, float b) { return a + b; });
    }
    return *this;
  }

  SMDL_ALWAYS_INLINE SpectralColor &
  operator-=(const SpectralColor &rhs) noexcept {
    SMDL_DEBUG_CHECK(mSize == rhs.mSize);
    if (SMDL_LIKELY(isInline())) {
      // NOLINTNEXTLINE
      alignas(32) float tmp[INLINE_CAPACITY];
      for (size_t i = 0; i < INLINE_CAPACITY; i++)
        tmp[i] = mBuf[i] - rhs.mBuf[i];
      for (size_t i = 0; i < INLINE_CAPACITY; i++) mBuf[i] = tmp[i];
    } else {
      applyHeap(rhs, [](float a, float b) { return a - b; });
    }
    return *this;
  }

  SMDL_ALWAYS_INLINE SpectralColor &
  operator*=(const SpectralColor &rhs) noexcept {
    SMDL_DEBUG_CHECK(mSize == rhs.mSize);
    if (SMDL_LIKELY(isInline())) {
      // NOLINTNEXTLINE
      alignas(32) float tmp[INLINE_CAPACITY];
      for (size_t i = 0; i < INLINE_CAPACITY; i++)
        tmp[i] = mBuf[i] * rhs.mBuf[i];
      for (size_t i = 0; i < INLINE_CAPACITY; i++) mBuf[i] = tmp[i];
    } else {
      applyHeap(rhs, [](float a, float b) { return a * b; });
    }
    return *this;
  }

  SMDL_ALWAYS_INLINE SpectralColor &
  operator/=(const SpectralColor &rhs) noexcept {
    SMDL_DEBUG_CHECK(mSize == rhs.mSize);
    if (SMDL_LIKELY(isInline())) {
      // NOLINTNEXTLINE
      alignas(32) float tmp[INLINE_CAPACITY];
      for (size_t i = 0; i < INLINE_CAPACITY; i++)
        tmp[i] = mBuf[i] / rhs.mBuf[i];
      for (size_t i = 0; i < INLINE_CAPACITY; i++) mBuf[i] = tmp[i];
    } else {
      applyHeap(rhs, [](float a, float b) { return a / b; });
    }
    return *this;
  }

  SMDL_ALWAYS_INLINE SpectralColor &operator+=(float rhs) noexcept {
    if (SMDL_LIKELY(isInline())) {
      // NOLINTNEXTLINE
      for (size_t i = 0; i < INLINE_CAPACITY; i++) mBuf[i] += rhs;
    } else {
      applyHeap(rhs, [](float a, float b) { return a + b; });
    }
    return *this;
  }

  SMDL_ALWAYS_INLINE SpectralColor &operator-=(float rhs) noexcept {
    if (SMDL_LIKELY(isInline())) {
      // NOLINTNEXTLINE
      for (size_t i = 0; i < INLINE_CAPACITY; i++) mBuf[i] -= rhs;
    } else {
      applyHeap(rhs, [](float a, float b) { return a - b; });
    }
    return *this;
  }

  SMDL_ALWAYS_INLINE SpectralColor &operator*=(float rhs) noexcept {
    if (SMDL_LIKELY(isInline())) {
      // NOLINTNEXTLINE
      for (size_t i = 0; i < INLINE_CAPACITY; i++) mBuf[i] *= rhs;
    } else {
      applyHeap(rhs, [](float a, float b) { return a * b; });
    }
    return *this;
  }

  SMDL_ALWAYS_INLINE SpectralColor &operator/=(float rhs) noexcept {
    if (SMDL_LIKELY(isInline())) {
      // NOLINTNEXTLINE
      for (size_t i = 0; i < INLINE_CAPACITY; i++) mBuf[i] /= rhs;
    } else {
      applyHeap(rhs, [](float a, float b) { return a / b; });
    }
    return *this;
  }

  [[nodiscard]] SMDL_ALWAYS_INLINE SpectralColor operator+() const {
    return *this;
  }

  [[nodiscard]] SMDL_ALWAYS_INLINE SpectralColor operator-() const {
    SpectralColor result{*this};
    if (SMDL_LIKELY(result.isInline())) {
      // NOLINTNEXTLINE
      for (size_t i = 0; i < INLINE_CAPACITY; i++)
        result.mBuf[i] = -result.mBuf[i];
    } else {
      result.mapHeap([](float a) { return -a; });
    }
    return result;
  }

  [[nodiscard]] SMDL_ALWAYS_INLINE SpectralColor
  operator+(const SpectralColor &rhs) const {
    SpectralColor result{*this};
    result += rhs;
    return result;
  }

  [[nodiscard]] SMDL_ALWAYS_INLINE SpectralColor
  operator-(const SpectralColor &rhs) const {
    SpectralColor result{*this};
    result -= rhs;
    return result;
  }

  [[nodiscard]] SMDL_ALWAYS_INLINE SpectralColor
  operator*(const SpectralColor &rhs) const {
    SpectralColor result{*this};
    result *= rhs;
    return result;
  }

  [[nodiscard]] SMDL_ALWAYS_INLINE SpectralColor
  operator/(const SpectralColor &rhs) const {
    SpectralColor result{*this};
    result /= rhs;
    return result;
  }

  [[nodiscard]] SMDL_ALWAYS_INLINE SpectralColor operator+(float rhs) const {
    SpectralColor result{*this};
    result += rhs;
    return result;
  }

  [[nodiscard]] SMDL_ALWAYS_INLINE SpectralColor operator-(float rhs) const {
    SpectralColor result{*this};
    result -= rhs;
    return result;
  }

  [[nodiscard]] SMDL_ALWAYS_INLINE SpectralColor operator*(float rhs) const {
    SpectralColor result{*this};
    result *= rhs;
    return result;
  }

  [[nodiscard]] SMDL_ALWAYS_INLINE SpectralColor operator/(float rhs) const {
    SpectralColor result{*this};
    result /= rhs;
    return result;
  }

  [[nodiscard]] friend SMDL_ALWAYS_INLINE SpectralColor
  operator+(float lhs, const SpectralColor &rhs) {
    return rhs + lhs;
  }

  [[nodiscard]] friend SMDL_ALWAYS_INLINE SpectralColor
  operator-(float lhs, const SpectralColor &rhs) {
    SpectralColor result{rhs.mSize, lhs};
    result -= rhs;
    return result;
  }

  [[nodiscard]] friend SMDL_ALWAYS_INLINE SpectralColor
  operator*(float lhs, const SpectralColor &rhs) {
    return rhs * lhs;
  }

  [[nodiscard]] friend SMDL_ALWAYS_INLINE SpectralColor
  operator/(float lhs, const SpectralColor &rhs) {
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
  [[nodiscard]] SMDL_ALWAYS_INLINE bool
  isAllZero(float thresh = 0.0f) const noexcept {
    if (SMDL_LIKELY(isInline())) {
      uint32_t bits{};
      for (size_t i = 0; i < INLINE_CAPACITY; i++)
        bits |= uint32_t(!(std::abs(mBuf[i]) <= thresh)) << i;
      return (bits & liveLanes()) == 0;
    }
    for (size_t i = 0; i < mSize; i++)
      if (!(std::abs(mHeap[i]) <= thresh)) return false;
    return true;
  }

  /// Is any component infinite?
  [[nodiscard]] SMDL_ALWAYS_INLINE bool isAnyInf() const noexcept {
    if (SMDL_LIKELY(isInline())) {
      uint32_t bits{};
      for (size_t i = 0; i < INLINE_CAPACITY; i++)
        bits |= uint32_t(std::isinf(mBuf[i])) << i;
      return (bits & liveLanes()) != 0;
    }
    for (size_t i = 0; i < mSize; i++)
      if (std::isinf(mHeap[i])) return true;
    return false;
  }

  /// Is any component not-a-number?
  [[nodiscard]] SMDL_ALWAYS_INLINE bool isAnyNan() const noexcept {
    if (SMDL_LIKELY(isInline())) {
      uint32_t bits{};
      for (size_t i = 0; i < INLINE_CAPACITY; i++)
        bits |= uint32_t(std::isnan(mBuf[i])) << i;
      return (bits & liveLanes()) != 0;
    }
    for (size_t i = 0; i < mSize; i++)
      if (std::isnan(mHeap[i])) return true;
    return false;
  }

  /// Is any component either infinite or not-a-number?
  [[nodiscard]] SMDL_ALWAYS_INLINE bool isAnyNonFinite() const noexcept {
    if (SMDL_LIKELY(isInline())) {
      uint32_t bits{};
      for (size_t i = 0; i < INLINE_CAPACITY; i++)
        bits |= uint32_t(!std::isfinite(mBuf[i])) << i;
      return (bits & liveLanes()) != 0;
    }
    for (size_t i = 0; i < mSize; i++)
      if (!std::isfinite(mHeap[i])) return true;
    return false;
  }

  /// Set all non-positive components to zero.
  void setNonPositiveToZero() noexcept {
    if (SMDL_LIKELY(isInline())) {
      // NOLINTNEXTLINE
      for (size_t i = 0; i < INLINE_CAPACITY; i++)
        mBuf[i] = std::max(mBuf[i], 0.0f);
    } else {
      mapHeap([](float a) { return std::max(a, 0.0f); });
    }
  }

  /// Set all non-finite components to zero.
  void setNonFiniteToZero() noexcept {
    if (SMDL_LIKELY(isInline())) {
      // NOLINTNEXTLINE
      for (size_t i = 0; i < INLINE_CAPACITY; i++)
        mBuf[i] = maskedFloat(mBuf[i], std::isfinite(mBuf[i]));
    } else {
      mapHeap([](float a) { return std::isfinite(a) ? a : 0.0f; });
    }
  }

  /// Calculate the average.
  [[nodiscard]] float average() const noexcept {
    SMDL_DEBUG_CHECK(mSize > 0);
    // Sequential, unlike the extrema below: the summation order is the
    // documented contract, and splitting the accumulator would reassociate
    // it. A band count this short cannot pay for that anyway.
    const float *SMDL_RESTRICT p{data()};
    float result{};
    for (size_t i = 0; i < mSize; i++) result += p[i];
    return result / float(mSize);
  }

  /// Find the maximum component.
  [[nodiscard]] float maxComponent() const noexcept {
    SMDL_DEBUG_CHECK(mSize > 0);
    const float *SMDL_RESTRICT p{data()};
    float r0{p[0]}, r1{r0}, r2{r0}, r3{r0};
    size_t i{1};
    for (; i + 3 < mSize; i += 4) {
      r0 = std::max(r0, p[i + 0]);
      r1 = std::max(r1, p[i + 1]);
      r2 = std::max(r2, p[i + 2]);
      r3 = std::max(r3, p[i + 3]);
    }
    for (; i < mSize; i++) r0 = std::max(r0, p[i]);
    // NOLINTNEXTLINE
    return std::max(std::max(r0, r1), std::max(r2, r3));
  }

  /// Find the minimum component.
  [[nodiscard]] float minComponent() const noexcept {
    SMDL_DEBUG_CHECK(mSize > 0);
    const float *SMDL_RESTRICT p{data()};
    float r0{p[0]}, r1{r0}, r2{r0}, r3{r0};
    size_t i{1};
    for (; i + 3 < mSize; i += 4) {
      r0 = std::min(r0, p[i + 0]);
      r1 = std::min(r1, p[i + 1]);
      r2 = std::min(r2, p[i + 2]);
      r3 = std::min(r3, p[i + 3]);
    }
    for (; i < mSize; i++) r0 = std::min(r0, p[i]);
    // NOLINTNEXTLINE
    return std::min(std::min(r0, r1), std::min(r2, r3));
  }

  [[nodiscard]] operator Span<float>() noexcept { return {data(), mSize}; }

  [[nodiscard]] operator Span<const float>() const noexcept {
    return {data(), mSize};
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

  /// Move-construct the representation: take the heap allocation if
  /// there is one, copy the whole inline buffer either way. The
  /// moved-from vector is left empty, so it never frees what it gave
  /// away.
  void steal(SpectralColor &other) noexcept {
    std::memcpy(mBuf, other.mBuf, sizeof(mBuf));
    mSize = other.mSize;
    mHeap = other.mHeap;
    other.mSize = 0;
    other.mHeap = nullptr;
  }

  /// Point the storage at `size` bands, uninitialized beyond the
  /// guarantee that inline lanes stay initialized. A size equal to the
  /// current one implies the storage is already right, because the heap
  /// is in use exactly when the size exceeds `INLINE_CAPACITY`.
  void resize(size_t size) {
    if (size == mSize) return;
    if (!isInline()) freeHeap();
    mSize = uint32_t(size);
    if (!isInline()) allocateHeap();
  }

  // The heap paths, out of line so that no caller inlines them.
  SMDL_NO_INLINE void allocateHeap() { mHeap = new float[mSize]; }

  SMDL_NO_INLINE void allocateHeapFilled(float value) {
    allocateHeap();
    for (size_t i = 0; i < mSize; i++) mHeap[i] = value;
  }

  SMDL_NO_INLINE void allocateHeapCopy(const float *values) {
    allocateHeap();
    std::memcpy(mHeap, values, mSize * sizeof(float));
  }

  SMDL_NO_INLINE void freeHeap() noexcept {
    delete[] mHeap;
    mHeap = nullptr;
  }

  /// Copy-assign when either side is on the heap; the caller has
  /// already copied the inline buffer.
  SMDL_NO_INLINE void assignSlow(const SpectralColor &other) {
    resize(other.mSize);
    if (!isInline()) std::memcpy(mHeap, other.mHeap, mSize * sizeof(float));
  }

  template <typename Op>
  SMDL_NO_INLINE void applyHeap(const SpectralColor &rhs, Op op) noexcept {
    for (size_t i = 0; i < mSize; i++) mHeap[i] = op(mHeap[i], rhs.mHeap[i]);
  }

  template <typename Op>
  SMDL_NO_INLINE void applyHeap(float rhs, Op op) noexcept {
    for (size_t i = 0; i < mSize; i++) mHeap[i] = op(mHeap[i], rhs);
  }

  template <typename Op> SMDL_NO_INLINE void mapHeap(Op op) noexcept {
    for (size_t i = 0; i < mSize; i++) mHeap[i] = op(mHeap[i]);
  }

  /// The number of bands. The storage follows from it alone: the
  /// inline buffer up to `INLINE_CAPACITY`, the heap allocation past
  /// it, so the object carries no pointer to itself.
  uint32_t mSize{};

  /// The heap allocation while `mSize` exceeds `INLINE_CAPACITY`, null
  /// otherwise. `data()` selects on the null rather than on the size:
  /// the loop vectorizer identifies the array bounds of a per-band loop
  /// through a null-tested pointer as it did through the old pointer
  /// member, and a select on the size defeats it, which turned every
  /// such loop scalar.
  float *mHeap{};

  /// The inline storage, which every constructor fills in full whether
  /// or not it is the storage in use, so that the fixed-length fast
  /// paths only ever read initialized lanes.
  ///
  /// Deliberately without an initializer here: every constructor fills
  /// it anyway, and a second fill is a dead store the optimizer has to
  /// prove dead, which it can only do once nothing has taken the
  /// buffer's address.
  alignas(32) float mBuf[INLINE_CAPACITY];
};

/// \}

} // namespace smdl
