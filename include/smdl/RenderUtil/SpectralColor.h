/// \file
#pragma once

#include <algorithm>
#include <cmath>
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
/// may compute to NaN (for example a division's `0/0`) but they are
/// never read by anything size-bounded, which is everything the class
/// exposes.
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
    if (isInline()) {
      for (size_t i = 0; i < INLINE_CAPACITY; i++) mLocal[i] = value;
    } else {
      for (size_t i = 0; i < mSize; i++) mData[i] = value;
    }
  }

  /// Construct with `values.size()` bands copied from `values`.
  explicit SpectralColor(Span<const float> values) {
    reallocate(values.size());
    if (mSize > 0) std::memcpy(mData, values.data(), mSize * sizeof(float));
  }

  SpectralColor(const SpectralColor &other) {
    if (other.isInline()) {
      std::memcpy(mLocal, other.mLocal, sizeof(mLocal));
      mSize = other.mSize;
    } else {
      reallocate(other.mSize);
      std::memcpy(mData, other.mData, mSize * sizeof(float));
    }
  }

  SpectralColor(SpectralColor &&other) noexcept { stealOrCopy(other); }

  SpectralColor &operator=(const SpectralColor &other) {
    if (this != &other) {
      if (other.isInline() && isInline()) {
        std::memcpy(mLocal, other.mLocal, sizeof(mLocal));
        mSize = other.mSize;
      } else {
        reallocate(other.mSize);
        if (mSize > 0) std::memcpy(mData, other.mData, mSize * sizeof(float));
      }
    }
    return *this;
  }

  SpectralColor &operator=(SpectralColor &&other) noexcept {
    if (this != &other) {
      if (!isInline()) delete[] mData;
      stealOrCopy(other);
    }
    return *this;
  }

  ~SpectralColor() {
    if (!isInline()) delete[] mData;
    mData = mLocal;
    mSize = 0;
  }

public:
  [[nodiscard]] size_t size() const noexcept { return mSize; }

  [[nodiscard]] float *data() noexcept { return mData; }

  [[nodiscard]] const float *data() const noexcept { return mData; }

  [[nodiscard]] float &operator[](size_t i) noexcept { return mData[i]; }

  [[nodiscard]] const float &operator[](size_t i) const noexcept {
    return mData[i];
  }

public:
  SpectralColor &operator+=(const SpectralColor &rhs) noexcept {
    SMDL_SANITY_CHECK(mSize == rhs.mSize);
    if (isInline()) {
      for (size_t i = 0; i < INLINE_CAPACITY; i++) mLocal[i] += rhs.mLocal[i];
    } else {
      for (size_t i = 0; i < mSize; i++) mData[i] += rhs.mData[i];
    }
    return *this;
  }

  SpectralColor &operator-=(const SpectralColor &rhs) noexcept {
    SMDL_SANITY_CHECK(mSize == rhs.mSize);
    if (isInline()) {
      for (size_t i = 0; i < INLINE_CAPACITY; i++) mLocal[i] -= rhs.mLocal[i];
    } else {
      for (size_t i = 0; i < mSize; i++) mData[i] -= rhs.mData[i];
    }
    return *this;
  }

  SpectralColor &operator*=(const SpectralColor &rhs) noexcept {
    SMDL_SANITY_CHECK(mSize == rhs.mSize);
    if (isInline()) {
      for (size_t i = 0; i < INLINE_CAPACITY; i++) mLocal[i] *= rhs.mLocal[i];
    } else {
      for (size_t i = 0; i < mSize; i++) mData[i] *= rhs.mData[i];
    }
    return *this;
  }

  SpectralColor &operator/=(const SpectralColor &rhs) noexcept {
    SMDL_SANITY_CHECK(mSize == rhs.mSize);
    if (isInline()) {
      for (size_t i = 0; i < INLINE_CAPACITY; i++) mLocal[i] /= rhs.mLocal[i];
    } else {
      for (size_t i = 0; i < mSize; i++) mData[i] /= rhs.mData[i];
    }
    return *this;
  }

  SpectralColor &operator+=(float rhs) noexcept {
    if (isInline()) {
      for (size_t i = 0; i < INLINE_CAPACITY; i++) mLocal[i] += rhs;
    } else {
      for (size_t i = 0; i < mSize; i++) mData[i] += rhs;
    }
    return *this;
  }

  SpectralColor &operator-=(float rhs) noexcept {
    if (isInline()) {
      for (size_t i = 0; i < INLINE_CAPACITY; i++) mLocal[i] -= rhs;
    } else {
      for (size_t i = 0; i < mSize; i++) mData[i] -= rhs;
    }
    return *this;
  }

  SpectralColor &operator*=(float rhs) noexcept {
    if (isInline()) {
      for (size_t i = 0; i < INLINE_CAPACITY; i++) mLocal[i] *= rhs;
    } else {
      for (size_t i = 0; i < mSize; i++) mData[i] *= rhs;
    }
    return *this;
  }

  SpectralColor &operator/=(float rhs) noexcept {
    if (isInline()) {
      for (size_t i = 0; i < INLINE_CAPACITY; i++) mLocal[i] /= rhs;
    } else {
      for (size_t i = 0; i < mSize; i++) mData[i] /= rhs;
    }
    return *this;
  }

  [[nodiscard]] SpectralColor operator+() const { return *this; }

  [[nodiscard]] SpectralColor operator-() const {
    SpectralColor result{*this};
    if (result.isInline()) {
      for (size_t i = 0; i < INLINE_CAPACITY; i++)
        result.mLocal[i] = -result.mLocal[i];
    } else {
      for (size_t i = 0; i < result.mSize; i++)
        result.mData[i] = -result.mData[i];
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
    for (size_t i = 0; i < mSize; i++)
      if (!(std::abs(mData[i]) <= thresh)) return false;
    return true;
  }

  /// Is any component infinite?
  [[nodiscard]] bool isAnyInf() const noexcept {
    for (size_t i = 0; i < mSize; i++)
      if (std::isinf(mData[i])) return true;
    return false;
  }

  /// Is any component not-a-number?
  [[nodiscard]] bool isAnyNan() const noexcept {
    for (size_t i = 0; i < mSize; i++)
      if (std::isnan(mData[i])) return true;
    return false;
  }

  /// Is any component either infinite or not-a-number?
  [[nodiscard]] bool isAnyNonFinite() const noexcept {
    for (size_t i = 0; i < mSize; i++)
      if (!std::isfinite(mData[i])) return true;
    return false;
  }

  /// Set all non-positive components to zero.
  void setNonPositiveToZero() noexcept {
    for (size_t i = 0; i < mSize; i++) {
      mData[i] = std::max(mData[i], 0.0f);
    }
  }

  /// Set all non-finite components to zero.
  void setNonFiniteToZero() noexcept {
    for (size_t i = 0; i < mSize; i++) {
      if (!std::isfinite(mData[i])) {
        mData[i] = 0.0f;
      }
    }
  }

  /// Calculate the average.
  [[nodiscard]] float average() const noexcept {
    SMDL_SANITY_CHECK(mSize > 0);
    float result{};
    for (size_t i = 0; i < mSize; i++) result += mData[i];
    return result / float(mSize);
  }

  /// Find the maximum component.
  [[nodiscard]] float maxComponent() const noexcept {
    SMDL_SANITY_CHECK(mSize > 0);
    float result{mData[0]};
    for (size_t i = 1; i < mSize; i++) result = std::max(result, mData[i]);
    return result;
  }

  /// Find the minimum component.
  [[nodiscard]] float minComponent() const noexcept {
    SMDL_SANITY_CHECK(mSize > 0);
    float result{mData[0]};
    for (size_t i = 1; i < mSize; i++) result = std::min(result, mData[i]);
    return result;
  }

  [[nodiscard]] operator Span<float>() noexcept {
    return Span<float>(mData, mSize);
  }

  [[nodiscard]] operator Span<const float>() const noexcept {
    return Span<const float>(mData, mSize);
  }

private:
  /// Is the storage the inline buffer? True exactly when
  /// `mSize <= INLINE_CAPACITY`, which is what makes the fixed-length
  /// fast paths sound: an inline operand of an agreeing size is
  /// another inline buffer.
  [[nodiscard]] bool isInline() const noexcept { return mData == mLocal; }

  /// Point `mData` at storage for `size` bands, uninitialized beyond
  /// the guarantee that inline lanes stay initialized. A size equal to
  /// the current one implies the storage is already right, because the
  /// heap is in use exactly when the size exceeds `INLINE_CAPACITY`.
  void reallocate(size_t size) {
    if (size == mSize) return;
    if (!isInline()) delete[] mData;
    mData = size <= INLINE_CAPACITY ? mLocal : new float[size];
    mSize = size;
  }

  /// Move-construct the representation: steal a heap allocation, copy
  /// the whole inline buffer. The moved-from vector is left empty.
  void stealOrCopy(SpectralColor &other) noexcept {
    mSize = other.mSize;
    if (!other.isInline()) {
      mData = other.mData;
    } else {
      mData = mLocal;
      std::memcpy(mLocal, other.mLocal, sizeof(mLocal));
    }
    other.mData = other.mLocal;
    other.mSize = 0;
  }

  /// The number of bands.
  size_t mSize{};

  /// The band values: `mLocal` when `mSize <= INLINE_CAPACITY`, a heap
  /// allocation otherwise.
  float *mData{mLocal};

  /// The inline storage. Zero-initialized in full so the fixed-length
  /// fast paths only ever read initialized lanes.
  alignas(32) float mLocal[INLINE_CAPACITY]{};
};

/// \}

} // namespace smdl
