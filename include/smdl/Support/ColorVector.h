/// \file
#pragma once

#include <array>
#include <cmath>

#include "smdl/Export.h"

#if __clang__
#define SMDL_USE_EXT_VECTOR_TYPES 1
#endif // #if __clang__

namespace smdl {

/// \addtogroup Support
/// \{

/// A color vector to parallelize math operations.
///
/// \tparam[N]
/// The number of bands/channels.
///
template <size_t N> class ColorVector final {
public:
#if SMDL_USE_EXT_VECTOR_TYPES
  using vector_type = float __attribute__((ext_vector_type(N)));
#else
  using vector_type = std::array<float, N>;
#endif // #if SMDL_USE_EXT_VECTOR_TYPES

  constexpr ColorVector() = default;

  constexpr ColorVector(vector_type v) : v(v) {}

#if SMDL_USE_EXT_VECTOR_TYPES
  constexpr ColorVector(float s) : v(s) {}
#else
  constexpr ColorVector(float s) {
    for (size_t i = 0; i < N; i++)
      v[i] = s;
  }
#endif

public:
  [[nodiscard]] constexpr size_t size() const noexcept { return N; }

  [[nodiscard]] auto data() noexcept -> float * {
#if SMDL_USE_EXT_VECTOR_TYPES
    return reinterpret_cast<float *>(&v);
#else
    return v.data();
#endif // #if SMDL_USE_EXT_VECTOR_TYPES
  }

  [[nodiscard]] auto data() const noexcept -> const float * {
    return const_cast<ColorVector &>(*this).data();
  }

  [[nodiscard]] auto operator[](size_t i) noexcept -> float & {
    return data()[i];
  }

  [[nodiscard]] auto operator[](size_t i) const noexcept -> const float & {
    return data()[i];
  }

public:
#if SMDL_USE_EXT_VECTOR_TYPES
  [[nodiscard]] constexpr ColorVector operator+() const noexcept { return v; }

  [[nodiscard]] constexpr ColorVector operator-() const noexcept { return -v; }

  [[nodiscard]]
  constexpr ColorVector operator+(const ColorVector &rhs) const noexcept {
    return v + rhs.v;
  }

  [[nodiscard]]
  constexpr ColorVector operator-(const ColorVector &rhs) const noexcept {
    return v - rhs.v;
  }

  [[nodiscard]]
  constexpr ColorVector operator*(const ColorVector &rhs) const noexcept {
    return v * rhs.v;
  }

  [[nodiscard]]
  constexpr ColorVector operator/(const ColorVector &rhs) const noexcept {
    return v / rhs.v;
  }

  [[nodiscard]] constexpr ColorVector operator+(float rhs) const noexcept {
    return v + rhs;
  }

  [[nodiscard]] constexpr ColorVector operator-(float rhs) const noexcept {
    return v - rhs;
  }

  [[nodiscard]] constexpr ColorVector operator*(float rhs) const noexcept {
    return v * rhs;
  }

  [[nodiscard]] constexpr ColorVector operator/(float rhs) const noexcept {
    return v / rhs;
  }

  [[nodiscard]]
  friend constexpr ColorVector operator+(float lhs,
                                         const ColorVector &rhs) noexcept {
    return lhs + rhs.v;
  }

  [[nodiscard]]
  friend constexpr ColorVector operator-(float lhs,
                                         const ColorVector &rhs) noexcept {
    return lhs - rhs.v;
  }

  [[nodiscard]]
  friend constexpr ColorVector operator*(float lhs,
                                         const ColorVector &rhs) noexcept {
    return lhs * rhs.v;
  }

  [[nodiscard]]
  friend constexpr ColorVector operator/(float lhs,
                                         const ColorVector &rhs) noexcept {
    return lhs / rhs.v;
  }
#else
  [[nodiscard]] constexpr ColorVector operator+() const noexcept { return v; }

  [[nodiscard]] constexpr ColorVector operator-() const noexcept {
    ColorVector result{};
    for (size_t i = 0; i < N; i++)
      result.v[i] = -v[i];
    return result;
  }

  [[nodiscard]]
  constexpr ColorVector operator+(const ColorVector &rhs) const noexcept {
    ColorVector result{};
    for (size_t i = 0; i < N; i++)
      result.v[i] = v[i] + rhs.v[i];
    return result;
  }

  [[nodiscard]]
  constexpr ColorVector operator-(const ColorVector &rhs) const noexcept {
    ColorVector result{};
    for (size_t i = 0; i < N; i++)
      result.v[i] = v[i] - rhs.v[i];
    return result;
  }

  [[nodiscard]]
  constexpr ColorVector operator*(const ColorVector &rhs) const noexcept {
    ColorVector result{};
    for (size_t i = 0; i < N; i++)
      result.v[i] = v[i] * rhs.v[i];
    return result;
  }

  [[nodiscard]]
  constexpr ColorVector operator/(const ColorVector &rhs) const noexcept {
    ColorVector result{};
    for (size_t i = 0; i < N; i++)
      result.v[i] = v[i] / rhs.v[i];
    return result;
  }

  [[nodiscard]] constexpr ColorVector operator+(float rhs) const noexcept {
    ColorVector result{};
    for (size_t i = 0; i < N; i++)
      result.v[i] = v[i] + rhs;
    return result;
  }

  [[nodiscard]] constexpr ColorVector operator-(float rhs) const noexcept {
    ColorVector result{};
    for (size_t i = 0; i < N; i++)
      result.v[i] = v[i] - rhs;
    return result;
  }

  [[nodiscard]] constexpr ColorVector operator*(float rhs) const noexcept {
    ColorVector result{};
    for (size_t i = 0; i < N; i++)
      result.v[i] = v[i] * rhs;
    return result;
  }

  [[nodiscard]] constexpr ColorVector operator/(float rhs) const noexcept {
    ColorVector result{};
    for (size_t i = 0; i < N; i++)
      result.v[i] = v[i] / rhs;
    return result;
  }

  [[nodiscard]]
  friend constexpr ColorVector operator+(float lhs,
                                         const ColorVector &rhs) noexcept {
    ColorVector result{};
    for (size_t i = 0; i < N; i++)
      result.v[i] = lhs + rhs.v[i];
    return result;
  }

  [[nodiscard]]
  friend constexpr ColorVector operator-(float lhs,
                                         const ColorVector &rhs) noexcept {
    ColorVector result{};
    for (size_t i = 0; i < N; i++)
      result.v[i] = lhs - rhs.v[i];
    return result;
  }

  [[nodiscard]]
  friend constexpr ColorVector operator*(float lhs,
                                         const ColorVector &rhs) noexcept {
    ColorVector result{};
    for (size_t i = 0; i < N; i++)
      result.v[i] = lhs * rhs.v[i];
    return result;
  }

  [[nodiscard]]
  friend constexpr ColorVector operator/(float lhs,
                                         const ColorVector &rhs) noexcept {
    ColorVector result{};
    for (size_t i = 0; i < N; i++)
      result.v[i] = lhs / rhs.v[i];
    return result;
  }
#endif // #if SMDL_USE_EXT_VECTOR_TYPES

  template <typename Other>
  constexpr ColorVector &operator+=(const Other &rhs) noexcept {
    return *this = *this + rhs;
  }

  template <typename Other>
  constexpr ColorVector &operator-=(const Other &rhs) noexcept {
    return *this = *this - rhs;
  }

  template <typename Other>
  constexpr ColorVector &operator*=(const Other &rhs) noexcept {
    return *this = *this * rhs;
  }

  template <typename Other>
  constexpr ColorVector &operator/=(const Other &rhs) noexcept {
    return *this = *this / rhs;
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
  [[nodiscard]] bool is_all_zero(float thresh = 0.0f) const noexcept {
    for (size_t i = 0; i < N; i++)
      if (!(std::abs(v[i]) <= thresh))
        return false;
    return true;
  }

  /// Is any component infinite?
  [[nodiscard]] bool is_any_inf() const noexcept {
    for (size_t i = 0; i < N; i++)
      if (std::isinf(v[i]))
        return true;
    return false;
  }

  /// Is any component not-a-number?
  [[nodiscard]] bool is_any_nan() const noexcept {
    for (size_t i = 0; i < N; i++)
      if (std::isnan(v[i]))
        return true;
    return false;
  }

  /// Is any component either infinite or not-a-number?
  [[nodiscard]] bool is_any_non_finite() const noexcept {
    for (size_t i = 0; i < N; i++)
      if (!std::isfinite(v[i]))
        return true;
    return false;
  }

  /// Set all non-positive components to zero.
  void set_non_positive_to_zero() noexcept {
    for (size_t i = 0; i < N; i++) {
      v[i] = std::fmax(v[i], 0.0f);
    }
  }

  /// Set all non-finite components to zero.
  void set_non_finite_to_zero() noexcept {
    for (size_t i = 0; i < N; i++) {
      if (!std::isfinite(v[i])) {
        v[i] = 0.0f;
      }
    }
  }

  /// Calculate the average.
  [[nodiscard]] float average() const noexcept {
    float result{};
    for (size_t i = 0; i < N; i++)
      result += v[i];
    return result / N;
  }

  /// Find the maximum component.
  [[nodiscard]] float maximum_component() const noexcept {
    float result{v[0]};
    for (size_t i = 1; i < N; i++)
      result = std::fmax(result, v[i]);
    return result;
  }

  /// Find the minimum component.
  [[nodiscard]] float minimum_component() const noexcept {
    float result{v[0]};
    for (size_t i = 1; i < N; i++)
      result = std::fmin(result, v[i]);
    return result;
  }

public:
  vector_type v{};
};

/// \}

} // namespace smdl
