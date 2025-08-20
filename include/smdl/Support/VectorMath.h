/// \file
#pragma once

#include <algorithm>
#include <cmath>
#include <limits>

namespace smdl {

/// \addtogroup Support
/// \{

/// The constant `PI`.
constexpr float PI = 3.141592653589793f;

/// \name Functions (math)
/// \{

[[nodiscard]] inline float finite_or_zero(float x) noexcept {
  return std::isfinite(x) ? x : 0.0f;
}

[[nodiscard]] inline float increment_float(float x) noexcept {
  return std::nextafter(x, +std::numeric_limits<float>::infinity());
}

[[nodiscard]] inline float decrement_float(float x) noexcept {
  return std::nextafter(x, -std::numeric_limits<float>::infinity());
}

/// \}

/// The vector template.
template <typename T, size_t M> class Vector;

/// The vector template for `N = 2`.
template <typename T> class alignas(2 * sizeof(T)) Vector<T, 2> {
public:
  constexpr Vector() = default;

  constexpr Vector(T x) : Vector(x, x) {}

  constexpr Vector(T x, T y) : x(x), y(y) {}

  template <typename U>
  explicit constexpr Vector(const U *v) : Vector(v[0], v[1]) {}

  /// The access operator.
  [[nodiscard]] constexpr T &operator[](size_t i) noexcept { return (&x)[i]; }

  /// The access operator, const variant.
  [[nodiscard]] constexpr const T &operator[](size_t i) const noexcept {
    return (&x)[i];
  }

  template <typename OtherT, size_t OtherN>
  [[nodiscard]] constexpr operator Vector<OtherT, OtherN>() const noexcept {
    Vector<OtherT, OtherN> result{};
    for (size_t i = 0; i < std::min<size_t>(2, OtherN); i++)
      result[i] = operator[](i);
    return result;
  }

  T x{}, y{};
};

/// The vector template for `N = 3`.
template <typename T> class alignas(4 * sizeof(T)) Vector<T, 3> {
public:
  constexpr Vector() = default;

  constexpr Vector(T x) : Vector(x, x, x) {}

  constexpr Vector(T x, T y, T z) : x(x), y(y), z(z) {}

  template <typename U>
  explicit constexpr Vector(const U *v) : Vector(v[0], v[1], v[2]) {}

  /// The access operator.
  [[nodiscard]] constexpr T &operator[](size_t i) noexcept { return (&x)[i]; }

  /// The access operator, const variant.
  [[nodiscard]] constexpr const T &operator[](size_t i) const noexcept {
    return (&x)[i];
  }

  template <typename OtherT, size_t OtherN>
  [[nodiscard]] constexpr operator Vector<OtherT, OtherN>() const noexcept {
    Vector<OtherT, OtherN> result{};
    for (size_t i = 0; i < std::min<size_t>(3, OtherN); i++)
      result[i] = operator[](i);
    return result;
  }

  T x{}, y{}, z{};
};

/// The vector template for `N = 4`.
template <typename T> class alignas(4 * sizeof(T)) Vector<T, 4> {
public:
  constexpr Vector() = default;

  constexpr Vector(T x) : Vector(x, x, x, x) {}

  constexpr Vector(T x, T y, T z, T w) : x(x), y(y), z(z), w(w) {}

  constexpr Vector(Vector<T, 3> v, T w) : Vector(v.x, v.y, v.z, w) {}

  template <typename U>
  explicit constexpr Vector(const U *v) : Vector(v[0], v[1], v[2], v[3]) {}

  /// The access operator.
  [[nodiscard]] constexpr T &operator[](size_t i) noexcept { return (&x)[i]; }

  /// The access operator, const variant.
  [[nodiscard]] constexpr const T &operator[](size_t i) const noexcept {
    return (&x)[i];
  }

  template <typename OtherT, size_t OtherN>
  [[nodiscard]] constexpr operator Vector<OtherT, OtherN>() const noexcept {
    Vector<OtherT, OtherN> result{};
    for (size_t i = 0; i < std::min<size_t>(4, OtherN); i++)
      result[i] = operator[](i);
    return result;
  }

  T x{}, y{}, z{}, w{};
};

inline namespace vector_type_aliases {

/// The equivalent of the MDL `int2` vector type.
using int2 = Vector<int, 2>;

/// The equivalent of the MDL `int3` vector type.
using int3 = Vector<int, 3>;

/// The equivalent of the MDL `int4` vector type.
using int4 = Vector<int, 4>;

/// The equivalent of the MDL `float2` vector type.
using float2 = Vector<float, 2>;

/// The equivalent of the MDL `float3` vector type.
using float3 = Vector<float, 3>;

/// The equivalent of the MDL `float4` vector type.
using float4 = Vector<float, 4>;

/// The equivalent of the MDL `double2` vector type.
using double2 = Vector<double, 2>;

/// The equivalent of the MDL `double3` vector type.
using double3 = Vector<double, 3>;

/// The equivalent of the MDL `double4` vector type.
using double4 = Vector<double, 4>;

static_assert(sizeof(float2) == 2 * sizeof(float));
static_assert(sizeof(float3) == 4 * sizeof(float));
static_assert(sizeof(float4) == 4 * sizeof(float));

} // namespace vector_type_aliases

/// \name Functions (math)
/// \{

/// Is any element true?
template <size_t N>
[[nodiscard]] inline bool is_any_true(Vector<bool, N> v) noexcept {
  for (size_t i = 0; i < N; i++)
    if (v[i])
      return true;
  return false;
}

/// Is every element true?
template <size_t N>
[[nodiscard]] inline bool is_all_true(Vector<bool, N> v) noexcept {
  for (size_t i = 0; i < N; i++)
    if (!v[i])
      return false;
  return true;
}

/// Is every element finite?
template <typename T, size_t N>
[[nodiscard]] inline bool is_all_finite(const Vector<T, N> &v) noexcept {
  for (size_t i = 0; i < N; i++)
    if (!std::isfinite(v[i]))
      return false;
  return true;
}

/// Vector unary `operator+`.
template <typename T, size_t N>
[[nodiscard]] constexpr Vector<T, N> operator+(Vector<T, N> v) noexcept {
  return v;
}

/// Vector unary `operator-`.
template <typename T, size_t N>
[[nodiscard]] constexpr Vector<T, N> operator-(Vector<T, N> v) noexcept {
  for (size_t i = 0; i < N; i++)
    v[i] = -v[i];
  return v;
}

/// Vector-Vector `operator+`.
template <typename T, size_t N>
[[nodiscard]] constexpr Vector<T, N>
operator+(const Vector<T, N> &v0, const Vector<T, N> &v1) noexcept {
  Vector<T, N> v{};
  for (size_t i = 0; i < N; i++)
    v[i] = v0[i] + v1[i];
  return v;
}

/// Vector-Vector `operator-`.
template <typename T, size_t N>
[[nodiscard]] constexpr Vector<T, N>
operator-(const Vector<T, N> &v0, const Vector<T, N> &v1) noexcept {
  Vector<T, N> v{};
  for (size_t i = 0; i < N; i++)
    v[i] = v0[i] - v1[i];
  return v;
}

/// Scalar-Vector `operator*`.
template <typename T, size_t N>
[[nodiscard]] constexpr Vector<T, N>
operator*(const T &s0, const Vector<T, N> &v1) noexcept {
  Vector<T, N> v{};
  for (size_t i = 0; i < N; i++)
    v[i] = s0 * v1[i];
  return v;
}

/// Vector-Scalar `operator*`.
template <typename T, size_t N>
[[nodiscard]] constexpr Vector<T, N> operator*(const Vector<T, N> &v0,
                                               const T &s1) noexcept {
  Vector<T, N> v{};
  for (size_t i = 0; i < N; i++)
    v[i] = v0[i] * s1;
  return v;
}

/// Vector-scalar `operator/`.
template <typename T, size_t N>
[[nodiscard]] constexpr Vector<T, N> operator/(const Vector<T, N> &v0,
                                               const T &s1) noexcept {
  Vector<T, N> v{};
  for (size_t i = 0; i < N; i++)
    v[i] = v0[i] / s1;
  return v;
}

/// Vector-Vector `operator==`.
template <typename T, size_t N>
[[nodiscard]] constexpr Vector<bool, N>
operator==(const Vector<T, N> &v0, const Vector<T, N> &v1) noexcept {
  Vector<bool, N> v{};
  for (size_t i = 0; i < N; i++)
    v[i] = v0[i] == v1[i];
  return v;
}

/// Vector-Vector `operator==`.
template <typename T, size_t N>
[[nodiscard]] constexpr Vector<bool, N>
operator!=(const Vector<T, N> &v0, const Vector<T, N> &v1) noexcept {
  Vector<bool, N> v{};
  for (size_t i = 0; i < N; i++)
    v[i] = v0[i] != v1[i];
  return v;
}

/// Vector dot product in 2 dimensions.
template <typename T>
[[nodiscard]] constexpr T dot(Vector<T, 2> u, Vector<T, 2> v) noexcept {
  return u.x * v.x + u.y * v.y;
}

/// Vector dot product in 3 dimensions.
template <typename T>
[[nodiscard]] constexpr T dot(Vector<T, 3> u, Vector<T, 3> v) noexcept {
  return u.x * v.x + u.y * v.y + u.z * v.z;
}

/// Vector dot product in 4 dimensions.
template <typename T>
[[nodiscard]] constexpr T dot(Vector<T, 4> u, Vector<T, 4> v) noexcept {
  return (u.x * v.x + u.y * v.y) + (u.z * v.z + u.w * v.w);
}

/// Absolute value of dot product.
template <typename T, size_t N>
[[nodiscard]] constexpr T abs_dot(Vector<T, N> u, Vector<T, N> v) noexcept {
  return std::abs(dot(u, v));
}

/// Vector length squared.
template <typename T, size_t N>
[[nodiscard]] constexpr T length_squared(Vector<T, N> v) noexcept {
  return dot(v, v);
}

/// Vector length.
template <typename T, size_t N>
[[nodiscard]] inline T length(Vector<T, N> v) noexcept {
  static_assert(std::is_floating_point_v<T>);
  return std::sqrt(dot(v, v));
}

/// Normalize.
template <typename T, size_t N>
[[nodiscard]] inline Vector<T, N> normalize(Vector<T, N> v) noexcept {
  static_assert(std::is_floating_point_v<T>);
  auto len{length(v)};
  auto invLen{len > 0 ? 1 / len : 0};
  return v * invLen;
}

/// Vector cross product in 3 dimensions.
template <typename T>
[[nodiscard]] constexpr Vector<T, 3> cross(Vector<T, 3> u,
                                           Vector<T, 3> v) noexcept {
  return {u.y * v.z - u.z * v.y, //
          u.z * v.x - u.x * v.z, //
          u.x * v.y - u.y * v.x};
}

/// \}

/// The matrix template.
///
/// \tparam T  The value type.
/// \tparam N  The number of columns.
/// \tparam M  The number of rows.
///
template <typename T, size_t N, size_t M> class Matrix final {
public:
  /// The zero constructor.
  constexpr Matrix() = default;

  /// The diagonal element constructor.
  constexpr Matrix(T x) {
    for (size_t i{}; i < std::min(N, M); i++)
      v[i][i] = x;
  }

  /// The column constructor.
  constexpr Matrix(const std::array<Vector<T, M>, N> &v) : v(v) {}

  template <typename... Args>
  constexpr Matrix(const Vector<T, M> &v0, const Args &...vs) : v{v0, vs...} {
    static_assert(1 + sizeof...(Args) == N);
  }

  /// The column access operator.
  [[nodiscard]] constexpr auto operator[](size_t j) noexcept -> Vector<T, M> & {
    return v[j];
  }

  /// The column access operator, const variant.
  [[nodiscard]] constexpr auto operator[](size_t j) const noexcept
      -> const Vector<T, M> & {
    return v[j];
  }

  // Get column vector.
  [[nodiscard]] constexpr Vector<T, M> col(size_t j) const noexcept {
    return v[j];
  }

  /// Get row vector.
  [[nodiscard]] constexpr Vector<T, N> row(size_t i) const noexcept {
    Vector<T, N> u{};
    for (size_t j = 0; j < N; j++)
      u[j] = v[j][i];
    return u;
  }

  template <typename OtherT, size_t OtherN, size_t OtherM>
  [[nodiscard]] constexpr
  operator Matrix<OtherT, OtherN, OtherM>() const noexcept {
    Matrix<OtherT, OtherN, OtherM> matrix{};
    for (size_t j = 0; j < std::min(N, OtherN); ++j)
      for (size_t i = 0; i < std::min(M, OtherM); ++i)
        matrix.v[j][i] = v[j][i];
    return matrix;
  }

  /// The column vectors.
  std::array<Vector<T, M>, N> v{};
};

inline namespace matrix_type_aliases {

/// The equivalent of the MDL `float2x2` matrix type.
using float2x2 = Matrix<float, 2, 2>;

/// The equivalent of the MDL `float3x2` matrix type.
using float3x2 = Matrix<float, 3, 2>;

/// The equivalent of the MDL `float4x2` matrix type.
using float4x2 = Matrix<float, 4, 2>;

/// The equivalent of the MDL `float2x3` matrix type.
using float2x3 = Matrix<float, 2, 3>;

/// The equivalent of the MDL `float3x3` matrix type.
using float3x3 = Matrix<float, 3, 3>;

/// The equivalent of the MDL `float4x3` matrix type.
using float4x3 = Matrix<float, 4, 3>;

/// The equivalent of the MDL `float2x4` matrix type.
using float2x4 = Matrix<float, 2, 4>;

/// The equivalent of the MDL `float3x4` matrix type.
using float3x4 = Matrix<float, 3, 4>;

/// The equivalent of the MDL `float4x4` matrix type.
using float4x4 = Matrix<float, 4, 4>;

/// The equivalent of the MDL `double2x2` matrix type.
using double2x2 = Matrix<double, 2, 2>;

/// The equivalent of the MDL `double3x2` matrix type.
using double3x2 = Matrix<double, 3, 2>;

/// The equivalent of the MDL `double4x2` matrix type.
using double4x2 = Matrix<double, 4, 2>;

/// The equivalent of the MDL `double2x3` matrix type.
using double2x3 = Matrix<double, 2, 3>;

/// The equivalent of the MDL `double3x3` matrix type.
using double3x3 = Matrix<double, 3, 3>;

/// The equivalent of the MDL `double4x3` matrix type.
using double4x3 = Matrix<double, 4, 3>;

/// The equivalent of the MDL `double2x4` matrix type.
using double2x4 = Matrix<double, 2, 4>;

/// The equivalent of the MDL `double3x4` matrix type.
using double3x4 = Matrix<double, 3, 4>;

/// The equivalent of the MDL `double4x4` matrix type.
using double4x4 = Matrix<double, 4, 4>;

} // namespace matrix_type_aliases

/// \name Functions (math)
/// \{

/// Matrix-Matrix `operator*`.
template <typename T, size_t P, size_t N, size_t M>
[[nodiscard]] constexpr Matrix<T, P, M>
operator*(const Matrix<T, N, M> &m0, const Matrix<T, P, N> &m1) noexcept {
  Matrix<T, P, M> m{};
  for (size_t i = 0; i < M; i++)
    for (size_t j = 0; j < P; j++)
      for (size_t k = 0; k < N; k++)
        m[j][i] += m0[k][i] * m1[j][k];
  return m;
}

/// Matrix-Vector `operator*`.
template <typename T, size_t N, size_t M>
[[nodiscard]] constexpr Vector<T, M>
operator*(const Matrix<T, N, M> &m0, const Vector<T, N> &v1) noexcept {
  Vector<T, M> v{};
  for (size_t i = 0; i < M; i++)
    for (size_t k = 0; k < N; k++)
      v[i] += m0[k][i] * v1[k];
  return v;
}

/// Matrix transpose.
template <typename T, size_t N, size_t M>
[[nodiscard]]
constexpr Matrix<T, M, N> transpose(const Matrix<T, N, M> &m) noexcept {
  auto mT{Matrix<T, M, N>{}};
  for (size_t i = 0; i < N; i++)
    for (size_t j = 0; j < M; j++)
      mT[j][i] = m[i][j];
  return mT;
}

/// Calculate affine inverse.
template <typename T>
[[nodiscard]]
constexpr Matrix<T, 4, 4> affine_inverse(const Matrix<T, 4, 4> &m) noexcept {
  auto mI{Matrix<T, 4, 4>{}};
  mI[0] = {m[0].x, m[1].x, m[2].x, T(0)};
  mI[1] = {m[0].y, m[1].y, m[2].y, T(0)};
  mI[2] = {m[0].z, m[1].z, m[2].z, T(0)};
  mI[3] = {-dot(m[0], m[3]), -dot(m[1], m[3]), -dot(m[2], m[3]), T(1)};
  return mI;
}

/// Calculate vector perpendicular to the given vector.
[[nodiscard]] inline float3 perpendicular_to(float3 z) noexcept {
  z = normalize(z);
  auto x{z.z < -0.9999f
             ? float3(0.0f, -1.0f, 0.0f)
             : float3(-z.x / (z.z + 1.0f) + 1.0f, -z.y / (z.z + 1.0f), -1.0f)};
  return normalize(x - dot(x, z) * z);
}

/// Calculate orthonormal coordinate system with the given vector as the Z axis.
[[nodiscard]] inline float3x3 coordinate_system(float3 z) noexcept {
  z = normalize(z);
  auto x{perpendicular_to(z)};
  auto y{normalize(cross(z, x))};
  return float3x3(x, y, z);
}

/// Calculate look-at transform.
[[nodiscard]] inline float4x4 look_at(float3 from, float3 to,
                                      float3 up = {0, 0, 1}) noexcept {
  float3 z{normalize(from - to)};
  float3 x{normalize(cross(up, z))};
  float3 y{cross(z, x)};
  return {float4{x.x, x.y, x.z, 0.0f}, //
          float4{y.x, y.y, y.z, 0.0f}, //
          float4{z.x, z.y, z.z, 0.0f}, //
          float4{from.x, from.y, from.z, 1.0f}};
}

/// \}

/// \}

} // namespace smdl
