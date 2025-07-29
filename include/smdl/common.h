/// \file
#pragma once

#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <functional>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "smdl/Export.h"
#include "smdl/Support/BumpPtrAllocator.h"
#include "smdl/Support/Error.h"
#include "smdl/Support/Filesystem.h"
#include "smdl/Support/Span.h"
#include "smdl/Support/StringHelpers.h"

namespace llvm {

class Constant;
class ConstantInt;
class DataLayout;
class LLVMContext;
class Module;
class TargetMachine;
class Type;
class Value;

namespace orc {

class ThreadSafeModule;
class LLJIT;

} // namespace orc

} // namespace llvm

/// The top-level SMDL namespace.
namespace smdl {

/// \defgroup Support Support
/// \{
/// \}

/// \defgroup Main Main
/// \{

/// Helper to implement `SMDL_CAT` correctly (Yes this is necessary!)
#define SMDL_CAT__HELPER(X, Y) X##Y

/// Concatenate macros.
#define SMDL_CAT(X, Y) SMDL_CAT__HELPER(X, Y)

/// Defer until end of scope.
#define SMDL_DEFER(...)                                                        \
  const auto SMDL_CAT(__defer, __LINE__) = ::smdl::Defer(__VA_ARGS__)

/// Preserve values, restoring at end of scope.
#define SMDL_PRESERVE(...)                                                     \
  const auto SMDL_CAT(__preserve, __LINE__) = ::smdl::Preserve(__VA_ARGS__)

/// Expand the correct sanity check macro.
#define SMDL_SANITY_CHECK__EXPAND(A, B, C, ...) C

/// Sanity check a condition.
#define SMDL_SANITY_CHECK__1(cond)                                             \
  do {                                                                         \
    if (!(cond))                                                               \
      ::smdl::sanity_check_failed(#cond, __FILE__, __LINE__);                  \
  } while (false)

/// Sanity check a condition with a message.
#define SMDL_SANITY_CHECK__2(cond, message)                                    \
  do {                                                                         \
    if (!(cond))                                                               \
      ::smdl::sanity_check_failed(#cond, __FILE__, __LINE__, message);         \
  } while (false)

/// Sanity check a condition with or without a message.
#define SMDL_SANITY_CHECK(...)                                                 \
  SMDL_SANITY_CHECK__EXPAND(__VA_ARGS__, SMDL_SANITY_CHECK__2,                 \
                            SMDL_SANITY_CHECK__1)(__VA_ARGS__)

/// Defer until end-of-scope.
template <typename F> class Defer final {
public:
  constexpr Defer(F f) : f(std::move(f)) {}

  ~Defer() { std::invoke(f); }

  F f;
};

/// Preserve values, restoring at end-of-scope.
template <typename... Ts> class Preserve final {
public:
  constexpr Preserve(Ts &...values)
      : values(values...), backupValues(values...) {}

  Preserve(const Preserve &) = delete;

  Preserve(Preserve &&) = delete;

  ~Preserve() { restore(); }

  template <size_t... I>
  constexpr void restore(std::integer_sequence<size_t, I...>) {
    ((std::get<I>(values) = std::get<I>(backupValues)), ...);
  }

  constexpr void restore() {
    restore(std::make_index_sequence<sizeof...(Ts)>());
  }

private:
  std::tuple<Ts &...> values;

  std::tuple<Ts...> backupValues;
};

/// If `SMDL_SANITY_CHECK` fails, this prints the relevant information and
/// exits the program with code `EXIT_FAILURE`.
[[noreturn]] SMDL_EXPORT void sanity_check_failed(const char *condition,
                                                  const char *file, int line,
                                                  const char *more = nullptr);

/// The SMDL build information.
class SMDL_EXPORT BuildInfo final {
public:
  /// Get.
  [[nodiscard]] static BuildInfo get() noexcept;

public:
  /// The major version number.
  uint32_t major{};

  /// The minor version number.
  uint32_t minor{};

  /// The patch version number.
  uint32_t patch{};

  /// The git branch name.
  const char *gitBranch{};

  /// The git commit hash.
  const char *gitCommit{};
};

/// The LLVM native target.
class SMDL_EXPORT NativeTarget final {
public:
  /// Get.
  [[nodiscard]] static const NativeTarget &get() noexcept;

public:
  /// The CPU name.
  std::string_view name{};

  /// The CPU triple.
  std::string_view triple{};

  /// The LLVM target machine representation.
  llvm::TargetMachine *machine{};
};

/// \}

/// \addtogroup Main
/// \{

class Compiler;
class Module;
class Type;

/// A source location somewhere in an MDL module.
class SMDL_EXPORT SourceLocation final {
public:
  /// Get the module name.
  [[nodiscard]] std::string_view get_module_name() const;

  /// Get the file name.
  [[nodiscard]] std::string_view get_module_file_name() const;

  /// Log a warning.
  void log_warn(std::string_view message) const;

  /// Log an error.
  void log_error(std::string_view message) const;

  /// Throw an `Error`.
  void throw_error(std::string message) const;

  /// Throw an `Error` using `concat` to concatenate the arguments.
  template <typename T0, typename T1, typename... Ts>
  void throw_error(T0 &&value0, T1 &&value1, Ts &&...values) const {
    throw_error(concat(std::forward<T0>(value0), std::forward<T1>(value1),
                       std::forward<Ts>(values)...));
  }

  /// Is not-valid?
  [[nodiscard]] bool operator!() const { return !module_; }

  /// Is valid?
  [[nodiscard]] operator bool() const { return module_; }

  /// Convert to string.
  [[nodiscard]] operator std::string() const;

public:
  /// The associated MDL module, which contains the filename and source code.
  Module *module_{};

  /// The line number.
  uint32_t lineNo{1};

  /// The character number in the line.
  uint32_t charNo{1};

  /// The raw index in the source code string.
  uint64_t i{};
};

/// \}

/// \addtogroup Main
/// \{

/// The constant `PI`.
constexpr float PI = 3.141592653589793f;

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

/// \}

/// \addtogroup Support
/// \{

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
  return u.x * v.x + u.y * v.y + u.z * v.z + u.w * v.w;
}

/// Vector cross product in 3 dimensions.
template <typename T>
[[nodiscard]] constexpr Vector<T, 3> cross(Vector<T, 3> u,
                                           Vector<T, 3> v) noexcept {
  return {u.y * v.z - u.z * v.y, //
          u.z * v.x - u.x * v.z, //
          u.x * v.y - u.y * v.x};
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
  return v / length(v);
}

/// Normalize, maybe returning length.
template <typename T, size_t N>
[[nodiscard]] inline Vector<T, N> normalize(Vector<T, N> v, T *vLen) noexcept {
  static_assert(std::is_floating_point_v<T>);
  auto len{length(v)};
  if (vLen)
    *vLen = len;
  return v / len;
}

/// Absolute value of dot product.
template <typename T, size_t N>
[[nodiscard]] constexpr T abs_dot(Vector<T, N> u, Vector<T, N> v) noexcept {
  return std::abs(dot(u, v));
}

/// Matrix transpose.
template <typename T, size_t N, size_t M>
[[nodiscard]] constexpr Matrix<T, M, N>
transpose(const Matrix<T, N, M> &m) noexcept {
  auto mT{Matrix<T, M, N>{}};
  for (size_t i = 0; i < N; i++)
    for (size_t j = 0; j < M; j++)
      mT[j][i] = m[i][j];
  return mT;
}

/// Calculate affine inverse.
template <typename T>
[[nodiscard]] constexpr Matrix<T, 4, 4>
affine_inverse(const Matrix<T, 4, 4> &m) noexcept {
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

[[nodiscard]] inline float finite_or_zero(float x) noexcept {
  return std::isfinite(x) ? x : 0.0f;
}

/// \}

/// \}

/// \addtogroup Main
/// \{

/// The format options.
class SMDL_EXPORT FormatOptions final {
public:
  /// Format files in-place. If false, prints formatted source code to `stdout`.
  bool inPlace{};

  /// Remove comments from formatted source code.
  bool noComments{};

  /// Remove annotations from formatted source code.
  bool noAnnotations{};

  /// Want compact?
  bool compact{};
};

/// The MDL state passed in at runtime.
class SMDL_EXPORT State final {
public:
  /// The allocator, which must point to thread-local
  /// instance of `BumpPtrAllocator`.
  void *allocator{};

  /// The position or ray intersection point in object space.
  float3 position{};

  /// The normal in object space.
  float3 normal{0, 0, 1};

  /// The geometry normal in object space.
  float3 geometry_normal{0, 0, 1};

  /// The motion vector in object space.
  float3 motion{};

  /// The max supported number of texture spaces.
  static constexpr size_t TEXTURE_SPACE_MAX = 4;

  /// The number of texture spaces.
  int texture_space_max{1};

  /// The texture coordinates.
  float3 texture_coordinate[TEXTURE_SPACE_MAX]{};

  // The texture tangent U vector(s) in object space.
  float3 texture_tangent_u[TEXTURE_SPACE_MAX] = {
      float3{1, 0, 0}, float3{1, 0, 0}, float3{1, 0, 0}, float3{1, 0, 0}};

  // The texture tangent V vector(s) in object space.
  float3 texture_tangent_v[TEXTURE_SPACE_MAX] = {
      float3{0, 1, 0}, float3{0, 1, 0}, float3{0, 1, 0}, float3{0, 1, 0}};

  // The geometry tangent U vector(s) in object space.
  float3 geometry_tangent_u[TEXTURE_SPACE_MAX] = {
      float3{1, 0, 0}, float3{1, 0, 0}, float3{1, 0, 0}, float3{1, 0, 0}};

  // The geometry tangent V vector(s) in object space.
  float3 geometry_tangent_v[TEXTURE_SPACE_MAX] = {
      float3{0, 1, 0}, float3{0, 1, 0}, float3{0, 1, 0}, float3{0, 1, 0}};

  /// The object ID.
  int object_id{};

  /// The Ptex face ID if applicable.
  int ptex_face_id{};

  /// The Ptex face UV if applicable.
  float2 ptex_face_uv{};

  float3 direction{};

  /// The animation time.
  float animation_time{0.0f};

  /// The wavelengths in nanometers.
  const float *wavelength_base{};

  /// The wavelength range minimum in nanometers.
  float wavelength_min{380.0f};

  /// The wavelength range maximum in nanometers.
  float wavelength_max{720.0f};

  /// The meters per scene unit.
  float meters_per_scene_unit{1.0f};

  /// The object-to-world matrix.
  float4x4 object_to_world_matrix{float4x4(1.0f)};

  /// The tangent-to-object matrix.
  ///
  /// The tangent space is the coordinate system where
  /// - The X axis is aligned to the geometry tangent in U.
  /// - The Y axis is aligned to the geometry tangent in V.
  /// - The Z axis is aligned to the geometry normal.
  /// - The origin is the ray intersection point.
  ///
  /// Do not populate this! Instead call `finalize_for_runtime_conventions()`
  /// to compute this from `geometry_tangent_u[0]`, `geometry_tangent_v[0]`,
  /// `geometry_normal`, and `position`.
  ///
  float4x4 tangent_to_object_matrix{float4x4(1.0f)};

public:
  void finalize_for_runtime_conventions();
};

/// An albedo look-up table (LUT) for energy compensation in lossy BSDFs.
class SMDL_EXPORT AlbedoLUT final {
public:
  /// The number of samples of the cosine of the viewing angle.
  const int num_cos_theta = 0;

  /// The number of samples of the roughness parameter.
  const int num_roughness = 0;

  /// The directional albedo.
  ///
  /// \note
  /// This must point to `num_cos_theta` rows by `num_roughness` values.
  ///
  const float *const directional_albedo = nullptr;

  /// The average albedo.
  ///
  /// \note
  /// This must point to `num_roughness` values.
  ///
  const float *const average_albedo = nullptr;
};

/// \}

/// \addtogroup Support
/// \{

SMDL_EXPORT void parallel_for(size_t num,
                              const std::function<void(size_t)> &func);

/// \}

} // namespace smdl
