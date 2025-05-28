#pragma once

#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "smdl/Export.h"

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

/// A span.
///
/// We target C++17, which does not have `std::span` yet. We also
/// do not want to include any LLVM headers from our headers, so
/// we also cannot use `llvm::ArrayRef`.
///
template <typename T> class Span final {
public:
  constexpr Span() = default;

  /// Construct from single element.
  constexpr Span(const T &elem) : first(&elem), count(1) {}

  /// Construct from pointer to first element and element count.
  constexpr Span(const T *first, size_t count) : first(first), count(count) {}

  /// Construct from `std::initializer_list`.
  constexpr Span(std::initializer_list<T> elems)
      : first(elems.begin()), count(elems.size()) {}

  /// Construct from `std::array`.
  template <size_t N>
  constexpr Span(const std::array<T, N> &elems)
      : first(elems.data()), count(elems.size()) {}

  /// Construct from `std::vector`.
  Span(const std::vector<T> &elems)
      : first(elems.data()), count(elems.size()) {}

  /// Is empty?
  [[nodiscard]] constexpr bool empty() const { return count == 0; }

  /// Get the size.
  [[nodiscard]] constexpr size_t size() const { return count; }

  /// Get the begin iterator.
  [[nodiscard]] constexpr auto begin() const { return first; }

  /// Get the end iterator.
  [[nodiscard]] constexpr auto end() const { return first + count; }

  /// Get the data pointer.
  [[nodiscard]] constexpr const T *data() const { return first; }

  /// Get the front element.
  [[nodiscard]] constexpr const T &front() const { return first[0]; }

  /// Get the back element.
  [[nodiscard]] constexpr const T &back() const { return first[count - 1]; }

  /// Drop the front element while the given predicate is true.
  template <typename Pred>
  [[nodiscard]] constexpr Span<T> drop_front_while(Pred &&pred) const {
    size_t i = 0;
    size_t n = count;
    while (i < count && pred(first[i])) {
      i++;
      n--;
    }
    return subspan(i, n);
  }

  /// Drop the front element.
  [[nodiscard]] constexpr Span<T> drop_front() const {
    return subspan(1, count - 1);
  }

  /// Drop the back element.
  [[nodiscard]] constexpr Span<T> drop_back() const {
    return subspan(0, count - 1);
  }

  /// Get subspan.
  [[nodiscard]] constexpr Span<T> subspan(size_t i,
                                          size_t n = size_t(-1)) const {
    return Span(first + i, std::min(count - i, n));
  }

  /// Contains the given value?
  [[nodiscard]] constexpr bool contains(const T &value) const {
    return std::find(begin(), end(), value) != end();
  }

  /// Starts with the given sequence of values?
  [[nodiscard]] constexpr bool starts_with(Span<T> other) const {
    if (count < other.count)
      return false;
    for (size_t i = 0; i < other.count; i++)
      if (operator[](i) != other[i])
        return false;
    return true;
  }

  /// Get element by index.
  [[nodiscard]] constexpr const T &operator[](size_t i) const {
    return first[i];
  }

  /// Implicit conversion to bool.
  [[nodiscard]] constexpr operator bool() const { return first && count > 0; }

  /// All equal?
  [[nodiscard]] constexpr bool operator==(const Span &other) const {
    if (count != other.count)
      return false;
    for (size_t i = 0; i < count; i++)
      if (first[i] != other.first[i])
        return false;
    return true;
  }

  /// Any not-equal?
  [[nodiscard]] constexpr bool operator!=(const Span &other) const {
    return !operator==(other);
  }

  /// The pointer to the first element.
  const T *first{};

  /// The element count.
  size_t count{};
};

/// \name Functions (strings)
/// \{

/// Is ASCII alphabetic character?
[[nodiscard]] constexpr bool is_alpha(char ch) {
  return (static_cast<int>('A' <= ch) & static_cast<int>(ch <= 'Z')) |
         (static_cast<int>('a' <= ch) & static_cast<int>(ch <= 'z'));
}

/// Is ASCII digit?
[[nodiscard]] constexpr bool is_digit(char ch) {
  return (static_cast<int>('0' <= ch) & static_cast<int>(ch <= '9'));
}

/// Is ASCII binary digit?
[[nodiscard]] constexpr bool is_digit_2(char ch) {
  return (static_cast<int>('0' <= ch) & static_cast<int>(ch <= '1'));
}

/// Is ASCII octal digit?
[[nodiscard]] constexpr bool is_digit_8(char ch) {
  return (static_cast<int>('0' <= ch) & static_cast<int>(ch <= '7'));
}

/// Is ASCII hexadecimal digit?
[[nodiscard]] constexpr bool is_digit_16(char ch) {
  return is_digit(ch) |
         (static_cast<int>('A' <= ch) & static_cast<int>(ch <= 'F')) |
         (static_cast<int>('a' <= ch) & static_cast<int>(ch <= 'f'));
}

/// Is ASCII alphabetic character, digit, or underscore?
[[nodiscard]] constexpr bool is_word(char ch) {
  return static_cast<int>(is_alpha(ch)) | static_cast<int>(is_digit(ch)) |
         static_cast<int>(ch == '_');
}

/// Is ASCII whitespace character?
[[nodiscard]] constexpr bool is_space(char ch) {
  return static_cast<int>(ch == ' ') | static_cast<int>(ch == '\t') |
         static_cast<int>(ch == '\n') | static_cast<int>(ch == '\r') |
         static_cast<int>(ch == '\v');
}

/// Convert ASCII octal character to runtime int.
[[nodiscard]] constexpr int oct_to_int(int ch) {
  if ('0' <= ch && ch <= '7')
    return ch - '0';
  return 0;
}

/// Convert ASCII hexadecimal character to runtime int.
[[nodiscard]] constexpr int hex_to_int(int ch) {
  if ('0' <= ch && ch <= '9')
    return ch - '0';
  if ('a' <= ch && ch <= 'f')
    return ch - 'a' + 10;
  if ('A' <= ch && ch <= 'F')
    return ch - 'A' + 10;
  return 0;
}

/// Determine if `str0` starts with `str1`.
[[nodiscard]] constexpr bool starts_with(std::string_view str0,
                                         std::string_view str1) {
  return str0.size() >= str1.size() && str0.substr(0, str1.size()) == str1;
}

/// \}

/// A quoted string for use with `concat`.
struct quoted final {
  constexpr quoted(std::string_view str) : str(str) {}

  std::string_view str{};
};

namespace detail {

template <typename T, typename... Ts>
inline void do_concat(std::string &str, T &&value, Ts &&...values) {
  using DecayT = std::decay_t<T>;
  if constexpr (std::is_arithmetic_v<DecayT>) {
    str += std::to_string(value);
  } else if constexpr (std::is_same_v<DecayT, quoted>) {
    str += '\'';
    str += value.str;
    str += '\'';
  } else {
    str += value;
  }
  if constexpr (sizeof...(Ts) > 0)
    do_concat(str, std::forward<Ts>(values)...);
}

} // namespace detail

/// \name Functions (strings)
/// \{

/// Concatenate the given values into a string.
template <typename T, typename... Ts>
[[nodiscard]] inline auto concat(T &&value0, Ts &&...values) {
  if constexpr (sizeof...(Ts) == 0 &&
                std::is_same_v<std::decay_t<T>, std::string>) {
    return value0;
  } else if constexpr (sizeof...(Ts) == 0 &&
                       std::is_constructible_v<std::string_view,
                                               std::decay_t<T>>) {
    return std::string_view(value0);
  } else {
    std::string str{};
    str.reserve(128);
    detail::do_concat(str, std::forward<T>(value0),
                      std::forward<Ts>(values)...);
    return str;
  }
}

/// Join the given string views by the given delimiter.
[[nodiscard]] inline std::string join(Span<std::string_view> strs,
                                      std::string_view delim) {
  std::string str{};
  str.reserve(128);
  for (size_t i = 0; i < strs.size(); i++) {
    str += strs[i];
    if (i + 1 < strs.size())
      str += delim;
  }
  return str;
}

/// \}

/// \}

/// \defgroup Main Main
/// \{

/// Either initialize successfully or dump an error message and
/// exit with code `EXIT_FAILURE`.
///
/// This may be called more than once, subsequent calls do nothing. If
/// this is not explicitly called by the host application, it will be
/// called automatically the first time an instance of the `Compiler`
/// is constructed.
///
SMDL_EXPORT void init_or_exit();

/// The LLVM native target.
class SMDL_EXPORT NativeTarget final {
public:
  /// The CPU name.
  std::string_view name{};

  /// The CPU triple.
  std::string_view triple{};

  /// The LLVM target machine representation.
  llvm::TargetMachine *machine{};
};

/// Get the LLVM native target, only available after `init_or_exit()`
[[nodiscard]] SMDL_EXPORT const NativeTarget &get_native_target();

/// \}

/// \addtogroup Support
/// \{

/// A bump pointer allocated by `BumpPtrAllocator` that does not need to be
/// freed, but does need to be destructed.
///
/// This is effectively a `std::unique_ptr` with a deleter that only
/// invokes the destructor.
template <typename T> class BumpPtr final {
public:
  BumpPtr() = default;

  BumpPtr(std::nullptr_t) {}

  /// Construct from raw pointer.
  template <typename U> BumpPtr(U *ptr) : ptr(static_cast<T *>(ptr)) {
    static_assert(std::is_base_of_v<T, U>);
  }

  /// Copy constructor is disabled!
  BumpPtr(const BumpPtr &) = delete;

  /// Move constructor.
  BumpPtr(BumpPtr &&other) : ptr(std::exchange(other.ptr, nullptr)) {}

  /// Move constructor from derived type.
  template <typename U>
  BumpPtr(BumpPtr<U> &&other) : BumpPtr(std::exchange(other.ptr, nullptr)) {}

  /// Copy assignment is disabled!
  BumpPtr &operator=(const BumpPtr &) = delete;

  /// Move assignment.
  BumpPtr &operator=(BumpPtr &&other) {
    reset(std::exchange(other.ptr, nullptr));
    return *this;
  }

  /// Move assignment from derived type.
  template <typename U> BumpPtr &operator=(BumpPtr<U> &&other) {
    static_assert(std::is_base_of_v<T, U>);
    reset(std::exchange(other.ptr, nullptr));
    return *this;
  }

  ~BumpPtr() { reset(); }

  [[nodiscard]] auto *get() { return ptr; }

  [[nodiscard]] auto *get() const { return ptr; }

  [[nodiscard]] auto *operator->() { return ptr; }

  [[nodiscard]] auto *operator->() const { return ptr; }

  [[nodiscard]] auto &operator*() { return *ptr; }

  [[nodiscard]] auto &operator*() const { return *ptr; }

  [[nodiscard]] operator bool() const { return ptr != nullptr; }

  [[nodiscard]] bool operator!() const { return ptr == nullptr; }

  void reset() {
    if (ptr) {
      ptr->~T();
      ptr = nullptr;
    }
  }

  template <typename U> void reset(U *newPtr) {
    static_assert(std::is_base_of_v<T, U>);
    reset();
    ptr = static_cast<T *>(newPtr);
  }

public:
  T *ptr{};
};

/// \}

/// \addtogroup Main
/// \{

/// A bump pointer allocator, opaque wrapper around
/// `llvm::BumpPtrAllocator`.
class SMDL_EXPORT BumpPtrAllocator final {
public:
  BumpPtrAllocator();

  BumpPtrAllocator(const BumpPtrAllocator &) = delete;

  ~BumpPtrAllocator();

  /// Allocate raw memory.
  ///
  /// \param[in] size   The size in bytes.
  /// \param[in] align  The alignment in bytes.
  ///
  [[nodiscard]] void *allocate(size_t size, size_t align);

  /// Allocate and initialize type `T` by passing `Args...` to the constructor.
  template <typename T, typename... Args>
  [[nodiscard]] auto allocate(Args &&...args) {
    auto result{new (allocate(sizeof(T), alignof(T)))
                    T{std::forward<Args>(args)...}};
    if constexpr (std::is_trivially_destructible_v<T>)
      return result;
    else
      return BumpPtr<T>(result);
  }

  /// Reset the allocator, freeing all memory.
  void reset();

  /// Get the number of bytes allocated.
  [[nodiscard]] size_t bytes_allocated() const;

private:
  /// The pointer to the `llvm::BumpPtrAllocator`.
  void *ptr{};
};

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

  /// Throw an `Error` with this source location attached.
  void throw_error(std::string message) const;

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

/// The error representation.
class SMDL_EXPORT Error final : public std::exception {
public:
  explicit Error(std::string message) : message(std::move(message)) {}

  /// Print to standard error.
  void print() const;

  /// Print to standard error and exit with `EXIT_FAILURE`.
  void print_and_exit() const;

  const char *what() const noexcept final { return message.c_str(); }

public:
  /// The message.
  std::string message{};
};

/// \}

/// \addtogroup Support
/// \{

/// Run the given function, catch whatever it might throw, and return it as
/// an `Error` value.
template <typename Func>
[[nodiscard]] inline std::optional<Error>
catch_and_return_error(Func &&func) try {
  std::invoke(std::forward<Func>(func));
  return std::nullopt;
} catch (Error error) {
  return std::move(error);
} catch (const std::exception &error) {
  return Error(error.what());
} catch (...) {
  return Error("unknown error converted from unknown exception type");
}

/// \}

/// \addtogroup Main
/// \{

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
  [[nodiscard]] constexpr T &operator[](size_t i) { return (&x)[i]; }

  /// The access operator, const variant.
  [[nodiscard]] constexpr const T &operator[](size_t i) const {
    return (&x)[i];
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
  [[nodiscard]] constexpr T &operator[](size_t i) { return (&x)[i]; }

  /// The access operator, const variant.
  [[nodiscard]] constexpr const T &operator[](size_t i) const {
    return (&x)[i];
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
  [[nodiscard]] constexpr T &operator[](size_t i) { return (&x)[i]; }

  /// The access operator, const variant.
  [[nodiscard]] constexpr const T &operator[](size_t i) const {
    return (&x)[i];
  }

  [[nodiscard]] constexpr operator Vector<T, 3>() const { return {x, y, z}; }

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
  [[nodiscard]] constexpr auto &operator[](size_t i) { return v[i]; }

  /// The column access operator, const variant.
  [[nodiscard]] constexpr auto &operator[](size_t i) const { return v[i]; }

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

/// Vector-Vector `operator+`.
template <typename T, size_t N>
[[nodiscard]] constexpr Vector<T, N> operator+(const Vector<T, N> &v0,
                                               const Vector<T, N> &v1) {
  Vector<T, N> v{};
  for (size_t i = 0; i < N; i++)
    v[i] = v0[i] + v1[i];
  return v;
}

/// Vector-Vector `operator-`.
template <typename T, size_t N>
[[nodiscard]] constexpr Vector<T, N> operator-(const Vector<T, N> &v0,
                                               const Vector<T, N> &v1) {
  Vector<T, N> v{};
  for (size_t i = 0; i < N; i++)
    v[i] = v0[i] - v1[i];
  return v;
}

/// Scalar-Vector `operator*`.
template <typename T, size_t N>
[[nodiscard]] constexpr Vector<T, N> operator*(const T &s0,
                                               const Vector<T, N> &v1) {
  Vector<T, N> v{};
  for (size_t i = 0; i < N; i++)
    v[i] = s0 * v1[i];
  return v;
}

/// Vector-Scalar `operator*`.
template <typename T, size_t N>
[[nodiscard]] constexpr Vector<T, N> operator*(const Vector<T, N> &v0,
                                               const T &s1) {
  Vector<T, N> v{};
  for (size_t i = 0; i < N; i++)
    v[i] = v0[i] * s1;
  return v;
}

/// Vector-scalar `operator/`.
template <typename T, size_t N>
[[nodiscard]] constexpr Vector<T, N> operator/(const Vector<T, N> &v0,
                                               const T &s1) {
  Vector<T, N> v{};
  for (size_t i = 0; i < N; i++)
    v[i] = v0[i] / s1;
  return v;
}

/// Matrix-Matrix `operator*`.
template <typename T, size_t P, size_t N, size_t M>
[[nodiscard]] constexpr Matrix<T, P, M> operator*(const Matrix<T, N, M> &m0,
                                                  const Matrix<T, P, N> &m1) {
  Matrix<T, P, M> m{};
  for (size_t i = 0; i < M; i++)
    for (size_t j = 0; j < P; j++)
      for (size_t k = 0; k < N; k++)
        m[j][i] += m0[k][i] * m1[j][k];
  return m;
}

/// Matrix-Vector `operator*`.
template <typename T, size_t N, size_t M>
[[nodiscard]] constexpr Vector<T, M> operator*(const Matrix<T, N, M> &m0,
                                               const Vector<T, N> &v1) {
  Vector<T, M> v{};
  for (size_t i = 0; i < M; i++)
    for (size_t k = 0; k < N; k++)
      v[i] += m0[k][i] * v1[k];
  return v;
}

/// Vector dot product in 2 dimensions.
template <typename T>
[[nodiscard]] constexpr T dot(Vector<T, 2> u, Vector<T, 2> v) {
  return u.x * v.x + u.y * v.y;
}

/// Vector dot product in 3 dimensions.
template <typename T>
[[nodiscard]] constexpr T dot(Vector<T, 3> u, Vector<T, 3> v) {
  return u.x * v.x + u.y * v.y + u.z * v.z;
}

/// Vector dot product in 4 dimensions.
template <typename T>
[[nodiscard]] constexpr T dot(Vector<T, 4> u, Vector<T, 4> v) {
  return u.x * v.x + u.y * v.y + u.z * v.z + u.w * v.w;
}

/// Vector cross product in 3 dimensions.
template <typename T>
[[nodiscard]] constexpr Vector<T, 3> cross(Vector<T, 3> u, Vector<T, 3> v) {
  return {u.y * v.z - u.z * v.y, //
          u.z * v.x - u.x * v.z, //
          u.x * v.y - u.y * v.x};
}

/// Vector length.
template <typename T, size_t N> [[nodiscard]] inline T length(Vector<T, N> v) {
  static_assert(std::is_floating_point_v<T>);
  return std::sqrt(dot(v, v));
}

/// Normalize.
template <typename T, size_t N>
[[nodiscard]] inline Vector<T, N> normalize(Vector<T, N> v) {
  static_assert(std::is_floating_point_v<T>);
  return v / length(v);
}

/// Matrix transpose.
template <typename T, size_t N, size_t M>
[[nodiscard]] constexpr Matrix<T, M, N> transpose(const Matrix<T, N, M> &m) {
  auto mT{Matrix<T, M, N>{}};
  for (size_t i = 0; i < N; i++)
    for (size_t j = 0; j < M; j++)
      mT[j][i] = m[i][j];
  return mT;
}

/// Calculate affine inverse.
template <typename T>
[[nodiscard]] constexpr Matrix<T, 4, 4>
affine_inverse(const Matrix<T, 4, 4> &m) {
  auto mI{Matrix<T, 4, 4>{}};
  mI[0] = {m[0].x, m[1].x, m[2].x, T(0)};
  mI[1] = {m[0].y, m[1].y, m[2].y, T(0)};
  mI[2] = {m[0].z, m[1].z, m[2].z, T(0)};
  mI[3] = {-dot(m[0], m[3]), -dot(m[1], m[3]), -dot(m[2], m[3]), T(1)};
  return mI;
}

/// Calculate vector perpendicular to the given vector.
[[nodiscard]] inline float3 perpendicular_to(float3 v) {
  float3 z{normalize(v)};
  float3 x{z.z < -0.9999f ? float3(0.0f, -1.0f, 0.0f)
                          : float3(-z.x / (z.z + 1.0f) + 1.0f,
                                   -z.y / (z.z + 1.0f), -1.0f)};
  return normalize(x - dot(x, z) * z);
}

/// Calculate look-at transform.
[[nodiscard]] inline float4x4 look_at(float3 from, float3 to,
                                      float3 up = {0, 0, 1}) {
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

/// \addtogroup Main
/// \{

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

/// \}

/// \addtogroup Support
/// \{

/// Defer until end-of-scope.
template <typename F> struct Defer final {
public:
  constexpr Defer(F f) : f(std::move(f)) {}

  ~Defer() { std::invoke(f); }

  F f;
};

/// Preserve values, restoring at end-of-scope.
template <typename... Ts> struct Preserve final {
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

/// The formatter options.
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

/// \}

/// \addtogroup Main
/// \{

/// Expand third macro in variadic arguments, used to implement
/// `SMDL_SANITY_CHECK`.
#define SMDL_EXPAND_THIRD_MACRO(A, B, C, ...) C

/// Sanity check a condition.
#define SMDL_SANITY_CHECK_1(cond)                                              \
  do {                                                                         \
    if (!(cond))                                                               \
      ::smdl::sanity_check_failed(#cond, __FILE__, __LINE__);                  \
  } while (false)

/// Sanity check a condition with a message.
#define SMDL_SANITY_CHECK_2(cond, message)                                     \
  do {                                                                         \
    if (!(cond))                                                               \
      ::smdl::sanity_check_failed(#cond, __FILE__, __LINE__, message);         \
  } while (false)

/// Sanity check a condition with or without a message.
#define SMDL_SANITY_CHECK(...)                                                 \
  SMDL_EXPAND_THIRD_MACRO(__VA_ARGS__, SMDL_SANITY_CHECK_2,                    \
                          SMDL_SANITY_CHECK_1)(__VA_ARGS__)

/// If `SMDL_SANITY_CHECK` fails, this prints the relevant information and
/// exits the program with code `EXIT_FAILURE`.
[[noreturn]] SMDL_EXPORT void sanity_check_failed(const char *condition,
                                                  const char *file, int line,
                                                  const char *more = nullptr);

/// \}

} // namespace smdl
