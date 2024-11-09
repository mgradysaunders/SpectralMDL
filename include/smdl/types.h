#pragma once

#include "smdl/common.h"

namespace smdl {

template <typename T, size_t M> struct Vector;

template <typename T> struct alignas(2 * sizeof(T)) Vector<T, 2> final {
public:
  constexpr Vector() = default;
  constexpr Vector(T x) : Vector(x, x) {}
  constexpr Vector(T x, T y) : x(x), y(y) {}
  T x{};
  T y{};
  [[nodiscard]] constexpr auto operator[](size_t i) -> T & { return (&x)[i]; }
  [[nodiscard]] constexpr auto operator[](size_t i) const -> const T & { return (&x)[i]; }
};

template <typename T> struct alignas(4 * sizeof(T)) Vector<T, 3> final {
public:
  constexpr Vector() = default;
  constexpr Vector(T x) : Vector(x, x, x) {}
  constexpr Vector(T x, T y, T z) : x(x), y(y), z(z) {}
  T x{};
  T y{};
  T z{};
  [[nodiscard]] constexpr auto operator[](size_t i) -> T & { return (&x)[i]; }
  [[nodiscard]] constexpr auto operator[](size_t i) const -> const T & { return (&x)[i]; }
};

template <typename T> struct alignas(4 * sizeof(T)) Vector<T, 4> final {
public:
  constexpr Vector() = default;
  constexpr Vector(T x) : Vector(x, x, x, x) {}
  constexpr Vector(T x, T y, T z, T w) : x(x), y(y), z(z), w(w) {}
  T x{};
  T y{};
  T z{};
  T w{};
  [[nodiscard]] constexpr auto operator[](size_t i) -> T & { return (&x)[i]; }
  [[nodiscard]] constexpr auto operator[](size_t i) const -> const T & { return (&x)[i]; }
};

using int_t = int32_t;
using int2_t = Vector<int_t, 2>;
using int3_t = Vector<int_t, 3>;
using int4_t = Vector<int_t, 4>;
static_assert(sizeof(int_t) == 4 && alignof(int_t) == 4);
static_assert(sizeof(int2_t) == 8 && alignof(int2_t) == 8);
static_assert(sizeof(int3_t) == 16 && alignof(int3_t) == 16);
static_assert(sizeof(int4_t) == 16 && alignof(int4_t) == 16);

using float_t = float;
using float2_t = Vector<float_t, 2>;
using float3_t = Vector<float_t, 3>;
using float4_t = Vector<float_t, 4>;
static_assert(sizeof(float_t) == 4 && alignof(float_t) == 4);
static_assert(sizeof(float2_t) == 8 && alignof(float2_t) == 8);
static_assert(sizeof(float3_t) == 16 && alignof(float3_t) == 16);
static_assert(sizeof(float4_t) == 16 && alignof(float4_t) == 16);

using double_t = double;
using double2_t = Vector<double_t, 2>;
using double3_t = Vector<double_t, 3>;
using double4_t = Vector<double_t, 4>;
static_assert(sizeof(double_t) == 8 && alignof(double_t) == 8);
static_assert(sizeof(double2_t) == 16 && alignof(double2_t) == 16);
static_assert(sizeof(double3_t) == 32 && alignof(double3_t) == 32);
static_assert(sizeof(double4_t) == 32 && alignof(double4_t) == 32);

template <typename T, size_t N, size_t M> struct Matrix final {
public:
  constexpr Matrix() = default;
  constexpr Matrix(T x) {
    for (size_t i{}; i < std::min(N, M); i++)
      v[i][i] = x;
  }
  constexpr Matrix(Vector<T, M> v0, Vector<T, M> v1) requires(N == 2) : v{v0, v1} {}
  constexpr Matrix(Vector<T, M> v0, Vector<T, M> v1, Vector<T, M> v2) requires(N == 3) : v{v0, v1, v2} {}
  constexpr Matrix(Vector<T, M> v0, Vector<T, M> v1, Vector<T, M> v2, Vector<T, M> v3) requires(N == 4) : v{v0, v1, v2, v3} {}
  [[nodiscard]] constexpr auto &operator[](size_t i) { return v[i]; }
  [[nodiscard]] constexpr auto &operator[](size_t i) const { return v[i]; }

  Vector<T, M> v[N]{};
};

using float2x2_t = Matrix<float_t, 2, 2>;
using float3x2_t = Matrix<float_t, 3, 2>;
using float4x2_t = Matrix<float_t, 4, 2>;
using float2x3_t = Matrix<float_t, 2, 3>;
using float3x3_t = Matrix<float_t, 3, 3>;
using float4x3_t = Matrix<float_t, 4, 3>;
using float2x4_t = Matrix<float_t, 2, 4>;
using float3x4_t = Matrix<float_t, 3, 4>;
using float4x4_t = Matrix<float_t, 4, 4>;

using double2x2_t = Matrix<double_t, 2, 2>;
using double3x2_t = Matrix<double_t, 3, 2>;
using double4x2_t = Matrix<double_t, 4, 2>;
using double2x3_t = Matrix<double_t, 2, 3>;
using double3x3_t = Matrix<double_t, 3, 3>;
using double4x3_t = Matrix<double_t, 4, 3>;
using double2x4_t = Matrix<double_t, 2, 4>;
using double3x4_t = Matrix<double_t, 3, 4>;
using double4x4_t = Matrix<double_t, 4, 4>;

// intensity_mode
enum class intensity_mode_t : int_t {
  intensity_radiant_exitance = 0,

  intensity_power = 1,
};

struct bsdf_measurement_t final {
  void *ptr{};
};

struct light_profile_t final {
  void *ptr{};
};

struct string_t final {
  const void *ptr{};

  int_t len{};

  [[nodiscard]] operator std::string_view() const { return std::string_view(static_cast<const char *>(ptr), len); }

  [[nodiscard]] operator std::string() const { return std::string(std::string_view(*this)); }
};

struct source_location_t final {
  string_t file{};

  int_t line{};
};

struct SMDL_EXPORT Image final {
  void flip_horizontal();

  void flip_vertical();

  [[nodiscard]] auto &get_texel(int2_t i) { return texels[i.y * extent.x + i.x]; }

  [[nodiscard]] auto &get_texel(int2_t i) const { return texels[i.y * extent.x + i.x]; }

  int2_t extent{};

  std::vector<float4_t> texels{};
};

struct image_t final {
  int2_t extent{};

  float4_t *texels{};
};

struct texture_2d_t final {
  int_t gamma{};

  int2_t tile_count{};

  image_t *tiles{};
};

struct texture_3d_t final {
  int_t gamma{};

  int3_t extent{};

  int3_t stride{};

  float4_t *texels{};
};

struct texture_cube_t final {
  int_t gamma{};

  int2_t extent{};

  float4_t **texels{};
};

struct texture_ptex_t final {
  int_t gamma{};

  void *ptr{};
};

struct SMDL_EXPORT state_t final {
public:
  /// The position in object space.
  float3_t position{};

  /// The normal in object space.
  float3_t normal{0, 0, 1};

  /// The geometry normal in object space.
  float3_t geometry_normal{0, 0, 1};

  float3_t motion{};

  static constexpr size_t TEXTURE_SPACE_MAX = 4;

  int_t texture_space_max{1};

  float3_t texture_coordinate[TEXTURE_SPACE_MAX]{};

  // The texture tangent U vector(s) in object space.
  float3_t texture_tangent_u[TEXTURE_SPACE_MAX] = {float3_t{1, 0, 0}, float3_t{1, 0, 0}, float3_t{1, 0, 0}, float3_t{1, 0, 0}};

  // The texture tangent V vector(s) in object space.
  float3_t texture_tangent_v[TEXTURE_SPACE_MAX] = {float3_t{0, 1, 0}, float3_t{0, 1, 0}, float3_t{0, 1, 0}, float3_t{0, 1, 0}};

  // The geometry tangent U vector(s) in object space.
  float3_t geometry_tangent_u[TEXTURE_SPACE_MAX] = {float3_t{1, 0, 0}, float3_t{1, 0, 0}, float3_t{1, 0, 0}, float3_t{1, 0, 0}};

  // The geometry tangent V vector(s) in object space.
  float3_t geometry_tangent_v[TEXTURE_SPACE_MAX] = {float3_t{0, 1, 0}, float3_t{0, 1, 0}, float3_t{0, 1, 0}, float3_t{0, 1, 0}};

  int_t object_id{};

  float3_t direction{};

  float_t animation_time{0.0f};

  const float_t *wavelength_base{};

  /// The wavelength range minimum in nanometers.
  float_t wavelength_min{380.0f};

  /// The wavelength range maximum in nanometers.
  float_t wavelength_max{720.0f};

  /// The meters per scene unit.
  float_t meters_per_scene_unit{1.0f};

  /// The object-to-world matrix.
  float4x4_t object_to_world_matrix_fwd{float4x4_t(1.0f)};

  /// The world-to-object matrix.
  float4x4_t object_to_world_matrix_inv{float4x4_t(1.0f)};

  /// The internal-to-object matrix.
  float4x4_t internal_to_object_matrix_fwd{float4x4_t(1.0f)};

  /// The internal-to-object matrix inverse.
  float4x4_t internal_to_object_matrix_inv{float4x4_t(1.0f)};

  /// The internal-to-world matrix.
  float4x4_t internal_to_world_matrix_fwd{float4x4_t(1.0f)};

  /// The internal-to-world matrix inverse.
  float4x4_t internal_to_world_matrix_inv{float4x4_t(1.0f)};

public:
  void finalize_for_runtime_conventions();
};

} // namespace smdl
