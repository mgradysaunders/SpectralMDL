#pragma once

#include <cmath>

#include "smdl/common.h"

namespace smdl {

template <typename T, size_t M> struct Vector;

template <typename T> struct alignas(2 * sizeof(T)) Vector<T, 2> {
public:
  constexpr Vector() = default;
  constexpr Vector(T x) : Vector(x, x) {}
  constexpr Vector(T x, T y) : x(x), y(y) {}
  T x{};
  T y{};
  [[nodiscard]] constexpr auto operator[](size_t i) -> T & { return (&x)[i]; }
  [[nodiscard]] constexpr auto operator[](size_t i) const -> const T & { return (&x)[i]; }
};

template <typename T> struct alignas(4 * sizeof(T)) Vector<T, 3> {
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

template <typename T> struct alignas(4 * sizeof(T)) Vector<T, 4> {
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

struct default_bsdf_t final {};

struct default_vdf_t final {};

struct default_edf_t final {};

struct default_hair_bsdf_t final {};

struct material_emission_t final {};

struct material_surface_t final {};

struct material_volume_t final {};

struct material_geometry_t final {};

struct material_t final {};

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

struct Ptexture_t final {
  void *texture{};

  void *filter{};
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

  /// The object id.
  int_t object_id{};

  int_t ptex_face_id{};

  float2_t ptex_face_uv{};

  float3_t direction{};

  /// The animation time.
  float_t animation_time{0.0f};

  /// The wavelengths in nanometers.
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

template <typename T, size_t N> [[nodiscard]] constexpr Vector<T, N> operator+(const Vector<T, N> &v0, const Vector<T, N> &v1) {
  Vector<T, N> v{};
  for (size_t i{}; i < N; i++)
    v[i] = v0[i] + v1[i];
  return v;
}

template <typename T, size_t N> [[nodiscard]] constexpr Vector<T, N> operator-(const Vector<T, N> &v0, const Vector<T, N> &v1) {
  Vector<T, N> v{};
  for (size_t i{}; i < N; i++)
    v[i] = v0[i] - v1[i];
  return v;
}

template <typename T, size_t N> [[nodiscard]] constexpr Vector<T, N> operator*(const T &s0, const Vector<T, N> &v1) {
  Vector<T, N> v{};
  for (size_t i{}; i < N; i++)
    v[i] = s0 * v1[i];
  return v;
}

template <typename T, size_t N> [[nodiscard]] constexpr Vector<T, N> operator*(const Vector<T, N> &v0, const T &s1) {
  Vector<T, N> v{};
  for (size_t i{}; i < N; i++)
    v[i] = v0[i] * s1;
  return v;
}

template <typename T, size_t N> [[nodiscard]] constexpr Vector<T, N> operator/(const Vector<T, N> &v0, const T &s1) {
  Vector<T, N> v{};
  for (size_t i{}; i < N; i++)
    v[i] = v0[i] / s1;
  return v;
}

template <typename T, size_t P, size_t N, size_t M>
[[nodiscard]] constexpr Matrix<T, P, M> operator*(const Matrix<T, N, M> &m0, const Matrix<T, P, N> &m1) {
  Matrix<T, P, M> m{};
  for (size_t i{}; i < M; i++)
    for (size_t j{}; j < P; j++)
      for (size_t k{}; k < N; k++)
        m[j][i] += m0[k][i] * m1[j][k];
  return m;
}

template <typename T, size_t N, size_t M>
[[nodiscard]] constexpr Vector<T, M> operator*(const Matrix<T, N, M> &m0, const Vector<T, N> &v1) {
  Vector<T, M> v{};
  for (size_t i{}; i < M; i++)
    for (size_t k{}; k < N; k++)
      v[i] += m0[k][i] * v1[k];
  return v;
}

template <typename T> [[nodiscard]] inline T dot(Vector<T, 2> u, Vector<T, 2> v) { return u.x * v.x + u.y * v.y; }

template <typename T> [[nodiscard]] inline T dot(Vector<T, 3> u, Vector<T, 3> v) { return u.x * v.x + u.y * v.y + u.z * v.z; }

template <typename T> [[nodiscard]] inline T dot(Vector<T, 4> u, Vector<T, 4> v) {
  return u.x * v.x + u.y * v.y + u.z * v.z + u.w * v.w;
}

template <typename T, size_t N> [[nodiscard]] inline auto length(Vector<T, N> v) { return std::sqrt(dot(v, v)); }

template <typename T, size_t N> [[nodiscard]] inline auto normalize(Vector<T, N> v) { return v / length(v); }

template <typename T> [[nodiscard]] inline Vector<T, 3> cross(Vector<T, 3> u, Vector<T, 3> v) {
  return {u.y * v.z - u.z * v.y, u.z * v.x - u.x * v.z, u.x * v.y - u.y * v.x};
}

template <bool Inverse = false> [[nodiscard]] inline float4x4_t look_at(float3_t from, float3_t to, float3_t up = {0, 0, 1}) {
  float3_t z{normalize(from - to)};
  float3_t x{normalize(cross(up, z))};
  float3_t y{cross(z, x)};
  if constexpr (!Inverse) {
    return float4x4_t{
        float4_t{x.x, x.y, x.z, 0.0f}, float4_t{y.x, y.y, y.z, 0.0f}, float4_t{z.x, z.y, z.z, 0.0f},
        float4_t{from.x, from.y, from.z, 1.0f}};
  } else {
    return float4x4_t{
        float4_t{x.x, y.x, z.x, 0.0f}, float4_t{x.y, y.y, z.y, 0.0f}, float4_t{x.z, y.z, z.z, 0.0f},
        float4_t{-dot(x, from), -dot(y, from), -dot(z, from), 1.0f}};
  }
}

[[nodiscard]] inline float3_t transform_affine(const float4x4_t &m, const float3_t &v, float w) {
  float4_t vw{v.x, v.y, v.z, w};
  vw = m * vw;
  return {vw.x, vw.y, vw.z};
}

[[nodiscard]] inline float3_t perpendicular_to(const float3_t &v) {
  float3_t z{normalize(v)};
  float3_t x{z.z < -0.9999f ? float3_t{0.0f, -1.0f, 0.0f} : float3_t{-z.x / (z.z + 1.0f) + 1.0f, -z.y / (z.z + 1.0f), -1.0f}};
  return normalize(x - dot(x, z) * z);
}

} // namespace smdl
