#pragma once

#include <array>
#include <random>
#include <variant>

#if __clang__
#pragma clang diagnostic ignored "-Wpsabi"
#endif // #if __clang__

#include "oneapi/tbb/parallel_for.h"

#include "embree4/rtcore_buffer.h"
#include "embree4/rtcore_common.h"
#include "embree4/rtcore_config.h"
#include "embree4/rtcore_device.h"
#include "embree4/rtcore_geometry.h"
#include "embree4/rtcore_ray.h"
#include "embree4/rtcore_scene.h"

#include "smdl/Compiler.h"
#include "smdl/Logger.h"
#include "smdl/Support/DiscreteDistribution.h"
#include "smdl/Support/SpectralRenderImage.h"

#include "pcg_random.hpp"

#if __clang__
#define SMDL_USE_EXT_VECTOR_TYPES 1
#endif // #if __clang__

constexpr float PI = smdl::PI;

constexpr float EPS = 0.0001f;

constexpr float INF = std::numeric_limits<float>::infinity();

constexpr float QUIET_NAN = std::numeric_limits<float>::quiet_NaN();

constexpr size_t WAVELENGTH_BASE_MAX = 16;

constexpr float WAVELENGTH_MIN = 380.0f;

constexpr float WAVELENGTH_MAX = 720.0f;

constexpr float DIRAC_DELTA = 1.0f;

using RNG = pcg32_k1024;

[[nodiscard]] inline float generate_canonical(RNG &rng) {
  return std::min(static_cast<float>(static_cast<double>(rng()) * 0x1.0p-32),
                  0x1.FFFFFEp-1f);
}

[[nodiscard]] constexpr float DiracDelta(float pdf) { return -pdf; }

[[nodiscard]] inline bool IsDiracDelta(float pdf) {
  return std::signbit(pdf) && std::isfinite(pdf);
}

class Color final {
public:
#if SMDL_USE_EXT_VECTOR_TYPES
  using vector_type =
      float __attribute__((ext_vector_type(WAVELENGTH_BASE_MAX)));
#else
  using vector_type = std::array<float, WAVELENGTH_BASE_MAX>;
#endif // #if SMDL_USE_EXT_VECTOR_TYPES

  constexpr Color() = default;

  constexpr Color(vector_type v) : v(v) {}

#if SMDL_USE_EXT_VECTOR_TYPES
  constexpr Color(float s) : v(s) {}
#else
  constexpr Color(float s) {
    for (size_t i = 0; i < WAVELENGTH_BASE_MAX; i++)
      v[i] = s;
  }
#endif

public:
  [[nodiscard]] constexpr size_t size() const noexcept {
    return WAVELENGTH_BASE_MAX;
  }

  [[nodiscard]] auto data() noexcept -> float * {
#if SMDL_USE_EXT_VECTOR_TYPES
    return reinterpret_cast<float *>(&v);
#else
    return v.data();
#endif // #if SMDL_USE_EXT_VECTOR_TYPES
  }

  [[nodiscard]] auto data() const noexcept -> const float * {
    return const_cast<Color &>(*this).data();
  }

  [[nodiscard]] auto operator[](size_t i) noexcept -> float & {
    return data()[i];
  }

  [[nodiscard]] auto operator[](size_t i) const noexcept -> const float & {
    return data()[i];
  }

public:
#if SMDL_USE_EXT_VECTOR_TYPES
  [[nodiscard]] constexpr Color operator+() const noexcept { return v; }

  [[nodiscard]] constexpr Color operator-() const noexcept { return -v; }

  [[nodiscard]]
  constexpr Color operator+(const Color &rhs) const noexcept {
    return v + rhs.v;
  }

  [[nodiscard]]
  constexpr Color operator-(const Color &rhs) const noexcept {
    return v - rhs.v;
  }

  [[nodiscard]]
  constexpr Color operator*(const Color &rhs) const noexcept {
    return v * rhs.v;
  }

  [[nodiscard]]
  constexpr Color operator/(const Color &rhs) const noexcept {
    return v / rhs.v;
  }

  [[nodiscard]] constexpr Color operator+(float rhs) const noexcept {
    return v + rhs;
  }

  [[nodiscard]] constexpr Color operator-(float rhs) const noexcept {
    return v - rhs;
  }

  [[nodiscard]] constexpr Color operator*(float rhs) const noexcept {
    return v * rhs;
  }

  [[nodiscard]] constexpr Color operator/(float rhs) const noexcept {
    return v / rhs;
  }

  [[nodiscard]]
  friend constexpr Color operator+(float lhs, const Color &rhs) noexcept {
    return lhs + rhs.v;
  }

  [[nodiscard]]
  friend constexpr Color operator-(float lhs, const Color &rhs) noexcept {
    return lhs - rhs.v;
  }

  [[nodiscard]]
  friend constexpr Color operator*(float lhs, const Color &rhs) noexcept {
    return lhs * rhs.v;
  }

  [[nodiscard]]
  friend constexpr Color operator/(float lhs, const Color &rhs) noexcept {
    return lhs / rhs.v;
  }
#else
  [[nodiscard]] constexpr Color operator+() const noexcept { return v; }

  [[nodiscard]] constexpr Color operator-() const noexcept {
    Color result{};
    for (size_t i = 0; i < WAVELENGTH_BASE_MAX; i++)
      result.v[i] = -v[i];
    return result;
  }

  [[nodiscard]]
  constexpr Color operator+(const Color &rhs) const noexcept {
    Color result{};
    for (size_t i = 0; i < WAVELENGTH_BASE_MAX; i++)
      result.v[i] = v[i] + rhs.v[i];
    return result;
  }

  [[nodiscard]]
  constexpr Color operator-(const Color &rhs) const noexcept {
    Color result{};
    for (size_t i = 0; i < WAVELENGTH_BASE_MAX; i++)
      result.v[i] = v[i] - rhs.v[i];
    return result;
  }

  [[nodiscard]]
  constexpr Color operator*(const Color &rhs) const noexcept {
    Color result{};
    for (size_t i = 0; i < WAVELENGTH_BASE_MAX; i++)
      result.v[i] = v[i] * rhs.v[i];
    return result;
  }

  [[nodiscard]]
  constexpr Color operator/(const Color &rhs) const noexcept {
    Color result{};
    for (size_t i = 0; i < WAVELENGTH_BASE_MAX; i++)
      result.v[i] = v[i] / rhs.v[i];
    return result;
  }

  [[nodiscard]] constexpr Color operator+(float rhs) const noexcept {
    Color result{};
    for (size_t i = 0; i < WAVELENGTH_BASE_MAX; i++)
      result.v[i] = v[i] + rhs;
    return result;
  }

  [[nodiscard]] constexpr Color operator-(float rhs) const noexcept {
    Color result{};
    for (size_t i = 0; i < WAVELENGTH_BASE_MAX; i++)
      result.v[i] = v[i] - rhs;
    return result;
  }

  [[nodiscard]] constexpr Color operator*(float rhs) const noexcept {
    Color result{};
    for (size_t i = 0; i < WAVELENGTH_BASE_MAX; i++)
      result.v[i] = v[i] * rhs;
    return result;
  }

  [[nodiscard]] constexpr Color operator/(float rhs) const noexcept {
    Color result{};
    for (size_t i = 0; i < WAVELENGTH_BASE_MAX; i++)
      result.v[i] = v[i] / rhs;
    return result;
  }

  [[nodiscard]]
  friend constexpr Color operator+(float lhs, const Color &rhs) noexcept {
    Color result{};
    for (size_t i = 0; i < WAVELENGTH_BASE_MAX; i++)
      result.v[i] = lhs + rhs.v[i];
    return result;
  }

  [[nodiscard]]
  friend constexpr Color operator-(float lhs, const Color &rhs) noexcept {
    Color result{};
    for (size_t i = 0; i < WAVELENGTH_BASE_MAX; i++)
      result.v[i] = lhs - rhs.v[i];
    return result;
  }

  [[nodiscard]]
  friend constexpr Color operator*(float lhs, const Color &rhs) noexcept {
    Color result{};
    for (size_t i = 0; i < WAVELENGTH_BASE_MAX; i++)
      result.v[i] = lhs * rhs.v[i];
    return result;
  }

  [[nodiscard]]
  friend constexpr Color operator/(float lhs, const Color &rhs) noexcept {
    Color result{};
    for (size_t i = 0; i < WAVELENGTH_BASE_MAX; i++)
      result.v[i] = lhs / rhs.v[i];
    return result;
  }
#endif // #if SMDL_USE_EXT_VECTOR_TYPES

  template <typename U> constexpr Color &operator+=(const U &rhs) noexcept {
    return *this = *this + rhs;
  }

  template <typename U> constexpr Color &operator-=(const U &rhs) noexcept {
    return *this = *this - rhs;
  }

  template <typename U> constexpr Color &operator*=(const U &rhs) noexcept {
    return *this = *this * rhs;
  }

  template <typename U> constexpr Color &operator/=(const U &rhs) noexcept {
    return *this = *this / rhs;
  }

  [[nodiscard]] float average() const noexcept {
    float result{};
    for (size_t i = 0; i < WAVELENGTH_BASE_MAX; i++)
      result += v[i];
    return result / WAVELENGTH_BASE_MAX;
  }

  void set_non_finite_to_zero() noexcept {
    for (size_t i = 0; i < WAVELENGTH_BASE_MAX; i++)
      v[i] = smdl::finite_or_zero(v[i]);
  }

public:
  vector_type v{};
};

[[nodiscard]] inline float uniform_disk_pdf(float radius = 1) {
  return 1.0f / (PI * radius * radius);
}

[[nodiscard]] inline smdl::float2 uniform_disk_sample(smdl::float2 xi) {
  xi = xi * 2.0f - smdl::float2(1.0f);
  xi.x = (xi.x == 0.0f) ? EPS : xi.x;
  xi.y = (xi.y == 0.0f) ? EPS : xi.y;
  bool cond = std::abs(xi.x) > std::abs(xi.y);
  float rad = cond ? xi.x : xi.y;
  float phi = cond ? (PI / 4.0f) * xi.y / xi.x
                   : (PI / 2.0f) - (PI / 4.0f) * xi.x / xi.y;
  return smdl::float2(rad * std::cos(phi), rad * std::sin(phi));
}

[[nodiscard]] inline float cosine_hemisphere_pdf(float cosTheta) {
  return std::max(cosTheta, 0.0f) / PI;
}

[[nodiscard]] inline smdl::float3 cosine_hemisphere_sample(smdl::float2 xi,
                                                           float *pdf = {}) {
  auto sinTheta{uniform_disk_sample(xi)};
  auto cosTheta{std::sqrt(std::max(0.0f, 1.0f - sinTheta.x * sinTheta.x -
                                             sinTheta.y * sinTheta.y))};
  if (pdf)
    *pdf = cosTheta / PI;
  return smdl::float3(sinTheta.x, sinTheta.y, cosTheta);
}

[[nodiscard]] inline float uniform_sphere_pdf(float radius = 1) {
  return 1.0f / (4.0f * PI * radius * radius);
}

[[nodiscard]] inline smdl::float3 uniform_sphere_sample(smdl::float2 xi,
                                                        float *pdf = {}) {
  float cosTheta{std::max(-1.0f, std::min(2.0f * xi.x - 1.0f, 1.0f))};
  float sinTheta{std::sqrt(1.0f - cosTheta * cosTheta)};
  float phi{2.0f * PI * xi.y};
  if (pdf)
    *pdf = uniform_sphere_pdf();
  return smdl::float3(sinTheta * std::cos(phi), sinTheta * std::sin(phi),
                      cosTheta);
}

[[nodiscard]] inline float uniform_cone_pdf(float zMin) {
  return 1.0f / (2.0f * PI * (1.0f - zMin));
}

[[nodiscard]] inline smdl::float3 uniform_cone_sample(smdl::float2 xi, //
                                                      float zMin,      //
                                                      float *pdf = {}) {
  float cosTheta{std::max(zMin, std::min((1.0f - xi.x) * zMin + xi.x, 1.0f))};
  float sinTheta{std::sqrt(1.0f - cosTheta * cosTheta)};
  float phi{2.0f * PI * xi.y};
  if (pdf)
    *pdf = uniform_cone_pdf(zMin);
  return smdl::float3(sinTheta * std::cos(phi), sinTheta * std::sin(phi),
                      cosTheta);
}

[[nodiscard]]
inline std::pair<smdl::float3, float> direction_and_distance(smdl::float3 from,
                                                             smdl::float3 to) {
  auto vec{smdl::double3(to) - smdl::double3(from)};
  if (auto vecLen{smdl::length(vec)}; vecLen > 0.0) {
    return {smdl::float3(vec / vecLen), float(vecLen)};
  } else {
    return {smdl::float3(0.f), 0.f};
  }
}

class Ray final {
public:
  smdl::float3 org{};

  smdl::float3 dir{};

  float tmin{};

  float tmax{};

  void transform(const smdl::float4x4 &m) {
    org = m * smdl::float4(org, 1.0f);
    dir = m * smdl::float4(dir, 0.0f);
  }
};

class Intersection final {
public:
  uint32_t meshInstanceIndex{uint32_t(-1)};

  uint32_t meshIndex{uint32_t(-1)};

  uint32_t faceIndex{uint32_t(-1)};

  uint32_t materialIndex{uint32_t(-1)};

  smdl::float3 bary{};

  smdl::float3 point{};

  smdl::float3 normal{};

  smdl::float3 tangent{};

  smdl::float3 geometryNormal{};

  smdl::float3 geometryTangent{};

  smdl::float2 texcoord{};

  void transform(const smdl::float4x4 &matrix) {
    point = matrix * smdl::float4(point, 1.0f);
    normal = matrix * smdl::float4(normal, 0.0f);
    tangent = matrix * smdl::float4(tangent, 0.0f);
    geometryNormal = matrix * smdl::float4(geometryNormal, 0.0f);
    geometryTangent = matrix * smdl::float4(geometryTangent, 0.0f);
  }

  void initialize_state(smdl::State &state) {
    state.position = point;
    state.normal = normal;
    state.texture_coordinate[0] = {texcoord.x, texcoord.y, 0};
    state.texture_tangent_u[0] = tangent;
    state.texture_tangent_v[0] =
        smdl::cross(state.normal, state.texture_tangent_u[0]);
    state.geometry_normal = geometryNormal;
    state.geometry_tangent_u[0] = geometryTangent;
    state.geometry_tangent_v[0] =
        smdl::cross(geometryNormal, state.geometry_tangent_u[0]);
    state.object_id = meshInstanceIndex;
    state.ptex_face_id = faceIndex;
    state.ptex_face_uv = {bary[1], bary[2]};
    state.finalize_for_runtime_conventions();
  }

  [[nodiscard]] float
  shading_normal_correction(const smdl::float3 &wPrev,
                            const smdl::float3 &wNext) const {
    float numer{smdl::dot(wPrev, normal) * smdl::dot(wNext, geometryNormal)};
    float denom{smdl::dot(wPrev, geometryNormal) * smdl::dot(wNext, normal)};
    return denom == 0.0f ? 1.0f : numer / denom;
  }
};
