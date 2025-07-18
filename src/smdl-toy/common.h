#pragma once

#include <array>
#include <random>

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

#include "pcg_random.hpp"

#if __clang__
#define SMDL_USE_EXT_VECTOR_TYPES 1
#endif // #if __clang__

typedef pcg32_k1024 RNG;

[[nodiscard]] inline float generate_canonical(RNG &rng) noexcept {
  return std::min(static_cast<float>(static_cast<double>(rng()) * 0x1.0p-32),
                  0x1.FFFFFEp-1f);
}

constexpr float Eps = 0.0001f;

constexpr float Inf = std::numeric_limits<float>::infinity();

struct Ray final {
  smdl::float3 org{};

  smdl::float3 dir{};

  float tmin{};

  float tmax{};

  void transform(const smdl::float4x4 &m) {
    org = m * smdl::float4(org, 1.0f);
    dir = m * smdl::float4(dir, 0.0f);
  }
};

struct Intersection final {
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
};

static constexpr size_t WAVELENGTH_BASE_MAX = 16;

static constexpr float WAVELENGTH_MIN = 380.0f;

static constexpr float WAVELENGTH_MAX = 720.0f;

typedef std::array<float, WAVELENGTH_BASE_MAX> Color;

#if 0
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
  constexpr ColorVector(float s) : v(v) {}
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

  template <typename U>
  constexpr ColorVector &operator+=(const U &rhs) noexcept {
    return *this = *this + rhs;
  }

  template <typename U>
  constexpr ColorVector &operator-=(const U &rhs) noexcept {
    return *this = *this - rhs;
  }

  template <typename U>
  constexpr ColorVector &operator*=(const U &rhs) noexcept {
    return *this = *this * rhs;
  }

  template <typename U>
  constexpr ColorVector &operator/=(const U &rhs) noexcept {
    return *this = *this / rhs;
  }

public:
  vector_type v{};
};
#endif
