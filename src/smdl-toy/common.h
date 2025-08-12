#pragma once

#include <array>
#include <random>
#include <variant>

#if __clang__
#pragma clang diagnostic ignored "-Wpsabi"
#endif // #if __clang__

#include "embree4/rtcore_buffer.h"
#include "embree4/rtcore_common.h"
#include "embree4/rtcore_config.h"
#include "embree4/rtcore_device.h"
#include "embree4/rtcore_geometry.h"
#include "embree4/rtcore_ray.h"
#include "embree4/rtcore_scene.h"

#include "smdl/Compiler.h"
#include "smdl/Support/ColorVector.h"
#include "smdl/Support/DiscreteDistribution.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Sampling.h"
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

template <typename... Seeds>
[[nodiscard]] inline RNG make_RNG(Seeds... seeds) noexcept {
  if constexpr (sizeof...(Seeds) == 1) {
    return RNG{seeds...};
  } else {
    using SeedT = std::common_type_t<Seeds...>;
    return RNG{std::seed_seq{static_cast<SeedT>(seeds)...}};
  }
}

class Random final {
public:
  Random(std::function<float()> gen) : gen(std::move(gen)) {}

  [[nodiscard]] float generate_canonical() const { return gen(); }

  [[nodiscard]] smdl::float2 generate_canonical2() const {
    return {gen(), gen()};
  }

  [[nodiscard]] smdl::float3 generate_canonical3() const {
    return {gen(), gen(), gen()};
  }

  [[nodiscard]] smdl::float4 generate_canonical4() const {
    return {gen(), gen(), gen(), gen()};
  }

private:
  std::function<float()> gen{};
};

using Color = smdl::ColorVector<WAVELENGTH_BASE_MAX>;

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
    return denom == 0.0f ? 1.0f : std::abs(numer / denom);
  }
};

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
