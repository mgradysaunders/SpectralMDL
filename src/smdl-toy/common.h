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

struct Hit final {
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

  void transform(const smdl::float4x4 &m) {
    point = m * smdl::float4(point, 1.0f);
    normal = m * smdl::float4(normal, 0.0f);
    tangent = m * smdl::float4(tangent, 0.0f);
    geometryNormal = m * smdl::float4(geometryNormal, 0.0f);
    geometryTangent = m * smdl::float4(geometryTangent, 0.0f);
  }
};

static constexpr size_t WAVELENGTH_BASE_MAX = 16;

static constexpr float WAVELENGTH_MIN = 380.0f;

static constexpr float WAVELENGTH_MAX = 720.0f;

typedef std::array<float, WAVELENGTH_BASE_MAX> Color;

typedef std::default_random_engine RNG;
