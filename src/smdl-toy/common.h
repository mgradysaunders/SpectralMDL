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

#include "smdl/type.h"

struct Ray final {
  smdl::float3_t org{};

  smdl::float3_t dir{};

  smdl::float_t tmin{};

  smdl::float_t tmax{};

  void transform(const smdl::float4x4_t &m) {
    org = smdl::transform_affine(m, org, 1.0f);
    dir = smdl::transform_affine(m, dir, 0.0f);
  }
};

struct Hit final {
  uint32_t meshInstanceIndex{uint32_t(-1)};

  uint32_t meshIndex{uint32_t(-1)};

  uint32_t faceIndex{uint32_t(-1)};

  smdl::float3_t bary{};

  smdl::float3_t point{};

  smdl::float3_t normal{};

  smdl::float3_t tangent{};

  smdl::float3_t geometryNormal{};

  smdl::float3_t geometryTangent{};

  smdl::float2_t texcoord{};

  void transform(const smdl::float4x4_t &m) {
    point = smdl::transform_affine(m, point, 1.0f);
    normal = smdl::transform_affine(m, point, 0.0f);
    tangent = smdl::transform_affine(m, point, 0.0f);
    geometryNormal = smdl::transform_affine(m, point, 0.0f);
    geometryTangent = smdl::transform_affine(m, point, 0.0f);
  }
};

static constexpr size_t NUM_WAVELENS = 16;

static constexpr float MIN_WAVELEN = 380.0f;

static constexpr float MAX_WAVELEN = 720.0f;

typedef std::array<float, NUM_WAVELENS> Color;

typedef std::default_random_engine RNG;

