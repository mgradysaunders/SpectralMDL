#pragma once

#include "assimp/scene.h"
#include "embree4/rtcore_buffer.h"
#include "embree4/rtcore_common.h"
#include "embree4/rtcore_config.h"
#include "embree4/rtcore_device.h"
#include "embree4/rtcore_geometry.h"
#include "embree4/rtcore_ray.h"
#include "embree4/rtcore_scene.h"
#include "pcg_random.hpp"

#include "smdl/Compiler.h"
#include "smdl/Support/ColorVector.h"
#include "smdl/Support/Sampling.h"

constexpr size_t WAVELENGTH_BASE_MAX = 16;
constexpr float WAVELENGTH_MIN = 380.0f;
constexpr float WAVELENGTH_MAX = 720.0f;
constexpr float EPS = 0.0001f;
constexpr float INF = std::numeric_limits<float>::infinity();
constexpr float QUIET_NAN = std::numeric_limits<float>::quiet_NaN();
constexpr uint32_t INVALID_INDEX = uint32_t(-1);
using smdl::PI;
using namespace smdl::vector_type_aliases;
using namespace smdl::matrix_type_aliases;
using Color = smdl::ColorVector<WAVELENGTH_BASE_MAX>;
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

class AnyRandom final {
public:
  explicit AnyRandom(std::in_place_t, std::function<float()> gen)
      : gen(std::move(gen)) {}

  explicit AnyRandom(RNG &rng)
      : gen([&rng]() { return smdl::generate_canonical(rng); }) {}

  [[nodiscard]] operator float() const { return gen(); }

  [[nodiscard]] operator float2() const { return {gen(), gen()}; }

  [[nodiscard]] operator float3() const { return {gen(), gen(), gen()}; }

  [[nodiscard]] operator float4() const { return {gen(), gen(), gen(), gen()}; }

private:
  std::function<float()> gen{};
};

class Ray final {
public:
  /// Evaluate.
  [[nodiscard]] float3 operator()(float t) const noexcept {
    return org + t * dir;
  }

  /// Apply transform.
  void transform(const float4x4 &xf) noexcept {
    org = xf * float4(org, 1.0f);
    dir = xf * float4(dir, 0.0f);
  }

public:
  float3 org{};            ///< The origin.
  float3 dir{};            ///< The direction.
  float tmin{EPS};         ///< The minimum parameter.
  mutable float tmax{INF}; ///< The maximum parameter.
};

/// A hit.
class Hit final {
public:
  /// Apply geometry to SMDL state.
  void apply_geometry_to_state(smdl::State &state) const noexcept {
    state.object_to_world_matrix = objectToWorld;
    state.position = point;
    state.normal = normal;
    state.texture_space_max = 1;
    state.texture_coordinate[0] = {texcoord.x, texcoord.y, 0};
    state.texture_tangent_u[0] = tangent;
    state.texture_tangent_v[0] = smdl::cross(normal, tangent);
    state.geometry_normal = geometryNormal;
    state.geometry_tangent_u[0] = geometryTangent;
    state.geometry_tangent_v[0] = smdl::cross(geometryNormal, geometryTangent);
    state.object_id = meshInstanceIndex;
    state.ptex_face_id = faceIndex;
    state.ptex_face_uv = {bary[1], bary[2]};
    state.finalize_and_apply_internal_space_conventions();
  }

  /// Apply transform.
  void transform(const float4x4 &xf) noexcept {
    point = xf * float4(point, 1.0f);
    normal = xf * float4(normal, 0.0f);
    tangent = xf * float4(tangent, 0.0f);
    geometryNormal = xf * float4(geometryNormal, 0.0f);
    geometryTangent = xf * float4(geometryTangent, 0.0f);
  }

public:
  uint32_t meshInstanceIndex{INVALID_INDEX}; ///< The mesh instance index.
  uint32_t meshIndex{INVALID_INDEX};         ///< The mesh index.
  uint32_t faceIndex{INVALID_INDEX};         ///< The face index.
  uint32_t materialIndex{INVALID_INDEX};     ///< The material index.
  const smdl::JIT::Material *material{};     ///< The material.
  float3 bary{};                             ///< The barycentric coordinate.
  float3 point{};                            ///< The point.
  float3 normal{};                           ///< The shading normal.
  float3 tangent{};                          ///< The shading tangent.
  float3 geometryNormal{};                   ///< The geometry normal.
  float3 geometryTangent{};                  ///< The geometry tangent.
  float2 texcoord{};                         ///< The texture coordinate.
  float4x4 objectToWorld{};                  ///< The object-to-world transform.
};

/// A mesh.
class Mesh final {
public:
  class Vert final {
  public:
    float3 point{};    ///< The point.
    float3 normal{};   ///< The normal direction.
    float3 tangent{};  ///< The tangent direction.
    float2 texcoord{}; ///< The texture coordinate.
  };

  using Face = std::array<uint32_t, 3>;

  RTCScene scene{};          ///< The Embree scene.
  std::vector<Vert> verts{}; ///< The verts.
  std::vector<Face> faces{}; ///< The faces.
  uint32_t materialIndex{};  ///< The index in the `Scene::materials` array.
};

/// A mesh instance.
class MeshInstance final {
public:
  float4x4 objectToWorld{}; ///< The object-to-world matrix.
  uint32_t meshIndex{};     ///< The index in the `Scene::meshes` array.
};

/// A scene.
class Scene final {
public:
  Scene(const smdl::Compiler &compiler, const std::string &fileName);
  Scene(const Scene &) = delete;
  ~Scene();

private:
  void load(const aiScene &assScene);
  void load(const aiMesh &assMesh);
  void load(const aiNode &assNode,
            aiMatrix4x4 xf = {1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1});

public:
  [[nodiscard]] bool intersect(Ray &ray, Hit &hit) const;

public:
  const smdl::Compiler &compiler;                       ///< The compiler.
  RTCDevice device{};                                   ///< The Embree device.
  RTCScene scene{};                                     ///< The Embree scene.
  float3 boundCenter{};                                 ///< The bound center.
  float boundRadius{};                                  ///< The bound radius.
  std::vector<std::unique_ptr<Mesh>> meshes{};          ///< The meshes.
  std::vector<MeshInstance> meshInstances{};            ///< The mesh instances.
  std::vector<const smdl::JIT::Material *> materials{}; ///< The materials.
};
