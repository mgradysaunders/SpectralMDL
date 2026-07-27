#pragma once

#include "assimp/scene.h"
#include "embree4/rtcore_buffer.h"
#include "embree4/rtcore_common.h"
#include "embree4/rtcore_config.h"
#include "embree4/rtcore_device.h"
#include "embree4/rtcore_geometry.h"
#include "embree4/rtcore_ray.h"
#include "embree4/rtcore_scene.h"

#include "smdl/Compiler.h"
#include "smdl/Support/ColorVector.h"
#include "smdl/Support/MonteCarlo.h"

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

/// The power heuristic with \f$ \beta = 2 \f$ for two sampling strategies.
///
/// This is written as \f$ 1/(1+(q/p)^2) \f$ rather than the equivalent
/// \f$ p^2/(p^2+q^2) \f$ to avoid overflowing on the enormous PDFs that
/// near-specular lobes produce.
///
[[nodiscard]] inline float powerHeuristic(float pdf0, float pdf1) noexcept {
  if (!(pdf0 > 0)) return 0.0f;
  float ratio{pdf1 / pdf0};
  return 1.0f / (1.0f + ratio * ratio);
}

/// A hash-based Owen-scrambled Sobol sampler after Burley, "Practical
/// Hash-Based Owen Scrambling," JCGT 9(4) 2020.
///
/// Each (pixel, sample) pair yields a deterministic low-discrepancy point
/// sequence consumed two dimensions at a time. Every 2D pair reuses the
/// first two Sobol dimensions with an independently hashed index shuffle
/// and per-dimension Owen scramble, which keeps each pair's stratification
/// while decorrelating the pairs from one another, so the sequence
/// extends to arbitrarily many dimensions with no direction-number tables
/// beyond the second dimension's.
class Sampler final {
public:
  Sampler() = default;

  /// Begin the sample `sampleIndex` of the pixel `pixelIndex`, resetting
  /// the dimension counter.
  void startPixelSample(uint32_t pixelIndex, uint32_t sampleIndex) noexcept {
    pixelHash = hash(pixelIndex);
    this->sampleIndex = sampleIndex;
    dimension = 0;
  }

  [[nodiscard]] operator float() { return next(); }

  [[nodiscard]] operator float2() { return {next(), next()}; }

  [[nodiscard]] operator float3() { return {next(), next(), next()}; }

  [[nodiscard]] operator float4() { return {next(), next(), next(), next()}; }

  [[nodiscard]] int index(int n) {
    int i{int(std::floor(n * next()))};
    i = std::min(i, n - 1);
    i = std::max(i, 0);
    return i;
  }

private:
  /// The next canonical sample in `(0,1)`, advancing the dimension.
  [[nodiscard]] float next() noexcept {
    uint32_t pair{dimension >> 1};
    uint32_t component{dimension & 1};
    ++dimension;
    uint32_t seed{hash(pixelHash ^ (0x9E3779B9U * pair))};
    uint32_t shuffledIndex{nestedUniformScramble(sampleIndex, seed)};
    uint32_t X{component == 0 ? reverseBits(shuffledIndex)
                              : sobolDim1(shuffledIndex)};
    X = nestedUniformScramble(X, hash(seed ^ (0x55555555U + component)));
    float xi{float(X) * 0x1p-32f};
    xi = std::fmax(xi, std::numeric_limits<float>::denorm_min());      // > 0
    xi = std::fmin(xi, 1 - std::numeric_limits<float>::epsilon() / 2); // < 1
    return xi;
  }

  /// Murmur3-style finalizer.
  [[nodiscard]] static uint32_t hash(uint32_t x) noexcept {
    x ^= x >> 16;
    x *= 0x85EBCA6BU;
    x ^= x >> 13;
    x *= 0xC2B2AE35U;
    x ^= x >> 16;
    return x;
  }

  [[nodiscard]] static uint32_t reverseBits(uint32_t x) noexcept {
    x = (x << 16) | (x >> 16);
    x = ((x & 0x00FF00FFU) << 8) | ((x & 0xFF00FF00U) >> 8);
    x = ((x & 0x0F0F0F0FU) << 4) | ((x & 0xF0F0F0F0U) >> 4);
    x = ((x & 0x33333333U) << 2) | ((x & 0xCCCCCCCCU) >> 2);
    x = ((x & 0x55555555U) << 1) | ((x & 0xAAAAAAAAU) >> 1);
    return x;
  }

  /// The hash-based Owen scramble: reverse so the high (most significant)
  /// bits sit low, run the Laine-Karras permutation, which only lets each
  /// bit affect bits above it, and reverse back.
  [[nodiscard]] static uint32_t nestedUniformScramble(uint32_t x,
                                                      uint32_t seed) noexcept {
    x = reverseBits(x);
    x += seed;
    x ^= x * 0x6C50B47CU;
    x ^= x * 0xB82F1E52U;
    x ^= x * 0xC7AFE638U;
    x ^= x * 0x8D22F6E6U;
    return reverseBits(x);
  }

  /// The second Sobol dimension. (The first is just `reverseBits`.)
  [[nodiscard]] static uint32_t sobolDim1(uint32_t index) noexcept {
    static constexpr uint32_t directions[32] = {
        0x80000000U, 0xC0000000U, 0xA0000000U, 0xF0000000U, //
        0x88000000U, 0xCC000000U, 0xAA000000U, 0xFF000000U, //
        0x80800000U, 0xC0C00000U, 0xA0A00000U, 0xF0F00000U, //
        0x88880000U, 0xCCCC0000U, 0xAAAA0000U, 0xFFFF0000U, //
        0x80008000U, 0xC000C000U, 0xA000A000U, 0xF000F000U, //
        0x88008800U, 0xCC00CC00U, 0xAA00AA00U, 0xFF00FF00U, //
        0x80808080U, 0xC0C0C0C0U, 0xA0A0A0A0U, 0xF0F0F0F0U, //
        0x88888888U, 0xCCCCCCCCU, 0xAAAAAAAAU, 0xFFFFFFFFU};
    uint32_t X{};
    for (int bit = 0; index; index >>= 1, bit++)
      if (index & 1) X ^= directions[bit];
    return X;
  }

  uint32_t pixelHash{};
  uint32_t sampleIndex{};
  uint32_t dimension{};
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
    state.finalizeAndApplyInternalSpaceConventions();
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
