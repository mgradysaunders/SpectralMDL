#pragma once

#include "common.h"

#include "assimp/scene.h"

class Mesh final {
public:
  class Vert final {
  public:
    smdl::float3_t point{};

    smdl::float3_t normal{};

    smdl::float3_t tangent{};

    smdl::float2_t texcoord{};
  };

  using Face = std::array<uint32_t, 3>;

  RTCScene scene{};

  std::vector<Vert> verts{};

  std::vector<Face> faces{};

  uint32_t materialIndex{};
};

class MeshInstance final {
public:
  smdl::float4x4_t transform{};

  smdl::float4x4_t transformInv{};

  uint32_t meshIndex{};
};

class Scene final {
public:
  Scene();

  Scene(const Scene &) = delete;

  Scene(Scene &&) = delete;

  ~Scene();

  void load(const std::string &filename);

private:
  void load(const aiScene &assScene);

  void load(const aiMesh &assMesh);

  void load(const aiNode &assNode, aiMatrix4x4 xf = {1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1});

public:
  bool intersect(Ray ray, Hit &hit) const;

  Color trace_path(
      std::span<const float> wavelengthBase, std::span<const smdl::jit::Material *> materials, RNG &rng, Ray ray) const;

public:
  RTCDevice device{};

  RTCScene scene{};

  std::vector<std::unique_ptr<Mesh>> meshes{};

  std::vector<MeshInstance> meshInstances{};

  std::vector<std::string> materialNames{};
};
