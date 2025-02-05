#pragma once

#include "common.h"

#include "assimp/scene.h"

class Mesh final {
public:
  class Vert final {
  public:
    smdl::float3 point{};

    smdl::float3 normal{};

    smdl::float3 tangent{};

    smdl::float2 texcoord{};
  };

  using Face = std::array<uint32_t, 3>;

  RTCScene scene{};

  std::vector<Vert> verts{};

  std::vector<Face> faces{};

  uint32_t materialIndex{};
};

class MeshInstance final {
public:
  smdl::float4x4 transform{};

  smdl::float4x4 transformInv{};

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

  void load(const aiNode &assNode,
            aiMatrix4x4 xf = {1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1});

public:
  bool intersect(Ray ray, Hit &hit) const;

  Color trace_path(smdl::BumpPtrAllocator &allocator,
                   smdl::Span<float> wavelengthBase,
                   smdl::Span<const smdl::JIT::Material *> jitMaterials,
                   RNG &rng, Ray ray) const;

public:
  RTCDevice device{};

  RTCScene scene{};

  std::vector<std::unique_ptr<Mesh>> meshes{};

  std::vector<MeshInstance> meshInstances{};

  std::vector<std::string> materialNames{};
};
