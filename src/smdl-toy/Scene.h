#pragma once

#include "Camera.h"
#include "Light.h"

#include "assimp/scene.h"

class Mesh final {
public:
  class Vert final {
  public:
    /// The point.
    smdl::float3 point{};

    /// The normal direction.
    smdl::float3 normal{};

    /// The tangent direction.
    smdl::float3 tangent{};

    /// The texture coordinate.
    smdl::float2 texcoord{};
  };

  using Face = std::array<uint32_t, 3>;

  /// The Embree scene.
  RTCScene scene{};

  /// The verts.
  std::vector<Vert> verts{};

  /// The faces.
  std::vector<Face> faces{};

  /// The material index in the `Scene::materials` array.
  uint32_t materialIndex{};
};

class MeshInstance final {
public:
  /// The transform matrix.
  smdl::float4x4 transform{};

  /// The transform inverse matrix.
  smdl::float4x4 transformInv{};

  /// The mesh index in the `Scene::meshes` array.
  uint32_t meshIndex{};
};

class Scene final {
public:
  Scene(const smdl::Compiler &compiler, const Camera &camera,
        const std::string &fileName);

  Scene(const Scene &) = delete;

  Scene(Scene &&) = delete;

  ~Scene();

private:
  void load(const aiScene &assScene);

  void load(const aiMesh &assMesh);

  void load(const aiNode &assNode,
            aiMatrix4x4 xf = {1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1});

public:
  [[nodiscard]] bool intersect(Ray &ray, Intersection &intersection) const;

  [[nodiscard]] size_t trace_path_from_camera(smdl::BumpPtrAllocator &allocator,
                                              const Random &random,
                                              const Color &wavelengthBase,
                                              smdl::float2 pixelCoord,
                                              size_t maxDepth,
                                              Vertex *path) const;

  [[nodiscard]] size_t trace_path_from_light(smdl::BumpPtrAllocator &allocator,
                                             const Random &random,
                                             const Color &wavelengthBase,
                                             size_t maxDepth,
                                             Vertex *path) const;

private:
  [[nodiscard]] size_t random_walk(smdl::BumpPtrAllocator &allocator,
                                   const Random &random,
                                   const Color &wavelengthBase, float dirPdf,
                                   size_t maxDepth, Vertex *path) const;

public:
  [[nodiscard]] bool test_visibility(smdl::BumpPtrAllocator &allocator,
                                     const Random &random,
                                     const Color &wavelengthBase,
                                     const Vertex &fromVertex,
                                     const Vertex &toVertex, Color &beta) const;

  [[nodiscard]] bool light_last_vertex_sample(smdl::BumpPtrAllocator &allocator,
                                              const Random &random,
                                              const Color &wavelengthBase,
                                              const Vertex &cameraVertex,
                                              Vertex &lightVertex) const;

public:
  /// The compiler.
  const smdl::Compiler &compiler;

  /// The camera.
  const Camera &camera;

  /// The Embree device.
  RTCDevice device{};

  /// The Embree scene.
  RTCScene scene{};

  /// The bounding sphere center.
  smdl::float3 boundCenter{};

  /// The bounding sphere radius.
  float boundRadius{};

  /// The meshes.
  std::vector<std::unique_ptr<Mesh>> meshes{};

  /// The mesh instances.
  std::vector<MeshInstance> meshInstances{};

  /// The materials.
  std::vector<const smdl::JIT::Material *> materials{};

  LightDistribution lights{};
};
