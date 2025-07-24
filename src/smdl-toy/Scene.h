#pragma once

#include "Vertex.h"

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
  [[nodiscard]]
  bool intersect(Ray &ray, Intersection &intersection) const;

  [[nodiscard]] int trace_path_from_camera(smdl::BumpPtrAllocator &allocator,
                                           const std::function<float()> &rng,
                                           const Color &wavelengthBase,
                                           smdl::float2 imageCoord,
                                           int maxDepth, Vertex *path) const {
    if (maxDepth > 0) {
      if (Camera_first_vertex_sample(camera, imageCoord, path[0]))
        return 1 + random_walk(allocator, rng, wavelengthBase, maxDepth - 1,
                               path + 1);
    }
    return 0;
  }

  [[nodiscard]] int trace_path_from_light(smdl::BumpPtrAllocator &allocator,
                                          const std::function<float()> &rng,
                                          const Color &wavelengthBase,
                                          int maxDepth, Vertex *path) const {
    if (maxDepth > 0) {
      auto xi0{smdl::float2(rng(), rng())};
      auto xi1{smdl::float2(rng(), rng())};
      auto &light{light_sample(xi1.y)};
      if (Light_first_vertex_sample(*this, light, xi0, xi1, path[0]))
        return 1 + random_walk(allocator, rng, wavelengthBase, maxDepth - 1,
                               path + 1);
    }
    return 0;
  }

  [[nodiscard]] bool test_visibility(const std::function<float()> &rng,
                                     smdl::BumpPtrAllocator &allocator,
                                     const Vertex &fromVertex,
                                     const Vertex &toVertex,
                                     Color &weight) const;

private:
  [[nodiscard]] int random_walk(smdl::BumpPtrAllocator &allocator,
                                const std::function<float()> &rng,
                                const Color &wavelengthBase, int maxDepth,
                                Vertex *path) const;

public:
  [[nodiscard]] const Light &light_sample(float &u) const noexcept {
    return lights[lightDistribution.index_sample(u).first];
  }

  [[nodiscard]] float light_probability(const Light &light) const noexcept {
    return lightDistribution.index_pmf(&light - &lights[0]);
  }

  [[nodiscard]]
  smdl::float3 infinite_disk_emission_point_sample(smdl::float3 omega,
                                                   smdl::float2 xi,
                                                   float *pdf) const noexcept {
    if (pdf)
      *pdf = 1.0f / (smdl::PI * boundRadius * boundRadius);
    return boundCenter - boundRadius * smdl::normalize(omega) +
           boundRadius * (smdl::coordinate_system(omega) *
                          smdl::float3(uniform_disk_sample(xi)));
  }

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

  /// The lights.
  std::vector<Light> lights{};

  /// The light distribution.
  smdl::DiscreteDistribution lightDistribution{};
};
