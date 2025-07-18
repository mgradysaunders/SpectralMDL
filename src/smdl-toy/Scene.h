#pragma once

#include "common.h"

#include "assimp/scene.h"

#include "Vertex.h"

class Camera final {
public:
  // TODO
};

#if 0
struct MI_RENDER_LINKAGE Perspective final {
public:
  void initializeFromFOVAndSize(double fov, Vector2d size) noexcept {
    imageA = 0.5 * +size;
    imageB = 0.5 * -size;
    focalLen = 0.5 * abs(size[1]) / tan(0.5 * fov);
  }

  void initializeFromFOVAndAspectRatio(double fov, double aspectRatio) noexcept { initializeFromFOVAndSize(fov, {1.0, 1.0 / aspectRatio}); }

  void takeSubsetOfImage(Vector2d fractionA, Vector2d fractionB) noexcept {
    fractionA = lerp(fractionA, imageA, imageB);
    fractionB = lerp(fractionB, imageA, imageB);
    imageA = fractionA;
    imageB = fractionB;
  }

  void flipImageX() noexcept { std::swap(imageA[0], imageB[0]); }

  void flipImageY() noexcept { std::swap(imageA[1], imageB[1]); }

  [[nodiscard]] double imageArea() const noexcept { return abs(imageB - imageA).product(); }

  [[nodiscard]] double solidAnglePDF(Vector3d omegaI) const noexcept {
    if (omegaI[2] < 0) {
      Vector3d pointP{
        focalLen * omegaI[0] / omegaI[2], //
        focalLen * omegaI[1] / omegaI[2], //
        focalLen};
      Vector3d pointQ{0, 0, 0};
      if (
        imageA[0] <= pointP[0] && pointP[0] <= imageB[0] && //
        imageA[1] <= pointP[1] && pointP[1] <= imageB[1])
        return 1 / imageArea() * convertAreaToSolidAngle(pointQ, pointP, Vector3d(0, 0, 1));
    }
    return 0;
  }

  [[nodiscard]] double solidAngleSample(Vector2d sampleU, Vector3d &omegaI) const noexcept {
    Vector3d pointP{
      lerp(sampleU[0], imageA[0], imageB[0]), //
      lerp(sampleU[1], imageA[1], imageB[1]), //
      focalLen};
    Vector3d pointQ{0, 0, 0};
    omegaI = normalize(pointQ - pointP);
    return 1 / imageArea() * convertAreaToSolidAngle(pointQ, pointP, Vector3d(0, 0, 1));
  }

  [[nodiscard]] Vector2d invert(Vector3d omegaI) const noexcept {
    Vector3d pointP{
      focalLen * omegaI[0] / omegaI[2], //
      focalLen * omegaI[1] / omegaI[2], //
      focalLen};
    return {unlerp(pointP[0], imageA[0], imageB[0]), unlerp(pointP[1], imageA[1], imageB[1])};
  }

public:
  Vector2d imageA{-1, -1};

  Vector2d imageB{+1, +1};

  double focalLen{1};
};
#endif

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
  Scene(const smdl::Compiler &compiler, const std::string &fileName);

  Scene(const Scene &) = delete;

  Scene(Scene &&) = delete;

  ~Scene();

private:
  void load(const aiScene &assScene);

  void load(const aiMesh &assMesh);

  void load(const aiNode &assNode,
            aiMatrix4x4 xf = {1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1});

  bool intersect(Ray ray, Intersection &intersection) const;

public:

  [[nodiscard]] int random_walk(const std::function<float()> &rng,
                                smdl::BumpPtrAllocator &allocator,
                                smdl::Span<float> wavelengthBase,
                                int maxPathLen, Vertex firstVertex,
                                Vertex *path) const;

  [[nodiscard]] bool test_visibility(const std::function<float()> &rng,
                                     smdl::BumpPtrAllocator &allocator,
                                     const Vertex &fromVertex,
                                     const Vertex &toVertex, Color &beta) const;

  // TODO sample_initial_vertex_on_camera()

  // TODO sample_initial_vertex_on_light()

  // TODO sample_final_vertex_on_camera()

  // TODO sample_final_vertex_on_light()

public:
  const smdl::Compiler &compiler;

  std::vector<const smdl::JIT::Material *> jitMaterials{};

  RTCDevice device{};

  RTCScene scene{};

  std::vector<std::unique_ptr<Mesh>> meshes{};

  std::vector<MeshInstance> meshInstances{};

  std::vector<std::string> materialNames{};

  std::unique_ptr<smdl::Image> imageLight{};

  float imageLightScale{1.0f};
};
