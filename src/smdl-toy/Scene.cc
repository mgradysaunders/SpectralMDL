#include "Scene.h"

#include "assimp/Importer.hpp"
#include "assimp/postprocess.h"
#include <iostream>

Scene::Scene(const smdl::Compiler &compiler, const std::string &fileName)
    : compiler(compiler), device(rtcNewDevice("verbose=0")),
      scene(rtcNewScene(device)) {
  auto assImporter{Assimp::Importer{}};
  auto assScene{assImporter.ReadFile(fileName.c_str(),
                                     aiProcessPreset_TargetRealtime_MaxQuality &
                                         ~aiProcess_RemoveRedundantMaterials)};
  if (!assScene)
    throw smdl::Error(
        smdl::concat("assimp failed to read ", smdl::quoted_path(fileName)));
  load(*assScene);
  for (auto &materialName : materialNames)
    jitMaterials.push_back(compiler.find_jit_material(materialName));
}

Scene::~Scene() {
  for (auto &mesh : meshes)
    rtcReleaseScene(mesh->scene), mesh->scene = {};
  rtcReleaseScene(scene), scene = {};
  rtcReleaseDevice(device), device = {};
}

void Scene::load(const aiScene &assScene) {
  for (unsigned int i = 0; i < assScene.mNumMeshes; i++)
    load(*assScene.mMeshes[i]);
  load(*assScene.mRootNode);
  rtcCommitScene(scene);
  for (unsigned int i = 0; i < assScene.mNumMaterials; i++) {
    auto name{assScene.mMaterials[i]->GetName()};
    materialNames.push_back(name.C_Str());
    std::cerr << "Material: " << materialNames.back() << '\n';
  }
}

void Scene::load(const aiMesh &assMesh) {
  auto &mesh{meshes.emplace_back(new Mesh())};
  mesh->scene = rtcNewScene(device);
  rtcSetSceneFlags(mesh->scene, RTC_SCENE_FLAG_ROBUST);
  rtcSetSceneBuildQuality(mesh->scene, RTC_BUILD_QUALITY_HIGH);
  mesh->verts.resize(assMesh.mNumVertices);
  mesh->faces.resize(assMesh.mNumFaces);
  for (unsigned int i = 0; i < assMesh.mNumVertices; i++) {
    auto &vert = mesh->verts[i];
    vert.point.x = assMesh.mVertices[i].x;
    vert.point.y = assMesh.mVertices[i].y;
    vert.point.z = assMesh.mVertices[i].z;
    vert.normal.x = assMesh.mNormals[i].x;
    vert.normal.y = assMesh.mNormals[i].y;
    vert.normal.z = assMesh.mNormals[i].z;
    vert.normal = smdl::normalize(vert.normal);
    if (assMesh.mTangents) {
      vert.tangent.x = assMesh.mTangents[i].x;
      vert.tangent.y = assMesh.mTangents[i].y;
      vert.tangent.z = assMesh.mTangents[i].z;
    } else {
      vert.tangent = smdl::perpendicular_to(smdl::normalize(vert.normal));
    }
    if (assMesh.mTextureCoords[0]) {
      vert.texcoord.x = assMesh.mTextureCoords[0][i].x;
      vert.texcoord.y = assMesh.mTextureCoords[0][i].y;
    }
  }
  for (unsigned int i = 0; i < assMesh.mNumFaces; i++)
    mesh->faces[i] = {uint32_t(assMesh.mFaces[i].mIndices[0]),
                      uint32_t(assMesh.mFaces[i].mIndices[1]),
                      uint32_t(assMesh.mFaces[i].mIndices[2])};
  mesh->materialIndex = assMesh.mMaterialIndex;
  auto geometry{rtcNewGeometry(device, RTC_GEOMETRY_TYPE_TRIANGLE)};
  rtcSetGeometryBuildQuality(geometry, RTC_BUILD_QUALITY_HIGH);
  rtcSetGeometryTimeStepCount(geometry, 1);
  rtcSetSharedGeometryBuffer(geometry, RTC_BUFFER_TYPE_VERTEX, 0,
                             RTC_FORMAT_FLOAT3, mesh->verts.data(), 0,
                             sizeof(Mesh::Vert), mesh->verts.size());
  rtcSetSharedGeometryBuffer(geometry, RTC_BUFFER_TYPE_INDEX, 0,
                             RTC_FORMAT_UINT3, mesh->faces.data(), 0,
                             sizeof(Mesh::Face), mesh->faces.size());
  rtcCommitGeometry(geometry);
  rtcAttachGeometry(mesh->scene, geometry);
  rtcCommitScene(mesh->scene);
  rtcReleaseGeometry(geometry);
}

void Scene::load(const aiNode &assNode, aiMatrix4x4 xf) {
  xf = xf * assNode.mTransformation;
  for (unsigned int i = 0; i < assNode.mNumMeshes; i++) {
    auto meshIndex = assNode.mMeshes[i];
    auto inst{rtcNewGeometry(device, RTC_GEOMETRY_TYPE_INSTANCE)};
    rtcSetGeometryBuildQuality(inst, RTC_BUILD_QUALITY_HIGH);
    rtcSetGeometryTimeStepCount(inst, 1);
    rtcSetGeometryTransform(inst, 0, RTC_FORMAT_FLOAT3X4_ROW_MAJOR, &xf[0][0]);
    rtcSetGeometryInstancedScene(inst, meshes[meshIndex]->scene);
    rtcCommitGeometry(inst);
    auto instID = rtcAttachGeometry(scene, inst);
    if (meshInstances.size() <= instID)
      meshInstances.resize(1 + instID);
    meshInstances[instID].transform =
        smdl::float4x4{smdl::float4{xf.a1, xf.b1, xf.c1, xf.d1}, //
                       smdl::float4{xf.a2, xf.b2, xf.c2, xf.d2}, //
                       smdl::float4{xf.a3, xf.b3, xf.c3, xf.d3}, //
                       smdl::float4{xf.a4, xf.b4, xf.c4, xf.d4}};
    meshInstances[instID].meshIndex = meshIndex;
    rtcReleaseGeometry(inst);
  }
  for (unsigned int i = 0; i < assNode.mNumChildren; i++)
    load(*assNode.mChildren[i], xf);
}

bool Scene::intersect(Ray ray, Intersection &intersection) const {
  RTCRayHit rayHit{};
  rayHit.ray.org_x = ray.org.x;
  rayHit.ray.org_y = ray.org.y;
  rayHit.ray.org_z = ray.org.z;
  rayHit.ray.dir_x = ray.dir.x;
  rayHit.ray.dir_y = ray.dir.y;
  rayHit.ray.dir_z = ray.dir.z;
  rayHit.ray.tnear = ray.tmin;
  rayHit.ray.tfar = ray.tmax;
  rayHit.ray.time = 0;
  rayHit.ray.mask = unsigned(-1);
  rayHit.ray.id = 0;
  rayHit.ray.flags = 0;
  rayHit.hit.primID = unsigned(-1);
  rayHit.hit.geomID = unsigned(-1);
  rtcIntersect1(scene, &rayHit, nullptr);
  if (rayHit.hit.primID == unsigned(-1)) {
    return false;
  } else {
    const auto &meshInstance{meshInstances[rayHit.hit.instID[0]]};
    const auto &mesh{*meshes[meshInstance.meshIndex]};
    const auto &face{mesh.faces[rayHit.hit.primID]};
    const auto &vert0{mesh.verts[face[0]]};
    const auto &vert1{mesh.verts[face[1]]};
    const auto &vert2{mesh.verts[face[2]]};
    float baryU{std::max(0.0f, std::min(1.0f, rayHit.hit.u))};
    float baryV{std::max(0.0f, std::min(1.0f, rayHit.hit.v))};
    float baryW{std::max(0.0f, std::min(1.0f, 1.0f - baryU - baryV))};
    intersection.meshInstanceIndex = rayHit.hit.instID[0];
    intersection.meshIndex = meshInstance.meshIndex;
    intersection.faceIndex = rayHit.hit.primID;
    intersection.materialIndex = mesh.materialIndex;
    intersection.bary = {baryW, baryU, baryV};
    intersection.point =
        baryW * vert0.point + baryU * vert1.point + baryV * vert2.point;
    intersection.normal = smdl::normalize(
        baryW * vert0.normal + baryU * vert1.normal + baryV * vert2.normal);
    intersection.tangent = smdl::normalize(
        baryW * vert0.tangent + baryU * vert1.tangent + baryV * vert2.tangent);
    intersection.geometryNormal = smdl::normalize(
        smdl::cross(vert1.point - vert0.point, vert2.point - vert0.point));
    intersection.geometryTangent =
        smdl::perpendicular_to(intersection.geometryNormal);
    intersection.texcoord = baryW * vert0.texcoord + baryU * vert1.texcoord +
                            baryV * vert2.texcoord;
    intersection.transform(meshInstance.transform);
    return true;
  }
}

#if 0
Color Scene::trace_path(smdl::BumpPtrAllocator &allocator,
                        smdl::Span<float> wavelengthBase, RNG &rng,
                        Ray ray) const {
  const smdl::float3 lightDir = {1, -2, 2};
  Color L{};
  Color w{};
  for (size_t i = 0; i < WAVELENGTH_BASE_MAX; i++)
    w[i] = 1;
  for (int bounce = 0; bounce < 5; bounce++) {
    ray.dir = smdl::normalize(ray.dir);
    Hit hit{};
    if (!intersect(ray, hit)) {
      if (imageLight) {
        float theta = std::acos(ray.dir.z);
        float phi = std::atan2(ray.dir.y, ray.dir.x);
        if (phi < 0)
          phi += 2 * 3.1415965359f;
        const int numTexelsX = imageLight->get_num_texels_x();
        const int numTexelsY = imageLight->get_num_texels_y();
        float u = std::max(0.0f, std::min(phi / (2 * 3.1415965359f), 1.0f));
        float v = std::max(0.0f, std::min(theta / 3.1415965359f, 1.0f));
        auto texel =
            imageLight->fetch(std::min(int(u * numTexelsX), numTexelsX - 1),
                              std::min(int(v * numTexelsY), numTexelsY - 1));
        switch (imageLight->get_num_channels()) {
        case 1:
        case 2: {
          for (size_t i = 0; i < WAVELENGTH_BASE_MAX; i++)
            L[i] += imageLightScale * w[i] * texel[0];
          break;
        }
        case 4: {
          Color Le{};
          smdl::State state{};
          state.wavelength_base = wavelengthBase.data();
          state.wavelength_min = WAVELENGTH_MIN;
          state.wavelength_max = WAVELENGTH_MAX;
          compiler.jit_rgb_to_color(state, smdl::float3(texel), &Le[0]);
          for (size_t i = 0; i < WAVELENGTH_BASE_MAX; i++)
            L[i] += imageLightScale * w[i] * Le[0];
          break;
        }
        default:
          SMDL_SANITY_CHECK(false, "Unexpected num channels!");
          break;
        }
      }
      break;
    }
    smdl::float3 wo{-ray.dir.x, -ray.dir.y, -ray.dir.z};
    auto jitMaterial{jitMaterials[hit.materialIndex]};
    auto jitMaterialInstance{smdl::JIT::Material::Instance{}};
    auto invTbn{smdl::transpose(jitMaterialInstance.tangent_space)};
    if ((jitMaterialInstance.flags &
         (smdl::JIT::Material::HAS_SURFACE_EMISSION |
          smdl::JIT::Material::HAS_BACKFACE_EMISSION)) != 0) {
      float pdf{};
      Color Le{};
      if (jitMaterial->emission_evaluate(jitMaterialInstance, invTbn * wo, pdf,
                                         &Le[0]))
        for (size_t i = 0; i < WAVELENGTH_BASE_MAX; i++)
          L[i] += w[i] * Le[i];
    }

  }
  return L;
}
#endif

int Scene::random_walk(const std::function<float()> &rng,
                       smdl::BumpPtrAllocator &allocator,
                       smdl::Span<float> wavelengthBase, int maxPathLen,
                       Vertex firstVertex, Vertex *path) const {
  if (maxPathLen <= 0)
    return 0;
  Color beta{};
  for (size_t i = 0; i < WAVELENGTH_BASE_MAX; i++)
    beta[i] = 1.0f;
  smdl::State state{};
  state.allocator = &allocator;
  state.wavelength_base = wavelengthBase.data();
  state.wavelength_min = WAVELENGTH_MIN;
  state.wavelength_max = WAVELENGTH_MAX;
  Ray ray = {firstVertex.position, firstVertex.omegaI, Eps, Inf};
  path[0] = std::move(firstVertex);
  path[0].beta = beta;
  int pathLen{1};
  for (; pathLen < maxPathLen; pathLen++) {
    ray.dir = smdl::normalize(ray.dir);
    Vertex &lastVertex{path[pathLen - 1]};
    Vertex &vertex{path[pathLen]};
    vertex = {}; // Reset
    vertex.flags.isOnLightSubpath = lastVertex.flags.isOnLightSubpath;
    vertex.beta = beta;
    vertex.omegaO = -ray.dir;
    if (!intersect(ray, vertex.intersection)) {
      vertex.position = lastVertex.position + ray.dir;
      vertex.flags.isTerminal = true;
      vertex.flags.isTerminalAtInfinity = true;
      break;
    }

#if 0
      // Account for volume scattering by sampling an intercept in the current medium, between the
      // current vertex position and the hit surface, or simply at any position if we did not
      // intersect a surface.
      if (auto medEvent{medium.transmission_sample(rng, ray, L)}; medEvent && medEvent->tHit < ray.tmax) {
        hit = hitMedium = true;
        v.p = ray(medEvent->tHit);
        v.manifold = {};            // Nullify
        v.materialInitializer = {}; // Nullify
        v.material.medium = MediumOrMediumTransition(std::move(medium));
        v.material.bsdf = std::move(medEvent->bsdf);
        v.info.isKnownOpaque = false;
        must_succeed(v.validate_initial_volume_vertex()); // Sanity checks.
      }

      // Intersected surface specifically?
      if (hit && !hitMedium) {
        v.material_initializer(waveLens);
        // Hit medium boundary? If so, pass through and skip to the next iteration.
        if (!v.material.has_BSDF()) {
          ray.org = v.p;
          ray.tmin = shadowEps;
          ray.tmax = K_inf<float>;
          medium = std::move(v.material.medium(ray.dir));
          depth--; // Also do not count this iteration as a bounce!
          continue;
        }
      }
#endif

    vertex.intersection.initialize_state(state);
    vertex.flags.isSurfaceScattering = true;
    vertex.position = vertex.intersection.point;
    vertex.material = jitMaterials[vertex.intersection.materialIndex];
    vertex.material->allocate(state, vertex.materialInstance);

    int isDelta{};
    if (vertex.scatter_sample(smdl::float4{rng(), rng(), rng(), rng()}, //
                              vertex.omegaO, vertex.omegaI,             //
                              vertex.directionSolidAnglePDF.forward,
                              vertex.directionSolidAnglePDF.reverse, beta,
                              isDelta)) {
      if (isDelta) {
        vertex.flags.isSurfaceScatteringDeltaDirection = true;
        vertex.directionSolidAnglePDF.forward = 1.0f; // TODO Ok?
        vertex.directionSolidAnglePDF.reverse = 1.0f; // TODO Ok?
      }
      ray = Ray{vertex.position, vertex.omegaI, Eps, Inf};
    } else {
      break;
    }
  }
  for (int i = 0; i + 1 < pathLen; i++) {
    path[i + 1].calculate_path_PDF(METHOD_FORWARD, path[i + 0]);
    path[i + 0].calculate_path_PDF(METHOD_REVERSE, path[i + 1]);
  }
  return pathLen;
}

bool Scene::test_visibility(const std::function<float()> &rng,
                            smdl::BumpPtrAllocator &allocator,
                            const Vertex &fromVertex, const Vertex &toVertex,
                            Color &beta) const {
  // TODO
  return true;
}

#if 0
bool Scene::visibility(const Spectrum &waveLens, RNG &rng, Vertex v0, vec3 wI, float d, Spectrum &L) const {
  if (d *= 1 - shadowEps; !(d > shadowEps)) return true;
  auto ray{Ray3f{v0.p, normalize(wI), shadowEps, d}};
  Vertex vLast{std::move(v0)};
  Vertex v{};
  while (true) {
    // First use the surface intersection routine to find the nearest surface vertex. If the surface has
    // no scattering functions and only serves to separate participating media, then we must iterate past
    // it and account for the intermediate transmission term. If the surface is otherwise opaque, then we
    // return false to indicate no visibility.
    bool hit{false};
    if (auto tHit{intersect(ray, v)}) {
      ray.tmax = *tHit;
      hit = true;
      must_succeed(v.validate_initial_surface_vertex());
      // Check the opaque flag first. If true, we already know that this vertex blocks visibility and
      // we do not have to construct the material. Otherwise, initialize the material from the provider and test
      // if it is opaque.
      if (v.info.isKnownOpaque) return false;
      if (v.material_initializer(waveLens); v.material.is_on_opaque_surface()) return false;
    }

    // Account for medium transmission.
    vLast.material.medium(ray.dir).transmission(rng, ray, L);
    // If the transmission collapses to zero (or potentially explodes, but hopefully not), then
    // return false to indicate no visibility.
    if (!is_positive_and_finite(L.span(), /*eps=*/1e-8f)) [[unlikely]]
      return false;

    // If we did not intersect something, then we are done. We have performed intersection tests until failure,
    // so we know the ray parameter range is exhausted, and we know that we have accounted for all transmission.
    if (!hit) break;

    // Scoot the ray parameters. In the extremely rare case that the shadow epsilon pushes the minimum past the
    // maximum, then we will assume that no surface intersection would be detected within that epsilon distance
    // and thus return true.
    ray.tmin = ray.tmax + shadowEps;
    ray.tmax = d;
    if (ray.tmin >= ray.tmax) [[unlikely]]
      break;

    // Prepare for the next iteration of the loop.
    vLast = std::move(v);
    v.clear();
  }
  return true;
}
#endif
