#include "Scene.h"

#include "assimp/Importer.hpp"
#include "assimp/postprocess.h"
#include <iostream>

Scene::Scene() : device(rtcNewDevice("verbose=0")), scene(rtcNewScene(device)) {}

Scene::~Scene() {
  for (auto &mesh : meshes)
    rtcReleaseScene(mesh->scene), mesh->scene = {};
  rtcReleaseScene(scene), scene = {};
  rtcReleaseDevice(device), device = {};
}

void Scene::load(const std::string &filename) {
  auto assImporter{Assimp::Importer{}};
  auto assScene{
      assImporter.ReadFile(filename.c_str(), aiProcessPreset_TargetRealtime_MaxQuality & ~aiProcess_RemoveRedundantMaterials)};
  if (!assScene)
    throw smdl::Error(std::format("assimp failed to read '{}'", filename));
  load(*assScene);
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
    mesh->faces[i] = {
        uint32_t(assMesh.mFaces[i].mIndices[0]), uint32_t(assMesh.mFaces[i].mIndices[1]),
        uint32_t(assMesh.mFaces[i].mIndices[2])};
  mesh->materialIndex = assMesh.mMaterialIndex;
  auto geometry{rtcNewGeometry(device, RTC_GEOMETRY_TYPE_TRIANGLE)};
  rtcSetGeometryBuildQuality(geometry, RTC_BUILD_QUALITY_HIGH);
  rtcSetGeometryTimeStepCount(geometry, 1);
  rtcSetSharedGeometryBuffer(
      geometry, RTC_BUFFER_TYPE_VERTEX, 0, RTC_FORMAT_FLOAT3, mesh->verts.data(), 0, sizeof(Mesh::Vert), mesh->verts.size());
  rtcSetSharedGeometryBuffer(
      geometry, RTC_BUFFER_TYPE_INDEX, 0, RTC_FORMAT_UINT3, mesh->faces.data(), 0, sizeof(Mesh::Face), mesh->faces.size());
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
    meshInstances[instID].transform = smdl::float4x4_t{
        smdl::float4_t{xf.a1, xf.b1, xf.c1, xf.d1}, //
        smdl::float4_t{xf.a2, xf.b2, xf.c2, xf.d2}, //
        smdl::float4_t{xf.a3, xf.b3, xf.c3, xf.d3}, //
        smdl::float4_t{xf.a4, xf.b4, xf.c4, xf.d4}};
    meshInstances[instID].meshIndex = meshIndex;
    rtcReleaseGeometry(inst);
  }
  for (unsigned int i = 0; i < assNode.mNumChildren; i++)
    load(*assNode.mChildren[i], xf);
}

bool Scene::intersect(Ray ray, Hit &hit) const {
  RTCRayHit rayhit{};
  rayhit.ray.org_x = ray.org.x, rayhit.ray.org_y = ray.org.y, rayhit.ray.org_z = ray.org.z;
  rayhit.ray.dir_x = ray.dir.x, rayhit.ray.dir_y = ray.dir.y, rayhit.ray.dir_z = ray.dir.z;
  rayhit.ray.tnear = ray.tmin, rayhit.ray.tfar = ray.tmax;
  rayhit.ray.time = 0;
  rayhit.ray.mask = unsigned(-1);
  rayhit.ray.id = 0;
  rayhit.ray.flags = 0;
  rayhit.hit.primID = unsigned(-1);
  rayhit.hit.geomID = unsigned(-1);
  rtcIntersect1(scene, &rayhit, nullptr);
  if (rayhit.hit.primID == unsigned(-1)) {
    return false;
  } else {
    const auto &meshInstance{meshInstances[rayhit.hit.instID[0]]};
    const auto &mesh{*meshes[meshInstance.meshIndex]};
    const auto &face{mesh.faces[rayhit.hit.primID]};
    const auto &vert0{mesh.verts[face[0]]};
    const auto &vert1{mesh.verts[face[1]]};
    const auto &vert2{mesh.verts[face[2]]};
    float u{std::max(0.0f, std::min(1.0f, rayhit.hit.u))};
    float v{std::max(0.0f, std::min(1.0f, rayhit.hit.v))};
    float w{std::max(0.0f, std::min(1.0f, 1.0f - u - v))};
    hit.meshInstanceIndex = rayhit.hit.instID[0];
    hit.meshIndex = meshInstance.meshIndex;
    hit.faceIndex = rayhit.hit.primID;
    hit.materialIndex = mesh.materialIndex;
    hit.bary = {w, u, v};
    hit.point = w * vert0.point + u * vert1.point + v * vert2.point;
    hit.normal = smdl::normalize(w * vert0.normal + u * vert1.normal + v * vert2.normal);
    hit.tangent = smdl::normalize(w * vert0.tangent + u * vert1.tangent + v * vert2.tangent);
    hit.geometryNormal = smdl::normalize(smdl::cross(vert1.point - vert0.point, vert2.point - vert0.point));
    hit.geometryTangent = smdl::perpendicular_to(hit.geometryNormal);
    hit.texcoord = w * vert0.texcoord + u * vert1.texcoord + v * vert2.texcoord;
    hit.transform(meshInstance.transform);
    return true;
  }
}

Color Scene::trace_path(
    std::span<const float> wavelengthBase, std::span<const smdl::jit::Material *> materials, RNG &rng, Ray ray) const {
  const smdl::float3_t lightDir = {1, -2, 2};
  Color L{};
  Color w{};
  for (size_t i = 0; i < WAVELENGTH_BASE_MAX; i++)
    w[i] = 1;
  for (int bounce = 0; bounce < 5; bounce++) {
    ray.dir = smdl::normalize(ray.dir);
    Hit hit{};
    if (!intersect(ray, hit))
      break;

    const auto &material{materials[hit.materialIndex]};
    smdl::float3_t wo{-ray.dir.x, -ray.dir.y, -ray.dir.z};
    smdl::state_t state{};
    state.wavelength_base = wavelengthBase.data();
    state.wavelength_min = WAVELENGTH_MIN;
    state.wavelength_max = WAVELENGTH_MAX;
    state.position = hit.point;
    state.normal = hit.normal;
    state.texture_coordinate[0] = {hit.texcoord.x, hit.texcoord.y, 0};
    state.texture_tangent_u[0] = hit.tangent;
    state.texture_tangent_v[0] = smdl::cross(state.normal, state.texture_tangent_u[0]);
    state.geometry_normal = hit.geometryNormal;
    state.geometry_tangent_u[0] = hit.geometryTangent;
    state.geometry_tangent_v[0] = smdl::cross(hit.geometryNormal, state.geometry_tangent_u[0]);
    state.object_id = hit.meshInstanceIndex;
    state.ptex_face_id = hit.faceIndex;
    state.ptex_face_uv = {hit.bary[1], hit.bary[2]};
    state.finalize_for_runtime_conventions();
    {
      Ray shadowRay{};
      Hit shadowHit{};
      shadowRay.org = hit.point;
      shadowRay.dir = smdl::normalize(lightDir);
      shadowRay.tmin = 0.0001f;
      shadowRay.tmax = std::numeric_limits<float>::infinity();
      if (!intersect(shadowRay, shadowHit)) {
        smdl::float_t pdf_fwd{};
        smdl::float_t pdf_rev{};
        alignas(64) Color f{};
        if (material->evalBsdf(state, wo, lightDir, pdf_fwd, pdf_rev, &f[0]))
          for (size_t i = 0; i < WAVELENGTH_BASE_MAX; i++)
            L[i] += w[i] * f[i];
      }
    }
    smdl::float3_t wi{};
    smdl::float4_t xi{
        std::generate_canonical<float, 32>(rng), std::generate_canonical<float, 32>(rng),
        std::generate_canonical<float, 32>(rng), std::generate_canonical<float, 32>(rng)};
    smdl::float_t pdf_fwd{};
    smdl::float_t pdf_rev{};
    alignas(64) Color f{};
    smdl::int_t is_delta{0};
    if (!material->evalBsdfSample(state, xi, wo, wi, pdf_fwd, pdf_rev, &f[0], is_delta) || pdf_fwd == 0)
      break;
    for (size_t i = 0; i < WAVELENGTH_BASE_MAX; i++) {
      w[i] *= f[i] / pdf_fwd;
      if (!std::isfinite(w[i]))
        w[i] = 0;
    }
    ray.org = hit.point;
    ray.dir = wi;
    ray.tmin = 0.0001f;
    ray.tmax = std::numeric_limits<float>::infinity();
  }
  return L;
}
