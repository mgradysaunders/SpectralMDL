#include "Scene.h"

#include "assimp/Importer.hpp"
#include "assimp/postprocess.h"

Scene::Scene() : device(rtcNewDevice("verbose=0")), scene(rtcNewScene(device)) {}

Scene::~Scene() {
  for (auto &mesh : meshes)
    rtcReleaseScene(mesh->scene), mesh->scene = {};
  rtcReleaseScene(scene), scene = {};
  rtcReleaseDevice(device), device = {};
}

void Scene::load(const std::string &filename) {
  auto assImporter{Assimp::Importer{}};
  auto assScene{assImporter.ReadFile(filename.c_str(), aiProcessPreset_TargetRealtime_MaxQuality)};
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
