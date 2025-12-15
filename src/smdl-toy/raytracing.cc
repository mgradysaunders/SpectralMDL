#include "raytracing.h"

#include "assimp/Importer.hpp"
#include "assimp/postprocess.h"

Scene::Scene(const smdl::Compiler &compiler, const std::string &fileName)
    : compiler(compiler), device(rtcNewDevice("verbose=0")),
      scene(rtcNewScene(device)) {
  auto assImporter{Assimp::Importer{}};
  auto assScene{assImporter.ReadFile(fileName.c_str(),
                                     aiProcessPreset_TargetRealtime_MaxQuality &
                                         ~aiProcess_RemoveRedundantMaterials)};
  if (!assScene)
    throw smdl::Error(
        smdl::concat("assimp failed to read ", smdl::QuotedPath(fileName)));
  load(*assScene);
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
  RTCBounds bounds{};
  rtcGetSceneBounds(scene, &bounds);
  auto lower{float3(bounds.lower_x, bounds.lower_y, bounds.lower_z)};
  auto upper{float3(bounds.upper_x, bounds.upper_y, bounds.upper_z)};
  boundCenter = 0.5f * (lower + upper);
  boundRadius = 0.5f * length(upper - lower);
  for (unsigned int i = 0; i < assScene.mNumMaterials; i++) {
    auto name{assScene.mMaterials[i]->GetName()};
    auto material{compiler.findJitMaterial(name.C_Str())};
    if (!material)
      material = compiler.findJitMaterial("default_material");
    materials.push_back(material);
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
      vert.tangent = smdl::perpendicularTo(smdl::normalize(vert.normal));
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
    meshInstances[instID].objectToWorld =
        float4x4{float4{xf.a1, xf.b1, xf.c1, xf.d1}, //
                 float4{xf.a2, xf.b2, xf.c2, xf.d2}, //
                 float4{xf.a3, xf.b3, xf.c3, xf.d3}, //
                 float4{xf.a4, xf.b4, xf.c4, xf.d4}};
    meshInstances[instID].meshIndex = meshIndex;
    rtcReleaseGeometry(inst);
  }
  for (unsigned int i = 0; i < assNode.mNumChildren; i++)
    load(*assNode.mChildren[i], xf);
}

bool Scene::intersect(Ray &ray, Hit &hit) const {
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
    ray.tmax = rayHit.ray.tfar;
    const auto &meshInstance{meshInstances[rayHit.hit.instID[0]]};
    const auto &mesh{*meshes[meshInstance.meshIndex]};
    const auto &face{mesh.faces[rayHit.hit.primID]};
    const auto &vert0{mesh.verts[face[0]]};
    const auto &vert1{mesh.verts[face[1]]};
    const auto &vert2{mesh.verts[face[2]]};
    auto edge1{smdl::normalize(vert1.point - vert0.point)};
    auto edge2{smdl::normalize(vert2.point - vert0.point)};
    auto bary{float3(
        std::max(0.0f, std::min(1.0f, 1.0f - rayHit.hit.u - rayHit.hit.v)),
        std::max(0.0f, std::min(1.0f, rayHit.hit.u)),
        std::max(0.0f, std::min(1.0f, rayHit.hit.v)))};
    auto barycentric{[&](auto member) {
      return bary[0] * vert0.*member + //
             bary[1] * vert1.*member + //
             bary[2] * vert2.*member;
    }};
    SMDL_SANITY_CHECK(mesh.materialIndex < materials.size());
    hit.meshInstanceIndex = rayHit.hit.instID[0];
    hit.meshIndex = meshInstance.meshIndex;
    hit.faceIndex = rayHit.hit.primID;
    hit.materialIndex = mesh.materialIndex;
    hit.material = materials[hit.materialIndex];
    hit.bary = bary;
    hit.point = barycentric(&Mesh::Vert::point);
    hit.normal = normalize(barycentric(&Mesh::Vert::normal));
    hit.tangent = normalize(barycentric(&Mesh::Vert::tangent));
    hit.geometryNormal = normalize(cross(edge1, edge2));
    hit.geometryTangent = edge1;
    hit.texcoord = barycentric(&Mesh::Vert::texcoord);
    hit.objectToWorld = meshInstance.objectToWorld;
    return true;
  }
}
