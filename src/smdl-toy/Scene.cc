#include "Scene.h"

#include "assimp/Importer.hpp"
#include "assimp/postprocess.h"
#include <iostream>

#include "smdl/Support/Profiler.h"

#include "llvm/Support/JSON.h"

Scene::Scene(const smdl::Compiler &compiler, const Camera &camera,
             const std::string &fileName)
    : compiler(compiler), camera(camera), device(rtcNewDevice("verbose=0")),
      scene(rtcNewScene(device)) {
  auto assImporter{Assimp::Importer{}};
  auto assScene{assImporter.ReadFile(fileName.c_str(),
                                     aiProcessPreset_TargetRealtime_MaxQuality &
                                         ~aiProcess_RemoveRedundantMaterials)};
  if (!assScene)
    throw smdl::Error(
        smdl::concat("assimp failed to read ", smdl::quoted_path(fileName)));
  load(*assScene);
}

Scene::~Scene() {
  for (auto &mesh : meshes)
    rtcReleaseScene(mesh->scene), mesh->scene = {};
  rtcReleaseScene(scene), scene = {};
  rtcReleaseDevice(device), device = {};
}

void Scene::load(const aiScene &assScene) {
  SMDL_PROFILER_ENTRY("Scene::load()", "Scene");
  for (unsigned int i = 0; i < assScene.mNumMeshes; i++)
    load(*assScene.mMeshes[i]);
  load(*assScene.mRootNode);
  rtcCommitScene(scene);
  RTCBounds bounds{};
  rtcGetSceneBounds(scene, &bounds);
  boundCenter = {0.5f * (bounds.lower_x + bounds.upper_x),
                 0.5f * (bounds.lower_y + bounds.upper_y),
                 0.5f * (bounds.lower_z + bounds.upper_z)};
  boundRadius =
      smdl::length(smdl::float3(0.5f * (bounds.upper_x - bounds.lower_x),
                                0.5f * (bounds.upper_y - bounds.lower_y),
                                0.5f * (bounds.upper_z - bounds.lower_z)));
  std::cerr << smdl::concat("boundCenter = (", //
                            boundCenter.x, ", ", boundCenter.y, ", ",
                            boundCenter.z, ")\n", //
                            "boundRadius = ", boundRadius, "\n");
  for (unsigned int i = 0; i < assScene.mNumMaterials; i++) {
    auto name{assScene.mMaterials[i]->GetName()};
    auto material{compiler.find_jit_material(name.C_Str())};
    if (!material)
      material = compiler.find_jit_material("default_material");
    materials.push_back(material);
    std::cerr << "Material: " << name.C_Str() << '\n';
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

bool Scene::intersect(Ray &ray, Intersection &intersection) const {
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
    auto bary{smdl::float3(
        std::max(0.0f, std::min(1.0f, 1.0f - rayHit.hit.u - rayHit.hit.v)),
        std::max(0.0f, std::min(1.0f, rayHit.hit.u)),
        std::max(0.0f, std::min(1.0f, rayHit.hit.v)))};
    auto barycentric{[&](auto member) {
      return bary[0] * vert0.*member + //
             bary[1] * vert1.*member + //
             bary[2] * vert2.*member;
    }};
    intersection.meshInstanceIndex = rayHit.hit.instID[0];
    intersection.meshIndex = meshInstance.meshIndex;
    intersection.faceIndex = rayHit.hit.primID;
    intersection.materialIndex = mesh.materialIndex;
    intersection.bary = bary;
    intersection.point = barycentric(&Mesh::Vert::point);
    intersection.normal = smdl::normalize(barycentric(&Mesh::Vert::normal));
    intersection.tangent = smdl::normalize(barycentric(&Mesh::Vert::tangent));
    intersection.geometryNormal = smdl::normalize(smdl::cross(edge1, edge2));
    intersection.geometryTangent = edge1;
    intersection.texcoord = barycentric(&Mesh::Vert::texcoord);
    intersection.transform(meshInstance.transform);
    return true;
  }
}

size_t Scene::trace_path_from_camera(smdl::BumpPtrAllocator &allocator,
                                     const Random &random,
                                     const Color &wavelengthBase,
                                     smdl::float2 pixelCoord, size_t maxDepth,
                                     Vertex *path) const {
  if (maxDepth > 0) {
    float dirPdf{};
    if (camera.first_vertex_sample(pixelCoord, path[0], dirPdf)) {
      return 1 + random_walk(allocator, random, wavelengthBase, dirPdf,
                             maxDepth - 1, path + 1);
    }
  }
  return 0;
}

size_t Scene::trace_path_from_light(smdl::BumpPtrAllocator &allocator,
                                    const Random &random,
                                    const Color &wavelengthBase,
                                    size_t maxDepth, Vertex *path) const {
  if (maxDepth > 0) {
    float dirPdf{};
    const Light *light{lights.light_sample(random)};
    if (light && light->first_vertex_sample(*this, random, path[0], dirPdf)) {
      return 1 + random_walk(allocator, random, wavelengthBase, dirPdf,
                             maxDepth - 1, path + 1);
    }
  }
  return 0;
}

size_t Scene::random_walk(smdl::BumpPtrAllocator &allocator,
                          const Random &random, const Color &wavelengthBase,
                          float dirPdf, size_t maxDepth, Vertex *path) const {
  if (maxDepth == 0)
    return 0;

  Color beta{path[-1].beta};
  Color f{};

  // We declare the state here and set up the variables that never
  // change. The other state variables get updated at every vertex
  // on the path by `Intersection::initialize_state()`.
  smdl::State state{};
  state.allocator = &allocator;
  state.wavelength_base = wavelengthBase.data();
  state.wavelength_min = WAVELENGTH_MIN;
  state.wavelength_max = WAVELENGTH_MAX;

  auto ray{Ray{path[-1].point, path[-1].wNext, EPS, INF}};
  int depth{};
  while (depth < int(maxDepth)) {
    // Intersect the ray from the previous vertex with the scene.
    auto &prevVertex{path[depth - 1]};
    auto &vertex{path[depth]};
    vertex = Vertex{};
    vertex.prevVertex = &prevVertex;
    vertex.transportMode = prevVertex.transportMode;
    vertex.beta = beta;
    vertex.wPrev = -prevVertex.wNext;
    auto intersection{Intersection{}};
    bool intersectedSurface{intersect(ray, intersection)};
    if (!intersectedSurface) {
      ++depth;
      vertex.point = prevVertex.point + 2 * boundRadius * prevVertex.wNext;
      vertex.pdf = dirPdf;
      vertex.isAtInfinity = true;
      // TODO adjointPdfOmega
      /*
      prevVertex.adjointPdfPoint = vertex.convert_direction_pdf_to_point_pdf(
          vertex.adjointPdfOmega, prevVertex);
       */
      break;
    }

    ++depth;
    vertex.point = intersection.point;
    vertex.intersection = intersection;
    vertex.intersection->initialize_state(state);
    vertex.material = materials[intersection.materialIndex];
    vertex.material->allocate(state, vertex.materialInstance);
    vertex.intersection->normal =
        smdl::normalize(vertex.materialInstance.tangent_space *
                        (*vertex.materialInstance.normal));
    if (*vertex.materialInstance.cutout_opacity < 1.0f) {
      if (*vertex.materialInstance.cutout_opacity <= 0.0f ||
          !(random.generate_canonical() <
            *vertex.materialInstance.cutout_opacity)) {
        --depth;
        ray.tmin = smdl::increment_float(ray.tmax + EPS);
        ray.tmax = INF;
        continue;
      }
    }
    vertex.pdf = prevVertex.convert_direction_pdf_to_point_pdf(dirPdf, vertex);
    float dirPdfAdjoint{};
    int isDelta{};
    if (!vertex.scatter_sample(random.generate_canonical4(), vertex.wNext,
                               dirPdf, dirPdfAdjoint, f, isDelta)) {
      break;
    }
    // TODO Handle isDelta
    beta *= (1.0f / dirPdf) * f;
    beta.set_non_finite_to_zero();
    prevVertex.pdfAdjoint =
        vertex.convert_direction_pdf_to_point_pdf(dirPdfAdjoint, prevVertex);
    ray = Ray{vertex.point, vertex.wNext, EPS, INF};
  }
  return depth;
}

bool Scene::test_visibility(smdl::BumpPtrAllocator &allocator,
                            const Random &random, const Color &wavelengthBase,
                            const Vertex &fromVertex, const Vertex &toVertex,
                            Color &beta) const {
  smdl::State state{};
  state.allocator = &allocator;
  state.wavelength_base = wavelengthBase.data();
  state.wavelength_min = WAVELENGTH_MIN;
  state.wavelength_max = WAVELENGTH_MAX;
  Ray ray{fromVertex.point, toVertex.point - fromVertex.point, EPS, 1 - EPS};
  while (true) {
    if (Intersection intersection{}; intersect(ray, intersection)) {
      intersection.initialize_state(state);
      auto material = materials[intersection.materialIndex];
      auto materialInstance{smdl::JIT::Material::Instance{}};
      material->allocate(state, materialInstance);
      if (*materialInstance.cutout_opacity < 1.0f) {
        if (*materialInstance.cutout_opacity <= 0.0f ||
            !(random.generate_canonical() < *materialInstance.cutout_opacity)) {
          ray.tmin = smdl::increment_float(ray.tmax + EPS);
          ray.tmax = 1 - EPS;
          if (!(ray.tmin < ray.tmax))
            return true;
          continue;
        }
      }
      break;
    } else {
      return true;
    }
  }
  return false;
}

bool Scene::light_last_vertex_sample(smdl::BumpPtrAllocator &allocator,
                                     const Random &random,
                                     const Color &wavelengthBase,
                                     const Vertex &cameraVertex,
                                     Vertex &lightVertex) const {
  const Light *light{lights.light_sample(random)};
  return light &&
         light->last_vertex_sample(*this, random, cameraVertex, lightVertex) &&
         test_visibility(allocator, random, wavelengthBase, cameraVertex,
                         lightVertex, lightVertex.beta);
}
