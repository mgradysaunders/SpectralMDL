#include "Scene/Scene.h"

#include "IO/MeshImport.h"
#include "Scene/Subdivide.h"

#include "assimp/Importer.hpp"
#include "assimp/postprocess.h"
#include "assimp/scene.h"

#include "embree4/rtcore_ray.h"

#include "smdl/Support/Logger.h"
#include "smdl/Support/Parallel.h"
#include "smdl/Support/Profiler.h"

#include <algorithm>
#include <atomic>
#include <chrono>
#include <filesystem>
#include <optional>
#include <unordered_map>

namespace {

// The post-processing for a full load.
//
// Spelled out rather than taken from
// `aiProcessPreset_TargetRealtime_MaxQuality`, which additionally enables
// four steps that are wrong here:
//
// - `aiProcess_RemoveRedundantMaterials` merges materials and renumbers
//   `mMaterialIndex`, discarding the names this renderer keys on.
// - `aiProcess_LimitBoneWeights` is pointless once bone weights are gone.
// - `aiProcess_SplitLargeMeshes` shatters one authored object into
//   several mesh instances and so into several `State::object_id`
//   values, so a material that varies by object ID would vary within
//   the object.
// - `aiProcess_OptimizeMeshes` merges meshes that share a material, which
//   folds separately authored emissive objects into one area light.
//
// `aiProcess_FindInstances` IS enabled: it replaces duplicated mesh data
// with shared meshes under distinct node transforms while leaving one
// instance, and so one `State::object_id`, per placement, which is what
// distinguishes it from `aiProcess_OptimizeMeshes`.
constexpr unsigned POSTPROCESS_FLAGS =
    aiProcess_Triangulate | aiProcess_RemoveComponent | aiProcess_SortByPType |
    aiProcess_GenSmoothNormals | aiProcess_CalcTangentSpace |
    aiProcess_GenUVCoords | aiProcess_JoinIdenticalVertices |
    aiProcess_ImproveCacheLocality | aiProcess_FindDegenerates |
    aiProcess_FindInvalidData | aiProcess_FindInstances |
    aiProcess_ValidateDataStructure;

// The post-processing for a subdivided load, which must preserve the
// authored polygons: Catmull-Clark on a triangulated mesh is a different,
// worse surface, so `aiProcess_Triangulate` is out and triangulation
// happens after refinement instead. Normal generation and tangent
// computation are out with it, both because assimp's implementations
// expect triangles and because the refined (and possibly displaced)
// surface recomputes both from scratch anyway; so is
// `aiProcess_ImproveCacheLocality`, which reorders triangle indices only.
// `aiProcess_JoinIdenticalVertices` stays, and welds tighter here than in
// the triangle path: with no normals loaded, vertices weld on position
// and UV alone, which is exactly the topology subdivision wants to see.
constexpr unsigned SUBDIV_POSTPROCESS_FLAGS =
    aiProcess_RemoveComponent | aiProcess_SortByPType | aiProcess_GenUVCoords |
    aiProcess_JoinIdenticalVertices | aiProcess_FindDegenerates |
    aiProcess_FindInvalidData | aiProcess_FindInstances |
    aiProcess_ValidateDataStructure;

// The post-processing for a Loop-subdivided load, which has the opposite
// requirement: Loop refines triangles into triangles, and OpenSubdiv
// rejects any other face outright, so `aiProcess_Triangulate` comes back.
// Letting assimp do it beats fanning the base polygons ourselves, which
// fans a concave polygon outside itself.
constexpr unsigned LOOP_SUBDIV_POSTPROCESS_FLAGS =
    SUBDIV_POSTPROCESS_FLAGS | aiProcess_Triangulate;

} // namespace

void registerSceneData(smdl::Compiler &compiler) {
  compiler.sceneData.set(
      "vertex_color",
      [&compiler](smdl::State *state, smdl::SceneData::Kind kind, int size,
                  void *out) {
        if (state->vertex_color_max == 0) return;
        const auto &rgba{state->vertex_color[0]};
        if (kind == smdl::SceneData::Kind::Float && (size == 3 || size == 4)) {
          for (int i = 0; i < size; i++) static_cast<float *>(out)[i] = rgba[i];
        } else if (kind == smdl::SceneData::Kind::Color) {
          compiler.convertRGBToColor(*state, float3(rgba),
                                     static_cast<float *>(out));
        }
      },
      [](const smdl::State *state) { return state->vertex_color_max > 0; });
}

void MeshInstance::setObjectToWorld(const float4x4 &xf,
                                    std::string_view fileName) {
  // Kept whole, shear and all. See `MeshInstance` for why that is allowed
  // even though MDL prohibits shear between coordinate spaces, and for
  // where the deformation is confined to.
  objectToWorld = xf;
  const auto axis0{float3(xf[0])};
  const auto axis1{float3(xf[1])};
  const auto axis2{float3(xf[2])};
  // The cofactor matrix, whose columns are the cross products of the other
  // two axes. This is `det(A) A^-T` without ever forming either factor,
  // which is both cheaper and better behaved: it stays finite as the
  // transform approaches singular, where the inverse does not.
  normalMatrix =
      float3x3(cross(axis1, axis2), cross(axis2, axis0), cross(axis0, axis1));
  const float determinant{dot(axis0, normalMatrix[0])};
  flipsWinding = determinant < 0;
  // The rigid frame, computed with the same helper the library uses, from
  // the same matrix the library is handed. See `rigidToWorld`.
  const auto rigidAxes{smdl::orthonormalize(float3x3(axis0, axis1, axis2))};
  rigidToWorld =
      float4x4{float4(rigidAxes[0], 0.0f), float4(rigidAxes[1], 0.0f),
               float4(rigidAxes[2], 0.0f), xf[3]};
  // Rigid, so the transpose-and-negate inverse is exact -- the same
  // reasoning by which `state.smdl`'s `affine_inverse()` is allowed to
  // invert `object_to_world_matrix` that way.
  worldToRigid = smdl::affineInverse(rigidToWorld);
  // Is anything actually deformed? Measure against the nearest
  // similarity, which is the same test as asking whether the axes are
  // mutually perpendicular and equally long.
  const float scale{(length(axis0) + length(axis1) + length(axis2)) / 3.0f};
  float residual{};
  for (size_t j = 0; j < 3; j++)
    for (size_t i = 0; i < 3; i++)
      residual = std::max(
          residual, std::fabs(scale * rigidAxes[j][i] - float3(xf[j])[i]));
  isDeformed = residual > 1e-4f * std::max(scale, 1.0f);
  // A transform that collapses the object into a plane or a line has no
  // interior to shade and no well defined normal anywhere on it, and Embree
  // cannot invert it either. Nothing sensible is renderable, so say so
  // rather than emit whatever the arithmetic happens to produce.
  if (!(std::fabs(determinant) > 1e-9f * std::max(scale * scale * scale, 1.0f)))
    SMDL_LOG_WARN("Instance transform in ", smdl::QuotedPath(fileName),
                  " is degenerate: it collapses the object onto a plane or a "
                  "line, which has no volume to intersect and no surface "
                  "normal to shade.");
}

Scene::~Scene() {
  for (auto &mesh : meshes) rtcReleaseScene(mesh->scene), mesh->scene = {};
  for (auto &primitive : primitives)
    rtcReleaseScene(primitive->scene), primitive->scene = {};
  for (auto &groom : curves) rtcReleaseScene(groom->scene), groom->scene = {};
  rtcReleaseScene(scene), scene = {};
  rtcReleaseDevice(device), device = {};
}

void Scene::add(const std::string &fileName, const float4x4 &objectToWorld,
                const ObjectSelection &selection, const SubdivSpec &subdiv,
                const MaterialAssignment &materials) {
  addMesh(fileName, smdl::Span<const float4x4>(&objectToWorld, 1), selection,
          subdiv, materials);
}

void Scene::addMesh(const std::string &fileName,
                    smdl::Span<const float4x4> worldXfs,
                    const ObjectSelection &selection, const SubdivSpec &subdiv,
                    const MaterialAssignment &materials) {
  // Layout files place the same file more than once as a matter of
  // course, so a file is parsed, copied and BVH-built exactly once. What is
  // cached is only where the file's node graph put its meshes, with the
  // file transform factored out; placing it again is then a handful of
  // Embree instances over mesh data that already exists. The selection is
  // applied below rather than here, so that the same file selected several
  // ways still hits this cache. The subdivision spec and the material
  // assignment, by contrast, join the key: both change the mesh data
  // itself, so two different specs are two different sets of meshes.
  //
  // The assignment splits first. The `all`/`bySlot` half renames the
  // file's own slots and so decides what the meshes bind; the `renames`
  // half acts on the result, and when it does not feed displacement it
  // is an instance-level fact: it changes what an instance shades with
  // and nothing about the mesh. Keeping it out of the cache key is what
  // lets N overridden placements share one mesh and one BVH
  // (`MeshInstance::matIndex`). Displacement is the exception: the
  // renamed material's `geometry.displacement` bakes into the vertices
  // at commit, so under `subdiv.displace` the renames stay in the key
  // and the meshes genuinely differ.
  auto meshLevel{materials};
  const bool renamesPerInstance{!subdiv.displace && !meshLevel.renames.empty()};
  if (renamesPerInstance) meshLevel.renames.clear();
  std::error_code ignored{};
  auto key{std::filesystem::weakly_canonical(fileName, ignored).string()};
  if (key.empty()) key = fileName;
  if (subdiv.active()) key += "|" + subdiv.key();
  if (!meshLevel.empty()) key += "|" + meshLevel.key();
  if (subdiv.levels >= 5)
    SMDL_LOG_WARN("'subdivide ", subdiv.levels, "' in ",
                  smdl::QuotedPath(fileName), " multiplies the face count by ",
                  (uint64_t(1) << (2 * subdiv.levels)),
                  "; expect memory and build time to match.");
  auto entry{importCache.find(key)};
  if (entry == importCache.end()) {
    auto assImporter{Assimp::Importer{}};
    configureImporter(assImporter);
    auto assScene{assImporter.ReadFile(
        fileName.c_str(), subdiv.levels == 0 ? POSTPROCESS_FLAGS
                          : subdiv.scheme == SubdivSpec::Scheme::LOOP
                              ? LOOP_SUBDIV_POSTPROCESS_FLAGS
                              : SUBDIV_POSTPROCESS_FLAGS)};
    if (!assScene)
      throw smdl::Error(smdl::concat("assimp failed to read ",
                                     smdl::QuotedPath(fileName), ": ",
                                     assImporter.GetErrorString()));
    const auto meshBase{meshes.size()};
    auto file{load(*assScene, subdiv, meshLevel)};
    // A subdivided mesh holds base polygons rather than triangles at this
    // point; count whichever the mesh has, and say which it was.
    uint64_t numFaces{};
    for (size_t i = meshBase; i < meshes.size(); i++)
      numFaces += meshes[i]->faces.size() + meshes[i]->baseFaceCounts.size();
    SMDL_LOG_DEBUG("Read ", smdl::QuotedPath(fileName), ": ",
                   meshes.size() - meshBase, " meshes, ",
                   file.placements.size(), " placement(s), ", numFaces,
                   subdiv.levels > 0 ? " base polygons" : " triangles");
    entry = importCache.emplace(std::move(key), std::move(file)).first;
  } else {
    SMDL_LOG_DEBUG("Reusing ", smdl::QuotedPath(fileName), ": ",
                   entry->second.placements.size(), " placement(s)");
  }
  const auto &file{entry->second};
  const auto selectedRoot{resolveSelection(file.nodes, selection, fileName)};
  fileNames.push_back(fileName);
  // The instance-level binding: what the renames turn each mesh's own
  // material into, interned on first use and memoized per mesh, or
  // `INVALID_INDEX` where the renames have nothing to say.
  auto overrideForMesh{std::map<uint32_t, uint32_t>()};
  auto instanceMaterial{[&](uint32_t meshIndex) {
    if (!renamesPerInstance) return INVALID_INDEX;
    auto [entry2, isNew]{overrideForMesh.try_emplace(meshIndex, INVALID_INDEX)};
    if (isNew) {
      const auto &baseName{materialNames[meshes[meshIndex]->matIndex]};
      if (auto itr{materials.renames.find(baseName)};
          itr != materials.renames.end() && itr->second != baseName)
        entry2->second = internMaterial(itr->second);
    }
    return entry2->second;
  }};
  uint32_t numInstances{};
  uint32_t numSkippedOnRoot{};
  for (const auto &placement : file.placements) {
    const auto root{selectedRoot[placement.nodeIndex]};
    if (root == INVALID_INDEX) {
      // Geometry hanging directly off the root has no name to select it by,
      // so a selection can only ever drop it. Say so rather than quietly
      // leaving a hole in the scene.
      if (file.nodes[placement.nodeIndex].path.empty()) numSkippedOnRoot++;
      continue;
    }
    auto nodeXf{file.nodes[placement.nodeIndex].nodeToFile};
    if (selection.recenter) {
      // Left-multiplying by the inverse translation of the subtree root
      // subtracts it from the translation column and leaves the linear part
      // alone, which is exactly what recentering means. Every placement in
      // the subtree shifts by the same amount, so the subtree stays rigid.
      const auto origin{float3(file.nodes[root].nodeToFile[3])};
      nodeXf[3] = float4(float3(nodeXf[3]) - origin, nodeXf[3].w);
    }
    if (worldXfs.size() == 1) {
      addInstance(placement.meshIndex, INVALID_INDEX, INVALID_INDEX,
                  worldXfs[0] * nodeXf, fileName,
                  instanceMaterial(placement.meshIndex));
      numInstances++;
    } else {
      addInstanceArray(placement.meshIndex, INVALID_INDEX, INVALID_INDEX,
                       worldXfs, nodeXf, fileName,
                       instanceMaterial(placement.meshIndex));
      numInstances += uint32_t(worldXfs.size());
    }
  }
  if (numSkippedOnRoot > 0)
    SMDL_LOG_WARN("Selection in ", smdl::QuotedPath(fileName), " skipped ",
                  numSkippedOnRoot,
                  " mesh(es) that sit directly on the file's root node, which "
                  "has no name to select it by.");
  if (!selection.patterns.empty())
    SMDL_LOG_DEBUG("Selected ", numInstances, " instance(s) from ",
                   smdl::QuotedPath(fileName));
}

void Scene::add(const LayoutItem &item) {
  const auto worldXfs{item.batchXfs.empty()
                          ? smdl::Span<const float4x4>(&item.objectToWorld, 1)
                          : smdl::Span<const float4x4>(item.batchXfs.data(),
                                                       item.batchXfs.size())};
  const auto firstInstance{meshInstances.size()};
  if (item.primitive.active()) {
    addPrimitive(item.primitive, worldXfs, item.materials);
  } else if (item.curves.active) {
    addCurves(item.fileName, worldXfs, item.curves, item.materials);
  } else {
    addMesh(item.fileName, worldXfs, item.selection, item.subdiv,
            item.materials);
  }
  // Every instance the item produced carries its mark; the lowering has
  // already refused it on a groom.
  if (item.caster && !item.curves.active)
    for (size_t i = firstInstance; i < meshInstances.size(); i++)
      meshInstances[i].causticCaster = true;
  if (item.causticLight && !item.curves.active)
    for (size_t i = firstInstance; i < meshInstances.size(); i++)
      meshInstances[i].causticLight = true;
  if (item.light && !item.curves.active)
    for (size_t i = firstInstance; i < meshInstances.size(); i++)
      meshInstances[i].light = true;
}

uint32_t Scene::addPrimitive(const PrimitiveSpec &spec,
                             smdl::Span<const float4x4> worldXfs,
                             const MaterialAssignment &materials) {
  // The same split `add()` gives meshes, without the displacement
  // exception: an analytic shape has no vertices for a material to
  // move, so the renames are always an instance-level fact.
  auto meshLevel{materials};
  meshLevel.renames.clear();
  const auto baseName{std::string(meshLevel.resolve(""))};
  auto key{spec.key() + "|" + baseName};
  auto entry{primitiveCache.find(key)};
  uint32_t primIndex{};
  if (entry == primitiveCache.end()) {
    primIndex = uint32_t(primitives.size());
    primitives.push_back(makePrimitive(device, spec, internMaterial(baseName)));
    primitiveCache.emplace(std::move(key), primIndex);
    SMDL_LOG_DEBUG("Built ", spec.key(), ": ", primitivePieceCount(spec),
                   " piece(s), area ", primitives.back()->objectArea);
  } else {
    primIndex = entry->second;
  }
  auto matIndex{INVALID_INDEX};
  if (auto itr{materials.renames.find(baseName)};
      itr != materials.renames.end() && itr->second != baseName)
    matIndex = internMaterial(itr->second);
  fileNames.push_back(smdl::concat("<", spec.name(), ">"));
  if (worldXfs.size() == 1)
    return addInstance(INVALID_INDEX, primIndex, INVALID_INDEX, worldXfs[0],
                       spec.name(), matIndex);
  return addInstanceArray(INVALID_INDEX, primIndex, INVALID_INDEX, worldXfs,
                          float4x4(1.0f), spec.name(), matIndex);
}

uint32_t Scene::addCurves(const std::string &fileName,
                          smdl::Span<const float4x4> worldXfs,
                          const CurvesSpec &spec,
                          const MaterialAssignment &materials) {
  // The same split `addPrimitive()` gives shapes: fibers have one
  // implicit slot and no vertices for a material to move, so the
  // whole-asset binding joins the cache key and the renames are always
  // an instance-level fact.
  auto meshLevel{materials};
  meshLevel.renames.clear();
  const auto baseName{std::string(meshLevel.resolve(""))};
  std::error_code ignored{};
  auto key{std::filesystem::weakly_canonical(fileName, ignored).string()};
  if (key.empty()) key = fileName;
  key += "|" + spec.key() + "|" + baseName;
  auto entry{curvesCache.find(key)};
  uint32_t curvesIndex{};
  if (entry == curvesCache.end()) {
    curvesIndex = uint32_t(curves.size());
    curves.push_back(makeCurves(device, readCurvesFile(fileName), spec,
                                internMaterial(baseName)));
    curvesCache.emplace(std::move(key), curvesIndex);
    SMDL_LOG_DEBUG(
        "Read ", smdl::QuotedPath(fileName), ": ", curves.back()->strandCount(),
        " strand(s), ", curves.back()->segCount(), " segment(s), ",
        CurvesFile::basisName(curves.back()->basis), " basis, ",
        spec.mode == CurvesSpec::Mode::RIBBON ? "ribbon" : "tube", " mode");
  } else {
    curvesIndex = entry->second;
    SMDL_LOG_DEBUG("Reusing ", smdl::QuotedPath(fileName), ": ",
                   curves[curvesIndex]->strandCount(), " strand(s)");
  }
  auto matIndex{INVALID_INDEX};
  if (auto itr{materials.renames.find(baseName)};
      itr != materials.renames.end() && itr->second != baseName)
    matIndex = internMaterial(itr->second);
  fileNames.push_back(fileName);
  if (worldXfs.size() == 1)
    return addInstance(INVALID_INDEX, INVALID_INDEX, curvesIndex, worldXfs[0],
                       fileName, matIndex);
  return addInstanceArray(INVALID_INDEX, INVALID_INDEX, curvesIndex, worldXfs,
                          float4x4(1.0f), fileName, matIndex);
}

ImportFile Scene::load(const aiScene &assScene, const SubdivSpec &subdiv,
                       const MaterialAssignment &materials) {
  // Material names are global to the composition, so a file's own material
  // indices have to be remapped as its meshes come in. The import's own
  // assignment applies here, at the one point where the file's names are
  // still distinguishable from every other file's: a name the import
  // reassigns is interned under the name it was given, so nothing
  // downstream can tell it apart from a file that named it that way.
  auto materialRemap{std::vector<uint32_t>()};
  materialRemap.reserve(assScene.mNumMaterials);
  for (unsigned int i = 0; i < assScene.mNumMaterials; i++)
    materialRemap.push_back(internMaterial(std::string(
        materials.resolve(assScene.mMaterials[i]->GetName().C_Str()))));
  const auto meshBase{uint32_t(meshes.size())};
  for (unsigned int i = 0; i < assScene.mNumMeshes; i++)
    load(*assScene.mMeshes[i], materialRemap, subdiv);
  auto file{ImportFile()};
  flattenNodes(*assScene.mRootNode, float4x4(1.0f), INVALID_INDEX, {}, meshBase,
               file);
  return file;
}

uint32_t Scene::addInstance(uint32_t meshIndex, uint32_t primIndex,
                            uint32_t curvesIndex, const float4x4 &xf,
                            std::string_view fileName, uint32_t matIndex) {
  // Embree gets the authored transform in full. It intersects in the
  // instance's own space and reports barycentrics, which are affine
  // invariant, so a sheared or non-uniformly scaled instance costs nothing
  // here and `makeHit()` rebuilds the world-space geometry from the same
  // matrix.
  auto instance{MeshInstance()};
  instance.setObjectToWorld(xf, fileName);
  instance.meshIndex = meshIndex;
  instance.primIndex = primIndex;
  instance.curvesIndex = curvesIndex;
  instance.matIndex = matIndex;
  auto inst{rtcNewGeometry(device, RTC_GEOMETRY_TYPE_INSTANCE)};
  rtcSetGeometryBuildQuality(inst, RTC_BUILD_QUALITY_HIGH);
  rtcSetGeometryTimeStepCount(inst, 1);
  rtcSetGeometryTransform(inst, 0, RTC_FORMAT_FLOAT4X4_COLUMN_MAJOR,
                          &instance.objectToWorld[0][0]);
  rtcSetGeometryInstancedScene(
      inst, curvesIndex != INVALID_INDEX ? curves[curvesIndex]->scene
            : primIndex != INVALID_INDEX ? primitives[primIndex]->scene
                                         : meshes[meshIndex]->scene);
  rtcCommitGeometry(inst);
  const auto geomID{rtcAttachGeometry(scene, inst)};
  rtcReleaseGeometry(inst);
  const auto base{uint32_t(meshInstances.size())};
  meshInstances.push_back(instance);
  if (instanceBaseByGeomID.size() <= geomID)
    instanceBaseByGeomID.resize(size_t(geomID) + 1, INVALID_INDEX);
  instanceBaseByGeomID[geomID] = base;
  return base;
}

uint32_t Scene::addInstanceArray(uint32_t meshIndex, uint32_t primIndex,
                                 uint32_t curvesIndex,
                                 smdl::Span<const float4x4> worldXfs,
                                 const float4x4 &nodeXf,
                                 std::string_view fileName, uint32_t matIndex) {
  // One geometry for the whole batch: Embree reads the transforms from
  // its own buffer (written row-major, exactly the `.places` record
  // layout), and reports hits as (this geometry, element index), which
  // `instanceIndexOf()` folds back onto the contiguous run of
  // `MeshInstance` entries appended here. The per-instance derived
  // matrices (rigid frame, cofactor) are still materialized per entry,
  // because every hit consumer wants them; what the array removes is
  // the per-instance Embree geometry, its commit, and its footprint in
  // the top-level BVH build.
  auto geometry{rtcNewGeometry(device, RTC_GEOMETRY_TYPE_INSTANCE_ARRAY)};
  rtcSetGeometryBuildQuality(geometry, RTC_BUILD_QUALITY_HIGH);
  rtcSetGeometryTimeStepCount(geometry, 1);
  rtcSetGeometryInstancedScene(
      geometry, curvesIndex != INVALID_INDEX ? curves[curvesIndex]->scene
                : primIndex != INVALID_INDEX ? primitives[primIndex]->scene
                                             : meshes[meshIndex]->scene);
  auto *transforms{static_cast<float *>(rtcSetNewGeometryBuffer(
      geometry, RTC_BUFFER_TYPE_TRANSFORM, 0, RTC_FORMAT_FLOAT3X4_ROW_MAJOR,
      sizeof(float) * 12, worldXfs.size()))};
  const auto base{uint32_t(meshInstances.size())};
  for (size_t i = 0; i < worldXfs.size(); i++) {
    const auto xf{worldXfs[i] * nodeXf};
    for (int row = 0; row < 3; row++)
      for (int column = 0; column < 4; column++)
        transforms[12 * i + 4 * row + column] = xf[column][row];
    auto instance{MeshInstance()};
    instance.setObjectToWorld(xf, fileName);
    instance.meshIndex = meshIndex;
    instance.primIndex = primIndex;
    instance.curvesIndex = curvesIndex;
    instance.matIndex = matIndex;
    meshInstances.push_back(instance);
  }
  rtcCommitGeometry(geometry);
  const auto geomID{rtcAttachGeometry(scene, geometry)};
  rtcReleaseGeometry(geometry);
  if (instanceBaseByGeomID.size() <= geomID)
    instanceBaseByGeomID.resize(size_t(geomID) + 1, INVALID_INDEX);
  instanceBaseByGeomID[geomID] = base;
  SMDL_LOG_DEBUG("Instance array: ", worldXfs.size(), " element(s) of ",
                 fileName);
  return base;
}

uint32_t Scene::addGroundPlane(float z, float halfExtent,
                               const std::string &materialName) {
  auto &mesh{meshes.emplace_back(new Mesh())};
  mesh->scene = rtcNewScene(device);
  rtcSetSceneFlags(mesh->scene, RTC_SCENE_FLAG_ROBUST);
  rtcSetSceneBuildQuality(mesh->scene, RTC_BUILD_QUALITY_HIGH);
  mesh->matIndex = internMaterial(materialName);
  mesh->verts.resize(4);
  for (uint32_t i = 0; i < 4; i++) {
    auto &vert{mesh->verts[i]};
    vert.point.x = i == 1 || i == 2 ? +halfExtent : -halfExtent;
    vert.point.y = i == 2 || i == 3 ? +halfExtent : -halfExtent;
    vert.point.z = z;
    vert.normal = float3(0, 0, 1);
    vert.tangent = float3(1, 0, 0);
    vert.texcoord = float2(vert.point.x, vert.point.y);
  }
  mesh->faces = {{0, 1, 2}, {0, 2, 3}};
  buildMeshGeometry(*mesh);
  return addInstance(uint32_t(meshes.size() - 1), INVALID_INDEX, INVALID_INDEX,
                     float4x4(1.0f), "ground plane");
}

BoundBox3 Scene::preCommitBounds() const {
  BoundBox3 bound{};
  for (const auto &instance : meshInstances) {
    auto fold{[&](const float3 &point) {
      bound.extend(float3(instance.objectToWorld * float4(point, 1.0f)));
    }};
    if (instance.isPrimitive()) {
      for (const auto &point : primitives[instance.primIndex]->proxyPoints)
        fold(point);
      continue;
    }
    if (instance.isCurves()) {
      for (const auto &point : curves[instance.curvesIndex]->proxyPoints)
        fold(point);
      continue;
    }
    const auto &mesh{*meshes[instance.meshIndex]};
    for (const auto &vert : mesh.verts) fold(vert.point);
    for (const auto &point : mesh.basePoints) fold(point);
  }
  return bound;
}

uint32_t Scene::internMaterial(std::string name) {
  auto [entry, isNew]{
      materialIndexByName.try_emplace(name, uint32_t(materials.size()))};
  if (isNew) {
    materialNames.push_back(std::move(name));
    materials.push_back(nullptr); // Resolved by `commit()`.
  }
  return entry->second;
}

void Scene::commit(const Color &wavelengths) {
  // Materials first, because displacement needs them; then the deferred
  // per-mesh work; and only then the top-level structure, whose bounds
  // must see the displaced geometry.
  resolveMaterials();
  // See the field: does every material block a shadow ray at its first
  // hit, which is what turns a visibility walk into a boolean occlusion
  // query.
  {
    opaqueShadows = true;
    const auto used{computeUsedMaterials()};
    for (size_t i = 0; i < materials.size(); i++) {
      if (!used[i] || !materials[i]) continue;
      if (!materials[i]->isShadowTrivial()) {
        opaqueShadows = false;
        break;
      }
    }
  }
  finalizeMeshes(wavelengths);
  rtcCommitScene(scene);
  RTCBounds bounds{};
  rtcGetSceneBounds(scene, &bounds);
  auto lower{float3(bounds.lower_x, bounds.lower_y, bounds.lower_z)};
  auto upper{float3(bounds.upper_x, bounds.upper_y, bounds.upper_z)};
  boundCenter = 0.5f * (lower + upper);
  boundRadius = 0.5f * length(upper - lower);
  uint64_t numTriangles{};
  for (const auto &mesh : meshes) numTriangles += mesh->faces.size();
  SMDL_LOG_DEBUG("Committed ", fileNames.size(), " file(s): ", meshes.size(),
                 " meshes, ", primitives.size(), " primitives, ", curves.size(),
                 " grooms, ", meshInstances.size(), " instances, ",
                 materials.size(), " materials, ", numTriangles,
                 " triangles, center (", boundCenter.x, ", ", boundCenter.y,
                 ", ", boundCenter.z, ") radius ", boundRadius,
                 opaqueShadows ? ", boolean shadows" : "");
}

std::vector<bool> Scene::computeUsedMaterials() const {
  auto isUsed{std::vector<bool>(materials.size(), false)};
  for (const auto &meshInstance : meshInstances)
    isUsed[materialIndexOf(meshInstance)] = true;
  return isUsed;
}

std::vector<std::string> Scene::usedMaterialNames() const {
  const auto isUsed{computeUsedMaterials()};
  auto names{std::vector<std::string>()};
  for (size_t i = 0; i < materialNames.size(); i++)
    if (isUsed[i]) names.push_back(materialNames[i]);
  return names;
}

void Scene::resolveMaterials() {
  const smdl::JIT::Material *fallback{};
  if (!fallbackMaterialName.empty()) {
    fallback = compiler.findMaterial(fallbackMaterialName);
    if (!fallback)
      throw smdl::Error(smdl::concat("no MDL material matches the fallback ",
                                     smdl::Quoted(fallbackMaterialName)));
  }
  // Only a material some instance actually shades with can ever be hit,
  // so only those resolve; the rest stay null. Scene files routinely
  // declare materials nothing uses, and a mesh whose every instance
  // overrides its material away leaves the mesh's own name legitimately
  // unresolved. Unused names are not even looked up: under the
  // desired-material filter they may name a deliberately skipped
  // material, which `findMaterial()` reports as a loud error.
  const auto isUsed{computeUsedMaterials()};
  auto unresolved{std::vector<std::string>()};
  for (size_t i = 0; i < materials.size(); i++) {
    if (!isUsed[i]) continue;
    materials[i] = compiler.findMaterial(materialNames[i]);
    if (!materials[i]) {
      // `findMaterial()` also returns null when more than one material
      // matches, having logged the candidates itself.
      if (fallback) {
        materials[i] = fallback;
      } else {
        unresolved.push_back(materialNames[i]);
      }
    }
  }
  if (!unresolved.empty()) {
    // Anything else is a null material pointer for the hit path to walk
    // into, so stop here and say exactly which names need attention.
    auto message{smdl::concat(
        unresolved.size(), " material name(s) in the scene do not resolve to "
                           "an MDL material:")};
    for (const auto &name : unresolved)
      message += smdl::concat("\n  ", smdl::Quoted(name));
    message += "\nRun with -list-materials to see how each name resolves, or "
               "pass -fallback-material=<name> to substitute a material.";
    throw smdl::Error(std::move(message));
  }
}

namespace {

// Weld the mesh's vertices by exact position bits. See `positionKey()` for
// why exactness is the point, and `WeldMap` for what the result promises.
[[nodiscard]] WeldMap weldByPosition(const Mesh &mesh) {
  auto groups{
      std::unordered_map<std::array<uint32_t, 3>, uint32_t, WeldHash>()};
  groups.reserve(mesh.verts.size());
  auto weld{WeldMap()};
  weld.groupOf.resize(mesh.verts.size());
  for (size_t i = 0; i < mesh.verts.size(); i++)
    weld.groupOf[i] = groups
                          .try_emplace(positionKey(mesh.verts[i].point),
                                       uint32_t(groups.size()))
                          .first->second;
  weld.numGroups = uint32_t(groups.size());
  return weld;
}

// Recompute shading normals from the triangles: area-weighted face
// normals accumulated over position-welded vertices, so the result is
// smooth and cannot crack along texture seams. Hard edges smooth out
// with everything else, which is the accepted trade of recomputing
// normals on a surface that subdivision or displacement just changed.
void recomputeNormals(Mesh &mesh, const WeldMap &weld) {
  const auto &weldOf{weld.groupOf};
  auto sums{std::vector<float3>(weld.numGroups)};
  for (const auto &face : mesh.faces) {
    const auto &p0{mesh.verts[face[0]].point};
    const auto &p1{mesh.verts[face[1]].point};
    const auto &p2{mesh.verts[face[2]].point};
    // Unnormalized: the cross product's length is twice the area, which
    // is exactly the weighting wanted.
    const auto faceNormal{smdl::cross(p1 - p0, p2 - p0)};
    sums[weldOf[face[0]]] = sums[weldOf[face[0]]] + faceNormal;
    sums[weldOf[face[1]]] = sums[weldOf[face[1]]] + faceNormal;
    sums[weldOf[face[2]]] = sums[weldOf[face[2]]] + faceNormal;
  }
  for (size_t i = 0; i < mesh.verts.size(); i++) {
    auto normal{sums[weldOf[i]]};
    const auto len{smdl::length(normal)};
    mesh.verts[i].normal = len > 0 ? normal / len : float3(0.0f, 0.0f, 1.0f);
  }
}

// Recompute tangents from the UV parameterization: the direction of
// increasing U accumulated per vertex (per copy, not per weld, since
// tangents legitimately differ across UV seams), orthonormalized against
// the normal, with a perpendicular fallback where the UVs are degenerate.
void recomputeTangents(Mesh &mesh) {
  auto sums{std::vector<float3>(mesh.verts.size())};
  for (const auto &face : mesh.faces) {
    const auto &v0{mesh.verts[face[0]]};
    const auto &v1{mesh.verts[face[1]]};
    const auto &v2{mesh.verts[face[2]]};
    const auto edge1{v1.point - v0.point};
    const auto edge2{v2.point - v0.point};
    const auto duv1{v1.texcoord - v0.texcoord};
    const auto duv2{v2.texcoord - v0.texcoord};
    const float det{duv1.x * duv2.y - duv1.y * duv2.x};
    if (!(std::fabs(det) > 1e-12f)) continue;
    const auto tangent{(duv2.y * edge1 - duv1.y * edge2) * (1.0f / det)};
    sums[face[0]] = sums[face[0]] + tangent;
    sums[face[1]] = sums[face[1]] + tangent;
    sums[face[2]] = sums[face[2]] + tangent;
  }
  for (size_t i = 0; i < mesh.verts.size(); i++) {
    auto &vert{mesh.verts[i]};
    auto tangent{sums[i] - smdl::dot(sums[i], vert.normal) * vert.normal};
    const auto len{smdl::length(tangent)};
    vert.tangent =
        len > 1e-12f ? tangent / len : smdl::perpendicularTo(vert.normal);
  }
}

} // namespace

void Scene::finalizeMeshes(const Color &wavelengths) {
  // Only instantiated meshes: nothing can ever hit the rest, and their
  // materials may legitimately be unresolved.
  auto isInstanced{std::vector<bool>(meshes.size(), false)};
  for (const auto &meshInstance : meshInstances)
    if (!meshInstance.isPrimitive() && !meshInstance.isCurves())
      isInstanced[meshInstance.meshIndex] = true;
  auto pending{std::vector<uint32_t>()};
  for (uint32_t i = 0; i < meshes.size(); i++)
    if (meshes[i]->needsFinalize && isInstanced[i]) pending.push_back(i);
  if (pending.empty()) return;
  SMDL_PROFILER_ENTRY("Finalize meshes");
  const auto startTime{std::chrono::steady_clock::now()};
  uint64_t facesBefore{};
  for (auto i : pending)
    facesBefore += meshes[i]->faces.size() + meshes[i]->baseFaceCounts.size();
  // Which level of the work gets the thread pool. A nested 'parallelFor'
  // runs every task inline, since only the outermost one is parallel, so
  // it has to be one or the other: enough meshes keep the pool busy side
  // by side, and a handful of large ones instead spread the per-vertex
  // displacement within each. A scene that subdivides at all usually
  // takes the second path, since 'subdivide' is marked per asset and one
  // asset is one mesh however many times it is placed.
  const auto perMesh{pending.size() >= smdl::getThreadCount()};
  std::atomic<uint32_t> numDisplaced{0};
  if (perMesh) {
    smdl::parallelFor(0, pending.size(), [&](size_t k) {
      if (finalizeMesh(*meshes[pending[k]], wavelengths, false)) numDisplaced++;
    });
  } else {
    for (auto i : pending)
      if (finalizeMesh(*meshes[i], wavelengths, true)) numDisplaced++;
  }
  uint64_t facesAfter{};
  for (auto i : pending) facesAfter += meshes[i]->faces.size();
  const auto seconds{std::chrono::duration<double>(
                         std::chrono::steady_clock::now() - startTime)
                         .count()};
  SMDL_LOG_INFO("Subdivision/displacement: ", pending.size(), " mesh(es), ",
                facesBefore, " faces to ", facesAfter, " triangles, ",
                numDisplaced.load(), " displaced, in ", seconds, "s");
  // 'displace' with nothing to displace usually means the scene resolved
  // to materials other than the ones the author had in mind.
  bool displaceRequested{};
  for (auto i : pending) displaceRequested |= meshes[i]->subdiv.displace;
  if (displaceRequested && numDisplaced.load() == 0)
    SMDL_LOG_WARN(
        "'displace' was requested but every material involved has provably "
        "zero 'geometry.displacement'; check that the scene resolves to the "
        "materials you meant (run with -list-materials).");
}

bool Scene::finalizeMesh(Mesh &mesh, const Color &wavelengths, bool spread) {
  // The one welding every pass below shares, built on first use: it costs
  // about as much as displacing the mesh does, and a smoothly subdivided
  // mesh that is never displaced needs none at all. Subdivision replaces
  // the vertices outright, so nothing may weld ahead of it.
  auto weld{std::optional<WeldMap>()};
  const auto weldOnce{[&]() -> const WeldMap & {
    if (!weld) weld = weldByPosition(mesh);
    return *weld;
  }};
  if (mesh.subdiv.levels > 0) {
    // Smooth refinement carries exact limit normals out of the refiner;
    // linear refinement, a degenerate limit normal, or the fallback
    // triangulation all leave the geometry as the only authority.
    if (!subdivideMesh(mesh)) recomputeNormals(mesh, weldOnce());
    recomputeTangents(mesh);
  }
  auto displaced{false};
  if (mesh.subdiv.displace) {
    displaced = displaceMesh(mesh, wavelengths, spread, weldOnce());
    if (displaced) {
      // The surface changed; the shading frame must follow it.
      recomputeNormals(mesh, weldOnce());
      recomputeTangents(mesh);
    }
  }
  buildMeshGeometry(mesh);
  mesh.needsFinalize = false;
  mesh.basePoints = {};
  mesh.baseTexcoords = {};
  mesh.baseColors = {};
  mesh.baseFaceCounts = {};
  mesh.baseIndices = {};
  return displaced;
}

bool Scene::displaceMesh(Mesh &mesh, const Color &wavelengths, bool spread,
                         const WeldMap &weld) {
  SMDL_SANITY_CHECK(mesh.matIndex < materials.size());
  const auto *material{materials[mesh.matIndex]};
  if (!material || material->hasZeroDisplacement()) return false;
  // One offset per position-welded vertex, averaged over the split
  // copies (whose UVs may disagree along texture seams) and applied to
  // every copy, so the displaced surface cannot crack. Well-authored
  // displacement matches across seams, where the averaging is a no-op.
  const auto &weldOf{weld.groupOf};
  auto offsets{std::vector<float3>(weld.numGroups)};
  auto counts{std::vector<uint32_t>(weld.numGroups)};
  // Evaluating the material is the expensive half and is independent per
  // vertex; the weld accumulation below is neither, and is left in vertex
  // order so that the sums do not depend on how this was scheduled.
  auto vertOffsets{std::vector<float3>(mesh.verts.size())};
  const auto evaluate{[&](size_t i) {
    const auto &vert{mesh.verts[i]};
    // The orthonormal shading frame, built here exactly as the state
    // finalize would Gram-Schmidt it, so that mapping the displacement
    // back out of internal (tangent) space is exact.
    auto normal{vert.normal};
    if (!(smdl::length(normal) > 0)) normal = float3(0.0f, 0.0f, 1.0f);
    normal = smdl::normalize(normal);
    auto tangent{vert.tangent - smdl::dot(vert.tangent, normal) * normal};
    const auto tangentLen{smdl::length(tangent)};
    tangent = tangentLen > 1e-12f ? tangent / tangentLen
                                  : smdl::perpendicularTo(normal);
    const auto bitangent{smdl::cross(normal, tangent)};
    // A partial state, like an opacity query: no allocator, and the
    // geometry handed over in the mesh's own space with the identity
    // instance transform, so internal space comes back out to mesh
    // space through the frame alone.
    auto state{makeRenderState(wavelengths)};
    state.position = vert.point;
    state.normal = normal;
    state.geometry_normal = normal;
    state.texture_coordinate[0] = float3(vert.texcoord.x, vert.texcoord.y, 0);
    state.texture_tangent_u[0] = tangent;
    state.texture_tangent_v[0] = bitangent;
    state.geometry_tangent_u[0] = tangent;
    state.geometry_tangent_v[0] = bitangent;
    if (!mesh.colors.empty()) {
      state.vertex_color_max = 1;
      state.vertex_color[0] = mesh.colors[i];
    }
    state.finalizeAndApplyInternalSpaceConventions();
    auto displacement{float3()};
    material->displacementEvaluate(state, displacement);
    // Internal space is the tangent frame, so the vector maps back
    // through it: `d.x` along U, `d.y` along V, `d.z` along the normal.
    vertOffsets[i] = displacement.x * tangent + displacement.y * bitangent +
                     displacement.z * normal;
  }};
  if (spread) {
    smdl::parallelFor(0, mesh.verts.size(), evaluate);
  } else {
    for (size_t i = 0; i < mesh.verts.size(); i++) evaluate(i);
  }
  for (size_t i = 0; i < mesh.verts.size(); i++) {
    offsets[weldOf[i]] = offsets[weldOf[i]] + vertOffsets[i];
    counts[weldOf[i]]++;
  }
  auto anyMoved{false};
  for (uint32_t g = 0; g < weld.numGroups; g++) {
    if (counts[g] > 1) offsets[g] = offsets[g] / float(counts[g]);
    anyMoved |= smdl::length(offsets[g]) > 0;
  }
  if (!anyMoved) return false;
  for (size_t i = 0; i < mesh.verts.size(); i++)
    mesh.verts[i].point = mesh.verts[i].point + offsets[weldOf[i]];
  return true;
}

void Scene::load(const aiMesh &assMesh,
                 const std::vector<uint32_t> &materialRemap,
                 const SubdivSpec &subdiv) {
  auto &mesh{meshes.emplace_back(new Mesh())};
  mesh->scene = rtcNewScene(device);
  rtcSetSceneFlags(mesh->scene, RTC_SCENE_FLAG_ROBUST);
  rtcSetSceneBuildQuality(mesh->scene, RTC_BUILD_QUALITY_HIGH);
  mesh->matIndex = assMesh.mMaterialIndex < materialRemap.size()
                       ? materialRemap[assMesh.mMaterialIndex]
                       : 0;
  mesh->subdiv = subdiv;
  if (subdiv.levels > 0) {
    // The subdivided path: keep the authored polygons for the refiner and
    // defer everything else, the Embree BVH included, to `commit()`. The
    // importer ran without triangulation, so faces are polygons of any
    // size; points and lines were still removed by `AI_CONFIG_PP_SBP_REMOVE`
    // and normals were deliberately not loaded (the refined surface
    // recomputes them).
    if ((assMesh.mPrimitiveTypes &
         ~unsigned(aiPrimitiveType_TRIANGLE | aiPrimitiveType_POLYGON)) != 0 ||
        assMesh.mNumFaces == 0) {
      rtcCommitScene(mesh->scene);
      return;
    }
    mesh->basePoints.resize(assMesh.mNumVertices);
    for (unsigned int i = 0; i < assMesh.mNumVertices; i++)
      mesh->basePoints[i] =
          float3(assMesh.mVertices[i].x, assMesh.mVertices[i].y,
                 assMesh.mVertices[i].z);
    if (assMesh.mTextureCoords[0]) {
      mesh->baseTexcoords.resize(assMesh.mNumVertices);
      for (unsigned int i = 0; i < assMesh.mNumVertices; i++)
        mesh->baseTexcoords[i] = float2(assMesh.mTextureCoords[0][i].x,
                                        assMesh.mTextureCoords[0][i].y);
    }
    if (assMesh.mColors[0]) {
      mesh->baseColors.resize(assMesh.mNumVertices);
      for (unsigned int i = 0; i < assMesh.mNumVertices; i++) {
        const auto &color{assMesh.mColors[0][i]};
        mesh->baseColors[i] = float4(color.r, color.g, color.b, color.a);
      }
    }
    mesh->baseFaceCounts.reserve(assMesh.mNumFaces);
    for (unsigned int i = 0; i < assMesh.mNumFaces; i++) {
      const auto &face{assMesh.mFaces[i]};
      mesh->baseFaceCounts.push_back(face.mNumIndices);
      for (unsigned int j = 0; j < face.mNumIndices; j++)
        mesh->baseIndices.push_back(face.mIndices[j]);
    }
    mesh->needsFinalize = true;
    return;
  }
  // `AI_CONFIG_PP_SBP_REMOVE` deletes point and line primitives, so every
  // surviving mesh is triangles only and `aiProcess_GenSmoothNormals` has
  // filled in its normals. Anything else is skipped rather than misread as
  // triangles, but it must still occupy its index, so it stays in the array
  // as an empty mesh.
  if (assMesh.mPrimitiveTypes != aiPrimitiveType_TRIANGLE ||
      assMesh.mNumFaces == 0 || !assMesh.mNormals) {
    rtcCommitScene(mesh->scene);
    return;
  }
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
  if (assMesh.mColors[0]) {
    mesh->colors.resize(assMesh.mNumVertices);
    for (unsigned int i = 0; i < assMesh.mNumVertices; i++) {
      const auto &color{assMesh.mColors[0][i]};
      mesh->colors[i] = float4(color.r, color.g, color.b, color.a);
    }
  }
  for (unsigned int i = 0; i < assMesh.mNumFaces; i++)
    mesh->faces[i] = {uint32_t(assMesh.mFaces[i].mIndices[0]),
                      uint32_t(assMesh.mFaces[i].mIndices[1]),
                      uint32_t(assMesh.mFaces[i].mIndices[2])};
  if (subdiv.displace) {
    // Displacement without subdivision: the triangles are final but the
    // vertices are not, and moving them needs the materials `commit()`
    // resolves, so the BVH waits.
    mesh->needsFinalize = true;
    return;
  }
  buildMeshGeometry(*mesh);
}

void Scene::buildMeshGeometry(Mesh &mesh) {
  if (mesh.faces.empty()) {
    rtcCommitScene(mesh.scene);
    return;
  }
  auto geometry{rtcNewGeometry(device, RTC_GEOMETRY_TYPE_TRIANGLE)};
  rtcSetGeometryBuildQuality(geometry, RTC_BUILD_QUALITY_HIGH);
  rtcSetGeometryTimeStepCount(geometry, 1);
  rtcSetSharedGeometryBuffer(geometry, RTC_BUFFER_TYPE_VERTEX, 0,
                             RTC_FORMAT_FLOAT3, mesh.verts.data(), 0,
                             sizeof(Mesh::Vert), mesh.verts.size());
  rtcSetSharedGeometryBuffer(geometry, RTC_BUFFER_TYPE_INDEX, 0,
                             RTC_FORMAT_UINT3, mesh.faces.data(), 0,
                             sizeof(Mesh::Face), mesh.faces.size());
  rtcCommitGeometry(geometry);
  rtcAttachGeometry(mesh.scene, geometry);
  rtcCommitScene(mesh.scene);
  rtcReleaseGeometry(geometry);
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
    const auto instIndex{
        instanceIndexOf(rayHit.hit.instID[0], rayHit.hit.instPrimID[0])};
    // A curve hit is built here rather than through `makeHit()`, because
    // it needs the ray: the point comes from `tfar`, the tube normal
    // from the object-space `Ng`, and the ribbon normal from the ray
    // direction. The (u, v) are the curve parameters, NOT barycentrics,
    // so the triangle path's clamp-and-complement does not apply
    // (ribbon `v` legitimately spans -1 to +1).
    if (meshInstances[instIndex].isCurves()) {
      hit = makeCurvesHit(
          instIndex, rayHit.hit.primID, rayHit.hit.u, rayHit.hit.v,
          float3(rayHit.hit.Ng_x, rayHit.hit.Ng_y, rayHit.hit.Ng_z),
          ray(rayHit.ray.tfar), ray.dir);
      return true;
    }
    auto clamp01{[](float value) { return std::clamp(value, 0.0f, 1.0f); }};
    auto bary{float3(clamp01(1.0f - rayHit.hit.u - rayHit.hit.v),
                     clamp01(rayHit.hit.u), clamp01(rayHit.hit.v))};
    // A primitive reports its object-space point in the normal slots,
    // and the hit is built from that rather than from the parameters.
    hit = meshInstances[instIndex].isPrimitive()
              ? makePrimitiveHit(
                    instIndex, rayHit.hit.primID, bary,
                    float3(rayHit.hit.Ng_x, rayHit.hit.Ng_y, rayHit.hit.Ng_z))
              : makeHit(instIndex, rayHit.hit.primID, bary);
    return true;
  }
}

bool Scene::isOccluded(const Ray &ray) const {
  RTCRay rtcRay{};
  rtcRay.org_x = ray.org.x;
  rtcRay.org_y = ray.org.y;
  rtcRay.org_z = ray.org.z;
  rtcRay.dir_x = ray.dir.x;
  rtcRay.dir_y = ray.dir.y;
  rtcRay.dir_z = ray.dir.z;
  rtcRay.tnear = ray.tmin;
  rtcRay.tfar = ray.tmax;
  rtcRay.time = 0;
  rtcRay.mask = unsigned(-1);
  rtcRay.id = 0;
  rtcRay.flags = 0;
  rtcOccluded1(scene, &rtcRay, nullptr);
  return rtcRay.tfar < 0.0f;
}

bool Scene::intersect(Ray &ray, ManifoldHit &hit) const {
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
  if (rayHit.hit.primID == unsigned(-1)) return false;
  ray.tmax = rayHit.ray.tfar;
  const auto instIndex{
      instanceIndexOf(rayHit.hit.instID[0], rayHit.hit.instPrimID[0])};
  const auto &meshInstance{meshInstances[instIndex]};
  hit.instance = &meshInstance;
  hit.material = materials[materialIndexOf(meshInstance)];
  hit.vertex.surface = instIndex;
  hit.vertex.face = rayHit.hit.primID;
  // The projection rejects curve pins, so a curve hit only ever
  // contributes its point, for a null-interface passthrough; computed
  // from the ray exactly as `makeCurvesHit()` receives it.
  if (meshInstance.isCurves()) {
    hit.vertex.point = ray(rayHit.ray.tfar);
    hit.vertex.coords = float3(0.0f, rayHit.hit.u, rayHit.hit.v);
    return true;
  }
  auto clamp01{[](float value) { return std::clamp(value, 0.0f, 1.0f); }};
  const auto bary{float3(clamp01(1.0f - rayHit.hit.u - rayHit.hit.v),
                         clamp01(rayHit.hit.u), clamp01(rayHit.hit.v))};
  hit.vertex.coords = bary;
  const auto &objectToWorld{meshInstance.objectToWorld};
  if (meshInstance.isPrimitive()) {
    const auto &primitive{*primitives[meshInstance.primIndex]};
    const auto surface{evalPrimitiveSurface(primitive.spec, rayHit.hit.primID,
                                            float2(bary[1], bary[2]))};
    hit.vertex.point = float3(objectToWorld * float4(surface.point, 1.0f));
  } else {
    const auto &mesh{*meshes[meshInstance.meshIndex]};
    const auto &face{mesh.faces[rayHit.hit.primID]};
    const auto &vert0{mesh.verts[face[0]]};
    const auto &vert1{mesh.verts[face[1]]};
    const auto &vert2{mesh.verts[face[2]]};
    const auto point0{float3(objectToWorld * float4(vert0.point, 1.0f))};
    const auto point1{float3(objectToWorld * float4(vert1.point, 1.0f))};
    const auto point2{float3(objectToWorld * float4(vert2.point, 1.0f))};
    hit.vertex.point = bary[0] * point0 + bary[1] * point1 + bary[2] * point2;
  }
  return true;
}

Hit Scene::makeHit(uint32_t instIndex, uint32_t faceIndex,
                   const float3 &bary) const {
  const auto &meshInstance{meshInstances[instIndex]};
  if (meshInstance.isPrimitive())
    return makePrimitiveHit(instIndex, faceIndex, bary);
  // Curve hits need the ray and only ever come from `intersect()`,
  // which builds them itself; nothing may rebuild one from indices.
  SMDL_SANITY_CHECK(!meshInstance.isCurves());
  const auto &mesh{*meshes[meshInstance.meshIndex]};
  const auto &face{mesh.faces[faceIndex]};
  const auto &vert0{mesh.verts[face[0]]};
  const auto &vert1{mesh.verts[face[1]]};
  const auto &vert2{mesh.verts[face[2]]};
  const auto &objectToWorld{meshInstance.objectToWorld};
  // World space first, everything else after. Interpolating the transformed
  // points is the same as transforming the interpolated point, so this
  // costs three matrix-vector products and buys exactness under scale.
  auto point0{float3(objectToWorld * float4(vert0.point, 1.0f))};
  auto point1{float3(objectToWorld * float4(vert1.point, 1.0f))};
  auto point2{float3(objectToWorld * float4(vert2.point, 1.0f))};
  // `Ng` from the raw edges: scaling either one only scales the cross
  // product, which the normalize divides back out, so the edges need not be
  // unit for it. `Tg` does need a unit edge, and is the only reason one of
  // them is normalized at all. `Scene::manifoldGeometry()` forms `Ng` by
  // this same expression, so the two agree bit for bit.
  const auto faceNormal{cross(point1 - point0, point2 - point0)};
  auto edge1{smdl::normalize(point1 - point0)};
  auto barycentric{[&](auto member) {
    return bary[0] * vert0.*member + bary[1] * vert1.*member +
           bary[2] * vert2.*member;
  }};
  Hit hit{};
  hit.instIndex = instIndex;
  hit.meshIndex = meshInstance.meshIndex;
  hit.faceIndex = faceIndex;
  hit.matIndex = materialIndexOf(meshInstance);
  SMDL_SANITY_CHECK(hit.matIndex < materials.size());
  hit.material = materials[hit.matIndex];
  hit.bary = bary;
  hit.point = bary[0] * point0 + bary[1] * point1 + bary[2] * point2;
  // Normals transform by the cofactor matrix and tangents by the linear
  // part, since a tangent is a direction lying in the surface while a
  // normal is not. The geometry normal is a cofactor image too,
  // implicitly: the cross product of two transformed edges is the
  // cofactor image of the object-space geometry normal. So both normals
  // pick up the sign of the determinant, and both are flipped back
  // together.
  hit.normal =
      normalize(meshInstance.normalMatrix * barycentric(&Mesh::Vert::normal));
  hit.tangent = normalize(
      float3(objectToWorld * float4(barycentric(&Mesh::Vert::tangent), 0.0f)));
  hit.Ng = normalize(faceNormal);
  if (meshInstance.flipsWinding) {
    hit.normal = -hit.normal;
    hit.Ng = -hit.Ng;
  }
  hit.Tg = edge1;
  hit.texcoord = barycentric(&Mesh::Vert::texcoord);
  hit.textureDensity = uvTextureDensity(point0, point1, point2, vert0.texcoord,
                                        vert1.texcoord, vert2.texcoord);
  if (!mesh.colors.empty()) {
    hit.vertexColorSets = 1;
    hit.vertexColor = bary[0] * mesh.colors[face[0]] +
                      bary[1] * mesh.colors[face[1]] +
                      bary[2] * mesh.colors[face[2]];
  }
  hit.instance = &meshInstance;
  return hit;
}

Hit Scene::makePrimitiveHit(uint32_t instIndex, uint32_t primID,
                            const float3 &bary) const {
  const auto &primitive{*primitives[meshInstances[instIndex].primIndex]};
  // The (u, v) ride in the barycentric slots, exactly as
  // `Scene::intersect()` packed them; see `Primitive.h`.
  return makePrimitiveHitFrom(
      instIndex, primID, bary,
      evalPrimitiveSurface(primitive.spec, primID, float2(bary[1], bary[2])));
}

Hit Scene::makePrimitiveHit(uint32_t instIndex, uint32_t primID,
                            const float3 &bary,
                            const float3 &objectPoint) const {
  const auto &primitive{*primitives[meshInstances[instIndex].primIndex]};
  return makePrimitiveHitFrom(
      instIndex, primID, bary,
      evalPrimitiveSurfaceAt(primitive.spec, primID, objectPoint));
}

Hit Scene::makePrimitiveHitFrom(uint32_t instIndex, uint32_t primID,
                                const float3 &bary,
                                const PrimitiveSurface &surface) const {
  const auto &meshInstance{meshInstances[instIndex]};
  const auto &objectToWorld{meshInstance.objectToWorld};
  Hit hit{};
  hit.instIndex = instIndex;
  hit.meshIndex = INVALID_INDEX;
  hit.faceIndex = primID;
  hit.matIndex = materialIndexOf(meshInstance);
  SMDL_SANITY_CHECK(hit.matIndex < materials.size());
  hit.material = materials[hit.matIndex];
  hit.bary = bary;
  hit.point = float3(objectToWorld * float4(surface.point, 1.0f));
  // The analytic normal transforms by the cofactor matrix like any
  // other; a mirroring instance flips its image inward, and the same
  // correction the mesh path applies flips it back out.
  hit.normal = normalize(meshInstance.normalMatrix * surface.normal);
  if (meshInstance.flipsWinding) hit.normal = -hit.normal;
  // Shading and geometric agree by construction: that is the point of
  // an analytic surface.
  hit.Ng = hit.normal;
  const auto dPduWorld{float3(objectToWorld * float4(surface.dPdu, 0.0f))};
  const auto dPdvWorld{float3(objectToWorld * float4(surface.dPdv, 0.0f))};
  auto tangent{dPduWorld};
  hit.tangent =
      smdl::tryNormalize(tangent) ? tangent : smdl::perpendicularTo(hit.normal);
  hit.Tg = hit.tangent;
  hit.texcoord = float2(bary[1], bary[2]);
  // UV area per world area, which is what the ray-cone footprint
  // multiplies into a texture-space filter width: exactly the triangle
  // path's quantity, from the parametric partials instead of the edges.
  const auto patchArea{length(cross(dPduWorld, dPdvWorld))};
  hit.textureDensity = patchArea > 1e-12f ? 1.0f / patchArea : 0.0f;
  hit.instance = &meshInstance;
  return hit;
}

ManifoldGeometry Scene::manifoldGeometry(const Hit &hit) const {
  return manifoldGeometry(hit.instIndex, hit.faceIndex, hit.bary);
}

ManifoldGeometry Scene::manifoldGeometry(uint32_t instIndex, uint32_t faceIndex,
                                         const float3 &bary) const {
  const auto &meshInstance{meshInstances[instIndex]};
  SMDL_SANITY_CHECK(!meshInstance.isCurves());
  const auto &objectToWorld{meshInstance.objectToWorld};
  ManifoldGeometry geometry{};
  // The point and geometry normal by the same expressions `makeHit()`
  // uses, and the unnormalized shading normal field and its parametric
  // partials through the cofactor matrix exactly as `makeHit()`
  // transforms the normal itself; the winding flip on the shading field
  // is applied at the end, where it negates the unit normal and its
  // partials together.
  float3 rawNormal{};
  float3 dRawdu{};
  float3 dRawdv{};
  if (meshInstance.isPrimitive()) {
    const auto &primitive{*primitives[meshInstance.primIndex]};
    const auto surface{evalPrimitiveSurface(primitive.spec, faceIndex,
                                            float2(bary[1], bary[2]))};
    geometry.point = float3(objectToWorld * float4(surface.point, 1.0f));
    geometry.dPdu = float3(objectToWorld * float4(surface.dPdu, 0.0f));
    geometry.dPdv = float3(objectToWorld * float4(surface.dPdv, 0.0f));
    rawNormal = meshInstance.normalMatrix * surface.normal;
    dRawdu = meshInstance.normalMatrix * surface.dNdu;
    dRawdv = meshInstance.normalMatrix * surface.dNdv;
    geometry.Ng = normalize(meshInstance.normalMatrix * surface.normal);
    if (meshInstance.flipsWinding) geometry.Ng = -geometry.Ng;
  } else {
    const auto &mesh{*meshes[meshInstance.meshIndex]};
    const auto &face{mesh.faces[faceIndex]};
    const auto &vert0{mesh.verts[face[0]]};
    const auto &vert1{mesh.verts[face[1]]};
    const auto &vert2{mesh.verts[face[2]]};
    const auto point0{float3(objectToWorld * float4(vert0.point, 1.0f))};
    const auto point1{float3(objectToWorld * float4(vert1.point, 1.0f))};
    const auto point2{float3(objectToWorld * float4(vert2.point, 1.0f))};
    geometry.point = bary[0] * point0 + bary[1] * point1 + bary[2] * point2;
    // The parameterization is the barycentric pair (bary[1], bary[2]).
    geometry.dPdu = point1 - point0;
    geometry.dPdv = point2 - point0;
    rawNormal = meshInstance.normalMatrix *
                (bary[0] * vert0.normal + bary[1] * vert1.normal +
                 bary[2] * vert2.normal);
    dRawdu = meshInstance.normalMatrix * (vert1.normal - vert0.normal);
    dRawdv = meshInstance.normalMatrix * (vert2.normal - vert0.normal);
    // The same expression `makeHit()` uses, see there.
    geometry.Ng = normalize(cross(point1 - point0, point2 - point0));
    if (meshInstance.flipsWinding) geometry.Ng = -geometry.Ng;
  }
  // Differentiate the normalization: with N = m / |m|,
  // dN = (dm - N dot(N, dm)) / |m|.
  const float rawLength{length(rawNormal)};
  if (!(rawLength > 0.0f)) {
    // The interpolated normal field collapsed here, which a seam whose
    // vertex normals cancel will do. Fall back to the facet, which is
    // the flat reading of a field that has no direction to give, and
    // leave the partials zero: there is nothing left to differentiate.
    // The alternative is a NaN normal propagating into the walk, and
    // `geometry.Ng` already carries the winding flip, so this returns
    // before the flip below.
    geometry.normal = geometry.Ng;
    return geometry;
  }
  geometry.normal = normalize(rawNormal);
  geometry.dNdu =
      (dRawdu - dot(dRawdu, geometry.normal) * geometry.normal) / rawLength;
  geometry.dNdv =
      (dRawdv - dot(dRawdv, geometry.normal) * geometry.normal) / rawLength;
  if (meshInstance.flipsWinding) {
    geometry.normal = -geometry.normal;
    geometry.dNdu = -geometry.dNdu;
    geometry.dNdv = -geometry.dNdv;
  }
  return geometry;
}

Hit Scene::makeCurvesHit(uint32_t instIndex, uint32_t primID, float u, float v,
                         const float3 &objectNg, const float3 &worldPoint,
                         const float3 &rayDir) const {
  const auto &meshInstance{meshInstances[instIndex]};
  const auto &groom{*curves[meshInstance.curvesIndex]};
  const auto &objectToWorld{meshInstance.objectToWorld};
  const auto matIndex{materialIndexOf(meshInstance)};
  SMDL_SANITY_CHECK(matIndex < materials.size());
  const auto *material{materials[matIndex]};
  // The fiber tangent, root toward tip: the axis derivative transforms
  // by the linear part like any tangent. Degenerate windows (repeated
  // control points) fall back to any perpendicular so the frame stays a
  // frame.
  const auto axis{groom.axisAt(primID, u)};
  auto tangent{float3(objectToWorld * float4(axis.tangent, 0.0f))};
  const bool ribbon{groom.spec.mode == CurvesSpec::Mode::RIBBON};
  auto normal{float3()};
  if (ribbon) {
    // The camera-facing ribbon normal: the ray direction reversed and
    // made perpendicular to the fiber. Embree's flat-curve `Ng` is the
    // curve tangent rather than a surface normal, so it is not used.
    if (!smdl::tryNormalize(tangent)) tangent = smdl::perpendicularTo(rayDir);
    normal = -(rayDir - dot(rayDir, tangent) * tangent);
    if (!smdl::tryNormalize(normal)) normal = smdl::perpendicularTo(tangent);
    // A hair material needs the normal to encode the cross-section
    // offset `h = sin(gamma)`: tilt the facing normal toward the hit's
    // offset from the fiber center by `sin(gamma) = |v|`, the fake tube
    // normal a true tube would report at that offset. Measuring the tilt
    // direction from the geometry pins its sign with no dependence on
    // Embree's across-axis convention. Gated on the material so ribbon
    // grooms with surface materials render exactly as before.
    if (material->hasHair()) {
      const float sinGamma{std::min(std::fabs(v), 1.0f)};
      auto widthDir{worldPoint -
                    float3(objectToWorld * float4(axis.point, 1.0f))};
      widthDir = widthDir - dot(widthDir, tangent) * tangent;
      if (sinGamma > 0.0f && smdl::tryNormalize(widthDir)) {
        auto tilted{std::sqrt(1.0f - sinGamma * sinGamma) * normal +
                    sinGamma * widthDir};
        if (smdl::tryNormalize(tilted)) normal = tilted;
      }
    }
  } else {
    // The swept surface normal, from Embree's object-space `Ng` through
    // the cofactor matrix; a mirroring instance flips it back outward
    // exactly as the mesh path does.
    normal = meshInstance.normalMatrix * objectNg;
    if (!smdl::tryNormalize(normal)) normal = -rayDir;
    if (meshInstance.flipsWinding) normal = -normal;
    if (!smdl::tryNormalize(tangent)) tangent = smdl::perpendicularTo(normal);
  }
  // The strand parameter: a strand's segments partition it uniformly,
  // so root-to-tip is the segment's place in the strand plus the hit's
  // place in the segment.
  const auto strand{groom.segStrand[primID]};
  const auto firstSeg{groom.strandFirstSeg[strand]};
  const auto numSegs{groom.strandFirstSeg[strand + 1] - firstSeg};
  const auto strandU{(float(primID - firstSeg) + u) / float(numSegs)};
  const auto vAcross{ribbon ? 0.5f * (v + 1.0f) : 0.0f};
  Hit hit{};
  hit.instIndex = instIndex;
  hit.meshIndex = INVALID_INDEX;
  hit.faceIndex = primID;
  hit.matIndex = matIndex;
  hit.material = material;
  hit.bary = float3(0.0f, u, vAcross);
  hit.point = worldPoint;
  hit.normal = normal;
  hit.tangent = tangent;
  hit.Ng = normal;
  hit.Tg = tangent;
  hit.texcoord = float2(strandU, vAcross);
  // Fibers opt out of the ray-cone LOD machinery; see `Curves.h`.
  hit.textureDensity = 0.0f;
  // The world-space fiber diameter, scaled by the isotropic part of the
  // instance transform (the cube root of the linear determinant), which
  // is exact for the rigid and uniformly scaled placements grooms use.
  const auto lx{float3(objectToWorld * float4(1.0f, 0.0f, 0.0f, 0.0f))};
  const auto ly{float3(objectToWorld * float4(0.0f, 1.0f, 0.0f, 0.0f))};
  const auto lz{float3(objectToWorld * float4(0.0f, 0.0f, 1.0f, 0.0f))};
  hit.fiberThickness =
      2.0f * axis.radius * std::cbrt(std::fabs(dot(lx, smdl::cross(ly, lz))));
  if (!groom.rootUVs.empty()) {
    hit.textureSpaces = 2;
    hit.texcoord1 = groom.rootUVs[strand];
  }
  hit.instance = &meshInstance;
  return hit;
}
