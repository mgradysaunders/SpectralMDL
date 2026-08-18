#include "Scene.h"
#include "Subdivide.h"

#include "assimp/Importer.hpp"
#include "assimp/config.h"
#include "assimp/postprocess.h"
#include "assimp/scene.h"

#include "embree4/rtcore_ray.h"

#include "llvm/Support/Parallel.h"

#include "smdl/Support/Logger.h"
#include "smdl/Support/Profiler.h"

#include <atomic>
#include <chrono>
#include <filesystem>
#include <unordered_map>

namespace {

// Configure an importer to read the least that assimp can be made to read:
// triangle geometry, one texture coordinate set, and material names. This
// renderer reads nothing else from a scene file: SMDL loads every texture
// itself, and the material name is the only thing a material contributes.
//
// `extraRemovedComponents` drops more per-vertex data on top of the
// baseline, for callers that do not build geometry at all.
//
// NOTE: `aiComponent_MATERIALS` must NEVER appear in the removal mask. It
// deletes every material and substitutes one generated default, which
// destroys the only thing we are here to read.
void configureImporter(Assimp::Importer &importer,
                       unsigned extraRemovedComponents = 0) {
  // NOTE: Texture coordinate sets 1 through 6 only. Assimp's
  // `aiComponent_TEXCOORDSn(n)` macro is `1u << (n + 25u)`, so set 6 lands
  // on the sign bit and set 7 would shift by 32, which is not representable
  // in the mask at all. The mask is built unsigned and cast, which is what
  // assimp itself does with it: the property is stored as `int` and read
  // back into an `unsigned int` field.
  const unsigned removedComponents{
      extraRemovedComponents |
      aiComponent_TEXTURES |    // Embedded FBX/GLB image blobs
      aiComponent_COLORS |      // Per-vertex colors
      aiComponent_BONEWEIGHTS | //
      aiComponent_ANIMATIONS |  //
      aiComponent_LIGHTS |      //
      aiComponent_CAMERAS |     //
      aiComponent_TEXCOORDSn(1) | aiComponent_TEXCOORDSn(2) |
      aiComponent_TEXCOORDSn(3) | aiComponent_TEXCOORDSn(4) |
      aiComponent_TEXCOORDSn(5) | aiComponent_TEXCOORDSn(6)};
  importer.SetPropertyInteger(AI_CONFIG_PP_RVC_FLAGS, int(removedComponents));
  // Delete point and line primitives outright instead of sorting them into
  // meshes of their own, which the triangle-only mesh loader misreads.
  importer.SetPropertyInteger(AI_CONFIG_PP_SBP_REMOVE,
                              aiPrimitiveType_POINT | aiPrimitiveType_LINE);
  // FBX reads these eagerly, so suppressing them at the source beats
  // removing them after the fact. Materials must stay on: the names come
  // from them. (`AI_CONFIG_IMPORT_FBX_READ_ALL_MATERIALS` stays at its
  // default of false, so unreferenced materials never appear either.)
  importer.SetPropertyBool(AI_CONFIG_IMPORT_FBX_READ_TEXTURES, false);
  importer.SetPropertyBool(AI_CONFIG_IMPORT_FBX_READ_CAMERAS, false);
  importer.SetPropertyBool(AI_CONFIG_IMPORT_FBX_READ_LIGHTS, false);
  importer.SetPropertyBool(AI_CONFIG_IMPORT_FBX_READ_ANIMATIONS, false);
  importer.SetPropertyBool(AI_CONFIG_IMPORT_FBX_READ_WEIGHTS, false);
  importer.SetPropertyBool(AI_CONFIG_IMPORT_NO_SKELETON_MESHES, true);
}

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

// The post-processing for a material-usage-only load, which needs the
// meshes (they carry `mMaterialIndex`) but none of the vertex data.
//
// `aiProcess_FindInstances` is deliberately absent even though the full
// load enables it. The listing answers "how much geometry needs this
// material", which is a question about the file as authored: deduplicating
// first would report one shared copy where the file has sixty-four, and
// undercount the triangles accordingly.
//
constexpr unsigned MATERIAL_POSTPROCESS_FLAGS =
    aiProcess_RemoveComponent | aiProcess_Triangulate | aiProcess_SortByPType;

// Flatten a node graph into nodes and placements, in preorder, so that a
// node always precedes its descendants.
//
// `meshBase` offsets the file's own mesh indices into `Scene::meshes`; a
// listing that builds no meshes passes zero and reads the file's indices
// straight back.
//
void flattenNodes(const aiNode &assNode, const float4x4 &parentXf,
                  uint32_t parentIndex, std::string_view parentPath,
                  uint32_t meshBase, ImportFile &file) {
  // `aiMatrix4x4` stores rows; `float4x4` stores columns. Both denote the
  // same map, so the composition order is the same either way, and the file
  // transform composes on the left of the whole node chain.
  const auto &m{assNode.mTransformation};
  const auto xf{parentXf * float4x4{float4{m.a1, m.b1, m.c1, m.d1}, //
                                    float4{m.a2, m.b2, m.c2, m.d2}, //
                                    float4{m.a3, m.b3, m.c3, m.d3}, //
                                    float4{m.a4, m.b4, m.c4, m.d4}}};
  const auto nodeIndex{uint32_t(file.nodes.size())};
  auto &node{file.nodes.emplace_back()};
  node.parent = parentIndex;
  node.nodeToFile = xf;
  // The root contributes no path component: its name is whatever the
  // importer decided to call it, not anything the file's author chose.
  if (parentIndex != INVALID_INDEX) {
    node.path = parentPath.empty()
                    ? std::string(assNode.mName.C_Str())
                    : smdl::concat(parentPath, "/", assNode.mName.C_Str());
  }
  // Copy the path before recursing: `file.nodes` reallocates, so a reference
  // into it cannot outlive the child calls that append to it.
  const auto path{node.path};
  for (unsigned int i = 0; i < assNode.mNumMeshes; i++)
    file.placements.push_back({meshBase + assNode.mMeshes[i], nodeIndex});
  for (unsigned int i = 0; i < assNode.mNumChildren; i++)
    flattenNodes(*assNode.mChildren[i], xf, nodeIndex, path, meshBase, file);
}

// Does `text` match `pattern`, where `*` stands for any run of characters
// and `?` for any one character?
//
// Iterative with one backtrack point, which is all a pattern with no
// alternation needs: on a mismatch, resume from just after the last `*`
// with one more character consumed by it.
//
[[nodiscard]] bool matchGlob(std::string_view pattern, std::string_view text) {
  size_t patternPos{}, textPos{};
  size_t starPos{std::string_view::npos}, starTextPos{};
  while (textPos < text.size()) {
    if (patternPos < pattern.size() &&
        (pattern[patternPos] == '?' || pattern[patternPos] == text[textPos])) {
      patternPos++, textPos++;
    } else if (patternPos < pattern.size() && pattern[patternPos] == '*') {
      starPos = patternPos++, starTextPos = textPos;
    } else if (starPos != std::string_view::npos) {
      patternPos = starPos + 1, textPos = ++starTextPos;
    } else {
      return false;
    }
  }
  while (patternPos < pattern.size() && pattern[patternPos] == '*')
    patternPos++;
  return patternPos == pattern.size();
}

// Does `pattern` select the node at `path`? A pattern that mentions `/`
// matches the whole path, and one that does not matches only its last
// component, so that the wrapper node an exporter puts above everything
// does not have to be spelled out.
[[nodiscard]] bool matchNodePath(std::string_view pattern,
                                 std::string_view path) {
  if (pattern.find('/') == std::string_view::npos) {
    const auto slash{path.rfind('/')};
    if (slash != std::string_view::npos) path.remove_prefix(slash + 1);
  }
  return matchGlob(pattern, path);
}

} // namespace

std::vector<uint32_t> resolveSelection(const std::vector<ImportNode> &nodes,
                                       const ObjectSelection &selection,
                                       std::string_view fileName) {
  auto selectedRoot{std::vector<uint32_t>(nodes.size(), INVALID_INDEX)};
  if (nodes.empty()) return selectedRoot;
  if (selection.patterns.empty()) {
    // The whole file, selected through the root, so that `recenter` still
    // has something to recenter about.
    std::fill(selectedRoot.begin(), selectedRoot.end(), 0);
    return selectedRoot;
  }
  auto isMatched{std::vector<bool>(nodes.size(), false)};
  auto patternMatched{std::vector<bool>(selection.patterns.size(), false)};
  for (size_t i = 0; i < nodes.size(); i++) {
    if (nodes[i].path.empty()) continue; // The root, which has no name.
    for (size_t j = 0; j < selection.patterns.size(); j++) {
      if (matchNodePath(selection.patterns[j], nodes[i].path)) {
        isMatched[i] = true;
        patternMatched[j] = true;
      }
    }
  }
  auto unmatched{std::vector<std::string>()};
  for (size_t j = 0; j < selection.patterns.size(); j++)
    if (!patternMatched[j]) unmatched.push_back(selection.patterns[j]);
  if (!unmatched.empty()) {
    auto message{smdl::concat(unmatched.size(),
                              " selection pattern(s) match "
                              "nothing in ",
                              smdl::QuotedPath(fileName), ":")};
    for (const auto &pattern : unmatched)
      message += smdl::concat("\n  ", smdl::Quoted(pattern));
    message += "\nThe file contains:";
    size_t numListed{};
    for (const auto &node : nodes) {
      if (node.path.empty()) continue;
      if (numListed++ == 32) {
        message +=
            smdl::concat("\n  ... and ", nodes.size() - numListed, " more");
        break;
      }
      message += smdl::concat("\n  ", smdl::Quoted(node.path));
    }
    message += "\nRun with -list-objects to see them with their geometry.";
    throw smdl::Error(std::move(message));
  }
  // One forward pass, which the preorder makes sufficient: a node inherits
  // whichever subtree root selected its parent, so a match nested inside
  // another match is folded into the outer one instead of being
  // instantiated a second time.
  for (size_t i = 0; i < nodes.size(); i++) {
    const auto parent{nodes[i].parent};
    const auto inherited{parent == INVALID_INDEX ? INVALID_INDEX
                                                 : selectedRoot[parent]};
    selectedRoot[i] = inherited != INVALID_INDEX ? inherited
                      : isMatched[i]             ? uint32_t(i)
                                                 : INVALID_INDEX;
  }
  return selectedRoot;
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

namespace {

// Read a scene file for listing only: the meshes come in because they carry
// `mMaterialIndex` and the face count, but no vertex data does and no
// acceleration structure is built. The node graph is flattened into `file`,
// whose mesh indices are the file's own since there are no `Scene::meshes`
// to offset them into.
[[nodiscard]]
const aiScene *readForListing(Assimp::Importer &assImporter,
                              const std::string &fileName, ImportFile &file) {
  configureImporter(assImporter, aiComponent_NORMALS |
                                     aiComponent_TANGENTS_AND_BITANGENTS |
                                     aiComponent_TEXCOORDS);
  auto assScene{
      assImporter.ReadFile(fileName.c_str(), MATERIAL_POSTPROCESS_FLAGS)};
  if (!assScene)
    throw smdl::Error(smdl::concat("assimp failed to read ",
                                   smdl::QuotedPath(fileName), ": ",
                                   assImporter.GetErrorString()));
  flattenNodes(*assScene->mRootNode, float4x4(1.0f), INVALID_INDEX, {}, 0,
               file);
  return assScene;
}

} // namespace

std::vector<MaterialUsage>
importMaterialUsage(const std::string &fileName,
                    const ObjectSelection &selection) {
  auto assImporter{Assimp::Importer{}};
  auto file{ImportFile()};
  auto assScene{readForListing(assImporter, fileName, file)};
  auto selectedRoot{resolveSelection(file.nodes, selection, fileName)};
  auto usage{std::vector<MaterialUsage>(assScene->mNumMaterials)};
  for (unsigned int i = 0; i < assScene->mNumMaterials; i++)
    usage[i].name = assScene->mMaterials[i]->GetName().C_Str();
  // A mesh that no selected node references is never hit, so it needs no
  // material; and a mesh referenced several times contributes its triangles
  // once but its instances every time.
  auto isCounted{std::vector<bool>(assScene->mNumMeshes, false)};
  for (const auto &placement : file.placements) {
    if (selectedRoot[placement.nodeIndex] == INVALID_INDEX) continue;
    const auto &assMesh{*assScene->mMeshes[placement.meshIndex]};
    if (assMesh.mMaterialIndex >= usage.size()) continue;
    auto &entry{usage[assMesh.mMaterialIndex]};
    entry.instanceCount++;
    if (!isCounted[placement.meshIndex]) {
      isCounted[placement.meshIndex] = true;
      entry.meshCount++;
      entry.triangleCount += assMesh.mNumFaces;
    }
  }
  usage.erase(std::remove_if(usage.begin(), usage.end(),
                             [](const MaterialUsage &entry) {
                               return entry.meshCount == 0;
                             }),
              usage.end());
  return usage;
}

std::vector<ObjectUsage> importObjectUsage(const std::string &fileName,
                                           ObjectFileInfo *info) {
  auto assImporter{Assimp::Importer{}};
  auto file{ImportFile()};
  auto assScene{readForListing(assImporter, fileName, file)};
  // Whatever the file says about itself, for a caller writing it down. The
  // FBX reader fills these from the file's own `GlobalSettings`; most other
  // formats leave them absent, either because the format fixes the answer or
  // because it never had one.
  if (info && assScene->mMetaData) {
    int value{};
    if (assScene->mMetaData->Get("UpAxis", value)) info->upAxis = value;
    if (assScene->mMetaData->Get("UpAxisSign", value)) info->upAxisSign = value;
    float unitScale{};
    if (assScene->mMetaData->Get("UnitScaleFactor", unitScale))
      info->unitsPerMeter = unitScale;
  }
  auto usage{std::vector<ObjectUsage>(file.nodes.size())};
  auto materialIndices{std::vector<std::vector<uint32_t>>(file.nodes.size())};
  for (const auto &placement : file.placements) {
    const auto &assMesh{*assScene->mMeshes[placement.meshIndex]};
    // Bound the placed vertices once, then merge that box into each
    // ancestor. Merging boxes is exact for a union, so this costs one pass
    // over the vertices rather than one per level.
    const auto &nodeToFile{file.nodes[placement.nodeIndex].nodeToFile};
    auto boundMin{float3(INF, INF, INF)};
    auto boundMax{float3(-INF, -INF, -INF)};
    for (unsigned int i = 0; i < assMesh.mNumVertices; i++) {
      const auto &vertex{assMesh.mVertices[i]};
      const auto point{
          float3(nodeToFile * float4(vertex.x, vertex.y, vertex.z, 1.0f))};
      for (size_t axis = 0; axis < 3; axis++) {
        boundMin[axis] = std::min(boundMin[axis], point[axis]);
        boundMax[axis] = std::max(boundMax[axis], point[axis]);
      }
    }
    // Charge the geometry to the node that places it and to every ancestor,
    // so that a subtree reports what selecting it would instantiate.
    for (auto i{placement.nodeIndex}; i != INVALID_INDEX;
         i = file.nodes[i].parent) {
      usage[i].instanceCount++;
      usage[i].triangleCount += assMesh.mNumFaces;
      for (size_t axis = 0; axis < 3; axis++) {
        usage[i].boundMin[axis] =
            std::min(usage[i].boundMin[axis], boundMin[axis]);
        usage[i].boundMax[axis] =
            std::max(usage[i].boundMax[axis], boundMax[axis]);
      }
      auto &indices{materialIndices[i]};
      if (std::find(indices.begin(), indices.end(), assMesh.mMaterialIndex) ==
          indices.end())
        indices.push_back(assMesh.mMaterialIndex);
    }
  }
  for (size_t i = 0; i < file.nodes.size(); i++) {
    usage[i].path = file.nodes[i].path;
    usage[i].depth =
        uint32_t(std::count(usage[i].path.begin(), usage[i].path.end(), '/'));
    usage[i].pivot = float3(file.nodes[i].nodeToFile[3]);
    for (auto materialIndex : materialIndices[i])
      if (materialIndex < assScene->mNumMaterials)
        usage[i].materialNames.push_back(
            assScene->mMaterials[materialIndex]->GetName().C_Str());
  }
  // The root aggregates the whole file, which is what a caller asking about
  // the file rather than about its objects wants. Read it before the filter
  // below removes it for having no name to select by.
  if (info && !usage.empty()) {
    info->boundMin = usage[0].boundMin;
    info->boundMax = usage[0].boundMax;
    info->materialNames = usage[0].materialNames;
    info->triangleCount = usage[0].triangleCount;
  }
  // The root has no name and so cannot be selected; an unnamed node cannot
  // either. Everything else is reported in preorder, as authored.
  usage.erase(std::remove_if(usage.begin(), usage.end(),
                             [](const ObjectUsage &entry) {
                               return entry.path.empty() ||
                                      entry.triangleCount == 0;
                             }),
              usage.end());
  return usage;
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
  // (`MeshInstance::materialIndex`). Displacement is the exception: the
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
      const auto &baseName{materialNames[meshes[meshIndex]->materialIndex]};
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
  const auto worldXfs{
      item.batchTransforms.empty()
          ? smdl::Span<const float4x4>(&item.objectToWorld, 1)
          : smdl::Span<const float4x4>(item.batchTransforms.data(),
                                       item.batchTransforms.size())};
  if (item.primitive.active()) {
    addPrimitive(item.primitive, worldXfs, item.materials);
  } else if (item.curves.active) {
    addCurves(item.fileName, worldXfs, item.curves, item.materials);
  } else {
    addMesh(item.fileName, worldXfs, item.selection, item.subdiv,
            item.materials);
  }
}

uint32_t Scene::addPrimitive(const PrimitiveSpec &spec,
                             smdl::Span<const float4x4> objectToWorlds,
                             const MaterialAssignment &materials) {
  // The same split `add()` gives meshes, without the displacement
  // exception: an analytic shape has no vertices for a material to
  // move, so the renames are always an instance-level fact.
  auto meshLevel{materials};
  meshLevel.renames.clear();
  const auto baseName{std::string(meshLevel.resolve(""))};
  auto key{spec.key() + "|" + baseName};
  auto entry{primitiveCache.find(key)};
  uint32_t primitiveIndex{};
  if (entry == primitiveCache.end()) {
    primitiveIndex = uint32_t(primitives.size());
    primitives.push_back(makePrimitive(device, spec, internMaterial(baseName)));
    primitiveCache.emplace(std::move(key), primitiveIndex);
    SMDL_LOG_DEBUG("Built ", spec.key(), ": ", primitivePieceCount(spec),
                   " piece(s), area ", primitives.back()->objectArea);
  } else {
    primitiveIndex = entry->second;
  }
  auto materialIndex{INVALID_INDEX};
  if (auto itr{materials.renames.find(baseName)};
      itr != materials.renames.end() && itr->second != baseName)
    materialIndex = internMaterial(itr->second);
  fileNames.push_back(smdl::concat("<", spec.name(), ">"));
  if (objectToWorlds.size() == 1)
    return addInstance(INVALID_INDEX, primitiveIndex, INVALID_INDEX,
                       objectToWorlds[0], spec.name(), materialIndex);
  return addInstanceArray(INVALID_INDEX, primitiveIndex, INVALID_INDEX,
                          objectToWorlds, float4x4(1.0f), spec.name(),
                          materialIndex);
}

uint32_t Scene::addCurves(const std::string &fileName,
                          smdl::Span<const float4x4> objectToWorlds,
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
        " strand(s), ", curves.back()->segmentCount(), " segment(s), ",
        CurvesFile::basisName(curves.back()->basis), " basis, ",
        spec.mode == CurvesSpec::Mode::RIBBON ? "ribbon" : "tube", " mode");
  } else {
    curvesIndex = entry->second;
    SMDL_LOG_DEBUG("Reusing ", smdl::QuotedPath(fileName), ": ",
                   curves[curvesIndex]->strandCount(), " strand(s)");
  }
  auto materialIndex{INVALID_INDEX};
  if (auto itr{materials.renames.find(baseName)};
      itr != materials.renames.end() && itr->second != baseName)
    materialIndex = internMaterial(itr->second);
  fileNames.push_back(fileName);
  if (objectToWorlds.size() == 1)
    return addInstance(INVALID_INDEX, INVALID_INDEX, curvesIndex,
                       objectToWorlds[0], fileName, materialIndex);
  return addInstanceArray(INVALID_INDEX, INVALID_INDEX, curvesIndex,
                          objectToWorlds, float4x4(1.0f), fileName,
                          materialIndex);
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

uint32_t Scene::addInstance(uint32_t meshIndex, uint32_t primitiveIndex,
                            uint32_t curvesIndex, const float4x4 &xf,
                            std::string_view fileName, uint32_t materialIndex) {
  // Embree gets the authored transform in full. It intersects in the
  // instance's own space and reports barycentrics, which are affine
  // invariant, so a sheared or non-uniformly scaled instance costs nothing
  // here and `makeHit()` rebuilds the world-space geometry from the same
  // matrix.
  auto instance{MeshInstance()};
  instance.setObjectToWorld(xf, fileName);
  instance.meshIndex = meshIndex;
  instance.primitiveIndex = primitiveIndex;
  instance.curvesIndex = curvesIndex;
  instance.materialIndex = materialIndex;
  auto inst{rtcNewGeometry(device, RTC_GEOMETRY_TYPE_INSTANCE)};
  rtcSetGeometryBuildQuality(inst, RTC_BUILD_QUALITY_HIGH);
  rtcSetGeometryTimeStepCount(inst, 1);
  rtcSetGeometryTransform(inst, 0, RTC_FORMAT_FLOAT4X4_COLUMN_MAJOR,
                          &instance.objectToWorld[0][0]);
  rtcSetGeometryInstancedScene(inst, curvesIndex != INVALID_INDEX
                                         ? curves[curvesIndex]->scene
                                     : primitiveIndex != INVALID_INDEX
                                         ? primitives[primitiveIndex]->scene
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

uint32_t Scene::addInstanceArray(uint32_t meshIndex, uint32_t primitiveIndex,
                                 uint32_t curvesIndex,
                                 smdl::Span<const float4x4> worldXfs,
                                 const float4x4 &nodeXf,
                                 std::string_view fileName,
                                 uint32_t materialIndex) {
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
  rtcSetGeometryInstancedScene(geometry, curvesIndex != INVALID_INDEX
                                             ? curves[curvesIndex]->scene
                                         : primitiveIndex != INVALID_INDEX
                                             ? primitives[primitiveIndex]->scene
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
    instance.primitiveIndex = primitiveIndex;
    instance.curvesIndex = curvesIndex;
    instance.materialIndex = materialIndex;
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
  mesh->materialIndex = internMaterial(materialName);
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

void Scene::preCommitBounds(float3 &lower, float3 &upper) const {
  lower = float3(+INF, +INF, +INF);
  upper = float3(-INF, -INF, -INF);
  for (const auto &instance : meshInstances) {
    auto fold{[&](const float3 &point) {
      const auto world{float3(instance.objectToWorld * float4(point, 1.0f))};
      for (int axis = 0; axis < 3; axis++) {
        lower[axis] = std::min(lower[axis], world[axis]);
        upper[axis] = std::max(upper[axis], world[axis]);
      }
    }};
    if (instance.isPrimitive()) {
      for (const auto &point : primitives[instance.primitiveIndex]->proxyPoints)
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
                 ", ", boundCenter.z, ") radius ", boundRadius);
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
  // so only those have to resolve. Scene files routinely declare
  // materials nothing uses, and a mesh whose every instance overrides
  // its material away leaves the mesh's own name legitimately
  // unresolved.
  auto isUsed{std::vector<bool>(materials.size(), false)};
  for (const auto &meshInstance : meshInstances)
    isUsed[materialIndexOf(meshInstance)] = true;
  auto unresolved{std::vector<std::string>()};
  for (size_t i = 0; i < materials.size(); i++) {
    materials[i] = compiler.findMaterial(materialNames[i]);
    if (!materials[i] && isUsed[i]) {
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
               "pass -material-fallback=<name> to substitute a material.";
    throw smdl::Error(std::move(message));
  }
}

namespace {

// Weld the mesh's vertices by exact position bits: `weldOf[i]` is the
// welded group of vertex `i`, and groups number `0` to the returned count
// minus one, in first-encounter order, so the grouping is deterministic
// regardless of the hashing underneath. See `positionKey()` for why
// exactness is the point.
[[nodiscard]] uint32_t weldByPosition(const Mesh &mesh,
                                      std::vector<uint32_t> &weldOf) {
  auto groups{
      std::unordered_map<std::array<uint32_t, 3>, uint32_t, WeldHash>()};
  groups.reserve(mesh.verts.size());
  weldOf.resize(mesh.verts.size());
  for (size_t i = 0; i < mesh.verts.size(); i++)
    weldOf[i] = groups
                    .try_emplace(positionKey(mesh.verts[i].point),
                                 uint32_t(groups.size()))
                    .first->second;
  return uint32_t(groups.size());
}

// Recompute shading normals from the triangles: area-weighted face
// normals accumulated over position-welded vertices, so the result is
// smooth and cannot crack along texture seams. Hard edges smooth out
// with everything else, which is the accepted trade of recomputing
// normals on a surface that subdivision or displacement just changed.
void recomputeNormals(Mesh &mesh) {
  auto weldOf{std::vector<uint32_t>()};
  const auto numGroups{weldByPosition(mesh, weldOf)};
  auto sums{std::vector<float3>(numGroups)};
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
  std::atomic<uint32_t> numDisplaced{0};
  llvm::parallelFor(0, pending.size(), [&](size_t k) {
    if (finalizeMesh(*meshes[pending[k]], wavelengths)) numDisplaced++;
  });
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

bool Scene::finalizeMesh(Mesh &mesh, const Color &wavelengths) {
  if (mesh.subdiv.levels > 0) {
    // Smooth refinement carries exact limit normals out of the refiner;
    // linear refinement, a degenerate limit normal, or the fallback
    // triangulation all leave the geometry as the only authority.
    if (!subdivideMesh(mesh)) recomputeNormals(mesh);
    recomputeTangents(mesh);
  }
  auto displaced{false};
  if (mesh.subdiv.displace) {
    displaced = displaceMesh(mesh, wavelengths);
    if (displaced) {
      // The surface changed; the shading frame must follow it.
      recomputeNormals(mesh);
      recomputeTangents(mesh);
    }
  }
  buildMeshGeometry(mesh);
  mesh.needsFinalize = false;
  mesh.basePoints = {};
  mesh.baseTexcoords = {};
  mesh.baseFaceCounts = {};
  mesh.baseIndices = {};
  return displaced;
}

bool Scene::displaceMesh(Mesh &mesh, const Color &wavelengths) {
  SMDL_SANITY_CHECK(mesh.materialIndex < materials.size());
  const auto *material{materials[mesh.materialIndex]};
  if (!material || material->hasZeroDisplacement()) return false;
  // One offset per position-welded vertex, averaged over the split
  // copies (whose UVs may disagree along texture seams) and applied to
  // every copy, so the displaced surface cannot crack. Well-authored
  // displacement matches across seams, where the averaging is a no-op.
  auto weldOf{std::vector<uint32_t>()};
  const auto numGroups{weldByPosition(mesh, weldOf)};
  auto offsets{std::vector<float3>(numGroups)};
  auto counts{std::vector<uint32_t>(numGroups)};
  for (size_t i = 0; i < mesh.verts.size(); i++) {
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
    state.finalizeAndApplyInternalSpaceConventions();
    auto displacement{float3()};
    material->displacementEvaluate(state, displacement);
    // Internal space is the tangent frame, so the vector maps back
    // through it: `d.x` along U, `d.y` along V, `d.z` along the normal.
    offsets[weldOf[i]] = offsets[weldOf[i]] + displacement.x * tangent +
                         displacement.y * bitangent + displacement.z * normal;
    counts[weldOf[i]]++;
  }
  auto anyMoved{false};
  for (uint32_t g = 0; g < numGroups; g++) {
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
  mesh->materialIndex = assMesh.mMaterialIndex < materialRemap.size()
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
    const auto meshInstanceIndex{
        instanceIndexOf(rayHit.hit.instID[0], rayHit.hit.instPrimID[0])};
    // A curve hit is built here rather than through `makeHit()`, because
    // it needs the ray: the point comes from `tfar`, the tube normal
    // from the object-space `Ng`, and the ribbon normal from the ray
    // direction. The (u, v) are the curve parameters, NOT barycentrics,
    // so the triangle path's clamp-and-complement does not apply
    // (ribbon `v` legitimately spans -1 to +1).
    if (meshInstances[meshInstanceIndex].isCurves()) {
      hit = makeCurvesHit(
          meshInstanceIndex, rayHit.hit.primID, rayHit.hit.u, rayHit.hit.v,
          float3(rayHit.hit.Ng_x, rayHit.hit.Ng_y, rayHit.hit.Ng_z),
          ray(rayHit.ray.tfar), ray.dir);
      return true;
    }
    auto clamp01{
        [](float value) { return std::max(0.0f, std::min(1.0f, value)); }};
    auto bary{float3(clamp01(1.0f - rayHit.hit.u - rayHit.hit.v),
                     clamp01(rayHit.hit.u), clamp01(rayHit.hit.v))};
    hit = makeHit(meshInstanceIndex, rayHit.hit.primID, bary);
    return true;
  }
}

Hit Scene::makeHit(uint32_t meshInstanceIndex, uint32_t faceIndex,
                   const float3 &bary) const {
  const auto &meshInstance{meshInstances[meshInstanceIndex]};
  if (meshInstance.isPrimitive())
    return makePrimitiveHit(meshInstanceIndex, faceIndex, bary);
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
  auto edge1{smdl::normalize(point1 - point0)};
  auto edge2{smdl::normalize(point2 - point0)};
  auto barycentric{[&](auto member) {
    return bary[0] * vert0.*member + bary[1] * vert1.*member +
           bary[2] * vert2.*member;
  }};
  Hit hit{};
  hit.meshInstanceIndex = meshInstanceIndex;
  hit.meshIndex = meshInstance.meshIndex;
  hit.faceIndex = faceIndex;
  hit.materialIndex = materialIndexOf(meshInstance);
  SMDL_SANITY_CHECK(hit.materialIndex < materials.size());
  hit.material = materials[hit.materialIndex];
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
  hit.geometryNormal = normalize(cross(edge1, edge2));
  if (meshInstance.flipsWinding) {
    hit.normal = -hit.normal;
    hit.geometryNormal = -hit.geometryNormal;
  }
  hit.geometryTangent = edge1;
  hit.texcoord = barycentric(&Mesh::Vert::texcoord);
  hit.textureDensity = uvTextureDensity(point0, point1, point2, vert0.texcoord,
                                        vert1.texcoord, vert2.texcoord);
  hit.instance = &meshInstance;
  return hit;
}

Hit Scene::makePrimitiveHit(uint32_t meshInstanceIndex, uint32_t primID,
                            const float3 &bary) const {
  const auto &meshInstance{meshInstances[meshInstanceIndex]};
  const auto &primitive{*primitives[meshInstance.primitiveIndex]};
  // The (u, v) ride in the barycentric slots, exactly as
  // `Scene::intersect()` packed them; see `Primitive.h`.
  const auto surface{
      evalPrimitiveSurface(primitive.spec, primID, float2(bary[1], bary[2]))};
  const auto &objectToWorld{meshInstance.objectToWorld};
  Hit hit{};
  hit.meshInstanceIndex = meshInstanceIndex;
  hit.meshIndex = INVALID_INDEX;
  hit.faceIndex = primID;
  hit.materialIndex = materialIndexOf(meshInstance);
  SMDL_SANITY_CHECK(hit.materialIndex < materials.size());
  hit.material = materials[hit.materialIndex];
  hit.bary = bary;
  hit.point = float3(objectToWorld * float4(surface.point, 1.0f));
  // The analytic normal transforms by the cofactor matrix like any
  // other; a mirroring instance flips its image inward, and the same
  // correction the mesh path applies flips it back out.
  hit.normal = normalize(meshInstance.normalMatrix * surface.normal);
  if (meshInstance.flipsWinding) hit.normal = -hit.normal;
  // Shading and geometric agree by construction: that is the point of
  // an analytic surface.
  hit.geometryNormal = hit.normal;
  const auto dPduWorld{float3(objectToWorld * float4(surface.dPdu, 0.0f))};
  const auto dPdvWorld{float3(objectToWorld * float4(surface.dPdv, 0.0f))};
  auto tangent{dPduWorld};
  hit.tangent =
      smdl::tryNormalize(tangent) ? tangent : smdl::perpendicularTo(hit.normal);
  hit.geometryTangent = hit.tangent;
  hit.texcoord = float2(bary[1], bary[2]);
  // UV area per world area, which is what the ray-cone footprint
  // multiplies into a texture-space filter width: exactly the triangle
  // path's quantity, from the parametric partials instead of the edges.
  const auto patchArea{length(cross(dPduWorld, dPdvWorld))};
  hit.textureDensity = patchArea > 1e-12f ? 1.0f / patchArea : 0.0f;
  hit.instance = &meshInstance;
  return hit;
}

Hit Scene::makeCurvesHit(uint32_t meshInstanceIndex, uint32_t primID, float u,
                         float v, const float3 &objectNg,
                         const float3 &worldPoint, const float3 &rayDir) const {
  const auto &meshInstance{meshInstances[meshInstanceIndex]};
  const auto &groom{*curves[meshInstance.curvesIndex]};
  const auto &objectToWorld{meshInstance.objectToWorld};
  const auto materialIndex{materialIndexOf(meshInstance)};
  SMDL_SANITY_CHECK(materialIndex < materials.size());
  const auto *material{materials[materialIndex]};
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
  const auto strand{groom.segmentStrand[primID]};
  const auto firstSegment{groom.strandFirstSegment[strand]};
  const auto numSegments{groom.strandFirstSegment[strand + 1] - firstSegment};
  const auto strandU{(float(primID - firstSegment) + u) / float(numSegments)};
  const auto vAcross{ribbon ? 0.5f * (v + 1.0f) : 0.0f};
  Hit hit{};
  hit.meshInstanceIndex = meshInstanceIndex;
  hit.meshIndex = INVALID_INDEX;
  hit.faceIndex = primID;
  hit.materialIndex = materialIndex;
  hit.material = material;
  hit.bary = float3(0.0f, u, vAcross);
  hit.point = worldPoint;
  hit.normal = normal;
  hit.tangent = tangent;
  hit.geometryNormal = normal;
  hit.geometryTangent = tangent;
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
