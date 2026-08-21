#include "MeshImportAssimp.h"

#include "assimp/config.h"

#include "llvm/Support/JSON.h"

#include "smdl/Support/Error.h"
#include "smdl/Support/StringHelpers.h"

#include <algorithm>
#include <cmath>
#include <cstdio>

void configureImporter(Assimp::Importer &importer,
                       unsigned extraRemovedComponents) {
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

const aiScene *readLossless(Assimp::Importer &importer,
                            const std::string &fileName) {
  // Not a call to `configureImporter()` with the removals cleared: that
  // function also suppresses the FBX reader's textures, cameras, lights,
  // animations and weights at the source, and those are exactly what a
  // report about the file has to see.
  importer.SetPropertyBool(AI_CONFIG_IMPORT_NO_SKELETON_MESHES, true);
  auto assScene{importer.ReadFile(fileName.c_str(), 0)};
  if (!assScene)
    throw smdl::Error(smdl::concat("assimp failed to read ",
                                   smdl::QuotedPath(fileName), ": ",
                                   importer.GetErrorString()));
  return assScene;
}

void flattenNodes(const aiNode &assNode, const float4x4 &parentXf,
                  uint32_t parentIndex, std::string_view parentPath,
                  uint32_t meshBase, ImportFile &file) {
  // The file transform composes on the left of the whole node chain.
  const auto xf{parentXf * fromAssimp(assNode.mTransformation)};
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

namespace {

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

namespace {

// Read a scene file for listing only: the meshes come in because they carry
// `mMaterialIndex` and the face count, but no vertex data does and no
// acceleration structure is built. The node graph is flattened into `file`,
// whose mesh indices are the file's own, since a listing has no mesh array
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
    if (assScene->mMetaData->Get("UnitScaleFactor", unitScale) && unitScale > 0)
      info->metersPerUnit = unitScale / 100.0f;
  }
  auto usage{std::vector<ObjectUsage>(file.nodes.size())};
  auto materialIndices{std::vector<std::vector<uint32_t>>(file.nodes.size())};
  for (const auto &placement : file.placements) {
    const auto &assMesh{*assScene->mMeshes[placement.meshIndex]};
    // Bound the placed vertices once, then merge that box into each
    // ancestor. Merging boxes is exact for a union, so this costs one pass
    // over the vertices rather than one per level.
    const auto &nodeToFile{file.nodes[placement.nodeIndex].nodeToFile};
    BoundBox3 bound{};
    for (unsigned int i = 0; i < assMesh.mNumVertices; i++) {
      const auto &vertex{assMesh.mVertices[i]};
      bound.extend(
          float3(nodeToFile * float4(vertex.x, vertex.y, vertex.z, 1.0f)));
    }
    // Charge the geometry to the node that places it and to every ancestor,
    // so that a subtree reports what selecting it would instantiate.
    for (auto i{placement.nodeIndex}; i != INVALID_INDEX;
         i = file.nodes[i].parent) {
      usage[i].instanceCount++;
      usage[i].triangleCount += assMesh.mNumFaces;
      usage[i].bound.extend(bound);
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
    for (auto matIndex : materialIndices[i])
      if (matIndex < assScene->mNumMaterials)
        usage[i].materialNames.push_back(
            assScene->mMaterials[matIndex]->GetName().C_Str());
  }
  // The root aggregates the whole file, which is what a caller asking about
  // the file rather than about its objects wants. Read it before the filter
  // below removes it for having no name to select by.
  if (info && !usage.empty()) {
    info->bound = usage[0].bound;
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

namespace {

// A float with enough digits to read back exactly. JSON has no infinity,
// and empty bounds are a real answer rather than an error, so a
// non-finite value is reported as the absence of a number.
[[nodiscard]] std::string jsonFloat(float value) {
  if (!std::isfinite(value)) return "null";
  char buffer[32]{};
  std::snprintf(buffer, sizeof(buffer), "%.9g", double(value));
  return buffer;
}

// Coordinate triples are written whole rather than through `array()`,
// which would put every component on its own line and bury the three
// numbers a reader is scanning for.
void jsonFloat3(llvm::json::OStream &json, const float3 &values) {
  json.rawValue(smdl::concat("[", jsonFloat(values.x), ", ",
                             jsonFloat(values.y), ", ", jsonFloat(values.z),
                             "]"));
}

void jsonStrings(llvm::json::OStream &json,
                 const std::vector<std::string> &values) {
  json.array([&] {
    for (const auto &value : values) json.value(value);
  });
}

} // namespace

void objectListingJSON(llvm::json::OStream &json, std::string_view fileName,
                       const ObjectFileInfo &info,
                       const std::vector<ObjectUsage> &usage) {
  json.object([&] {
    json.attribute("file", llvm::StringRef(fileName.data(), fileName.size()));
    json.attribute("triangles", info.triangleCount);
    json.attribute("up_axis", info.upAxis);
    json.attribute("up_axis_sign", info.upAxisSign);
    json.attributeBegin("meters_per_unit");
    json.rawValue(jsonFloat(info.metersPerUnit));
    json.attributeEnd();
    json.attributeArray("bounds", [&] {
      jsonFloat3(json, info.bound.lower);
      jsonFloat3(json, info.bound.upper);
    });
    json.attributeBegin("materials");
    jsonStrings(json, info.materialNames);
    json.attributeEnd();
    json.attributeArray("objects", [&] {
      for (const auto &entry : usage)
        json.object([&] {
          json.attribute("path", entry.path);
          json.attribute("depth", entry.depth);
          json.attribute("triangles", entry.triangleCount);
          json.attribute("instances", entry.instanceCount);
          json.attributeBegin("materials");
          jsonStrings(json, entry.materialNames);
          json.attributeEnd();
          json.attributeBegin("pivot");
          jsonFloat3(json, entry.pivot);
          json.attributeEnd();
          json.attributeArray("bounds", [&] {
            jsonFloat3(json, entry.bound.lower);
            jsonFloat3(json, entry.bound.upper);
          });
        });
    });
  });
}
