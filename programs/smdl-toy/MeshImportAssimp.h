/// \file
/// The assimp-facing half of the shared import: how a scene file is read,
/// and how its node graph is flattened into an `ImportFile`.
///
/// Include this only from a source file that already talks to assimp.
/// `MeshImport.h` is the assimp-free surface and is what headers should
/// include.
///
#pragma once

#include "assimp/Importer.hpp"
#include "assimp/SceneCombiner.h"
#include "assimp/postprocess.h"
#include "assimp/scene.h"

#include "MeshImport.h"

/// An assimp matrix as a `float4x4`. assimp stores rows, this stores
/// columns; both denote the same map, so nothing about the composition
/// order changes with the conversion.
[[nodiscard]] inline float4x4 fromAssimp(const aiMatrix4x4 &m) noexcept {
  return float4x4{
      float4{m.a1, m.b1, m.c1, m.d1}, float4{m.a2, m.b2, m.c2, m.d2},
      float4{m.a3, m.b3, m.c3, m.d3}, float4{m.a4, m.b4, m.c4, m.d4}};
}

/// A `float4x4` as an assimp matrix.
[[nodiscard]] inline aiMatrix4x4 toAssimp(const float4x4 &m) noexcept {
  return aiMatrix4x4{m[0][0], m[1][0], m[2][0], m[3][0], //
                     m[0][1], m[1][1], m[2][1], m[3][1], //
                     m[0][2], m[1][2], m[2][2], m[3][2], //
                     m[0][3], m[1][3], m[2][3], m[3][3]};
}

/// Configure an importer to read the least that assimp can be made to
/// read: triangle geometry, one texture coordinate set, and material
/// names. That is everything the renderer takes from a scene file, since
/// SMDL loads every texture itself and the material name is the only
/// thing a material contributes.
///
/// A listing built on this therefore answers "what will the renderer
/// see" rather than "what is in the file". The configuration is lossy by
/// design, so anything meaning to write a file back out must read it
/// some other way.
///
/// `extraRemovedComponents` drops more per-vertex data on top of the
/// baseline, for callers that do not build geometry at all.
///
/// \note `aiComponent_MATERIALS` must NEVER appear in the removal mask.
///       It deletes every material and substitutes one generated
///       default, which destroys the only thing we are here to read.
///
void configureImporter(Assimp::Importer &importer,
                       unsigned extraRemovedComponents = 0);

/// The post-processing for a material-usage-only load, which needs the
/// meshes (they carry `mMaterialIndex`) but none of the vertex data.
///
/// `aiProcess_FindInstances` is deliberately absent even though a full
/// geometry load enables it. The listing answers "how much geometry needs
/// this material", which is a question about the file as authored:
/// deduplicating first would report one shared copy where the file has
/// sixty-four, and undercount the triangles accordingly.
///
constexpr unsigned MATERIAL_POSTPROCESS_FLAGS =
    aiProcess_RemoveComponent | aiProcess_Triangulate | aiProcess_SortByPType;

/// Flatten a node graph into nodes and placements, in preorder, so that a
/// node always precedes its descendants.
///
/// `meshBase` offsets the file's own mesh indices into the caller's mesh
/// array; a listing that builds no meshes passes zero and reads the
/// file's indices straight back.
///
void flattenNodes(const aiNode &assNode, const float4x4 &parentXf,
                  uint32_t parentIndex, std::string_view parentPath,
                  uint32_t meshBase, ImportFile &file);

/// Read a scene file keeping everything in it: no components removed, no
/// triangulation, no welding, no generated normals, no flattening.
///
/// This is the read for a caller that reports or rewrites what a file
/// actually contains, and it is deliberately the opposite of
/// `configureImporter()`. The two answer different questions and the
/// answers differ: the listing read reports triangles after
/// triangulation and vertices after welding, this one reports the
/// polygons and the vertices the file was authored with. Neither number
/// is wrong and they must never be presented as the same number.
///
/// Skeleton mesh generation is off, since it adds geometry the file does
/// not have. Structure validation is off too, because
/// `aiProcess_ValidateDataStructure` repairs what it finds, and a read
/// that repairs cannot report.
///
/// The returned scene belongs to `importer`. Call
/// `Assimp::Importer::GetOrphanedScene()` to take it.
///
/// 	hrows smdl::Error  If assimp cannot read the file.
///
[[nodiscard]] const aiScene *readLossless(Assimp::Importer &importer,
                                          const std::string &fileName);
