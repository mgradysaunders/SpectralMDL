/// \file
/// Reading a scene file: what its node graph contains, and what a
/// selection of it stands for.
///
/// Everything here goes through one assimp configuration, so node paths,
/// pivots, material names and triangle counts are what the renderer
/// itself will see rather than a second reader's opinion of the file.
///
/// Deliberately free of assimp, so that including it costs nothing.
/// The assimp-facing pieces live in `MeshImportAssimp.h`.
///
#pragma once

#include <algorithm>
#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

#include "smdl/Support/VectorMath.h"

namespace llvm::json {
class OStream;
} // namespace llvm::json

using namespace smdl::vector_type_aliases;
using namespace smdl::matrix_type_aliases;

// The math constants the programs spell unqualified. They live in the
// library so that host and library agree on them by construction.
using smdl::INF;
using smdl::ONE_MINUS_EPS;
using smdl::PI;
using smdl::TWO_PI;

constexpr uint32_t INVALID_INDEX = uint32_t(-1);

/// An axis-aligned box, accumulated by folding in points or other
/// boxes. The default is the empty box, whose `lower` exceeds its
/// `upper` on every axis, so folding the first point in makes it that
/// point and `isEmpty()` reports what "no geometry at all" looks like.
class BoundBox3 final {
public:
  BoundBox3() = default;

  BoundBox3(const float3 &lower, const float3 &upper)
      : lower(lower), upper(upper) {}

  void extend(const float3 &point) noexcept {
    for (int axis = 0; axis < 3; axis++) {
      lower[axis] = std::min(lower[axis], point[axis]);
      upper[axis] = std::max(upper[axis], point[axis]);
    }
  }

  void extend(const BoundBox3 &other) noexcept {
    extend(other.lower);
    extend(other.upper);
  }

  /// Did nothing ever fold in?
  [[nodiscard]] bool isEmpty() const noexcept { return !(lower.x <= upper.x); }

  [[nodiscard]] float3 center() const noexcept {
    return 0.5f * (lower + upper);
  }

  [[nodiscard]] float3 extent() const noexcept { return upper - lower; }

  float3 lower{+INF, +INF, +INF};
  float3 upper{-INF, -INF, -INF};
};

/// Which objects of a mesh file to instantiate, and how to pivot them.
///
/// A pattern containing `/` matches an `ImportNode::path`; one without
/// matches only the last component of it, so that the wrapper node an
/// exporter puts above everything does not have to be spelled out. Both
/// support the `*` and `?` wildcards. A match takes the matched node's
/// **whole subtree**, since one authored object may well be a subtree,
/// and a match nested inside another match is dropped rather than
/// instantiated twice.
///
class ObjectSelection final {
public:
  /// The patterns, unioned. Empty means the whole file, which is what an
  /// `asset` with no `select` asks for.
  std::vector<std::string> patterns{};

  /// Remove the **translation** of the matched node's accumulated
  /// transform, keeping its rotation and scale, so that an object
  /// authored in place arrives at the origin ready to be put somewhere
  /// else. With no patterns this recenters the file as a whole.
  bool recenter{};

  /// A key that distinguishes two selections of the same file, for
  /// callers that cache or merge by (file, selection).
  [[nodiscard]] std::string key() const {
    auto result{std::string(recenter ? "recenter" : "")};
    for (const auto &pattern : patterns) result += '\n', result += pattern;
    return result;
  }
};

/// One node of an imported file's graph, flattened.
///
/// The `path` is what a selection pattern matches against. It joins the node
/// names from the root with `/`, **leaving the root node's own name out**,
/// because that name is importer trivia rather than anything the file's
/// author chose: assimp calls it `ROOT` for glTF and `RootNode` elsewhere.
/// The root node itself therefore has an empty path.
///
class ImportNode final {
public:
  std::string path{};                  ///< The `/`-joined path.
  float4x4 nodeToFile{float4x4(1.0f)}; ///< Accumulated from the root.
  uint32_t parent{INVALID_INDEX};      ///< The parent, or `INVALID_INDEX`.
};

/// Where one file's node graph puts one of its meshes: the node that places
/// it and the mesh it places, with the file transform left out so that the
/// pair can be replayed at a different placement.
///
/// The mesh index is the file's own, offset by whatever `meshBase`
/// `flattenNodes()` was given, so that several files can share one mesh
/// array.
///
class Placement final {
public:
  uint32_t meshIndex{}; ///< The index in the caller's mesh array.
  uint32_t nodeIndex{}; ///< The index in the `ImportFile::nodes` array.
};

/// One scene file's node graph, flattened.
///
/// The nodes are in preorder, so a node always precedes its descendants,
/// which is what lets `resolveSelection()` propagate a match down a subtree
/// in one forward pass.
///
class ImportFile final {
public:
  std::vector<ImportNode> nodes{};
  std::vector<Placement> placements{};
};

/// Resolve a selection against a node table.
///
/// Returns, for each node, the index of the matched node it is selected
/// through, or `INVALID_INDEX` if it is not selected at all. That index is
/// the subtree root whose translation `ObjectSelection::recenter` removes,
/// which is why this reports the ancestor rather than a plain yes or no.
///
/// With no patterns every node is selected through the root, so that
/// `recenter` still has a well defined meaning.
///
/// \throws smdl::Error  If any pattern matches nothing, listing the names
///                      the file does have. A pattern that silently selects
///                      nothing renders an empty image, which is the worst
///                      way to find out about a typo.
///
[[nodiscard]] std::vector<uint32_t>
resolveSelection(const std::vector<ImportNode> &nodes,
                 const ObjectSelection &selection, std::string_view fileName);

/// How one material name is used by a scene file.
class MaterialUsage final {
public:
  std::string name{};       ///< The name as the scene file spells it.
  uint32_t meshCount{};     ///< The number of meshes that reference it.
  uint32_t instanceCount{}; ///< The number of instantiated references.
  uint64_t triangleCount{}; ///< The number of triangles that reference it.
};

/// One selectable object of a scene file: a node whose subtree places
/// geometry, reported as the file authored it.
class ObjectUsage final {
public:
  std::string path{};       ///< The path a pattern would match.
  uint32_t depth{};         ///< The depth below the root, for indenting.
  uint32_t instanceCount{}; ///< The meshes the subtree places.
  uint64_t triangleCount{}; ///< The triangles the subtree places.

  /// The distinct materials the subtree uses, in the order encountered.
  std::vector<std::string> materialNames{};

  /// The translation of the node's accumulated transform, which is what
  /// `ObjectSelection::recenter` removes. Reported so that a tool building
  /// a stand-in for this object can put its origin where the renderer will.
  float3 pivot{};

  /// The axis-aligned bounds of the subtree, in the file's own space.
  ///
  /// Exact rather than conservative: the vertices are transformed and then
  /// bounded, not the other way around. A tool comparing these against what
  /// some other importer produced for the same file learns the axis and unit
  /// conventions that separate the two, which beats guessing them from the
  /// file extension.
  ///
  /// Empty if the subtree places no geometry.
  BoundBox3 bound{};
};

/// Import only the material usage of a scene file.
///
/// This reports the material names that the scene actually needs: the ones
/// on a mesh that the node graph instantiates and `selection` keeps, which
/// are exactly the ones a ray can hit. Materials that the file declares and
/// never uses are left out. Vertex data is dropped on the way in and no
/// acceleration structure is built, so this is much cheaper than
/// constructing a `Scene`.
///
/// \throws smdl::Error  If assimp cannot read the file, or if a selection
///                      pattern matches nothing.
///
[[nodiscard]] std::vector<MaterialUsage>
importMaterialUsage(const std::string &fileName,
                    const ObjectSelection &selection = {});

/// What a scene file declares about its own conventions, where it declares
/// anything at all.
///
/// Nothing in the renderer acts on this: assimp reports the file in the
/// file's own space and the renderer places it there, so a convention is
/// something a composition or an `.asset` manifest states rather than
/// something anyone infers. It is reported because a tool writing such a
/// manifest would otherwise be guessing from the file extension, and an FBX
/// that says it is Y-up in centimeters should not have to be guessed at.
///
class ObjectFileInfo final {
public:
  /// The up axis as 0, 1 or 2, or -1 if the file does not say.
  int upAxis{-1};

  /// The sign of the up axis, 1 unless the file says otherwise.
  int upAxisSign{1};

  /// The meters one file unit spans, or 0 if the file does not say.
  ///
  /// FBX is the only format here that says. Its `UnitScaleFactor` is
  /// centimeters per unit, so an FBX authored in centimeters stores 1
  /// and one authored in meters stores 100; this reports that divided by
  /// a hundred, which is the factor that converts the file to meters and
  /// so is exactly what an `.asset` manifest's `scale` key wants.
  ///
  float metersPerUnit{};

  /// The bounds of everything the file places, in the file's own space.
  ///
  /// Reported separately from the object listing because a file whose
  /// geometry sits on its unnamed root node offers nothing to `select` and
  /// so lists no objects at all, while still being perfectly placeable as a
  /// whole. A tool preparing such a file needs its size from somewhere.
  ///
  BoundBox3 bound{};

  /// Every material name the file uses, in the order encountered.
  std::vector<std::string> materialNames{};

  /// Every triangle the file places, including those on nodes that have no
  /// name to be selected by and so appear in no object listing.
  uint64_t triangleCount{};
};

/// Import only the object listing of a scene file: what `select` can name,
/// and how much geometry each name stands for. Reported in preorder, as the
/// file is authored, with no selection applied, since this is what a user
/// reads to write a selection in the first place.
///
/// \throws smdl::Error  If assimp cannot read the file.
///
[[nodiscard]] std::vector<ObjectUsage>
importObjectUsage(const std::string &fileName, ObjectFileInfo *info = nullptr);

/// Write one file's listing as a JSON object into `json`, which must be
/// positioned where a value is expected, inside the `{"files": [...]}`
/// envelope that the asset preparation tooling reads
/// (`etc/prepare_asset.py`). The shape is a contract with that tooling.
/// Pure formatting: the caller does the import and writes the envelope.
///
void objectListingJSON(llvm::json::OStream &json, std::string_view fileName,
                       const ObjectFileInfo &info,
                       const std::vector<ObjectUsage> &usage);
