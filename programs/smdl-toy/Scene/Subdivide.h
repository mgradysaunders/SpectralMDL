#pragma once

#include <array>
#include <cstring>
#include <utility>

#include "Scene/Scene.h"

/// The exact position bits that welding keys on, component-wise because
/// `float3` is padded to 16 bytes by its alignment. Exactness is the
/// point, not a limitation: copies that exist only to carry different
/// UVs or normals (texture seams, hard edges) were split from one
/// authored vertex and have bit-identical positions, and anything less
/// than exact welding would move them apart.
[[nodiscard]] inline std::array<uint32_t, 3> positionKey(const float3 &point) {
  auto result{std::array<uint32_t, 3>{}};
  std::memcpy(&result[0], &point.x, sizeof(float));
  std::memcpy(&result[1], &point.y, sizeof(float));
  std::memcpy(&result[2], &point.z, sizeof(float));
  return result;
}

/// The hash behind the unordered welding maps, which want the linear
/// scaling of hashing rather than the n log n of ordering: position keys
/// and the (vertex, corner) pairs that subdivision uniques its output
/// vertices by, each word run through the 64-bit murmur3 finalizer.
class WeldHash final {
public:
  [[nodiscard]] size_t
  operator()(const std::array<uint32_t, 3> &key) const noexcept {
    uint64_t hash{0x9E3779B97F4A7C15ull};
    for (const auto word : key) hash = mix(hash ^ word);
    return size_t(hash);
  }

  [[nodiscard]] size_t
  operator()(const std::pair<int, int> &key) const noexcept {
    return size_t(
        mix((uint64_t(uint32_t(key.first)) << 32) | uint32_t(key.second)));
  }

private:
  /// The murmur3 64-bit finalizer.
  [[nodiscard]] static uint64_t mix(uint64_t x) noexcept {
    x ^= x >> 33;
    x *= 0xFF51AFD7ED558CCDull;
    x ^= x >> 33;
    x *= 0xC4CEB9FE1A85EC53ull;
    x ^= x >> 33;
    return x;
  }
};

/// Uniformly refine `mesh` per `mesh.subdiv` with OpenSubdiv, consuming
/// the base polygon arrays and filling `verts` and `faces` with the
/// triangulated result. A deforming mesh (`basePointsShut` present)
/// refines its shut key through the same refiner into `vertsShut`, which
/// is parallel to `verts` by construction: one topology, welded by the
/// open key's positions, two point buffers.
///
/// The spec's scheme and its smoothing are independent, and make a 2x2:
///
/// | | `smooth` | `!smooth` |
/// |---|---|---|
/// | `CATMARK` | Catmull-Clark, limit surface | bilinear split to quads |
/// | `LOOP` | Loop, limit surface | 1-to-4 split, vertices unmoved |
///
/// Both splits run with `EDGE_AND_CORNER` vertex boundaries, so an open
/// ground plane keeps its corners. Smooth refinement snaps the last
/// level to the limit surface and carries UVs and vertex colors as one
/// face-varying channel that is smooth in the interior but linear along
/// boundaries and seams, so texture seams cannot slide. Linear
/// refinement (`smooth` false) leaves positions, UVs, and colors where
/// linear interpolation puts them, which raises the density of the mesh
/// without moving its surface; the Loop
/// half of that row is the uniform triangle split, which costs two
/// thirds of the triangles the quad split does and keeps the shape of
/// the triangles it refines.
///
/// The Loop scheme accepts triangles only, and OpenSubdiv rejects
/// anything else, so the import triangulates when the spec asks for it
/// (`LOOP_SUBDIV_POSTPROCESS_FLAGS`). Note that Loop over a mesh that
/// was authored as quads and triangulated on the way in is a different
/// surface from Catmull-Clark over those quads, and depends on which
/// diagonal the triangulator picked: Loop is for meshes modeled as
/// triangles.
///
/// Vertex topology is welded by exact position bits, so copies that
/// exist only to carry different UVs or colors (texture and color seams)
/// subdivide as one surface; the final vertices are the unique (vertex,
/// corner) pairs of the last refinement level, a corner being the UV and
/// color an imported vertex carried, and `Mesh::colors` follows `verts`
/// whenever the base mesh had colors.
///
/// \returns
/// True if `verts` carry valid normals (the smooth limit normals);
/// false if the caller must recompute normals geometrically, either
/// because refinement was linear, a limit normal came out degenerate,
/// or the topology was rejected and the base polygons were fan
/// triangulated as a fallback.
///
[[nodiscard]] bool subdivideMesh(Mesh &mesh);
