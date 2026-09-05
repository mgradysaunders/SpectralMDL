/// \file
/// Deformation from a scene file's own animation data: the clips it
/// carries, the pose of its node graph at a time, and a mesh baked at
/// that pose under its skin and morph targets.
///
/// This is the renderer-side evaluator that lets a rigged glTF, FBX, or
/// Collada file render at any time on the layout's clock with nothing
/// re-exported. The scene bakes each animated mesh twice, at shutter
/// open and shut, and hands Embree the two keys; everything here is one
/// bake at one time.
///
/// The assimp types stay forward declared, as in `MeshImport.h`, so that
/// including this costs nothing.
///
#pragma once

#include <cstdint>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

#include "Common.h"

struct aiAnimation;
struct aiScene;

/// How an asset plays its animation: the `animation` operation in the
/// layout grammar, and the part of the mesh cache key that says which
/// pose a file was baked in.
///
/// ```
/// asset hero = "hero.glb" { animation "walk" offset 0.25 speed 1 }
/// asset flag = "flag.glb" { animation once }   # the file's one clip
/// asset bust = "bust.glb" { animation off }    # the bind pose
/// ```
///
/// The clip's local time at render seconds `s` is `offset + speed * s`,
/// wrapped into the clip's duration unless `once` clamps it. A file with
/// exactly one clip plays it without being asked; a file with several
/// needs a name or an index; `off` renders the bind pose and reads the
/// file exactly as a static one.
///
class AnimationSpec final {
public:
  /// The clip by name (`animation "walk"`), or empty.
  std::string clipName{};

  /// The clip by index (`animation 2`), or `INVALID_INDEX`.
  uint32_t clipIndex{INVALID_INDEX};

  /// Seconds added to the render clock before the clip reads it. A
  /// place's `offset` adds onto the asset's.
  float offset{};

  /// The rate the clock runs at in clip time. Negative plays backwards.
  float speed{1.0f};

  /// Clamp to the clip's ends instead of looping.
  bool once{};

  /// `animation off`: never evaluate, whatever the file carries.
  bool off{};

  /// Was a clip named or indexed at all?
  [[nodiscard]] bool hasClip() const noexcept {
    return !clipName.empty() || clipIndex != INVALID_INDEX;
  }

  /// A key that distinguishes two specs that bake different meshes,
  /// empty for the default spec.
  [[nodiscard]] std::string key() const;
};

/// One clip of a scene file, as `-list-objects` reports it.
class ClipInfo final {
public:
  std::string name{};           ///< The clip's name, possibly empty.
  float duration{};             ///< In seconds.
  uint32_t nodeChannelCount{};  ///< Nodes the clip moves.
  uint32_t morphChannelCount{}; ///< Meshes whose morph weights it drives.
};

/// The clips a scene file carries, in the file's order.
[[nodiscard]] std::vector<ClipInfo> listClips(const aiScene &assScene);

/// The clip `spec` plays from `assScene`, or null for none: `off`, or a
/// file without clips that names none.
///
/// \throws smdl::Error  If a name matches no clip or an index is out of
///                      range (listing the clips the file has), if the
///                      file has several clips and the spec names none,
///                      or if the spec names a clip and the file has none.
///
[[nodiscard]] const aiAnimation *resolveClip(const aiScene &assScene,
                                             const AnimationSpec &spec,
                                             std::string_view fileName);

/// The tick rate of a clip, 25 per second when the file does not say,
/// which is assimp's own viewer's fallback.
[[nodiscard]] double ticksPerSecond(const aiAnimation &clip) noexcept;

/// The clip's local time in ticks at `seconds` on the render clock:
/// `offset + speed * seconds`, wrapped into the duration, or clamped to
/// it under `once`. A clip of zero duration is at zero.
[[nodiscard]] double clipTime(const aiAnimation &clip,
                              const AnimationSpec &spec,
                              double seconds) noexcept;

/// The node graph at one time: every node's transform into file space,
/// numbered in the preorder `flattenNodes()` uses, so the scene can read
/// a placement's transform at a time straight out of it.
class NodePose final {
public:
  /// Node-to-file transforms, in preorder.
  std::vector<float4x4> nodeToFile{};

  /// The index of the first node called `name`, or `INVALID_INDEX`.
  /// Bones and animation channels address nodes by name; assimp asks
  /// files for unique names but does not enforce them, so the first
  /// one wins.
  [[nodiscard]] uint32_t find(std::string_view name) const;

  /// The node names, indexed by node.
  std::unordered_map<std::string, uint32_t> indexByName{};
};

/// The pose of the node graph at `ticks` on `clip`, or its authored
/// pose when `clip` is null (the same transforms `flattenNodes()`
/// accumulates).
///
/// A node with a channel takes scale, rotation, and translation from
/// the channel's keys: linear between keys (spherical for rotations),
/// held at a step key, Hermite through a cubic spline's tangents, and
/// the nearest key outside the channel's own range. That last rule is
/// glTF's and is applied whatever the file's pre and post states say:
/// assimp's default would snap a node whose channel ends early back to
/// its authored transform mid-clip.
///
[[nodiscard]] NodePose evaluatePose(const aiScene &assScene,
                                    const aiAnimation *clip, double ticks);

/// One mesh baked at one time.
class MeshBake final {
public:
  /// Per vertex; the normals and tangents are empty when the mesh has
  /// none.
  std::vector<float3> points{};
  std::vector<float3> normals{};
  std::vector<float3> tangents{};

  /// Is the bake in FILE space rather than the mesh's own? True for a
  /// skinned mesh, whose bones put it in the space of the node graph's
  /// root: the placing node's transform must then NOT be applied, since
  /// FBX bakes it into the bone offsets and glTF ignores it by
  /// specification.
  bool isSkinned{};
};

/// Does `clip` move the vertices of mesh `meshIndex`? A skin under a
/// clip does; a morph target driven by a channel of the clip does; a
/// glTF morph target with a nonzero default weight does with no clip at
/// all. A skin with no clip does not: the file renders as authored, in
/// its bind pose, which is what the static read gives.
[[nodiscard]] bool meshDeforms(const aiScene &assScene, uint32_t meshIndex,
                               const aiAnimation *clip);

/// Bake mesh `meshIndex` at `ticks` on `clip` under `pose`, which must
/// be the pose at the same time. Morph targets blend first, relative to
/// the base (`p0 + sum w_i (t_i - p0)`, normals and tangents alike where
/// the target carries them), then the skin: each vertex through the
/// weighted sum of its bones' `nodeToFile * offset`, the weights
/// normalized, a vertex no bone touches left where it is; the normal
/// through the blended matrix's cofactor, the tangent through its linear
/// part.
///
/// Morph weights come from the clip's channel for the mesh, matched by
/// the name of the node that places it (glTF's convention) and then by
/// the mesh's own name (FBX's); with no channel, glTF's default weights
/// apply, and other formats' static weights are ignored because assimp
/// reports 1.0 for a shape whose channel value it does not carry.
///
/// \throws smdl::Error  If a bone names a node the file does not have,
///                      or a weight or morph key indexes out of range.
///
[[nodiscard]] MeshBake bakeMesh(const aiScene &assScene, uint32_t meshIndex,
                                const NodePose &pose, const aiAnimation *clip,
                                double ticks, std::string_view fileName);
