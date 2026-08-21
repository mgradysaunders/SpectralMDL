/// \file
/// The Embree-backed runtime object a curves asset instantiates: the
/// fiber counterpart of `Primitive.h`, over the `.curves` format in
/// `CurvesFile.h`.
#pragma once

#include <memory>
#include <vector>

#include "embree4/rtcore_geometry.h"
#include "embree4/rtcore_scene.h"

#include "CurvesFile.h"

/// One loaded groom, the curves counterpart of `Mesh` and `Primitive`:
/// its own Embree scene holding one curve geometry, instantiated by
/// `MeshInstance` exactly as a mesh is.
///
/// Embree's `primID` names the curve SEGMENT the ray hit (each strand
/// of N usable windows contributes N consecutive segments), and the hit
/// `u` runs along that segment. `Scene::makeCurvesHit()` widens both to
/// the strand: the tables below map a segment back to its strand and
/// its place within it.
///
/// **State conventions** (the fiber answer to `Primitive.h`'s):
///
/// - `texture_coordinate[0]` is `(strandU, v)`: `strandU` runs 0 at the
///   root to 1 at the tip, uniformly by segment count; `v` runs across
///   the ribbon's width mapped to [0, 1], and is 0 on a tube, where
///   Embree defines no across-parameter.
/// - `texture_tangent_u[0]` is the fiber tangent, root toward tip: the
///   axis a hair BSDF's azimuth is measured around.
/// - The shading and geometry normal of a **tube** is the true swept
///   surface normal; of a **ribbon**, the camera-facing normal (the ray
///   direction reversed and made perpendicular to the fiber), which is
///   the standard flat-hair approximation and the reason ribbons are
///   the fast-and-far mode rather than the closeup mode.
/// - `texture_coordinate[1]` is the strand's root UV when the file
///   carries the column (`texture_space_max` becomes 2), so a scalp
///   texture drives per-strand color without per-strand materials.
/// - `texture_density` stays 0: fibers opt out of the ray-cone LOD
///   machinery, honoring the zero-means-off convention.
///
/// Curves do not register as area lights; an emissive material bound to
/// a groom renders through path hits only, and `LightSampler` says so
/// once rather than silently.
class Curves final {
public:
  /// The rendering spec the asset declaration chose.
  CurvesSpec spec{};

  /// The basis the file declared.
  CurvesFile::Basis basis{};

  /// The Embree scene holding the one curve geometry, which instances
  /// wrap exactly as they wrap a mesh's scene.
  RTCScene scene{};

  /// The index in the `Scene::materials` array, interned from the
  /// asset's `material <name>` under the same instance-level split
  /// meshes get: overrides that rename it bind on the instance.
  uint32_t matIndex{};

  /// The Embree vertex buffer, shared with the geometry (so this vector
  /// must never reallocate once the geometry is built): the file's
  /// points with `spec.radiusScale` applied, and with each strand's
  /// ends duplicated when the basis is Catmull-Rom.
  std::vector<float4> points{};

  /// The Embree index buffer source: the first control vertex of each
  /// segment.
  std::vector<uint32_t> segIndices{};

  /// The strand each segment belongs to. Size = segment count.
  std::vector<uint32_t> segStrand{};

  /// The fence-post table of segments per strand: strand `i` owns the
  /// segments `[strandFirstSeg[i], strandFirstSeg[i + 1])`,
  /// which is what turns a per-segment `u` into the root-to-tip
  /// `strandU`.
  std::vector<uint32_t> strandFirstSeg{};

  /// The per-strand root UVs, straight from the file, or empty.
  std::vector<float2> rootUVs{};

  /// A coarse set of points standing in for mesh vertices wherever the
  /// renderer folds geometry into bounds (`Scene::preCommitBounds()`,
  /// the framing solver): the control points subsampled to a workable
  /// count, plus the radius-inflated bounding corners so a thick cable
  /// is not framed by its centerline.
  std::vector<float3> proxyPoints{};

  [[nodiscard]] uint32_t strandCount() const noexcept {
    return strandFirstSeg.empty() ? 0 : uint32_t(strandFirstSeg.size() - 1);
  }

  [[nodiscard]] uint32_t segCount() const noexcept {
    return uint32_t(segIndices.size());
  }

  /// The fiber center at (`segIndex`, `u` in [0, 1]).
  [[nodiscard]] CurveAxis axisAt(uint32_t segIndex, float u) const {
    return evalCurveAxis(basis, &points[segIndices[segIndex]], u);
  }
};

/// Create a curves object: build the Embree curve geometry (the file's
/// basis crossed with `spec.mode`), the strand tables, and the proxy
/// points, and commit its scene. The caller interns `matIndex` and
/// owns the result; the geometry shares the point buffer, so the result
/// must not be relocated afterward.
[[nodiscard]] std::unique_ptr<Curves> makeCurves(RTCDevice device,
                                                 CurvesFile file,
                                                 const CurvesSpec &spec,
                                                 uint32_t matIndex);
