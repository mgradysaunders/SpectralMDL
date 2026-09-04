/// \file
/// Analytic primitive shapes as Embree user geometry: exact
/// intersections, exact normals, and closed-form parameterizations, with
/// no tessellation anywhere.
#pragma once

#include <memory>
#include <vector>

#include "embree4/rtcore_geometry.h"
#include "embree4/rtcore_scene.h"

#include "Layout/Layout.h"

/// One analytic shape, the primitive counterpart of `Mesh`: its own
/// Embree scene holding one user geometry, instantiated by
/// `MeshInstance` exactly as a mesh is.
///
/// A shape is one geometry with one user primitive per SUB-SURFACE, so
/// Embree's `primID` names the piece the ray hit and every piece gets
/// its own bounds: the sphere and disk are one piece; the cylinder is
/// its side plus two caps; the cone is its side plus a base cap. The
/// caps are what make the cylinder and cone watertight, so that a
/// transmissive material gets a consistent interior.
///
/// The surface parameterization is the fixed (u, v) each piece defines
/// (see `evalPrimitiveSurface()`), and it rides through the renderer in
/// the same slots a triangle's barycentrics do: the intersect callback
/// reports (u, v), `Scene::intersect()` packs them as `bary[1]` and
/// `bary[2]`, and `Scene::makeHit()` can rebuild the full differential
/// geometry from them. Texture coordinates ARE the parameterization.
///
/// The callback also reports the object-space hit point, in the slots a
/// triangle's geometric normal would take, and a hit built from a ray
/// takes its geometry from that point (`evalPrimitiveSurfaceAt()`),
/// which costs no trigonometry. Only a hit rebuilt from the parameters
/// alone, as the manifold walk does, pays for the angles.
///
class Primitive final {
public:
  /// The shape and its dimensions, in object space.
  PrimitiveSpec spec{};

  /// The Embree scene holding the one user geometry, which instances
  /// wrap exactly as they wrap a mesh's scene.
  RTCScene scene{};

  /// The index in the `Scene::materials` array, interned from the
  /// asset's `material <name>` under the same instance-level split
  /// meshes get: overrides that rename it bind on the instance.
  uint32_t matIndex{};

  /// The total object-space surface area, exact.
  float objectArea{};

  /// A coarse set of surface points, in object space, standing in for
  /// mesh vertices wherever the renderer folds geometry into bounds:
  /// `Scene::preCommitBounds()` and the autolook solver both walk these.
  std::vector<float3> proxyPoints{};
};

/// The differential geometry of a shape at (piece, u, v), in object
/// space: the point, the outward unit normal, the parametric partials
/// the texture frame and the ray-cone density come from, and the
/// parametric partials of the unit normal itself, which the manifold
/// connection walk differentiates. The caps have constant normals, so
/// their normal partials are zero.
class PrimitiveSurface final {
public:
  float3 point{};
  float3 normal{};
  float3 dPdu{};
  float3 dPdv{};
  float3 dNdu{};
  float3 dNdv{};
};

/// One uniform-area sample of a shape's whole surface, in object space,
/// for area lighting: the piece and parameter to rebuild the hit from,
/// and the point and outward normal directly. The density is uniform
/// over the OBJECT-space surface, `1 / primitiveObjectArea()`; the
/// caller converts to world area through the instance's normal
/// (cofactor) matrix, which is exact under any affine placement.
class PrimitiveAreaSample final {
public:
  uint32_t primID{};
  float2 uv{};
  float3 point{};
  float3 normal{};
};

/// Create a primitive: build its user geometry, bounds, and proxy
/// points, and commit its scene. The caller interns `matIndex` and
/// owns the result; the geometry's user pointer refers back to the
/// returned object, so it must not be relocated afterward.
[[nodiscard]] std::unique_ptr<Primitive>
makePrimitive(RTCDevice device, const PrimitiveSpec &spec, uint32_t matIndex);

/// The number of sub-surface pieces of a shape, which is the user
/// geometry's primitive count.
[[nodiscard]] uint32_t primitivePieceCount(const PrimitiveSpec &spec);

/// The exact object-space surface area of the whole shape.
[[nodiscard]] float primitiveObjectArea(const PrimitiveSpec &spec);

/// The differential geometry at (piece `primID`, `uv`).
[[nodiscard]] PrimitiveSurface evalPrimitiveSurface(const PrimitiveSpec &spec,
                                                    uint32_t primID, float2 uv);

/// The differential geometry at an object-space `point` on piece
/// `primID`, which must lie on it: the same construction as
/// `evalPrimitiveSurface()` with the trigonometry read off the point, so
/// a hit or a sample that holds its point need not go through the
/// parameters and back. The two agree to float rounding.
[[nodiscard]] PrimitiveSurface evalPrimitiveSurfaceAt(const PrimitiveSpec &spec,
                                                      uint32_t primID,
                                                      const float3 &point);

/// Sample the whole surface uniformly by object-space area.
[[nodiscard]] PrimitiveAreaSample samplePrimitiveArea(const PrimitiveSpec &spec,
                                                      float2 xi);
