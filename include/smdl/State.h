/// \file
#pragma once

#include <cstddef>
#include <cstdint>

#include "smdl/Export.h"
#include "smdl/Support/VectorMath.h"

namespace smdl {

/// \addtogroup resource
/// \{

/// The transport mode.
enum Transport : int {
  /// Transport radiance (tracing paths from cameras to lights).
  TRANSPORT_RADIANCE = 0,
  /// Transport importance (tracing paths from lights to cameras).
  TRANSPORT_IMPORTANCE = 1,
};

/// The MDL state passed in at runtime.
class SMDL_EXPORT State final {
public:
  /// The allocator, which must point to thread-local
  /// instance of `BumpPtrAllocator`.
  void *allocator{};

  /// The opaque host context, which is never interpreted by SpectralMDL.
  ///
  /// This is provided so that host applications can associate a `State` with
  /// whatever context it was constructed from, and recover that context in
  /// `@(foreign)` functions and `SceneData::Getter` callbacks, both of which
  /// receive the `State` but are otherwise unable to determine which shading
  /// point they are being asked about.
  ///
  /// \note
  /// The host is responsible for the lifetime of whatever this points to. It
  /// must remain valid for at least as long as the `State` that refers to it.
  void *userData{};

  /// The transport mode.
  ///
  /// \note
  /// This is necessary to account for asymmetric scattering in
  /// bidirectional methods.
  /// - `TRANSPORT_RADIANCE` means tracing paths from cameras to lights,
  /// - `TRANSPORT_IMPORTANCE` means tracing paths from lights to cameras.
  Transport transport{TRANSPORT_RADIANCE};

  /// The sequence the material's own draws (`state::random_float()`)
  /// come from: a hash-based Owen-scrambled low-discrepancy sequence
  /// after Burley, "Practical Hash-Based Owen Scrambling," JCGT 9(4) 2020.
  ///
  /// \note
  /// The renderer sets this and `sampleIndex` per evaluation. A draw is
  /// stratified across the evaluations that share a seed, differ in
  /// index, and reach it after as many draws, so a renderer with a
  /// low-discrepancy sampler of its own derives the seed from the hit and
  /// passes its sample index through, and one without hashes a seed per
  /// hit. The instance also seeds stochastically evaluated BSDFs, e.g.,
  /// the diffuse component of `df::micrograin_layer`, from the two. The
  /// draw lives in the builtin `::api` module with no C++ twin; the
  /// language test `builtin/state.mdl` pins it to golden words that a host
  /// sampler of the same construction can pin its own draw to.
  uint32_t sampleSeed{};

  /// The sample index within the sequence `sampleSeed` selects.
  uint32_t sampleIndex{};

  /// The number of draws the evaluation has made so far, which every
  /// entry point taking a `State` zeroes first, so that evaluating one
  /// state twice draws the same values.
  ///
  /// Do not populate this!
  uint32_t sampleDimension{};

  /// The meters per scene unit.
  float metersPerSceneUnit{1.0f};

  /// The animation time.
  float animationTime{0.0f};

  /// The minimum wavelength in nanometers.
  float wavelengthMin{};

  /// The maximum wavelength in nanometers.
  float wavelengthMax{};

  /// The wavelength in nanometers the path is committed to: what a lens
  /// whose glasses disperse was traced at, and what a material evaluates
  /// a wavelength-dependent index of refraction at. The same at every
  /// point of one path, and always positive.
  ///
  /// This does not narrow a `color`, which still carries every band of
  /// `wavelengthBase`; only the refraction geometry is the hero's, which
  /// is dispersion approximated by committing the whole spectrum of a
  /// path to one index. It is therefore not hero wavelength spectral
  /// sampling in the usual sense: no band is zeroed and there is no
  /// multiple importance sampling over wavelength shifts.
  ///
  /// The default is the helium d line, which is the wavelength a glass
  /// catalog states `nd` at and a lens prescription's bare index means,
  /// so a host that never draws one evaluates every dispersion model at
  /// its published reference.
  ///
  /// \note This is non-standard!
  float wavelengthHero{587.5618f};

  /// The wavelengths in nanometers, must be sorted in increasing order!
  const float *wavelengthBase{};

  /// If non-null, this necessarily points to `wavelengthBaseMax`
  /// per-band quadrature weights in nanometers: the effective width of
  /// each band, for integrating spectral quantities over a non-uniform
  /// wavelength grid. Null means the uniform default of
  /// `(wavelengthMax - wavelengthMin) / wavelengthBaseMax` per band,
  /// which is what color-to-RGB conversion has always assumed.
  const float *wavelengthWeight{};

  /// The number of path segments traversed to reach this shading point, so
  /// 1 at a primary hit. Zero means "not provided", which consumers must treat
  /// exactly like 1, i.e., highest fidelity.
  ///
  /// NOTE: This is non-standard!
  int scatteringOrder{};

  /// The accumulated distance in scene units traveled by the path to reach
  /// this shading point. Zero conventionally means "not provided" and implies
  /// highest fidelity.
  ///
  /// \note This is non-standard!
  float travelDistance{};

  /// The pixel ray cone spread angle in radians, using the small-angle
  /// convention that the cone width grows by `coneAngle` per unit
  /// distance. Zero means "no cone", i.e., level-of-detail off.
  ///
  /// \note This is non-standard!
  float coneAngle{};

  /// The pixel ray cone width in scene units at the shading point. Zero means
  /// "no footprint", i.e., level-of-detail off.
  ///
  /// \note This is non-standard!
  float coneWidth{};

  /// The object ID.
  int objectId{};

  /// If applicable, the Ptex face ID.
  int ptexFaceId{};

  /// If applicable, the Ptex face UV.
  float2 ptexFaceUV{};

  /// The position or ray intersection point in object space.
  float3 position{};

  /// The normalized direction of propagation of the ray that produced this
  /// evaluation, pointing toward the shading point, in object space (internal
  /// space after `finalize()`). In the
  /// context of an environment lookup, the lookup direction.
  ///
  /// \note
  /// Zero if the renderer does not provide it, in which case
  /// direction-dependent material effects must be skipped. Populating this
  /// at surface hits is non-standard.
  float3 direction{};

  /// The motion vector in object space.
  float3 motion{};

  /// The normal in object space.
  float3 normal{0, 0, 1};

  /// The geometry normal in object space.
  float3 geometryNormal{0, 0, 1};

  /// The max supported number of texture spaces.
  ///
  /// \note
  /// Half of `State` scales with this, so it is set to what materials
  /// actually index: a base space and an optional second one, which is as
  /// many as any of the geometry paths fill. A constant index past it is a
  /// compile error in SMDL; `textureSpaceCount` gates the rest, and
  /// `finalize()` clamps it.
  static constexpr size_t TEXTURE_SPACE_MAX = 2;

  /// The number of texture spaces, clamped to `TEXTURE_SPACE_MAX`.
  int textureSpaceCount{1};

  /// The texture coordinates.
  float3 textureCoordinate[TEXTURE_SPACE_MAX]{};

  /// The texture tangent U vector(s) in object space.
  float3 textureTangentU[TEXTURE_SPACE_MAX] = {float3{1, 0, 0},
                                               float3{1, 0, 0}};

  /// The texture tangent V vector(s) in object space.
  float3 textureTangentV[TEXTURE_SPACE_MAX] = {float3{0, 1, 0},
                                               float3{0, 1, 0}};

  /// The UV texture density of each texture space: UV area per world-space
  /// area of the underlying geometry, so `coneWidth * sqrt(textureDensity)`
  /// is a UV-space filter width. Zero means "unknown", i.e., no filtering.
  /// Renderers must guard the defining division against degenerate geometry:
  /// a degenerate triangle must produce 0, never infinity.
  ///
  /// \note This is non-standard!
  float textureDensity[TEXTURE_SPACE_MAX]{};

  /// The geometry tangent U vector(s) in object space.
  float3 geometryTangentU[TEXTURE_SPACE_MAX] = {float3{1, 0, 0},
                                                float3{1, 0, 0}};

  /// The geometry tangent V vector(s) in object space.
  float3 geometryTangentV[TEXTURE_SPACE_MAX] = {float3{0, 1, 0},
                                                float3{0, 1, 0}};

  /// The second fundamental form of the surface at the shading point, as
  /// `(II_xx, II_xy, II_yy)` against the geometric tangents of texture
  /// space 0 (the X and Y axes of internal space), in inverse scene
  /// units: the normal curvature along a unit tangent direction `(a, b)`
  /// is `II_xx a^2 + 2 II_xy a b + II_yy b^2`. Positive where the surface
  /// curves away from the geometric normal, as the outside of a sphere of
  /// radius `r` does at `1 / r`. Zero means flat or "not provided", which
  /// are the same to everything that reads it.
  ///
  /// These are components against the frame's own axes, so `finalize()`
  /// leaves them as the renderer gave them.
  ///
  /// \note This is non-standard!
  float3 curvature{};

  /// How far along `direction` from the shading point the ray next leaves
  /// the closed surface it has just entered, in scene units, and infinity
  /// where it never does. Zero means "not provided". It is the length of
  /// the ray's chord through the object as the object really is, which the
  /// curvature at the one point can only estimate.
  ///
  /// \note This is non-standard!
  float chordLength{};

  /// The sagitta of the shading point: how far under the smooth surface
  /// it stands, along the normal, in scene units. On a mesh, the height
  /// of the surface its vertex normals describe over the face at the
  /// point. Zero on a surface that is its own smooth surface, and "not
  /// provided".
  ///
  /// \note This is non-standard!
  float sagitta{};

  /// The tangent-to-object matrix.
  ///
  /// The tangent space is the coordinate system where
  /// - The X axis is aligned to the geometry tangent in U.
  /// - The Y axis is aligned to the geometry tangent in V.
  /// - The Z axis is aligned to the geometry normal.
  /// - The origin is the ray intersection point.
  ///
  /// Do not populate this!
  ///
  /// Instead call `finalize()` to compute this from
  /// `geometryTangentU[0]`, `geometryTangentV[0]`, `geometryNormal`, and
  /// `position`.
  ///
  float4x4 tangentToObject{float4x4(1.0f)};

  /// The object-to-world matrix.
  float4x4 objectToWorld{float4x4(1.0f)};

  /// The max supported number of vertex color sets.
  ///
  /// \note
  /// One: a base RGBA set, which is as many as any geometry path fills. A
  /// constant index past it is a compile error in SMDL; `vertexColorCount`
  /// gates the rest, and `finalize()`
  /// clamps it.
  ///
  /// \note This is non-standard!
  static constexpr size_t VERTEX_COLOR_MAX = 1;

  /// The number of vertex color sets the geometry carries, clamped to
  /// `VERTEX_COLOR_MAX`. Zero means "not provided".
  ///
  /// \note This is non-standard!
  int vertexColorCount{};

  /// The vertex colors: RGBA as the geometry stores them, interpolated to
  /// the shading point, with no color management and no premultiplication.
  /// White where no set is present, so an ungated read still behaves.
  ///
  /// \note This is non-standard!
  float4 vertexColor[VERTEX_COLOR_MAX] = {float4{1, 1, 1, 1}};

public:
  /// Finalize for evaluation: establish the internal space conventions
  /// from what the host filled in, repairing what needs it.
  ///
  /// The implementation does the following:
  /// 1. Clamp `textureSpaceCount` and `vertexColorCount` to their limits.
  /// 2. Orthonormalize the normal and tangent vectors.
  /// 3. Orthonormalize the geometric normal and tangent vectors.
  /// 4. Orthonormalize the object-to-world matrix, unless it already is,
  ///    in which case it is left exactly as given.
  /// 5. Construct the matrix pair for transforming between geometric tangent
  ///    space and object space.
  /// 6. Transform every member variable defined in object space to
  ///    geometric tangent space.
  ///
  /// Afterward,
  /// - `position` is at the origin `float3(0,0,0)`
  /// - `geometryTangentU[0]` is the X axis `float3(1,0,0)`
  /// - `geometryTangentV[0]` is the Y axis `float3(0,1,0)`
  /// - `geometryNormal` is the Z axis `float3(0,0,1)`
  ///
  /// Steps 5 and 6 are `finalizeUnchecked()`, which a host whose inputs
  /// need none of the rest may call instead.
  void finalize() noexcept;

  /// Finalize as `finalize()` does, taking the inputs as given: no clamp,
  /// no normalization, no orthogonalization, and the object-to-world
  /// matrix left as it is. The host guarantees that `normal` and
  /// `geometryNormal` are unit, that each tangent pair is orthonormal
  /// with its normal, that the object-to-world matrix is orthonormal, and
  /// that the space and color counts are within their limits. A renderer
  /// that built its frame from unit vectors under a rigid placement has
  /// all of that already, and pays here only for the matrix and the
  /// transform into tangent space. Inline so that a host filling the state
  /// and finalizing it in one place keeps the vectors in registers
  /// across the two.
  void finalizeUnchecked() noexcept {
    tangentToObject[0] = float4(geometryTangentU[0], 0.0f);
    tangentToObject[1] = float4(geometryTangentV[0], 0.0f);
    tangentToObject[2] = float4(geometryNormal, 0.0f);
    tangentToObject[3] = float4(position, 1.0f);
    // The frame is orthonormal, so the inverse of its linear part is its
    // transpose and a direction maps to its three dots with the axes,
    // which is the whole of `affineInverse()` and the 4x4 product for a
    // vector whose `w` is zero.
    const float3 u{geometryTangentU[0]};
    const float3 v{geometryTangentV[0]};
    const float3 w{geometryNormal};
    const auto toTangent{[&](const float3 &d) {
      return float3(dot(d, u), dot(d, v), dot(d, w));
    }};
    position = {};
    direction = toTangent(direction);
    motion = toTangent(motion);
    normal = toTangent(normal);
    for (int i = 0; i < textureSpaceCount; i++) {
      textureTangentU[i] = toTangent(textureTangentU[i]);
      textureTangentV[i] = toTangent(textureTangentV[i]);
    }
    for (int i = 1; i < textureSpaceCount; i++) {
      geometryTangentU[i] = toTangent(geometryTangentU[i]);
      geometryTangentV[i] = toTangent(geometryTangentV[i]);
    }
    // Space 0's geometry frame is the frame itself, so it lands on the
    // axes exactly rather than within rounding of them, which is what
    // this function documents.
    geometryNormal = {0, 0, 1};
    geometryTangentU[0] = {1, 0, 0};
    geometryTangentV[0] = {0, 1, 0};
  }
};

/// \}

} // namespace smdl
