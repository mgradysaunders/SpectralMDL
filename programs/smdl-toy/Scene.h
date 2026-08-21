#pragma once

// `rtcore_scene.h` needs `rtcore_geometry.h` before it, and the pair pulls
// in the buffer, device, and common headers, covering the `RTCDevice` and
// `RTCScene` members below. The ray-query API and all of assimp are
// implementation details of `Scene.cc`.
#include "embree4/rtcore_geometry.h"
#include "embree4/rtcore_scene.h"

#include "smdl/Compiler.h"
#include "smdl/Support/ColorVector.h"
#include "smdl/Support/MonteCarlo.h"

// For the import data model, the listing entry points, and `INF` and
// `INVALID_INDEX`.
#include "MeshImport.h"

// For `SubdivSpec` and `MaterialAssignment`: the properties an item
// carries live with the layout format that spells them, and the scene
// consumes them as plain data.
#include "Layout.h"

// The analytic shapes, which instances wrap exactly as they wrap meshes.
#include "Primitive.h"

// The fiber geometry, the third thing an instance can wrap.
#include "Curves.h"

/// The default wavelength range in nanometers, spanning the visible.
constexpr float WAVELENGTH_MIN = 380.0f;
constexpr float WAVELENGTH_MAX = 720.0f;

/// The self-intersection offset, in scene units.
constexpr float EPS = 0.0001f;

/// The render-wide wavelength band count, which sizes every `Color`.
///
/// Set exactly once in `main()` before anything constructs a `Color`
/// and long before rendering threads start. The default of 16 matches
/// `smdl::ColorVector::INLINE_CAPACITY`, so a default render's colors
/// never touch the heap.
[[nodiscard]] inline size_t &renderNumBands() noexcept {
  static size_t numBands{16};
  return numBands;
}

/// The render-wide per-band quadrature weights in nanometers, empty
/// for a uniformly spaced grid.
///
/// Set once in `main()` alongside `renderNumBands()`. Empty keeps
/// `State::wavelength_weight` null, which the library treats as
/// uniform quadrature; a non-uniform `-wavelengths` grid fills this
/// with trapezoid band widths, which both the JIT color-to-RGB
/// conversion and the night tonemap integrate against.
[[nodiscard]] inline std::vector<float> &renderWavelengthWeights() noexcept {
  static std::vector<float> weights{};
  return weights;
}

/// The render-wide time in seconds.
[[nodiscard]] inline float &renderTime() noexcept {
  static float time{};
  return time;
}

/// The render color type: an `smdl::ColorVector` whose constructors
/// supply the render-wide band count, so the ubiquitous `Color c{}`
/// zero vector and `Color(scalar)` splat idioms work with a runtime
/// band count.
class Color final : public smdl::ColorVector {
public:
  Color() : ColorVector(renderNumBands()) {}

  Color(float value) : ColorVector(renderNumBands(), value) {}

  /// Construct from however many values are present: a shorter or
  /// empty span (a material coefficient the instance does not have)
  /// leaves the remaining bands zero.
  Color(smdl::Span<const float> values) : ColorVector(renderNumBands()) {
    const size_t n{values.size() < size() ? values.size() : size()};
    for (size_t i = 0; i < n; i++) (*this)[i] = values[i];
  }

  Color(const ColorVector &other) : ColorVector(other) {}

  Color(ColorVector &&other) noexcept
      : ColorVector(static_cast<ColorVector &&>(other)) {}
};

/// An `smdl::State` carrying the render-wide fields every evaluation
/// needs: the wavelength grid and, when material construction is involved,
/// the allocator. The geometric fields are applied afterward by
/// `Hit::apply_geometry_to_state()`.
[[nodiscard]] inline smdl::State
makeRenderState(const Color &wavelengths,
                smdl::BumpPtrAllocator *allocator = nullptr) noexcept {
  smdl::State state{};
  state.allocator = allocator;
  state.wavelength_base = wavelengths.data();
  state.wavelength_min = wavelengths[0];
  state.wavelength_max = wavelengths[wavelengths.size() - 1];
  state.animation_time = renderTime();
  const auto &weights{renderWavelengthWeights()};
  state.wavelength_weight = weights.empty() ? nullptr : weights.data();
  return state;
}

/// The sampler version tag written into resumable output metadata. A
/// resumed render continues the sampler's deterministic (pixel, sample
/// index) sequence, so after a change to the sampling scheme the
/// continuation samples are merely independent of the first session's
/// rather than jointly stratified with them; still unbiased, but worth
/// a warning. Bump this whenever the sequence changes.
constexpr const char *SAMPLER_VERSION = "owen-sobol-1";

/// A hash-based Owen-scrambled Sobol sampler after Burley, "Practical
/// Hash-Based Owen Scrambling," JCGT 9(4) 2020.
///
/// Each (pixel, sample) pair yields a deterministic low-discrepancy point
/// sequence consumed two dimensions at a time. Every 2D pair reuses the
/// first two Sobol dimensions with an independently hashed index shuffle
/// and per-dimension Owen scramble, which keeps each pair's stratification
/// while decorrelating the pairs from one another, so the sequence
/// extends to arbitrarily many dimensions with no direction-number tables
/// beyond the second dimension's.
class Sampler final {
public:
  Sampler() = default;

  /// Begin the sample `sampleIndex` of the pixel `pixelIndex`, resetting
  /// the dimension counter.
  void startPixelSample(uint32_t pixelIndex, uint32_t sampleIndex) noexcept {
    pixelHash = hash(pixelIndex);
    this->sampleIndex = sampleIndex;
    dimension = 0;
  }

  [[nodiscard]] operator float() { return next(); }

  [[nodiscard]] operator float2() { return {next(), next()}; }

  [[nodiscard]] operator float3() { return {next(), next(), next()}; }

  [[nodiscard]] operator float4() { return {next(), next(), next(), next()}; }

  [[nodiscard]] int index(int n) {
    int i{int(std::floor(float(n) * next()))};
    i = std::min(i, n - 1);
    i = std::max(i, 0);
    return i;
  }

  /// The next scrambled sample as raw bits, advancing the dimension.
  [[nodiscard]] uint32_t nextBits() noexcept {
    uint32_t pair{dimension >> 1};
    uint32_t component{dimension & 1};
    ++dimension;
    uint32_t seed{hash(pixelHash ^ (0x9E3779B9U * pair))};
    uint32_t shuffledIndex{nestedUniformScramble(sampleIndex, seed)};
    uint32_t X{component == 0 ? reverseBits(shuffledIndex)
                              : sobolDim1(shuffledIndex)};
    return nestedUniformScramble(X, hash(seed ^ (0x55555555U + component)));
  }

private:
  /// The next canonical sample in `(0,1)`, advancing the dimension.
  [[nodiscard]] float next() noexcept {
    float xi{float(nextBits()) * 0x1p-32f};
    xi = std::fmax(xi, std::numeric_limits<float>::denorm_min()); // > 0
    xi = std::fmin(xi, ONE_MINUS_EPS);                            // < 1
    return xi;
  }

  /// Murmur3-style finalizer.
  [[nodiscard]] static uint32_t hash(uint32_t x) noexcept {
    x ^= x >> 16;
    x *= 0x85EBCA6BU;
    x ^= x >> 13;
    x *= 0xC2B2AE35U;
    x ^= x >> 16;
    return x;
  }

  [[nodiscard]] static uint32_t reverseBits(uint32_t x) noexcept {
    x = (x << 16) | (x >> 16);
    x = ((x & 0x00FF00FFU) << 8) | ((x & 0xFF00FF00U) >> 8);
    x = ((x & 0x0F0F0F0FU) << 4) | ((x & 0xF0F0F0F0U) >> 4);
    x = ((x & 0x33333333U) << 2) | ((x & 0xCCCCCCCCU) >> 2);
    x = ((x & 0x55555555U) << 1) | ((x & 0xAAAAAAAAU) >> 1);
    return x;
  }

  /// The hash-based Owen scramble: reverse so the high (most significant)
  /// bits sit low, run the Laine-Karras permutation, which only lets each
  /// bit affect bits above it, and reverse back.
  [[nodiscard]] static uint32_t nestedUniformScramble(uint32_t x,
                                                      uint32_t seed) noexcept {
    x = reverseBits(x);
    x += seed;
    x ^= x * 0x6C50B47CU;
    x ^= x * 0xB82F1E52U;
    x ^= x * 0xC7AFE638U;
    x ^= x * 0x8D22F6E6U;
    return reverseBits(x);
  }

  /// The second Sobol dimension. (The first is just `reverseBits`.)
  [[nodiscard]] static uint32_t sobolDim1(uint32_t index) noexcept {
    static constexpr std::array<uint32_t, 32> directions = {
        0x80000000U, 0xC0000000U, 0xA0000000U, 0xF0000000U, //
        0x88000000U, 0xCC000000U, 0xAA000000U, 0xFF000000U, //
        0x80800000U, 0xC0C00000U, 0xA0A00000U, 0xF0F00000U, //
        0x88880000U, 0xCCCC0000U, 0xAAAA0000U, 0xFFFF0000U, //
        0x80008000U, 0xC000C000U, 0xA000A000U, 0xF000F000U, //
        0x88008800U, 0xCC00CC00U, 0xAA00AA00U, 0xFF00FF00U, //
        0x80808080U, 0xC0C0C0C0U, 0xA0A0A0A0U, 0xF0F0F0F0U, //
        0x88888888U, 0xCCCCCCCCU, 0xAAAAAAAAU, 0xFFFFFFFFU};
    uint32_t X{};
    for (int bit = 0; index; index >>= 1, bit++)
      if (index & 1) X ^= directions[bit];
    return X;
  }

  uint32_t pixelHash{};
  uint32_t sampleIndex{};
  uint32_t dimension{};
};

class Ray final {
public:
  /// Evaluate.
  [[nodiscard]] float3 operator()(float t) const noexcept {
    return org + t * dir;
  }

  /// Apply transform.
  void transform(const float4x4 &xf) noexcept {
    org = xf * float4(org, 1.0f);
    dir = xf * float4(dir, 0.0f);
  }

public:
  float3 org{};    ///< The origin.
  float3 dir{};    ///< The direction.
  float tmin{EPS}; ///< The minimum parameter.
  float tmax{INF}; ///< The maximum parameter.
};

struct aiMesh;
struct aiScene;

class MeshInstance;

/// A hit.
///
/// Every geometric field is in **world space**. `Scene::makeHit()` applies
/// the instance transform because that is the space the renderer traces,
/// shadows, and measures distances in.
///
class Hit final {
public:
  /// Apply geometry to SMDL state.
  ///
  /// `rayDir` is the normalized direction of propagation of the ray that
  /// produced the hit, which `finalizeAndApplyInternalSpaceConventions()`
  /// rotates into internal space. The zero default means "not provided"
  /// per the `State` conventions.
  ///
  /// The geometry is handed over in the instance's **rigid frame** rather
  /// than in world space, paired with the instance transform in
  /// `State::object_to_world_matrix`, so that the library reassembles world
  /// space itself. See `MeshInstance::rigidToWorld` for why the rigid frame
  /// and not the raw object space, and for when the two coincide.
  ///
  void applyGeometryToState(smdl::State &state,
                            const float3 &rayDir = float3{}) const noexcept;

public:
  const MeshInstance *instance{};        ///< The mesh instance.
  uint32_t instIndex{INVALID_INDEX};     ///< The mesh instance index.
  uint32_t meshIndex{INVALID_INDEX};     ///< The mesh index.
  uint32_t faceIndex{INVALID_INDEX};     ///< The face index.
  uint32_t matIndex{INVALID_INDEX};      ///< The material index.
  const smdl::JIT::Material *material{}; ///< The material.
  float3 bary{};                         ///< The barycentric coordinate.
  float3 point{};                        ///< The point.
  float3 normal{};                       ///< The shading normal.
  float3 tangent{};                      ///< The shading tangent.
  float3 Ng{};                           ///< The geometry normal.
  float3 Tg{};                           ///< The geometry tangent.
  float2 texcoord{};                     ///< The texture coordinate.
  float textureDensity{};                ///< The UV texture density.

  /// The world-space fiber diameter at a curves hit, 0 otherwise. Feeds
  /// `texture_coordinate[0].z` per the MDL hair texturing convention.
  float fiberThickness{};

  /// The number of texture spaces the hit fills, 1 for everything but a
  /// curves hit whose file carries the root UV column.
  int textureSpaces{1};

  /// The second texture coordinate (a curve strand's root UV), read
  /// only when `textureSpaces` is 2. It shares the first space's
  /// tangents, since a per-strand constant has no derivatives of its
  /// own.
  float2 texcoord1{};
};

/// The differential geometry a manifold connection walk differentiates
/// at a hit, in world space: the point, the unit shading normal, and
/// the parametric partials of both over the hit's own surface
/// parameterization, which is the triangle's barycentric coordinates
/// (`bary[1]`, `bary[2]`) or the primitive piece's (u, v). The two
/// parameterizations need not agree on units; each only has to span
/// the tangent plane consistently with itself, which is all a Newton
/// step needs.
class ManifoldGeometry final {
public:
  float3 point{};
  float3 normal{};
  float3 dPdu{};
  float3 dPdv{};
  float3 dNdu{};
  float3 dNdv{};
};

/// A mesh.
class Mesh final {
public:
  class Vert final {
  public:
    float3 point{};    ///< The point.
    float3 normal{};   ///< The normal direction.
    float3 tangent{};  ///< The tangent direction.
    float2 texcoord{}; ///< The texture coordinate.
  };

  using Face = std::array<uint32_t, 3>;

  RTCScene scene{};          ///< The Embree scene.
  std::vector<Vert> verts{}; ///< The verts.
  std::vector<Face> faces{}; ///< The faces.
  uint32_t matIndex{};       ///< The index in the `Scene::materials` array.

  /// The refinement the import asked for; inactive by default.
  SubdivSpec subdiv{};

  /// True while the mesh still owes work to `Scene::commit()`:
  /// subdivision, displacement, and the Embree BVH are all deferred for
  /// meshes with an active `subdiv`, so that materials are resolved by
  /// the time displacement runs and the vertex work can run in parallel
  /// across meshes.
  bool needsFinalize{};

  /// The base polygon mesh that subdivision refines, stored only while
  /// `needsFinalize` holds and `subdiv.levels > 0`: the importer keeps
  /// the authored polygons (no triangulation) because Catmull-Clark on
  /// a triangulated mesh is a different, worse surface. The Loop scheme
  /// inverts that requirement and the import triangulates for it, so
  /// these are triangles in that case and polygons of any size
  /// otherwise. Consumed by `subdivideMesh()` and released after
  /// finalize. Normals and
  /// tangents are deliberately absent: both are recomputed from the
  /// refined (and possibly displaced) surface.
  std::vector<float3> basePoints{};       ///< Per imported vertex.
  std::vector<float2> baseTexcoords{};    ///< Empty if the file has no UVs.
  std::vector<uint32_t> baseFaceCounts{}; ///< Vertex count per polygon.
  std::vector<uint32_t> baseIndices{};    ///< Concatenated corner indices.
};

/// The UV texture density of a triangle: UV area per world-space area, so
/// `State::cone_width * sqrt(density)` is a UV-space filter width. Returns
/// 0 for degenerate triangles (never infinity or NaN), honoring the
/// zero-means-off convention of `State::texture_density`. The points must
/// be in world space, so that a scaled instance reports the density its
/// scaled triangles actually have.
[[nodiscard]] inline float
uvTextureDensity(const float3 &point0, const float3 &point1,
                 const float3 &point2, const float2 &texcoord0,
                 const float2 &texcoord1, const float2 &texcoord2) noexcept {
  auto edge1{point1 - point0};
  auto edge2{point2 - point0};
  float worldArea{0.5f * smdl::length(smdl::cross(edge1, edge2))};
  if (!(worldArea > 1e-12f)) return 0.0f;
  auto uv1{texcoord1 - texcoord0};
  auto uv2{texcoord2 - texcoord0};
  float uvArea{0.5f * std::fabs(uv1.x * uv2.y - uv1.y * uv2.x)};
  return uvArea / worldArea;
}

/// A mesh instance.
///
/// The transform is kept in full, shear and non-uniform scale included, and
/// is what both Embree and `Scene::makeHit()` use, so the silhouette and the
/// shading geometry agree however the instance is deformed. The SpectralMDL
/// implementation still uses orthonormalized versions in order to transform
/// directions correctly without perturbing scattering distributions.
///
class MeshInstance final {
public:
  /// Set `objectToWorld` to the transform the scene file authored and derive
  /// everything else from it.
  ///
  /// `fileName` only names the file in the warning that a degenerate
  /// transform emits.
  ///
  void setObjectToWorld(const float4x4 &xf, std::string_view fileName);

  /// The object-to-world matrix as authored, which may shear and scale
  /// non-uniformly. This is what Embree traces and what `makeHit()` puts
  /// geometry in world space with.
  float4x4 objectToWorld{};

  /// The rigid frame of the instance: `objectToWorld` orthonormalized,
  /// keeping its translation.
  ///
  /// This is the frame the shading point is expressed in, and it is
  /// deliberately computed with the same `orthonormalize()` the library
  /// applies to `State::object_to_world_matrix`, from the same matrix. The
  /// round trip is therefore exact rather than merely close: what
  /// `worldToRigid` undoes here is bit for bit what the library rebuilds
  /// there.
  ///
  /// Note that a rigid frame carries **world units**, so object space is
  /// world-scaled even for a uniformly scaled instance.
  ///
  float4x4 rigidToWorld{};

  /// The inverse of `rigidToWorld`.
  float4x4 worldToRigid{};

  /// The cofactor matrix of the linear part of `objectToWorld`, which is how
  /// normals transform: `cof(A) = det(A) A^-T`.
  ///
  /// The geometry normal needs no such matrix only because it already has
  /// one implicitly, by way of `cross(A u, A v) == cof(A) (u × v)`: the
  /// cross product of two transformed edges *is* the cofactor image of the
  /// object-space geometry normal. So the shading normal and the geometry
  /// normal are the same construction, and `flipsWinding` corrects both or
  /// neither.
  ///
  float3x3 normalMatrix{};

  /// Does `objectToWorld` reverse handedness, i.e. is its determinant
  /// negative? A mirroring instance reverses triangle winding, and the
  /// cofactor matrix picks up the same sign, so both normals are flipped
  /// back together.
  bool flipsWinding{};

  /// Does `objectToWorld` shear or scale non-uniformly? Only used to warn
  /// about the one place the deformation cannot reach, which is the interior
  /// of a volume; see `Medium`.
  bool isDeformed{};

  /// The index in the `Scene::meshes` array, or `INVALID_INDEX` when
  /// the instance instantiates a primitive instead.
  uint32_t meshIndex{};

  /// The index in the `Scene::primitives` array, or `INVALID_INDEX`.
  /// Exactly one of `meshIndex`, `primIndex`, and `curvesIndex` is
  /// valid.
  uint32_t primIndex{INVALID_INDEX};

  /// The index in the `Scene::curves` array, or `INVALID_INDEX`.
  uint32_t curvesIndex{INVALID_INDEX};

  /// Instantiates a primitive rather than a mesh?
  [[nodiscard]] bool isPrimitive() const noexcept {
    return primIndex != INVALID_INDEX;
  }

  /// Instantiates a curves groom rather than a mesh?
  [[nodiscard]] bool isCurves() const noexcept {
    return curvesIndex != INVALID_INDEX;
  }

  /// The instance's own material binding: an index in `Scene::materials`,
  /// or `INVALID_INDEX` to use the mesh's or primitive's.
  ///
  /// This is what lets N placements of one asset carry N different
  /// shading-only material overrides over one shared mesh and one shared
  /// BVH: `Scene::add()` bakes the assignment into the mesh but resolves
  /// the override renames per instance, precisely because a rename that
  /// does not feed displacement changes nothing about the geometry. See
  /// `Scene::materialIndexOf()`, the one accessor every shading consumer
  /// goes through.
  uint32_t matIndex{INVALID_INDEX};
};

inline void Hit::applyGeometryToState(smdl::State &state,
                                      const float3 &rayDir) const noexcept {
  // World space to the instance's rigid frame. The library multiplies by
  // `object_to_world_matrix` on the way back out, which lands on world
  // space again exactly, because `rigidToWorld` is what the library
  // reduces `objectToWorld` to.
  const auto &toRigid{instance->worldToRigid};
  auto pointR{float3(toRigid * float4(point, 1.0f))};
  auto rayDirR{float3(toRigid * float4(rayDir, 0.0f))};
  auto normalR{float3(toRigid * float4(normal, 0.0f))};
  auto tangentR{float3(toRigid * float4(tangent, 0.0f))};
  auto NgR{float3(toRigid * float4(Ng, 0.0f))};
  auto TgR{float3(toRigid * float4(Tg, 0.0f))};
  state.object_to_world_matrix = instance->objectToWorld;
  state.position = pointR;
  state.direction = rayDirR;
  state.normal = normalR;
  state.texture_space_max = textureSpaces;
  state.texture_coordinate[0] = {texcoord.x, texcoord.y, fiberThickness};
  state.texture_tangent_u[0] = tangentR;
  state.texture_tangent_v[0] = smdl::cross(normalR, tangentR);
  if (textureSpaces > 1) {
    state.texture_coordinate[1] = {texcoord1.x, texcoord1.y, 0};
    state.texture_tangent_u[1] = tangentR;
    state.texture_tangent_v[1] = state.texture_tangent_v[0];
  }
  state.geometry_normal = NgR;
  state.geometry_tangent_u[0] = TgR;
  state.geometry_tangent_v[0] = smdl::cross(NgR, TgR);
  if (textureSpaces > 1) {
    state.geometry_tangent_u[1] = state.geometry_tangent_u[0];
    state.geometry_tangent_v[1] = state.geometry_tangent_v[0];
  }
  state.object_id = int(instIndex);
  state.ptex_face_id = int(faceIndex);
  state.ptex_face_uv = {bary[1], bary[2]};
  state.texture_density[0] = textureDensity;
  state.finalizeAndApplyInternalSpaceConventions();
}

/// A scene, composed from one or more scene files: call `add()` once
/// per file, then `commit()` once.
class Scene final {
public:
  /// \param[in] fallbackMaterialName
  /// The material to substitute for scene material names that do not
  /// resolve. If empty, an unresolved name is an error instead.
  ///
  explicit Scene(const smdl::Compiler &compiler,
                 std::string_view fallbackMaterialName = {})
      : compiler(compiler), fallbackMaterialName(fallbackMaterialName),
        device(rtcNewDevice("verbose=0")), scene(rtcNewScene(device)) {}
  Scene(const Scene &) = delete;
  ~Scene();

  /// Add the meshes in `fileName` that `selection` keeps, placing them under
  /// `objectToWorld`, which composes on the left of the file's own node
  /// transforms.
  ///
  /// The selection is applied here rather than during the import itself, so
  /// that adding one file under several different selections still costs one
  /// parse and one set of acceleration structures.
  ///
  /// Material names are shared across files: two files that both name a
  /// material `wood` resolve to one entry in `materials`, and so to one
  /// MDL material. Names are resolved by `commit()`, not here, so that
  /// every unresolvable name in the whole composition is reported at once.
  /// `materials` renames this import's slots on the way in, which is how
  /// two files that both call their material `MatID_1` can be shaded
  /// differently; the name that lands in `materials` is the renamed one,
  /// so everything downstream, the file-wide aliases included, sees the
  /// name the import asked for.
  ///
  /// `subdiv` and the assignment half of `materials` (`all` and
  /// `bySlot`) are properties of the import like `selection`, but unlike
  /// the selection they change the mesh data itself, so they join the
  /// cache key: the same file under two different specs is parsed and
  /// refined twice, while identical specs share. The `renames` half is
  /// different: a rename that does not feed displacement changes
  /// nothing about the geometry, so it stays OUT of the cache key and is
  /// resolved per instance instead (`MeshInstance::matIndex`). That
  /// is what lets N placements of one asset carry N shading-only
  /// overrides over one shared mesh and one BVH. The exception is an
  /// import with `subdiv.displace`, where the renamed material's
  /// displacement bakes into the vertices at commit, so the renames
  /// rejoin the key and the meshes genuinely differ.
  ///
  /// \throws smdl::Error  If assimp cannot read the file, or if a selection
  ///                      pattern matches nothing.
  ///
  void add(const std::string &fileName,
           const float4x4 &objectToWorld = float4x4(1.0f),
           const ObjectSelection &selection = {}, const SubdivSpec &subdiv = {},
           const MaterialAssignment &materials = {});

  /// Add one lowered layout item, whichever kind it is: a mesh file
  /// through `add()`, a primitive through `addPrimitive()`, or a curves
  /// file through `addCurves()`. An item carrying `batchXfs`
  /// lands as one Embree instance ARRAY per instantiated geometry
  /// rather than one instance geometry per entry, which is what lets a
  /// 100k-record scatter commit in a handful of geometry objects.
  void add(const LayoutItem &item);

  /// Add a `.curves` groom, placed once per entry of `worldXfs`
  /// exactly as `addPrimitive()` places a shape, and with the same
  /// material split: fibers have one implicit slot, the whole-asset
  /// binding joins the (file, spec, binding) cache key, and the renames
  /// bind on the instance, so N shading-only overrides of one groom
  /// share one loaded groom and one BVH. Returns the first instance
  /// index.
  ///
  /// \throws smdl::Error  If the file cannot be read or fails
  ///                      validation; see `readCurvesFile()`.
  uint32_t addCurves(const std::string &fileName,
                     smdl::Span<const float4x4> worldXfs,
                     const CurvesSpec &spec,
                     const MaterialAssignment &materials = {});

  /// Add an analytic primitive, placed once per entry of
  /// `worldXfs` (one entry is an ordinary instance; several are
  /// an instance array).
  ///
  /// The material assignment splits exactly as `add()` splits it for
  /// meshes: the whole-asset binding (`materials.resolve("")` before
  /// renames) joins the primitive cache key, and the renames bind on
  /// the instance, so N shading-only overrides of one shape share one
  /// `Primitive` and its tiny BVH. Returns the first instance index.
  uint32_t addPrimitive(const PrimitiveSpec &spec,
                        smdl::Span<const float4x4> worldXfs,
                        const MaterialAssignment &materials = {});

  /// Add a ground plane: a two-triangle quad of `halfExtent` at height
  /// `z`, shaded by `materialName`, which `commit()` resolves like any
  /// other material name, fallback included. Texture
  /// coordinates are in scene units (one UV unit per meter), so a
  /// textured material tiles at a physical scale rather than stretching
  /// once across the quad.
  ///
  /// Call between the last `add()` and `commit()`. Returns the plane's
  /// mesh instance index, which is what lets the framing solver tell
  /// scenery to be framed through from geometry to be framed; see
  /// `FramingOptions::skipInstance`.
  [[nodiscard]] uint32_t addGroundPlane(float z, float halfExtent,
                                        const std::string &materialName);

  /// The axis-aligned bounds of everything instantiated so far, before
  /// `commit()` exists to ask Embree: the instance transforms folded over
  /// the mesh vertices, base vertices standing in for meshes whose
  /// refinement is deferred (displacement can therefore still move
  /// geometry slightly past this). Empty if there is no geometry at all.
  [[nodiscard]] BoundBox3 preCommitBounds() const;

  /// The deduped names of materials some instance actually shades with,
  /// in first-interned order: exactly the names `resolveMaterials()`
  /// will demand, so what a host passes to
  /// `smdl::Compiler::setDesiredMaterials()` before compiling. Call
  /// between the last `add()` and `commit()`.
  [[nodiscard]] std::vector<std::string> usedMaterialNames() const;

  /// Finish the scene: resolve every material name that an instantiated
  /// mesh uses, run the deferred per-mesh work (subdivision, displacement,
  /// and the Embree BVHs of meshes that asked for either), and build the
  /// top-level acceleration structure and bounds. Call once, after the
  /// last `add()`.
  ///
  /// Materials resolve first because displacement needs them; the
  /// per-mesh work then runs in parallel across meshes, which are
  /// independent by construction.
  ///
  /// `wavelengths` seeds the `smdl::State` that displacement is evaluated
  /// with, matching the grid the render itself uses.
  ///
  /// \throws smdl::Error  If any material name does not resolve and no
  ///                      fallback was given.
  ///
  void commit(const Color &wavelengths);

private:
  [[nodiscard]] ImportFile load(const aiScene &assScene,
                                const SubdivSpec &subdiv,
                                const MaterialAssignment &materials);
  void load(const aiMesh &assMesh, const std::vector<uint32_t> &materialRemap,
            const SubdivSpec &subdiv);
  /// The batch-capable body of `add()`: every entry of `worldXfs` is
  /// one placement of the file. One entry becomes an ordinary instance;
  /// several become one Embree instance array per instantiated mesh,
  /// with the file's node transform composed into every element.
  void addMesh(const std::string &fileName, smdl::Span<const float4x4> worldXfs,
               const ObjectSelection &selection, const SubdivSpec &subdiv,
               const MaterialAssignment &materials);

  /// Returns the new instance's index in `meshInstances`. Exactly one
  /// of `meshIndex`, `primIndex`, and `curvesIndex` names the
  /// instantiated geometry; `matIndex` is the instance's own
  /// binding, or `INVALID_INDEX` to shade with the geometry's.
  uint32_t addInstance(uint32_t meshIndex, uint32_t primIndex,
                       uint32_t curvesIndex, const float4x4 &xf,
                       std::string_view fileName,
                       uint32_t matIndex = INVALID_INDEX);

  /// The array counterpart of `addInstance()`: one Embree instance
  /// array geometry whose element `i` places the geometry under
  /// `worldXfs[i] * nodeXf`, appending one `MeshInstance` per element
  /// so every consumer that walks `meshInstances` is none the wiser.
  /// Returns the first element's index in `meshInstances`.
  uint32_t addInstanceArray(uint32_t meshIndex, uint32_t primIndex,
                            uint32_t curvesIndex,
                            smdl::Span<const float4x4> worldXfs,
                            const float4x4 &nodeXf, std::string_view fileName,
                            uint32_t matIndex = INVALID_INDEX);

  /// The index of `name` in `materials`, appending it if this is the first
  /// file to mention it.
  [[nodiscard]] uint32_t internMaterial(std::string name);

  /// Whether each entry of `materials` is shaded with by some instance,
  /// via `materialIndexOf()`. Shared by `usedMaterialNames()` and
  /// `resolveMaterials()` so the desired set and the demanded set
  /// cannot drift.
  [[nodiscard]] std::vector<bool> computeUsedMaterials() const;

  /// Resolve every material name some instance actually shades with,
  /// instance-level overrides included; the body of what `commit()`
  /// promises about names. See `commit()`.
  void resolveMaterials();

  /// Run the deferred per-mesh work in parallel: subdivision,
  /// displacement, normal and tangent recomputation, and each finalized
  /// mesh's Embree BVH. Meshes that were never instantiated are skipped
  /// outright, since nothing can ever hit them and their materials may
  /// legitimately be unresolved.
  void finalizeMeshes(const Color &wavelengths);

  /// One mesh of `finalizeMeshes()`, safe to run concurrently with other
  /// meshes. Returns true if displacement actually moved vertices.
  bool finalizeMesh(Mesh &mesh, const Color &wavelengths);

  /// Apply the material `geometry.displacement` to the final vertices, in
  /// the mesh's own space. Offsets are evaluated once per position-welded
  /// vertex, averaging over split copies whose UVs disagree (texture
  /// seams), and applied to every copy, so the displaced surface cannot
  /// crack along seams. Returns true if any vertex moved, false when the
  /// material is null or provably undisplaced.
  bool displaceMesh(Mesh &mesh, const Color &wavelengths);

  /// Build and commit the Embree triangle geometry for `mesh.verts` and
  /// `mesh.faces` into `mesh.scene`. The buffers are shared, so the
  /// vectors must not change size afterward.
  void buildMeshGeometry(Mesh &mesh);

  /// The primitive half of `makeHit()`: rebuild the differential
  /// geometry of `primID`'s piece at the (u, v) packed in `bary[1]` and
  /// `bary[2]`, in world space.
  [[nodiscard]] Hit makePrimitiveHit(uint32_t instIndex, uint32_t primID,
                                     const float3 &bary) const;

  /// The curves half of `intersect()`: build the hit record for the
  /// (segment `primID`, `u`) Embree reports, in world space. Unlike
  /// triangles and primitives, a curve hit cannot be rebuilt from
  /// indices alone: the point comes from the ray, the tube's surface
  /// normal from Embree's object-space `Ng`, and the ribbon's normal
  /// from the ray direction, so this runs only where the ray is in
  /// hand. That is not a loss, because nothing re-derives curve hits:
  /// grooms never register as area lights. See `Curves.h` for the
  /// state conventions this encodes.
  [[nodiscard]] Hit makeCurvesHit(uint32_t instIndex, uint32_t primID, float u,
                                  float v, const float3 &objectNg,
                                  const float3 &worldPoint,
                                  const float3 &rayDir) const;

public:
  [[nodiscard]] bool intersect(Ray &ray, Hit &hit) const;

  /// Build the hit record for a barycentric point on a triangle, in world
  /// space.
  ///
  /// The triangle's vertices are transformed to world space first and
  /// everything else is derived from the world triangle: interpolating
  /// transformed vertices is the same as transforming the interpolated
  /// vertex, and the geometry normal, geometry tangent, and UV density
  /// all come out right with no special cases. Shading normals transform
  /// by the instance's cofactor matrix, so they stay correct under shear
  /// and non-uniform scale.
  ///
  /// Both the camera hit path and area light sampling go through here, so
  /// that the geometry a light reports and the geometry a ray finds cannot
  /// drift apart.
  ///
  [[nodiscard]] Hit makeHit(uint32_t instIndex, uint32_t faceIndex,
                            const float3 &bary) const;

  /// The differential geometry of the shading normal field at a mesh or
  /// primitive hit, for the manifold connection walk. The point and
  /// normal reproduce the hit's own bit for bit; the normal partials
  /// come from the interpolated vertex normals or the analytic surface,
  /// through the same cofactor transform and winding flip `makeHit()`
  /// applies. Curve hits have no rebuildable parameterization and are
  /// not supported.
  [[nodiscard]] ManifoldGeometry manifoldGeometry(const Hit &hit) const;

  /// The index in `meshInstances` of the hit Embree reports: the
  /// geometry's first instance plus the element within it, which is 0
  /// for an ordinary instance and the array index for an instance
  /// array, so one decode covers both.
  [[nodiscard]] uint32_t instanceIndexOf(unsigned geomID,
                                         unsigned instPrimID) const noexcept {
    return instanceBaseByGeomID[geomID] + instPrimID;
  }

  /// The index in `materials` the instance actually shades with: its
  /// own binding if it has one, else its mesh's, primitive's, or
  /// groom's. Every consumer that turns an instance into a material
  /// goes through here, so that an instance-level override and a
  /// geometry-level binding cannot disagree about what emits, shadows,
  /// or shades.
  [[nodiscard]] uint32_t
  materialIndexOf(const MeshInstance &instance) const noexcept {
    if (instance.matIndex != INVALID_INDEX) return instance.matIndex;
    if (instance.isPrimitive()) return primitives[instance.primIndex]->matIndex;
    if (instance.isCurves()) return curves[instance.curvesIndex]->matIndex;
    return meshes[instance.meshIndex]->matIndex;
  }

public:
  const smdl::Compiler &compiler;       ///< The compiler.
  std::string fallbackMaterialName{};   ///< The unresolved-name substitute.
  RTCDevice device{};                   ///< The Embree device.
  RTCScene scene{};                     ///< The Embree scene.
  float3 boundCenter{};                 ///< The bound center.
  float boundRadius{};                  ///< The bound radius.
  std::vector<std::string> fileNames{}; ///< The files that were added.
  std::vector<std::unique_ptr<Mesh>> meshes{};          ///< The meshes.
  std::vector<std::unique_ptr<Primitive>> primitives{}; ///< The primitives.
  std::vector<std::unique_ptr<Curves>> curves{};        ///< The grooms.
  std::vector<MeshInstance> meshInstances{};            ///< The mesh instances.
  std::vector<const smdl::JIT::Material *> materials{}; ///< The materials.
  std::vector<std::string> materialNames{}; ///< Parallel to `materials`.

  /// The index in `materials` of each material name, so that a name shared
  /// between files resolves to one material.
  std::map<std::string, uint32_t, std::less<>> materialIndexByName{};

  /// What each file already contributed, keyed by canonical path, so that
  /// placing a file a second time costs a few Embree instances rather than
  /// another parse and another BVH. Deliberately blind to selections, so
  /// that the same file selected several ways is still imported once.
  std::map<std::string, ImportFile, std::less<>> importCache{};

  /// The index in `primitives` of each (shape, base material) pair, so
  /// that placing the same primitive again costs an Embree instance.
  std::map<std::string, uint32_t, std::less<>> primitiveCache{};

  /// The index in `curves` of each (file, spec, base material) triple,
  /// so that placing the same groom again costs an Embree instance.
  std::map<std::string, uint32_t, std::less<>> curvesCache{};

  /// The first `meshInstances` index of each attached geometry, by
  /// Embree geometry ID: an ordinary instance owns one entry and an
  /// instance array owns a contiguous run, so a hit decodes as
  /// `instanceBaseByGeomID[instID] + instPrimID`.
  std::vector<uint32_t> instanceBaseByGeomID{};
};
