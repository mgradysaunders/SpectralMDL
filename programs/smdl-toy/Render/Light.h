#pragma once

#include <memory>
#include <optional>

#include "Layout/Layout.h"
#include "Render/LightTree.h"
#include "Render/Sampler.h"
#include "Scene/Scene.h"

#include "smdl/Compiler.h"
#include "smdl/RenderUtil/MonteCarlo.h"
#include "smdl/RenderUtil/SunSky.h"
#include "smdl/Resource/Image.h"
#include "smdl/Resource/LightProfile.h"

class EnvLight final {
public:
  EnvLight() = default;

  EnvLight(const std::string &fileName, float scaleFactor = 1.0f);

  /// Construct as the procedural MODTRAN-fitted sun and sky instead of
  /// an image, see `smdl::SunSky`. The radiance is evaluated spectrally
  /// at the render wavelengths, so unlike the image path there is no
  /// RGB round trip, and the reported pdf is the sun/sky mixture
  /// density, so the existing MIS logic covers the sun disk.
  explicit EnvLight(const smdl::SunSkyOptions &options);

  [[nodiscard]] Color Li(smdl::Compiler &compiler, const smdl::State &state,
                         float3 wi, float &pdf) const;

  [[nodiscard]] float3 Li_sample(smdl::Compiler &compiler,
                                 const smdl::State &state, float2 xi,
                                 float &pdf, Color &Li) const;

  /// The mean radiance over the sphere of directions, for weighing the
  /// environment against area lights in light selection.
  [[nodiscard]] float averageRadiance() const noexcept {
    return scaleFactor * meanRadiance;
  }

  /// The procedural sun disk, for gating manifold work to the sun cone:
  /// fills the unit direction toward the disk center and the cosine of
  /// its angular radius and returns true when constructed as the
  /// procedural sun-sky with the sun enabled. An image environment has
  /// no analytic sun and returns false.
  [[nodiscard]] bool sunCone(float3 &direction, float &cosRadius) const {
    if (!sunSky || !sunSky->hasSun()) return false;
    direction = sunSky->sunDirection();
    cosRadius = sunSky->cosSunAngularRadius();
    return true;
  }

private:
  float scaleFactor{1.0f};

  /// The procedural sun and sky in place of `image` when constructed
  /// from `smdl::SunSkyOptions`.
  std::optional<smdl::SunSky> sunSky{};

  smdl::Image image{};

  /// The sampling distribution over the image, MIS-compensated: the mean
  /// radiance is subtracted from the tabulated density (clamped at zero),
  /// so light sampling stops spending samples where BSDF sampling already
  /// covers well (Karlík et al., SIGGRAPH Asia 2019). The pdf this
  /// reports is the true density actually sampled from, so the estimator
  /// stays unbiased; texels at or below the mean are reachable only by
  /// BSDF sampling, whose MIS weight there becomes 1.
  smdl::Distribution2D imageDistr{};

  /// The mean image radiance before `scaleFactor`, kept because
  /// compensation makes it unrecoverable from `imageDistr`.
  float meanRadiance{};
};

/// A mesh or primitive instance whose material has a non-default
/// emission EDF, whether or not light selection aims at it.
class AreaLight final {
public:
  /// The index in the `Scene::meshInstances` array.
  uint32_t instIndex{INVALID_INDEX};

  /// Does light selection aim at it? From the instance's `light` mark
  /// (see `LayoutAssetDecl::light`) or the `-all-lights` switch. An
  /// unsampled emitter is registered for `totalArea` alone, which the
  /// `intensity_power` normalization of its path hits needs, and gets
  /// no selection weight: `sample()` never draws it, `solidAnglePDF()`
  /// reports zero for it, and it is never a caustic target, so its
  /// arrivals keep their full weight.
  bool isSampled{};

  /// The area-weighted distribution over the mesh faces, by world area
  /// for a static light and by object area at the open key for a moving
  /// or deforming one, whose world area is a function of time. Empty
  /// for a primitive light, which samples its shape analytically
  /// instead, and for an unsampled light, which is never drawn.
  smdl::Distribution1D faceDistr{};

  /// The total world-space surface area: the divisor `intensity_power`
  /// emission requires the host to apply, and, for a mesh light, also
  /// the reciprocal of the uniform area sampling density. A primitive
  /// light's sampling density comes from `objectArea` and the exact
  /// area stretch of its placement instead; under a deformed
  /// (non-similarity) placement this field is itself the shape's mean
  /// stretch estimate, which biases only the power heuristic weight and
  /// the `intensity_power` normalization, never the pdf.
  float totalArea{};

  /// Lights a primitive rather than a mesh?
  bool isPrimitive{};

  /// Is a caustic target, from the instance's layout mark, normalized
  /// by the `LightSampler` constructor so that "no marks anywhere"
  /// reads as every light marked.
  bool isCaustic{};

  /// The object-space surface area: a primitive's, or a moving or
  /// deforming mesh light's at the open key, whose faces are drawn by
  /// their object areas and whose density is each face's share over its
  /// world area at the time; zero for a static mesh light, which is
  /// drawn by world area.
  float objectArea{};

  /// The inverse of the instance's cofactor matrix (static primitive
  /// lights only; a moving light derives it from the frame at the
  /// time): what turns a WORLD unit normal back into the local area
  /// stretch, so the MIS pdf of a BSDF-sampled hit is exact even under
  /// a deformed placement. See `LightSampler::solidAnglePDF()`.
  float3x3 invCofactor{};

  /// A sphere under a similarity placement, which `LightSampler::sample()`
  /// draws by its cone from the receiver: the world center and radius
  /// at the open key, the radius zero for every other light, and the
  /// object radius that the frame at a time turns into a moving sphere's
  /// world center and radius.
  float3 sphereCenter{};
  float sphereRadius{};
  float sphereObjectRadius{};
};

/// A light the layout declares: a point, a spot, an IES profile, a
/// rectangle, or a disk, described analytically rather than by a
/// surface in the scene. BSDF sampling can never reach one, so every
/// kind contributes through `LightSampler::sample()` alone, with a unit
/// MIS weight (see `LightSample::isReachable`). The punctual kinds are
/// Dirac besides; the two shapes are sampled by area, one-sided and
/// Lambertian, in the plane the placement puts them in.
///
/// A light whose placement carries a shut key moves as the camera
/// does: the placement matrix lerps between the keys and the light is
/// derived from the lerped matrix at the time asked about, so a turning
/// spot's axis follows the chord of its end directions. Nothing in
/// Embree traces these lights, so there is no second interpolation to
/// agree with. The intensity is baked once, from the open key.
class AnalyticLight final {
public:
  /// What a placement matrix derives to: the position, the local frame
  /// of a punctual kind, and the in-plane axes, emitting-side normal,
  /// and world area of a shape. A static light holds one; a moving
  /// light derives one on the stack for the time asked about.
  class Placement final {
  public:
    float3 position{};

    /// Punctual kinds: the world-space rows of the local frame, emission
    /// aiming along the local -Z axis. Columns of the placement,
    /// normalized; a sheared placement gets the nearest frame rather
    /// than an error.
    float3 localX{1.0f, 0.0f, 0.0f};
    float3 localY{0.0f, 1.0f, 0.0f};
    float3 localZ{0.0f, 0.0f, 1.0f};

    /// Shapes: the world-space in-plane axes, the placement's first two
    /// columns unnormalized so that their lengths carry the scale; the
    /// unit normal of the emitting side, which is the side the placement
    /// maps local -Z into; and the world area.
    float3 axisU{1.0f, 0.0f, 0.0f};
    float3 axisV{0.0f, 1.0f, 0.0f};
    float3 normal{0.0f, 0.0f, -1.0f};
    float worldArea{};
  };

  /// Bake the lowered light at the render wavelengths: the spectral
  /// shape (blackbody or flat, times the uplifted RGB tint) normalized
  /// to unit integral over the band, scaled into the per-kind
  /// directional intensity, or into the radiance for a shape. `state`
  /// must carry the wavelength fields for the RGB uplift and the scene
  /// units, and `profile` the loaded IES profile for a PROFILE light
  /// (null otherwise).
  AnalyticLight(smdl::Compiler &compiler, const smdl::State &state,
                const Color &wavelengths, const LayoutLight &light,
                std::shared_ptr<const smdl::LightProfile> profile);

  /// Is the directional density a delta? True for the punctual kinds,
  /// false for the two shapes.
  [[nodiscard]] bool isDirac() const noexcept {
    return mKind != LayoutLightDecl::Kind::RECT &&
           mKind != LayoutLightDecl::Kind::DISK;
  }

  /// Punctual kinds: the unoccluded spectral incident radiance
  /// equivalent at `point`, the directional intensity toward it over
  /// the squared distance in meters. Zero outside a spot cone or the
  /// profile's support. `time` is the path's shutter fraction, which
  /// every method here takes and which only a moving light reads.
  [[nodiscard]] Color Li(const float3 &point, float metersPerSceneUnit,
                         float time) const noexcept;

  /// Punctual kinds: the same with the directional (spot cone or
  /// profile) factor evaluated toward `incidencePoint` instead, what a
  /// manifold gather needs when the segment actually arriving at the
  /// light starts at the last chain crossing rather than at the
  /// receiver. The inverse-square falloff stays at `point`, matching
  /// the straight-line solid-angle measure the gather's estimator is
  /// built in.
  [[nodiscard]] Color Li(const float3 &point, const float3 &incidencePoint,
                         float metersPerSceneUnit, float time) const noexcept;

  /// Shapes: a point on the shape as placed, and its density in solid
  /// angle at `receiver`. A rect whose placed axes are orthogonal is
  /// drawn uniformly over the spherical rectangle it subtends (Urena,
  /// Fajardo, and King, "An Area-Preserving Parametrization for
  /// Spherical Rectangles", 2013), so every direction into it is
  /// equally likely; otherwise, and for a disk, uniformly by area, which
  /// a planar shape keeps exact under any affine placement. The density
  /// is zero when the receiver is in the shape's plane. A moving shape
  /// pays its area at the time in the density, so a shape that grows
  /// over the shutter emits more.
  [[nodiscard]] float3 sampleShape(const float3 &receiver, float2 xi,
                                   float &pdf, float time) const noexcept;

  /// Shapes: the radiance emitted from `lightPoint` on the shape toward
  /// `incidencePoint`, the baked radiance when that point is on the
  /// emitting side of the plane and zero behind it.
  [[nodiscard]] Color Le(const float3 &lightPoint, const float3 &incidencePoint,
                         float time) const noexcept;

  /// Shapes: the unit normal of the emitting side.
  [[nodiscard]] float3 normal(float time) const noexcept;

  /// The position in world space: the point itself, or the center of a
  /// shape.
  [[nodiscard]] float3 position(float time) const noexcept;

  /// The world-space box: the point itself, or a shape's corners, at
  /// both keys of a moving light, which is a hull, since a lerped corner
  /// moves on a segment.
  [[nodiscard]] BoundBox3 bounds() const noexcept;

  /// The light selection weight: the mean over the render bands of the
  /// spectral radiant power after the tint. This is the quantity the
  /// area lights weigh by (their per-band emission intensity times
  /// area), so a declared light and an emissive surface of the same
  /// brightness draw the same share of samples.
  [[nodiscard]] float weight() const noexcept { return mWeight; }

  /// Is a caustic target; see `AreaLight::caustic`. Normalized by the
  /// `LightSampler` constructor, hence settable.
  bool isCaustic{};

private:
  /// The placement `xf` derives to; see `Placement`.
  [[nodiscard]] Placement derivePlacement(const float4x4 &xf) const noexcept;

  /// The placement at `time`: `mPlacement` for a static light, and for
  /// a moving one the lerped matrix derived into `scratch`. The
  /// returned reference is to one or the other, never to a temporary.
  [[nodiscard]] const Placement &
  placementAt(float time, std::optional<Placement> &scratch) const noexcept;

  LayoutLightDecl::Kind mKind{LayoutLightDecl::Kind::POINT};

  /// The placement at the open key, and the placement at every time of
  /// a static light.
  Placement mPlacement{};

  /// The two keys, read only under `mMoving`.
  bool mIsMoving{};
  float4x4 mLightToWorld{float4x4(1.0f)};
  float4x4 mLightToWorldShut{float4x4(1.0f)};

  /// Shapes: the half extents along the in-plane axes in object units,
  /// and the object area they enclose.
  float2 mHalfExtent{};
  float mObjectArea{};

  /// The per-band spectral intensity: for the punctual kinds W/(sr nm),
  /// the full intensity of a point, the on-axis peak of a spot, and the
  /// per-unit multiplier on the profile's broadband W/sr; for a shape
  /// the radiance in W/(sr m^2 nm).
  smdl::SpectralColor mIntensity{};

  /// SPOT: the cosine of the outer (cutoff) and inner (full intensity)
  /// half angles.
  float mCosOuter{-1.0f};
  float mCosInner{-1.0f};

  /// PROFILE: the loaded profile, shared between placements of the
  /// same file (see the dedupe cache in the `LightSampler` constructor).
  std::shared_ptr<const smdl::LightProfile> mProfile{};

  float mWeight{};
};

/// The result of `LightSampler::sample()`.
struct LightSample final {
  /// The direction from the receiving point toward the light.
  float3 wi{};

  /// The point to test visibility against.
  float3 target{};

  /// The full density of this sample in solid angle at the receiving
  /// point: the selection PMF times the per-light directional PDF.
  /// For a Dirac light the directional density is a Dirac delta and
  /// `pdf` is the selection PMF alone, with the delta folded into `Li`.
  float pdf{};

  /// The unoccluded incident radiance along the straight segment; zero
  /// for a sample kept by `keepDark`.
  Color Li{};

  /// Is the directional density a Dirac delta? True for a punctual
  /// light: `pdf` is then the selection PMF alone and `Li` carries the
  /// falloff. Says nothing about MIS; see `reachable`.
  bool isDirac{};

  /// Can the walk's continuation, BSDF sampling, arrive at this light?
  /// True for an area light and the environment, whose arrivals compete
  /// with this sample and are weighed against it; false for every light
  /// the layout declares, which has no surface a path can hit, so this
  /// sample's MIS weight is 1 whether its density is a delta or not.
  bool isReachable{};

  /// Sampled the environment? The target is then a far point in the
  /// `wi` direction rather than a real position.
  bool isInfinite{};

  /// The emitter's unit normal at the sampled point for a finite sample
  /// with an orientation (an area light's geometric normal), which the
  /// manifold target's offset Jacobian reads, and a shape light's
  /// emitting-side normal; zero for a punctual or an infinite sample,
  /// which have none.
  float3 normal{};

  /// The index in the analytic light array when the sample is one of
  /// the lights the layout declares, else `INVALID_INDEX`: the identity
  /// `reevaluateLi()` needs.
  uint32_t analyticIndex{INVALID_INDEX};

  /// The sampled point on an area light, empty for the other kinds:
  /// what `reevaluateLi()` rebuilds the emitting material from.
  Hit hit{};

  /// Is the sampled light a caustic target, one the manifold
  /// reflective gather searches the casters for? Always true while no
  /// light in the scene carries the layout's `caustic` mark; with any
  /// mark, only the marked lights are targets, the arrivals at
  /// everything else keep their ordinary weights, and the environment
  /// is not a target at all.
  bool isCaustic{true};
};

/// The light selection strategy: which light a gather draws for a
/// receiver, and with what probability, over the index space
/// `LightSampler` keeps (its area lights, then its analytic lights,
/// then one entry for the environment if present). Every query names
/// the receiving point, because the selection density is defined per
/// receiver: an arrival site recomputes a light's probability for the
/// receiver whose gather competes with the arrival, and it must be the
/// probability that gather drew with.
///
/// Two strategies: the `LightTree`, which weighs each light by its
/// power over its squared distance to the receiver, and on request the
/// flat power-weighted distribution, the same probabilities at every
/// receiver. The environment keeps the flat distribution's share under
/// both, so the tree redistributes the lights' share alone. An
/// unsampled area light has weight zero, which keeps it out of
/// `select()` and out of every PMF either way.
class LightSelection final {
public:
  LightSelection() = default;

  /// Construct over the lights' bounds and weights in index order, then
  /// the environment's weight when `hasEnv`, through the tree when
  /// `useTree` and the flat distribution otherwise.
  LightSelection(smdl::Span<const LightBounds> lights, bool hasEnv,
                 float envWeight, bool useTree);

  /// Is there nothing to select?
  [[nodiscard]] bool empty() const noexcept {
    return mDistr.size() == 0 || !(mDistr.unnormalizedSum() > 0.0f);
  }

  /// Select a light for the receiver at `point` on the uniform `xi`,
  /// returning its index and filling `pmf` with its probability.
  [[nodiscard]] int select(const float3 &point, float xi,
                           float &pmf) const noexcept;

  /// The probability that `select()` picks `lightIndex` for the
  /// receiver at `point`.
  [[nodiscard]] float pmf(int lightIndex, const float3 &point) const noexcept;

  /// The tree, or null under the flat distribution.
  [[nodiscard]] const LightTree *tree() const noexcept {
    return mTree ? &*mTree : nullptr;
  }

private:
  smdl::Distribution1D mDistr{};

  std::optional<LightTree> mTree{};

  /// The number of lights, which is also the environment's index.
  int mLightCount{};

  bool mHasEnv{};
};

/// The unified light-selection path over every light in the scene: each
/// emissive mesh instance the layout marks `light`, plus the layout's
/// declared lights, plus the environment, weighted by power and, through
/// the `LightTree`, by distance to the receiver. Every other emissive
/// instance renders through the path hits the walk finds on its own, at
/// MIS weight 1; see `AreaLight::isSampled`.
class LightSampler final {
public:
  /// `allLights` samples every emissive instance whether or not it is
  /// marked: the `-all-lights` switch, and what a render without a
  /// layout to carry marks wants. `useTree` selects through the
  /// `LightTree` rather than the flat distribution, which is what
  /// `-no-light-tree` asks for; see `LightSelection`.
  LightSampler(smdl::Compiler &compiler, const Scene &scene,
               const EnvLight *envLight,
               const std::vector<LayoutLight> &layoutLights,
               const Color &wavelengths, bool allLights = false,
               bool useTree = false);

  /// Are there no lights to sample?
  [[nodiscard]] bool empty() const noexcept { return mSelection.empty(); }

  /// The environment light, or null.
  [[nodiscard]] const EnvLight *env() const noexcept { return mEnvLight; }

  /// The probability of light selection picking the environment for
  /// the receiver at `point`.
  [[nodiscard]] float envSelectionPMF(const float3 &point) const noexcept {
    return mEnvLight && !empty()
               ? mSelection.pmf(
                     int(mAreaLights.size() + mAnalyticLights.size()), point)
               : 0.0f;
  }

  /// Sample a direction toward one light from `point`. The `state` must
  /// carry the allocator and wavelengths; an area sample copies it to
  /// construct the material instance at the sampled point, and the other
  /// kinds only read it, so the copy is confined to the branch that
  /// mutates. Returns `false` on a zero probability sample, and on a
  /// zero radiance one unless `keepDark`.
  ///
  /// `keepDark` keeps an area or punctual sample that radiates nothing
  /// toward `point`, with `Li` zero and the measure untouched. A manifold
  /// connection arrives at the light from its last crossing, not from
  /// `point`, and re-evaluates the radiance from there: a point on a lamp
  /// that faces the mirror but not the receiver, or a spot cone aimed at
  /// the glass, is a legitimate target for it, and refusing such samples
  /// loses their transport outright, since the path tracer is barred from
  /// the same paths on the strength of the gather producing them. For
  /// the same reason a `keepDark` draw of a sphere is uniform over its
  /// whole area, where a plain draw is uniform over the cone the sphere
  /// subtends at `point` and never lands on the far side.
  ///
  /// `time` is the path's shutter fraction, which the hit an area sample
  /// carries is built at; see `PathTime`.
  [[nodiscard]] bool sample(const smdl::State &state, Sampler &sampler,
                            const float3 &point, float time,
                            LightSample &lightSample,
                            bool keepDark = false) const;

  /// Re-evaluate a sample's incident radiance for a segment that
  /// arrives at the light from `incidencePoint` rather than from
  /// `point`, the receiver it was sampled from. `state` must carry the
  /// allocator, wavelengths, and scene units; an area sample copies it
  /// to rebuild the emitting material at its `hit`.
  ///
  /// What a manifold connection needs, because the segment that actually
  /// arrives at the light starts at the last chain crossing, and a light
  /// that does anything with direction radiates something else that way:
  /// a spot cone or an IES profile evaluated toward the crossing, an
  /// emitter's EDF and which of its sides faces that way, possibly
  /// nothing at all. The distance falloff and the measure are untouched,
  /// since they carry the straight-line solid-angle measure the gather's
  /// estimator is built in: a punctual sample keeps its inverse square
  /// at `point`, an area sample's `pdf` stands as it is, and only the
  /// directional part moves. The environment has no position to
  /// re-evaluate from and returns the sample's own `Li`.
  ///
  /// This is the same quantity the path tracer reads off an emitter it
  /// hits through the chain, so the two halves of the manifold estimator
  /// agree on what the transport carries, including on its being zero.
  ///
  /// `time` is the path's shutter fraction, which a moving declared
  /// light is placed at; an area sample carries its own on its hit.
  [[nodiscard]] Color reevaluateLi(const LightSample &lightSample,
                                   const smdl::State &state,
                                   const float3 &point,
                                   const float3 &incidencePoint,
                                   float time) const;

  /// The emitted radiance of an already-constructed material instance in
  /// direction `wi` pointing away from the emitting surface, with the
  /// `intensity_power` area normalization applied. Returns `false` if the
  /// instance does not emit in `wi`.
  [[nodiscard]] bool emittedRadiance(const smdl::JIT::MaterialInstance &mat,
                                     uint32_t instIndex, const float3 &wi,
                                     Color &Le) const;

  /// Is an arrival at an emitter on the given mesh instance a caustic
  /// target's, so the claimed share of the arriving throughput is the
  /// reflective gather's to drop? An unsampled emitter is nobody's
  /// target: light selection never aims at it, so no gather claims its
  /// transport and its arrivals keep their ordinary weights.
  [[nodiscard]] bool causticLight(uint32_t instIndex) const noexcept {
    const auto lightIndex{instIndex < mInstanceToLight.size()
                              ? mInstanceToLight[instIndex]
                              : INVALID_INDEX};
    return lightIndex != INVALID_INDEX && mAreaLights[lightIndex].isSampled &&
           mAreaLights[lightIndex].isCaustic;
  }

  /// Is an environment escape a caustic target's; see `causticLight()`.
  [[nodiscard]] bool causticEnv() const noexcept { return mEnvCaustic; }

  /// The solid-angle density of `sample()` connecting `point` to
  /// `lightPoint` on face `faceIndex` of the given mesh instance, for
  /// MIS when a BSDF sample happens to hit an emitter. `areaSampled`
  /// says the gather at `point` drew by area, as a `keepDark` draw
  /// does, rather than by a sphere's cone. `time` is the hit's shutter
  /// fraction, which a moving or deforming light's geometry is read at;
  /// the face is what a moving or deforming mesh light's density is
  /// recovered from, since its faces are drawn by object area and each
  /// pays its own world area at the time. Returns zero if the mesh
  /// instance is not a sampled light, or the cone does not reach
  /// `lightPoint`.
  [[nodiscard]] float solidAnglePDF(uint32_t instIndex, uint32_t faceIndex,
                                    const float3 &lightPoint,
                                    const float3 &lightNormal,
                                    const float3 &point, bool areaSampled,
                                    float time) const;

private:
  /// Draw a sphere light by its cone from `point`: the hit at the
  /// sampled point and the density in solid angle, the sphere given by
  /// its world center and radius and the instance's frame at the time.
  /// Returns false with nothing drawn when `point` is inside the
  /// sphere, for the caller to draw by area instead.
  [[nodiscard]] bool sampleSphereCone(const AreaLight &light,
                                      const InstanceFrame &frame,
                                      const float3 &center, float radius,
                                      const float3 &point, float time,
                                      float2 xi, Hit &hit, float &pdf) const;

  /// The area-light draw of `sample()` for a moving or deforming
  /// emitter: the frame resolved once at the path's time, every read
  /// through it, a primitive light drawn uniformly by object area with
  /// the exact stretch of that frame, and a mesh light by a face drawn
  /// by its object area and a point uniform within the face as it
  /// stands at the time, at the density that face's share over its
  /// world area then. Draws the static path's dimensions in the static
  /// path's order. Fills the hit and the position density, or the cone
  /// density when the sphere cone drew.
  [[nodiscard]] bool sampleAreaMoving(const AreaLight &light,
                                      const MeshInstance &instance,
                                      Sampler &sampler, const float3 &point,
                                      float time, bool keepDark, Hit &hit,
                                      float &positionPDF, float &conePDF) const;

  smdl::Compiler &mCompiler;

  const Scene &mScene;

  const EnvLight *mEnvLight{};

  std::vector<AreaLight> mAreaLights{};

  std::vector<AnalyticLight> mAnalyticLights{};

  /// Map from mesh instance index to index in `areaLights`, or
  /// `INVALID_INDEX`.
  std::vector<uint32_t> mInstanceToLight{};

  /// The selection over `areaLights`, then `analyticLights`, then the
  /// environment if present; see `LightSelection`.
  LightSelection mSelection{};

  // TODO Revisit this?
  /// Is the environment a caustic target: true exactly while no light
  /// carries a mark, since the environment cannot be marked.
  bool mEnvCaustic{true};
};
