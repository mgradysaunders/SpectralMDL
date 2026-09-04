#pragma once

#include <memory>
#include <optional>

#include "Layout/Layout.h"
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
  bool sampled{};

  /// The area-weighted distribution over the mesh faces. Empty for a
  /// primitive light, which samples its shape analytically instead,
  /// and for an unsampled light, which is never drawn.
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
  bool caustic{};

  /// The object-space surface area (primitive lights only).
  float objectArea{};

  /// The inverse of the instance's cofactor matrix (primitive lights
  /// only): what turns a WORLD unit normal back into the local area
  /// stretch, so the MIS pdf of a BSDF-sampled hit is exact even under
  /// a deformed placement. See `LightSampler::solidAnglePDF()`.
  float3x3 invCofactor{};
};

/// A light the layout declares: a point, a spot, an IES profile, a
/// rectangle, or a disk, described analytically rather than by a
/// surface in the scene. BSDF sampling can never reach one, so every
/// kind contributes through `LightSampler::sample()` alone, with a unit
/// MIS weight (see `LightSample::reachable`). The punctual kinds are
/// Dirac besides; the two shapes are sampled by area, one-sided and
/// Lambertian, in the plane the placement puts them in.
class AnalyticLight final {
public:
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
  /// profile's support.
  [[nodiscard]] Color Li(const float3 &point,
                         float metersPerSceneUnit) const noexcept;

  /// Punctual kinds: the same with the directional (spot cone or
  /// profile) factor evaluated toward `incidencePoint` instead, what a
  /// manifold gather needs when the segment actually arriving at the
  /// light starts at the last chain crossing rather than at the
  /// receiver. The inverse-square falloff stays at `point`, matching
  /// the straight-line solid-angle measure the gather's estimator is
  /// built in.
  [[nodiscard]] Color Li(const float3 &point, const float3 &incidencePoint,
                         float metersPerSceneUnit) const noexcept;

  /// Shapes: a point drawn uniformly over the shape as placed, and its
  /// position density, which is one over the world area everywhere. A
  /// planar shape has one area stretch under any affine placement, so
  /// object-uniform is world-uniform and the density is exact.
  [[nodiscard]] float3 sampleShape(float2 xi,
                                   float &positionPDF) const noexcept;

  /// Shapes: the radiance emitted from `lightPoint` on the shape toward
  /// `incidencePoint`, the baked radiance when that point is on the
  /// emitting side of the plane and zero behind it.
  [[nodiscard]] Color Le(const float3 &lightPoint,
                         const float3 &incidencePoint) const noexcept;

  /// Shapes: the unit normal of the emitting side.
  [[nodiscard]] const float3 &normal() const noexcept { return mNormal; }

  /// The position in world space: the point itself, or the center of a
  /// shape.
  [[nodiscard]] const float3 &position() const noexcept { return mPosition; }

  /// The light selection weight: the mean over the render bands of the
  /// spectral radiant power after the tint. This is the quantity the
  /// area lights weigh by (their per-band emission intensity times
  /// area), so a declared light and an emissive surface of the same
  /// brightness draw the same share of samples.
  [[nodiscard]] float weight() const noexcept { return mWeight; }

  /// Is a caustic target; see `AreaLight::caustic`. Normalized by the
  /// `LightSampler` constructor, hence settable.
  bool caustic{};

private:
  LayoutLightDecl::Kind mKind{LayoutLightDecl::Kind::POINT};

  /// The position, or the center of a shape.
  float3 mPosition{};

  /// Punctual kinds: the world-space rows of the local frame, emission
  /// aiming along the local -Z axis. Columns of the placement,
  /// normalized; a sheared placement gets the nearest frame rather than
  /// an error.
  float3 mLocalX{1.0f, 0.0f, 0.0f};
  float3 mLocalY{0.0f, 1.0f, 0.0f};
  float3 mLocalZ{0.0f, 0.0f, 1.0f};

  /// Shapes: the world-space in-plane axes, the placement's first two
  /// columns unnormalized so that their lengths carry the scale; the
  /// half extents along them in object units; the unit normal of the
  /// emitting side, which is the side the placement maps local -Z
  /// into; and the world area.
  float3 mAxisU{1.0f, 0.0f, 0.0f};
  float3 mAxisV{0.0f, 1.0f, 0.0f};
  float2 mHalfExtent{};
  float3 mNormal{0.0f, 0.0f, -1.0f};
  float mWorldArea{};

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
  bool reachable{};

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
  bool caustic{true};
};

/// The unified light-selection path over every light in the scene: each
/// emissive mesh instance the layout marks `light`, plus the layout's
/// declared lights, plus the environment, weighted by power. Every
/// other emissive instance renders through the path hits the walk finds
/// on its own, at MIS weight 1; see `AreaLight::sampled`.
class LightSampler final {
public:
  /// `allLights` samples every emissive instance whether or not it is
  /// marked: the `-all-lights` switch, and what a render without a
  /// layout to carry marks wants.
  LightSampler(smdl::Compiler &compiler, const Scene &scene,
               const EnvLight *envLight,
               const std::vector<LayoutLight> &layoutLights,
               const Color &wavelengths, bool allLights = false);

  /// Are there no lights to sample?
  [[nodiscard]] bool empty() const noexcept {
    return lightDistr.size() == 0 || !(lightDistr.unnormalizedSum() > 0.0f);
  }

  /// The environment light, or null.
  [[nodiscard]] const EnvLight *env() const noexcept { return envLight; }

  /// The probability of light selection picking the environment.
  [[nodiscard]] float envSelectionPMF() const noexcept {
    return envLight && !empty()
               ? lightDistr.indexPMF(
                     int(areaLights.size() + analyticLights.size()))
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
  /// the same paths on the strength of the gather producing them.
  [[nodiscard]] bool sample(const smdl::State &state, Sampler &sampler,
                            const float3 &point, LightSample &lightSample,
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
  [[nodiscard]] Color reevaluateLi(const LightSample &lightSample,
                                   const smdl::State &state,
                                   const float3 &point,
                                   const float3 &incidencePoint) const;

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
    const auto lightIndex{instIndex < instanceToLight.size()
                              ? instanceToLight[instIndex]
                              : INVALID_INDEX};
    return lightIndex != INVALID_INDEX && areaLights[lightIndex].sampled &&
           areaLights[lightIndex].caustic;
  }

  /// Is an environment escape a caustic target's; see `causticLight()`.
  [[nodiscard]] bool causticEnv() const noexcept { return mEnvCaustic; }

  /// The solid-angle density of `sample()` connecting `point` to
  /// `lightPoint` on the given mesh instance, for MIS when a BSDF sample
  /// happens to hit an emitter. Returns zero if the mesh instance is not a
  /// sampled light.
  [[nodiscard]] float solidAnglePDF(uint32_t instIndex,
                                    const float3 &lightPoint,
                                    const float3 &lightNormal,
                                    const float3 &point) const;

private:
  smdl::Compiler &compiler;

  const Scene &scene;

  const EnvLight *envLight{};

  std::vector<AreaLight> areaLights{};

  std::vector<AnalyticLight> analyticLights{};

  /// Map from mesh instance index to index in `areaLights`, or
  /// `INVALID_INDEX`.
  std::vector<uint32_t> instanceToLight{};

  /// The power-weighted distribution over `areaLights`, then
  /// `analyticLights`, with one extra entry at the end for the
  /// environment if present. An unsampled area light has weight zero,
  /// which is what keeps it out of `sample()` and out of every PMF.
  smdl::Distribution1D lightDistr{};

  /// Is the environment a caustic target: true exactly while no light
  /// carries a mark, since the environment cannot be marked.
  bool mEnvCaustic{true};
};
