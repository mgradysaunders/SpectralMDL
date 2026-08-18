#pragma once

#include <optional>

#include "Scene.h"

#include "smdl/Compiler.h"
#include "smdl/Image.h"
#include "smdl/Support/MonteCarlo.h"
#include "smdl/Support/SunSky.h"

class EnvLight final {
public:
  EnvLight() = default;

  EnvLight(const std::string &filename, float scaleFactor = 1.0f);

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
/// emission EDF.
class AreaLight final {
public:
  /// The index in the `Scene::meshInstances` array.
  uint32_t meshInstanceIndex{INVALID_INDEX};

  /// The area-weighted distribution over the mesh faces. Empty for a
  /// primitive light, which samples its shape analytically instead.
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

  /// The object-space surface area (primitive lights only).
  float objectArea{};

  /// The inverse of the instance's cofactor matrix (primitive lights
  /// only): what turns a WORLD unit normal back into the local area
  /// stretch, so the MIS pdf of a BSDF-sampled hit is exact even under
  /// a deformed placement. See `LightSampler::solidAnglePDF()`.
  float3x3 invCofactor{};
};

/// The unified light-selection path over every light in the scene: each
/// emissive mesh instance plus the environment, weighted by power.
class LightSampler final {
public:
  LightSampler(smdl::Compiler &compiler, const Scene &scene,
               const EnvLight *envLight, const Color &wavelengths);

  /// The result of `sample()`.
  struct LightSample final {
    /// The direction from the receiving point toward the light.
    float3 wi{};

    /// The point to test visibility against.
    float3 target{};

    /// The full density of this sample in solid angle at the receiving
    /// point: the selection PMF times the per-light directional PDF.
    float pdf{};

    /// The unoccluded incident radiance.
    Color Li{};
  };

  /// Are there no lights to sample?
  [[nodiscard]] bool empty() const noexcept {
    return lightDistr.size() == 0 || !(lightDistr.unnormalizedSum() > 0.0f);
  }

  /// The environment light, or null.
  [[nodiscard]] const EnvLight *env() const noexcept { return envLight; }

  /// The probability of light selection picking the environment.
  [[nodiscard]] float envSelectionPMF() const noexcept {
    return envLight && !empty() ? lightDistr.indexPMF(int(areaLights.size()))
                                : 0.0f;
  }

  /// Sample a direction toward one light from `point`. The `state` is
  /// copied to construct the material instance at the sampled point, so it
  /// must carry the allocator and wavelengths. Returns `false` on a zero
  /// probability or zero radiance sample.
  [[nodiscard]] bool sample(smdl::State state, Sampler &sampler,
                            const float3 &point,
                            LightSample &lightSample) const;

  /// The emitted radiance of an already-constructed material instance in
  /// direction `wi` pointing away from the emitting surface, with the
  /// `intensity_power` area normalization applied. Returns `false` if the
  /// instance does not emit in `wi`.
  [[nodiscard]] bool
  emittedRadiance(const smdl::JIT::MaterialInstance &materialInstance,
                  uint32_t meshInstanceIndex, const float3 &wi,
                  Color &Le) const;

  /// The solid-angle density of `sample()` connecting `point` to
  /// `lightPoint` on the given mesh instance, for MIS when a BSDF sample
  /// happens to hit an emitter. Returns zero if the mesh instance is not a
  /// registered light.
  [[nodiscard]] float solidAnglePDF(uint32_t meshInstanceIndex,
                                    const float3 &lightPoint,
                                    const float3 &lightNormal,
                                    const float3 &point) const;

private:
  smdl::Compiler &compiler;

  const Scene &scene;

  const EnvLight *envLight{};

  std::vector<AreaLight> areaLights{};

  /// Map from mesh instance index to index in `areaLights`, or
  /// `INVALID_INDEX`.
  std::vector<uint32_t> instanceToLight{};

  /// The power-weighted distribution over `areaLights`, with one extra
  /// entry at the end for the environment if present.
  smdl::Distribution1D lightDistr{};
};
