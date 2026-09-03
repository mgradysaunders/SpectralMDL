/// \file
#pragma once

#include "smdl/Export.h"
#include "smdl/RenderUtil/SpectralColor.h"
#include "smdl/Support/Span.h"
#include "smdl/Support/VectorMath.h"

namespace smdl {

/// \addtogroup renderutil
/// \{

/// The approximate Mie phase function of Jendersie and d'Eon, "An
/// Approximate Mie Scattering Function for Fog and Cloud Rendering"
/// (SIGGRAPH 2023 Talks): a Henyey-Greenstein lobe carrying the
/// diffraction peak blended with a Draine lobe carrying the bulk, with
/// all four lobe parameters driven by the water droplet diameter
/// through the paper's piecewise fits.
///
/// Both lobes invert their cumulative distribution in closed form, so
/// the phase value is its own solid-angle sampling density and there is
/// neither a table nor a rejection loop. This is the same function the
/// builtin `df::fog_vdf` carries, fitted identically, so that one scene
/// reads the same phase function whichever side describes its medium.
///
/// The deflection cosine is measured from the direction of propagation
/// throughout, so forward scattering lives at `u` near 1. The direction
/// pair overloads take the JIT volume convention instead, where both
/// directions point away from the vertex and the deflection cosine is
/// `-dot(wo, wi)`.
class SMDL_EXPORT MiePhase final {
public:
  /// Construct isotropic.
  MiePhase() = default;

  /// Construct for a water droplet diameter in micrometers, clamped
  /// into `[0, 50]`, the domain of the parameter fits.
  explicit MiePhase(float dropletSize);

  /// The phase function of the deflection cosine `u`.
  [[nodiscard]] float evaluate(float u) const noexcept;

  /// The phase function of a direction pair, both pointing away from
  /// the scattering vertex.
  [[nodiscard]] float evaluate(const float3 &wo,
                               const float3 &wi) const noexcept {
    return evaluate(-dot(wo, wi));
  }

  /// Sample the phase function, returning its value, which is also the
  /// solid-angle density of the sample. The third component of `xi`
  /// picks the lobe.
  [[nodiscard]] float sample(float3 xi, const float3 &wo,
                             float3 &wi) const noexcept;

  /// \name Fitted parameters
  ///
  /// The lobe parameters the droplet diameter resolved to, which are
  /// what a test compares against the goldens of `df::fog_vdf`.
  ///
  /// \{

  /// The Henyey-Greenstein lobe anisotropy.
  [[nodiscard]] float asymmetryHG() const noexcept { return mGHG; }

  /// The Draine lobe anisotropy.
  [[nodiscard]] float asymmetryDraine() const noexcept { return mGD; }

  /// The Draine lobe shape parameter.
  [[nodiscard]] float alphaDraine() const noexcept { return mAlpha; }

  /// The Draine lobe's weight in the mixture, which is also the
  /// probability `sample()` picks it.
  [[nodiscard]] float weightDraine() const noexcept { return mWD; }

  /// \}

private:
  float mGHG{};

  float mGD{};

  float mAlpha{};

  float mWD{};
};

/// A directional source for `Haze::sunInscatter()`: the sun, or
/// anything else far enough away to read as a beam.
///
/// The caller builds this from whatever light model it has, so the haze
/// is coupled to the idea of a distant beam, which its closed form
/// genuinely requires, rather than to any one sky model.
class HazeSun final {
public:
  /// The unit direction toward the disk center, whose `z` must be
  /// positive: the closed form integrates the beam's path straight out
  /// of the atmosphere, which a source at or below the horizon does not
  /// have.
  float3 direction{0.0f, 0.0f, 1.0f};

  /// The cosine of the disk's angular radius, 1 for a point source.
  float cosRadius{1.0f};

  /// The spectral irradiance on a surface facing the disk, on the
  /// wavelength grid the `Haze` was built with. Empty means there is no
  /// source and the analytic term is off.
  SpectralColor irradiance{};

  /// Is there a source at all?
  [[nodiscard]] bool isValid() const noexcept {
    return irradiance.size() > 0 && direction.z > 0.0f;
  }

  /// Does `wi` point into the disk? Transport the analytic term already
  /// carries must not be gathered or arrived at a second time.
  [[nodiscard]] bool contains(const float3 &wi) const noexcept {
    return isValid() && dot(wi, direction) >= cosRadius;
  }
};

/// The parameters of a `Haze`: one aerosol species whose extinction
/// falls off exponentially with height above `baseHeight`.
struct HazeOptions final {
  /// The meteorological range in kilometers at 550nm, which fixes the
  /// extinction at `baseHeight` through Koschmieder's relation. The
  /// same quantity `SunSkyOptions::visibility` names.
  float visibility{23.0f};

  /// The scale height in meters. The default is the boundary-layer
  /// aerosol; molecular scattering is nearer 8000.
  float scaleHeight{1200.0f};

  /// The height in scene units at which the extinction is the one
  /// `visibility` names.
  float baseHeight{0.0f};

  /// The single-scattering albedo.
  float albedo{0.9f};

  /// The Angstrom exponent of the extinction: `a` in `sigma(lambda) =
  /// sigma(550nm) * (lambda / 550nm)^-a`. The default is the
  /// continental-rural aerosol; 4 is molecular scattering.
  float angstrom{1.3f};

  /// The water droplet diameter in micrometers driving the phase
  /// function; see `MiePhase`. The default is the accumulation-mode
  /// aerosol that dominates visibility reduction, and fog and cloud
  /// droplets run from about 5 to 50.
  float dropletSize{1.0f};
};

/// An exponential-height atmospheric haze: the participating medium
/// that produces aerial perspective.
///
/// Unlike a heterogeneous medium tracked against a majorant, this one is
/// analytic in every quantity a path tracer asks of it. Along a ray
/// leaving `org` toward the unit direction `dir`, the optical depth
/// separates into a per-band amplitude and one scalar shape,
///
///     tau_i(t) = sigmaC_i * shape(t),   shape(t) = (1 - exp(-k t)) / k,
///
/// where `sigmaC_i` is the extinction at the segment origin and `k =
/// dir.z / H`, both in inverse scene units. Every band therefore shares
/// one monotone shape, whose exact inverse is the free-flight distance
/// and whose limit `1/k` for an upward ray is the finite zenith optical
/// depth: a haze that falls off with height dims the horizon without
/// ever washing out the sky, which is what a homogeneous one cannot do.
///
/// Renderer-agnostic by design: it never traces a ray. A renderer
/// supplies the distant source through `HazeSun` and keeps the
/// transport-side work, including the visibility test that
/// `sunInscatter()` hands back a distance for.
///
/// The height is world `z`, matching the `+Z` zenith of `SunSky`.
/// Coefficients are in inverse scene units and distances in scene
/// units, converted from the physical parameters at construction with
/// `metersPerSceneUnit`, the same convention as `State`. A `Haze` is
/// immutable after construction, so render threads share one.
class SMDL_EXPORT Haze final {
public:
  Haze() = default;

  /// Construct on the wavelength grid in `wavelens`, in nanometers,
  /// which fixes the extinction spectrum. `sun` is the distant source
  /// the analytic term integrates, and may be left empty, in which case
  /// `sunInscatter()` reports nothing and the renderer is expected to
  /// estimate every source by sampling.
  Haze(const HazeOptions &options, Span<const float> wavelens,
       float metersPerSceneUnit, HazeSun sun = {});

  /// The band count of the wavelength grid, which sizes every spectral
  /// output.
  [[nodiscard]] size_t size() const noexcept { return mSigmaRef.size(); }

  /// The single-scattering albedo, uniform over the spectrum.
  [[nodiscard]] float albedo() const noexcept { return mAlbedo; }

  /// The phase function.
  [[nodiscard]] const MiePhase &phase() const noexcept { return mPhase; }

  /// The distant source, whose `isValid()` says whether there is one.
  [[nodiscard]] const HazeSun &sun() const noexcept { return mSun; }

  /// The extinction spectrum at `height` in scene units, in inverse
  /// scene units, written into `sigma`, which must have `size()`
  /// elements.
  void extinctionAt(float height, Span<float> sigma) const noexcept;

  /// The shape exponent `k` of a segment traveling with vertical
  /// direction component `dirZ`, in inverse scene units.
  [[nodiscard]] float shapeExponent(float dirZ) const noexcept {
    return dirZ * mInvScaleHeight;
  }

  /// The shared distance shape of the optical depth over `[0, t]`, which
  /// the per-band extinction at the segment origin scales.
  [[nodiscard]] static float shape(float k, float t) noexcept;

  /// The distance at which `shape` reaches `s`, or infinity when it
  /// never does, which is an upward ray leaving the atmosphere.
  [[nodiscard]] static float shapeInverse(float k, float s) noexcept;

  /// The phase function averaged over the source's disk, for a segment
  /// traveling toward `dir`, which is what `sunInscatter()` factors out
  /// of its integral.
  ///
  /// The analytic term treats the source as a direction, which the
  /// approximate Mie function will not stand for on its own: at a
  /// droplet diameter of 12 micrometers the diffraction peak falls by a
  /// quarter across the solar disk's own radius, so the value at the
  /// center is not the value the disk delivers. Sixteen directions
  /// stratified in solid angle over the cone are enough, the integrand
  /// being smooth at that scale.
  [[nodiscard]] float phaseOverSunDisk(const float3 &dir) const noexcept;

  /// The unshadowed single scattering of the source into the segment of
  /// length `tEnd` leaving `org` toward the unit direction `dir`,
  /// written to `radiance`, along with the distance `tShadow` at which
  /// to test whether the source reaches the segment at all.
  ///
  /// The whole segment is integrated in closed form. Writing the optical
  /// depths of the segment and of the source's own path out of the
  /// atmosphere as `a = tau_cam(tEnd)`, `b = tau_sun(0)` and `c =
  /// tau_sun(tEnd)`, the integral is
  ///
  ///     L = p(cos theta) * E * albedo * a * (e^-b - e^-(a+c)) / x,
  ///     x = a - b + c,
  ///
  /// per band, exact for a single exponential species and a directional
  /// source, with the `x` near zero limit `a * e^-b`. The scattering
  /// angle does not vary along the segment, the source being
  /// directional, which is what leaves the phase function outside the
  /// integral and makes it elementary.
  ///
  /// `tShadow` is drawn with `xi` from the density of that same
  /// integrand, so multiplying `radiance` by the source's visibility
  /// there estimates the shadowed integral. It is exact wherever the
  /// segment is entirely lit or entirely shadowed, which in an open
  /// scene is almost everywhere, and where a shadow edge crosses the
  /// segment it is off only by the spectral spread of the density, which
  /// vanishes with optical depth: at first order in the depth every band
  /// has the same normalized density.
  ///
  /// Returns false, leaving both outputs untouched, when there is
  /// nothing to add.
  [[nodiscard]] bool sunInscatter(const float3 &org, const float3 &dir,
                                  float tEnd, float xi, Span<float> radiance,
                                  float &tShadow) const;

private:
  /// The extinction spectrum at `mBaseHeight`, in inverse scene units.
  SpectralColor mSigmaRef{};

  /// See `albedo()`.
  float mAlbedo{};

  /// One over the scale height, in inverse scene units.
  float mInvScaleHeight{};

  /// The reference height in scene units.
  float mBaseHeight{};

  /// See `phase()`.
  MiePhase mPhase{};

  /// See `sun()`.
  HazeSun mSun{};
};

/// \}

} // namespace smdl
