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
/// Renderer-agnostic by design: it never traces a ray, and knows
/// nothing about the lights the medium it describes is lit by.
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
  /// which fixes the extinction spectrum.
  Haze(const HazeOptions &options, Span<const float> wavelens,
       float metersPerSceneUnit);

  /// The band count of the wavelength grid, which sizes every spectral
  /// output.
  [[nodiscard]] size_t size() const noexcept { return mSigmaRef.size(); }

  /// The single-scattering albedo, uniform over the spectrum.
  [[nodiscard]] float albedo() const noexcept { return mAlbedo; }

  /// The phase function.
  [[nodiscard]] const MiePhase &phase() const noexcept { return mPhase; }

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
};

/// \}

} // namespace smdl
