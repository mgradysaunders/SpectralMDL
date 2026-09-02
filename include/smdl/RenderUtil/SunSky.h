/// \file
#pragma once

#include <vector>

#include "smdl/Export.h"
#include "smdl/RenderUtil/MonteCarlo.h"
#include "smdl/Support/VectorMath.h"

namespace smdl {

/// \addtogroup renderutil
/// \{

/// Options for `SunSky`.
struct SunSkyOptions final {
  /// The direction toward the sun, +Z up, need not be normalized. The
  /// sun zenith angle is clamped to the trained range of 5 to 88
  /// degrees, so a sun given at or below the horizon is glued slightly
  /// above it.
  float3 sunDirection{0.0f, 0.0f, 1.0f};

  /// The aerosol visibility in kilometers, clamped to the trained range
  /// of 5 to 100.
  float visibility{23.0f};

  /// The water-vapor column scale factor, clamped to the trained range
  /// of 0.3 to 3.
  float waterVaporScale{1.0f};

  /// The extra scale factor applied to all radiance outputs. The
  /// outputs are in W/(m^2 sr nm), the library-wide spectral radiance
  /// convention, so the default of 1 is physical as-is; multiply by
  /// 0.1 to recover the MODTRAN-native W/(cm^2 sr um).
  float scaleFactor{1.0f};

  /// Include the direct sun disk in `radiance()`, `sample()`, `pdf()`,
  /// and `averageRadiance()`? The sky dome is unaffected either way.
  bool enableSun{true};

  /// Apply MIS compensation to the sky sampling distribution? The mean
  /// sky radiance is subtracted from the tabulated density (clamped at
  /// zero), so light sampling stops spending samples where BSDF
  /// sampling already covers well (Karlík et al., SIGGRAPH Asia 2019).
  /// The pdf reported by `sample()` and `pdf()` is the true density
  /// actually sampled from, so the estimator stays unbiased.
  bool enableMISCompensation{true};

  /// Moonlight mode: treat the source as the moon instead of the sun.
  /// `sunDirection` positions the moon, and every radiance output
  /// (sky dome and disk alike) is multiplied per wavelength by the
  /// ROLO lunar-to-solar irradiance multiplier at `moonPhase`. This is
  /// exact for the atmosphere: scattered radiance is linear in the
  /// source irradiance, so a moonlit sky is the sunlit sky evaluated
  /// at the moon's position times the multiplier. The disk keeps the
  /// solar angular radius of 0.2665 degrees (the moon's mean is
  /// 0.259), so its radiance is ~5% dim while its irradiance is exact.
  bool moon{false};

  /// The signed lunar phase angle in degrees for moonlight mode,
  /// clamped to [-180, 180]: 0 is full moon, +/-180 new moon (where
  /// all outputs are zero), and the sign selects the waxing/waning
  /// branch (a 4-13% asymmetry). See `SunSky::moonMultiplier()`.
  float moonPhase{0.0f};

  /// The lunar distance factor for moonlight mode:
  /// (1 AU / sun-moon distance)^2 times (384400 km / observer-moon
  /// distance)^2. The default of 1 means mean distances; the observer
  /// term alone swings +/-14% over the anomalistic month.
  float moonDistanceScale{1.0f};
};

/// An empirical clear-sky sun and sky model fitted to MODTRAN
/// simulations, spanning the VNIR-SWIR range of 400nm to 2500nm with
/// the rural boundary-layer aerosol family.
///
/// The model is a pair of sparse polynomial fits on a universal
/// spectral grid of 421 channels in 5nm steps: scattered sky radiance
/// as a function of view direction, and direct solar irradiance, which
/// is exposed as the uniform radiance of a sun disk of angular radius
/// 0.2665 degrees. Evaluation at arbitrary wavelengths linearly
/// interpolates the grid, and wavelengths outside 400-2500nm clamp to
/// the end channels. All radiance outputs are in W/(m^2 sr nm), the
/// library-wide spectral radiance convention, see
/// `SunSkyOptions::scaleFactor`. Directions use +Z as the zenith. View
/// directions beyond the trained zenith range of 88 degrees clamp to
/// it, so the sky continues the horizon-ring values below the horizon.
///
/// Held-out accuracy vs MODTRAN: sky 1.5% median spectral error (CIE
/// dE 0.71 median), direct beam 1.7% median with 0.8% median broadband
/// irradiance error. Baked assumptions: sea level, mid-latitude
/// summer, surface albedo 0.15.
///
/// Moonlight mode (`SunSkyOptions::moon`) reuses both fits untouched
/// with the source at the moon's position and multiplies every output
/// per wavelength by the ROLO lunar-to-solar irradiance multiplier
/// (Kieffer & Stone 2005), see `moonMultiplier()`. Not included:
/// airglow (comparable to the moonlit sky below quarter phase) and
/// twilight from a sun just below the horizon.
///
/// A `SunSky` is immutable after construction, so all methods are safe
/// to call concurrently from render threads.
class SMDL_EXPORT SunSky final {
public:
  SunSky() = default;

  explicit SunSky(const SunSkyOptions &options);

public:
  /// The sky-dome spectral radiance toward `direction`, excluding the
  /// sun disk.
  ///
  /// The wavelengths in `wavelens` must be in nanometers, need not be
  /// sorted, and clamp to the model grid of 400-2500nm. The resulting
  /// `radiance` is in W/(m^2 sr nm) times the `scaleFactor` option.
  void skyRadiance(const float3 &direction, int numWavelens,
                   const float *wavelens, float *radiance) const;

  /// The sun-disk spectral radiance, uniform over the disk: the direct
  /// solar irradiance divided by `sunSolidAngle()`. Filled with zeros
  /// if the `enableSun` option is off. Same wavelength and unit
  /// conventions as `skyRadiance()`.
  void sunRadiance(int numWavelens, const float *wavelens,
                   float *radiance) const;

  /// The total spectral radiance toward `direction`: the sky plus the
  /// sun disk when `direction` is inside it.
  void radiance(const float3 &direction, int numWavelens, const float *wavelens,
                float *radiance) const;

  /// The unit direction toward the sun-disk center after the zenith
  /// clamp, which is the direction the model actually uses.
  [[nodiscard]] float3 sunDirection() const noexcept { return sunDir; }

  /// Is the sun disk enabled?
  [[nodiscard]] bool hasSun() const noexcept { return sunEnabled; }

  /// The cosine of the solar angular radius of 0.2665 degrees.
  [[nodiscard]] float cosSunAngularRadius() const noexcept;

  /// The solid angle of the sun disk.
  [[nodiscard]] float sunSolidAngle() const noexcept;

  /// The direction sampling routine, distributed approximately
  /// proportional to broadband radiance: a power-weighted mixture of
  /// uniform-cone sampling over the sun disk and a tabulated sky
  /// distribution over the sphere.
  ///
  /// \param[in]  xi   The random sample \f$ \xi \in (0,1)^2 \f$.
  /// \param[out] pdf  If non-null, receives the full mixture density
  ///                  in solid angle, i.e., exactly `pdf()` of the
  ///                  returned direction.
  ///
  [[nodiscard]] float3 sample(float2 xi, float *pdf = {}) const noexcept;

  /// The solid-angle density `sample()` realizes in `direction`, for
  /// multiple importance sampling.
  [[nodiscard]] float pdf(const float3 &direction) const noexcept;

  /// The broadband mean radiance over the sphere of directions
  /// including the sun, for weighing against other lights in light
  /// selection. Broadband means the average over the model's spectral
  /// channels, times the `scaleFactor` option.
  [[nodiscard]] float averageRadiance() const noexcept { return meanRadiance; }

  /// The dimensionless ROLO lunar-to-solar irradiance multiplier
  /// m(wavelength, phase) with E_moon = m * E_sun, from the
  /// disk-integrated lunar reflectance model of Kieffer & Stone 2005
  /// at zero libration: the per-wavelength factor moonlight mode
  /// applies on top of the sun-sky fit. The wavelength is in
  /// nanometers (clamped to 350-2500), the signed phase angle in
  /// degrees as in `SunSkyOptions::moonPhase`, and `distanceScale` as
  /// in `SunSkyOptions::moonDistanceScale`. Magnitude for intuition:
  /// about 2.4e-6 at 550nm at full moon, falling to 6.7e-8 at quarter
  /// phase and exactly zero at new moon.
  [[nodiscard]] static double moonMultiplier(double wavelenNm, double phaseDeg,
                                             double distanceScale = 1.0);

private:
  /// The scale factor applied to every radiance output:
  /// `SunSkyOptions::scaleFactor` times the conversion from the fit
  /// tables' native W/(cm^2 sr um) to W/(m^2 sr nm).
  float scaleFactor{1.0f};

  /// Is the sun disk enabled?
  bool sunEnabled{false};

  /// The effective unit direction toward the sun, zenith clamped.
  float3 sunDir{0.0f, 0.0f, 1.0f};

  /// The unit horizontal projection of `sunDir`, for relative-azimuth
  /// calculations.
  float2 sunDirHorizontal{1.0f, 0.0f};

  /// The clamped sun zenith angle in degrees.
  double sunZenithDeg{};

  /// The clamped aerosol visibility in kilometers.
  double visibility{};

  /// The clamped water-vapor column scale factor.
  double waterVapor{};

  /// The number of sky fit outputs: the log broadband brightness and one
  /// coefficient per spectral mode. Mirrors `SKY_OUTPUT_COUNT` in the fit
  /// tables, which the implementation static asserts against.
  static constexpr int SKY_FIT_OUTPUT_COUNT = 6;

  /// The number of terms the specialized sky polynomial can have: every
  /// monomial up to the cubic in the five features a view direction
  /// supplies, padded to the accumulator count of its kernel. It is a
  /// bound rather than the count of any one fit, so that a refit which
  /// evaluates fewer of them does not change the size of this class and
  /// oblige every consumer to rebuild. The implementation static asserts
  /// that the fit it carries fits; see `skyMatrix`.
  static constexpr int SKY_FIT_TERM_COUNT = 56;

  /// The number of output lanes the kernel carries, which is the output
  /// count rounded up to a vector.
  static constexpr int SKY_FIT_OUTPUT_STRIDE = 8;

  /// \name Sky fit constants
  ///
  /// The parts of the sky fit's standardized feature vector that no view
  /// direction can change. They are computed once here rather than on
  /// every evaluation, which is where the whole cost of a `std::pow` and
  /// six other transcendentals used to fall.
  ///
  /// \{

  /// The cosine of the clamped sun zenith angle.
  float cosSunZenith{1.0f};

  /// The sine of the clamped sun zenith angle.
  float sinSunZenith{};

  /// The standardized cosine-of-sun-zenith feature.
  double zSunZenith{};

  /// The standardized log-sun-airmass feature.
  double zSunAirmass{};

  /// The standardized log-visibility feature.
  double zVisibility{};

  /// The standardized log-water-vapor feature.
  double zWaterVapor{};

  /// \}

  /// Evaluate the sky fit for one view direction, writing the log
  /// broadband brightness and the spectral mode coefficients.
  ///
  /// The view zenith arrives both as an angle in degrees and as its
  /// cosine, already clamped to the trained range, because the caller
  /// has both and recovering either from the other costs a transcendental.
  ///
  void evalSkyFit(float cosView, float viewZenithDeg,
                  float cosRelativeAzimuth,
                  float (&outputs)[SKY_FIT_OUTPUT_COUNT]) const noexcept;

  /// The sky polynomial specialized to these options: the fit is cubic
  /// in nine standardized features, four of which no view direction can
  /// change (see the sky fit constants above), so folding those four
  /// into the coefficients here leaves a polynomial in the five the view
  /// supplies. It is the same polynomial, evaluated over a quarter as
  /// many terms.
  ///
  /// Row `t` holds the weight of term `t` in every output, so the
  /// contraction is one vertical multiply-accumulate per term with the
  /// outputs falling out as the lanes of the accumulator.
  alignas(32) float skyMatrix[SKY_FIT_TERM_COUNT][SKY_FIT_OUTPUT_STRIDE]{};

  /// The direct solar spectral irradiance on the model grid in the fit
  /// tables' native W/(cm^2 um), evaluated once at construction (it
  /// does not depend on the view direction). Unscaled by `scaleFactor`,
  /// which carries the conversion to output units. In moonlight mode
  /// this is the direct lunar irradiance, i.e., already multiplied by
  /// `channelScale`.
  std::vector<float> sunIrradiance{};

  /// The per-channel spectral scale on the model grid: empty in sun
  /// mode, the ROLO lunar multiplier in moonlight mode. Applies to the
  /// sky-dome evaluation and the sampling weights; `sunIrradiance` has
  /// it baked in already.
  std::vector<float> channelScale{};

  /// The probability of `sample()` picking the sun disk.
  float sunSelectionProbability{};

  /// The sampling distribution over the sky dome, possibly
  /// MIS-compensated, see `SunSkyOptions::enableMISCompensation`.
  Distribution2D skyDistr{};

  /// The broadband mean radiance over the sphere, sun included, times
  /// `scaleFactor`.
  float meanRadiance{};
};

/// \}

} // namespace smdl
