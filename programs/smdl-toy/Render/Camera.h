#pragma once

#include "Common.h"
#include "Render/Sampler.h"

/// The camera and lens parameters, merged from the scene file's `camera`
/// directive and the command line into plain values. The
/// occurrence-dependent CLI checks (mutually exclusive flags, explicitly
/// given values that must be positive) run before this is built, so here
/// zero uniformly means "unset" for every quantity that derives a default
/// and "off" for every optional effect.
///
/// `lookFrom` and `lookTo` are not final here under `-autolook`:
/// `solveAutolook()` overwrites both after `Scene::commit()` measures the
/// geometry, so the `Camera` is constructed only once that has run.
struct CameraOptions final {
  /// The image dimensions in pixels.
  int2 resolution{1280, 720};

  /// The position to look from.
  float3 lookFrom{-6, 0, 2};

  /// The position to look to.
  float3 lookTo{0, 0, 0.5f};

  /// The up vector.
  float3 lookUp{0, 0, 1};

  /// The vertical field of view in degrees.
  float fovYDeg{37.8f};

  /// Enable DOF by f-number assuming a 35mm-format frame, or 0.
  /// Mutually exclusive with `aperture`.
  float fStop{};

  /// Enable DOF by aperture radius in scene units, or 0.
  float aperture{};

  /// The focus distance along the view axis in scene units, or 0 to
  /// use the distance between `lookFrom` and `lookTo`.
  float focus{};

  /// The number of aperture blades, or 0 for a round lens.
  int blades{};

  /// With `blades`, the rotation of the aperture polygon in degrees.
  float bladeAngleDeg{};

  /// The radial distortion (barrel > 0, pincushion < 0), in units of
  /// relative corner displacement.
  float distortionK1{};

  /// The quartic term of radial distortion, in the same units.
  float distortionK2{};

  /// Refit so frame corner directions hold constant under distortion.
  bool distortionFit{};

  /// The strength of cos^4 falloff: 0 is off, 1 is the physical law.
  float vignetting{};

  /// Mechanical vignette from the lens barrel: relative displacement at
  /// the frame corner in units of rim radius, 0 is off.
  float catEye{};

  /// With `catEye`, the barrel rim radius in scene units, or 0 to use
  /// the aperture radius (wide open).
  float catEyeRadius{};

  /// Disable LOD by zeroing the camera ray cone spread.
  bool noLOD{};
};

/// One camera ray, built by `Camera::sample()`.
struct CameraSample final {
  /// The world-space ray, direction normalized.
  Ray ray{};

  /// The camera response: 1 unless a vignetting mechanism is on, and
  /// exactly 0 for a sample the lens barrel blocks. A zero-weight
  /// sample must still count in the pixel average to keep the
  /// darkening unbiased.
  float weight{1};

  /// The ray cone spread that seeds the LOD state, already scaled by
  /// the local distortion footprint. Zero switches the cone off end to
  /// end.
  float coneAngle{};
};

/// The camera: everything between a pixel coordinate and a world-space
/// ray carrying a response weight, which is the thin lens, the radial
/// distortion, and the natural and mechanical vignetting.
class Camera final {
public:
  /// Validate the value-dependent constraints (blade count, vignetting
  /// ranges, the distortion map staying monotone over the frame),
  /// derive everything else, and log the enabled lens effects.
  ///
  /// Nothing here depends on the scene, so construct this before
  /// anything slow loads and a typo fails fast.
  ///
  /// \throws smdl::Error if validation fails.
  explicit Camera(const CameraOptions &options);

  /// Sample the camera ray for the pixel `(x, y)`.
  ///
  /// Consumes sampler dimensions in a fixed order downstream renders
  /// depend on: the pixel jitter is always dimensions 0-1, and the
  /// lens point is drawn only when DOF is enabled, so a pinhole
  /// render consumes exactly the dimensions it would with no lens
  /// code at all.
  [[nodiscard]] CameraSample sample(size_t x, size_t y,
                                    Sampler &sampler) const noexcept;

private:
  /// The image dimensions in pixels.
  float numPixelsX{}, numPixelsY{};

  /// The aspect ratio, X over Y.
  float aspectRatio{};

  /// The image plane distance in units of image height.
  float focalLength{};

  /// One pixel of the image plane (height 1 at distance `focalLength`)
  /// subtends this angle, or 0 when LOD is disabled.
  float coneAngleBase{};

  /// The camera-to-world transform, orthonormal by construction.
  float4x4 cameraToWorld{float4x4(1.0f)};

  /// The image radius at the frame corner, which normalizes the
  /// distortion polynomial and the rim displacement.
  float rCorner{};

  /// The radial distortion coefficients.
  float distortionK1{}, distortionK2{};

  /// Is either distortion coefficient nonzero?
  bool hasDistortion{};

  /// Under `distortionFit` the whole map is divided by its value at
  /// the corner, so only the interior warps. The monotonicity scan in
  /// the constructor guarantees the divisor is positive.
  float distortionScale{1};

  /// The thin-lens radius in scene units, zero for the pinhole default.
  float lensRadius{};

  /// The focus distance along the view axis in scene units.
  float focusDistance{};

  /// The number of aperture blades, 0 for a round lens.
  int numBlades{};

  /// The rotation of the aperture polygon in radians.
  float bladeAngle{};

  /// The strength of cos^4 falloff, 0 when off.
  float vignetteStrength{};

  /// The barrel rim radius in scene units, 0 when mechanical
  /// vignetting is off.
  float rimRadius{};

  /// The rim displacement per unit of image radius, 0 when off.
  float rimSlope{};
};
