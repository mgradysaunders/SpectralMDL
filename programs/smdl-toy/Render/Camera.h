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

  /// Does the camera move over the shutter? When set, the three shut
  /// keys below are the framing at shutter shut, and a ray at shutter
  /// fraction `u` sees the framing vectors interpolated linearly
  /// between the two. Field of view, focus, aperture, and distortion
  /// hold over the shutter.
  bool motion{};

  /// The position to look from at shutter shut.
  float3 lookFromShut{};

  /// The position to look to at shutter shut.
  float3 lookToShut{};

  /// The up vector at shutter shut.
  float3 lookUpShut{};

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

/// One camera ray, built by `Camera::sample()` and placed in the world
/// by `Camera::toWorld()`.
struct CameraSample final {
  /// The ray: in camera space with an unnormalized direction as
  /// `sample()` leaves it, in world space with the direction
  /// normalized and the time stamped once `toWorld()` has run.
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

  /// Sample the camera ray for the pixel `(x, y)`, in camera space.
  ///
  /// Consumes sampler dimensions in a fixed order downstream renders
  /// depend on: the pixel jitter is always dimensions 0-1, and the
  /// lens point is drawn only when DOF is enabled, so a pinhole
  /// render consumes exactly the dimensions it would with no lens
  /// code at all. The ray stays in camera space until `toWorld()`, so
  /// that the caller can draw the shutter fraction after the lens
  /// point and before the frame is chosen.
  [[nodiscard]] CameraSample sample(size_t x, size_t y,
                                    Sampler &sampler) const noexcept;

  /// Place the ray `sample()` built into the world at shutter fraction
  /// `u`: apply the camera frame at `u`, normalize the direction, and
  /// stamp the ray's time. A still camera applies its one frame
  /// whatever `u` is, here; a moving camera builds its frame at `u` out
  /// of line, in `toWorldMoving()`.
  void toWorld(CameraSample &sample, float u) const noexcept {
    if (mMoving) return toWorldMoving(sample, u);
    sample.ray.transform(mCameraToWorld);
    sample.ray.dir = normalize(sample.ray.dir);
    sample.ray.time = u;
  }

private:
  /// The moving half of `toWorld()`: the look-at of the framing vectors
  /// interpolated to `u`, see `mLookFrom`.
  SMDL_NO_INLINE void toWorldMoving(CameraSample &sample,
                                    float u) const noexcept;

  /// The image dimensions in pixels.
  float mNumPixelsX{}, mNumPixelsY{};

  /// The aspect ratio, X over Y.
  float mAspectRatio{};

  /// The image plane distance in units of image height.
  float mFocalLength{};

  /// One pixel of the image plane (height 1 at distance `mFocalLength`)
  /// subtends this angle, or 0 when LOD is disabled.
  float mConeAngleBase{};

  /// The camera-to-world transform at shutter open, orthonormal by
  /// construction.
  float4x4 mCameraToWorld{float4x4(1.0f)};

  /// Does the camera move over the shutter? False when the shut keys
  /// equal the open keys, so a still camera exported under motion blur
  /// renders bit for bit what it renders exported without.
  bool mMoving{};

  /// The framing at shutter open and at shutter shut, read only when
  /// `mMoving`. The frame at fraction `u` is the look-at of the vectors
  /// interpolated as `(1 - u) * open + u * shut`, spelled so that the
  /// two ends reproduce the keys exactly. The view direction is then
  /// the normalized chord, whose angular rate differs from a slerp's by
  /// third order in the pan angle: nothing over the angle a shutter
  /// spans, and the interpolation of what the file states.
  float3 mLookFrom{}, mLookTo{}, mLookUp{};
  float3 mLookFromShut{}, mLookToShut{}, mLookUpShut{};

  /// The image radius at the frame corner, which normalizes the
  /// distortion polynomial and the rim displacement.
  float mRCorner{};

  /// The radial distortion coefficients.
  float mDistortionK1{}, mDistortionK2{};

  /// Is either distortion coefficient nonzero?
  bool mHasDistortion{};

  /// Under `distortionFit` the whole map is divided by its value at
  /// the corner, so only the interior warps. The monotonicity scan in
  /// the constructor guarantees the divisor is positive.
  float mDistortionScale{1};

  /// The thin-lens radius in scene units, zero for the pinhole default.
  float mLensRadius{};

  /// The focus distance along the view axis in scene units.
  float mFocusDistance{};

  /// The number of aperture blades, 0 for a round lens.
  int mNumBlades{};

  /// The rotation of the aperture polygon in radians.
  float mBladeAngle{};

  /// The strength of cos^4 falloff, 0 when off.
  float mVignetteStrength{};

  /// The barrel rim radius in scene units, 0 when mechanical
  /// vignetting is off.
  float mRimRadius{};

  /// The rim displacement per unit of image radius, 0 when off.
  float mRimSlope{};
};
