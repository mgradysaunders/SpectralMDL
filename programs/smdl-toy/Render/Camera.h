#pragma once

#include <optional>

#include "Common.h"

#include "Render/Lens.h"
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
  bool hasMotion{};

  /// The position to look from at shutter shut.
  float3 lookFromShut{};

  /// The position to look to at shutter shut.
  float3 lookToShut{};

  /// The up vector at shutter shut.
  float3 lookUpShut{};

  /// The lens to look through, or none for the thin lens model.
  ///
  /// With one, the field of view is a consequence of the sensor and the
  /// prescription rather than an input, so `fovYDeg` becomes a way of
  /// asking for a sensor, and the distortion, the vignetting and the
  /// cat's eye are all emergent rather than settings. The caller refuses
  /// those combinations rather than silently ignoring them, so nothing
  /// here has to.
  std::optional<LensPrescription> lens{};

  /// The sensor width and height in millimeters. With `lens`, zero
  /// solves it from `fovYDeg`, or takes the 36 by 24 of full frame when
  /// that is zero too; without, zero is full frame, and the size sets
  /// the pixel pitch and the frame height `fStop` is a fraction of.
  float2 sensorMM{};

  /// With `lens`, normalize the exposure to the lens's own f-number, so
  /// that the frame holds its brightness whatever lens is on it and
  /// however far it is stopped down. That is what the thin lens does,
  /// where `fStop` buys depth of field and costs nothing.
  ///
  /// Otherwise, and by default, the response is the physical one: an
  /// ideal f/1 lens reads 1 on axis and a slower one reads `1 / N^2`, so
  /// two lenses and two apertures are on the same exposure and a fast
  /// lens is worth what it is worth.
  bool shouldNormalizeLensExposure{};

  /// The vertical field of view in degrees.
  ///
  /// With a lens it is not a setting but a request: the sensor height
  /// that looks out at it is solved from the traced field angle, and the
  /// width follows from the picture's shape. Zero takes the default
  /// sensor instead, and `sensorMM` states the sensor outright; the two
  /// contradict, and stating both is refused before this is built.
  float fovYDeg{37.8f};

  /// Enable DOF by f-number, or 0: with a lens its working stop, with
  /// the thin lens the aperture radius as a fraction of the focal length
  /// `sensorMM` makes real. Mutually exclusive with `aperture`.
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
  bool shouldFitDistortion{};

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

  /// The camera response, and exactly 0 for a sample the lens barrel
  /// blocks. A zero-weight sample must still count in the pixel average
  /// to keep the darkening unbiased.
  ///
  /// The thin lens reads 1 unless a vignetting mechanism is on. A real
  /// lens reads the pupil integral instead, which is `1 / N^2` on axis
  /// for an ideal lens of f-number `N` and less wherever the glass takes
  /// something, unless `CameraOptions::shouldNormalizeLensExposure`
  /// takes the f-number back out.
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
    if (mIsMoving) return toWorldMoving(sample, u);
    sample.ray.transform(mCameraToWorld);
    sample.ray.dir = normalize(sample.ray.dir);
    sample.ray.time = u;
  }

  /// The frame's physical size in scene units: the sensor a lens covers,
  /// or the frame the thin lens's field of view spans, which
  /// `CameraOptions::sensorMM` states and full frame stands in for.
  [[nodiscard]] float2 sensorSize() const noexcept {
    return float2(mSensorWidth, mSensorHeight);
  }

  /// The working f-number: a lens's own, stopped down by `fStop`; a thin
  /// lens's aperture radius against the focal length its sensor height
  /// makes real, which is `fStop` back again when that set the radius;
  /// and 0 for a pinhole, which has none.
  [[nodiscard]] float fNumber() const noexcept;

  /// Does the film hold radiance? The thin lens and a lens normalized to
  /// its f-number average radiance; a lens on the physical exposure
  /// averages the pupil integral instead, `4 / pi` times the spectral
  /// irradiance at the sensor, which is what the spectral output's units
  /// line has to say.
  [[nodiscard]] bool holdsRadiance() const noexcept {
    return !mLens || mIsLensExposureNormalized;
  }

  /// What turns one unit of film into spectral irradiance at the sensor
  /// in W/(m^2 nm), for a readout that counts electrons. A lens on the
  /// physical exposure holds `4 / pi` times the irradiance exactly, on
  /// and off axis, so this is `pi / 4`; normalized to its f-number, and
  /// for a thin lens with an aperture, it is `pi / (4 N^2)`, the paraxial
  /// form for a lens focused at infinity, which the exact circular
  /// pupil's `pi / (4 N^2 + 1)` sits under by 0.4% at f/8; and 0 for a
  /// pinhole, whose film is radiance with no pupil to turn it into
  /// anything.
  [[nodiscard]] double irradianceScale() const noexcept;

private:
  /// The two halves of the constructor that differ, one of which runs.
  /// Everything the two share, the framing and its motion, the focus
  /// distance and the aperture polygon, is settled before either.
  ///
  /// \throws smdl::Error if the prescription cannot be a camera lens.
  void buildLens(const CameraOptions &options);
  void buildThinLens(const CameraOptions &options);

  /// The lens half of `sample()`, out of line as `toWorldMoving()` is,
  /// so that the thin lens keeps the smaller body.
  [[nodiscard]] SMDL_NO_INLINE CameraSample
  sampleThroughLens(float u, float v, Sampler &sampler) const noexcept;

  /// The moving half of `toWorld()`: the look-at of the framing vectors
  /// interpolated to `u`, see `mLookFrom`.
  SMDL_NO_INLINE void toWorldMoving(CameraSample &sample,
                                    float u) const noexcept;

  /// The image dimensions in pixels.
  float mNumPixelsX{}, mNumPixelsY{};

  /// The aspect ratio, X over Y.
  float mAspectRatio{};

  /// The lens, or none for the thin lens model. Its presence is what
  /// `sample()` branches on, and the only thing it branches on.
  std::optional<Lens> mLens{};

  /// With `mLens`, the table that says where on the rear aperture a
  /// film point has any chance of getting a ray out. Present whenever
  /// `mLens` is.
  std::optional<ExitPupil> mExitPupil{};

  /// The frame's physical size in scene units: the sensor a lens
  /// covers, or the frame the thin lens spans, which sizes its pixels
  /// and makes its focal length real.
  float mSensorWidth{}, mSensorHeight{};

  /// With `mLens`, the response one unit of drawn pupil area carries:
  /// the pupil integral's own `1 / d^2`, over the constant that sets the
  /// exposure convention. See `CameraOptions`.
  float mExposurePerPupilArea{};

  /// With `mLens`, is the exposure normalized to its f-number? See
  /// `CameraOptions::shouldNormalizeLensExposure`.
  bool mIsLensExposureNormalized{};

  /// The image plane distance in units of image height. Unused with a
  /// lens, whose film distance is a real one and lives in `mLens`.
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
  bool mIsMoving{};

  /// The framing at shutter open and at shutter shut, read only when
  /// `mIsMoving`. The frame at fraction `u` is the look-at of the vectors
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

  /// Under `shouldFitDistortion` the whole map is divided by its value at
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
