#pragma once

#include <optional>

#include "Common.h"

#include "Render/Lens.h"
#include "Render/Sampler.h"

/// What one unit of film holds, which the sensor decides and never the
/// lens: the observer's film holds spectral radiance, and a physical
/// sensor's holds the spectral irradiance at the focal plane, which is
/// what a photon-counting detector integrates. See
/// `CameraOptions::filmQuantity`.
enum class FilmQuantity { RADIANCE, IRRADIANCE };

/// The camera and lens parameters, merged from the camera file, the
/// files it names, and the command line into plain values. The
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
  /// With one, the field of view is a consequence of the frame and the
  /// prescription rather than an input, and the distortion, the
  /// vignetting and the cat's eye are all emergent rather than settings.
  /// The caller refuses those combinations rather than silently ignoring
  /// them, so nothing here has to.
  std::optional<LensPrescription> lens{};

  /// The frame's physical size in scene units, which are meters: a
  /// physical sensor's pixels times its pitch, or for the observer a
  /// frame 24 mm tall whose width follows the picture. With a lens it is
  /// what the prescription images onto and so decides the field of view;
  /// with the thin lens it sizes the pixels a readout counts over and
  /// makes the focal length `fStop` is a fraction of real. Never zero:
  /// the caller decides it.
  float2 frameSize{1e-3f * 36.0f, 1e-3f * 24.0f};

  /// What the film holds, which decides the weight a sample carries.
  ///
  /// Radiance is the observer's: the thin lens weights by its vignetting
  /// factor alone, and a lens is normalized to its own f-number so that
  /// it reads the scene radiance on axis. Irradiance is a physical
  /// sensor's: the pupil integral exactly, `(A / d^2) cos^4` for a lens
  /// and `(pi R^2 / z^2) cos^4` for the thin lens, so that a readout
  /// counting electrons from the film is exact.
  FilmQuantity filmQuantity{FilmQuantity::RADIANCE};

  /// The vertical field of view in degrees, which the thin lens spans
  /// over `frameSize`. Refused with a lens, whose field is the frame and
  /// the glass together.
  float fovYDeg{37.8f};

  /// Enable DOF by f-number, or 0: with a lens its working stop, with
  /// the thin lens the aperture radius as a fraction of the focal length
  /// `frameSize` makes real. Mutually exclusive with `aperture`.
  float fStop{};

  /// Enable DOF by aperture radius in scene units, or 0.
  float aperture{};

  /// The focus distance along the view axis in scene units, measured
  /// from the camera origin, which is the entrance pupil of both optics;
  /// `INF` to focus at infinity; or 0 to use the distance between
  /// `lookFrom` and `lookTo`. Never `focus auto` here: the model settles
  /// that into a distance by measuring the committed scene before the
  /// camera is built, as it settles `-autolook`.
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

  /// The strength of cos^4 falloff: 0 is off, 1 is the physical law,
  /// taken over the film-to-lens segment of each sample with the film at
  /// the focal length. The observer's knob: a physical sensor's
  /// irradiance weight carries the law exactly, at the image distance,
  /// and ignores this.
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
  /// On the observer's film the thin lens reads 1 unless a vignetting
  /// mechanism is on, and a lens reads its pupil integral normalized to
  /// its f-number, which is 1 on axis for an ideal lens. On a physical
  /// sensor's film both read the pupil integral itself, the thin lens's
  /// `pi / (4 N^2)` on axis at infinity focus; see
  /// `CameraOptions::filmQuantity`.
  float weight{1};

  /// The ray cone spread that seeds the LOD state, already scaled by
  /// the local distortion footprint. Zero switches the cone off end to
  /// end.
  float coneAngle{};
};

/// The depth of field of a lens, in scene units: what `depthOfField()`
/// computes from the focal length, the f-number, the focus distance,
/// and the frame, and what one log line per render and the camera
/// report state.
struct DepthOfField final {
  /// The circle of confusion the limits are taken at: the frame
  /// diagonal over 1500, the usual print-viewing criterion, which is
  /// 0.029 mm on full frame.
  float circleOfConfusion{};

  /// The hyperfocal distance, `f^2 / (N c) + f`: focused here, the far
  /// limit is at infinity and the near limit as close as it gets.
  float hyperfocal{};

  /// The near and far limits of acceptable sharpness. `farLimit` is
  /// `INF` at or beyond the hyperfocal distance, and both are `INF` for
  /// a pinhole, which has no aperture to blur with.
  ///
  /// \{
  float nearLimit{};
  float farLimit{};
  /// \}

  /// Does anything blur? False for a pinhole.
  [[nodiscard]] bool hasLimits() const noexcept { return nearLimit < INF; }
};

/// The depth of field of a lens of focal length `focalLength` at
/// f/`fNumber`, focused at `focus` (`INF` for infinity), on a frame of
/// `frameSize`, all in scene units, by the thin-lens closed forms:
/// `H = f^2 / (N c) + f`, near `s (H - f) / (H + s - 2 f)`, far
/// `s (H - f) / (H - s)`. A `fNumber` of 0 is a pinhole, and reads as
/// everything in focus.
[[nodiscard]] DepthOfField depthOfField(float focalLength, float fNumber,
                                        float focus, float2 frameSize) noexcept;

/// The camera: everything between a pixel coordinate and a world-space
/// ray carrying a response weight, which is the thin lens or the traced
/// one, the radial distortion, and the natural and mechanical
/// vignetting.
class Camera final {
public:
  /// Validate the value-dependent constraints (blade count, vignetting
  /// ranges, the distortion map staying monotone over the frame, a pupil
  /// to integrate irradiance over), derive everything else, and log the
  /// enabled lens effects.
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

  /// The frame's physical size in scene units, as `CameraOptions::frameSize`
  /// gave it.
  [[nodiscard]] float2 frameSize() const noexcept {
    return float2(mFrameWidth, mFrameHeight);
  }

  /// The working f-number: a lens's own, stopped down by `fStop`; a thin
  /// lens's aperture radius against the focal length its frame height
  /// makes real, which is `fStop` back again when that set the radius;
  /// and 0 for a pinhole, which has none.
  [[nodiscard]] float fNumber() const noexcept;

  /// The effective focal length in scene units: a lens's own, or the
  /// thin lens's over the frame its field of view spans.
  [[nodiscard]] float focalLength() const noexcept {
    return mLens ? mLens->focalLength() : mFocalLength * mFrameHeight;
  }

  /// The focus distance along the view axis in scene units, `INF` at
  /// infinity.
  [[nodiscard]] float focusDistance() const noexcept { return mFocusDistance; }

  /// The depth of field, by `depthOfField()` over what this camera
  /// resolved.
  [[nodiscard]] DepthOfField depthOfField() const noexcept {
    return ::depthOfField(focalLength(), fNumber(), mFocusDistance,
                          frameSize());
  }

private:
  /// The two halves of the constructor that differ, one of which runs.
  /// Everything the two share, the framing and its motion, the focus
  /// distance and the aperture polygon, is settled before either.
  ///
  /// \throws smdl::Error if the prescription cannot be a camera lens, or
  ///                     a physical sensor is asked to integrate over a
  ///                     pinhole.
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

  /// The frame's physical size in scene units.
  float mFrameWidth{}, mFrameHeight{};

  /// What the film holds. See `CameraOptions::filmQuantity`.
  FilmQuantity mFilmQuantity{FilmQuantity::RADIANCE};

  /// With `mLens`, the response one unit of drawn pupil area carries:
  /// the pupil integral's own `1 / d^2`, over the constant the film
  /// quantity sets. See `CameraOptions`.
  float mExposurePerPupilArea{};

  /// The image plane distance in units of image height. Unused with a
  /// lens, whose film distance is a real one and lives in `mLens`.
  float mFocalLength{};

  /// With the thin lens on a physical sensor's film, the image distance
  /// in scene units, `f s / (s - f)` for a focal length `f` and a focus
  /// distance `s` (`f` itself at infinity), and the pupil area over its
  /// square: what one sample weighs on axis before the `cos^4` of its
  /// own segment. Zero on the observer's film.
  float mImageDistance{};
  float mPupilIrradiance{};

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

  /// The focus distance along the view axis in scene units, `INF` at
  /// infinity, where the thin lens sends the rays through every lens
  /// point out parallel.
  float mFocusDistance{};

  /// Is the focus at infinity? What `sample()` branches on, so that the
  /// focus plane's point is never formed from an infinite distance.
  bool mIsFocusedAtInfinity{};

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
