#include "Render/Camera.h"

#include <algorithm>

#include "smdl/RenderUtil/MonteCarlo.h"
#include "smdl/Support/Error.h"
#include "smdl/Support/Logger.h"

namespace {
// Apply radial lens distortion to a sensor point, returning the ideal
// image point to build the ray from. The model maps sensor to ideal, so
// this is one polynomial evaluation with nothing to invert. `coneScale`
// receives the change in this pixel's angular footprint, which seeds
// the ray cone. Both are the identity when the coefficients are zero.
[[nodiscard]] float2 distortSensorPoint(float2 image, float k1, float k2,
                                        float fitScale, float rCorner,
                                        float focalLength,
                                        float &coneScale) noexcept {
  const float s2{lengthSquared(image) / (rCorner * rCorner)};
  const float scale{fitScale * (1 + s2 * (k1 + s2 * k2))};
  const float radial{fitScale * (1 + s2 * (3 * k1 + s2 * 5 * k2))};
  const float2 imageIdeal{scale * image};
  // The cone is isotropic, so take the geometric mean of the tangential
  // (`scale`) and radial (`radial`) stretches, then correct for the
  // plane-to-direction foreshortening differing between the original and
  // displaced radii; the correction is exactly 1 with no distortion.
  const float foreshorten{std::hypot(focalLength, length(image)) /
                          std::hypot(focalLength, length(imageIdeal))};
  coneScale =
      std::sqrt(std::max(scale * radial, 0.0f)) * std::pow(foreshorten, 1.5f);
  return imageIdeal;
}

// The fraction of the rear aperture that a film point can see out
// through the lens, measured rather than guessed: a fixed grid over the
// disk, traced. On axis it is the throughput the whole exposure is
// relative to; at the sensor corner it is the mechanical vignette, which
// the thin lens can only approximate with its cat's eye. Zero at the
// corner means the sensor reaches past what the lens covers.
[[nodiscard]] float probeTransmission(const Lens &lens, float2 film) noexcept {
  constexpr int NUM_STEPS = 32;
  const auto radius{lens.rearApertureRadius()};
  auto numPassed{0};
  for (int i = 0; i < NUM_STEPS; i++) {
    for (int j = 0; j < NUM_STEPS; j++) {
      const auto disk{radius *
                      smdl::uniformDiskSample(float2((i + 0.5f) / NUM_STEPS,
                                                     (j + 0.5f) / NUM_STEPS))};
      const float3 org{film.x, film.y, lens.filmZ()};
      auto ray{Ray{org, float3(disk.x, disk.y, lens.rearZ()) - org, EPS, INF}};
      if (lens.traceFromFilm(ray)) numPassed++;
    }
  }
  return float(numPassed) / float(NUM_STEPS * NUM_STEPS);
}
} // namespace

Camera::Camera(const CameraOptions &options) {
  if (options.blades != 0 && options.blades < 3)
    throw smdl::Error("expected -blades to be 0 (a round lens) or at "
                      "least 3");
  if (!(options.vignetting >= 0 && options.vignetting <= 1))
    throw smdl::Error("expected -vignetting to be between 0 (off) "
                      "and 1 (the physical cos^4 law)");
  if (!(options.catEye >= 0 && options.catEye <= 1))
    throw smdl::Error("expected -cat-eye to be between 0 (off) and 1 "
                      "(fully dark corners)");
  // The radial distortion map must stay monotone over the frame or the
  // image folds over itself. Scan rather than solve, so combinations of
  // the two coefficients are covered as well as either alone; the radius
  // is corner-normalized, so this is aspect independent.
  if (options.distortionK1 != 0 || options.distortionK2 != 0) {
    constexpr int NUM_STEPS = 512;
    const float k1Times3 = 3 * options.distortionK1;
    const float k2Times5 = 5 * options.distortionK2;
    for (int i = 0; i <= NUM_STEPS; i++) {
      const float t = float(i) / float(NUM_STEPS);
      const float t2 = t * t;
      if (!(1 + t2 * (k1Times3 + t2 * k2Times5) > 0))
        throw smdl::Error(smdl::concat(
            "the distortion folds the image at ", t,
            " of the corner radius, where the radial map stops increasing. "
            "Reduce -distortion-k1, which must exceed -1/3 on its own, or "
            "-distortion-k2"));
    }
  }
  mNumPixelsX = float(options.resolution.x);
  mNumPixelsY = float(options.resolution.y);
  mAspectRatio = mNumPixelsX / mNumPixelsY;
  mCameraToWorld =
      smdl::lookAt(options.lookFrom, options.lookTo, options.lookUp);
  mLookFrom = options.lookFrom;
  mLookTo = options.lookTo;
  mLookUp = options.lookUp;
  if (options.hasMotion) {
    mLookFromShut = options.lookFromShut;
    mLookToShut = options.lookToShut;
    mLookUpShut = options.lookUpShut;
    mIsMoving = !(smdl::isAllTrue(mLookFromShut == mLookFrom) &&
                  smdl::isAllTrue(mLookToShut == mLookTo) &&
                  smdl::isAllTrue(mLookUpShut == mLookUp));
  }
  mFocusDistance = options.focus > 0
                       ? options.focus
                       : length(options.lookTo - options.lookFrom);
  mNumBlades = options.blades;
  mBladeAngle = smdl::radians(options.bladeAngleDeg);
  if (options.lens) {
    buildLens(options);
  } else {
    buildThinLens(options);
  }
  if (mIsMoving) {
    SMDL_LOG_INFO("Camera motion: over the shutter the position moves ",
                  length(mLookFromShut - mLookFrom),
                  " scene units and the target moves ",
                  length(mLookToShut - mLookTo));
  } else if (options.hasMotion) {
    SMDL_LOG_INFO("Camera motion: the shut keys equal the open keys, "
                  "rendering still");
  }
}

// The lens the file names, and everything that follows from it: the
// sensor it covers, the pixel footprint that seeds the LOD cone, and the
// two probes that say what actually reaches the film.
void Camera::buildLens(const CameraOptions &options) {
  const auto sensorMM{options.sensorMM.x > 0 && options.sensorMM.y > 0
                          ? options.sensorMM
                          : float2(36.0f, 24.0f)};
  mSensorWidth = 1e-3f * sensorMM.x;
  mSensorHeight = 1e-3f * sensorMM.y;
  mLens.emplace(*options.lens, LensOptions{mFocusDistance, options.fStop,
                                           mNumBlades, mBladeAngle});
  // The pixel's angular footprint, the same quantity the thin lens takes
  // from its field of view, now in the millimeters of a real sensor over
  // a real focal length.
  mConeAngleBase =
      options.noLOD
          ? 0.0f
          : std::atan(mSensorHeight / (mNumPixelsY * mLens->focalLength()));
  mLens->logSummary();
  const auto halfDiagonal{0.5f * std::hypot(mSensorWidth, mSensorHeight)};
  mExitPupil.emplace(*mLens, halfDiagonal);
  mExitPupil->logSummary();
  // What one unit of drawn pupil area is worth. Irradiance at a film
  // point is the pupil integral of `L cos^4(theta) dA / d^2`, with `d`
  // the axial distance from the film to the plane the point is drawn on,
  // and the constant in front sets where the scale sits: this one makes
  // an ideal f/1 lens read 1 on axis, so that two lenses, and one lens at
  // two apertures, are on the same exposure.
  const auto filmToPupil{mLens->filmZ() - mLens->rearZ()};
  mExposurePerPupilArea = 4 / (PI * filmToPupil * filmToPupil);
  if (options.shouldNormalizeLensExposure) {
    mExposurePerPupilArea *= mLens->fNumber() * mLens->fNumber();
    SMDL_LOG_INFO("Lens exposure: normalized to f/", mLens->fNumber(),
                  ", so the frame holds its brightness whatever lens takes "
                  "it and however far it is stopped down");
  } else {
    SMDL_LOG_INFO("Lens exposure: f/", mLens->fNumber(), " gathers ",
                  1 / (mLens->fNumber() * mLens->fNumber()),
                  " of what an ideal f/1 lens would, before what the glass "
                  "takes");
  }
  SMDL_LOG_INFO(
      "Lens frame: a ", sensorMM.x, " by ", sensorMM.y, " mm sensor, ",
      smdl::degrees(2 * std::atan(halfDiagonal / mLens->focalLength())),
      " degrees across the diagonal");
  if (const auto sensorAspect{mSensorWidth / mSensorHeight};
      std::abs(sensorAspect - mAspectRatio) > 0.01f * mAspectRatio)
    SMDL_LOG_WARN("Lens: the sensor is ", sensorAspect,
                  " wide for its height and -resolution is ", mAspectRatio,
                  ", so the picture is stretched out of the shape of the "
                  "sensor it names");
  const auto onAxis{probeTransmission(*mLens, float2(0.0f))};
  if (!(onAxis > 0))
    throw smdl::Error("no ray from the middle of the sensor reaches the "
                      "scene through this lens: check that the surfaces are "
                      "in front-to-film order and that the clear apertures "
                      "are diameters");
  const auto atCorner{
      probeTransmission(*mLens, 0.5f * float2(mSensorWidth, mSensorHeight))};
  if (!(atCorner > 0)) {
    SMDL_LOG_WARN("Lens: nothing reaches the corner of the sensor through "
                  "this lens, so the frame is dark outside the circle it "
                  "covers; a smaller 'sensor' is what fits it");
  } else {
    SMDL_LOG_INFO("Lens vignetting: the sensor corner sees ",
                  100 * atCorner / onAxis,
                  "% of what its middle sees, measured through the glass");
  }
}

// The thin lens: a pinhole at the focal length the field of view sets,
// with the radial distortion, the depth of field, and the two vignettes
// that stand in for what a real lens does on its own.
void Camera::buildThinLens(const CameraOptions &options) {
  mFocalLength = 0.5f / std::tan(smdl::radians(options.fovYDeg / 2));
  // One pixel's subtended angle, the ray cone spread that seeds the LOD
  // state; zero switches the cone off end to end.
  mConeAngleBase =
      options.noLOD ? 0.0f : std::atan(1.0f / (mFocalLength * mNumPixelsY));
  // The distortion radius is corner-normalized so the coefficients sum to
  // the fractional corner displacement at any aspect ratio and FOV.
  mRCorner = std::hypot(0.5f * mAspectRatio, 0.5f);
  mDistortionK1 = options.distortionK1;
  mDistortionK2 = options.distortionK2;
  mHasDistortion = mDistortionK1 != 0 || mDistortionK2 != 0;
  mDistortionScale = options.shouldFitDistortion
                         ? 1 / (1 + mDistortionK1 + mDistortionK2)
                         : 1.0f;
  if (mHasDistortion) {
    SMDL_LOG_INFO(
        "Lens distortion: corner displacement ",
        100 * (mDistortionScale * (1 + mDistortionK1 + mDistortionK2) - 1),
        "%, center scale ", mDistortionScale);
  }
  // `lookAt()` is orthonormal, so the lens disk needs no unit conversion.
  // The LOD ray cone keeps the per-pixel spread: defocus blur comes out of
  // averaging the lens samples, so widening the cone for it would blur the
  // textures a second time.
  if (options.aperture > 0) {
    mLensRadius = options.aperture;
  } else if (options.fStop > 0) {
    // A 35mm frame is 24mm high and `mFocalLength` is in units of image
    // height, so the equivalent lens is 24mm*focalLength long and 1/fstop
    // of that across.
    mLensRadius = 0.5f * 0.024f * mFocalLength / options.fStop;
  }
  mVignetteStrength = options.vignetting;
  // The barrel rim radius and the rim displacement per unit of image
  // radius, both zero when mechanical vignetting is off. Parameterizing
  // the barrel half-length by its corner displacement is exact, since the
  // displacement is proportional to the image radius.
  mRimRadius = options.catEyeRadius > 0 ? options.catEyeRadius : mLensRadius;
  mRimSlope = options.catEye * mRimRadius / mRCorner;
  if (mLensRadius > 0) {
    SMDL_LOG_INFO("Depth of field: lens radius ", mLensRadius,
                  " scene units, focus at ", mFocusDistance,
                  mNumBlades >= 3 ? smdl::concat(", ", mNumBlades, " blades")
                                  : std::string());
  }
  if (mVignetteStrength > 0) {
    SMDL_LOG_INFO(
        "Natural vignetting: strength ", mVignetteStrength,
        ", corner transmission ",
        std::pow(mFocalLength * mFocalLength /
                     (mRCorner * mRCorner + mFocalLength * mFocalLength),
                 2 * mVignetteStrength));
  }
  if (mRimSlope > 0 && mLensRadius > 0) {
    SMDL_LOG_INFO("Mechanical vignetting: rim radius ", mRimRadius,
                  " scene units against a lens radius of ", mLensRadius,
                  ", displaced ", options.catEye * mRimRadius,
                  " at the frame corner");
  }
}

CameraSample Camera::sample(size_t x, size_t y,
                            Sampler &sampler) const noexcept {
  // The pixel jitter is always dimensions 0-1 of the sequence.
  const auto xi{float2(sampler)};
  const float u{(float(x) + xi.x) / mNumPixelsX};
  const float v{(float(y) + xi.y) / mNumPixelsY};
  if (mLens) return sampleThroughLens(u, v, sampler);
  // The image-plane point: the film point inverted through the lens.
  const float2 image{+(u - 0.5f) * mAspectRatio, -(v - 0.5f)};
  // Distortion remaps only the direction this sensor point looks in;
  // `image` stays the sensor coordinate, which is what the vignetting
  // below needs, since vignetting is film and pupil geometry rather
  // than a property of the outgoing ray.
  float2 imageIdeal{image};
  float distortConeScale{1.0f};
  if (mHasDistortion)
    imageIdeal = distortSensorPoint(image, mDistortionK1, mDistortionK2,
                                    mDistortionScale, mRCorner, mFocalLength,
                                    distortConeScale);
  float2 lens{};
  auto result{CameraSample{}};
  result.ray = Ray{float3(0.0f),
                   float3(imageIdeal.x, imageIdeal.y, -mFocalLength), EPS, INF};
  if (mLensRadius > 0) {
    // Thin lens: the pinhole direction locates the point of the focus
    // plane (camera-space z = -focusDistance) that this pixel images,
    // and the ray runs to it from a point on the lens.
    float3 pointOnFocusPlane{result.ray.dir * (mFocusDistance / mFocalLength)};
    lens = mLensRadius * smdl::uniformApertureSample(mNumBlades, mBladeAngle,
                                                     float2(sampler));
    result.ray.org = float3(lens.x, lens.y, 0.0f);
    result.ray.dir = pointOnFocusPlane - result.ray.org;
  }
  // The estimator averages radiance with no geometric weight of its
  // own, so everything between the scene and the film lands on the
  // weight here.
  if (mVignetteStrength > 0) {
    // Natural vignetting: cos^4 of the film-to-lens segment. The film
    // point is the image point inverted, hence the sum. The strength
    // enters as an exponent so it scales the falloff in stops.
    float cosSquared{
        mFocalLength * mFocalLength /
        (lengthSquared(lens + image) + mFocalLength * mFocalLength)};
    result.weight *= std::pow(cosSquared, 2 * mVignetteStrength);
  }
  if (mRimSlope > 0 && mLensRadius > 0) {
    // Mechanical vignetting: the barrel rims, projected onto the
    // lens plane and displaced either way along the image point's
    // radial direction. The lens point must clear both, so the
    // effective aperture is the vesica where they overlap: the cat's
    // eye seen in corner bokeh. Rejection darkens the corner by the
    // same factor that thins the samples, so relative noise barely
    // moves.
    float2 offset{mRimSlope * image};
    if (lengthSquared(lens - offset) > mRimRadius * mRimRadius ||
        lengthSquared(lens + offset) > mRimRadius * mRimRadius)
      result.weight = 0;
  }
  result.coneAngle = mConeAngleBase * distortConeScale;
  return result;
}

CameraSample Camera::sampleThroughLens(float u, float v,
                                       Sampler &sampler) const noexcept {
  auto result{CameraSample{}};
  result.coneAngle = mConeAngleBase;
  // The film point: the sensor coordinate, with the image inverted on it
  // the way a lens leaves it, which is why both signs run against the
  // pixel coordinate.
  const float3 film{-(u - 0.5f) * mSensorWidth, (v - 0.5f) * mSensorHeight,
                    mLens->filmZ()};
  // The pupil point, drawn inside the part of the rear aperture this
  // film point can see out through. These are the two dimensions the
  // thin lens point takes, in the same place, so no existing sequence
  // moves; a blocked ray is weight 0 rather than a redraw, so the count
  // is fixed as well.
  auto pupilArea{0.0f};
  const auto pupil{
      mExitPupil->sample(float2(film.x, film.y), float2(sampler), pupilArea)};
  result.ray =
      Ray{film, float3(pupil.x, pupil.y, mLens->rearZ()) - film, EPS, INF};
  if (pupilArea <= 0) {
    result.weight = 0;
    return result;
  }
  // The pupil integral: the area drawn from, over the density it was
  // drawn with, times the `cos^4` falloff. Unlike the thin lens's, that
  // falloff is not an effect to switch on; it is the estimator, and the
  // whole of the natural vignetting comes out of it.
  const auto cosTheta{-result.ray.dir.z / length(result.ray.dir)};
  const auto cosSquared{cosTheta * cosTheta};
  result.weight = mExposurePerPupilArea * pupilArea * cosSquared * cosSquared;
  if (!mLens->traceFromFilm(result.ray)) result.weight = 0;
  return result;
}

void Camera::toWorldMoving(CameraSample &sample, float u) const noexcept {
  sample.ray.transform(smdl::lookAt((1 - u) * mLookFrom + u * mLookFromShut,
                                    (1 - u) * mLookTo + u * mLookToShut,
                                    (1 - u) * mLookUp + u * mLookUpShut));
  sample.ray.dir = normalize(sample.ray.dir);
  sample.ray.time = u;
}
