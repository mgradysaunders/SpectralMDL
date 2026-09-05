#include "Render/Camera.h"

#include <algorithm>

#include "smdl/RenderUtil/MonteCarlo.h"
#include "smdl/Support/Error.h"
#include "smdl/Support/Logger.h"

// Apply radial lens distortion to a sensor point, returning the ideal
// image point to build the ray from. The model maps sensor to ideal, so
// this is one polynomial evaluation with nothing to invert. `coneScale`
// receives the change in this pixel's angular footprint, which seeds
// the ray cone. Both are the identity when the coefficients are zero.
[[nodiscard]] static float2 distortSensorPoint(float2 image, float k1, float k2,
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
  mFocalLength = 0.5f / std::tan(smdl::radians(options.fovYDeg / 2));
  // One pixel's subtended angle, the ray cone spread that seeds the LOD
  // state; zero switches the cone off end to end.
  mConeAngleBase =
      options.noLOD ? 0.0f : std::atan(1.0f / (mFocalLength * mNumPixelsY));
  mCameraToWorld =
      smdl::lookAt(options.lookFrom, options.lookTo, options.lookUp);
  mLookFrom = options.lookFrom;
  mLookTo = options.lookTo;
  mLookUp = options.lookUp;
  if (options.motion) {
    mLookFromShut = options.lookFromShut;
    mLookToShut = options.lookToShut;
    mLookUpShut = options.lookUpShut;
    mMoving = !(smdl::isAllTrue(mLookFromShut == mLookFrom) &&
                smdl::isAllTrue(mLookToShut == mLookTo) &&
                smdl::isAllTrue(mLookUpShut == mLookUp));
  }
  // The distortion radius is corner-normalized so the coefficients sum to
  // the fractional corner displacement at any aspect ratio and FOV.
  mRCorner = std::hypot(0.5f * mAspectRatio, 0.5f);
  mDistortionK1 = options.distortionK1;
  mDistortionK2 = options.distortionK2;
  mHasDistortion = mDistortionK1 != 0 || mDistortionK2 != 0;
  mDistortionScale =
      options.distortionFit ? 1 / (1 + mDistortionK1 + mDistortionK2) : 1.0f;
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
  mFocusDistance = options.focus > 0
                       ? options.focus
                       : length(options.lookTo - options.lookFrom);
  mNumBlades = options.blades;
  mBladeAngle = smdl::radians(options.bladeAngleDeg);
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
  if (mMoving) {
    SMDL_LOG_INFO("Camera motion: over the shutter the position moves ",
                  length(mLookFromShut - mLookFrom),
                  " scene units and the target moves ",
                  length(mLookToShut - mLookTo));
  } else if (options.motion) {
    SMDL_LOG_INFO("Camera motion: the shut keys equal the open keys, "
                  "rendering still");
  }
}

CameraSample Camera::sample(size_t x, size_t y,
                            Sampler &sampler) const noexcept {
  // The pixel jitter is always dimensions 0-1 of the sequence.
  const auto xi{float2(sampler)};
  const float u{(float(x) + xi.x) / mNumPixelsX};
  const float v{(float(y) + xi.y) / mNumPixelsY};
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

void Camera::toWorldMoving(CameraSample &sample, float u) const noexcept {
  sample.ray.transform(smdl::lookAt((1 - u) * mLookFrom + u * mLookFromShut,
                                    (1 - u) * mLookTo + u * mLookToShut,
                                    (1 - u) * mLookUp + u * mLookUpShut));
  sample.ray.dir = normalize(sample.ray.dir);
  sample.ray.time = u;
}
