#include "Render/Camera.h"

#include <algorithm>
#include <cmath>
#include <vector>

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

// Where the radial distortion map `1 + k1 t^2 + k2 t^4` of the
// corner-normalized radius `t` stops increasing, which folds the image
// over itself, or a negative number for a map that stays monotone out to
// the corner. Scanned rather than solved, so combinations of the two
// coefficients are covered as well as either alone.
[[nodiscard]] float distortionFoldAt(float k1, float k2) noexcept {
  constexpr int NUM_STEPS = 512;
  const float k1Times3 = 3 * k1;
  const float k2Times5 = 5 * k2;
  for (int i = 0; i <= NUM_STEPS; i++) {
    const float t = float(i) / float(NUM_STEPS);
    const float t2 = t * t;
    if (!(1 + t2 * (k1Times3 + t2 * k2Times5) > 0)) return t;
  }
  return -1;
}

// The film radii out to the corner `approximateLens()` fits the
// projection at.
constexpr int NUM_FIT_RADII = 32;

// Solve the normal equations `m x = b` of a three-term least squares fit
// by Cramer's rule, or say they are singular.
[[nodiscard]] bool solveNormalEquations(const double (&m)[3][3],
                                        const double (&b)[3],
                                        double (&x)[3]) noexcept {
  const auto det{[](const double (&a)[3][3]) {
    return a[0][0] * (a[1][1] * a[2][2] - a[1][2] * a[2][1]) -
           a[0][1] * (a[1][0] * a[2][2] - a[1][2] * a[2][0]) +
           a[0][2] * (a[1][0] * a[2][1] - a[1][1] * a[2][0]);
  }};
  const double d{det(m)};
  if (!(std::abs(d) > 0)) return false;
  for (int k = 0; k < 3; k++) {
    double replaced[3][3]{};
    for (int i = 0; i < 3; i++)
      for (int j = 0; j < 3; j++) replaced[i][j] = j == k ? b[i] : m[i][j];
    x[k] = det(replaced) / d;
  }
  return true;
}
} // namespace

DepthOfField depthOfField(float focalLength, float fNumber, float focus,
                          float2 frameSize) noexcept {
  auto result{DepthOfField{}};
  result.circleOfConfusion = std::hypot(frameSize.x, frameSize.y) / 1500.0f;
  result.hyperfocal = INF;
  result.nearLimit = INF;
  result.farLimit = INF;
  if (!(fNumber > 0)) return result;
  const float f{focalLength};
  const float H{f * f / (fNumber * result.circleOfConfusion) + f};
  result.hyperfocal = H;
  if (std::isinf(focus)) {
    // The limit of the near formula as the focus recedes.
    result.nearLimit = H - f;
    return result;
  }
  const float s{focus};
  result.nearLimit = s * (H - f) / (H + s - 2 * f);
  result.farLimit = s < H ? s * (H - f) / (H - s) : INF;
  return result;
}

float darkShareOfFrame(float2 frameSize, float radius) noexcept {
  const double a{0.5 * double(frameSize.x)};
  const double b{0.5 * double(frameSize.y)};
  const double r{radius};
  if (!(r > 0)) return 1;
  if (r * r >= a * a + b * b) return 0;
  // One quadrant of the frame, `[0, a]` by `[0, b]`, is lit where it lies
  // under the circle: the integral of `min(b, sqrt(r^2 - x^2))` over `x`
  // in `[0, min(a, r)]`, flat at `b` out to where the arc drops below it.
  const auto underArc{[r](double x) {
    return 0.5 * (x * std::sqrt(std::max(0.0, r * r - x * x)) +
                  r * r * std::asin(std::min(1.0, x / r)));
  }};
  const double end{std::min(a, r)};
  const double knee{std::min(end, r > b ? std::sqrt(r * r - b * b) : 0.0)};
  const double lit{b * knee + underArc(end) - underArc(knee)};
  return float(1.0 - lit / (a * b));
}

LensApproximation approximateLens(const CameraOptions &options) {
  SMDL_SANITY_CHECK(options.lens.has_value());
  auto result{LensApproximation{}};
  const float focus{options.focus > 0
                        ? options.focus
                        : length(options.lookTo - options.lookFrom)};
  const Lens lens{*options.lens,
                  LensOptions{std::isinf(focus) ? 0.0f : focus, options.fStop,
                              options.blades,
                              smdl::radians(options.bladeAngleDeg)}};
  const float frameHeight{options.frameSize.y};
  const float halfDiagonal{
      0.5f * std::hypot(options.frameSize.x, options.frameSize.y)};
  // The projection in units of the corner radius: the chief ray's tangent
  // at `s` is `A s + B s^3 + C s^5`, whose coefficients are the corner
  // radius times `a`, `b`, and `c`. Fitting the tangent rather than the
  // tangent over the radius weighs each radius by how far a mismatch
  // moves the image on the film.
  auto radii{std::vector<double>()};
  auto tangents{std::vector<double>()};
  double normal[3][3]{};
  double moment[3]{};
  for (int i = 1; i <= NUM_FIT_RADII; i++) {
    const double s{double(i) / NUM_FIT_RADII};
    const float theta{lens.fieldAngleAt(float(s) * halfDiagonal)};
    if (!(theta >= 0 && theta < 0.5f * PI)) {
      result.numDroppedRadii++;
      continue;
    }
    const double basis[3]{s, s * s * s, s * s * s * s * s};
    const double tangent{std::tan(double(theta))};
    for (int k = 0; k < 3; k++) {
      for (int l = 0; l < 3; l++) normal[k][l] += basis[k] * basis[l];
      moment[k] += basis[k] * tangent;
    }
    radii.push_back(s);
    tangents.push_back(tangent);
  }
  result.numFittedRadii = radii.size();
  double coefficients[3]{};
  const bool hasFit{radii.size() >= 3 &&
                    solveNormalEquations(normal, moment, coefficients) &&
                    coefficients[0] > 0};
  auto k1{hasFit ? float(coefficients[1] / coefficients[0]) : 0.0f};
  auto k2{hasFit ? float(coefficients[2] / coefficients[0]) : 0.0f};
  if (!hasFit || distortionFoldAt(k1, k2) >= 0) {
    result.doesFold = true;
    coefficients[0] = double(halfDiagonal) / double(lens.focalLength());
    coefficients[1] = coefficients[2] = 0;
    k1 = k2 = 0;
  }
  const double A{coefficients[0]}, B{coefficients[1]}, C{coefficients[2]};
  // The thin lens's focal length in image heights, `1 / (a H)`.
  const double focalLength{double(halfDiagonal) / (A * double(frameHeight))};
  // Where the thin lens images each traced chief ray: the radius whose
  // tangent it is, by Newton's method on the monotone map.
  for (size_t i = 0; i < radii.size(); i++) {
    auto s{radii[i]};
    for (int iteration = 0; iteration < 16; iteration++) {
      const double s2{s * s};
      const double slope{A + s2 * (3 * B + s2 * 5 * C)};
      if (!(slope > 0)) break;
      s -= (s * (A + s2 * (B + s2 * C)) - tangents[i]) / slope;
    }
    result.maxChiefRayError =
        std::max(result.maxChiefRayError,
                 float(std::abs(s - radii[i]) * double(halfDiagonal)));
  }
  auto &thin{result.options};
  thin = options;
  thin.lens.reset();
  thin.fovYDeg = 2 * smdl::degrees(std::atan(0.5f / float(focalLength)));
  thin.fStop = 0;
  thin.aperture = lens.entrancePupilRadius();
  thin.distortionK1 = k1;
  thin.distortionK2 = k2;
  thin.shouldFitDistortion = false;
  thin.vignetting = 0;
  thin.catEye = 0;
  thin.catEyeRadius = 0;
  return result;
}

Camera::Camera(const CameraOptions &options) {
  if (options.blades != 0 && options.blades < 3)
    throw smdl::Error("expected 'blades' to be 0 (a round lens) or at "
                      "least 3");
  if (!(options.vignetting >= 0 && options.vignetting <= 1))
    throw smdl::Error("expected 'vignetting' to be between 0 (off) "
                      "and 1 (the physical cos^4 law)");
  if (!(options.catEye >= 0 && options.catEye <= 1))
    throw smdl::Error("expected 'cat_eye' to be between 0 (off) and 1 "
                      "(fully dark corners)");
  if (!(options.frameSize.x > 0 && options.frameSize.y > 0))
    throw smdl::Error("expected the frame to have a size");
  // The radial distortion map must stay monotone over the frame or the
  // image folds over itself; the radius is corner-normalized, so this is
  // aspect independent.
  if (const float t{
          distortionFoldAt(options.distortionK1, options.distortionK2)};
      t >= 0)
    throw smdl::Error(smdl::concat(
        "the distortion folds the image at ", t,
        " of the corner radius, where the radial map stops increasing. "
        "Reduce 'distortion_k1', which must exceed -1/3 on its own, or "
        "'distortion_k2'"));
  mNumPixelsX = float(options.resolution.x);
  mNumPixelsY = float(options.resolution.y);
  mAspectRatio = mNumPixelsX / mNumPixelsY;
  mFrameWidth = options.frameSize.x;
  mFrameHeight = options.frameSize.y;
  mFilmQuantity = options.filmQuantity;
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
  mIsFocusedAtInfinity = std::isinf(mFocusDistance);
  mNumBlades = options.blades;
  mBladeAngle = smdl::radians(options.bladeAngleDeg);
  if (options.lens) {
    buildLens(options);
  } else {
    buildThinLens(options);
  }
  if (const auto dof{depthOfField()}; dof.hasLimits()) {
    SMDL_LOG_INFO("Depth of field: f/", fNumber(), " focused at ",
                  mIsFocusedAtInfinity ? std::string("infinity")
                                       : smdl::concat(mFocusDistance),
                  " is sharp from ", dof.nearLimit, " to ",
                  dof.farLimit < INF ? smdl::concat(dof.farLimit)
                                     : std::string("infinity"),
                  " scene units, hyperfocal ", dof.hyperfocal,
                  ", at a circle of confusion of ",
                  1e3f * dof.circleOfConfusion, " mm");
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
// field the frame looks out at, the pixel footprint that seeds the LOD
// cone, and the two probes that say what actually reaches the film.
void Camera::buildLens(const CameraOptions &options) {
  // The lens focuses at infinity for a distance of 0, which is what
  // `INF` here means.
  mLens.emplace(*options.lens,
                LensOptions{mIsFocusedAtInfinity ? 0.0f : mFocusDistance,
                            options.fStop, mNumBlades, mBladeAngle});
  const auto frameMM{1e3f * float2(mFrameWidth, mFrameHeight)};
  // The pixel's angular footprint, the same quantity the thin lens takes
  // from its field of view, now in the millimeters of a real frame over
  // a real focal length.
  mConeAngleBase =
      options.noLOD
          ? 0.0f
          : std::atan(mFrameHeight / (mNumPixelsY * mLens->focalLength()));
  mLens->logSummary();
  const auto halfDiagonal{0.5f * std::hypot(mFrameWidth, mFrameHeight)};
  mExitPupil.emplace(*mLens, halfDiagonal);
  mExitPupil->logSummary();
  // What one unit of drawn pupil area is worth. Irradiance at a film
  // point is the pupil integral of `L cos^4(theta) dA / d^2`, with `d`
  // the axial distance from the film to the plane the point is drawn on.
  // A physical sensor's film holds exactly that. The observer's holds
  // the scene radiance instead, so the integral is normalized to the
  // lens's own f-number: an ideal lens then reads 1 on axis, and the
  // frame holds its brightness whatever lens takes it and however far it
  // is stopped down.
  const auto filmToPupil{mLens->filmZ() - mLens->rearZ()};
  if (mFilmQuantity == FilmQuantity::IRRADIANCE) {
    mExposurePerPupilArea = 1 / (filmToPupil * filmToPupil);
    SMDL_LOG_INFO("Lens exposure: f/", mLens->fNumber(),
                  ", the film holds the irradiance at the sensor");
  } else {
    mExposurePerPupilArea = 4 / (PI * filmToPupil * filmToPupil);
    mExposurePerPupilArea *= mLens->fNumber() * mLens->fNumber();
    SMDL_LOG_INFO("Lens exposure: normalized to f/", mLens->fNumber(),
                  ", so the film holds the scene radiance on axis whatever "
                  "lens takes it and however far it is stopped down");
  }
  // Traced rather than taken from the focal length, so what it reports
  // is the frame the picture actually has, distortion and all.
  const auto verticalDeg{
      2 * smdl::degrees(mLens->fieldAngleAt(0.5f * mFrameHeight))};
  const auto diagonalDeg{2 * smdl::degrees(mLens->fieldAngleAt(halfDiagonal))};
  SMDL_LOG_INFO("Lens frame: a ", frameMM.x, " by ", frameMM.y, " mm frame, ",
                verticalDeg, " degrees top to bottom",
                diagonalDeg > 0 ? smdl::concat(" and ", diagonalDeg,
                                               " degrees across the diagonal")
                                : std::string(" and dark in the corners"));
  // The two ends of the frame, as areas on the plane of the rear vertex
  // rather than shares of it: what the corner gets against what the
  // middle gets is the mechanical vignette, which the thin lens can only
  // approximate with its cat's eye.
  const auto onAxis{mLens->transmittedArea(0.0f)};
  if (!(onAxis > 0))
    throw smdl::Error("no ray from the middle of the frame reaches the "
                      "scene through this lens: check that the surfaces are "
                      "in front-to-film order and that the clear apertures "
                      "are diameters");
  const auto atCorner{mLens->transmittedArea(halfDiagonal)};
  if (!(atCorner > 0)) {
    SMDL_LOG_WARN(
        "Lens: nothing reaches the corner of the frame through "
        "this lens, so ",
        smdl::Brief(100 * darkShareOfFrame(float2(mFrameWidth, mFrameHeight),
                                           mLens->imageCircleRadius()),
                    3),
        "% of the frame is dark outside the circle it covers; a "
        "smaller sensor is what fits it");
  } else {
    SMDL_LOG_INFO("Lens vignetting: the frame corner sees ",
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
    // `mFocalLength` is in units of image height, so the lens is
    // `mFrameHeight * mFocalLength` long and `1 / fstop` of that across:
    // the 24 mm of a 35mm frame unless a body says otherwise.
    mLensRadius = 0.5f * mFrameHeight * mFocalLength / options.fStop;
  }
  mVignetteStrength = options.vignetting;
  if (mFilmQuantity == FilmQuantity::IRRADIANCE) {
    // The pupil integral of the thin lens itself: a disk of the lens's
    // radius at the image distance behind it, bellows factor included,
    // which reduces to the paraxial `pi / (4 N^2)` on axis at infinity
    // focus. The `cos^4` of each sample's own segment lands in
    // `sample()`.
    if (!(mLensRadius > 0))
      throw smdl::Error("a physical sensor integrates the irradiance over a "
                        "pupil, and a pinhole has none: state 'fstop' or "
                        "'aperture'");
    const float focalLength{mFocalLength * mFrameHeight};
    if (!(mFocusDistance > focalLength))
      throw smdl::Error(smdl::concat(
          "the thin lens cannot focus at ", mFocusDistance,
          " scene units, inside its focal length of ", focalLength));
    mImageDistance = mIsFocusedAtInfinity ? focalLength
                                          : focalLength * mFocusDistance /
                                                (mFocusDistance - focalLength);
    mPupilIrradiance =
        PI * mLensRadius * mLensRadius / (mImageDistance * mImageDistance);
    if (mVignetteStrength > 0) {
      SMDL_LOG_INFO("Natural vignetting: the irradiance at the sensor "
                    "carries the cos^4 law exactly, so 'vignetting' is not "
                    "applied on top of it");
      mVignetteStrength = 0;
    }
    SMDL_LOG_INFO("Thin lens exposure: f/", fNumber(), ", the film holds ",
                  mPupilIrradiance,
                  " of the scene radiance on axis, the pupil integral at an "
                  "image distance of ",
                  1e3f * mImageDistance, " mm");
  }
  // The barrel rim radius and the rim displacement per unit of image
  // radius, both zero when mechanical vignetting is off. Parameterizing
  // the barrel half-length by its corner displacement is exact, since the
  // displacement is proportional to the image radius.
  mRimRadius = options.catEyeRadius > 0 ? options.catEyeRadius : mLensRadius;
  mRimSlope = options.catEye * mRimRadius / mRCorner;
  if (mLensRadius > 0) {
    SMDL_LOG_INFO("Thin lens: radius ", mLensRadius, " scene units",
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

float Camera::fNumber() const noexcept {
  if (mLens) return mLens->fNumber();
  return mLensRadius > 0 ? mFocalLength * mFrameHeight / (2 * mLensRadius)
                         : 0.0f;
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
    // and the ray runs to it from a point on the lens. At infinity
    // there is no such point, and the rays through every lens point
    // leave parallel to the pinhole's.
    float3 pointOnFocusPlane{result.ray.dir * (mFocusDistance / mFocalLength)};
    lens = mLensRadius * smdl::uniformApertureSample(mNumBlades, mBladeAngle,
                                                     float2(sampler));
    result.ray.org = float3(lens.x, lens.y, 0.0f);
    if (!mIsFocusedAtInfinity)
      result.ray.dir = pointOnFocusPlane - result.ray.org;
  }
  // The estimator averages radiance with no geometric weight of its
  // own, so everything between the scene and the film lands on the
  // weight here.
  if (mVignetteStrength > 0) {
    // Natural vignetting: cos^4 of the film-to-lens segment, in image
    // heights, which the lens point is scaled into. The film point is
    // the image point inverted, hence the sum. The strength enters as
    // an exponent so it scales the falloff in stops.
    const float2 lensOverHeight{(1.0f / mFrameHeight) * lens};
    float cosSquared{
        mFocalLength * mFocalLength /
        (lengthSquared(lensOverHeight + image) + mFocalLength * mFocalLength)};
    result.weight *= std::pow(cosSquared, 2 * mVignetteStrength);
  }
  if (mFilmQuantity == FilmQuantity::IRRADIANCE) {
    // The pupil integral: the disk over the image distance squared, and
    // the `cos^4` of this sample's own film-to-lens segment, in meters
    // throughout. The film point is the image point inverted and scaled
    // to the frame, hence the sum.
    const float2 filmOffset{mFrameHeight * image};
    const float cosSquared{
        mImageDistance * mImageDistance /
        (lengthSquared(lens + filmOffset) + mImageDistance * mImageDistance)};
    result.weight *= mPupilIrradiance * cosSquared * cosSquared;
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
  const float3 film{-(u - 0.5f) * mFrameWidth, (v - 0.5f) * mFrameHeight,
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
