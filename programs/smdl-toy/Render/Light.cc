#include "Render/Light.h"

#include "smdl/Support/Logger.h"

#include <map>
#include <set>

EnvLight::EnvLight(const std::string &fileName, float scaleFactor)
    : scaleFactor(scaleFactor) {
  // Never mipped: the environment is sampled by direction through the
  // tabulated density below, never with a texture-space footprint. So
  // nothing here requests a chain, and none is allocated.
  if (auto error{image.startLoad(fileName)}) error->printAndExit();
  image.finishLoad();
  auto weights{std::vector<float>{}};
  const int numTexelsX{image.getNumTexelsX()};
  const int numTexelsY{image.getNumTexelsY()};
  weights.reserve(numTexelsX * numTexelsY);
  double lumSum{};
  double sinThetaSum{};
  for (int iY = 0; iY < numTexelsY; iY++) {
    auto theta{PI * (iY + 0.5f) / float(numTexelsY)};
    auto sinTheta{std::sin(theta)};
    for (int iX = 0; iX < numTexelsX; iX++) {
      auto value{image.fetch(iX, iY)};
      auto lum{(value.x + value.y + value.z) / 3.0f};
      weights.push_back(sinTheta * lum);
      lumSum += double(sinTheta) * lum;
      sinThetaSum += sinTheta;
    }
  }
  meanRadiance = sinThetaSum > 0 ? float(lumSum / sinThetaSum) : 0.0f;
  // MIS compensation (Karlík et al., SIGGRAPH Asia 2019): subtract the
  // mean radiance from the tabulated density and clamp at zero, so light
  // sampling concentrates on the above-average part of the image. Fall
  // back to the uncompensated weights if compensation removes everything
  // (a constant environment).
  {
    auto compensated{weights};
    double compensatedSum{};
    size_t texel{};
    for (int iY = 0; iY < numTexelsY; iY++) {
      auto theta{PI * (iY + 0.5f) / float(numTexelsY)};
      auto sinTheta{std::sin(theta)};
      for (int iX = 0; iX < numTexelsX; iX++, texel++) {
        auto lum{sinTheta > 0 ? weights[texel] / sinTheta : 0.0f};
        compensated[texel] = sinTheta * std::max(lum - meanRadiance, 0.0f);
        compensatedSum += compensated[texel];
      }
    }
    if (compensatedSum > 0) weights = std::move(compensated);
  }
  imageDistr = smdl::Distribution2D(numTexelsX, numTexelsY, weights);
}

EnvLight::EnvLight(const smdl::SunSkyOptions &options)
    : sunSky(smdl::SunSky(options)) {
  // The `SunSky` applies its own scale factor internally, so the mean
  // radiance it reports is final.
  meanRadiance = sunSky->averageRadiance();
}

Color EnvLight::Li(smdl::Compiler &compiler, const smdl::State &state,
                   float3 wi, float &pdf) const {
  Color Li{};
  if (sunSky) {
    // Spectral end to end: the model evaluates directly at the render
    // wavelengths. The pdf is the sun/sky mixture density, so hitting
    // the sun disk by BSDF sampling MIS-weights correctly against the
    // cone-sampling branch of `sample()`.
    sunSky->radiance(wi, Li.size(), state.wavelength_base, Li.data());
    pdf = sunSky->pdf(wi);
    return Li;
  }
  int2 iPixel{-1, -1};
  pdf = imageDistr.directionPDF(wi, &iPixel);
  // The radiance must be fetched independently of the pdf: with MIS
  // compensation the sampling density is zero wherever the radiance is at
  // or below the mean, but the radiance itself is not.
  if (iPixel.x >= 0 && iPixel.y >= 0)
    compiler.convertRGBToColor(state, image.fetch(iPixel.x, iPixel.y),
                               Li.data());
  return Li * scaleFactor;
}

float3 EnvLight::Li_sample(smdl::Compiler &compiler, const smdl::State &state,
                           float2 xi, float &pdf, Color &Li) const {
  if (sunSky) {
    float3 wi{sunSky->sample(xi, &pdf)};
    if (pdf > 0.0f) {
      sunSky->radiance(wi, Li.size(), state.wavelength_base, Li.data());
    } else {
      Li = Color(0.0f);
    }
    return wi;
  }
  int2 iPixel{};
  float3 wi{imageDistr.directionSample(xi, &iPixel, &pdf)};
  if (pdf > 0.0f) {
    compiler.convertRGBToColor(state, image.fetch(iPixel.x, iPixel.y),
                               Li.data());
    Li *= scaleFactor;
  } else {
    Li = Color(0.0f);
  }
  return wi;
}

// The quadrature width of each wavelength band in nanometers, for
// normalizing a spectral shape to unit integral over the render band:
// the render-wide trapezoid weights when the grid is non-uniform, the
// uniform spacing otherwise, and 1 for a single-band render, where
// "per nanometer" degenerates to a plain per-band value.
[[nodiscard]] static std::vector<float> bandWidths(const Color &wavelengths) {
  if (const auto &weights{renderWavelengthWeights()}; !weights.empty())
    return weights;
  auto widths{std::vector<float>(wavelengths.size(), 1.0f)};
  if (wavelengths.size() > 1) {
    const float spacing{(wavelengths[wavelengths.size() - 1] - wavelengths[0]) /
                        float(wavelengths.size() - 1)};
    for (auto &width : widths) width = spacing;
  }
  return widths;
}

AnalyticLight::AnalyticLight(smdl::Compiler &compiler, const smdl::State &state,
                             const Color &wavelengths, const LayoutLight &light,
                             std::shared_ptr<const smdl::LightProfile> profile)
    : mKind(light.decl.kind), mIntensity(wavelengths.size()),
      mProfile(std::move(profile)) {
  const auto &decl{light.decl};
  mLightToWorld = light.lightToWorld;
  if (light.lightToWorldShut) {
    mMoving = true;
    mLightToWorldShut = *light.lightToWorldShut;
  }
  if (!isDirac()) {
    const bool isRect{mKind == LayoutLightDecl::Kind::RECT};
    mHalfExtent = isRect ? float2(0.5f * decl.size.x, 0.5f * decl.size.y)
                         : float2(decl.radius, decl.radius);
    mObjectArea =
        isRect ? decl.size.x * decl.size.y : PI * decl.radius * decl.radius;
  }
  mPlacement = derivePlacement(mLightToWorld);
  // The spectral shape: blackbody or flat, normalized to unit integral
  // over the render band, then the RGB tint applied WITHOUT
  // renormalizing, so tinting dims the way dimming a lamp does.
  auto shape{Color(1.0f)};
  if (decl.temperature > 0) {
    for (size_t i = 0; i < wavelengths.size(); i++) {
      const float lambda{wavelengths[i] * 1.0e-3f}; // micrometers
      constexpr float c2 = 1.4388e4f;               // micrometer kelvins
      shape[i] = 1.0f / (lambda * lambda * lambda * lambda * lambda *
                         std::expm1(c2 / (lambda * decl.temperature)));
    }
  }
  const auto widths{bandWidths(wavelengths)};
  double integral{};
  for (size_t i = 0; i < wavelengths.size(); i++)
    integral += double(shape[i]) * widths[i];
  if (integral > 0) shape *= float(1.0 / integral);
  if (decl.color.x != 1.0f || decl.color.y != 1.0f || decl.color.z != 1.0f) {
    auto tint{Color()};
    compiler.convertRGBToColor(state, decl.color, tint.data());
    shape *= tint;
  }
  // The per-band mean of the unit-power spectrum after the tint, which
  // turns a broadband power into the selection weight; see `weight()`.
  double meanShape{};
  for (size_t i = 0; i < wavelengths.size(); i++) meanShape += shape[i];
  if (wavelengths.size() > 0) meanShape /= double(wavelengths.size());
  // The per-kind directional intensity scale, and the broadband power
  // the selection weight starts from.
  float intensityScale{};
  float power{decl.power};
  switch (mKind) {
  case LayoutLightDecl::Kind::POINT:
    intensityScale = decl.power / (4.0f * PI);
    break;
  case LayoutLightDecl::Kind::SPOT: {
    mCosOuter = std::cos(smdl::radians(decl.spotAngle) * 0.5f);
    mCosInner = std::cos(smdl::radians(decl.spotAngle) * 0.5f *
                         (1.0f - decl.spotBlend));
    // The peak from the power: the falloff is a smoothstep in the
    // cosine, whose mean over the blend band is exactly 1/2.
    const float solidAngle{2.0f * PI *
                           ((1.0f - mCosInner) + (mCosInner - mCosOuter) / 2)};
    intensityScale = decl.power / std::max(solidAngle, 1.0e-6f);
    break;
  }
  case LayoutLightDecl::Kind::PROFILE: {
    // The profile's intensities are already broadband W/sr, so the
    // scale is per unit, renormalized to `power` watts when given.
    intensityScale = decl.scale;
    if (decl.powerSet) {
      const float profilePower{mProfile->power()};
      if (profilePower > 0) intensityScale = decl.power / profilePower;
    }
    power = mProfile->power() * intensityScale;
    break;
  }
  case LayoutLightDecl::Kind::RECT:
  case LayoutLightDecl::Kind::DISK: {
    if (!(mPlacement.worldArea > 0)) {
      SMDL_LOG_WARN("The ", decl.kindName(), " light ", smdl::Quoted(decl.name),
                    " is placed with no area and is never sampled.");
      power = 0.0f;
      break;
    }
    // One-sided Lambertian: the radiance is the power over pi times the
    // area in square meters, at the open key.
    const float metersPerSceneUnit{state.meters_per_scene_unit};
    intensityScale = decl.power / (PI * mPlacement.worldArea *
                                   metersPerSceneUnit * metersPerSceneUnit);
    break;
  }
  }
  mWeight = power * float(meanShape);
  for (size_t i = 0; i < wavelengths.size(); i++)
    mIntensity[i] = intensityScale * shape[i];
}

AnalyticLight::Placement
AnalyticLight::derivePlacement(const float4x4 &xf) const noexcept {
  auto result{Placement()};
  result.position = float3(xf[3]);
  auto column{[&](int i) {
    auto v{float3(xf[i])};
    return smdl::tryNormalize(v) ? v : float3(i == 0, i == 1, i == 2);
  }};
  result.localX = column(0);
  result.localY = column(1);
  result.localZ = column(2);
  if (!isDirac()) {
    // The plane the placement puts the shape in: the in-plane axes keep
    // the placement's scale, the area stretch is the length of their
    // cross product, constant over a plane, and the emitting side is
    // the one the placement maps local -Z into.
    result.axisU = float3(xf[0]);
    result.axisV = float3(xf[1]);
    auto planeNormal{cross(result.axisU, result.axisV)};
    const float stretch{length(planeNormal)};
    result.worldArea = mObjectArea * stretch;
    if (result.worldArea > 0) {
      planeNormal /= stretch;
      result.normal =
          dot(planeNormal, float3(xf[2])) > 0 ? -planeNormal : planeNormal;
    }
  }
  return result;
}

const AnalyticLight::Placement &
AnalyticLight::placementAt(float time,
                           std::optional<Placement> &scratch) const noexcept {
  if (!mMoving) return mPlacement;
  // The lerp of the two keys, spelled so that the ends reproduce them
  // exactly; the placement then follows from the lerped matrix as it
  // does from a static one.
  auto xf{float4x4()};
  for (int j = 0; j < 4; j++)
    xf[j] = (1.0f - time) * mLightToWorld[j] + time * mLightToWorldShut[j];
  return scratch.emplace(derivePlacement(xf));
}

Color AnalyticLight::Li(const float3 &point, float metersPerSceneUnit,
                        float time) const noexcept {
  return Li(point, point, metersPerSceneUnit, time);
}

Color AnalyticLight::Li(const float3 &point, const float3 &incidencePoint,
                        float metersPerSceneUnit, float time) const noexcept {
  std::optional<Placement> scratch{};
  const auto &placed{placementAt(time, scratch)};
  const float distSq{lengthSquared(point - placed.position)};
  if (!(distSq > 0)) return Color(0.0f);
  auto direction{incidencePoint - placed.position};
  if (!(lengthSquared(direction) > 0)) return Color(0.0f);
  direction = normalize(direction);
  float factor{1.0f};
  if (mKind == LayoutLightDecl::Kind::SPOT) {
    // Emission aims along the local -Z axis, full intensity inside the
    // inner cone, smoothstepped in the cosine down to zero at the outer.
    const float cosTheta{dot(direction, -placed.localZ)};
    if (!(cosTheta > mCosOuter)) return Color(0.0f);
    if (cosTheta < mCosInner) {
      const float t{(cosTheta - mCosOuter) / (mCosInner - mCosOuter)};
      factor = t * t * (3.0f - 2.0f * t);
    }
  } else if (mKind == LayoutLightDecl::Kind::PROFILE) {
    // The profile's polar axis is the local Z axis with its photometric
    // zero (the nadir of a typical luminaire) along local -Z, matching
    // the spot convention, so the Z component flips going into
    // `LightProfile::interpolate`, whose zero vertical angle is at +Z.
    factor = mProfile->interpolate(float3(dot(direction, placed.localX),
                                          dot(direction, placed.localY),
                                          -dot(direction, placed.localZ)));
    if (!(factor > 0)) return Color(0.0f);
  }
  const float distSqMeters{distSq * metersPerSceneUnit * metersPerSceneUnit};
  auto Li{Color(mIntensity)};
  Li *= factor / distSqMeters;
  return Li;
}

// A uniform point over the spherical rectangle that the rectangle with
// corner `s` and orthogonal edges `ex`, `ey` subtends at `receiver`
// (Urena, Fajardo, and King 2013, as pbrt-v4 spells it), with its solid
// angle. Returns false when the solid angle is degenerate or outside the
// range the parametrization is accurate in, the caller then drawing by
// area.
[[nodiscard]] static bool
sampleSphericalRectangle(const float3 &receiver, const float3 &s,
                         const float3 &ex, const float3 &ey, float2 u,
                         float3 &point, float &solidAngle) noexcept {
  const float exl{length(ex)};
  const float eyl{length(ey)};
  if (!(exl > 0.0f) || !(eyl > 0.0f)) return false;
  const float3 x{ex / exl};
  const float3 y{ey / eyl};
  float3 z{cross(x, y)};
  const float3 d{s - receiver};
  float z0{dot(d, z)};
  if (z0 > 0.0f) {
    z = -z;
    z0 = -z0;
  }
  const float z0sq{z0 * z0};
  const float x0{dot(d, x)};
  const float y0{dot(d, y)};
  const float x1{x0 + exl};
  const float y1{y0 + eyl};
  const float y0sq{y0 * y0};
  const float y1sq{y1 * y1};
  const float3 v00{x0, y0, z0};
  const float3 v01{x0, y1, z0};
  const float3 v10{x1, y0, z0};
  const float3 v11{x1, y1, z0};
  float3 n0{cross(v00, v10)};
  float3 n1{cross(v10, v11)};
  float3 n2{cross(v11, v01)};
  float3 n3{cross(v01, v00)};
  if (!smdl::tryNormalize(n0) || !smdl::tryNormalize(n1) ||
      !smdl::tryNormalize(n2) || !smdl::tryNormalize(n3))
    return false;
  const auto angle{[](const float3 &a, const float3 &b) {
    return std::acos(std::clamp(-dot(a, b), -1.0f, 1.0f));
  }};
  const float g0{angle(n0, n1)};
  const float g1{angle(n1, n2)};
  const float g2{angle(n2, n3)};
  const float g3{angle(n3, n0)};
  const float b0{n0.z};
  const float b1{n2.z};
  const float b0sq{b0 * b0};
  const float k{TWO_PI - g2 - g3};
  solidAngle = g0 + g1 - k;
  if (!(solidAngle > 3.0e-4f) || !(solidAngle < 6.22f)) return false;
  const float au{u.x * solidAngle + k};
  const float fu{(std::cos(au) * b0 - b1) / std::sin(au)};
  float cu{(fu > 0.0f ? 1.0f : -1.0f) / std::sqrt(fu * fu + b0sq)};
  cu = std::clamp(cu, -1.0f, 1.0f);
  float xu{-(cu * z0) / std::max(std::sqrt(1.0f - cu * cu), 1.0e-6f)};
  xu = std::clamp(xu, x0, x1);
  const float d2{std::sqrt(xu * xu + z0sq)};
  const float h0{y0 / std::sqrt(d2 * d2 + y0sq)};
  const float h1{y1 / std::sqrt(d2 * d2 + y1sq)};
  const float hv{h0 + u.y * (h1 - h0)};
  const float hv2{hv * hv};
  const float yv{hv2 < 1.0f - 1.0e-6f ? (hv * d2) / std::sqrt(1.0f - hv2) : y1};
  point = receiver + xu * x + yv * y + z0 * z;
  return true;
}

float3 AnalyticLight::sampleShape(const float3 &receiver, float2 xi, float &pdf,
                                  float time) const noexcept {
  std::optional<Placement> scratch{};
  const auto &placed{placementAt(time, scratch)};
  pdf = 0.0f;
  if (mKind == LayoutLightDecl::Kind::RECT) {
    // Over the spherical rectangle when the placed axes are orthogonal;
    // a sheared placement makes a parallelogram, which the
    // parametrization does not cover.
    const float3 ex{2.0f * mHalfExtent.x * placed.axisU};
    const float3 ey{2.0f * mHalfExtent.y * placed.axisV};
    if (std::abs(dot(ex, ey)) <= 1.0e-4f * length(ex) * length(ey)) {
      float3 point{};
      float solidAngle{};
      if (sampleSphericalRectangle(receiver,
                                   placed.position - 0.5f * ex - 0.5f * ey, ex,
                                   ey, xi, point, solidAngle)) {
        pdf = 1.0f / solidAngle;
        return point;
      }
    }
  }
  float2 local{};
  if (mKind == LayoutLightDecl::Kind::DISK) {
    local = smdl::uniformDiskSample(xi);
    local.x *= mHalfExtent.x;
    local.y *= mHalfExtent.y;
  } else {
    local = float2((2.0f * xi.x - 1.0f) * mHalfExtent.x,
                   (2.0f * xi.y - 1.0f) * mHalfExtent.y);
  }
  const float3 point{placed.position + local.x * placed.axisU +
                     local.y * placed.axisV};
  const float3 direction{point - receiver};
  const float distSq{lengthSquared(direction)};
  if (!(distSq > 0.0f)) return point;
  const float cosTheta{absDot(placed.normal, direction / std::sqrt(distSq))};
  if (!(cosTheta > 0.0f)) return point;
  pdf = distSq / (placed.worldArea * cosTheta);
  return point;
}

// One minus the cosine of a cone's half angle from its squared sine, by
// the series when the angle is too small for the difference to survive
// float rounding.
[[nodiscard]] static float coneOneMinusCos(float sinThetaSq,
                                           float cosTheta) noexcept {
  return sinThetaSq < 0.00068523f * 0.00068523f ? 0.5f * sinThetaSq
                                                : 1.0f - cosTheta;
}

// The inverse of the cofactor matrix of a placement's linear part,
// directly: inv(cof(M)) is transpose(M) over det(M). What turns a WORLD
// unit normal back into the local area stretch; see
// `AreaLight::invCofactor`.
[[nodiscard]] static float3x3
inverseCofactorOf(const float4x4 &objectToWorld) noexcept {
  const auto column0{float3(objectToWorld[0])};
  const auto column1{float3(objectToWorld[1])};
  const auto column2{float3(objectToWorld[2])};
  const float det{dot(column0, cross(column1, column2))};
  return float3x3(float3(column0.x, column1.x, column2.x) / det,
                  float3(column0.y, column1.y, column2.y) / det,
                  float3(column0.z, column1.z, column2.z) / det);
}

Color AnalyticLight::Le(const float3 &lightPoint, const float3 &incidencePoint,
                        float time) const noexcept {
  std::optional<Placement> scratch{};
  const auto &placed{placementAt(time, scratch)};
  return dot(incidencePoint - lightPoint, placed.normal) > 0 ? Color(mIntensity)
                                                             : Color(0.0f);
}

float3 AnalyticLight::normal(float time) const noexcept {
  std::optional<Placement> scratch{};
  return placementAt(time, scratch).normal;
}

float3 AnalyticLight::position(float time) const noexcept {
  std::optional<Placement> scratch{};
  return placementAt(time, scratch).position;
}

BoundBox3 AnalyticLight::bounds() const noexcept {
  auto box{BoundBox3()};
  const auto extend{[&](const Placement &placed) {
    if (isDirac()) {
      box.extend(placed.position);
      return;
    }
    const float3 u{mHalfExtent.x * placed.axisU};
    const float3 v{mHalfExtent.y * placed.axisV};
    box.extend(placed.position + u + v);
    box.extend(placed.position + u - v);
    box.extend(placed.position - u + v);
    box.extend(placed.position - u - v);
  }};
  extend(mPlacement);
  if (mMoving) extend(derivePlacement(mLightToWorldShut));
  return box;
}

LightSelection::LightSelection(smdl::Span<const LightBounds> lights,
                               bool hasEnv, float envWeight, bool useTree)
    : mLightCount(int(lights.size())), mHasEnv(hasEnv) {
  auto weights{std::vector<float>()};
  weights.reserve(lights.size() + 1);
  for (const auto &light : lights) weights.push_back(light.phi);
  if (hasEnv) weights.push_back(envWeight);
  mDistr = smdl::Distribution1D(weights);
  if (useTree) mTree.emplace(lights);
}

int LightSelection::select(const float3 &point, float xi,
                           float &pmf) const noexcept {
  if (!mTree) return mDistr.indexSample(xi, nullptr, &pmf);
  // The environment takes the top of the unit interval, where the flat
  // distribution's last entry puts it, so a scene of one light and the
  // sky draws the same light for the same float either way.
  const float envPMF{mHasEnv ? mDistr.indexPMF(mLightCount) : 0.0f};
  const float lightShare{1.0f - envPMF};
  if (mHasEnv && xi >= lightShare) {
    pmf = envPMF;
    return mLightCount;
  }
  if (mTree->empty() || !(lightShare > 0.0f)) {
    pmf = 0.0f;
    return 0;
  }
  float treePMF{};
  const int lightIndex{
      mTree->sample(point, clampUnit(xi / lightShare), treePMF)};
  pmf = lightShare * treePMF;
  return lightIndex;
}

float LightSelection::pmf(int lightIndex, const float3 &point) const noexcept {
  if (!mTree) return mDistr.indexPMF(lightIndex);
  const float envPMF{mHasEnv ? mDistr.indexPMF(mLightCount) : 0.0f};
  if (mHasEnv && lightIndex == mLightCount) return envPMF;
  return (1.0f - envPMF) * mTree->pmf(lightIndex, point);
}

LightSampler::LightSampler(smdl::Compiler &compiler, const Scene &scene,
                           const EnvLight *envLight,
                           const std::vector<LayoutLight> &layoutLights,
                           const Color &wavelengths, bool allLights,
                           bool useTree)
    : compiler(compiler), scene(scene), envLight(envLight) {
  auto allocator{smdl::BumpPtrAllocator()};
  auto bounds{std::vector<LightBounds>()};
  auto warnedCurveMaterials{std::set<uint32_t>()};
  auto warnedMarkMaterials{std::set<uint32_t>()};
  size_t numSampledArea{};
  size_t numUnsampledArea{};
  instanceToLight.resize(scene.meshInstances.size(), INVALID_INDEX);
  for (uint32_t instIndex = 0; instIndex < scene.meshInstances.size();
       instIndex++) {
    const auto &instance{scene.meshInstances[instIndex]};
    // The instance-resolved material: an instance whose override maps a
    // plain material to an emissive one is an emitter, and one that maps
    // an emissive material away is not.
    const auto matIndex{scene.materialIndexOf(instance)};
    const auto *material{scene.materials[matIndex]};
    if (!material) continue;
    // Evaluate the material once with a placeholder state to read the
    // structural emission flags and a representative intensity. The flags
    // are decided by whether the emission EDF is non-default, so they do
    // not depend on the state; the intensity may be spatially varying, in
    // which case its value here is only a representative selection weight.
    auto state{makeRenderState(wavelengths, &allocator)};
    state.texture_space_max = 1;
    state.finalizeAndApplyInternalSpaceConventions();
    auto mat{smdl::JIT::MaterialInstance(state, material)};
    if (!mat.hasEmission()) {
      // The mark is scene judgment about an emitter; on anything else it
      // is a mistake worth one line, as the caster mark's is.
      if (instance.light && warnedMarkMaterials.insert(matIndex).second)
        SMDL_LOG_WARN("The material ",
                      smdl::Quoted(scene.materialNames[matIndex]),
                      " is marked 'light' but has no emission; the mark is "
                      "ignored.");
      allocator.reset();
      continue;
    }
    // Curves have no area-sampling machinery, so an emissive groom does
    // not register as a light: its emission still renders wherever a
    // path hits a fiber, but next-event estimation never aims at it.
    // Say so once per material rather than silently.
    if (instance.isCurves()) {
      if (warnedCurveMaterials.insert(scene.materialIndexOf(instance)).second)
        SMDL_LOG_WARN(
            "Curves shaded by the emissive material ",
            smdl::Quoted(scene.materialNames[scene.materialIndexOf(instance)]),
            " do not register as area lights; their emission renders "
            "through path hits only.");
      allocator.reset();
      continue;
    }
    auto light{AreaLight()};
    light.instIndex = instIndex;
    light.isSampled = instance.light || allLights;
    light.caustic = instance.causticLight;
    // Areas are world-space areas, matching the world-space geometry
    // `Scene::makeHit` reports: a scaled instance covers more surface and
    // must emit proportionally more power. Because an `AreaLight` is per
    // instance rather than per mesh, transforming the vertices here is
    // exact even under non-uniform scale, where no single area factor
    // would do.
    const auto &objectToWorld{instance.frame.objectToWorld};
    // A moving emitter's shut frame: its box covers both keys, so that
    // the light tree sees where it can be (a proxy rather than a hull
    // for a turn, and consistent between `select()` and `pmf()`, which
    // is all the tree needs), and the sphere cone is drawn only when
    // the shape is a sphere at both keys.
    std::optional<InstanceFrame> shutScratch{};
    const InstanceFrame *shutFrame{
        instance.isMoving ? &instance.frameAt(1.0f, shutScratch) : nullptr};
    auto box{BoundBox3()};
    if (instance.isPrimitive()) {
      const auto &primitive{*scene.primitives[instance.primIndex]};
      light.isPrimitive = true;
      light.objectArea = primitive.objectArea;
      for (const auto &point : primitive.proxyPoints) {
        box.extend(float3(objectToWorld * float4(point, 1.0f)));
        if (shutFrame)
          box.extend(float3(shutFrame->objectToWorld * float4(point, 1.0f)));
      }
      if (primitive.spec.shape == PrimitiveSpec::Shape::SPHERE &&
          !instance.frame.isDeformed && !(shutFrame && shutFrame->isDeformed)) {
        light.sphereCenter = float3(objectToWorld[3]);
        light.sphereRadius =
            primitive.spec.radius * length(float3(objectToWorld[0]));
        light.sphereObjectRadius = primitive.spec.radius;
      }
      // The world area through the placement's area stretch, J(n) =
      // |cofactor * n|: constant under a similarity (so this is exact),
      // estimated as the mean over a deterministic sample set otherwise.
      double stretchSum{};
      constexpr int STRETCH_SAMPLES = 64;
      for (int i = 0; i < STRETCH_SAMPLES; i++) {
        const float x1{(float(i) + 0.5f) / float(STRETCH_SAMPLES)};
        const float x2{float(i) * 0.6180339887f};
        const auto areaSample{samplePrimitiveArea(
            primitive.spec, float2(x1, x2 - std::floor(x2)))};
        stretchSum +=
            double(length(instance.frame.normalMatrix * areaSample.normal));
      }
      light.totalArea = light.objectArea * float(stretchSum / STRETCH_SAMPLES);
      light.invCofactor = inverseCofactorOf(objectToWorld);
    } else {
      const auto &mesh{*scene.meshes[instance.meshIndex]};
      auto toWorld{[&](const float3 &point) {
        return float3(objectToWorld * float4(point, 1.0f));
      }};
      // The face distribution serves `sample()` alone, so an unsampled
      // emitter, which only needs its total area, does not build one. A
      // moving light's is over object area, since its world area is a
      // function of time; see `sampleAreaMoving()`.
      auto faceAreas{std::vector<float>()};
      auto objectFaceAreas{std::vector<float>()};
      if (light.isSampled) faceAreas.reserve(mesh.faces.size());
      if (shutFrame) objectFaceAreas.reserve(mesh.faces.size());
      for (const auto &face : mesh.faces) {
        const auto point0{toWorld(mesh.verts[face[0]].point)};
        const auto point1{toWorld(mesh.verts[face[1]].point)};
        const auto point2{toWorld(mesh.verts[face[2]].point)};
        box.extend(point0);
        box.extend(point1);
        box.extend(point2);
        auto area{0.5f * length(cross(point1 - point0, point2 - point0))};
        if (light.isSampled) faceAreas.push_back(area);
        light.totalArea += area;
        if (shutFrame) {
          const auto &object0{mesh.verts[face[0]].point};
          const auto &object1{mesh.verts[face[1]].point};
          const auto &object2{mesh.verts[face[2]].point};
          const float objectArea{
              0.5f * length(cross(object1 - object0, object2 - object0))};
          objectFaceAreas.push_back(objectArea);
          light.objectArea += objectArea;
          for (const auto &object : {object0, object1, object2})
            box.extend(float3(shutFrame->objectToWorld * float4(object, 1.0f)));
        }
      }
      if (light.isSampled && light.totalArea > 0)
        light.faceDistr =
            smdl::Distribution1D(shutFrame ? objectFaceAreas : faceAreas);
    }
    if (!(light.totalArea > 0)) {
      allocator.reset();
      continue;
    }
    // The selection weight is the power: intensity times area under
    // `intensity_radiant_exitance`, the intensity itself under
    // `intensity_power`. An unsampled emitter weighs nothing, which is
    // the whole of what the mark decides.
    auto average{[](smdl::Span<const float> values) {
      float sum{};
      for (float value : values) sum += value;
      return values.empty() ? 0.0f : sum / values.size();
    }};
    float weight{};
    if (float intensity{average(mat.getSurfaceEmissionIntensity())};
        intensity > 0)
      weight += mat.isSurfaceEmissionPower() ? intensity
                                             : intensity * light.totalArea;
    if (float intensity{average(mat.getBackfaceEmissionIntensity())};
        intensity > 0)
      weight += mat.isBackfaceEmissionPower() ? intensity
                                              : intensity * light.totalArea;
    instanceToLight[instIndex] = uint32_t(areaLights.size());
    (light.isSampled ? numSampledArea : numUnsampledArea)++;
    bounds.push_back({box, light.isSampled ? weight : 0.0f});
    areaLights.push_back(std::move(light));
    allocator.reset();
  }
  if (!layoutLights.empty()) {
    // One profile per distinct resolved path, shared between its
    // placements: a layout that scatters a hundred streetlights loads
    // the IES file once.
    auto profiles{
        std::map<std::string, std::shared_ptr<const smdl::LightProfile>>()};
    auto state{makeRenderState(wavelengths)};
    analyticLights.reserve(layoutLights.size());
    for (const auto &layoutLight : layoutLights) {
      auto profile{std::shared_ptr<const smdl::LightProfile>()};
      if (layoutLight.decl.kind == LayoutLightDecl::Kind::PROFILE) {
        auto &cached{profiles[layoutLight.decl.profilePath]};
        if (!cached) {
          auto loaded{std::make_shared<smdl::LightProfile>()};
          if (auto error{loaded->loadFromFile(layoutLight.decl.profilePath)})
            error->printAndExit();
          cached = std::move(loaded);
        }
        profile = cached;
      }
      auto &light{analyticLights.emplace_back(compiler, state, wavelengths,
                                              layoutLight, std::move(profile))};
      light.caustic = layoutLight.decl.caustic;
      bounds.push_back({light.bounds(), light.weight()});
    }
  }
  float envWeight{};
  if (envLight) {
    // Treat the environment as shining on a disk of the scene radius.
    float radius{std::max(scene.boundRadius, 1.0f)};
    envWeight = envLight->averageRadiance() * PI * radius * radius;
  }
  if (!bounds.empty() || envLight)
    mSelection =
        LightSelection(bounds, envLight != nullptr, envWeight, useTree);
  // The `caustic` marks restrict the manifold reflective gather to the
  // marked lights; no marks anywhere means no restriction, so the flags
  // normalize to all-true and the whole mechanism disappears. The
  // environment cannot carry a mark, so it is a target exactly while
  // nothing is restricted. Only sampled lights take part: an unsampled
  // emitter is never a target, since no gather aims at it.
  {
    bool anyMark{false};
    for (const auto &light : areaLights)
      anyMark |= light.isSampled && light.caustic;
    for (const auto &light : analyticLights) anyMark |= light.caustic;
    if (!anyMark) {
      for (auto &light : areaLights) light.caustic = light.isSampled;
      for (auto &light : analyticLights) light.caustic = true;
    }
    mEnvCaustic = !anyMark;
  }
  // Leaving an emitter unmarked is a choice the layout makes silently;
  // leaving every emitter unmarked is far more often a layout that has
  // not been marked yet, and gets one line.
  if (numSampledArea == 0 && numUnsampledArea > 0)
    SMDL_LOG_INFO("No emitter is marked 'light': ", numUnsampledArea,
                  " emissive instance(s) render through path hits alone; "
                  "mark them in the layout, or pass -all-lights.");
  SMDL_LOG_DEBUG("Light sampler: ", numSampledArea, " area light(s), ",
                 numUnsampledArea, " unsampled emitter(s), ",
                 analyticLights.size(), " analytic light(s)",
                 envLight ? ", plus the environment" : "");
  if (const auto *tree{mSelection.tree()})
    SMDL_LOG_DEBUG("Light tree: ", tree->nodeCount(), " node(s), depth ",
                   tree->depth());
}

bool LightSampler::sample(const smdl::State &state, Sampler &sampler,
                          const float3 &point, float time,
                          LightSample &lightSample, bool keepDark) const {
  if (empty()) return false;
  float selectPMF{};
  const int lightIndex{mSelection.select(point, float(sampler), selectPMF)};
  if (!(selectPMF > 0)) return false;
  lightSample.isDirac = false;
  lightSample.isReachable = true;
  lightSample.normal = float3(0.0f);
  lightSample.analyticIndex = INVALID_INDEX;
  if (envLight &&
      lightIndex == int(areaLights.size() + analyticLights.size())) {
    float dirPDF{};
    lightSample.wi = envLight->Li_sample(compiler, state, float2(sampler),
                                         dirPDF, lightSample.Li);
    if (!(dirPDF > 0)) return false;
    lightSample.pdf = selectPMF * dirPDF;
    lightSample.target = point + 2.0f * scene.boundRadius * lightSample.wi;
    lightSample.isInfinite = true;
    lightSample.isCaustic = mEnvCaustic;
    return true;
  }
  if (lightIndex >= int(areaLights.size())) {
    const uint32_t analyticIndex{uint32_t(lightIndex) -
                                 uint32_t(areaLights.size())};
    const auto &light{analyticLights[analyticIndex]};
    lightSample.isReachable = false;
    lightSample.analyticIndex = analyticIndex;
    lightSample.isCaustic = light.caustic;
    if (light.isDirac()) {
      // A punctual light: the direction is a Dirac, so the pdf is the
      // selection PMF alone and `Li` carries the inverse-square falloff.
      const float3 position{light.position(time)};
      auto direction{position - point};
      if (!(lengthSquared(direction) > 0)) return false;
      lightSample.Li = light.Li(point, state.meters_per_scene_unit, time);
      if (lightSample.Li.isAllZero() && !keepDark) return false;
      lightSample.wi = normalize(direction);
      lightSample.pdf = selectPMF;
      lightSample.target = position;
      lightSample.isDirac = true;
      return true;
    }
    // A shape: a point on it at a solid-angle density, and the radiance
    // toward the receiver from its emitting side, exactly as for an
    // area light except that no material stands behind the point.
    float shapePDF{};
    const float3 lightPoint{
        light.sampleShape(point, float2(sampler), shapePDF, time)};
    if (!(shapePDF > 0.0f)) return false;
    auto direction{lightPoint - point};
    if (!(lengthSquared(direction) > 0.0f)) return false;
    lightSample.wi = normalize(direction);
    lightSample.Li = light.Le(lightPoint, point, time);
    if (lightSample.Li.isAllZero() && !keepDark) return false;
    lightSample.pdf = selectPMF * shapePDF;
    lightSample.target = lightPoint;
    lightSample.normal = light.normal(time);
    return true;
  }
  const auto &light{areaLights[lightIndex]};
  // The zero selection weight is what keeps an unsampled emitter out.
  SMDL_SANITY_CHECK(light.isSampled);
  Hit hit{};
  lightSample.isCaustic = light.caustic;
  float positionPDF{}; // world-space area density at the sampled point
  float conePDF{};     // solid-angle density instead, when drawn by cone
  const auto &instance{scene.meshInstances[light.instIndex]};
  if (instance.isMoving) {
    if (!sampleAreaMoving(light, instance, sampler, point, time, keepDark, hit,
                          positionPDF, conePDF))
      return false;
  } else if (light.isPrimitive) {
    const auto &primitive{*scene.primitives[instance.primIndex]};
    const float2 xi{sampler};
    // A sphere is drawn by its cone from the receiver, except for a
    // manifold gather, which keeps the uniform area draw: a cone never
    // reaches the far side, where the lamp point of a reflective
    // connection can lie. The alternative is the cone everywhere,
    // leaving those arrivals to the path tracer at weight 1.
    if (!(light.sphereRadius > 0.0f && !keepDark &&
          sampleSphereCone(light, instance.frame, light.sphereCenter,
                           light.sphereRadius, point, time, xi, hit,
                           conePDF))) {
      // Sample the shape uniformly by OBJECT area and pay the placement's
      // exact area stretch in the pdf: still unbiased under any affine
      // placement, and exactly uniform under a similarity.
      const auto areaSample{samplePrimitiveArea(primitive.spec, xi)};
      const float stretch{
          length(instance.frame.normalMatrix * areaSample.normal)};
      if (!(stretch > 0)) return false;
      hit =
          scene.makePrimitiveHit(light.instIndex, areaSample.primID,
                                 float3(0.0f, areaSample.uv.x, areaSample.uv.y),
                                 time, areaSample.point);
      positionPDF = 1.0f / (light.objectArea * stretch);
    }
  } else {
    int faceIndex{light.faceDistr.indexSample(float(sampler))};
    // Sample uniformly over the triangle. The face CMF is proportional to
    // world-space area, so this is uniform over the instance as it stands
    // in the world, and `Scene::makeHit` reports the point in the same
    // space.
    float2 xi{sampler};
    float sqrtXi{std::sqrt(xi.x)};
    auto bary{float3(1.0f - sqrtXi, sqrtXi * (1.0f - xi.y), sqrtXi * xi.y)};
    hit = scene.makeHit(light.instIndex, uint32_t(faceIndex), bary, time);
    positionPDF = 1.0f / light.totalArea;
  }
  auto direction{hit.point - point};
  float distSq{lengthSquared(direction)};
  if (!(distSq > 0)) return false;
  lightSample.wi = normalize(direction);
  float cosTheta{absDot(hit.Ng, lightSample.wi)};
  if (!(cosTheta > 0)) return false;
  // The NEE ray arrives at the light surface along `wi`. The LOD fields
  // stay zero here deliberately: emission evaluates at full fidelity.
  auto lightState{state};
  hit.applyGeometryToState(lightState, lightSample.wi);
  auto mat{smdl::JIT::MaterialInstance(lightState, hit.material)};
  if (!emittedRadiance(mat, light.instIndex, -lightSample.wi, lightSample.Li)) {
    if (!keepDark) return false;
    lightSample.Li = Color(0.0f);
  }
  // Convert the position density to solid angle at the receiver.
  lightSample.pdf =
      selectPMF * (conePDF > 0.0f ? conePDF : distSq * positionPDF / cosTheta);
  lightSample.target = hit.point;
  lightSample.normal = hit.Ng;
  lightSample.hit = hit;
  return true;
}

bool LightSampler::sampleAreaMoving(const AreaLight &light,
                                    const MeshInstance &instance,
                                    Sampler &sampler, const float3 &point,
                                    float time, bool keepDark, Hit &hit,
                                    float &positionPDF, float &conePDF) const {
  std::optional<InstanceFrame> scratch{};
  const auto &frame{instance.frameAtMoving(time, scratch)};
  if (light.isPrimitive) {
    const auto &primitive{*scene.primitives[instance.primIndex]};
    const float2 xi{sampler};
    if (light.sphereObjectRadius > 0.0f && !keepDark &&
        sampleSphereCone(light, frame, float3(frame.objectToWorld[3]),
                         light.sphereObjectRadius *
                             length(float3(frame.objectToWorld[0])),
                         point, time, xi, hit, conePDF))
      return true;
    const auto areaSample{samplePrimitiveArea(primitive.spec, xi)};
    const float stretch{length(frame.normalMatrix * areaSample.normal)};
    if (!(stretch > 0)) return false;
    hit = scene.makePrimitiveHitFrom(
        frame, light.instIndex, areaSample.primID,
        float3(0.0f, areaSample.uv.x, areaSample.uv.y), time,
        evalPrimitiveSurfaceAt(primitive.spec, areaSample.primID,
                               areaSample.point));
    positionPDF = 1.0f / (light.objectArea * stretch);
    return true;
  }
  // Uniform by object area: the face by its object area, the point
  // uniformly within it, and the density through the exact stretch of
  // the frame at the time, which is constant over a face.
  const auto &mesh{*scene.meshes[instance.meshIndex]};
  const int faceIndex{light.faceDistr.indexSample(float(sampler))};
  const float2 xi{sampler};
  const float sqrtXi{std::sqrt(xi.x)};
  const auto bary{float3(1.0f - sqrtXi, sqrtXi * (1.0f - xi.y), sqrtXi * xi.y)};
  hit = scene.makeHit(frame, light.instIndex, uint32_t(faceIndex), bary, time);
  const auto &face{mesh.faces[size_t(faceIndex)]};
  const auto &object0{mesh.verts[face[0]].point};
  const auto &object1{mesh.verts[face[1]].point};
  const auto &object2{mesh.verts[face[2]].point};
  const float3 objectNormal{
      normalize(cross(object1 - object0, object2 - object0))};
  const float stretch{length(frame.normalMatrix * objectNormal)};
  if (!(stretch > 0)) return false;
  positionPDF = 1.0f / (light.objectArea * stretch);
  return true;
}

bool LightSampler::sampleSphereCone(const AreaLight &light,
                                    const InstanceFrame &frame,
                                    const float3 &center, float radius,
                                    const float3 &point, float time, float2 xi,
                                    Hit &hit, float &pdf) const {
  const float3 toCenter{center - point};
  const float distSq{lengthSquared(toCenter)};
  const float radiusSq{radius * radius};
  if (!(distSq > radiusSq)) return false;
  const float sinThetaMaxSq{radiusSq / distSq};
  const float cosThetaMax{std::sqrt(std::max(1.0f - sinThetaMaxSq, 0.0f))};
  const float3 axis{toCenter / std::sqrt(distSq)};
  const float3 u{smdl::perpendicularTo(axis)};
  const float3 v{cross(axis, u)};
  const float3 local{smdl::uniformConeSample(cosThetaMax, xi)};
  const float3 wi{local.x * u + local.y * v + local.z * axis};
  // The near intersection of the sampled direction with the sphere,
  // then back to object space through the rigid frame: the sphere is
  // centered there, so the point is the radius along the rotated normal.
  const float b{dot(wi, toCenter)};
  const float t{b - std::sqrt(std::max(b * b - (distSq - radiusSq), 0.0f))};
  const float3 normal{normalize(point + t * wi - center)};
  const auto &instance{scene.meshInstances[light.instIndex]};
  const auto &primitive{*scene.primitives[instance.primIndex]};
  const float3 objectNormal{
      normalize(float3(frame.worldToRigid * float4(normal, 0.0f)))};
  const float3 objectPoint{primitive.spec.radius * objectNormal};
  const float2 uv{primitiveUV(primitive.spec, 0, objectPoint)};
  hit = scene.makePrimitiveHitFrom(
      frame, light.instIndex, 0, float3(0.0f, uv.x, uv.y), time,
      evalPrimitiveSurfaceAt(primitive.spec, 0, objectPoint));
  pdf = 1.0f / (TWO_PI * coneOneMinusCos(sinThetaMaxSq, cosThetaMax));
  return true;
}

Color LightSampler::reevaluateLi(const LightSample &lightSample,
                                 const smdl::State &state, const float3 &point,
                                 const float3 &incidencePoint,
                                 float time) const {
  if (lightSample.isInfinite) return lightSample.Li;
  if (lightSample.analyticIndex != INVALID_INDEX) {
    if (lightSample.analyticIndex >= analyticLights.size()) return Color(0.0f);
    const auto &light{analyticLights[lightSample.analyticIndex]};
    return light.isDirac() ? light.Li(point, incidencePoint,
                                      state.meters_per_scene_unit, time)
                           : light.Le(lightSample.target, incidencePoint, time);
  }
  const auto &hit{lightSample.hit};
  if (!hit.material) return Color(0.0f);
  auto wEmit{incidencePoint - hit.point};
  if (!smdl::tryNormalize(wEmit)) return Color(0.0f);
  // The arriving ray travels the other way down the same segment, which
  // is the sense `sample()` applies the geometry in.
  auto lightState{state};
  hit.applyGeometryToState(lightState, -wEmit);
  auto mat{smdl::JIT::MaterialInstance(lightState, hit.material)};
  Color Le{};
  if (!emittedRadiance(mat, hit.instIndex, wEmit, Le)) return Color(0.0f);
  return Le;
}

bool LightSampler::emittedRadiance(const smdl::JIT::MaterialInstance &mat,
                                   uint32_t instIndex, const float3 &wi,
                                   Color &Le) const {
  float edfPDF{};
  if (!mat.emissionEvaluate(wi, edfPDF, Le)) return false;
  // `intensity_power` leaves the host to divide by the total emitting
  // surface area; see `JIT::Material::emissionEvaluate`. The mode is per
  // side, so pick the side actually emitting toward `wi`.
  bool isPower{mat.isExterior(wi)          ? mat.isSurfaceEmissionPower()
               : mat.hasBackfaceEmission() ? mat.isBackfaceEmissionPower()
                                           : mat.isSurfaceEmissionPower()};
  if (isPower) {
    float area{1.0f};
    if (instIndex < instanceToLight.size() &&
        instanceToLight[instIndex] != INVALID_INDEX) {
      area = areaLights[instanceToLight[instIndex]].totalArea;
    }
    Le = Le / area;
  }
  return true;
}

// The solid-angle density of a moving light's draw, the geometry read
// from the frame at the hit's time: the sphere's center and radius from
// its columns, and the area stretch through its inverse cofactor, for
// a mesh light exactly as for a primitive. The static path below keeps
// its own arithmetic.
[[nodiscard]] static float
solidAnglePDFMoving(const AreaLight &light, const MeshInstance &instance,
                    const float3 &lightPoint, const float3 &lightNormal,
                    const float3 &point, bool areaSampled, float time,
                    float selectPMF) {
  std::optional<InstanceFrame> scratch{};
  const auto &objectToWorld{
      instance.frameAtMoving(time, scratch).objectToWorld};
  if (light.sphereObjectRadius > 0.0f && !areaSampled) {
    const float3 center{objectToWorld[3]};
    const float radius{light.sphereObjectRadius *
                       length(float3(objectToWorld[0]))};
    const float distSqCenter{lengthSquared(center - point)};
    const float radiusSq{radius * radius};
    if (distSqCenter > radiusSq) {
      if (!(dot(lightNormal, point - lightPoint) > 0.0f)) return 0.0f;
      const float sinThetaMaxSq{radiusSq / distSqCenter};
      const float cosThetaMax{std::sqrt(std::max(1.0f - sinThetaMaxSq, 0.0f))};
      return selectPMF / (TWO_PI * coneOneMinusCos(sinThetaMaxSq, cosThetaMax));
    }
  }
  auto direction{lightPoint - point};
  const float distSq{lengthSquared(direction)};
  if (!(distSq > 0)) return 0.0f;
  const float cosTheta{absDot(lightNormal, normalize(direction))};
  if (!(cosTheta > 0)) return 0.0f;
  const float positionPDF{
      length(inverseCofactorOf(objectToWorld) * lightNormal) /
      light.objectArea};
  return selectPMF * distSq * positionPDF / cosTheta;
}

float LightSampler::solidAnglePDF(uint32_t instIndex, const float3 &lightPoint,
                                  const float3 &lightNormal,
                                  const float3 &point, bool areaSampled,
                                  float time) const {
  if (empty() || instIndex >= instanceToLight.size() ||
      instanceToLight[instIndex] == INVALID_INDEX) {
    return 0.0f;
  }
  auto lightIndex{instanceToLight[instIndex]};
  const auto &light{areaLights[lightIndex]};
  if (!light.isSampled) return 0.0f;
  const float selectPMF{mSelection.pmf(int(lightIndex), point)};
  if (const auto &instance{scene.meshInstances[light.instIndex]};
      instance.isMoving)
    return solidAnglePDFMoving(light, instance, lightPoint, lightNormal, point,
                               areaSampled, time, selectPMF);
  if (light.sphereRadius > 0.0f && !areaSampled) {
    const float distSqCenter{lengthSquared(light.sphereCenter - point)};
    const float radiusSq{light.sphereRadius * light.sphereRadius};
    if (distSqCenter > radiusSq) {
      // The cone covers the cap facing the receiver and nothing else.
      if (!(dot(lightNormal, point - lightPoint) > 0.0f)) return 0.0f;
      const float sinThetaMaxSq{radiusSq / distSqCenter};
      const float cosThetaMax{std::sqrt(std::max(1.0f - sinThetaMaxSq, 0.0f))};
      return selectPMF / (TWO_PI * coneOneMinusCos(sinThetaMaxSq, cosThetaMax));
    }
  }
  auto direction{lightPoint - point};
  float distSq{lengthSquared(direction)};
  if (!(distSq > 0)) return 0.0f;
  float cosTheta{absDot(lightNormal, normalize(direction))};
  if (!(cosTheta > 0)) return 0.0f;
  // A primitive light's position density is object-uniform through the
  // placement's area stretch, recovered exactly from the world normal:
  // J = 1 / |inv(cofactor) * n|, so 1 / (A J) = |inv(cofactor) * n| / A.
  const float positionPDF{light.isPrimitive
                              ? length(light.invCofactor * lightNormal) /
                                    light.objectArea
                              : 1.0f / light.totalArea};
  return selectPMF * distSq * positionPDF / cosTheta;
}
