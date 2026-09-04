#include "Render/Light.h"

#include "smdl/Support/Logger.h"

#include <map>
#include <set>

EnvLight::EnvLight(const std::string &fileName, float scaleFactor)
    : scaleFactor(scaleFactor) {
  // Never mipped: the environment is sampled by direction through the
  // tabulated density below, never with a texture-space footprint, so
  // the chain would be reserved and never touched. That matters here,
  // where the image is routinely a multi-thousand-pixel HDR.
  if (auto error{image.startLoad(fileName, /*allowMipLevels=*/false)})
    error->printAndExit();
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
  const auto &xf{light.lightToWorld};
  mPosition = float3(xf[3]);
  auto column{[&](int i) {
    auto v{float3(xf[i])};
    return smdl::tryNormalize(v) ? v : float3(i == 0, i == 1, i == 2);
  }};
  mLocalX = column(0);
  mLocalY = column(1);
  mLocalZ = column(2);
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
    // The plane the placement puts the shape in: the in-plane axes keep
    // the placement's scale, the area stretch is the length of their
    // cross product, constant over a plane, and the emitting side is
    // the one the placement maps local -Z into.
    mAxisU = float3(xf[0]);
    mAxisV = float3(xf[1]);
    const bool isRect{mKind == LayoutLightDecl::Kind::RECT};
    mHalfExtent = isRect ? float2(0.5f * decl.size.x, 0.5f * decl.size.y)
                         : float2(decl.radius, decl.radius);
    const float objectArea{isRect ? decl.size.x * decl.size.y
                                  : PI * decl.radius * decl.radius};
    auto planeNormal{cross(mAxisU, mAxisV)};
    const float stretch{length(planeNormal)};
    mWorldArea = objectArea * stretch;
    if (!(mWorldArea > 0)) {
      SMDL_LOG_WARN("The ", decl.kindName(), " light ", smdl::Quoted(decl.name),
                    " is placed with no area and is never sampled.");
      power = 0.0f;
      break;
    }
    planeNormal /= stretch;
    mNormal = dot(planeNormal, float3(xf[2])) > 0 ? -planeNormal : planeNormal;
    // One-sided Lambertian: the radiance is the power over pi times the
    // area in square meters.
    const float metersPerSceneUnit{state.meters_per_scene_unit};
    intensityScale = decl.power / (PI * mWorldArea * metersPerSceneUnit *
                                   metersPerSceneUnit);
    break;
  }
  }
  mWeight = power * float(meanShape);
  for (size_t i = 0; i < wavelengths.size(); i++)
    mIntensity[i] = intensityScale * shape[i];
}

Color AnalyticLight::Li(const float3 &point,
                        float metersPerSceneUnit) const noexcept {
  return Li(point, point, metersPerSceneUnit);
}

Color AnalyticLight::Li(const float3 &point, const float3 &incidencePoint,
                        float metersPerSceneUnit) const noexcept {
  const float distSq{lengthSquared(point - mPosition)};
  if (!(distSq > 0)) return Color(0.0f);
  auto direction{incidencePoint - mPosition};
  if (!(lengthSquared(direction) > 0)) return Color(0.0f);
  direction = normalize(direction);
  float factor{1.0f};
  if (mKind == LayoutLightDecl::Kind::SPOT) {
    // Emission aims along the local -Z axis, full intensity inside the
    // inner cone, smoothstepped in the cosine down to zero at the outer.
    const float cosTheta{dot(direction, -mLocalZ)};
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
    factor = mProfile->interpolate(float3(dot(direction, mLocalX),
                                          dot(direction, mLocalY),
                                          -dot(direction, mLocalZ)));
    if (!(factor > 0)) return Color(0.0f);
  }
  const float distSqMeters{distSq * metersPerSceneUnit * metersPerSceneUnit};
  auto Li{Color(mIntensity)};
  Li *= factor / distSqMeters;
  return Li;
}

float3 AnalyticLight::sampleShape(float2 xi,
                                  float &positionPDF) const noexcept {
  float2 local{};
  if (mKind == LayoutLightDecl::Kind::DISK) {
    local = smdl::uniformDiskSample(xi);
    local.x *= mHalfExtent.x;
    local.y *= mHalfExtent.y;
  } else {
    local = float2((2.0f * xi.x - 1.0f) * mHalfExtent.x,
                   (2.0f * xi.y - 1.0f) * mHalfExtent.y);
  }
  positionPDF = 1.0f / mWorldArea;
  return mPosition + local.x * mAxisU + local.y * mAxisV;
}

Color AnalyticLight::Le(const float3 &lightPoint,
                        const float3 &incidencePoint) const noexcept {
  return dot(incidencePoint - lightPoint, mNormal) > 0 ? Color(mIntensity)
                                                       : Color(0.0f);
}

BoundBox3 AnalyticLight::bounds() const noexcept {
  auto box{BoundBox3()};
  if (isDirac()) {
    box.extend(mPosition);
    return box;
  }
  const float3 u{mHalfExtent.x * mAxisU};
  const float3 v{mHalfExtent.y * mAxisV};
  box.extend(mPosition + u + v);
  box.extend(mPosition + u - v);
  box.extend(mPosition - u + v);
  box.extend(mPosition - u - v);
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
    const auto &objectToWorld{instance.objectToWorld};
    auto box{BoundBox3()};
    if (instance.isPrimitive()) {
      const auto &primitive{*scene.primitives[instance.primIndex]};
      light.isPrimitive = true;
      light.objectArea = primitive.objectArea;
      for (const auto &point : primitive.proxyPoints)
        box.extend(float3(objectToWorld * float4(point, 1.0f)));
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
        stretchSum += double(length(instance.normalMatrix * areaSample.normal));
      }
      light.totalArea = light.objectArea * float(stretchSum / STRETCH_SAMPLES);
      // The inverse cofactor, directly: inv(cof(M)) is transpose(M)
      // over det(M).
      const auto column0{float3(objectToWorld[0])};
      const auto column1{float3(objectToWorld[1])};
      const auto column2{float3(objectToWorld[2])};
      const float det{dot(column0, cross(column1, column2))};
      light.invCofactor =
          float3x3(float3(column0.x, column1.x, column2.x) / det,
                   float3(column0.y, column1.y, column2.y) / det,
                   float3(column0.z, column1.z, column2.z) / det);
    } else {
      const auto &mesh{*scene.meshes[instance.meshIndex]};
      auto toWorld{[&](const float3 &point) {
        return float3(objectToWorld * float4(point, 1.0f));
      }};
      // The face distribution serves `sample()` alone, so an unsampled
      // emitter, which only needs its total area, does not build one.
      auto faceAreas{std::vector<float>()};
      if (light.isSampled) faceAreas.reserve(mesh.faces.size());
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
      }
      if (light.isSampled && light.totalArea > 0)
        light.faceDistr = smdl::Distribution1D(faceAreas);
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
                          const float3 &point, LightSample &lightSample,
                          bool keepDark) const {
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
      auto direction{light.position() - point};
      if (!(lengthSquared(direction) > 0)) return false;
      lightSample.Li = light.Li(point, state.meters_per_scene_unit);
      if (lightSample.Li.isAllZero() && !keepDark) return false;
      lightSample.wi = normalize(direction);
      lightSample.pdf = selectPMF;
      lightSample.target = light.position();
      lightSample.isDirac = true;
      return true;
    }
    // A shape: a uniform-area point on it, the radiance toward the
    // receiver from its emitting side, and the position density
    // converted to solid angle, exactly as for an area light except
    // that no material stands behind the point.
    float positionPDF{};
    const float3 lightPoint{light.sampleShape(float2(sampler), positionPDF)};
    auto direction{lightPoint - point};
    const float distSq{lengthSquared(direction)};
    if (!(distSq > 0)) return false;
    lightSample.wi = normalize(direction);
    const float cosTheta{absDot(light.normal(), lightSample.wi)};
    if (!(cosTheta > 0)) return false;
    lightSample.Li = light.Le(lightPoint, point);
    if (lightSample.Li.isAllZero() && !keepDark) return false;
    lightSample.pdf = selectPMF * distSq * positionPDF / cosTheta;
    lightSample.target = lightPoint;
    lightSample.normal = light.normal();
    return true;
  }
  const auto &light{areaLights[lightIndex]};
  // The zero selection weight is what keeps an unsampled emitter out.
  SMDL_SANITY_CHECK(light.isSampled);
  Hit hit{};
  lightSample.isCaustic = light.caustic;
  float positionPDF{}; // world-space area density at the sampled point
  if (light.isPrimitive) {
    // Sample the shape uniformly by OBJECT area and pay the placement's
    // exact area stretch in the pdf: still unbiased under any affine
    // placement, and exactly uniform under a similarity.
    const auto &instance{scene.meshInstances[light.instIndex]};
    const auto &primitive{*scene.primitives[instance.primIndex]};
    const auto areaSample{samplePrimitiveArea(primitive.spec, float2(sampler))};
    const float stretch{length(instance.normalMatrix * areaSample.normal)};
    if (!(stretch > 0)) return false;
    hit = scene.makePrimitiveHit(light.instIndex, areaSample.primID,
                                 float3(0.0f, areaSample.uv.x, areaSample.uv.y),
                                 areaSample.point);
    positionPDF = 1.0f / (light.objectArea * stretch);
  } else {
    int faceIndex{light.faceDistr.indexSample(float(sampler))};
    // Sample uniformly over the triangle. The face CMF is proportional to
    // world-space area, so this is uniform over the instance as it stands
    // in the world, and `Scene::makeHit` reports the point in the same
    // space.
    float2 xi{sampler};
    float sqrtXi{std::sqrt(xi.x)};
    auto bary{float3(1.0f - sqrtXi, sqrtXi * (1.0f - xi.y), sqrtXi * xi.y)};
    hit = scene.makeHit(light.instIndex, uint32_t(faceIndex), bary);
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
  lightSample.pdf = selectPMF * distSq * positionPDF / cosTheta;
  lightSample.target = hit.point;
  lightSample.normal = hit.Ng;
  lightSample.hit = hit;
  return true;
}

Color LightSampler::reevaluateLi(const LightSample &lightSample,
                                 const smdl::State &state, const float3 &point,
                                 const float3 &incidencePoint) const {
  if (lightSample.isInfinite) return lightSample.Li;
  if (lightSample.analyticIndex != INVALID_INDEX) {
    if (lightSample.analyticIndex >= analyticLights.size()) return Color(0.0f);
    const auto &light{analyticLights[lightSample.analyticIndex]};
    return light.isDirac()
               ? light.Li(point, incidencePoint, state.meters_per_scene_unit)
               : light.Le(lightSample.target, incidencePoint);
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

float LightSampler::solidAnglePDF(uint32_t instIndex, const float3 &lightPoint,
                                  const float3 &lightNormal,
                                  const float3 &point) const {
  if (empty() || instIndex >= instanceToLight.size() ||
      instanceToLight[instIndex] == INVALID_INDEX) {
    return 0.0f;
  }
  auto lightIndex{instanceToLight[instIndex]};
  const auto &light{areaLights[lightIndex]};
  if (!light.isSampled) return 0.0f;
  const float selectPMF{mSelection.pmf(int(lightIndex), point)};
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
