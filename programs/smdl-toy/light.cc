#include "light.h"

EnvLight::EnvLight(const std::string &filename, float scaleFactor)
    : scaleFactor(scaleFactor) {
  if (auto error{image.startLoad(filename)}) error->printAndExit();
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
  // mean radiance from the tabulated density and clamp at zero. The
  // balance between light and BSDF sampling is over-defensive where BSDF
  // sampling already covers, so light sampling should concentrate on the
  // above-average part of the image. Fall back to the uncompensated
  // weights if compensation removes everything (a constant environment).
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

Color EnvLight::Li(smdl::Compiler &compiler, const smdl::State &state,
                   float3 wi, float &pdf) const {
  Color Li{};
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

bool EnvLight::Le_sample(smdl::Compiler &compiler, const smdl::State &state,
                         const Scene &scene, float4 xi, Ray &ray, float &ppdf,
                         float &wpdf, Color &Le) const {
  ray.dir = -Li_sample(compiler, state, float2(xi.x, xi.y), wpdf, Le);
  if (wpdf == 0.0f) {
    return false;
  }
  ray.tmin = EPS;
  ray.tmax = INF;
  auto coords{smdl::coordinateSystem(ray.dir)};
  auto disk{smdl::uniformDiskSample(float2(xi.z, xi.w))};
  ray.org = scene.boundCenter +
            scene.boundRadius * (coords * float3(disk.x, disk.y, -1.0f));
  ppdf = 1.0f / (PI * scene.boundRadius * scene.boundRadius);
  return true;
}

LightSampler::LightSampler(smdl::Compiler &compiler, const Scene &scene,
                           const EnvLight *envLight, const Color &wavelengths)
    : compiler(compiler), scene(scene), envLight(envLight) {
  auto allocator{smdl::BumpPtrAllocator()};
  auto weights{std::vector<float>()};
  instanceToLight.resize(scene.meshInstances.size(), INVALID_INDEX);
  for (uint32_t instIndex = 0; instIndex < scene.meshInstances.size();
       instIndex++) {
    const auto &mesh{*scene.meshes[scene.meshInstances[instIndex].meshIndex]};
    const auto *material{scene.materials[mesh.materialIndex]};
    if (!material) continue;
    // Evaluate the material once with a placeholder state to read the
    // structural emission flags and a representative intensity. The flags
    // are decided by whether the emission EDF is non-default, so they do
    // not depend on the state; the intensity may be spatially varying, in
    // which case its value here is only a representative selection weight.
    smdl::State state{};
    state.allocator = &allocator;
    state.wavelength_base = wavelengths.data();
    state.wavelength_min = WAVELENGTH_MIN;
    state.wavelength_max = WAVELENGTH_MAX;
    state.texture_space_max = 1;
    state.finalizeAndApplyInternalSpaceConventions();
    auto materialInstance{smdl::JIT::MaterialInstance(state, material)};
    if (!materialInstance.hasEmission()) {
      allocator.reset();
      continue;
    }
    auto light{AreaLight()};
    light.meshInstanceIndex = instIndex;
    // NOTE: Areas are computed from the raw mesh vertices, which is the
    // same convention `Scene::intersect` reports hit points in — instance
    // transforms are assumed to be identity throughout the renderer.
    auto faceAreas{std::vector<float>()};
    faceAreas.reserve(mesh.faces.size());
    for (const auto &face : mesh.faces) {
      const auto &point0{mesh.verts[face[0]].point};
      const auto &point1{mesh.verts[face[1]].point};
      const auto &point2{mesh.verts[face[2]].point};
      auto area{0.5f * length(cross(point1 - point0, point2 - point0))};
      faceAreas.push_back(area);
      light.totalArea += area;
    }
    if (!(light.totalArea > 0)) {
      allocator.reset();
      continue;
    }
    light.faceDistr = smdl::Distribution1D(faceAreas);
    // The selection weight is the power: intensity times area under
    // `intensity_radiant_exitance`, the intensity itself under
    // `intensity_power`.
    auto average{[](smdl::Span<const float> values) {
      float sum{};
      for (float value : values) sum += value;
      return values.empty() ? 0.0f : sum / values.size();
    }};
    float weight{};
    if (float intensity{
            average(materialInstance.getSurfaceEmissionIntensity())};
        intensity > 0)
      weight += materialInstance.isSurfaceEmissionPower()
                    ? intensity
                    : intensity * light.totalArea;
    if (float intensity{
            average(materialInstance.getBackfaceEmissionIntensity())};
        intensity > 0)
      weight += materialInstance.isBackfaceEmissionPower()
                    ? intensity
                    : intensity * light.totalArea;
    instanceToLight[instIndex] = uint32_t(areaLights.size());
    areaLights.push_back(std::move(light));
    weights.push_back(weight);
    allocator.reset();
  }
  if (envLight) {
    // Treat the environment as shining on a disk of the scene radius.
    float radius{std::max(scene.boundRadius, 1.0f)};
    weights.push_back(envLight->averageRadiance() * PI * radius * radius);
  }
  if (!weights.empty()) lightDistr = smdl::Distribution1D(weights);
}

bool LightSampler::sample(smdl::State state, Sampler &sampler,
                          const float3 &point, LightSample &lightSample) const {
  if (empty()) return false;
  float selectPMF{};
  int lightIndex{lightDistr.indexSample(float(sampler), nullptr, &selectPMF)};
  if (!(selectPMF > 0)) return false;
  if (envLight && lightIndex == int(areaLights.size())) {
    float dirPDF{};
    lightSample.wi = envLight->Li_sample(compiler, state, float2(sampler),
                                         dirPDF, lightSample.Li);
    if (!(dirPDF > 0)) return false;
    lightSample.pdf = selectPMF * dirPDF;
    lightSample.target = point + 2.0f * scene.boundRadius * lightSample.wi;
    return true;
  }
  const auto &light{areaLights[lightIndex]};
  const auto &meshInstance{scene.meshInstances[light.meshInstanceIndex]};
  const auto &mesh{*scene.meshes[meshInstance.meshIndex]};
  int faceIndex{light.faceDistr.indexSample(float(sampler))};
  const auto &face{mesh.faces[faceIndex]};
  const auto &vert0{mesh.verts[face[0]]};
  const auto &vert1{mesh.verts[face[1]]};
  const auto &vert2{mesh.verts[face[2]]};
  // Sample uniformly over the triangle.
  float2 xi{sampler};
  float sqrtXi{std::sqrt(xi.x)};
  auto bary{float3(1.0f - sqrtXi, sqrtXi * (1.0f - xi.y), sqrtXi * xi.y)};
  auto barycentric{[&](auto member) {
    return bary[0] * vert0.*member + //
           bary[1] * vert1.*member + //
           bary[2] * vert2.*member;
  }};
  auto edge1{normalize(vert1.point - vert0.point)};
  auto edge2{normalize(vert2.point - vert0.point)};
  Hit hit{};
  hit.meshInstanceIndex = light.meshInstanceIndex;
  hit.meshIndex = meshInstance.meshIndex;
  hit.faceIndex = uint32_t(faceIndex);
  hit.materialIndex = mesh.materialIndex;
  hit.material = scene.materials[mesh.materialIndex];
  hit.bary = bary;
  hit.point = barycentric(&Mesh::Vert::point);
  hit.normal = normalize(barycentric(&Mesh::Vert::normal));
  hit.tangent = normalize(barycentric(&Mesh::Vert::tangent));
  hit.geometryNormal = normalize(cross(edge1, edge2));
  hit.geometryTangent = edge1;
  hit.texcoord = barycentric(&Mesh::Vert::texcoord);
  hit.objectToWorld = meshInstance.objectToWorld;
  auto direction{hit.point - point};
  float distSq{lengthSquared(direction)};
  if (!(distSq > 0)) return false;
  lightSample.wi = normalize(direction);
  float cosTheta{absDot(hit.geometryNormal, lightSample.wi)};
  if (!(cosTheta > 0)) return false;
  hit.apply_geometry_to_state(state);
  auto materialInstance{smdl::JIT::MaterialInstance(state, hit.material)};
  if (!emittedRadiance(materialInstance, light.meshInstanceIndex,
                       -lightSample.wi, lightSample.Li)) {
    return false;
  }
  // The face CMF is proportional to area, so the position density is
  // uniform over the total area; convert to solid angle at the receiver.
  lightSample.pdf = selectPMF * distSq / (cosTheta * light.totalArea);
  lightSample.target = hit.point;
  return true;
}

bool LightSampler::emittedRadiance(
    const smdl::JIT::MaterialInstance &materialInstance,
    uint32_t meshInstanceIndex, const float3 &wi, Color &Le) const {
  float edfPDF{};
  if (!materialInstance.emissionEvaluate(wi, edfPDF, Le)) return false;
  // `intensity_power` leaves the host to divide by the total emitting
  // surface area; see `JIT::Material::emissionEvaluate`. The mode is per
  // side, so pick the side actually emitting toward `wi`.
  bool isPower{materialInstance.isExterior(wi)
                   ? materialInstance.isSurfaceEmissionPower()
               : materialInstance.hasBackfaceEmission()
                   ? materialInstance.isBackfaceEmissionPower()
                   : materialInstance.isSurfaceEmissionPower()};
  if (isPower) {
    float area{1.0f};
    if (meshInstanceIndex < instanceToLight.size() &&
        instanceToLight[meshInstanceIndex] != INVALID_INDEX) {
      area = areaLights[instanceToLight[meshInstanceIndex]].totalArea;
    }
    Le = Le / area;
  }
  return true;
}

float LightSampler::solidAnglePDF(uint32_t meshInstanceIndex,
                                  const float3 &lightPoint,
                                  const float3 &lightNormal,
                                  const float3 &point) const {
  if (empty() || meshInstanceIndex >= instanceToLight.size() ||
      instanceToLight[meshInstanceIndex] == INVALID_INDEX) {
    return 0.0f;
  }
  auto lightIndex{instanceToLight[meshInstanceIndex]};
  const auto &light{areaLights[lightIndex]};
  float selectPMF{lightDistr.indexPMF(int(lightIndex))};
  auto direction{lightPoint - point};
  float distSq{lengthSquared(direction)};
  if (!(distSq > 0)) return 0.0f;
  float cosTheta{absDot(lightNormal, normalize(direction))};
  if (!(cosTheta > 0)) return 0.0f;
  return selectPMF * distSq / (cosTheta * light.totalArea);
}
