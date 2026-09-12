#include <iostream>

#include "Render/Manifold.h"
#include "Scene/Primitive.h"

#include "smdl/Support/Logger.h"

namespace {
// A cap on the null-interface hops the projection casts skip.
constexpr int MAX_SKIPS{16};
} // namespace

ManifoldVertex vertexOf(const Hit &hit) {
  ManifoldVertex vertex;
  vertex.point = hit.point;
  vertex.surface = hit.instIndex;
  vertex.face = hit.faceIndex;
  vertex.coords = hit.bary;
  return vertex;
}

void hitOf(const Scene &scene, const ManifoldVertex &vertex, float time,
           Hit &hit) {
  scene.makeHit(uint32_t(vertex.surface), uint32_t(vertex.face), vertex.coords,
                time, hit);
}

bool SceneManifoldSurfaces::evaluateGeometry(
    const ManifoldVertex &vertex, smdl::ManifoldGeometry &geometry) const {
  const MeshInstance &instance{scene.meshInstances[vertex.surface]};
  if (instance.isCurves()) return false;
  // Only the remapped-normal hook needs the full hit record; the mesh
  // field goes straight to the fused derivation, skipping the shading
  // fields `makeHit()` computes that the walk never reads. This runs
  // once per vertex per Newton iteration, so it is the hot path of the
  // whole solver. 'canRemapNormal()' is conservative: unproven reads as
  // remapped, and the hook then reports the same field the mesh
  // carries, at the cost of the query; the mesh path is the fallback
  // either way, so a material without the hook solves the mesh field
  // as it always has.
  if (const smdl::JIT::MaterialDef *materialDef{
          scene.materialDefs[scene.materialIndexOf(instance)]};
      materialDef && materialDef->canRemapNormal()) {
    Hit hit{};
    hitOf(scene, vertex, time.fraction, hit);
    if (!hit.instance) return false;
    if (evaluateManifoldHookGeometry(scene, hit, geometry)) return true;
  }
  geometry =
      scene.manifoldGeometry(uint32_t(vertex.surface), uint32_t(vertex.face),
                             vertex.coords, time.fraction);
  return true;
}

// Re-anchor a Newton step onto the real surface: cast from the previous
// vertex (or the receiver) toward the stepped position and accept the
// first hit on the pinned vertex's own instance, passing through null
// interfaces. Anything else in the way fails the step, so a converged
// connection's segments are known to see their endpoints. A primitive
// hit must also land on the pinned vertex's own piece: a shape's pieces
// (a cylinder's side and caps) are distinct smooth surfaces, and letting
// a vertex hop between them mid-walk corrupts the iterate; a failed
// projection just halves the step instead. Mesh vertices slide across
// faces freely, since the faces tile one smooth surface.
bool SceneManifoldSurfaces::project(const ManifoldVertex &pin,
                                    const float3 &origin, const float3 &target,
                                    ManifoldVertex &moved) const {
  float3 dir{target - origin};
  if (!smdl::tryNormalize(dir)) return false;
  Ray ray{origin, dir, EPS, INF, time.fraction};
  for (int skip = 0; skip < MAX_SKIPS; skip++) {
    ManifoldHit hit;
    if (!scene.intersect(ray, hit)) return false;
    if (hit.vertex.surface == pin.surface && !hit.instance->isCurves()) {
      if (hit.instance->isPrimitive() && hit.vertex.face != pin.face)
        return false;
      moved = hit.vertex;
      return true;
    }
    if (!hit.materialDef->isNullInterface()) return false;
    ray = Ray{hit.vertex.point, dir, EPS, INF, time.fraction};
  }
  return false;
}

namespace {
// The hook's normal at a face parameter, in world space: build the
// shading state exactly as a render hit would, ask the material for
// `geometry.normal`, and carry the answer back out through the
// internal-to-object and object-to-world frames the state itself holds
// after finalization.
[[nodiscard]] bool hookNormalAt(const Scene &scene,
                                const smdl::JIT::MaterialDef &materialDef,
                                const Hit &seedHit, const float3 &bary,
                                float3 &normal) {
  Hit hit{};
  scene.makeHit(seedHit.instIndex, seedHit.faceIndex, bary, seedHit.time, hit);
  if (!hit.instance) return false;
  smdl::State state{makeRenderState(gRenderGrid.wavelengths)};
  hit.applyGeometryToState(state, float3());
  float3 internalNormal{};
  materialDef.geometryNormalEvaluate(state, internalNormal);
  const float3 objectNormal{
      transformDirection(state.tangentToObject, internalNormal)};
  normal = transformDirection(state.objectToWorld, objectNormal);
  return smdl::tryNormalize(normal);
}
} // namespace

bool evaluateManifoldHookGeometry(const Scene &scene, const Hit &hit,
                                  ManifoldGeometry &geometry) {
  const smdl::JIT::MaterialDef *materialDef{hit.materialDef};
  if (!materialDef || !materialDef->geometryNormalEvaluate ||
      hit.instance->isCurves())
    return false;
  geometry = scene.manifoldGeometry(hit);
  auto baryAt{[&](float du, float dv) {
    const float u{hit.bary[1] + du};
    const float v{hit.bary[2] + dv};
    return float3(1.0f - u - v, u, v);
  }};
  // The constraint solves against the normal itself; without it there
  // is nothing to substitute. The partials only steer the Newton step,
  // so a failed difference sample (a start pushed just off the surface
  // parameterization) degrades to a zero partial rather than failing
  // the whole query.
  float3 normal{};
  if (!hookNormalAt(scene, *materialDef, hit, baryAt(0.0f, 0.0f), normal))
    return false;
  geometry.normal = normal;
  geometry.dNdu = float3();
  geometry.dNdv = float3();
  const float span{std::max(length(geometry.dPdu), length(geometry.dPdv))};
  const float h{span > 0.0f ? std::clamp(MANIFOLD_NORMAL_STEP_WORLD / span,
                                         MANIFOLD_NORMAL_STEP_MIN,
                                         MANIFOLD_NORMAL_STEP_MAX)
                            : MANIFOLD_NORMAL_STEP_MAX};
  float3 nPu{}, nMu{}, nPv{}, nMv{};
  if (hookNormalAt(scene, *materialDef, hit, baryAt(+h, 0.0f), nPu) &&
      hookNormalAt(scene, *materialDef, hit, baryAt(-h, 0.0f), nMu)) {
    const float3 d{(nPu - nMu) / (2.0f * h)};
    geometry.dNdu = d - dot(d, normal) * normal;
  }
  if (hookNormalAt(scene, *materialDef, hit, baryAt(0.0f, +h), nPv) &&
      hookNormalAt(scene, *materialDef, hit, baryAt(0.0f, -h), nMv)) {
    const float3 d{(nPv - nMv) / (2.0f * h)};
    geometry.dNdv = d - dot(d, normal) * normal;
  }
  return true;
}

MNEECasterSet::MNEECasterSet(const Scene &scene, const Color &wavelengths,
                             float maxGlossyAlpha) {
  smdl::BumpPtrAllocator allocator{};
  for (uint32_t instIndex = 0; instIndex < scene.meshInstances.size();
       instIndex++) {
    const MeshInstance &instance{scene.meshInstances[instIndex]};
    if (!instance.isCausticCaster || instance.isCurves()) continue;
    const uint32_t matIndex{scene.materialIndexOf(instance)};
    const smdl::JIT::MaterialDef *materialDef{scene.materialDefs[matIndex]};
    if (!materialDef) continue;
    smdl::State state{makeRenderState(wavelengths, &allocator)};
    state.textureSpaceCount = 1;
    state.finalize();
    smdl::JIT::Material material{state, materialDef};
    // The transmission claim measures the index contrast against the
    // exterior the instance sits in; here that is the vacuum, which is
    // what an unplaced material sees, and the per-hit claim measures it
    // against the medium the path is actually in.
    material.setExteriorIOR(
        ExteriorIOR(nullptr, material, float3(0.0f, 0.0f, 1.0f)));
    // Either side of the instance: a reflective walk's starts land
    // wherever the caster faces, and the masked query at the converged
    // crossing settles which side actually scatters.
    const ManifoldClaim claim{
        manifoldClaim(material, /*isMarked=*/true, maxGlossyAlpha)};
    const int dfLobes{material.getLobes()};
    allocator.reset();
    if (claim.empty()) {
      const char *reason{") claims nothing: the material has no Dirac or "
                         "glossy lobe in either domain, so the mark is "
                         "ignored"};
      if ((dfLobes & smdl::DF_SETS_NORMAL) != 0)
        reason = ") claims nothing: a df node was given its own normal, "
                 "which the manifold walk cannot solve against; leave it "
                 "defaulted to inherit 'geometry.normal', else the mark is "
                 "ignored";
      else if (material.def->canRemapNormal() &&
               (dfLobes & smdl::DF_CAN_SET_NORMAL) != 0)
        reason = ") claims nothing: the material remaps 'geometry.normal' "
                 "while a df node was given a normal of its own, which "
                 "detaches it from the remapped field, so the mark is "
                 "ignored";
      else if (maxGlossyAlpha > 0.0f &&
               !manifoldClaim(material, /*isMarked=*/true).empty())
        reason = ") claims nothing under '-mnee-max-roughness': every "
                 "claimable lobe is wider, so the mark is ignored and the "
                 "transport stays with ordinary sampling";
      SMDL_LOG_WARN("'caster' on ",
                    smdl::QuotedPath(scene.fileNames[instIndex]), " (material ",
                    smdl::Quoted(scene.materialNames[matIndex]), reason);
      continue;
    }
    if (claim.reflectLobes == 0) continue;
    MNEECaster caster{};
    caster.instIndex = instIndex;
    caster.reflectLobes = claim.reflectLobes;
    if (instance.isPrimitive()) {
      caster.primitive = scene.primitives[instance.primIndex]->spec;
      caster.totalArea = scene.primitives[instance.primIndex]->objectArea;
    } else {
      const Mesh &mesh{*scene.meshes[instance.meshIndex]};
      std::vector<float> faceAreas{};
      faceAreas.reserve(mesh.faces.size());
      auto toWorld{[&](const float3 &point) {
        return transformPoint(instance.frame.objectToWorld, point);
      }};
      for (const auto &face : mesh.faces) {
        const float3 point0{toWorld(mesh.verts[face[0]].point)};
        const float3 point1{toWorld(mesh.verts[face[1]].point)};
        const float3 point2{toWorld(mesh.verts[face[2]].point)};
        const float area{triangleArea(point0, point1, point2)};
        faceAreas.push_back(area);
        caster.totalArea += area;
      }
      if (!(caster.totalArea > 0.0f)) continue;
      caster.faceDistr = smdl::Distribution1D(faceAreas);
    }
    casters.push_back(std::move(caster));
  }
}

const MNEECaster *MNEECasterSet::sampleCaster(Sampler &sampler,
                                              float &pdf) const {
  if (casters.empty()) return nullptr;
  const size_t which{std::min(size_t(float(sampler) * float(casters.size())),
                              casters.size() - 1)};
  pdf = 1.0f / float(casters.size());
  return &casters[which];
}

bool MNEECasterSet::samplePoint(const Scene &scene, Sampler &sampler,
                                const MNEECaster &caster, float time,
                                Hit &hit) const {
  // By area within the caster. This density is never divided out; see the
  // class comment.
  if (caster.primitive.isActive()) {
    const PrimitiveAreaSample sample{
        samplePrimitiveArea(caster.primitive, float2(sampler))};
    scene.makeHit(caster.instIndex, sample.primID,
                  float3(0.0f, sample.surface.uv.x, sample.surface.uv.y), time,
                  hit);
    return hit.instance != nullptr;
  }
  const int faceIndex{caster.faceDistr.indexSample(float(sampler))};
  scene.makeHit(caster.instIndex, uint32_t(faceIndex),
                smdl::uniformTriangleSample(float2(sampler)), time, hit);
  return hit.instance != nullptr;
}

bool makeManifoldSeed(const MediumStack *medium, smdl::JIT::Material &material,
                      const Hit &hit, const float3 &wl, float maxGlossyAlpha,
                      ManifoldVertexSeed &seed) {
  const float3 woStraight{-wl};
  material.setExteriorIOR(ExteriorIOR(medium, material, woStraight));
  // The side the straight segment arrives on, which is both the side
  // whose scattering tree the claim may speak for and the side whose
  // index is the previous one.
  const bool isPrevInterior{material.isInterior(woStraight)};
  const ManifoldClaim claim{manifoldClaim(
      material, isPrevInterior, hit.instance->isCausticCaster, maxGlossyAlpha)};
  if (claim.refractLobes == 0) return false;
  seed.claimedLobes = claim.refractLobes;
  seed.isGlossy = false;
  seed.vertex = vertexOf(hit);
  seed.etaPrev = isPrevInterior ? material.getIOR() : material.getExteriorIOR();
  seed.etaNext = isPrevInterior ? material.getExteriorIOR() : material.getIOR();
  seed.sideSign = dot(wl, hit.normal) < 0 ? -1.0f : 1.0f;
  return true;
}

int runMNEETestNormalHook(const Scene &scene) {
  constexpr int NUM_SAMPLES{64};
  constexpr float TOLERANCE{1e-3f};
  int failures{0};
  for (uint32_t instIndex = 0; instIndex < scene.meshInstances.size();
       instIndex++) {
    const MeshInstance &instance{scene.meshInstances[instIndex]};
    if (instance.isCurves()) continue;
    const uint32_t matIndex{scene.materialIndexOf(instance)};
    const smdl::JIT::MaterialDef *materialDef{scene.materialDefs[matIndex]};
    if (!materialDef || !materialDef->geometryNormalEvaluate) continue;
    const size_t faceCount{
        instance.isPrimitive()
            ? size_t(1)
            : scene.meshes[instance.meshIndex]->faces.size()};
    if (faceCount == 0) continue;
    const bool wasRemapped{materialDef->canRemapNormal()};
    float maxNormalDot{-1.0f};
    float minNormalDot{+1.0f};
    float maxPartialErr{0.0f};
    int samples{0};
    for (int k = 0; k < NUM_SAMPLES; k++) {
      // Deterministic low-discrepancy-ish points, interior to the face
      // parameterization so the central differences stay inside it.
      const uint32_t faceIndex{
          uint32_t((size_t(k) * 2654435761UL) % faceCount)};
      const float u{0.05f + 0.35f * std::fmod(0.618034f * float(k + 1), 1.0f)};
      const float v{0.05f + 0.35f * std::fmod(0.754878f * float(k + 2), 1.0f)};
      Hit hit{};
      scene.makeHit(instIndex, faceIndex, float3(1.0f - u - v, u, v), 0.0f,
                    hit);
      if (!hit.instance) continue;
      const ManifoldGeometry meshGeometry{scene.manifoldGeometry(hit)};
      ManifoldGeometry hookGeometry{};
      if (!evaluateManifoldHookGeometry(scene, hit, hookGeometry)) continue;
      samples++;
      const float normalDot{dot(meshGeometry.normal, hookGeometry.normal)};
      maxNormalDot = std::max(maxNormalDot, normalDot);
      minNormalDot = std::min(minNormalDot, normalDot);
      for (int axis = 0; axis < 2; axis++) {
        const float3 &a{axis == 0 ? meshGeometry.dNdu : meshGeometry.dNdv};
        const float3 &b{axis == 0 ? hookGeometry.dNdu : hookGeometry.dNdv};
        const float scale{std::max(length(a), length(b))};
        if (scale > 1e-4f)
          maxPartialErr = std::max(maxPartialErr, length(a - b) / scale);
      }
    }
    if (samples == 0) continue;
    if (wasRemapped) {
      std::cout << "  " << scene.fileNames[instIndex] << " (material "
                << scene.materialNames[matIndex]
                << "): remapped, max bend from mesh normal "
                << smdl::degrees(
                       std::acos(std::clamp(minNormalDot, -1.0f, 1.0f)))
                << " deg over " << samples << " samples\n";
      continue;
    }
    const bool isWithinTolerance{minNormalDot > 1.0f - TOLERANCE &&
                                 maxPartialErr <= TOLERANCE};
    if (!isWithinTolerance) failures++;
    std::cout << "  " << scene.fileNames[instIndex] << " (material "
              << scene.materialNames[matIndex]
              << "): " << (isWithinTolerance ? "OK" : "MISMATCH")
              << ", max relative dN error " << maxPartialErr << " over "
              << samples << " samples\n";
  }
  return failures;
}
