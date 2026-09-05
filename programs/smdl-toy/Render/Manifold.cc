#include "Render/Manifold.h"
#include "Scene/Primitive.h"

#include "smdl/Support/Logger.h"

// A cap on the null-interface hops the projection casts skip.
static constexpr int MAX_SKIPS{16};

ManifoldVertex vertexOf(const Hit &hit) {
  ManifoldVertex vertex{};
  vertex.point = hit.point;
  vertex.surface = hit.instIndex;
  vertex.face = hit.faceIndex;
  vertex.coords = hit.bary;
  return vertex;
}

Hit hitOf(const Scene &scene, const ManifoldVertex &vertex, float time) {
  return scene.makeHit(uint32_t(vertex.surface), uint32_t(vertex.face),
                       vertex.coords, time);
}

bool SceneManifoldSurfaces::geometry(const ManifoldVertex &vertex,
                                     smdl::ManifoldGeometry &geometry) const {
  const auto &instance{scene.meshInstances[vertex.surface]};
  if (instance.isCurves()) return false;
  // Only the remapped-normal hook needs the full hit record; the mesh
  // field goes straight to the fused derivation, skipping the shading
  // fields `makeHit()` computes that the walk never reads. This runs
  // once per vertex per Newton iteration, so it is the hot path of the
  // whole solver. 'remapsNormal()' is conservative: unproven reads as
  // remapped, and the hook then reports the same field the mesh
  // carries, at the cost of the query; the mesh path is the fallback
  // either way, so a material without the hook solves the mesh field
  // as it always has.
  if (const auto *material{scene.materials[scene.materialIndexOf(instance)]};
      material && material->remapsNormal()) {
    const Hit hit{hitOf(scene, vertex, time.fraction)};
    if (!hit.instance) return false;
    if (manifoldHookGeometry(scene, hit, geometry)) return true;
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
  auto dir{target - origin};
  if (!smdl::tryNormalize(dir)) return false;
  Ray ray{origin, dir, EPS, INF, time.fraction};
  for (int skip = 0; skip < MAX_SKIPS; skip++) {
    ManifoldHit hit{};
    if (!scene.intersect(ray, hit)) return false;
    if (hit.vertex.surface == pin.surface && !hit.instance->isCurves()) {
      if (hit.instance->isPrimitive() && hit.vertex.face != pin.face)
        return false;
      moved = hit.vertex;
      return true;
    }
    if (!hit.material->isNullInterface()) return false;
    ray = Ray{hit.vertex.point, dir, EPS, INF, time.fraction};
  }
  return false;
}

// The hook's normal at a face parameter, in world space: build the
// shading state exactly as a render hit would, ask the material for
// `geometry.normal`, and carry the answer back out through the
// internal-to-object and object-to-world frames the state itself holds
// after finalization.
[[nodiscard]] static bool hookNormalAt(const Scene &scene,
                                       const smdl::JIT::Material &material,
                                       const Hit &seedHit, const float3 &bary,
                                       float3 &normal) {
  const Hit hit{
      scene.makeHit(seedHit.instIndex, seedHit.faceIndex, bary, seedHit.time)};
  if (!hit.instance) return false;
  auto state{makeRenderState(renderWavelengths())};
  hit.applyGeometryToState(state, float3());
  auto internalNormal{float3()};
  material.geometryNormalEvaluate(state, internalNormal);
  const auto objectNormal{
      transformDirection(state.tangent_to_object_matrix, internalNormal)};
  normal = transformDirection(state.object_to_world_matrix, objectNormal);
  return smdl::tryNormalize(normal);
}

bool manifoldHookGeometry(const Scene &scene, const Hit &hit,
                          ManifoldGeometry &geometry) {
  const auto *material{hit.material};
  if (!material || !material->geometryNormalEvaluate ||
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
  if (!hookNormalAt(scene, *material, hit, baryAt(0.0f, 0.0f), normal))
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
  if (hookNormalAt(scene, *material, hit, baryAt(+h, 0.0f), nPu) &&
      hookNormalAt(scene, *material, hit, baryAt(-h, 0.0f), nMu)) {
    const float3 d{(nPu - nMu) / (2.0f * h)};
    geometry.dNdu = d - dot(d, normal) * normal;
  }
  if (hookNormalAt(scene, *material, hit, baryAt(0.0f, +h), nPv) &&
      hookNormalAt(scene, *material, hit, baryAt(0.0f, -h), nMv)) {
    const float3 d{(nPv - nMv) / (2.0f * h)};
    geometry.dNdv = d - dot(d, normal) * normal;
  }
  return true;
}

MNEECasterSet::MNEECasterSet(const Scene &scene, const Color &wavelengths,
                             float maxGlossyAlpha) {
  auto allocator{smdl::BumpPtrAllocator()};
  for (uint32_t instIndex = 0; instIndex < scene.meshInstances.size();
       instIndex++) {
    const auto &instance{scene.meshInstances[instIndex]};
    if (!instance.isCausticCaster || instance.isCurves()) continue;
    const auto matIndex{scene.materialIndexOf(instance)};
    const auto *material{scene.materials[matIndex]};
    if (!material) continue;
    auto state{makeRenderState(wavelengths, &allocator)};
    state.texture_space_max = 1;
    state.finalizeAndApplyInternalSpaceConventions();
    auto mat{smdl::JIT::MaterialInstance(state, material)};
    // The transmission claim measures the index contrast against the
    // exterior the instance sits in; here that is the vacuum, which is
    // what an unplaced material sees, and the per-hit claim measures it
    // against the medium the path is actually in.
    mat.setExteriorIOR(ExteriorIOR(nullptr, mat, float3(0.0f, 0.0f, 1.0f)));
    // Either side of the instance: a reflective walk's starts land
    // wherever the caster faces, and the masked query at the converged
    // crossing settles which side actually scatters.
    const auto claim{manifoldClaim(mat, /*marked=*/true, maxGlossyAlpha)};
    const int dfLobes{mat.getLobes()};
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
      else if (mat.material->remapsNormal() &&
               (dfLobes & smdl::DF_CAN_SET_NORMAL) != 0)
        reason = ") claims nothing: the material remaps 'geometry.normal' "
                 "while a df node was given a normal of its own, which "
                 "detaches it from the remapped field, so the mark is "
                 "ignored";
      else if (maxGlossyAlpha > 0.0f &&
               !manifoldClaim(mat, /*marked=*/true).empty())
        reason = ") claims nothing under '-mnee-max-roughness': every "
                 "claimable lobe is wider, so the mark is ignored and the "
                 "transport stays with ordinary sampling";
      SMDL_LOG_WARN("'caster' on ",
                    smdl::QuotedPath(scene.fileNames[instIndex]), " (material ",
                    smdl::Quoted(scene.materialNames[matIndex]), reason);
      continue;
    }
    if (claim.reflectLobes == 0) continue;
    auto caster{MNEECaster()};
    caster.instIndex = instIndex;
    caster.reflectLobes = claim.reflectLobes;
    if (instance.isPrimitive()) {
      caster.primitive = scene.primitives[instance.primIndex]->spec;
      caster.totalArea = scene.primitives[instance.primIndex]->objectArea;
    } else {
      const auto &mesh{*scene.meshes[instance.meshIndex]};
      auto faceAreas{std::vector<float>()};
      faceAreas.reserve(mesh.faces.size());
      auto toWorld{[&](const float3 &point) {
        return transformPoint(instance.frame.objectToWorld, point);
      }};
      for (const auto &face : mesh.faces) {
        const auto point0{toWorld(mesh.verts[face[0]].point)};
        const auto point1{toWorld(mesh.verts[face[1]].point)};
        const auto point2{toWorld(mesh.verts[face[2]].point)};
        const auto area{triangleArea(point0, point1, point2)};
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
  const auto which{std::min(size_t(float(sampler) * float(casters.size())),
                            casters.size() - 1)};
  pdf = 1.0f / float(casters.size());
  return &casters[which];
}

bool MNEECasterSet::samplePoint(const Scene &scene, Sampler &sampler,
                                const MNEECaster &caster, float time,
                                Hit &hit) const {
  // By area within the caster. This density is never divided out; see the
  // class comment.
  if (caster.primitive.active()) {
    const auto sample{samplePrimitiveArea(caster.primitive, float2(sampler))};
    hit = scene.makeHit(caster.instIndex, sample.primID,
                        float3(0.0f, sample.uv.x, sample.uv.y), time);
    return hit.instance != nullptr;
  }
  const auto faceIndex{caster.faceDistr.indexSample(float(sampler))};
  hit = scene.makeHit(caster.instIndex, uint32_t(faceIndex),
                      smdl::uniformTriangleSample(float2(sampler)), time);
  return hit.instance != nullptr;
}

bool makeManifoldSeed(const MediumStack *medium,
                      smdl::JIT::MaterialInstance &mat, const Hit &hit,
                      const float3 &wl, float maxGlossyAlpha,
                      ManifoldVertexSeed &seed) {
  const float3 woStraight{-wl};
  mat.setExteriorIOR(ExteriorIOR(medium, mat, woStraight));
  // The side the straight segment arrives on, which is both the side
  // whose scattering tree the claim may speak for and the side whose
  // index is the previous one.
  const bool prevInterior{mat.isInterior(woStraight)};
  const auto claim{manifoldClaim(
      mat, prevInterior, hit.instance->isCausticCaster, maxGlossyAlpha)};
  if (claim.refractLobes == 0) return false;
  seed.claimedLobes = claim.refractLobes;
  seed.isGlossy = false;
  seed.vertex = vertexOf(hit);
  seed.etaPrev = prevInterior ? mat.getIOR() : mat.getExteriorIOR();
  seed.etaNext = prevInterior ? mat.getExteriorIOR() : mat.getIOR();
  seed.sideSign = dot(wl, hit.normal) < 0 ? -1.0f : 1.0f;
  return true;
}
