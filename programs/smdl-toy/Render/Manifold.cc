#include "Render/Manifold.h"
#include "Scene/Primitive.h"

#include "smdl/Compiler.h"
#include "smdl/Support/Logger.h"

#include <iostream>

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

Hit hitOf(const Scene &scene, const ManifoldVertex &vertex) {
  return scene.makeHit(uint32_t(vertex.surface), uint32_t(vertex.face),
                       vertex.coords);
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
    const Hit hit{hitOf(scene, vertex)};
    if (!hit.instance) return false;
    if (manifoldHookGeometry(scene, hit, geometry)) return true;
  }
  geometry = scene.manifoldGeometry(uint32_t(vertex.surface),
                                    uint32_t(vertex.face), vertex.coords);
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
  Ray ray{origin, dir, EPS, INF};
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
    ray = Ray{hit.vertex.point, dir, EPS, INF};
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
  const Hit hit{scene.makeHit(seedHit.instIndex, seedHit.faceIndex, bary)};
  if (!hit.instance) return false;
  auto state{makeRenderState(renderWavelengths())};
  hit.applyGeometryToState(state, float3());
  auto internalNormal{float3()};
  material.geometryNormalEvaluate(state, internalNormal);
  const auto objectNormal{
      float3(state.tangent_to_object_matrix * float4(internalNormal, 0.0f))};
  normal = float3(state.object_to_world_matrix * float4(objectNormal, 0.0f));
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
    if (!instance.causticCaster || instance.isCurves()) continue;
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
    const auto claim{manifoldClaim(mat, /*marked=*/true, maxGlossyAlpha)};
    const int dfLobes{dfLobesOf(mat)};
    allocator.reset();
    if (claim.empty()) {
      const char *reason{") claims nothing: the material has no Dirac or "
                         "glossy lobe in either domain, so the mark is "
                         "ignored"};
      if ((dfLobes & smdl::JIT::DF_SETS_NORMAL) != 0)
        reason = ") claims nothing: a df node was given its own normal, "
                 "which the manifold walk cannot solve against; leave it "
                 "defaulted to inherit 'geometry.normal', else the mark is "
                 "ignored";
      else if (mat.material->remapsNormal() &&
               (dfLobes & smdl::JIT::DF_CAN_SET_NORMAL) != 0)
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
        return float3(instance.objectToWorld * float4(point, 1.0f));
      }};
      for (const auto &face : mesh.faces) {
        const auto point0{toWorld(mesh.verts[face[0]].point)};
        const auto point1{toWorld(mesh.verts[face[1]].point)};
        const auto point2{toWorld(mesh.verts[face[2]].point)};
        const auto area{0.5f * length(cross(point1 - point0, point2 - point0))};
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
                                const MNEECaster &caster, Hit &hit) const {
  // By area within the caster. This density is never divided out; see the
  // class comment.
  if (caster.primitive.active()) {
    const auto sample{samplePrimitiveArea(caster.primitive, float2(sampler))};
    hit = scene.makeHit(caster.instIndex, sample.primID,
                        float3(0.0f, sample.uv.x, sample.uv.y));
    return hit.instance != nullptr;
  }
  const auto faceIndex{caster.faceDistr.indexSample(float(sampler))};
  const float2 xi{sampler};
  const float sqrtXi{std::sqrt(xi.x)};
  hit = scene.makeHit(
      caster.instIndex, uint32_t(faceIndex),
      float3(1.0f - sqrtXi, sqrtXi * (1.0f - xi.y), sqrtXi * xi.y));
  return hit.instance != nullptr;
}

bool makeManifoldSeed(const MediumStack *medium,
                      smdl::JIT::MaterialInstance &mat, const Hit &hit,
                      const float3 &wl, float maxGlossyAlpha,
                      ManifoldVertexSeed &seed) {
  const float3 woStraight{-wl};
  mat.setExteriorIOR(ExteriorIOR(medium, mat, woStraight));
  const auto claim{
      manifoldClaim(mat, hit.instance->causticCaster, maxGlossyAlpha)};
  if (claim.refractLobes == 0) return false;
  seed.claimedLobes = claim.refractLobes;
  seed.isGlossy = false;
  const bool prevInterior{mat.isInterior(woStraight)};
  seed.vertex = vertexOf(hit);
  seed.etaPrev = prevInterior ? mat.getIOR() : mat.getExteriorIOR();
  seed.etaNext = prevInterior ? mat.getExteriorIOR() : mat.getIOR();
  seed.sideSign = dot(wl, hit.normal) < 0 ? -1.0f : 1.0f;
  return true;
}

// Flat mirrors and dielectric interfaces, on which the walk converges
// from anywhere and the measure has independent ground truth.
static const char *SELFTEST_MATERIALS{
    "#smdl\n"
    "import ::df::*;\n"
    "export material self_mirror() = material(\n"
    "  surface: material_surface(scattering:\n"
    "    df::specular_bsdf(mode: df::scatter_reflect)));\n"
    "export material self_glass() = material(\n"
    "  ior: 1.5,\n"
    "  surface: material_surface(scattering:\n"
    "    df::specular_bsdf(mode: df::scatter_reflect_transmit)));\n"};

// The first surface hit casting from `from` toward `toward`.
[[nodiscard]] static Hit castOnto(const Scene &scene, const float3 &from,
                                  const float3 &toward) {
  Ray ray{from, toward - from, EPS, INF};
  Hit hit{};
  if (!scene.intersect(ray, hit)) return Hit{};
  return hit;
}

namespace {

struct Solve final {
  bool ok{};
  float3 wr{};
  float measure{};
};

[[nodiscard]] Solve solveOnce(const SceneManifoldSurfaces &surfaces,
                              const float3 &receiver,
                              const ManifoldTarget &target,
                              const ManifoldChain &chain,
                              ManifoldWalkReport *report = nullptr) {
  ManifoldConnection connection{};
  if (!solveManifoldConnection(surfaces, receiver, target, chain, connection,
                               report))
    return {};
  return {true, connection.wr, connection.measure(chain)};
}

// One check: solve the chain for its target, difference the solved
// receiver direction over the light direction, and compare the numeric
// Jacobian against the connection measure; `analytic`, if nonnegative,
// is an additional closed-form value the measure must match. A finite
// target is perturbed on the plane through it perpendicular to the
// straight line at the straight distance, which is the unoriented
// convention the measure is expressed in.
[[nodiscard]] bool checkMeasure(const SceneManifoldSurfaces &surfaces,
                                const char *name, const float3 &receiver,
                                const ManifoldTarget &target,
                                const ManifoldChain &chain,
                                float analytic = -1.0f) {
  auto failWalk{[&](const char *what, const ManifoldWalkReport &walkReport) {
    std::cout << "  FAIL " << name << ": " << what << " (outcome "
              << int(walkReport.outcome) << ", failure "
              << int(walkReport.failure) << ", iterations "
              << walkReport.iterations << ", residual " << walkReport.residual
              << ")\n";
    return false;
  }};
  ManifoldWalkReport report{};
  const auto center{solveOnce(surfaces, receiver, target, chain, &report)};
  if (!center.ok) return failWalk("the walk did not converge", report);
  constexpr float STEP{2e-3f};
  constexpr float TOLERANCE{0.02f};
  const float3 a1{smdl::perpendicularTo(target.wl)};
  const float3 a2{cross(target.wl, a1)};
  float3 dwr[2]{};
  for (int k = 0; k < 2; k++) {
    const float3 &axis{k == 0 ? a1 : a2};
    float3 wrPlus{}, wrMinus{};
    for (int side = 0; side < 2; side++) {
      const float sign{side == 0 ? +1.0f : -1.0f};
      ManifoldTarget perturbed{target};
      if (target.infinite) {
        perturbed.wl = normalize(target.wl + sign * STEP * axis);
      } else {
        const float distStraight{length(target.point - receiver)};
        perturbed.point = target.point + sign * STEP * distStraight * axis;
        perturbed.wl = normalize(perturbed.point - receiver);
      }
      ManifoldWalkReport perturbedReport{};
      const auto solved{
          solveOnce(surfaces, receiver, perturbed, chain, &perturbedReport)};
      if (!solved.ok)
        return failWalk("a perturbed walk did not converge", perturbedReport);
      (side == 0 ? wrPlus : wrMinus) = solved.wr;
    }
    dwr[k] = (wrPlus - wrMinus) / (2.0f * STEP);
  }
  const float numeric{length(cross(dwr[0], dwr[1]))};
  const float scale{std::max(numeric, center.measure)};
  const float err{scale > 0.0f ? std::abs(numeric - center.measure) / scale
                               : 0.0f};
  bool ok{err <= TOLERANCE};
  float analyticErr{0.0f};
  if (analytic >= 0.0f) {
    analyticErr =
        std::abs(center.measure - analytic) / std::max(analytic, 1e-6f);
    ok = ok && analyticErr <= 1e-3f;
  }
  std::cout << "  " << (ok ? "ok   " : "FAIL ") << name << ": measure "
            << center.measure << ", finite differences " << numeric
            << " (rel err " << err << ")";
  if (analytic >= 0.0f)
    std::cout << ", analytic " << analytic << " (rel err " << analyticErr
              << ")";
  std::cout << "\n";
  return ok;
}

} // namespace

bool runManifoldWalkTest() {
  auto compiler{smdl::Compiler()};
  if (auto error{compiler.addCode("::selftest", SELFTEST_MATERIALS)}) {
    std::cout << "  FAIL setup: " << error->message << "\n";
    return false;
  }
  auto scene{Scene(compiler)};
  // A mirror disk at the origin and two stacked glass disks off to the
  // side, so the two families' casts never see each other; not too far,
  // because float resolution of the crossing positions is what sets the
  // residual floor the walks can reach.
  {
    LayoutItem mirror{};
    mirror.primitive.shape = PrimitiveSpec::Shape::DISK;
    mirror.primitive.radius = 20.0f;
    mirror.materials.all = "self_mirror";
    scene.add(mirror);
    LayoutItem glassA{mirror};
    glassA.materials.all = "self_glass";
    glassA.objectToWorld[3] = float4(50.0f, 0.0f, 0.0f, 1.0f);
    scene.add(glassA);
    LayoutItem glassB{glassA};
    glassB.objectToWorld[3] = float4(50.0f, 0.0f, -2.0f, 1.0f);
    scene.add(glassB);
  }
  if (auto error{compiler.compile(smdl::OPT_LEVEL_O2)}) {
    std::cout << "  FAIL setup: " << error->message << "\n";
    return false;
  }
  if (auto error{compiler.jitCompile()}) {
    std::cout << "  FAIL setup: " << error->message << "\n";
    return false;
  }
  auto gridSpec{std::vector<float>(16)};
  for (size_t i = 0; i < gridSpec.size(); i++)
    gridSpec[i] = 400.0f + 300.0f * float(i) / float(gridSpec.size() - 1);
  const auto wavelengths{
      Color(smdl::Span<const float>(gridSpec.data(), gridSpec.size()))};
  renderWavelengths() = wavelengths;
  scene.commit(wavelengths);
  const SceneManifoldSurfaces surfaces{scene};
  bool ok{true};
  // The flat mirror. For a distant light the reflected connection is the
  // mirrored light direction independent of the receiver, so the measure
  // is exactly 1; the finite light has no value this clean and rests on
  // the finite differences alone.
  {
    const float3 receiver{0.5f, -0.8f, 1.2f};
    ManifoldChain chain{};
    chain.count = 1;
    chain.residualTolerance = 1e-5f;
    auto &seed{chain.vertices[0]};
    const Hit mirrorHit{castOnto(scene, receiver, float3(0.3f, 0.2f, 0.0f))};
    if (!mirrorHit.instance) {
      std::cout << "  FAIL setup: no mirror hit\n";
      return false;
    }
    seed.vertex = vertexOf(mirrorHit);
    seed.etaPrev = seed.etaNext = 1.0f;
    seed.sideSign = 1.0f;
    seed.isReflect = true;
    {
      ManifoldTarget target{};
      target.wl = normalize(float3(-0.2f, 0.35f, 0.91f));
      ok &= checkMeasure(surfaces, "mirror, distant light", receiver, target,
                         chain, 1.0f);
    }
    {
      ManifoldTarget target{};
      target.point = float3(-1.3f, 0.9f, 2.1f);
      target.wl = normalize(target.point - receiver);
      target.infinite = false;
      ok &= checkMeasure(surfaces, "mirror, finite light", receiver, target,
                         chain);
    }
  }
  // One flat dielectric interface, the receiver on the dense side, like
  // a submerged surface looking up through still water.
  {
    const float3 receiver{50.2f, 0.3f, -1.0f};
    ManifoldChain chain{};
    chain.count = 1;
    chain.residualTolerance = 1e-5f;
    auto &seed{chain.vertices[0]};
    const Hit glassHit{castOnto(scene, receiver, float3(50.1f, 0.2f, 0.0f))};
    if (!glassHit.instance) {
      std::cout << "  FAIL setup: no glass hit\n";
      return false;
    }
    seed.vertex = vertexOf(glassHit);
    seed.etaPrev = 1.33f;
    seed.etaNext = 1.0f;
    seed.sideSign = 1.0f;
    {
      ManifoldTarget target{};
      target.wl = normalize(float3(0.25f, -0.15f, 0.96f));
      ok &= checkMeasure(surfaces, "refraction, distant light", receiver,
                         target, chain);
    }
    {
      ManifoldTarget target{};
      target.point = float3(49.4f, 1.1f, 3.0f);
      target.wl = normalize(target.point - receiver);
      target.infinite = false;
      ok &= checkMeasure(surfaces, "refraction, finite light", receiver, target,
                         chain);
    }
  }
  // Two stacked interfaces, the coupled system.
  {
    const float3 receiver{50.2f, 0.15f, -3.5f};
    ManifoldChain chain{};
    chain.count = 2;
    chain.residualTolerance = 1e-5f;
    auto &lower{chain.vertices[0]};
    auto &upper{chain.vertices[1]};
    const Hit lowerHit{castOnto(scene, receiver, receiver + float3(0, 0, 1))};
    Hit upperHit{};
    if (lowerHit.instance)
      upperHit = castOnto(scene, lowerHit.point + float3(0, 0, EPS),
                          lowerHit.point + float3(0, 0, 1));
    if (!lowerHit.instance || !upperHit.instance) {
      std::cout << "  FAIL setup: no stacked glass hits\n";
      return false;
    }
    lower.vertex = vertexOf(lowerHit);
    upper.vertex = vertexOf(upperHit);
    lower.etaPrev = 1.4f;
    lower.etaNext = 1.0f;
    lower.sideSign = 1.0f;
    upper.etaPrev = 1.0f;
    upper.etaNext = 1.6f;
    upper.sideSign = 1.0f;
    ManifoldTarget target{};
    target.wl = normalize(float3(-0.2f, 0.1f, 0.97f));
    ok &= checkMeasure(surfaces, "two refractions, distant light", receiver,
                       target, chain);
  }
  return ok;
}
