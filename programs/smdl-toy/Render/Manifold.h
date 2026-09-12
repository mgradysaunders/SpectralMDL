/// \file
/// The renderer's half of the manifold estimators: the solver itself
/// lives in the library (`smdl/Manifold.h`), and this is the glue that
/// binds it to this renderer's world. `SceneManifoldSurfaces` answers
/// the solver's projection casts and differential-geometry queries over
/// the `Scene` (Embree casts, mesh interpolation, and the
/// geometry-normal hook for remapped materials); the caster set,
/// seeding, and the transport-side work (Fresnel, medium attenuation,
/// MIS and reciprocal-probability bookkeeping) stay with the path
/// tracer.
#pragma once

#include <vector>

#include "Layout/Layout.h"
#include "Render/Medium.h"
#include "Render/PathStats.h"
#include "Render/Sampler.h"
#include "smdl/Manifold.h"
#include "smdl/RenderUtil/MonteCarlo.h"

struct RenderContext;
struct PathContext;
class VisibilityWalk;

// The solver types keep their unqualified spellings here; the solver
// itself is the library's.
using smdl::buildManifoldSeedFrame;
using smdl::isManifoldReceiver;
using smdl::isSameManifoldSolution;
using smdl::MANIFOLD_IDENTITY_FRACTION;
using smdl::MANIFOLD_MAX_DEPTH;
using smdl::MANIFOLD_MAX_TRIALS;
using smdl::MANIFOLD_RECIPROCAL_RESIDUAL;
using smdl::MANIFOLD_SOLUTION_IDENTITY_FRACTION;
using smdl::ManifoldChain;
using smdl::ManifoldClaim;
using smdl::manifoldClaim;
using smdl::ManifoldConnection;
using smdl::ManifoldConnectionVertex;
using smdl::manifoldFrameSeed;
using smdl::manifoldReciprocal;
using smdl::ManifoldSurfaces;
using smdl::ManifoldTarget;
using smdl::ManifoldVertex;
using smdl::ManifoldVertexSeed;
using smdl::ManifoldWalkReport;
using smdl::solveManifoldConnection;

/// How far a jittered start may sit from the straight-line crossing, as a
/// fraction of the distance from the receiver.
///
/// This is the reach of the straight-line search: a solution further from
/// the straight segment than this is never found by it, and unless a
/// marked caster's search reaches it (`traceManifoldCasterSeed()`, which
/// samples the whole caster and has no reach limit) its transport is
/// lost. Larger costs convergence, since the walk starts further from
/// every solution.
constexpr float MNEE_STRAIGHT_SEED_JITTER{0.60f};

/// The surface hits one straight receiver-to-light line may resolve and
/// still carry transport the manifold estimators claim. Both halves of
/// the estimator discover that line through `discoverStraightChain()`,
/// so the budget is one decision: past it the gather stands down and
/// the arrival keeps the ordinary weight, the way every other
/// undiscovered chain does.
constexpr int MNEE_STRAIGHT_MAX_HOPS{64};

/// How precisely a glossy chain's walk must match its drawn microfacet
/// normals, as a fraction of the narrowest lobe's squared roughness, so
/// that the distribution the estimate evaluates at the converged half
/// vector agrees with the density of the drawn one; see
/// `ManifoldChain::residualTolerance`.
constexpr float MANIFOLD_GLOSSY_RESIDUAL_FRACTION{0.05f};

/// The central-difference step for differentiating a remapped shading
/// normal through the geometry-normal hook. The step is chosen per
/// face: it aims for `MANIFOLD_NORMAL_STEP_WORLD` scene units, because
/// the field's own scale is a world length that owes nothing to the
/// tessellation of the flat geometry underneath (a step proportional to
/// the face sampled the pool's wave field 8 mm apart on the
/// unsubdivided box top, and the underestimated derivatives read 5
/// percent dark against path tracing), and it is clamped in
/// face-parameter units so a fine tessellation keeps a step that
/// resolves its own faces and float noise in the differenced normals
/// stays small against the derivative.
constexpr float MANIFOLD_NORMAL_STEP_WORLD{1e-3f};
constexpr float MANIFOLD_NORMAL_STEP_MIN{1e-5f};
constexpr float MANIFOLD_NORMAL_STEP_MAX{4e-3f};

/// The solver's vertex handle for a hit: the point, and the addressing
/// (`instIndex`, `faceIndex`, barycentrics) `hitOf()` rebuilds the hit
/// record from.
[[nodiscard]] ManifoldVertex vertexOf(const Hit &hit);

/// The hit record a solver vertex stands for, rebuilt through
/// `Scene::makeHit()` at the shutter fraction `time` into `hit`.
void hitOf(const Scene &scene, const ManifoldVertex &vertex, float time,
           Hit &hit);

/// The scene as the manifold solver's surfaces: projection casts pass
/// through null interfaces and pin to the vertex's own instance (and
/// piece, for a primitive), and the differential geometry is the mesh
/// field through the fused `Scene::manifoldGeometry()` derivation, or
/// the remapped field read through the geometry-normal hook when the
/// material remaps `geometry.normal`. Every consumer of the walk's
/// normal goes through `geometry()`, so the constraint, its Jacobian,
/// the offset frames, and the arrival-side transfer all solve against
/// the same field.
class SceneManifoldSurfaces final : public ManifoldSurfaces {
public:
  /// `time` is the path's: its fraction is what the projection casts
  /// trace at.
  SceneManifoldSurfaces(const Scene &scene, PathTime time) noexcept
      : scene(scene), time(time) {}

  [[nodiscard]] bool
  evaluateGeometry(const ManifoldVertex &vertex,
                   smdl::ManifoldGeometry &geometry) const override;

  [[nodiscard]] bool project(const ManifoldVertex &pin, const float3 &origin,
                             const float3 &target,
                             ManifoldVertex &moved) const override;

  const Scene &scene;
  const PathTime time;
};

/// The differential shading geometry read through the material's
/// geometry-normal hook, unconditionally: the normal from the hook at
/// the hit, `dNdu` and `dNdv` by central differences of the hook over
/// the surface parameterization with the per-face step the
/// `MANIFOLD_NORMAL_STEP_WORLD` target picks, and
/// the positions and position partials from the mesh unchanged. False
/// when the hook was not compiled or the hook's normal is degenerate.
/// `SceneManifoldSurfaces::evaluateGeometry()` is the caller; this is exposed
/// on its own so a host check can difference an unmapped material's
/// field against the analytic mesh geometry.
[[nodiscard]] bool evaluateManifoldHookGeometry(const Scene &scene,
                                                const Hit &hit,
                                                ManifoldGeometry &geometry);

/// One instance a caster connection may bounce off or pass through: a
/// marked mesh or shape, with the lobes it claims by domain and the
/// area-weighted face distribution (meshes) a start is drawn from.
class MNEECaster final {
public:
  uint32_t instIndex{INVALID_INDEX};
  /// `DF_DIRAC_BRDF` and or `DF_GLOSSY_BRDF`: one reflective estimate
  /// per lobe.
  int reflectLobes{};
  /// `DF_DIRAC_BTDF` and or `DF_GLOSSY_BTDF`: one refractive estimate
  /// per lobe, on a chain traced through the caster from a sampled
  /// point on it.
  int refractLobes{};
  /// The shape, when the caster is a primitive; starts are then drawn by
  /// `samplePrimitiveArea()` and the projection pins the walk to the piece.
  PrimitiveSpec primitive{};
  smdl::Distribution1D faceDistr{};
  float totalArea{};
  /// The center and squared half diagonal of the caster's world box,
  /// what `MNEECasterSet::weight()` weighs the caster by from a
  /// receiver. A moving caster's box covers both keys, a proxy for
  /// where it can be; nothing but the weights read it, and every
  /// caster keeps a positive weight, so a loose box costs variance and
  /// never energy.
  float3 boundCenter{};
  float boundRadiusSq{};
};

/// Every marked instance the caster gathers sample.
///
/// This is what the caster gathers sample in place of the straight
/// shadow segment the straight-line refractive gather is handed. A
/// mirror is nowhere near the line from the receiver to the light, so
/// there is no crossing to seed from and the surface has to be sampled
/// instead; a prism's refracted caustic lands beside its shadow, where
/// the straight line crosses nothing, and the same sampling seeds the
/// first crossing of a chain that `traceManifoldCasterSeed()` then
/// refracts through the rest of the caster.
///
/// An estimate is made on ONE caster, drawn by `sampleCaster()` with a
/// probability the estimate divides out, and every start of that estimate
/// is drawn on it by `samplePoint()`. The draw weighs each caster by its
/// solid angle from the receiver, near enough, so that a receiver in
/// one mirror's patch draws that mirror rather than one of the N others
/// the scene has, which is where the caster count would otherwise cost
/// variance. The solid angle is a proxy and not the contribution: a
/// flat mirror's is the lamp's image whatever the mirror's size, a
/// convex caster demagnifies it and a concave one magnifies it, and
/// whether the caster can bend the one light the gather also drew
/// toward the receiver is not asked at all (a flat mirror serves one
/// patch of the floor per lamp). A mixture with the uniform draw was
/// weighed and rejected: it halves the loss where the proxy is wrong
/// and halves the win where it is right. The start density never enters the
/// estimator: the reciprocal estimate asks how often a fresh start reaches
/// the same solution, which already accounts for however the starts are
/// distributed, so all that has to hold is that every start of one
/// estimate is drawn the same way and can reach the solutions that matter.
/// Mixing casters within an estimate breaks that: a start on another
/// instance can never re-find a solution on this one, and the two may not
/// even share a material.
///
/// Membership is decided once here, against a placeholder state; whether
/// a given crossing is admitted is `makeManifoldSeed()`'s per-hit answer.
/// The gather and the arrival side ask the two questions the same way,
/// membership through `casterOf()` and admission through the seed, so a
/// caster whose claim differs between the two is straight-only on both.
class MNEECasterSet final {
public:
  MNEECasterSet() = default;

  /// Enumerate the scene's marked instances, evaluating each instance's
  /// material once against a placeholder state exactly as the light
  /// sampler does for emission, and keeping those with a claim in
  /// either domain. A marked instance whose material claims nothing in
  /// either is reported and ignored: the mark is judgment, and the one
  /// way to misapply it is to mark something that cannot focus light.
  MNEECasterSet(const Scene &scene, const Color &wavelengths);

  [[nodiscard]] bool empty() const noexcept { return casters.empty(); }

  /// Draw the caster an estimate is made on from the receiver at
  /// `point`, in proportion to `weight()`, and the probability of
  /// having drawn it. Null when there is none. One draw is consumed
  /// whatever the count. The probability is never recomputed anywhere
  /// else: a caster estimate is claimed outright, so no arrival weighs
  /// against it, which is what lets the draw depend on the receiver at
  /// all.
  [[nodiscard]] const MNEECaster *
  sampleCaster(Sampler &sampler, const float3 &point, float &pdf) const;

  /// The weight `sampleCaster()` draws the caster by from `point`: its
  /// area over the mean squared distance to it, the rule light
  /// selection weighs a cluster by (`LightTree::importance()`), which
  /// stays finite for a receiver inside the bound.
  [[nodiscard]] static float weight(const MNEECaster &caster,
                                    const float3 &point) noexcept;

  /// The caster the instance is, or null when it is unmarked or claims
  /// nothing: the membership question both halves of the caster
  /// refractive estimator ask.
  [[nodiscard]] const MNEECaster *casterOf(uint32_t instIndex) const noexcept {
    if (instIndex >= mCasterOfInstance.size()) return nullptr;
    const uint32_t which{mCasterOfInstance[instIndex]};
    return which == INVALID_INDEX ? nullptr : &casters[which];
  }

  /// Draw a start on a caster: a face by area and a uniform point on it,
  /// the hit built at the shutter fraction `time`. Returns false when the
  /// hit cannot be made.
  [[nodiscard]] static bool samplePoint(const Scene &scene, Sampler &sampler,
                                        const MNEECaster &caster, float time,
                                        Hit &hit);

  std::vector<MNEECaster> casters{};

private:
  /// The index in `casters` of each scene instance, `INVALID_INDEX` for
  /// one that is not a caster.
  std::vector<uint32_t> mCasterOfInstance{};
};

/// The distinct solutions one biased clustered estimate has found.
///
/// The biased claimed mode runs a fixed number of walks per estimate
/// rather than the reciprocal one, so the same solution is reached
/// several times and has to be summed once. That rule lives here so
/// that the two estimates running it, the refractive Dirac chain and
/// the biased branch of the reciprocal estimators, cannot drift apart
/// on the cluster cap or on what happens past it.
class ManifoldSolutionSet final {
public:
  /// Count `connection` unless it is a re-find of one already counted,
  /// valuing a genuinely new solution with `value` and adding that to
  /// the sum, and tallying it into `stats` when there is one.
  ///
  /// A distinct solution past the cap is dropped rather than summed
  /// unclustered, so a re-find can never double-count; a surface with
  /// more solutions in reach than the cap needs the walk count raised
  /// far past it anyway.
  template <typename Value>
  void consider(const float3 &receiver, const ManifoldConnection &connection,
                const Value &value, MNEEStats *stats) {
    for (int i = 0; i < mCount; i++)
      if (isSameManifoldSolution(receiver, mSolutions[i], connection)) return;
    if (mCount == MAX_SOLUTIONS) return;
    mSolutions[mCount++].set(connection);
    const Color contribution{value(connection)};
    if (stats) stats->recordContribution(!contribution.isAllZero());
    mSum += contribution;
  }

  /// The sum over the distinct solutions.
  [[nodiscard]] const Color &sum() const noexcept { return mSum; }

private:
  /// The most distinct solutions one estimate clusters.
  static constexpr int MAX_SOLUTIONS{32};

  /// The keys of the solutions counted so far, which is all a re-find
  /// is told apart by; scratch below `mCount`, indeterminate past it.
  std::array<smdl::ManifoldSolutionKey, MAX_SOLUTIONS> mSolutions;
  int mCount{};
  Color mSum{};
};

/// Seed one chain vertex from an interface the straight segment
/// crosses: resolve the instance's exterior IOR against `medium`, admit
/// the interface only if `manifoldClaim()` claims a transmission lobe
/// there, record which lobes in `claimedLobes`, and fill in the
/// per-side indices and the side of the shading normal the segment
/// arrived from.
///
/// Every discovery goes through here: the straight-line one both halves
/// of the estimator run (`discoverStraightChain()`) and the caster
/// trace. The seeds of the two halves must agree vertex for vertex or
/// the two MIS weights stop summing to one, so the eligibility test and
/// the index assignment deliberately have exactly one implementation.
///
/// `wl` is the direction of travel along the straight segment, toward
/// the light. `material` is modified in place by the exterior IOR
/// resolution.
[[nodiscard]] bool makeManifoldSeed(const MediumStack *medium,
                                    smdl::JIT::Material &material,
                                    const Hit &hit, const float3 &wl,
                                    ManifoldVertexSeed &seed);

/// The receiver a connection leaves from, as both halves of the
/// estimator see it: the vertex, the direction that arrived there, the
/// nested medium it sits in, and the interface that scatters there,
/// null at a volume vertex, which has none.
class MNEEReceiver final {
public:
  /// The nested medium a segment leaving the receiver toward `wi` starts
  /// in: the receiver's own where the direction stays on the side the
  /// path arrived from, and across the receiver's interface where it
  /// transmits, exactly as the walk's own continuation crosses it at
  /// the bounce. A segment started in the receiver's own medium
  /// regardless would miss the interior of a rough-glass receiver a
  /// connection transmits into.
  [[nodiscard]] const MediumStack *
  mediumToward(smdl::BumpPtrAllocator &allocator, const float3 &wi) const {
    const MediumStack *result{medium};
    if (material)
      MediumStack::Update(result, allocator, material, instance, wo, wi);
    return result;
  }

  float3 point{};

  /// The direction back along the segment that arrived at the receiver.
  float3 wo{};

  const MediumStack *medium{};

  const smdl::JIT::Material *material{};

  const MeshInstance *instance{};
};

/// A chain family: the crossings in order from the receiver, by instance
/// and, for a primitive, by piece. Two chains of one family cross the
/// same surfaces in the same order, which is what the caster
/// refractive estimator and the straight-line one are partitioned by:
/// the straight-line gather owns the family its straight segment
/// discovers, and the caster gather every other family that starts on
/// its caster. Both halves of the estimator compare families the same
/// way, so the partition is the same on both sides.
class MNEEChainFamily final {
public:
  void append(const Hit &hit) noexcept {
    if (count < MANIFOLD_MAX_DEPTH) {
      instances[count] = hit.instIndex;
      pieces[count] =
          hit.instance->isPrimitive() ? hit.faceIndex : INVALID_INDEX;
    }
    count++;
  }

  [[nodiscard]] bool operator==(const MNEEChainFamily &other) const noexcept {
    if (count != other.count) return false;
    for (int i = 0; i < std::min(count, MANIFOLD_MAX_DEPTH); i++)
      if (instances[i] != other.instances[i] || pieces[i] != other.pieces[i])
        return false;
    return true;
  }

  [[nodiscard]] bool operator!=(const MNEEChainFamily &other) const noexcept {
    return !(*this == other);
  }

  /// The crossings, which keeps counting past `MANIFOLD_MAX_DEPTH` so an
  /// overlong chain never reads as a shorter one; zero for no chain.
  int count{};
  std::array<uint32_t, MANIFOLD_MAX_DEPTH> instances{};
  std::array<uint32_t, MANIFOLD_MAX_DEPTH> pieces{};
};

/// A seed chain, as a discovery hands it to the estimators: the chain,
/// and per crossing the discovery hit and the receiver-side medium,
/// which the glossy kind draws its offsets at.
class MNEEChainSeed final {
public:
  [[nodiscard]] MNEEChainFamily family() const noexcept {
    MNEEChainFamily result{};
    for (int i = 0; i < chain.count; i++) result.append(hits[i]);
    return result;
  }

  ManifoldChain chain{};
  std::array<Hit, MANIFOLD_MAX_DEPTH> hits{};
  std::array<const MediumStack *, MANIFOLD_MAX_DEPTH> medium{};

  /// The transmission lobes every crossing of the chain claims, of those
  /// the discovery was asked for.
  int lobes{};
};

/// How a straight-line discovery ended. Only `REACHED` yields an
/// estimate; the rest are the one list of reasons the gather stands
/// down and the arrival side keeps its ordinary weight, decided in one
/// place for both.
enum class MNEEStraightEnd {
  /// Every surface on the line was an admitted crossing, and the line
  /// reached the light.
  REACHED,
  /// A surface no chain of the wanted kinds crosses: not an interface
  /// the claim admits, a curve, or a crossing claiming none of the kinds
  /// the chain so far claims (a mixed chain is nobody's).
  BLOCKED,
  /// More crossings than the depth allows.
  OVERLONG,
  /// More surface hits than `MNEE_STRAIGHT_MAX_HOPS`.
  BUDGET,
  /// The line passed a cutout on a draw of the walk's own, which the
  /// arrival side cannot replay.
  CUTOUT,
};

/// Discover the chain along one straight receiver-to-light line, from
/// the first blocker `walk` returned in `blocker` (the caller's scratch,
/// which the discovery keeps walking in): admit each blocker as a
/// crossing through `makeManifoldSeed()`, narrowing `wantedLobes` to
/// what every crossing claims, pass through it, and continue to the next
/// until the line reaches the light or something ends it. `wl` is the
/// direction of travel toward the light; `maxDepth` is the estimator's
/// depth.
///
/// This is the one discovery both halves of the Dirac pair run, the
/// gather to seed its estimate and the arrival side to prove the chain
/// covered, so the two resolve the same hits with the same epsilons, the
/// same hops, the same endpoint (an infinite target's line runs to
/// infinity) and the same budget by construction rather than by twin
/// predicates.
[[nodiscard]] MNEEStraightEnd
discoverStraightChain(PathContext &path, VisibilityWalk &walk, Hit &blocker,
                      const float3 &wl, int wantedLobes, int maxDepth,
                      MNEEChainSeed &seed);

/// Trace the start of a caster refractive estimate: draw a point on
/// `caster` by area, walk to it from the receiver and take the caster's
/// first crossing on the way (the near face where the far one was
/// drawn), then refract by Snell's law about the shading normal with
/// the seed's own indices, cross the medium stack, and walk on,
/// admitting every interface `makeManifoldSeed()` admits, until the
/// walk meets something that is not one (the floor, an emitter, a
/// curve), escapes, or the chain reaches `maxDepth`. The chain's last
/// crossing is what the solve connects to the light. Every walk is a
/// `VisibilityWalk`, so what it passes through is what every shadow
/// segment passes through, a partial cutout included, by a draw that
/// only ever moves the start; anything else before the caster fails the
/// trace, as does a mixed chain (the straight-line gather refuses those
/// too), total internal reflection, or a first crossing the seed
/// refuses.
///
/// The refracted direction is only a start: the solve enforces the
/// constraint exactly and the transport is asked of the material at the
/// converged crossing, which is why the trace uses Snell's law and the
/// seed's indices rather than the material's own Dirac sample, and so
/// serves a rough-glass caster that has no Dirac lobe to draw.
///
/// The reach of this search is what the trace can seed: a solution whose
/// crossing count differs from what the refracted trace from the receiver
/// reaches is never found, and its transport is lost.
///
/// Returns false with `seed.lobes` zero when no start could be traced.
[[nodiscard]] bool traceManifoldCasterSeed(const RenderContext &render,
                                           PathContext &path,
                                           const MNEECaster &caster,
                                           const MNEEReceiver &receiver,
                                           int maxDepth, MNEEChainSeed &seed);

/// The '-mnee-test-normalhook' pass: at deterministic quasi-random points of
/// every surface instance, read the shading normal field through the
/// geometry-normal hook (with its central-difference partials) and compare
/// against the analytic mesh derivatives. Wherever the material leaves
/// 'geometry.normal' alone the two are the same field and must agree; a
/// remapped material has no analytic truth to compare against, so it
/// reports how far its field bends instead. Returns the number of
/// unmapped instances that disagree.
[[nodiscard]] int runMNEETestNormalHook(const Scene &scene);
