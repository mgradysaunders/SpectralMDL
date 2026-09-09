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
/// This is the reach of the search: a solution further from the straight
/// segment than this is never found, and its transport is lost. Larger costs
/// convergence, since the walk starts further from every solution. The
/// reference implementation samples the whole caster uniformly instead,
/// which has no reach limit and needs area sampling per interface.
constexpr float MANIFOLD_SEED_JITTER{0.60f};

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

/// One instance a reflective connection may bounce off: a marked mesh
/// or shape, with the reflection lobes it claims and the area-weighted
/// face distribution (meshes) a start is drawn from.
class MNEECaster final {
public:
  uint32_t instIndex{INVALID_INDEX};
  /// `DF_DIRAC_BRDF` and or `DF_GLOSSY_BRDF`: one estimate per lobe.
  int reflectLobes{};
  /// The shape, when the caster is a primitive; starts are then drawn by
  /// `samplePrimitiveArea()` and the projection pins the walk to the piece.
  PrimitiveSpec primitive{};
  smdl::Distribution1D faceDistr{};
  float totalArea{};
};

/// Every mesh instance a reflective connection may bounce off.
///
/// This is what a reflective gather searches in place of the straight
/// shadow segment a refractive one is handed. A mirror is nowhere near the
/// line from the receiver to the light, so there is no crossing to seed
/// from and the surface has to be sampled instead.
///
/// An estimate is made on ONE caster, drawn by `sampleCaster()` with a
/// probability the estimate divides out, and every start of that estimate
/// is drawn on it by `samplePoint()`. The start density never enters the
/// estimator: the reciprocal estimate asks how often a fresh start reaches
/// the same solution, which already accounts for however the starts are
/// distributed, so all that has to hold is that every start of one
/// estimate is drawn the same way and can reach the solutions that matter.
/// Mixing casters within an estimate breaks that: a start on another
/// instance can never re-find a solution on this one, and the two may not
/// even share a material.
class MNEECasterSet final {
public:
  MNEECasterSet() = default;

  /// Enumerate the scene's marked instances, evaluating each instance's
  /// material once against a placeholder state exactly as the light
  /// sampler does for emission, and keeping those with a reflection
  /// claim. A marked instance whose material claims nothing in either
  /// domain is reported and ignored: the mark is judgment, and the one
  /// way to misapply it is to mark something that cannot focus light.
  MNEECasterSet(const Scene &scene, const Color &wavelengths,
                float maxGlossyAlpha = 0.0f);

  [[nodiscard]] bool empty() const noexcept { return casters.empty(); }

  /// Draw the caster an estimate is made on, uniformly, and the
  /// probability of having drawn it. Null when there is none.
  [[nodiscard]] const MNEECaster *sampleCaster(Sampler &sampler,
                                               float &pdf) const;

  /// Draw a start on a caster: a face by area and a uniform point on it,
  /// the hit built at the shutter fraction `time`. Returns false when the
  /// hit cannot be made.
  [[nodiscard]] bool samplePoint(const Scene &scene, Sampler &sampler,
                                 const MNEECaster &caster, float time,
                                 Hit &hit) const;

  std::vector<MNEECaster> casters{};
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
/// Both halves of the manifold estimator go through here: the gather
/// discovering its chain and the arrival-side re-walk reconstructing
/// it. Their seeds must agree vertex for vertex or the two MIS weights
/// stop summing to one, so the eligibility test and the index
/// assignment deliberately have exactly one implementation.
///
/// `wl` is the direction of travel along the straight segment, toward
/// the light. `material` is modified in place by the exterior IOR
/// resolution; `maxGlossyAlpha` is the claim's width gate, passed
/// through so both halves of the estimator gate identically.
[[nodiscard]] bool makeManifoldSeed(const MediumStack *medium,
                                    smdl::JIT::Material &material,
                                    const Hit &hit, const float3 &wl,
                                    float maxGlossyAlpha,
                                    ManifoldVertexSeed &seed);

/// The '-mnee-test-normalhook' pass: at deterministic quasi-random points of
/// every surface instance, read the shading normal field through the
/// geometry-normal hook (with its central-difference partials) and compare
/// against the analytic mesh derivatives. Wherever the material leaves
/// 'geometry.normal' alone the two are the same field and must agree; a
/// remapped material has no analytic truth to compare against, so it
/// reports how far its field bends instead. Returns the number of
/// unmapped instances that disagree.
[[nodiscard]] int runMNEETestNormalHook(const Scene &scene);
