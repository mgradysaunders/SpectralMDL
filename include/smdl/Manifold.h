/// \file
/// The Newton solve behind manifold connections: given a receiver point
/// and a light target (a distant direction or a finite point), find the
/// points on a chain of smooth interfaces where the connection obeys
/// Snell's law or the law of reflection at every crossing, exactly for a
/// Dirac crossing or about a drawn microfacet normal for a glossy one
/// (Hanika, Droske & Fascione, "Manifold Next Event Estimation", EGSR
/// 2015; Zeltner, Georgiev & Jakob, "Specular Manifold Sampling",
/// SIGGRAPH 2020).
///
/// The solver is renderer-agnostic: it never traces a ray or interprets
/// geometry itself, and instead moves on the surfaces a renderer supplies
/// through `ManifoldSurfaces`. The transport-side work (Fresnel, medium
/// attenuation, MIS and reciprocal-probability bookkeeping) stays with
/// the renderer; this is the geometry, the measures, the trial counting,
/// and the eligibility questions that are answerable from `JIT` material
/// instances alone.
#pragma once

#include <array>
#include <atomic>
#include <cmath>
#include <ostream>

#include "smdl/JIT.h"

namespace smdl {

/// \addtogroup manifold
/// \{

/// The most interfaces a connection may cross.
constexpr int MANIFOLD_MAX_DEPTH{4};

/// How far a converged crossing may sit from the one a path actually
/// took and still count as the same solution, as a fraction of the
/// distance from the receiver.
///
/// This number calibrates the Dirac re-walk pair. The arrival side
/// identifies the gather's solution with the path's crossing by it,
/// deciding whether the gather can produce what a path found, and the
/// deterministic walk converges against a fraction of it, so neither
/// side can be more precise than the other needs. It used to be a
/// literal on the arrival side facing an unrelated residual tolerance in
/// the walk, and the two disagreeing is what made coverage unreliable.
constexpr float MANIFOLD_IDENTITY_FRACTION{1e-2f};

/// How far two converged crossings of randomly started walks may sit
/// apart and still count as the same solution, as a fraction of the
/// distance from the receiver. This is the reciprocal estimates'
/// currency, not the re-walk pair's: distinct solutions of one offset
/// approach each other at every caustic fold, and a test this coarse
/// merges the pair and loses half of it, which on a surface with tens of
/// solutions per receiver read 1.8 percent dark at 1e-2. The walks it
/// judges converge on `MANIFOLD_RECIPROCAL_RESIDUAL` so that a re-hit of
/// one solution reliably lands inside it; at 1e-2 they did not have to,
/// and re-hits that fell outside inflated the trial count and hid most
/// of that loss.
constexpr float MANIFOLD_SOLUTION_IDENTITY_FRACTION{1e-3f};

/// The constraint residual a randomly started walk converges to, on top
/// of the position test, so that its solutions are pinned well inside
/// `MANIFOLD_SOLUTION_IDENTITY_FRACTION`; the reference implementation's
/// solver threshold. A glossy chain tightens this further to a fraction
/// of its lobe, see `ManifoldChain::residualTolerance`.
constexpr float MANIFOLD_RECIPROCAL_RESIDUAL{1e-5f};

/// How many fresh starts a reciprocal estimate may draw before giving up
/// and dropping the sample, by default.
///
/// The count until a solution recurs is geometric, so its mean is the
/// reciprocal being estimated and its tail is unbounded. Truncating is the
/// one knowing departure from unbiasedness in the roughened estimator, and
/// it loses the solutions that are hardest to find, which are the ones a
/// bounded search would have found least often anyway. A surface with
/// many solutions per receiver (a wavy sheet has tens) re-finds each one
/// rarely and needs the cap raised well past their count to keep its
/// energy.
constexpr int MANIFOLD_MAX_TRIALS{64};

/// A point pinned to one of the renderer's surfaces: where it is, and
/// the renderer's own addressing of the surface, the face, and the face
/// parameterization of the point. The solver reads only `point`; the
/// rest exists so the renderer can rebuild its own hit record from a
/// vertex the walk hands back, and means whatever its
/// `ManifoldSurfaces` implementation says it means.
class ManifoldVertex final {
public:
  /// The world-space point on the surface.
  float3 point{};

  /// The renderer's identity of the surface, e.g. an instance index.
  uint64_t surface{};

  /// The renderer's identity of the face or smooth piece within the
  /// surface, e.g. a triangle or primitive-piece index.
  uint64_t face{};

  /// The face parameterization of the point, e.g. barycentric
  /// coordinates or a local `uv`, up to three components.
  float3 coords{};
};

/// The differential shading geometry at a vertex, in world space: the
/// shading normal field the walk constrains against and differentiates,
/// the position partials of the face parameterization it steps in, and
/// the geometric (facet) normal for the factors that belong to the real
/// surface rather than the interpolated field.
class ManifoldGeometry final {
public:
  float3 point{};

  /// The shading normal: the field the material's lobes actually
  /// scatter about, which for a material that remaps `geometry.normal`
  /// is the remapped field (see `JIT::Material::geometryNormalEvaluate`).
  float3 normal{};

  /// The position partials over the face parameterization
  /// `ManifoldVertex::coords` steps in.
  float3 dPdu{};
  float3 dPdv{};

  /// The shading normal partials over the same parameterization.
  float3 dNdu{};
  float3 dNdv{};

  /// The geometric (facet) normal.
  float3 Ng{};
};

/// The surfaces a manifold walk moves on, supplied by the renderer.
///
/// Two conventions are the contract. First, each vertex lives on a
/// piecewise-smooth surface a re-anchoring cast can return to:
/// `project()` accepts only a hit on the pinned vertex's own surface and
/// smooth piece (a mesh's faces tile one smooth surface; a shape's
/// pieces, like a cylinder's side and caps, do not), and whatever
/// pass-through policy the renderer has for null interfaces or cutouts
/// is its own. Second, the shading field `geometry()` reports must be
/// the field the material's lobes actually scatter about, differentiated
/// consistently with the position partials; a renderer whose material
/// remaps the shading normal reads the remapped field back through
/// `JIT::Material::geometryNormalEvaluate` and differences it.
class SMDL_EXPORT ManifoldSurfaces {
public:
  ManifoldSurfaces() = default;
  ManifoldSurfaces(const ManifoldSurfaces &) = delete;
  virtual ~ManifoldSurfaces();

  /// The differential shading geometry at `vertex`. False when the
  /// vertex cannot be evaluated, which fails the walk iterate cleanly.
  [[nodiscard]] virtual bool geometry(const ManifoldVertex &vertex,
                                      ManifoldGeometry &geometry) const = 0;

  /// Re-anchor a stepped position onto the real surface: cast from
  /// `origin` toward `target` and accept the first hit on the same
  /// surface and smooth piece as `pin`, filling `moved` with the hit and
  /// its addressing. Anything else in the way fails the step, so a
  /// converged connection's segments are known to see their endpoints.
  [[nodiscard]] virtual bool project(const ManifoldVertex &pin,
                                     const float3 &origin, const float3 &target,
                                     ManifoldVertex &moved) const = 0;
};

/// One interface of a seed chain: where it was hit, the absolute
/// refractive indices on the previous (receiver-facing) and next
/// (light-facing) sides (resolved by the renderer against its media as of
/// the crossing, and equal for a reflection), and which side of the
/// shading normal the straight segment arrived from, so a refractive walk
/// that migrates across a silhouette is rejected rather than solved with
/// swapped indices.
class ManifoldVertexSeed final {
public:
  ManifoldVertex vertex{};
  float etaPrev{};
  float etaNext{};
  float sideSign{};

  /// The tangential half vector the crossing is solved for, in the walk's
  /// own frame at the vertex: zero solves Snell's law exactly, which is a
  /// Dirac interface, and a nonzero offset solves for a microfacet normal
  /// drawn from the interface's own distribution, which is a glossy one.
  ///
  /// The walk carries this rather than deriving it, because the offset has
  /// to be fixed before the solve for the density of drawing it to mean
  /// anything, and because every walk of one estimate must solve the same
  /// constraint.
  float2 offset{};

  /// The density the offset was drawn with, in the offset's own measure,
  /// which is the interface's normal density over the cosine that projects
  /// a solid angle onto the tangent plane. One for a Dirac crossing, which
  /// has no offset to draw and no chance of drawing it.
  float offsetDensity{1.0f};

  /// The squared roughness of the lobe the offset was drawn from, the
  /// smaller of its two axes, in the slope units the offset is measured
  /// in: the width of the distribution the estimate evaluates at the
  /// converged half vector, which is how precisely that half vector has
  /// to match the offset. Zero for a Dirac crossing.
  float alpha{};

  /// The world-space vector the walk's tangent frame at this vertex is
  /// seeded from: `t1` is its projection onto the tangent plane of the
  /// iterate and `t2 = n x t1`. It is held fixed for the whole estimate so
  /// that `offset` names one world normal in every walk: the trials of the
  /// reciprocal estimate must repeat the first walk's constraint, not
  /// solve a family of constraints rotated by whatever tangent each start
  /// happens to have. Set where the offset is drawn or the start is
  /// chosen; zero means derive it from `vertex`, which a chain whose
  /// starts never move can afford.
  float3 frameSeed{};

  /// A tangential displacement of the starting iterate, in the walk's own
  /// frame at the vertex and in units of the distance from the receiver.
  ///
  /// The walk is otherwise deterministic, which is what lets the arrival
  /// side ask whether the gather could have produced a path. A roughened
  /// connection cannot use that: with the offset held fixed the constraint
  /// has several isolated solutions, and a deterministic walk reaches
  /// exactly one of them however many there are, which over-counts by the
  /// number it cannot see. Randomizing where the walk STARTS turns "which
  /// solution" into a question with a probability, which the reciprocal
  /// estimate in the gather then counts.
  float2 seedJitter{};

  /// Does the crossing reflect rather than transmit?
  ///
  /// The constraint is the same either way. `H` is
  /// `etaPrev wPrev + etaNext wNext`, and with the two indices equal, as
  /// they are on a reflection, that IS the reflection half vector; only
  /// which side the two segments have to be on differs. What differs
  /// outside the constraint is where the seed comes from: a refractive
  /// crossing lies on the straight shadow segment and is handed one, and a
  /// reflective one does not and has to be searched for.
  bool isReflect{};

  /// Is this a glossy crossing rather than a Dirac one? Per crossing:
  /// a glossy one is solved for a drawn offset whose density the
  /// estimate divides out, a Dirac one for the zero offset with its
  /// `halfVectorJacobian` standing in; see
  /// `ManifoldConnection::measure()`.
  bool isGlossy{};

  /// The transmission lobes the interface claims, `DF_DIRAC_BTDF` and or
  /// `DF_GLOSSY_BTDF`, as the renderer's seeding found them. An estimate
  /// runs once per lobe the whole chain offers, setting `isGlossy` from
  /// the lobe it is on.
  int claimedLobes{};
};

/// A seed chain: the interfaces a connection is solved through, in order
/// from the receiver: the eligible crossings of the straight shadow segment
/// for a refractive connection, a sampled caster point for a reflective one.
class ManifoldChain final {
public:
  [[nodiscard]] size_t size() const noexcept { return size_t(count); }

  [[nodiscard]] auto *begin() noexcept { return vertices.data(); }

  [[nodiscard]] auto *begin() const noexcept { return vertices.data(); }

  [[nodiscard]] auto *end() noexcept { return vertices.data() + size(); }

  [[nodiscard]] auto *end() const noexcept { return vertices.data() + size(); }

  [[nodiscard]] auto &operator[](int i) noexcept { return vertices[i]; }

  [[nodiscard]] auto &operator[](int i) const noexcept { return vertices[i]; }

public:
  std::array<ManifoldVertexSeed, MANIFOLD_MAX_DEPTH> vertices{};
  int count{};

  /// The constraint residual a walk must reach to count as converged,
  /// on top of the position test every walk passes; zero asks for no
  /// more than the walk's own sanity bound, which is what the Dirac
  /// re-walk pair wants. A randomly started walk asks for
  /// `MANIFOLD_RECIPROCAL_RESIDUAL`, and a glossy chain for a fraction of
  /// its lobe width besides: the estimate evaluates the interface
  /// distribution at the converged half vector and divides by the density
  /// of the drawn one, so the two must agree to a fraction of the lobe,
  /// which a position test cannot promise.
  float residualTolerance{};
};

/// One interface of a converged connection.
class ManifoldConnectionVertex final {
public:
  /// The interface vertex the walk converged to.
  ManifoldVertex vertex{};

  /// The differential geometry at the vertex.
  ManifoldGeometry geometry{};

  /// The unit direction toward the previous vertex (or the receiver).
  float3 wPrev{};

  /// The unit direction toward the next vertex (or the light).
  float3 wNext{};

  /// The cosine of `wPrev` against the shading normal, positive.
  float cosPrev{};

  /// The cosine of `wNext` against the shading normal, positive.
  float cosNext{};

  /// The measure `|d h / d omega_next|` of this crossing: how much
  /// tangential half vector a unit of outgoing solid angle is worth,
  /// holding the arriving direction and the vertex fixed.
  ///
  /// This is what a Dirac crossing contributes to the offset Jacobian in
  /// place of the density a glossy one has, since the Dirac delta that
  /// collapses its two dimensions is expressed in direction and the walk
  /// works in half vectors.
  float halfVectorJacobian{};
};

/// A converged connection.
class ManifoldConnection final {
public:
  std::array<ManifoldConnectionVertex, MANIFOLD_MAX_DEPTH> vertices{};

  int count{};

  /// The unit direction from the receiver toward the first vertex.
  float3 wr{};

  /// The offset Jacobian: the measure of the nested outgoing solid angles
  /// per unit of the variables the connection is drawn in, which are the
  /// light direction and one tangential half vector per crossing.
  ///
  ///     [prod_i cosPrev_i / distPrev_i^2] . [prod_i A_i] / |det J| . R
  ///
  /// with `A_i` the area element of the parameterization the constraint
  /// Jacobian is expressed in, so that `prod A_i / |det J|` is invariant to
  /// that choice, and `R` the correction from the straight-line geometry
  /// term the light sampler measured in to the one the chain's last segment
  /// actually arrives with.
  ///
  /// For a finite light the light-direction measure is the solid angle
  /// of the straight line from the receiver to the light point, which is
  /// exactly the measure the light sampler's density and radiance are
  /// expressed in, so the estimator keeps the same form as the distant
  /// case. This is the purely geometric factor; the per-crossing
  /// radiance compression `eta^2` that rides with refracted radiance is
  /// deliberately not included, so the caller applies the same
  /// convention the specular BSDF uses.
  float offsetJacobian{};

  /// The connection's measure for the chain it solved: the offset
  /// Jacobian times, at every Dirac crossing, the half-vector measure
  /// that stands in for the density a glossy crossing divides out. A
  /// glossy crossing contributes a drawn half vector the caller divides
  /// by `offsetDensity`; a Dirac crossing has no draw, its Dirac delta
  /// collapses the two half-vector dimensions instead, and
  /// `halfVectorJacobian` converts that collapse into the outgoing solid
  /// angle. For an all-Dirac chain this is the transfer Jacobian
  /// `|d omega_r / d omega_l|` whole, so every chain, pure or mixed,
  /// carries one measure.
  [[nodiscard]] float measure(const ManifoldChain &chain) const noexcept {
    float result{offsetJacobian};
    for (int i = 0; i < count; i++)
      if (!chain.vertices[i].isGlossy) result *= vertices[i].halfVectorJacobian;
    return result;
  }
};

/// The light side of a manifold connection: a distant direction (the
/// environment) or a finite light point (a punctual light or a point
/// on an area light). `wl` is always the unit direction of the
/// STRAIGHT segment from the receiver, which for a finite target must
/// equal `normalize(point - receiver)`.
class ManifoldTarget final {
public:
  float3 wl{};
  float3 point{};
  bool infinite{true};

  /// The light surface normal at `point`, or zero when the target has no
  /// orientation, which is every distant and punctual one. Only the offset
  /// Jacobian reads it, to carry the light-side geometry term across from
  /// the straight line to the segment that actually arrives.
  float3 normal{};
};

/// What one Newton walk did, for the caller's statistics: the steps it
/// took, the constraint residual where it stopped, and how it ended.
class ManifoldWalkReport final {
public:
  enum class Outcome {
    CONVERGED, ///< Converged to a valid crossing at every vertex.
    DIVERGED,  ///< Ran out of iterations, lost the surface, or stalled.
    REJECTED,  ///< Converged, but to a configuration the estimator refuses.
  };
  /// Why a walk diverged.
  enum class Failure {
    NONE,
    START,      ///< The start could not be evaluated or moved onto the surface.
    SINGULAR,   ///< The Newton system was singular or the step not finite.
    PROJECTION, ///< No halving of the step re-anchored onto the surface.
    STALLED,    ///< Halvings re-anchored, but none lowered the residual.
    ITERATIONS, ///< The iteration budget ran out.
    NUM_FAILURES
  };
  int iterations{};
  float residual{};
  Outcome outcome{Outcome::DIVERGED};
  Failure failure{Failure::NONE};
};

/// Are two converged connections of randomly started walks the same
/// solution? Compared by where the crossings land, within
/// `MANIFOLD_SOLUTION_IDENTITY_FRACTION` of the receiver distance.
[[nodiscard]] SMDL_EXPORT bool
isSameManifoldSolution(const float3 &receiver, const ManifoldConnection &a,
                       const ManifoldConnection &b);

/// The walk's tangent frame at a vertex: the shading normal it
/// constrains against and the two tangents an offset is expressed in,
/// built from `frameSeed` exactly as the walk builds them at every
/// iterate. Fails when the vertex cannot be evaluated or the seed is
/// degenerate against the normal.
[[nodiscard]] SMDL_EXPORT bool
manifoldSeedFrame(const ManifoldSurfaces &surfaces,
                  const ManifoldVertex &vertex, const float3 &frameSeed,
                  float3 &normal, float3 &t1, float3 &t2);

/// A frame seed for a vertex whose seed has none: the vertex's own
/// position tangent, or any perpendicular when that is degenerate
/// against the normal.
[[nodiscard]] SMDL_EXPORT float3 manifoldFrameSeed(
    const ManifoldSurfaces &surfaces, const ManifoldVertex &vertex);

/// Solve the connection from `receiver` to the light target through the
/// seed chain, by damped Newton iteration on the block-coupled per-vertex
/// constraints. Steps re-anchor onto the real surfaces through
/// `ManifoldSurfaces::project()` from each vertex's (already updated)
/// predecessor, so a converged connection's segments are known to see
/// their endpoints, up to whatever the renderer's projection passes
/// through. Returns true on convergence to a valid crossing on the
/// seed's own side of every interface; failure (divergence, leaving a
/// seed surface, total internal reflection, a silhouette migration, a
/// grazing or degenerate frame) means no contribution, never a wrong
/// one. `report`, if given, receives what the walk did either way.
[[nodiscard]] SMDL_EXPORT bool solveManifoldConnection(
    const ManifoldSurfaces &surfaces, const float3 &receiver,
    const ManifoldTarget &target, const ManifoldChain &chain,
    ManifoldConnection &connection, ManifoldWalkReport *report = nullptr);

/// The Bernoulli trial loop of the reciprocal estimators: draw fresh
/// starts of the same estimate until one re-finds `connection`, judged
/// by `isSameManifoldSolution()`, and report how many attempts that
/// took. The count is geometric with mean one over the chance of
/// reaching the solution, so `inverseProbability` (the attempt count)
/// estimates that reciprocal without ever computing it; a caller
/// multiplies it into the solution's value. `retry` re-seeds and solves
/// one fresh walk, filling its connection argument and returning false
/// when no start could be drawn or the walk failed, which counts as an
/// attempt that found nothing. Returns false when `maxTrials` attempts
/// all missed, in which case the sample is dropped rather than
/// truncated, the one knowing departure from unbiasedness.
template <typename Retry>
[[nodiscard]] inline bool
manifoldReciprocal(const float3 &receiver, const ManifoldConnection &connection,
                   int maxTrials, int &trials, float &inverseProbability,
                   Retry &&retry) {
  inverseProbability = 1.0f;
  for (trials = 1; trials <= maxTrials; trials++) {
    ManifoldConnection other{};
    if (retry(other) && isSameManifoldSolution(receiver, connection, other))
      return true;
    inverseProbability += 1.0f;
  }
  trials = maxTrials;
  return false;
}

/// What manifold estimators claim at an instance: the lobe kinds of
/// its material they estimate and, for the randomly seeded estimators,
/// bar the renderer's path tracer from, by domain.
///
/// Only the Dirac transmission is claimed without the renderer's caster
/// mark, and it is the one claim that bars nothing: the refractive walk
/// is deterministic, so its gather and the path tracer's own arrivals
/// are weighed against each other by re-walk MIS. Everything else, the
/// reflections and the glossy transmission, is searched for with random
/// starts, reaches each solution with a probability it cannot report,
/// and so has to be claimed outright, which is a decision the scene
/// makes by marking the instance. The claim is a static, whole-tree
/// question asked of `df_lobes`, so it can only say that the material
/// HAS a kind, never that a given crossing reaches it; both sides of the
/// estimator confirm that with a masked query at the converged geometry.
class ManifoldClaim final {
public:
  /// `DF_DIRAC_BRDF` and or `DF_GLOSSY_BRDF`, estimated by a
  /// reflective gather.
  int reflectLobes{};

  /// `DF_DIRAC_BTDF` and or `DF_GLOSSY_BTDF`, estimated by
  /// refractive chains.
  int refractLobes{};

  [[nodiscard]] bool empty() const noexcept { return lobes() == 0; }

  [[nodiscard]] int lobes() const noexcept {
    return reflectLobes | refractLobes;
  }

  /// The kinds the path tracer is barred from: every claimed kind but
  /// the Dirac transmission, which is weighed against instead.
  [[nodiscard]] int barredLobes() const noexcept {
    return reflectLobes | (refractLobes & ~JIT::DF_DIRAC_BTDF);
  }
};

/// The claim at an instance whose material instance is `mat`, with its
/// exterior IOR already resolved (for the index contrast), `marked`
/// being the renderer's caster mark on the instance.
///
/// A material that remaps `geometry.normal` (statically, see
/// `JIT::Material::remapsNormal()`) claims only when the walk can
/// solve against the remapped field, which needs the geometry-normal
/// hook compiled and a tree whose lobes all follow that field: a df
/// node given its own live normal (`DF_SETS_NORMAL`) detaches its lobes
/// from it outright, and under a remap even a node given a normal that
/// merely equals the state normal (`DF_CAN_SET_NORMAL`) detaches, that
/// not being the remapped field. A node left defaulted inherits the
/// field and reports neither bit, so it never bars a claim. An emitter
/// claims nothing either (it is light, not glass), and the transmission
/// claim needs a solid that bends: thin walls transmit without bending
/// and an index-matched boundary has no refraction to solve.
///
/// `maxGlossyAlpha`, when positive, hands glossy lobes wider than that
/// squared roughness to ordinary sampling: the estimator's variance
/// grows with the lobe width while ordinary sampling's shrinks, so past
/// some width the claim costs more than it saves (the reference's
/// figure 17 trade). The width is read from the normal hook at a fixed
/// center draw on the front side, so both sides of the estimator gate
/// identically; for a single-lobe material that is the lobe's own
/// width exactly. Dirac kinds are never gated, having no width.
[[nodiscard]] SMDL_EXPORT ManifoldClaim manifoldClaim(
    const JIT::MaterialInstance &mat, bool marked, float maxGlossyAlpha = 0.0f);

/// Is a vertex whose material instance is `mat` one the manifold
/// gathers run from and claim for? A receiver's BSDF is evaluated at
/// whatever bent direction a connection lands on, so a narrow lobe
/// there makes an estimator that is zero almost always and enormous
/// otherwise; the paths it claims are then lost at any feasible sample
/// count, while ordinary sampling handles a narrow lobe well. So a
/// vertex receives only with a generic (diffuse-like) lobe, or with a
/// glossy lobe whose squared roughness reaches `minAlpha`, read from the
/// normal hook on the side the path arrived (a material layering several
/// lobes reports the one its proposal draws from `xi`, which makes
/// the answer a draw too; it is made once per vertex and both the
/// gathers and the claims behind read the same one). The hook takes one
/// glossy kind, and the predicate asks for the reflection kind when the
/// material has it and the transmission kind otherwise: a single
/// reflect-transmit leaf reports the same lobe either way, and a
/// layering that differs by domain answers for its reflection side,
/// which is the side the receiver's own gather evaluates. Without the
/// hook (see `Compiler::enableScatterNormal`), every vertex with a
/// finite lobe receives, as the Dirac estimator always has.
///
/// The right threshold is the lobe's angular width against the light's
/// angular radius from the receiver, which is a property of the scene;
/// until a partition reads it, `minAlpha` is the renderer's one knob.
///
/// `drawXi` is a callable producing the `float4` for the hook's draw,
/// consulted only on the path that actually draws, so a renderer's
/// deterministic sampler advances exactly when a lobe is proposed.
template <typename DrawXi>
[[nodiscard]] inline bool isManifoldReceiver(const JIT::MaterialInstance &mat,
                                             bool backface, DrawXi &&drawXi,
                                             float minAlpha) {
  const int dfLobes{mat.getLobes()};
  if ((dfLobes & JIT::DF_FINITE) == 0) return false;
  if ((dfLobes & JIT::DF_GENERIC) != 0) return true;
  if (!(minAlpha > 0.0f) || !mat.material->scatterNormalSample) return true;
  // One glossy kind, per the hook's contract; see above.
  const int kind{(dfLobes & JIT::DF_GLOSSY_BRDF) != 0 ? JIT::DF_GLOSSY_BRDF
                                                      : JIT::DF_GLOSSY_BTDF};
  float3 wm{};
  float pdf{};
  float2 alpha{};
  if (!mat.scatterNormalSample(drawXi(), backface, wm, pdf, alpha, kind))
    return false;
  return std::sqrt(alpha.x * alpha.y) >= minAlpha;
}

/// Counters over every manifold estimate of a render, printed on request.
/// Relaxed atomics on one process-wide instance, disabled by default: the
/// counters share cache lines across every render thread, and on wide
/// machines that contention costs a measurable share of the render, so
/// the record calls are no-ops until the renderer that will print the
/// counters calls `setEnabled(true)`.
class SMDL_EXPORT ManifoldStats final {
public:
  /// Which gather and kind an estimate belongs to.
  enum Kind : int {
    DIRAC_REFRACT,
    GLOSSY_REFRACT,
    DIRAC_REFLECT,
    GLOSSY_REFLECT,
    NUM_KINDS
  };

  [[nodiscard]] static ManifoldStats &global() noexcept;

  /// Enable or disable recording. Set once before render threads start;
  /// while disabled, every record call below is a no-op.
  void setEnabled(bool enabled) noexcept { mEnabled = enabled; }

  /// A gather that reached its first walk, and whether that walk
  /// converged.
  void recordEstimate(Kind kind, bool firstWalkConverged) noexcept;

  /// One walk of a gather, first or trial.
  void recordWalk(const ManifoldWalkReport &report) noexcept;

  /// One re-walk the arrival side ran for MIS.
  void recordRewalk(const ManifoldWalkReport &report) noexcept;

  /// One Dirac-chain arrival weighed by re-walk MIS, and whether the
  /// re-walk reproduced the crossings the path took. The unmatched rest
  /// keeps weight 1, so the matched fraction is the share of covered
  /// arrivals any single-seed gather can ever claim.
  void recordCover(bool matched) noexcept;

  /// The reciprocal estimate of one gather, run only for a first solution
  /// that carried transport: how many trials it took to re-find the
  /// solution, or that it ran out and dropped the sample.
  void recordTrials(Kind kind, int trials, bool dropped) noexcept;

  /// A converged connection weighed, and whether anything came of it.
  void recordContribution(bool nonZero) noexcept;

  void print(std::ostream &out) const;

private:
  using Counter = std::atomic<uint64_t>;
  template <typename T>
  static void addMax(std::atomic<T> &value, T other) noexcept;
  static void addSum(std::atomic<double> &sum, double value) noexcept;

  bool mEnabled{};

  std::array<Counter, NUM_KINDS> mEstimates{};
  std::array<Counter, NUM_KINDS> mFirstWalkConverged{};
  std::array<Counter, NUM_KINDS> mTrialEstimates{};
  std::array<Counter, NUM_KINDS> mTrials{};
  std::array<Counter, NUM_KINDS> mTrialsMax{};
  std::array<Counter, NUM_KINDS> mCapDrops{};
  Counter mWalks{};
  Counter mWalksConverged{};
  Counter mWalksRejected{};
  std::array<Counter, int(ManifoldWalkReport::Failure::NUM_FAILURES)>
      mWalkFailures{};
  Counter mWalkIterations{};
  Counter mWalkIterationsMax{};
  /// Iterations of the walks that reached convergence (the rejected
  /// ones included: they converged first), bucketed by count, which is
  /// what sizing the iteration budget needs; the average above mixes in
  /// the early failures.
  std::array<Counter, 65> mConvergedIterations{};
  std::atomic<double> mWalkResidual{};
  std::atomic<double> mWalkResidualMax{};
  Counter mRewalks{};
  Counter mRewalksConverged{};
  Counter mCoverArrivals{};
  Counter mCoverMatched{};
  Counter mContributions{};
  Counter mContributionsNonZero{};
};

/// \}

} // namespace smdl
