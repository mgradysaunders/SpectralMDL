/// \file
/// The Newton solve behind manifold connections: given a receiver point
/// and a light target (a distant direction or a finite point), find the
/// points on a chain of smooth interfaces where the connection obeys
/// Snell's law or the law of reflection at every crossing, exactly for a
/// Dirac crossing or about a drawn microfacet normal for a glossy one
/// (Hanika, Droske & Fascione, "Manifold Next Event Estimation", EGSR
/// 2015; Zeltner, Georgiev & Jakob, "Specular Manifold Sampling",
/// SIGGRAPH 2020). The transport-side work (Fresnel, medium attenuation,
/// MIS and reciprocal-probability bookkeeping) lives with the path
/// tracer; this is the geometry.
#pragma once

#include <algorithm>
#include <array>
#include <atomic>
#include <cstdint>
#include <ostream>
#include <vector>

// For the nested-medium stack the per-side refractive indices resolve
// against.
#include "Medium.h"

/// The most refractive interfaces a connection may cross.
constexpr int MNEE_MAX_DEPTH{4};

/// How far a converged crossing may sit from the one a path actually
/// took and still count as the same solution, as a fraction of the
/// distance from the receiver.
///
/// This one number calibrates the whole estimator. The arrival side
/// identifies solutions with it, deciding whether the gather can produce
/// what a path found, and the walk converges against a fraction of it,
/// so neither side can be more precise than the other needs. It used to
/// be a literal on the arrival side facing an unrelated residual
/// tolerance in the walk, and the two disagreeing is what made coverage
/// unreliable.
constexpr float MANIFOLD_IDENTITY_FRACTION{1e-2f};

/// The lobes a material instance can produce anywhere in its
/// scattering tree, both sides together. What every eligibility question
/// here and in the path tracer is asked of.
[[nodiscard]] inline int
dfLobesOf(const smdl::JIT::MaterialInstance &mat) noexcept {
  return mat.instance.df_lobes_surface | mat.instance.df_lobes_backface;
}

/// One interface of a seed chain: where it was hit, the absolute
/// refractive indices on the receiver-facing and light-facing sides
/// (resolved against the medium stack as of the crossing, and equal for a
/// reflection), and which side of the shading normal the straight segment
/// arrived from, so a refractive walk that migrates across a silhouette is
/// rejected rather than solved with swapped indices.
class ManifoldVertexSeed final {
public:
  Hit hit{};
  float etaFront{};
  float etaBack{};
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

  /// A tangential displacement of the starting iterate, in the walk's own
  /// frame at the hit and in units of the distance from the receiver.
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
  /// `etaFront wFront + etaBack wBack`, and with the two indices equal, as
  /// they are on a reflection, that IS the reflection half vector; only
  /// which side the two segments have to be on differs. What differs
  /// outside the constraint is where the seed comes from: a refractive
  /// crossing lies on the straight shadow segment and is handed one, and a
  /// reflective one does not and has to be searched for.
  bool isReflect{};

  /// Is this a glossy crossing rather than a Dirac one? A chain must be
  /// all of one or all of the other: the Dirac crossings collapse two
  /// dimensions each, so a mixed chain's map is not square and its measure
  /// is not either of the two the estimator knows how to write.
  bool isGlossy{};
};

/// A seed chain: the interfaces a connection is solved through, in order
/// from the receiver: the eligible crossings of the straight shadow segment
/// for a refractive connection, a sampled caster point for a reflective one.
class ManifoldChain final {
public:
  std::array<ManifoldVertexSeed, MNEE_MAX_DEPTH> vertices{};
  int count{};
};

/// One interface of a converged connection.
class ManifoldConnectionVertex final {
public:
  /// The interface vertex the walk converged to.
  Hit hit{};

  /// The differential geometry at the vertex.
  ManifoldGeometry geometry{};

  /// The unit direction toward the previous vertex (or the receiver).
  float3 wFront{};

  /// The unit direction toward the next vertex (or the light).
  float3 wBack{};

  /// The cosine of `wFront` against the shading normal, positive.
  float cosFront{};

  /// The cosine of `wBack` against the shading normal, positive.
  float cosBack{};

  /// The measure `|d h / d omega_back|` of this crossing: how much
  /// tangential half vector a unit of outgoing solid angle is worth,
  /// holding the arriving direction and the vertex fixed.
  ///
  /// This is what a Dirac crossing contributes to the offset Jacobian in
  /// place of the density a glossy one has, since the delta that collapses
  /// its two dimensions is expressed in direction and the walk works in
  /// half vectors.
  float halfVectorJacobian{};
};

/// A converged connection.
class ManifoldConnection final {
public:
  std::array<ManifoldConnectionVertex, MNEE_MAX_DEPTH> vertices{};

  int count{};

  /// The unit direction from the receiver toward the first vertex.
  float3 wr{};

  /// The transfer Jacobian `|d omega_r / d omega_l|`: the solid angle
  /// the connection subtends at the receiver per unit solid angle of
  /// the light direction, through the whole chain. For a finite light
  /// the light-direction measure is the solid angle of the straight
  /// line from the receiver to the light point, which is exactly the
  /// measure the light sampler's density and radiance are expressed
  /// in, so the estimator keeps the same form as the distant case.
  /// This is the purely geometric factor; the per-crossing radiance
  /// compression `eta^2` that rides with refracted radiance is
  /// deliberately not included, so the caller applies the same
  /// convention the specular BSDF uses.
  float transfer{};

  /// The offset Jacobian: the measure of the nested outgoing solid angles
  /// per unit of the variables a roughened connection is drawn in, which
  /// are the light direction and one tangential half vector per crossing.
  ///
  ///     [prod_i cosFront_i / distFront_i^2] . [prod_i A_i] / |det J| . R
  ///
  /// with `A_i` the area element of the parameterization the constraint
  /// Jacobian is expressed in, so that `prod A_i / |det J|` is invariant to
  /// that choice, and `R` the correction from the straight-line geometry
  /// term the light sampler measured in to the one the chain's last segment
  /// actually arrives with.
  ///
  /// `transfer` is the same quantity for a chain with no offsets to draw,
  /// where the crossings' deltas have collapsed all but two dimensions.
  /// The two agree where both apply: an all-Dirac chain has
  /// `offsetJacobian * prod_i halfVectorJacobian_i == transfer`, which is
  /// how this is checked.
  float offsetJacobian{};
};

/// How far a jittered start may sit from the straight-line crossing, as a
/// fraction of the distance from the receiver.
///
/// This is the reach of the search: a solution further from the straight
/// segment than this is never found, and its transport is lost. Larger costs
/// convergence, since the walk starts further from every solution. The
/// reference implementation samples the whole caster uniformly instead,
/// which has no reach limit and needs area sampling per interface.
constexpr float MANIFOLD_SEED_JITTER{0.60f};

/// How many fresh starts the reciprocal estimate may draw before giving up
/// and dropping the sample.
///
/// The count until a solution recurs is geometric, so its mean is the
/// reciprocal being estimated and its tail is unbounded. Truncating is the
/// one knowing departure from unbiasedness in the roughened estimator, and
/// it loses the solutions that are hardest to find, which are the ones a
/// bounded search would have found least often anyway.
constexpr int MANIFOLD_MAX_TRIALS{64};

/// Are two converged connections the same solution? Compared by where the
/// crossings land, at the same scale the arrival side identifies solutions
/// at, so the estimator has one notion of "same" rather than two.
[[nodiscard]] bool isSameManifoldSolution(const float3 &receiver,
                                          const ManifoldConnection &a,
                                          const ManifoldConnection &b);

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

/// One instance a reflective connection may bounce off, with the
/// area-weighted face distribution a seed is drawn from.
class ManifoldCaster final {
public:
  uint32_t instIndex{INVALID_INDEX};
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
/// The sampling density never enters the estimator. The reciprocal estimate
/// asks how often a fresh draw reaches the same solution, and that already
/// accounts for however the draws are distributed; all this has to be is
/// the same distribution every time, and able to reach the solutions that
/// matter.
class ManifoldCasterSet final {
public:
  ManifoldCasterSet() = default;

  /// Enumerate the scene's eligible instances, evaluating each instance's
  /// material once against a placeholder state exactly as the light
  /// sampler does for emission.
  ManifoldCasterSet(const Scene &scene, const Color &wavelengths,
                    bool allowGlossy);

  [[nodiscard]] bool empty() const noexcept { return casters.empty(); }

  /// Draw a seed: a caster, a face on it by area, and a point on that
  /// face. Returns false when there is nothing to draw.
  [[nodiscard]] bool sampleSeed(const Scene &scene, Sampler &sampler,
                                Hit &hit) const;

  std::vector<ManifoldCaster> casters{};
};

/// Is the material at a hit a surface a reflective connection may bounce
/// off: a mirror, or a rough one, that is not itself a light?
///
/// Unlike a refractive interface this asks nothing about indices or thin
/// walls, since a mirror bends nothing and a thin one reflects the same as
/// a thick one. The normal-remap veto is the same and for the same reason:
/// the walk differentiates the mesh shading-normal field, so a material
/// that moves the normal is solving a different constraint than the one it
/// scatters about.
[[nodiscard]] bool isManifoldCaster(const smdl::JIT::MaterialInstance &mat,
                                    const float3 &stateNormal,
                                    bool allowGlossy);

/// Is the material at a blocking hit an interface a connection may
/// refract through: a solid carrying a Dirac transmission that actually
/// bends light? Thin walls transmit without bending and emitters are
/// light, not glass; either disqualifies, and so does an index-matched
/// boundary, whose transport has no refraction to solve and stays with
/// the ordinary estimators.
///
/// The test is on the Dirac transmission LOBE, `DF_DELTA_BTDF`, and not
/// on the material being purely specular. A material may carry any other
/// lobes alongside it: the chain claims only the Dirac transmission,
/// asking the material for that one lobe by mask, and everything else the
/// interface does is transport the ordinary estimators carry. Testing
/// the two axes separately would not do, since a Dirac reflection over a
/// diffuse transmission has the same domains and the same kinds as a
/// Dirac transmission over a diffuse reflection and no manifold to
/// solve.
///
/// A material that remaps `geometry.normal` disqualifies: the walk
/// differentiates the mesh shading-normal field, so the BSDF would
/// refract about a frame the walk does not solve for, and gathering on
/// the unremapped manifold while the path side refracts about the
/// remapped one silently over-counts. `stateNormal` is the internal-space
/// shading normal of the state the instance was evaluated with; the test
/// is byte equality, not a tolerance, because an untouched
/// `geometry.normal` is initialized from `$state.normal` and a remap
/// genuinely moves the manifold.
///
/// The instance's exterior IOR must already be resolved against the
/// medium stack, since the contrast is measured against it.
///
/// This is a static, whole-tree question, so it can only say that the
/// interface HAS a Dirac transmission, never that this crossing reaches
/// it. Both sides of the estimator confirm that with a masked
/// `scatterSample` at the converged geometry and decline complementarily
/// when it fails.
///
/// Several Dirac transmissions mixed at one interface are the one case
/// that query cannot settle: they are indistinguishable by direction, so
/// each side reports the chance of the branch its own draw took rather
/// than the sum over every branch that produces the direction. Each side
/// still estimates its own transport correctly; it is the two MIS weights
/// that stop summing to exactly one.
[[nodiscard]] bool isManifoldInterface(const smdl::JIT::MaterialInstance &mat,
                                       const float3 &stateNormal,
                                       bool allowGlossy);

/// Does the interface's transmission bend light through a normal
/// distribution rather than a Dirac delta? Only meaningful where
/// `isManifoldInterface()` already said yes, and it decides which of the
/// two measures the connection is written in, so both halves of the
/// estimator must agree on it. A material carrying both is claimed by the
/// Dirac reading, which is the one that was there first and the one whose
/// transport the ordinary estimators cannot reach at all.
[[nodiscard]] bool
isGlossyManifoldInterface(const smdl::JIT::MaterialInstance &mat);

/// The reflective twin: does the caster reflect through a normal
/// distribution rather than a Dirac delta?
[[nodiscard]] bool
isGlossyManifoldCaster(const smdl::JIT::MaterialInstance &mat);

/// The walk's tangent frame at a hit: the shading normal it constrains
/// against and the two tangents an offset is expressed in, built from the
/// hit alone.
[[nodiscard]] bool manifoldSeedFrame(const Scene &scene, const Hit &hit,
                                     float3 &normal, float3 &t1, float3 &t2);

/// Seed one chain vertex from an interface the straight segment
/// crosses: resolve the instance's exterior IOR against `medium`,
/// admit the interface only if `isManifoldInterface()` does, and fill
/// in the per-side indices and the side of the shading normal the
/// segment arrived from.
///
/// Both halves of the manifold estimator go through here: the gather
/// discovering its chain and the arrival-side re-walk reconstructing
/// it. Their seeds must agree vertex for vertex or the two MIS weights
/// stop summing to one, so the eligibility test and the index
/// assignment deliberately have exactly one implementation.
///
/// `wl` is the direction of travel along the straight segment, toward
/// the light. `mat` is modified in place by the exterior
/// IOR resolution, and `state` must be the state it was evaluated
/// with, for the normal-remap eligibility test.
[[nodiscard]] bool makeManifoldSeed(const MediumStack *medium,
                                    smdl::JIT::MaterialInstance &mat,
                                    const smdl::State &state, const Hit &hit,
                                    const float3 &wl, bool allowGlossy,
                                    ManifoldVertexSeed &seed);

/// What one Newton walk did, for the caller's statistics: the steps it
/// took, the constraint residual where it stopped, and how it ended.
class ManifoldWalkReport final {
public:
  enum class Outcome {
    CONVERGED, ///< Converged to a valid crossing at every vertex.
    DIVERGED,  ///< Ran out of iterations, lost the surface, or stalled.
    REJECTED,  ///< Converged, but to a configuration the estimator refuses.
  };
  int iterations{};
  float residual{};
  Outcome outcome{Outcome::DIVERGED};
};

/// Solve the connection from `receiver` to the light target through the
/// seed chain, by damped Newton iteration on the block-coupled per-vertex
/// constraints. Steps re-anchor onto the real surfaces by re-casting each
/// vertex from its (already updated) predecessor, so a converged
/// connection's segments are known to see their endpoints (null
/// interfaces excepted). Returns true on convergence to a valid crossing
/// on the seed's own side of every interface; failure (divergence,
/// leaving a seed instance, total internal reflection, a silhouette
/// migration, a grazing or degenerate frame) means no contribution, never
/// a wrong one. `report`, if given, receives what the walk did either way.
[[nodiscard]] bool solveManifoldConnection(
    const Scene &scene, const float3 &receiver, const ManifoldTarget &target,
    const ManifoldChain &chain, ManifoldConnection &connection,
    ManifoldWalkReport *report = nullptr);

/// Is a converged crossing one the estimator accepts? The two segments
/// must lie on opposite sides of the shading normal, which is what makes
/// the crossing a transmission, and the arriving segment must be on the
/// side the seed crossed from, which rejects a solution that migrated
/// across a silhouette and would otherwise be weighed with swapped
/// indices.
///
/// Both halves of the estimator apply this to whatever crossing they
/// have in hand, the gather to each solution and the arrival-side
/// cancelation to the crossing the path actually took, which is what
/// keeps the two coverage sets identical. `wFront` points toward the
/// receiver and `wBack` toward the light.
[[nodiscard]] bool isManifoldCrossing(const ManifoldVertexSeed &seed,
                                      const float3 &normal,
                                      const float3 &wFront,
                                      const float3 &wBack);

/// Evaluate the transfer Jacobian of a chain that already satisfies the
/// constraints, e.g., the actual crossings of a path that refracted
/// through the interfaces, without running the Newton walk. The chain's
/// hits must be the crossing points in order from `receiver`, with the
/// target past the last one. Fails on a degenerate frame or a chain
/// whose residual says it is not actually a solution.
[[nodiscard]] bool evaluateManifoldTransfer(const Scene &scene,
                                            const float3 &receiver,
                                            const ManifoldTarget &target,
                                            const ManifoldChain &chain,
                                            float &transfer);

/// Counters over every manifold estimate of a render, printed on request.
/// Relaxed atomics on one process-wide instance: each increment is per
/// walk or per estimate, which is nothing next to the ray casts a walk
/// makes.
class ManifoldStats final {
public:
  /// Which gather an estimate belongs to.
  enum Kind : int { DIRAC_REFRACT, GLOSSY_REFRACT, REFLECT, NUM_KINDS };

  [[nodiscard]] static ManifoldStats &global() noexcept;

  /// A gather that reached its first walk, and whether that walk
  /// converged.
  void recordEstimate(Kind kind, bool firstWalkConverged) noexcept;

  /// One walk of a gather, first or trial.
  void recordWalk(const ManifoldWalkReport &report) noexcept;

  /// One re-walk the arrival side ran for MIS.
  void recordRewalk(const ManifoldWalkReport &report) noexcept;

  /// The reciprocal estimate of one gather: how many trials it took to
  /// re-find the solution, or that it ran out and dropped the sample.
  void recordTrials(Kind kind, int trials, bool dropped) noexcept;

  /// A converged connection weighed, and whether anything came of it.
  void recordContribution(bool nonZero) noexcept;

  void print(std::ostream &out) const;

private:
  using Counter = std::atomic<uint64_t>;
  static void addMax(Counter &counter, uint64_t value) noexcept;
  static void addSum(std::atomic<double> &sum, double value) noexcept;
  static void addMax(std::atomic<double> &value, double other) noexcept;

  std::array<Counter, NUM_KINDS> mEstimates{};
  std::array<Counter, NUM_KINDS> mFirstWalkConverged{};
  std::array<Counter, NUM_KINDS> mTrialEstimates{};
  std::array<Counter, NUM_KINDS> mTrials{};
  std::array<Counter, NUM_KINDS> mTrialsMax{};
  std::array<Counter, NUM_KINDS> mCapDrops{};
  Counter mWalks{};
  Counter mWalksConverged{};
  Counter mWalksRejected{};
  Counter mWalkIterations{};
  Counter mWalkIterationsMax{};
  std::atomic<double> mWalkResidual{};
  std::atomic<double> mWalkResidualMax{};
  Counter mRewalks{};
  Counter mRewalksConverged{};
  Counter mContributions{};
  Counter mContributionsNonZero{};
};
