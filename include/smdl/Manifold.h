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

#include <cmath>
#include <vector>

#include "smdl/JIT.h"

namespace smdl {

/// \addtogroup manifold
/// \{

/// The most interfaces a connection may cross.
///
/// A sanity bound: the walk's workspace is dynamically sizable
/// to any conceivable chain, so this is what the solver refuses
/// rather than what anything is built for.
inline constexpr int MANIFOLD_MAX_DEPTH{16};

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
inline constexpr float MANIFOLD_IDENTITY_FRACTION{1e-2f};

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
inline constexpr float MANIFOLD_SOLUTION_IDENTITY_FRACTION{1e-3f};

/// The constraint residual a randomly started walk converges to on top
/// of the position test so that its solutions are pinned well inside
/// `MANIFOLD_SOLUTION_IDENTITY_FRACTION`.
///
/// \note
/// A glossy chain tightens this further to a fraction of its lobe,
/// see `ManifoldChain::residualTolerance`.
inline constexpr float MANIFOLD_RECIPROCAL_RESIDUAL{1e-5f};

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
inline constexpr int MANIFOLD_MAX_TRIALS{64};

/// A point pinned to one of the renderer's surfaces: where it is, and
/// the renderer's own addressing of the surface, the face, and the face
/// parameterization of the point. The solver reads only `point`; the
/// rest exists so the renderer can rebuild its own hit record from a
/// vertex the walk hands back, and means whatever its
/// `ManifoldSurfaces` implementation says it means.
///
/// This and the geometry and connection records below carry no default
/// values: they are the solver's scratch, declared without braces and
/// written whole before they are read, and a walk fills thousands of
/// them per estimate. Value-initialize (`{}`) one that must start zero.
class ManifoldVertex final {
public:
  /// The world-space point on the surface.
  float3 point;

  /// The renderer's identity of the surface, e.g. an instance index.
  uint64_t surface;

  /// The renderer's identity of the face or smooth piece within the
  /// surface, e.g. a triangle or primitive-piece index.
  uint64_t face;

  /// The face parameterization of the point, e.g. barycentric
  /// coordinates or a local `uv`, up to three components.
  float3 coords;
};

/// The differential shading geometry at a vertex, in world space: the
/// shading normal field the walk constrains against and differentiates,
/// the position partials of the face parameterization it steps in, and
/// the geometric (facet) normal for the factors that belong to the real
/// surface rather than the interpolated field.
class ManifoldGeometry final {
public:
  float3 point;

  /// The shading normal: the field the material's lobes actually
  /// scatter about, which for a material that remaps `geometry.normal`
  /// is the remapped field (see `JIT::MaterialDef::geometryNormalEvaluate`).
  float3 normal;

  /// The position partials over the face parameterization
  /// `ManifoldVertex::coords` steps in.
  float3 dPdu;
  float3 dPdv;

  /// The shading normal partials over the same parameterization.
  float3 dNdu;
  float3 dNdv;

  /// The geometric (facet) normal.
  float3 Ng;
};

/// The surfaces a manifold walk moves on, supplied by the renderer.
///
/// Two conventions are the contract. First, each vertex lives on a
/// piecewise-smooth surface a re-anchoring cast can return to:
/// `project()` accepts only a hit on the pinned vertex's own surface and
/// smooth piece (a mesh's faces tile one smooth surface; a shape's
/// pieces, like a cylinder's side and caps, do not), and whatever
/// pass-through policy the renderer has for null interfaces or cutouts
/// is its own. Second, the shading field `evaluateGeometry()` reports must be
/// the field the material's lobes actually scatter about, differentiated
/// consistently with the position partials; a renderer whose material
/// remaps the shading normal reads the remapped field back through
/// `JIT::MaterialDef::geometryNormalEvaluate` and differences it.
class SMDL_EXPORT ManifoldSurfaces {
public:
  ManifoldSurfaces() = default;
  ManifoldSurfaces(const ManifoldSurfaces &) = delete;
  virtual ~ManifoldSurfaces();

  /// The differential shading geometry at `vertex`. False when the
  /// vertex cannot be evaluated, which fails the walk iterate cleanly.
  [[nodiscard]] virtual bool
  evaluateGeometry(const ManifoldVertex &vertex,
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

  /// The squared roughness of the narrowest lobe of the distribution
  /// the offset was drawn from, the smaller of its two axes, in the
  /// slope units the offset is measured in: the finest scale of the
  /// density the estimate divides by at the converged half vector, which
  /// is how precisely that half vector has to match the offset. A
  /// mixture of widths is as fine as its narrowest part, whichever lobe
  /// the draw took. Zero for a Dirac crossing.
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
  /// crossing may lie on the straight shadow segment and be handed one,
  /// and a reflective one never does and has to be searched for, as a
  /// refractive one is too wherever the straight segment crosses nothing.
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
/// for a refractive connection handed them, a sampled caster point for a
/// reflective one, and for a caster refractive one the sampled caster
/// point and the crossings a ray refracted through it goes on to meet.
class ManifoldChain final {
public:
  /// The number of crossings, as an `int` because every loop over a
  /// chain is one and a `size_t` would sign-compare against all of them.
  [[nodiscard]] int size() const noexcept {
    return static_cast<int>(vertices.size());
  }

  [[nodiscard]] bool empty() const noexcept { return vertices.empty(); }

  [[nodiscard]] auto *begin() noexcept { return vertices.data(); }

  [[nodiscard]] auto *begin() const noexcept { return vertices.data(); }

  [[nodiscard]] auto *end() noexcept {
    return vertices.data() + vertices.size();
  }

  [[nodiscard]] auto *end() const noexcept {
    return vertices.data() + vertices.size();
  }

  [[nodiscard]] auto &operator[](int i) noexcept { return vertices[i]; }

  [[nodiscard]] auto &operator[](int i) const noexcept { return vertices[i]; }

  /// Room for `depth` crossings without allocating again, which is the
  /// only allocation the chain makes: a caller that reserves once and
  /// refills in place never allocates after that. It never shrinks.
  void reserve(int depth) { vertices.reserve(static_cast<size_t>(depth)); }

  /// Empty the chain and leave room for `depth`: a chain reused from a
  /// caller's scratch, as clean as a default-constructed one.
  void restart(int depth) {
    vertices.clear();
    reserve(depth);
    residualTolerance = 0.0f;
  }

  /// Admit one crossing and hand back the cleared seed to fill. A
  /// filler that then refuses it calls `pop()`. The reference is good
  /// until an `append()` outgrows the reserve, so reserve the whole
  /// chain first, which `restart()` does.
  [[nodiscard]] ManifoldVertexSeed &append() { return vertices.emplace_back(); }

  /// Drop the crossing `append()` just admitted.
  void pop() noexcept { vertices.pop_back(); }

public:
  /// The crossings, which ARE the chain: there is no separate count to
  /// disagree with them.
  std::vector<ManifoldVertexSeed> vertices{};

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
  ManifoldVertex vertex;

  /// The differential geometry at the vertex.
  ManifoldGeometry geometry;

  /// The unit direction toward the previous vertex (or the receiver).
  float3 wPrev;

  /// The unit direction toward the next vertex (or the light).
  float3 wNext;

  /// The cosine of `wPrev` against the shading normal, positive.
  float cosPrev;

  /// The cosine of `wNext` against the shading normal, positive.
  float cosNext;

  /// The measure `|d h / d omega_next|` of this crossing: how much
  /// tangential half vector a unit of outgoing solid angle is worth,
  /// holding the arriving direction and the vertex fixed.
  ///
  /// This is what a Dirac crossing contributes to the offset Jacobian in
  /// place of the density a glossy one has, since the Dirac delta that
  /// collapses its two dimensions is expressed in direction and the walk
  /// works in half vectors.
  float halfVectorJacobian;
};

/// A converged connection.
class ManifoldConnection final {
public:
  /// The number of crossings; see `ManifoldChain::size()`.
  [[nodiscard]] int size() const noexcept {
    return static_cast<int>(vertices.size());
  }

  /// Room for `depth` crossings without allocating again; see
  /// `ManifoldChain::reserve()`.
  void reserve(int depth) { vertices.reserve(static_cast<size_t>(depth)); }

  /// Give the connection exactly `count` crossings, keeping whatever
  /// room it has and, as the records themselves do, leaving them
  /// uninitialized: `solveManifoldConnection()` writes every field of
  /// every crossing it keeps. Reused across an estimate's walks, which
  /// all have one length, this does nothing after the first.
  void resize(int count) { vertices.resize(static_cast<size_t>(count)); }

public:
  std::vector<ManifoldConnectionVertex> vertices;

  /// The unit direction from the receiver toward the first vertex.
  float3 wr;

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
  float offsetJacobian;

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
    const int numCrossings{size()};
    for (int i = 0; i < numCrossings; i++)
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
  /// The light surface normal at `point`, or zero when the target has no
  /// orientation, which is every distant and punctual one. Only the offset
  /// Jacobian reads it, to carry the light-side geometry term across from
  /// the straight line to the segment that actually arrives.
  float3 normal{};
  bool isInfinite{true};
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

/// What tells one converged connection from another for
/// `isSameManifoldSolution()`: where its crossings land, which is all
/// the comparison reads, so a set of found solutions keeps these rather
/// than whole connections. Scratch like the connection itself: `set()`
/// writes the points below `count` and nothing past them.
class ManifoldSolutionKey final {
public:
  void set(const ManifoldConnection &connection) {
    const int numCrossings{connection.size()};
    points.resize(static_cast<size_t>(numCrossings));
    for (int i = 0; i < numCrossings; i++)
      points[i] = connection.vertices[i].vertex.point;
  }

  std::vector<float3> points;
};

/// Are two converged connections of randomly started walks the same
/// solution? Compared by where the crossings land, within
/// `MANIFOLD_SOLUTION_IDENTITY_FRACTION` of the receiver distance.
[[nodiscard]] SMDL_EXPORT bool
isSameManifoldSolution(const float3 &receiver, const ManifoldSolutionKey &a,
                       const ManifoldConnection &b);

/// The same, keyed on the fly.
[[nodiscard]] SMDL_EXPORT bool
isSameManifoldSolution(const float3 &receiver, const ManifoldConnection &a,
                       const ManifoldConnection &b);

/// The walk's tangent frame at a vertex: the shading normal it
/// constrains against and the two tangents an offset is expressed in,
/// built from `frameSeed` exactly as the walk builds them at every
/// iterate. Fails when the vertex cannot be evaluated or the seed is
/// degenerate against the normal.
[[nodiscard]] SMDL_EXPORT bool
buildManifoldSeedFrame(const ManifoldSurfaces &surfaces,
                       const ManifoldVertex &vertex, const float3 &frameSeed,
                       float3 &normal, float3 &t1, float3 &t2);

/// A frame seed for a vertex whose seed has none: the vertex's own
/// position tangent, or any perpendicular when that is degenerate
/// against the normal.
[[nodiscard]] SMDL_EXPORT float3 manifoldFrameSeed(
    const ManifoldSurfaces &surfaces, const ManifoldVertex &vertex);

/// The walk's iterate at one vertex of the chain: the differential
/// geometry it last re-anchored to, the two segments meeting there, and
/// the generalized half vector and tangent frame the constraint is
/// expressed in.
///
/// This is `ManifoldWalkScratch`'s element rather than anything a caller
/// reads. It is here, and not in the solver, only so that the workspace
/// can hold it by value; a walk writes every field it reads and nothing
/// past the chain's own length, so it carries no meaning between walks.
/// Like the records above it carries no default values.
class ManifoldWalkVertex final {
public:
  ManifoldGeometry geometry;

  /// Toward the previous vertex, or the receiver, and its distance.
  float3 wPrev;
  float distPrev;

  /// Toward the next vertex, or the light. The distance is 0 for a
  /// distant target, which drops the position-derivative term.
  float3 wNext;
  float distNext;

  /// The generalized half vector, and its length before normalizing.
  float3 hHat;
  float hLen;

  /// The sign that orients `hHat` onto the shading normal's side, so
  /// that the constraint means a microfacet normal rather than a line
  /// through one.
  float hSign;

  /// The area element of the parameterization the Jacobian is expressed
  /// in, and the half-vector measure of the crossing; see
  /// `ManifoldConnectionVertex::halfVectorJacobian`.
  float areaElement;
  float halfVectorJacobian;

  /// The tangents the constraint projects onto, built from the seed
  /// vector the walk holds fixed.
  float3 t1;
  float3 t2;
};

/// The workspace one manifold walk runs in, sized once for the deepest
/// chain a render can ask for and reused by every walk after, so that a
/// walk allocates nothing however deep the chain.
///
/// Not thread safe: give each thread its own, as `BumpPtrAllocator`
/// asks. A renderer buys one per thread or per block of work and hands
/// the same one to every solve that thread runs.
///
/// The buffers are the solver's own and carry no meaning between walks:
/// a walk writes every entry it reads, nothing past the chain's length
/// is touched, and how they are carved up is the solver's business, not
/// a promise. `reserve()` is the only member a caller has business
/// calling; the sizes are documented so that the cost of a depth is
/// possible to reason about, not so that anything may index them.
class SMDL_EXPORT ManifoldWalkScratch final {
public:
  ManifoldWalkScratch() = default;

  explicit ManifoldWalkScratch(int depth) { reserve(depth); }

  /// Non-copyable: it is a thread's workspace, never a value.
  ManifoldWalkScratch(const ManifoldWalkScratch &) = delete;

  ManifoldWalkScratch &operator=(const ManifoldWalkScratch &) = delete;

  /// Size the workspace for chains of up to `depth` crossings, which is
  /// the only allocation this class makes. It never shrinks, so a
  /// workspace reserved for a deeper chain serves a shallower one, and
  /// a depth already covered costs one predicted branch. A solve
  /// reserves for its own chain, so a caller that reserves for the
  /// render's depth up front is buying the allocation at a moment of
  /// its choosing rather than avoiding one.
  void reserve(int depth) {
    if (depth > maxDepth) grow(depth);
  }

public:
  /// The deepest chain the buffers below are sized for, 0 until
  /// `reserve()`.
  int maxDepth{};

  /// The walk's own iterate, then the trial step's: `maxDepth` apiece.
  std::vector<ManifoldVertex> vertices;

  /// The fixed frame seeds, then the world-space Newton steps:
  /// `maxDepth` apiece.
  std::vector<float3> vectors;

  /// The two chain states the iteration swaps between: `maxDepth`
  /// apiece.
  std::vector<ManifoldWalkVertex> iterates;

  /// The tangent-frame lengths of one iterate, `maxDepth`.
  std::vector<float> frameLengths;

  /// The two states' constraint residuals, `2 * maxDepth` apiece.
  std::vector<float> constraints;

  /// The two states' constraint Jacobians and the copy the solve
  /// eliminates in place, `2 * maxDepth` rows of a fixed band width
  /// apiece. The constraints couple neighbours only, so the system is
  /// banded and nothing here grows faster than the depth.
  std::vector<float> jacobians;

  /// The solve's right-hand side, `2 * maxDepth`.
  std::vector<float> rhs;

private:
  /// The out-of-line half of `reserve()`, so that the common call is a
  /// branch rather than a call across a shared library boundary.
  void grow(int depth);
};

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
/// `scratch` is the caller's workspace, reserved for this chain if it
/// was not already; see `ManifoldWalkScratch`.
[[nodiscard]] SMDL_EXPORT bool solveManifoldConnection(
    const ManifoldSurfaces &surfaces, ManifoldWalkScratch &scratch,
    const float3 &receiver, const ManifoldTarget &target,
    const ManifoldChain &chain, ManifoldConnection &connection,
    ManifoldWalkReport *report = nullptr);

/// The Bernoulli trial loop of the reciprocal estimators: draw fresh
/// starts of the same estimate until one re-finds `connection`, judged
/// by `isSameManifoldSolution()`, and report how many attempts that
/// took. The count is geometric with mean one over the chance of
/// reaching the solution, so `inverseProbability` (the attempt count)
/// estimates that reciprocal without ever computing it; a caller
/// multiplies it into the solution's value. `key` and `other` are the
/// caller's scratch, so that a loop of trials allocates nothing; both
/// are written here and carry nothing in. `retry` re-seeds and solves
/// one fresh walk, filling its connection argument and returning false
/// when no start could be drawn or the walk failed, which counts as an
/// attempt that found nothing. Returns false when `maxTrials` attempts
/// all missed, in which case the sample is dropped rather than
/// truncated, the one knowing departure from unbiasedness.
template <typename Retry>
[[nodiscard]] inline bool
manifoldReciprocal(const float3 &receiver, const ManifoldConnection &connection,
                   ManifoldSolutionKey &key, ManifoldConnection &other,
                   int maxTrials, int &trials, float &inverseProbability,
                   Retry &&retry) {
  inverseProbability = 1.0f;
  // The solution is keyed once rather than on every comparison, since it
  // does not move; `other` is one buffer for every trial, because `retry`
  // writes the whole connection whenever it succeeds and one it refused is
  // never read.
  key.set(connection);
  for (trials = 1; trials <= maxTrials; trials++) {
    if (retry(other) && isSameManifoldSolution(receiver, key, other))
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
/// mark, and without the mark it is the one claim that bars nothing: the
/// straight-line refractive walk is deterministic, so its gather and the
/// path tracer's own arrivals are weighed against each other by re-walk
/// MIS. Everything else, the reflections and the glossy transmission, is
/// searched for with random starts, reaches each solution with a
/// probability it cannot report, and so has to be claimed outright,
/// which is a decision the scene makes by marking the instance; on a
/// marked instance the Dirac transmission is searched for the same way
/// wherever the straight segment does not hand it a crossing, and those
/// chains are claimed outright too. The claim is a static, whole-tree
/// question asked of one side's `df_lobes`, so it can only say that the
/// material HAS a kind on the side asked, never that a given crossing
/// reaches it; both halves of the estimator confirm that with a masked
/// query at the converged geometry.
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

  /// The kinds the path tracer is barred from by a share of its
  /// throughput: every claimed kind but the Dirac transmission, whose
  /// chains are weighed against, or dropped whole, by family instead.
  [[nodiscard]] int barredLobes() const noexcept {
    return reflectLobes | (refractLobes & ~DF_DIRAC_BTDF);
  }
};

/// The claim at an instance whose evaluated material is `material`, with its
/// exterior IOR already resolved (for the index contrast), on the side
/// `backface` names, which is the side the scattering functions
/// dispatch on and a caller spells `JIT::Material::isInterior(wo)`,
/// `isMarked` being the renderer's caster mark on the instance.
///
/// The side is asked because a two-sided material scatters by a
/// different tree on each of them. Claiming the union bars the path
/// tracer from a kind on the side that cannot produce it while the
/// gather's own draw fails there, and the transport falls between the
/// two.
///
/// A material that remaps `geometry.normal` (statically, see
/// `JIT::MaterialDef::canRemapNormal()`) claims only when the walk can
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
/// The width of a glossy lobe is never read here: it is part of the
/// kind. A microfacet lobe wider than the builtin cutoff labels itself
/// `DF_SMOOTH_BRDF` (see `DF_GLOSSY_BRDF`), so a layered material's word
/// carries its narrow lobe as glossy and its wide one as smooth, the
/// claim takes the one and leaves the other to ordinary sampling, and
/// both halves of the estimator read the same word.
[[nodiscard]] SMDL_EXPORT ManifoldClaim
manifoldClaim(const JIT::Material &material, bool isBackface, bool isMarked);

/// The claim on either side, the union of the two: what a caller with no
/// one side in hand asks, a load-time enumeration of marked instances
/// among them, where a walk's starts may land on either side of the
/// instance and the masked query at the converged crossing settles which
/// one actually scatters.
[[nodiscard]] SMDL_EXPORT ManifoldClaim
manifoldClaim(const JIT::Material &material, bool isMarked);

/// The narrowest width of the glossy lobes of `material` on the side
/// `isBackface`, read from the normal hook, which reports it whichever
/// lobe its draw takes; `drawXi` is a callable producing the `float4`
/// for that draw, consulted only when there is a glossy lobe to read,
/// so a renderer's deterministic sampler advances exactly then. The
/// width is the squared roughness, the slope of the lobe's half-width,
/// which is what `manifoldReceiverLobes()` judges against a light's
/// angular radius. `INFINITY` without a glossy lobe, so that the
/// question never arises, and zero without the hook (see
/// `Compiler::shouldEmitScatterNormal`), so that every finite lobe
/// receives, as the Dirac estimator always has.
template <typename DrawXi>
[[nodiscard]] inline float manifoldGlossyWidth(const JIT::Material &material,
                                               bool isBackface,
                                               DrawXi &&drawXi) {
  const int dfLobes{material.getLobes(isBackface)};
  const int glossy{dfLobes & DF_GLOSSY};
  if (glossy == 0) return INFINITY;
  if (!material.def->scatterNormalSample) return 0.0f;
  // One glossy kind, per the hook's contract: the reflection kind when
  // the material has it and the transmission kind otherwise, since a
  // single reflect-transmit leaf reports the same lobe either way and a
  // layering that differs by domain answers for its reflection side,
  // which is the side the receiver's own gather evaluates.
  const int kind{(glossy & DF_GLOSSY_BRDF) != 0 ? DF_GLOSSY_BRDF
                                                : DF_GLOSSY_BTDF};
  float3 wm{};
  float pdf{};
  float2 alpha{};
  if (!material.scatterNormalSample(drawXi(), isBackface, wm, pdf, alpha, kind))
    return 0.0f;
  return std::sqrt(alpha.x * alpha.y);
}

/// The lobes a vertex whose lobe word is `dfLobes` receives a light
/// with: the ones the manifold gathers run from and value, and whose
/// share of the vertex's bounce the arrivals behind it drop. A
/// receiver's BSDF is evaluated at whatever bent direction a connection
/// lands on, and the connections toward one light spread over its
/// angular extent from the receiver, so a lobe narrower than that
/// extent makes an estimator that is zero almost always and enormous
/// otherwise, while ordinary sampling handles a narrow lobe well. So the
/// smooth (diffuse-like) lobes receive, a microfacet lobe above the
/// glossy cutoff among them (see `DF_GLOSSY_BRDF`), and the glossy
/// lobes receive when `glossyWidth` (see `manifoldGlossyWidth()`)
/// reaches `minWidth`, which a renderer sets from the light's angular
/// radius. Zero means the vertex is no receiver of that light.
///
/// The answer is a mask rather than a verdict on the vertex so that a
/// narrow coat over a wide base leaves the base receiving and the coat
/// to ordinary sampling: the gathers value the receiving lobes only,
/// and an arrival behind keeps the share of the receiver's bounce the
/// other lobes carried, the same partition the casters' claims make.
[[nodiscard]] inline int manifoldReceiverLobes(int dfLobes, float glossyWidth,
                                               float minWidth) noexcept {
  return (dfLobes & DF_SMOOTH) |
         (glossyWidth >= minWidth ? dfLobes & DF_GLOSSY : 0);
}

/// \}

} // namespace smdl
