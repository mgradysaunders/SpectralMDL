/// \file
/// The Newton solve behind manifold connections: given a receiver point
/// and a light target (a distant direction or a finite point), find the
/// points on a chain of smooth interfaces where the connection obeys
/// Snell's law or the law of reflection at every bounce, exactly for a
/// Dirac bounce or about a drawn microfacet normal for a glossy one
/// (Hanika, Droske & Fascione, "Manifold Next Event Estimation", EGSR
/// 2015; Zeltner, Georgiev & Jakob, "Specular Manifold Sampling",
/// SIGGRAPH 2020). A renderer poses each connection as a
/// `ManifoldProblem`, and `ManifoldSolver::solve()` answers with the
/// `ManifoldSolution` a walk converges to.
///
/// The solver is renderer-agnostic: it never traces a ray or interprets
/// geometry itself, and instead moves on the surfaces a renderer supplies
/// through `ManifoldSurfaces`. The transport-side work (Fresnel, medium
/// attenuation, MIS and reciprocal-probability bookkeeping) stays with
/// the renderer; this is the geometry, the measures, the test of whether
/// two solutions are one, and the eligibility questions that are
/// answerable from `jit` material instances alone.
#pragma once

#include <cmath>
#include <vector>

#include "smdl/JIT.h"

namespace smdl {

/// \addtogroup manifold
/// \{

/// The most bounces a problem may have.
///
/// A sanity bound: the walk's workspace is dynamically sizable
/// to any conceivable problem, so this is what the solver refuses
/// rather than what anything is built for.
constexpr int MANIFOLD_MAX_DEPTH{16};

/// How far a converged crossing may sit from the one a path actually
/// took and still count as the same solution, as a fraction of the
/// distance from the receiver.
///
/// A renderer that asks whether a walk reaches the crossings a path
/// took compares the two by it, and a walk stops only once its step is
/// under it, so the walk is never less precise than that comparison
/// needs.
constexpr double MANIFOLD_IDENTITY_FRACTION{1e-2};

/// How far two converged bounces of randomly started walks may sit
/// apart and still count as the same solution, as a fraction of the
/// distance from the receiver. This is the reciprocal estimates'
/// currency, not the re-walk pair's: distinct solutions of one offset
/// approach each other at every caustic fold, and a test this coarse
/// merges the pair and loses half of it, which on a surface with tens of
/// solutions per receiver read 1.8 percent dark at 1e-2. The walks it
/// judges converge on `MANIFOLD_RESIDUAL` so that a re-hit of one
/// solution reliably lands inside it; at 1e-2 they did not have to, and
/// re-hits that fell outside inflated the trial count and hid most of
/// that loss.
constexpr double MANIFOLD_SOLUTION_IDENTITY_FRACTION{1e-3};

/// The constraint residual a walk converges to, on top of the position
/// test. A solution is evaluated where its walk stopped, and what it
/// is worth is steep in that where an exit nears the critical angle or
/// the chain a fold, so a walk stops on the solution and not a step
/// beside it; and two walks to one solution stop well inside
/// `MANIFOLD_SOLUTION_IDENTITY_FRACTION` of each other.
///
/// \note
/// A glossy problem tightens this further to a fraction of its lobe,
/// see `ManifoldProblem::residualTolerance`.
constexpr float MANIFOLD_RESIDUAL{1e-5f};

/// A point pinned to one of the renderer's surfaces: where it is, and
/// the renderer's own addressing of the surface, the face, and the face
/// parameterization of the point. The solver reads only `point`; the
/// rest exists so the renderer can rebuild its own hit record from a
/// vertex the walk hands back, and means whatever its
/// `ManifoldSurfaces` implementation says it means.
///
/// This and the geometry and solution records below carry no default
/// values: they are the solver's scratch, declared without braces and
/// written whole before they are read, and a walk fills thousands of
/// them per estimate. Value-initialize (`{}`) one that must start zero.
class ManifoldVertex final {
public:
  /// The world-space point on the surface, in double. The constraint
  /// is built from the segment between two bounces, and two points a
  /// pane's thickness apart and a room's width from the origin have no
  /// digits left to difference as floats: the direction of the segment
  /// would be good to the spacing of the coordinates over its length,
  /// a floor under the residual that no step gets beneath.
  ///
  /// A vertex `ManifoldSurfaces::project()` fills is on its surface and
  /// on the cast's line to double precision. A seed may carry a point
  /// rounded to a float, a renderer's hit being one: the walk's first
  /// step re-anchors it.
  double3 point;

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
/// surface rather than the interpolated field. Where the vertex is, is
/// the vertex's own to say (`ManifoldVertex::point`).
///
/// These are floats, as a renderer's geometry is. A rounding here is a
/// surface a little different, and a connection through it is worth the
/// same to that rounding; it is the walk's own arithmetic on them that
/// must not round, see `ManifoldSolverVertex`.
class ManifoldGeometry final {
public:
  /// The shading normal: the field the material's lobes actually
  /// scatter about, which for a material that remaps `geometry.normal`
  /// is the remapped field (see `jit::MaterialDef::geometryNormalEvaluate`).
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
/// `jit::MaterialDef::geometryNormalEvaluate` and differences it.
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
  /// solution's segments are known to see their endpoints.
  ///
  /// The cast is of the whole line from `origin` through `target`, and
  /// not of the segment between them: a walk following its chain as
  /// light would aims along a direction, and its target says which way
  /// and not how far.
  ///
  /// The point of `moved` is where the line meets the surface in
  /// double, whatever precision the cast that found the surface ran at:
  /// a step that lands beside where it aimed by the rounding of a float
  /// leaves a residual of that rounding over the distance to the next
  /// bounce, which the next step cannot remove, landing no better.
  [[nodiscard]] virtual bool project(const ManifoldVertex &pin,
                                     const double3 &origin,
                                     const double3 &target,
                                     ManifoldVertex &moved) const = 0;

  /// A unit tangent at `vertex`: its `dPdu` projected off the shading
  /// normal, or any perpendicular to the normal when that is degenerate.
  /// What the walk seeds its frame from at a bounce whose seed names no
  /// `ManifoldProblemVertex::tangentAim`. The x axis when the vertex cannot be
  /// evaluated.
  [[nodiscard]] float3 tangentOf(const ManifoldVertex &vertex) const;

  /// The walk's tangent frame at `vertex`: the shading normal it
  /// constrains against and the two tangents an offset is expressed in,
  /// `tangentU` being `tangentAim` projected onto the tangent plane and
  /// `tangentV = normal x tangentU`, as the walk builds them at every
  /// iterate, to a float's rounding. False when the vertex cannot be
  /// evaluated or `tangentAim` is degenerate against the normal.
  [[nodiscard]] bool tangentSpaceOf(const ManifoldVertex &vertex,
                                    const float3 &tangentAim, //
                                    float3 &normal,           //
                                    float3 &tangentU,         //
                                    float3 &tangentV) const;
};

/// One interface of a problem: where it was hit, the absolute
/// refractive indices on the previous (receiver-facing) and next
/// (light-facing) sides (resolved by the renderer against its media as of
/// the bounce, and equal for a reflection), which side of the shading
/// normal the chain arrives on, and how the bounce scatters.
class ManifoldProblemVertex final {
public:
  /// The manifold vertex.
  ManifoldVertex vertex{};

  /// The IOR on the same side as the direction toward the previous vertex.
  float etaPrev{};

  /// The IOR on the same side as the direction toward the next vertex.
  float etaNext{};

  /// The side of the shading normal the chain arrives on, as the sign of
  /// its direction of travel at the seed (from the receiver's side toward
  /// the light's) against the normal: +1 arriving from behind the normal,
  /// -1 from in front of it.
  ///
  /// A transmission's walk keeps only a solution that still arrives from
  /// this side, so one that migrated across a silhouette is rejected
  /// rather than solved with `etaPrev` and `etaNext` swapped. A
  /// transmission must set it, because zero rejects every solution. A
  /// reflection does not read it.
  float sideSign{};

  /// The tangential half vector the bounce is solved for, in the walk's
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
  /// a solid angle onto the tangent plane. One for a Dirac bounce, which
  /// has no offset to draw and no chance of drawing it.
  float offsetDensity{1.0f};

  /// The squared roughness of the narrowest lobe of the distribution
  /// the offset was drawn from, the smaller of its two axes, in the
  /// slope units the offset is measured in: the finest scale of the
  /// density the estimate divides by at the converged half vector, which
  /// is how precisely that half vector has to match the offset. A
  /// mixture of widths is as fine as its narrowest part, whichever lobe
  /// the draw took. Zero for a Dirac bounce.
  float alpha{};

  /// The world-space vector the walk's tangent frame at this vertex is
  /// seeded from: `t1` is its projection onto the tangent plane of the
  /// iterate and `t2 = n x t1`. It is held fixed for the whole estimate so
  /// that `offset` names one world normal in every walk: the trials of the
  /// reciprocal estimate must repeat the first walk's constraint, not
  /// solve a family of constraints rotated by whatever tangent each start
  /// happens to have. Set where the offset is drawn or the start is
  /// chosen; zero means derive it from `vertex` (see
  /// `ManifoldSurfaces::tangentOf()`), which a problem whose starts never
  /// move can afford.
  float3 tangentAim{};

  /// A tangential displacement of the starting iterate, in the walk's own
  /// frame at the vertex and in units of the distance from the receiver.
  ///
  /// The walk is otherwise deterministic, which is what lets the arrival
  /// side ask whether the gather could have produced a path. A roughened
  /// problem cannot use that: with the offset held fixed the constraint
  /// has several isolated solutions, and a deterministic walk reaches
  /// exactly one of them however many there are, which over-counts by the
  /// number it cannot see. Randomizing where the walk STARTS turns "which
  /// solution" into a question with a probability, which the reciprocal
  /// estimate in the gather then counts.
  float2 seedJitter{};

  /// Is the bounce a reflection rather than a transmission?
  ///
  /// The constraint is the same either way. `H` is
  /// `etaPrev wPrev + etaNext wNext`, and with the two indices equal, as
  /// they are on a reflection, that IS the reflection half vector; only
  /// which side the two segments have to be on differs. What differs
  /// outside the constraint is where the seed comes from: a refractive
  /// bounce may lie on the straight shadow segment and be handed one,
  /// and a reflective one never does and has to be searched for, as a
  /// refractive one is too wherever the straight segment crosses nothing.
  bool isReflection{};

  /// Is this a glossy bounce rather than a Dirac one? Per bounce:
  /// a glossy one is solved for a drawn offset whose density the
  /// estimate divides out, a Dirac one for the zero offset with its
  /// `halfVectorJacobian` standing in; see
  /// `ManifoldSolution::measure()`.
  bool isGlossy{};
};

/// A problem: the seed chain of interfaces a connection is solved through,
/// in order from the receiver: the eligible crossings of the straight
/// shadow segment for a refractive connection handed them, a sampled
/// caster point for a reflective one, and for a caster refractive one the
/// sampled caster point and the crossings a ray refracted through it goes
/// on to meet.
class ManifoldProblem final {
public:
  /// The number of bounces, as an `int` because every loop over them is
  /// one and a `size_t` would sign-compare against all of them.
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

  /// Room for `depth` bounces without allocating again, which is the
  /// only allocation the problem makes: a caller that reserves once and
  /// refills in place never allocates after that. It never shrinks.
  void reserve(int depth) { vertices.reserve(static_cast<size_t>(depth)); }

  /// Empty the problem and leave room for `depth`: a problem reused from
  /// a caller's scratch, as clean as a default-constructed one.
  void restart(int depth) {
    vertices.clear();
    reserve(depth);
    residualTolerance = 0.0f;
    isThorough = false;
  }

  /// Admit one bounce and hand back the cleared seed to fill. A
  /// filler that then refuses it calls `pop()`. The reference is good
  /// until an `append()` outgrows the reserve, so reserve for every
  /// bounce first, which `restart()` does.
  [[nodiscard]] ManifoldProblemVertex &append() {
    return vertices.emplace_back();
  }

  /// Drop the bounce `append()` just admitted.
  void pop() noexcept { vertices.pop_back(); }

public:
  /// The bounces, whose length is the problem's size: there is no
  /// separate count to disagree with them.
  std::vector<ManifoldProblemVertex> vertices{};

  /// The constraint residual a walk of this problem must reach where
  /// that is less than `MANIFOLD_RESIDUAL`, which every walk reaches;
  /// zero asks for no less. A glossy problem asks for a fraction of its
  /// lobe width: the estimate evaluates the interface distribution at
  /// the converged half vector and divides by the density of the drawn
  /// one, so the two must agree to a fraction of the lobe.
  float residualTolerance{};

  /// Does a walk that fails walk again, by every way
  /// `ManifoldSolver::solve()` has of walking? A walk reaches the
  /// solution whose basin its start is in, and no one way of walking
  /// is in every basin. For a problem that is the one chance at its
  /// solution, seeded where a straight line crosses, whose failure
  /// leaves that light path to whatever else finds it. Not for a
  /// problem started at random, where a walk that fails is one trial
  /// more and costs what it costs over again.
  bool isThorough{};
};

/// One interface of a solution.
class ManifoldSolutionVertex final {
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

  /// The measure `|d h / d omega_next|` of this bounce: how much
  /// tangential half vector a unit of outgoing solid angle is worth,
  /// holding the arriving direction and the vertex fixed.
  ///
  /// This is what a Dirac bounce contributes to the offset Jacobian in
  /// place of the density a glossy one has, since the Dirac delta that
  /// collapses its two dimensions is expressed in direction and the walk
  /// works in half vectors.
  float halfVectorJacobian;
};

/// A solution: the connection a walk converged to.
class ManifoldSolution final {
public:
  /// The number of bounces; see `ManifoldProblem::size()`.
  [[nodiscard]] int size() const noexcept {
    return static_cast<int>(vertices.size());
  }

  /// Room for `depth` bounces without allocating again; see
  /// `ManifoldProblem::reserve()`.
  void reserve(int depth) { vertices.reserve(static_cast<size_t>(depth)); }

  /// Give the solution exactly `count` bounces, keeping whatever
  /// room it has and, as the records themselves do, leaving them
  /// uninitialized: `ManifoldSolver::solve()` writes every field of
  /// every bounce it keeps. Reused across an estimate's walks, which
  /// all have one length, this does nothing after the first.
  void resize(int count) { vertices.resize(static_cast<size_t>(count)); }

  /// The vertex point at the given bounce index.
  [[nodiscard]] const double3 &pointAt(int i) const {
    SMDL_DEBUG_CHECK(i >= 0);
    SMDL_DEBUG_CHECK(i < size());
    return vertices[i].vertex.point;
  }

public:
  /// The vertices.
  std::vector<ManifoldSolutionVertex> vertices;

  /// The unit direction from the receiver toward the first vertex.
  float3 wr;

  /// The offset Jacobian: the measure of the nested outgoing solid angles
  /// per unit of the variables the solution is drawn in, which are the
  /// light direction and one tangential half vector per bounce.
  ///
  ///     [prod_i cosPrev_i / distPrev_i^2] . [prod_i A_i] / |det J| . R
  ///
  /// with `A_i` the area element of the parameterization the constraint
  /// Jacobian is expressed in, so that `prod A_i / |det J|` is invariant to
  /// that choice, and `R` the correction from the straight-line geometry
  /// term the light sampler measured in to the one the solution's last
  /// segment actually arrives with.
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

  /// The solution's measure for the problem it solves: the offset
  /// Jacobian times, at every Dirac bounce, the half-vector measure
  /// that stands in for the density a glossy bounce divides out. A
  /// glossy bounce contributes a drawn half vector the caller divides
  /// by `offsetDensity`; a Dirac bounce has no draw, its Dirac delta
  /// collapses the two half-vector dimensions instead, and
  /// `halfVectorJacobian` converts that collapse into the outgoing solid
  /// angle. For an all-Dirac problem this is the transfer Jacobian
  /// `|d omega_r / d omega_l|` whole, so every problem, pure or mixed,
  /// carries one measure.
  [[nodiscard]] float measure(const ManifoldProblem &problem) const noexcept {
    float result{offsetJacobian};
    const int numBounces{size()};
    for (int i = 0; i < numBounces; i++)
      if (!problem.vertices[i].isGlossy)
        result *= vertices[i].halfVectorJacobian;
    return result;
  }
};

/// What tells one solution from another for `isSameManifoldSolution()`:
/// where its bounces land, which is all the comparison reads, so a set
/// of found solutions keeps these rather than whole solutions. Scratch
/// like the solution itself: `set()` rewrites it whole.
class ManifoldSolutionKey final {
public:
  /// Extract the vertex points from the given solution.
  void set(const ManifoldSolution &solution) {
    const int numBounces{solution.size()};
    points.resize(static_cast<size_t>(numBounces));
    for (int i = 0; i < numBounces; i++)
      points[i] = solution.vertices[i].vertex.point;
  }

  /// The number of bounces; see `ManifoldProblem::size()`.
  [[nodiscard]] int size() const noexcept {
    return static_cast<int>(points.size());
  }

  /// The vertex point at the given bounce index.
  [[nodiscard]] const double3 &pointAt(int i) const {
    SMDL_DEBUG_CHECK(i >= 0);
    SMDL_DEBUG_CHECK(i < size());
    return points[i];
  }

public:
  /// The vertex points.
  std::vector<double3> points;
};

/// Are the solutions of two randomly started walks the same one?
/// Compared by where the bounces land, within
/// `MANIFOLD_SOLUTION_IDENTITY_FRACTION` of the receiver distance.
template <typename SolutionA, typename SolutionB>
[[nodiscard]] inline bool isSameManifoldSolution(const double3 &receiver,    //
                                                 const SolutionA &solutionA, //
                                                 const SolutionB &solutionB) {
  static_assert(std::is_same_v<SolutionA, ManifoldSolution> ||
                std::is_same_v<SolutionA, ManifoldSolutionKey>);
  static_assert(std::is_same_v<SolutionB, ManifoldSolution> ||
                std::is_same_v<SolutionB, ManifoldSolutionKey>);
  if (solutionA.size() != solutionB.size()) return false;
  const int numBounces{static_cast<int>(solutionA.size())};
  for (int i = 0; i < numBounces; i++) {
    const auto &pointA{solutionA.pointAt(i)};
    const auto &pointB{solutionB.pointAt(i)};
    if (!(length(pointB - pointA) <
          MANIFOLD_SOLUTION_IDENTITY_FRACTION *
              std::max(1e-3, length(pointA - receiver))))
      return false;
  }
  return true;
}

/// The light side of a manifold connection: a distant direction (the
/// environment) or a finite light point (a punctual light or a point
/// on an area light). `wl` is always the unit direction of the
/// STRAIGHT segment from the receiver, which for a finite target must
/// equal `normalize(point - receiver)`.
class ManifoldTarget final {
public:
  float3 wl{};
  double3 point{};
  /// The light surface normal at `point`, or zero when the target has no
  /// orientation, which is every distant and punctual one. Only the offset
  /// Jacobian reads it, to carry the light-side geometry term across from
  /// the straight line to the segment that actually arrives.
  float3 normal{};
  bool isInfinite{true};
};

/// What a solve did, for the caller's statistics: the steps it took,
/// over every walk it tried, and of its last walk the constraint
/// residual where it stopped and how it ended.
class ManifoldWalkReport final {
public:
  enum class Outcome {
    CONVERGED, ///< Converged to a valid bounce at every vertex.
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

/// The walk's iterate at one vertex of the chain: the differential
/// geometry it last re-anchored to, the two segments meeting there, and
/// the generalized half vector and tangent frame the constraint is
/// expressed in.
///
/// Everything the walk derives is a double, whatever the geometry it
/// derives it from. The constraint Jacobian's entries for two bounces
/// close together are of the order of one over the distance between
/// them, and cancel in its determinant to what the rest of the chain
/// leaves, of the order of one over the distance to the receiver or the
/// focal length of the glass. Rounded each on its own as floats, the
/// measure would be good to a float's rounding times the ratio of the
/// two, which through a wall of glass is in the thousands.
///
/// This is `ManifoldSolver`'s element rather than anything a caller
/// reads. It is here, and not in the solver's source, only so that the
/// workspace can hold it by value; a walk writes every field it reads and
/// nothing past the problem's own size, so it carries no meaning between
/// walks. Like the records above it carries no default values.
class ManifoldSolverVertex final {
public:
  ManifoldGeometry geometry;

  /// The tangents the constraint projects onto, built from the seed
  /// vector the walk holds fixed.
  double3 t1;
  double3 t2;

  double3 wPrev;   ///< The direction toward the previous vertex or receiver.
  double3 wNext;   ///< The direction toward the next vertex or light.
  double3 hHat;    ///< The generalized half vector.
  double distPrev; ///< The distance to the previous vertex.
  double distNext; ///< The distance to the next vertex or 0 if infinite.
  double hLen;     ///< The generalized half vector length before normalizing.

  /// The sign that orients `hHat` onto the shading normal's side, so
  /// that the constraint means a microfacet normal rather than a line
  /// through one.
  double hSign;
};

/// The manifold solver: the walk that solves a problem, and the workspace
/// it runs in, sized once for the deepest problem a render can pose and
/// reused by every walk after, so that a walk allocates nothing however
/// deep the problem.
///
/// Not thread safe: give each thread its own, as `BumpPtrAllocator`
/// asks. A renderer buys one per thread or per block of work, and every
/// solve that thread runs goes through it.
///
/// The buffers are the solver's own and carry no meaning between walks:
/// a walk writes every entry it reads and touches nothing past the
/// problem's size, so whatever the last walk left behind never matters.
class SMDL_EXPORT ManifoldSolver final {
public:
  ManifoldSolver() = default;

  explicit ManifoldSolver(int depth) { reserve(depth); }

  /// Non-copyable: it is a thread's workspace, never a value.
  ManifoldSolver(const ManifoldSolver &) = delete;

  ManifoldSolver &operator=(const ManifoldSolver &) = delete;

  /// Size the workspace for problems of up to `depth` bounces, which is
  /// the only allocation this class makes. It never shrinks, so a
  /// workspace reserved for a deeper problem serves a shallower one, and
  /// a depth already covered costs one predicted branch. A solve
  /// reserves for its own problem, so a caller that reserves for the
  /// render's depth up front is buying the allocation at a moment of
  /// its choosing rather than avoiding one.
  void reserve(int depth) {
    if (depth > mMaxDepth) grow(depth);
  }

  /// Solve `problem` into `solution`: the connection from `receiver` to
  /// the light target through its seed chain, by damped Newton iteration
  /// on the block-coupled per-vertex constraints.
  ///
  /// A walk starts from the seed as handed, takes its step at the first
  /// bounce, and follows the chain from there as light would, each
  /// bounce after it where the direction scattered at the one before
  /// lands (the manifold walk of Jakob & Marschner, "Manifold
  /// Exploration", SIGGRAPH 2012). Every iterate after the start then
  /// obeys its constraint at every bounce but the last, however weak an
  /// interface and however thin a wall.
  ///
  /// A problem that asks (`ManifoldProblem::isThorough`) is walked three
  /// ways, each where the one before failed: from the seed traced from
  /// its first bounce, as every step is; from the seed as handed; and
  /// from the seed as handed with every bounce stepping for itself.
  /// Where glass bends light much the solution is far from where a
  /// straight line crosses and nearer a trace of that; where light
  /// leaves its glass nearly straight, as through a hollow vessel, it is
  /// the other way round.
  ///
  /// Every landing is a cast through `ManifoldSurfaces::project()` from
  /// the bounce before, so a solution's segments are known to see their
  /// endpoints, up to whatever the renderer's projection
  /// passes through. Returns true on convergence to a valid bounce on
  /// the seed's own side of every interface; failure (divergence, leaving
  /// a seed surface, total internal reflection, a silhouette migration, a
  /// grazing or degenerate frame) means no contribution, never a wrong
  /// one. `report`, if given, receives what the solve did either way.
  /// The workspace is reserved for this problem if it was not already.
  [[nodiscard]] bool
  solve(const ManifoldSurfaces &surfaces, const double3 &receiver,
        const ManifoldTarget &target, const ManifoldProblem &problem,
        ManifoldSolution &solution, ManifoldWalkReport *report = nullptr);

private:
  /// Where a walk starts and how it steps. Opaque here, so that the ways
  /// of walking stay the solver's own.
  enum class Walk;

  /// One damped Newton walk of the chain, started and stepped the way
  /// `walk` names.
  [[nodiscard]] bool walkChain(Walk walk, const ManifoldSurfaces &surfaces,
                               const double3 &receiver,
                               const ManifoldTarget &target,
                               const ManifoldProblem &problem,
                               ManifoldSolution &solution,
                               ManifoldWalkReport &report);

  /// The out-of-line half of `reserve()`, so that the common call is a
  /// branch rather than a call across a shared library boundary.
  void grow(int depth);

private:
  /// The deepest problem the buffers below are sized for, 0 until
  /// `reserve()`.
  int mMaxDepth{};

  /// The walk's own iterate, then the trial step's: `mMaxDepth` apiece.
  std::vector<ManifoldVertex> mVertices;

  /// The fixed frame seeds, then the world-space Newton steps:
  /// `mMaxDepth` apiece.
  std::vector<double3> mVectors;

  /// The two chain states the iteration swaps between: `mMaxDepth`
  /// apiece.
  std::vector<ManifoldSolverVertex> mIterates;

  /// The tangent-frame lengths of one iterate, `mMaxDepth`.
  std::vector<double> mFrameLengths;

  /// The two states' constraint residuals, `2 * mMaxDepth` apiece.
  std::vector<double> mConstraints;

  /// The two states' constraint Jacobians and the copy the solve
  /// eliminates in place, `2 * mMaxDepth` rows of a fixed band width
  /// apiece. The constraints couple neighbours only, so the system is
  /// banded and nothing here grows faster than the depth.
  std::vector<double> mJacobians;

  /// The solve's right-hand side, `2 * mMaxDepth`.
  std::vector<double> mRhs;
};

/// What manifold estimators claim at an instance: the Dirac lobes of its
/// material they estimate, which the renderer's path tracer then leaves
/// to them.
///
/// A glossy lobe is never claimed, and is left to ordinary sampling. The
/// Dirac reflection is claimed only on an instance the renderer marks,
/// and the Dirac transmission on any solid that bends, so that a chain
/// through a marked instance may cross unmarked glass on its way (a
/// glass cover over a lamp). The claim is a static, whole-tree question
/// asked of one side's `df_lobes`, so it can only say that the material
/// HAS a kind on the side asked, never that a given bounce reaches it;
/// both halves of the estimator confirm that with a masked query at the
/// converged geometry.
class ManifoldClaim final {
public:
  /// `DF_DIRAC_BRDF`, or zero.
  int reflectLobes{};

  /// `DF_DIRAC_BTDF`, or zero.
  int refractLobes{};

  [[nodiscard]] bool empty() const noexcept { return lobes() == 0; }

  [[nodiscard]] int lobes() const noexcept {
    return reflectLobes | refractLobes;
  }
};

/// The claim at an instance whose evaluated material is `material`, with its
/// exterior IOR already resolved (for the index contrast), on the side
/// `isBackface` names, which is the side the scattering functions
/// dispatch on and a caller spells `jit::Material::isInterior(wo)`,
/// `isMarked` being the renderer's mark on the instance.
///
/// The side is asked because a two-sided material scatters by a
/// different tree on each of them. Claiming the union bars the path
/// tracer from a kind on the side that cannot produce it while the
/// gather's own draw fails there, and the transport falls between the
/// two.
///
/// A material that remaps `geometry.normal` (statically, see
/// `jit::MaterialDef::canRemapNormal()`) claims only when the walk can
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
[[nodiscard]] SMDL_EXPORT ManifoldClaim
manifoldClaim(const jit::Material &material, bool isBackface, bool isMarked);

/// The claim on either side, the union of the two: what a caller with no
/// one side in hand asks, a load-time enumeration of marked instances
/// among them, where a walk's starts may land on either side of the
/// instance and the masked query at the converged bounce settles which
/// one actually scatters.
[[nodiscard]] SMDL_EXPORT ManifoldClaim
manifoldClaim(const jit::Material &material, bool isMarked);

/// The narrowest width of the glossy lobes of `material` on the side
/// `isBackface`, read from the normal hook, which reports it whichever
/// lobe its draw takes; `drawXi` is a callable producing the `float4`
/// for that draw, consulted only when there is a glossy lobe to read,
/// so a renderer's deterministic sampler advances exactly then. The
/// width is the squared roughness, the slope of the lobe's half-width,
/// which is what `manifoldReceiverLobes()` judges against a light's
/// angular radius. `INFINITY` without a glossy lobe, so that the
/// question never arises, and zero without the hook (see
/// `Compiler::shouldEmitScatterNormal`), so that a glossy lobe whose
/// width cannot be read receives no light with an angular extent.
template <typename DrawXi>
[[nodiscard]] inline float manifoldGlossyWidth(const jit::Material &material,
                                               bool isBackface,
                                               DrawXi &&drawXi) {
  const int gloss{material.getLobes(isBackface) & DF_GLOSS};
  if (gloss == 0) return INFINITY;
  if (!material.def->scatterNormalSample) return 0.0f;
  // One glossy kind, per the hook's contract: the reflection kind when
  // the material has it and the transmission kind otherwise, since a
  // single reflect-transmit leaf reports the same lobe either way and a
  // layering that differs by domain answers for its reflection side,
  // which is the side the receiver's own gather evaluates.
  const int kind{(gloss & DF_GLOSS_BRDF) != 0 ? DF_GLOSS_BRDF : DF_GLOSS_BTDF};
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
/// matte (diffuse-like) lobes receive, a microfacet lobe above the
/// glossy cutoff among them (see `DF_GLOSS_BRDF`), and the glossy
/// lobes receive when `glossyWidth` (see `manifoldGlossyWidth()`)
/// reaches `minWidth`, which a renderer sets from the light's angular
/// radius. Zero means the vertex is no receiver of that light.
///
/// The answer is a mask rather than a verdict on the vertex so that a
/// narrow coat over a wide base leaves the base receiving and the coat
/// to ordinary sampling: the gathers value the receiving lobes only,
/// and an arrival behind keeps the share of the receiver's bounce the
/// other lobes carried.
[[nodiscard]] inline int manifoldReceiverLobes(int dfLobes, float glossyWidth,
                                               float minWidth) noexcept {
  return (dfLobes & DF_MATTE) |
         (glossyWidth >= minWidth ? dfLobes & DF_GLOSS : 0);
}

/// \}

} // namespace smdl
