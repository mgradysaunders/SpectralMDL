#include "smdl/Manifold.h"

#include "smdl/Support/Span.h"

#include <algorithm>
#include <cmath>
#include <iomanip>

namespace smdl {

ManifoldSurfaces::~ManifoldSurfaces() = default;

namespace {

// The walk fails cleanly rather than loop or wander: iteration and
// step-halving budgets.
constexpr int MAX_ITERATIONS{64};

constexpr int MAX_HALVINGS{5};

// A step can also fall small because the Jacobian is nearly singular
// rather than because the walk has arrived, so the residual still has to
// be plausible for a solution. Loose on purpose: it rejects a stuck
// walk, not an imprecise one.
constexpr float RESIDUAL_SANITY{1e-3f};

// The pivot the dense solve refuses to divide by, as a fraction of the
// largest entry of the system. Absolute would not do: the Jacobian
// entries carry units of inverse distance, so one geometry measured in
// millimeters and the same in kilometers would land on opposite sides of
// a fixed number. A pivot this far below the scale of the matrix is
// noise at float precision, and the walk is better off failing.
constexpr float MIN_PIVOT_RELATIVE{1e-7f};

// The dimension of the coupled constraint system.
constexpr int MAX_DIM{2 * MANIFOLD_MAX_DEPTH};

class ConstraintVector final {
public:
  [[nodiscard]] auto &operator[](int i) noexcept { return coeffs[i]; }

  [[nodiscard]] auto &operator[](int i) const noexcept { return coeffs[i]; }

  alignas(32) std::array<float, static_cast<size_t>(MAX_DIM)> coeffs;
};

class ConstraintMatrix final {
public:
  [[nodiscard]] auto &operator()(int i, int j) noexcept {
    return coeffs[i * MAX_DIM + j];
  }
  [[nodiscard]] auto &operator()(int i, int j) const noexcept {
    return coeffs[i * MAX_DIM + j];
  }
  alignas(32) std::array<float, static_cast<size_t>(MAX_DIM *MAX_DIM)> coeffs;
};

// Solve `A x = b` in place by Gaussian elimination with partial
// pivoting, for `n` unknowns and however many right-hand sides `b`
// holds. Returns false on a (numerically) singular system. `det`, if
// given, receives the determinant, which is the signed product of the
// pivots the elimination leaves on the diagonal and so costs nothing to
// report.
[[nodiscard]] bool solveDense(int n, ConstraintMatrix &A,
                              Span<ConstraintVector> b = {},
                              float *det = nullptr) {
  float scale{};
  for (int r = 0; r < n; r++)
    for (int c = 0; c < n; c++) scale = std::max(scale, std::abs(A(r, c)));
  if (!(scale > 0.0f)) return false;
  const float minPivot{scale * MIN_PIVOT_RELATIVE};
  if (det) *det = 1.0f;
  for (int c = 0; c < n; c++) {
    int pivot{c};
    for (int r = c + 1; r < n; r++)
      if (std::abs(A(r, c)) > std::abs(A(pivot, c))) pivot = r;
    if (!(std::abs(A(pivot, c)) > minPivot)) return false;
    if (pivot != c) {
      for (int k = c; k < n; k++) std::swap(A(c, k), A(pivot, k));
      for (ConstraintVector &x : b) std::swap(x[c], x[pivot]);
      if (det) *det = -*det;
    }
    for (int r = c + 1; r < n; r++) {
      const float m{A(r, c) / A(c, c)};
      if (m == 0.0f) continue;
      for (int k = c; k < n; k++) A(r, k) -= m * A(c, k);
      for (ConstraintVector &x : b) x[r] -= m * x[c];
    }
  }
  if (det)
    for (int c = 0; c < n; c++) *det *= A(c, c);
  if (!b.empty()) {
    for (int c = n - 1; c >= 0; c--) {
      for (ConstraintVector &x : b) {
        float sum{x[c]};
        for (int j = c + 1; j < n; j++) sum -= A(c, j) * x[j];
        x[c] = sum / A(c, c);
      }
    }
  }
  return true;
}

// One Newton iterate over the whole chain: the differential geometry at
// every vertex, the per-vertex constraints (each generalized half
// vector projected into its tangent plane), and the coupled Jacobian
// over the surface parameterizations, including the frame terms the
// shading-normal derivatives induce, which is what buys quadratic
// convergence on normal-interpolated meshes. Constraint `i` couples
// vertices `i-1`, `i`, and `i+1`, so the system is block tridiagonal;
// at this size a dense solve is simpler and just as fast.
class ManifoldChainState final {
public:
  // The slice of the iterate at one vertex of the chain.
  struct Vertex final {
    ManifoldGeometry geometry{};
    float3 wPrev{}; // Toward the previous vertex, or the receiver.
    float3 wNext{}; // Toward the next vertex, or the light.
    float distPrev{};
    float distNext{}; // 0 for the distant light.
    float3 hHat{};
    float hLen{};
    // The sign that orients `Hhat` onto the shading normal's side, so that
    // the constraint means a microfacet normal rather than a line through
    // one. Zero offsets do not care, which is why it never mattered before.
    float hSign{};
    // The area element of the parameterization the Jacobian is expressed in,
    // and the half-vector measure of the crossing; see the header.
    float areaElement{};
    float halfVectorJacobian{};
    float3 t1{};
    float3 t2{};
  };

  [[nodiscard]] auto &operator[](int i) noexcept { return vertices[i]; }
  [[nodiscard]] auto &operator[](int i) const noexcept { return vertices[i]; }

  [[nodiscard]] float residual() const noexcept {
    float sum{};
    for (int i = 0; i < 2 * count; i++) sum += C[i] * C[i];
    return std::sqrt(sum);
  }

public:
  std::array<Vertex, MANIFOLD_MAX_DEPTH> vertices{};
  int count{};

  ConstraintVector C{};
  ConstraintMatrix J{};
};

// The derivative of a unit direction `w = (q - p)/d` with respect to a
// perturbation `dp` of the base point `p`: the tangential projector
// over the distance, negated. A perturbation of the far point `q` is
// the same expression with the opposite sign.
[[nodiscard]]
float3 unitDirDeriv(const float3 &w, float d, const float3 &dp) {
  return -(dp - dot(w, dp) * w) / d;
}

[[nodiscard]]
bool evaluateChain(
    const ManifoldSurfaces &surfaces, const float3 &receiver,
    const ManifoldTarget &target, const ManifoldChain &chain,
    const std::array<float3, MANIFOLD_MAX_DEPTH> &frameSeeds,
    const std::array<ManifoldVertex, MANIFOLD_MAX_DEPTH> &vertices,
    ManifoldChainState &chainState) {
  const auto count{chain.count};
  chainState.count = count;
  for (int i = 0; i < count; i++)
    if (!surfaces.geometry(vertices[i], chainState[i].geometry)) return false;
  // Segment directions, half vectors, frames, and constraints. The last
  // vertex's next segment is the distant light direction (whose zero
  // distance drops the position-derivative term below) or the segment
  // to the finite light point (whose derivative term the shared
  // formula picks up through the real distance).
  std::array<float, MANIFOLD_MAX_DEPTH> gLen{};
  for (int i = 0; i < count; i++) {
    auto &sv{chainState[i]};
    const auto &geometry{sv.geometry};
    const float3 prev{i == 0 ? receiver : chainState[i - 1].geometry.point};
    auto toPrev{prev - geometry.point};
    sv.distPrev = length(toPrev);
    if (!(sv.distPrev > 1e-6f)) return false;
    sv.wPrev = toPrev / sv.distPrev;
    if (i + 1 < count) {
      auto toNext{chainState[i + 1].geometry.point - geometry.point};
      sv.distNext = length(toNext);
      if (!(sv.distNext > 1e-6f)) return false;
      sv.wNext = toNext / sv.distNext;
    } else if (target.infinite) {
      sv.wNext = target.wl;
      sv.distNext = 0.0f;
    } else {
      auto toLight{target.point - geometry.point};
      sv.distNext = length(toLight);
      if (!(sv.distNext > 1e-6f)) return false;
      sv.wNext = toLight / sv.distNext;
    }
    // The generalized half vector of refraction, parallel to the normal
    // exactly when the segment pair obeys Snell's law.
    const float3 h{chain[i].etaPrev * sv.wPrev + chain[i].etaNext * sv.wNext};
    sv.hLen = length(h);
    if (!(sv.hLen > 1e-6f)) return false;
    sv.hHat = h / sv.hLen;
    // `H` points into the denser medium, so which side it lands on depends
    // on which side is denser. Orienting it onto the shading normal makes
    // it the microfacet normal the interface's own distribution is
    // expressed in, which is what an offset has to be measured against.
    sv.hSign = dot(sv.hHat, geometry.normal) < 0.0f ? -1.0f : 1.0f;
    sv.areaElement = length(cross(geometry.dPdu, geometry.dPdv));
    // `|d h / d omega_next|`, being the refraction half-vector Jacobian
    // times the cosine that converts its solid angle into the projected
    // measure the constraint lives in.
    sv.halfVectorJacobian =
        absDot(sv.hHat, geometry.normal) * absDot(sv.wNext, sv.hHat) *
        (chain[i].etaNext * chain[i].etaNext) / (sv.hLen * sv.hLen);
    // The tangent frame the constraint projects onto, seeded from a
    // vector held FIXED for the whole walk, so the frame varies only
    // through the shading normal and the frame derivatives below are
    // exact. Seeding from the local dPdu instead would rotate the frame
    // with the parameterization itself, a variation dNdu cannot see (a
    // flat cap rotates its azimuthal tangent with zero dN), and the
    // resulting Jacobian error stalls the walk.
    const float3 n{geometry.normal};
    const float3 g{frameSeeds[i] - dot(n, frameSeeds[i]) * n};
    gLen[i] = length(g);
    if (!(gLen[i] > 1e-6f)) return false;
    sv.t1 = g / gLen[i];
    sv.t2 = cross(n, sv.t1);
    // The constraint is the oriented tangential half vector against the
    // offset this crossing is solved for. At zero offset the sign cancels
    // out of every use, since the Newton step scales a row of the matrix
    // and of the right-hand side together.
    const float2 &offset{chain[i].offset};
    chainState.C[2 * i + 0] = sv.hSign * dot(sv.hHat, sv.t1) - offset.x;
    chainState.C[2 * i + 1] = sv.hSign * dot(sv.hHat, sv.t2) - offset.y;
  }
  // The coupled Jacobian. Constraint `i` sees vertex `j` through the
  // segment directions (and, for `j == i`, through its own frame, whose
  // variation the shading-normal derivatives induce; the seed vector
  // dPdu is treated as constant, which drops second-order surface terms
  // the geometry query cannot provide).
  for (int r = 0; r < 2 * count; r++)
    for (int c = 0; c < 2 * count; c++) chainState.J(r, c) = 0.0f;
  for (int i = 0; i < count; i++) {
    const auto &seed{chain[i]};
    const auto &seedv{chainState[i]};
    const float3 n{seedv.geometry.normal};
    for (int j = std::max(i - 1, 0); j <= std::min(i + 1, count - 1); j++) {
      const std::array<float3, 2> dPde{chainState[j].geometry.dPdu,
                                       chainState[j].geometry.dPdv};
      for (int k = 0; k < 2; k++) {
        float3 dh{};
        if (j == i) {
          dh =
              seed.etaPrev * unitDirDeriv(seedv.wPrev, seedv.distPrev, dPde[k]);
          if (seedv.distNext > 0)
            dh += seed.etaNext *
                  unitDirDeriv(seedv.wNext, seedv.distNext, dPde[k]);
        } else if (j == i - 1) {
          dh = -seed.etaPrev *
               unitDirDeriv(seedv.wPrev, seedv.distPrev, dPde[k]);
        } else {
          dh = -seed.etaNext *
               unitDirDeriv(seedv.wNext, seedv.distNext, dPde[k]);
        }
        const float3 dhHat{(dh - dot(seedv.hHat, dh) * seedv.hHat) /
                           seedv.hLen};
        float term1{dot(dhHat, seedv.t1)};
        float term2{dot(dhHat, seedv.t2)};
        if (j == i) {
          const float3 a{frameSeeds[i]};
          const float3 dn{k == 0 ? seedv.geometry.dNdu : seedv.geometry.dNdv};
          const float3 dg{-(dot(n, a) * dn + dot(dn, a) * n)};
          const float3 dt1{(dg - dot(seedv.t1, dg) * seedv.t1) / gLen[i]};
          const float3 dt2{cross(dn, seedv.t1) + cross(n, dt1)};
          term1 += dot(seedv.hHat, dt1);
          term2 += dot(seedv.hHat, dt2);
        }
        // The same orientation the constraint carries, applied to the
        // whole row so that a sign flip cancels out of every solve.
        chainState.J(2 * i + 0, 2 * j + k) = seedv.hSign * term1;
        chainState.J(2 * i + 1, 2 * j + k) = seedv.hSign * term2;
      }
    }
  }
  return true;
}

// The offset Jacobian of an evaluated chain: the measure of the nested
// outgoing solid angles per unit of the variables the connection is
// drawn in. See the header for the expression.
//
// The determinant is the constraint Jacobian's, so it is expressed in the
// surface parameterization, and the area elements convert it back out.
// Their ratio is what is invariant; neither factor is on its own.
[[nodiscard]] bool computeOffsetJacobian(const ManifoldChainState &chainState,
                                         const float3 &receiver,
                                         const ManifoldTarget &target,
                                         float &offsetJacobian) {
  const int count{chainState.count};
  const int last{count - 1};
  const int dim{2 * count};
  ConstraintMatrix A;
  for (int r = 0; r < dim; r++)
    for (int c = 0; c < dim; c++) A(r, c) = chainState.J(r, c);
  float detJ{};
  if (!solveDense(dim, A, {}, &detJ)) return false;
  if (!(std::abs(detJ) > 0.0f)) return false;
  float factor{1.0f / std::abs(detJ)};
  for (int i = 0; i < count; i++) {
    const auto &sv{chainState[i]};
    if (!(sv.distPrev > 0.0f)) return false;
    // Projected against the geometric normal: the area-to-solid-angle
    // factor is a property of the facet, not of the interpolated normal
    // the constraint is solved against.
    factor *= absDot(sv.wPrev, sv.geometry.Ng) * sv.areaElement /
              (sv.distPrev * sv.distPrev);
  }
  // The light-side correction, carrying the geometry term across from the
  // straight line the sampler measured in to the segment that arrives. A
  // distant target needs none, since its direction is the straight one.
  // An oriented target's patch lies on its own emitter surface, so the
  // two projections are against that surface's normal; an unoriented
  // (punctual) target's patch lies on the plane perpendicular to the
  // straight line, which projects with 1 on the straight side and with
  // however far the chain bent the arriving segment on the other. On a
  // refractive chain that bend is nearly nothing, which is why the
  // selftest's finite mirror is what caught its absence.
  if (!target.infinite) {
    const auto &sv{chainState[last]};
    const float distStraight{length(target.point - receiver)};
    const float distNext{sv.distNext};
    if (!(distStraight > 0.0f) || !(distNext > 0.0f)) return false;
    const float distFactor{distStraight / distNext};
    factor *= distFactor * distFactor;
    if (lengthSquared(target.normal) > 0.0f) {
      const float cosStraight{absDot(target.normal, target.wl)};
      const float cosNext{absDot(target.normal, sv.wNext)};
      if (!(cosStraight > 0.0f)) return false;
      factor *= cosNext / cosStraight;
    } else {
      factor *= absDot(target.wl, sv.wNext);
    }
  }
  offsetJacobian = factor;
  return std::isfinite(offsetJacobian) && offsetJacobian > 0.0f;
}

// The fixed frame-seed vectors of a chain: each seed's own, or one derived
// from the seed vertex when the seed carries none. Never from a start the
// jitter has moved, since the frame has to be the same in every walk of an
// estimate; see `ManifoldVertexSeed::frameSeed`.
void buildFrameSeeds(const ManifoldSurfaces &surfaces,
                     const ManifoldChain &chain,
                     std::array<float3, MANIFOLD_MAX_DEPTH> &frameSeeds) {
  for (int i = 0; i < chain.count; i++) {
    const float3 &seed{chain[i].frameSeed};
    frameSeeds[i] = lengthSquared(seed) > 0.0f
                        ? seed
                        : manifoldFrameSeed(surfaces, chain[i].vertex);
  }
}

} // namespace

ManifoldClaim manifoldClaim(const JIT::MaterialInstance &mat, bool backface,
                            bool marked, float maxGlossyAlpha) {
  ManifoldClaim claim{};
  if (mat.hasEmission()) return claim;
  const int dfLobes{mat.getLobes(backface)};
  // A df node scattering about a normal it was given is a field the walk
  // does not solve for, and under a remapped `geometry.normal` even a
  // given normal equal to the state normal detaches, that not being the
  // remapped field. A node left defaulted inherits the field and bars
  // nothing. A remap without the hook has no field to read at all. See
  // the header.
  if ((dfLobes & DF_SETS_NORMAL) != 0) return claim;
  if (mat.material->remapsNormal() && (!mat.material->geometryNormalEvaluate ||
                                       (dfLobes & DF_CAN_SET_NORMAL) != 0))
    return claim;
  const bool bends{!mat.isThinWalled() &&
                   std::abs(mat.getIOR() - mat.getExteriorIOR()) > 1e-4f};
  if (bends)
    claim.refractLobes =
        dfLobes & (DF_DIRAC_BTDF | (marked ? DF_GLOSSY_BTDF : 0));
  if (marked) claim.reflectLobes = dfLobes & (DF_DIRAC_BRDF | DF_GLOSSY_BRDF);
  // The width gate, at a FIXED center draw so every evaluation of the
  // claim on this side answers the same however the two halves of the
  // estimator reached it; see the header.
  if (maxGlossyAlpha > 0.0f && mat.material->scatterNormalSample &&
      (claim.lobes() & DF_GLOSSY) != 0) {
    auto tooWide{[&](int kind) {
      float3 wm{};
      float pdf{};
      float2 alpha{};
      if (!mat.scatterNormalSample(float4(0.5f, 0.5f, 0.5f, 0.5f), backface, wm,
                                   pdf, alpha, kind))
        return false;
      return std::min(alpha.x, alpha.y) > maxGlossyAlpha;
    }};
    if ((claim.refractLobes & DF_GLOSSY_BTDF) != 0 && tooWide(DF_GLOSSY_BTDF))
      claim.refractLobes &= ~DF_GLOSSY_BTDF;
    if ((claim.reflectLobes & DF_GLOSSY_BRDF) != 0 && tooWide(DF_GLOSSY_BRDF))
      claim.reflectLobes &= ~DF_GLOSSY_BRDF;
  }
  return claim;
}

ManifoldClaim manifoldClaim(const JIT::MaterialInstance &mat, bool marked,
                            float maxGlossyAlpha) {
  ManifoldClaim claim{
      manifoldClaim(mat, /*backface=*/false, marked, maxGlossyAlpha)};
  const ManifoldClaim back{
      manifoldClaim(mat, /*backface=*/true, marked, maxGlossyAlpha)};
  claim.reflectLobes |= back.reflectLobes;
  claim.refractLobes |= back.refractLobes;
  return claim;
}

bool solveManifoldConnection(const ManifoldSurfaces &surfaces,
                             const float3 &receiver,
                             const ManifoldTarget &target,
                             const ManifoldChain &chain,
                             ManifoldConnection &connection,
                             ManifoldWalkReport *report) {
  using Outcome = ManifoldWalkReport::Outcome;
  using Failure = ManifoldWalkReport::Failure;
  int iterationsDone{0};
  float residual{0.0f};
  auto finish{[&](Outcome outcome, Failure failure = Failure::NONE) {
    if (report) {
      report->iterations = iterationsDone;
      report->residual = residual;
      report->outcome = outcome;
      report->failure = failure;
    }
    return outcome == Outcome::CONVERGED;
  }};
  const int count{chain.count};
  if (count < 1 || count > MANIFOLD_MAX_DEPTH)
    return finish(Outcome::DIVERGED, Failure::START);
  std::array<ManifoldVertex, MANIFOLD_MAX_DEPTH> vertices{};
  std::array<float3, MANIFOLD_MAX_DEPTH> frameSeeds{};
  for (int i = 0; i < count; i++) vertices[i] = chain[i].vertex;
  buildFrameSeeds(surfaces, chain, frameSeeds);
  // Move the starting iterate off the straight-line crossing, if asked. The
  // walk re-anchors onto the real surface from the previous vertex, which is
  // the same cast a Newton step takes, so a displaced start is an ordinary
  // start somewhere else rather than a special case.
  {
    float3 origin{receiver};
    for (int i = 0; i < count; i++) {
      const auto &jitter{chain[i].seedJitter};
      if (dot(jitter, jitter) > 0.0f) {
        float3 normal{}, t1{}, t2{};
        if (!manifoldSeedFrame(surfaces, vertices[i], frameSeeds[i], normal, t1,
                               t2))
          return finish(Outcome::DIVERGED, Failure::START);
        const float scale{length(vertices[i].point - receiver)};
        ManifoldVertex moved{};
        if (!surfaces.project(chain[i].vertex, origin,
                              vertices[i].point +
                                  scale * (jitter.x * t1 + jitter.y * t2),
                              moved))
          return finish(Outcome::DIVERGED, Failure::START);
        vertices[i] = moved;
      }
      origin = vertices[i].point;
    }
  }
  // Two buffers, swapped by pointer: a trial step is evaluated into the
  // spare one and accepted by exchanging the two, so the iteration neither
  // rebuilds a state per halving nor copies one back on acceptance. The
  // state is a kilobyte and `evaluateChain()` writes every field of it that
  // anything reads, so a trial that fails leaves nothing behind to matter.
  ManifoldChainState stateBuffers[2]{};
  ManifoldChainState *state{&stateBuffers[0]};
  ManifoldChainState *trial{&stateBuffers[1]};
  if (!evaluateChain(surfaces, receiver, target, chain, frameSeeds, vertices,
                     *state))
    return finish(Outcome::DIVERGED, Failure::START);
  const float residualTolerance{
      chain.residualTolerance > 0.0f
          ? std::min(chain.residualTolerance, RESIDUAL_SANITY)
          : RESIDUAL_SANITY};
  bool converged{false};
  // The trial vertices, likewise reused: a step writes every entry the
  // chain has, and a step that fails part way is abandoned unread.
  std::array<ManifoldVertex, MANIFOLD_MAX_DEPTH> stepVertices{};
  for (int iteration = 0; iteration < MAX_ITERATIONS && !converged;
       iteration++) {
    iterationsDone = iteration;
    residual = state->residual();
    // Solve for the Newton step of every vertex at once. Only the leading
    // block of the matrix belongs to this chain, and a chain is usually one
    // vertex, so copy that rather than all of `MANIFOLD_MAX_DEPTH`.
    const int dim{2 * count};
    ConstraintMatrix A;
    for (int r = 0; r < dim; r++)
      for (int c = 0; c < dim; c++) A(r, c) = state->J(r, c);
    ConstraintVector rhs;
    for (int r = 0; r < dim; r++) rhs[r] = -state->C[r];
    if (!solveDense(dim, A, rhs))
      return finish(Outcome::DIVERGED, Failure::SINGULAR);
    // The world-space steps, clamped together so a bad early Jacobian
    // cannot fling any vertex across the scene, and measured against the
    // distance to the receiver, which is the scale the arrival side
    // judges the same answer at.
    std::array<float3, MANIFOLD_MAX_DEPTH> steps{};
    float maxStepLen{};
    float maxStepFraction{};
    float minDist{(*state)[0].distPrev};
    for (int i = 0; i < count; i++) {
      const auto &sv{(*state)[i]};
      steps[i] = rhs[2 * i + 0] * sv.geometry.dPdu + //
                 rhs[2 * i + 1] * sv.geometry.dPdv;
      const float stepLen{length(steps[i])};
      const float scale{std::max(1e-3f, length(sv.geometry.point - receiver))};
      maxStepLen = std::max(maxStepLen, stepLen);
      maxStepFraction = std::max(maxStepFraction, stepLen / scale);
      minDist = std::min(minDist, sv.distPrev);
    }
    if (!std::isfinite(maxStepLen))
      return finish(Outcome::DIVERGED, Failure::SINGULAR);
    // Arrived: the step left to take cannot move the answer far enough to
    // change what the arrival side makes of it, and the residual agrees
    // this is a solution rather than a stall. Stop without taking it.
    //
    // A small step with a bad residual is NOT an arrival, and it is not a
    // failure to declare here either: it is Newton making no progress,
    // which the line search below reports on its own terms by failing to
    // find a step that lowers the residual. Deciding it here instead made
    // a looser threshold converge LESS often, since it reached this test
    // earlier and gave up before the residual had come down.
    if (maxStepFraction < MANIFOLD_IDENTITY_FRACTION &&
        residual < residualTolerance) {
      converged = true;
      break;
    }
    if (!(maxStepLen > 0.0f))
      return finish(Outcome::DIVERGED, Failure::STALLED);
    float beta{1.0f};
    if (maxStepLen > 0.5f * minDist) beta = 0.5f * minDist / maxStepLen;
    // Damped Newton: re-anchor each stepped vertex by casting from its
    // updated predecessor, and halve the step until the residual
    // decreases.
    bool accepted{false};
    bool anyProjected{false};
    for (int halving = 0; halving < MAX_HALVINGS; halving++, beta *= 0.5f) {
      float3 origin{receiver};
      bool projected{true};
      for (int i = 0; i < count; i++) {
        if (!surfaces.project(chain[i].vertex, origin,
                              (*state)[i].geometry.point + beta * steps[i],
                              stepVertices[i])) {
          projected = false;
          break;
        }
        origin = stepVertices[i].point;
      }
      if (!projected) continue;
      anyProjected = true;
      if (!evaluateChain(surfaces, receiver, target, chain, frameSeeds,
                         stepVertices, *trial))
        continue;
      if (trial->residual() < residual) {
        vertices = stepVertices;
        std::swap(state, trial);
        accepted = true;
        break;
      }
    }
    if (!accepted)
      return finish(Outcome::DIVERGED,
                    anyProjected ? Failure::STALLED : Failure::PROJECTION);
  }
  if (!converged) {
    iterationsDone = MAX_ITERATIONS;
    residual = state->residual();
    return finish(Outcome::DIVERGED, Failure::ITERATIONS);
  }
  // A valid connection scatters the right way at every vertex. A
  // transmission's segments must lie on opposite sides of the shading
  // normal, with the arriving segment on the side the seed crossed from,
  // which rejects a solution that migrated across a silhouette and would
  // otherwise be weighed with swapped indices. A reflection's segments
  // must lie on one side of the shading normal, which the BSDF scatters
  // about, and of the geometric normal, which the segments actually
  // cross; it was searched for rather than handed over, so there is no
  // straight segment whose side it has to have kept.
  for (int i = 0; i < count; i++) {
    const auto &sv{(*state)[i]};
    const float sidePrev{dot(sv.wPrev, sv.geometry.normal)};
    const float sideNext{dot(sv.wNext, sv.geometry.normal)};
    const bool crossing{chain[i].isReflect
                            ? sidePrev * sideNext > 0.0f &&
                                  dot(sv.wPrev, sv.geometry.Ng) *
                                          dot(sv.wNext, sv.geometry.Ng) >
                                      0.0f
                            : sidePrev * sideNext < 0.0f &&
                                  -sidePrev * chain[i].sideSign > 0.0f};
    if (!crossing) return finish(Outcome::REJECTED);
    auto &vertex{connection.vertices[i]};
    vertex.vertex = vertices[i];
    vertex.geometry = sv.geometry;
    vertex.wPrev = sv.wPrev;
    vertex.wNext = sv.wNext;
    vertex.cosPrev = std::abs(sidePrev);
    vertex.cosNext = std::abs(sideNext);
    vertex.halfVectorJacobian = sv.halfVectorJacobian;
  }
  connection.count = count;
  connection.wr = -(*state)[0].wPrev;
  if (!computeOffsetJacobian(*state, receiver, target,
                             connection.offsetJacobian))
    return finish(Outcome::REJECTED);
  return finish(Outcome::CONVERGED);
}

bool manifoldSeedFrame(const ManifoldSurfaces &surfaces,
                       const ManifoldVertex &vertex, const float3 &frameSeed,
                       float3 &normal, float3 &t1, float3 &t2) {
  ManifoldGeometry geometry{};
  if (!surfaces.geometry(vertex, geometry)) return false;
  normal = geometry.normal;
  float3 t{frameSeed - dot(normal, frameSeed) * normal};
  if (!tryNormalize(t)) return false;
  t1 = t, t2 = cross(normal, t1);
  return true;
}

float3 manifoldFrameSeed(const ManifoldSurfaces &surfaces,
                         const ManifoldVertex &vertex) {
  ManifoldGeometry geometry{};
  if (!surfaces.geometry(vertex, geometry)) return {1.0f, 0.0f, 0.0f};
  float3 g{geometry.dPdu -
           dot(geometry.normal, geometry.dPdu) * geometry.normal};
  return tryNormalize(g) ? g : perpendicularTo(geometry.normal);
}

bool isSameManifoldSolution(const float3 &receiver, const ManifoldConnection &a,
                            const ManifoldConnection &b) {
  if (a.count != b.count) return false;
  for (int i = 0; i < a.count; i++) {
    const float scale{
        std::max(1e-3f, length(a.vertices[i].vertex.point - receiver))};
    if (!(length(a.vertices[i].vertex.point - b.vertices[i].vertex.point) <
          MANIFOLD_SOLUTION_IDENTITY_FRACTION * scale))
      return false;
  }
  return true;
}

ManifoldStats &ManifoldStats::global() noexcept {
  static ManifoldStats stats{};
  return stats;
}

template <typename T>
void ManifoldStats::addMax(std::atomic<T> &value, T other) noexcept {
  T seen{value.load(std::memory_order_relaxed)};
  while (seen < other &&
         !value.compare_exchange_weak(seen, other, std::memory_order_relaxed)) {
  }
}

void ManifoldStats::addSum(std::atomic<double> &sum, double value) noexcept {
  double seen{sum.load(std::memory_order_relaxed)};
  while (!sum.compare_exchange_weak(seen, seen + value,
                                    std::memory_order_relaxed)) {
  }
}

void ManifoldStats::recordEstimate(Kind kind,
                                   bool firstWalkConverged) noexcept {
  if (!mEnabled) return;
  mEstimates[kind].fetch_add(1, std::memory_order_relaxed);
  if (firstWalkConverged)
    mFirstWalkConverged[kind].fetch_add(1, std::memory_order_relaxed);
}

void ManifoldStats::recordWalk(const ManifoldWalkReport &report) noexcept {
  if (!mEnabled) return;
  mWalks.fetch_add(1, std::memory_order_relaxed);
  mWalkIterations.fetch_add(uint64_t(std::max(report.iterations, 0)),
                            std::memory_order_relaxed);
  addMax(mWalkIterationsMax, uint64_t(std::max(report.iterations, 0)));
  if (report.outcome != ManifoldWalkReport::Outcome::DIVERGED) {
    const auto bucket{size_t(std::clamp(report.iterations, 0, 64))};
    mConvergedIterations[bucket].fetch_add(1, std::memory_order_relaxed);
  }
  if (report.outcome == ManifoldWalkReport::Outcome::CONVERGED) {
    mWalksConverged.fetch_add(1, std::memory_order_relaxed);
    addSum(mWalkResidual, double(report.residual));
    addMax(mWalkResidualMax, double(report.residual));
  } else if (report.outcome == ManifoldWalkReport::Outcome::REJECTED) {
    mWalksRejected.fetch_add(1, std::memory_order_relaxed);
  } else {
    mWalkFailures[int(report.failure)].fetch_add(1, std::memory_order_relaxed);
  }
}

void ManifoldStats::recordRewalk(const ManifoldWalkReport &report) noexcept {
  if (!mEnabled) return;
  mRewalks.fetch_add(1, std::memory_order_relaxed);
  if (report.outcome == ManifoldWalkReport::Outcome::CONVERGED)
    mRewalksConverged.fetch_add(1, std::memory_order_relaxed);
}

void ManifoldStats::recordCover(bool matched) noexcept {
  if (!mEnabled) return;
  mCoverArrivals.fetch_add(1, std::memory_order_relaxed);
  if (matched) mCoverMatched.fetch_add(1, std::memory_order_relaxed);
}

void ManifoldStats::recordTrials(Kind kind, int trials, bool dropped) noexcept {
  if (!mEnabled) return;
  mTrialEstimates[kind].fetch_add(1, std::memory_order_relaxed);
  mTrials[kind].fetch_add(uint64_t(std::max(trials, 0)),
                          std::memory_order_relaxed);
  addMax(mTrialsMax[kind], uint64_t(std::max(trials, 0)));
  if (dropped) mCapDrops[kind].fetch_add(1, std::memory_order_relaxed);
}

void ManifoldStats::recordContribution(bool nonZero) noexcept {
  if (!mEnabled) return;
  mContributions.fetch_add(1, std::memory_order_relaxed);
  if (nonZero) mContributionsNonZero.fetch_add(1, std::memory_order_relaxed);
}

void ManifoldStats::print(std::ostream &out) const {
  auto load{[](const Counter &counter) {
    return counter.load(std::memory_order_relaxed);
  }};
  auto percent{[](uint64_t part, uint64_t whole) {
    return whole > 0 ? 100.0 * double(part) / double(whole) : 0.0;
  }};
  auto mean{[](double sum, uint64_t count) {
    return count > 0 ? sum / double(count) : 0.0;
  }};
  static constexpr const char *KIND_NAME[NUM_KINDS]{
      "dirac refraction", "glossy refraction", "dirac reflection",
      "glossy reflection"};
  out << std::fixed << std::setprecision(2);
  out << "----------------------------------------------------------\n";
  out << "    Manifold sampling statistics\n";
  out << "----------------------------------------------------------\n";
  out << std::left << std::setw(20) << "Estimates" << std::right
      << std::setw(12) << "count" << std::setw(18) << "first converged"
      << std::setw(12) << "nonzero" << std::setw(12) << "trials avg"
      << std::setw(12) << "trials max" << std::setw(12) << "cap drops" << '\n';
  for (int kind = 0; kind < NUM_KINDS; kind++) {
    const uint64_t estimates{load(mEstimates[kind])};
    const uint64_t converged{load(mFirstWalkConverged[kind])};
    const uint64_t trialEstimates{load(mTrialEstimates[kind])};
    out << "  " << std::left << std::setw(18) << KIND_NAME[kind] << std::right
        << std::setw(12) << estimates << std::setw(11) << converged << " ("
        << std::setw(5) << percent(converged, estimates) << "%)"
        << std::setw(12) << trialEstimates << std::setw(12)
        << mean(double(load(mTrials[kind])), trialEstimates) << std::setw(12)
        << load(mTrialsMax[kind]) << std::setw(12) << load(mCapDrops[kind])
        << '\n';
  }
  const uint64_t walks{load(mWalks)};
  const uint64_t walksConverged{load(mWalksConverged)};
  out << std::left << std::setw(20) << "Walks" << std::right << std::setw(12)
      << walks << std::setw(11) << walksConverged << " (" << std::setw(5)
      << percent(walksConverged, walks) << "%) converged, "
      << load(mWalksRejected) << " rejected after converging\n";
  using Failure = ManifoldWalkReport::Failure;
  out << std::left << std::setw(20) << "  diverged" << std::right << "start "
      << load(mWalkFailures[int(Failure::START)]) << ", singular "
      << load(mWalkFailures[int(Failure::SINGULAR)]) << ", projection "
      << load(mWalkFailures[int(Failure::PROJECTION)]) << ", stalled "
      << load(mWalkFailures[int(Failure::STALLED)]) << ", iterations "
      << load(mWalkFailures[int(Failure::ITERATIONS)]) << '\n';
  out << std::left << std::setw(20) << "  iterations" << std::right << "avg "
      << mean(double(load(mWalkIterations)), walks) << ", max "
      << load(mWalkIterationsMax) << '\n';
  {
    // The budget question: how many iterations the walks that reach
    // convergence actually take.
    uint64_t total{};
    for (const auto &counter : mConvergedIterations) total += load(counter);
    if (total > 0) {
      auto percentile{[&](double p) {
        const uint64_t want{uint64_t(p * double(total - 1))};
        uint64_t seen{};
        for (size_t i = 0; i < mConvergedIterations.size(); i++) {
          seen += load(mConvergedIterations[i]);
          if (seen > want) return i;
        }
        return mConvergedIterations.size() - 1;
      }};
      out << std::left << std::setw(20) << "  to converge" << std::right
          << "p50 " << percentile(0.50) << ", p90 " << percentile(0.90)
          << ", p99 " << percentile(0.99) << ", p99.9 " << percentile(0.999)
          << ", p99.99 " << percentile(0.9999) << ", max ";
      size_t top{0};
      for (size_t i = 0; i < mConvergedIterations.size(); i++)
        if (load(mConvergedIterations[i]) > 0) top = i;
      out << top << '\n';
    }
  }
  out << std::left << std::setw(20) << "  residual" << std::right
      << std::scientific << "avg "
      << mean(mWalkResidual.load(std::memory_order_relaxed), walksConverged)
      << ", max " << mWalkResidualMax.load(std::memory_order_relaxed)
      << " (converged walks)\n"
      << std::fixed;
  const uint64_t rewalks{load(mRewalks)};
  const uint64_t rewalksConverged{load(mRewalksConverged)};
  out << std::left << std::setw(20) << "Re-walks (MIS)" << std::right
      << std::setw(12) << rewalks << std::setw(11) << rewalksConverged << " ("
      << std::setw(5) << percent(rewalksConverged, rewalks) << "%) converged\n";
  const uint64_t coverArrivals{load(mCoverArrivals)};
  const uint64_t coverMatched{load(mCoverMatched)};
  out << std::left << std::setw(20) << "Covered arrivals" << std::right
      << std::setw(12) << coverArrivals << std::setw(11) << coverMatched << " ("
      << std::setw(5) << percent(coverMatched, coverArrivals)
      << "%) matched by the re-walk\n";
  const uint64_t contributions{load(mContributions)};
  const uint64_t nonZero{load(mContributionsNonZero)};
  out << std::left << std::setw(20) << "Contributions" << std::right
      << std::setw(12) << contributions << std::setw(11) << nonZero << " ("
      << std::setw(5) << percent(nonZero, contributions) << "%) non-zero\n";
  out << "----------------------------------------------------------\n";
}

} // namespace smdl
