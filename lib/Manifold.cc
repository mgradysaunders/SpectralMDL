#include "smdl/Manifold.h"

#include <algorithm>
#include <cmath>

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

// The scalar bandwidths of the constraint system. Constraint `i`
// couples vertices `i-1`, `i` and `i+1`, so row `2i+r` runs from column
// `2i-2` to `2i+3`: three below the diagonal and three above.
constexpr int BAND_LOWER{3};

constexpr int BAND_UPPER{3};

// Partial pivoting fills `BAND_LOWER` further superdiagonals, so a
// stored row spans this much, the same count LAPACK's band storage
// keeps for the same reason.
constexpr int BAND_STRIDE{2 * BAND_LOWER + BAND_UPPER + 1};

// The constraint system over the walk's workspace, in band storage: one
// row of `BAND_STRIDE` per unknown, with entry `(i, j)` at
// `j - i + BAND_LOWER`. Everything outside the band is a structural
// zero that the assembly never writes and the elimination never
// creates, so an index past it is a bug rather than a zero.
struct BandView final {
  void clear(int n) const noexcept {
    std::fill_n(coeffs, n * BAND_STRIDE, 0.0f);
  }
  void copyFrom(const BandView &other, int n) const noexcept {
    std::copy_n(other.coeffs, n * BAND_STRIDE, coeffs);
  }
  // Row `i` addressed from its own diagonal, so that `row(i)[d]` is
  // `(i, i + d)` for `d` in `[-BAND_LOWER, BAND_LOWER + BAND_UPPER]`.
  [[nodiscard]] float *row(int i) const noexcept {
    return &coeffs[i * BAND_STRIDE + BAND_LOWER];
  }
  [[nodiscard]] float &operator()(int i, int j) const noexcept {
    SMDL_DEBUG_CHECK(j >= i - BAND_LOWER && j <= i + BAND_LOWER + BAND_UPPER);
    return coeffs[i * BAND_STRIDE + (j - i + BAND_LOWER)];
  }

  float *coeffs;
};

// Solve `A x = b` in place by Gaussian elimination with partial
// pivoting, for `n` unknowns and the one right-hand side `b`, or none
// when it is null. Returns false on a (numerically) singular system.
// `det`, if given, receives the determinant, which is the signed
// product of the pivots the elimination leaves on the diagonal and so
// costs nothing to report. It is a double because those pivots each
// carry the scale of the surface parameterization, so their product
// leaves float's range at a depth and a fineness an ordinary scene
// reaches; see `computeOffsetJacobian()`.
//
// The band is what makes this linear in the chain's length rather than
// cubic, and it changes no arithmetic on the way: the rows more than
// `BAND_LOWER` below the column hold a structural zero there, so the
// pivot search sees the candidates a dense elimination would see, picks
// the same pivot and forms the same multipliers over the same columns.
[[nodiscard]] bool solveBand(int n, BandView A, float *b = nullptr,
                             double *det = nullptr) {
  float scale{};
  for (int r = 0; r < n; r++) {
    const float *coeffs{A.row(r)};
    for (int d = std::max(-BAND_LOWER, -r);
         d <= std::min(BAND_LOWER + BAND_UPPER, n - 1 - r); d++)
      scale = std::max(scale, std::abs(coeffs[d]));
  }
  if (!(scale > 0.0f)) return false;
  const float minPivot{scale * MIN_PIVOT_RELATIVE};
  if (det) *det = 1.0;
  for (int c = 0; c < n; c++) {
    const int rowLast{std::min(c + BAND_LOWER, n - 1)};
    const int width{std::min(c + BAND_LOWER + BAND_UPPER, n - 1) - c + 1};
    int pivot{c};
    for (int r = c + 1; r <= rowLast; r++)
      if (std::abs(A(r, c)) > std::abs(A(pivot, c))) pivot = r;
    if (!(std::abs(A(pivot, c)) > minPivot)) return false;
    // Both rows store the whole span the elimination still touches, so
    // the exchange is that span and not the rows entire.
    float *pivotRow{A.row(c)};
    if (pivot != c) {
      float *other{A.row(pivot) + (c - pivot)};
      for (int k = 0; k < width; k++) std::swap(pivotRow[k], other[k]);
      if (b) std::swap(b[c], b[pivot]);
      if (det) *det = -*det;
    }
    // The pivot is guarded above, so its reciprocal is the standard
    // elimination form: one division a column instead of one a row.
    const float invPivot{1.0f / pivotRow[0]};
    for (int r = c + 1; r <= rowLast; r++) {
      float *coeffs{A.row(r) + (c - r)};
      const float m{coeffs[0] * invPivot};
      if (m == 0.0f) continue;
      for (int k = 0; k < width; k++) coeffs[k] -= m * pivotRow[k];
      if (b) b[r] -= m * b[c];
    }
  }
  if (det)
    for (int c = 0; c < n; c++) *det *= A(c, c);
  if (b) {
    for (int c = n - 1; c >= 0; c--) {
      const float *coeffs{A.row(c)};
      const int last{std::min(c + BAND_LOWER + BAND_UPPER, n - 1) - c};
      float sum{b[c]};
      for (int j = 1; j <= last; j++) sum -= coeffs[j] * b[c + j];
      b[c] = sum / coeffs[0];
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
// vertices `i-1`, `i`, and `i+1` and nothing further, so the system is
// block tridiagonal in 2x2 blocks, which is the band `solveBand()`
// eliminates within and the reason a walk costs the chain's length
// rather than its cube.
class ChainState final {
public:
  [[nodiscard]] ManifoldWalkVertex &operator[](int i) const noexcept {
    return vertices[i];
  }

  [[nodiscard]] float residual() const noexcept {
    float sum{};
    for (int i = 0; i < 2 * count; i++) sum += C[i] * C[i];
    return std::sqrt(sum);
  }

public:
  ManifoldWalkVertex *vertices;
  float *C;
  BandView J;
  int count;
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
bool evaluateChain(const ManifoldSurfaces &surfaces, const float3 &receiver,
                   const ManifoldTarget &target, const ManifoldChain &chain,
                   const float3 *frameSeeds, const ManifoldVertex *vertices,
                   float *gLen, ChainState &chainState) {
  const int count{chain.size()};
  chainState.count = count;
  for (int i = 0; i < count; i++)
    if (!surfaces.evaluateGeometry(vertices[i], chainState[i].geometry))
      return false;
  // Segment directions, half vectors, frames, and constraints. The last
  // vertex's next segment is the distant light direction (whose zero
  // distance drops the position-derivative term below) or the segment
  // to the finite light point (whose derivative term the shared
  // formula picks up through the real distance).
  for (int i = 0; i < count; i++) {
    ManifoldWalkVertex &sv{chainState[i]};
    const ManifoldGeometry &geometry{sv.geometry};
    const float3 prev{i == 0 ? receiver : chainState[i - 1].geometry.point};
    float3 toPrev{prev - geometry.point};
    sv.distPrev = length(toPrev);
    if (!(sv.distPrev > 1e-6f)) return false;
    sv.wPrev = toPrev / sv.distPrev;
    if (i + 1 < count) {
      float3 toNext{chainState[i + 1].geometry.point - geometry.point};
      sv.distNext = length(toNext);
      if (!(sv.distNext > 1e-6f)) return false;
      sv.wNext = toNext / sv.distNext;
    } else if (target.isInfinite) {
      sv.wNext = target.wl;
      sv.distNext = 0.0f;
    } else {
      float3 toLight{target.point - geometry.point};
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
    // `h` points into the denser medium, so which side it lands on depends
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
  chainState.J.clear(2 * count);
  for (int i = 0; i < count; i++) {
    const ManifoldVertexSeed &seed{chain[i]};
    const ManifoldWalkVertex &seedv{chainState[i]};
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
// `detJ` is the constraint Jacobian's determinant, so it is expressed in
// the surface parameterization, and the area elements convert it back out.
// Their ratio is what is invariant; neither factor is on its own, and
// neither stays in float's range either, both scaling as the
// parameterization to the power of the dimension. So the whole product
// accumulates in double and narrows once, after the ratio is taken.
[[nodiscard]] bool computeOffsetJacobian(const ChainState &chainState,
                                         double detJ, const float3 &receiver,
                                         const ManifoldTarget &target,
                                         float &offsetJacobian) {
  const int count{chainState.count};
  const int last{count - 1};
  if (!(std::abs(detJ) > 0.0)) return false;
  double factor{1.0 / std::abs(detJ)};
  for (int i = 0; i < count; i++) {
    const ManifoldWalkVertex &sv{chainState[i]};
    if (!(sv.distPrev > 0.0f)) return false;
    // Projected against the geometric normal: the area-to-solid-angle
    // factor is a property of the facet, not of the interpolated normal
    // the constraint is solved against.
    const double distPrev{sv.distPrev};
    factor *= absDot(sv.wPrev, sv.geometry.Ng) * sv.areaElement /
              (distPrev * distPrev);
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
  if (!target.isInfinite) {
    const ManifoldWalkVertex &sv{chainState[last]};
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
  offsetJacobian = static_cast<float>(factor);
  return std::isfinite(offsetJacobian) && offsetJacobian > 0.0f;
}

// The fixed frame-seed vectors of a chain: each seed's own, or one derived
// from the seed vertex when the seed carries none. Never from a start the
// jitter has moved, since the frame has to be the same in every walk of an
// estimate; see `ManifoldVertexSeed::frameSeed`.
void buildFrameSeeds(const ManifoldSurfaces &surfaces,
                     const ManifoldChain &chain, float3 *frameSeeds) {
  for (int i = 0; i < chain.size(); i++) {
    const float3 &seed{chain[i].frameSeed};
    frameSeeds[i] = lengthSquared(seed) > 0.0f
                        ? seed
                        : manifoldFrameSeed(surfaces, chain[i].vertex);
  }
}

} // namespace

void ManifoldWalkScratch::grow(int depth) {
  maxDepth = depth;
  const size_t n{static_cast<size_t>(depth)};
  const size_t dim{2 * n};
  vertices.resize(2 * n);
  vectors.resize(2 * n);
  iterates.resize(2 * n);
  frameLengths.resize(n);
  constraints.resize(2 * dim);
  jacobians.resize(3 * dim * BAND_STRIDE);
  rhs.resize(dim);
}

ManifoldClaim manifoldClaim(const JIT::Material &material, bool isBackface,
                            bool isMarked) {
  ManifoldClaim claim{};
  if (material.hasEmission()) return claim;
  const int dfLobes{material.getLobes(isBackface)};
  // A df node scattering about a normal it was given is a field the walk
  // does not solve for, and under a remapped `geometry.normal` even a
  // given normal equal to the state normal detaches, that not being the
  // remapped field. A node left defaulted inherits the field and bars
  // nothing. A remap without the hook has no field to read at all. See
  // the header.
  if ((dfLobes & DF_SETS_NORMAL) != 0) return claim;
  if (material.def->canRemapNormal() &&
      (!material.def->geometryNormalEvaluate ||
       (dfLobes & DF_CAN_SET_NORMAL) != 0))
    return claim;
  const bool bends{!material.isThinWalled() &&
                   std::abs(material.getIOR() - material.getExteriorIOR()) >
                       1e-4f};
  if (bends)
    claim.refractLobes =
        dfLobes & (DF_DIRAC_BTDF | (isMarked ? DF_GLOSSY_BTDF : 0));
  if (isMarked) claim.reflectLobes = dfLobes & (DF_DIRAC_BRDF | DF_GLOSSY_BRDF);
  return claim;
}

ManifoldClaim manifoldClaim(const JIT::Material &material, bool isMarked) {
  ManifoldClaim claim{manifoldClaim(material, /*isBackface=*/false, isMarked)};
  const ManifoldClaim back{
      manifoldClaim(material, /*isBackface=*/true, isMarked)};
  claim.reflectLobes |= back.reflectLobes;
  claim.refractLobes |= back.refractLobes;
  return claim;
}

bool solveManifoldConnection(const ManifoldSurfaces &surfaces,
                             ManifoldWalkScratch &scratch,
                             const float3 &receiver,
                             const ManifoldTarget &target,
                             const ManifoldChain &chain,
                             ManifoldConnection &connection,
                             ManifoldWalkReport *report) {
  using Outcome = ManifoldWalkReport::Outcome;
  using Failure = ManifoldWalkReport::Failure;
  int iterationsDone{0};
  float residual{0.0f};
  const auto finish{[&](Outcome outcome, Failure failure = Failure::NONE) {
    if (report) {
      report->iterations = iterationsDone;
      report->residual = residual;
      report->outcome = outcome;
      report->failure = failure;
    }
    return outcome == Outcome::CONVERGED;
  }};
  const int count{chain.size()};
  if (count < 1 || count > MANIFOLD_MAX_DEPTH)
    return finish(Outcome::DIVERGED, Failure::START);
  // The connection takes the chain's length, which is what every write
  // below indexes it by.
  connection.resize(count);
  // The workspace, carved for this chain's own dimension. Every entry
  // under `count` is written before it is read and nothing past it is
  // touched, so whatever the last walk left behind never matters.
  scratch.reserve(count);
  const int dim{2 * count};
  const int maxDim{2 * scratch.maxDepth};
  // One band matrix of the workspace, which holds three of them.
  const size_t bandSize{static_cast<size_t>(maxDim) * BAND_STRIDE};
  ManifoldVertex *const vertices{scratch.vertices.data()};
  ManifoldVertex *const stepVertices{vertices + scratch.maxDepth};
  float3 *const frameSeeds{scratch.vectors.data()};
  float3 *const steps{frameSeeds + scratch.maxDepth};
  float *const rhs{scratch.rhs.data()};
  // The elimination matrix, which the Newton step destroys every
  // iteration.
  const BandView A{scratch.jacobians.data() + 2 * bandSize};
  for (int i = 0; i < count; i++) vertices[i] = chain[i].vertex;
  buildFrameSeeds(surfaces, chain, frameSeeds);
  // Move the starting iterate off the straight-line crossing, if asked. The
  // walk re-anchors onto the real surface from the previous vertex, which is
  // the same cast a Newton step takes, so a displaced start is an ordinary
  // start somewhere else rather than a special case.
  {
    float3 origin{receiver};
    for (int i = 0; i < count; i++) {
      const float2 &jitter{chain[i].seedJitter};
      if (lengthSquared(jitter) > 0.0f) {
        float3 normal{}, t1{}, t2{};
        if (!buildManifoldSeedFrame(surfaces, vertices[i], frameSeeds[i],
                                    normal, t1, t2))
          return finish(Outcome::DIVERGED, Failure::START);
        const float scale{length(vertices[i].point - receiver)};
        ManifoldVertex moved;
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
  // Two states, swapped by pointer: a trial step is evaluated into the
  // spare one and accepted by exchanging the two, so the iteration neither
  // rebuilds a state per halving nor copies one back on acceptance.
  // `evaluateChain()` writes every field of a state that anything reads,
  // so a trial that fails leaves nothing behind to matter.
  ChainState stateBuffers[2]{
      {scratch.iterates.data(), scratch.constraints.data(),
       BandView{scratch.jacobians.data()}, count},
      {scratch.iterates.data() + scratch.maxDepth,
       scratch.constraints.data() + maxDim,
       BandView{scratch.jacobians.data() + bandSize}, count}};
  ChainState *state{&stateBuffers[0]};
  ChainState *trial{&stateBuffers[1]};
  if (!evaluateChain(surfaces, receiver, target, chain, frameSeeds, vertices,
                     scratch.frameLengths.data(), *state))
    return finish(Outcome::DIVERGED, Failure::START);
  const float residualTolerance{
      chain.residualTolerance > 0.0f
          ? std::min(chain.residualTolerance, RESIDUAL_SANITY)
          : RESIDUAL_SANITY};
  // The determinant of the constraint Jacobian, which the offset
  // Jacobian needs and the Newton solve already produces. The walk
  // leaves the loop below in the same iteration it solved in and
  // without swapping `state`, so the last value this takes is the
  // converged iterate's.
  double detJ{};
  bool hasConverged{false};
  for (int iteration = 0; iteration < MAX_ITERATIONS && !hasConverged;
       iteration++) {
    iterationsDone = iteration;
    residual = state->residual();
    // Solve for the Newton step of every vertex at once, on a copy the
    // elimination is free to destroy.
    A.copyFrom(state->J, dim);
    for (int r = 0; r < dim; r++) rhs[r] = -state->C[r];
    if (!solveBand(dim, A, rhs, &detJ))
      return finish(Outcome::DIVERGED, Failure::SINGULAR);
    // The world-space steps, clamped together so a bad early Jacobian
    // cannot fling any vertex across the scene, and measured against the
    // distance to the receiver, which is the scale the arrival side
    // judges the same answer at.
    float maxStepLen{};
    float maxStepFraction{};
    float minDist{(*state)[0].distPrev};
    for (int i = 0; i < count; i++) {
      const ManifoldWalkVertex &sv{(*state)[i]};
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
      hasConverged = true;
      break;
    }
    if (!(maxStepLen > 0.0f))
      return finish(Outcome::DIVERGED, Failure::STALLED);
    float beta{1.0f};
    if (maxStepLen > 0.5f * minDist) beta = 0.5f * minDist / maxStepLen;
    // Damped Newton: re-anchor each stepped vertex by casting from its
    // updated predecessor, and halve the step until the residual
    // decreases.
    bool isAccepted{false};
    bool anyProjected{false};
    for (int halving = 0; halving < MAX_HALVINGS; halving++, beta *= 0.5f) {
      float3 origin{receiver};
      bool isProjected{true};
      for (int i = 0; i < count; i++) {
        if (!surfaces.project(chain[i].vertex, origin,
                              (*state)[i].geometry.point + beta * steps[i],
                              stepVertices[i])) {
          isProjected = false;
          break;
        }
        origin = stepVertices[i].point;
      }
      if (!isProjected) continue;
      anyProjected = true;
      if (!evaluateChain(surfaces, receiver, target, chain, frameSeeds,
                         stepVertices, scratch.frameLengths.data(), *trial))
        continue;
      if (trial->residual() < residual) {
        for (int i = 0; i < count; i++) vertices[i] = stepVertices[i];
        std::swap(state, trial);
        isAccepted = true;
        break;
      }
    }
    if (!isAccepted)
      return finish(Outcome::DIVERGED,
                    anyProjected ? Failure::STALLED : Failure::PROJECTION);
  }
  if (!hasConverged) {
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
    const ManifoldWalkVertex &sv{(*state)[i]};
    const float sidePrev{dot(sv.wPrev, sv.geometry.normal)};
    const float sideNext{dot(sv.wNext, sv.geometry.normal)};
    const bool isCrossing{chain[i].isReflect
                              ? sidePrev * sideNext > 0.0f &&
                                    dot(sv.wPrev, sv.geometry.Ng) *
                                            dot(sv.wNext, sv.geometry.Ng) >
                                        0.0f
                              : sidePrev * sideNext < 0.0f &&
                                    -sidePrev * chain[i].sideSign > 0.0f};
    if (!isCrossing) return finish(Outcome::REJECTED);
    ManifoldConnectionVertex &vertex{connection.vertices[i]};
    vertex.vertex = vertices[i];
    vertex.geometry = sv.geometry;
    vertex.wPrev = sv.wPrev;
    vertex.wNext = sv.wNext;
    vertex.cosPrev = std::abs(sidePrev);
    vertex.cosNext = std::abs(sideNext);
    vertex.halfVectorJacobian = sv.halfVectorJacobian;
  }
  connection.wr = -(*state)[0].wPrev;
  if (!computeOffsetJacobian(*state, detJ, receiver, target,
                             connection.offsetJacobian))
    return finish(Outcome::REJECTED);
  return finish(Outcome::CONVERGED);
}

bool buildManifoldSeedFrame(const ManifoldSurfaces &surfaces,
                            const ManifoldVertex &vertex,
                            const float3 &frameSeed, float3 &normal, float3 &t1,
                            float3 &t2) {
  ManifoldGeometry geometry;
  if (!surfaces.evaluateGeometry(vertex, geometry)) return false;
  normal = geometry.normal;
  float3 t{frameSeed - dot(normal, frameSeed) * normal};
  if (!tryNormalize(t)) return false;
  t1 = t, t2 = cross(normal, t1);
  return true;
}

float3 manifoldFrameSeed(const ManifoldSurfaces &surfaces,
                         const ManifoldVertex &vertex) {
  ManifoldGeometry geometry;
  if (!surfaces.evaluateGeometry(vertex, geometry)) return {1.0f, 0.0f, 0.0f};
  float3 g{geometry.dPdu -
           dot(geometry.normal, geometry.dPdu) * geometry.normal};
  return tryNormalize(g) ? g : perpendicularTo(geometry.normal);
}

bool isSameManifoldSolution(const float3 &receiver,
                            const ManifoldSolutionKey &a,
                            const ManifoldConnection &b) {
  const int numCrossings{static_cast<int>(a.points.size())};
  if (numCrossings != b.size()) return false;
  for (int i = 0; i < numCrossings; i++) {
    const float scale{std::max(1e-3f, length(a.points[i] - receiver))};
    if (!(length(a.points[i] - b.vertices[i].vertex.point) <
          MANIFOLD_SOLUTION_IDENTITY_FRACTION * scale))
      return false;
  }
  return true;
}

bool isSameManifoldSolution(const float3 &receiver, const ManifoldConnection &a,
                            const ManifoldConnection &b) {
  ManifoldSolutionKey key;
  key.set(a);
  return isSameManifoldSolution(receiver, key, b);
}

} // namespace smdl
