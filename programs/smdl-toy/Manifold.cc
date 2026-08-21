#include "Manifold.h"

// The walk fails cleanly rather than loop or wander: iteration and
// step-halving budgets, and a cap on the null-interface hops the
// re-anchoring casts skip.
static constexpr int MAX_ITERATIONS{64};
static constexpr int MAX_HALVINGS{5};
static constexpr int MAX_SKIPS{16};

// The walk has arrived when its next step would move every vertex less
// than this fraction of that vertex's distance to the receiver.
//
// The stopping rule is in the units the ARRIVAL side judges the answer
// in, which is the whole point. Re-walk MIS decides whether the gather
// can produce a path by re-running this walk and comparing where it lands
// against the crossings the path took, within
// `MANIFOLD_IDENTITY_FRACTION` of the distance to the receiver. Stopping
// on the residual instead asks for a precision in a different currency:
// the exchange rate between a tangential half vector and a position is
// the conditioning of the constraint, which is worst exactly where
// caustics are, so a residual tight enough to pin the position on easy
// geometry is unreachable on hard geometry. Measured, that was not a
// small effect. At a residual tolerance of 1e-7 the walk converged on
// 0.38 percent of attempts and the estimator degraded to path tracing
// with the cost of a search; at 1e-5 it converged on 86 percent but the
// position was no longer reliably inside the identity test, which cost
// about 0.8 percent of bias.
//
// The fraction is 1, which is the principle taken literally: stop at the
// precision the identity test judges at, no tighter. Measured, the exact
// match matters. Converging an order of magnitude further costs nothing
// in convergence rate, since Newton collapses the step quadratically once
// it is close, but it reads 0.76 percent dark against brute force where
// stopping at the identity scale reads 0.19 percent, on a scene where
// path tracing alone reads 0.02. Both sides of the estimator run this
// same walk, and they agree best when neither is more precise than the
// comparison between them.
static constexpr float STEP_FRACTION{1.0f * MANIFOLD_IDENTITY_FRACTION};

// A step can also fall small because the Jacobian is nearly singular
// rather than because the walk has arrived, so the residual still has to
// be plausible for a solution. This is the same gate
// `evaluateManifoldTransfer()` holds a caller's chain to, and it is loose
// on purpose: it rejects a stuck walk, not an imprecise one.
static constexpr float RESIDUAL_SANITY{1e-3f};

// The pivot the dense solve refuses to divide by, as a fraction of the
// largest entry of the system. Absolute would not do: the Jacobian
// entries carry units of inverse distance, so one geometry measured in
// millimeters and the same in kilometers would land on opposite sides of
// a fixed number. A pivot this far below the scale of the matrix is
// noise at float precision, and the walk is better off failing.
static constexpr float MIN_PIVOT_RELATIVE{1e-7f};

// The dimension of the coupled constraint system.
static constexpr int MAX_DIM{2 * MNEE_MAX_DEPTH};

// Re-anchor a Newton step onto the real surface: cast from the previous
// vertex (or the receiver) toward the stepped position and accept the
// first hit on the seed's own instance, passing through null
// interfaces. Anything else in the way fails the step, so a converged
// connection's segments are known to see their endpoints. A primitive
// hit must also land on the seed's own piece: a shape's pieces (a
// cylinder's side and caps) are distinct smooth surfaces, and letting a
// vertex hop between them mid-walk corrupts the iterate; a failed
// projection just halves the step instead. Mesh vertices slide across
// faces freely, since the faces tile one smooth surface.
[[nodiscard]] static bool project(const Scene &scene, const float3 &origin,
                                  const float3 &target, const Hit &seedHit,
                                  Hit &hit) {
  auto dir{target - origin};
  if (!smdl::tryNormalize(dir)) return false;
  Ray ray{origin, dir, EPS, INF};
  for (int skip = 0; skip < MAX_SKIPS; skip++) {
    hit = Hit{};
    if (!scene.intersect(ray, hit)) return false;
    if (hit.instance == seedHit.instance && !hit.instance->isCurves())
      return !hit.instance->isPrimitive() || hit.faceIndex == seedHit.faceIndex;
    if (!hit.material->isNullInterface()) return false;
    ray = Ray{hit.point, dir, EPS, INF};
  }
  return false;
}

// Solve `A x = b` in place by Gaussian elimination with partial
// pivoting, for `n` unknowns and `nrhs` right-hand sides. Returns false
// on a (numerically) singular system. `det`, if given, receives the
// determinant, which is the signed product of the pivots the elimination
// leaves on the diagonal and so costs nothing to report.
[[nodiscard]] static bool solveDense(int n, int nrhs, float A[MAX_DIM][MAX_DIM],
                                     float b[MAX_DIM][2],
                                     float *det = nullptr) {
  float scale{};
  for (int r = 0; r < n; r++)
    for (int c = 0; c < n; c++) scale = std::max(scale, std::abs(A[r][c]));
  if (!(scale > 0.0f)) return false;
  const float minPivot{scale * MIN_PIVOT_RELATIVE};
  if (det) *det = 1.0f;
  for (int c = 0; c < n; c++) {
    int pivot{c};
    for (int r = c + 1; r < n; r++)
      if (std::abs(A[r][c]) > std::abs(A[pivot][c])) pivot = r;
    if (!(std::abs(A[pivot][c]) > minPivot)) return false;
    if (pivot != c) {
      for (int k = c; k < n; k++) std::swap(A[c][k], A[pivot][k]);
      for (int k = 0; k < nrhs; k++) std::swap(b[c][k], b[pivot][k]);
      if (det) *det = -*det;
    }
    for (int r = c + 1; r < n; r++) {
      const float m{A[r][c] / A[c][c]};
      if (m == 0.0f) continue;
      for (int k = c; k < n; k++) A[r][k] -= m * A[c][k];
      for (int k = 0; k < nrhs; k++) b[r][k] -= m * b[c][k];
    }
  }
  if (det)
    for (int c = 0; c < n; c++) *det *= A[c][c];
  for (int c = n - 1; c >= 0; c--) {
    for (int k = 0; k < nrhs; k++) {
      float sum{b[c][k]};
      for (int j = c + 1; j < n; j++) sum -= A[c][j] * b[j][k];
      b[c][k] = sum / A[c][c];
    }
  }
  return true;
}

// Defined below, and used by the jittered start above.
[[nodiscard]] bool manifoldSeedFrame(const Scene &scene, const Hit &hit,
                                     float3 &normal, float3 &t1, float3 &t2);

namespace {

// One Newton iterate over the whole chain: the differential geometry at
// every vertex, the per-vertex constraints (each generalized half
// vector projected into its tangent plane), and the coupled Jacobian
// over the surface parameterizations, including the frame terms the
// shading-normal derivatives induce, which is what buys quadratic
// convergence on normal-interpolated meshes. Constraint `i` couples
// vertices `i-1`, `i`, and `i+1`, so the system is block tridiagonal;
// at this size a dense solve is simpler and just as fast.
class ChainState final {
public:
  int count{};
  std::array<ManifoldGeometry, MNEE_MAX_DEPTH> geometry{};
  std::array<float3, MNEE_MAX_DEPTH> wFront{}; // Toward the previous vertex.
  std::array<float3, MNEE_MAX_DEPTH> wBack{};  // Toward the next, or the light.
  std::array<float, MNEE_MAX_DEPTH> distFront{};
  std::array<float, MNEE_MAX_DEPTH> distBack{}; // 0 for the distant light.
  std::array<float3, MNEE_MAX_DEPTH> Hhat{};
  std::array<float, MNEE_MAX_DEPTH> Hlen{};
  // The sign that orients `Hhat` onto the shading normal's side, so that
  // the constraint means a microfacet normal rather than a line through
  // one. Zero offsets do not care, which is why it never mattered before.
  std::array<float, MNEE_MAX_DEPTH> Hsign{};
  // The area element of the parameterization the Jacobian is expressed in,
  // and the half-vector measure of the crossing; see the header.
  std::array<float, MNEE_MAX_DEPTH> areaElement{};
  std::array<float, MNEE_MAX_DEPTH> halfVectorJacobian{};
  std::array<float3, MNEE_MAX_DEPTH> t1{};
  std::array<float3, MNEE_MAX_DEPTH> t2{};
  float C[MAX_DIM]{};
  float J[MAX_DIM][MAX_DIM]{};

  [[nodiscard]] float residual() const noexcept {
    float sum{};
    for (int i = 0; i < 2 * count; i++) sum += C[i] * C[i];
    return std::sqrt(sum);
  }
};

} // namespace

// The derivative of a unit direction `w = (q - p)/d` with respect to a
// perturbation `dp` of the base point `p`: the tangential projector
// over the distance, negated. A perturbation of the far point `q` is
// the same expression with the opposite sign.
[[nodiscard]] static float3 unitDirDerivative(const float3 &w, float d,
                                              const float3 &dp) {
  return -(dp - dot(w, dp) * w) / d;
}

[[nodiscard]] static bool
evaluateChain(const Scene &scene, const float3 &receiver,
              const ManifoldTarget &target, const ManifoldChain &chain,
              const std::array<float3, MNEE_MAX_DEPTH> &frameSeeds,
              const std::array<Hit, MNEE_MAX_DEPTH> &hits, ChainState &state) {
  const int count{chain.count};
  state.count = count;
  for (int i = 0; i < count; i++)
    state.geometry[i] = scene.manifoldGeometry(hits[i]);
  // Segment directions, half vectors, frames, and constraints. The last
  // vertex's back segment is the distant light direction (whose zero
  // distance drops the position-derivative term below) or the segment
  // to the finite light point (whose derivative term the shared
  // formula picks up through the real distance).
  std::array<float, MNEE_MAX_DEPTH> gLen{};
  for (int i = 0; i < count; i++) {
    const auto &geometry{state.geometry[i]};
    const float3 prev{i == 0 ? receiver : state.geometry[i - 1].point};
    auto toPrev{prev - geometry.point};
    state.distFront[i] = length(toPrev);
    if (!(state.distFront[i] > 1e-6f)) return false;
    state.wFront[i] = toPrev / state.distFront[i];
    if (i + 1 < count) {
      auto toNext{state.geometry[i + 1].point - geometry.point};
      state.distBack[i] = length(toNext);
      if (!(state.distBack[i] > 1e-6f)) return false;
      state.wBack[i] = toNext / state.distBack[i];
    } else if (target.infinite) {
      state.wBack[i] = target.wl;
      state.distBack[i] = 0.0f;
    } else {
      auto toLight{target.point - geometry.point};
      state.distBack[i] = length(toLight);
      if (!(state.distBack[i] > 1e-6f)) return false;
      state.wBack[i] = toLight / state.distBack[i];
    }
    // The generalized half vector of refraction, parallel to the normal
    // exactly when the segment pair obeys Snell's law.
    const float3 H{chain.vertices[i].etaFront * state.wFront[i] +
                   chain.vertices[i].etaBack * state.wBack[i]};
    state.Hlen[i] = length(H);
    if (!(state.Hlen[i] > 1e-6f)) return false;
    state.Hhat[i] = H / state.Hlen[i];
    // `H` points into the denser medium, so which side it lands on depends
    // on which side is denser. Orienting it onto the shading normal makes
    // it the microfacet normal the interface's own distribution is
    // expressed in, which is what an offset has to be measured against.
    state.Hsign[i] = dot(state.Hhat[i], geometry.normal) < 0.0f ? -1.0f : 1.0f;
    state.areaElement[i] = length(cross(geometry.dPdu, geometry.dPdv));
    // `|d h / d omega_back|`, being the refraction half-vector Jacobian
    // times the cosine that converts its solid angle into the projected
    // measure the constraint lives in.
    state.halfVectorJacobian[i] =
        std::abs(dot(state.Hhat[i], geometry.normal)) *
        std::abs(dot(state.wBack[i], state.Hhat[i])) *
        (chain.vertices[i].etaBack * chain.vertices[i].etaBack) /
        (state.Hlen[i] * state.Hlen[i]);
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
    state.t1[i] = g / gLen[i];
    state.t2[i] = cross(n, state.t1[i]);
    // The constraint is the oriented tangential half vector against the
    // offset this crossing is solved for. At zero offset the sign cancels
    // out of every use, since the Newton step and the transfer solve scale
    // a row of the matrix and of the right-hand side together.
    const float2 &offset{chain.vertices[i].offset};
    state.C[2 * i + 0] =
        state.Hsign[i] * dot(state.Hhat[i], state.t1[i]) - offset.x;
    state.C[2 * i + 1] =
        state.Hsign[i] * dot(state.Hhat[i], state.t2[i]) - offset.y;
  }
  // The coupled Jacobian. Constraint `i` sees vertex `j` through the
  // segment directions (and, for `j == i`, through its own frame, whose
  // variation the shading-normal derivatives induce; the seed vector
  // dPdu is treated as constant, which drops second-order surface terms
  // the geometry query cannot provide).
  for (int r = 0; r < 2 * count; r++)
    for (int c = 0; c < 2 * count; c++) state.J[r][c] = 0.0f;
  for (int i = 0; i < count; i++) {
    const auto &seed{chain.vertices[i]};
    const float3 n{state.geometry[i].normal};
    for (int j = std::max(i - 1, 0); j <= std::min(i + 1, count - 1); j++) {
      const float3 e[2]{state.geometry[j].dPdu, state.geometry[j].dPdv};
      for (int k = 0; k < 2; k++) {
        float3 dH{};
        if (j == i) {
          dH = seed.etaFront *
               unitDirDerivative(state.wFront[i], state.distFront[i], e[k]);
          if (state.distBack[i] > 0.0f)
            dH = dH + seed.etaBack * unitDirDerivative(state.wBack[i],
                                                       state.distBack[i], e[k]);
        } else if (j == i - 1) {
          dH = -seed.etaFront *
               unitDirDerivative(state.wFront[i], state.distFront[i], e[k]);
        } else {
          dH = -seed.etaBack *
               unitDirDerivative(state.wBack[i], state.distBack[i], e[k]);
        }
        const float3 dHhat{(dH - dot(state.Hhat[i], dH) * state.Hhat[i]) /
                           state.Hlen[i]};
        float term1{dot(dHhat, state.t1[i])};
        float term2{dot(dHhat, state.t2[i])};
        if (j == i) {
          const float3 dn{k == 0 ? state.geometry[i].dNdu
                                 : state.geometry[i].dNdv};
          const float3 a{frameSeeds[i]};
          const float3 dg{-(dot(n, a) * dn + dot(dn, a) * n)};
          const float3 dt1{(dg - dot(state.t1[i], dg) * state.t1[i]) / gLen[i]};
          const float3 dt2{cross(dn, state.t1[i]) + cross(n, dt1)};
          term1 += dot(state.Hhat[i], dt1);
          term2 += dot(state.Hhat[i], dt2);
        }
        // The same orientation the constraint carries, applied to the
        // whole row so that a sign flip cancels out of every solve.
        state.J[2 * i + 0][2 * j + k] = state.Hsign[i] * term1;
        state.J[2 * i + 1][2 * j + k] = state.Hsign[i] * term2;
      }
    }
  }
  return true;
}

// The transfer Jacobian |d omega_r / d omega_l| of an evaluated chain by
// the implicit function theorem: only the last constraint block sees the
// light (with the vertices held fixed, so no frame terms), and only the
// first vertex moves the receiver-side direction. Solve
// J Y = dC/d(omega_l) and combine the top block with the receiver-side
// direction partials. The light-direction measure is the solid angle of
// the straight receiver-to-light line: a distant light perturbs the
// last back direction directly, while a finite light point moves on the
// plane through it perpendicular to the straight line, scaled by the
// straight distance so a unit perturbation is a unit of solid angle at
// the receiver, and reaches the back direction through the geometry of
// the last segment.
[[nodiscard]] static bool computeTransfer(const ChainState &state, float etaL,
                                          const float3 &receiver,
                                          const ManifoldTarget &target,
                                          float &transfer) {
  const int count{state.count};
  const float3 a1{smdl::perpendicularTo(target.wl)};
  const float3 a2{cross(target.wl, a1)};
  const int last{count - 1};
  float3 a[2]{a1, a2};
  if (!target.infinite) {
    const float distStraight{length(target.point - receiver)};
    const float3 wB{state.wBack[last]};
    for (auto &ak : a) {
      ak = (distStraight / state.distBack[last]) * (ak - dot(wB, ak) * wB);
    }
  }
  float A[MAX_DIM][MAX_DIM];
  float Y[MAX_DIM][2];
  for (int r = 0; r < 2 * count; r++) {
    for (int c = 0; c < 2 * count; c++) A[r][c] = state.J[r][c];
    Y[r][0] = Y[r][1] = 0.0f;
  }
  const float3 t[2]{state.t1[last], state.t2[last]};
  for (int m = 0; m < 2; m++)
    for (int k = 0; k < 2; k++)
      Y[2 * last + m][k] = state.Hsign[last] * etaL / state.Hlen[last] *
                           (dot(a[k], t[m]) - dot(state.Hhat[last], a[k]) *
                                                  dot(state.Hhat[last], t[m]));
  if (!solveDense(2 * count, 2, A, Y)) return false;
  const float detTop{Y[0][0] * Y[1][1] - Y[0][1] * Y[1][0]};
  const float3 dwr[2]{unitDirDerivative(state.wFront[0], state.distFront[0],
                                        state.geometry[0].dPdu),
                      unitDirDerivative(state.wFront[0], state.distFront[0],
                                        state.geometry[0].dPdv)};
  transfer = length(cross(dwr[0], dwr[1])) * std::abs(detTop);
  return std::isfinite(transfer) && transfer > 0.0f;
}

// The offset Jacobian of an evaluated chain: the measure of the nested
// outgoing solid angles per unit of the variables a roughened connection
// is drawn in. See the header for the expression; this assembles it from
// the same state the transfer solve uses.
//
// The determinant is the constraint Jacobian's, so it is expressed in the
// surface parameterization, and the area elements convert it back out.
// Their ratio is what is invariant; neither factor is on its own.
[[nodiscard]] static bool computeOffsetJacobian(const ChainState &state,
                                                const float3 &receiver,
                                                const ManifoldTarget &target,
                                                float &offsetJacobian) {
  const int count{state.count};
  const int last{count - 1};
  float A[MAX_DIM][MAX_DIM];
  float rhs[MAX_DIM][2]{};
  for (int r = 0; r < 2 * count; r++)
    for (int c = 0; c < 2 * count; c++) A[r][c] = state.J[r][c];
  float detJ{};
  if (!solveDense(2 * count, 0, A, rhs, &detJ)) return false;
  if (!(std::abs(detJ) > 0.0f)) return false;
  float factor{1.0f / std::abs(detJ)};
  for (int i = 0; i < count; i++) {
    if (!(state.distFront[i] > 0.0f)) return false;
    const float cosFront{
        std::abs(dot(state.wFront[i], state.geometry[i].normal))};
    factor *= cosFront * state.areaElement[i] /
              (state.distFront[i] * state.distFront[i]);
  }
  // The light-side correction, carrying the geometry term across from the
  // straight line the sampler measured in to the segment that arrives. A
  // distant target needs none, since its direction is the straight one; a
  // punctual target has no orientation, so only the distances differ.
  if (!target.infinite) {
    const float distStraight{length(target.point - receiver)};
    const float distBack{state.distBack[last]};
    if (!(distStraight > 0.0f) || !(distBack > 0.0f)) return false;
    factor *= (distStraight * distStraight) / (distBack * distBack);
    if (dot(target.normal, target.normal) > 0.0f) {
      const float cosStraight{std::abs(dot(target.normal, target.wl))};
      const float cosBack{std::abs(dot(target.normal, state.wBack[last]))};
      if (!(cosStraight > 0.0f)) return false;
      factor *= cosBack / cosStraight;
    }
  }
  offsetJacobian = factor;
  return std::isfinite(offsetJacobian) && offsetJacobian > 0.0f;
}

// The fixed frame-seed vectors for a chain's hits: each seed hit's own
// tangent, or any perpendicular when the tangent is degenerate against
// the normal.
static void buildFrameSeeds(const Scene &scene,
                            const std::array<Hit, MNEE_MAX_DEPTH> &hits,
                            int count,
                            std::array<float3, MNEE_MAX_DEPTH> &frameSeeds) {
  for (int i = 0; i < count; i++) {
    const auto geometry{scene.manifoldGeometry(hits[i])};
    float3 g{geometry.dPdu -
             dot(geometry.normal, geometry.dPdu) * geometry.normal};
    frameSeeds[i] =
        smdl::tryNormalize(g) ? g : smdl::perpendicularTo(geometry.normal);
  }
}

// Did the material leave `geometry.normal` alone? Byte equality rather
// than a tolerance, because an untouched normal is initialized from
// `$state.normal` and a remap genuinely moves the manifold. Shared by
// both eligibility tests so the two vetoes cannot drift apart.
[[nodiscard]] static bool
normalIsUnremapped(const smdl::JIT::MaterialInstance &mat,
                   const float3 &stateNormal) {
  const float3 &materialNormal{mat.instance.geometry->normal};
  return materialNormal.x == stateNormal.x &&
         materialNormal.y == stateNormal.y && materialNormal.z == stateNormal.z;
}

bool isGlossyManifoldInterface(const smdl::JIT::MaterialInstance &mat) {
  const int dfLobes{dfLobesOf(mat)};
  return (dfLobes & smdl::JIT::DF_DELTA_BTDF) == 0 &&
         (dfLobes & smdl::JIT::DF_GLOSSY_BTDF) != 0;
}

bool isManifoldInterface(const smdl::JIT::MaterialInstance &mat,
                         const float3 &stateNormal, bool allowGlossy) {
  const int dfLobes{dfLobesOf(mat)};
  const int transmits{smdl::JIT::DF_DELTA_BTDF |
                      (allowGlossy ? smdl::JIT::DF_GLOSSY_BTDF : 0)};
  if ((dfLobes & transmits) == 0 || mat.isThinWalled() || mat.hasEmission() ||
      !(std::abs(mat.getIOR() - mat.getExteriorIOR()) > 1e-4f))
    return false;
  return normalIsUnremapped(mat, stateNormal);
}

bool isManifoldCrossing(const ManifoldVertexSeed &seed, const float3 &normal,
                        const float3 &wFront, const float3 &wBack) {
  const float sideF{dot(wFront, normal)};
  const float sideB{dot(wBack, normal)};
  return sideF * sideB < 0.0f && -sideF * seed.sideSign > 0.0f;
}

bool makeManifoldSeed(const MediumStack *medium,
                      smdl::JIT::MaterialInstance &mat,
                      const smdl::State &state, const Hit &hit,
                      const float3 &wl, bool allowGlossy,
                      ManifoldVertexSeed &seed) {
  const float3 woStraight{-wl};
  mat.setExteriorIOR(ExteriorIOR(medium, mat, woStraight));
  if (!isManifoldInterface(mat, state.normal, allowGlossy)) return false;
  seed.isGlossy = isGlossyManifoldInterface(mat);
  const bool frontInterior{mat.isInterior(woStraight)};
  seed.hit = hit;
  seed.etaFront = frontInterior ? mat.getIOR() : mat.getExteriorIOR();
  seed.etaBack = frontInterior ? mat.getExteriorIOR() : mat.getIOR();
  seed.sideSign = dot(wl, hit.normal) < 0 ? -1.0f : 1.0f;
  return true;
}

bool solveManifoldConnection(const Scene &scene, const float3 &receiver,
                             const ManifoldTarget &target,
                             const ManifoldChain &chain,
                             ManifoldConnection &connection) {
  const int count{chain.count};
  if (count < 1 || count > MNEE_MAX_DEPTH) return false;
  std::array<Hit, MNEE_MAX_DEPTH> hits{};
  std::array<float3, MNEE_MAX_DEPTH> frameSeeds{};
  for (int i = 0; i < count; i++) hits[i] = chain.vertices[i].hit;
  // Move the starting iterate off the straight-line crossing, if asked. The
  // walk re-anchors onto the real surface from the previous vertex, which is
  // the same cast a Newton step takes, so a displaced start is an ordinary
  // start somewhere else rather than a special case.
  {
    float3 origin{receiver};
    for (int i = 0; i < count; i++) {
      const auto &jitter{chain.vertices[i].seedJitter};
      if (dot(jitter, jitter) > 0.0f) {
        float3 normal{}, t1{}, t2{};
        if (!manifoldSeedFrame(scene, hits[i], normal, t1, t2)) return false;
        const float scale{length(hits[i].point - receiver)};
        Hit moved{};
        if (!project(scene, origin,
                     hits[i].point + scale * (jitter.x * t1 + jitter.y * t2),
                     chain.vertices[i].hit, moved))
          return false;
        hits[i] = moved;
      }
      origin = hits[i].point;
    }
  }
  buildFrameSeeds(scene, hits, count, frameSeeds);
  ChainState state{};
  if (!evaluateChain(scene, receiver, target, chain, frameSeeds, hits, state))
    return false;
  bool converged{false};
  for (int iteration = 0; iteration < MAX_ITERATIONS && !converged;
       iteration++) {
    // Solve for the Newton step of every vertex at once.
    float A[MAX_DIM][MAX_DIM];
    float rhs[MAX_DIM][2];
    for (int r = 0; r < 2 * count; r++) {
      for (int c = 0; c < 2 * count; c++) A[r][c] = state.J[r][c];
      rhs[r][0] = -state.C[r];
    }
    if (!solveDense(2 * count, 1, A, rhs)) return false;
    // The world-space steps, clamped together so a bad early Jacobian
    // cannot fling any vertex across the scene, and measured against the
    // distance to the receiver, which is the scale the arrival side
    // judges the same answer at.
    std::array<float3, MNEE_MAX_DEPTH> steps{};
    float maxStepLen{};
    float maxStepFraction{};
    float minDist{state.distFront[0]};
    for (int i = 0; i < count; i++) {
      steps[i] = rhs[2 * i + 0][0] * state.geometry[i].dPdu +
                 rhs[2 * i + 1][0] * state.geometry[i].dPdv;
      const float stepLen{length(steps[i])};
      const float scale{
          std::max(1e-3f, length(state.geometry[i].point - receiver))};
      maxStepLen = std::max(maxStepLen, stepLen);
      maxStepFraction = std::max(maxStepFraction, stepLen / scale);
      minDist = std::min(minDist, state.distFront[i]);
    }
    if (!std::isfinite(maxStepLen)) return false;
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
    if (maxStepFraction < STEP_FRACTION && state.residual() < RESIDUAL_SANITY) {
      converged = true;
      break;
    }
    if (!(maxStepLen > 0.0f)) return false;
    float beta{1.0f};
    if (maxStepLen > 0.5f * minDist) beta = 0.5f * minDist / maxStepLen;
    // Damped Newton: re-anchor each stepped vertex by casting from its
    // updated predecessor, and halve the step until the residual
    // decreases.
    bool accepted{false};
    for (int halving = 0; halving < MAX_HALVINGS; halving++, beta *= 0.5f) {
      std::array<Hit, MNEE_MAX_DEPTH> stepHits{};
      float3 origin{receiver};
      bool projected{true};
      for (int i = 0; i < count; i++) {
        if (!project(scene, origin, state.geometry[i].point + beta * steps[i],
                     chain.vertices[i].hit, stepHits[i])) {
          projected = false;
          break;
        }
        origin = stepHits[i].point;
      }
      if (!projected) continue;
      ChainState stepState{};
      if (!evaluateChain(scene, receiver, target, chain, frameSeeds, stepHits,
                         stepState))
        continue;
      if (stepState.residual() < state.residual()) {
        hits = stepHits;
        state = stepState;
        accepted = true;
        break;
      }
    }
    if (!accepted) return false;
  }
  if (!converged) return false;
  // A valid connection refracts at every vertex, on the same side of
  // the surface the seed chain crossed.
  for (int i = 0; i < count; i++) {
    const float sideF{dot(state.wFront[i], state.geometry[i].normal)};
    const float sideB{dot(state.wBack[i], state.geometry[i].normal)};
    if (!isManifoldCrossing(chain.vertices[i], state.geometry[i].normal,
                            state.wFront[i], state.wBack[i]))
      return false;
    auto &vertex{connection.vertices[i]};
    vertex.hit = hits[i];
    vertex.geometry = state.geometry[i];
    vertex.wFront = state.wFront[i];
    vertex.wBack = state.wBack[i];
    vertex.cosFront = std::abs(sideF);
    vertex.cosBack = std::abs(sideB);
    vertex.halfVectorJacobian = state.halfVectorJacobian[i];
  }
  connection.count = count;
  connection.wr = -state.wFront[0];
  if (!computeOffsetJacobian(state, receiver, target,
                             connection.offsetJacobian))
    return false;
  if (!computeTransfer(state, chain.vertices[count - 1].etaBack, receiver,
                       target, connection.transfer))
    return false;
  return true;
}

bool evaluateManifoldTransfer(const Scene &scene, const float3 &receiver,
                              const ManifoldTarget &target,
                              const ManifoldChain &chain, float &transfer) {
  const int count{chain.count};
  if (count < 1 || count > MNEE_MAX_DEPTH) return false;
  std::array<Hit, MNEE_MAX_DEPTH> hits{};
  std::array<float3, MNEE_MAX_DEPTH> frameSeeds{};
  for (int i = 0; i < count; i++) hits[i] = chain.vertices[i].hit;
  buildFrameSeeds(scene, hits, count, frameSeeds);
  ChainState state{};
  if (!evaluateChain(scene, receiver, target, chain, frameSeeds, hits, state))
    return false;
  // A chain that refracted through these interfaces satisfies the
  // constraints up to float noise in the interpolated normals; a large
  // residual means the caller handed over something that is not a
  // solution.
  if (!(state.residual() < RESIDUAL_SANITY)) return false;
  return computeTransfer(state, chain.vertices[count - 1].etaBack, receiver,
                         target, transfer);
}

bool evaluateManifoldOffsets(const Scene &scene, const float3 &receiver,
                             const ManifoldTarget &target,
                             ManifoldChain &chain) {
  const int count{chain.count};
  if (count < 1 || count > MNEE_MAX_DEPTH) return false;
  std::array<Hit, MNEE_MAX_DEPTH> hits{};
  std::array<float3, MNEE_MAX_DEPTH> frameSeeds{};
  for (int i = 0; i < count; i++) {
    hits[i] = chain.vertices[i].hit;
    // Read the constraint itself rather than its residual against whatever
    // was there, which is what makes this the inverse of the solve.
    chain.vertices[i].offset = float2();
  }
  buildFrameSeeds(scene, hits, count, frameSeeds);
  ChainState state{};
  if (!evaluateChain(scene, receiver, target, chain, frameSeeds, hits, state))
    return false;
  for (int i = 0; i < count; i++) {
    if (!std::isfinite(state.C[2 * i + 0]) ||
        !std::isfinite(state.C[2 * i + 1]))
      return false;
    chain.vertices[i].offset = float2(state.C[2 * i + 0], state.C[2 * i + 1]);
  }
  return true;
}

bool manifoldSeedFrame(const Scene &scene, const Hit &hit, float3 &normal,
                       float3 &t1, float3 &t2) {
  if (hit.instance->isCurves()) return false;
  const auto geometry{scene.manifoldGeometry(hit)};
  normal = geometry.normal;
  float3 g{geometry.dPdu - dot(normal, geometry.dPdu) * normal};
  const float3 seed{smdl::tryNormalize(g) ? g : smdl::perpendicularTo(normal)};
  float3 t{seed - dot(normal, seed) * normal};
  if (!smdl::tryNormalize(t)) return false;
  t1 = t, t2 = cross(normal, t1);
  return true;
}

bool isSameManifoldSolution(const float3 &receiver, const ManifoldConnection &a,
                            const ManifoldConnection &b) {
  if (a.count != b.count) return false;
  for (int i = 0; i < a.count; i++) {
    const float scale{
        std::max(1e-3f, length(a.vertices[i].hit.point - receiver))};
    if (!(length(a.vertices[i].hit.point - b.vertices[i].hit.point) <
          MANIFOLD_IDENTITY_FRACTION * scale))
      return false;
  }
  return true;
}
