#include "CompileFixtures.h"
#include "Fixtures.h"

#include <cmath>
#include <utility>
#include <vector>

#include "smdl/Manifold.h"

using smdl::double3;
using smdl::float2;
using smdl::float3;

// The proof the solver is renderer-agnostic: these surfaces are closed
// forms, no ray tracer and no scene behind them. Planes perpendicular to
// z, a pool's wall and top, balls, and the unit sphere at the origin,
// each one smooth surface, parameterized by a local frame the walk's
// parameterization-invariance does not care about.
namespace {

// The planes perpendicular to z, one per distinct height, which a chain
// bounces between: `project()` re-anchors onto the pinned vertex's own
// plane, as the contract asks, so a vertex never migrates to another.
// `scale` is the parameterization the measure is supposed to be
// invariant to, and `normal` the shading normal, which need not be the
// planes' own.
class PlaneSurfaces final : public smdl::ManifoldSurfaces {
public:
  PlaneSurfaces() = default;

  explicit PlaneSurfaces(float scale) : mScale{scale} {}

  PlaneSurfaces(float scale, const float3 &normal)
      : mScale{scale}, mNormal{normal} {}

  [[nodiscard]] bool
  evaluateGeometry(const smdl::ManifoldVertex &,
                   smdl::ManifoldGeometry &geometry) const override {
    geometry = {};
    geometry.normal = mNormal;
    geometry.dPdu = float3(mScale, 0.0f, 0.0f);
    geometry.dPdv = float3(0.0f, mScale, 0.0f);
    geometry.Ng = float3(0.0f, 0.0f, 1.0f);
    return true;
  }

  [[nodiscard]] bool project(const smdl::ManifoldVertex &pin,
                             const double3 &origin, const double3 &target,
                             smdl::ManifoldVertex &moved) const override {
    const double z{pin.point.z};
    const double3 dir{target - origin};
    if (!(std::abs(dir.z) > 0.0)) return false;
    const double t{(z - origin.z) / dir.z};
    if (!(t > 0.0)) return false;
    moved = {};
    moved.point = origin + t * dir;
    moved.point.z = z;
    moved.surface = pin.surface;
    moved.coords = float3(moved.point) / mScale;
    return true;
  }

private:
  float mScale{1.0f};
  float3 mNormal{0.0f, 0.0f, 1.0f};
};

class SphereSurfaces final : public smdl::ManifoldSurfaces {
public:
  [[nodiscard]] bool
  evaluateGeometry(const smdl::ManifoldVertex &vertex,
                   smdl::ManifoldGeometry &geometry) const override {
    geometry = {};
    geometry.normal = normalize(float3(vertex.point));
    // Any local frame spanning the tangent plane serves as the
    // parameterization, as long as the normal derivatives correspond to
    // the position derivatives, which on a unit sphere is dN = dP.
    geometry.dPdu = smdl::perpendicularTo(geometry.normal);
    geometry.dPdv = cross(geometry.normal, geometry.dPdu);
    geometry.dNdu = geometry.dPdu;
    geometry.dNdv = geometry.dPdv;
    geometry.Ng = geometry.normal;
    return true;
  }

  [[nodiscard]] bool project(const smdl::ManifoldVertex &,
                             const double3 &origin, const double3 &target,
                             smdl::ManifoldVertex &moved) const override {
    double3 dir{target - origin};
    if (!smdl::tryNormalize(dir)) return false;
    // The first sphere hit along the ray, like a ray tracer would report.
    const double b{dot(origin, dir)};
    const double c{lengthSquared(origin) - 1.0};
    const double disc{b * b - c};
    if (!(disc > 0.0)) return false;
    const double sqrtDisc{std::sqrt(disc)};
    double t{-b - sqrtDisc};
    if (!(t > 1e-5)) t = -b + sqrtDisc;
    if (!(t > 1e-5)) return false;
    moved = {};
    moved.point = normalize(origin + t * dir);
    moved.coords = moved.point;
    return true;
  }
};

// A problem of `count` mirror bounces visiting z = 0, 1, 0, 1, ...,
// seeded off the solution so that the walk has to find it rather than
// start on it. The light has to be on the side the last bounce
// leaves toward, which alternates with the depth; see
// `mirrorProblemDirection()`.
void seedMirrorProblem(smdl::ManifoldProblem &problem, int count) {
  problem.restart(count);
  problem.residualTolerance = 1e-6f;
  for (int i = 0; i < count; i++) {
    smdl::ManifoldProblemVertex &seed{problem.append()};
    const float away{static_cast<float>(i)};
    seed.vertex.point =
        float3(0.02f * away, 0.01f * away, i % 2 == 0 ? 0.0f : 1.0f);
    seed.vertex.surface = static_cast<uint64_t>(i % 2);
    seed.vertex.coords = seed.vertex.point;
    seed.etaPrev = seed.etaNext = 1.0f;
    seed.sideSign = 1.0f;
    seed.isReflection = true;
  }
}

[[nodiscard]] float3 mirrorProblemDirection(int count) {
  return normalize(float3(-0.15f, 0.25f, count % 2 == 0 ? -0.95f : 0.95f));
}

// The numeric |d omega_r / d omega_l| by central differences of the
// solved receiver direction, the ground truth every solution's measure
// must agree with. A finite target is perturbed on the plane through it
// perpendicular to the straight line, which is the unoriented
// convention the measure is expressed in.
[[nodiscard]] double
finiteDifferenceMeasure(const smdl::ManifoldSurfaces &surfaces,
                        smdl::ManifoldSolver &solver, const double3 &receiver,
                        const smdl::ManifoldTarget &target,
                        const smdl::ManifoldProblem &problem) {
  constexpr float STEP{2e-3f};
  const float3 a1{smdl::perpendicularTo(target.wl)};
  const float3 a2{cross(target.wl, a1)};
  float3 dwr[2]{};
  for (int k = 0; k < 2; k++) {
    const float3 &axis{k == 0 ? a1 : a2};
    float3 wr[2]{};
    for (int side = 0; side < 2; side++) {
      const float sign{side == 0 ? +1.0f : -1.0f};
      smdl::ManifoldTarget perturbed{target};
      if (target.isInfinite) {
        perturbed.wl = normalize(target.wl + sign * STEP * axis);
      } else {
        const float distStraight{float(length(target.point - receiver))};
        perturbed.point =
            target.point + double3(sign * STEP * distStraight * axis);
        perturbed.wl = normalize(float3(perturbed.point - receiver));
      }
      smdl::ManifoldSolution solution{};
      REQUIRE(solver.solve(surfaces, receiver, perturbed, problem, solution));
      wr[side] = solution.wr;
    }
    dwr[k] = (wr[0] - wr[1]) / (2.0f * STEP);
  }
  return length(cross(dwr[0], dwr[1]));
}

} // namespace

TEST_CASE("Manifold: a connection through a flat mirror") {
  // The workspace every solve of this case runs in, as a renderer
  // keeps one per thread.
  smdl::ManifoldSolver solver{};
  const PlaneSurfaces surfaces{};
  const float3 receiver{0.5f, -0.8f, 1.2f};
  smdl::ManifoldProblem problem{};
  problem.restart(1);
  smdl::ManifoldProblemVertex &seed{problem.append()};
  problem.residualTolerance = 1e-6f;
  seed.vertex.point = float3(0.3f, 0.2f, 0.0f);
  seed.vertex.coords = seed.vertex.point;
  seed.etaPrev = seed.etaNext = 1.0f;
  seed.sideSign = 1.0f;
  seed.isReflection = true;
  SUBCASE("Distant light reflects at measure exactly 1") {
    // The reflected connection is the mirrored light direction
    // independent of the receiver, the one closed form clean enough to
    // hold the measure to directly.
    smdl::ManifoldTarget target{};
    target.wl = normalize(float3(-0.2f, 0.35f, 0.91f));
    smdl::ManifoldSolution solution{};
    REQUIRE(solver.solve(surfaces, receiver, target, problem, solution));
    CHECK(solution.measure(problem) == doctest::Approx(1.0).epsilon(1e-3));
    // And the solved direction is the mirrored light direction, which
    // is where the virtual image of a distant light sits.
    const float3 mirrored{target.wl.x, target.wl.y, -target.wl.z};
    CHECK(dot(solution.wr, mirrored) == doctest::Approx(1.0).epsilon(1e-5));
    CHECK(
        finiteDifferenceMeasure(surfaces, solver, receiver, target, problem) ==
        doctest::Approx(solution.measure(problem)).epsilon(0.02));
  }
  SUBCASE("Finite light agrees with finite differences and the image") {
    smdl::ManifoldTarget target{};
    target.point = float3(-1.3f, 0.9f, 2.1f);
    target.wl = normalize(target.point - receiver);
    target.isInfinite = false;
    smdl::ManifoldSolution solution{};
    REQUIRE(solver.solve(surfaces, receiver, target, problem, solution));
    // The connection passes through the mirror image of the light.
    const double3 image{target.point.x, target.point.y, -target.point.z};
    CHECK(dot(solution.wr, normalize(float3(image) - receiver)) ==
          doctest::Approx(1.0).epsilon(1e-5));
    CHECK(
        finiteDifferenceMeasure(surfaces, solver, receiver, target, problem) ==
        doctest::Approx(solution.measure(problem)).epsilon(0.02));
  }
}

TEST_CASE("Manifold: a bounce is judged against the shading normal") {
  // A mirror whose shading normal leans from its plane's. The material
  // scatters about the shading normal, so the connection reflects about
  // it, and the cosines it reports are against it.
  smdl::ManifoldSolver solver{};
  const float3 normal{normalize(float3(0.3f, -0.2f, 1.0f))};
  const PlaneSurfaces surfaces{1.0f, normal};
  const float3 receiver{0.5f, -0.8f, 1.2f};
  smdl::ManifoldProblem problem{};
  problem.restart(1);
  smdl::ManifoldProblemVertex &seed{problem.append()};
  seed.vertex.point = float3(0.3f, 0.2f, 0.0f);
  seed.vertex.coords = seed.vertex.point;
  seed.etaPrev = seed.etaNext = 1.0f;
  seed.sideSign = 1.0f;
  seed.isReflection = true;
  smdl::ManifoldTarget target{};
  target.wl = normalize(float3(-0.2f, 0.35f, 0.91f));
  smdl::ManifoldSolution solution{};
  REQUIRE(solver.solve(surfaces, receiver, target, problem, solution));
  const smdl::ManifoldSolutionVertex &bounce{solution.vertices[0]};
  CHECK_NEAR(solution.wr, target.wl - 2.0f * dot(target.wl, normal) * normal,
             1e-5f);
  CHECK(bounce.cosPrev ==
        doctest::Approx(dot(bounce.wPrev, normal)).epsilon(1e-5));
  CHECK(bounce.cosNext ==
        doctest::Approx(dot(bounce.wNext, normal)).epsilon(1e-5));
  // Which the cosine against the plane's own normal is not.
  CHECK(std::abs(bounce.cosPrev - bounce.wPrev.z) > 0.05f);
}

TEST_CASE("Manifold: a chain of planar mirrors") {
  // The case for the coupling. A planar mirror preserves solid angle
  // and so does a composition of them, so the measure is exactly 1
  // however deep the chain runs: the one analytic value a chain of any
  // depth can be held to, and the depths past three are where the
  // constraint system stops being all corner and starts being all band.
  smdl::ManifoldSolver solver{};
  const PlaneSurfaces surfaces{};
  // Between the mirrors, so the chain alternates down and up.
  const float3 receiver{0.3f, -0.2f, 0.5f};
  SUBCASE("Every depth reflects at measure exactly 1") {
    for (const int count : {1, 2, 3, 4, 8}) {
      CAPTURE(count);
      smdl::ManifoldProblem problem{};
      seedMirrorProblem(problem, count);
      // The last bounce leaves toward the light, so which side of the
      // mirrors the light has to be on alternates with the depth.
      smdl::ManifoldTarget target{};
      target.wl = mirrorProblemDirection(count);
      smdl::ManifoldSolution solution{};
      REQUIRE(solver.solve(surfaces, receiver, target, problem, solution));
      CHECK(solution.size() == count);
      CHECK(solution.measure(problem) == doctest::Approx(1.0).epsilon(1e-3));
      // Each reflection flips the z of a direction and leaves the rest
      // alone, whatever the mirrors' heights, so the virtual image of a
      // distant light is its direction with the z flipped once per
      // bounce.
      const float3 mirrored{target.wl.x, target.wl.y,
                            count % 2 == 0 ? target.wl.z : -target.wl.z};
      CHECK(dot(solution.wr, mirrored) == doctest::Approx(1.0).epsilon(1e-5));
      CHECK(finiteDifferenceMeasure(surfaces, solver, receiver, target,
                                    problem) ==
            doctest::Approx(solution.measure(problem)).epsilon(0.02));
    }
  }
  SUBCASE("A finite light passes through the composed image") {
    constexpr int COUNT{3};
    smdl::ManifoldProblem problem{};
    seedMirrorProblem(problem, COUNT);
    smdl::ManifoldTarget target{};
    target.point = float3(-1.3f, 0.9f, 2.1f);
    target.wl = normalize(target.point - receiver);
    target.isInfinite = false;
    smdl::ManifoldSolution solution{};
    REQUIRE(solver.solve(surfaces, receiver, target, problem, solution));
    // The image of a finite light is the light reflected through the
    // mirrors from the far end back, each one taking z to 2z - z.
    float3 image{target.point};
    for (int i = COUNT - 1; i >= 0; i--)
      image.z = 2.0f * problem.vertices[i].vertex.point.z - image.z;
    CHECK(dot(solution.wr, normalize(image - receiver)) ==
          doctest::Approx(1.0).epsilon(1e-5));
    CHECK(
        finiteDifferenceMeasure(surfaces, solver, receiver, target, problem) ==
        doctest::Approx(solution.measure(problem)).epsilon(0.02));
  }
}

TEST_CASE("Manifold: the measure is invariant to the parameterization") {
  // The area elements and the constraint determinant are each expressed
  // in whatever parameterization the surfaces report, and only their
  // ratio means anything, so the same geometry under a finer one has to
  // give the same measure. It is the determinant that makes this worth
  // a case: it is a product of two entries per bounce, each scaling
  // with the parameterization, so it leaves the range a float can hold
  // long before the ratio it belongs to goes anywhere.
  constexpr int COUNT{4};
  smdl::ManifoldSolver solver{};
  const float3 receiver{0.3f, -0.2f, 0.5f};
  smdl::ManifoldProblem problem{};
  seedMirrorProblem(problem, COUNT);
  smdl::ManifoldTarget target{};
  target.wl = mirrorProblemDirection(COUNT);
  for (const float scale : {1.0f, 1e-3f, 1e-5f, 1e-7f}) {
    CAPTURE(scale);
    const PlaneSurfaces surfaces{scale};
    smdl::ManifoldSolution solution{};
    REQUIRE(solver.solve(surfaces, receiver, target, problem, solution));
    CHECK(solution.measure(problem) == doctest::Approx(1.0).epsilon(1e-3));
  }
}

TEST_CASE("Manifold: a connection refracted through a sphere") {
  // The workspace every solve of this case runs in, as a renderer
  // keeps one per thread.
  smdl::ManifoldSolver solver{};
  const SphereSurfaces surfaces{};
  // The receiver on the axis inside the unit sphere, on the dense side,
  // and the light distant along the axis: by symmetry the crossing is
  // exactly the pole, the one refractive configuration with an analytic
  // solution to converge to. The seed starts well off the pole to make
  // the walk earn it.
  const float3 receiver{0.0f, 0.0f, 0.3f};
  smdl::ManifoldProblem problem{};
  problem.restart(1);
  smdl::ManifoldProblemVertex &seed{problem.append()};
  problem.residualTolerance = 1e-6f;
  seed.vertex.point = normalize(float3(0.25f, -0.2f, 0.94f));
  seed.vertex.coords = seed.vertex.point;
  seed.etaPrev = 1.5f;
  seed.etaNext = 1.0f;
  seed.sideSign = 1.0f;
  SUBCASE("The axial connection converges to the pole") {
    smdl::ManifoldTarget target{};
    target.wl = float3(0.0f, 0.0f, 1.0f);
    smdl::ManifoldSolution solution{};
    REQUIRE(solver.solve(surfaces, receiver, target, problem, solution));
    CHECK(std::abs(solution.vertices[0].vertex.point.x) < 1e-4f);
    CHECK(std::abs(solution.vertices[0].vertex.point.y) < 1e-4f);
    CHECK(solution.vertices[0].vertex.point.z ==
          doctest::Approx(1.0).epsilon(1e-5));
    CHECK(
        finiteDifferenceMeasure(surfaces, solver, receiver, target, problem) ==
        doctest::Approx(solution.measure(problem)).epsilon(0.02));
  }
  SUBCASE("Off-axis targets agree with finite differences") {
    for (const auto &wl : {normalize(float3(0.3f, 0.1f, 0.95f)),
                           normalize(float3(-0.2f, 0.25f, 0.9f))}) {
      smdl::ManifoldTarget target{};
      target.wl = wl;
      smdl::ManifoldSolution solution{};
      REQUIRE(solver.solve(surfaces, receiver, target, problem, solution));
      CHECK(finiteDifferenceMeasure(surfaces, solver, receiver, target,
                                    problem) ==
            doctest::Approx(solution.measure(problem)).epsilon(0.02));
    }
  }
  SUBCASE("A finite light agrees with finite differences") {
    smdl::ManifoldTarget target{};
    target.point = float3(0.4f, -0.3f, 3.0f);
    target.wl = normalize(target.point - receiver);
    target.isInfinite = false;
    smdl::ManifoldSolution solution{};
    REQUIRE(solver.solve(surfaces, receiver, target, problem, solution));
    CHECK(
        finiteDifferenceMeasure(surfaces, solver, receiver, target, problem) ==
        doctest::Approx(solution.measure(problem)).epsilon(0.02));
  }
}

TEST_CASE("Manifold: a chain refracted in and out of a sphere") {
  // The coupled case whose blocks carry real numbers. A sphere's
  // shading normal turns with the surface, so the frame terms the
  // diagonal blocks pick up are nonzero, which the mirrors leave empty;
  // and both of its bounces refract, which the mirrors' do not.
  // The receiver and a distant light on the axis put both crossings on
  // the poles by symmetry, the one two-crossing configuration with an
  // analytic solution to converge to.
  smdl::ManifoldSolver solver{};
  const SphereSurfaces surfaces{};
  const float3 receiver{0.0f, 0.0f, -3.0f};
  smdl::ManifoldProblem problem{};
  problem.restart(2);
  problem.residualTolerance = 1e-6f;
  // Entering the glass at the near pole and leaving it at the far one,
  // seeded well off both so the walk has to find them.
  smdl::ManifoldProblemVertex &entry{problem.append()};
  entry.vertex.point = normalize(float3(0.22f, -0.17f, -0.95f));
  entry.vertex.coords = entry.vertex.point;
  entry.etaPrev = 1.0f;
  entry.etaNext = 1.5f;
  entry.sideSign = -1.0f;
  smdl::ManifoldProblemVertex &exit{problem.append()};
  exit.vertex.point = normalize(float3(-0.15f, 0.2f, 0.96f));
  exit.vertex.coords = exit.vertex.point;
  exit.etaPrev = 1.5f;
  exit.etaNext = 1.0f;
  exit.sideSign = 1.0f;
  SUBCASE("The axial connection converges to the poles") {
    smdl::ManifoldTarget target{};
    target.wl = float3(0.0f, 0.0f, 1.0f);
    smdl::ManifoldSolution solution{};
    REQUIRE(solver.solve(surfaces, receiver, target, problem, solution));
    REQUIRE(solution.size() == 2);
    CHECK_NEAR(solution.vertices[0].vertex.point, double3(0.0, 0.0, -1.0),
               1e-4);
    CHECK_NEAR(solution.vertices[1].vertex.point, double3(0.0, 0.0, 1.0), 1e-4);
    // On the axis the connection never bends, so it leaves the receiver
    // straight at the light.
    CHECK_NEAR(solution.wr, target.wl, 1e-5f);
    CHECK(
        finiteDifferenceMeasure(surfaces, solver, receiver, target, problem) ==
        doctest::Approx(solution.measure(problem)).epsilon(0.02));
  }
  SUBCASE("Off-axis targets agree with finite differences") {
    for (const auto &wl : {normalize(float3(0.06f, 0.03f, 0.998f)),
                           normalize(float3(-0.05f, 0.07f, 0.996f))}) {
      smdl::ManifoldTarget target{};
      target.wl = wl;
      smdl::ManifoldSolution solution{};
      REQUIRE(solver.solve(surfaces, receiver, target, problem, solution));
      CHECK(finiteDifferenceMeasure(surfaces, solver, receiver, target,
                                    problem) ==
            doctest::Approx(solution.measure(problem)).epsilon(0.02));
    }
  }
}

TEST_CASE("Manifold: a slab is solved however thin and wherever it stands") {
  // A pane of glass between two of the planes, under a distant light.
  // A slab bends no direction, so the connection leaves the receiver
  // straight at the light at measure 1, and the segment inside obeys
  // Snell's law, which is the closed form the walk is held to. The
  // segment is as long as the pane is thick, and what the walk makes of
  // it must not depend on how many digits the pane's coordinates spend
  // on where it stands, nor the measure on how far the receiver stands
  // from a pane that thin: the constraint Jacobian's entries are of the
  // order of one over the thickness, and its determinant is what they
  // leave of one over the distance.
  constexpr float IOR{1.5f};
  smdl::ManifoldSolver solver{};
  const PlaneSurfaces surfaces{};
  const auto solveSlab{[&](const double3 &receiver, const float3 &wl,
                           double distance, double thickness, bool isRounded) {
    smdl::ManifoldProblem problem{};
    problem.restart(2);
    problem.residualTolerance = smdl::MANIFOLD_RESIDUAL;
    // Seeded where the straight line crosses, as a discovery would.
    for (int i = 0; i < 2; i++) {
      smdl::ManifoldProblemVertex &seed{problem.append()};
      const double z{distance + (i == 0 ? 0.0 : thickness)};
      double3 point{receiver + (z - receiver.z) / double(wl.z) * double3(wl)};
      point.z = z;
      seed.vertex.point = isRounded ? double3(float3(point)) : point;
      seed.vertex.surface = uint64_t(i);
      seed.vertex.coords = float3(point);
      seed.etaPrev = i == 0 ? 1.0f : IOR;
      seed.etaNext = i == 0 ? IOR : 1.0f;
      seed.sideSign = 1.0f;
    }
    smdl::ManifoldTarget target{};
    target.wl = wl;
    smdl::ManifoldSolution solution{};
    smdl::ManifoldWalkReport report{};
    REQUIRE(
        solver.solve(surfaces, receiver, target, problem, solution, &report));
    CHECK(report.residual < smdl::MANIFOLD_RESIDUAL);
    CHECK(dot(solution.wr, wl) > 1.0f - 1e-6f);
    // The segment inside, differenced as the walk differences it.
    const double3 inside{normalize(solution.vertices[1].vertex.point -
                                   solution.vertices[0].vertex.point)};
    const double sinOutside{std::sqrt(1.0 - double(wl.z) * double(wl.z))};
    const double sinInside{std::sqrt(1.0 - inside.z * inside.z)};
    CHECK(std::abs(sinInside * double(IOR) - sinOutside) < 1e-5);
    CHECK(std::abs(double(solution.measure(problem)) - 1.0) < 2e-6);
  }};
  for (const double offset : {0.0, 100.0, 1000.0})
    for (const double distance : {1.0, 30.0, 1000.0})
      for (const double thickness : {0.2, 2e-3, 5e-5}) {
        CAPTURE(offset);
        CAPTURE(distance);
        CAPTURE(thickness);
        for (int k = 0; k < 64; k++) {
          const double a{std::fmod(0.618034 * double(k + 1), 1.0)};
          const double b{std::fmod(0.754878 * double(k + 2), 1.0)};
          solveSlab(
              double3(offset + 2.0 * a - 1.0, offset + 2.0 * b - 1.0, 0.0),
              normalize(float3(0.4f * float(b - 0.5), 0.5f, 0.85f)), distance,
              thickness, /*isRounded=*/false);
        }
      }
  SUBCASE("A seed rounded to a float is re-anchored by the first step") {
    for (int k = 0; k < 64; k++) {
      const double a{std::fmod(0.618034 * double(k + 1), 1.0)};
      const double b{std::fmod(0.754878 * double(k + 2), 1.0)};
      solveSlab(double3(100.0 + 2.0 * a - 1.0, 100.0 + 2.0 * b - 1.0, 0.0),
                normalize(float3(0.4f * float(b - 0.5), 0.5f, 0.85f)), 1.0,
                2e-3, /*isRounded=*/true);
    }
  }
}

namespace {

// The faces of lens elements along the z axis, each a sphere, in the
// order a chain from a receiver below them crosses: into the glass at
// the even ones and out of it at the odd. `ManifoldVertex::surface`
// says which.
class LensSurfaces final : public smdl::ManifoldSurfaces {
public:
  class Face final {
  public:
    double3 center{};
    double radius{};
    // The point of the face on the axis, which tells the place a line
    // meets the face from the other place it meets the sphere.
    double3 pole{};
  };

  // Biconvex elements of glass `thickness` on the axis, their faces of
  // radius `radius`, the first at the height `distance` and each next
  // one `gap` above the last.
  LensSurfaces(int numElements, double distance, double thickness,
               double radius, double gap) {
    for (int i = 0; i < numElements; i++) {
      const double z{distance + double(i) * (thickness + gap)};
      mFaces.push_back(
          {double3(0.0, 0.0, z + radius), radius, double3(0.0, 0.0, z)});
      mFaces.push_back({double3(0.0, 0.0, z + thickness - radius), radius,
                        double3(0.0, 0.0, z + thickness)});
    }
  }

  [[nodiscard]] int faceCount() const noexcept {
    return static_cast<int>(mFaces.size());
  }

  [[nodiscard]] bool
  evaluateGeometry(const smdl::ManifoldVertex &vertex,
                   smdl::ManifoldGeometry &geometry) const override {
    const Face &face{mFaces[vertex.surface]};
    geometry = {};
    geometry.normal = normalize(float3(vertex.point - face.center));
    geometry.dPdu = smdl::perpendicularTo(geometry.normal);
    geometry.dPdv = cross(geometry.normal, geometry.dPdu);
    geometry.dNdu = geometry.dPdu / float(face.radius);
    geometry.dNdv = geometry.dPdv / float(face.radius);
    geometry.Ng = geometry.normal;
    return true;
  }

  [[nodiscard]] bool project(const smdl::ManifoldVertex &pin,
                             const double3 &origin, const double3 &target,
                             smdl::ManifoldVertex &moved) const override {
    double3 dir{target - origin};
    if (!smdl::tryNormalize(dir)) return false;
    moved = {};
    moved.surface = pin.surface;
    return meet(int(pin.surface), origin, dir, moved.point);
  }

  // Where the line from `origin` along the unit `dir` meets a face.
  [[nodiscard]] bool meet(int index, const double3 &origin, const double3 &dir,
                          double3 &point) const {
    const Face &face{mFaces[index]};
    const double3 offset{origin - face.center};
    const double b{dot(offset, dir)};
    const double disc{b * b - lengthSquared(offset) +
                      face.radius * face.radius};
    if (!(disc > 0.0)) return false;
    const double3 near{origin + (-b - std::sqrt(disc)) * dir};
    const double3 far{origin + (-b + std::sqrt(disc)) * dir};
    point = lengthSquared(near - face.pole) < lengthSquared(far - face.pole)
                ? near
                : far;
    point = face.center + face.radius * normalize(point - face.center);
    return dot(point - origin, dir) > 0.0;
  }

  // The direction a ray from `origin` along `dir` leaves the last face
  // in, bent at every face by Snell's law, and where it crossed each:
  // the trace a connection through the elements is held to.
  [[nodiscard]] bool trace(double3 origin, double3 dir, double ior,
                           std::vector<double3> &points,
                           double3 &leaving) const {
    points.resize(mFaces.size());
    for (int i = 0; i < faceCount(); i++) {
      if (!meet(i, origin, dir, points[i])) return false;
      double3 normal{normalize(points[i] - mFaces[i].center)};
      double cosFrom{-dot(dir, normal)};
      if (cosFrom < 0.0) normal = -normal, cosFrom = -cosFrom;
      const double ratio{i % 2 == 0 ? 1.0 / ior : ior};
      const double cosToSquared{1.0 -
                                ratio * ratio * (1.0 - cosFrom * cosFrom)};
      if (!(cosToSquared > 0.0)) return false;
      origin = points[i];
      dir = normalize(ratio * dir +
                      (ratio * cosFrom - std::sqrt(cosToSquared)) * normal);
    }
    leaving = dir;
    return true;
  }

  // |d omega_r / d omega_l| where the ray that leaves `origin` along
  // `wr` leaves the elements, by central differences of the trace.
  [[nodiscard]] bool tracedMeasure(const double3 &origin, const double3 &wr,
                                   double ior, double &measure) const {
    constexpr double STEP{1e-5};
    const double3 axes[2]{smdl::perpendicularTo(wr),
                          cross(wr, smdl::perpendicularTo(wr))};
    std::vector<double3> points{};
    double3 derivs[2]{};
    for (int k = 0; k < 2; k++) {
      double3 leaving[2]{};
      for (int side = 0; side < 2; side++)
        if (!trace(origin, normalize(wr + (side == 0 ? STEP : -STEP) * axes[k]),
                   ior, points, leaving[side]))
          return false;
      derivs[k] = (leaving[0] - leaving[1]) / (2.0 * STEP);
    }
    measure = 1.0 / length(cross(derivs[0], derivs[1]));
    return std::isfinite(measure);
  }

private:
  std::vector<Face> mFaces{};
};

} // namespace

TEST_CASE("Manifold: the measure through thin glass is the traced one") {
  // A lens, and two of them, of glass as thin as a wall of a vessel,
  // under a distant light. The measure of a connection is the solid
  // angle at the receiver to one of light, which a bundle of rays traced
  // from the receiver in double says on its own. The glass being thin
  // and its faces curved, the entries of the constraint Jacobian are of
  // the order of one over the thickness and its determinant of one over
  // the focal length or the receiver's distance, so the measure is what
  // the arithmetic leaves of the difference. The walk is asked for a
  // residual under its own, so that where it stopped is less of what is
  // measured, and no less than the normals it is handed let every walk
  // reach, which are floats.
  constexpr double IOR{1.5};
  constexpr double TWO_PI{6.283185307179586};
  smdl::ManifoldSolver solver{};
  std::vector<double3> points{};
  for (const int numElements : {1, 2})
    for (const double thickness : {2e-3, 5e-4})
      for (const double distance : {1.0, 10.0, 100.0}) {
        CAPTURE(numElements);
        CAPTURE(thickness);
        CAPTURE(distance);
        const LensSurfaces surfaces{numElements, distance, thickness,
                                    /*radius=*/2.5, /*gap=*/0.05};
        for (int k = 0; k < 64; k++) {
          CAPTURE(k);
          const double a{std::fmod(0.618034 * double(k + 1), 1.0)};
          const double b{std::fmod(0.754878 * double(k + 2), 1.0)};
          const double c{std::fmod(0.569840 * double(k + 3), 1.0)};
          const double d{std::fmod(0.819173 * double(k + 4), 1.0)};
          const double3 receiver{0.3 * std::sqrt(a) * std::cos(TWO_PI * b),
                                 0.3 * std::sqrt(a) * std::sin(TWO_PI * b),
                                 0.0};
          // The light is wherever the ray through a point of the first
          // element leaves for, so that there is a solution to find.
          const double3 aim{0.02 * std::sqrt(c) * std::cos(TWO_PI * d),
                            0.02 * std::sqrt(c) * std::sin(TWO_PI * d),
                            distance};
          double3 leaving{};
          REQUIRE(surfaces.trace(receiver, normalize(aim - receiver), IOR,
                                 points, leaving));
          smdl::ManifoldTarget target{};
          target.wl = normalize(float3(leaving));
          // Seeded where a ray beside the connection crosses, by less
          // than the glass is thick.
          REQUIRE(surfaces.trace(
              receiver,
              normalize(aim + thickness * double3(b - 0.5, a - 0.5, 0.0) -
                        receiver),
              IOR, points, leaving));
          smdl::ManifoldProblem problem{};
          problem.restart(surfaces.faceCount());
          problem.residualTolerance = 1e-6f;
          for (int i = 0; i < surfaces.faceCount(); i++) {
            smdl::ManifoldProblemVertex &seed{problem.append()};
            seed.vertex.point = points[i];
            seed.vertex.surface = uint64_t(i);
            seed.etaPrev = i % 2 == 0 ? 1.0f : float(IOR);
            seed.etaNext = i % 2 == 0 ? float(IOR) : 1.0f;
            seed.sideSign = i % 2 == 0 ? -1.0f : 1.0f;
            // A frame seed well out of the tangent plane and of no unit
            // length, as a renderer may hand one: the measure is of the
            // solution and not of the frame it was solved in.
            seed.tangentAim = float3(0.9f, -0.4f, 0.7f);
          }
          smdl::ManifoldSolution solution{};
          smdl::ManifoldWalkReport report{};
          REQUIRE(solver.solve(surfaces, receiver, target, problem, solution,
                               &report));
          CHECK(report.residual < 1e-6f);
          double traced{};
          REQUIRE(surfaces.tracedMeasure(receiver, double3(solution.wr), IOR,
                                         traced));
          CHECK(std::abs(double(solution.measure(problem)) / traced - 1.0) <
                2e-5);
        }
      }
}

namespace {

// Balls about the origin, each inside the one before it, and the index
// inside each: a vessel and what it holds. `ManifoldVertex::surface`
// says which ball.
class BallSurfaces final : public smdl::ManifoldSurfaces {
public:
  class Ball final {
  public:
    double radius{};
    double ior{};
  };

  // Where a line crosses a ball, and the indices either side of it in
  // the order the line meets them.
  class Crossing final {
  public:
    int ball{};
    double3 point{};
    double etaPrev{};
    double etaNext{};
  };

  explicit BallSurfaces(std::vector<Ball> balls) : mBalls{std::move(balls)} {}

  [[nodiscard]] bool
  evaluateGeometry(const smdl::ManifoldVertex &vertex,
                   smdl::ManifoldGeometry &geometry) const override {
    const double radius{mBalls[vertex.surface].radius};
    geometry = {};
    geometry.normal = normalize(float3(vertex.point));
    geometry.dPdu = smdl::perpendicularTo(geometry.normal);
    geometry.dPdv = cross(geometry.normal, geometry.dPdu);
    geometry.dNdu = geometry.dPdu / float(radius);
    geometry.dNdv = geometry.dPdv / float(radius);
    geometry.Ng = geometry.normal;
    return true;
  }

  // As a renderer's cast: whatever ball the line meets first is what it
  // lands on, and a landing on another than the pinned one fails.
  [[nodiscard]] bool project(const smdl::ManifoldVertex &pin,
                             const double3 &origin, const double3 &target,
                             smdl::ManifoldVertex &moved) const override {
    double3 dir{target - origin};
    const double distance{length(dir)};
    if (!smdl::tryNormalize(dir)) return false;
    int ball{};
    moved = {};
    if (!cast(origin, dir, std::min(1e-4, 0.25 * distance), ball,
              moved.point) ||
        uint64_t(ball) != pin.surface)
      return false;
    moved.surface = pin.surface;
    return true;
  }

  // The first crossing of any ball along a ray, past `margin`.
  [[nodiscard]] bool cast(const double3 &origin, const double3 &dir,
                          double margin, int &ball, double3 &point) const {
    double nearest{INFINITY};
    for (int i = 0; i < int(mBalls.size()); i++) {
      const double b{dot(origin, dir)};
      const double disc{b * b - lengthSquared(origin) +
                        mBalls[i].radius * mBalls[i].radius};
      if (!(disc > 0.0)) continue;
      for (const double t : {-b - std::sqrt(disc), -b + std::sqrt(disc)})
        if (t > margin && t < nearest) nearest = t, ball = i;
    }
    if (!(nearest < INFINITY)) return false;
    point = mBalls[ball].radius * normalize(origin + nearest * dir);
    return true;
  }

  // Follow a ray through the balls until it has left them, bent at
  // every crossing by Snell's law where `bends` and straight through
  // where not, as a discovery follows a shadow segment. False where the
  // ray misses them or is totally reflected.
  [[nodiscard]] bool trace(double3 origin, double3 dir, bool bends,
                           std::vector<Crossing> &crossings,
                           double3 &leavingFrom, double3 &leaving) const {
    crossings.clear();
    int ball{};
    double3 point{};
    while (crossings.size() < 2 * mBalls.size() &&
           cast(origin, dir, 1e-9, ball, point)) {
      const double3 outward{normalize(point)};
      const bool isEntering{dot(dir, outward) < 0.0};
      const double etaOutside{ball == 0 ? 1.0 : mBalls[ball - 1].ior};
      const double etaInside{mBalls[ball].ior};
      const Crossing crossing{ball, point, isEntering ? etaOutside : etaInside,
                              isEntering ? etaInside : etaOutside};
      crossings.push_back(crossing);
      if (bends) {
        const double3 normal{isEntering ? outward : -outward};
        const double cosFrom{-dot(dir, normal)};
        const double ratio{crossing.etaPrev / crossing.etaNext};
        const double cosToSquared{1.0 -
                                  ratio * ratio * (1.0 - cosFrom * cosFrom)};
        if (!(cosToSquared > 0.0)) return false;
        dir = normalize(ratio * dir +
                        (ratio * cosFrom - std::sqrt(cosToSquared)) * normal);
      }
      origin = point;
    }
    leavingFrom = origin;
    leaving = dir;
    return !crossings.empty();
  }

private:
  std::vector<Ball> mBalls{};
};

// How many problems through some balls a walk solves, seeded where the
// straight line crosses as a discovery seeds them, from receivers on a
// floor under the balls to the points of a lamp over them. Whatever
// solution a walk comes to must obey Snell's law at every crossing.
class FoundThrough final {
public:
  FoundThrough(std::vector<BallSurfaces::Ball> balls, bool isThorough) {
    constexpr double TWO_PI{6.283185307179586};
    const int numCrossings{2 * int(balls.size())};
    const BallSurfaces surfaces{std::move(balls)};
    smdl::ManifoldSolver solver{};
    std::vector<BallSurfaces::Crossing> crossings{};
    for (int k = 0; k < 1024; k++) {
      const double a{std::fmod(0.618034 * double(k + 1), 1.0)};
      const double b{std::fmod(0.754878 * double(k + 2), 1.0)};
      const double c{std::fmod(0.569840 * double(k + 3), 1.0)};
      const double d{std::fmod(0.819173 * double(k + 4), 1.0)};
      const double3 receiver{1.4 * (a - 0.5) - 0.45, 1.4 * (b - 0.5) + 0.3,
                             -0.8};
      const double height{2.0 * c - 1.0};
      const double3 light{
          double3(1.2, -0.8, 2.2) +
          0.3 * double3(std::sqrt(1.0 - height * height) * std::cos(TWO_PI * d),
                        std::sqrt(1.0 - height * height) * std::sin(TWO_PI * d),
                        height)};
      const double3 wl{normalize(light - receiver)};
      double3 leavingFrom{}, leaving{};
      if (!surfaces.trace(receiver, wl, /*bends=*/false, crossings, leavingFrom,
                          leaving) ||
          int(crossings.size()) != numCrossings)
        continue;
      const size_t index{isFound.size()};
      isFound.push_back(false);
      iterationCounts.push_back(0);
      smdl::ManifoldProblem problem{};
      problem.restart(numCrossings);
      problem.isThorough = isThorough;
      for (const BallSurfaces::Crossing &crossing : crossings) {
        smdl::ManifoldProblemVertex &seed{problem.append()};
        seed.vertex.point = crossing.point;
        seed.vertex.surface = uint64_t(crossing.ball);
        seed.etaPrev = float(crossing.etaPrev);
        seed.etaNext = float(crossing.etaNext);
        seed.sideSign = dot(wl, crossing.point) < 0.0 ? -1.0f : 1.0f;
      }
      smdl::ManifoldTarget target{};
      target.wl = float3(wl);
      target.point = light;
      target.isInfinite = false;
      smdl::ManifoldSolution solution{};
      smdl::ManifoldWalkReport report{};
      isFound[index] =
          solver.solve(surfaces, receiver, target, problem, solution, &report);
      iterationCounts[index] = report.iterations;
      if (!isFound[index]) continue;
      foundCount++;
      for (int i = 0; i < numCrossings; i++) {
        CAPTURE(k);
        CAPTURE(i);
        const smdl::ManifoldSolutionVertex &crossing{solution.vertices[i]};
        const double3 normal{crossing.geometry.normal};
        const double3 half{
            double(problem[i].etaPrev) * double3(crossing.wPrev) +
            double(problem[i].etaNext) * double3(crossing.wNext)};
        CHECK(length(half - dot(half, normal) * normal) < 2e-5);
        CHECK(dot(double3(crossing.wPrev), normal) *
                  dot(double3(crossing.wNext), normal) <
              0.0);
      }
    }
  }

  [[nodiscard]] int problemCount() const noexcept {
    return int(isFound.size());
  }

  // Whether each problem was solved, and the iterations its solve reports.
  std::vector<bool> isFound{};
  std::vector<int> iterationCounts{};
  int foundCount{};
};

} // namespace

TEST_CASE("Manifold: a connection is found through glass that bends") {
  // A walk reaches the solution whose basin its start is in, and
  // where glass bends light much the straight line's crossings are far
  // from it. Through a vessel of water the inner interface is weak
  // besides, and the half vector there as long as the indices differ,
  // so that a walk moving every crossing for itself, which holds
  // Snell's law at the inner ones to first order, holds it nowhere
  // near. Each case needs one of the three ways of walking that the
  // other two do not make up for, so each holds one to its place.
  SUBCASE("A ball of glass, by a start that is traced") {
    const FoundThrough thorough{{{0.5, 1.5}}, /*isThorough=*/true};
    REQUIRE(thorough.problemCount() > 800);
    CHECK(thorough.foundCount > 0.88 * thorough.problemCount());
  }
  SUBCASE("A vessel of water, by a seed that is traced from") {
    const FoundThrough thorough{{{0.5, 1.5}, {0.498, 1.33}},
                                /*isThorough=*/true};
    const FoundThrough once{{{0.5, 1.5}, {0.498, 1.33}}, /*isThorough=*/false};
    REQUIRE(thorough.problemCount() > 800);
    REQUIRE(once.problemCount() == thorough.problemCount());
    CHECK(thorough.foundCount > 0.64 * thorough.problemCount());
    CHECK(once.foundCount > 0.62 * once.problemCount());
    // A problem that asks is walked again where a walk fails, and what
    // the solve reports is every walk's steps: no fewer than the one
    // walk's, and fewer than a walk takes that crosses the wall at half
    // its thickness a step.
    int numFailed{0}, numLonger{0}, numSteps{0};
    for (int k = 0; k < thorough.problemCount(); k++) {
      if (thorough.isFound[k]) continue;
      CAPTURE(k);
      CHECK(!once.isFound[k]);
      CHECK(thorough.iterationCounts[k] >= once.iterationCounts[k]);
      numLonger += thorough.iterationCounts[k] > once.iterationCounts[k];
      numSteps += thorough.iterationCounts[k];
      numFailed++;
    }
    CHECK(numLonger > numFailed / 2);
    CHECK(numSteps < 40 * numFailed);
  }
  SUBCASE("A hollow vessel, by every crossing stepping for itself") {
    // Light leaves it nearly as straight as it came, so the seed is
    // all but the solution, and a trace from the seed's first
    // crossing is not.
    const FoundThrough thorough{{{0.5, 1.5}, {0.45, 1.0}}, /*isThorough=*/true};
    const FoundThrough once{{{0.5, 1.5}, {0.45, 1.0}}, /*isThorough=*/false};
    REQUIRE(thorough.problemCount() > 700);
    CHECK(thorough.foundCount > 0.98 * thorough.problemCount());
    CHECK(once.foundCount < 0.95 * once.problemCount());
  }
}

TEST_CASE("Manifold: a problem restarted is as a fresh one") {
  smdl::ManifoldProblem problem{};
  problem.restart(2);
  problem.residualTolerance = 1e-6f;
  problem.isThorough = true;
  (void)problem.append();
  problem.restart(3);
  CHECK(problem.empty());
  CHECK(problem.residualTolerance == 0.0f);
  CHECK(!problem.isThorough);
}

TEST_CASE("Manifold: a problem of one bounce is walked once") {
  // It has nothing to trace, so its walks are one walk, and a problem
  // that asks to be walked again is not.
  smdl::ManifoldSolver solver{};
  const PlaneSurfaces surfaces{};
  const float3 receiver{0.5f, -0.8f, 1.2f};
  smdl::ManifoldTarget target{};
  // Under the mirror, where no reflection leaves for.
  target.wl = normalize(float3(-0.2f, 0.35f, -0.91f));
  int iterations[2]{};
  for (const bool isThorough : {false, true}) {
    smdl::ManifoldProblem problem{};
    problem.restart(1);
    problem.isThorough = isThorough;
    smdl::ManifoldProblemVertex &seed{problem.append()};
    seed.vertex.point = float3(0.3f, 0.2f, 0.0f);
    seed.vertex.coords = float3(seed.vertex.point);
    seed.etaPrev = seed.etaNext = 1.0f;
    seed.sideSign = 1.0f;
    seed.isReflection = true;
    smdl::ManifoldSolution solution{};
    smdl::ManifoldWalkReport report{};
    CHECK(
        !solver.solve(surfaces, receiver, target, problem, solution, &report));
    iterations[isThorough ? 1 : 0] = report.iterations;
  }
  CHECK(iterations[0] > 0);
  CHECK(iterations[1] == iterations[0]);
}

TEST_CASE("Manifold: a walk crosses a thin wall in a few steps") {
  // A pane far thinner than the way its chain has to go, the seed up
  // to a hundredth of the receiver's distance beside the solution.
  // A step held to half the shortest segment of the chain would be
  // half the pane's thickness, and a walk of such steps would not
  // arrive.
  constexpr float IOR{1.5f};
  smdl::ManifoldSolver solver{};
  const PlaneSurfaces surfaces{};
  for (const bool isThorough : {false, true})
    for (const double distance : {1.0, 10.0, 100.0})
      for (const double thickness : {2e-3, 5e-5})
        for (int k = 0; k < 16; k++) {
          CAPTURE(isThorough);
          CAPTURE(distance);
          CAPTURE(thickness);
          CAPTURE(k);
          const double a{std::fmod(0.618034 * double(k + 1), 1.0)};
          const double b{std::fmod(0.754878 * double(k + 2), 1.0)};
          const double3 receiver{2.0 * a - 1.0, 2.0 * b - 1.0, 0.0};
          const float3 wl{
              normalize(float3(0.4f * float(b - 0.5), 0.5f, 0.85f))};
          // Along the line to the light the pane is crossed at the
          // solution, so the seed is along a line beside it.
          const double3 beside{
              normalize(double3(wl) + 0.01 * double3(b - 0.5, a - 0.5, 0.0))};
          smdl::ManifoldProblem problem{};
          problem.restart(2);
          problem.isThorough = isThorough;
          for (int i = 0; i < 2; i++) {
            smdl::ManifoldProblemVertex &seed{problem.append()};
            const double z{distance + (i == 0 ? 0.0 : thickness)};
            seed.vertex.point = receiver + z / beside.z * beside;
            seed.vertex.point.z = z;
            seed.vertex.surface = uint64_t(i);
            seed.etaPrev = i == 0 ? 1.0f : IOR;
            seed.etaNext = i == 0 ? IOR : 1.0f;
            seed.sideSign = 1.0f;
          }
          smdl::ManifoldTarget target{};
          target.wl = wl;
          smdl::ManifoldSolution solution{};
          smdl::ManifoldWalkReport report{};
          REQUIRE(solver.solve(surfaces, receiver, target, problem, solution,
                               &report));
          CHECK(report.iterations <= 4);
          CHECK(dot(solution.wr, wl) > 1.0f - 1e-6f);
        }
}

namespace {

// A pool of water: its wall, surface 0, the plane x = 0 with the water on
// its +x side, and its top, surface 1, the plane z = 1 with air above.
class PoolSurfaces final : public smdl::ManifoldSurfaces {
public:
  [[nodiscard]] bool
  evaluateGeometry(const smdl::ManifoldVertex &vertex,
                   smdl::ManifoldGeometry &geometry) const override {
    const bool isWall{vertex.surface == 0};
    geometry = {};
    geometry.normal = isWall ? float3(1.0f, 0.0f, 0.0f) : float3(0, 0, 1.0f);
    geometry.Ng = geometry.normal;
    geometry.dPdu = isWall ? float3(0.0f, 1.0f, 0.0f) : float3(1.0f, 0, 0);
    geometry.dPdv = isWall ? float3(0.0f, 0.0f, 1.0f) : float3(0, 1.0f, 0);
    return true;
  }

  [[nodiscard]] bool project(const smdl::ManifoldVertex &pin,
                             const double3 &origin, const double3 &target,
                             smdl::ManifoldVertex &moved) const override {
    const size_t axis{pin.surface == 0 ? size_t(0) : size_t(2)};
    const double plane{pin.surface == 0 ? 0.0 : 1.0};
    const double3 dir{target - origin};
    if (!(std::abs(dir[axis]) > 0.0)) return false;
    const double t{(plane - origin[axis]) / dir[axis]};
    if (!(t > 0.0)) return false;
    moved = {};
    moved.point = origin + t * dir;
    moved.point[axis] = plane;
    moved.surface = pin.surface;
    moved.coords = float3(moved.point);
    return true;
  }
};

} // namespace

TEST_CASE("Manifold: a walk seeded by a neighbor's solution takes its "
          "first step") {
  // A receiver on a pool's floor beside its wall, lit by reflection off
  // the wall and then through the top, walked from the solution for a
  // receiver a little way off, as a route gather's start is the bounces
  // of a light path that came down beside it. The first segment, to the
  // wall, is short, so the wall's bounce steers the rest of the chain
  // hard, and a trace from it lands nowhere near the top bounce the
  // neighbor's solution puts there. A walk that holds every trial to
  // the start as handed takes no first step.
  constexpr float WATER{1.33f};
  const PoolSurfaces surfaces{};
  smdl::ManifoldSolver solver{};
  const double3 light{1.2, 0.3, 4.0};
  smdl::ManifoldTarget target{};
  target.point = light;
  target.isInfinite = false;
  // A problem of the route seeded at `wall` and `top`.
  const auto problemOf{[&](const double3 &wall, const double3 &top,
                           smdl::ManifoldProblem &problem) {
    problem.restart(2);
    smdl::ManifoldProblemVertex &reflection{problem.append()};
    reflection.vertex.point = wall;
    reflection.vertex.surface = 0;
    reflection.etaPrev = reflection.etaNext = WATER;
    reflection.isReflection = true;
    reflection.sideSign = -1.0f;
    smdl::ManifoldProblemVertex &refraction{problem.append()};
    refraction.vertex.point = top;
    refraction.vertex.surface = 1;
    refraction.etaPrev = WATER;
    refraction.etaNext = 1.0f;
    refraction.sideSign = 1.0f;
  }};
  int numWalks{0};
  int numFound{0};
  for (int k = 0; k < 64; k++) {
    CAPTURE(k);
    const double a{std::fmod(0.618034 * double(k + 1), 1.0)};
    const double b{std::fmod(0.754878 * double(k + 2), 1.0)};
    const double c{std::fmod(0.569840 * double(k + 3), 1.0)};
    const double d{std::fmod(0.819173 * double(k + 4), 1.0)};
    // The neighbor's solution, from where the line to the light from
    // its image in the wall crosses the wall and the top.
    const double3 neighbor{0.005 + 0.03 * a, 0.4 * b - 0.2, 0.0};
    const double3 image{-neighbor.x, neighbor.y, 0.0};
    const double3 toLight{light - image};
    smdl::ManifoldProblem problem{};
    problemOf(image + (-image.x / toLight.x) * toLight,
              image + (1.0 / toLight.z) * toLight, problem);
    problem.isThorough = true;
    smdl::ManifoldSolution solution{};
    target.wl = float3(normalize(light - neighbor));
    REQUIRE(solver.solve(surfaces, neighbor, target, problem, solution));
    // A receiver a centimeter or two off, walked once from it.
    const double3 receiver{std::max(0.003, neighbor.x + 0.02 * (c - 0.5)),
                           neighbor.y + 0.03 * (d - 0.5), 0.0};
    problemOf(solution.vertices[0].vertex.point,
              solution.vertices[1].vertex.point, problem);
    target.wl = float3(normalize(light - receiver));
    smdl::ManifoldWalkReport report{};
    numWalks++;
    if (!solver.solve(surfaces, receiver, target, problem, solution, &report))
      continue;
    numFound++;
    // The mirror's law at the wall and Snell's at the top.
    for (int i = 0; i < 2; i++) {
      CAPTURE(i);
      const smdl::ManifoldSolutionVertex &bounce{solution.vertices[i]};
      const double3 normal{bounce.geometry.normal};
      const double3 half{double(problem[i].etaPrev) * double3(bounce.wPrev) +
                         double(problem[i].etaNext) * double3(bounce.wNext)};
      CHECK(length(half - dot(half, normal) * normal) < 2e-5);
    }
  }
  CHECK(numFound > 0.95 * numWalks);
}

TEST_CASE("Manifold: a glossy chain is traced about the normals it is "
          "solved for") {
  // Each crossing of a glossy problem is solved for a microfacet normal,
  // the offset of its seed in the walk's frame, and a walk that follows
  // its chain scatters about that normal. The solution's half vector
  // at each crossing is then the one the offset names.
  smdl::ManifoldSolver solver{};
  const BallSurfaces surfaces{{{0.5, 1.5}}};
  std::vector<BallSurfaces::Crossing> crossings{};
  const float2 offsets[2]{float2(0.05f, -0.03f), float2(-0.02f, 0.04f)};
  int numSolved[2]{};
  for (const bool isThorough : {false, true})
    for (int k = 0; k < 128; k++) {
      CAPTURE(isThorough);
      CAPTURE(k);
      const double a{std::fmod(0.618034 * double(k + 1), 1.0)};
      const double b{std::fmod(0.754878 * double(k + 2), 1.0)};
      const double3 receiver{1.4 * (a - 0.5) - 0.45, 1.4 * (b - 0.5) + 0.3,
                             -0.8};
      const double3 light{1.2 + 0.3 * (b - 0.5), -0.8 + 0.3 * (a - 0.5), 2.2};
      const double3 wl{normalize(light - receiver)};
      double3 leavingFrom{}, leaving{};
      if (!surfaces.trace(receiver, wl, /*bends=*/false, crossings, leavingFrom,
                          leaving))
        continue;
      REQUIRE(crossings.size() == 2);
      smdl::ManifoldProblem problem{};
      problem.restart(2);
      problem.isThorough = isThorough;
      for (int i = 0; i < 2; i++) {
        smdl::ManifoldProblemVertex &seed{problem.append()};
        seed.vertex.point = crossings[i].point;
        seed.vertex.surface = 0;
        seed.etaPrev = float(crossings[i].etaPrev);
        seed.etaNext = float(crossings[i].etaNext);
        seed.sideSign = i == 0 ? -1.0f : 1.0f;
        seed.isGlossy = true;
        seed.offset = offsets[i];
        seed.tangentAim = float3(0.8f, 0.3f, 0.1f);
      }
      smdl::ManifoldTarget target{};
      target.wl = float3(wl);
      target.point = light;
      target.isInfinite = false;
      smdl::ManifoldSolution solution{};
      if (!solver.solve(surfaces, receiver, target, problem, solution))
        continue;
      numSolved[isThorough ? 1 : 0]++;
      for (int i = 0; i < 2; i++) {
        CAPTURE(i);
        const smdl::ManifoldSolutionVertex &crossing{solution.vertices[i]};
        float3 normal{}, t1{}, t2{};
        REQUIRE(surfaces.tangentSpaceOf(crossing.vertex, problem[i].tangentAim,
                                        normal, t1, t2));
        float3 half{normalize(problem[i].etaPrev * crossing.wPrev +
                              problem[i].etaNext * crossing.wNext)};
        if (dot(half, normal) < 0.0f) half = -half;
        CHECK(std::abs(dot(half, t1) - offsets[i].x) < 2e-5f);
        CHECK(std::abs(dot(half, t2) - offsets[i].y) < 2e-5f);
      }
    }
  CHECK(numSolved[0] > 60);
  CHECK(numSolved[1] > 80);
}

namespace {

// The unpolarized transmittance of a dielectric interface as a renderer
// reads it: from the one direction it is handed, the other by Snell's
// law.
[[nodiscard]] double transmittanceFrom(double cosFrom, double etaFrom,
                                       double etaTo) {
  const double sinTo{etaFrom / etaTo * std::sqrt(1.0 - cosFrom * cosFrom)};
  if (!(sinTo < 1.0)) return 0.0;
  const double cosTo{std::sqrt(1.0 - sinTo * sinTo)};
  const double rs{(etaFrom * cosFrom - etaTo * cosTo) /
                  (etaFrom * cosFrom + etaTo * cosTo)};
  const double rp{(etaTo * cosFrom - etaFrom * cosTo) /
                  (etaTo * cosFrom + etaFrom * cosTo)};
  return 1.0 - 0.5 * (rs * rs + rp * rp);
}

} // namespace

TEST_CASE("Manifold: a walk stops at a solution and not beside one") {
  // A ball of glass between a receiver and a distant light, seeded where
  // the straight line crosses it, from receivers near enough that the
  // connection leaves the ball toward grazing. What a renderer makes of
  // a solution it makes where the walk stopped, and the transmittance
  // out of the dense side is steep in the direction inside as the exit
  // nears the critical angle: a walk that stops a thousandth beside the
  // solution is worth percents less. So a problem that asks for nothing is
  // held to the walk's own residual, and each crossing to Snell's law
  // and to one transmittance, read from the arriving side or from the
  // leaving one.
  constexpr double IOR{1.5};
  constexpr double TWO_PI{6.283185307179586};
  smdl::ManifoldSolver solver{};
  const SphereSurfaces surfaces{};
  int numSolved{0};
  double leastCos{1.0};
  for (const double depth : {-1.2, -1.6, -3.0})
    for (int k = 0; k < 64; k++) {
      CAPTURE(depth);
      CAPTURE(k);
      const double a{std::fmod(0.618034 * double(k + 1), 1.0)};
      const double b{std::fmod(0.754878 * double(k + 2), 1.0)};
      const double radius{0.95 * std::sqrt(a)};
      const double3 receiver{radius * std::cos(TWO_PI * b),
                             radius * std::sin(TWO_PI * b), depth};
      const double reach{std::sqrt(1.0 - radius * radius)};
      smdl::ManifoldProblem problem{};
      problem.restart(2);
      for (int i = 0; i < 2; i++) {
        smdl::ManifoldProblemVertex &seed{problem.append()};
        seed.vertex.point =
            double3(receiver.x, receiver.y, i == 0 ? -reach : reach);
        seed.vertex.coords = float3(seed.vertex.point);
        seed.etaPrev = i == 0 ? 1.0f : float(IOR);
        seed.etaNext = i == 0 ? float(IOR) : 1.0f;
        seed.sideSign = i == 0 ? -1.0f : 1.0f;
      }
      smdl::ManifoldTarget target{};
      target.wl = float3(0.0f, 0.0f, 1.0f);
      smdl::ManifoldSolution solution{};
      smdl::ManifoldWalkReport report{};
      // Not every receiver sees the light from where its line crosses.
      if (!solver.solve(surfaces, receiver, target, problem, solution, &report))
        continue;
      numSolved++;
      CHECK(report.residual < smdl::MANIFOLD_RESIDUAL);
      for (int i = 0; i < 2; i++) {
        CAPTURE(i);
        const smdl::ManifoldSolutionVertex &crossing{solution.vertices[i]};
        const double etaPrev{problem[i].etaPrev};
        const double etaNext{problem[i].etaNext};
        const double cosPrev{crossing.cosPrev};
        const double cosNext{crossing.cosNext};
        CHECK(std::abs(etaPrev * std::sqrt(1.0 - cosPrev * cosPrev) -
                       etaNext * std::sqrt(1.0 - cosNext * cosNext)) < 2e-5);
        CHECK(transmittanceFrom(cosPrev, etaPrev, etaNext) ==
              doctest::Approx(transmittanceFrom(cosNext, etaNext, etaPrev))
                  .epsilon(2e-3));
        leastCos = std::min(leastCos, std::min(cosPrev, cosNext));
      }
    }
  // Enough of them, and toward grazing among them, or the case holds
  // the walk to nothing.
  CHECK(numSolved >= 100);
  CHECK(leastCos < 0.2);
}

TEST_CASE("Manifold: two solutions are one within a fraction of the distance") {
  // The same displacement of a bounce is the same solution seen from
  // far and another seen from near: the fraction is of the distance from
  // the receiver.
  const double3 receiver{0.0, 0.0, 0.0};
  for (const double distance : {0.1, 10.0}) {
    CAPTURE(distance);
    smdl::ManifoldSolution a{};
    a.resize(1);
    a.vertices[0].vertex.point = double3(0.0, 0.0, distance);
    const auto isSameBeside{[&](double fraction) {
      smdl::ManifoldSolution b{a};
      b.vertices[0].vertex.point.x = fraction * distance;
      return smdl::isSameManifoldSolution(receiver, a, b);
    }};
    CHECK(
        isSameBeside(0.5 * double(smdl::MANIFOLD_SOLUTION_IDENTITY_FRACTION)));
    CHECK(
        !isSameBeside(2.0 * double(smdl::MANIFOLD_SOLUTION_IDENTITY_FRACTION)));
  }
}

namespace {

// The planes, keeping where every projection was aimed.
class RecordingSurfaces final : public smdl::ManifoldSurfaces {
public:
  RecordingSurfaces() = default;

  explicit RecordingSurfaces(const float3 &normal) : mPlanes{1.0f, normal} {}

  [[nodiscard]] bool
  evaluateGeometry(const smdl::ManifoldVertex &vertex,
                   smdl::ManifoldGeometry &geometry) const override {
    return mPlanes.evaluateGeometry(vertex, geometry);
  }

  [[nodiscard]] bool project(const smdl::ManifoldVertex &pin,
                             const double3 &origin, const double3 &target,
                             smdl::ManifoldVertex &moved) const override {
    targets.push_back(target);
    return mPlanes.project(pin, origin, target, moved);
  }

  mutable std::vector<double3> targets{};

private:
  PlaneSurfaces mPlanes{};
};

} // namespace

TEST_CASE("Manifold: a jittered start is displaced by a fraction of the "
          "distance") {
  // The jitter is in units of the distance from the receiver to the
  // seed, along the walk's own tangents there, and the start is where a
  // cast from the receiver toward the displaced point lands.
  smdl::ManifoldSolver solver{};
  const RecordingSurfaces surfaces{};
  const double3 receiver{0.5, -0.8, 4.0};
  smdl::ManifoldProblem problem{};
  problem.restart(1);
  smdl::ManifoldProblemVertex &seed{problem.append()};
  seed.vertex.point = double3(0.3, 0.2, 0.0);
  seed.vertex.coords = float3(seed.vertex.point);
  seed.etaPrev = seed.etaNext = 1.0f;
  seed.sideSign = 1.0f;
  seed.isReflection = true;
  seed.tangentAim = float3(1.0f, 0.0f, 0.0f);
  seed.seedJitter = float2(0.05f, -0.02f);
  smdl::ManifoldTarget target{};
  target.wl = normalize(float3(-0.2f, 0.35f, 0.91f));
  smdl::ManifoldSolution solution{};
  REQUIRE(solver.solve(surfaces, receiver, target, problem, solution));
  REQUIRE(!surfaces.targets.empty());
  const double distance{length(seed.vertex.point - receiver)};
  // The plane's normal is z and the frame seed x, so the tangents are x
  // and y.
  CHECK_NEAR(surfaces.targets[0],
             seed.vertex.point + distance * double3(double(seed.seedJitter.x),
                                                    double(seed.seedJitter.y),
                                                    0.0),
             1e-6);
  // And the walk finds the mirror's one solution from there.
  CHECK(solution.measure(problem) == doctest::Approx(1.0).epsilon(1e-3));
}

namespace {

// A surface with no geometry anywhere, which refuses every query.
class VoidSurfaces final : public smdl::ManifoldSurfaces {
public:
  [[nodiscard]] bool evaluateGeometry(const smdl::ManifoldVertex &,
                                      smdl::ManifoldGeometry &) const override {
    return false;
  }

  [[nodiscard]] bool project(const smdl::ManifoldVertex &, const double3 &,
                             const double3 &,
                             smdl::ManifoldVertex &) const override {
    return false;
  }
};

} // namespace

TEST_CASE("Manifold: the tangent frame a surface gives a vertex") {
  // A plane whose shading normal leans from its own, parameterized at a
  // scale other than one, so that neither the partial nor the reference
  // is already a unit tangent.
  const float3 normal{normalize(float3(0.3f, -0.2f, 1.0f))};
  const PlaneSurfaces surfaces{2.0f, normal};
  smdl::ManifoldVertex vertex{};
  vertex.point = double3(0.3, 0.2, 0.0);
  // The unit vector in the plane of `along` and the normal, perpendicular
  // to the normal and on the side of `along`, which is what projecting
  // `along` onto the tangent plane gives.
  const auto checkProjectionOf{[&](const float3 &tangent, const float3 &along) {
    CHECK(length(tangent) == doctest::Approx(1.0).epsilon(1e-6));
    CHECK(std::abs(dot(tangent, normal)) < 1e-6f);
    CHECK(std::abs(dot(tangent, normalize(cross(along, normal)))) < 1e-6f);
    CHECK(dot(tangent, along) > 0.0f);
  }};
  SUBCASE("The tangent is the position partial off the shading normal") {
    checkProjectionOf(surfaces.tangentOf(vertex), float3(1.0f, 0.0f, 0.0f));
  }
  SUBCASE("The tangent is any perpendicular where the partial is normal") {
    const float3 along{1.0f, 0.0f, 0.0f};
    const PlaneSurfaces edgeOn{1.0f, along};
    const float3 tangent{edgeOn.tangentOf(vertex)};
    CHECK_SAME(tangent, smdl::perpendicularTo(along));
    CHECK(std::abs(dot(tangent, along)) < 1e-6f);
  }
  SUBCASE("The tangent space projects the reference onto the tangent plane") {
    const float3 referenceU{0.8f, 0.3f, 0.1f};
    float3 frameNormal{}, tangentU{}, tangentV{};
    REQUIRE(surfaces.tangentSpaceOf(vertex, referenceU, frameNormal, tangentU,
                                    tangentV));
    CHECK_SAME(frameNormal, normal);
    checkProjectionOf(tangentU, referenceU);
    CHECK_NEAR(tangentV, cross(normal, tangentU), 1e-6f);
  }
  SUBCASE("The tangent space refuses a reference along the normal") {
    const PlaneSurfaces flat{};
    float3 frameNormal{}, tangentU{}, tangentV{};
    CHECK(!flat.tangentSpaceOf(vertex, float3(0.0f, 0.0f, 3.0f), frameNormal,
                               tangentU, tangentV));
    CHECK(!flat.tangentSpaceOf(vertex, float3(0.0f), frameNormal, tangentU,
                               tangentV));
  }
  SUBCASE("A vertex that cannot be evaluated has neither") {
    const VoidSurfaces nowhere{};
    CHECK_SAME(nowhere.tangentOf(vertex), float3(1.0f, 0.0f, 0.0f));
    float3 frameNormal{}, tangentU{}, tangentV{};
    CHECK(!nowhere.tangentSpaceOf(vertex, float3(1.0f, 0.0f, 0.0f), frameNormal,
                                  tangentU, tangentV));
  }
  SUBCASE("A seed with no frame seed is jittered in the surface's tangents") {
    // The same jittered start as above with the frame left to the
    // surface, on a shading normal that leans, so that the tangents are
    // neither axis.
    smdl::ManifoldSolver solver{};
    const RecordingSurfaces recording{normal};
    const double3 receiver{0.5, -0.8, 4.0};
    smdl::ManifoldProblem problem{};
    problem.restart(1);
    smdl::ManifoldProblemVertex &seed{problem.append()};
    seed.vertex.point = double3(0.3, 0.2, 0.0);
    seed.vertex.coords = float3(seed.vertex.point);
    seed.etaPrev = seed.etaNext = 1.0f;
    seed.sideSign = 1.0f;
    seed.isReflection = true;
    seed.seedJitter = float2(0.05f, -0.02f);
    smdl::ManifoldTarget target{};
    target.wl = normalize(float3(-0.2f, 0.35f, 0.91f));
    smdl::ManifoldSolution solution{};
    REQUIRE(solver.solve(recording, receiver, target, problem, solution));
    REQUIRE(!recording.targets.empty());
    float3 frameNormal{}, tangentU{}, tangentV{};
    REQUIRE(recording.tangentSpaceOf(seed.vertex,
                                     recording.tangentOf(seed.vertex),
                                     frameNormal, tangentU, tangentV));
    const double distance{length(seed.vertex.point - receiver)};
    CHECK_NEAR(recording.targets[0],
               seed.vertex.point +
                   distance * double3(seed.seedJitter.x * tangentU +
                                      seed.seedJitter.y * tangentV),
               1e-6);
  }
}

TEST_CASE("Manifold: the lobes a receiver receives with") {
  // The width is read once from the hook, and the lobes that receive a
  // light are decided against the width that light asks for, as a mask
  // over the material's lobes: a narrow coat over a diffuse base leaves
  // the base receiving and hands the coat to ordinary sampling.
  smdl::Compiler compiler{};
  compiler.shouldEmitScatterNormal = true;
  REQUIRE_OK(compiler.addCode(
      "::receivers",
      "#smdl\nimport ::df::*;\n"
      "export material diffuse() = material(surface: material_surface(\n"
      "  scattering: df::diffuse_reflection_bsdf(tint: 0.6)));\n"
      "export material narrow() = material(surface: material_surface(\n"
      "  scattering: df::microfacet_ggx_smith_bsdf(roughness_u: 0.05, "
      "tint: 0.9)));\n"
      "export material wide() = material(surface: material_surface(\n"
      "  scattering: df::microfacet_ggx_smith_bsdf(roughness_u: 0.2, "
      "tint: 0.9)));\n"
      "export material coated() = material(surface: material_surface(\n"
      "  scattering: df::fresnel_layer(ior: 1.5,\n"
      "    layer: df::microfacet_ggx_smith_bsdf(roughness_u: 0.05, "
      "tint: 1.0),\n"
      "    base: df::diffuse_reflection_bsdf(tint: 0.6))));\n"
      "export material mirror() = material(surface: material_surface(\n"
      "  scattering: df::specular_bsdf()));\n"));
  REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_NONE));
  REQUIRE_OK(compiler.jitCompile());
  StateStorage storage{compiler};
  smdl::State state{storage.makeState()};
  state.finalize();
  // The glossy width and how many times the hook was drawn for it.
  const auto widthOf{[&](const char *name) {
    const smdl::jit::MaterialDef *materialDef{compiler.findMaterial(name)};
    REQUIRE(materialDef);
    smdl::jit::Material material{state, materialDef};
    int draws{0};
    const float width{
        smdl::manifoldGlossyWidth(material, /*isBackface=*/false, [&] {
          draws++;
          return smdl::float4(0.3f, 0.7f, 0.5f, 0.5f);
        })};
    return std::pair<float, int>{width, draws};
  }};
  // The lobes that receive a light asking for `minWidth`.
  const auto lobesOf{[&](const char *name, float minWidth) {
    const smdl::jit::MaterialDef *materialDef{compiler.findMaterial(name)};
    REQUIRE(materialDef);
    smdl::jit::Material material{state, materialDef};
    return smdl::manifoldReceiverLobes(material.getLobes(false),
                                       widthOf(name).first, minWidth);
  }};
  SUBCASE("A diffuse lobe receives any light without a draw") {
    const auto [width, draws]{widthOf("diffuse")};
    CHECK(std::isinf(width));
    CHECK(draws == 0);
    CHECK(lobesOf("diffuse", 0.06f) == smdl::DF_MATTE_BRDF);
  }
  SUBCASE("A glossy lobe receives by its width against the light's") {
    // Squared roughness 0.0025 and 0.04 against a light asking 0.005.
    const auto [narrow, narrowDraws]{widthOf("narrow")};
    CHECK(narrow == doctest::Approx(0.0025f));
    CHECK(narrowDraws == 1);
    CHECK(lobesOf("narrow", 0.005f) == 0);
    CHECK(lobesOf("narrow", 0.001f) == smdl::DF_GLOSS_BRDF);
    const auto [wide, wideDraws]{widthOf("wide")};
    CHECK(wide == doctest::Approx(0.04f));
    CHECK(wideDraws == 1);
    CHECK(lobesOf("wide", 0.005f) == smdl::DF_GLOSS_BRDF);
    // A light asking nothing: every finite lobe receives.
    CHECK(lobesOf("narrow", 0.0f) == smdl::DF_GLOSS_BRDF);
  }
  SUBCASE("A narrow coat over a diffuse base leaves the base receiving") {
    const auto [width, draws]{widthOf("coated")};
    CHECK(width == doctest::Approx(0.0025f));
    CHECK(draws == 1);
    CHECK(lobesOf("coated", 0.005f) == smdl::DF_MATTE_BRDF);
    // A light the coat is wide enough for lets it receive too.
    CHECK(lobesOf("coated", 0.001f) ==
          (smdl::DF_MATTE_BRDF | smdl::DF_GLOSS_BRDF));
  }
  SUBCASE("A Dirac lobe never receives") {
    const auto [width, draws]{widthOf("mirror")};
    CHECK(std::isinf(width));
    CHECK(draws == 0);
    CHECK(lobesOf("mirror", 0.005f) == 0);
  }
}
