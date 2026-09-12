#include "RenderFixtures.h"

#include <algorithm>
#include <cmath>
#include <string>

#include "smdl/Compiler.h"
#include "smdl/Support/Span.h"

#include "Color.h"
#include "Layout/Layout.h"
#include "Render/Manifold.h"
#include "Scene/Scene.h"

// The manifold walk over this renderer's own surfaces: small flat scenes
// built here, reflective and refractive connections solved through them,
// and the connection measure checked against the flat-mirror analytic
// value and against finite differences of the solved receiver direction
// over the light direction, which is the one ground truth every chain
// measure must agree with. The library suite runs the solver over
// analytic surfaces; this exercises `SceneManifoldSurfaces` end to end.

namespace {
// Flat mirrors and dielectric interfaces, on which the walk converges
// from anywhere and the measure has independent ground truth.
const char *SELFTEST_MATERIALS{
    "#smdl\n"
    "import ::df::*;\n"
    "export material self_mirror() = material(\n"
    "  surface: material_surface(scattering:\n"
    "    df::specular_bsdf(mode: df::scatter_reflect)));\n"
    "export material self_glass() = material(\n"
    "  ior: 1.5,\n"
    "  surface: material_surface(scattering:\n"
    "    df::specular_bsdf(mode: df::scatter_reflect_transmit)));\n"};

// The first surface hit casting from `from` toward `toward`.
[[nodiscard]] Hit castOnto(const Scene &scene, const float3 &from,
                           const float3 &toward) {
  Ray ray{from, toward - from, EPS, INF};
  Hit hit{};
  if (!scene.intersect(ray, hit)) return Hit{};
  return hit;
}
} // namespace

namespace {

struct Solve final {
  bool hasSolution{};
  float3 wr{};
  float measure{};
};

[[nodiscard]] Solve solveOnce(const SceneManifoldSurfaces &surfaces,
                              const float3 &receiver,
                              const ManifoldTarget &target,
                              const ManifoldChain &chain,
                              ManifoldWalkReport *report = nullptr) {
  ManifoldConnection connection{};
  if (!solveManifoldConnection(surfaces, receiver, target, chain, connection,
                               report))
    return {};
  return {true, connection.wr, connection.measure(chain)};
}

// A walk report, for the failure message of a walk that did not converge.
[[nodiscard]] std::string describe(const ManifoldWalkReport &report) {
  return "outcome " + std::to_string(int(report.outcome)) + ", failure " +
         std::to_string(int(report.failure)) + ", iterations " +
         std::to_string(report.iterations) + ", residual " +
         std::to_string(report.residual);
}

// One check: solve the chain for its target, difference the solved
// receiver direction over the light direction, and compare the numeric
// Jacobian against the connection measure; `analytic`, if nonnegative,
// is an additional closed-form value the measure must match. A finite
// target is perturbed on the plane through it perpendicular to the
// straight line at the straight distance, which is the unoriented
// convention the measure is expressed in.
void checkMeasure(const SceneManifoldSurfaces &surfaces, const char *name,
                  const float3 &receiver, const ManifoldTarget &target,
                  const ManifoldChain &chain, float analytic = -1.0f) {
  INFO(name);
  ManifoldWalkReport report{};
  const Solve center{solveOnce(surfaces, receiver, target, chain, &report)};
  {
    INFO("the walk did not converge: ", describe(report));
    REQUIRE(center.hasSolution);
  }
  constexpr float STEP{2e-3f};
  constexpr float TOLERANCE{0.02f};
  const float3 a1{smdl::perpendicularTo(target.wl)};
  const float3 a2{cross(target.wl, a1)};
  float3 dwr[2]{};
  for (int k = 0; k < 2; k++) {
    const float3 &axis{k == 0 ? a1 : a2};
    float3 wrPlus{}, wrMinus{};
    for (int side = 0; side < 2; side++) {
      const float sign{side == 0 ? +1.0f : -1.0f};
      ManifoldTarget perturbed{target};
      if (target.isInfinite) {
        perturbed.wl = normalize(target.wl + sign * STEP * axis);
      } else {
        const float distStraight{length(target.point - receiver)};
        perturbed.point = target.point + sign * STEP * distStraight * axis;
        perturbed.wl = normalize(perturbed.point - receiver);
      }
      ManifoldWalkReport perturbedReport{};
      const Solve solved{
          solveOnce(surfaces, receiver, perturbed, chain, &perturbedReport)};
      {
        INFO("a perturbed walk did not converge: ", describe(perturbedReport));
        REQUIRE(solved.hasSolution);
      }
      (side == 0 ? wrPlus : wrMinus) = solved.wr;
    }
    dwr[k] = (wrPlus - wrMinus) / (2.0f * STEP);
  }
  const float numeric{length(cross(dwr[0], dwr[1]))};
  const float scale{std::max(numeric, center.measure)};
  const float err{scale > 0.0f ? std::abs(numeric - center.measure) / scale
                               : 0.0f};
  CAPTURE(center.measure);
  CAPTURE(numeric);
  CAPTURE(err);
  CHECK(err <= TOLERANCE);
  if (analytic >= 0.0f) {
    const float analyticErr{std::abs(center.measure - analytic) /
                            std::max(analytic, 1e-6f)};
    CAPTURE(analytic);
    CAPTURE(analyticErr);
    CHECK(analyticErr <= 1e-3f);
  }
}

} // namespace

TEST_CASE("SceneManifoldSurfaces: the connection measure over scene surfaces") {
  smdl::Compiler compiler{};
  REQUIRE_OK(compiler.addCode("::selftest", SELFTEST_MATERIALS));
  Scene scene{compiler};
  // A mirror disk at the origin and two stacked glass disks off to the
  // side, so the two families' casts never see each other; not too far,
  // because float resolution of the crossing positions is what sets the
  // residual floor the walks can reach.
  {
    LayoutItem mirror{};
    mirror.primitive.shape = PrimitiveSpec::Shape::DISK;
    mirror.primitive.radius = 20.0f;
    mirror.materials.all = "self_mirror";
    scene.add(mirror);
    LayoutItem glassA{mirror};
    glassA.materials.all = "self_glass";
    glassA.objectToWorld[3] = float4(50.0f, 0.0f, 0.0f, 1.0f);
    scene.add(glassA);
    LayoutItem glassB{glassA};
    glassB.objectToWorld[3] = float4(50.0f, 0.0f, -2.0f, 1.0f);
    scene.add(glassB);
  }
  REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_O2));
  REQUIRE_OK(compiler.jitCompile());
  const ScopedGrid grid{};
  const Color &wavelengths{grid.wavelengths()};
  scene.commit(wavelengths);
  const SceneManifoldSurfaces surfaces{scene, PathTime(0.0f)};

  // The flat mirror. For a distant light the reflected connection is the
  // mirrored light direction independent of the receiver, so the measure
  // is exactly 1; the finite light has no value this clean and rests on
  // the finite differences alone.
  {
    const float3 receiver{0.5f, -0.8f, 1.2f};
    ManifoldChain chain{};
    chain.count = 1;
    chain.residualTolerance = 1e-5f;
    ManifoldVertexSeed &seed{chain.vertices[0]};
    const Hit mirrorHit{castOnto(scene, receiver, float3(0.3f, 0.2f, 0.0f))};
    REQUIRE(mirrorHit.instance);
    seed.vertex = vertexOf(mirrorHit);
    seed.etaPrev = seed.etaNext = 1.0f;
    seed.sideSign = 1.0f;
    seed.isReflect = true;
    {
      ManifoldTarget target{};
      target.wl = normalize(float3(-0.2f, 0.35f, 0.91f));
      checkMeasure(surfaces, "mirror, distant light", receiver, target, chain,
                   1.0f);
    }
    {
      ManifoldTarget target{};
      target.point = float3(-1.3f, 0.9f, 2.1f);
      target.wl = normalize(target.point - receiver);
      target.isInfinite = false;
      checkMeasure(surfaces, "mirror, finite light", receiver, target, chain);
    }
  }
  // One flat dielectric interface, the receiver on the dense side, like
  // a submerged surface looking up through still water.
  {
    const float3 receiver{50.2f, 0.3f, -1.0f};
    ManifoldChain chain{};
    chain.count = 1;
    chain.residualTolerance = 1e-5f;
    ManifoldVertexSeed &seed{chain.vertices[0]};
    const Hit glassHit{castOnto(scene, receiver, float3(50.1f, 0.2f, 0.0f))};
    REQUIRE(glassHit.instance);
    seed.vertex = vertexOf(glassHit);
    seed.etaPrev = 1.33f;
    seed.etaNext = 1.0f;
    seed.sideSign = 1.0f;
    {
      ManifoldTarget target{};
      target.wl = normalize(float3(0.25f, -0.15f, 0.96f));
      checkMeasure(surfaces, "refraction, distant light", receiver, target,
                   chain);
    }
    {
      ManifoldTarget target{};
      target.point = float3(49.4f, 1.1f, 3.0f);
      target.wl = normalize(target.point - receiver);
      target.isInfinite = false;
      checkMeasure(surfaces, "refraction, finite light", receiver, target,
                   chain);
    }
  }
  // Two stacked interfaces, the coupled system.
  {
    const float3 receiver{50.2f, 0.15f, -3.5f};
    ManifoldChain chain{};
    chain.count = 2;
    chain.residualTolerance = 1e-5f;
    ManifoldVertexSeed &lower{chain.vertices[0]};
    ManifoldVertexSeed &upper{chain.vertices[1]};
    const Hit lowerHit{castOnto(scene, receiver, receiver + float3(0, 0, 1))};
    REQUIRE(lowerHit.instance);
    const Hit upperHit{castOnto(scene, lowerHit.point + float3(0, 0, EPS),
                                lowerHit.point + float3(0, 0, 1))};
    REQUIRE(upperHit.instance);
    lower.vertex = vertexOf(lowerHit);
    upper.vertex = vertexOf(upperHit);
    lower.etaPrev = 1.4f;
    lower.etaNext = 1.0f;
    lower.sideSign = 1.0f;
    upper.etaPrev = 1.0f;
    upper.etaNext = 1.6f;
    upper.sideSign = 1.0f;
    ManifoldTarget target{};
    target.wl = normalize(float3(-0.2f, 0.1f, 0.97f));
    checkMeasure(surfaces, "two refractions, distant light", receiver, target,
                 chain);
  }
}

// The clustering the biased claimed estimators accumulate through: one
// value per distinct solution however often a walk re-finds it, and a
// hard cap past which a distinct solution is dropped rather than summed
// unclustered.
TEST_CASE("ManifoldSolutionSet: distinct solutions counted once") {
  const float3 receiver{};
  // A one-crossing connection at `point`, which is all
  // `isSameManifoldSolution()` reads.
  const auto connectionAt{[](const float3 &point) {
    ManifoldConnection connection{};
    connection.count = 1;
    connection.vertices[0].vertex.point = point;
    return connection;
  }};
  // Distinct by a wide margin against
  // `MANIFOLD_SOLUTION_IDENTITY_FRACTION` of the unit receiver distance.
  const auto solutionAt{[&](int i) {
    return connectionAt(float3(1.0f, 0.01f * float(i), 0.0f));
  }};
  int valued{};
  const auto value{[&](const ManifoldConnection &) {
    valued++;
    return Color(1.0f);
  }};
  SUBCASE("A re-found solution is valued and summed once") {
    ManifoldSolutionSet solutions{};
    solutions.consider(receiver, solutionAt(0), value, nullptr);
    solutions.consider(receiver, solutionAt(0), value, nullptr);
    solutions.consider(receiver, solutionAt(1), value, nullptr);
    solutions.consider(receiver, solutionAt(0), value, nullptr);
    CHECK(valued == 2);
    CHECK(solutions.sum()[0] == doctest::Approx(2.0f));
    // A tally counts each distinct solution once, the re-finds never.
    MNEEStats stats{};
    solutions.consider(receiver, solutionAt(2), value, &stats);
    solutions.consider(receiver, solutionAt(2), value, &stats);
    CHECK(stats.contributionCount == 1);
    CHECK(stats.contributionNonZeroCount == 1);
  }
  SUBCASE("A distinct solution past the cap is dropped, not summed") {
    ManifoldSolutionSet solutions{};
    for (int i = 0; i < 64; i++)
      solutions.consider(receiver, solutionAt(i), value, nullptr);
    // The cap is what the sum stops at, and nothing past it is valued.
    CHECK(valued == int(solutions.sum()[0]));
    CHECK(valued < 64);
    // A re-find of one already counted still costs nothing.
    solutions.consider(receiver, solutionAt(0), value, nullptr);
    CHECK(valued == int(solutions.sum()[0]));
  }
}

// The caster set as the searched gathers see it: every marked instance
// with a claim in either domain, looked up by instance.
TEST_CASE("MNEECasterSet: a refractive caster is kept") {
  smdl::Compiler compiler{};
  REQUIRE_OK(compiler.addCode("::selftest", SELFTEST_MATERIALS));
  Scene scene{compiler};
  {
    LayoutItem mirror{};
    mirror.primitive.shape = PrimitiveSpec::Shape::DISK;
    mirror.primitive.radius = 2.0f;
    mirror.materials.all = "self_mirror";
    mirror.isCaster = true;
    scene.add(mirror);
    LayoutItem glass{mirror};
    glass.materials.all = "self_glass";
    glass.objectToWorld[3] = float4(10.0f, 0.0f, 0.0f, 1.0f);
    scene.add(glass);
    LayoutItem unmarked{glass};
    unmarked.isCaster = false;
    unmarked.objectToWorld[3] = float4(20.0f, 0.0f, 0.0f, 1.0f);
    scene.add(unmarked);
  }
  REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_O2));
  REQUIRE_OK(compiler.jitCompile());
  const ScopedGrid grid{};
  scene.commit(grid.wavelengths());
  const MNEECasterSet casters{scene, grid.wavelengths()};
  CHECK(casters.casters.size() == 2);
  const Hit mirrorHit{castOnto(scene, float3(0, 0, 1), float3(0, 0, 0))};
  const Hit glassHit{castOnto(scene, float3(10, 0, 1), float3(10, 0, 0))};
  const Hit unmarkedHit{castOnto(scene, float3(20, 0, 1), float3(20, 0, 0))};
  REQUIRE(mirrorHit.instance);
  REQUIRE(glassHit.instance);
  REQUIRE(unmarkedHit.instance);
  const MNEECaster *mirror{casters.casterOf(mirrorHit.instIndex)};
  REQUIRE(mirror);
  CHECK(mirror->instIndex == mirrorHit.instIndex);
  CHECK((mirror->reflectLobes & smdl::DF_DIRAC_BRDF) != 0);
  CHECK(mirror->refractLobes == 0);
  const MNEECaster *glass{casters.casterOf(glassHit.instIndex)};
  REQUIRE(glass);
  CHECK((glass->refractLobes & smdl::DF_DIRAC_BTDF) != 0);
  CHECK((glass->reflectLobes & smdl::DF_DIRAC_BRDF) != 0);
  CHECK(casters.casterOf(unmarkedHit.instIndex) == nullptr);
  CHECK(casters.casterOf(INVALID_INDEX) == nullptr);
}

// The start of a searched refractive estimate: a glass ball as the
// caster, the trace entering it on the near face wherever the point was
// drawn, leaving it by Snell's law, and handing the solver a start it
// converges from; and the ways a trace refuses to start.
TEST_CASE("Caster seed trace: a ball is entered and left") {
  smdl::Compiler compiler{};
  REQUIRE_OK(compiler.addCode("::selftest", SELFTEST_MATERIALS));
  Scene scene{compiler};
  // A glass ball at the origin; another, shadowed from below by a
  // mirror disk wider than itself; and a glass disk seen at grazing
  // incidence from its dense side.
  {
    LayoutItem ball{};
    ball.primitive.shape = PrimitiveSpec::Shape::SPHERE;
    ball.primitive.radius = 1.0f;
    ball.materials.all = "self_glass";
    ball.isCaster = true;
    scene.add(ball);
    LayoutItem shadowed{ball};
    shadowed.objectToWorld[3] = float4(20.0f, 0.0f, 0.0f, 1.0f);
    scene.add(shadowed);
    LayoutItem mirror{};
    mirror.primitive.shape = PrimitiveSpec::Shape::DISK;
    mirror.primitive.radius = 5.0f;
    mirror.materials.all = "self_mirror";
    mirror.objectToWorld[3] = float4(20.0f, 0.0f, -2.0f, 1.0f);
    scene.add(mirror);
    LayoutItem sheet{mirror};
    sheet.primitive.radius = 1.0f;
    sheet.materials.all = "self_glass";
    sheet.isCaster = true;
    sheet.objectToWorld[3] = float4(40.0f, 0.0f, 0.0f, 1.0f);
    scene.add(sheet);
  }
  REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_O2));
  REQUIRE_OK(compiler.jitCompile());
  const ScopedGrid grid{};
  const Color &wavelengths{grid.wavelengths()};
  scene.commit(wavelengths);
  const MNEECasterSet casters{scene, wavelengths};
  REQUIRE(casters.casters.size() == 3);
  const SceneManifoldSurfaces surfaces{scene, PathTime(0.0f)};
  smdl::BumpPtrAllocator allocator{};
  smdl::State state{makeRenderState(wavelengths, &allocator)};
  Sampler sampler{};
  // The caster the cast from `from` toward `toward` lands on.
  const auto casterAt{[&](const float3 &from, const float3 &toward) {
    const Hit hit{castOnto(scene, from, toward)};
    REQUIRE(hit.instance);
    const MNEECaster *caster{casters.casterOf(hit.instIndex)};
    REQUIRE(caster);
    return caster;
  }};
  // One trace on the sample `sampleIndex`, from vacuum, at shutter open.
  const auto traceFrom{[&](const MNEECaster &caster, const float3 &receiver,
                           int maxDepth, uint32_t sampleIndex,
                           MNEECasterSeed &seed) {
    sampler.startPixelSample(0, sampleIndex);
    return traceManifoldCasterSeed(scene, state, allocator, sampler, caster,
                                   receiver, nullptr, 0.0f, maxDepth, 0.0f,
                                   seed);
  }};
  constexpr uint32_t NUM_TRACES{16};
  const MNEECaster &ball{*casterAt(float3(0, 0, -3), float3(0, 0, 0))};
  const float3 below{0.0f, 0.0f, -3.0f};
  SUBCASE("The chain enters on the near face and leaves by Snell's law") {
    for (uint32_t i = 0; i < NUM_TRACES; i++) {
      MNEECasterSeed seed{};
      CAPTURE(i);
      REQUIRE(traceFrom(ball, below, MANIFOLD_MAX_DEPTH, i, seed));
      REQUIRE(seed.chain.count == 2);
      CHECK((seed.lobes & smdl::DF_DIRAC_BTDF) != 0);
      const ManifoldVertexSeed &enter{seed.chain[0]};
      const ManifoldVertexSeed &leave{seed.chain[1]};
      CHECK(enter.etaPrev == doctest::Approx(1.0f));
      CHECK(enter.etaNext == doctest::Approx(1.5f));
      CHECK(leave.etaPrev == doctest::Approx(1.5f));
      CHECK(leave.etaNext == doctest::Approx(1.0f));
      CHECK(enter.sideSign == -leave.sideSign);
      // The entry is on the cap the receiver sees, whichever hemisphere
      // the point was drawn on, and the exit is on the ball.
      const float3 &entry{seed.hits[0].point};
      const float3 &exit{seed.hits[1].point};
      CHECK(entry.z < -1.0f / 3.0f);
      CHECK(length(exit) == doctest::Approx(1.0f).epsilon(1e-3));
      // Snell's law about the outward normal at the entry.
      const float3 wIn{normalize(entry - below)};
      const float3 n{seed.hits[0].normal};
      const float eta{1.0f / 1.5f};
      const float cosI{-dot(wIn, n)};
      REQUIRE(cosI > 0.0f);
      const float cosT{std::sqrt(1.0f - eta * eta * (1.0f - cosI * cosI))};
      const float3 wt{eta * wIn + (eta * cosI - cosT) * n};
      const float3 toExit{normalize(exit - entry)};
      CHECK(length(cross(toExit, wt)) < 1e-3f);
      CHECK(dot(toExit, wt) > 0.0f);
      // The family names the ball twice.
      const MNEEChainFamily family{seed.family()};
      CHECK(family.count == 2);
      CHECK(family.instances[0] == ball.instIndex);
      CHECK(family.instances[1] == ball.instIndex);
      CHECK(family == seed.family());
      allocator.reset();
    }
  }
  SUBCASE("The solver converges from the traced start") {
    // A distant light off the axis: the axial connection would pass
    // through the poles of the ball's parameterization, where the
    // walk's frame is degenerate by construction.
    ManifoldTarget target{};
    target.wl = normalize(float3(0.35f, 0.2f, 0.9f));
    bool hasConverged{false};
    for (uint32_t i = 0; i < NUM_TRACES && !hasConverged; i++) {
      MNEECasterSeed seed{};
      if (!traceFrom(ball, below, MANIFOLD_MAX_DEPTH, i, seed)) continue;
      seed.chain.residualTolerance = 1e-5f;
      ManifoldConnection connection{};
      if (!solveManifoldConnection(surfaces, below, target, seed.chain,
                                   connection))
        continue;
      hasConverged = true;
      // The crossings stay on the ball, entering below and leaving
      // above, and the measure agrees with finite differences.
      const float3 &entry{connection.vertices[0].vertex.point};
      const float3 &exit{connection.vertices[1].vertex.point};
      CHECK(length(entry) == doctest::Approx(1.0f).epsilon(1e-3));
      CHECK(length(exit) == doctest::Approx(1.0f).epsilon(1e-3));
      CHECK(entry.z < 0.0f);
      CHECK(exit.z > 0.0f);
      checkMeasure(surfaces, "ball, distant light", below, target, seed.chain);
    }
    CHECK(hasConverged);
  }
  SUBCASE("The depth cap stops the trace at the entry") {
    MNEECasterSeed seed{};
    REQUIRE(traceFrom(ball, below, 1, 0, seed));
    CHECK(seed.chain.count == 1);
    CHECK((seed.lobes & smdl::DF_DIRAC_BTDF) != 0);
    CHECK(seed.chain[0].etaNext == doctest::Approx(1.5f));
  }
  SUBCASE("A foreign blocker before the caster fails the trace") {
    const MNEECaster &shadowed{*casterAt(float3(20, 0, 3), float3(20, 0, 0))};
    for (uint32_t i = 0; i < NUM_TRACES; i++) {
      MNEECasterSeed seed{};
      CAPTURE(i);
      CHECK(!traceFrom(shadowed, float3(20.0f, 0.0f, -3.0f), MANIFOLD_MAX_DEPTH,
                       i, seed));
      CHECK(seed.lobes == 0);
      allocator.reset();
    }
  }
  SUBCASE("Total internal reflection fails the trace") {
    const MNEECaster &sheet{*casterAt(float3(40, 0, -1), float3(40, 0, 0))};
    for (uint32_t i = 0; i < NUM_TRACES; i++) {
      MNEECasterSeed seed{};
      CAPTURE(i);
      CHECK(!traceFrom(sheet, float3(45.0f, 0.0f, -0.2f), MANIFOLD_MAX_DEPTH, i,
                       seed));
      allocator.reset();
    }
  }
}
