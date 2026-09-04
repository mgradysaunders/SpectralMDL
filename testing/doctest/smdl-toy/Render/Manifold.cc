#include "doctest.h"

#include <algorithm>
#include <cmath>
#include <string>
#include <vector>

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

// Flat mirrors and dielectric interfaces, on which the walk converges
// from anywhere and the measure has independent ground truth.
static const char *SELFTEST_MATERIALS{
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
[[nodiscard]] static Hit castOnto(const Scene &scene, const float3 &from,
                                  const float3 &toward) {
  Ray ray{from, toward - from, EPS, INF};
  Hit hit{};
  if (!scene.intersect(ray, hit)) return Hit{};
  return hit;
}

namespace {

struct Solve final {
  bool ok{};
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
  const auto center{solveOnce(surfaces, receiver, target, chain, &report)};
  {
    INFO("the walk did not converge: ", describe(report));
    REQUIRE(center.ok);
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
      if (target.infinite) {
        perturbed.wl = normalize(target.wl + sign * STEP * axis);
      } else {
        const float distStraight{length(target.point - receiver)};
        perturbed.point = target.point + sign * STEP * distStraight * axis;
        perturbed.wl = normalize(perturbed.point - receiver);
      }
      ManifoldWalkReport perturbedReport{};
      const auto solved{
          solveOnce(surfaces, receiver, perturbed, chain, &perturbedReport)};
      {
        INFO("a perturbed walk did not converge: ", describe(perturbedReport));
        REQUIRE(solved.ok);
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

TEST_CASE("Manifold walk: connection measure over scene surfaces") {
  auto compiler{smdl::Compiler()};
  if (auto error{compiler.addCode("::selftest", SELFTEST_MATERIALS)}) {
    MESSAGE(error->message);
    REQUIRE(false);
  }
  auto scene{Scene(compiler)};
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
  if (auto error{compiler.compile(smdl::OPT_LEVEL_O2)}) {
    MESSAGE(error->message);
    REQUIRE(false);
  }
  if (auto error{compiler.jitCompile()}) {
    MESSAGE(error->message);
    REQUIRE(false);
  }
  auto gridSpec{std::vector<float>(16)};
  for (size_t i = 0; i < gridSpec.size(); i++)
    gridSpec[i] = 400.0f + 300.0f * float(i) / float(gridSpec.size() - 1);
  const auto wavelengths{
      Color(smdl::Span<const float>(gridSpec.data(), gridSpec.size()))};
  renderWavelengths() = wavelengths;
  scene.commit(wavelengths);
  const SceneManifoldSurfaces surfaces{scene};

  // The flat mirror. For a distant light the reflected connection is the
  // mirrored light direction independent of the receiver, so the measure
  // is exactly 1; the finite light has no value this clean and rests on
  // the finite differences alone.
  {
    const float3 receiver{0.5f, -0.8f, 1.2f};
    ManifoldChain chain{};
    chain.count = 1;
    chain.residualTolerance = 1e-5f;
    auto &seed{chain.vertices[0]};
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
      target.infinite = false;
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
    auto &seed{chain.vertices[0]};
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
      target.infinite = false;
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
    auto &lower{chain.vertices[0]};
    auto &upper{chain.vertices[1]};
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
