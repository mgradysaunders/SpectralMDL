#include "CompileFixtures.h"
#include "Fixtures.h"

#include <cmath>
#include <utility>

#include "smdl/Manifold.h"

using smdl::float2;
using smdl::float3;

// The proof the solver is renderer-agnostic: these surfaces are closed
// forms, no ray tracer and no scene behind them. Planes perpendicular to
// z and the unit sphere at the origin, each one smooth surface,
// parameterized by a local frame the walk's parameterization-invariance
// does not care about.
namespace {

// The planes perpendicular to z, one per distinct height, which a chain
// bounces between: `project()` re-anchors onto the pinned vertex's own
// plane, as the contract asks, so a vertex never migrates to another.
// `scale` is the parameterization the measure is supposed to be
// invariant to.
class PlaneSurfaces final : public smdl::ManifoldSurfaces {
public:
  PlaneSurfaces() = default;

  explicit PlaneSurfaces(float scale) : mScale{scale} {}

  [[nodiscard]] bool
  evaluateGeometry(const smdl::ManifoldVertex &vertex,
                   smdl::ManifoldGeometry &geometry) const override {
    geometry = {};
    geometry.point = vertex.point;
    geometry.normal = float3(0.0f, 0.0f, 1.0f);
    geometry.dPdu = float3(mScale, 0.0f, 0.0f);
    geometry.dPdv = float3(0.0f, mScale, 0.0f);
    geometry.Ng = geometry.normal;
    return true;
  }

  [[nodiscard]] bool project(const smdl::ManifoldVertex &pin,
                             const float3 &origin, const float3 &target,
                             smdl::ManifoldVertex &moved) const override {
    const float z{pin.point.z};
    const float3 dir{target - origin};
    if (!(std::abs(dir.z) > 0.0f)) return false;
    const float t{(z - origin.z) / dir.z};
    if (!(t > 0.0f)) return false;
    moved = {};
    moved.point = origin + t * dir;
    moved.point.z = z;
    moved.surface = pin.surface;
    moved.coords = float3(moved.point.x / mScale, moved.point.y / mScale, 0.0f);
    return true;
  }

private:
  float mScale{1.0f};
};

class SphereSurfaces final : public smdl::ManifoldSurfaces {
public:
  [[nodiscard]] bool
  evaluateGeometry(const smdl::ManifoldVertex &vertex,
                   smdl::ManifoldGeometry &geometry) const override {
    geometry = {};
    geometry.point = vertex.point;
    geometry.normal = normalize(vertex.point);
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

  [[nodiscard]] bool project(const smdl::ManifoldVertex &, const float3 &origin,
                             const float3 &target,
                             smdl::ManifoldVertex &moved) const override {
    float3 dir{target - origin};
    if (!smdl::tryNormalize(dir)) return false;
    // The first sphere hit along the ray, like a ray tracer would report.
    const float b{dot(origin, dir)};
    const float c{lengthSquared(origin) - 1.0f};
    const float disc{b * b - c};
    if (!(disc > 0.0f)) return false;
    const float sqrtDisc{std::sqrt(disc)};
    float t{-b - sqrtDisc};
    if (!(t > 1e-5f)) t = -b + sqrtDisc;
    if (!(t > 1e-5f)) return false;
    moved = {};
    moved.point = normalize(origin + t * dir);
    moved.coords = moved.point;
    return true;
  }
};

// A chain of `count` mirror crossings visiting z = 0, 1, 0, 1, ...,
// seeded off the solution so that the walk has to find it rather than
// start on it. The light has to be on the side the last crossing
// leaves toward, which alternates with the depth; see
// `mirrorChainTarget()`.
void seedMirrorChain(smdl::ManifoldChain &chain, int count) {
  chain.restart(count);
  chain.residualTolerance = 1e-6f;
  for (int i = 0; i < count; i++) {
    smdl::ManifoldVertexSeed &seed{chain.append()};
    const float away{static_cast<float>(i)};
    seed.vertex.point =
        float3(0.02f * away, 0.01f * away, i % 2 == 0 ? 0.0f : 1.0f);
    seed.vertex.surface = static_cast<uint64_t>(i % 2);
    seed.vertex.coords = seed.vertex.point;
    seed.etaPrev = seed.etaNext = 1.0f;
    seed.sideSign = 1.0f;
    seed.isReflect = true;
  }
}

[[nodiscard]] float3 mirrorChainDirection(int count) {
  return normalize(float3(-0.15f, 0.25f, count % 2 == 0 ? -0.95f : 0.95f));
}

// The numeric |d omega_r / d omega_l| by central differences of the
// solved receiver direction, the ground truth every chain measure must
// agree with. A finite target is perturbed on the plane through it
// perpendicular to the straight line, which is the unoriented
// convention the measure is expressed in.
[[nodiscard]] double finiteDifferenceMeasure(
    const smdl::ManifoldSurfaces &surfaces, smdl::ManifoldWalkScratch &scratch,
    const float3 &receiver, const smdl::ManifoldTarget &target,
    const smdl::ManifoldChain &chain) {
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
        const float distStraight{length(target.point - receiver)};
        perturbed.point = target.point + sign * STEP * distStraight * axis;
        perturbed.wl = normalize(perturbed.point - receiver);
      }
      smdl::ManifoldConnection connection{};
      REQUIRE(smdl::solveManifoldConnection(surfaces, scratch, receiver,
                                            perturbed, chain, connection));
      wr[side] = connection.wr;
    }
    dwr[k] = (wr[0] - wr[1]) / (2.0f * STEP);
  }
  return length(cross(dwr[0], dwr[1]));
}

} // namespace

TEST_CASE("Manifold: a connection through a flat mirror") {
  // The workspace every solve of this case runs in, as a renderer
  // keeps one per thread.
  smdl::ManifoldWalkScratch scratch{};
  const PlaneSurfaces surfaces{};
  const float3 receiver{0.5f, -0.8f, 1.2f};
  smdl::ManifoldChain chain{};
  chain.restart(1);
  smdl::ManifoldVertexSeed &seed{chain.append()};
  chain.residualTolerance = 1e-6f;
  seed.vertex.point = float3(0.3f, 0.2f, 0.0f);
  seed.vertex.coords = seed.vertex.point;
  seed.etaPrev = seed.etaNext = 1.0f;
  seed.sideSign = 1.0f;
  seed.isReflect = true;
  SUBCASE("Distant light reflects at measure exactly 1") {
    // The reflected connection is the mirrored light direction
    // independent of the receiver, the one closed form clean enough to
    // hold the measure to directly.
    smdl::ManifoldTarget target{};
    target.wl = normalize(float3(-0.2f, 0.35f, 0.91f));
    smdl::ManifoldConnection connection{};
    REQUIRE(smdl::solveManifoldConnection(surfaces, scratch, receiver, target,
                                          chain, connection));
    CHECK(connection.measure(chain) == doctest::Approx(1.0).epsilon(1e-3));
    // And the solved direction is the mirrored light direction, which
    // is where the virtual image of a distant light sits.
    const float3 mirrored{target.wl.x, target.wl.y, -target.wl.z};
    CHECK(dot(connection.wr, mirrored) == doctest::Approx(1.0).epsilon(1e-5));
    CHECK(finiteDifferenceMeasure(surfaces, scratch, receiver, target, chain) ==
          doctest::Approx(connection.measure(chain)).epsilon(0.02));
  }
  SUBCASE("Finite light agrees with finite differences and the image") {
    smdl::ManifoldTarget target{};
    target.point = float3(-1.3f, 0.9f, 2.1f);
    target.wl = normalize(target.point - receiver);
    target.isInfinite = false;
    smdl::ManifoldConnection connection{};
    REQUIRE(smdl::solveManifoldConnection(surfaces, scratch, receiver, target,
                                          chain, connection));
    // The connection passes through the mirror image of the light.
    const float3 image{target.point.x, target.point.y, -target.point.z};
    CHECK(dot(connection.wr, normalize(image - receiver)) ==
          doctest::Approx(1.0).epsilon(1e-5));
    CHECK(finiteDifferenceMeasure(surfaces, scratch, receiver, target, chain) ==
          doctest::Approx(connection.measure(chain)).epsilon(0.02));
  }
}

TEST_CASE("Manifold: a chain of planar mirrors") {
  // The case for the coupling. A planar mirror preserves solid angle
  // and so does a composition of them, so the measure is exactly 1
  // however deep the chain runs: the one analytic value a chain of any
  // depth can be held to, and the depths past three are where the
  // constraint system stops being all corner and starts being all band.
  smdl::ManifoldWalkScratch scratch{};
  const PlaneSurfaces surfaces{};
  // Between the mirrors, so the chain alternates down and up.
  const float3 receiver{0.3f, -0.2f, 0.5f};
  SUBCASE("Every depth reflects at measure exactly 1") {
    for (const int count : {1, 2, 3, 4, 8}) {
      CAPTURE(count);
      smdl::ManifoldChain chain{};
      seedMirrorChain(chain, count);
      // The last crossing leaves toward the light, so which side of the
      // mirrors the light has to be on alternates with the depth.
      smdl::ManifoldTarget target{};
      target.wl = mirrorChainDirection(count);
      smdl::ManifoldConnection connection{};
      REQUIRE(smdl::solveManifoldConnection(surfaces, scratch, receiver, target,
                                            chain, connection));
      CHECK(connection.size() == count);
      CHECK(connection.measure(chain) == doctest::Approx(1.0).epsilon(1e-3));
      // Each reflection flips the z of a direction and leaves the rest
      // alone, whatever the mirrors' heights, so the virtual image of a
      // distant light is its direction with the z flipped once per
      // crossing.
      const float3 mirrored{target.wl.x, target.wl.y,
                            count % 2 == 0 ? target.wl.z : -target.wl.z};
      CHECK(dot(connection.wr, mirrored) == doctest::Approx(1.0).epsilon(1e-5));
      CHECK(
          finiteDifferenceMeasure(surfaces, scratch, receiver, target, chain) ==
          doctest::Approx(connection.measure(chain)).epsilon(0.02));
    }
  }
  SUBCASE("A finite light passes through the composed image") {
    constexpr int COUNT{3};
    smdl::ManifoldChain chain{};
    seedMirrorChain(chain, COUNT);
    smdl::ManifoldTarget target{};
    target.point = float3(-1.3f, 0.9f, 2.1f);
    target.wl = normalize(target.point - receiver);
    target.isInfinite = false;
    smdl::ManifoldConnection connection{};
    REQUIRE(smdl::solveManifoldConnection(surfaces, scratch, receiver, target,
                                          chain, connection));
    // The image of a finite light is the light reflected through the
    // mirrors from the far end back, each one taking z to 2z - z.
    float3 image{target.point};
    for (int i = COUNT - 1; i >= 0; i--)
      image.z = 2.0f * chain.vertices[i].vertex.point.z - image.z;
    CHECK(dot(connection.wr, normalize(image - receiver)) ==
          doctest::Approx(1.0).epsilon(1e-5));
    CHECK(finiteDifferenceMeasure(surfaces, scratch, receiver, target, chain) ==
          doctest::Approx(connection.measure(chain)).epsilon(0.02));
  }
}

TEST_CASE("Manifold: the measure is invariant to the parameterization") {
  // The area elements and the constraint determinant are each expressed
  // in whatever parameterization the surfaces report, and only their
  // ratio means anything, so the same geometry under a finer one has to
  // give the same measure. It is the determinant that makes this worth
  // a case: it is a product of two entries per crossing, each scaling
  // with the parameterization, so it leaves the range a float can hold
  // long before the ratio it belongs to goes anywhere.
  constexpr int COUNT{4};
  smdl::ManifoldWalkScratch scratch{};
  const float3 receiver{0.3f, -0.2f, 0.5f};
  smdl::ManifoldChain chain{};
  seedMirrorChain(chain, COUNT);
  smdl::ManifoldTarget target{};
  target.wl = mirrorChainDirection(COUNT);
  for (const float scale : {1.0f, 1e-3f, 1e-5f, 1e-7f}) {
    CAPTURE(scale);
    const PlaneSurfaces surfaces{scale};
    smdl::ManifoldConnection connection{};
    REQUIRE(smdl::solveManifoldConnection(surfaces, scratch, receiver, target,
                                          chain, connection));
    CHECK(connection.measure(chain) == doctest::Approx(1.0).epsilon(1e-3));
  }
}

TEST_CASE("Manifold: a connection refracted through a sphere") {
  // The workspace every solve of this case runs in, as a renderer
  // keeps one per thread.
  smdl::ManifoldWalkScratch scratch{};
  const SphereSurfaces surfaces{};
  // The receiver on the axis inside the unit sphere, on the dense side,
  // and the light distant along the axis: by symmetry the crossing is
  // exactly the pole, the one refractive configuration with an analytic
  // solution to converge to. The seed starts well off the pole to make
  // the walk earn it.
  const float3 receiver{0.0f, 0.0f, 0.3f};
  smdl::ManifoldChain chain{};
  chain.restart(1);
  smdl::ManifoldVertexSeed &seed{chain.append()};
  chain.residualTolerance = 1e-6f;
  seed.vertex.point = normalize(float3(0.25f, -0.2f, 0.94f));
  seed.vertex.coords = seed.vertex.point;
  seed.etaPrev = 1.5f;
  seed.etaNext = 1.0f;
  seed.sideSign = 1.0f;
  SUBCASE("The axial connection converges to the pole") {
    smdl::ManifoldTarget target{};
    target.wl = float3(0.0f, 0.0f, 1.0f);
    smdl::ManifoldConnection connection{};
    REQUIRE(smdl::solveManifoldConnection(surfaces, scratch, receiver, target,
                                          chain, connection));
    CHECK(std::abs(connection.vertices[0].vertex.point.x) < 1e-4f);
    CHECK(std::abs(connection.vertices[0].vertex.point.y) < 1e-4f);
    CHECK(connection.vertices[0].vertex.point.z ==
          doctest::Approx(1.0).epsilon(1e-5));
    CHECK(finiteDifferenceMeasure(surfaces, scratch, receiver, target, chain) ==
          doctest::Approx(connection.measure(chain)).epsilon(0.02));
  }
  SUBCASE("Off-axis targets agree with finite differences") {
    for (const auto &wl : {normalize(float3(0.3f, 0.1f, 0.95f)),
                           normalize(float3(-0.2f, 0.25f, 0.9f))}) {
      smdl::ManifoldTarget target{};
      target.wl = wl;
      smdl::ManifoldConnection connection{};
      REQUIRE(smdl::solveManifoldConnection(surfaces, scratch, receiver, target,
                                            chain, connection));
      CHECK(
          finiteDifferenceMeasure(surfaces, scratch, receiver, target, chain) ==
          doctest::Approx(connection.measure(chain)).epsilon(0.02));
    }
  }
  SUBCASE("A finite light agrees with finite differences") {
    smdl::ManifoldTarget target{};
    target.point = float3(0.4f, -0.3f, 3.0f);
    target.wl = normalize(target.point - receiver);
    target.isInfinite = false;
    smdl::ManifoldConnection connection{};
    REQUIRE(smdl::solveManifoldConnection(surfaces, scratch, receiver, target,
                                          chain, connection));
    CHECK(finiteDifferenceMeasure(surfaces, scratch, receiver, target, chain) ==
          doctest::Approx(connection.measure(chain)).epsilon(0.02));
  }
}

TEST_CASE("Manifold: a chain refracted in and out of a sphere") {
  // The coupled case whose blocks carry real numbers. A sphere's
  // shading normal turns with the surface, so the frame terms the
  // diagonal blocks pick up are nonzero, which the mirrors leave empty;
  // and the two crossings are refractive, which the mirrors are not.
  // The receiver and a distant light on the axis put both crossings on
  // the poles by symmetry, the one two-crossing configuration with an
  // analytic solution to converge to.
  smdl::ManifoldWalkScratch scratch{};
  const SphereSurfaces surfaces{};
  const float3 receiver{0.0f, 0.0f, -3.0f};
  smdl::ManifoldChain chain{};
  chain.restart(2);
  chain.residualTolerance = 1e-6f;
  // Entering the glass at the near pole and leaving it at the far one,
  // seeded well off both so the walk has to find them.
  smdl::ManifoldVertexSeed &entry{chain.append()};
  entry.vertex.point = normalize(float3(0.22f, -0.17f, -0.95f));
  entry.vertex.coords = entry.vertex.point;
  entry.etaPrev = 1.0f;
  entry.etaNext = 1.5f;
  entry.sideSign = -1.0f;
  smdl::ManifoldVertexSeed &exit{chain.append()};
  exit.vertex.point = normalize(float3(-0.15f, 0.2f, 0.96f));
  exit.vertex.coords = exit.vertex.point;
  exit.etaPrev = 1.5f;
  exit.etaNext = 1.0f;
  exit.sideSign = 1.0f;
  SUBCASE("The axial connection converges to the poles") {
    smdl::ManifoldTarget target{};
    target.wl = float3(0.0f, 0.0f, 1.0f);
    smdl::ManifoldConnection connection{};
    REQUIRE(smdl::solveManifoldConnection(surfaces, scratch, receiver, target,
                                          chain, connection));
    REQUIRE(connection.size() == 2);
    CHECK_NEAR(connection.vertices[0].vertex.point, float3(0.0f, 0.0f, -1.0f),
               1e-4f);
    CHECK_NEAR(connection.vertices[1].vertex.point, float3(0.0f, 0.0f, 1.0f),
               1e-4f);
    // On the axis the connection never bends, so it leaves the receiver
    // straight at the light.
    CHECK_NEAR(connection.wr, target.wl, 1e-5f);
    CHECK(finiteDifferenceMeasure(surfaces, scratch, receiver, target, chain) ==
          doctest::Approx(connection.measure(chain)).epsilon(0.02));
  }
  SUBCASE("Off-axis targets agree with finite differences") {
    for (const auto &wl : {normalize(float3(0.06f, 0.03f, 0.998f)),
                           normalize(float3(-0.05f, 0.07f, 0.996f))}) {
      smdl::ManifoldTarget target{};
      target.wl = wl;
      smdl::ManifoldConnection connection{};
      REQUIRE(smdl::solveManifoldConnection(surfaces, scratch, receiver, target,
                                            chain, connection));
      CHECK(
          finiteDifferenceMeasure(surfaces, scratch, receiver, target, chain) ==
          doctest::Approx(connection.measure(chain)).epsilon(0.02));
    }
  }
}

TEST_CASE("Manifold: the trials a reciprocal walk counts") {
  // The count is the estimate: a retry that lands on the solution at
  // attempt k reports inverse probability k, and running out drops.
  smdl::ManifoldConnection connection{};
  connection.resize(1);
  connection.vertices[0].vertex.point = float3(1.0f, 0.0f, 0.0f);
  const float3 receiver{};
  smdl::ManifoldSolutionKey key{};
  smdl::ManifoldConnection other{};
  auto retryLandingAt{[&](int landing) {
    return [&, landing, attempt = 0](smdl::ManifoldConnection &other) mutable {
      other = connection;
      return ++attempt == landing;
    };
  }};
  int trials{};
  float inverseProbability{};
  CHECK(smdl::manifoldReciprocal(receiver, connection, key, other, 8, trials,
                                 inverseProbability, retryLandingAt(3)));
  CHECK(trials == 3);
  CHECK(inverseProbability == doctest::Approx(3.0));
  CHECK(!smdl::manifoldReciprocal(receiver, connection, key, other, 8, trials,
                                  inverseProbability, retryLandingAt(9)));
  CHECK(trials == 8);
  // A retry landing somewhere else is an attempt that found nothing.
  auto elsewhere{[&](smdl::ManifoldConnection &other) {
    other = connection;
    other.vertices[0].vertex.point = float3(0.0f, 1.0f, 0.0f);
    return true;
  }};
  CHECK(!smdl::manifoldReciprocal(receiver, connection, key, other, 4, trials,
                                  inverseProbability, elsewhere));
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
    const smdl::JIT::MaterialDef *materialDef{compiler.findMaterial(name)};
    REQUIRE(materialDef);
    smdl::JIT::Material material{state, materialDef};
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
    const smdl::JIT::MaterialDef *materialDef{compiler.findMaterial(name)};
    REQUIRE(materialDef);
    smdl::JIT::Material material{state, materialDef};
    return smdl::manifoldReceiverLobes(material.getLobes(false),
                                       widthOf(name).first, minWidth);
  }};
  SUBCASE("A diffuse lobe receives any light without a draw") {
    const auto [width, draws]{widthOf("diffuse")};
    CHECK(std::isinf(width));
    CHECK(draws == 0);
    CHECK(lobesOf("diffuse", 0.06f) == smdl::DF_SMOOTH_BRDF);
  }
  SUBCASE("A glossy lobe receives by its width against the light's") {
    // Squared roughness 0.0025 and 0.04 against a light asking 0.005.
    const auto [narrow, narrowDraws]{widthOf("narrow")};
    CHECK(narrow == doctest::Approx(0.0025f));
    CHECK(narrowDraws == 1);
    CHECK(lobesOf("narrow", 0.005f) == 0);
    CHECK(lobesOf("narrow", 0.001f) == smdl::DF_GLOSSY_BRDF);
    const auto [wide, wideDraws]{widthOf("wide")};
    CHECK(wide == doctest::Approx(0.04f));
    CHECK(wideDraws == 1);
    CHECK(lobesOf("wide", 0.005f) == smdl::DF_GLOSSY_BRDF);
    // A light asking nothing: every finite lobe receives.
    CHECK(lobesOf("narrow", 0.0f) == smdl::DF_GLOSSY_BRDF);
  }
  SUBCASE("A narrow coat over a diffuse base leaves the base receiving") {
    const auto [width, draws]{widthOf("coated")};
    CHECK(width == doctest::Approx(0.0025f));
    CHECK(draws == 1);
    CHECK(lobesOf("coated", 0.005f) == smdl::DF_SMOOTH_BRDF);
    // A light the coat is wide enough for lets it receive too.
    CHECK(lobesOf("coated", 0.001f) ==
          (smdl::DF_SMOOTH_BRDF | smdl::DF_GLOSSY_BRDF));
  }
  SUBCASE("A Dirac lobe never receives") {
    const auto [width, draws]{widthOf("mirror")};
    CHECK(std::isinf(width));
    CHECK(draws == 0);
    CHECK(lobesOf("mirror", 0.005f) == 0);
  }
}
