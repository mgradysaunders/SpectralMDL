#include "doctest.h"

#include <set>
#include <vector>

#include "smdl/Compiler.h"
#include "smdl/Support/Span.h"

#include "Color.h"
#include "Layout/Layout.h"
#include "Render/Light.h"
#include "Render/Sampler.h"
#include "Scene/Scene.h"

// The light sampler over marked and unmarked emitters: what the `light`
// mark decides and, just as important, what it leaves alone. Two
// emissive spheres, one marked; a marked sphere with no emission; and
// two `intensity_power` spheres, one marked, so that the area
// normalization of an unsampled emitter's path hits, which nothing but
// the sampler's registration can supply, is checked against the
// radiant-exitance material it must agree with.

static const char *MATERIALS{
    "#smdl\n"
    "import ::df::*;\n"
    "export material glow() = material(\n"
    "  surface: material_surface(emission: material_emission(\n"
    "    emission: df::diffuse_edf(), intensity: color(2.0))));\n"
    "export material glow_power() = material(\n"
    "  surface: material_surface(emission: material_emission(\n"
    "    emission: df::diffuse_edf(), intensity: color(2.0),\n"
    "    mode: intensity_power)));\n"
    "export material dull() = material(\n"
    "  surface: material_surface(scattering: "
    "df::diffuse_reflection_bsdf()));\n"};

namespace {

// The scene the test cases share: built once per test case, since the
// compiler must outlive everything evaluated through it.
class Fixture final {
public:
  Fixture() {
    if (auto error{compiler.addCode("::lighttest", MATERIALS)}) {
      MESSAGE(error->message);
      REQUIRE(false);
    }
    // Spheres of radius 0.5 along +X, two units apart, so every one is
    // visible from the receiver below and none shadows another.
    const char *materials[]{"glow", "glow", "dull", "glow_power", "glow_power"};
    const bool marks[]{true, false, true, false, true};
    for (int i = 0; i < 5; i++) {
      LayoutItem item{};
      item.primitive.shape = PrimitiveSpec::Shape::SPHERE;
      item.primitive.radius = RADIUS;
      item.materials.all = materials[i];
      item.light = marks[i];
      item.objectToWorld[3] = float4(2.0f * float(i), 0.0f, 0.0f, 1.0f);
      scene.add(item);
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
    wavelengths =
        Color(smdl::Span<const float>(gridSpec.data(), gridSpec.size()));
    renderWavelengths() = wavelengths;
    scene.commit(wavelengths);
  }

  /// The center of sphere `i`.
  [[nodiscard]] static float3 center(int i) noexcept {
    return float3(2.0f * float(i), 0.0f, 0.0f);
  }

  /// The hit on sphere `i` seen from the receiver, and the state and
  /// material instance at it.
  [[nodiscard]] Hit hitOn(int i, smdl::State &state) const {
    Ray ray{RECEIVER, normalize(center(i) - RECEIVER), EPS, INF};
    Hit hit{};
    REQUIRE(scene.intersect(ray, hit));
    REQUIRE(hit.instIndex == uint32_t(i));
    hit.applyGeometryToState(state, ray.dir);
    return hit;
  }

  static constexpr float RADIUS{0.5f};
  static constexpr float3 RECEIVER{4.0f, 0.0f, -3.0f};

  smdl::Compiler compiler{};
  Scene scene{compiler};
  Color wavelengths{};
};

// The instances `sample()` draws over `numDraws` draws from the receiver.
[[nodiscard]] std::set<uint32_t> drawnInstances(const LightSampler &lights,
                                                const Fixture &fixture,
                                                int numDraws) {
  auto allocator{smdl::BumpPtrAllocator()};
  auto state{makeRenderState(fixture.wavelengths, &allocator)};
  auto drawn{std::set<uint32_t>()};
  Sampler sampler{};
  for (int i = 0; i < numDraws; i++) {
    sampler.startPixelSample(0, uint32_t(i));
    LightSample lightSample{};
    if (lights.sample(state, sampler, Fixture::RECEIVER, lightSample)) {
      CHECK(!lightSample.isDirac);
      CHECK(!lightSample.isInfinite);
      drawn.insert(lightSample.hit.instIndex);
    }
    allocator.reset();
  }
  return drawn;
}

} // namespace

TEST_CASE("LightSampler: the light mark decides selection alone") {
  Fixture fixture{};
  const LightSampler lights{
      fixture.compiler, fixture.scene, nullptr, {}, fixture.wavelengths};
  CHECK(!lights.empty());
  // The density and the caustic-target answer follow the mark; the
  // marked non-emitter is no light at all.
  auto allocator{smdl::BumpPtrAllocator()};
  for (int i = 0; i < 5; i++) {
    CAPTURE(i);
    auto state{makeRenderState(fixture.wavelengths, &allocator)};
    const auto hit{fixture.hitOn(i, state)};
    const float pdf{lights.solidAnglePDF(hit.instIndex, hit.point, hit.Ng,
                                         Fixture::RECEIVER)};
    const bool sampled{i == 0 || i == 4};
    CHECK((pdf > 0.0f) == sampled);
    CHECK(lights.causticLight(hit.instIndex) == sampled);
    allocator.reset();
  }
  // Over many draws the unmarked emitters never come up and both marked
  // ones do.
  const auto drawn{drawnInstances(lights, fixture, 512)};
  CHECK(drawn == std::set<uint32_t>{0, 4});
}

TEST_CASE("LightSampler: an unsampled emitter still normalizes by area") {
  Fixture fixture{};
  const LightSampler lights{
      fixture.compiler, fixture.scene, nullptr, {}, fixture.wavelengths};
  auto allocator{smdl::BumpPtrAllocator()};
  // The emitted radiance toward the receiver of each sphere: the two
  // radiant-exitance spheres agree with each other, the two power
  // spheres agree with each other, and power over exitance is the
  // sphere's area, marked or not.
  Color Le[5]{};
  for (int i = 0; i < 5; i++) {
    CAPTURE(i);
    auto state{makeRenderState(fixture.wavelengths, &allocator)};
    const auto hit{fixture.hitOn(i, state)};
    const auto mat{smdl::JIT::MaterialInstance(state, hit.material)};
    const float3 wi{normalize(Fixture::RECEIVER - hit.point)};
    const bool emits{lights.emittedRadiance(mat, hit.instIndex, wi, Le[i])};
    CHECK(emits == (i != 2));
    allocator.reset();
  }
  const float area{4.0f * PI * Fixture::RADIUS * Fixture::RADIUS};
  for (size_t k = 0; k < fixture.wavelengths.size(); k++) {
    CAPTURE(k);
    CHECK(Le[0][k] > 0.0f);
    CHECK(Le[1][k] == doctest::Approx(Le[0][k]));
    CHECK(Le[4][k] == doctest::Approx(Le[3][k]));
    CHECK(Le[3][k] * area == doctest::Approx(Le[0][k]).epsilon(1e-3));
  }
}

TEST_CASE("LightSampler: -all-lights samples every emitter") {
  Fixture fixture{};
  const LightSampler lights{fixture.compiler,    fixture.scene, nullptr, {},
                            fixture.wavelengths, true};
  auto allocator{smdl::BumpPtrAllocator()};
  for (int i = 0; i < 5; i++) {
    CAPTURE(i);
    auto state{makeRenderState(fixture.wavelengths, &allocator)};
    const auto hit{fixture.hitOn(i, state)};
    const float pdf{lights.solidAnglePDF(hit.instIndex, hit.point, hit.Ng,
                                         Fixture::RECEIVER)};
    const bool emitter{i != 2};
    CHECK((pdf > 0.0f) == emitter);
    CHECK(lights.causticLight(hit.instIndex) == emitter);
    allocator.reset();
  }
  const auto drawn{drawnInstances(lights, fixture, 512)};
  CHECK(drawn == std::set<uint32_t>{0, 1, 3, 4});
}

TEST_CASE("LightSampler: what each kind of sample says") {
  Fixture fixture{};
  // A point light above the receiver, beside the spheres: the sampler
  // then draws punctual and area samples alike, and each kind has to
  // say the right things about its density, its reachability, its
  // orientation, and what a re-evaluation from elsewhere returns.
  LayoutLight lamp{};
  lamp.decl.kind = LayoutLightDecl::Kind::POINT;
  lamp.decl.power = 20.0f;
  lamp.decl.powerSet = true;
  lamp.lightToWorld[3] = float4(4.0f, 0.0f, 3.0f, 1.0f);
  const LightSampler lights{
      fixture.compiler, fixture.scene, nullptr, {lamp}, fixture.wavelengths};
  auto allocator{smdl::BumpPtrAllocator()};
  auto state{makeRenderState(fixture.wavelengths, &allocator)};
  Sampler sampler{};
  int numPunctual{};
  int numArea{};
  for (int i = 0; i < 512; i++) {
    sampler.startPixelSample(0, uint32_t(i));
    LightSample sample{};
    if (!lights.sample(state, sampler, Fixture::RECEIVER, sample)) {
      allocator.reset();
      continue;
    }
    CAPTURE(i);
    CHECK(!sample.isInfinite);
    if (sample.isDirac) {
      numPunctual++;
      CHECK(!sample.reachable);
      CHECK(sample.punctualIndex == 0);
      CHECK(sample.hit.material == nullptr);
      CHECK(lengthSquared(sample.normal) == 0.0f);
      // A point light has no directional factor, so re-evaluating toward
      // any other point leaves the radiance alone.
      const Color again{
          lights.reevaluateLi(sample, state, Fixture::RECEIVER, float3(0.0f))};
      for (size_t k = 0; k < fixture.wavelengths.size(); k++)
        CHECK(again[k] == doctest::Approx(sample.Li[k]));
    } else {
      numArea++;
      CHECK(sample.reachable);
      CHECK(sample.punctualIndex == INVALID_INDEX);
      CHECK(sample.hit.material != nullptr);
      CHECK(lengthSquared(sample.normal - sample.hit.Ng) == 0.0f);
      // Toward the receiver it was sampled from, the re-evaluation is the
      // sample's own radiance; toward the sphere's center, behind the
      // emitting surface, it is zero.
      const Color same{lights.reevaluateLi(sample, state, Fixture::RECEIVER,
                                           Fixture::RECEIVER)};
      for (size_t k = 0; k < fixture.wavelengths.size(); k++)
        CHECK(same[k] == doctest::Approx(sample.Li[k]));
      const Color behind{
          lights.reevaluateLi(sample, state, Fixture::RECEIVER,
                              Fixture::center(int(sample.hit.instIndex)))};
      CHECK(behind.isAllZero());
    }
    allocator.reset();
  }
  CHECK(numPunctual > 0);
  CHECK(numArea > 0);
}
