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
      CHECK(sample.analyticIndex == 0);
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
      CHECK(sample.analyticIndex == INVALID_INDEX);
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

// The lights the layout declares, against the visible lamp they stand
// in for: a `disk` primitive shaded by an `intensity_power` emitter,
// turned to face down at a receiver on its axis, and a disk light of the
// same radius, position, and radiance placed over the same receiver.
namespace {

class LampFixture final {
public:
  LampFixture() {
    if (auto error{compiler.addCode("::lamptest", MATERIALS)}) {
      MESSAGE(error->message);
      REQUIRE(false);
    }
    // The primitive faces +Z, so a half turn about X points it down; the
    // disk light faces down under the identity.
    LayoutItem item{};
    item.primitive.shape = PrimitiveSpec::Shape::DISK;
    item.primitive.radius = RADIUS;
    item.materials.all = "glow_power";
    item.light = true;
    item.objectToWorld[1] = float4(0.0f, -1.0f, 0.0f, 0.0f);
    item.objectToWorld[2] = float4(0.0f, 0.0f, -1.0f, 0.0f);
    item.objectToWorld[3] = float4(0.0f, 0.0f, HEIGHT, 1.0f);
    scene.add(item);
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

  /// A disk light of the lamp's radius at the lamp's height, of `power`
  /// watts, under `xf` composed beneath the translation.
  [[nodiscard]] static LayoutLight
  diskLight(float power, const float4x4 &xf = float4x4(1.0f)) {
    LayoutLight light{};
    light.decl.kind = LayoutLightDecl::Kind::DISK;
    light.decl.radius = RADIUS;
    light.decl.power = power;
    light.decl.powerSet = true;
    float4x4 lift{1.0f};
    lift[3] = float4(0.0f, 0.0f, HEIGHT, 1.0f);
    light.lightToWorld = lift * xf;
    return light;
  }

  static constexpr float RADIUS{0.3f};
  static constexpr float HEIGHT{3.0f};
  static constexpr float3 RECEIVER{0.0f, 0.0f, 0.0f};

  /// Sixteen bands 20 nm apart: the flat spectral shape integrates to 1
  /// over 320 nm, so a light's `power` watts spread to `power / 320`
  /// per band, while a material's `color(2.0)` is 2 in every band.
  static constexpr float BAND_TOTAL{320.0f};

  /// The `power` that puts the disk light at the lamp's per-band
  /// radiance, and that radiance, `2 / (pi A)`.
  static constexpr float POWER{2.0f * BAND_TOTAL};
  static constexpr float AREA{PI * RADIUS * RADIUS};
  static constexpr float RADIANCE{2.0f / (PI * AREA)};

  /// The closed-form irradiance on the axis of a Lambertian disk of
  /// radius `r` and radiance `L` at distance `d`.
  [[nodiscard]] static float diskIrradiance(float L, float r, float d) {
    return PI * L * r * r / (r * r + d * d);
  }

  smdl::Compiler compiler{};
  Scene scene{compiler};
  Color wavelengths{};
};

} // namespace

TEST_CASE("AnalyticLight: a disk light matches the visible disk lamp") {
  LampFixture fixture{};
  const LightSampler lights{fixture.compiler,
                            fixture.scene,
                            nullptr,
                            {LampFixture::diskLight(LampFixture::POWER)},
                            fixture.wavelengths};
  auto allocator{smdl::BumpPtrAllocator()};
  auto state{makeRenderState(fixture.wavelengths, &allocator)};
  Sampler sampler{};
  // Per light: the draws, the irradiance estimator `Li cos / pdf` summed
  // over them (the receiver's normal is +Z), and the selection PMF, which
  // is what is left of the pdf after the geometry.
  constexpr int NUM_DRAWS{8192};
  int numShape{};
  int numLamp{};
  double sumShape{};
  double sumLamp{};
  float pmfShape{-1.0f};
  float pmfLamp{-1.0f};
  for (int i = 0; i < NUM_DRAWS; i++) {
    sampler.startPixelSample(0, uint32_t(i));
    LightSample sample{};
    if (!lights.sample(state, sampler, LampFixture::RECEIVER, sample)) {
      allocator.reset();
      continue;
    }
    CAPTURE(i);
    CHECK(!sample.isDirac);
    CHECK(!sample.isInfinite);
    CHECK(sample.wi.z > 0.0f);
    const float distSq{lengthSquared(sample.target - LampFixture::RECEIVER)};
    const float cosLight{absDot(sample.normal, sample.wi)};
    const float pmf{sample.pdf * LampFixture::AREA * cosLight / distSq};
    const double estimate{double(sample.Li[0]) * sample.wi.z / sample.pdf};
    CHECK(sample.target.z == doctest::Approx(LampFixture::HEIGHT));
    CHECK(sample.target.x * sample.target.x +
              sample.target.y * sample.target.y <=
          LampFixture::RADIUS * LampFixture::RADIUS * (1.0f + 1.0e-5f));
    CHECK(sample.normal.z == doctest::Approx(-1.0f));
    if (sample.analyticIndex == 0) {
      numShape++;
      sumShape += estimate;
      CHECK(!sample.reachable);
      CHECK(sample.hit.material == nullptr);
      for (size_t k = 0; k < fixture.wavelengths.size(); k++)
        CHECK(sample.Li[k] == doctest::Approx(LampFixture::RADIANCE));
      if (pmfShape < 0.0f) pmfShape = pmf;
      CHECK(pmf == doctest::Approx(pmfShape).epsilon(1.0e-4));
    } else {
      numLamp++;
      sumLamp += estimate;
      CHECK(sample.reachable);
      CHECK(sample.analyticIndex == INVALID_INDEX);
      CHECK(sample.hit.instIndex == 0);
      for (size_t k = 0; k < fixture.wavelengths.size(); k++)
        CHECK(sample.Li[k] ==
              doctest::Approx(LampFixture::RADIANCE).epsilon(1.0e-3));
      if (pmfLamp < 0.0f) pmfLamp = pmf;
      CHECK(pmf == doctest::Approx(pmfLamp).epsilon(1.0e-4));
    }
    allocator.reset();
  }
  CHECK(numShape > NUM_DRAWS / 4);
  CHECK(numLamp > NUM_DRAWS / 4);
  // Equal weights, so each is drawn half the time, and each light's
  // irradiance at the receiver is the closed form.
  CHECK(pmfShape == doctest::Approx(0.5f).epsilon(1.0e-3));
  CHECK(pmfLamp == doctest::Approx(0.5f).epsilon(1.0e-3));
  const float irradiance{LampFixture::diskIrradiance(
      LampFixture::RADIANCE, LampFixture::RADIUS, LampFixture::HEIGHT)};
  CHECK(sumShape / NUM_DRAWS == doctest::Approx(irradiance).epsilon(0.02));
  CHECK(sumLamp / NUM_DRAWS == doctest::Approx(irradiance).epsilon(0.02));
}

TEST_CASE("AnalyticLight: the placement scales the extent, not the power") {
  LampFixture fixture{};
  auto allocator{smdl::BumpPtrAllocator()};
  auto state{makeRenderState(fixture.wavelengths, &allocator)};
  Sampler sampler{};
  SUBCASE("A disk under scale 2 2 1") {
    // Twice the radius, four times the area, a quarter of the radiance,
    // and the closed form follows the new radius; the disk light is the
    // only sampled light besides the lamp, and this sampler leaves the
    // lamp out by marking nothing, so the PMF is 1.
    float4x4 xf{1.0f};
    xf[0] = float4(2.0f, 0.0f, 0.0f, 0.0f);
    xf[1] = float4(0.0f, 2.0f, 0.0f, 0.0f);
    const LightSampler lights{fixture.compiler,
                              fixture.scene,
                              nullptr,
                              {LampFixture::diskLight(LampFixture::POWER, xf)},
                              fixture.wavelengths};
    const float radius{2.0f * LampFixture::RADIUS};
    const float area{4.0f * LampFixture::AREA};
    const float radiance{LampFixture::RADIANCE / 4.0f};
    constexpr int NUM_DRAWS{4096};
    double sum{};
    int num{};
    for (int i = 0; i < NUM_DRAWS; i++) {
      sampler.startPixelSample(0, uint32_t(i));
      LightSample sample{};
      if (!lights.sample(state, sampler, LampFixture::RECEIVER, sample) ||
          sample.analyticIndex != 0) {
        allocator.reset();
        continue;
      }
      CAPTURE(i);
      num++;
      const float distSq{lengthSquared(sample.target - LampFixture::RECEIVER)};
      const float cosLight{absDot(sample.normal, sample.wi)};
      CHECK(sample.target.x * sample.target.x +
                sample.target.y * sample.target.y <=
            radius * radius * (1.0f + 1.0e-5f));
      CHECK(sample.Li[0] == doctest::Approx(radiance));
      CHECK(sample.pdf * area * cosLight / distSq ==
            doctest::Approx(0.5f).epsilon(1.0e-3));
      sum += double(sample.Li[0]) * sample.wi.z / sample.pdf;
      allocator.reset();
    }
    CHECK(num > NUM_DRAWS / 4);
    CHECK(sum / NUM_DRAWS ==
          doctest::Approx(LampFixture::diskIrradiance(radiance, radius,
                                                      LampFixture::HEIGHT))
              .epsilon(0.02));
  }
  SUBCASE("A rect under scale 3 2 1") {
    LayoutLight panel{};
    panel.decl.kind = LayoutLightDecl::Kind::RECT;
    panel.decl.size = float2(2.0f, 1.0f);
    panel.decl.power = LampFixture::POWER;
    panel.decl.powerSet = true;
    panel.lightToWorld[0] = float4(3.0f, 0.0f, 0.0f, 0.0f);
    panel.lightToWorld[1] = float4(0.0f, 2.0f, 0.0f, 0.0f);
    panel.lightToWorld[3] = float4(0.0f, 0.0f, LampFixture::HEIGHT, 1.0f);
    const LightSampler lights{
        fixture.compiler, fixture.scene, nullptr, {panel}, fixture.wavelengths};
    // Six by two in the world, so a radiance of `2 / (pi 12)` per band.
    const float area{12.0f};
    const float radiance{2.0f / (PI * area)};
    int num{};
    for (int i = 0; i < 1024; i++) {
      sampler.startPixelSample(0, uint32_t(i));
      LightSample sample{};
      if (!lights.sample(state, sampler, LampFixture::RECEIVER, sample) ||
          sample.analyticIndex != 0) {
        allocator.reset();
        continue;
      }
      CAPTURE(i);
      num++;
      const float distSq{lengthSquared(sample.target - LampFixture::RECEIVER)};
      const float cosLight{absDot(sample.normal, sample.wi)};
      CHECK(std::abs(sample.target.x) <= 3.0f * (1.0f + 1.0e-5f));
      CHECK(std::abs(sample.target.y) <= 1.0f * (1.0f + 1.0e-5f));
      CHECK(sample.target.z == doctest::Approx(LampFixture::HEIGHT));
      CHECK(sample.normal.z == doctest::Approx(-1.0f));
      CHECK(sample.Li[0] == doctest::Approx(radiance));
      CHECK(sample.pdf * area * cosLight / distSq ==
            doctest::Approx(0.5f).epsilon(1.0e-3));
      allocator.reset();
    }
    CHECK(num > 256);
  }
}

TEST_CASE("AnalyticLight: the emitting side and the re-evaluation") {
  LampFixture fixture{};
  auto allocator{smdl::BumpPtrAllocator()};
  auto state{makeRenderState(fixture.wavelengths, &allocator)};
  Sampler sampler{};
  const float3 above{0.0f, 0.0f, 2.0f * LampFixture::HEIGHT};
  const float3 beside{1.0f, 0.0f, 2.0f * LampFixture::HEIGHT};
  SUBCASE("Under the identity the light faces down") {
    const LightSampler lights{fixture.compiler,
                              fixture.scene,
                              nullptr,
                              {LampFixture::diskLight(LampFixture::POWER)},
                              fixture.wavelengths};
    int numDark{};
    for (int i = 0; i < 64; i++) {
      // From above, a plain draw of the light fails, a `keepDark` draw
      // keeps it with nothing arriving, and re-evaluating toward the
      // receiver below finds the radiance again.
      sampler.startPixelSample(0, uint32_t(i));
      LightSample sample{};
      const bool drawn{lights.sample(state, sampler, above, sample)};
      if (drawn) CHECK(sample.analyticIndex == INVALID_INDEX);
      sampler.startPixelSample(0, uint32_t(i));
      if (!lights.sample(state, sampler, above, sample, true) ||
          sample.analyticIndex != 0) {
        allocator.reset();
        continue;
      }
      CAPTURE(i);
      numDark++;
      CHECK(!drawn);
      CHECK(sample.Li.isAllZero());
      CHECK(sample.normal.z == doctest::Approx(-1.0f));
      const Color below{
          lights.reevaluateLi(sample, state, above, LampFixture::RECEIVER)};
      CHECK(below[0] == doctest::Approx(LampFixture::RADIANCE));
      CHECK(lights.reevaluateLi(sample, state, above, beside).isAllZero());
      allocator.reset();
    }
    CHECK(numDark > 8);
  }
  SUBCASE("A mirrored placement faces up") {
    float4x4 xf{1.0f};
    xf[2] = float4(0.0f, 0.0f, -1.0f, 0.0f);
    const LightSampler lights{fixture.compiler,
                              fixture.scene,
                              nullptr,
                              {LampFixture::diskLight(LampFixture::POWER, xf)},
                              fixture.wavelengths};
    int numUp{};
    for (int i = 0; i < 64; i++) {
      sampler.startPixelSample(0, uint32_t(i));
      LightSample sample{};
      if (!lights.sample(state, sampler, above, sample) ||
          sample.analyticIndex != 0) {
        allocator.reset();
        continue;
      }
      CAPTURE(i);
      numUp++;
      CHECK(sample.normal.z == doctest::Approx(1.0f));
      CHECK(sample.Li[0] == doctest::Approx(LampFixture::RADIANCE));
      CHECK(lights.reevaluateLi(sample, state, above, LampFixture::RECEIVER)
                .isAllZero());
      allocator.reset();
    }
    CHECK(numUp > 8);
  }
}
