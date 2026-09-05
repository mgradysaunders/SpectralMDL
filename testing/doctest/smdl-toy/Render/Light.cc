#include "doctest.h"

#include <filesystem>
#include <fstream>
#include <set>
#include <string>
#include <vector>

#include "smdl/Compiler.h"
#include "smdl/Support/Span.h"

#include "Color.h"
#include "Layout/Layout.h"
#include "Render/Light.h"
#include "Render/Sampler.h"
#include "RigFixtures.h"
#include "Scene/Scene.h"

namespace fs = std::filesystem;

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
      item.isLight = marks[i];
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
    renderGrid().wavelengths = wavelengths;
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
    if (lights.sample(state, sampler, Fixture::RECEIVER, 0.0f, lightSample)) {
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
    const float pdf{lights.solidAnglePDF(hit.instIndex, hit.faceIndex,
                                         hit.point, hit.Ng, Fixture::RECEIVER,
                                         false, 0.0f)};
    const bool sampled{i == 0 || i == 4};
    CHECK((pdf > 0.0f) == sampled);
    CHECK(lights.isCausticLight(hit.instIndex) == sampled);
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
    const float pdf{lights.solidAnglePDF(hit.instIndex, hit.faceIndex,
                                         hit.point, hit.Ng, Fixture::RECEIVER,
                                         false, 0.0f)};
    const bool emitter{i != 2};
    CHECK((pdf > 0.0f) == emitter);
    CHECK(lights.isCausticLight(hit.instIndex) == emitter);
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
    if (!lights.sample(state, sampler, Fixture::RECEIVER, 0.0f, sample)) {
      allocator.reset();
      continue;
    }
    CAPTURE(i);
    CHECK(!sample.isInfinite);
    if (sample.isDirac) {
      numPunctual++;
      CHECK(!sample.isReachable);
      CHECK(sample.analyticIndex == 0);
      CHECK(sample.hit.material == nullptr);
      CHECK(lengthSquared(sample.normal) == 0.0f);
      // A point light has no directional factor, so re-evaluating toward
      // any other point leaves the radiance alone.
      const Color again{lights.reevaluateLi(sample, state, Fixture::RECEIVER,
                                            float3(0.0f), 0.0f)};
      for (size_t k = 0; k < fixture.wavelengths.size(); k++)
        CHECK(again[k] == doctest::Approx(sample.Li[k]));
    } else {
      numArea++;
      CHECK(sample.isReachable);
      CHECK(sample.analyticIndex == INVALID_INDEX);
      CHECK(sample.hit.material != nullptr);
      CHECK(lengthSquared(sample.normal - sample.hit.Ng) == 0.0f);
      // Toward the receiver it was sampled from, the re-evaluation is the
      // sample's own radiance; toward the sphere's center, behind the
      // emitting surface, it is zero.
      const Color same{lights.reevaluateLi(sample, state, Fixture::RECEIVER,
                                           Fixture::RECEIVER, 0.0f)};
      for (size_t k = 0; k < fixture.wavelengths.size(); k++)
        CHECK(same[k] == doctest::Approx(sample.Li[k]));
      const Color behind{lights.reevaluateLi(
          sample, state, Fixture::RECEIVER,
          Fixture::center(int(sample.hit.instIndex)), 0.0f)};
      CHECK(behind.isAllZero());
    }
    allocator.reset();
  }
  CHECK(numPunctual > 0);
  CHECK(numArea > 0);
}

TEST_CASE("LightSampler: every kind of light weighs by its power") {
  Fixture fixture{};
  // A point light whose radiant power per band is the exitance sphere's:
  // intensity 2 per band over the sphere's area pi, so 2 pi per band,
  // spread over the fixture's 320 nm band. The power-mode sphere emits
  // 2 per band outright, so it must draw 1/pi as often.
  LayoutLight lamp{};
  lamp.decl.kind = LayoutLightDecl::Kind::POINT;
  lamp.decl.power = 2.0f * PI * 320.0f;
  lamp.decl.powerSet = true;
  lamp.lightToWorld[3] = float4(4.0f, 0.0f, 3.0f, 1.0f);
  const LightSampler lights{
      fixture.compiler, fixture.scene, nullptr, {lamp}, fixture.wavelengths};
  auto allocator{smdl::BumpPtrAllocator()};
  auto state{makeRenderState(fixture.wavelengths, &allocator)};
  const float area{4.0f * PI * Fixture::RADIUS * Fixture::RADIUS};
  // The point light's selection PMF is its sample's pdf outright, the
  // density being a delta. An area sample's pdf must be what the
  // arrival site recomputes for the same receiver.
  Sampler sampler{};
  float pmfLamp{-1.0f};
  int numArea{};
  for (int i = 0; i < 256; i++) {
    sampler.startPixelSample(0, uint32_t(i));
    LightSample sample{};
    if (lights.sample(state, sampler, Fixture::RECEIVER, 0.0f, sample)) {
      CAPTURE(i);
      if (sample.isDirac) {
        pmfLamp = sample.pdf;
      } else {
        numArea++;
        CHECK(sample.pdf ==
              doctest::Approx(lights.solidAnglePDF(
                                  sample.hit.instIndex, sample.hit.faceIndex,
                                  sample.target, sample.normal,
                                  Fixture::RECEIVER, false, 0.0f))
                  .epsilon(1e-4));
      }
    }
    allocator.reset();
  }
  REQUIRE(pmfLamp > 0.0f);
  CHECK(numArea > 0);
  // A sphere's selection PMF is what is left of `solidAnglePDF(, false)` after
  // the geometry: the instances are translated only, so the position
  // density is one over the object area.
  auto spherePMF{[&](int i) {
    auto hitState{makeRenderState(fixture.wavelengths, &allocator)};
    const auto hit{fixture.hitOn(i, hitState)};
    const float3 toLight{hit.point - Fixture::RECEIVER};
    const float distSq{lengthSquared(toLight)};
    const float cosLight{absDot(hit.Ng, toLight / std::sqrt(distSq))};
    const float pmf{lights.solidAnglePDF(hit.instIndex, hit.faceIndex,
                                         hit.point, hit.Ng, Fixture::RECEIVER,
                                         true, 0.0f) *
                    area * cosLight / distSq};
    allocator.reset();
    return pmf;
  }};
  const float pmfExitance{spherePMF(0)};
  const float pmfPower{spherePMF(4)};
  CHECK(pmfLamp == doctest::Approx(pmfExitance).epsilon(1e-3));
  CHECK(pmfPower == doctest::Approx(pmfExitance / PI).epsilon(1e-3));
  CHECK(pmfLamp + pmfExitance + pmfPower ==
        doctest::Approx(1.0f).epsilon(1e-3));
}

namespace {

// Two `glow` spheres, one of them mirrored across X so that the cone
// draw's object-space mapping is exercised through a reflecting
// placement, seen from the receiver below and to the side.
class ConeFixture final {
public:
  ConeFixture() {
    if (auto error{compiler.addCode("::conetest", MATERIALS)}) {
      MESSAGE(error->message);
      REQUIRE(false);
    }
    for (int i = 0; i < 2; i++) {
      LayoutItem item{};
      item.primitive.shape = PrimitiveSpec::Shape::SPHERE;
      item.primitive.radius = RADIUS;
      item.materials.all = "glow";
      item.isLight = true;
      if (i == 1) item.objectToWorld[0] = float4(-1.0f, 0.0f, 0.0f, 0.0f);
      item.objectToWorld[3] = float4(center(i), 1.0f);
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
    renderGrid().wavelengths = wavelengths;
    scene.commit(wavelengths);
  }

  [[nodiscard]] static float3 center(int i) noexcept {
    return float3(2.0f * float(i), 0.0f, 0.0f);
  }

  static constexpr float RADIUS{0.5f};
  static constexpr float3 RECEIVER{4.0f, 0.0f, -3.0f};

  smdl::Compiler compiler{};
  Scene scene{compiler};
  Color wavelengths{};
};

} // namespace

TEST_CASE("LightSampler: a sphere is drawn by its cone, or by area for a "
          "manifold gather") {
  ConeFixture fixture{};
  const LightSampler lights{
      fixture.compiler, fixture.scene, nullptr, {}, fixture.wavelengths};
  auto allocator{smdl::BumpPtrAllocator()};
  auto state{makeRenderState(fixture.wavelengths, &allocator)};
  Sampler sampler{};
  // Exitance 2 per band is a radiance of `2 / pi`; the irradiance of a
  // sphere on a receiver facing its center is `pi L (R / d)^2`; both
  // spheres weigh the same, so each is drawn half the time.
  const float radiance{2.0f / PI};
  constexpr int NUM_DRAWS{4096};
  double sum[2][2]{};
  double sumSq[2][2]{};
  int num[2][2]{};
  for (int i = 0; i < NUM_DRAWS; i++) {
    for (int pass = 0; pass < 2; pass++) {
      const bool keepDark{pass == 1};
      sampler.startPixelSample(0, uint32_t(i));
      LightSample sample{};
      if (!lights.sample(state, sampler, ConeFixture::RECEIVER, 0.0f, sample,
                         keepDark)) {
        allocator.reset();
        continue;
      }
      CAPTURE(i);
      CAPTURE(pass);
      const int k{int(sample.hit.instIndex)};
      REQUIRE(k < 2);
      const float3 toCenter{ConeFixture::center(k) - ConeFixture::RECEIVER};
      const float dist{length(toCenter)};
      const float3 axis{toCenter / dist};
      const float cosThetaMax{std::sqrt(
          1.0f - ConeFixture::RADIUS * ConeFixture::RADIUS / (dist * dist))};
      // On the sphere, at the density the arrival site recomputes for the
      // same technique.
      CHECK(length(sample.target - ConeFixture::center(k)) ==
            doctest::Approx(ConeFixture::RADIUS).epsilon(1.0e-4));
      CHECK(sample.pdf ==
            doctest::Approx(
                lights.solidAnglePDF(uint32_t(k), sample.hit.faceIndex,
                                     sample.target, sample.normal,
                                     ConeFixture::RECEIVER, keepDark, 0.0f))
                .epsilon(1.0e-4));
      const double estimate{double(sample.Li[0]) * dot(sample.wi, axis) /
                            sample.pdf};
      if (!keepDark) {
        // Inside the cone, facing the receiver, lit, at the cone's density.
        CHECK(dot(sample.wi, axis) >= cosThetaMax * (1.0f - 1.0e-5f));
        CHECK(dot(sample.normal, -sample.wi) > 0.0f);
        CHECK(sample.Li[0] == doctest::Approx(radiance).epsilon(1.0e-3));
        CHECK(sample.pdf ==
              doctest::Approx(0.5f / (TWO_PI * (1.0f - cosThetaMax)))
                  .epsilon(1.0e-3));
      }
      num[pass][k]++;
      sum[pass][k] += estimate;
      sumSq[pass][k] += estimate * estimate;
      allocator.reset();
    }
  }
  for (int k = 0; k < 2; k++) {
    CAPTURE(k);
    const float dist{length(ConeFixture::center(k) - ConeFixture::RECEIVER)};
    const double irradiance{PI * radiance * ConeFixture::RADIUS *
                            ConeFixture::RADIUS / (dist * dist)};
    CHECK(num[0][k] > NUM_DRAWS / 4);
    CHECK(num[1][k] > NUM_DRAWS / 4);
    CHECK(sum[0][k] / NUM_DRAWS == doctest::Approx(irradiance).epsilon(0.02));
    CHECK(sum[1][k] / NUM_DRAWS == doctest::Approx(irradiance).epsilon(0.08));
    // Among the draws of one sphere, the cone estimate barely varies
    // while the area estimate is zero half the time.
    const auto spread{[&](int pass) {
      const double mean{sum[pass][k] / num[pass][k]};
      return std::sqrt(
          std::max(sumSq[pass][k] / num[pass][k] - mean * mean, 0.0));
    }};
    CHECK(spread(0) < 0.1 * spread(1));
  }
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
    item.isLight = true;
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
    renderGrid().wavelengths = wavelengths;
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
    if (!lights.sample(state, sampler, LampFixture::RECEIVER, 0.0f, sample)) {
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
      CHECK(!sample.isReachable);
      CHECK(sample.hit.material == nullptr);
      for (size_t k = 0; k < fixture.wavelengths.size(); k++)
        CHECK(sample.Li[k] == doctest::Approx(LampFixture::RADIANCE));
      if (pmfShape < 0.0f) pmfShape = pmf;
      CHECK(pmf == doctest::Approx(pmfShape).epsilon(1.0e-4));
    } else {
      numLamp++;
      sumLamp += estimate;
      CHECK(sample.isReachable);
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
      if (!lights.sample(state, sampler, LampFixture::RECEIVER, 0.0f, sample) ||
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
    const float solidAngle{
        4.0f * std::atan(3.0f * 1.0f /
                         (LampFixture::HEIGHT *
                          std::sqrt(LampFixture::HEIGHT * LampFixture::HEIGHT +
                                    9.0f + 1.0f)))};
    int num{};
    for (int i = 0; i < 1024; i++) {
      sampler.startPixelSample(0, uint32_t(i));
      LightSample sample{};
      if (!lights.sample(state, sampler, LampFixture::RECEIVER, 0.0f, sample) ||
          sample.analyticIndex != 0) {
        allocator.reset();
        continue;
      }
      CAPTURE(i);
      num++;
      CHECK(std::abs(sample.target.x) <= 3.0f * (1.0f + 1.0e-5f));
      CHECK(std::abs(sample.target.y) <= 1.0f * (1.0f + 1.0e-5f));
      CHECK(sample.target.z == doctest::Approx(LampFixture::HEIGHT));
      CHECK(sample.normal.z == doctest::Approx(-1.0f));
      CHECK(sample.Li[0] == doctest::Approx(radiance));
      // Uniform over the spherical rectangle: the same density for every
      // draw, one over the solid angle a 6 by 2 rectangle subtends 3
      // units below its center, times the selection PMF.
      CHECK(sample.pdf == doctest::Approx(0.5f / solidAngle).epsilon(1.0e-3));
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
      const bool drawn{lights.sample(state, sampler, above, 0.0f, sample)};
      if (drawn) CHECK(sample.analyticIndex == INVALID_INDEX);
      sampler.startPixelSample(0, uint32_t(i));
      if (!lights.sample(state, sampler, above, 0.0f, sample, true) ||
          sample.analyticIndex != 0) {
        allocator.reset();
        continue;
      }
      CAPTURE(i);
      numDark++;
      CHECK(!drawn);
      CHECK(sample.Li.isAllZero());
      CHECK(sample.normal.z == doctest::Approx(-1.0f));
      const Color below{lights.reevaluateLi(sample, state, above,
                                            LampFixture::RECEIVER, 0.0f)};
      CHECK(below[0] == doctest::Approx(LampFixture::RADIANCE));
      CHECK(
          lights.reevaluateLi(sample, state, above, beside, 0.0f).isAllZero());
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
      if (!lights.sample(state, sampler, above, 0.0f, sample) ||
          sample.analyticIndex != 0) {
        allocator.reset();
        continue;
      }
      CAPTURE(i);
      numUp++;
      CHECK(sample.normal.z == doctest::Approx(1.0f));
      CHECK(sample.Li[0] == doctest::Approx(LampFixture::RADIANCE));
      CHECK(
          lights.reevaluateLi(sample, state, above, LampFixture::RECEIVER, 0.0f)
              .isAllZero());
      allocator.reset();
    }
    CHECK(numUp > 8);
  }
}

// Moving emitters against still twins placed at their keys: a sphere
// lamp translating over the shutter beside still lamps at its open and
// shut positions, and a mesh light scaling over the shutter beside a
// still light at its shut scale. The sampler must place a moving
// light where the path's time puts it, and its densities must agree
// both with themselves and with the still twin's, which for the mesh
// light is the object-area path against the world-area path.
namespace {

class MotionFixture final {
public:
  MotionFixture() {
    fs::remove_all(dir);
    fs::create_directories(dir);
    quad = (dir / "quad.obj").string();
    {
      std::ofstream file(quad, std::ios::binary | std::ios::trunc);
      file << "o quad\nv -1 -1 0\nv 1 -1 0\nv 1 1 0\nv -1 1 0\nf 1 2 3 4\n";
    }
    if (auto error{compiler.addCode("::motiontest", MATERIALS)}) {
      MESSAGE(error->message);
      REQUIRE(false);
    }
    const auto sphereAt{[](const float3 &center) {
      LayoutItem item{};
      item.primitive.shape = PrimitiveSpec::Shape::SPHERE;
      item.primitive.radius = RADIUS;
      item.materials.all = "glow";
      item.isLight = true;
      item.objectToWorld[3] = float4(center, 1.0f);
      return item;
    }};
    // 0: the moving lamp; 1 and 2: still lamps at its two keys.
    auto moving{sphereAt(CENTER_OPEN)};
    moving.objectToWorldShut = sphereAt(CENTER_SHUT).objectToWorld;
    scene.add(moving);
    scene.add(sphereAt(CENTER_OPEN));
    scene.add(sphereAt(CENTER_SHUT));
    // 3: the quad below the receiver, facing up, scaling from 1 to 2
    // over the shutter; 4: a still quad at the shut scale.
    const auto quadAt{[&](float scale) {
      LayoutItem item{};
      item.fileName = quad;
      item.materials.all = "glow";
      item.isLight = true;
      item.objectToWorld[0].x = scale;
      item.objectToWorld[1].y = scale;
      item.objectToWorld[3] = float4(QUAD_CENTER, 1.0f);
      return item;
    }};
    auto movingQuad{quadAt(1.0f)};
    movingQuad.objectToWorldShut = quadAt(2.0f).objectToWorld;
    scene.add(movingQuad);
    scene.add(quadAt(2.0f));
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
    renderGrid().wavelengths = wavelengths;
    scene.commit(wavelengths);
  }
  ~MotionFixture() { fs::remove_all(dir); }

  static constexpr float RADIUS{0.5f};
  static constexpr float3 CENTER_OPEN{0.0f, 0.0f, 0.0f};
  static constexpr float3 CENTER_SHUT{4.0f, 0.0f, 0.0f};
  static constexpr float3 QUAD_CENTER{2.0f, 0.0f, -5.0f};
  static constexpr float3 RECEIVER{2.0f, 0.0f, -3.0f};

  fs::path dir{fs::temp_directory_path() / "smdl-toy-light-motion-test"};
  std::string quad{};
  smdl::Compiler compiler{};
  Scene scene{compiler};
  Color wavelengths{};
};

} // namespace

TEST_CASE("LightSampler: a moving emitter is placed at the path's time") {
  MotionFixture fixture{};
  // The flat distribution, so that lights of equal power have equal
  // selection probabilities wherever the receiver is: the moving lamp's
  // weight is its open-key area, the same as its still twins'.
  const LightSampler lights{
      fixture.compiler,    fixture.scene, nullptr,          {},
      fixture.wavelengths, false,         /*useTree=*/false};
  auto allocator{smdl::BumpPtrAllocator()};
  auto state{makeRenderState(fixture.wavelengths, &allocator)};
  Sampler sampler{};
  constexpr int NUM_DRAWS{2048};
  SUBCASE("The sphere lamp: the cone at the time, and the still twin's") {
    for (const float u : {0.0f, 1.0f, 0.5f}) {
      CAPTURE(u);
      const float3 center{(1.0f - u) * MotionFixture::CENTER_OPEN +
                          u * MotionFixture::CENTER_SHUT};
      int numDrawn{};
      for (int i = 0; i < NUM_DRAWS; i++) {
        sampler.startPixelSample(0, uint32_t(i));
        LightSample sample{};
        const bool drawn{
            lights.sample(state, sampler, MotionFixture::RECEIVER, u, sample)};
        allocator.reset();
        if (!drawn || sample.hit.instIndex != 0) continue;
        numDrawn++;
        CAPTURE(i);
        CHECK(sample.hit.time == u);
        CHECK(length(sample.target - center) ==
              doctest::Approx(MotionFixture::RADIUS).epsilon(1.0e-4));
        // The density the arrival site recomputes, for this light at
        // this time, and for the still twin at the same place.
        const float own{lights.solidAnglePDF(
            0, sample.hit.faceIndex, sample.target, sample.normal,
            MotionFixture::RECEIVER, false, u)};
        CHECK(sample.pdf == doctest::Approx(own).epsilon(1.0e-4));
        if (u == 0.0f || u == 1.0f) {
          const uint32_t twin{u == 0.0f ? 1u : 2u};
          const float still{lights.solidAnglePDF(
              twin, sample.hit.faceIndex, sample.target, sample.normal,
              MotionFixture::RECEIVER, false, 0.0f)};
          CHECK(sample.pdf == doctest::Approx(still).epsilon(1.0e-4));
        }
      }
      CHECK(numDrawn > NUM_DRAWS / 16);
    }
  }
  SUBCASE("The mesh light: object area with the stretch at the time") {
    for (const float u : {0.0f, 1.0f}) {
      CAPTURE(u);
      const float scale{1.0f + u};
      int numDrawn{};
      for (int i = 0; i < NUM_DRAWS; i++) {
        sampler.startPixelSample(0, uint32_t(i));
        LightSample sample{};
        const bool drawn{
            lights.sample(state, sampler, MotionFixture::RECEIVER, u, sample)};
        allocator.reset();
        if (!drawn || sample.hit.instIndex != 3) continue;
        numDrawn++;
        CAPTURE(i);
        // On the quad as scaled at the time.
        CHECK(sample.target.z ==
              doctest::Approx(MotionFixture::QUAD_CENTER.z).epsilon(1.0e-5));
        CHECK(std::fabs(sample.target.x - MotionFixture::QUAD_CENTER.x) <=
              scale * (1.0f + 1.0e-5f));
        CHECK(std::fabs(sample.target.y - MotionFixture::QUAD_CENTER.y) <=
              scale * (1.0f + 1.0e-5f));
        const float own{lights.solidAnglePDF(3, sample.hit.faceIndex,
                                             sample.target, sample.normal,
                                             MotionFixture::RECEIVER, true, u)};
        CHECK(sample.pdf == doctest::Approx(own).epsilon(1.0e-4));
        if (u == 1.0f) {
          // The still twin at the shut scale weighs four times as much
          // (its area is the shut-key area, the moving light's the
          // open-key one), so its selection probability is four times
          // the moving light's; past that, the two densities are one.
          const float still{lights.solidAnglePDF(
              4, sample.hit.faceIndex, sample.target, sample.normal,
              MotionFixture::RECEIVER, true, 0.0f)};
          CHECK(4.0f * own == doctest::Approx(still).epsilon(1.0e-4));
        }
      }
      CHECK(numDrawn > NUM_DRAWS / 32);
    }
  }
}

TEST_CASE("AnalyticLight: a moving light interpolates its placement") {
  MotionFixture fixture{};
  auto state{makeRenderState(fixture.wavelengths)};
  const auto translated{[](const float3 &offset) {
    float4x4 xf{1.0f};
    xf[3] = float4(offset, 1.0f);
    return xf;
  }};
  SUBCASE("A point light moves along the chord") {
    LayoutLight lamp{};
    lamp.decl.kind = LayoutLightDecl::Kind::POINT;
    lamp.decl.power = 10.0f;
    lamp.lightToWorld = translated(float3(0.0f, 0.0f, 5.0f));
    lamp.lightToWorldShut = translated(float3(4.0f, 0.0f, 5.0f));
    const AnalyticLight light{fixture.compiler, state, fixture.wavelengths,
                              lamp, nullptr};
    CHECK(light.position(0.0f).x == 0.0f);
    CHECK(light.position(1.0f).x == 4.0f);
    CHECK(light.position(0.5f).x == doctest::Approx(2.0f));
    CHECK(light.position(0.5f).z == doctest::Approx(5.0f));
    const auto box{light.bounds()};
    CHECK(box.lower.x == 0.0f);
    CHECK(box.upper.x == 4.0f);
    // At the shut key the moving light is the still light placed
    // there, bit for bit: the lerp reproduces its ends exactly.
    LayoutLight still{lamp};
    still.lightToWorld = *lamp.lightToWorldShut;
    still.lightToWorldShut.reset();
    const AnalyticLight stillLight{fixture.compiler, state, fixture.wavelengths,
                                   still, nullptr};
    const float3 point{4.0f, 1.0f, 0.0f};
    const Color moving{light.Li(point, 1.0f, 1.0f)};
    const Color placed{stillLight.Li(point, 1.0f, 0.0f)};
    for (size_t k = 0; k < fixture.wavelengths.size(); k++)
      CHECK(moving[k] == placed[k]);
    CHECK(moving[0] > 0.0f);
    CHECK(light.Li(point, 1.0f, 0.0f)[0] < moving[0]);
  }
  SUBCASE("A rect pays its area at the time") {
    LayoutLight panel{};
    panel.decl.kind = LayoutLightDecl::Kind::RECT;
    panel.decl.size = float2(2.0f, 1.0f);
    panel.decl.power = 10.0f;
    panel.lightToWorld = translated(float3(0.0f, 0.0f, 5.0f));
    auto shut{panel.lightToWorld};
    shut[0].x = 2.0f;
    shut[1].y = 2.0f;
    shut[3].z = 7.0f;
    panel.lightToWorldShut = shut;
    const AnalyticLight light{fixture.compiler, state, fixture.wavelengths,
                              panel, nullptr};
    LayoutLight still{panel};
    still.lightToWorld = shut;
    still.lightToWorldShut.reset();
    const AnalyticLight stillLight{fixture.compiler, state, fixture.wavelengths,
                                   still, nullptr};
    const float3 receiver{0.5f, 0.2f, 0.0f};
    for (int i = 0; i < 16; i++) {
      CAPTURE(i);
      const float2 xi{(float(i) + 0.5f) / 16.0f,
                      std::fmod(0.618034f * float(i) + 0.3f, 1.0f)};
      float pdfMoving{};
      float pdfStill{};
      const float3 pointMoving{
          light.sampleShape(receiver, xi, pdfMoving, 1.0f)};
      const float3 pointStill{
          stillLight.sampleShape(receiver, xi, pdfStill, 0.0f)};
      CHECK(pointMoving.x == pointStill.x);
      CHECK(pointMoving.y == pointStill.y);
      CHECK(pointMoving.z == 7.0f);
      CHECK(pdfMoving == pdfStill);
      CHECK(pdfMoving > 0.0f);
      // At the open key the panel is half the size, so the same draw
      // lands within the smaller extent at the lower height.
      float pdfOpen{};
      const float3 pointOpen{light.sampleShape(receiver, xi, pdfOpen, 0.0f)};
      CHECK(pointOpen.z == 5.0f);
      CHECK(std::fabs(pointOpen.x) <= 1.0f + 1.0e-5f);
      CHECK(std::fabs(pointOpen.y) <= 0.5f + 1.0e-5f);
    }
    CHECK(light.normal(1.0f).z == doctest::Approx(-1.0f));
    // The radiance is baked at the open key, where the panel has a
    // quarter of the shut-key area, so the moving panel radiates four
    // times what the still twin does from the same power; the emitting
    // side faces the receiver below at both keys.
    const float3 onPanel{0.0f, 0.0f, 7.0f};
    CHECK(light.Le(onPanel, receiver, 1.0f)[0] ==
          doctest::Approx(4.0f * stillLight.Le(onPanel, receiver, 0.0f)[0])
              .epsilon(1.0e-5));
    CHECK(light.Le(onPanel, receiver, 1.0f)[0] > 0.0f);
    const auto box{light.bounds()};
    CHECK(box.lower.z == 5.0f);
    CHECK(box.upper.z == 7.0f);
    CHECK(box.upper.x == 2.0f);
  }
}

namespace {

// A deforming emitter: the morph quad of `RigFixtures.h`, lit, alone in
// the scene so its selection probability is one, under the clock at
// 0.25 s with a 0.5 s shutter. Its lift is 0.25 at open and 0.75 at
// shut, and its x = 1 edge stands 0.125 out at open and 0.375 at shut,
// so its area is 1.125 at open, 1.375 at shut, and linear between.
class DeformFixture final {
public:
  DeformFixture() {
    fs::remove_all(dir);
    fs::create_directories(dir);
    files = rig::writeFiles(dir);
    if (auto error{compiler.addCode("::deformtest", MATERIALS)}) {
      MESSAGE(error->message);
      REQUIRE(false);
    }
    renderShutter().time = 0.25f;
    renderShutter().length = 0.5f;
    LayoutItem item{};
    item.fileName = files.morph;
    item.materials.all = "glow";
    item.isLight = true;
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
    renderGrid().wavelengths = wavelengths;
    scene.commit(wavelengths);
  }
  ~DeformFixture() {
    renderShutter().time = 0.0f;
    renderShutter().length = 0.0f;
    fs::remove_all(dir);
  }

  static constexpr float3 RECEIVER{0.6f, 0.5f, 3.0f};

  fs::path dir{fs::temp_directory_path() / "smdl-toy-light-deform-test"};
  rig::Files files{};
  smdl::Compiler compiler{};
  Scene scene{compiler};
  Color wavelengths{};
};

} // namespace

TEST_CASE("LightSampler: a deforming emitter is drawn on its surface at the "
          "time") {
  DeformFixture fixture{};
  const LightSampler lights{
      fixture.compiler,    fixture.scene, nullptr,          {},
      fixture.wavelengths, false,         /*useTree=*/false};
  auto allocator{smdl::BumpPtrAllocator()};
  auto state{makeRenderState(fixture.wavelengths, &allocator)};
  Sampler sampler{};
  constexpr int NUM_DRAWS{1024};
  REQUIRE(fixture.scene.meshInstances[0].isDeforming);
  for (const float u : {0.0f, 0.5f, 1.0f}) {
    CAPTURE(u);
    const float lift{0.25f + 0.5f * u};
    const float stretch{0.125f + 0.25f * u};
    const float area{1.0f + stretch};
    int numDrawn{};
    for (int i = 0; i < NUM_DRAWS; i++) {
      sampler.startPixelSample(0, uint32_t(i));
      LightSample sample{};
      const bool drawn{
          lights.sample(state, sampler, DeformFixture::RECEIVER, u, sample)};
      allocator.reset();
      if (!drawn) continue;
      numDrawn++;
      CAPTURE(i);
      CHECK(sample.hit.time == u);
      // On the quad as it stands at the time.
      CHECK(sample.target.z == doctest::Approx(lift).epsilon(1e-4));
      CHECK(sample.target.x >= -1e-4f);
      CHECK(sample.target.x <= 1.0f + stretch + 1e-4f);
      CHECK(sample.target.y >= -1e-4f);
      CHECK(sample.target.y <= 1.0f + 1e-4f);
      // Uniform over the quad's area at the time: each face is drawn by
      // its object area, the two being equal, and pays its own world
      // area then, so the density is one over the whole area.
      const float3 direction{sample.target - DeformFixture::RECEIVER};
      const float distSq{smdl::dot(direction, direction)};
      const float cosTheta{
          std::fabs(smdl::dot(sample.normal, smdl::normalize(direction)))};
      CHECK(sample.pdf ==
            doctest::Approx(distSq / (area * cosTheta)).epsilon(1e-3));
      // The arrival site recovers the same density from the hit's face.
      const float own{lights.solidAnglePDF(
          sample.hit.instIndex, sample.hit.faceIndex, sample.target,
          sample.normal, DeformFixture::RECEIVER, true, u)};
      CHECK(sample.pdf == doctest::Approx(own).epsilon(1e-4));
    }
    CHECK(numDrawn > NUM_DRAWS / 2);
  }
}
