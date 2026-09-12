#include "RenderFixtures.h"

#include <algorithm>
#include <cmath>
#include <memory>
#include <optional>
#include <vector>

#include "smdl/Compiler.h"
#include "smdl/RenderUtil/OpticalGlass.h"
#include "smdl/Support/Span.h"

#include "Color.h"
#include "Render/Medium.h"
#include "Render/Sampler.h"

// The participating-medium estimators over materials compiled here: the
// closed forms a homogeneous medium takes, the null-collision tracking
// everything else takes, the additive overlap that sums two media into
// one segment, and the phase function a collision scatters with.
//
// What is checked is the estimator's EXPECTATION, because that is the
// only thing a Monte Carlo estimator promises and the only thing a
// change to how the medium is represented has to preserve. Every
// expectation here has a closed form: Beer-Lambert for the
// transmittance, the collision-density integral for the scattering
// weight, and, where nothing absorbs, the two summing to one per band.
// The sampler is deterministic in (pixel, sample index), so the sample
// means below are fixed numbers rather than a flaky draw.

namespace {
// Four bands, so that the coefficient spectra vary from bin to bin and
// the estimators' wavelength-hero weighting is exercised instead of
// collapsing to a scalar. Uniformly spaced, so the grid stays uniform
// quadrature.
const std::vector<float> GRID{420.0f, 500.0f, 580.0f, 660.0f};

// Two kinds of coefficient appear below: flat constants, and
// `ramp(lo, hi)`, a spectrum that slopes across the four bands, which is
// where the wavelength-hero weighting is exercised. Either proves
// homogeneous when nothing reads the position: a spectral constant is
// resampled onto the wavelength basis, a read the proof allows, so the
// tracked cases are exactly the ones whose coefficients read
// `state::position()`. The phase function is proven on its own, and
// `bias_sign` is the one whose VDF reads the position.
const char *MATERIALS{
    "#smdl\n"
    "import ::df::*;\n"
    "import ::math::*;\n"
    "import ::state::*;\n"
    "@(macro) color ramp(const float lo, const float hi) =\n"
    "  math::emission_color(float[](380.0, 780.0), float[](lo, hi));\n"
    // No volume at all: a stack entry that carries nothing.
    "export material clear() = material(\n"
    "  surface: material_surface(scattering: "
    "df::diffuse_reflection_bsdf()));\n"
    // Homogeneous, absorbing and scattering.
    "export material fog() = material(\n"
    "  volume: material_volume(\n"
    "    scattering: df::anisotropic_vdf(),\n"
    "    absorption_coefficient: color(0.20),\n"
    "    scattering_coefficient: color(0.30)));\n"
    // Homogeneous with a sloped spectrum, so that the closed forms see
    // the wavelength-hero weighting that flat coefficients hide.
    "export material fog_sloped() = material(\n"
    "  volume: material_volume(\n"
    "    scattering: df::anisotropic_vdf(),\n"
    "    absorption_coefficient: ramp(0.10, 0.30),\n"
    "    scattering_coefficient: ramp(0.20, 0.40)));\n"
    // The two halves of an additive overlap, and the single medium whose
    // coefficients are their sum, written as the same sum so that the
    // two answers are the same floating-point number.
    "export material fog_a() = material(\n"
    "  volume: material_volume(\n"
    "    scattering: df::anisotropic_vdf(directional_bias: 0.0),\n"
    "    absorption_coefficient: color(0.05),\n"
    "    scattering_coefficient: color(0.30),\n"
    "    additive: true));\n"
    "export material fog_b() = material(\n"
    "  volume: material_volume(\n"
    "    scattering: df::anisotropic_vdf(directional_bias: 0.6),\n"
    "    absorption_coefficient: color(0.02),\n"
    "    scattering_coefficient: color(0.12),\n"
    "    additive: true));\n"
    "export material fog_sum() = material(\n"
    "  volume: material_volume(\n"
    "    scattering: df::anisotropic_vdf(),\n"
    "    absorption_coefficient: color(0.05) + color(0.02),\n"
    "    scattering_coefficient: color(0.30) + color(0.12)));\n"
    // Homogeneous and emitting, for the closed-form emission integral.
    "export material glow() = material(\n"
    "  volume: material_volume(\n"
    "    absorption_coefficient: color(0.20),\n"
    "    emission_intensity: color(3.0)));\n"
    // Heterogeneous: sigma_s(x) = base * x / 2 along +X, so over the
    // segment [0, 2] the optical depth is exactly `base` and the
    // declared majorant `base` is reached only at the far end.
    "export material ramp_vol() = material(\n"
    "  volume: material_volume(\n"
    "    scattering: df::anisotropic_vdf(),\n"
    "    scattering_coefficient:\n"
    "      ramp(0.30, 0.90) * (0.5 * #max(state::position().x, 0.0)),\n"
    "    max_scattering_coefficient: ramp(0.30, 0.90)));\n"
    // The same ramp, additive, and a second one with a different
    // spectrum, so that an overlap of two tracked components divides its
    // collisions between two per-band scattering coefficients.
    "export material ramp_add() = material(\n"
    "  volume: material_volume(\n"
    "    scattering: df::anisotropic_vdf(directional_bias: 0.0),\n"
    "    scattering_coefficient:\n"
    "      ramp(0.30, 0.90) * (0.5 * #max(state::position().x, 0.0)),\n"
    "    max_scattering_coefficient: ramp(0.30, 0.90),\n"
    "    additive: true));\n"
    "export material ramp_add2() = material(\n"
    "  volume: material_volume(\n"
    "    scattering: df::anisotropic_vdf(directional_bias: 0.5),\n"
    "    scattering_coefficient:\n"
    "      ramp(0.80, 0.20) * (0.5 * #max(state::position().x, 0.0)),\n"
    "    max_scattering_coefficient: ramp(0.80, 0.20),\n"
    "    additive: true));\n"
    // Position-dependent with no majorant to track against, which falls
    // back to the coefficient snapshot the evaluation captured (nonzero at
    // the origin, unlike the ramps above).
    "export material ramp_nomax() = material(\n"
    "  volume: material_volume(\n"
    "    scattering: df::anisotropic_vdf(),\n"
    "    scattering_coefficient:\n"
    "      ramp(0.30, 0.90) * (0.5 + 0.25 * state::position().x)));\n"
    // Constant coefficients under a phase function whose bias changes
    // sign across x = 0: the closed forms sample the distance, and each
    // collision must scatter with the bias of its own side.
    "export material bias_sign() = material(\n"
    "  volume: material_volume(\n"
    "    scattering: df::anisotropic_vdf(\n"
    "      directional_bias: state::position().x < 0.0 ? -0.8 : 0.8),\n"
    "    scattering_coefficient: color(0.5)));\n"};
} // namespace

namespace {

// The compiler, the render grid, and the evaluated materials the cases
// build stacks out of. Built per test case, since the compiler must
// outlive everything evaluated through it and the evaluations point into
// its allocator.
class Fixture final {
private:
  // Declared first, so that every `Color` below is sized to this grid.
  const ScopedGrid mGrid{GRID, false};

public:
  Fixture() {
    REQUIRE_OK(compiler.addCode("::mediumtest", MATERIALS));
    // O2, because `hasHomogeneousCoefficients()` is derived after optimization
    // and degrades to unknown without it, which would send every
    // material here down the heterogeneous path.
    REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_O2));
    REQUIRE_OK(compiler.jitCompile());
    state.emplace(makeRenderState(wavelengths, &allocator, 0.0f));
    // The coefficient snapshots are captured at this point, which is
    // what a position-dependent material without a majorant falls back
    // on.
    state->position = float3(0.0f, 0.0f, 0.0f);
  }

  Fixture(const Fixture &) = delete;

  Fixture &operator=(const Fixture &) = delete;

  // Did the compiler prove the material's volume coefficients
  // position-independent? Which of the two paths below a medium takes
  // hangs on this, so every case states the answer it is written for.
  [[nodiscard]] bool isProvablyHomogeneous(const char *name) {
    const smdl::JIT::MaterialDef *materialDef{compiler.findMaterial(name)};
    REQUIRE(materialDef);
    return materialDef->hasHomogeneousCoefficients();
  }

  // A stack entry over the material `name`, whose evaluation lives as long
  // as the fixture. `prev` is the entry below it.
  [[nodiscard]] MediumStack &entry(const char *name,
                                   const MediumStack *prev = nullptr) {
    const smdl::JIT::MaterialDef *materialDef{compiler.findMaterial(name)};
    REQUIRE(materialDef);
    mMaterials.push_back(
        std::make_unique<smdl::JIT::Material>(*state, materialDef));
    mEntries.push_back(std::make_unique<MediumStack>(
        MediumStack{prev, mMaterials.back().get(), nullptr}));
    return *mEntries.back();
  }

  // The render basis, as the `Color` the medium's interface takes.
  const Color wavelengths{gRenderGrid.wavelengths};

  smdl::Compiler compiler{uint32_t(GRID.size())};
  smdl::BumpPtrAllocator allocator{};
  std::optional<smdl::State> state{};

private:
  std::vector<std::unique_ptr<smdl::JIT::Material>> mMaterials{};

  std::vector<std::unique_ptr<MediumStack>> mEntries{};
};

// The absorption and scattering coefficients the evaluation captured, in
// inverse scene units, which is what the closed forms below are the
// closed forms of.
struct Coefficients final {
  Color sigmaA{};
  Color sigmaS{};
  Color emission{};

  [[nodiscard]] Color extinction() const { return sigmaA + sigmaS; }
};

[[nodiscard]] Coefficients coefficientsOf(const MediumStack &entry,
                                          float unitScale) {
  return {Color(entry.material->getAbsorptionCoefficient()) * unitScale,
          Color(entry.material->getScatteringCoefficient()) * unitScale,
          Color(entry.material->getVolumeEmissionIntensity()) * unitScale};
}

// exp(-x) per band, which is what the medium's own `transmittance()`
// approximates; compared against below at a tolerance that admits the
// approximation but nothing else.
[[nodiscard]] Color beerLambert(const Color &mu, float distance) {
  Color result{};
  for (size_t i = 0; i < result.size(); i++)
    result[i] = std::exp(-double(mu[i]) * double(distance));
  return result;
}

void checkClose(const Color &value, const Color &expect, float tolerance,
                const char *what) {
  INFO(what);
  for (size_t i = 0; i < expect.size(); i++) {
    CAPTURE(i);
    CAPTURE(value[i]);
    CAPTURE(expect[i]);
    CHECK(std::abs(value[i] - expect[i]) <=
          tolerance * std::max(std::abs(expect[i]), 1e-3f));
  }
}

// How many samples the expectation checks average over. Every draw is
// deterministic, so this is a fixed cost and a fixed answer.
constexpr int NUM_SAMPLES{16384};

// The tolerance the sample means are checked at. Loose enough to cover
// the estimators' own variance at `NUM_SAMPLES` and any platform
// difference in the exponential, tight enough that a dropped component,
// a transposed majorant or a mis-normalized weight cannot pass.
constexpr float MEAN_TOLERANCE{0.02f};

// The sample means of one segment's estimators: what the walk keeps when
// the segment survives, what it keeps when it scatters, and (with an
// overlap) the same split by which component's phase function was
// picked.
struct Means final {
  Color survived{};
  Color scattered{};
  Color pickedFirst{};
  Color emitted{};
  int numScattered{};
};

[[nodiscard]] Means sampleMeans(Medium &medium, const MediumStack *stack,
                                const Color &wavelengths, const float3 &org,
                                const float3 &dir, float tEnd,
                                const smdl::JIT::Material *first) {
  Means means{};
  Sampler sampler{};
  // Where a collision's own VDF goes when the phase function is not
  // provably point-independent: the path's allocator in the renderer,
  // reset per sample as the renderer resets it per path.
  smdl::BumpPtrAllocator allocator{};
  for (int i = 0; i < NUM_SAMPLES; i++) {
    sampler.startPixelSample(uint32_t(i), 0);
    allocator.reset();
    medium.reset(stack, wavelengths, PathTime(0.0f), smdl::FRAUNHOFER_D_LINE,
                 org, dir);
    Color beta{1.0f};
    Color emitted{};
    float t{};
    if (medium.sampleDistance(sampler, tEnd, t, beta, emitted)) {
      means.scattered += beta;
      means.numScattered++;
      // The scatterer names the evaluation it scatters with by its VDF.
      if (first && medium.scatterer(allocator).vdf().ptr == first->getVDF().ptr)
        means.pickedFirst += beta;
    } else {
      means.survived += beta;
    }
    means.emitted += emitted;
  }
  const float scale{1.0f / float(NUM_SAMPLES)};
  means.survived *= scale;
  means.scattered *= scale;
  means.pickedFirst *= scale;
  means.emitted *= scale;
  return means;
}

// The mean transmittance `attenuate()` estimates over the segment.
[[nodiscard]] Color attenuateMean(Medium &medium, const MediumStack *stack,
                                  const Color &wavelengths, const float3 &org,
                                  const float3 &dir, float tEnd) {
  Color mean{};
  Sampler sampler{};
  for (int i = 0; i < NUM_SAMPLES; i++) {
    sampler.startPixelSample(uint32_t(i), 0);
    medium.reset(stack, wavelengths, PathTime(0.0f), smdl::FRAUNHOFER_D_LINE,
                 org, dir);
    Color beta{1.0f};
    medium.attenuate(sampler, tEnd, beta);
    mean += beta;
  }
  return mean * (1.0f / float(NUM_SAMPLES));
}

} // namespace

TEST_CASE("Medium: the vacuum and the homogeneous closed forms") {
  Fixture fixture{};
  const Color &wavelengths{fixture.wavelengths};
  const float unitScale{fixture.state->metersPerSceneUnit};
  const float3 org{};
  const float3 dir{1.0f, 0.0f, 0.0f};
  constexpr float DISTANCE{1.5f};
  Medium medium{};

  {
    INFO("an empty stack with no haze is a vacuum");
    medium.reset(nullptr, wavelengths, PathTime(0.0f), smdl::FRAUNHOFER_D_LINE,
                 org, dir);
    CHECK_FALSE(medium.hasHaze());
    CHECK_FALSE(medium.hasMedium());
    CHECK_FALSE(medium.attenuationDraws());
    Sampler sampler{};
    sampler.startPixelSample(0, 0);
    Color beta{1.0f};
    medium.attenuate(sampler, DISTANCE, beta);
    Color emitted{};
    float t{-1.0f};
    CHECK_FALSE(medium.sampleDistance(sampler, DISTANCE, t, beta, emitted));
    checkClose(beta, Color(1.0f), 0.0f, "the vacuum attenuates nothing");
    checkClose(emitted, Color(), 0.0f, "the vacuum emits nothing");
    // Neither call may draw, which is what lets a caller skip the view
    // outright over a vacuum segment without moving the sample sequence.
    Sampler reference{};
    reference.startPixelSample(0, 0);
    CHECK(float(sampler) == float(reference));
  }

  {
    INFO("an entry whose material has no volume carries no medium");
    MediumStack &clear{fixture.entry("clear")};
    medium.reset(&clear, wavelengths, PathTime(0.0f), smdl::FRAUNHOFER_D_LINE,
                 org, dir);
    CHECK_FALSE(medium.hasMedium());
  }

  REQUIRE(fixture.isProvablyHomogeneous("fog") == true);
  MediumStack &fog{fixture.entry("fog")};
  const Coefficients fogC{coefficientsOf(fog, unitScale)};
  const Color fogTr{beerLambert(fogC.extinction(), DISTANCE)};
  {
    INFO("a homogeneous medium attenuates by Beer-Lambert, drawing nothing");
    medium.reset(&fog, wavelengths, PathTime(0.0f), smdl::FRAUNHOFER_D_LINE,
                 org, dir);
    REQUIRE(medium.hasMedium());
    CHECK_FALSE(medium.attenuationDraws());
    Sampler sampler{};
    sampler.startPixelSample(0, 0);
    Color beta{1.0f};
    medium.attenuate(sampler, DISTANCE, beta);
    checkClose(beta, fogTr, 1e-4f, "the closed-form transmittance");
    Sampler reference{};
    reference.startPixelSample(0, 0);
    CHECK(float(sampler) == float(reference));
  }

  {
    INFO("the free-flight estimator's expectation is the closed form");
    const Means means{
        sampleMeans(medium, &fog, wavelengths, org, dir, DISTANCE, nullptr)};
    checkClose(means.survived, fogTr, MEAN_TOLERANCE, "the survival weight");
    // What the walk keeps on scattering is the scattering coefficient
    // integrated against the transmittance up to the collision.
    Color expect{};
    for (size_t i = 0; i < expect.size(); i++)
      expect[i] = fogC.sigmaS[i] * (1.0f - fogTr[i]) / fogC.extinction()[i];
    checkClose(means.scattered, expect, MEAN_TOLERANCE,
               "the scattering weight");
  }

  {
    INFO("a sloped spectrum proves homogeneous and takes the closed forms");
    REQUIRE(fixture.isProvablyHomogeneous("fog_sloped"));
    MediumStack &sloped{fixture.entry("fog_sloped")};
    const Coefficients c{coefficientsOf(sloped, unitScale)};
    // The bands must actually differ, or the wavelength-hero weighting
    // of the closed forms collapses to the flat case above.
    REQUIRE(c.extinction()[0] !=
            doctest::Approx(c.extinction()[GRID.size() - 1]));
    const Color slopedTr{beerLambert(c.extinction(), DISTANCE)};
    medium.reset(&sloped, wavelengths, PathTime(0.0f), smdl::FRAUNHOFER_D_LINE,
                 org, dir);
    REQUIRE(medium.hasMedium());
    CHECK_FALSE(medium.attenuationDraws());
    Sampler sampler{};
    sampler.startPixelSample(0, 0);
    Color beta{1.0f};
    medium.attenuate(sampler, DISTANCE, beta);
    checkClose(beta, slopedTr, 1e-4f, "the closed-form transmittance");
    const Means means{
        sampleMeans(medium, &sloped, wavelengths, org, dir, DISTANCE, nullptr)};
    checkClose(means.survived, slopedTr, MEAN_TOLERANCE, "the survival weight");
    Color expect{};
    for (size_t i = 0; i < expect.size(); i++)
      expect[i] = c.sigmaS[i] * (1.0f - slopedTr[i]) / c.extinction()[i];
    checkClose(means.scattered, expect, MEAN_TOLERANCE,
               "the scattering weight");
  }

  {
    INFO("a homogeneous medium's emission is the closed-form integral");
    REQUIRE(fixture.isProvablyHomogeneous("glow"));
    MediumStack &glow{fixture.entry("glow")};
    const Coefficients glowC{coefficientsOf(glow, unitScale)};
    medium.reset(&glow, wavelengths, PathTime(0.0f), smdl::FRAUNHOFER_D_LINE,
                 org, dir);
    REQUIRE(medium.hasMedium());
    Sampler sampler{};
    sampler.startPixelSample(0, 0);
    Color beta{1.0f}, emitted{};
    float t{};
    (void)medium.sampleDistance(sampler, DISTANCE, t, beta, emitted);
    const Color glowTr{beerLambert(glowC.extinction(), DISTANCE)};
    Color expect{};
    for (size_t i = 0; i < expect.size(); i++)
      expect[i] =
          glowC.emission[i] * (1.0f - glowTr[i]) / glowC.extinction()[i];
    checkClose(emitted, expect, 1e-3f, "the emitted radiance");
  }
}

TEST_CASE("Medium: null-collision tracking is unbiased") {
  Fixture fixture{};
  const Color &wavelengths{fixture.wavelengths};
  const float unitScale{fixture.state->metersPerSceneUnit};
  const float3 org{};
  const float3 dir{1.0f, 0.0f, 0.0f};
  // The ramp is sigma_s(x) = base * x / 2 along +X, so over [0, 2] the
  // optical depth is exactly `base`, nothing absorbs, and the declared
  // majorant is reached only at the far end.
  constexpr float DISTANCE{2.0f};
  Medium medium{};
  REQUIRE(fixture.isProvablyHomogeneous("ramp_vol") == false);
  MediumStack &ramp{fixture.entry("ramp_vol")};
  const Color base{Color(ramp.material->getMaxScatteringCoefficient()) *
                   unitScale};
  const Color rampTr{beerLambert(base, 1.0f)};
  // The bands must actually differ, or the wavelength-hero weighting the
  // tracking carries through its null collisions is not exercised.
  REQUIRE(base[0] != doctest::Approx(base[GRID.size() - 1]));

  {
    INFO("a heterogeneous medium is tracked, at a fixed cost in draws");
    medium.reset(&ramp, wavelengths, PathTime(0.0f), smdl::FRAUNHOFER_D_LINE,
                 org, dir);
    REQUIRE(medium.hasMedium());
    CHECK(medium.attenuationDraws());
    // Exactly the two the generator is seeded from, whatever the segment
    // does: a caller that discards the transmittance of a blocked shadow
    // segment consumes the same two in place of the tracking.
    Sampler sampler{};
    sampler.startPixelSample(0, 0);
    Color beta{1.0f};
    medium.attenuate(sampler, DISTANCE, beta);
    Sampler reference{};
    reference.startPixelSample(0, 0);
    (void)reference.nextBits();
    (void)reference.nextBits();
    CHECK(float(sampler) == float(reference));
  }

  {
    INFO("ratio tracking estimates the transmittance");
    checkClose(attenuateMean(medium, &ramp, wavelengths, org, dir, DISTANCE),
               rampTr, MEAN_TOLERANCE, "the mean transmittance");
  }

  {
    INFO("delta tracking splits the segment without losing energy");
    const Means means{
        sampleMeans(medium, &ramp, wavelengths, org, dir, DISTANCE, nullptr)};
    checkClose(means.survived, rampTr, MEAN_TOLERANCE, "the survival weight");
    // Nothing absorbs here, so the two halves are complements and must
    // sum to one in every band.
    checkClose(means.scattered, Color(1.0f) - rampTr, MEAN_TOLERANCE,
               "the scattering weight");
    checkClose(means.survived + means.scattered, Color(1.0f), MEAN_TOLERANCE,
               "the two halves together");
  }

  {
    INFO("a heterogeneous volume with no majorant falls back to its snapshot");
    // The material cannot bound its own field, so the medium stands
    // down to the snapshot the evaluation captured rather than tracking
    // against a majorant it does not have.
    REQUIRE_FALSE(fixture.isProvablyHomogeneous("ramp_nomax"));
    MediumStack &nomax{fixture.entry("ramp_nomax")};
    medium.reset(&nomax, wavelengths, PathTime(0.0f), smdl::FRAUNHOFER_D_LINE,
                 org, dir);
    REQUIRE(medium.hasMedium());
    CHECK_FALSE(medium.attenuationDraws());
    const Coefficients c{coefficientsOf(nomax, unitScale)};
    REQUIRE(c.extinction().maxComponent() > 0.0f);
    Sampler sampler{};
    sampler.startPixelSample(0, 0);
    Color beta{1.0f};
    medium.attenuate(sampler, DISTANCE, beta);
    checkClose(beta, beerLambert(c.extinction(), DISTANCE), 1e-4f,
               "the closed form on the captured snapshot");
  }
}

TEST_CASE("Medium: additive overlap") {
  Fixture fixture{};
  const Color &wavelengths{fixture.wavelengths};
  const float unitScale{fixture.state->metersPerSceneUnit};
  const float3 org{};
  const float3 dir{1.0f, 0.0f, 0.0f};
  constexpr float DISTANCE{1.5f};
  Medium medium{};
  REQUIRE(fixture.isProvablyHomogeneous("fog_a") == true);
  MediumStack &a{fixture.entry("fog_a")};
  MediumStack &b{fixture.entry("fog_b", &a)};
  const Coefficients ac{coefficientsOf(a, unitScale)};
  const Coefficients bc{coefficientsOf(b, unitScale)};
  const Color mu{ac.extinction() + bc.extinction()};
  const Color overlapTr{beerLambert(mu, DISTANCE)};

  {
    INFO("two additive media attenuate as the single medium of their sum");
    REQUIRE(fixture.isProvablyHomogeneous("fog_sum"));
    MediumStack &sum{fixture.entry("fog_sum")};
    Sampler s1{}, s2{};
    s1.startPixelSample(0, 0);
    s2.startPixelSample(0, 0);
    Color betaOverlap{1.0f}, betaSum{1.0f};
    medium.reset(&b, wavelengths, PathTime(0.0f), smdl::FRAUNHOFER_D_LINE, org,
                 dir);
    medium.attenuate(s1, DISTANCE, betaOverlap);
    medium.reset(&sum, wavelengths, PathTime(0.0f), smdl::FRAUNHOFER_D_LINE,
                 org, dir);
    medium.attenuate(s2, DISTANCE, betaSum);
    checkClose(betaOverlap, betaSum, 1e-6f, "the overlap against the sum");
    checkClose(betaOverlap, overlapTr, 1e-4f, "the closed form of the sum");
  }

  {
    INFO("a non-additive entry replaces everything below it");
    // The run of additive entries from the top plus the first
    // non-additive one, and nothing under that.
    MediumStack &deep{fixture.entry("fog_b")};
    MediumStack &blocking{fixture.entry("fog", &deep)};
    MediumStack &over1{fixture.entry("fog_a", &blocking)};
    MediumStack &over2{fixture.entry("fog_b", &over1)};
    // The same three entries with nothing underneath them.
    MediumStack &plain1{fixture.entry("fog")};
    MediumStack &plain2{fixture.entry("fog_a", &plain1)};
    MediumStack &plain3{fixture.entry("fog_b", &plain2)};
    Sampler s1{}, s2{};
    s1.startPixelSample(0, 0);
    s2.startPixelSample(0, 0);
    Color betaDeep{1.0f}, betaPlain{1.0f};
    medium.reset(&over2, wavelengths, PathTime(0.0f), smdl::FRAUNHOFER_D_LINE,
                 org, dir);
    medium.attenuate(s1, DISTANCE, betaDeep);
    medium.reset(&plain3, wavelengths, PathTime(0.0f), smdl::FRAUNHOFER_D_LINE,
                 org, dir);
    medium.attenuate(s2, DISTANCE, betaPlain);
    checkClose(betaDeep, betaPlain, 1e-6f,
               "the entry below the non-additive one contributes nothing");
  }

  {
    INFO("the phase pick carries each component's share of the scattering");
    const Means means{
        sampleMeans(medium, &b, wavelengths, org, dir, DISTANCE, a.material)};
    Color expectFirst{}, expectAll{};
    for (size_t i = 0; i < mu.size(); i++) {
      expectFirst[i] = ac.sigmaS[i] * (1.0f - overlapTr[i]) / mu[i];
      expectAll[i] =
          (ac.sigmaS[i] + bc.sigmaS[i]) * (1.0f - overlapTr[i]) / mu[i];
    }
    CHECK(means.numScattered > 0);
    checkClose(means.survived, overlapTr, MEAN_TOLERANCE,
               "the survival weight");
    checkClose(means.scattered, expectAll, MEAN_TOLERANCE,
               "the whole scattering weight");
    checkClose(means.pickedFirst, expectFirst, MEAN_TOLERANCE,
               "the share the first component's phase function carries");
  }

  {
    INFO("two tracked components divide their collisions by their spectra");
    REQUIRE_FALSE(fixture.isProvablyHomogeneous("ramp_add2"));
    MediumStack &first{fixture.entry("ramp_add")};
    MediumStack &second{fixture.entry("ramp_add2", &first)};
    constexpr float SPAN{2.0f};
    const Color baseFirst{Color(first.material->getMaxScatteringCoefficient()) *
                          unitScale};
    const Color baseSecond{
        Color(second.material->getMaxScatteringCoefficient()) * unitScale};
    const Color baseTotal{baseFirst + baseSecond};
    // The two spectra must slope opposite ways, or the per-band share
    // below is the same number in every band and proves nothing.
    REQUIRE(baseFirst[0] / baseTotal[0] !=
            doctest::Approx(baseFirst[GRID.size() - 1] /
                            baseTotal[GRID.size() - 1]));
    const Color trackedTr{beerLambert(baseTotal, 1.0f)};
    Color expectFirst{}, expectAll{};
    for (size_t i = 0; i < trackedTr.size(); i++) {
      expectAll[i] = 1.0f - trackedTr[i];
      expectFirst[i] = baseFirst[i] / baseTotal[i] * expectAll[i];
    }
    const Means means{sampleMeans(medium, &second, wavelengths, org, dir, SPAN,
                                  first.material)};
    CHECK(means.numScattered > 0);
    checkClose(means.survived, trackedTr, MEAN_TOLERANCE,
               "the survival weight");
    checkClose(means.scattered, expectAll, MEAN_TOLERANCE,
               "the whole scattering weight");
    checkClose(means.pickedFirst, expectFirst, MEAN_TOLERANCE,
               "the first component's per-band share");
  }

  {
    INFO("a homogeneous component tracked alongside a isHeterogeneous one");
    MediumStack &hom{fixture.entry("fog_a")};
    REQUIRE_FALSE(fixture.isProvablyHomogeneous("ramp_add"));
    MediumStack &het{fixture.entry("ramp_add", &hom)};
    constexpr float SPAN{2.0f};
    // The ramp contributes an optical depth of `base` over [0, 2]; the
    // homogeneous component contributes its extinction over the span.
    const Color base{Color(het.material->getMaxScatteringCoefficient()) *
                     unitScale};
    Color expect{};
    for (size_t i = 0; i < expect.size(); i++)
      expect[i] = std::exp(-double(base[i] + ac.extinction()[i] * SPAN));
    medium.reset(&het, wavelengths, PathTime(0.0f), smdl::FRAUNHOFER_D_LINE,
                 org, dir);
    CHECK(medium.attenuationDraws());
    checkClose(attenuateMean(medium, &het, wavelengths, org, dir, SPAN), expect,
               MEAN_TOLERANCE, "the mean transmittance of the overlap");
  }
}

namespace {
// Are the two the same floating-point numbers band for band? What a
// resolution kept from one path to the next must reproduce: not close,
// identical.
[[nodiscard]] bool isIdentical(const Color &a, const Color &b) {
  if (a.size() != b.size()) return false;
  for (size_t i = 0; i < a.size(); i++)
    if (a[i] != b[i]) return false;
  return true;
}
} // namespace

TEST_CASE("Medium: the resolution carries across paths") {
  Fixture fixture{};
  const Color &wavelengths{fixture.wavelengths};
  const float3 org{0.0f, 0.0f, 0.0f};
  const float3 dir{1.0f, 0.0f, 0.0f};
  constexpr float DISTANCE{2.0f};
  Medium medium{};

  {
    INFO("the same homogeneous medium on a new stack");
    // Two stacks over one material, the second seen after `beginPath()`
    // as the next path would see it: the answers must be the same
    // floating-point numbers, and every scattering event must name the
    // new stack's evaluation, never the old one's.
    REQUIRE(fixture.isProvablyHomogeneous("fog"));
    MediumStack &a{fixture.entry("fog")};
    MediumStack &b{fixture.entry("fog")};
    const Means first{
        sampleMeans(medium, &a, wavelengths, org, dir, DISTANCE, a.material)};
    medium.beginPath();
    const Means second{
        sampleMeans(medium, &b, wavelengths, org, dir, DISTANCE, b.material)};
    REQUIRE(first.numScattered > 0);
    CHECK(first.numScattered == second.numScattered);
    CHECK(isIdentical(first.survived, second.survived));
    CHECK(isIdentical(first.scattered, second.scattered));
    CHECK(isIdentical(first.pickedFirst, first.scattered));
    CHECK(isIdentical(second.pickedFirst, second.scattered));
  }
  {
    INFO("the same tracked medium on a new stack");
    REQUIRE_FALSE(fixture.isProvablyHomogeneous("ramp_vol"));
    MediumStack &a{fixture.entry("ramp_vol")};
    MediumStack &b{fixture.entry("ramp_vol")};
    const Means first{
        sampleMeans(medium, &a, wavelengths, org, dir, DISTANCE, a.material)};
    medium.beginPath();
    const Means second{
        sampleMeans(medium, &b, wavelengths, org, dir, DISTANCE, b.material)};
    CHECK(medium.attenuationDraws());
    REQUIRE(first.numScattered > 0);
    CHECK(first.numScattered == second.numScattered);
    CHECK(isIdentical(first.survived, second.survived));
    CHECK(isIdentical(first.scattered, second.scattered));
    CHECK(isIdentical(second.pickedFirst, second.scattered));
  }
  {
    INFO("a snapshot captured elsewhere is another medium");
    // The fallback medium is its captured coefficients, which this
    // material varies along X: an evaluation captured at another point
    // must resolve to its own snapshot, not be taken for the last one.
    REQUIRE_FALSE(fixture.isProvablyHomogeneous("ramp_nomax"));
    const float unitScale{fixture.state->metersPerSceneUnit};
    MediumStack &a{fixture.entry("ramp_nomax")};
    fixture.state->position = float3(2.0f, 0.0f, 0.0f);
    MediumStack &b{fixture.entry("ramp_nomax")};
    fixture.state->position = float3(0.0f, 0.0f, 0.0f);
    const Color first{
        attenuateMean(medium, &a, wavelengths, org, dir, DISTANCE)};
    medium.beginPath();
    const Color second{
        attenuateMean(medium, &b, wavelengths, org, dir, DISTANCE)};
    CHECK_FALSE(isIdentical(first, second));
    checkClose(second,
               beerLambert(coefficientsOf(b, unitScale).extinction(), DISTANCE),
               1e-3f, "the closed form on the new snapshot");
  }
  {
    INFO("a different medium at a reused address");
    // The path allocator hands the same addresses out again, so a stack
    // at the address of the last path's must be seen for what it holds.
    std::optional<smdl::JIT::Material> material{};
    std::optional<MediumStack> slot{};
    material.emplace(*fixture.state, fixture.compiler.findMaterial("fog"));
    slot.emplace(MediumStack{nullptr, &*material, nullptr});
    const Color fog{
        attenuateMean(medium, &*slot, wavelengths, org, dir, DISTANCE)};
    material.emplace(*fixture.state, fixture.compiler.findMaterial("fog_a"));
    slot.emplace(MediumStack{nullptr, &*material, nullptr});
    medium.beginPath();
    const Color fogA{
        attenuateMean(medium, &*slot, wavelengths, org, dir, DISTANCE)};
    Medium fresh{};
    CHECK_FALSE(isIdentical(fog, fogA));
    CHECK(isIdentical(
        fogA, attenuateMean(fresh, &*slot, wavelengths, org, dir, DISTANCE)));
  }
}

TEST_CASE("Medium: the phase function at the collision") {
  // A phase function the compiler cannot prove point-independent is
  // evaluated where the collision is, not where the path entered. Over
  // `bias_sign`, backward for x < 0 and forward for x > 0, every
  // collision scatters with the bias of its own side whichever side the
  // path entered from, while the constant coefficients still take the
  // closed forms.
  Fixture fixture{};
  const Color &wavelengths{fixture.wavelengths};
  const float3 org{-1.0f, 0.0f, 0.0f};
  const float3 dir{1.0f, 0.0f, 0.0f};
  constexpr float DISTANCE{2.0f};
  const smdl::JIT::MaterialDef *materialDef{
      fixture.compiler.findMaterial("bias_sign")};
  REQUIRE(materialDef);
  REQUIRE(materialDef->hasHomogeneousCoefficients());
  REQUIRE_FALSE(materialDef->hasHomogeneousVDF());
  // Two instances entered from either side of x = 0, so that each
  // carries the bias of its own side as its snapshot.
  fixture.state->position = float3(-2.0f, 0.0f, 0.0f);
  MediumStack &fromBackward{fixture.entry("bias_sign")};
  fixture.state->position = float3(+2.0f, 0.0f, 0.0f);
  MediumStack &fromForward{fixture.entry("bias_sign")};
  fixture.state->position = float3(0.0f, 0.0f, 0.0f);
  // `wo` points back along the segment, so continuing straight on is
  // `dir`, the direction a positive bias favors.
  const float3 wo{-dir};
  const auto isForward{[&](const Scatterer &scatterer) {
    return scatterer.volumeScatterEvaluate(wo, dir) >
           scatterer.volumeScatterEvaluate(wo, -dir);
  }};
  CHECK_FALSE(isForward(Scatterer(fromBackward.material->getVDF())));
  CHECK(isForward(Scatterer(fromForward.material->getVDF())));
  Medium medium{};
  smdl::BumpPtrAllocator allocator{};
  for (const MediumStack *entry : {&fromBackward, &fromForward}) {
    const char *side{entry == &fromBackward ? "entered from the backward side"
                                            : "entered from the forward side"};
    INFO(side);
    medium.beginPath();
    Sampler sampler{};
    int numBackward{}, numForward{}, numWrongSide{}, numFromInstance{};
    for (int i = 0; i < NUM_SAMPLES; i++) {
      sampler.startPixelSample(uint32_t(i), 0);
      allocator.reset();
      medium.reset(entry, wavelengths, PathTime(0.0f), smdl::FRAUNHOFER_D_LINE,
                   org, dir);
      Color beta{1.0f};
      Color emitted{};
      float t{};
      if (!medium.sampleDistance(sampler, DISTANCE, t, beta, emitted)) continue;
      const float x{org.x + t * dir.x};
      const Scatterer scatterer{medium.scatterer(allocator)};
      if (isForward(scatterer) != (x >= 0.0f)) numWrongSide++;
      // The collision's VDF is its own, never the instance's.
      if (scatterer.vdf().ptr == entry->material->getVDF().ptr)
        numFromInstance++;
      if (x < 0.0f)
        numBackward++;
      else
        numForward++;
    }
    // Closed-form coefficients: nothing is tracked.
    CHECK_FALSE(medium.attenuationDraws());
    // Both sides were reached, or the side check proved nothing.
    CHECK(numBackward > 0);
    CHECK(numForward > 0);
    CHECK(numWrongSide == 0);
    CHECK(numFromInstance == 0);
  }
}
