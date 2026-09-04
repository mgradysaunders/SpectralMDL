#include "doctest.h"

#include <random>
#include <vector>

#include "Render/LightTree.h"

// The light tree over synthetic bounds, with no scene: what a selection
// structure owes the estimator. The probabilities over every light sum
// to one at any receiver, a draw reports the probability an arrival
// recomputes, the draws follow the probabilities, and the build is the
// same tree every time.

namespace {

[[nodiscard]] std::vector<LightBounds> randomLights(int count, uint32_t seed) {
  std::mt19937 rng{seed};
  std::uniform_real_distribution<float> position{-10.0f, 10.0f};
  std::uniform_real_distribution<float> extent{0.0f, 0.5f};
  std::uniform_real_distribution<float> power{0.1f, 10.0f};
  auto lights{std::vector<LightBounds>()};
  for (int i = 0; i < count; i++) {
    const float3 center{position(rng), position(rng), position(rng)};
    const float3 half{extent(rng), extent(rng), extent(rng)};
    auto &light{lights.emplace_back()};
    light.box = BoundBox3(center - half, center + half);
    light.phi = power(rng);
  }
  return lights;
}

[[nodiscard]] float3 randomPoint(std::mt19937 &rng) {
  std::uniform_real_distribution<float> position{-12.0f, 12.0f};
  return float3(position(rng), position(rng), position(rng));
}

// The invariants every tree must satisfy at `numPoints` receivers over
// `numLights` lights: the probabilities sum to one, and each draw
// reports the probability `pmf()` recomputes for the light it picked.
void checkEstimatorContract(const LightTree &tree, int numLights, int numPoints,
                            uint32_t seed) {
  std::mt19937 rng{seed};
  for (int k = 0; k < numPoints; k++) {
    const float3 point{randomPoint(rng)};
    CAPTURE(k);
    double sum{};
    for (int i = 0; i < numLights; i++) sum += tree.pmf(i, point);
    CHECK(sum == doctest::Approx(1.0).epsilon(1e-4));
    for (int j = 0; j < 64; j++) {
      const float xi{(float(j) + 0.5f) / 64.0f};
      float pmf{};
      const int light{tree.sample(point, xi, pmf)};
      CAPTURE(j);
      REQUIRE(light >= 0);
      REQUIRE(light < numLights);
      CHECK(pmf > 0.0f);
      CHECK(pmf == doctest::Approx(tree.pmf(light, point)).epsilon(1e-6));
    }
  }
}

// Stratified draws at one receiver against the probabilities: every
// light's share of the unit interval is one contiguous piece, so the
// counts match to within two draws.
void checkDrawsFollowProbabilities(const LightTree &tree, int numLights,
                                   const float3 &point) {
  constexpr int NUM_DRAWS{1 << 16};
  auto counts{std::vector<int>(size_t(numLights))};
  for (int j = 0; j < NUM_DRAWS; j++) {
    float pmf{};
    const int light{
        tree.sample(point, (float(j) + 0.5f) / float(NUM_DRAWS), pmf)};
    counts[size_t(light)]++;
  }
  for (int i = 0; i < numLights; i++) {
    CAPTURE(i);
    const double frequency{double(counts[size_t(i)]) / NUM_DRAWS};
    CHECK(std::abs(frequency - double(tree.pmf(i, point))) <=
          2.0 / NUM_DRAWS + 1e-5);
  }
}

} // namespace

TEST_CASE("LightTree: the estimator contract over random lights") {
  const auto lights{randomLights(64, 1)};
  const LightTree tree{lights};
  CHECK(!tree.empty());
  CHECK(tree.nodeCount() == 2 * 64 - 1);
  CHECK(tree.depth() >= 6);
  checkEstimatorContract(tree, 64, 32, 2);
  std::mt19937 rng{3};
  checkDrawsFollowProbabilities(tree, 64, randomPoint(rng));
  checkDrawsFollowProbabilities(tree, 64, float3(0.0f));
}

TEST_CASE("LightTree: a near light draws over a far one by the inverse "
          "square") {
  auto lights{std::vector<LightBounds>(2)};
  lights[0].box = BoundBox3(float3(1.0f, 0.0f, 0.0f), float3(1.0f, 0.0f, 0.0f));
  lights[0].phi = 1.0f;
  lights[1].box =
      BoundBox3(float3(100.0f, 0.0f, 0.0f), float3(100.0f, 0.0f, 0.0f));
  lights[1].phi = 1.0f;
  const LightTree tree{lights};
  const float3 receiver{0.0f};
  CHECK(tree.pmf(0, receiver) == doctest::Approx(10000.0 / 10001.0));
  CHECK(tree.pmf(1, receiver) == doctest::Approx(1.0 / 10001.0));
  // Between them, equidistant, they draw alike; ten times the power
  // draws ten times as often.
  const float3 middle{50.5f, 0.0f, 0.0f};
  CHECK(tree.pmf(0, middle) == doctest::Approx(0.5));
  lights[1].phi = 10.0f;
  const LightTree heavier{lights};
  CHECK(heavier.pmf(1, middle) == doctest::Approx(10.0 / 11.0));
}

TEST_CASE("LightTree: a light without weight is never drawn") {
  auto lights{randomLights(3, 4)};
  lights[1].phi = 0.0f;
  const LightTree tree{lights};
  CHECK(tree.nodeCount() == 3);
  std::mt19937 rng{5};
  for (int k = 0; k < 8; k++) {
    const float3 point{randomPoint(rng)};
    CHECK(tree.pmf(1, point) == 0.0f);
    CHECK(tree.pmf(0, point) + tree.pmf(2, point) ==
          doctest::Approx(1.0).epsilon(1e-6));
    for (int j = 0; j < 32; j++) {
      float pmf{};
      CHECK(tree.sample(point, (float(j) + 0.5f) / 32.0f, pmf) != 1);
    }
  }
  // Out of range asks are zero, not crashes.
  CHECK(tree.pmf(-1, float3(0.0f)) == 0.0f);
  CHECK(tree.pmf(3, float3(0.0f)) == 0.0f);
}

TEST_CASE("LightTree: the build is deterministic") {
  const auto lights{randomLights(200, 6)};
  const LightTree first{lights};
  const LightTree second{lights};
  CHECK(first.nodeCount() == second.nodeCount());
  CHECK(first.depth() == second.depth());
  std::mt19937 rng{7};
  for (int k = 0; k < 16; k++) {
    const float3 point{randomPoint(rng)};
    for (int i = 0; i < 200; i++)
      CHECK(first.pmf(i, point) == second.pmf(i, point));
  }
}

TEST_CASE("LightTree: a depth cap closes the rest into a leaf drawn by "
          "weight") {
  const auto lights{randomLights(40, 8)};
  const LightTree tree{lights, 3};
  CHECK(tree.depth() == 3);
  CHECK(tree.nodeCount() <= 15);
  checkEstimatorContract(tree, 40, 16, 9);
  std::mt19937 rng{10};
  checkDrawsFollowProbabilities(tree, 40, randomPoint(rng));
  // Everything in one leaf is the flat distribution.
  const LightTree flat{lights, 0};
  CHECK(flat.nodeCount() == 1);
  double total{};
  for (const auto &light : lights) total += light.phi;
  for (int i = 0; i < 40; i++)
    CHECK(flat.pmf(i, float3(0.0f)) ==
          doctest::Approx(lights[size_t(i)].phi / total).epsilon(1e-5));
}

TEST_CASE("LightTree: coincident lights and a receiver inside a cluster") {
  // Forty lights at one point: no split improves on any other, so the
  // build halves by index and the tree is balanced; every draw is by
  // weight alone, from anywhere, the receiver at the lights included.
  auto lights{randomLights(40, 11)};
  for (auto &light : lights)
    light.box = BoundBox3(float3(1.0f, 2.0f, 3.0f), float3(1.0f, 2.0f, 3.0f));
  const LightTree tree{lights};
  CHECK(tree.depth() == 6);
  double total{};
  for (const auto &light : lights) total += light.phi;
  for (const float3 &point : {float3(1.0f, 2.0f, 3.0f), float3(0.0f)})
    for (int i = 0; i < 40; i++)
      CHECK(tree.pmf(i, point) ==
            doctest::Approx(lights[size_t(i)].phi / total).epsilon(1e-4));
  checkEstimatorContract(tree, 40, 4, 12);
}

TEST_CASE("LightTree: nothing to draw") {
  const LightTree empty{};
  CHECK(empty.empty());
  float pmf{1.0f};
  (void)empty.sample(float3(0.0f), 0.5f, pmf);
  CHECK(pmf == 0.0f);
  CHECK(empty.pmf(0, float3(0.0f)) == 0.0f);
  auto lights{randomLights(4, 13)};
  for (auto &light : lights) light.phi = 0.0f;
  const LightTree dark{lights};
  CHECK(dark.empty());
  CHECK(dark.pmf(2, float3(0.0f)) == 0.0f);
}
