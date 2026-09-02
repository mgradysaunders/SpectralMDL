#include "doctest.h"

#include <array>

#include "smdl/Support/RNG.h"

TEST_CASE("RNG") {
  SUBCASE("seeding") {
    // The seeding sequence must match the PCG32 reference: starting from
    // zero, advance, add the seed, advance, with the stream selector
    // mapped to an odd increment. The language test 'rng.smdl' pins the
    // same states for the builtin mirror.
    auto rng{smdl::RNG(42, 54)};
    CHECK(rng.increment == 109ULL);
    CHECK(rng.state == 1753877967969059832ULL);
    CHECK(smdl::RNG(42).increment == smdl::RNG().increment);
    CHECK(smdl::RNG().increment == smdl::RNG::DEFAULT_INCREMENT);
  }
  SUBCASE("golden vectors") {
    // The published outputs of O'Neill's pcg32-demo for seed 42,
    // stream 54.
    auto rng{smdl::RNG(42, 54)};
    CHECK(rng.generate() == 0xA15C02B7U);
    CHECK(rng.generate() == 0x7B47F409U);
    CHECK(rng.generate() == 0xBA1D3330U);
    CHECK(rng.generate() == 0x83D2F293U);
    CHECK(rng.generate() == 0xBFA4784BU);
    CHECK(rng.generate() == 0xCBED606EU);
  }
  SUBCASE("discard") {
    auto rng0{smdl::RNG(7, 11)};
    auto rng1{rng0};
    rng0.discard(1000);
    for (int i = 0; i < 1000; i++) (void)rng1.generate();
    CHECK(rng0 == rng1);
    rng1.discard(0);
    CHECK(rng0 == rng1);
  }
  SUBCASE("generateInt") {
    auto rng{smdl::RNG(123)};
    std::array<int, 7> hits{};
    bool inBounds{true};
    for (int i = 0; i < 10000; i++) {
      const int x{rng.generateInt(7)};
      inBounds &= 0 <= x && x < 7;
      hits[x % 7]++;
    }
    CHECK(inBounds);
    for (int count : hits) CHECK(count > 0);
    CHECK(rng.generateInt(1) == 0);
    CHECK(rng.generateInt(0) == 0);
  }
  SUBCASE("generateFloat range") {
    auto rng{smdl::RNG(5)};
    bool inRange{true};
    for (int i = 0; i < 10000; i++) {
      const float x{rng.generateFloat()};
      inRange &= 0.0f <= x && x < 1.0f;
    }
    CHECK(inRange);
  }
  SUBCASE("streams are distinct") {
    auto rng0{smdl::RNG(42, 1)};
    auto rng1{smdl::RNG(42, 2)};
    bool anyDiff{false};
    for (int i = 0; i < 16; i++) anyDiff |= rng0.generate() != rng1.generate();
    CHECK(anyDiff);
  }
}
