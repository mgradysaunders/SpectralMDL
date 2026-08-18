/// \file
#pragma once

#include <algorithm>
#include <cstdint>
#include <limits>

#include "smdl/Support/VectorMath.h"

namespace smdl {

/// \addtogroup support
/// \{

/// The pseudo-random number generator used for stochastic evaluation.
///
/// This is bit-exact with `rng_t` in the builtin `::extras::rng` SMDL
/// module and with the draw primitives that advance `State::rng` during
/// material evaluation (a unit test pins the parity). The implementation
/// is the PCG32 generator by Melissa O'Neill, being the 32-bit output
/// variant of the permuted congruential generator family, bit-exact with
/// the reference implementation. The specific generator is an
/// implementation detail: rely only on the draws being uniform and
/// deterministic for a fixed initial state.
///
struct RNG final {
public:
  /// The multiplier of the linear congruential generator.
  static constexpr uint64_t MULTIPLIER = 6364136223846793005ULL;

  /// The increment selecting the default stream.
  static constexpr uint64_t DEFAULT_INCREMENT = 1442695040888963407ULL;

  /// Construct with unseeded state: deterministic, on the default stream.
  constexpr RNG() noexcept = default;

  /// Construct by seeding on the default stream.
  explicit constexpr RNG(uint64_t seed) noexcept {
    state = seed + increment;
    state = state * MULTIPLIER + increment;
  }

  /// Construct by seeding on the stream selected by `stream`, such that
  /// distinct stream selectors yield statistically independent output
  /// sequences.
  constexpr RNG(uint64_t seed, uint64_t stream) noexcept
      : increment((stream << 1) | 1) {
    state = seed + increment;
    state = state * MULTIPLIER + increment;
  }

  /// Generates the next 32-bit integer.
  constexpr uint32_t generate() noexcept {
    const uint64_t state0{state};
    state = state0 * MULTIPLIER + increment;
    const auto value{uint32_t(((state0 >> 18) ^ state0) >> 27)};
    const auto rot{uint32_t(state0 >> 59)};
    return (value >> rot) | (value << ((32 - rot) % 32));
  }

  /// Generates a uniform integer in `[0, bound)` by rejection sampling.
  constexpr int generateInt(int bound) noexcept {
    if (bound > 1) {
      const auto bound32{uint32_t(bound)};
      const uint32_t xMin{(uint32_t(0) - bound32) % bound32};
      while (true) {
        const uint32_t x{generate()};
        if (x >= xMin)
          return int(x % bound32);
      }
    }
    return 0;
  }

  /// Generates a uniform `float` in `[0, 1)`.
  constexpr float generateFloat() noexcept {
    return std::min(float(double(generate()) / 4294967296.0),
                    1.0f - std::numeric_limits<float>::epsilon() / 2);
  }

  /// Generates a uniform `float2` in `[0, 1)^2`.
  constexpr float2 generateFloat2() noexcept {
    return {generateFloat(), generateFloat()};
  }

  /// Generates a uniform `float3` in `[0, 1)^3`.
  constexpr float3 generateFloat3() noexcept {
    return {generateFloat(), generateFloat(), generateFloat()};
  }

  /// Generates a uniform `float4` in `[0, 1)^4`.
  constexpr float4 generateFloat4() noexcept {
    return {generateFloat(), generateFloat(), generateFloat(),
            generateFloat()};
  }

  /// Advances the generator by `n` steps in logarithmic time, as if
  /// calling `generate()` `n` times.
  constexpr void discard(uint64_t n) noexcept {
    // Fast power by squaring.
    uint64_t aTotal{1};
    uint64_t bTotal{0};
    uint64_t a{MULTIPLIER};
    uint64_t b{increment};
    while (n != 0) {
      if ((n & 1) != 0) {
        aTotal = aTotal * a;
        bTotal = bTotal * a + b;
      }
      b *= a + 1;
      a *= a;
      n >>= 1;
    }
    state = state * aTotal + bTotal;
  }

  constexpr bool operator==(const RNG &other) const noexcept {
    return state == other.state && increment == other.increment;
  }

  constexpr bool operator!=(const RNG &other) const noexcept {
    return !(*this == other);
  }

public:
  /// The state of the linear congruential generator.
  uint64_t state = 0;

  /// The stream-selecting increment, which must be odd.
  uint64_t increment = DEFAULT_INCREMENT;
};

/// \}

} // namespace smdl
