#include "Fixtures.h"

#include <algorithm>
#include <numeric>
#include <string>
#include <vector>

#include "Render.h"

namespace {

[[nodiscard]] std::string spellSchedule(const std::vector<size_t> &passes) {
  std::string text{"["};
  for (size_t i = 0; i < passes.size(); i++)
    text += (i > 0 ? ", " : "") + std::to_string(passes[i]);
  return text + "]";
}

// The whole schedule of a budget, against what it must be.
void checkSchedule(size_t spp, bool useGuiding, size_t trainedSpp,
                   const std::vector<size_t> &expected) {
  const std::vector<size_t> passes{
      solveSamplePasses(spp, useGuiding, trainedSpp)};
  CHECK_MESSAGE(passes == expected, spp, " spp gives ", spellSchedule(passes),
                " and not ", spellSchedule(expected));
}

// Far enough past a power of two to reach the tenth pass, and cheap
// enough to walk every budget below it.
constexpr size_t MANY_SPP{4096};

// Does `passes` spend `spp` over passes that are none of them empty?
void checkSpends(size_t spp, size_t trainedSpp,
                 const std::vector<size_t> &passes) {
  const size_t total{std::accumulate(passes.begin(), passes.end(), size_t(0))};
  const bool isWhole{total == spp &&
                     std::none_of(passes.begin(), passes.end(),
                                  [](size_t pass) { return pass == 0; })};
  CHECK_MESSAGE(isWhole, spp, " spp resuming ", trainedSpp, " spends ", total,
                " over ", spellSchedule(passes));
}

} // namespace

TEST_CASE("solveSamplePasses: what a budget is split into") {
  SUBCASE("Without guiding the budget is one pass") {
    checkSchedule(1, false, 0, {1});
    checkSchedule(8, false, 0, {8});
    checkSchedule(1000, false, 0, {1000});
    // What trained a tree cannot matter where there is no tree.
    checkSchedule(1000, false, 512, {1000});
  }
  SUBCASE("An empty budget is no passes at all") {
    checkSchedule(0, false, 0, {});
    checkSchedule(0, true, 0, {});
    checkSchedule(0, true, 64, {});
  }
  SUBCASE("Guiding warms up through the powers of two") {
    checkSchedule(1000, true, 0, {1, 2, 4, 8, 16, 32, 64, 128, 256, 489});
  }
  SUBCASE("The smallest budgets are exactly these") {
    checkSchedule(1, true, 0, {1});
    checkSchedule(2, true, 0, {1, 1});
    checkSchedule(3, true, 0, {1, 2});
    checkSchedule(4, true, 0, {1, 3});
    checkSchedule(5, true, 0, {1, 2, 2});
    checkSchedule(6, true, 0, {1, 2, 3});
    checkSchedule(7, true, 0, {1, 2, 4});
    checkSchedule(8, true, 0, {1, 2, 5});
  }
  SUBCASE("Every schedule spends the budget exactly") {
    for (size_t spp = 0; spp <= MANY_SPP; spp++)
      checkSpends(spp, 0, solveSamplePasses(spp, true, 0));
  }
  SUBCASE("No pass is smaller than the one before it") {
    for (size_t spp = 1; spp <= MANY_SPP; spp++) {
      const std::vector<size_t> passes{solveSamplePasses(spp, true, 0)};
      CHECK_MESSAGE(std::is_sorted(passes.begin(), passes.end()), spp,
                    " spp gives ", spellSchedule(passes));
    }
  }
  SUBCASE("The final pass is more than a third of the budget") {
    // More than a third and not half: the warmup costs 2^k - 1 samples
    // whatever the budget is, so a budget just past one of those spends
    // most of itself warming up. 3 * 2^k - 1 is where a third is
    // approached, 5 being the smallest.
    for (size_t spp = 1; spp <= MANY_SPP; spp++) {
      const std::vector<size_t> passes{solveSamplePasses(spp, true, 0)};
      CHECK_MESSAGE(3 * passes.back() > spp, spp, " spp ends on ",
                    passes.back(), " of ", spellSchedule(passes));
    }
    CHECK(solveSamplePasses(5, true, 0).back() * 2 < 5);
    CHECK(solveSamplePasses(3 * 1024 - 1, true, 0).back() == 1024);
  }
}

TEST_CASE("solveSamplePasses: resuming a tree skips the warmup it outgrew") {
  SUBCASE(
      "The first pass is the largest power of two the tree was trained to") {
    checkSchedule(64, true, 0, {1, 2, 4, 8, 16, 33});
    checkSchedule(64, true, 1, {1, 2, 4, 8, 16, 33});
    checkSchedule(64, true, 2, {2, 4, 8, 16, 34});
    checkSchedule(64, true, 3, {2, 4, 8, 16, 34});
    checkSchedule(64, true, 4, {4, 8, 16, 36});
    checkSchedule(64, true, 7, {4, 8, 16, 36});
    checkSchedule(64, true, 8, {8, 16, 40});
    checkSchedule(64, true, 16, {16, 48});
  }
  SUBCASE("A tree trained past the budget leaves one pass") {
    checkSchedule(64, true, 64, {64});
    checkSchedule(64, true, 100, {64});
    checkSchedule(64, true, 1 << 20, {64});
  }
  SUBCASE("Resuming still spends the budget exactly") {
    // The powers of two and their neighbors, which is where the first
    // pass moves.
    for (const size_t trainedSpp : {0u, 1u, 2u, 3u, 4u, 5u, 7u, 8u, 9u, 15u,
                                    16u, 17u, 31u, 32u, 63u, 64u, 65u})
      for (size_t spp = 1; spp <= 256; spp++)
        checkSpends(spp, trainedSpp, solveSamplePasses(spp, true, trainedSpp));
  }
}
