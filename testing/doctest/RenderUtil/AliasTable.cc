#include "Fixtures.h"

#include <cmath>
#include <random>
#include <vector>

#include "smdl/RenderUtil/AliasTable.h"

// The alias table: that a draw lands on an index exactly as often as the
// index's share of the weights, and that the degenerate tables a caller
// can hand it are empty rather than wrong. The lopsided case is the one
// that matters, since a table is only worth building where the weights
// are uneven.

namespace {

// A purely relative tolerance; `doctest::Approx` alone never tightens
// below its epsilon in absolute terms.
[[nodiscard]] doctest::Approx within(double value, double epsilon) {
  return doctest::Approx(value).epsilon(epsilon).scale(0.0);
}

// The share of `count` draws that lands on each index, sweeping the unit
// interval so the result is the distribution rather than a sample of it.
//
// The sweep resolves each entry's threshold to `count / size` steps, and
// a small index is reached as the leftovers of many entries, so its
// share carries that step divided by its share of one entry. Hence the
// looser tolerances on the small indexes below: the sweep is what is
// coarse there, not the table.
[[nodiscard]] std::vector<double> drawShares(const smdl::AliasTable &table,
                                             int count) {
  std::vector<double> shares(size_t(table.size()), 0.0);
  for (int i = 0; i < count; i++) {
    const float xi{(float(i) + 0.5f) / float(count)};
    const int index{table.indexDraw(xi)};
    REQUIRE(index >= 0);
    REQUIRE(index < table.size());
    shares[size_t(index)] += 1.0 / double(count);
  }
  return shares;
}

// The weights normalized to shares, to compare a draw against.
[[nodiscard]] std::vector<double>
weightShares(smdl::Span<const float> weights) {
  double sum{};
  for (const auto &weight : weights) sum += double(weight);
  std::vector<double> shares{};
  for (const auto &weight : weights) shares.push_back(double(weight) / sum);
  return shares;
}

} // namespace

TEST_CASE("AliasTable: draws in proportion to the weights") {
  SUBCASE("With uniform weights") {
    const std::vector<float> weights(64, 1.0f);
    const smdl::AliasTable table{weights};
    REQUIRE(table.size() == 64);
    for (const auto &share : drawShares(table, 1 << 16))
      CHECK(share == within(1.0 / 64.0, 1e-3));
  }
  SUBCASE("With a mesh whose area sits in a minority of its faces") {
    std::vector<float> weights(512, 0.01f);
    for (int i = 0; i < 8; i++) weights[size_t(i)] = 10.0f;
    const smdl::AliasTable table{weights};
    const std::vector<double> want{weightShares(weights)};
    const std::vector<double> got{drawShares(table, 1 << 20)};
    for (size_t i = 0; i < want.size(); i++)
      CHECK(got[i] == within(want[i], 2e-2));
    // The eight large faces hold well over nine tenths of the weight,
    // where a draw by index would give them eight parts in five hundred
    // twelve, sixty times less.
    double largeGot{}, largeWant{};
    for (int i = 0; i < 8; i++) {
      largeGot += got[size_t(i)];
      largeWant += want[size_t(i)];
    }
    CHECK(largeGot == within(largeWant, 5e-3));
    CHECK(largeGot > 20.0 * (8.0 / 512.0));
  }
  SUBCASE("With random weights") {
    std::mt19937 rng{12345};
    std::uniform_real_distribution<float> dist{0.0f, 1.0f};
    std::vector<float> weights{};
    for (int i = 0; i < 300; i++) weights.push_back(dist(rng));
    const smdl::AliasTable table{weights};
    const std::vector<double> want{weightShares(weights)};
    const std::vector<double> got{drawShares(table, 1 << 20)};
    for (size_t i = 0; i < want.size(); i++)
      CHECK(got[i] == within(want[i], 1e-2));
  }
}

TEST_CASE("AliasTable: reaches every index with weight") {
  // A weight of zero is never drawn, and every positive weight is,
  // however much smaller it is than the rest.
  std::vector<float> weights(64, 0.0f);
  weights[3] = 1.0f;
  weights[17] = 1e-4f;
  weights[63] = 1e-4f;
  const smdl::AliasTable table{weights};
  const std::vector<double> got{drawShares(table, 1 << 20)};
  CHECK(got[3] > 0.0);
  CHECK(got[17] > 0.0);
  CHECK(got[63] > 0.0);
  for (size_t i = 0; i < got.size(); i++)
    if (i != 3 && i != 17 && i != 63) CHECK(got[i] == 0.0);
}

TEST_CASE("AliasTable: degenerate tables are empty") {
  CHECK(smdl::AliasTable{}.empty());
  CHECK(smdl::AliasTable{smdl::Span<const float>{}}.empty());
  const std::vector<float> zeros(8, 0.0f);
  CHECK(smdl::AliasTable{zeros}.empty());
  // A negative or NaN weight counts as zero rather than poisoning the
  // sum, as `Distribution1D` also holds.
  const std::vector<float> bad{-1.0f, std::nanf(""), -0.0f};
  CHECK(smdl::AliasTable{bad}.empty());
  const std::vector<float> mixed{-1.0f, 2.0f, std::nanf("")};
  const smdl::AliasTable table{mixed};
  REQUIRE(table.size() == 3);
  for (int i = 0; i < (1 << 12); i++)
    CHECK(table.indexDraw((float(i) + 0.5f) / float(1 << 12)) == 1);
}
