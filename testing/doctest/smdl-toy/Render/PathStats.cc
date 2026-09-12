#include "RenderFixtures.h"

#include <cfloat>
#include <cmath>
#include <limits>
#include <string>
#include <vector>

#include "llvm/Support/Error.h"
#include "llvm/Support/JSON.h"
#include "llvm/Support/raw_ostream.h"

#include "smdl/Support/Strings.h"

#include "Color.h"
#include "Render/PathStats.h"
#include "Render/PathTracing.h"

// The tally is checked against brute force over a hand-built set of
// contributions at every bin edge and gate, because the removed-energy
// identity is what the report's numbers stand on.

namespace {

[[nodiscard]] Color color4(float a, float b, float c, float d) {
  const float values[]{a, b, c, d};
  return Color(smdl::Span<const float>(values, 4));
}

struct Entry final {
  uint64_t bounces{};
  Color color{};
};

// Twenty contributions over four bounce counts, spanning the decades,
// some landing exactly on an edge.
[[nodiscard]] std::vector<Entry> makeEntries() {
  return {
      {0, color4(30.0f, 10.0f, 5.0f, 1.0f)},
      {0, color4(1.0f, 1.0f, 1.0f, 1.0f)},
      {0, color4(0.5f, 0.25f, 0.125f, 0.0f)},
      {0, color4(2.0e5f, 1.0e5f, 3.0e4f, 0.0f)},
      {1, color4(5.0f, 4.0f, 3.0f, 2.0f)},
      {1, color4(0.01f, 0.02f, 0.03f, 0.04f)},
      {1, color4(700.0f, 1.0f, 1.0f, 1.0f)},
      {1, color4(1.0e-3f, 2.0e-3f, 0.0f, 0.0f)},
      {1, color4(45.0f, 45.0f, 45.0f, 45.0f)},
      {2, color4(1.0e3f, 0.0f, 0.0f, 0.0f)},
      {2, color4(3.3e-5f, 1.0e-5f, 0.0f, 0.0f)},
      {2, color4(12.0f, 11.0f, 10.0f, 9.0f)},
      {2, color4(0.2f, 0.3f, 0.5f, 0.7f)},
      {2, color4(2.5e4f, 1.0e4f, 5.0e3f, 2.0e3f)},
      {3, color4(8.0e6f, 0.0f, 0.0f, 0.0f)},
      {3, color4(0.9f, 0.9f, 0.9f, 0.9f)},
      {3, color4(31.0f, 0.0f, 0.0f, 0.0f)},
      {3, color4(1.0e-8f, 5.0e-9f, 0.0f, 0.0f)},
      {3, color4(60.0f, 30.0f, 15.0f, 7.5f)},
      {3, color4(0.05f, 0.05f, 0.05f, 0.05f)},
  };
}

[[nodiscard]] PathStats tallyOf(const std::vector<Entry> &entries) {
  PathStats stats{};
  for (const auto &entry : entries)
    stats.recordContribution(entry.bounces, entry.color);
  return stats;
}

// What a clamp at `bound` removes from the entries of at least
// `minBounces` bounces, by brute force.
[[nodiscard]] double bruteForceRemoved(const std::vector<Entry> &entries,
                                       double bound, uint64_t minBounces) {
  double removed{};
  for (const auto &entry : entries) {
    if (entry.bounces < minBounces) continue;
    const double maxValue{entry.color.maxComponent()};
    if (!(maxValue > bound)) continue;
    removed += double(entry.color.average()) * (1.0 - bound / maxValue);
  }
  return removed;
}

[[nodiscard]] uint64_t
bruteForceCountAtOrAbove(const std::vector<Entry> &entries, double bound,
                         uint64_t minBounces) {
  uint64_t count{};
  for (const auto &entry : entries) {
    const double maxValue{entry.color.maxComponent()};
    if (entry.bounces >= minBounces && maxValue > 0.0 && maxValue >= bound)
      count++;
  }
  return count;
}

// Equal to within `tolerance` of `scale`, which is the size of the
// terms summed rather than of the result: a contribution exactly on an
// edge cancels to a rounding error of its own size, not to zero.
[[nodiscard]] bool isCloseTo(double value, double expected, double scale,
                             double tolerance = 1e-9) {
  return std::fabs(value - expected) <= tolerance * scale;
}

} // namespace

TEST_CASE("PathStats: the contribution bins") {
  const ScopedGrid grid{{400, 500, 600, 700}, false};
  SUBCASE("The edges are the 1-2-3-5 series across twenty decades") {
    CHECK(PathStats::binEdge(0) == 0.0);
    CHECK(PathStats::binEdge(1) == 1e-10);
    CHECK(PathStats::binEdge(PathStats::NUM_BINS - 1) == 1e10);
    CHECK(std::isinf(PathStats::binEdge(PathStats::NUM_BINS)));
    for (size_t b = 1; b < PathStats::NUM_BINS; b++)
      CHECK(PathStats::binEdge(b - 1) < PathStats::binEdge(b));
    CHECK(PathStats::binEdge(PathStats::binIndex(1.0)) == 1.0);
    CHECK(PathStats::binEdge(PathStats::binIndex(30.0)) == 30.0);
    CHECK(PathStats::binEdge(PathStats::binIndex(1.0e3)) == 1.0e3);
    CHECK(PathStats::binIndex(1.9999) == PathStats::binIndex(1.0));
    CHECK(PathStats::binIndex(2.0) == PathStats::binIndex(1.0) + 1);
    CHECK(PathStats::binIndex(5.0e-11) == 0);
    CHECK(PathStats::binIndex(1.0e10) == PathStats::NUM_BINS - 1);
    CHECK(PathStats::binIndex(double(FLT_MAX)) == PathStats::NUM_BINS - 1);
  }
  SUBCASE("Zero, negative, and non-finite contributions") {
    constexpr float INF{std::numeric_limits<float>::infinity()};
    constexpr float NAN_VALUE{std::numeric_limits<float>::quiet_NaN()};
    PathStats stats{};
    stats.recordContribution(2, color4(0.0f, 0.0f, 0.0f, 0.0f));
    stats.recordContribution(2, color4(-1.0f, 0.0f, 0.0f, 0.0f));
    stats.recordContribution(2, color4(INF, 1.0f, 1.0f, 1.0f));
    stats.recordContribution(2, color4(NAN_VALUE, 1.0f, 1.0f, 1.0f));
    REQUIRE(stats.rows().size() == 3);
    CHECK(stats.rows()[2].zeroCount == 2);
    CHECK(stats.rows()[2].contributionCount() == 0);
    CHECK(stats.nonFiniteCount() == 2);
    CHECK(stats.totalEnergy() == 0.0);
  }
}

TEST_CASE("PathStats: the removed-energy identity against brute force") {
  const ScopedGrid grid{{400, 500, 600, 700}, false};
  const std::vector<Entry> entries{makeEntries()};
  const PathStats stats{tallyOf(entries)};
  const double scale{stats.totalEnergy()};
  REQUIRE(scale > 0.0);
  CHECK(stats.contributionCountFrom(0) == entries.size());
  double energy{};
  for (const auto &entry : entries) energy += double(entry.color.average());
  CHECK(isCloseTo(stats.energyFrom(0), energy, scale));
  CHECK(stats.rows().size() == 4);
  CHECK(stats.rows()[3].maxContribution == doctest::Approx(8.0e6));
  for (const auto &row : stats.rows())
    for (const auto &bin : row.bins)
      CHECK(bin.sumAverageOverMax <= double(bin.count) * (1.0 + 1e-12));
  for (size_t b = 1; b < PathStats::NUM_BINS; b++) {
    const double bound{PathStats::binEdge(b)};
    for (uint64_t gate = 0; gate <= 4; gate++) {
      INFO(smdl::concat("bound ", bound, ", gate ", gate));
      CHECK(stats.countAbove(bound, gate) ==
            bruteForceCountAtOrAbove(entries, bound, gate));
      CHECK(isCloseTo(stats.energyRemovedAbove(bound, gate),
                      bruteForceRemoved(entries, bound, gate), scale));
    }
  }
}

TEST_CASE("PathStats: a tally taken with a bound already in force") {
  const ScopedGrid grid{{400, 500, 600, 700}, false};
  const std::vector<Entry> entries{makeEntries()};
  PathStats stats{tallyOf(entries)};
  const double scale{stats.totalEnergy()};
  const double bound{30.0};
  const uint64_t gate{1};
  uint64_t scaled{};
  for (const auto &entry : entries) {
    if (entry.bounces < gate) continue;
    const float maxValue{entry.color.maxComponent()};
    if (!(maxValue > bound)) continue;
    stats.recordClamp(entry.bounces, entry.color, float(bound) / maxValue);
    scaled++;
  }
  CHECK(scaled > 0);
  CHECK(stats.clampedCount() == scaled);
  // The scale is a float, so the tally of what the bound did agrees with
  // the identity to single precision.
  CHECK(isCloseTo(stats.energyClamped(), stats.energyRemovedAbove(bound, gate),
                  scale, 1e-6));
  for (size_t r = gate; r < stats.rows().size(); r++)
    CHECK(isCloseTo(stats.rows()[r].energyClamped,
                    stats.energyRemovedAbove(bound, r) -
                        stats.energyRemovedAbove(bound, r + 1),
                    scale, 1e-6));
}

TEST_CASE("PathStats: where paths end and what they carried") {
  const ScopedGrid grid{{400, 500, 600, 700}, false};
  PathStats stats{};
  for (int i = 0; i < 3; i++) stats.recordPath(0, PathEnd::ESCAPED);
  stats.recordPath(2, PathEnd::ABSORBED);
  for (int i = 0; i < 4; i++) stats.recordPath(5, PathEnd::ROULETTE);
  for (int i = 0; i < 2; i++) stats.recordPath(7, PathEnd::BOUND);
  stats.addSamples(12);
  CHECK(stats.pathCount() == 10);
  CHECK(stats.sampleCount() == 12);
  CHECK(stats.meanBounces() == doctest::Approx(3.6));
  CHECK(stats.maxBouncesReached() == 7);
  REQUIRE(stats.rows().size() == 8);
  CHECK(stats.rows()[5].ends[size_t(PathEnd::ROULETTE)] == 4);
  CHECK(stats.rows()[5].pathCount() == 4);
  uint64_t ended{};
  for (const auto &row : stats.rows()) ended += row.pathCount();
  CHECK(ended == stats.pathCount());
  // Volume emission is in the total and in no row.
  stats.recordContribution(1, color4(1.0f, 2.0f, 3.0f, 4.0f));
  stats.recordMediumEmission(color4(4.0f, 4.0f, 4.0f, 4.0f));
  CHECK(stats.mediumEmission() == doctest::Approx(4.0));
  CHECK(stats.energyFrom(0) == doctest::Approx(2.5));
  CHECK(stats.totalEnergy() == doctest::Approx(6.5));
  const PathStats empty{};
  CHECK(empty.meanBounces() == 0.0);
  CHECK(empty.maxBouncesReached() == 0);
  CHECK(empty.totalEnergy() == 0.0);
  CHECK(empty.rows().empty());
}

TEST_CASE("PathStats: merging one block tally into another") {
  const ScopedGrid grid{{400, 500, 600, 700}, false};
  const std::vector<Entry> entries{makeEntries()};
  // One tally takes the first half, another the rest and a deeper path
  // than the first ever sees; a third takes everything.
  PathStats a{};
  PathStats b{};
  PathStats whole{};
  for (size_t i = 0; i < entries.size(); i++) {
    PathStats &part{i < entries.size() / 2 ? a : b};
    part.recordContribution(entries[i].bounces, entries[i].color);
    whole.recordContribution(entries[i].bounces, entries[i].color);
  }
  a.recordPath(1, PathEnd::ESCAPED);
  whole.recordPath(1, PathEnd::ESCAPED);
  b.recordPath(9, PathEnd::ROULETTE);
  whole.recordPath(9, PathEnd::ROULETTE);
  a.addSamples(3);
  b.addSamples(4);
  whole.addSamples(7);
  b.recordMediumEmission(color4(1.0f, 1.0f, 1.0f, 1.0f));
  whole.recordMediumEmission(color4(1.0f, 1.0f, 1.0f, 1.0f));
  b.recordClamp(3, entries.back().color, 0.5f);
  whole.recordClamp(3, entries.back().color, 0.5f);
  a.add(b);
  REQUIRE(a.rows().size() == whole.rows().size());
  for (size_t i = 0; i < a.rows().size(); i++) {
    const PathStats::Row &row{a.rows()[i]};
    const PathStats::Row &expected{whole.rows()[i]};
    for (size_t bin = 0; bin < PathStats::NUM_BINS; bin++) {
      CHECK(row.bins[bin].count == expected.bins[bin].count);
      CHECK(row.bins[bin].sumAverage ==
            doctest::Approx(expected.bins[bin].sumAverage));
      CHECK(row.bins[bin].sumAverageOverMax ==
            doctest::Approx(expected.bins[bin].sumAverageOverMax));
    }
    CHECK(row.zeroCount == expected.zeroCount);
    CHECK(row.maxContribution == expected.maxContribution);
    CHECK(row.clampedCount == expected.clampedCount);
    CHECK(row.energyClamped == doctest::Approx(expected.energyClamped));
    CHECK(row.ends == expected.ends);
  }
  CHECK(a.sampleCount() == 7);
  CHECK(a.pathCount() == 2);
  CHECK(a.mediumEmission() == doctest::Approx(1.0));
  CHECK(a.maxBouncesReached() == 9);
  const double before{a.totalEnergy()};
  a.add(PathStats{});
  CHECK(a.totalEnergy() == before);
  CHECK(a.rows().size() == whole.rows().size());
}

TEST_CASE("PathStats: the text and JSON reports") {
  const ScopedGrid grid{{400, 500, 600, 700}, false};
  const std::vector<Entry> entries{makeEntries()};
  PathStats stats{tallyOf(entries)};
  stats.recordPath(0, PathEnd::ESCAPED);
  stats.recordPath(3, PathEnd::ROULETTE);
  stats.recordPath(3, PathEnd::BOUND);
  stats.addSamples(5);
  const PathStatsSession session{int4{0, 0, 4, 2}, 1, 7};
  PathOptions path{};
  MNEEOptions mnee{};
  const auto printText{[&](const PathStats &tally) {
    std::string text{};
    llvm::raw_string_ostream os{text};
    tally.print(os, session, path, mnee);
    os.flush();
    return text;
  }};
  const auto printJSON{[&](const PathStats &tally) {
    std::string text{};
    llvm::raw_string_ostream os{text};
    tally.printJSON(os, session, path, mnee);
    os.flush();
    return text;
  }};
  SUBCASE("The text, without and with a bound in force") {
    std::string text{printText(stats)};
    CHECK_CONTAINS(text,
                   "5 samples over window 0,0,4,2 at 1 spp from sample 7");
    CHECK_CONTAINS(text, "2 camera samples vignetted");
    CHECK_NOT_CONTAINS(text, "failed");
    CHECK_NOT_CONTAINS(text, "bound in force");
    CHECK_NOT_CONTAINS(text, "removed at");
    path.maxContribution = 30.0f;
    path.maxContributionBounces = 2;
    text = printText(stats);
    CHECK_CONTAINS(text, "removed at 30");
    CHECK_CONTAINS(text, "-max-contribution 30 from 2 bounces");
    path.useRoulette = false;
    path.maxBounces = 3;
    text = printText(stats);
    CHECK_CONTAINS(text, "-max-bounces 3");
  }
  SUBCASE("A failed path is named only when there is one") {
    stats.recordPath(4, PathEnd::FAILED);
    CHECK_CONTAINS(printText(stats), "failed");
  }
  SUBCASE("A gate past every row says so") {
    path.maxContribution = 30.0f;
    path.maxContributionBounces = 9;
    CHECK_CONTAINS(printText(stats), "no bound would apply");
  }
  SUBCASE("An empty tally prints and parses") {
    const PathStats empty{};
    CHECK_CONTAINS(printText(empty), "No paths were traced");
    llvm::Expected<llvm::json::Value> parsed{
        llvm::json::parse(printJSON(empty))};
    if (!parsed) FAIL(llvm::toString(parsed.takeError()));
    const llvm::json::Object *object{parsed->getAsObject()};
    REQUIRE(object);
    CHECK(object->getInteger("samples") == int64_t(0));
    const llvm::json::Array *bounces{object->getArray("bounces")};
    REQUIRE(bounces);
    CHECK(bounces->empty());
  }
  SUBCASE("The JSON parses and carries the tally") {
    path.maxContribution = 30.0f;
    llvm::Expected<llvm::json::Value> parsed{
        llvm::json::parse(printJSON(stats))};
    if (!parsed) FAIL(llvm::toString(parsed.takeError()));
    const llvm::json::Object *object{parsed->getAsObject()};
    REQUIRE(object);
    CHECK(object->getInteger("samples") == int64_t(5));
    CHECK(object->getInteger("paths") == int64_t(3));
    const llvm::json::Object *sessionObject{object->getObject("session")};
    REQUIRE(sessionObject);
    CHECK(sessionObject->getInteger("spp") == int64_t(1));
    CHECK(sessionObject->getInteger("sample_index_base") == int64_t(7));
    const llvm::json::Object *bounds{object->getObject("bounds")};
    REQUIRE(bounds);
    CHECK(bounds->getNumber("max_contribution") == 30.0);
    const llvm::json::Array *edges{object->getArray("edges")};
    REQUIRE(edges);
    CHECK(edges->size() == PathStats::NUM_BINS);
    const llvm::json::Array *bounces{object->getArray("bounces")};
    REQUIRE(bounces);
    REQUIRE(bounces->size() == stats.rows().size());
    const llvm::json::Object *row{(*bounces)[3].getAsObject()};
    REQUIRE(row);
    CHECK(row->getInteger("bounces") == int64_t(3));
    const llvm::json::Array *bins{row->getArray("bins")};
    REQUIRE(bins);
    CHECK(bins->size() == PathStats::NUM_BINS);
    const llvm::json::Object *ended{row->getObject("ended")};
    REQUIRE(ended);
    CHECK(ended->getInteger("roulette") == int64_t(1));
    CHECK(ended->getInteger("bound") == int64_t(1));
  }
}

namespace {

[[nodiscard]] smdl::ManifoldWalkReport
walkReport(int iterations, smdl::ManifoldWalkReport::Outcome outcome,
           smdl::ManifoldWalkReport::Failure failure =
               smdl::ManifoldWalkReport::Failure::NONE,
           float residual = 0.0f) {
  smdl::ManifoldWalkReport report{};
  report.iterations = iterations;
  report.outcome = outcome;
  report.failure = failure;
  report.residual = residual;
  return report;
}

} // namespace

TEST_CASE("PathStats: the manifold section") {
  const ScopedGrid grid{{400, 500, 600, 700}, false};
  using Outcome = smdl::ManifoldWalkReport::Outcome;
  using Failure = smdl::ManifoldWalkReport::Failure;
  PathStats stats{};
  MNEEStats &mnee{stats.mnee()};
  mnee.recordEstimate(MNEEStats::DIRAC_REFRACT, true);
  mnee.recordEstimate(MNEEStats::DIRAC_REFRACT, false);
  mnee.recordEstimate(MNEEStats::GLOSSY_REFLECT, true);
  mnee.recordWalk(walkReport(3, Outcome::CONVERGED, Failure::NONE, 1e-6f));
  mnee.recordWalk(walkReport(5, Outcome::CONVERGED, Failure::NONE, 3e-6f));
  mnee.recordWalk(walkReport(7, Outcome::REJECTED));
  mnee.recordWalk(walkReport(64, Outcome::DIVERGED, Failure::ITERATIONS));
  // Past the last bin, which holds every count from it up.
  mnee.recordWalk(walkReport(200, Outcome::CONVERGED, Failure::NONE, 2e-6f));
  mnee.recordRewalk(walkReport(2, Outcome::CONVERGED));
  mnee.recordRewalk(walkReport(9, Outcome::DIVERGED, Failure::STALLED));
  mnee.recordCover(MNEEStats::Cover::MATCHED);
  mnee.recordCover(MNEEStats::Cover::UNMATCHED);
  mnee.recordCover(MNEEStats::Cover::MATCHED);
  mnee.recordCover(MNEEStats::Cover::DROPPED);
  mnee.recordTrials(MNEEStats::DIRAC_REFRACT, 4, false);
  mnee.recordTrials(MNEEStats::DIRAC_REFRACT, 10, true);
  mnee.recordContribution(true);
  mnee.recordContribution(false);
  const MNEEStats::KindCounts &dirac{mnee.kinds[MNEEStats::DIRAC_REFRACT]};
  CHECK(dirac.estimateCount == 2);
  CHECK(dirac.firstConvergedCount == 1);
  CHECK(mnee.kinds[MNEEStats::GLOSSY_REFLECT].estimateCount == 1);
  CHECK(dirac.trialEstimateCount == 2);
  CHECK(dirac.trialCount == 14);
  CHECK(dirac.trialsMax == 10);
  CHECK(dirac.capDropCount == 1);
  CHECK(mnee.walkCount == 5);
  CHECK(mnee.walkConvergedCount == 3);
  CHECK(mnee.walkRejectedCount == 1);
  CHECK(mnee.walkFailureCounts[size_t(Failure::ITERATIONS)] == 1);
  CHECK(mnee.walkIterations == 279);
  CHECK(mnee.walkIterationsMax == 200);
  // The rejected walk converged first, so it counts; the diverged one
  // does not.
  CHECK(mnee.convergedIterationCounts[3] == 1);
  CHECK(mnee.convergedIterationCounts[7] == 1);
  CHECK(mnee.convergedIterationCounts[MNEEStats::NUM_ITERATION_BINS - 1] == 1);
  CHECK(mnee.walkResidual == doctest::Approx(6e-6));
  CHECK(mnee.walkResidualMax == doctest::Approx(3e-6));
  CHECK(mnee.iterationsPercentile(0.0) == 3);
  CHECK(mnee.iterationsPercentile(0.5) == 5);
  CHECK(mnee.iterationsPercentile(0.99) == 7);
  CHECK(mnee.iterationsPercentile(1.0) == MNEEStats::NUM_ITERATION_BINS - 1);
  CHECK(mnee.rewalkCount == 2);
  CHECK(mnee.rewalkConvergedCount == 1);
  CHECK(mnee.coverArrivalCount == 4);
  CHECK(mnee.coverMatchedCount == 2);
  CHECK(mnee.coverDroppedCount == 1);
  CHECK(mnee.contributionCount == 2);
  CHECK(mnee.contributionNonZeroCount == 1);
  CHECK(PathStats{}.mnee().iterationsPercentile(0.5) == 0);
  SUBCASE("Merging sums the counts and keeps the maxima") {
    PathStats other{};
    other.mnee().recordWalk(
        walkReport(300, Outcome::CONVERGED, Failure::NONE, 9e-6f));
    other.mnee().recordTrials(MNEEStats::DIRAC_REFRACT, 20, false);
    other.mnee().recordCover(MNEEStats::Cover::DROPPED);
    stats.add(other);
    CHECK(mnee.walkCount == 6);
    CHECK(mnee.coverArrivalCount == 5);
    CHECK(mnee.coverDroppedCount == 2);
    CHECK(mnee.walkIterationsMax == 300);
    CHECK(mnee.walkResidualMax == doctest::Approx(9e-6));
    CHECK(dirac.trialsMax == 20);
    CHECK(dirac.trialCount == 34);
    CHECK(mnee.convergedIterationCounts[MNEEStats::NUM_ITERATION_BINS - 1] ==
          2);
  }
  SUBCASE("The section prints only under -mnee") {
    stats.recordPath(0, PathEnd::ESCAPED);
    stats.addSamples(1);
    const PathStatsSession session{int4{0, 0, 1, 1}, 1, 0};
    PathOptions path{};
    MNEEOptions mneeOptions{};
    const auto printText{[&] {
      std::string text{};
      llvm::raw_string_ostream os{text};
      stats.print(os, session, path, mneeOptions);
      os.flush();
      return text;
    }};
    const auto printJSON{[&] {
      std::string text{};
      llvm::raw_string_ostream os{text};
      stats.printJSON(os, session, path, mneeOptions);
      os.flush();
      return text;
    }};
    CHECK_NOT_CONTAINS(printText(), "Manifold estimators");
    CHECK_NOT_CONTAINS(printJSON(), "\"mnee\"");
    mneeOptions.depth = 2;
    const std::string text{printText()};
    CHECK_CONTAINS(text, "Manifold estimators");
    CHECK_CONTAINS(text, "p50 5, p90 7");
    CHECK_CONTAINS(text, "dirac refraction");
    CHECK_CONTAINS(text, "caster dirac refraction");
    CHECK_CONTAINS(text, "dropped as the caster gather's");
    const std::string json{printJSON()};
    INFO(json);
    CHECK_CONTAINS(json, "\"mnee\"");
    llvm::Expected<llvm::json::Value> parsed{llvm::json::parse(json)};
    if (!parsed) FAIL(llvm::toString(parsed.takeError()));
    const llvm::json::Object *object{parsed->getAsObject()->getObject("mnee")};
    REQUIRE(object);
    const llvm::json::Array *kinds{object->getArray("kinds")};
    REQUIRE(kinds);
    CHECK(kinds->size() == size_t(MNEEStats::NUM_KINDS));
    const llvm::json::Object *walks{object->getObject("walks")};
    REQUIRE(walks);
    CHECK(walks->getInteger("count") == int64_t(5));
    const llvm::json::Array *bins{walks->getArray("converged_iterations")};
    REQUIRE(bins);
    CHECK(bins->size() == MNEEStats::NUM_ITERATION_BINS);
    CHECK(walks->getObject("diverged")->getInteger("iterations") == int64_t(1));
    const llvm::json::Object *covered{object->getObject("covered_arrivals")};
    REQUIRE(covered);
    CHECK(covered->getInteger("count") == int64_t(4));
    CHECK(covered->getInteger("dropped") == int64_t(1));
  }
}
