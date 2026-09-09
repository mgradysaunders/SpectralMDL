#include "Render/PathStats.h"
#include "Render/PathTracing.h"

#include "llvm/Support/JSON.h"
#include "llvm/Support/raw_ostream.h"

#include "smdl/Support/Strings.h"

#include <algorithm>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <iterator>
#include <limits>
#include <string>

namespace {

// The lower edges of every bin but the underflow bin, the last being
// where the overflow bin begins: the 1-2-3-5 series across the twenty
// decades from 1e-10, so that every edge is a number a person types.
constexpr size_t NUM_EDGES{PathStats::NUM_BINS - 1};

constexpr int FIRST_DECADE{-10};

[[nodiscard]] constexpr std::array<double, NUM_EDGES> makeBinEdges() noexcept {
  constexpr double MANTISSAS[]{1.0, 2.0, 3.0, 5.0};
  std::array<double, NUM_EDGES> edges{};
  size_t i{};
  for (int decade = FIRST_DECADE; i < NUM_EDGES; decade++) {
    // The power of ten is exact, so one multiply or divide lands each
    // edge on the nearest double to its decimal, the value its literal
    // would have.
    double power{1.0};
    for (int k = 0; k < (decade < 0 ? -decade : decade); k++) power *= 10.0;
    for (double mantissa : MANTISSAS)
      if (i < NUM_EDGES)
        edges[i++] = decade < 0 ? mantissa / power : mantissa * power;
  }
  return edges;
}

constexpr auto BIN_EDGES{makeBinEdges()};

static_assert(BIN_EDGES.back() == 1e10);

// The first bin whose lower edge is at or above `bound`, so that every
// bin from it up holds only contributions at or above the bound.
[[nodiscard]] size_t firstBinAtOrAbove(double bound) noexcept {
  if (!(bound > 0.0)) return 0;
  return size_t(std::lower_bound(BIN_EDGES.begin(), BIN_EDGES.end(), bound) -
                BIN_EDGES.begin()) +
         1;
}

// The names of the ends, in `PathEnd` order.
constexpr const char *END_NAMES[]{"escaped", "absorbed", "roulette", "bound",
                                  "failed"};

static_assert(std::size(END_NAMES) == size_t(PathEnd::NUM_ENDS));

// The names of the gather kinds and the walk failures, in enum order.
constexpr const char *KIND_NAMES[]{"dirac refraction", "glossy refraction",
                                   "dirac reflection", "glossy reflection"};

static_assert(std::size(KIND_NAMES) == size_t(MNEEStats::NUM_KINDS));

constexpr const char *FAILURE_NAMES[]{"none",       "start",   "singular",
                                      "projection", "stalled", "iterations"};

static_assert(std::size(FAILURE_NAMES) ==
              size_t(smdl::ManifoldWalkReport::Failure::NUM_FAILURES));

// The bound table stops at whichever comes first: this many rows, or
// the row past which a bound removes more than this share of the
// energy, where nobody would set one.
constexpr size_t MAX_BOUND_ROWS{15};

constexpr double MAX_BOUND_REMOVAL{0.25};

// A number through a printf format, for the '%g' spellings the tables
// read best in.
[[nodiscard]] std::string spell(const char *format, double value) {
  char buffer[64]{};
  std::snprintf(buffer, sizeof(buffer), format, value);
  return buffer;
}

[[nodiscard]] std::string spellPercent(double fraction) {
  return spell("%.3g%%", 100.0 * fraction);
}

// A double with the fewest digits that read back exactly: an edge of
// 2e-10 prints as that, not as the seventeen digits of its nearest
// double. Every double in the JSON goes through this, the attributes
// included, so the document spells its numbers one way.
[[nodiscard]] std::string spellDouble(double value) {
  for (int precision = 15; precision < 17; precision++) {
    char buffer[64]{};
    std::snprintf(buffer, sizeof(buffer), "%.*g", precision, value);
    if (std::strtod(buffer, nullptr) == value) return buffer;
  }
  return spell("%.17g", value);
}

// A double attribute, spelled by `spellDouble()`.
void attributeDouble(llvm::json::OStream &json, const char *key, double value) {
  json.attributeBegin(key);
  json.rawValue(spellDouble(value));
  json.attributeEnd();
}

// A ratio that reads as zero rather than as a division by zero when
// there is nothing to divide by.
[[nodiscard]] double ratio(double numerator, double denominator) noexcept {
  return denominator > 0.0 ? numerator / denominator : 0.0;
}

// Print `rows` under `header`, every column right-aligned to its widest
// entry and two spaces apart, the whole table indented by two.
void printTable(llvm::raw_ostream &os, const std::vector<std::string> &header,
                const std::vector<std::vector<std::string>> &rows) {
  auto widths{std::vector<size_t>(header.size())};
  for (size_t c = 0; c < header.size(); c++) widths[c] = header[c].size();
  for (const auto &row : rows)
    for (size_t c = 0; c < row.size(); c++)
      widths[c] = std::max(widths[c], row[c].size());
  const auto printRow{[&](const std::vector<std::string> &row) {
    for (size_t c = 0; c < row.size(); c++) {
      os.indent(unsigned(widths[c] - row[c].size() + 2));
      os << row[c];
    }
    os << '\n';
  }};
  printRow(header);
  for (const auto &row : rows) printRow(row);
}

} // namespace

void MNEEStats::recordEstimate(Kind kind, bool isFirstWalkConverged) noexcept {
  kinds[kind].estimateCount++;
  if (isFirstWalkConverged) kinds[kind].firstConvergedCount++;
}

void MNEEStats::recordWalk(const smdl::ManifoldWalkReport &report) noexcept {
  using Outcome = smdl::ManifoldWalkReport::Outcome;
  const auto iterations{uint64_t(std::max(report.iterations, 0))};
  walkCount++;
  walkIterations += iterations;
  walkIterationsMax = std::max(walkIterationsMax, iterations);
  if (report.outcome != Outcome::DIVERGED)
    convergedIterationCounts[std::min<size_t>(iterations,
                                              NUM_ITERATION_BINS - 1)]++;
  if (report.outcome == Outcome::CONVERGED) {
    walkConvergedCount++;
    walkResidual += double(report.residual);
    walkResidualMax = std::max(walkResidualMax, double(report.residual));
  } else if (report.outcome == Outcome::REJECTED) {
    walkRejectedCount++;
  } else {
    walkFailureCounts[size_t(report.failure)]++;
  }
}

void MNEEStats::recordRewalk(const smdl::ManifoldWalkReport &report) noexcept {
  rewalkCount++;
  if (report.outcome == smdl::ManifoldWalkReport::Outcome::CONVERGED)
    rewalkConvergedCount++;
}

void MNEEStats::recordCover(bool isMatched) noexcept {
  coverArrivalCount++;
  if (isMatched) coverMatchedCount++;
}

void MNEEStats::recordTrials(Kind kind, int trials, bool wasDropped) noexcept {
  auto &counts{kinds[kind]};
  const auto count{uint64_t(std::max(trials, 0))};
  counts.trialEstimateCount++;
  counts.trialCount += count;
  counts.trialsMax = std::max(counts.trialsMax, count);
  if (wasDropped) counts.capDropCount++;
}

void MNEEStats::recordContribution(bool isNonZero) noexcept {
  contributionCount++;
  if (isNonZero) contributionNonZeroCount++;
}

void MNEEStats::add(const MNEEStats &other) noexcept {
  for (size_t k = 0; k < kinds.size(); k++) {
    kinds[k].estimateCount += other.kinds[k].estimateCount;
    kinds[k].firstConvergedCount += other.kinds[k].firstConvergedCount;
    kinds[k].trialEstimateCount += other.kinds[k].trialEstimateCount;
    kinds[k].trialCount += other.kinds[k].trialCount;
    kinds[k].trialsMax = std::max(kinds[k].trialsMax, other.kinds[k].trialsMax);
    kinds[k].capDropCount += other.kinds[k].capDropCount;
  }
  walkCount += other.walkCount;
  walkConvergedCount += other.walkConvergedCount;
  walkRejectedCount += other.walkRejectedCount;
  for (size_t f = 0; f < walkFailureCounts.size(); f++)
    walkFailureCounts[f] += other.walkFailureCounts[f];
  walkIterations += other.walkIterations;
  walkIterationsMax = std::max(walkIterationsMax, other.walkIterationsMax);
  for (size_t b = 0; b < NUM_ITERATION_BINS; b++)
    convergedIterationCounts[b] += other.convergedIterationCounts[b];
  walkResidual += other.walkResidual;
  walkResidualMax = std::max(walkResidualMax, other.walkResidualMax);
  rewalkCount += other.rewalkCount;
  rewalkConvergedCount += other.rewalkConvergedCount;
  coverArrivalCount += other.coverArrivalCount;
  coverMatchedCount += other.coverMatchedCount;
  contributionCount += other.contributionCount;
  contributionNonZeroCount += other.contributionNonZeroCount;
}

size_t MNEEStats::iterationsPercentile(double p) const noexcept {
  uint64_t total{};
  for (const auto count : convergedIterationCounts) total += count;
  if (total == 0) return 0;
  const auto want{uint64_t(p * double(total - 1))};
  uint64_t seen{};
  for (size_t b = 0; b < NUM_ITERATION_BINS; b++) {
    seen += convergedIterationCounts[b];
    if (seen > want) return b;
  }
  return NUM_ITERATION_BINS - 1;
}

void MNEEStats::print(llvm::raw_ostream &os) const {
  os << "Manifold estimators by gather kind, the first walk's convergence "
        "over the estimates and the reciprocal trials over the nonzero "
        "ones:\n";
  auto rows{std::vector<std::vector<std::string>>()};
  for (size_t k = 0; k < kinds.size(); k++) {
    const auto &counts{kinds[k]};
    rows.push_back(
        {KIND_NAMES[k], std::to_string(counts.estimateCount),
         smdl::concat(counts.firstConvergedCount, " (",
                      spellPercent(ratio(double(counts.firstConvergedCount),
                                         double(counts.estimateCount))),
                      ")"),
         std::to_string(counts.trialEstimateCount),
         spell("%.2f", ratio(double(counts.trialCount),
                             double(counts.trialEstimateCount))),
         std::to_string(counts.trialsMax),
         std::to_string(counts.capDropCount)});
  }
  printTable(os,
             {"kind", "estimates", "first converged", "nonzero", "trials avg",
              "trials max", "cap drops"},
             rows);
  const auto share{[](uint64_t part, uint64_t whole) {
    return smdl::concat(part, " (",
                        spellPercent(ratio(double(part), double(whole))), ")");
  }};
  os << "  walks: " << walkCount << ", " << share(walkConvergedCount, walkCount)
     << " converged, " << walkRejectedCount << " rejected after converging\n";
  os << "    diverged:";
  using Failure = smdl::ManifoldWalkReport::Failure;
  for (size_t f = size_t(Failure::START); f < walkFailureCounts.size(); f++)
    os << (f == size_t(Failure::START) ? " " : ", ") << FAILURE_NAMES[f] << ' '
       << walkFailureCounts[f];
  os << "\n    iterations: avg "
     << spell("%.2f", ratio(double(walkIterations), double(walkCount)))
     << ", max " << walkIterationsMax;
  size_t top{};
  bool hasConverged{false};
  for (size_t b = 0; b < NUM_ITERATION_BINS; b++)
    if (convergedIterationCounts[b] > 0) {
      top = b;
      hasConverged = true;
    }
  if (hasConverged)
    os << "; to converge p50 " << iterationsPercentile(0.50) << ", p90 "
       << iterationsPercentile(0.90) << ", p99 " << iterationsPercentile(0.99)
       << ", p99.9 " << iterationsPercentile(0.999) << ", p99.99 "
       << iterationsPercentile(0.9999) << ", max " << top;
  os << "\n    residual over converged walks: avg "
     << spell("%.3g", ratio(walkResidual, double(walkConvergedCount)))
     << ", max " << spell("%.3g", walkResidualMax) << '\n';
  os << "  re-walks for MIS: " << rewalkCount << ", "
     << share(rewalkConvergedCount, rewalkCount) << " converged\n";
  os << "  covered arrivals: " << coverArrivalCount << ", "
     << share(coverMatchedCount, coverArrivalCount)
     << " matched by the re-walk\n";
  os << "  contributions: " << contributionCount << ", "
     << share(contributionNonZeroCount, contributionCount) << " non-zero\n";
}

void MNEEStats::printJSON(llvm::json::OStream &json) const {
  json.attributeArray("kinds", [&] {
    for (size_t k = 0; k < kinds.size(); k++) {
      const auto &counts{kinds[k]};
      json.object([&] {
        json.attribute("kind", KIND_NAMES[k]);
        json.attribute("estimates", int64_t(counts.estimateCount));
        json.attribute("first_converged", int64_t(counts.firstConvergedCount));
        json.attribute("nonzero", int64_t(counts.trialEstimateCount));
        json.attribute("trials", int64_t(counts.trialCount));
        json.attribute("trials_max", int64_t(counts.trialsMax));
        json.attribute("cap_drops", int64_t(counts.capDropCount));
      });
    }
  });
  json.attributeObject("walks", [&] {
    json.attribute("count", int64_t(walkCount));
    json.attribute("converged", int64_t(walkConvergedCount));
    json.attribute("rejected", int64_t(walkRejectedCount));
    json.attributeObject("diverged", [&] {
      using Failure = smdl::ManifoldWalkReport::Failure;
      for (size_t f = size_t(Failure::START); f < walkFailureCounts.size(); f++)
        json.attribute(FAILURE_NAMES[f], int64_t(walkFailureCounts[f]));
    });
    json.attribute("iterations", int64_t(walkIterations));
    json.attribute("iterations_max", int64_t(walkIterationsMax));
    // The converged-iteration histogram, bin `i` holding the walks that
    // converged in `i` iterations and the last every count from it up.
    json.attributeBegin("converged_iterations");
    {
      auto counts{std::string("[")};
      for (size_t b = 0; b < NUM_ITERATION_BINS; b++)
        counts += smdl::concat(b > 0 ? ", " : "", convergedIterationCounts[b]);
      json.rawValue(counts + "]");
    }
    json.attributeEnd();
    attributeDouble(json, "residual", walkResidual);
    attributeDouble(json, "residual_max", walkResidualMax);
  });
  json.attributeObject("rewalks", [&] {
    json.attribute("count", int64_t(rewalkCount));
    json.attribute("converged", int64_t(rewalkConvergedCount));
  });
  json.attributeObject("covered_arrivals", [&] {
    json.attribute("count", int64_t(coverArrivalCount));
    json.attribute("matched", int64_t(coverMatchedCount));
  });
  json.attributeObject("contributions", [&] {
    json.attribute("count", int64_t(contributionCount));
    json.attribute("nonzero", int64_t(contributionNonZeroCount));
  });
}

uint64_t PathStats::Row::contributionCount() const noexcept {
  uint64_t count{};
  for (const auto &bin : bins) count += bin.count;
  return count;
}

double PathStats::Row::energy() const noexcept {
  double energy{};
  for (const auto &bin : bins) energy += bin.sumAverage;
  return energy;
}

uint64_t PathStats::Row::pathCount() const noexcept {
  uint64_t count{};
  for (const auto end : ends) count += end;
  return count;
}

size_t PathStats::binIndex(double maxValue) noexcept {
  return size_t(std::upper_bound(BIN_EDGES.begin(), BIN_EDGES.end(), maxValue) -
                BIN_EDGES.begin());
}

double PathStats::binEdge(size_t bin) noexcept {
  if (bin == 0) return 0.0;
  if (bin > NUM_EDGES) return std::numeric_limits<double>::infinity();
  return BIN_EDGES[bin - 1];
}

PathStats::Row &PathStats::rowAt(uint64_t bounces) {
  if (bounces >= mRows.size()) mRows.resize(size_t(bounces) + 1);
  return mRows[size_t(bounces)];
}

void PathStats::recordContribution(uint64_t bounces,
                                   const Color &contribution) {
  if (contribution.isAnyNonFinite()) {
    mNumNonFinite++;
    return;
  }
  Row &row{rowAt(bounces)};
  const double maxValue{contribution.maxComponent()};
  if (!(maxValue > 0.0)) {
    row.zeroCount++;
    return;
  }
  const double average{contribution.average()};
  Bin &bin{row.bins[binIndex(maxValue)]};
  bin.count++;
  bin.sumAverage += average;
  bin.sumAverageOverMax += average / maxValue;
  row.maxContribution = std::max(row.maxContribution, maxValue);
}

void PathStats::recordClamp(uint64_t bounces, const Color &contribution,
                            float scale) {
  Row &row{rowAt(bounces)};
  row.clampedCount++;
  row.energyClamped += double(contribution.average()) * (1.0 - double(scale));
}

void PathStats::recordMediumEmission(const Color &emission) {
  mMediumEmission += double(emission.average());
}

void PathStats::recordPath(uint64_t bounces, PathEnd end) {
  rowAt(bounces).ends[size_t(end)]++;
  mNumPaths++;
}

void PathStats::add(const PathStats &other) {
  if (other.mRows.size() > mRows.size()) mRows.resize(other.mRows.size());
  for (size_t i = 0; i < other.mRows.size(); i++) {
    Row &row{mRows[i]};
    const Row &otherRow{other.mRows[i]};
    for (size_t b = 0; b < NUM_BINS; b++) {
      row.bins[b].count += otherRow.bins[b].count;
      row.bins[b].sumAverage += otherRow.bins[b].sumAverage;
      row.bins[b].sumAverageOverMax += otherRow.bins[b].sumAverageOverMax;
    }
    row.zeroCount += otherRow.zeroCount;
    row.maxContribution =
        std::max(row.maxContribution, otherRow.maxContribution);
    row.clampedCount += otherRow.clampedCount;
    row.energyClamped += otherRow.energyClamped;
    for (size_t e = 0; e < row.ends.size(); e++)
      row.ends[e] += otherRow.ends[e];
  }
  mNumSamples += other.mNumSamples;
  mNumPaths += other.mNumPaths;
  mNumNonFinite += other.mNumNonFinite;
  mMediumEmission += other.mMediumEmission;
  mMNEE.add(other.mMNEE);
}

double PathStats::totalEnergy() const noexcept {
  return energyFrom(0) + mMediumEmission;
}

uint64_t PathStats::contributionCountFrom(uint64_t minBounces) const noexcept {
  uint64_t count{};
  for (size_t i = size_t(minBounces); i < mRows.size(); i++)
    count += mRows[i].contributionCount();
  return count;
}

double PathStats::energyFrom(uint64_t minBounces) const noexcept {
  double energy{};
  for (size_t i = size_t(minBounces); i < mRows.size(); i++)
    energy += mRows[i].energy();
  return energy;
}

uint64_t PathStats::countAbove(double bound,
                               uint64_t minBounces) const noexcept {
  const size_t first{firstBinAtOrAbove(bound)};
  uint64_t count{};
  for (size_t i = size_t(minBounces); i < mRows.size(); i++)
    for (size_t b = first; b < NUM_BINS; b++) count += mRows[i].bins[b].count;
  return count;
}

double PathStats::energyRemovedAbove(double bound,
                                     uint64_t minBounces) const noexcept {
  const size_t first{firstBinAtOrAbove(bound)};
  double removed{};
  for (size_t i = size_t(minBounces); i < mRows.size(); i++)
    for (size_t b = first; b < NUM_BINS; b++) {
      const Bin &bin{mRows[i].bins[b]};
      removed += bin.sumAverage - bound * bin.sumAverageOverMax;
    }
  return removed;
}

uint64_t PathStats::clampedCount() const noexcept {
  uint64_t count{};
  for (const auto &row : mRows) count += row.clampedCount;
  return count;
}

double PathStats::energyClamped() const noexcept {
  double energy{};
  for (const auto &row : mRows) energy += row.energyClamped;
  return energy;
}

double PathStats::meanBounces() const noexcept {
  if (mNumPaths == 0) return 0.0;
  double sum{};
  for (size_t i = 0; i < mRows.size(); i++)
    sum += double(i) * double(mRows[i].pathCount());
  return sum / double(mNumPaths);
}

uint64_t PathStats::maxBouncesReached() const noexcept {
  for (size_t i = mRows.size(); i-- > 0;)
    if (mRows[i].pathCount() > 0) return i;
  return 0;
}

void PathStats::print(llvm::raw_ostream &os, const PathStatsSession &session,
                      const PathOptions &path,
                      const MNEEOptions &mneeOptions) const {
  const auto &window{session.window};
  os << "Path statistics for this session: " << mNumSamples
     << " samples over window " << window[0] << ',' << window[1] << ','
     << window[2] << ',' << window[3] << " at " << session.spp
     << " spp from sample " << session.sampleIndexBase << '\n';
  if (mNumPaths == 0) {
    os << "  No paths were traced.\n";
    return;
  }
  os << "  paths traced: " << mNumPaths;
  if (mNumSamples > mNumPaths)
    os << " (" << mNumSamples - mNumPaths
       << " camera samples vignetted and not traced)";
  os << "\n  bounces: mean " << spell("%.2f", meanBounces()) << ", max "
     << maxBouncesReached() << '\n';
  os << "  ended by:";
  {
    std::array<uint64_t, size_t(PathEnd::NUM_ENDS)> ends{};
    for (const auto &row : mRows)
      for (size_t e = 0; e < ends.size(); e++) ends[e] += row.ends[e];
    bool isFirst{true};
    for (size_t e = 0; e < ends.size(); e++) {
      // A failure is a bug, so it is named only when it happened.
      if (ends[e] == 0 && PathEnd(e) == PathEnd::FAILED) continue;
      os << (isFirst ? " " : ", ") << END_NAMES[e] << ' '
         << spellPercent(ratio(double(ends[e]), double(mNumPaths)));
      isFirst = false;
    }
  }
  os << '\n';
  // Every energy share below is of the contribution energy before the
  // bound: volume emission is neither bounce-indexed nor boundable.
  const double energy{energyFrom(0)};
  os << "  radiance per sample: "
     << spell("%.4g", ratio(totalEnergy(), double(mNumSamples)))
     << " (band average, before the bound)";
  if (mMediumEmission > 0.0)
    os << ", of which volume emission "
       << spell("%.4g", ratio(mMediumEmission, double(mNumSamples)));
  os << "\n\n";

  const bool hasBound{path.maxContribution > 0.0f};
  {
    auto header{std::vector<std::string>{"bounce", "paths ending", "paths past",
                                         "contributions", "energy",
                                         "cumulative", "largest"}};
    if (hasBound)
      header.push_back(
          smdl::concat("removed at ", smdl::Brief(path.maxContribution)));
    auto rows{std::vector<std::vector<std::string>>()};
    uint64_t pathsPast{mNumPaths};
    double cumulative{};
    for (size_t i = 0; i < mRows.size(); i++) {
      const Row &row{mRows[i]};
      pathsPast -= row.pathCount();
      cumulative += row.energy();
      auto cells{std::vector<std::string>{
          std::to_string(i),
          spellPercent(ratio(double(row.pathCount()), double(mNumPaths))),
          spellPercent(ratio(double(pathsPast), double(mNumPaths))),
          std::to_string(row.contributionCount()),
          spellPercent(ratio(row.energy(), energy)),
          spellPercent(ratio(cumulative, energy)),
          spell("%.3g", row.maxContribution)}};
      if (hasBound)
        cells.push_back(spellPercent(ratio(row.energyClamped, energy)));
      rows.push_back(std::move(cells));
    }
    os << "By bounce, energy as a share of the contribution energy and the "
          "largest contribution before the bound:\n";
    printTable(os, header, rows);
  }

  const auto gate{uint64_t(std::max(path.maxContributionBounces, 1))};
  const uint64_t gatedCount{contributionCountFrom(gate)};
  os << "\nBound on the largest band of a contribution, over the " << gatedCount
     << " non-zero contributions of at least " << gate
     << (gate == 1 ? " bounce" : " bounces")
     << ", energy as a share of the contribution energy:\n";
  if (gatedCount == 0) {
    os << "  No contribution reaches " << gate
       << " bounces, so no bound would apply.\n";
  } else {
    // From the top populated bin of the gated rows downward.
    size_t top{};
    for (size_t i = size_t(gate); i < mRows.size(); i++)
      for (size_t b = NUM_BINS; b-- > 0;)
        if (mRows[i].bins[b].count > 0) {
          top = std::max(top, b);
          break;
        }
    auto rows{std::vector<std::vector<std::string>>()};
    for (size_t b = top; b > 0 && rows.size() < MAX_BOUND_ROWS; b--) {
      const double bound{binEdge(b)};
      const uint64_t above{countAbove(bound, gate)};
      const double removed{ratio(energyRemovedAbove(bound, gate), energy)};
      rows.push_back({spell("%g", bound), std::to_string(above),
                      spellPercent(ratio(double(above), double(gatedCount))),
                      spellPercent(removed)});
      if (removed > MAX_BOUND_REMOVAL) break;
    }
    printTable(os, {"bound", "at or above", "share", "energy removed"}, rows);
  }

  if (mneeOptions.isEnabled()) {
    os << '\n';
    mMNEE.print(os);
  }

  if (hasBound)
    os << "\nThe bound in force, -max-contribution "
       << smdl::concat(smdl::Brief(path.maxContribution)) << " from " << gate
       << (gate == 1 ? " bounce" : " bounces") << ", scaled " << clampedCount()
       << " contributions ("
       << spellPercent(ratio(double(clampedCount()), double(gatedCount)))
       << ") and removed " << spellPercent(ratio(energyClamped(), energy))
       << " of the energy.\n";
  if (!path.useRoulette)
    os << "\nThe walk was bounded at -max-bounces " << path.maxBounces
       << ", so no path reached past that row.\n";
  if (mneeOptions.isEnabled() && mneeOptions.biasedTrials > 0)
    os << "\nUnder -mnee-biased, arrivals through covered Dirac chains are "
          "dropped before they are tallied.\n";
  if (mNumNonFinite > 0)
    os << '\n'
       << mNumNonFinite
       << " contributions with a non-finite band were discarded, which is a "
          "bug.\n";
}

void PathStats::printJSON(llvm::raw_ostream &os,
                          const PathStatsSession &session,
                          const PathOptions &path,
                          const MNEEOptions &mneeOptions) const {
  llvm::json::OStream json{os, 2};
  json.object([&] {
    // The arrays a reader indexes are written whole, one to a line, as
    // `IO/MeshImport.cc` writes its triples: through `array()` every
    // number would take a line of its own and bury the row it belongs to.
    json.attributeObject("session", [&] {
      json.attributeBegin("window");
      json.rawValue(smdl::concat("[", session.window[0], ", ",
                                 session.window[1], ", ", session.window[2],
                                 ", ", session.window[3], "]"));
      json.attributeEnd();
      json.attribute("spp", int64_t(session.spp));
      json.attribute("sample_index_base", int64_t(session.sampleIndexBase));
    });
    json.attributeObject("bounds", [&] {
      json.attribute("max_bounces", int64_t(path.maxBounces));
      json.attribute("roulette", path.useRoulette);
      attributeDouble(json, "max_contribution", double(path.maxContribution));
      json.attribute("max_contribution_bounces",
                     int64_t(path.maxContributionBounces));
      json.attribute("mnee_biased",
                     mneeOptions.isEnabled() && mneeOptions.biasedTrials > 0);
    });
    json.attribute("samples", int64_t(mNumSamples));
    json.attribute("paths", int64_t(mNumPaths));
    json.attribute("non_finite", int64_t(mNumNonFinite));
    attributeDouble(json, "medium_emission", mMediumEmission);
    attributeDouble(json, "energy", totalEnergy());
    json.attributeObject("clamp", [&] {
      json.attribute("scaled", int64_t(clampedCount()));
      attributeDouble(json, "removed", energyClamped());
    });
    // The lower edge of every bin, so that `bins[b]` below is read
    // against `edges[b]`.
    json.attributeBegin("edges");
    {
      auto edges{std::string("[")};
      for (size_t b = 0; b < NUM_BINS; b++)
        edges += (b > 0 ? ", " : "") + spellDouble(binEdge(b));
      json.rawValue(edges + "]");
    }
    json.attributeEnd();
    json.attributeArray("bounces", [&] {
      for (size_t i = 0; i < mRows.size(); i++) {
        const Row &row{mRows[i]};
        json.object([&] {
          json.attribute("bounces", int64_t(i));
          json.attributeObject("ended", [&] {
            for (size_t e = 0; e < row.ends.size(); e++)
              json.attribute(END_NAMES[e], int64_t(row.ends[e]));
          });
          json.attribute("zero", int64_t(row.zeroCount));
          attributeDouble(json, "largest", row.maxContribution);
          json.attribute("scaled", int64_t(row.clampedCount));
          attributeDouble(json, "removed", row.energyClamped);
          // Each bin as its count and two sums, in that order.
          json.attributeBegin("bins");
          {
            auto bins{std::string("[")};
            for (size_t b = 0; b < NUM_BINS; b++) {
              const Bin &bin{row.bins[b]};
              bins += smdl::concat(b > 0 ? ", [" : "[", bin.count, ", ",
                                   spellDouble(bin.sumAverage), ", ",
                                   spellDouble(bin.sumAverageOverMax), "]");
            }
            json.rawValue(bins + "]");
          }
          json.attributeEnd();
        });
      }
    });
    if (mneeOptions.isEnabled())
      json.attributeObject("mnee", [&] { mMNEE.printJSON(json); });
  });
  os << '\n';
}
