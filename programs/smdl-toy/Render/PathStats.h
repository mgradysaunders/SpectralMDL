/// \file
/// The tally behind `-report`: what every contribution of a render
/// added, by size and bounce count, and where every path ended.
#pragma once

#include <array>
#include <cstddef>
#include <cstdint>
#include <vector>

#include "Color.h"
#include "Common.h"

#include "smdl/Manifold.h"

namespace llvm {
class raw_ostream;
namespace json {
class OStream;
} // namespace json
} // namespace llvm

class MNEEOptions;
class PathOptions;

/// Why a path walk ended.
enum class PathEnd : int {
  /// The walk left the scene.
  ESCAPED,

  /// A phase function or BSDF sample failed, so the throughput went to
  /// zero.
  ABSORBED,

  /// Russian roulette retired the walk.
  ROULETTE,

  /// The walk reached `PathOptions::maxBounces`.
  BOUND,

  /// The throughput went non-finite, which is a bug and not transport.
  FAILED,

  NUM_ENDS
};

/// The render a tally describes, which the tally cannot know from the
/// paths alone and the report's header states.
struct PathStatsSession final {
  /// The pixel window rendered, `x0,y0,x1,y1`.
  int4 window{};

  /// The samples per pixel drawn.
  uint64_t spp{};

  /// The sample index the session started from.
  uint64_t sampleIndexBase{};
};

/// The manifold estimators' counters of the tally, recorded at every
/// gather, walk, and arrival the estimators run, and printed as the
/// report's manifold section under `-mnee`. Plain counters, one set per
/// block like the rest of the tally, so recording shares nothing.
struct MNEEStats final {
  /// Which gather and kind an estimate belongs to: the straight-line
  /// refractive gather's two kinds, the searched refractive gather's,
  /// and the reflective gather's.
  enum Kind : int {
    DIRAC_REFRACT,
    GLOSSY_REFRACT,
    CASTER_DIRAC_REFRACT,
    CASTER_GLOSSY_REFRACT,
    DIRAC_REFLECT,
    GLOSSY_REFLECT,
    NUM_KINDS
  };

  /// What became of a Dirac-chain arrival the coverage weighed: the
  /// re-walk reproduced the crossings the path took, it did not and
  /// the arrival kept weight 1, or the searched refractive gather owned
  /// the chain's family and the arrival was dropped.
  enum class Cover { MATCHED, UNMATCHED, DROPPED };

  /// The counters of one kind.
  struct KindCounts final {
    /// The gathers that reached their first walk, and how many of those
    /// first walks converged.
    uint64_t estimateCount{};
    uint64_t firstConvergedCount{};

    /// The reciprocal estimates run, which a first solution that carried
    /// transport gets; the trials they took in all and at most; and how
    /// many ran out and dropped the sample.
    uint64_t trialEstimateCount{};
    uint64_t trialCount{};
    uint64_t trialsMax{};
    uint64_t capDropCount{};
  };

  /// The bins of the converged-iteration histogram: one per iteration
  /// count, the last holding every count from it up.
  static constexpr size_t NUM_ITERATION_BINS{65};

  std::array<KindCounts, NUM_KINDS> kinds{};

  /// The walks, by outcome and by failure.
  uint64_t walkCount{};
  uint64_t walkConvergedCount{};
  uint64_t walkRejectedCount{};
  std::array<uint64_t, size_t(smdl::ManifoldWalkReport::Failure::NUM_FAILURES)>
      walkFailureCounts{};

  /// The iterations over every walk, in all and at most.
  uint64_t walkIterations{};
  uint64_t walkIterationsMax{};

  /// The iterations of the walks that reached convergence (the rejected
  /// ones included: they converged first), bucketed by count, which is
  /// what sizing the iteration budget needs; the average above mixes in
  /// the early failures.
  std::array<uint64_t, NUM_ITERATION_BINS> convergedIterationCounts{};

  /// The residual of the converged walks, summed and at most.
  double walkResidual{};
  double walkResidualMax{};

  /// The re-walks the arrival side ran for MIS, and how many converged.
  uint64_t rewalkCount{};
  uint64_t rewalkConvergedCount{};

  /// The Dirac-chain arrivals the coverage weighed: how many the
  /// re-walk matched, and how many were dropped as the searched
  /// refractive gather's. The rest keeps weight 1, so the matched share
  /// is what the straight-line gather can ever claim.
  uint64_t coverArrivalCount{};
  uint64_t coverMatchedCount{};
  uint64_t coverDroppedCount{};

  /// The converged connections weighed, and how many came to anything.
  uint64_t contributionCount{};
  uint64_t contributionNonZeroCount{};

  /// A gather that reached its first walk, and whether that walk
  /// converged.
  void recordEstimate(Kind kind, bool isFirstWalkConverged) noexcept;

  /// One walk of a gather, first or trial.
  void recordWalk(const smdl::ManifoldWalkReport &report) noexcept;

  /// One re-walk the arrival side ran for MIS.
  void recordRewalk(const smdl::ManifoldWalkReport &report) noexcept;

  /// One Dirac-chain arrival the coverage weighed, and what became of
  /// it.
  void recordCover(Cover cover) noexcept;

  /// The reciprocal estimate of one gather: how many trials it took to
  /// re-find the solution, or that it ran out and dropped the sample.
  void recordTrials(Kind kind, int trials, bool wasDropped) noexcept;

  /// A converged connection weighed, and whether anything came of it.
  void recordContribution(bool isNonZero) noexcept;

  void add(const MNEEStats &other) noexcept;

  /// The iterations within which the fraction `p` of the walks that
  /// reached convergence did so, zero without any.
  [[nodiscard]] size_t iterationsPercentile(double p) const noexcept;

  /// Print the manifold section of the report.
  void print(llvm::raw_ostream &os) const;

  /// Write the counters as the attributes of the open JSON object.
  void printJSON(llvm::json::OStream &json) const;
};

/// The tally behind `-report`: two histograms over the bounce count
/// `PathOptions` defines.
///
/// The first is what every contribution added, binned by its largest
/// band, which is the scalar `-max-contribution` bounds. The bins are
/// log spaced in the 1-2-3-5 series, so that a bound read off the table
/// is a number a person types, and each carries the two sums that make
/// the energy a clamp at a bin edge removes exact: a clamp at X scales
/// a contribution of largest band M above X by X / M, and so removes
/// average * (1 - X / M) of its energy, which summed over a bin is
/// `sumAverage - X * sumAverageOverMax`. Energy throughout is the band
/// average, per band like the bound and the spectral output's units.
/// Contributions are tallied before the clamp, so a tally taken with a
/// bound in force is the unbounded render's, and what the bound did is
/// tallied beside it.
///
/// The second is where every path ended, by reason, at the bounce count
/// of the deepest contribution the path could have made. The cumulative
/// energy through a bounce count is then what `-max-bounces` at that
/// count keeps, and since roulette is unbiased that holds for the
/// default walk too.
///
/// Under `-mnee` the manifold estimators' counters ride along; see
/// `MNEEStats`.
///
/// One tally per block of paths, which the render adds into its own at
/// the block's end; nothing here is thread safe.
class PathStats final {
public:
  /// One magnitude bin: how many contributions landed in it and the two
  /// sums the removed-energy identity reads.
  struct Bin final {
    uint64_t count{};

    /// The sum of the contributions' band averages: their energy.
    double sumAverage{};

    /// The sum of each contribution's band average over its largest
    /// band, at most `count`.
    double sumAverageOverMax{};
  };

  /// The bins per row: an underflow bin below the first edge, the
  /// 1-2-3-5 series across the twenty decades from 1e-10, and an
  /// overflow bin from 1e10 up.
  static constexpr size_t NUM_BINS{82};

  /// One bounce count: its magnitude bins, its zero contributions, the
  /// largest contribution seen, what the bound in force did, and how
  /// many paths ended here by each reason.
  struct Row final {
    std::array<Bin, NUM_BINS> bins{};

    /// The contributions with no positive band, which count but have no
    /// magnitude.
    uint64_t zeroCount{};

    /// The largest band of any contribution, before the clamp.
    double maxContribution{};

    /// The contributions the bound in force scaled.
    uint64_t clampedCount{};

    /// The energy the bound in force removed.
    double energyClamped{};

    /// The paths that ended here, by `PathEnd`.
    std::array<uint64_t, size_t(PathEnd::NUM_ENDS)> ends{};

    /// The non-zero contributions.
    [[nodiscard]] uint64_t contributionCount() const noexcept;

    /// The energy of the contributions.
    [[nodiscard]] double energy() const noexcept;

    /// The paths that ended here.
    [[nodiscard]] uint64_t pathCount() const noexcept;
  };

  /// The bin a contribution whose largest band is `maxValue` lands in.
  [[nodiscard]] static size_t binIndex(double maxValue) noexcept;

  /// The lower edge of bin `bin`, which holds `[binEdge(bin),
  /// binEdge(bin + 1))`: zero for the underflow bin, and infinite past
  /// the last bin.
  [[nodiscard]] static double binEdge(size_t bin) noexcept;

  /// Tally a contribution of `bounces` bounces, before the clamp.
  void recordContribution(uint64_t bounces, const Color &contribution);

  /// Tally what the bound in force did to a contribution of `bounces`
  /// bounces it scaled by `scale`, which is below one.
  void recordClamp(uint64_t bounces, const Color &contribution, float scale);

  /// Tally volume emission, which no bound applies to and no bin holds.
  void recordMediumEmission(const Color &emission);

  /// Tally a path that ended by `end` at `bounces`.
  void recordPath(uint64_t bounces, PathEnd end);

  /// Count camera samples, traced or not.
  void addSamples(uint64_t numSamples) noexcept { mNumSamples += numSamples; }

  /// Add another tally into this one.
  void add(const PathStats &other);

  /// The manifold estimators' counters, which the gathers and the
  /// arrival side record under `-mnee`.
  ///
  /// \{
  [[nodiscard]] MNEEStats &mnee() noexcept { return mMNEE; }

  [[nodiscard]] const MNEEStats &mnee() const noexcept { return mMNEE; }
  /// \}

  /// The rows, one per bounce count from zero to the deepest seen.
  [[nodiscard]] const std::vector<Row> &rows() const noexcept { return mRows; }

  [[nodiscard]] uint64_t sampleCount() const noexcept { return mNumSamples; }

  [[nodiscard]] uint64_t pathCount() const noexcept { return mNumPaths; }

  /// The contributions discarded for a non-finite band.
  [[nodiscard]] uint64_t nonFiniteCount() const noexcept {
    return mNumNonFinite;
  }

  /// The volume emission, which is energy outside every row.
  [[nodiscard]] double mediumEmission() const noexcept {
    return mMediumEmission;
  }

  /// The energy of every contribution before the clamp, volume emission
  /// included.
  [[nodiscard]] double totalEnergy() const noexcept;

  /// The non-zero contributions of at least `minBounces` bounces, and
  /// their energy.
  ///
  /// \{
  [[nodiscard]] uint64_t
  contributionCountFrom(uint64_t minBounces) const noexcept;

  [[nodiscard]] double energyFrom(uint64_t minBounces) const noexcept;
  /// \}

  /// Of the non-zero contributions of at least `minBounces` bounces,
  /// those whose largest band is at or above `bound`, and the energy a
  /// clamp at `bound` removes from them. Exact when `bound` is a bin
  /// edge, which the report's rows are; between edges the partial bin
  /// is left out.
  ///
  /// \{
  [[nodiscard]] uint64_t countAbove(double bound,
                                    uint64_t minBounces) const noexcept;

  [[nodiscard]] double energyRemovedAbove(double bound,
                                          uint64_t minBounces) const noexcept;
  /// \}

  /// What the bound in force did over every row.
  ///
  /// \{
  [[nodiscard]] uint64_t clampedCount() const noexcept;

  [[nodiscard]] double energyClamped() const noexcept;
  /// \}

  /// The mean bounces per path, zero without paths.
  [[nodiscard]] double meanBounces() const noexcept;

  /// The deepest bounce count any path ended at, zero without paths.
  [[nodiscard]] uint64_t maxBouncesReached() const noexcept;

  /// Print the report: the header, the per-bounce table, the bound
  /// table, and the notes that apply.
  void print(llvm::raw_ostream &os, const PathStatsSession &session,
             const PathOptions &path, const MNEEOptions &mneeOptions) const;

  /// Print the tally as one JSON document, every bin included, so that a
  /// script can evaluate any bound and gate exactly.
  void printJSON(llvm::raw_ostream &os, const PathStatsSession &session,
                 const PathOptions &path, const MNEEOptions &mneeOptions) const;

private:
  /// The row of `bounces`, grown to on first touch.
  [[nodiscard]] Row &rowAt(uint64_t bounces);

  std::vector<Row> mRows{};

  uint64_t mNumSamples{};

  uint64_t mNumPaths{};

  uint64_t mNumNonFinite{};

  double mMediumEmission{};

  MNEEStats mMNEE{};
};
