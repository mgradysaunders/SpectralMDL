/// \file
/// Continuing a render sequence across sessions.
#pragma once

#include <cstddef>

#include "smdl/RenderUtil/SpectralFilm.h"

#include "Common.h"
#include "IO/RenderHeader.h"

struct Options;

/// What `-resume` found: a prior session's accumulation, and the record
/// of the sequence it belongs to.
///
/// The sampler is deterministic in (pixel, sample index) with no seed,
/// so continuing the sample index where the file left off and merging
/// afterward yields the same estimator as one longer uninterrupted run.
/// That is the whole reason a sequence can be paused at all.
struct ResumedSequence final {
  /// Was `-resume` given?
  bool requested{};

  /// Was a sequence actually loaded? False both when `-resume` was not
  /// given and when it named a file that does not exist yet, which is
  /// how a sequence is started rather than an error.
  bool loaded{};

  /// What the prior sessions accumulated, empty unless `loaded`. It is
  /// merged into the render's own film before the first sample, so that
  /// every preview written along the way already stands on every sample
  /// taken; `clear()` it once that is done, since it is as large as the
  /// film being rendered into.
  smdl::SpectralFilm film{};

  /// The header the file carried, including the wavelength grid a
  /// session given no grid flags adopts.
  smdl::SpectralFilm::ENVIFileInfo info{};

  /// The sequence's tally and fingerprint, seeded from the file and
  /// added to by this session before being written back.
  RenderHeader header{};

  /// The sample index this session starts drawing at.
  size_t sampleIndexBase{};
};

/// Load the sequence `-resume` names, before anything slow happens, so
/// that a mismatched file fails fast.
///
/// A wholly missing data-plus-header pair is not an error: it makes this
/// run the first session of an intended sequence. Half a pair is a
/// damaged prior session, and starting fresh over it would clobber what
/// is left, so that stays fatal. Everything the sequence must hold
/// constant (the resolution, the window) is a hard error; everything it
/// merely ought to (the sampler, the jitter, the flags) is a warning.
///
/// \throws smdl::Error  If the file cannot be resumed from.
///
[[nodiscard]] ResumedSequence resumeSequence(const Options &opts,
                                             int2 resolution, int4 window);
