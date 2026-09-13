/// \file
/// Continuing a render sequence across sessions.
#pragma once

#include <cstddef>

#include "smdl/RenderUtil/SpectralFilm.h"

#include "Common.h"
#include "IO/RenderHeader.h"

struct Options;
struct Frame;
class ResponseSettings;

/// What `-resume` found: a prior session's accumulation, and the record
/// of the sequence it belongs to.
///
/// The sampler is deterministic in (pixel, sample index) with no seed,
/// so continuing the sample index where the file left off and merging
/// afterward yields the same estimator as one longer uninterrupted run.
/// That is the whole reason a sequence can be paused at all.
struct ResumedSequence final {
  /// Was `-resume` given?
  bool wasRequested{};

  /// Was a sequence actually loaded? False both when `-resume` was not
  /// given and when it named a file that does not exist yet, which is
  /// how a sequence is started rather than an error.
  bool wasLoaded{};

  /// What the prior sessions accumulated, empty unless `loaded`: the
  /// observer's spectral film, or through a sensor its band film, which
  /// is the kind this session's camera makes. It is merged into the
  /// render's own film before the first sample, so that every preview
  /// written along the way already stands on every sample taken;
  /// `clear()` it once that is done, since it is as large as the film
  /// being rendered into.
  smdl::SpectralFilm film{};

  /// The header the file carried, including the wavelength grid a
  /// session given no grid flags adopts.
  smdl::SpectralFilm::ENVIFileInfo info{};

  /// The grids the file states beyond its wavelength list: the one
  /// grid's edges, and its wavelengths when the file is a band film,
  /// or under a tile one grid per tile band; nothing for a file written
  /// before they were recorded, whose cells are the ones its wavelengths
  /// imply.
  GridHeader grids{};

  /// The sequence's tally, its fingerprint, and the meter's record,
  /// seeded from the file and added to by this session before being
  /// written back.
  RenderHeader header{};

  /// The response a band film recorded; empty unless a response is
  /// present and the film loaded, in which case it matched this one,
  /// since a band film drawn under other curves is an error rather than
  /// a gap.
  ResponseHeader responseHeader{};

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
/// `frame` is this render's resolved frame: the resolution and the
/// window the file must match, the film quantity it must hold, and the
/// jitter it is compared against.
///
/// `response` is this render's sensor response, or null for none. With
/// one, the file must be a band film of the same bands drawn under the
/// same curves by hash, both hard errors: the curves decide what the
/// file's numbers are, and a re-run of the output stage would relabel a
/// mixture as one sensor with nothing to say so.
///
/// \throws smdl::Error  If the file cannot be resumed from.
///
[[nodiscard]] ResumedSequence resumeSequence(const Options &opts,
                                             const Frame &frame,
                                             const ResponseSettings *response);
