/// \file
/// The `render *` lines the renderer adds to its spectral output's ENVI
/// header, beyond the ones `smdl::SpectralFilm` writes for itself.
#pragma once

#include <cstdint>
#include <map>
#include <string>
#include <vector>

/// What one render sequence has accumulated, and under what settings.
///
/// `-output-spectrum` stamps these onto the header and `-resume` reads
/// them back. Some are a tally the sequence continues across sessions;
/// the rest are a fingerprint of the settings the samples were drawn
/// under, which a resumed session compares against its own and warns
/// about rather than refuses.
///
/// Every field name is spelled exactly once, in the table both
/// directions walk. That is not tidiness. A name written at one site and
/// looked up at another drifts in silence: the read simply does not find
/// the field, the sequence loses its tally or its sample offset, and
/// nothing says so. It has happened here once already.
struct RenderHeader final {
  /// How many sessions have rendered into the sequence.
  uint64_t sessions{};

  /// The wall clock seconds the sequence has cost over every session.
  double seconds{};

  /// The CPU seconds, summed over the worker threads of every session.
  double cpuSeconds{};

  /// The sampler that drew the samples, `SAMPLER_VERSION`.
  std::string sampler{};

  /// The sample index the sequence began at, which `-sample-offset` sets
  /// and every later session keeps, so that a two-seed reference pair
  /// stays decorrelated.
  uint64_t sampleOffset{};

  /// Whether the samples were drawn with `-wavelength-jitter`.
  bool wavelengthJitter{};

  /// The command line that started the first session.
  std::string args{};

  /// The lines to hand `smdl::SpectralFilm::writeENVIFile()`.
  [[nodiscard]] std::vector<std::string> headerLines() const;

  /// Take whatever of these `fields` carries, leaving the rest alone: a
  /// file written before a field existed simply does not set it, which
  /// is what makes an older sequence resumable rather than an error.
  void readFrom(const std::map<std::string, std::string> &fields);
};
