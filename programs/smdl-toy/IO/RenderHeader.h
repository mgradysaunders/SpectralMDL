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
  bool hasWavelengthJitter{};

  /// The command line that started the first session.
  std::string args{};

  /// The lines to hand `smdl::SpectralFilm::writeENVIFile()`.
  [[nodiscard]] std::vector<std::string> headerLines() const;

  /// Take whatever of these `fields` carries, leaving the rest alone: a
  /// file written before a field existed simply does not set it, which
  /// is what makes an older sequence resumable rather than an error.
  void readFrom(const std::map<std::string, std::string> &fields);
};

/// What the band film's header says about the response that produced
/// it: the fingerprint a resumed session has to match, since the curves
/// decide what the file's numbers are, the way the resolution decides
/// what its pixels are. Written and read through one table, as
/// `RenderHeader` is and for the same reason.
struct ResponseHeader final {
  /// The response's kind, `relative` or `qe`.
  std::string kind{};

  /// The hash of the curve set; see `responseHash()`.
  std::string hash{};

  /// The tile's width in pixels, 0 without a tile.
  uint64_t cfaColumns{};

  /// The tile row by row as band names, empty without a tile.
  std::vector<std::string> cfa{};

  /// The lines to hand `smdl::SpectralFilm::writeENVIFile()`.
  [[nodiscard]] std::vector<std::string> headerLines() const;

  /// Take whatever of these `fields` carries, leaving the rest alone.
  void readFrom(const std::map<std::string, std::string> &fields);
};

/// What the readout's header says about how its digital numbers were
/// computed: every factor between the band film and the file, so that a
/// reader can recover the electrons, and which realization it is.
/// Nothing resumes a readout, so this is written for the reader; it
/// still goes through one table, so the doctest can read it back.
struct DetectorHeader final {
  /// The realization, `-detector-seed`.
  uint64_t seed{};

  /// Which noise was drawn, `-detector-noise`.
  std::string noise{};

  /// The exposure in seconds.
  double exposure{};

  /// The pixel pitch in micrometers, across and down.
  ///
  /// \{
  double pixelWidth{};
  double pixelHeight{};
  /// \}

  /// The f-number the irradiance came through.
  double fNumber{};

  /// The well in electrons, stated or derived.
  double fullWell{};

  /// The read noise in electrons rms.
  double readNoise{};

  /// The dark electrons at this exposure and temperature.
  double darkElectrons{};

  /// The gain in digital numbers per electron, stated or derived.
  double gain{};

  /// The black level in electrons.
  double blackLevel{};

  /// The ADC's depth.
  uint64_t bits{};

  /// The signal electrons one unit of the band film is worth: the pixel
  /// area, the exposure, and the irradiance scale together, so that a
  /// reader recovers the electrons from the band film alone.
  double electronsPerFilmUnit{};

  /// The share of the window's pixel bands whose render noise, by the
  /// film's own estimate, exceeded their shot noise.
  double noiseLimitedShare{};

  /// The lines to hand `smdl::writeENVIFileUInt16()`.
  [[nodiscard]] std::vector<std::string> headerLines() const;

  /// Take whatever of these `fields` carries, leaving the rest alone.
  void readFrom(const std::map<std::string, std::string> &fields);
};
