/// \file
/// The sensor's response: the per-sample projection of the spectral film
/// onto named bands, and the tile that picks one band per pixel. The band
/// film this fills is written beside the spectral film and resumed with
/// it. Under a tile it also draws the wavelength a lens whose glasses
/// disperse is traced at.
#pragma once

#include <optional>
#include <string>
#include <vector>

#include "smdl/Support/Span.h"

#include "Color.h"
#include "Layout/SensorFile.h"
#include "Sensor/Sensor.h"

/// The band units, as the band film's header states them: a band counts
/// the photoelectrons its curve turns the spectral irradiance at the
/// sensor into, per square meter and second.
constexpr const char *BAND_UNITS{"electrons/(m^2 s)"};

/// The name of the one band a tiled response writes.
constexpr const char *MOSAIC_BAND_NAME{"mosaic"};

/// The names of the bands a response's film holds: its bands, or the one
/// mosaic band under a tile. On the settings alone, so that a resume can
/// compare a file's bands before the grid exists.
[[nodiscard]] std::vector<std::string>
responseFilmBandNames(const ResponseSettings &settings);

/// The fingerprint of a response's curve set: an MD5 over each band's
/// name and knots in electrons per photon, so that `peak_qe` is in it,
/// and the tile, as text.
[[nodiscard]] std::string responseHash(const ResponseSettings &settings);

/// Where the band film goes beside a spectral film: `out.img` becomes
/// `out-bands.img`, with the header at `out-bands.img.hdr`. Not a suffix
/// on the whole name, because a reader that finds the header by
/// replacing the extension would open the spectral film's.
[[nodiscard]] std::string bandFilmFileName(const std::string &spectrumName);

/// The wavelengths one band traces a lens whose glasses disperse at, in
/// nanometers: the span its draw's density is positive over, and the
/// draw's median.
struct TracedSpan final {
  float lo{};
  float median{};
  float hi{};
};

/// The wavelength a lens whose glasses disperse is traced at, drawn for
/// the pixels of one band. See `Response::traceWavelengthAt()`.
///
/// The density is the band's curve, times the illuminant, times the
/// photon factor. It is held constant between breakpoints, which are the
/// curve's knots, every whole nanometer, and the ends of what the grid
/// sees, each piece at its middle. The curve is linear between its knots,
/// so each piece carries the curve's own integral over it, and the
/// cumulative distribution inverts exactly.
class LensWavelengthDraw final {
public:
  LensWavelengthDraw() = default;

  /// Tabulate the draw of the curve through `knots` and `values` over
  /// `[lo, hi]`, what the grid sees, under `illuminant`. Empty when the
  /// density is zero throughout: a curve the range misses, or one the
  /// illuminant leaves dark.
  LensWavelengthDraw(smdl::Span<const double> knots,
                     smdl::Span<const double> values, double lo, double hi,
                     const SensorSpectrum &illuminant);

  [[nodiscard]] bool isEmpty() const noexcept { return mCDF.empty(); }

  /// The wavelength at the fraction `xi` of the distribution, which rises
  /// with `xi` and lies where the density is positive. Not for an empty
  /// draw.
  [[nodiscard]] double at(double xi) const noexcept;

  /// The span the density is positive over, and the median. Not for an
  /// empty draw.
  [[nodiscard]] TracedSpan span() const noexcept;

private:
  /// The breakpoints, and the cumulative distribution at each, from 0 at
  /// the first to 1 at the last.
  std::vector<double> mWavelengths{};
  std::vector<double> mCDF{};
};

/// What `band` draws for a lens whose glasses disperse, under
/// `illuminant`, over the band's own knots: what a grid spanning the
/// curves sees, which is a physical sensor's default. Nothing for a band
/// the illuminant leaves dark or that is zero throughout.
[[nodiscard]] std::optional<TracedSpan>
tracedSpanOf(const ResponseBand &band, const SensorSpectrum &illuminant);

/// A response resolved against the render-wide wavelength grid: what
/// each sample of spectral irradiance contributes to each band.
///
/// A band is the photon integral of the sample against its curve in
/// electrons per photon, under the rule the sample's wavelengths follow.
/// Without `-wavelength-jitter` the wavelengths are the grid's and the
/// rule is the trapezoid over it, so the band film is exactly the dot
/// product of the spectral film's means with fixed weights; with the
/// jitter each band of the sample covers its own rectangle, as wide as
/// the trapezoid weighs the band, and the curve is evaluated at the
/// sample's own wavelengths, which is what lets a band narrower than the
/// grid integrate without bias. The photon factor `lambda / (h c)` sits
/// inside the integral, which is what makes a readout exact.
///
/// Construction is where the curves meet the grid, and where a band the
/// grid cannot see is refused.
class Response final {
public:
  /// Resolve `settings` against `wavelengths`, the render grid, jittered
  /// when `gRenderGrid` has band edges. Under a tile, also tabulate the
  /// draw of each band the tile lays down under `illuminant`, the white
  /// balance's; see `traceWavelengthAt()`. Warns about a band the grid only
  /// partly sees, about one narrow enough to alias against a grid held
  /// still, and about a tiled one the illuminant leaves dark.
  ///
  /// \throws smdl::Error  If a band has no weight inside the grid, or
  ///                      falls between the wavelengths of a grid held
  ///                      still.
  ///
  Response(const ResponseSettings &settings, const Color &wavelengths,
           const SensorSpectrum &illuminant);

  /// The curves.
  [[nodiscard]] size_t bandCount() const noexcept { return mBands.size(); }

  /// Is there a tile, so that each pixel reads through one band?
  [[nodiscard]] bool hasTile() const noexcept { return mCFAColumns > 0; }

  /// The bands the film holds: one under a tile, else every curve.
  [[nodiscard]] size_t filmBandCount() const noexcept {
    return hasTile() ? 1 : mBands.size();
  }

  /// See `responseFilmBandNames()`.
  [[nodiscard]] const std::vector<std::string> &filmBandNames() const noexcept {
    return mFilmBandNames;
  }

  /// The tile row by row as band names, empty without one.
  [[nodiscard]] const std::vector<std::string> &tileNames() const noexcept {
    return mTileNames;
  }

  [[nodiscard]] size_t tileColumns() const noexcept { return mCFAColumns; }

  /// See `responseHash()`.
  [[nodiscard]] const std::string &hash() const noexcept { return mHash; }

  /// The band that frame pixel `(x, y)` reads through under the tile;
  /// see `tileIndexAt()`. Without a tile, 0.
  [[nodiscard]] size_t bandAt(size_t x, size_t y) const noexcept {
    return hasTile() ? mCFA[tileIndexAt(mCFAColumns, mCFARows, x, y)] : 0;
  }

  /// Add one sample's spectral irradiance `E`, evaluated at
  /// `wavelengths`, into the film-band sums of pixel `(x, y)`: every band
  /// without a tile, the tile's one band with it. `sums` holds
  /// `filmBandCount()` values. Spans rather than `Color`s, so a
  /// heap-sized grid builds nothing.
  void accumulate(smdl::Span<const float> wavelengths,
                  smdl::Span<const float> E, size_t x, size_t y,
                  double *sums) const noexcept;

  /// The wavelength in nanometers a lens whose glasses disperse is traced
  /// at, for a sample of frame pixel `(x, y)`, drawn at `xi` from the band
  /// the tile lays there, or 0, the reference, for a band the illuminant
  /// leaves dark. Defined only under a tile.
  ///
  /// Factor the radiance along the camera ray into its geometry and its
  /// spectrum `s`. The sample's projection onto the band is then the
  /// band's integral of `s`, times the radiance at the geometry of the
  /// traced wavelength, and the draw is independent of the grid's jitter,
  /// so the expectation factors: the band's integral of `s`, times the
  /// average geometry over the draw. That is the band's true integral
  /// exactly when the draw's density is the curve times the photon factor
  /// times `s`, which is what it is for any surface under the illuminant
  /// that has no color of its own: the black-and-white edge a lens's
  /// color is judged on. What is left is color and geometry varying
  /// together inside one band. Each band draws only inside what the grid
  /// sees, from `LensWavelengthDraw`.
  [[nodiscard]] float traceWavelengthAt(size_t x, size_t y,
                                        float xi) const noexcept;

  /// Log, for each band the tile lays down, the span and the median of
  /// the wavelengths its pixels trace a lens that disperses at.
  void logTracedSpans() const;

private:
  /// One curve and what the constructor derived from it.
  struct Band final {
    std::string name{};

    /// The knots in double, in electrons per photon, for the evaluation
    /// and the integrals.
    std::vector<double> wavelengths{};
    std::vector<double> values{};

    /// The per-grid-band weight when the grid holds still, so a sample
    /// projects by one dot product; empty under the jitter.
    std::vector<double> fixedWeights{};

    /// Under a tile, what the band's pixels trace a dispersive lens at;
    /// empty without a tile, and for a band the tile does not lay down.
    LensWavelengthDraw draw{};
  };

  /// The band's curve at `lambda`; see `curveAt()`.
  [[nodiscard]] static double evaluate(const Band &band,
                                       double lambda) noexcept {
    return curveAt(band.wavelengths, band.values, lambda);
  }

  /// The curve integrated over `[lo, hi]`, exactly, by the trapezoid rule
  /// over its knots clipped to the range.
  [[nodiscard]] static double integrate(const Band &band, double lo,
                                        double hi) noexcept;

  /// One sample's projection onto one band.
  [[nodiscard]] double project(const Band &band,
                               smdl::Span<const float> wavelengths,
                               smdl::Span<const float> E) const noexcept;

  std::string mHash{};

  std::vector<Band> mBands{};

  std::vector<std::string> mFilmBandNames{};

  size_t mCFAColumns{};

  size_t mCFARows{};

  std::vector<size_t> mCFA{};

  std::vector<std::string> mTileNames{};

  /// Is the grid jittered, so that the curve is evaluated per sample?
  bool mIsJittering{};

  /// The width of each grid band's rectangle under the jitter, empty
  /// otherwise.
  std::vector<double> mWidths{};
};

/// The response of a frame resolved against the grid, under the white
/// balance's `illuminant`, or nothing without one. See `Response`.
[[nodiscard]] std::optional<Response>
resolveResponse(const std::optional<ResponseSettings> &settings,
                const Color &wavelengths, const SensorSpectrum &illuminant);
