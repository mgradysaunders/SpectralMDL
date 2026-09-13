/// \file
/// The sensor's response: the per-sample projection of the spectral film
/// onto named bands, and the tile that picks one band per pixel. The band
/// film this fills is written beside the spectral film and resumed with
/// it. Under a tile it also draws the wavelength a lens whose glasses
/// disperse is traced at.
#pragma once

#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "smdl/RenderUtil/MonteCarlo.h"
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

/// A span as both the render's log and the camera's report spell it:
/// `"<lo>-<hi> nm, median <median>"`.
[[nodiscard]] std::string spellTracedSpan(const TracedSpan &span);

/// A piecewise-constant density over wavelength, sampled by the inverse
/// of its cumulative distribution. Its breakpoints are a curve's knots,
/// every whole nanometer, and the ends of the range, each piece at its
/// middle. Two densities are built over them: `ofResponse()`, what the
/// pixels of one band trace a lens whose glasses disperse at, see
/// `Response::traceWavelengthAt()`; and `ofPlacement()`, what a grid
/// placed by the curve tiles its cells by, see `cells()`.
class WavelengthDensity final {
public:
  WavelengthDensity() = default;

  /// The response density of the curve through `knots` and `values`
  /// over `[lo, hi]`, what the grid sees, under `illuminant`: the curve,
  /// times the illuminant, times the photon factor. The curve is linear
  /// between its knots, so each piece carries the curve's own integral
  /// over it, and the cumulative distribution inverts exactly. Empty
  /// when the density is zero throughout: a curve the range misses, or
  /// one the illuminant leaves dark.
  [[nodiscard]] static WavelengthDensity
  ofResponse(smdl::Span<const double> knots, smdl::Span<const double> values,
             double lo, double hi, const SensorSpectrum &illuminant);

  /// One curve of a placement: its knots and values.
  struct PlacementCurve final {
    smdl::Span<const double> knots{};
    smdl::Span<const double> values{};
  };

  /// The placement density of `curves` over `[lo, hi]`: half the sum
  /// over the curves, each at its peak, of the slope of the curve times
  /// the photon factor to the two thirds power, and half uniform over
  /// the range. Never empty.
  ///
  /// A grid's band estimates its integral from the curve at one point
  /// drawn uniformly across the cell, weighed by the cell's width, so
  /// its variance follows not the curve's mass in the cell but its
  /// variation across it: the slope times the width, squared, times the
  /// width squared again for the weight. Cells narrow where the curve is
  /// steep and wide where it is flat minimize that sum, and the slope to
  /// the two thirds is the density that places them, the photon factor's
  /// own slope keeping a plateau from collapsing into one cell. Cells of
  /// equal mass in the curve itself, the draw that suits a sample weighed
  /// by its own probability, leave a band's steep skirts in wide cells
  /// and come out two to three times worse than a uniform grid. The
  /// uniform half is a floor: no cell is wider than twice a uniform
  /// grid's, so a scene spectrum climbing across a band's far tail,
  /// where the curve is flat and nearly nothing, is never left in one
  /// cell. Half is where the placement stops being worse than uniform on
  /// a ramp of a percent per nanometer over every shipped curve, alone
  /// or every band of a sensor from one grid, at almost no cost on a
  /// smooth spectrum. Summing the curves' slopes rather than taking the
  /// slope of their sum is what lets one grid serve every band: each
  /// band's skirts buy their cells whatever the others do there.
  [[nodiscard]] static WavelengthDensity
  ofPlacement(smdl::Span<const PlacementCurve> curves, double lo, double hi);

  [[nodiscard]] bool isEmpty() const noexcept { return mWavelengths.empty(); }

  /// The wavelength at the fraction `xi` of the distribution, which rises
  /// with `xi` and lies where the density is positive. Not for an empty
  /// density.
  [[nodiscard]] double at(float xi) const noexcept;

  /// The span the draw can land in, which is where the density is
  /// positive, and the median. Not for an empty density.
  [[nodiscard]] TracedSpan span() const noexcept;

  /// `count` cells of equal mass tiling the span, each labeled by its
  /// midpoint, where a curve linear across the cell takes its mean. Not
  /// for an empty density.
  [[nodiscard]] WavelengthCells cells(size_t count) const;

private:
  /// The pieces' masses over `breakpoints`; empty when there is no mass
  /// at all.
  WavelengthDensity(std::vector<double> breakpoints,
                    smdl::Span<const float> masses);

  /// The breakpoints over `[lo, hi]` held inside the sorted `knots`: the
  /// knots between, every whole nanometer, and the ends; empty when
  /// nothing is left of the range.
  [[nodiscard]] static std::vector<double>
  breakpointsOf(smdl::Span<const double> knots, double lo, double hi);

  /// The first and last piece with mass.
  [[nodiscard]] std::pair<size_t, size_t> massRange() const noexcept;

  /// The breakpoints.
  std::vector<double> mWavelengths{};

  /// The mass of each piece between two breakpoints, one fewer than the
  /// breakpoints, which the draw picks a piece from.
  smdl::Distribution1D mPieces{};
};

/// What `band` draws for a lens whose glasses disperse, under
/// `illuminant`, over the band's own knots: what a grid spanning the
/// curves sees. Nothing for a band the illuminant leaves dark or that is
/// zero throughout.
[[nodiscard]] std::optional<TracedSpan>
tracedSpanOf(const ResponseBand &band, const SensorSpectrum &illuminant);

/// The grid `band` places itself: `count` cells over its knots by
/// `WavelengthDensity::ofPlacement()`. The illuminant has no part in it,
/// so the grid is the sensor's alone and a resumed sequence finds the
/// same one under any white balance.
[[nodiscard]] WavelengthCells bandWavelengthCells(const ResponseBand &band,
                                                  size_t count);

/// The grid a response places itself for every band at once: `count`
/// cells over the union of its bands' knots by
/// `WavelengthDensity::ofPlacement()` of every curve, for a sensor
/// without a tile, whose every sample projects onto every band.
[[nodiscard]] WavelengthCells
responseWavelengthCells(const ResponseSettings &settings, size_t count);

/// A response resolved against the render's wavelength grids: what
/// each sample of spectral irradiance contributes to each band.
///
/// A band is the photon integral of the sample against its curve in
/// electrons per photon, under the rule the sample's wavelengths follow
/// on the grid the band projects on: under a tile the grid of its own
/// pixels, else the one grid. Without `-wavelength-jitter` the
/// wavelengths are the grid's and the rule is its widths, so the band
/// film is exactly the dot product of the spectral film's means with
/// fixed weights; with the jitter each band of the sample covers its
/// own cell, as wide as the grid weighs the band, and the curve is
/// evaluated at the
/// sample's own wavelengths, which is what lets a band narrower than the
/// grid integrate without bias. The photon factor `lambda / (h c)` sits
/// inside the integral, which is what makes a readout exact.
///
/// Construction is where the curves meet the grid, and where a band the
/// grid cannot see is refused.
class Response final {
public:
  /// Resolve `settings` against `gRenderGrid`, jittered when it
  /// jitters. Under a tile, also tabulate the draw of each band the tile
  /// lays down under `illuminant`, the white balance's; see
  /// `traceWavelengthAt()`. Warns about a band its grid only partly
  /// sees, about one narrow enough to alias against a grid held still,
  /// and about a tiled one the illuminant leaves dark. A band the tile
  /// does not lay down projects never, and is left alone.
  ///
  /// \throws smdl::Error  If a band has no weight inside its grid, or
  ///                      falls between the wavelengths of a grid held
  ///                      still.
  ///
  Response(const ResponseSettings &settings, const SensorSpectrum &illuminant);

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

  /// The names of the bands the tile lays down, once each in the order
  /// it first names them, which is the order of the render's grids under
  /// a tile; empty without one.
  [[nodiscard]] const std::vector<std::string> &tileBandNames() const noexcept {
    return mTileBandNames;
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
  /// sees, from `WavelengthDensity`.
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

    /// The width of each cell of the band's grid under the jitter; empty
    /// otherwise.
    std::vector<double> widths{};

    /// Under a tile, what the band's pixels trace a dispersive lens at;
    /// empty without a tile, and for a band the tile does not lay down.
    WavelengthDensity draw{};
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

  std::vector<std::string> mTileBandNames{};

  /// Is the grid jittered, so that the curve is evaluated per sample?
  bool mIsJittering{};
};

/// The response of a frame resolved against the grids, under the white
/// balance's `illuminant`, or nothing without one. See `Response`.
[[nodiscard]] std::optional<Response>
resolveResponse(const std::optional<ResponseSettings> &settings,
                const SensorSpectrum &illuminant);
