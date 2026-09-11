/// \file
/// The detector's response: the per-sample projection of spectral
/// radiance onto named bands, and the tile that picks one band per
/// pixel. The band film this fills is written beside the spectral film
/// and resumed with it.
#pragma once

#include <optional>
#include <string>
#include <vector>

#include "smdl/Support/Span.h"

#include "Color.h"
#include "Layout/CameraFile.h"

/// Planck's constant in joule seconds and the speed of light in meters
/// per second, both exact by definition.
///
/// \{
constexpr double PLANCK{6.62607015e-34};
constexpr double SPEED_OF_LIGHT{2.99792458e8};
/// \}

/// The band units of each response kind, as the band film's header
/// states them: a `relative` band is a radiance averaged over its curve,
/// and a `qe` band counts the electrons the curve turns that radiance
/// into, per square meter, steradian, and second.
///
/// \{
constexpr const char *RELATIVE_BAND_UNITS{"W/(m^2 sr nm)"};
constexpr const char *QE_BAND_UNITS{"electrons/(m^2 sr s)"};
/// \}

/// The name of the one band a tiled response writes.
constexpr const char *MOSAIC_BAND_NAME{"mosaic"};

/// The names of the bands a response's film holds: its bands, or the one
/// mosaic band under a tile. On the settings alone, so that a resume can
/// compare a file's bands before the grid exists.
[[nodiscard]] std::vector<std::string>
responseFilmBandNames(const ResponseSettings &settings);

/// The fingerprint of a response's curve set: an MD5 over the kind, each
/// band's name and knots, and the tile, as text. Not the free-text
/// `name`, so relabeling a sensor does not invalidate a sequence.
[[nodiscard]] std::string responseHash(const ResponseSettings &settings);

/// Where the band film goes beside a spectral film: `out.img` becomes
/// `out-bands.img`, with the header at `out-bands.img.hdr`. Not a suffix
/// on the whole name, because a reader that finds the header by
/// replacing the extension would open the spectral film's.
[[nodiscard]] std::string bandFilmFileName(const std::string &spectrumName);

/// A response resolved against the render-wide wavelength grid: what
/// each sample of spectral radiance contributes to each band.
///
/// A band is the quadrature of the sample's radiance against its curve,
/// under the rule the sample's wavelengths follow. Without
/// `-wavelength-jitter` the wavelengths are the grid's and the rule is
/// the trapezoid over it, so the band film is exactly the dot product
/// of the spectral film's means with fixed weights; with the jitter each
/// band of the sample covers its own rectangle, whose width is the
/// weight, and the curve is evaluated at the sample's own wavelengths,
/// which is what lets a band narrower than the grid integrate without
/// bias. A `relative` band divides by the same quadrature of the curve
/// alone (the grid's sum, or the analytic integral over the jitter's
/// extent, which that sum estimates), so a flat spectrum passes through
/// as itself; a `qe` band multiplies by photons per joule instead and
/// divides by nothing.
///
/// Construction is where the curves meet the grid, and where a band the
/// grid cannot see is refused.
class Response final {
public:
  /// Resolve `settings` against `wavelengths`, the render grid, and
  /// `gRenderGrid`'s jitter rectangles. Warns about a band the grid only
  /// partly sees and about one narrow enough to alias against a grid
  /// held still.
  ///
  /// \throws smdl::Error  If a band has no weight inside the grid, or
  ///                      falls between the wavelengths of a grid held
  ///                      still.
  ///
  Response(const ResponseSettings &settings, const Color &wavelengths);

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

  /// The kind, as the grammar spells it.
  [[nodiscard]] const char *kindName() const noexcept {
    return mKind == ResponseKind::QE ? "qe" : "relative";
  }

  /// The band units, as the header states them.
  [[nodiscard]] const char *units() const noexcept {
    return mKind == ResponseKind::QE ? QE_BAND_UNITS : RELATIVE_BAND_UNITS;
  }

  /// The sensor's name from the settings, possibly empty.
  [[nodiscard]] const std::string &name() const noexcept { return mName; }

  /// See `responseHash()`.
  [[nodiscard]] const std::string &hash() const noexcept { return mHash; }

  /// The band that frame pixel `(x, y)` reads through under the tile,
  /// anchored at the frame's origin so a crop window changes nothing.
  /// Without a tile, 0.
  [[nodiscard]] size_t bandAt(size_t x, size_t y) const noexcept {
    return hasTile() ? mCFA[(y % mCFARows) * mCFAColumns + x % mCFAColumns] : 0;
  }

  /// Add one sample's radiance `L`, evaluated at `wavelengths`, into the
  /// film-band sums of pixel `(x, y)`: every band without a tile, the
  /// tile's one band with it. `sums` holds `filmBandCount()` values.
  /// Spans rather than `Color`s, so a heap-sized grid builds nothing.
  void accumulate(smdl::Span<const float> wavelengths,
                  smdl::Span<const float> L, size_t x, size_t y,
                  double *sums) const noexcept;

private:
  /// One curve and what the constructor derived from it.
  struct Band final {
    std::string name{};

    /// The knots in double, for the evaluation and the integrals.
    std::vector<double> wavelengths{};
    std::vector<double> values{};

    /// One over the normalizer: 1 for `qe`, else the curve's own
    /// quadrature under the sample's rule.
    double scale{1};

    /// The per-grid-band weight when the grid holds still, so a sample
    /// projects by one dot product; empty under the jitter.
    std::vector<double> fixedWeights{};
  };

  /// The curve at `lambda`, zero outside its knots.
  [[nodiscard]] static double evaluate(const Band &band,
                                       double lambda) noexcept;

  /// The curve integrated over `[lo, hi]`, exactly, by the trapezoid rule
  /// over its knots clipped to the range.
  [[nodiscard]] static double integrate(const Band &band, double lo,
                                        double hi) noexcept;

  /// Photons per joule at `lambda` nanometers under `qe`, else 1.
  [[nodiscard]] double photonsPerJoule(double lambda) const noexcept;

  /// One sample's projection onto one band.
  [[nodiscard]] double project(const Band &band,
                               smdl::Span<const float> wavelengths,
                               smdl::Span<const float> L) const noexcept;

  ResponseKind mKind{ResponseKind::RELATIVE};

  std::string mName{};

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

/// The response of a frame resolved against the grid, or nothing without
/// one. See `Response`.
[[nodiscard]] std::optional<Response>
resolveResponse(const std::optional<ResponseSettings> &settings,
                const Color &wavelengths);
