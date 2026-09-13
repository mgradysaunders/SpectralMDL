#include <algorithm>
#include <cmath>
#include <cstdint>
#include <cstdio>

#include "smdl/Support/Error.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/MD5Hash.h"
#include "smdl/Support/Strings.h"

#include "Sensor/Response.h"

namespace {

// Nine significant digits, which is every bit of a float, so that the
// hash changes exactly when a knot does.
void appendNumber(std::string &text, double value) {
  char buffer[32]{};
  std::snprintf(buffer, sizeof(buffer), "%.9g", value);
  text += buffer;
  text += ' ';
}

} // namespace

std::vector<std::string>
responseFilmBandNames(const ResponseSettings &settings) {
  if (settings.hasCFA()) return {MOSAIC_BAND_NAME};
  std::vector<std::string> names{};
  for (const auto &band : settings.bands) names.push_back(band.name);
  return names;
}

std::string responseHash(const ResponseSettings &settings) {
  const double scale{settings.qeScale()};
  std::string text{};
  for (const auto &band : settings.bands) {
    text += band.name;
    text += ' ';
    for (size_t i = 0; i < band.wavelengths.size(); i++) {
      appendNumber(text, band.wavelengths[i]);
      appendNumber(text, scale * double(band.values[i]));
    }
    text += '\n';
  }
  text += std::to_string(settings.cfaColumns);
  for (const auto index : settings.cfa) text += ' ' + std::to_string(index);
  // The leaks decide what the band film holds, since a response de-mixes
  // the stated curves by what they imply, so a film drawn under one is
  // not a film drawn under another and a resume across the two has to
  // refuse itself. A response stating no leak hashes as one stating zero,
  // which is what they both mean.
  if (settings.hasCrosstalk())
    for (const auto leak : settings.crosstalk) appendNumber(text, leak);
  return std::string(smdl::MD5Hash::hashMemory(text));
}

std::vector<double>
WavelengthDensity::breakpointsOf(smdl::Span<const double> knots, double lo,
                                 double hi) {
  SMDL_SANITY_CHECK(!knots.empty());
  lo = std::max(lo, knots.front());
  hi = std::min(hi, knots.back());
  std::vector<double> w{};
  if (!(lo < hi)) return w;
  w.push_back(lo);
  for (int64_t nm{int64_t(std::floor(lo)) + 1}; double(nm) < hi; nm++)
    w.push_back(double(nm));
  for (const auto knot : knots)
    if (knot > lo && knot < hi) w.push_back(knot);
  w.push_back(hi);
  std::sort(w.begin(), w.end());
  w.erase(std::unique(w.begin(), w.end()), w.end());
  return w;
}

WavelengthDensity::WavelengthDensity(std::vector<double> breakpoints,
                                     smdl::Span<const float> masses)
    : mWavelengths(std::move(breakpoints)), mPieces(masses) {
  SMDL_SANITY_CHECK(masses.size() + 1 == mWavelengths.size());
  if (!(mPieces.unnormalizedSum() > 0)) {
    mWavelengths.clear();
    mPieces.clear();
  }
}

WavelengthDensity
WavelengthDensity::ofResponse(smdl::Span<const double> knots,
                              smdl::Span<const double> values, double lo,
                              double hi, const SensorSpectrum &illuminant) {
  SMDL_SANITY_CHECK(knots.size() == values.size());
  SMDL_SANITY_CHECK(illuminant.size() == SENSOR_WAVELENGTH_COUNT);
  std::vector<double> w{breakpointsOf(knots, lo, hi)};
  if (w.empty()) return {};
  // The photon factor's `1 / (h c)` and the curve's scale cancel in the
  // normalization, so a piece's mass is the curve, the illuminant, and
  // the wavelength at its middle, times its width.
  std::vector<float> masses{};
  masses.reserve(w.size() - 1);
  for (size_t i = 0; i + 1 < w.size(); i++) {
    const double middle{0.5 * (w[i] + w[i + 1])};
    masses.push_back(float(curveAt(knots, values, middle) *
                           sensorSpectrumAt(illuminant, middle) * middle *
                           (w[i + 1] - w[i])));
  }
  return WavelengthDensity(std::move(w), masses);
}
WavelengthDensity
WavelengthDensity::ofPlacement(smdl::Span<const PlacementCurve> curves,
                               double lo, double hi) {
  // Every curve's knots together, so that each curve is linear between
  // any two neighboring breakpoints.
  std::vector<double> knots{};
  for (const auto &curve : curves) {
    SMDL_SANITY_CHECK(curve.knots.size() == curve.values.size() &&
                      !curve.knots.empty());
    knots.insert(knots.end(), curve.knots.begin(), curve.knots.end());
  }
  std::sort(knots.begin(), knots.end());
  knots.erase(std::unique(knots.begin(), knots.end()), knots.end());
  std::vector<double> w{breakpointsOf(knots, lo, hi)};
  if (w.empty()) return {};
  // The slope of each curve at its peak times the wavelength over each
  // piece, to the two thirds, times the width, summed over the curves;
  // then the two halves each normalized to one. Curves with no slope
  // anywhere, flat or zero, leave the uniform half alone.
  const size_t numPieces{w.size() - 1};
  std::vector<double> slopes(numPieces);
  for (const auto &curve : curves) {
    const double peak{
        *std::max_element(curve.values.begin(), curve.values.end())};
    if (!(peak > 0)) continue;
    for (size_t i = 0; i < numPieces; i++) {
      const double f0{curveAt(curve.knots, curve.values, w[i]) / peak * w[i]};
      const double f1{curveAt(curve.knots, curve.values, w[i + 1]) / peak *
                      w[i + 1]};
      const double width{w[i + 1] - w[i]};
      slopes[i] += std::pow(std::abs(f1 - f0) / width, 2.0 / 3.0) * width;
    }
  }
  double slopeTotal{};
  for (const auto slope : slopes) slopeTotal += slope;
  const double span{w.back() - w.front()};
  std::vector<float> masses(numPieces);
  for (size_t i = 0; i < numPieces; i++) {
    const double uniform{(w[i + 1] - w[i]) / span};
    masses[i] =
        float(slopeTotal > 0 ? 0.5 * slopes[i] / slopeTotal + 0.5 * uniform
                             : uniform);
  }
  return WavelengthDensity(std::move(w), masses);
}

double WavelengthDensity::at(float xi) const noexcept {
  // The piece the distribution puts `xi` in, which always has mass, and
  // the fraction of the way across it.
  float t{};
  const int k{mPieces.indexSample(xi, &t)};
  return mWavelengths[k] + double(t) * (mWavelengths[k + 1] - mWavelengths[k]);
}

std::pair<size_t, size_t> WavelengthDensity::massRange() const noexcept {
  int first{0}, last{mPieces.size() - 1};
  while (!(mPieces.indexPMF(first) > 0)) first++;
  while (!(mPieces.indexPMF(last) > 0)) last--;
  return {size_t(first), size_t(last)};
}

TracedSpan WavelengthDensity::span() const noexcept {
  const auto [first, last]{massRange()};
  return TracedSpan{float(mWavelengths[first]), float(at(0.5f)),
                    float(mWavelengths[last + 1])};
}

WavelengthCells WavelengthDensity::cells(size_t count) const {
  SMDL_SANITY_CHECK(!isEmpty() && count > 0);
  const auto [first, last]{massRange()};
  WavelengthCells cells{};
  // The ends are where the mass begins and ends, exactly; the edges
  // between are the draw at the fractions.
  cells.edges.resize(count + 1);
  cells.edges.front() = mWavelengths[first];
  cells.edges.back() = mWavelengths[last + 1];
  for (size_t i = 1; i < count; i++)
    cells.edges[i] = at(float(i) / float(count));
  cells.wavelengths.resize(count);
  for (size_t i = 0; i < count; i++)
    cells.wavelengths[i] = float(0.5 * (cells.edges[i] + cells.edges[i + 1]));
  SMDL_SANITY_CHECK(cells.isValid());
  return cells;
}

std::string spellTracedSpan(const TracedSpan &span) {
  return smdl::concat(smdl::Brief(span.lo, 4), "-", smdl::Brief(span.hi, 4),
                      " nm, median ", smdl::Brief(span.median, 4));
}

std::optional<TracedSpan> tracedSpanOf(const ResponseBand &band,
                                       const SensorSpectrum &illuminant) {
  const std::vector<double> knots(band.wavelengths.begin(),
                                  band.wavelengths.end());
  const std::vector<double> values(band.values.begin(), band.values.end());
  const WavelengthDensity density{WavelengthDensity::ofResponse(
      knots, values, knots.front(), knots.back(), illuminant)};
  if (density.isEmpty()) return std::nullopt;
  return density.span();
}

namespace {

// The grid `bands` place together over the union of their knots.
[[nodiscard]] WavelengthCells placeCells(smdl::Span<const ResponseBand> bands,
                                         size_t count) {
  std::vector<std::vector<double>> knots{};
  std::vector<std::vector<double>> values{};
  double lo{INF};
  double hi{-INF};
  for (const auto &band : bands) {
    knots.emplace_back(band.wavelengths.begin(), band.wavelengths.end());
    values.emplace_back(band.values.begin(), band.values.end());
    lo = std::min(lo, knots.back().front());
    hi = std::max(hi, knots.back().back());
  }
  std::vector<WavelengthDensity::PlacementCurve> curves{};
  for (size_t i = 0; i < knots.size(); i++)
    curves.push_back({knots[i], values[i]});
  return WavelengthDensity::ofPlacement(curves, lo, hi).cells(count);
}

} // namespace

WavelengthCells bandWavelengthCells(const ResponseBand &band, size_t count) {
  return placeCells(smdl::Span<const ResponseBand>(&band, 1), count);
}

WavelengthCells responseWavelengthCells(const ResponseSettings &settings,
                                        size_t count) {
  return placeCells(settings.bands, count);
}

Response::Response(const ResponseSettings &settings,
                   const SensorSpectrum &illuminant)
    : mHash(responseHash(settings)),
      mFilmBandNames(responseFilmBandNames(settings)),
      mCFAColumns(settings.cfaColumns), mCFARows(settings.cfaRows()),
      mCFA(settings.cfa) {
  for (const auto index : mCFA)
    mTileNames.push_back(settings.bands[index].name);
  const std::vector<size_t> tileBandIndices{tileBands(mCFA)};
  for (const auto index : tileBandIndices)
    mTileBandNames.push_back(settings.bands[index].name);
  // The grid a band projects on: under a tile the grid of its pixels,
  // which is the render's tile's, else the one grid; null for a band the
  // tile does not lay down, which projects never.
  const RenderGrid &grids{gRenderGrid};
  SMDL_SANITY_CHECK(!grids.hasTile() ||
                    (grids.tileColumns == mCFAColumns &&
                     grids.tileGrids.size() == mCFA.size()));
  const auto gridOf{[&](size_t bandIndex) -> const WavelengthGrid * {
    if (!hasTile()) return &grids.first();
    for (size_t cell = 0; cell < mCFA.size(); cell++)
      if (mCFA[cell] == bandIndex)
        return &grids.grids[grids.hasTile() ? grids.tileGrids[cell] : 0];
    return nullptr;
  }};
  const bool isJittering{grids.isJittering};
  const double scale{settings.qeScale()};
  for (size_t bandIndex = 0; bandIndex < settings.bands.size(); bandIndex++) {
    const ResponseBand &curve{settings.bands[bandIndex]};
    Band band{};
    band.name = curve.name;
    const WavelengthGrid *grid{gridOf(bandIndex)};
    if (!grid) {
      mBands.push_back(std::move(band));
      continue;
    }
    // What the grid can see and what each cell weighs, which the jitter
    // leaves alone: the samples move inside the cells, which tile the
    // same span with the same widths.
    const smdl::SpectralColor &wavelengths{grid->wavelengths};
    const size_t numBands{wavelengths.size()};
    SMDL_SANITY_CHECK(numBands == grids.numBands);
    const double gridLo{double(grid->minWavelength())};
    const double gridHi{double(grid->maxWavelength())};
    double minSpacing{gridHi - gridLo};
    for (size_t i = 1; i < numBands; i++)
      minSpacing = std::min(minSpacing, double(wavelengths[i]) -
                                            double(wavelengths[i - 1]));
    band.projection = BandProjection(curve, scale, *grid, isJittering);
    const BandProjection &projection{band.projection};
    const std::vector<double> &knots{projection.wavelengths()};
    const double whole{projection.integrate(knots.front(), knots.back())};
    const double inside{projection.integrate(gridLo, gridHi)};
    if (!(inside > 0))
      throw smdl::Error(smdl::concat(
          "Response band ", smdl::Quoted(band.name), " (",
          smdl::Brief(knots.front(), 6), "-", smdl::Brief(knots.back(), 6),
          " nm) lies outside the wavelength grid (", smdl::Brief(gridLo, 6),
          "-", smdl::Brief(gridHi, 6),
          " nm); widen -wavelength-range to "
          "cover it"));
    if (inside < 0.99 * whole)
      SMDL_LOG_WARN("Response band ", smdl::Quoted(band.name), " has ",
                    smdl::Brief(100.0 * (1.0 - inside / whole), 3),
                    "% of its weight outside the wavelength grid (",
                    smdl::Brief(gridLo, 6), "-", smdl::Brief(gridHi, 6),
                    " nm), so the band sees only the part inside; "
                    "-wavelength-range ",
                    int64_t(std::floor(std::min(knots.front(), gridLo))), ",",
                    int64_t(std::ceil(std::max(knots.back(), gridHi))),
                    " would cover it");
    // The equivalent width, the integral over the peak, which has no
    // edge cases where a full width at half maximum has several.
    const double peak{*std::max_element(projection.values().begin(),
                                        projection.values().end())};
    const double equivalentWidth{peak > 0 ? whole / peak : 0.0};
    if (!isJittering && equivalentWidth < minSpacing)
      SMDL_LOG_WARN("Response band ", smdl::Quoted(band.name), " is ",
                    smdl::Brief(equivalentWidth, 3),
                    " nm wide against a grid spaced ",
                    smdl::Brief(minSpacing, 3),
                    " nm; without -wavelength-jitter the band comb aliases "
                    "against it");
    // A grid held still sees the curve at its wavelengths and nowhere
    // else, so a band that falls between them is invisible to every
    // sample.
    if (!projection.isSeen())
      throw smdl::Error(smdl::concat(
          "Response band ", smdl::Quoted(band.name),
          " falls between the wavelengths of the grid, so no sample "
          "can see it; use -wavelength-jitter, or a finer grid"));
    mBands.push_back(std::move(band));
  }
  // Each band the tile lays down draws from inside what its grid sees,
  // even when the grid holds still: a lens's geometry is continuous in
  // the wavelength, so it has no comb to alias against.
  for (const auto index : tileBandIndices) {
    Band &band{mBands[index]};
    const WavelengthGrid &grid{*gridOf(index)};
    band.draw = WavelengthDensity::ofResponse(
        band.projection.wavelengths(), band.projection.values(),
        double(grid.minWavelength()), double(grid.maxWavelength()), illuminant);
    if (band.draw.isEmpty())
      SMDL_LOG_WARN("Response band ", smdl::Quoted(band.name),
                    " sees none of the white balance's illuminant inside the "
                    "wavelength grid, so its pixels trace a lens whose "
                    "glasses disperse at the d line");
  }
}

BandProjection::BandProjection(const ResponseBand &band, double qeScale,
                               const WavelengthGrid &grid, bool isJittering)
    : mIsJittering(isJittering) {
  mWavelengths.assign(band.wavelengths.begin(), band.wavelengths.end());
  for (const auto value : band.values)
    mValues.push_back(qeScale * double(value));
  const smdl::SpectralColor &wavelengths{grid.wavelengths};
  const size_t numBands{wavelengths.size()};
  if (mIsJittering) {
    mWidths = grid.widths;
    return;
  }
  mFixedWeights.resize(numBands);
  for (size_t i = 0; i < numBands; i++) {
    const double lambda{double(wavelengths[i])};
    mFixedWeights[i] =
        evaluate(lambda) * grid.widths[i] * photonsPerJoule(lambda);
  }
}

double BandProjection::integrate(double lo, double hi) const noexcept {
  const std::vector<double> &w{mWavelengths};
  const std::vector<double> &v{mValues};
  double total{};
  for (size_t i = 0; i + 1 < w.size(); i++) {
    const double a{std::max(w[i], lo)};
    const double b{std::min(w[i + 1], hi)};
    if (!(b > a)) continue;
    const double slope{(v[i + 1] - v[i]) / (w[i + 1] - w[i])};
    const double va{v[i] + slope * (a - w[i])};
    const double vb{v[i] + slope * (b - w[i])};
    total += 0.5 * (va + vb) * (b - a);
  }
  return total;
}

bool BandProjection::isSeen() const noexcept {
  if (mIsJittering) return true;
  double seen{};
  for (const auto weight : mFixedWeights) seen += weight;
  return seen > 0;
}

double BandProjection::project(smdl::Span<const float> wavelengths,
                               smdl::Span<const float> E) const noexcept {
  double total{};
  if (!mIsJittering) {
    for (size_t i = 0; i < E.size(); i++)
      total += mFixedWeights[i] * double(E[i]);
    return total;
  }
  for (size_t i = 0; i < E.size(); i++) {
    const double lambda{double(wavelengths[i])};
    total +=
        evaluate(lambda) * mWidths[i] * photonsPerJoule(lambda) * double(E[i]);
  }
  return total;
}

void Response::accumulate(smdl::Span<const float> wavelengths,
                          smdl::Span<const float> E, size_t x, size_t y,
                          double *sums) const noexcept {
  SMDL_SANITY_CHECK(wavelengths.size() == E.size());
  if (hasTile()) {
    sums[0] += mBands[bandAt(x, y)].projection.project(wavelengths, E);
    return;
  }
  for (size_t b = 0; b < mBands.size(); b++)
    sums[b] += mBands[b].projection.project(wavelengths, E);
}

float Response::traceWavelengthAt(size_t x, size_t y, float xi) const noexcept {
  SMDL_DEBUG_CHECK(hasTile());
  const WavelengthDensity &draw{mBands[bandAt(x, y)].draw};
  return draw.isEmpty() ? 0.0f : float(draw.at(xi));
}

void Response::logTracedSpans() const {
  for (const auto index : tileBands(mCFA)) {
    const Band &band{mBands[index]};
    if (band.draw.isEmpty()) continue;
    SMDL_LOG_INFO("Lens color: the ", smdl::Quoted(band.name),
                  " pixels trace the lens at ",
                  spellTracedSpan(band.draw.span()));
  }
}

std::optional<Response>
resolveResponse(const std::optional<ResponseSettings> &settings,
                const SensorSpectrum &illuminant) {
  std::optional<Response> response{};
  if (settings) response.emplace(*settings, illuminant);
  return response;
}
