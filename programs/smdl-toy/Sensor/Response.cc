#include <algorithm>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <filesystem>

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

// The name beside a spectral film with `suffix` before its extension.
[[nodiscard]] std::string besideSpectrum(const std::string &spectrumName,
                                         const char *suffix) {
  std::filesystem::path path{spectrumName};
  const std::string extension{path.extension().string()};
  path.replace_extension();
  return path.string() + suffix + extension;
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
  return std::string(smdl::MD5Hash::hashMemory(text));
}

std::string bandFilmFileName(const std::string &spectrumName) {
  return besideSpectrum(spectrumName, "-bands");
}

LensWavelengthDraw::LensWavelengthDraw(smdl::Span<const double> knots,
                                       smdl::Span<const double> values,
                                       double lo, double hi,
                                       const SensorSpectrum &illuminant) {
  SMDL_SANITY_CHECK(knots.size() == values.size() && !knots.empty());
  SMDL_SANITY_CHECK(illuminant.size() == SENSOR_WAVELENGTH_COUNT);
  lo = std::max(lo, knots.front());
  hi = std::min(hi, knots.back());
  if (!(lo < hi)) return;
  std::vector<double> &w{mWavelengths};
  w.push_back(lo);
  for (int64_t nm{int64_t(std::floor(lo)) + 1}; double(nm) < hi; nm++)
    w.push_back(double(nm));
  for (const auto knot : knots)
    if (knot > lo && knot < hi) w.push_back(knot);
  w.push_back(hi);
  std::sort(w.begin(), w.end());
  w.erase(std::unique(w.begin(), w.end()), w.end());
  // The photon factor's `1 / (h c)` and the curve's scale cancel in the
  // normalization, so the density here is the curve, the illuminant, and
  // the wavelength.
  mCDF.assign(1, 0.0);
  for (size_t i = 0; i + 1 < w.size(); i++) {
    const double middle{0.5 * (w[i] + w[i + 1])};
    mCDF.push_back(mCDF.back() + curveAt(knots, values, middle) *
                                     sensorSpectrumAt(illuminant, middle) *
                                     middle * (w[i + 1] - w[i]));
  }
  const double total{mCDF.back()};
  if (!(total > 0)) {
    mWavelengths.clear();
    mCDF.clear();
    return;
  }
  for (auto &value : mCDF) value /= total;
  mCDF.back() = 1;
}

double LensWavelengthDraw::at(double xi) const noexcept {
  // The piece whose share of the distribution reaches past `xi`. A piece
  // with no share leaves the distribution flat across it, and the search
  // steps over it, so the draw never lands where the density is zero.
  const auto itr{std::upper_bound(mCDF.begin() + 1, mCDF.end() - 1, xi)};
  const size_t k{size_t(itr - mCDF.begin()) - 1};
  const double share{mCDF[k + 1] - mCDF[k]};
  const double t{share > 0 ? std::clamp((xi - mCDF[k]) / share, 0.0, 1.0)
                           : 0.0};
  return mWavelengths[k] + t * (mWavelengths[k + 1] - mWavelengths[k]);
}

TracedSpan LensWavelengthDraw::span() const noexcept {
  size_t first{0}, last{mCDF.size() - 2};
  while (!(mCDF[first + 1] > mCDF[first])) first++;
  while (!(mCDF[last + 1] > mCDF[last])) last--;
  return TracedSpan{float(mWavelengths[first]), float(at(0.5)),
                    float(mWavelengths[last + 1])};
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
  const LensWavelengthDraw draw{knots, values, knots.front(), knots.back(),
                                illuminant};
  if (draw.isEmpty()) return std::nullopt;
  return draw.span();
}

Response::Response(const ResponseSettings &settings, const Color &wavelengths,
                   const SensorSpectrum &illuminant)
    : mHash(responseHash(settings)),
      mFilmBandNames(responseFilmBandNames(settings)),
      mCFAColumns(settings.cfaColumns), mCFARows(settings.cfaRows()),
      mCFA(settings.cfa), mIsJittering(!gRenderGrid.bandEdges.empty()) {
  for (const auto index : mCFA)
    mTileNames.push_back(settings.bands[index].name);
  const size_t numBands{wavelengths.size()};
  // What the grid can see and what each band weighs, which the jitter
  // leaves alone: its rectangles tile the same span with the same widths.
  const double gridLo{double(wavelengths[0])};
  const double gridHi{double(wavelengths[numBands - 1])};
  const std::vector<double> widths{wavelengthTrapezoidWidths(wavelengths)};
  if (mIsJittering) mWidths = widths;
  double minSpacing{gridHi - gridLo};
  for (size_t i = 1; i < numBands; i++)
    minSpacing = std::min(minSpacing,
                          double(wavelengths[i]) - double(wavelengths[i - 1]));
  const double scale{settings.qeScale()};
  for (const auto &curve : settings.bands) {
    Band band{};
    band.name = curve.name;
    band.wavelengths.assign(curve.wavelengths.begin(), curve.wavelengths.end());
    for (const auto value : curve.values)
      band.values.push_back(scale * double(value));
    const double whole{
        integrate(band, band.wavelengths.front(), band.wavelengths.back())};
    const double inside{integrate(band, gridLo, gridHi)};
    if (!(inside > 0))
      throw smdl::Error(
          smdl::concat("response band ", smdl::Quoted(band.name), " (",
                       smdl::Brief(band.wavelengths.front(), 6), "-",
                       smdl::Brief(band.wavelengths.back(), 6),
                       " nm) lies outside the wavelength grid (",
                       smdl::Brief(gridLo, 6), "-", smdl::Brief(gridHi, 6),
                       " nm); widen -wavelength-range to "
                       "cover it"));
    if (inside < 0.99 * whole)
      SMDL_LOG_WARN(
          "response band ", smdl::Quoted(band.name), " has ",
          smdl::Brief(100.0 * (1.0 - inside / whole), 3),
          "% of its weight outside the wavelength grid (",
          smdl::Brief(gridLo, 6), "-", smdl::Brief(gridHi, 6),
          " nm), so the band sees only the part inside; "
          "-wavelength-range ",
          int64_t(std::floor(std::min(band.wavelengths.front(), gridLo))), ",",
          int64_t(std::ceil(std::max(band.wavelengths.back(), gridHi))),
          " would cover it");
    // The equivalent width, the integral over the peak, which has no
    // edge cases where a full width at half maximum has several.
    const double peak{
        *std::max_element(band.values.begin(), band.values.end())};
    const double equivalentWidth{peak > 0 ? whole / peak : 0.0};
    if (!mIsJittering && equivalentWidth < minSpacing)
      SMDL_LOG_WARN("response band ", smdl::Quoted(band.name), " is ",
                    smdl::Brief(equivalentWidth, 3),
                    " nm wide against a grid spaced ",
                    smdl::Brief(minSpacing, 3),
                    " nm; without -wavelength-jitter the band comb aliases "
                    "against it");
    if (!mIsJittering) {
      // A grid held still sees the curve at its own wavelengths and
      // nowhere else, so a band that falls between them is invisible
      // to every sample.
      double seen{0.0};
      for (size_t i = 0; i < numBands; i++)
        seen += evaluate(band, double(wavelengths[i])) * widths[i];
      if (!(seen > 0))
        throw smdl::Error(smdl::concat(
            "response band ", smdl::Quoted(band.name),
            " falls between the wavelengths of the grid, so no sample "
            "can see it; use -wavelength-jitter, or a finer grid"));
      band.fixedWeights.resize(numBands);
      for (size_t i = 0; i < numBands; i++) {
        const double lambda{double(wavelengths[i])};
        band.fixedWeights[i] =
            evaluate(band, lambda) * widths[i] * photonsPerJoule(lambda);
      }
    }
    mBands.push_back(std::move(band));
  }
  // Each band the tile lays down draws from inside what the grid sees,
  // even when the grid holds still: a lens's geometry is continuous in the
  // wavelength, so it has no comb to alias against.
  for (const auto index : tileBands(mCFA)) {
    Band &band{mBands[index]};
    band.draw = LensWavelengthDraw(band.wavelengths, band.values, gridLo,
                                   gridHi, illuminant);
    if (band.draw.isEmpty())
      SMDL_LOG_WARN("response band ", smdl::Quoted(band.name),
                    " sees none of the white balance's illuminant inside the "
                    "wavelength grid, so its pixels trace a lens whose "
                    "glasses disperse at the d line");
  }
}

double Response::integrate(const Band &band, double lo, double hi) noexcept {
  const std::vector<double> &w{band.wavelengths};
  const std::vector<double> &v{band.values};
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

double Response::project(const Band &band, smdl::Span<const float> wavelengths,
                         smdl::Span<const float> E) const noexcept {
  double total{};
  if (!mIsJittering) {
    for (size_t i = 0; i < E.size(); i++)
      total += band.fixedWeights[i] * double(E[i]);
    return total;
  }
  for (size_t i = 0; i < E.size(); i++) {
    const double lambda{double(wavelengths[i])};
    total += evaluate(band, lambda) * mWidths[i] * photonsPerJoule(lambda) *
             double(E[i]);
  }
  return total;
}

void Response::accumulate(smdl::Span<const float> wavelengths,
                          smdl::Span<const float> E, size_t x, size_t y,
                          double *sums) const noexcept {
  SMDL_SANITY_CHECK(wavelengths.size() == E.size());
  if (hasTile()) {
    sums[0] += project(mBands[bandAt(x, y)], wavelengths, E);
    return;
  }
  for (size_t b = 0; b < mBands.size(); b++)
    sums[b] += project(mBands[b], wavelengths, E);
}

float Response::traceWavelengthAt(size_t x, size_t y, float xi) const noexcept {
  SMDL_DEBUG_CHECK(hasTile());
  const LensWavelengthDraw &draw{mBands[bandAt(x, y)].draw};
  return draw.isEmpty() ? 0.0f : float(draw.at(double(xi)));
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
                const Color &wavelengths, const SensorSpectrum &illuminant) {
  std::optional<Response> response{};
  if (settings) response.emplace(*settings, wavelengths, illuminant);
  return response;
}
