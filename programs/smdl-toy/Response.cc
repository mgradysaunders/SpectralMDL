#include <algorithm>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <filesystem>

#include "smdl/Support/Error.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/MD5Hash.h"
#include "smdl/Support/Strings.h"

#include "Response.h"

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
  auto path{std::filesystem::path(spectrumName)};
  const auto extension{path.extension().string()};
  path.replace_extension();
  return path.string() + suffix + extension;
}

} // namespace

std::vector<std::string>
responseFilmBandNames(const ResponseSettings &settings) {
  if (settings.hasCFA()) return {MOSAIC_BAND_NAME};
  auto names{std::vector<std::string>()};
  for (const auto &band : settings.bands) names.push_back(band.name);
  return names;
}

std::string responseHash(const ResponseSettings &settings) {
  auto text{std::string(settings.kind == ResponseKind::QE ? "qe" : "relative")};
  text += '\n';
  for (const auto &band : settings.bands) {
    text += band.name;
    text += ' ';
    for (size_t i = 0; i < band.wavelengths.size(); i++) {
      appendNumber(text, band.wavelengths[i]);
      appendNumber(text, band.values[i]);
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

std::string bandSquaresFileName(const std::string &spectrumName) {
  return besideSpectrum(spectrumName, "-bands-squares");
}

Response::Response(const ResponseSettings &settings, const Color &wavelengths)
    : mKind(settings.kind), mName(settings.name), mHash(responseHash(settings)),
      mFilmBandNames(responseFilmBandNames(settings)),
      mCFAColumns(settings.cfaColumns), mCFARows(settings.cfaRows()),
      mCFA(settings.cfa), mIsJittering(!gRenderGrid.bandEdges.empty()) {
  for (const auto index : mCFA)
    mTileNames.push_back(settings.bands[index].name);
  const size_t numBands{wavelengths.size()};
  const auto &edges{gRenderGrid.bandEdges};
  // What the grid can see: the jitter's outer rectangle edges, else the
  // grid's own ends. And what each grid band weighs.
  const double gridLo{mIsJittering ? double(edges.front())
                                   : double(wavelengths[0])};
  const double gridHi{mIsJittering ? double(edges.back())
                                   : double(wavelengths[numBands - 1])};
  auto widths{std::vector<double>()};
  if (mIsJittering) {
    widths.resize(numBands);
    for (size_t i = 0; i < numBands; i++)
      widths[i] = double(edges[i + 1]) - double(edges[i]);
    mWidths = widths;
  } else {
    widths = wavelengthTrapezoidWidths(wavelengths);
  }
  double minSpacing{gridHi - gridLo};
  for (size_t i = 1; i < numBands; i++)
    minSpacing = std::min(minSpacing,
                          double(wavelengths[i]) - double(wavelengths[i - 1]));
  for (const auto &curve : settings.bands) {
    auto band{Band{}};
    band.name = curve.name;
    band.wavelengths.assign(curve.wavelengths.begin(), curve.wavelengths.end());
    band.values.assign(curve.values.begin(), curve.values.end());
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
    // The normalizer, under the sample's own rule.
    double normalizer{1.0};
    if (mKind == ResponseKind::RELATIVE) {
      if (mIsJittering) {
        normalizer = inside;
      } else {
        normalizer = 0.0;
        for (size_t i = 0; i < numBands; i++)
          normalizer += evaluate(band, double(wavelengths[i])) * widths[i];
        if (!(normalizer > 0))
          throw smdl::Error(smdl::concat(
              "response band ", smdl::Quoted(band.name),
              " falls between the wavelengths of the grid, so no sample "
              "can see it; use -wavelength-jitter, or a finer grid"));
      }
    }
    band.scale = 1.0 / normalizer;
    if (!mIsJittering) {
      band.fixedWeights.resize(numBands);
      for (size_t i = 0; i < numBands; i++) {
        const double lambda{double(wavelengths[i])};
        band.fixedWeights[i] = evaluate(band, lambda) * widths[i] *
                               photonsPerJoule(lambda) * band.scale;
      }
    }
    mBands.push_back(std::move(band));
  }
}

double Response::evaluate(const Band &band, double lambda) noexcept {
  const auto &w{band.wavelengths};
  const auto &v{band.values};
  if (!(lambda >= w.front() && lambda <= w.back())) return 0.0;
  const auto itr{std::upper_bound(w.begin(), w.end(), lambda)};
  const size_t i{size_t(itr - w.begin())};
  if (i == 0) return v.front();
  if (i == w.size()) return v.back();
  const double t{(lambda - w[i - 1]) / (w[i] - w[i - 1])};
  return v[i - 1] + t * (v[i] - v[i - 1]);
}

double Response::integrate(const Band &band, double lo, double hi) noexcept {
  const auto &w{band.wavelengths};
  const auto &v{band.values};
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

double Response::photonsPerJoule(double lambda) const noexcept {
  return mKind == ResponseKind::QE ? lambda * 1e-9 / (PLANCK * SPEED_OF_LIGHT)
                                   : 1.0;
}

double Response::project(const Band &band, smdl::Span<const float> wavelengths,
                         smdl::Span<const float> L) const noexcept {
  double total{};
  if (!mIsJittering) {
    for (size_t i = 0; i < L.size(); i++)
      total += band.fixedWeights[i] * double(L[i]);
    return total;
  }
  for (size_t i = 0; i < L.size(); i++) {
    const double lambda{double(wavelengths[i])};
    total += evaluate(band, lambda) * mWidths[i] * photonsPerJoule(lambda) *
             double(L[i]);
  }
  return total * band.scale;
}

void Response::accumulate(smdl::Span<const float> wavelengths,
                          smdl::Span<const float> L, size_t x, size_t y,
                          double *sums, double *squares) const noexcept {
  SMDL_SANITY_CHECK(wavelengths.size() == L.size());
  const auto add{[&](size_t b, const Band &band) {
    const double value{project(band, wavelengths, L)};
    sums[b] += value;
    if (squares) squares[b] += value * value;
  }};
  if (hasTile()) {
    add(0, mBands[bandAt(x, y)]);
    return;
  }
  for (size_t b = 0; b < mBands.size(); b++) add(b, mBands[b]);
}

std::optional<Response>
resolveResponse(const std::optional<ResponseSettings> &settings,
                const Color &wavelengths) {
  auto response{std::optional<Response>()};
  if (settings) response.emplace(*settings, wavelengths);
  return response;
}
