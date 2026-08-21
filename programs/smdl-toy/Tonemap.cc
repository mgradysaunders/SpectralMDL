#include <algorithm>
#include <cmath>
#include <cstdio>
#include <iostream>

#include "Tonemap.h"

//--{ Spectral to RGB

// The photopic luminous efficiency V(lambda) for lambda in nm, as the
// same Gaussian fit the night filter uses. It closely tracks the CIE
// y-bar observer, which makes it the right mass for deciding whether
// the grid can see color at all.
[[nodiscard]] static double photopicV(double lambda) {
  const double um{lambda * 1.0e-3};
  return 1.019 * std::exp(-285.4 * (um - 0.5590) * (um - 0.5590));
}

// The fraction of the photopic luminous mass the wavelength grid can
// see: a fine sweep of the visible, crediting the mass wherever some
// band lies within 30nm. This decides whether the CIE projection of
// the spectral buffer means anything as a picture.
[[nodiscard]] static double visibleCoverage(const Color &wavelengths) {
  double total{};
  double covered{};
  for (double lambda = 380.0; lambda <= 780.0; lambda += 5.0) {
    const double mass{photopicV(lambda)};
    total += mass;
    for (size_t i = 0; i < wavelengths.size(); i++) {
      if (std::abs(double(wavelengths[i]) - lambda) <= 30.0) {
        covered += mass;
        break;
      }
    }
  }
  return covered / total;
}

// The band nearest a wavelength in nm.
[[nodiscard]] static size_t nearestBand(const Color &wavelengths,
                                        float lambda) {
  size_t best{0};
  for (size_t i = 1; i < wavelengths.size(); i++)
    if (std::abs(wavelengths[i] - lambda) <
        std::abs(wavelengths[best] - lambda))
      best = i;
  return best;
}

std::vector<float> resolveRGB(smdl::Compiler &compiler,
                              const smdl::SpectralRenderImage &spectral,
                              const Color &wavelengths,
                              const RGBPolicy &policy) {
  const size_t numPixelsX{spectral.getNumPixelsX()};
  const size_t numPixelsY{spectral.getNumPixelsY()};
  const size_t numBands{spectral.getNumBands()};
  auto rgbImage{std::vector<float>(numPixelsX * numPixelsY * 3)};
  // Down to 90% coverage the CIE projection is a faithful picture, and
  // down to 35% it is still the physically correct band-limited view
  // (tinted or dim) and only earns a note; below that a color has no
  // meaning and three bands map to R, G, B instead. Fewer than three
  // bands cannot even pretend, and write the grayscale mean radiance;
  // this outranks coverage because a lone mid-visible band can cover
  // much of the photopic mass while its "color" is still garbage.
  enum class Mode { TRUE_COLOR, FALSE_COLOR, GRAYSCALE };
  const double coverage{visibleCoverage(wavelengths)};
  auto mode{Mode::TRUE_COLOR};
  if (numBands < 3) {
    mode = Mode::GRAYSCALE;
  } else if (policy.forceFalseColor || coverage < 0.35) {
    mode = Mode::FALSE_COLOR;
  }
  if (mode == Mode::GRAYSCALE) {
    char note[96]{};
    std::snprintf(note, sizeof(note),
                  "spectral to RGB: %zu band(s) cannot carry color, "
                  "writing the grayscale mean radiance\n",
                  numBands);
    std::cerr << note;
    for (size_t p = 0; p < numPixelsX * numPixelsY; p++) {
      auto pixel{spectral(p % numPixelsX, p / numPixelsX)};
      double mean{};
      for (size_t i = 0; i < numBands; i++) mean += pixel.mean(i);
      mean /= double(numBands);
      auto texel{&rgbImage[3 * p]};
      texel[0] = texel[1] = texel[2] = float(mean);
    }
    return rgbImage;
  }
  if (mode == Mode::FALSE_COLOR) {
    // Long wavelengths on red, remote-sensing style: by default the
    // bands nearest 5/6, 1/2, and 1/6 of the grid span.
    const float waveMin{wavelengths[0]};
    const float waveMax{wavelengths[wavelengths.size() - 1]};
    size_t bandR{}, bandG{}, bandB{};
    if (policy.falseColorWaves.size() == 3) {
      bandR = nearestBand(wavelengths, policy.falseColorWaves[0]);
      bandG = nearestBand(wavelengths, policy.falseColorWaves[1]);
      bandB = nearestBand(wavelengths, policy.falseColorWaves[2]);
    } else {
      bandR = nearestBand(wavelengths, waveMin + (waveMax - waveMin) * 5 / 6);
      bandG = nearestBand(wavelengths, waveMin + (waveMax - waveMin) / 2);
      bandB = nearestBand(wavelengths, waveMin + (waveMax - waveMin) / 6);
    }
    char note[160]{};
    std::snprintf(note, sizeof(note),
                  "spectral to RGB: false color%s, R=%.0fnm G=%.0fnm "
                  "B=%.0fnm (the grid covers %.0f%% of the visible)\n",
                  policy.forceFalseColor ? " (forced)" : "",
                  double(wavelengths[bandR]), double(wavelengths[bandG]),
                  double(wavelengths[bandB]), 100.0 * coverage);
    std::cerr << note;
    for (size_t p = 0; p < numPixelsX * numPixelsY; p++) {
      auto pixel{spectral(p % numPixelsX, p / numPixelsX)};
      auto texel{&rgbImage[3 * p]};
      texel[0] = float(pixel.mean(bandR));
      texel[1] = float(pixel.mean(bandG));
      texel[2] = float(pixel.mean(bandB));
    }
    return rgbImage;
  }
  if (coverage < 0.9) {
    char note[160]{};
    std::snprintf(note, sizeof(note),
                  "spectral to RGB: the grid covers %.0f%% of the visible, "
                  "true color will be band-limited (dim or tinted)\n",
                  100.0 * coverage);
    std::cerr << note;
  }
  for (size_t y{}; y < numPixelsY; y++) {
    for (size_t x{}; x < numPixelsX; x++) {
      auto color{Color()};
      auto pixel{spectral(x, y)};
      for (size_t i = 0; i < color.size(); i++) color[i] = float(pixel.mean(i));
      auto state{makeRenderState(wavelengths)};
      auto rgb{compiler.convertColorToRGB(state, color.data())};
      auto texel{&rgbImage[3 * (x + numPixelsX * y)]};
      texel[0] = rgb[0];
      texel[1] = rgb[1];
      texel[2] = rgb[2];
    }
  }
  return rgbImage;
}

//--}

//--{ Display curves

// The Khronos PBR Neutral tone mapper. Below `PBR_NEUTRAL_START` the
// mapping is an exact shift, so base colors reproduce one to one; above
// it the peak channel rolls off hyperbolically toward 1 and the whole
// color is mixed toward that peak, desaturating bright colors to white
// the way film does instead of clipping them channel by channel to a
// primary. The quadratic toe below `PBR_NEUTRAL_TOE_END` keeps the
// shift from lifting black off zero. The curve is unbounded above,
// which is what lets the local operator below invert it everywhere.
constexpr float PBR_NEUTRAL_TOE_END{0.08f};
constexpr float PBR_NEUTRAL_OFFSET{0.04f};
constexpr float PBR_NEUTRAL_START{0.8f - PBR_NEUTRAL_OFFSET};
constexpr float PBR_NEUTRAL_DESATURATION{0.15f};

// The achromatic action of the tone mapper: what it does to a gray,
// which is the scalar curve the local operator needs to invert. The
// color path agrees with this on grays by construction, because the mix
// toward the peak is a no-op when every channel is the peak.
[[nodiscard]] static float pbrNeutralGray(float value) noexcept {
  const float peak{value < PBR_NEUTRAL_TOE_END ? 6.25f * value * value
                                               : value - PBR_NEUTRAL_OFFSET};
  if (peak < PBR_NEUTRAL_START) return peak;
  const float d{1.0f - PBR_NEUTRAL_START};
  return 1.0f - d * d / (peak + d - PBR_NEUTRAL_START);
}

[[nodiscard]] static float pbrNeutralGrayInverse(float display) noexcept {
  if (!(display > 0.0f)) return 0.0f;
  if (display < PBR_NEUTRAL_OFFSET) return std::sqrt(display / 6.25f);
  if (display < PBR_NEUTRAL_START) return display + PBR_NEUTRAL_OFFSET;
  const float d{1.0f - PBR_NEUTRAL_START};
  return d * d / (1.0f - display) - d + PBR_NEUTRAL_START + PBR_NEUTRAL_OFFSET;
}

static void pbrNeutralRGB(const float *rgb, float *out) noexcept {
  std::array<float, 3> color = {std::fmax(rgb[0], 0.0f),
                                std::fmax(rgb[1], 0.0f),
                                std::fmax(rgb[2], 0.0f)};
  // Subtracting the offset from every channel, tapering to zero as the
  // darkest channel approaches black, is the toe. It never drives a
  // channel negative: the darkest is left at 6.25 x^2 >= 0 in the toe,
  // and no smaller than 0.04 above it.
  const float low{std::fmin(color[0], std::fmin(color[1], color[2]))};
  const float offset{low < PBR_NEUTRAL_TOE_END ? low - 6.25f * low * low
                                               : PBR_NEUTRAL_OFFSET};
  for (auto &channel : color) channel -= offset;
  const float peak{std::fmax(color[0], std::fmax(color[1], color[2]))};
  if (peak < PBR_NEUTRAL_START) {
    out[0] = color[0], out[1] = color[1], out[2] = color[2];
    return;
  }
  const float d{1.0f - PBR_NEUTRAL_START};
  const float newPeak{1.0f - d * d / (peak + d - PBR_NEUTRAL_START)};
  for (auto &channel : color) channel *= newPeak / peak;
  // How far the peak had to travel is how saturated the color was for
  // its intensity, so it drives how much the color bleaches. This is
  // the whole reason bright colors go white rather than to a primary.
  const float mix{1.0f -
                  1.0f / (PBR_NEUTRAL_DESATURATION * (peak - newPeak) + 1.0f)};
  for (auto &channel : color) channel += mix * (newPeak - channel);
  out[0] = color[0], out[1] = color[1], out[2] = color[2];
}

// A display curve: a monotone map from a linear display-referred value
// to the [0, 1] display range, with an inverse.
//
// The scalar form runs on luminance and is what the local operator
// inverts to recover an exposure; `applyRGB` is the color path. Each
// curve is free to treat color its own way as long as the two agree on
// grays, which is all the local operator relies on.
struct DisplayCurve final {
  enum Kind { GAMMA, LOG, FILMIC };

  Kind kind{GAMMA};

  float logDecades{4.0f};

  [[nodiscard]] float apply(float value) const noexcept {
    switch (kind) {
    case GAMMA:
      return std::pow(std::fmin(std::fmax(value, 0.0f), 1.0f), 1.0f / 2.2f);
    case LOG:
      return value > 0.0f
                 ? std::fmin(
                       std::fmax(1.0f + std::log10(value) / logDecades, 0.0f),
                       1.0f)
                 : 0.0f;
    default:
      return std::pow(std::fmin(std::fmax(pbrNeutralGray(value), 0.0f), 1.0f),
                      1.0f / 2.2f);
    }
  }

  [[nodiscard]] float applyInverse(float display) const noexcept {
    // Every curve saturates or asymptotes at 1, so hold the top of the
    // range just short of it. The caller only ever feeds this a fused
    // display value that is about to become a bounded exposure, so the
    // exact cutoff matters far less than staying finite.
    display = std::fmin(std::fmax(display, 0.0f), 1.0f - 1e-4f);
    switch (kind) {
    case GAMMA:
      return std::pow(display, 2.2f);
    case LOG:
      return std::pow(10.0f, (display - 1.0f) * logDecades);
    default:
      return pbrNeutralGrayInverse(std::pow(display, 2.2f));
    }
  }

  void applyRGB(const float *rgb, float *out) const noexcept {
    switch (kind) {
    case GAMMA:
      for (int i = 0; i < 3; i++) out[i] = apply(rgb[i]);
      return;
    case LOG: {
      // Scale the channels by the luminance ratio so the log curve
      // moves brightness without moving hue. The luminance here is
      // deliberately the channel mean, not the Rec.709 weighting.
      const float r{std::fmax(rgb[0], 0.0f)};
      const float g{std::fmax(rgb[1], 0.0f)};
      const float b{std::fmax(rgb[2], 0.0f)};
      const float lum{(r + g + b) / 3.0f};
      const float scale{lum > 0.0f ? apply(lum) / lum : 0.0f};
      out[0] = std::fmin(r * scale, 1.0f);
      out[1] = std::fmin(g * scale, 1.0f);
      out[2] = std::fmin(b * scale, 1.0f);
      return;
    }
    default:
      pbrNeutralRGB(rgb, out);
      for (int i = 0; i < 3; i++)
        out[i] =
            std::pow(std::fmin(std::fmax(out[i], 0.0f), 1.0f), 1.0f / 2.2f);
      return;
    }
  }
};

//--}
//--{ Gaussian and Laplacian pyramids

// One single-channel image, the unit the pyramid routines pass around.
struct Plane final {
  size_t sizeX{};

  size_t sizeY{};

  std::vector<float> values{};

  [[nodiscard]] static Plane zeros(size_t sizeX, size_t sizeY) {
    return Plane{sizeX, sizeY, std::vector<float>(sizeX * sizeY)};
  }

  [[nodiscard]] float at(size_t x, size_t y) const noexcept {
    return values[x + sizeX * y];
  }
};

// Clamp a possibly out-of-bounds tap back into the image, so edges
// repeat rather than darken.
[[nodiscard]] static size_t clampIndex(long i, size_t size) noexcept {
  return size_t(std::min<long>(std::max<long>(i, 0), long(size) - 1));
}

// Burt-Adelson reduce: the separable [1 4 6 4 1]/16 kernel, then keep
// the even samples.
[[nodiscard]] static Plane pyramidDown(const Plane &src) {
  constexpr float K[5]{1.0f / 16, 4.0f / 16, 6.0f / 16, 4.0f / 16, 1.0f / 16};
  const size_t sizeX{std::max<size_t>((src.sizeX + 1) / 2, 1)};
  const size_t sizeY{std::max<size_t>((src.sizeY + 1) / 2, 1)};
  auto rows{Plane::zeros(sizeX, src.sizeY)};
  for (size_t y{}; y < src.sizeY; y++)
    for (size_t x{}; x < sizeX; x++) {
      float sum{};
      for (int k = -2; k <= 2; k++)
        sum += K[k + 2] * src.at(clampIndex(long(2 * x) + k, src.sizeX), y);
      rows.values[x + sizeX * y] = sum;
    }
  auto dst{Plane::zeros(sizeX, sizeY)};
  for (size_t y{}; y < sizeY; y++)
    for (size_t x{}; x < sizeX; x++) {
      float sum{};
      for (int k = -2; k <= 2; k++)
        sum += K[k + 2] * rows.at(x, clampIndex(long(2 * y) + k, src.sizeY));
      dst.values[x + sizeX * y] = sum;
    }
  return dst;
}

// The matching expand, back to an explicitly given size because halving
// rounds up and so is not invertible from the small size alone.
//
// This is the zero-stuffed [1 4 6 4 1]/8 convolution written out: an
// output sample at an even index averages the three nearest parents by
// (1, 6, 1)/8, one at an odd index the two parents straddling it.
// Perfect reconstruction would hold for any expand as long as the build
// and the collapse agree, but the proper kernel is what keeps each
// Laplacian level band-passed, and fusion reweights those levels
// against each other.
[[nodiscard]] static Plane pyramidUp(const Plane &src, size_t sizeX,
                                     size_t sizeY) {
  auto tap{
      [](const Plane &plane, size_t x, size_t y, long offsetX, long offsetY) {
        return plane.at(clampIndex(long(x) + offsetX, plane.sizeX),
                        clampIndex(long(y) + offsetY, plane.sizeY));
      }};
  auto cols{Plane::zeros(sizeX, src.sizeY)};
  for (size_t y{}; y < src.sizeY; y++)
    for (size_t x{}; x < sizeX; x++) {
      const size_t m{x / 2};
      cols.values[x + sizeX * y] =
          x % 2 == 0 ? (tap(src, m, y, -1, 0) + 6.0f * tap(src, m, y, 0, 0) +
                        tap(src, m, y, +1, 0)) /
                           8.0f
                     : (tap(src, m, y, 0, 0) + tap(src, m, y, +1, 0)) / 2.0f;
    }
  auto dst{Plane::zeros(sizeX, sizeY)};
  for (size_t y{}; y < sizeY; y++)
    for (size_t x{}; x < sizeX; x++) {
      const size_t m{y / 2};
      dst.values[x + sizeX * y] =
          y % 2 == 0 ? (tap(cols, x, m, 0, -1) + 6.0f * tap(cols, x, m, 0, 0) +
                        tap(cols, x, m, 0, +1)) /
                           8.0f
                     : (tap(cols, x, m, 0, 0) + tap(cols, x, m, 0, +1)) / 2.0f;
    }
  return dst;
}

// How many pyramid levels an image of this size supports, stopping
// before any dimension reaches zero. Fusion wants the full depth: the
// coarsest level is what carries the large-scale exposure decisions.
[[nodiscard]] static size_t pyramidDepth(size_t sizeX, size_t sizeY) noexcept {
  size_t depth{1};
  for (size_t size = std::min(sizeX, sizeY); size > 1; size = (size + 1) / 2)
    depth++;
  return depth;
}

[[nodiscard]] static std::vector<Plane> buildGaussian(Plane image,
                                                      size_t numLevels) {
  auto levels{std::vector<Plane>()};
  levels.reserve(numLevels);
  for (size_t l = 0; l + 1 < numLevels; l++) {
    auto next{pyramidDown(image)};
    levels.push_back(std::move(image));
    image = std::move(next);
  }
  levels.push_back(std::move(image));
  return levels;
}

// The Laplacian pyramid: level l holds the detail lost in reducing
// level l, and the last level is the residual that everything is
// rebuilt on top of.
[[nodiscard]] static std::vector<Plane> buildLaplacian(Plane image,
                                                       size_t numLevels) {
  auto levels{std::vector<Plane>()};
  levels.reserve(numLevels);
  for (size_t l = 0; l + 1 < numLevels; l++) {
    auto next{pyramidDown(image)};
    auto expanded{pyramidUp(next, image.sizeX, image.sizeY)};
    for (size_t i = 0; i < image.values.size(); i++)
      image.values[i] -= expanded.values[i];
    levels.push_back(std::move(image));
    image = std::move(next);
  }
  levels.push_back(std::move(image));
  return levels;
}

[[nodiscard]] static Plane collapseLaplacian(const std::vector<Plane> &levels) {
  auto image{levels.back()};
  for (size_t l = levels.size() - 1; l-- > 0;) {
    auto expanded{pyramidUp(image, levels[l].sizeX, levels[l].sizeY)};
    for (size_t i = 0; i < expanded.values.size(); i++)
      expanded.values[i] += levels[l].values[i];
    image = std::move(expanded);
  }
  return image;
}

//--}

//--{ Local operator: exposure fusion

// Exposure fusion (Mertens, Kautz, and Van Reeth 2007) over a synthetic
// bracket of the render itself, reduced to a smooth per-pixel exposure.
// The renderer knows the scene's whole dynamic range, so the "bracket"
// is exact: multiply by a gain and run the display curve. Each exposure
// is scored per pixel by how close it lands to mid gray, and the scores
// blend the exposures' Laplacian pyramids using Gaussian pyramids of
// the scores, which is what keeps a bright region's exposure from
// haloing across edges.
//
// What comes back is not an image but a gain field, taken relative to
// the ladder's own middle exposure, which `autoExposure` reports. The
// color is never touched here; it goes through the display curve once,
// at a locally adjusted exposure, so a strength of 0 provably
// degenerates to the global operator.
//
// Returns an empty vector when the image has too little to work with,
// which the caller reads as "no local exposure".
[[nodiscard]] static std::vector<float>
computeFusionGain(const std::vector<float> &image, size_t numPixelsX,
                  size_t numPixelsY, const DisplayCurve &curve,
                  const TonemapOptions &options, float &autoExposure) {
  const size_t numPixels{numPixelsX * numPixelsY};
  auto luminance{std::vector<float>(numPixels)};
  for (size_t p = 0; p < numPixels; p++) {
    const auto texel{&image[3 * p]};
    luminance[p] = std::fmax(
        0.2126f * texel[0] + 0.7152f * texel[1] + 0.0722f * texel[2], 0.0f);
  }
  // The ladder spans the 1st to 99th percentile of the luminance that
  // is actually there. Percentiles rather than extremes, so one firefly
  // or one black pixel cannot set the range.
  auto sorted{std::vector<float>()};
  sorted.reserve(numPixels);
  for (size_t p = 0; p < numPixels; p++)
    if (luminance[p] > 0.0f) sorted.push_back(luminance[p]);
  if (sorted.size() < 64) {
    std::cerr << "fusion tonemap: fewer than 64 lit pixels, "
                 "no local exposure applied\n";
    return {};
  }
  auto percentile{[&](double fraction) {
    const size_t i{std::min(size_t(fraction * double(sorted.size() - 1)),
                            sorted.size() - 1)};
    std::nth_element(sorted.begin(), sorted.begin() + long(i), sorted.end());
    return sorted[i];
  }};
  const float lumLo{percentile(0.01)};
  const float lumHi{percentile(0.99)};
  if (!(lumLo > 0.0f) || !(lumHi > lumLo)) {
    std::cerr << "fusion tonemap: the image has no dynamic range to "
                 "redistribute, no local exposure applied\n";
    return {};
  }
  // The middle of the ladder is the reference exposure everything else
  // is measured against, and it is what the operator falls back to at
  // zero strength.
  autoExposure = 0.18f / std::sqrt(lumLo * lumHi);
  // Quantize the span to a quarter stop so two renders of the same
  // scene at different sample counts pick the same ladder.
  float spanInEV{options.localRange > 0.0f ? options.localRange
                                           : std::log2(lumHi / lumLo)};
  spanInEV =
      std::round(std::fmin(std::fmax(spanInEV, 2.0f), 16.0f) * 4.0f) / 4.0f;
  const size_t numExposures{std::min<size_t>(
      std::max<size_t>(size_t(std::lround(spanInEV / 2.0f)) + 1, 2), 9)};
  auto exposureGain{std::vector<float>(numExposures)};
  for (size_t k = 0; k < numExposures; k++)
    exposureGain[k] =
        autoExposure *
        std::exp2(spanInEV * (0.5f - float(k) / float(numExposures - 1)));
  // Well-exposedness alone, no contrast or saturation term. Weighting
  // by saturation is what gives fused images the lurid look, and the
  // contrast term buys little once the exposures are exact rather than
  // photographed.
  auto wellExposed{[](float display) {
    const float d{display - 0.5f};
    return std::exp(-d * d / (2.0f * 0.2f * 0.2f)) + 1e-6f;
  }};
  auto weightSum{std::vector<float>(numPixels)};
  for (size_t k = 0; k < numExposures; k++)
    for (size_t p = 0; p < numPixels; p++)
      weightSum[p] += wellExposed(curve.apply(exposureGain[k] * luminance[p]));
  // Accumulate the blend one exposure at a time, so only two pyramids
  // plus the accumulator are ever live.
  const size_t numLevels{pyramidDepth(numPixelsX, numPixelsY)};
  auto fused{std::vector<Plane>()};
  for (size_t k = 0; k < numExposures; k++) {
    auto display{Plane::zeros(numPixelsX, numPixelsY)};
    auto weight{Plane::zeros(numPixelsX, numPixelsY)};
    for (size_t p = 0; p < numPixels; p++) {
      display.values[p] = curve.apply(exposureGain[k] * luminance[p]);
      weight.values[p] = wellExposed(display.values[p]) / weightSum[p];
    }
    const auto detail{buildLaplacian(std::move(display), numLevels)};
    const auto score{buildGaussian(std::move(weight), numLevels)};
    if (fused.empty()) {
      fused.reserve(numLevels);
      for (const auto &level : detail)
        fused.push_back(Plane::zeros(level.sizeX, level.sizeY));
    }
    for (size_t l = 0; l < numLevels; l++)
      for (size_t i = 0; i < fused[l].values.size(); i++)
        fused[l].values[i] += score[l].values[i] * detail[l].values[i];
  }
  const auto fusedDisplay{collapseLaplacian(fused)};
  // Read the fused lightness back as an exposure: what the curve would
  // have needed at this pixel to land there, relative to the reference.
  auto gain{std::vector<float>(numPixels, 1.0f)};
  auto deviation{std::vector<float>()};
  deviation.reserve(numPixels);
  for (size_t p = 0; p < numPixels; p++) {
    if (!(luminance[p] > 0.0f)) continue; // Black stays black at any gain.
    const float target{curve.applyInverse(fusedDisplay.values[p])};
    float ev{
        std::log2(std::fmax(target, 1e-30f) / (autoExposure * luminance[p]))};
    ev = std::fmin(std::fmax(ev, -options.localClamp), options.localClamp);
    deviation.push_back(ev);
    gain[p] = std::exp2(options.localStrength * ev);
  }
  auto evPercentile{[&](double fraction) {
    if (deviation.empty()) return 0.0f;
    const size_t i{std::min(size_t(fraction * double(deviation.size() - 1)),
                            deviation.size() - 1)};
    std::nth_element(deviation.begin(), deviation.begin() + long(i),
                     deviation.end());
    return deviation[i];
  }};
  const float evLo{evPercentile(0.05)};
  const float evHi{evPercentile(0.95)};
  char note[192]{};
  std::snprintf(note, sizeof(note),
                "fusion tonemap: %.2f EV span over %zu exposures, auto "
                "exposure %.3g, local exposure %+.2f to %+.2f EV\n",
                double(spanInEV), numExposures, double(autoExposure),
                double(options.localStrength * evLo),
                double(options.localStrength * evHi));
  std::cerr << note;
  return gain;
}

//--}

//--{ Appearance stage

// The appearance stage's output: the linear display color to map, and a
// multiplier applied after any local exposure.
//
// The split matters when night mode and fusion are both on. Fusion's
// auto exposure is scale invariant, so anything folded into the image
// cancels out, while the night filter's deliberate dimming is a
// perceptual statement that has to survive.
struct Appearance final {
  std::vector<float> image{};

  float scale{1.0f};
};

// Perceptual low-light reproduction in the spirit of Jensen et
// al. 2000 and Kirk & O'Brien 2011. The spectral buffer gives the
// absolute photopic and scotopic luminances of every pixel; the
// mesopic blend between cone color and the blue-shifted colorless
// rod signal runs on those PHYSICAL values, modeling the observer
// standing in the scene. Only afterward is display brightness
// auto-exposed (white at the 99.5th-percentile channel value,
// with -exposure as a relative adjustment), so the picture is
// visible at any absolute level: moonlight comes out dim-looking,
// desaturated, and blue, while a daylight render passes through
// as ordinary auto-exposed color.
[[nodiscard]] static Appearance
applyNightFilter(const std::vector<float> &rgbImage,
                 const smdl::SpectralRenderImage &spectral,
                 const Color &wavelengths) {
  const size_t numPixelsX{spectral.getNumPixelsX()};
  const size_t numPixelsY{spectral.getNumPixelsY()};
  // Luminous efficiency as Gaussian fits over wavelength in um:
  // photopic V(l) ~ 1.019 exp(-285.4 (l - 0.5590)^2), 683 lm/W at
  // peak; scotopic V'(l) ~ 0.992 exp(-321.9 (l - 0.5030)^2), 1700
  // lm/W at peak. Trapezoid weights over the render's band grid.
  const size_t numBands{wavelengths.size()};
  auto weightPhotopic{std::vector<double>(numBands)};
  auto weightScotopic{std::vector<double>(numBands)};
  const double dLambda{numBands > 1 ? (double(wavelengths[numBands - 1]) -
                                       double(wavelengths[0])) /
                                          double(numBands - 1)
                                    : 1.0};
  // A non-uniform grid carries its own trapezoid band widths; the
  // uniform formula below is what those widths degenerate to on a
  // uniform grid, kept spelled out so the default render is unchanged
  // to the bit.
  const auto &quadWeights{renderWavelengthWeights()};
  double photopicMass{};
  for (size_t i = 0; i < numBands; i++) {
    const double um{double(wavelengths[i]) * 1.0e-3};
    const double trap{i == 0 || i == numBands - 1 ? 0.5 : 1.0};
    const double width{quadWeights.empty() ? dLambda * trap
                                           : double(quadWeights[i])};
    weightPhotopic[i] = 683.0 * 1.019 *
                        std::exp(-285.4 * (um - 0.5590) * (um - 0.5590)) *
                        width;
    weightScotopic[i] = 1700.0 * 0.992 *
                        std::exp(-321.9 * (um - 0.5030) * (um - 0.5030)) *
                        width;
    photopicMass += weightPhotopic[i];
  }
  // A grid that misses the visible has no photopic signal to model an
  // observer with; keep the linear appearance rather than dividing by
  // zeros into a black frame.
  if (!(photopicMass > 1e-6)) {
    std::cerr << "night tonemap: the wavelength grid has no photopic "
                 "coverage, keeping the linear appearance\n";
    return Appearance{rgbImage, 1.0f};
  }
  // The classic scotopic blue shift, chromaticity xy = (0.25, 0.25),
  // as linear sRGB at unit relative luminance.
  constexpr double ROD_TINT[3] = {0.7062, 0.8240, 1.9656};
  // The mesopic range in cd/m^2: cones are fully adapted above
  // ~3, gone below ~0.005, smoothstepped in log luminance.
  const double logConesOut{std::log10(0.005)};
  const double logRodsSaturate{std::log10(3.0)};
  auto nightImage{std::vector<float>(rgbImage.size())};
  double luminanceSum{0.0};
  double coneWeightSum{0.0};
  for (size_t y{}; y < numPixelsY; y++) {
    for (size_t x{}; x < numPixelsX; x++) {
      auto pixel{spectral(x, y)};
      double yPhotopic{0.0};
      double yScotopic{0.0};
      for (size_t i = 0; i < numBands; i++) {
        const double radiance{std::fmax(0.0, pixel.mean(i))};
        yPhotopic += weightPhotopic[i] * radiance;
        yScotopic += weightScotopic[i] * radiance;
      }
      luminanceSum += yPhotopic;
      double u{yPhotopic > 0.0 ? (std::log10(yPhotopic) - logConesOut) /
                                     (logRodsSaturate - logConesOut)
                               : 0.0};
      u = std::fmin(std::fmax(u, 0.0), 1.0);
      const double coneWeight{u * u * (3.0 - 2.0 * u)};
      coneWeightSum += coneWeight;
      // The rod signal expressed on the RGB image's own photopic
      // scale: pixel luminance times the spectrum's scotopic-to-
      // photopic ratio, so the blend is immune to any calibration
      // constant in the spectral-to-RGB conversion.
      const auto texel{&rgbImage[3 * (x + numPixelsX * y)]};
      const double r{std::fmax(0.0, double(texel[0]))};
      const double g{std::fmax(0.0, double(texel[1]))};
      const double b{std::fmax(0.0, double(texel[2]))};
      const double yImage{0.2126 * r + 0.7152 * g + 0.0722 * b};
      const double rodY{yPhotopic > 0.0 ? yImage * (yScotopic / yPhotopic)
                                        : 0.0};
      const auto out{&nightImage[3 * (x + numPixelsX * y)]};
      out[0] = float(coneWeight * r + (1.0 - coneWeight) * rodY * ROD_TINT[0]);
      out[1] = float(coneWeight * g + (1.0 - coneWeight) * rodY * ROD_TINT[1]);
      out[2] = float(coneWeight * b + (1.0 - coneWeight) * rodY * ROD_TINT[2]);
    }
  }
  // Auto white point. This is folded into the image rather than left in
  // the scale because it is an exposure decision, and so is exactly what
  // fusion should be free to supersede.
  auto peaks{std::vector<float>(numPixelsX * numPixelsY)};
  for (size_t p = 0; p < peaks.size(); p++)
    peaks[p] =
        std::fmax(nightImage[3 * p + 0],
                  std::fmax(nightImage[3 * p + 1], nightImage[3 * p + 2]));
  const size_t nth{
      std::min(size_t(0.995 * double(peaks.size())), peaks.size() - 1)};
  std::nth_element(peaks.begin(), peaks.begin() + long(nth), peaks.end());
  float white{peaks[nth]};
  if (!(white > 0.0f)) white = 1.0f;
  for (auto &value : nightImage) value /= white;
  // The night filter's mild darkening (Jensen et al.): the display
  // is viewed photopically, so a scotopic scene reads "dim" only
  // if reproduced below the display's white. Scale with the mean
  // cone weight, bottoming out at ~40% for fully rod-dominated
  // scenes; daylight passes through at full brightness. This one is
  // a statement about appearance, not exposure, so it stays in the
  // scale where no auto exposure can cancel it.
  const double meanConeWeight{coneWeightSum / double(peaks.size())};
  char note[160]{};
  std::snprintf(note, sizeof(note),
                "night tonemap: mean luminance %.3g cd/m^2, mean cone "
                "weight %.2f, auto white point %.3g\n",
                luminanceSum / double(peaks.size()), meanConeWeight,
                double(white));
  std::cerr << note;
  return Appearance{std::move(nightImage), float(0.4 + 0.6 * meanConeWeight)};
}

//--}

//--{ Option names

// The appearance stage, once the name is known to be one of the three.
enum class AppearanceMode { LINEAR, LOG, NIGHT };

// These three are the only place a name is spelled out, so validating
// early and resolving later cannot drift apart.
[[nodiscard]] static AppearanceMode parseMode(const std::string &mode) {
  if (mode == "linear") return AppearanceMode::LINEAR;
  if (mode == "log") return AppearanceMode::LOG;
  if (mode == "night") return AppearanceMode::NIGHT;
  throw smdl::Error(smdl::concat("unknown -tonemap mode ", mode,
                                 " (expected 'linear', 'log', or 'night')"));
}

[[nodiscard]] static DisplayCurve::Kind parseCurve(const std::string &curve) {
  if (curve == "gamma") return DisplayCurve::GAMMA;
  if (curve == "log") return DisplayCurve::LOG;
  if (curve == "filmic") return DisplayCurve::FILMIC;
  throw smdl::Error(smdl::concat("unknown -curve ", curve,
                                 " (expected 'gamma', 'log', or 'filmic')"));
}

[[nodiscard]] static bool parseLocalIsFusion(const std::string &local) {
  if (local == "off") return false;
  if (local == "fusion") return true;
  throw smdl::Error(
      smdl::concat("unknown -local ", local, " (expected 'off' or 'fusion')"));
}

//--}

void validateTonemapOptions(const TonemapOptions &options) {
  (void)parseMode(options.mode);
  (void)parseCurve(options.curve);
  (void)parseLocalIsFusion(options.local);
}

std::vector<uint8_t> tonemap(const TonemapOptions &options,
                             const std::vector<float> &rgbImage,
                             const smdl::SpectralRenderImage &spectral,
                             const Color &wavelengths) {
  const size_t numPixelsX{spectral.getNumPixelsX()};
  const size_t numPixelsY{spectral.getNumPixelsY()};
  const auto mode{parseMode(options.mode)};
  auto curve{DisplayCurve{}};
  curve.kind = parseCurve(options.curve);
  curve.logDecades = std::max(0.1f, options.logDecades);
  auto appearance{Appearance{}};
  if (mode == AppearanceMode::NIGHT) {
    appearance = applyNightFilter(rgbImage, spectral, wavelengths);
  } else {
    appearance = Appearance{rgbImage, 1.0f};
    // '-tonemap log' is shorthand for '-tonemap linear -curve log'.
    if (mode == AppearanceMode::LOG) curve.kind = DisplayCurve::LOG;
  }
  auto gain{std::vector<float>()};
  float autoExposure{1.0f};
  if (parseLocalIsFusion(options.local))
    gain = computeFusionGain(appearance.image, numPixelsX, numPixelsY, curve,
                             options, autoExposure);
  const float scale{options.exposure * appearance.scale *
                    (gain.empty() ? 1.0f : autoExposure)};
  auto ldrImage{std::vector<uint8_t>(appearance.image.size())};
  for (size_t p = 0; p < numPixelsX * numPixelsY; p++) {
    const float pixelScale{scale * (gain.empty() ? 1.0f : gain[p])};
    const auto texel{&appearance.image[3 * p]};
    const float rgb[3]{pixelScale * texel[0], pixelScale * texel[1],
                       pixelScale * texel[2]};
    float display[3]{};
    curve.applyRGB(rgb, display);
    for (int i = 0; i < 3; i++)
      ldrImage[3 * p + i] = uint8_t(
          std::round(255.0f * std::fmin(std::fmax(display[i], 0.0f), 1.0f)));
  }
  return ldrImage;
}
