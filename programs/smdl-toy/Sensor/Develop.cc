#include <algorithm>
#include <cmath>
#include <cstdio>
#include <iostream>
#include <string>

#include "smdl/Compiler.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Parallel.h"
#include "smdl/Support/Strings.h"

#include "Sensor/Colorimetry.h"
#include "Sensor/Develop.h"

//--{ The observer's develop

namespace {

// The fraction of the photopic luminous mass the wavelength grid can
// see: a fine sweep of the visible, crediting the mass wherever some
// band lies within 30nm. This decides whether the CIE projection of
// the film means anything as a picture.
[[nodiscard]] double visibleCoverage(const Color &wavelengths) {
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
[[nodiscard]] size_t nearestBand(const Color &wavelengths, float lambda) {
  size_t best{0};
  for (size_t i = 1; i < wavelengths.size(); i++)
    if (std::abs(wavelengths[i] - lambda) <
        std::abs(wavelengths[best] - lambda))
      best = i;
  return best;
}

} // namespace

std::vector<float> resolveRGB(smdl::Compiler &compiler,
                              const smdl::SpectralFilm &film,
                              const Color &wavelengths,
                              const RGBPolicy &policy) {
  const size_t numPixelsX{film.getNumPixelsX()};
  const size_t numPixelsY{film.getNumPixelsY()};
  const size_t numBands{film.getNumBands()};
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
  } else if (policy.shouldForceFalseColor || coverage < 0.35) {
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
      const size_t x{p % numPixelsX}, y{p / numPixelsX};
      double mean{};
      for (size_t i = 0; i < numBands; i++) mean += filmMean(film, x, y, i);
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
                  policy.shouldForceFalseColor ? " (forced)" : "",
                  double(wavelengths[bandR]), double(wavelengths[bandG]),
                  double(wavelengths[bandB]), 100.0 * coverage);
    std::cerr << note;
    for (size_t p = 0; p < numPixelsX * numPixelsY; p++) {
      const size_t x{p % numPixelsX}, y{p / numPixelsX};
      auto texel{&rgbImage[3 * p]};
      texel[0] = float(filmMean(film, x, y, bandR));
      texel[1] = float(filmMean(film, x, y, bandG));
      texel[2] = float(filmMean(film, x, y, bandB));
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
  // The JIT'd conversion reads only the state it is handed, so the rows
  // go in parallel.
  smdl::parallelFor(0, numPixelsY, [&](size_t y) {
    const auto state{makeRenderState(wavelengths)};
    for (size_t x{}; x < numPixelsX; x++) {
      auto color{Color()};
      for (size_t i = 0; i < color.size(); i++)
        color[i] = float(filmMean(film, x, y, i));
      const auto rgb{compiler.convertColorToRGB(state, color.data())};
      auto texel{&rgbImage[3 * (x + numPixelsX * y)]};
      texel[0] = rgb[0];
      texel[1] = rgb[1];
      texel[2] = rgb[2];
    }
  });
  return rgbImage;
}

//--}

//--{ The physical develop

namespace {

// McCamy's cubic follows the Planckian locus over this range and wanders
// off it past either end, so the gray world's temperature is held
// within it.
constexpr double AUTO_KELVIN_MIN{2000.0};
constexpr double AUTO_KELVIN_MAX{12000.0};

// The band the tile puts at pixel (x, y), anchored at the frame's origin
// as the response anchors it.
[[nodiscard]] size_t tileBandAt(const ResponseSettings &response, size_t x,
                                size_t y) noexcept {
  return response.cfa[(y % response.cfaRows()) * response.cfaColumns +
                      x % response.cfaColumns];
}

// Where `band` sits in `bands`, or `bands.size()` if it is not there.
[[nodiscard]] size_t positionOf(smdl::Span<const size_t> bands,
                                size_t band) noexcept {
  for (size_t k = 0; k < bands.size(); k++)
    if (bands[k] == band) return k;
  return bands.size();
}

[[nodiscard]] bool isInside(int4 window, int x, int y) noexcept {
  return x >= window[0] && x < window[2] && y >= window[1] && y < window[3];
}

// Hamilton and Adams over the bilinear planes of a Bayer tile, whose
// three planes `planes` holds in the order of `bands`.
void refineHamiltonAdams(const ResponseSettings &response,
                         smdl::Span<const size_t> bands,
                         smdl::Span<const float> mosaic, size_t numPixelsX,
                         int4 window, std::vector<float> &planes) {
  const size_t green{bands[1]};
  const auto at{[&](int x, int y) {
    return double(mosaic[size_t(y) * numPixelsX + size_t(x)]);
  }};
  const auto plane{[&](int x, int y, size_t k) -> float & {
    return planes[(size_t(y) * numPixelsX + size_t(x)) * 3 + k];
  }};
  // Green at every red and blue sample two in from the window's edge,
  // where every tap is inside: along the direction whose green difference
  // and own second difference are the smaller, corrected by the second
  // difference, which a plane has none of.
  const auto rowsFrom{[&](int inset) {
    return std::pair(size_t(window[1] + inset),
                     size_t(std::max(window[3] - inset, window[1] + inset)));
  }};
  const auto [greenBegin, greenEnd]{rowsFrom(2)};
  smdl::parallelFor(greenBegin, greenEnd, [&](size_t row) {
    const int y{int(row)};
    for (int x = window[0] + 2; x < window[2] - 2; x++) {
      if (tileBandAt(response, size_t(x), size_t(y)) == green) continue;
      const double center{at(x, y)};
      const double left{at(x - 1, y)};
      const double right{at(x + 1, y)};
      const double up{at(x, y - 1)};
      const double down{at(x, y + 1)};
      const double across{2.0 * center - at(x - 2, y) - at(x + 2, y)};
      const double along{2.0 * center - at(x, y - 2) - at(x, y + 2)};
      const double gradientAcross{std::abs(left - right) + std::abs(across)};
      const double gradientAlong{std::abs(up - down) + std::abs(along)};
      plane(x, y, 1) = float(
          gradientAcross < gradientAlong ? 0.5 * (left + right) + 0.25 * across
          : gradientAlong < gradientAcross
              ? 0.5 * (up + down) + 0.25 * along
              : 0.25 * (left + right + up + down) + 0.125 * (across + along));
    }
  });
  // Red and blue two in from the edge, where every green they read is
  // interpolated from taps inside the window: green plus the mean of
  // their difference from green at their samples around, which are the
  // two beside a green sample and the four diagonal to the other color.
  const auto [colorBegin, colorEnd]{rowsFrom(2)};
  smdl::parallelFor(colorBegin, colorEnd, [&](size_t row) {
    const int y{int(row)};
    for (int x = window[0] + 2; x < window[2] - 2; x++) {
      const size_t here{tileBandAt(response, size_t(x), size_t(y))};
      for (const size_t k : {size_t(0), size_t(2)}) {
        if (here == bands[k]) continue;
        double total{};
        int count{};
        for (int dy = -1; dy <= 1; dy++) {
          for (int dx = -1; dx <= 1; dx++) {
            if (tileBandAt(response, size_t(x + dx), size_t(y + dy)) !=
                bands[k])
              continue;
            total += at(x + dx, y + dy) - double(plane(x + dx, y + dy, 1));
            count++;
          }
        }
        if (count > 0)
          plane(x, y, k) = float(double(plane(x, y, 1)) + total / count);
      }
    }
  });
}

// The frame's gray world: the mean of each of `bands` over the window's
// samples of it below `saturation`, or, when every pixel holds every
// band, over the pixels with all of them below it. Each row sums for
// itself and the rows fold in order, so the means never depend on which
// thread took which row.
[[nodiscard]] std::vector<double> grayWorld(const ResponseSettings &response,
                                            smdl::Span<const size_t> bands,
                                            const std::vector<float> &values,
                                            size_t bandCount, size_t numPixelsX,
                                            int4 window, float saturation) {
  const size_t numBands{bands.size()};
  const size_t numRows{size_t(window[3] - window[1])};
  auto rows{std::vector<double>(numRows * numBands * 2)};
  smdl::parallelFor(0, numRows, [&](size_t row) {
    const size_t y{size_t(window[1]) + row};
    auto *tally{&rows[row * numBands * 2]};
    for (size_t x = size_t(window[0]); x < size_t(window[2]); x++) {
      const size_t pixel{y * numPixelsX + x};
      if (response.hasCFA()) {
        const size_t k{positionOf(bands, tileBandAt(response, x, y))};
        if (k < numBands && values[pixel] < saturation) {
          tally[2 * k] += double(values[pixel]);
          tally[2 * k + 1] += 1.0;
        }
        continue;
      }
      const auto *samples{&values[pixel * bandCount]};
      if (!std::all_of(bands.begin(), bands.end(),
                       [&](size_t b) { return samples[b] < saturation; }))
        continue;
      for (size_t k = 0; k < numBands; k++) {
        tally[2 * k] += double(samples[bands[k]]);
        tally[2 * k + 1] += 1.0;
      }
    }
  });
  auto means{std::vector<double>(numBands)};
  for (size_t k = 0; k < numBands; k++) {
    double total{};
    double count{};
    for (size_t row = 0; row < numRows; row++) {
      total += rows[(row * numBands + k) * 2];
      count += rows[(row * numBands + k) * 2 + 1];
    }
    means[k] = count > 0 ? total / count : 0.0;
  }
  return means;
}

[[nodiscard]] std::string spellBands(const ResponseSettings &response,
                                     smdl::Span<const size_t> bands) {
  auto text{std::string()};
  for (size_t k = 0; k < bands.size(); k++)
    text += smdl::concat(k > 0 ? ", " : "", response.bands[bands[k]].name);
  return text;
}

[[nodiscard]] std::string describeDemosaic(DemosaicMethod method,
                                           const ResponseSettings &response) {
  switch (method) {
  case DemosaicMethod::HAMILTON_ADAMS:
    return "Hamilton-Adams demosaic";
  case DemosaicMethod::BILINEAR:
    return smdl::concat("bilinear demosaic over the ", response.cfaColumns, "x",
                        response.cfaRows(), " tile");
  default:
    return "no demosaic, every pixel holding every band";
  }
}

} // namespace

DemosaicMethod demosaicMethod(const ResponseSettings &response,
                              smdl::Span<const size_t> bands) {
  if (!response.hasCFA()) return DemosaicMethod::NONE;
  if (bands.size() == 3 && response.cfaColumns == 2 &&
      response.cfaRows() == 2) {
    const auto &cfa{response.cfa};
    const size_t green{bands[1]};
    // Green twice on one diagonal, and red and blue once each on the
    // other.
    const bool isGreenOnLead{cfa[0] == green && cfa[3] == green};
    const bool isGreenOnOff{cfa[1] == green && cfa[2] == green};
    const size_t a{isGreenOnLead ? cfa[1] : cfa[0]};
    const size_t b{isGreenOnLead ? cfa[2] : cfa[3]};
    const auto isRedOrBlue{
        [&](size_t band) { return band == bands[0] || band == bands[2]; }};
    if ((isGreenOnLead || isGreenOnOff) && a != b && isRedOrBlue(a) &&
        isRedOrBlue(b))
      return DemosaicMethod::HAMILTON_ADAMS;
  }
  return DemosaicMethod::BILINEAR;
}

std::vector<float> demosaic(DemosaicMethod method,
                            const ResponseSettings &response,
                            smdl::Span<const size_t> bands,
                            smdl::Span<const float> mosaic, size_t numPixelsX,
                            size_t numPixelsY, int4 window) {
  SMDL_SANITY_CHECK(response.hasCFA() &&
                    mosaic.size() == numPixelsX * numPixelsY);
  const size_t numBands{bands.size()};
  auto planes{std::vector<float>(numPixelsX * numPixelsY * numBands)};
  const int columns{int(response.cfaColumns)};
  const int rows{int(response.cfaRows())};
  // Bilinear everywhere first: the pixel's own value where the band
  // sampled it, and otherwise the tent-weighted mean of the band's
  // samples within a tile of it.
  smdl::parallelFor(size_t(window[1]), size_t(window[3]), [&](size_t row) {
    const int y{int(row)};
    for (int x = window[0]; x < window[2]; x++) {
      const size_t here{tileBandAt(response, size_t(x), size_t(y))};
      for (size_t k = 0; k < numBands; k++) {
        auto &out{planes[(size_t(y) * numPixelsX + size_t(x)) * numBands + k]};
        if (here == bands[k]) {
          out = mosaic[size_t(y) * numPixelsX + size_t(x)];
          continue;
        }
        double total{};
        double weight{};
        for (int dy = 1 - rows; dy < rows; dy++) {
          for (int dx = 1 - columns; dx < columns; dx++) {
            const int sx{x + dx};
            const int sy{y + dy};
            if (!isInside(window, sx, sy) ||
                tileBandAt(response, size_t(sx), size_t(sy)) != bands[k])
              continue;
            const double w{(1.0 - std::abs(dx) / double(columns)) *
                           (1.0 - std::abs(dy) / double(rows))};
            total += w * double(mosaic[size_t(sy) * numPixelsX + size_t(sx)]);
            weight += w;
          }
        }
        out = weight > 0 ? float(total / weight) : 0.0f;
      }
    }
  });
  if (method == DemosaicMethod::HAMILTON_ADAMS)
    refineHamiltonAdams(response, bands, mosaic, numPixelsX, window, planes);
  return planes;
}

std::vector<float> developReadout(const Sensor &sensor,
                                  const Detector &detector,
                                  const Readout &readout,
                                  const WhiteBalance &whiteBalance, int4 window,
                                  bool shouldLog) {
  const auto &response{sensor.settings().response};
  const size_t numPixelsX{readout.pixelCountX};
  const size_t numPixelsY{readout.pixelCountY};
  const size_t bandCount{readout.bandCount};
  SMDL_SANITY_CHECK(bandCount ==
                    (response.hasCFA() ? 1 : response.bands.size()));
  // Fractions of the top code over the black level, which keeps the read
  // noise's lower half as negatives; a sample at the white level reads
  // `saturation`, 1 unless the well clips below the top code.
  const double black{detector.blackLevel()};
  const double range{double(detector.topCode()) - black};
  const auto saturation{float((double(detector.whiteLevel()) - black) / range)};
  auto values{std::vector<float>(readout.digitalNumbers.size())};
  for (size_t i = 0; i < values.size(); i++)
    values[i] = float((double(readout.digitalNumbers[i]) - black) / range);
  // What the picture is made of: three bands through the fitted matrix,
  // or as they are when they fit the observer badly, or fewer as a gray.
  enum class Mode { COLOR, FALSE_COLOR, GRAY };
  const auto rgb{response.rgbBands()};
  auto bands{std::vector<size_t>()};
  if (rgb) {
    bands.assign(rgb->begin(), rgb->end());
  } else {
    for (size_t b = 0; b < response.bands.size(); b++) bands.push_back(b);
  }
  const auto bandSpan{smdl::Span<const size_t>(bands.data(), bands.size())};
  // The white balance: the illuminant it names, or for `auto` the one the
  // frame's gray world reads as.
  const bool isAuto{whiteBalance.kind == WhiteBalanceKind::AUTO};
  auto illuminant{whiteBalanceSpectrum(whiteBalance)};
  const auto gray{isAuto ? grayWorld(response, bandSpan, values, bandCount,
                                     numPixelsX, window, saturation)
                         : std::vector<double>()};
  const bool hasGray{isAuto &&
                     std::all_of(gray.begin(), gray.end(),
                                 [](double mean) { return mean > 0; })};
  double measuredKelvin{};
  auto mode{Mode::GRAY};
  auto fit{ColorFit{}};
  auto multipliers{std::vector<double>(bands.size(), 1.0)};
  size_t reference{sensor.peakBand()};
  if (rgb) {
    reference = (*rgb)[1];
    fit = sensor.fitColor(*rgb, illuminant);
    if (hasGray && !fit.isSingular) {
      auto balanced{smdl::double3()};
      for (size_t k = 0; k < 3; k++) balanced[k] = gray[k] * fit.multipliers[k];
      const auto xyz{fit.cameraToXYZ * balanced};
      if (const double sum{xyz.x + xyz.y + xyz.z}; sum > 0) {
        measuredKelvin =
            std::clamp(mccamyKelvin(smdl::double2(xyz.x / sum, xyz.y / sum)),
                       AUTO_KELVIN_MIN, AUTO_KELVIN_MAX);
        illuminant = kelvinSpectrum(measuredKelvin);
        fit = sensor.fitColor(*rgb, illuminant);
        for (size_t k = 0; k < 3; k++) fit.multipliers[k] = gray[1] / gray[k];
      }
    }
    mode = fit.isFaithful() ? Mode::COLOR : Mode::FALSE_COLOR;
    for (size_t k = 0; k < 3; k++) multipliers[k] = fit.multipliers[k];
  } else {
    // Each band against the most sensitive, whose saturation the ISO was
    // rated at.
    const double referenceRate{sensor.electronRate(reference, illuminant)};
    for (size_t k = 0; k < bands.size(); k++) {
      const double rate{sensor.electronRate(bands[k], illuminant)};
      multipliers[k] = hasGray    ? gray[reference] / gray[k]
                       : rate > 0 ? referenceRate / rate
                                  : 1.0;
    }
  }
  // The exposure: the baseline that puts a metered mean at middle gray,
  // times the ratio that keeps a neutral under the illuminant there
  // whichever band the ISO was rated in.
  const double rated{sensor.peakElectronsPerLuxSecond()};
  const double referenceRated{
      sensor.electronsPerLuxSecond(reference, illuminant)};
  const double exposure{
      DEVELOP_MIDDLE_GRAY * ISO_SATURATION_LUX_SECONDS / (METER_Q * METER_K) *
      (rated > 0 && referenceRated > 0 ? rated / referenceRated : 1.0)};
  // Balanced, and every sample held where the least multiplied band
  // saturates, so that a saturated white stays white rather than taking
  // the color of the other multipliers.
  const auto ceiling{
      float(double(saturation) *
            *std::min_element(multipliers.begin(), multipliers.end()))};
  smdl::parallelFor(size_t(window[1]), size_t(window[3]), [&](size_t y) {
    for (size_t x = size_t(window[0]); x < size_t(window[2]); x++) {
      const size_t pixel{y * numPixelsX + x};
      if (response.hasCFA()) {
        if (const size_t k{positionOf(bandSpan, tileBandAt(response, x, y))};
            k < bands.size())
          values[pixel] =
              std::min(float(double(values[pixel]) * multipliers[k]), ceiling);
        continue;
      }
      for (size_t k = 0; k < bands.size(); k++) {
        auto &value{values[pixel * bandCount + bands[k]]};
        value = std::min(float(double(value) * multipliers[k]), ceiling);
      }
    }
  });
  const auto method{demosaicMethod(response, bandSpan)};
  const auto planes{
      method == DemosaicMethod::NONE
          ? std::vector<float>()
          : demosaic(method, response, bandSpan,
                     smdl::Span<const float>(values.data(), values.size()),
                     numPixelsX, numPixelsY, window)};
  const auto sampleOf{[&](size_t pixel, size_t k) {
    return double(method == DemosaicMethod::NONE
                      ? values[pixel * bandCount + bands[k]]
                      : planes[pixel * bands.size() + k]);
  }};
  // Into linear sRGB: the fitted matrix to XYZ under the illuminant,
  // Bradford to the sRGB white, and the builtin's matrix out.
  const auto toSRGB{
      mode == Mode::COLOR
          ? xyzToLinearSRGB() *
                (bradfordAdaptation(fit.white, linearSRGBWhite()) *
                 fit.cameraToXYZ)
          : smdl::double3x3(1.0)};
  auto rgbImage{std::vector<float>(numPixelsX * numPixelsY * 3)};
  smdl::parallelFor(size_t(window[1]), size_t(window[3]), [&](size_t y) {
    for (size_t x = size_t(window[0]); x < size_t(window[2]); x++) {
      const size_t pixel{y * numPixelsX + x};
      auto color{smdl::double3()};
      if (mode == Mode::GRAY) {
        double total{};
        for (size_t k = 0; k < bands.size(); k++) total += sampleOf(pixel, k);
        color = smdl::double3(total / double(bands.size()));
      } else {
        color = toSRGB * smdl::double3(sampleOf(pixel, 0), sampleOf(pixel, 1),
                                       sampleOf(pixel, 2));
      }
      color *= exposure;
      auto *texel{&rgbImage[3 * pixel]};
      texel[0] = float(color.x);
      texel[1] = float(color.y);
      texel[2] = float(color.z);
    }
  });
  if (shouldLog) {
    const auto balance{
        !isAuto    ? whiteBalanceName(whiteBalance)
        : !hasGray ? std::string("auto, which found no gray world in the "
                                 "frame and took D65")
        : measuredKelvin > 0
            ? smdl::concat("auto, the frame's gray world reading ",
                           smdl::Brief(measuredKelvin, 4), " K")
            : std::string("auto, the frame's gray world")};
    auto factors{std::string()};
    for (size_t k = 0; k < bands.size(); k++)
      factors += smdl::concat(k > 0 ? ", " : "", response.bands[bands[k]].name,
                              " ", smdl::Brief(multipliers[k], 4));
    SMDL_LOG_INFO("Develop: white balance ", balance, ": ", factors,
                  ", every sample held at ", smdl::Brief(ceiling, 4));
    const auto how{smdl::concat(describeDemosaic(method, response),
                                "; exposed by ",
                                smdl::Brief(std::log2(exposure), 3),
                                " EV, so that a metered neutral develops to ",
                                smdl::Brief(DEVELOP_MIDDLE_GRAY, 3))};
    const auto names{spellBands(response, bandSpan)};
    if (mode == Mode::COLOR)
      SMDL_LOG_INFO("Develop: ", names,
                    " to XYZ through the matrix fitted "
                    "over ",
                    trainingReflectances().size(),
                    " training reflectances, a mean of ",
                    smdl::Brief(fit.meanDeltaE00, 3),
                    " dE00, then Bradford to the sRGB white; ", how);
    else if (mode == Mode::FALSE_COLOR)
      SMDL_LOG_WARN(
          "Develop: ", names,
          fit.isSingular
              ? std::string(" respond too much alike to tell colors apart")
              : smdl::concat(" fit the observer to a mean of ",
                             smdl::Brief(fit.meanDeltaE00, 3),
                             " dE00 over the training reflectances, past ",
                             smdl::Brief(FAITHFUL_FIT_DELTA_E00, 3)),
          ", so the picture is false color, each band on its own channel; ",
          how);
    else
      SMDL_LOG_INFO("Develop: ", bands.size(),
                    " band(s) cannot carry color, so the picture is gray; ",
                    how);
  }
  return rgbImage;
}

//--}
