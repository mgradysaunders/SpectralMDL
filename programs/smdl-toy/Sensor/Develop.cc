#include <algorithm>
#include <cmath>
#include <cstdio>
#include <iostream>
#include <string>

#include "smdl/Compiler.h"
#include "smdl/RenderUtil/Colorimetry.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Parallel.h"
#include "smdl/Support/Strings.h"

#include "Sensor/Develop.h"

namespace {

// The fraction of the photopic luminous mass the wavelength grid can
// see: a fine sweep of the visible, crediting the mass wherever some
// band lies within 30nm. This decides whether the CIE projection of
// the film means anything as a picture.
[[nodiscard]] double visibleCoverage(const Color &wavelengths) {
  double total{};
  double covered{};
  for (double lambda = 380.0; lambda <= 780.0; lambda += 5.0) {
    const double mass{smdl::photopicV(lambda)};
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
  std::vector<float> rgbImage(numPixelsX * numPixelsY * 3);
  // Down to 90% coverage the CIE projection is a faithful picture, and
  // down to 35% it is still the physically correct band-limited view
  // (tinted or dim) and only earns a note; below that a color has no
  // meaning and three bands map to R, G, B instead. Fewer than three
  // bands cannot even pretend, and write the grayscale mean radiance;
  // this outranks coverage because a lone mid-visible band can cover
  // much of the photopic mass while its "color" is still garbage.
  enum class Mode { TRUE_COLOR, FALSE_COLOR, GRAYSCALE };
  const double coverage{visibleCoverage(wavelengths)};
  Mode mode{Mode::TRUE_COLOR};
  if (numBands < 3) {
    mode = Mode::GRAYSCALE;
  } else if (policy.shouldForceFalseColor || coverage < 0.35) {
    mode = Mode::FALSE_COLOR;
  }
  if (mode == Mode::GRAYSCALE) {
    std::cerr << smdl::concat(
        "spectral to RGB: ", smdl::Counted(numBands, "band"),
        " cannot carry color, writing the grayscale mean radiance\n");
    for (size_t p = 0; p < numPixelsX * numPixelsY; p++) {
      const size_t x{p % numPixelsX}, y{p / numPixelsX};
      double mean{};
      for (size_t i = 0; i < numBands; i++) mean += filmMean(film, x, y, i);
      mean /= double(numBands);
      float *texel{&rgbImage[3 * p]};
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
      float *texel{&rgbImage[3 * p]};
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
    const smdl::State state{makeRenderState(wavelengths)};
    for (size_t x{}; x < numPixelsX; x++) {
      Color color{};
      for (size_t i = 0; i < color.size(); i++)
        color[i] = float(filmMean(film, x, y, i));
      const float3 rgb{compiler.convertColorToRGB(state, color.data())};
      float *texel{&rgbImage[3 * (x + numPixelsX * y)]};
      texel[0] = rgb[0];
      texel[1] = rgb[1];
      texel[2] = rgb[2];
    }
  });
  return rgbImage;
}

namespace {

// McCamy's cubic follows the Planckian locus over this range and wanders
// off it past either end, so the gray world's temperature is held
// within it.
constexpr double AUTO_KELVIN_MIN{2000.0};
constexpr double AUTO_KELVIN_MAX{12000.0};

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
  // Both passes below run two rows in from the window's edge, which is
  // where every tap they read is inside it.
  const size_t rowBegin{size_t(window[1] + 2)};
  const size_t rowEnd{size_t(std::max(window[3] - 2, window[1] + 2))};
  // Green at every red and blue sample: along the direction whose green
  // difference and own second difference are the smaller, corrected by
  // the second difference, which a plane has none of.
  smdl::parallelFor(rowBegin, rowEnd, [&](size_t row) {
    const int y{int(row)};
    for (int x = window[0] + 2; x < window[2] - 2; x++) {
      if (response.bandAt(size_t(x), size_t(y)) == green) continue;
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
  // Red and blue, whose green is interpolated from taps inside the
  // window: green plus the mean of their difference from green at their
  // samples around, which are the two beside a green sample and the four
  // diagonal to the other color.
  smdl::parallelFor(rowBegin, rowEnd, [&](size_t row) {
    const int y{int(row)};
    for (int x = window[0] + 2; x < window[2] - 2; x++) {
      const size_t here{response.bandAt(size_t(x), size_t(y))};
      for (const size_t k : {size_t(0), size_t(2)}) {
        if (here == bands[k]) continue;
        double total{};
        int count{};
        for (int dy = -1; dy <= 1; dy++) {
          for (int dx = -1; dx <= 1; dx++) {
            if (response.bandAt(size_t(x + dx), size_t(y + dy)) != bands[k])
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
                                            const Readout &readout,
                                            const Detector &detector,
                                            int4 window, float saturation) {
  const size_t numBands{bands.size()};
  const size_t bandCount{readout.bandCount};
  const size_t numPixelsX{readout.pixelCountX};
  const double black{detector.blackLevel()};
  const double range{detector.codeRange()};
  // The fraction of the code range a sample reads, which is what the
  // develop makes of it too.
  const auto sampleAt{[&](size_t i) {
    return float((double(readout.digitalNumbers[i]) - black) / range);
  }};
  // Each band's sum and its count, side by side.
  const std::vector<double> totals{parallelRowFold(
      size_t(window[1]), size_t(window[3]), std::vector<double>(numBands * 2),
      [&](size_t y) {
        std::vector<double> tally(numBands * 2);
        for (size_t x = size_t(window[0]); x < size_t(window[2]); x++) {
          const size_t pixel{y * numPixelsX + x};
          if (response.hasCFA()) {
            const size_t k{positionOf(bands, response.bandAt(x, y))};
            if (const float value{sampleAt(pixel)};
                k < numBands && value < saturation) {
              tally[2 * k] += double(value);
              tally[2 * k + 1] += 1.0;
            }
            continue;
          }
          const auto sampleOf{
              [&](size_t b) { return sampleAt(pixel * bandCount + b); }};
          if (!std::all_of(bands.begin(), bands.end(),
                           [&](size_t b) { return sampleOf(b) < saturation; }))
            continue;
          for (size_t k = 0; k < numBands; k++) {
            tally[2 * k] += double(sampleOf(bands[k]));
            tally[2 * k + 1] += 1.0;
          }
        }
        return tally;
      },
      [](std::vector<double> &running, const std::vector<double> &row) {
        for (size_t i = 0; i < running.size(); i++) running[i] += row[i];
      })};
  std::vector<double> means(numBands);
  for (size_t k = 0; k < numBands; k++)
    means[k] = totals[2 * k + 1] > 0 ? totals[2 * k] / totals[2 * k + 1] : 0.0;
  return means;
}

[[nodiscard]] std::string spellBands(const ResponseSettings &response,
                                     smdl::Span<const size_t> bands) {
  std::string text{};
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
    const std::vector<size_t> &cfa{response.cfa};
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
  std::vector<float> planes(numPixelsX * numPixelsY * numBands);
  const int columns{int(response.cfaColumns)};
  const int rows{int(response.cfaRows())};
  // Bilinear everywhere first: the pixel's own value where the band
  // sampled it, and otherwise the tent-weighted mean of the band's
  // samples within a tile of it.
  smdl::parallelFor(size_t(window[1]), size_t(window[3]), [&](size_t row) {
    const int y{int(row)};
    for (int x = window[0]; x < window[2]; x++) {
      const size_t here{response.bandAt(size_t(x), size_t(y))};
      for (size_t k = 0; k < numBands; k++) {
        float &out{planes[(size_t(y) * numPixelsX + size_t(x)) * numBands + k]};
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
                response.bandAt(size_t(sx), size_t(sy)) != bands[k])
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

DevelopFit resolveDevelopFit(const Sensor &sensor, const Detector &detector,
                             const Readout &readout,
                             const WhiteBalance &whiteBalance, int4 window) {
  const ResponseSettings &response{sensor.settings().response};
  DevelopFit resolved{};
  // What a sample at the white level reads as a fraction of the code
  // range over the black level, 1 unless the well clips below the top
  // code.
  resolved.saturation =
      float((double(detector.whiteLevel()) - detector.blackLevel()) /
            detector.codeRange());
  // What the picture is made of: three bands through the fitted matrix,
  // or as they are when they fit the observer badly, or fewer as a gray.
  const std::optional<std::array<size_t, 3>> rgb{response.rgbBands()};
  if (rgb) {
    resolved.bands.assign(rgb->begin(), rgb->end());
  } else {
    for (size_t b = 0; b < response.bands.size(); b++)
      resolved.bands.push_back(b);
  }
  const smdl::Span<const size_t> bandSpan{resolved.bands.data(),
                                          resolved.bands.size()};
  // The white balance: the illuminant it names, or for `auto` the one the
  // frame's gray world reads as.
  resolved.wasAuto = whiteBalance.kind == WhiteBalanceKind::AUTO;
  resolved.illuminant = whiteBalanceSpectrum(whiteBalance);
  const std::vector<double> gray{
      resolved.wasAuto ? grayWorld(response, bandSpan, readout, detector,
                                   window, resolved.saturation)
                       : std::vector<double>()};
  resolved.hasGrayWorld =
      resolved.wasAuto && std::all_of(gray.begin(), gray.end(),
                                      [](double mean) { return mean > 0; });
  resolved.multipliers.assign(resolved.bands.size(), 1.0);
  size_t reference{sensor.peakBand()};
  if (rgb) {
    ColorFit &fit{resolved.fit};
    reference = (*rgb)[1];
    fit = sensor.fitColor(*rgb, resolved.illuminant);
    if (resolved.hasGrayWorld && !fit.isSingular) {
      double3 balanced{};
      for (size_t k = 0; k < 3; k++) balanced[k] = gray[k] * fit.multipliers[k];
      const double3 xyz{fit.cameraToXYZ * balanced};
      if (const double sum{xyz.x + xyz.y + xyz.z}; sum > 0) {
        resolved.measuredKelvin =
            std::clamp(smdl::mccamyKelvin(double2(xyz.x / sum, xyz.y / sum)),
                       AUTO_KELVIN_MIN, AUTO_KELVIN_MAX);
        resolved.illuminant = kelvinSpectrum(resolved.measuredKelvin);
        fit = sensor.fitColor(*rgb, resolved.illuminant);
        for (size_t k = 0; k < 3; k++) fit.multipliers[k] = gray[1] / gray[k];
      }
    }
    resolved.mode =
        fit.isFaithful() ? DevelopMode::TRUE_COLOR : DevelopMode::FALSE_COLOR;
    for (size_t k = 0; k < 3; k++) resolved.multipliers[k] = fit.multipliers[k];
  } else {
    // Each band against the most sensitive, whose saturation the ISO was
    // rated at.
    const double referenceRate{
        sensor.electronRate(reference, resolved.illuminant)};
    for (size_t k = 0; k < resolved.bands.size(); k++) {
      const double rate{
          sensor.electronRate(resolved.bands[k], resolved.illuminant)};
      resolved.multipliers[k] = resolved.hasGrayWorld
                                    ? gray[reference] / gray[k]
                                : rate > 0 ? referenceRate / rate
                                           : 1.0;
    }
  }
  // The exposure: the baseline that puts a metered mean at middle gray,
  // times the ratio that keeps a neutral under the illuminant there
  // whichever band the ISO was rated in.
  const double rated{sensor.peakElectronsPerLuxSecond()};
  const double referenceRated{
      sensor.electronsPerLuxSecond(reference, resolved.illuminant)};
  resolved.exposure =
      DEVELOP_MIDDLE_GRAY * ISO_SATURATION_LUX_SECONDS / (METER_Q * METER_K) *
      (rated > 0 && referenceRated > 0 ? rated / referenceRated : 1.0);
  return resolved;
}

std::vector<float> developReadout(const Sensor &sensor,
                                  const Detector &detector,
                                  const Readout &readout,
                                  const WhiteBalance &whiteBalance, int4 window,
                                  DevelopLogging logging) {
  const ResponseSettings &response{sensor.settings().response};
  const size_t numPixelsX{readout.pixelCountX};
  const size_t numPixelsY{readout.pixelCountY};
  const size_t bandCount{readout.bandCount};
  SMDL_SANITY_CHECK(bandCount ==
                    (response.hasCFA() ? 1 : response.bands.size()));
  const DevelopFit resolved{
      resolveDevelopFit(sensor, detector, readout, whiteBalance, window)};
  const std::vector<size_t> &bands{resolved.bands};
  const std::vector<double> &multipliers{resolved.multipliers};
  const ColorFit &fit{resolved.fit};
  const DevelopMode mode{resolved.mode};
  const double exposure{resolved.exposure};
  const float saturation{resolved.saturation};
  const smdl::Span<const size_t> bandSpan{bands.data(), bands.size()};
  // Fractions of the top code over the black level, which keeps the read
  // noise's lower half as negatives.
  const double black{detector.blackLevel()};
  const double range{detector.codeRange()};
  std::vector<float> values(readout.digitalNumbers.size());
  for (size_t i = 0; i < values.size(); i++)
    values[i] = float((double(readout.digitalNumbers[i]) - black) / range);
  // Balanced, and every sample held where the least multiplied band
  // saturates, so that a saturated white stays white rather than taking
  // the color of the other multipliers.
  const float ceiling{
      float(double(saturation) *
            *std::min_element(multipliers.begin(), multipliers.end()))};
  smdl::parallelFor(size_t(window[1]), size_t(window[3]), [&](size_t y) {
    for (size_t x = size_t(window[0]); x < size_t(window[2]); x++) {
      const size_t pixel{y * numPixelsX + x};
      if (response.hasCFA()) {
        if (const size_t k{positionOf(bandSpan, response.bandAt(x, y))};
            k < bands.size())
          values[pixel] =
              std::min(float(double(values[pixel]) * multipliers[k]), ceiling);
        continue;
      }
      for (size_t k = 0; k < bands.size(); k++) {
        float &value{values[pixel * bandCount + bands[k]]};
        value = std::min(float(double(value) * multipliers[k]), ceiling);
      }
    }
  });
  const DemosaicMethod method{demosaicMethod(response, bandSpan)};
  const std::vector<float> planes{
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
  const double3x3 toSRGB{
      mode == DevelopMode::TRUE_COLOR
          ? smdl::xyzToLinearSRGB() *
                (smdl::bradfordAdaptation(fit.white, smdl::linearSRGBWhite()) *
                 fit.cameraToXYZ)
          : double3x3(1.0)};
  std::vector<float> rgbImage(numPixelsX * numPixelsY * 3);
  smdl::parallelFor(size_t(window[1]), size_t(window[3]), [&](size_t y) {
    for (size_t x = size_t(window[0]); x < size_t(window[2]); x++) {
      const size_t pixel{y * numPixelsX + x};
      double3 color{};
      if (mode == DevelopMode::GRAYSCALE) {
        double total{};
        for (size_t k = 0; k < bands.size(); k++) total += sampleOf(pixel, k);
        color = double3(total / double(bands.size()));
      } else {
        color = toSRGB * double3(sampleOf(pixel, 0), sampleOf(pixel, 1),
                                 sampleOf(pixel, 2));
      }
      color *= exposure;
      float *texel{&rgbImage[3 * pixel]};
      texel[0] = float(color.x);
      texel[1] = float(color.y);
      texel[2] = float(color.z);
    }
  });
  if (logging == DevelopLogging::VERBOSE) {
    const std::string balance{
        !resolved.wasAuto ? whiteBalanceName(whiteBalance)
        : !resolved.hasGrayWorld
            ? std::string("auto, which found no gray world in the "
                          "frame and took D65")
        : resolved.measuredKelvin > 0
            ? smdl::concat("auto, the frame's gray world reading ",
                           smdl::Brief(resolved.measuredKelvin, 4), " K")
            : std::string("auto, the frame's gray world")};
    std::string factors{};
    for (size_t k = 0; k < bands.size(); k++)
      factors += smdl::concat(k > 0 ? ", " : "", response.bands[bands[k]].name,
                              " ", smdl::Brief(multipliers[k], 4));
    SMDL_LOG_INFO("Develop: white balance ", balance, ": ", factors,
                  ", every sample held at ", smdl::Brief(ceiling, 4));
    const std::string how{
        smdl::concat(describeDemosaic(method, response), "; exposed by ",
                     smdl::Brief(std::log2(exposure), 3),
                     " EV, so that a metered neutral develops to ",
                     smdl::Brief(DEVELOP_MIDDLE_GRAY, 3))};
    const std::string names{spellBands(response, bandSpan)};
    if (mode == DevelopMode::TRUE_COLOR)
      SMDL_LOG_INFO("Develop: ", names,
                    " to XYZ through the matrix fitted "
                    "over ",
                    trainingReflectances().size(),
                    " training reflectances, a mean of ",
                    smdl::Brief(fit.meanDeltaE00, 3),
                    " dE00, then Bradford to the sRGB white; ", how);
    else if (mode == DevelopMode::FALSE_COLOR)
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
      SMDL_LOG_INFO("Develop: ", smdl::Counted(bands.size(), "band"),
                    " cannot carry color, so the picture is gray; ", how);
  }
  return rgbImage;
}

namespace {

// The D50 white the DNG color model refers a forward matrix to, which
// is ICC's own rather than an integral of a spectrum.
constexpr double3 DNG_D50_WHITE{0.9642, 1.0, 0.8249};

// The Exif light source codes of the two calibrations below.
//
// \{
constexpr uint16_t DNG_ILLUMINANT_A{17};
constexpr uint16_t DNG_ILLUMINANT_D65{21};
// \}

// One calibration a camera profile carries: how the three bands see
// color under one illuminant, and the matrix from the camera's own
// three, as they read before any balance, to XYZ under it, which is the
// fit after what balances it.
struct Calibration final {
  ColorFit fit{};

  double3x3 cameraToXYZ{};
};

[[nodiscard]] Calibration calibrationAt(const Sensor &sensor,
                                        const std::array<size_t, 3> &rgb,
                                        double kelvin) {
  Calibration calibration{};
  calibration.fit = sensor.fitColor(rgb, kelvinSpectrum(kelvin));
  calibration.cameraToXYZ = calibration.fit.cameraToXYZ;
  for (size_t k = 0; k < 3; k++)
    calibration.cameraToXYZ[k] *= calibration.fit.multipliers[k];
  return calibration;
}

// What the calibration's matrix reads backward, which is the direction
// the format states it in.
[[nodiscard]] double3x3 xyzToCameraOf(const Calibration &calibration,
                                      double kelvin) {
  double3x3 matrix{calibration.cameraToXYZ};
  if (!tryInvert(matrix))
    throw smdl::Error(smdl::concat(
        "This sensor's three bands respond too much alike under ",
        smdl::Brief(kelvin, 4),
        " K to carry the color matrix a DNG states. Write the readout to a "
        "'.img' instead"));
  return matrix;
}

} // namespace

void requireDNGSensor(const Sensor &sensor) {
  const ResponseSettings &response{sensor.settings().response};
  const auto refuse{[](auto &&...args) {
    throw smdl::Error(smdl::concat(
        args..., ". Write the readout to a '.img' instead, which carries "
                 "any number of bands under any name"));
  }};
  if (response.bands.size() != 3)
    refuse("A DNG holds red, green, and blue alone, and this sensor has ",
           smdl::Counted(response.bands.size(), "band"));
  if (response.hasCFA()) {
    if (response.cfaColumns != 2 || response.cfaRows() != 2)
      refuse("A DNG's mosaic is a 2 by 2 tile, and this sensor's is ",
             response.cfaColumns, " by ", response.cfaRows());
    if (const size_t count{tileBands(response.cfa).size()}; count != 3)
      refuse("A DNG's mosaic lays down all three colors, and this sensor's "
             "tile lays down ",
             smdl::Counted(count, "band"));
  }
  // The matrices themselves are wanted at readout and not here; building
  // one is the check, since that is what refuses a sensor whose three
  // bands respond too much alike to state one.
  const std::array<size_t, 3> rgb{*response.rgbBands()};
  for (const double kelvin : {ILLUMINANT_A_KELVIN, D65_KELVIN})
    (void)xyzToCameraOf(calibrationAt(sensor, rgb, kelvin), kelvin);
}

DNGImage makeDNGImage(const Sensor &sensor, const Detector &detector,
                      const Readout &readout, const DevelopFit &develop,
                      const DetectorShot &shot, int4 window,
                      std::vector<uint16_t> &planes) {
  const SensorSettings &settings{sensor.settings()};
  const ResponseSettings &response{settings.response};
  SMDL_SANITY_CHECK(response.bands.size() == 3 && develop.bands.size() == 3);
  const std::array<size_t, 3> rgb{*response.rgbBands()};
  const smdl::Span<const size_t> rgbSpan{rgb.data(), rgb.size()};
  DNGImage image{};
  image.pixelCountX = readout.pixelCountX;
  image.pixelCountY = readout.pixelCountY;
  image.window = window;
  image.hasCFA = response.hasCFA();
  if (image.hasCFA) {
    for (size_t i = 0; i < 4; i++)
      image.cfa[i] = uint8_t(positionOf(rgbSpan, response.cfa[i]));
    image.digitalNumbers = smdl::Span<const uint16_t>(
        readout.digitalNumbers.data(), readout.digitalNumbers.size());
  } else {
    // The planes in red, green, blue order, which the bands are in only
    // where the file named them that way.
    planes.resize(readout.digitalNumbers.size());
    for (size_t pixel = 0; 3 * pixel < planes.size(); pixel++)
      for (size_t k = 0; k < 3; k++)
        planes[3 * pixel + k] = readout.digitalNumbers[3 * pixel + rgb[k]];
    image.digitalNumbers =
        smdl::Span<const uint16_t>(planes.data(), planes.size());
  }
  image.blackLevel = detector.blackLevel();
  image.whiteLevel = detector.whiteLevel();
  // The two calibrations, so that a developer interpolates the matrix by
  // the temperature the neutral reads as, the way it does for a body,
  // rather than taking one light's fit for every light.
  const Calibration warm{calibrationAt(sensor, rgb, ILLUMINANT_A_KELVIN)};
  const Calibration cool{calibrationAt(sensor, rgb, D65_KELVIN)};
  image.illuminant1 = DNG_ILLUMINANT_A;
  image.illuminant2 = DNG_ILLUMINANT_D65;
  image.xyzToCamera1 = xyzToCameraOf(warm, ILLUMINANT_A_KELVIN);
  image.xyzToCamera2 = xyzToCameraOf(cool, D65_KELVIN);
  image.cameraToXYZ1 = smdl::bradfordAdaptation(warm.fit.white, DNG_D50_WHITE) *
                       warm.fit.cameraToXYZ;
  image.cameraToXYZ2 = smdl::bradfordAdaptation(cool.fit.white, DNG_D50_WHITE) *
                       cool.fit.cameraToXYZ;
  // The balance the develop resolved, as what a neutral reads in the
  // camera's own three with green at 1, which is how a body writes it.
  for (size_t k = 0; k < 3; k++)
    image.asShotNeutral[k] = develop.multipliers[1] / develop.multipliers[k];
  image.baselineExposure = std::log2(develop.exposure);
  // The noise the chain implies over the signal the tag is stated in: a
  // unit of it stands for `codeRange / gain` electrons, whose shot noise
  // is the scale, and the read noise through the same gain is the
  // offset. A real profile is measured; this one is the instrument's own
  // numbers read forward.
  const double scale{detector.gain() / detector.codeRange()};
  const double offset{detector.gain() * detector.readNoise() /
                      detector.codeRange()};
  image.noiseProfile.fill(double2(scale, offset * offset));
  image.make = "smdl-toy";
  image.model = settings.name.empty() ? std::string("sensor") : settings.name;
  // Prefixed, so that a developer holding a profile for the body whose
  // curves these are takes the matrices in the file rather than its own
  // for that name.
  image.uniqueCameraModel = smdl::concat("smdl-toy ", image.model);
  image.software = "smdl-toy";
  image.exposureTime = shot.exposure;
  image.fNumber = shot.fNumber;
  image.iso = shot.iso;
  return image;
}
