#include "MedianFilter.h"

#include <algorithm>
#include <array>
#include <utility>

#include "smdl/Support/Parallel.h"

namespace {

// The largest neighborhood, which is what the per-pixel tap buffer is
// sized to so the selection never allocates.
constexpr size_t MAX_TAPS_PER_SIDE = 2 * size_t(MEDIAN_FILTER_MAX_RADIUS) + 1;
constexpr size_t MAX_TAPS = MAX_TAPS_PER_SIDE * MAX_TAPS_PER_SIDE;

// The brightness a pixel is judged by. The clamp is load bearing: the
// spectral projection can emit negatives, and a negative median would
// invert the comparison the whole filter rests on. It also means a pixel
// with a large negative excursion reads as zero, so it never fires and,
// as a tap, drags the median toward zero.
[[nodiscard]] float pixelPeak(const float *texel) noexcept {
  return std::max({0.0f, texel[0], texel[1], texel[2]});
}

} // namespace

MedianFilterReport medianFilterRGB(const MedianFilterOptions &options,
                                   std::vector<float> &rgbImage,
                                   size_t numPixelsX, int4 window) {
  MedianFilterReport report{};
  if (!options.isEnabled) return report;
  SMDL_SANITY_CHECK(options.radius >= 1 &&
                    options.radius <= MEDIAN_FILTER_MAX_RADIUS);
  if (window[2] <= window[0] || window[3] <= window[1]) return report;
  const int radius{options.radius};
  const float factor{options.factor};
  const size_t numRows{size_t(window[3] - window[1])};
  report.examinedCount = numRows * size_t(window[2] - window[0]);
  // The pass reads the image it is rewriting, so it reads a copy.
  const std::vector<float> source{rgbImage};
  // One accumulator per row, folded in row order afterward, so that the
  // totals are the same however the rows are scheduled.
  std::vector<uint32_t> rowCounts(numRows);
  std::vector<double> rowRemoved(numRows);
  std::vector<double> rowTotals(numRows);
  smdl::parallelFor(0, numRows, [&](size_t row) {
    const int y{window[1] + int(row)};
    // The taps, paired with their pixel index so that the pair orders
    // totally: two taps of equal peak resolve by position, and the
    // replacement is the same pixel in every build.
    std::array<std::pair<float, size_t>, MAX_TAPS> taps{};
    for (int x = window[0]; x < window[2]; x++) {
      const size_t center{size_t(x) + numPixelsX * size_t(y)};
      const float centerPeak{pixelPeak(&source[3 * center])};
      rowTotals[row] += centerPeak;
      size_t numTaps{};
      for (int dy = -radius; dy <= radius; dy++) {
        const int yy{y + dy};
        if (yy < window[1] || yy >= window[3]) continue;
        for (int dx = -radius; dx <= radius; dx++) {
          const int xx{x + dx};
          if (xx < window[0] || xx >= window[2]) continue;
          const size_t tap{size_t(xx) + numPixelsX * size_t(yy)};
          taps[numTaps++] = {pixelPeak(&source[3 * tap]), tap};
        }
      }
      const auto middle{taps.begin() + numTaps / 2};
      std::nth_element(taps.begin(), middle, taps.begin() + numTaps);
      // Written as a multiply, so a neighborhood median of zero flags any
      // positive center instead of dividing by it.
      if (!(centerPeak > factor * middle->first)) continue;
      const float *texel{&source[3 * middle->second]};
      std::copy(texel, texel + 3, &rgbImage[3 * center]);
      rowCounts[row]++;
      rowRemoved[row] += double(centerPeak) - double(middle->first);
    }
  });
  for (size_t row{}; row < numRows; row++) {
    report.replacedCount += rowCounts[row];
    report.energyRemoved += rowRemoved[row];
    report.energyTotal += rowTotals[row];
  }
  return report;
}
