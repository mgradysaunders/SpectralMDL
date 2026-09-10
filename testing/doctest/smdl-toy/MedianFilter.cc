#include "Fixtures.h"

#include <vector>

#include "MedianFilter.h"

namespace {

// A flat gray image, the background every case below plants something
// in. Three interleaved floats per pixel, row major, which is what
// `resolveRGB()` hands over.
[[nodiscard]] std::vector<float> flatImage(int sizeX, int sizeY,
                                           float value = 1.0f) {
  return std::vector<float>(size_t(sizeX) * size_t(sizeY) * 3, value);
}

void setPixel(std::vector<float> &image, int sizeX, int x, int y, float r,
              float g, float b) {
  const auto texel{&image[3 * (size_t(x) + size_t(sizeX) * size_t(y))]};
  texel[0] = r, texel[1] = g, texel[2] = b;
}

[[nodiscard]] float3 getPixel(const std::vector<float> &image, int sizeX, int x,
                              int y) {
  const auto texel{&image[3 * (size_t(x) + size_t(sizeX) * size_t(y))]};
  return float3(texel[0], texel[1], texel[2]);
}

// The filter as every case wants it: on, at the default factor and the
// 3x3 window unless the case says otherwise.
[[nodiscard]] MedianFilterOptions filterOptions(float factor = 8.0f,
                                                int radius = 1) {
  auto options{MedianFilterOptions()};
  options.isEnabled = true;
  options.factor = factor;
  options.radius = radius;
  return options;
}

} // namespace

TEST_CASE("medianFilterRGB: what it replaces") {
  constexpr int SIZE_X{16};
  constexpr int SIZE_Y{12};
  const int4 wholeFrame{0, 0, SIZE_X, SIZE_Y};
  SUBCASE("An isolated bright pixel falls to its neighborhood") {
    auto image{flatImage(SIZE_X, SIZE_Y)};
    setPixel(image, SIZE_X, 5, 6, 900.0f, 900.0f, 900.0f);
    const auto report{
        medianFilterRGB(filterOptions(), image, SIZE_X, wholeFrame)};
    CHECK(report.replacedCount == 1);
    CHECK_SAME(getPixel(image, SIZE_X, 5, 6), float3(1.0f));
    // The energy removed is the peak the pixel lost, and the total is
    // every pixel's peak, the replaced one counted as it arrived.
    CHECK(report.energyRemoved == doctest::Approx(899.0));
    CHECK(report.energyTotal ==
          doctest::Approx(double(SIZE_X * SIZE_Y - 1) + 900.0));
  }
  SUBCASE("A pixel below the factor is left alone, bit for bit") {
    auto image{flatImage(SIZE_X, SIZE_Y)};
    setPixel(image, SIZE_X, 5, 6, 7.9f, 7.9f, 7.9f);
    const auto before{image};
    const auto report{
        medianFilterRGB(filterOptions(), image, SIZE_X, wholeFrame)};
    CHECK(report.replacedCount == 0);
    CHECK(report.energyRemoved == 0.0);
    CHECK(image == before);
  }
  SUBCASE("A step edge survives") {
    // Half the frame a hundred times the other half, which is far past
    // the factor and yet no pixel disagrees with its own side.
    auto image{flatImage(SIZE_X, SIZE_Y)};
    for (int y = 0; y < SIZE_Y; y++)
      for (int x = SIZE_X / 2; x < SIZE_X; x++)
        setPixel(image, SIZE_X, x, y, 100.0f, 100.0f, 100.0f);
    const auto before{image};
    const auto report{
        medianFilterRGB(filterOptions(), image, SIZE_X, wholeFrame)};
    CHECK(report.replacedCount == 0);
    CHECK(image == before);
  }
  SUBCASE("A bright region keeps everything but its corners") {
    // A 5x5 block, well past the factor. Its interior and its edges see
    // six or more of themselves in the 3x3 window and stay; each corner
    // sees only four, so the median is the background and the corner is
    // clipped. That is what a median filter does to a corner, and here
    // it costs a corner only where the contrast exceeds the factor.
    auto image{flatImage(SIZE_X, SIZE_Y)};
    for (int y = 4; y <= 8; y++)
      for (int x = 4; x <= 8; x++)
        setPixel(image, SIZE_X, x, y, 500.0f, 500.0f, 500.0f);
    const auto report{
        medianFilterRGB(filterOptions(), image, SIZE_X, wholeFrame)};
    CHECK(report.replacedCount == 4);
    CHECK_SAME(getPixel(image, SIZE_X, 6, 6), float3(500.0f));
    CHECK_SAME(getPixel(image, SIZE_X, 6, 4), float3(500.0f));
    CHECK_SAME(getPixel(image, SIZE_X, 4, 6), float3(500.0f));
    CHECK_SAME(getPixel(image, SIZE_X, 4, 4), float3(1.0f));
  }
  SUBCASE("A block of fireflies is what the radius is for") {
    // Three by three is most of the 3x3 window, so at radius 1 only the
    // block's own corners are outliers and the plus inside it survives.
    const auto blocked{[&] {
      auto image{flatImage(SIZE_X, SIZE_Y)};
      for (int y = 5; y <= 7; y++)
        for (int x = 4; x <= 6; x++)
          setPixel(image, SIZE_X, x, y, 500.0f, 500.0f, 500.0f);
      return image;
    }};
    auto image{blocked()};
    const auto report{
        medianFilterRGB(filterOptions(), image, SIZE_X, wholeFrame)};
    CHECK(report.replacedCount == 4);
    CHECK_SAME(getPixel(image, SIZE_X, 5, 6), float3(500.0f));
    // The same block is a minority of the 5x5 window, so all nine go.
    auto wider{blocked()};
    const auto widerReport{
        medianFilterRGB(filterOptions(8.0f, 2), wider, SIZE_X, wholeFrame)};
    CHECK(widerReport.replacedCount == 9);
    CHECK_SAME(getPixel(wider, SIZE_X, 5, 6), float3(1.0f));
  }
  SUBCASE("A spike in one channel is caught") {
    // Blue carries the least luminance of the three, so a luminance test
    // would let this one through.
    auto image{flatImage(SIZE_X, SIZE_Y)};
    setPixel(image, SIZE_X, 5, 6, 1.0f, 1.0f, 400.0f);
    const auto report{
        medianFilterRGB(filterOptions(), image, SIZE_X, wholeFrame)};
    CHECK(report.replacedCount == 1);
    CHECK_SAME(getPixel(image, SIZE_X, 5, 6), float3(1.0f));
  }
  SUBCASE("The replacement is a neighbor's own triple") {
    // Every neighbor a different color, so a per-channel median would
    // mix three of them into a color the image does not contain.
    auto image{flatImage(SIZE_X, SIZE_Y)};
    setPixel(image, SIZE_X, 4, 5, 0.3f, 0.1f, 0.2f);
    setPixel(image, SIZE_X, 5, 5, 0.4f, 0.2f, 0.1f);
    setPixel(image, SIZE_X, 6, 5, 0.1f, 0.5f, 0.3f);
    setPixel(image, SIZE_X, 4, 6, 0.2f, 0.3f, 0.6f);
    setPixel(image, SIZE_X, 6, 6, 0.6f, 0.1f, 0.4f);
    setPixel(image, SIZE_X, 4, 7, 0.5f, 0.4f, 0.2f);
    setPixel(image, SIZE_X, 5, 7, 0.2f, 0.6f, 0.1f);
    setPixel(image, SIZE_X, 6, 7, 0.1f, 0.2f, 0.5f);
    setPixel(image, SIZE_X, 5, 6, 90.0f, 90.0f, 90.0f);
    const auto report{
        medianFilterRGB(filterOptions(), image, SIZE_X, wholeFrame)};
    REQUIRE(report.replacedCount == 1);
    const auto replacement{getPixel(image, SIZE_X, 5, 6)};
    bool isANeighbor{false};
    for (int y = 5; y <= 7; y++)
      for (int x = 4; x <= 6; x++)
        if (!(x == 5 && y == 6) &&
            isSame(replacement, getPixel(image, SIZE_X, x, y)))
          isANeighbor = true;
    CHECK_MESSAGE(isANeighbor, replacement, " is no neighbor's color");
  }
  SUBCASE("Pixels outside the crop window are neither modified nor taps") {
    // Outside the window nothing was rendered, so those pixels must not
    // change and must not drag a border pixel down.
    const int4 window{4, 4, 10, 9};
    auto image{flatImage(SIZE_X, SIZE_Y, 0.0f)};
    for (int y = window[1]; y < window[3]; y++)
      for (int x = window[0]; x < window[2]; x++)
        setPixel(image, SIZE_X, x, y, 1.0f, 1.0f, 1.0f);
    // One firefly on the window's own border, where every dropped tap
    // would otherwise be a black one.
    setPixel(image, SIZE_X, 4, 4, 900.0f, 900.0f, 900.0f);
    const auto report{medianFilterRGB(filterOptions(), image, SIZE_X, window)};
    CHECK(report.replacedCount == 1);
    CHECK(report.examinedCount ==
          size_t((window[2] - window[0]) * (window[3] - window[1])));
    CHECK_SAME(getPixel(image, SIZE_X, 4, 4), float3(1.0f));
    CHECK_SAME(getPixel(image, SIZE_X, 5, 5), float3(1.0f));
    CHECK_SAME(getPixel(image, SIZE_X, 3, 4), float3(0.0f));
    CHECK_SAME(getPixel(image, SIZE_X, 10, 8), float3(0.0f));
    // The window is what the totals are over, not the frame.
    CHECK(report.energyTotal ==
          doctest::Approx(
              double((window[2] - window[0]) * (window[3] - window[1]) - 1) +
              900.0));
  }
  SUBCASE("A negative pixel neither fires nor is replaced") {
    // The spectral projection can emit negatives, and the clamp that
    // keeps the comparison ordered is what makes this hold.
    auto image{flatImage(SIZE_X, SIZE_Y)};
    setPixel(image, SIZE_X, 5, 6, -900.0f, -900.0f, -900.0f);
    const auto before{image};
    const auto report{
        medianFilterRGB(filterOptions(), image, SIZE_X, wholeFrame)};
    CHECK(report.replacedCount == 0);
    CHECK(image == before);
  }
  SUBCASE("A black frame is a no-op") {
    auto image{flatImage(SIZE_X, SIZE_Y, 0.0f)};
    const auto before{image};
    const auto report{
        medianFilterRGB(filterOptions(), image, SIZE_X, wholeFrame)};
    CHECK(report.replacedCount == 0);
    CHECK(report.energyTotal == 0.0);
    CHECK(image == before);
  }
  SUBCASE("A pixel alone on black is an outlier") {
    // The concession the header states: nothing in one image tells a
    // firefly from a genuine sub-pixel emitter, and a zero median makes
    // any positive pixel an outlier.
    auto image{flatImage(SIZE_X, SIZE_Y, 0.0f)};
    setPixel(image, SIZE_X, 5, 6, 0.001f, 0.001f, 0.001f);
    const auto report{
        medianFilterRGB(filterOptions(), image, SIZE_X, wholeFrame)};
    CHECK(report.replacedCount == 1);
    CHECK_SAME(getPixel(image, SIZE_X, 5, 6), float3(0.0f));
  }
  SUBCASE("A one pixel window is a no-op") {
    auto image{flatImage(SIZE_X, SIZE_Y)};
    setPixel(image, SIZE_X, 5, 6, 900.0f, 900.0f, 900.0f);
    const auto before{image};
    // The center is the only tap, so it is its own median.
    const auto report{
        medianFilterRGB(filterOptions(), image, SIZE_X, int4{5, 6, 6, 7})};
    CHECK(report.replacedCount == 0);
    CHECK(image == before);
  }
  SUBCASE("An empty window is a no-op") {
    auto image{flatImage(SIZE_X, SIZE_Y)};
    const auto before{image};
    const auto report{
        medianFilterRGB(filterOptions(), image, SIZE_X, int4{4, 4, 4, 9})};
    CHECK(report.replacedCount == 0);
    CHECK(report.energyTotal == 0.0);
    CHECK(image == before);
  }
  SUBCASE("The disabled filter is a no-op and reports nothing") {
    auto image{flatImage(SIZE_X, SIZE_Y)};
    setPixel(image, SIZE_X, 5, 6, 900.0f, 900.0f, 900.0f);
    const auto before{image};
    const auto report{
        medianFilterRGB(MedianFilterOptions(), image, SIZE_X, wholeFrame)};
    CHECK(report.replacedCount == 0);
    CHECK(report.examinedCount == 0);
    CHECK(report.energyTotal == 0.0);
    CHECK(image == before);
  }
  SUBCASE("A second pass over the same image changes nothing more") {
    // Not a property of the filter in general, but the case that matters:
    // a lone firefly is gone in one pass, so re-running a render's output
    // stage does not keep eating the image.
    auto image{flatImage(SIZE_X, SIZE_Y)};
    setPixel(image, SIZE_X, 5, 6, 900.0f, 900.0f, 900.0f);
    (void)medianFilterRGB(filterOptions(), image, SIZE_X, wholeFrame);
    const auto once{image};
    const auto report{
        medianFilterRGB(filterOptions(), image, SIZE_X, wholeFrame)};
    CHECK(report.replacedCount == 0);
    CHECK(image == once);
  }
}
