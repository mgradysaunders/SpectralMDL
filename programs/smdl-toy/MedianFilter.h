/// \file
/// The firefly filter: one image-space pass over the linear RGB image,
/// after it is resolved from the film and before anything writes it.
/// Nothing else in the renderer is image-space, so this sits at the top
/// level beside `Tonemap` rather than under a layer.
#pragma once

#include <vector>

#include "Common.h"

/// The largest neighborhood radius the filter admits, which is what
/// bounds the tap buffer it selects over.
constexpr int MEDIAN_FILTER_MAX_RADIUS = 4;

/// The firefly filter, resolved from the command line into plain values.
///
/// The other answer to fireflies is `-max-contribution`, which acts in
/// path space, before the film, and so removes the energy from every
/// output. This one acts on the picture and leaves the spectral record
/// alone.
struct MedianFilterOptions final {
  /// Filter at all?
  bool isEnabled{};

  /// How many times its neighborhood a pixel must exceed to be replaced.
  float factor{8.0f};

  /// The neighborhood radius in pixels, 1 for the 3x3 window.
  int radius{1};
};

/// What one pass of the filter did, for the line that reports it. Every
/// tally is over the window the pass ran on and not the frame, so the two
/// shares it states are shares of the same thing.
struct MedianFilterReport final {
  /// The pixels replaced.
  size_t replacedCount{};

  /// The pixels looked at.
  size_t examinedCount{};

  /// The peak-channel radiance the replacements removed.
  double energyRemoved{};

  /// The peak-channel radiance that was there to remove.
  double energyTotal{};
};

/// Replace outlier pixels in a linear RGB image, in place.
///
/// A pixel is judged by its peak channel clamped at zero, which is what
/// the tonemapper already means by a pixel's brightness and what keeps
/// the comparison well ordered where the spectral projection emits
/// negatives. The pixel is replaced when its peak exceeds
/// `options.factor` times the median peak of the `(2r+1)^2` neighborhood
/// around it, and what replaces it is the whole RGB triple of the tap
/// whose peak is that median, so the result is a color that occurs in
/// the image rather than a channel-wise mixture of three pixels. Taps
/// outside `window` are dropped rather than clamped, because outside it
/// there are no samples and a pixel that was never rendered must not be
/// allowed to pull a border pixel down. The pass is out of place, so the
/// result depends on neither the traversal order nor the thread count.
///
/// Four limitations are deliberate. A neighborhood median of zero makes
/// any positive pixel an outlier, so a genuine sub-pixel emitter alone
/// on black is removed along with the fireflies; nothing in one image can
/// tell the two apart. Only bright outliers are removed, so the black
/// pixel the film writes where a sample was not finite stays black. A
/// solid block of fireflies wider than the neighborhood survives all but
/// its corners, which is what a larger `options.radius` is for. And the
/// corner of any bright region is clipped where the contrast across it
/// exceeds the factor, since a corner sees only four of itself in the
/// nine taps: that is what a median does to a corner, and the decision
/// is what confines it to corners that are `options.factor` times their
/// surroundings.
///
/// \param[in]     options    The filter, which does nothing when disabled.
/// \param[in,out] rgbImage   Three interleaved floats per pixel, row major.
/// \param[in]     numPixelsX The row stride of `rgbImage` in pixels.
/// \param[in]     window     The pixel window to filter, `x0,y0,x1,y1`.
///
/// \note `options.radius` must be between 1 and
///       `MEDIAN_FILTER_MAX_RADIUS`, which is what bounds the tap buffer.
[[nodiscard]] MedianFilterReport
medianFilterRGB(const MedianFilterOptions &options,
                std::vector<float> &rgbImage, size_t numPixelsX, int4 window);
