#pragma once

#include <string_view>

#include "Color.h"

#include "smdl/RenderUtil/SpectralFilm.h"

namespace smdl {
class Compiler;
}

/// The display curve: how a linear display color reaches the 8-bit range.
enum class TonemapCurve {
  /// Clamp and gamma-encode.
  GAMMA,

  /// Map the decades below the exposure-scaled white point.
  LOG,

  /// Roll the highlights off toward white instead of clipping them.
  FILMIC
};

/// The display transform, resolved from the command line into plain
/// values.
///
/// The transform runs in three independent stages. The night filter
/// decides how the estimated radiance becomes a linear display color,
/// fusion decides whether that color is redistributed by a smooth
/// per-pixel exposure, and the `curve` decides how the result reaches
/// the 8-bit range. None of this touches the floating point or spectral
/// outputs, which stay radiometric.
struct TonemapOptions final {
  /// The display curve.
  TonemapCurve curve{TonemapCurve::GAMMA};

  /// Model human vision at absolute luminance and auto-expose, for
  /// physically dim scenes like moonlight.
  bool isNight{};

  /// Redistribute the exposure locally by Laplacian pyramid.
  bool useFusion{};

  /// The exposure applied before the display curve.
  float exposure{1.0f};

  /// With the log curve, how many decades below white reach black.
  float logDecades{4.0f};

  /// With fusion, how much of the local exposure to keep: 0 reproduces
  /// the globally auto-exposed image, 1 is the fused result in full.
  float fusionStrength{0.75f};

  /// With fusion, the largest local exposure deviation in EV.
  float fusionClamp{3.0f};

  /// With fusion, the exposure ladder's total span in EV, or 0 to take
  /// the span from the image histogram.
  float fusionSpan{};
};

/// Parse the `-tonemap` spec: the stages joined by `+`, each optionally
/// followed by `:` and its comma-separated parameters. The stages are
/// `night`, one of `gamma`, `log[:DECADES]`, and `filmic`, and
/// `fusion[:STRENGTH[,CLAMP[,SPAN]]]`, in any order and at most one per
/// stage. The `exposure` is left at its default for the caller to fill.
///
/// This is the one place the names are spelled. The display transform
/// runs after the last sample, so parsing at the command line is what
/// reports a misspelling before the render is paid for rather than
/// after.
///
/// \throws smdl::Error  If the spec is malformed or out of range.
///
[[nodiscard]] TonemapOptions parseTonemapOptions(std::string_view spec);

/// Tone map the resolved RGB image to 8-bit display values.
///
/// The `film` and its `wavelengths` back the night filter, which needs
/// the absolute photopic and scotopic luminance of every pixel rather
/// than the RGB projection alone; `rgbImage` must be the `resolveRGB()`
/// of the same film, so every display transform shows the same
/// radiance. The one thing that may have happened to it besides is
/// `medianFilterRGB()`, which the night filter cannot see: a replaced
/// pixel still reads as bright in the film, so it stays cone adapted
/// while its rod adapted neighbors desaturate, and a removed firefly can
/// leave a faint colored dot in a moonlit frame.
[[nodiscard]] std::vector<uint8_t> tonemap(const TonemapOptions &options,
                                           const std::vector<float> &rgbImage,
                                           const smdl::SpectralFilm &film,
                                           const Color &wavelengths);
