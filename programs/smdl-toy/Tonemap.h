#pragma once

#include <string_view>

#include "Color.h"

#include "smdl/RenderUtil/SpectralFilm.h"

namespace smdl {
class Compiler;
}

/// The appearance stage: how the estimated radiance becomes a linear
/// display color.
enum class AppearanceMode {
  /// Pass the radiance through to the display curve.
  LINEAR,

  /// Shorthand for `LINEAR` with the log curve.
  LOG,

  /// Model human vision at absolute luminance and auto-expose, for
  /// physically dim scenes like moonlight.
  NIGHT
};

/// The display curve: how a linear display color reaches the 8-bit range.
enum class DisplayCurveKind {
  /// Clamp and gamma-encode.
  GAMMA,

  /// Map the decades below the exposure-scaled white point.
  LOG,

  /// Roll the highlights off toward white instead of clipping them.
  FILMIC
};

/// The local operator: a smooth per-pixel exposure, or none.
enum class LocalOperator { OFF, FUSION };

/// \name Command line names
///
/// The one place each name is spelled. The display transform runs after
/// the last sample, so parsing at the command line is what reports a
/// misspelling before the render is paid for rather than after.
///
/// \throws smdl::Error  If the name is not recognized.
///
/// \{
[[nodiscard]] AppearanceMode parseAppearanceMode(std::string_view name);

[[nodiscard]] DisplayCurveKind parseDisplayCurveKind(std::string_view name);

[[nodiscard]] LocalOperator parseLocalOperator(std::string_view name);
/// \}

/// The display transform, resolved from the command line into plain
/// values.
///
/// The transform runs in three independent stages. The `mode` decides how
/// the estimated radiance becomes a linear display color, the `local`
/// operator decides whether that color is redistributed by a smooth
/// per-pixel exposure, and the `curve` decides how the result reaches the
/// 8-bit range. None of this touches the floating point or spectral
/// outputs, which stay radiometric.
struct TonemapOptions final {
  /// The appearance stage.
  AppearanceMode mode{AppearanceMode::LINEAR};

  /// The display curve.
  DisplayCurveKind curve{DisplayCurveKind::GAMMA};

  /// The local operator.
  LocalOperator local{LocalOperator::OFF};

  /// The exposure applied before the display curve.
  float exposure{1.0f};

  /// With the log curve, how many decades below white reach black.
  float logDecades{4.0f};

  /// With fusion, how much of the local exposure to keep: 0 reproduces
  /// the globally auto-exposed image, 1 is the fused result in full.
  float localStrength{0.75f};

  /// With fusion, the exposure ladder's total span in EV, or 0 to take
  /// the span from the image histogram.
  float localRange{};

  /// With fusion, the largest local exposure deviation in EV.
  float localClamp{3.0f};
};

/// How `resolveRGB()` maps the film to RGB when the CIE
/// projection is not simply the right thing.
struct RGBPolicy final {
  /// Force the false-color band mapping even when the grid could carry
  /// true color.
  bool forceFalseColor{};

  /// With false color, the wavelengths in nm mapped to R, G, and B, or
  /// empty to pick the bands at 5/6, 1/2, and 1/6 of the grid span,
  /// long wavelengths on red.
  std::vector<float> falseColorWaves{};
};

/// Resolve the film to linear RGB once. This is the radiance
/// the renderer actually estimated, so it is what gets written to the
/// floating point file; everything in `tonemap()` is a display transform
/// applied only on the way to an 8-bit file.
///
/// How the spectrum becomes RGB depends on how much of the visible the
/// wavelength grid can see. A grid that covers the photopic luminous
/// mass projects through the CIE observer as always; a grid that covers
/// part of it still does, with a note that the picture is band-limited;
/// a grid that misses the visible (or `policy.forceFalseColor`) maps
/// three bands to R, G, and B instead, and fewer than three bands write
/// the grayscale mean radiance. Every non-true-color choice is
/// announced on stderr; the spectral ENVI output is always the
/// radiometric record.
[[nodiscard]] std::vector<float> resolveRGB(smdl::Compiler &compiler,
                                            const smdl::SpectralFilm &film,
                                            const Color &wavelengths,
                                            const RGBPolicy &policy = {});

/// Tone map the resolved RGB image to 8-bit display values.
///
/// The `film` and its `wavelengths` back the night mode, which needs the
/// absolute photopic and scotopic luminance of every pixel rather than
/// the RGB projection alone; `rgbImage` must be the `resolveRGB()` of
/// the same film, so every mode displays the same radiance.
[[nodiscard]] std::vector<uint8_t> tonemap(const TonemapOptions &options,
                                           const std::vector<float> &rgbImage,
                                           const smdl::SpectralFilm &film,
                                           const Color &wavelengths);
