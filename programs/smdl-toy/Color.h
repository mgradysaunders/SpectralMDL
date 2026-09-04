/// \file
/// The render-wide vocabulary the scene and the renderer share: the
/// wavelength grid, its quadrature weights, and the base time, all set
/// once in `main()`; the `Color` type the grid sizes; and the
/// `smdl::State` builder that seeds every material evaluation with
/// them.
#pragma once

#include <vector>

#include "smdl/Common.h"
#include "smdl/RenderUtil/SpectralColor.h"

#include "Common.h"

/// The default wavelength range in nanometers, spanning the visible.
constexpr float WAVELENGTH_MIN = 380.0f;
constexpr float WAVELENGTH_MAX = 720.0f;

/// The render-wide wavelength band count, which sizes every `Color`.
///
/// Set exactly once in `main()` before anything constructs a `Color`
/// and long before rendering threads start. The default of 16 matches
/// `smdl::SpectralColor::INLINE_CAPACITY`, so a default render's colors
/// never touch the heap.
[[nodiscard]] inline size_t &renderNumBands() noexcept {
  static size_t numBands{16};
  return numBands;
}

/// The render-wide per-band quadrature weights in nanometers, empty
/// for a uniformly spaced grid.
///
/// Set once in `main()` alongside `renderNumBands()`. Empty keeps
/// `State::wavelength_weight` null, which the library treats as
/// uniform quadrature; a non-uniform `-wavelengths` grid fills this
/// with trapezoid band widths, which both the JIT color-to-RGB
/// conversion and the night tonemap integrate against.
[[nodiscard]] inline std::vector<float> &renderWavelengthWeights() noexcept {
  static std::vector<float> weights{};
  return weights;
}

/// The render-wide base animation time in seconds. Setup-time
/// evaluations use it directly; a path traced with an open shutter
/// evaluates at its own offset from it.
[[nodiscard]] inline float &renderTime() noexcept {
  static float time{};
  return time;
}

/// The render color type: an `smdl::SpectralColor` whose constructors
/// supply the render-wide band count, so the ubiquitous `Color c{}`
/// zero vector and `Color(scalar)` splat idioms work with a runtime
/// band count.
class Color final : public smdl::SpectralColor {
public:
  Color() : SpectralColor(renderNumBands()) {}

  Color(float value) : SpectralColor(renderNumBands(), value) {}

  /// Construct from however many values are present: a shorter or
  /// empty span (a material coefficient the instance does not have)
  /// leaves the remaining bands zero.
  Color(smdl::Span<const float> values) : SpectralColor(renderNumBands()) {
    const size_t n{values.size() < size() ? values.size() : size()};
    for (size_t i = 0; i < n; i++) (*this)[i] = values[i];
  }

  Color(const SpectralColor &other) : SpectralColor(other) {}

  Color(SpectralColor &&other) noexcept
      : SpectralColor(static_cast<SpectralColor &&>(other)) {}
};

/// The render-wide wavelength grid in nanometers.
///
/// Set once in `main()` alongside `renderNumBands()`. Most evaluations
/// carry their own copy through call arguments; this is for the few
/// places too far from the render loop to be handed one, such as the
/// geometry-normal queries inside a manifold walk.
[[nodiscard]] inline Color &renderWavelengths() noexcept {
  static Color wavelengths{};
  return wavelengths;
}

/// An `smdl::State` carrying the render-wide fields every evaluation
/// needs: the wavelength grid and, when material construction is involved,
/// the allocator. The geometric fields are applied afterward by
/// `Hit::apply_geometry_to_state()`. The time defaults to the render-wide
/// base time; per-path callers pass the path's own.
[[nodiscard]] inline smdl::State
makeRenderState(const Color &wavelengths,
                smdl::BumpPtrAllocator *allocator = nullptr,
                float time = renderTime()) noexcept {
  smdl::State state{};
  state.allocator = allocator;
  state.wavelength_base = wavelengths.data();
  state.wavelength_min = wavelengths[0];
  state.wavelength_max = wavelengths[wavelengths.size() - 1];
  state.animation_time = time;
  const auto &weights{renderWavelengthWeights()};
  state.wavelength_weight = weights.empty() ? nullptr : weights.data();
  return state;
}
