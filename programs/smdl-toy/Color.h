/// \file
/// The render-wide vocabulary the scene and the renderer share: the
/// wavelength grid and the shutter interval, both set once in `main()`;
/// the `Color` type the grid sizes; and the `smdl::State` builder that
/// seeds every material evaluation with them.
#pragma once

#include <cmath>
#include <vector>

#include "smdl/Common.h"
#include "smdl/RenderUtil/SpectralColor.h"

#include "Common.h"

/// The default wavelength range in nanometers, spanning the visible.
constexpr float WAVELENGTH_MIN = 380.0f;
constexpr float WAVELENGTH_MAX = 720.0f;

/// The jitter band edges of the wavelength grid `wavelens`, as
/// `WavelengthGrid::bandEdges` describes them, or empty for a grid of
/// fewer than 2 wavelengths, which has no band width to speak of.
[[nodiscard]] inline std::vector<float>
wavelengthBandEdges(smdl::Span<const float> wavelens) {
  const size_t numBands{wavelens.size()};
  if (numBands < 2) return {};
  auto edges{std::vector<float>(numBands + 1)};
  for (size_t i = 1; i < numBands; i++)
    edges[i] = 0.5f * (wavelens[i - 1] + wavelens[i]);
  edges.front() = wavelens[0] - (edges[1] - wavelens[0]);
  edges.back() =
      wavelens[numBands - 1] + (wavelens[numBands - 1] - edges[numBands - 1]);
  return edges;
}

/// The render-wide wavelength grid: the bands every `Color` is sized by,
/// their quadrature weights, and their jitter band edges.
///
/// Set exactly once in `main()`, through `reset()`, before anything
/// constructs a `Color` and long before rendering threads start; read
/// everywhere after. The four members describe one grid and have to
/// agree about it, which is what `reset()` is for.
struct WavelengthGrid final {
  /// The band count, which sizes every `Color`. The default of 16
  /// matches `smdl::SpectralColor::INLINE_CAPACITY`, so a default
  /// render's colors never touch the heap.
  size_t numBands{16};

  /// The wavelengths in nanometers.
  ///
  /// Most evaluations carry their own copy through call arguments; this
  /// is for the few places too far from the render loop to be handed
  /// one, such as the geometry-normal queries inside a manifold walk.
  smdl::SpectralColor wavelengths{};

  /// The per-band quadrature weights in nanometers, empty for a
  /// uniformly spaced grid.
  ///
  /// Empty keeps `State::wavelength_weight` null, which the library
  /// treats as uniform quadrature; a non-uniform `-wavelengths` grid
  /// fills this with trapezoid band widths, which both the JIT
  /// color-to-RGB conversion and the night tonemap integrate against.
  std::vector<float> weights{};

  /// The band edges in nanometers, one more than the band count, or
  /// empty when `-wavelength-jitter` is off.
  ///
  /// Band `i` spans `[bandEdges[i], bandEdges[i + 1]]`, the halfway
  /// points to its neighbors, so the bands tile the grid with no gap and
  /// no overlap. The outermost edges mirror the inner half-width, which
  /// keeps the end bands full width instead of half at the cost of
  /// reaching half a band past each end of the grid. Tiling comes first,
  /// so a band of a non-uniform grid holds its nominal wavelength off
  /// center and averages about its own center instead. Empty is how the
  /// renderer asks whether the jitter is on at all, so a grid with too
  /// few bands to have a width leaves it empty.
  std::vector<float> bandEdges{};

  /// Set all four members from one grid, which is the only way they are
  /// guaranteed to describe the same one.
  void reset(smdl::Span<const float> grid, bool jitter) {
    numBands = grid.size();
    wavelengths = smdl::SpectralColor(grid);
    // Trapezoid band widths for a non-uniform grid. A uniform grid keeps
    // the weights empty and `State::wavelength_weight` null, which the
    // library treats as uniform quadrature, so the default render is
    // unchanged to the bit.
    weights.clear();
    bool uniform{true};
    for (size_t i = 2; i < grid.size(); i++)
      if (std::abs((grid[i] - grid[i - 1]) - (grid[1] - grid[0])) >
          1e-3f * (grid[1] - grid[0]))
        uniform = false;
    if (!uniform) {
      weights.resize(grid.size());
      for (size_t i = 0; i < grid.size(); i++) {
        const float lo{i > 0 ? grid[i - 1] : grid[0]};
        const float hi{i + 1 < grid.size() ? grid[i + 1]
                                           : grid[grid.size() - 1]};
        weights[i] = 0.5f * (hi - lo);
      }
    }
    bandEdges = jitter ? wavelengthBandEdges(grid) : std::vector<float>{};
  }
};

/// The render-wide wavelength grid. See `WavelengthGrid`.
[[nodiscard]] inline WavelengthGrid &renderGrid() noexcept {
  static WavelengthGrid grid{};
  return grid;
}

/// The render-wide shutter interval.
///
/// Set once in `main()` before rendering threads start. Setup-time
/// evaluations sit at `time`; a path traced with an open shutter
/// evaluates at its own offset into the interval, see `PathTime`.
struct Shutter final {
  /// The animation time at shutter open, in seconds.
  float time{};

  /// The shutter length in seconds.
  float length{};

  /// Is the shutter open, so that paths spread over the interval rather
  /// than all landing at shutter open?
  [[nodiscard]] bool isOpen() const noexcept { return length > 0; }

  /// The animation time `fraction` of the way through the interval, in
  /// seconds.
  [[nodiscard]] float secondsAt(float fraction) const noexcept {
    return time + length * fraction;
  }
};

/// The render-wide shutter interval. See `Shutter`.
[[nodiscard]] inline Shutter &renderShutter() noexcept {
  static Shutter shutter{};
  return shutter;
}

/// When a path happens, on both clocks: the shutter fraction in
/// `[0, 1]`, which is what the rays trace at and where every motion
/// key sits, and the seconds `renderShutter().secondsAt(fraction)`,
/// which is what the materials, lights, and media see as
/// `State::animation_time`. The fraction must never reach a state and
/// the seconds must never reach a ray, which is why the two travel as
/// one value. There is no default: a zero pair is right only when the
/// base time is zero.
class PathTime final {
public:
  explicit PathTime(float fraction) noexcept
      : fraction(fraction), seconds(renderShutter().secondsAt(fraction)) {}

  float fraction{};
  float seconds{};
};

/// The render color type: an `smdl::SpectralColor` whose constructors
/// supply the render-wide band count, so the ubiquitous `Color c{}`
/// zero vector and `Color(scalar)` splat idioms work with a runtime
/// band count.
class Color final : public smdl::SpectralColor {
public:
  Color() : SpectralColor(renderGrid().numBands) {}

  Color(float value) : SpectralColor(renderGrid().numBands, value) {}

  /// Construct from however many values are present: a shorter or
  /// empty span (a material coefficient the instance does not have)
  /// leaves the remaining bands zero.
  Color(smdl::Span<const float> values) : SpectralColor(renderGrid().numBands) {
    const size_t n{values.size() < size() ? values.size() : size()};
    for (size_t i = 0; i < n; i++) (*this)[i] = values[i];
  }

  Color(const SpectralColor &other) : SpectralColor(other) {}

  Color(SpectralColor &&other) noexcept
      : SpectralColor(static_cast<SpectralColor &&>(other)) {}
};

/// An `smdl::State` carrying the render-wide fields every evaluation
/// needs: the wavelength grid and, when material construction is involved,
/// the allocator. The geometric fields are applied afterward by
/// `Hit::apply_geometry_to_state()`. The time defaults to the render-wide
/// base time; per-path callers pass the path's own.
[[nodiscard]] inline smdl::State
makeRenderState(const smdl::SpectralColor &wavelengths,
                smdl::BumpPtrAllocator *allocator = nullptr,
                float time = renderShutter().time) noexcept {
  smdl::State state{};
  state.allocator = allocator;
  state.wavelength_base = wavelengths.data();
  // The endpoints come from the nominal grid rather than from
  // `wavelengths`, which under `-wavelength-jitter` is the sample's own
  // perturbed grid: `state::wavelength_min()` and `wavelength_max()` are
  // render-wide constants, and the library's uniform quadrature falls
  // back on their difference, which must not wobble per sample.
  const auto &nominal{renderGrid().wavelengths};
  state.wavelength_min = nominal[0];
  state.wavelength_max = nominal[nominal.size() - 1];
  state.animation_time = time;
  const auto &weights{renderGrid().weights};
  state.wavelength_weight = weights.empty() ? nullptr : weights.data();
  return state;
}

/// Write the sample's jittered wavelength grid into `wavelengths`: band
/// `i` lands at the fraction `xi` across its rectangle in
/// `WavelengthGrid::bandEdges`, so that over many samples the band
/// estimates the mean radiance over that rectangle instead of the
/// radiance at one wavelength.
///
/// One shared `xi` rather than one per band: each band still covers its
/// own rectangle uniformly either way, so the mean is the same, and the
/// rigid shift keeps the spectrum of a single sample correlated, which
/// is what stops the RGB outputs from gaining color noise.
inline void jitterWavelengths(Color &wavelengths, float xi) noexcept {
  const auto &edges{renderGrid().bandEdges};
  for (size_t i = 0; i < wavelengths.size(); i++)
    wavelengths[i] = edges[i] + xi * (edges[i + 1] - edges[i]);
}
