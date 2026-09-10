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

/// The jitter band edges of the wavelength grid `wavelens`, as
/// `WavelengthGrid::bandEdges` describes them, or empty for a grid of
/// fewer than 2 wavelengths, which has no band width to speak of.
[[nodiscard]]
inline std::vector<float>
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
  /// Empty keeps `State::wavelengthWeight` null, which the library
  /// treats as uniform quadrature; a non-uniform `-wavelengths` grid
  /// fills this with trapezoid band widths, which both the JIT
  /// color-to-RGB conversion and the night tonemap integrate against.
  std::vector<float> weights{};

  /// The `smdl::State` every evaluation starts from: the library
  /// defaults plus the wavelength endpoints and quadrature weights,
  /// which no evaluation varies. `makeRenderState()` copies it rather
  /// than building a fresh state, which is half a kilobyte of stores
  /// per path vertex otherwise.
  smdl::State stateBase{};

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
  void reset(smdl::Span<const float> grid, bool shouldJitter) {
    numBands = grid.size();
    wavelengths = smdl::SpectralColor(grid);
    // Trapezoid band widths for a non-uniform grid. A uniform grid keeps
    // the weights empty and `State::wavelengthWeight` null, which the
    // library treats as uniform quadrature, so the default render is
    // unchanged to the bit.
    weights.clear();
    bool isUniform{true};
    for (size_t i = 2; i < grid.size(); i++)
      if (std::abs((grid[i] - grid[i - 1]) - (grid[1] - grid[0])) >
          1e-3f * (grid[1] - grid[0]))
        isUniform = false;
    if (!isUniform) {
      weights.resize(grid.size());
      for (size_t i = 0; i < grid.size(); i++) {
        const float lo{i > 0 ? grid[i - 1] : grid[0]};
        const float hi{i + 1 < grid.size() ? grid[i + 1]
                                           : grid[grid.size() - 1]};
        weights[i] = 0.5f * (hi - lo);
      }
    }
    bandEdges = shouldJitter ? wavelengthBandEdges(grid) : std::vector<float>{};
    // The endpoints come from the nominal grid rather than from the
    // wavelengths an evaluation carries, which under
    // `-wavelength-jitter` is the sample's own perturbed grid:
    // `state::wavelength_min()` and `wavelength_max()` are render-wide
    // constants, and the library's uniform quadrature falls back on
    // their difference, which must not wobble per sample.
    stateBase = smdl::State{};
    stateBase.wavelengthMin = grid[0];
    stateBase.wavelengthMax = grid[grid.size() - 1];
    stateBase.wavelengthWeight = weights.empty() ? nullptr : weights.data();
  }
};

/// The render-wide wavelength grid. See `WavelengthGrid`.
///
/// A namespace-scope variable rather than a function-local static
/// because the latter is read through a guard, and `Color`'s
/// constructor reads this at every path vertex. The cost is that it is
/// initialized during startup instead of on first use, so nothing may
/// touch it from another translation unit's static initializer.
inline WavelengthGrid gRenderGrid{};

/// The trapezoid quadrature widths of the render grid `wavelens` in
/// nanometers, one per band: on a uniform grid the spacing, halved at
/// the two ends, and on a non-uniform grid `gRenderGrid.weights`, which
/// are that rule. What a grid held still integrates against, as the
/// rectangles of `WavelengthGrid::bandEdges` are what a jittered one
/// does.
///
/// The uniform arithmetic is spelled out rather than taken from the
/// weights so that a default render is unchanged to the bit; a grid of
/// one band has half a unit of width, as the end of any grid does.
[[nodiscard]] inline std::vector<double>
wavelengthTrapezoidWidths(smdl::Span<const float> wavelens) {
  const size_t numBands{wavelens.size()};
  auto widths{std::vector<double>(numBands)};
  const double dLambda{
      numBands > 1 ? (double(wavelens[numBands - 1]) - double(wavelens[0])) /
                         double(numBands - 1)
                   : 1.0};
  const auto &quadWeights{gRenderGrid.weights};
  for (size_t i = 0; i < numBands; i++) {
    const double trap{i == 0 || i == numBands - 1 ? 0.5 : 1.0};
    widths[i] = quadWeights.empty() ? dLambda * trap : double(quadWeights[i]);
  }
  return widths;
}

/// The render-wide shutter interval: the frame.
///
/// Set once in `main()` before rendering threads start. The frame opens
/// when the first line does, at `time`, and shuts `exposure + readout`
/// seconds later, when the last line does; every shut key in the render
/// (the layout's motion, the camera's framing, a deforming mesh) is
/// sampled at that instant. Setup-time evaluations sit at `time`; a path
/// evaluates at its own fraction of the frame, see `PathTime`.
///
/// A global shutter exposes every line over the whole frame. A rolling
/// shutter reads the lines out one after another, so line `i` of `N`
/// opens `readout * i / (N - 1)` after the first and stays open for
/// `exposure`; `fractionAt()` is where a sample's draw within its line's
/// exposure becomes its fraction of the frame.
struct Shutter final {
  /// The animation time at which the first line opens, in seconds.
  float time{};

  /// The seconds each line stays open: the camera file's `shutter`.
  float exposure{};

  /// The seconds the readout sweeps the frame, 0 for a global shutter.
  float readout{};

  /// Does the readout sweep along x (left or right) rather than y?
  bool isReadoutAlongX{};

  /// Does the readout sweep toward the smaller pixel index (up or left)?
  bool isReadoutReversed{};

  /// The pixel lines along the sweep axis.
  size_t numReadoutLines{1};

  /// The frame interval in seconds.
  [[nodiscard]] float length() const noexcept { return exposure + readout; }

  /// Does the frame span time, so that motion has two keys to lower
  /// between and a shut pose to bake, rather than everything landing on
  /// `time`?
  [[nodiscard]] bool spansTime() const noexcept { return length() > 0; }

  /// Is there an exposure for a sample to draw its instant within? A
  /// readout with a shut exposure spans time and draws nothing: each
  /// line lands at its own instant.
  [[nodiscard]] bool hasExposure() const noexcept { return exposure > 0; }

  /// The animation time `fraction` of the way through the frame, in
  /// seconds.
  [[nodiscard]] float secondsAt(float fraction) const noexcept {
    return time + length() * fraction;
  }

  /// The frame fraction of a sample of pixel `(x, y)` drawn at `xi`
  /// within its line's exposure.
  ///
  /// Without a readout this is `xi` itself, by a branch rather than by
  /// arithmetic, so that a global shutter renders bit for bit what it
  /// did before there was a readout to speak of. With one, the ends are
  /// exact: the first line at `xi = 0` is 0 and the last at `xi = 1` is
  /// 1, and nothing in between exceeds 1, because Embree's motion domain
  /// is `[0, 1]` and a ray whose time rounds past it misses every moving
  /// geometry.
  [[nodiscard]] float fractionAt(size_t x, size_t y, float xi) const noexcept {
    if (readout <= 0) return xi;
    if (numReadoutLines <= 1) return xi * exposure / length();
    size_t line{isReadoutAlongX ? x : y};
    SMDL_SANITY_CHECK(line < numReadoutLines);
    if (isReadoutReversed) line = numReadoutLines - 1 - line;
    const float u{float(line) / float(numReadoutLines - 1)};
    if (exposure <= 0) return u;
    // At `u = 1` the numerator is at most the denominator, since `xi`
    // is at most 1 and rounding is monotone, so the quotient is at most
    // 1.
    return (u * readout + xi * exposure) / (readout + exposure);
  }
};

/// The render-wide shutter interval, a namespace-scope variable for the
/// reason `gRenderGrid` is. See `Shutter`.
inline Shutter gRenderShutter{};

/// When a path happens, on both clocks: the shutter fraction in
/// `[0, 1]`, which is what the rays trace at and where every motion
/// key sits, and the seconds `gRenderShutter.secondsAt(fraction)`,
/// which is what the materials, lights, and media see as
/// `State::animationTime`. The fraction must never reach a state and
/// the seconds must never reach a ray, which is why the two travel as
/// one value. There is no default: a zero pair is right only when the
/// base time is zero.
class PathTime final {
public:
  explicit PathTime(float fraction) noexcept
      : fraction(fraction), seconds(gRenderShutter.secondsAt(fraction)) {}

  float fraction{};
  float seconds{};
};

/// The render color type: an `smdl::SpectralColor` whose constructors
/// supply the render-wide band count, so the ubiquitous `Color c{}`
/// zero vector and `Color(scalar)` splat idioms work with a runtime
/// band count.
class Color final : public smdl::SpectralColor {
public:
  Color() : SpectralColor(gRenderGrid.numBands) {}

  Color(float value) : SpectralColor(gRenderGrid.numBands, value) {}

  /// Construct from however many values are present: a shorter or
  /// empty span (a material coefficient the instance does not have)
  /// leaves the remaining bands zero.
  Color(smdl::Span<const float> values) : SpectralColor(gRenderGrid.numBands) {
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
/// `Hit::applyGeometryToState()`. The time defaults to the render-wide
/// base time; per-path callers pass the path's own.
[[nodiscard]] inline smdl::State
makeRenderState(const smdl::SpectralColor &wavelengths,
                smdl::BumpPtrAllocator *allocator = nullptr,
                float time = gRenderShutter.time) noexcept {
  smdl::State state{gRenderGrid.stateBase};
  state.allocator = allocator;
  state.wavelengthBase = wavelengths.data();
  state.animationTime = time;
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
  const auto &edges{gRenderGrid.bandEdges};
  for (size_t i = 0; i < wavelengths.size(); i++)
    wavelengths[i] = edges[i] + xi * (edges[i + 1] - edges[i]);
}
