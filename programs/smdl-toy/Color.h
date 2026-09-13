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

/// The cell edges a list of wavelengths implies, as
/// `WavelengthGrid::bandEdges` describes them: halfway to each neighbor,
/// the end cells stopping at the list's ends. Empty for a list of fewer
/// than 2 wavelengths, which has no width to speak of.
[[nodiscard]] inline std::vector<double>
wavelengthBandEdges(smdl::Span<const float> wavelens) {
  const size_t numBands{wavelens.size()};
  if (numBands < 2) return {};
  std::vector<double> edges(numBands + 1);
  for (size_t i = 1; i < numBands; i++)
    edges[i] = 0.5 * (double(wavelens[i - 1]) + double(wavelens[i]));
  edges.front() = double(wavelens[0]);
  edges.back() = double(wavelens[numBands - 1]);
  return edges;
}

/// A grid before it is the render's: the cell edges and the wavelength
/// of each cell, as `WavelengthGrid` holds them. Either a list of
/// wavelengths with the cells it implies, through `fromWavelengths()`,
/// or cells placed one by one with a wavelength inside each, which is
/// how a sensor's curves place a grid and how a resumed file restates
/// one.
struct WavelengthCells final {
  /// The cell edges in nanometers, one more than the wavelengths, or
  /// empty for a single wavelength, which has no width to speak of.
  std::vector<double> edges{};

  /// The wavelengths in nanometers, each inside its cell.
  std::vector<float> wavelengths{};

  /// The list `wavelens` with the cells it implies; see
  /// `wavelengthBandEdges()`.
  [[nodiscard]] static WavelengthCells
  fromWavelengths(smdl::Span<const float> wavelens) {
    WavelengthCells cells{};
    cells.edges = wavelengthBandEdges(wavelens);
    cells.wavelengths.assign(wavelens.data(),
                             wavelens.data() + wavelens.size());
    return cells;
  }

  /// Do these describe a grid: at least one wavelength, one more edge
  /// than wavelengths or none for a single wavelength, the edges
  /// strictly increasing, and each wavelength inside its cell?
  [[nodiscard]] bool isValid() const noexcept {
    const size_t numBands{wavelengths.size()};
    if (numBands == 0) return false;
    if (edges.empty()) return numBands == 1;
    if (edges.size() != numBands + 1) return false;
    for (size_t i = 0; i < numBands; i++)
      if (!(edges[i] < edges[i + 1] && edges[i] <= double(wavelengths[i]) &&
            double(wavelengths[i]) <= edges[i + 1]))
        return false;
    return true;
  }
};

/// One wavelength grid: `size()` cells tiling one span of wavelength,
/// each with the wavelength that represents it and the width it weighs.
///
/// The cells come first. Band `i` is the cell from `bandEdges[i]` to
/// `bandEdges[i + 1]`, `widths[i]` wide, and `wavelengths[i]` is where
/// the band is evaluated when the grid holds still and what labels it in
/// every output. Every integral the renderer takes over the grid weighs
/// band `i` by `widths[i]`, and so does the JIT's color to RGB through
/// `State::wavelengthWeight`, so the two agree on any grid.
///
/// A list of wavelengths, from the command line or a resumed file,
/// implies its cells by `wavelengthBandEdges()`: halfway to each
/// neighbor, the end cells stopping at the list's ends. The cells then
/// tile exactly the span of the list with no gap and no overlap, and
/// each is as wide as the trapezoid rule weighs its wavelength, so the
/// grid held still integrates the trapezoid rule and the grid under the
/// jitter integrates the same thing without aliasing. Full-width end
/// cells would center every cell of a uniform list, at the cost of
/// sampling half a cell past each end, outside the span asked for.
/// Tiling comes first, so a wavelength sits off its cell's center
/// wherever its neighbors are unequally far, and on the outer edge of
/// each end cell; the cell averages about its own center instead.
struct WavelengthGrid final {
  /// The wavelengths in nanometers, one per band.
  smdl::SpectralColor wavelengths{};

  /// The cell edges in nanometers, one more than the band count, or
  /// empty for a grid of fewer than 2 bands, which has no width to speak
  /// of. In double, so that a width, the difference of two, is exact.
  std::vector<double> bandEdges{};

  /// The cell widths in nanometers, one per band: what every integral
  /// the renderer takes over the grid weighs a band by. A grid of one
  /// band is half a unit wide, by convention.
  std::vector<double> widths{};

  /// The widths as the JIT reads them through `State::wavelengthWeight`,
  /// which is a `float` table. The library's rule for no weights gives
  /// every band the same width, end bands included, which a grid ending
  /// on its own wavelengths does not have.
  std::vector<float> weights{};

  /// The band count.
  [[nodiscard]] size_t size() const noexcept { return wavelengths.size(); }

  /// The span: the edges' ends, or the one wavelength of a grid with no
  /// cells.
  ///
  /// \{
  [[nodiscard]] float minWavelength() const noexcept {
    return bandEdges.empty() ? wavelengths[0] : float(bandEdges.front());
  }
  [[nodiscard]] float maxWavelength() const noexcept {
    return bandEdges.empty() ? wavelengths[size() - 1]
                             : float(bandEdges.back());
  }
  /// \}

  /// Set every member from `cells`, which must describe a grid, which
  /// is the only way the members are guaranteed to describe the same
  /// one.
  void reset(const WavelengthCells &cells) {
    SMDL_SANITY_CHECK(cells.isValid());
    const size_t numBands{cells.wavelengths.size()};
    wavelengths = smdl::SpectralColor(
        smdl::Span<const float>(cells.wavelengths.data(), numBands));
    bandEdges = cells.edges;
    widths.assign(numBands, 0.5);
    for (size_t i = 0; i + 1 < bandEdges.size(); i++)
      widths[i] = bandEdges[i + 1] - bandEdges[i];
    weights.resize(numBands);
    for (size_t i = 0; i < numBands; i++) weights[i] = float(widths[i]);
  }

  /// Write the grid into `state`: the endpoints and the weights, which
  /// is what an evaluation on this grid reads beside the wavelengths it
  /// carries. The endpoints are the grid's own span rather than the
  /// wavelengths an evaluation carries, which under `-wavelength-jitter`
  /// is the sample's own perturbed grid: `state::wavelength_min()` and
  /// `wavelength_max()` are constants of the grid, which must not wobble
  /// per sample.
  void applyTo(smdl::State &state) const noexcept {
    state.wavelengthMin = minWavelength();
    state.wavelengthMax = maxWavelength();
    state.wavelengthWeight = weights.data();
  }
};

/// The span the RGB pipeline is defined over, in nanometers, with a
/// nanometer of slack at each end so that a grid ending exactly on one of
/// them is not called out. Outside it, RGB-sourced spectra extend flat
/// from their end values and the CIE projection sees little; see
/// `RenderGrid::isBeyondVisible`.
///
/// \{
constexpr float VISIBLE_MIN{380.0f};
constexpr float VISIBLE_MAX{780.0f};
constexpr float VISIBLE_SLACK{1.0f};
/// \}

/// Declared here for `RenderGrid::wavelengths()`; defined below, since
/// its constructors read `gRenderGrid`.
class Color;

/// The render's wavelength grids: one, or one per band of a sensor's
/// tile, every pixel evaluating on the grid of the band it reads
/// through.
///
/// Set exactly once in `main()`, through `reset()`, before anything
/// constructs a `Color` and long before rendering threads start; read
/// everywhere after. Every grid has the band count, which sizes every
/// `Color` and the films. Under a tile, a pixel's samples are evaluated
/// at its own grid's wavelengths and its states carry that grid; what
/// is evaluated once for the whole render, a material's structural
/// flags, a light's selection weight, a displacement, is evaluated on
/// the first grid, where any grid of the count would do. A spectrum
/// baked once and read along a path, a layout light's or the haze's,
/// is baked per grid and read by the path's grid index.
struct RenderGrid final {
  /// The band count, which sizes every `Color`. The default of 16
  /// matches `smdl::SpectralColor::INLINE_CAPACITY`, so a default
  /// render's colors never touch the heap.
  size_t numBands{16};

  /// The grids, at least one, every one of the band count. Under a tile
  /// one per band the tile lays down, in the order the tile first names
  /// them; else the one grid.
  std::vector<WavelengthGrid> grids = std::vector<WavelengthGrid>(1);

  /// The tile that picks a pixel's grid, as the sensor's tile does its
  /// band: its width in pixels, or 0 without one, and the grid index of
  /// each cell row by row; see `tileIndexAt()`.
  ///
  /// \{
  size_t tileColumns{};
  size_t tileRows{};
  std::vector<size_t> tileGrids{};
  /// \}

  /// Does every sample draw its own grid inside the cells, which is
  /// `-wavelength-jitter`? Never on a grid with no cells.
  bool isJittering{};

  /// Does any grid reach outside the visible? Everything RGB-sourced
  /// degrades there, so several later stages say so once rather than
  /// rendering a mysteriously dark image. Derived by `reset()` from the
  /// grids themselves, so it cannot disagree with them.
  bool isBeyondVisible{};

  /// The `smdl::State` every evaluation starts from: the library
  /// defaults plus the first grid's endpoints and quadrature weights.
  /// `makeRenderState()` copies it rather than building a fresh state,
  /// which is half a kilobyte of stores per path vertex otherwise; a
  /// pixel under a tile then applies its own grid over it.
  smdl::State stateBase{};

  /// Is there a tile, so that each pixel evaluates on its own grid?
  [[nodiscard]] bool hasTile() const noexcept { return tileColumns > 0; }

  /// The grid frame pixel `(x, y)` evaluates on, by index and itself.
  ///
  /// \{
  [[nodiscard]] size_t gridIndexAt(size_t x, size_t y) const noexcept {
    return hasTile() ? tileGrids[tileIndexAt(tileColumns, tileRows, x, y)] : 0;
  }
  [[nodiscard]] const WavelengthGrid &at(size_t x, size_t y) const noexcept {
    return grids[gridIndexAt(x, y)];
  }
  /// \}

  /// The first grid: the one grid without a tile, and under one the
  /// grid a render-wide evaluation runs on.
  [[nodiscard]] const WavelengthGrid &first() const noexcept {
    return grids.front();
  }

  /// The first grid's wavelengths, which is what a render-wide
  /// evaluation runs on and what every stage outside the render loop
  /// reads the grid as. Defined out of line, `Color` being declared
  /// below, and returned by value, `Color` not being a member for the
  /// same reason; every caller is setup-time or output-time.
  [[nodiscard]] Color wavelengths() const;

  /// The span every grid lies within.
  ///
  /// \{
  [[nodiscard]] float minWavelength() const noexcept {
    float value{INF};
    for (const auto &grid : grids)
      value = std::min(value, grid.minWavelength());
    return value;
  }
  [[nodiscard]] float maxWavelength() const noexcept {
    float value{-INF};
    for (const auto &grid : grids)
      value = std::max(value, grid.maxWavelength());
    return value;
  }
  /// \}

  /// Set every member: the grids from `family`, one per band the tile
  /// lays down, and the tile as `tileColumns` cells wide holding the
  /// grid index of each cell, which is the only way the members are
  /// guaranteed to describe the same render. Every member of the family
  /// must have the same band count.
  void reset(std::vector<WavelengthCells> family, size_t columns,
             std::vector<size_t> tile, bool shouldJitter) {
    SMDL_SANITY_CHECK(!family.empty());
    SMDL_SANITY_CHECK(columns == 0
                          ? tile.empty()
                          : !tile.empty() && tile.size() % columns == 0);
    numBands = family.front().wavelengths.size();
    grids.resize(family.size());
    for (size_t k = 0; k < family.size(); k++) {
      SMDL_SANITY_CHECK(family[k].wavelengths.size() == numBands);
      grids[k].reset(family[k]);
    }
    for (const auto index : tile) SMDL_SANITY_CHECK(index < grids.size());
    tileColumns = columns;
    tileRows = columns > 0 ? tile.size() / columns : 0;
    tileGrids = std::move(tile);
    isJittering = shouldJitter && !grids.front().bandEdges.empty();
    isBeyondVisible = minWavelength() < VISIBLE_MIN - VISIBLE_SLACK ||
                      maxWavelength() > VISIBLE_MAX + VISIBLE_SLACK;
    stateBase = smdl::State{};
    grids.front().applyTo(stateBase);
  }

  /// One grid for every pixel.
  void reset(const WavelengthCells &cells, bool shouldJitter) {
    reset(std::vector<WavelengthCells>{cells}, 0, {}, shouldJitter);
  }

  /// The list `grid` with the cells it implies; see
  /// `WavelengthCells::fromWavelengths()`.
  void reset(smdl::Span<const float> grid, bool shouldJitter) {
    reset(WavelengthCells::fromWavelengths(grid), shouldJitter);
  }
};

/// The render's wavelength grids. See `RenderGrid`.
///
/// A namespace-scope variable rather than a function-local static
/// because the latter is read through a guard, and `Color`'s
/// constructor reads this at every path vertex. The cost is that it is
/// initialized during startup instead of on first use, so nothing may
/// touch it from another translation unit's static initializer.
inline RenderGrid gRenderGrid{};

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

inline Color RenderGrid::wavelengths() const { return {first().wavelengths}; }

/// An `smdl::State` carrying the render-wide fields every evaluation
/// needs: the wavelength grid and, when material construction is involved,
/// the allocator. The geometric fields are applied afterward by
/// `Hit::applyGeometryToState()`. The time defaults to the render-wide
/// base time, the hero wavelength to the library's reference, the d
/// line, and the grid to the first; per-path callers pass the path's own
/// of each, since a material may read any of them.
[[nodiscard]] inline smdl::State
makeRenderState(const smdl::SpectralColor &wavelengths,
                smdl::BumpPtrAllocator *allocator = nullptr,
                float time = gRenderShutter.time,
                float wavelengthHero = gRenderGrid.stateBase.wavelengthHero,
                size_t gridIndex = 0) noexcept {
  smdl::State state{gRenderGrid.stateBase};
  if (gridIndex != 0) gRenderGrid.grids[gridIndex].applyTo(state);
  state.allocator = allocator;
  state.wavelengthBase = wavelengths.data();
  state.animationTime = time;
  state.wavelengthHero = wavelengthHero;
  return state;
}

/// Write the sample's jittered wavelength grid into `wavelengths`: band
/// `i` lands at the fraction `xi` across its cell in `grid`, so that
/// over many samples the band estimates the mean radiance over that
/// cell instead of the radiance at one wavelength. The edges are
/// rounded to `float` first, so the draw is `float` arithmetic between
/// `float` edges, as the grid it writes is.
///
/// One shared `xi` rather than one per band: each band still covers its
/// own cell uniformly either way, so the mean is the same, and the
/// shared shift keeps the spectrum of a single sample correlated, which
/// is what stops the RGB outputs from gaining color noise. Every band
/// moves the same way across its own cell, so the grid stays strictly
/// increasing, as the library requires.
inline void jitterWavelengths(Color &wavelengths, const WavelengthGrid &grid,
                              float xi) noexcept {
  const std::vector<double> &edges{grid.bandEdges};
  for (size_t i = 0; i < wavelengths.size(); i++) {
    const float lo{float(edges[i])};
    const float hi{float(edges[i + 1])};
    wavelengths[i] = lo + xi * (hi - lo);
  }
}
