/// \file
/// The render-wide state a renderer test has to install before it can
/// evaluate anything, held for the duration of a scope.
///
/// `Color.h` keeps the wavelength grid and the shutter in namespace-scope
/// variables because the render loop reads them at every path vertex. The
/// suite shares one process and runs its cases in no fixed order, so a
/// test that installs either one and walks away changes what a later case
/// builds its materials against. Everything here puts back what it found.
///
/// Only `Color.h` is needed, so this stays on the near side of
/// `Scene/Scene.h` and does not pull Embree into the cases that have no
/// scene. `RigFixtures.h` beside it writes the glTF rigs.
#pragma once

#include "Fixtures.h"

#include <vector>

#include "Color.h"

/// The render-wide wavelength grid for the duration of a scope.
class ScopedGrid final {
public:
  /// The ramp from 400 to 700 nm across the default band count, which is
  /// the grid every scene test shares.
  ///
  /// This installs the wavelengths alone and leaves the band count, the
  /// quadrature weights, and `stateBase` at their defaults, which is what
  /// a scene test needs and all it needs.
  ScopedGrid() {
    auto grid{std::vector<float>(gRenderGrid.numBands)};
    for (size_t i = 0; i < grid.size(); i++)
      grid[i] = 400.0f + 300.0f * float(i) / float(grid.size() - 1);
    mWavelengths = Color(smdl::Span<const float>(grid.data(), grid.size()));
    gRenderGrid.wavelengths = mWavelengths;
  }

  /// An explicit grid through `WavelengthGrid::reset()`, which sets the
  /// band count and the quadrature weights along with the wavelengths, so
  /// that every `Color` constructed inside the scope is sized to it.
  ScopedGrid(const std::vector<float> &grid, bool shouldJitter) {
    const auto span{smdl::Span<const float>(grid.data(), grid.size())};
    gRenderGrid.reset(span, shouldJitter);
    mWavelengths = Color(span);
  }

  ScopedGrid(const ScopedGrid &) = delete;

  ScopedGrid &operator=(const ScopedGrid &) = delete;

  ~ScopedGrid() { gRenderGrid = mSaved; }

  /// The wavelengths installed, which every `Color` in scope is sized to.
  [[nodiscard]] const Color &wavelengths() const noexcept {
    return mWavelengths;
  }

private:
  const WavelengthGrid mSaved{gRenderGrid};
  Color mWavelengths{};
};

/// The render-wide shutter for the duration of a scope.
class ScopedShutter final {
public:
  ScopedShutter(float time, float exposure) {
    gRenderShutter.time = time;
    gRenderShutter.exposure = exposure;
  }

  ScopedShutter(const ScopedShutter &) = delete;

  ScopedShutter &operator=(const ScopedShutter &) = delete;

  ~ScopedShutter() { gRenderShutter = mSaved; }

private:
  const Shutter mSaved{gRenderShutter};
};
