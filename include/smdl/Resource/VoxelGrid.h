/// \file
#pragma once

#include <vector>

#include "smdl/Common.h"

namespace smdl {

/// \addtogroup scene
/// \{

/// A voxel grid.
///
/// This is the container for volumetric data held by the compiler that
/// is used at runtime by instances of `texture_3d`, the way `Image` is
/// used by instances of `texture_2d`. It stores one scalar field, e.g.,
/// the density or temperature of a participating medium.
///
/// The storage is a brick grid: the extent is covered by bricks of
/// `BRICK_EXTENT` voxels per axis, and each brick is either backed by a
/// dense block of values or is empty, in which case every voxel in it
/// has the uniform background value. Voxels within a brick are stored
/// x-fastest, then y, then z, and so are the bricks themselves within
/// the brick table. There is no mirroring anywhere: unlike 2D texture
/// space, 3D texture space needs no v-flip, so `w` of 0 is `z` of 0.
///
/// The grid remembers the value bounds that make unbiased volume
/// rendering practical: the global minimum and maximum over the whole
/// field, and per-brick minimums and maximums taken over the brick
/// voxels dilated by one voxel on every side, so that the per-brick
/// maximum bounds every trilinearly interpolated value whose support
/// touches the brick. Renderers use these as majorants.
///
/// \note
/// The in-memory layout of the brick table and brick data is
/// deliberately unspecified and may change between versions. The only
/// supported ways to read values are `fetch()` and `sample()`.
///
class SMDL_EXPORT VoxelGrid final {
public:
  /// The number of voxels per axis in a brick.
  static constexpr int BRICK_EXTENT = 16;

  VoxelGrid() = default;

  /// Non-copyable, and non-movable!
  VoxelGrid(const VoxelGrid &) = delete;

public:
  /// Clear everything.
  void clear() noexcept;

  /// Load from file.
  ///
  /// The file format is determined by the extension, case-insensitively:
  /// - `.nvdb` loads a NanoVDB grid,
  /// - `.vol` loads a Mitsuba volume, which must be single-channel
  ///   `float32`.
  ///
  /// A NanoVDB file may contain several named grids (`density`,
  /// `temperature`, ...). `gridName` selects which one to load; empty
  /// selects the first grid in the file. Quantized NanoVDB value types
  /// (`Fp4`, `Fp8`, `Fp16`, `FpN`) are widened to `float` on load.
  /// Mitsuba volumes hold one anonymous grid, so `gridName` must be
  /// empty.
  ///
  /// \return
  /// `std::nullopt` if successful, or else an `Error` describing why
  /// the grid could not be loaded, in which case the grid is left
  /// cleared.
  ///
  [[nodiscard]] std::optional<Error>
  loadFromFile(const std::string &fileName,
               const std::string &gridName = {}) noexcept;

public:
  /// Is valid, i.e., backed by loaded data with a positive extent?
  [[nodiscard]] bool isValid() const noexcept {
    return mExtent.x > 0 && mExtent.y > 0 && mExtent.z > 0;
  }

  /// Get the extent in voxels.
  [[nodiscard]] int3 getExtent() const noexcept { return mExtent; }

  /// Get the number of bricks per axis, i.e., the extent divided by
  /// `BRICK_EXTENT` rounded up.
  [[nodiscard]] int3 getBrickCount() const noexcept { return mBrickCount; }

  /// Get the background value, which fills empty bricks and everything
  /// outside the extent.
  [[nodiscard]] float getBackground() const noexcept { return mBackground; }

  /// Get the global minimum value.
  [[nodiscard]] float getMinValue() const noexcept { return mMinValue; }

  /// Get the global maximum value.
  [[nodiscard]] float getMaxValue() const noexcept { return mMaxValue; }

  /// Get the minimum value of the brick at the given brick coordinate,
  /// taken over the brick voxels dilated by one voxel on every side.
  /// Returns the background outside the brick count.
  [[nodiscard]] float getBrickMinValue(int bx, int by, int bz) const noexcept;

  /// Get the maximum value of the brick at the given brick coordinate,
  /// see `getBrickMinValue()`. This bounds every trilinearly
  /// interpolated value whose support touches the brick, so it is
  /// usable as a per-brick majorant.
  [[nodiscard]] float getBrickMaxValue(int bx, int by, int bz) const noexcept;

  /// Get the world-space bounding box minimum, from the file's
  /// index-to-world transform. Purely metadata: texture space `[0,1]^3`
  /// always spans the extent, and nothing here applies the transform.
  [[nodiscard]] float3 getWorldBoundMin() const noexcept {
    return mWorldBoundMin;
  }

  /// Get the world-space bounding box maximum, see `getWorldBoundMin()`.
  [[nodiscard]] float3 getWorldBoundMax() const noexcept {
    return mWorldBoundMax;
  }

  /// Fetch the value at the given voxel coordinate, or the background
  /// value if the coordinate is outside the extent.
  [[nodiscard]] float fetch(int x, int y, int z) const noexcept;

  /// Sample by trilinear interpolation at the given texture-space
  /// coordinate, where `[0,1]^3` spans the extent, with clamping at the
  /// boundaries. This matches `tex::lookup_float()` with `wrap_clamp`
  /// in every direction, and is the C++ reference for the JIT-compiled
  /// lookup.
  [[nodiscard]] float sample(float3 coord) const noexcept;

private:
  /// The `Emitter` bakes the brick table and brick data pointers into
  /// JIT-compiled code, so it is the one component that legitimately
  /// depends on the storage layout.
  friend class Emitter;

  /// Get the brick table, holding one entry per brick in x-fastest
  /// order: the index of the brick's block in `getBrickData()`, or -1
  /// if the brick is empty. Internal-only: the layout is unspecified.
  [[nodiscard]] const int32_t *getBrickTable() const noexcept {
    return mBrickTable.data();
  }

  /// Get the brick data, holding one dense x-fastest block of
  /// `BRICK_EXTENT^3` values per non-empty brick, in the order the
  /// brick table assigned the indices. Internal-only: the layout is
  /// unspecified.
  [[nodiscard]] const float *getBrickData() const noexcept {
    return mBrickData.data();
  }

  /// The extent in voxels.
  int3 mExtent{};

  /// The number of bricks per axis.
  int3 mBrickCount{};

  /// The background value.
  float mBackground{0.0f};

  /// The global minimum value.
  float mMinValue{0.0f};

  /// The global maximum value.
  float mMaxValue{0.0f};

  /// The world-space bounding box minimum.
  float3 mWorldBoundMin{};

  /// The world-space bounding box maximum.
  float3 mWorldBoundMax{};

  /// The brick table, see `getBrickTable()`. Immutable after load, so
  /// the data pointer is stable and may be baked into JIT-compiled
  /// code.
  std::vector<int32_t> mBrickTable{};

  /// The brick data, see `getBrickData()`. Immutable after load, like
  /// the brick table.
  std::vector<float> mBrickData{};

  /// The per-brick minimum values, dilated, see `getBrickMinValue()`.
  std::vector<float> mBrickMinValues{};

  /// The per-brick maximum values, dilated, see `getBrickMaxValue()`.
  std::vector<float> mBrickMaxValues{};
};

/// \}

} // namespace smdl
