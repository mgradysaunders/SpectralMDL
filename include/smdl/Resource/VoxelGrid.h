/// \file
#pragma once

#include <algorithm>
#include <vector>

#include "smdl/Common.h"

namespace smdl {

/// \addtogroup resource
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
/// The grid also carries the value bounds that make unbiased volume
/// rendering practical: the global minimum and maximum over the whole
/// field, and a *majorant grid* of local minimums and maximums over
/// cells of `MAJORANT_EXTENT` voxels per axis, dilated by one voxel on
/// every side so that a cell's maximum bounds every trilinearly
/// interpolated value whose support touches it. A null-collision
/// tracker steps this grid and tracks against those local bounds
/// instead of against the global maximum.
///
/// The majorant cell is deliberately not the brick. The brick is sized
/// for the density lookup and the sparse storage; the cell is sized for
/// how tightly it bounds the field, which is a different question with
/// a different answer, and the two are free to move independently.
///
/// \note
/// The in-memory layout of the brick table and brick data is
/// deliberately unspecified and may change between versions. The only
/// supported ways to read values are `fetch()` and `sample()`.
///
class SMDL_EXPORT VoxelGrid final {
public:
  /// The number of voxels per axis in a brick, which is a unit of
  /// storage. See `getMajorantExtent()` for the unit of majorant
  /// bounds.
  static constexpr int BRICK_EXTENT = 16;

  /// The most majorant cells the grid is divided into along its
  /// longest axis, which `majorantExtentFor()` sizes the cell to hit.
  ///
  /// Tighter cells cost a null-collision tracker fewer wasted queries
  /// and more traversal steps, and where the balance falls is a
  /// property of the traversal, so measure before moving this. What it
  /// must not become is a fixed voxels-per-cell: that ties the majorant
  /// granularity to the field's resolution, so the same medium
  /// resampled finer gets a finer majorant grid it does not want and
  /// drowns in traversal steps. Bounding the cell count instead bounds
  /// both the steps per ray and the side table, at 8 bytes per cell.
  static constexpr int MAJORANT_TARGET_CELLS = 64;

  /// The number of voxels per axis in a majorant cell of a grid with
  /// the given extent: the smallest power of two that keeps every axis
  /// within `MAJORANT_TARGET_CELLS` cells.
  [[nodiscard]] static int majorantExtentFor(int3 extent) noexcept {
    const int longest{
        std::max(std::max(extent.x, extent.y), std::max(extent.z, 1))};
    int result{1};
    while ((longest + result - 1) / result > MAJORANT_TARGET_CELLS) result *= 2;
    return result;
  }

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

  /// Save to file.
  ///
  /// The file format is determined by the extension, exactly as
  /// `loadFromFile()` determines it. `gridName` names the grid a NanoVDB
  /// file carries, defaulting to `density`; Mitsuba volumes have no
  /// named grids, so it must be empty for `.vol`.
  ///
  /// This is a format conversion rather than an archive. Every value
  /// inside the extent survives it exactly, as do the extent itself and
  /// the world bounds, but a NanoVDB grid is written from what differs
  /// from the background, so the sparse topology of a grid that was
  /// loaded from NanoVDB comes back only approximately: `loadFromFile()`
  /// keeps no record of which voxels were active, and rebases the index
  /// origin to zero.
  ///
  /// \return
  /// `std::nullopt` if successful, or else an `Error` describing why the
  /// grid could not be saved.
  ///
  [[nodiscard]] std::optional<Error>
  saveToFile(const std::string &fileName,
             const std::string &gridName = {}) const noexcept;

  /// Save several grids to one NanoVDB file, which is how a medium whose
  /// material reads more than one field (`density` and `temperature`,
  /// say) travels as one file. The two vectors must be the same
  /// non-zero length, every grid must be valid, and the names must be
  /// non-empty and distinct. Mitsuba volumes hold one anonymous grid, so
  /// `.vol` is rejected here. See `saveToFile()` for what survives.
  [[nodiscard]] static std::optional<Error>
  saveToFile(const std::string &fileName,
             const std::vector<const VoxelGrid *> &voxelGrids,
             const std::vector<std::string> &gridNames) noexcept;

public:
  /// Is valid, i.e., backed by loaded data with a positive extent?
  [[nodiscard]] bool isValid() const noexcept {
    return mExtent.x > 0 && mExtent.y > 0 && mExtent.z > 0;
  }

  /// Get the extent in voxels.
  [[nodiscard]] int3 getExtent() const noexcept { return mExtent; }

  /// Get the memory the grid holds in bytes: the brick table, the brick
  /// data, and the majorant table.
  [[nodiscard]] size_t getSizeInBytes() const noexcept {
    return mBrickTable.size() * sizeof(int32_t) +
           mBrickData.size() * sizeof(float) +
           mMajorantBounds.size() * sizeof(float2);
  }

  /// Get the number of bricks per axis, i.e., the extent divided by
  /// `BRICK_EXTENT` rounded up.
  [[nodiscard]] int3 getBrickCount() const noexcept { return mBrickCount; }

  /// Get the number of voxels per axis in a majorant cell, which
  /// `majorantExtentFor()` chose from the extent at load.
  [[nodiscard]] int getMajorantExtent() const noexcept {
    return mMajorantExtent;
  }

  /// Get the number of majorant cells per axis, i.e., the extent
  /// divided by `getMajorantExtent()` rounded up.
  [[nodiscard]] int3 getMajorantCount() const noexcept {
    return mMajorantCount;
  }

  /// Get the background value, which fills empty bricks and everything
  /// outside the extent.
  [[nodiscard]] float getBackground() const noexcept { return mBackground; }

  /// Get the global minimum value.
  [[nodiscard]] float getMinValue() const noexcept { return mMinValue; }

  /// Get the global maximum value.
  [[nodiscard]] float getMaxValue() const noexcept { return mMaxValue; }

  /// Get the minimum and maximum value, in that order, of the majorant
  /// cell at the given cell coordinate, taken over the cell voxels
  /// dilated by one voxel on every side. The maximum bounds every
  /// trilinearly interpolated value whose support touches the cell, so
  /// it is usable as a local majorant, and the minimum lower bounds the
  /// same set, which is what residual ratio tracking wants as its
  /// analytic control. Returns the background twice outside the cell
  /// count.
  ///
  /// The pair comes back together, and is stored together, because a
  /// tracker crossing a cell wants both and would otherwise pay for the
  /// bounds check, the index arithmetic, and the cache miss twice.
  [[nodiscard]] float2 getMajorantBounds(int cx, int cy,
                                         int cz) const noexcept {
    if (!isMajorantCellInside(cx, cy, cz)) return float2(mBackground);
    return mMajorantBounds[majorantIndex(cx, cy, cz)];
  }

  /// Get the whole majorant cell table, `getMajorantCount()` cells with
  /// `x` fastest, for a tracker that steps a flat index through the
  /// cells it crosses instead of re-deriving it per cell. Empty until
  /// a grid is loaded.
  [[nodiscard]] Span<const float2> getMajorantTable() const noexcept {
    return {mMajorantBounds.data(), mMajorantBounds.size()};
  }

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

  /// The number of voxels per axis in a majorant cell.
  int mMajorantExtent{1};

  /// The number of majorant cells per axis.
  int3 mMajorantCount{};

  [[nodiscard]] bool isBrickInside(int bx, int by, int bz) const noexcept {
    return 0 <= bx && bx < mBrickCount.x && //
           0 <= by && by < mBrickCount.y && //
           0 <= bz && bz < mBrickCount.z;
  }

  [[nodiscard]] size_t brickIndex(int bx, int by, int bz) const noexcept {
    return size_t(bx + mBrickCount.x * (by + int64_t(mBrickCount.y) * bz));
  }

  [[nodiscard]] bool isMajorantCellInside(int cx, int cy,
                                          int cz) const noexcept {
    return 0 <= cx && cx < mMajorantCount.x && //
           0 <= cy && cy < mMajorantCount.y && //
           0 <= cz && cz < mMajorantCount.z;
  }

  [[nodiscard]] size_t majorantIndex(int cx, int cy, int cz) const noexcept {
    return size_t(cx +
                  mMajorantCount.x * (cy + int64_t(mMajorantCount.y) * cz));
  }

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

  /// The per-cell dilated minimum and maximum, in that order, see
  /// `getMajorantBounds()`.
  std::vector<float2> mMajorantBounds{};
};

/// \}

} // namespace smdl
