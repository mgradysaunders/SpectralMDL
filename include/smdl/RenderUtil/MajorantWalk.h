/// \file
#pragma once

#include <algorithm>

#include "smdl/Resource/VoxelGrid.h"
#include "smdl/Support/Macros.h"

namespace smdl {

/// \addtogroup renderutil
/// \{

/// One majorant span of a segment: over `[t0, t1)` of the segment
/// parameter the tracking majorant is the declared majorant scaled by
/// `scale`, and the declared majorant scaled by `scaleMin` is a lower
/// bound of the extinction over the span, which residual ratio tracking
/// uses as an analytic control. Outside the grid, and without one, the
/// bounds are the trivial 1 and 0.
struct MajorantSpan final {
  float t0{};
  float t1{};
  float scale{};
  float scaleMin{};
};

/// Walks the majorant spans of one segment against the majorant cells
/// of a `VoxelGrid`. Without a grid there is a single span at the global
/// scale of 1. With one, an Amanatides-Woo walk over the cells the
/// segment crosses inside the grid box yields one span per cell at the
/// cell's dilated-maximum-over-global-maximum scale, and the portions of
/// the segment outside the box are conservative global-scale spans,
/// since the coefficient the caller tracks need not be bounded there by
/// any cell (a clamp-wrapped lookup holds edge values).
///
/// Majorant cell space is voxel space divided by `getMajorantExtent()`:
/// the cell at index `(i, j, k)` is the unit cube with that corner, and
/// the grid box runs from the origin to `getExtent()` over the majorant
/// extent, so partial cells at the high boundary are covered exactly.
///
/// The walk has no phase state: the stretch before the box, the cells,
/// and the stretch after it are three sequential tests on the distance
/// reached so far. A step is an axis pick, one table load through a flat
/// index that the step adjusts by a per-axis stride, and the empty test;
/// only a non-empty cell pays for its scale. The walk leaves the box only
/// by rounding, a hair before the exit distance, and from then on reads
/// the background as the grid would, so its spans are the grid's.
class MajorantSpanWalk final {
public:
  /// The segment `cellOrg + t * cellDir` in majorant cell space, over
  /// `[0, tEnd)`; `invMaxValue`, one over the grid's `getMaxValue()`,
  /// scales cell maxima into `[0, 1]`. `shouldSkipEmptyCells` says a zero
  /// maximum means a zero majorant, so such a cell never reaches the
  /// caller. A null grid gives the one global span.
  SMDL_ALWAYS_INLINE
  MajorantSpanWalk(const VoxelGrid *grid, const float3 &cellOrg,
                   const float3 &cellDir, float invMaxValue, float tEnd,
                   bool shouldSkipEmptyCells) noexcept
      : invMaxValue_(invMaxValue), tEnd_(tEnd),
        shouldSkipEmptyCells_(shouldSkipEmptyCells) {
    if (!grid) return;
    // One reciprocal per axis, which both the slab test and the walk
    // setup below multiply by.
    float3 invDir{};
    for (int j = 0; j < 3; j++)
      if (cellDir[j] != 0.0f) invDir[j] = 1.0f / cellDir[j];
    // Clip the segment against the cell-space box with the slab test.
    // The box upper corner is the fractional cell extent, so partial
    // cells at the high boundary are covered exactly.
    const int3 extent{grid->getExtent()};
    const float cellExtent{float(grid->getMajorantExtent())};
    const float3 boxMax{float(extent.x) / cellExtent,
                        float(extent.y) / cellExtent,
                        float(extent.z) / cellExtent};
    float tEnter{0.0f};
    float tLeave{tEnd};
    for (int j = 0; j < 3; j++) {
      const float o{cellOrg[j]};
      if (SMDL_LIKELY(cellDir[j] != 0.0f)) {
        const float tA{(0.0f - o) * invDir[j]};
        const float tB{(boxMax[j] - o) * invDir[j]};
        tEnter = std::max(tEnter, std::min(tA, tB));
        tLeave = std::min(tLeave, std::max(tA, tB));
      } else if (o < 0.0f || o > boxMax[j]) {
        tEnter = tEnd;
        tLeave = 0.0f;
      }
    }
    // A segment that never enters the grid box leaves the empty cell
    // interval the walk starts on, which yields the one global span.
    if (!(tEnter < tLeave)) return;
    table_ = grid->getMajorantTable().data();
    background_ = grid->getBackground();
    size_ = grid->getMajorantCount();
    tEnter_ = tEnter;
    tLeave_ = tLeave;
    const float3 pEnter{cellOrg + tEnter * cellDir};
    for (int j = 0; j < 3; j++) {
      // Truncation, not floor: the entry point is on or inside the box
      // up to rounding, and the clamp catches a hair below zero either
      // way.
      cell_[j] = std::clamp(int(pEnter[j]), 0, size_[j] - 1);
      if (cellDir[j] > 0.0f) {
        step_[j] = 1;
        tNext_[j] = (float(cell_[j] + 1) - cellOrg[j]) * invDir[j];
        tStep_[j] = invDir[j];
      } else if (cellDir[j] < 0.0f) {
        step_[j] = -1;
        tNext_[j] = (float(cell_[j]) - cellOrg[j]) * invDir[j];
        tStep_[j] = -invDir[j];
      } else {
        step_[j] = 0;
        tNext_[j] = INF;
        tStep_[j] = INF;
      }
    }
    index_ = cell_.x + size_.x * (cell_.y + size_.y * cell_.z);
    stride_ = int3(step_.x, step_.y * size_.x, step_.z * size_.x * size_.y);
  }

  /// The next span, or false when the segment is exhausted.
  [[nodiscard]] SMDL_ALWAYS_INLINE bool next(MajorantSpan &span) noexcept {
    // The stretch before the grid box, at the global scale. With no grid
    // this interval is empty and the whole segment falls to the last
    // test below.
    if (SMDL_UNLIKELY(tCurr_ < tEnter_)) {
      span = {tCurr_, tEnter_, 1.0f, 0.0f};
      tCurr_ = tEnter_;
      return true;
    }
    // One span per majorant cell.
    while (tCurr_ < tLeave_) {
      const int j{tNext_[0] < tNext_[1] ? (tNext_[0] < tNext_[2] ? 0 : 2)
                                        : (tNext_[1] < tNext_[2] ? 1 : 2)};
      const float2 bounds{SMDL_LIKELY(!isOutside_) ? table_[index_]
                                                   : float2(background_)};
      const float tCurr{tCurr_};
      const float tNext{std::min(tNext_[j], tLeave_)};
      tCurr_ = tNext, tNext_[j] += tStep_[j];
      index_ += stride_[j];
      cell_[j] += step_[j];
      // Steps are monotone per axis, so a cell that leaves the box never
      // comes back: one unsigned compare on the stepped axis is the whole
      // inside test.
      isOutside_ |= unsigned(cell_[j]) >= unsigned(size_[j]);
      if (shouldSkipEmptyCells_ && !(bounds.y > 0.0f)) continue;
      // Both bounds of the cell come from the one load: the dilated
      // minimum lower-bounds every trilinear value in the cell the same
      // way the dilated maximum upper-bounds it. The lower bound is
      // clamped into `[0, scale]` defensively, since a grid holding
      // negative values must not produce a negative control.
      const float scale{std::min(bounds.y * invMaxValue_, 1.0f)};
      const float scaleMin{std::clamp(bounds.x * invMaxValue_, 0.0f, scale)};
      span = {tCurr, tNext, scale, scaleMin};
      return true;
    }
    // The stretch after the grid box, at the global scale.
    if (SMDL_UNLIKELY(tCurr_ < tEnd_)) {
      span = {tCurr_, tEnd_, 1.0f, 0.0f};
      tCurr_ = tEnd_;
      return true;
    }
    return false;
  }

private:
  /// The grid's cell table, `x` fastest, null when there is no walk to
  /// make: either no grid, or a segment that misses the grid box. The
  /// empty `[tEnter_, tLeave_)` those leave keeps `next()` out of the
  /// cell loop, so this is only ever read inside it.
  const float2 *table_{};

  /// What the table answers outside the cell count.
  float background_{};

  float invMaxValue_{};
  float tEnd_{};
  float tEnter_{};
  float tLeave_{};
  float tCurr_{};

  /// The flat index of the current cell, and its change per step along
  /// each axis.
  int index_{};
  int3 stride_{};

  int3 cell_{};
  int3 size_{};
  int3 step_{};
  float3 tNext_{};
  float3 tStep_{};
  bool shouldSkipEmptyCells_{};
  bool isOutside_{};
};

/// \}

} // namespace smdl
