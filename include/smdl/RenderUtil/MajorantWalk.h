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
      : mInvMaxValue(invMaxValue), mTEnd(tEnd),
        mShouldSkipEmptyCells(shouldSkipEmptyCells) {
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
    mTable = grid->getMajorantTable().data();
    mBackground = grid->getBackground();
    mSize = grid->getMajorantCount();
    mTEnter = tEnter;
    mTLeave = tLeave;
    const float3 pEnter{cellOrg + tEnter * cellDir};
    for (int j = 0; j < 3; j++) {
      // Truncation, not floor: the entry point is on or inside the box
      // up to rounding, and the clamp catches a hair below zero either
      // way.
      mCell[j] = std::clamp(int(pEnter[j]), 0, mSize[j] - 1);
      if (cellDir[j] > 0.0f) {
        mStep[j] = 1;
        mTNext[j] = (float(mCell[j] + 1) - cellOrg[j]) * invDir[j];
        mTStep[j] = invDir[j];
      } else if (cellDir[j] < 0.0f) {
        mStep[j] = -1;
        mTNext[j] = (float(mCell[j]) - cellOrg[j]) * invDir[j];
        mTStep[j] = -invDir[j];
      } else {
        mStep[j] = 0;
        mTNext[j] = INF;
        mTStep[j] = INF;
      }
    }
    mIndex = mCell.x + mSize.x * (mCell.y + mSize.y * mCell.z);
    mStride = int3(mStep.x, mStep.y * mSize.x, mStep.z * mSize.x * mSize.y);
  }

  /// The next span, or false when the segment is exhausted.
  [[nodiscard]] SMDL_ALWAYS_INLINE bool next(MajorantSpan &span) noexcept {
    // The stretch before the grid box, at the global scale. With no grid
    // this interval is empty and the whole segment falls to the last
    // test below.
    if (SMDL_UNLIKELY(mTCurr < mTEnter)) {
      span = {mTCurr, mTEnter, 1.0f, 0.0f};
      mTCurr = mTEnter;
      return true;
    }
    // One span per majorant cell.
    while (mTCurr < mTLeave) {
      const int j{mTNext[0] < mTNext[1] ? (mTNext[0] < mTNext[2] ? 0 : 2)
                                        : (mTNext[1] < mTNext[2] ? 1 : 2)};
      const float2 bounds{SMDL_LIKELY(!mIsOutside) ? mTable[mIndex]
                                                   : float2(mBackground)};
      const float tCurr{mTCurr};
      const float tNext{std::min(mTNext[j], mTLeave)};
      mTCurr = tNext, mTNext[j] += mTStep[j];
      mIndex += mStride[j];
      mCell[j] += mStep[j];
      // Steps are monotone per axis, so a cell that leaves the box never
      // comes back: one unsigned compare on the stepped axis is the whole
      // inside test.
      mIsOutside |= unsigned(mCell[j]) >= unsigned(mSize[j]);
      if (mShouldSkipEmptyCells && !(bounds.y > 0.0f)) continue;
      // Both bounds of the cell come from the one load: the dilated
      // minimum lower-bounds every trilinear value in the cell the same
      // way the dilated maximum upper-bounds it. The lower bound is
      // clamped into `[0, scale]` defensively, since a grid holding
      // negative values must not produce a negative control.
      const float scale{std::min(bounds.y * mInvMaxValue, 1.0f)};
      const float scaleMin{std::clamp(bounds.x * mInvMaxValue, 0.0f, scale)};
      span = {tCurr, tNext, scale, scaleMin};
      return true;
    }
    // The stretch after the grid box, at the global scale.
    if (SMDL_UNLIKELY(mTCurr < mTEnd)) {
      span = {mTCurr, mTEnd, 1.0f, 0.0f};
      mTCurr = mTEnd;
      return true;
    }
    return false;
  }

private:
  /// The grid's cell table, `x` fastest, null when there is no walk to
  /// make: either no grid, or a segment that misses the grid box. The
  /// empty `[mTEnter, mTLeave)` those leave keeps `next()` out of the
  /// cell loop, so this is only ever read inside it.
  const float2 *mTable{};

  /// What the table answers outside the cell count.
  float mBackground{};

  float mInvMaxValue{};
  float mTEnd{};
  float mTEnter{};
  float mTLeave{};
  float mTCurr{};

  /// The flat index of the current cell, and its change per step along
  /// each axis.
  int mIndex{};
  int3 mStride{};

  int3 mCell{};
  int3 mSize{};
  int3 mStep{};
  float3 mTNext{};
  float3 mTStep{};
  bool mShouldSkipEmptyCells{};
  bool mIsOutside{};
};

/// \}

} // namespace smdl
