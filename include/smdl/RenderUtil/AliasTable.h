/// \file
#pragma once

#include <algorithm>
#include <cstdint>
#include <vector>

#include "smdl/Export.h"
#include "smdl/Support/Macros.h"
#include "smdl/Support/Span.h"
#include "smdl/Support/VectorMath.h"

namespace smdl {

/// \addtogroup renderutil
/// \{

/// A discrete distribution sampled in constant time (Walker, "New Fast
/// Method for Generating Discrete Random Numbers with Arbitrary Frequency
/// Distributions," Electronics Letters 10(8) 1974; built by Vose's
/// linear-time algorithm).
///
/// The draw is exactly in proportion to the weights. A caller that
/// weighs a draw against a per-entry quantity needs that exactness: a
/// distribution over triangle areas, divided by the area of the face it
/// draws, is uniform over the surface exactly, and the two cancel rather
/// than nearly cancel.
///
/// Two words an entry, against the one `Distribution1D` spends, bought
/// with the binary search that table costs on every draw.
class SMDL_EXPORT AliasTable final {
public:
  AliasTable() = default;

  /// Build over `weights`, in proportion to them. A negative or NaN
  /// weight counts as zero. An empty span, or one summing to zero,
  /// leaves the table empty.
  explicit AliasTable(Span<const float> weights);

  [[nodiscard]] bool empty() const noexcept { return entries_.empty(); }

  /// The number of indexes.
  [[nodiscard]] int size() const noexcept { return int(entries_.size()); }

  /// Draw an index in `[0, size())` from the sample `xi` in `(0,1)`.
  ///
  /// The sample is spent twice, on the entry and on the choice between
  /// that entry and its alias. For `xi` uniform the scaled sample's
  /// integer and fractional parts are independent, so this is exact and
  /// costs one dimension where two would do.
  [[nodiscard]] int indexDraw(float xi) const noexcept {
    SMDL_SANITY_CHECK(!empty());
    const float scaled{float(size()) * std::clamp(xi, 0.0f, ALMOST_ONE)};
    const int entry{std::clamp(int(scaled), 0, size() - 1)};
    const Entry &e{entries_[entry]};
    return scaled - float(entry) < e.threshold ? entry : int(e.alias);
  }

private:
  /// One entry: the share of it that stays with the entry itself, and
  /// the index the rest of it is given away to.
  struct Entry final {
    float threshold{1.0f};
    std::uint32_t alias{};
  };

  std::vector<Entry> entries_{};
};

/// \}

} // namespace smdl
