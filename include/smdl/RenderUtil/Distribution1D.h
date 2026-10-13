/// \file
#pragma once

#include <cstdint>
#include <vector>

#include "smdl/Export.h"
#include "smdl/Support/Span.h"

namespace smdl {

/// \addtogroup renderutil
/// \{

/// A data-driven distribution in 1 dimension.
class SMDL_EXPORT Distribution1D final {
public:
  /// Default constructor.
  Distribution1D() = default;

  /// Constructor.
  Distribution1D(Span<const float> values);

public:
  /// Clear.
  void clear() noexcept {
    totalSum_ = 0;
    cmfs_.clear();
  }

  /// The number of indexes.
  [[nodiscard]] int size() const noexcept { return int(cmfs_.size()) - 1; }

  /// The index probability mass function (PMF). Inline, because a
  /// light sampler asks for it per sample.
  [[nodiscard]] float indexPMF(int i) const noexcept {
    if (0 <= i && i < size())
      return float(INV_CMF_SCALE * double(cmfs_[i + 1] - cmfs_[i]));
    return 0.0f;
  }

  /// The index cumulative mass function (CMF).
  [[nodiscard]] float indexCMF(int i) const noexcept {
    if (0 <= i && i < size()) return float(INV_CMF_SCALE * double(cmfs_[i]));
    return i < 0 ? 0.0f : 1.0f;
  }

  /// The index sampling routine. Never an index with no probability,
  /// even at the bottom of the range, so long as some index has one.
  ///
  /// \param[in]  xi       The random sample \f$ \xi \in (0,1) \f$.
  /// \param[out] xiRemap  If non-null, receives the remapped random sample.
  /// \param[out] pmf      If non-null, receives the associated PMF.
  ///
  [[nodiscard]] int indexDraw(float xi, float *xiRemap = {},
                              float *pmf = {}) const noexcept;

  /// The unnormalized sum.
  [[nodiscard]] float unnormalizedSum() const noexcept {
    return static_cast<float>(totalSum_);
  }

private:
  double totalSum_{};

  /// The cumulative mass function over the unit interval, as 32-bit
  /// fixed point: half the size of a table of `double`, and unlike a
  /// table of `float` the difference of two entries stays exact, which
  /// is what `indexPMF()` reads.
  std::vector<std::uint32_t> cmfs_{};

  /// The unit of `cmfs_`, `2^-32`.
  static constexpr double INV_CMF_SCALE = 1.0 / 4294967296.0;
};

/// \}

} // namespace smdl
