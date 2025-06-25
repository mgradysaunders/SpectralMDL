/// \file
#pragma once

#include "smdl/common.h"

namespace smdl {

/// \addtogroup Support
/// \{

/// A discrete distribution.
class SMDL_EXPORT DiscreteDistribution final {
public:
  DiscreteDistribution() = default;

  DiscreteDistribution(const std::vector<double> &weights);

  template <typename Iterator>
  DiscreteDistribution(Iterator weight0, Iterator weightN)
      : DiscreteDistribution(std::vector<double>(weight0, weightN)) {}

public:
  /// The number of indexes.
  [[nodiscard]] int size() const { return int(cmfs.size()) - 1; }

  /// The index probability mass function.
  [[nodiscard]] float index_pmf(int i) const;

  /// The index cumulative mass function.
  [[nodiscard]] float index_cmf(int i) const;

  /// The index sample function.
  ///
  /// \param[inout] u
  /// The canonical random sample in `[0,1]`, which is overwritten after
  /// sampling to remap it back into `[0,1]` so it can be reused.
  ///
  [[nodiscard]] std::pair<int, float> index_sample(float &u) const;

private:
  /// The cumulative mass function values.
  std::vector<double> cmfs{};
};

/// \}

} // namespace smdl
