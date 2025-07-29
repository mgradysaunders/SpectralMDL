/// \file
#pragma once

#include <random>
#include <vector>

#include "smdl/Export.h"

namespace smdl {

/// \addtogroup Support
/// \{

/// A discrete distribution.
class SMDL_EXPORT DiscreteDistribution final {
public:
  DiscreteDistribution() = default;

  DiscreteDistribution(const std::vector<double> &weights);

public:
  /// The number of indexes.
  [[nodiscard]] int size() const { return int(cmfs.size()) - 1; }

  /// The index probability mass function (PMF).
  [[nodiscard]] float index_pmf(int i) const;

  /// The index cumulative mass function (CMF).
  [[nodiscard]] float index_cmf(int i) const;

  /// The index sample function.
  ///
  /// \param[inout] u
  /// The canonical random sample in `[0,1]`, which is overwritten after
  /// sampling to remap it back into `[0,1]` so it can be reused.
  ///
  /// \returns
  /// The sampled index and the associated PMF.
  ///
  [[nodiscard]] std::pair<int, float> index_sample(float &u) const;

  template <typename G> [[nodiscard]] int operator()(G &gen) const {
    float u{std::generate_canonical<float, 32>(gen)};
    return index_sample(u).first;
  }

private:
  /// The cumulative mass function values.
  std::vector<double> cmfs{};
};

/// \}

} // namespace smdl
