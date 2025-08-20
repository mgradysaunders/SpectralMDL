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
  [[nodiscard]] float index_pmf(int i) const {
    if (0 <= i && i < size())
      return static_cast<float>(cmfs[i + 1] - cmfs[i]);
    return 0.0f;
  }

  /// The index cumulative mass function (CMF).
  [[nodiscard]] float index_cmf(int i) const {
    if (0 <= i && i < size())
      return static_cast<float>(cmfs[i]);
    return i < 0 ? 0.0f : 1.0f;
  }

  /// The index sample function.
  ///
  /// \param[in] u
  /// The random sample in \f$ (0,1) \f$.
  ///
  /// \param[out] uRemap
  /// If non-null, receives the random sample remapped back into \f$ (0,1) \f$
  /// so it can be reused.
  ///
  /// \returns
  /// The sampled index and the associated PMF.
  ///
  [[nodiscard]] std::pair<int, float> index_sample(float u,
                                                   float *uRemap = {}) const;

  template <typename G> [[nodiscard]] int operator()(G &gen) const {
    return index_sample(std::generate_canonical<float, 32>(gen)).first;
  }

private:
  /// The cumulative mass function values.
  std::vector<double> cmfs{};
};

/// \}

} // namespace smdl
