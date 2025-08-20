#include "smdl/Support/DiscreteDistribution.h"

namespace smdl {

DiscreteDistribution::DiscreteDistribution(const std::vector<double> &weights) {
  cmfs.reserve(weights.size() + 1);
  cmfs.emplace_back(0.0);
  for (auto &weight : weights)
    cmfs.emplace_back(cmfs.back() + std::fmax(weight, 0.0));
  for (auto &cmf : cmfs)
    cmf /= cmfs.back();
}

std::pair<int, float> DiscreteDistribution::index_sample(float u,
                                                         float *uRemap) const {
  if (cmfs.size() < 2)
    return {0, 1.0f};
  auto itr{std::lower_bound(cmfs.begin(), cmfs.end(), static_cast<double>(u))};
  if (itr == cmfs.begin())
    ++itr;
  if (itr == cmfs.end())
    --itr;
  --itr;
  auto i{int(itr - cmfs.begin())};
  auto cmf0{*itr++};
  auto cmf1{*itr};
  auto pmf{cmf1 - cmf0};
  if (uRemap) {
    *uRemap = static_cast<float>(static_cast<double>(u) - cmf0) / pmf;
    *uRemap = std::fmax(*uRemap, std::numeric_limits<float>::denorm_min());
    *uRemap = std::fmin(*uRemap, 1 - std::numeric_limits<float>::epsilon() / 2);
  }
  return {i, static_cast<float>(pmf)};
}

} // namespace smdl
