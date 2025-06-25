#include "smdl/Distribution.h"

namespace smdl {

DiscreteDistribution::DiscreteDistribution(const std::vector<double> &weights) {
  cmfs.reserve(weights.size() + 1);
  cmfs.emplace_back(0.0);
  for (auto &weight : weights)
    cmfs.emplace_back(cmfs.back() + std::fmax(weight, 0.0));
  for (auto &cmf : cmfs)
    cmf /= cmfs.back();
}

float DiscreteDistribution::index_pmf(int i) const {
  if (0 <= i && i < size())
    return static_cast<float>(cmfs[i + 1] - cmfs[i]);
  return 0.0f;
}

float DiscreteDistribution::index_cmf(int i) const {
  if (0 <= i && i < size())
    return static_cast<float>(cmfs[i]);
  return i < 0 ? 0.0f : 1.0f;
}

std::pair<int, float> DiscreteDistribution::index_sample(float &u) const {
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
  u = static_cast<float>(static_cast<double>(u) - cmf0) / pmf;
  u = std::fmax(u, 0.0f);
  u = std::fmin(u, 1.0f);
  return {i, static_cast<float>(pmf)};
}

} // namespace smdl
