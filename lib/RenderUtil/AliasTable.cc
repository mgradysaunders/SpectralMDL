#include "smdl/RenderUtil/AliasTable.h"

#include <cmath>

namespace smdl {

AliasTable::AliasTable(Span<const float> weights) {
  if (weights.empty()) return;
  // Scale so that the mean share is one: an entry over one has excess to
  // give away, an entry under one is short by what it lacks, and the
  // pass below pairs them off until every entry is whole with at most
  // one donor. Accumulated in double, since the subtraction that pays a
  // short entry is where the build would otherwise lose its digits.
  double totalSum{};
  auto weightOf{
      [](float weight) { return std::fmax(static_cast<double>(weight), 0.0); }};
  for (float weight : weights) totalSum += weightOf(weight);
  if (!(totalSum > 0)) return;
  const size_t numEntries{weights.size()};
  std::vector<double> shares{};
  shares.reserve(numEntries);
  for (float weight : weights)
    shares.emplace_back(double(numEntries) * weightOf(weight) / totalSum);
  std::vector<uint32_t> under{}, over{};
  under.reserve(numEntries);
  over.reserve(numEntries);
  for (size_t i = 0; i < numEntries; i++)
    (shares[i] < 1.0 ? under : over).push_back(uint32_t(i));
  // An entry no pass ever pays keeps the whole of itself, which is the
  // threshold the default gives it.
  entries_.resize(numEntries);
  while (!under.empty() && !over.empty()) {
    const uint32_t shortOf{under.back()};
    const uint32_t donor{over.back()};
    under.pop_back();
    entries_[shortOf].threshold = float(shares[shortOf]);
    entries_[shortOf].alias = donor;
    shares[donor] -= 1.0 - shares[shortOf];
    if (shares[donor] < 1.0) {
      over.pop_back();
      under.push_back(donor);
    }
  }
}

} // namespace smdl
