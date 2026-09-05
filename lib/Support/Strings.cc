#include "smdl/Support/Strings.h"
#include "smdl/Support/Filesystem.h"

#include <algorithm>
#include <cstdio>
#include <vector>

namespace smdl {

void Quoted::appendTo(std::string &result) const {
  result += '\'';
  result += str;
  result += '\'';
}

void QuotedPath::appendTo(std::string &result) const {
  result += '\'';
  result += bestPathForPrinting(std::string(str));
  result += '\'';
}

// Both of these go through `snprintf` rather than the `<charconv>`
// floating point overloads, which libstdc++ only grew in GCC 11 and
// which this build's floor does not assume.
void Precise::appendTo(std::string &result) const {
  // Nine significant digits round-trip every float, and seventeen every
  // double; nine is the right number here because every caller is
  // writing a float that came out of a `float` field.
  // NOLINTNEXTLINE
  char buffer[32]{};
  std::snprintf(buffer, sizeof(buffer), "%.9g", value);
  result += buffer;
}

void Brief::appendTo(std::string &result) const {
  // NOLINTNEXTLINE
  char buffer[32]{};
  std::snprintf(buffer, sizeof(buffer), "%.*g", std::clamp(digits, 1, 17),
                value);
  result += buffer;
}

std::string_view suggestNearestName(std::string_view name,
                                    Span<const std::string_view> candidates) {
  auto tailOf{[](std::string_view str) {
    auto i{str.rfind('_')};
    return i == std::string_view::npos ? std::string_view() : str.substr(i + 1);
  }};
  const auto maxDistance{std::min<size_t>(1 + name.size() / 4, 4)};
  if (auto tail{tailOf(name)}; !tail.empty()) {
    auto sameKind{std::vector<std::string_view>{}};
    for (auto candidate : candidates)
      if (tailOf(candidate) == tail) sameKind.push_back(candidate);
    if (!sameKind.empty()) return suggestNearest(name, sameKind, maxDistance);
  }
  return suggestNearest(name, candidates, maxDistance);
}

std::string_view suggestNearest(std::string_view name,
                                Span<const std::string_view> candidates,
                                size_t maxDistance) {
  // Levenshtein over two rows. The candidate list is a keyword table or a
  // set of declared names, so everything here is tiny; clarity beats
  // cleverness.
  auto distance{[](std::string_view a, std::string_view b) {
    auto row{std::vector<size_t>(b.size() + 1)};
    for (size_t j = 0; j <= b.size(); j++) row[j] = j;
    for (size_t i = 1; i <= a.size(); i++) {
      auto diagonal{row[0]};
      row[0] = i;
      for (size_t j = 1; j <= b.size(); j++) {
        const auto previous{row[j]};
        const auto substitution{diagonal + (a[i - 1] == b[j - 1] ? 0 : 1)};
        row[j] = std::min({row[j] + 1, row[j - 1] + 1, substitution});
        diagonal = previous;
      }
    }
    return row[b.size()];
  }};
  auto best{std::string_view()};
  auto bestDistance{maxDistance + 1};
  for (const auto &candidate : candidates) {
    // The length difference alone bounds the distance from below.
    const auto lengthDelta{name.size() > candidate.size()
                               ? name.size() - candidate.size()
                               : candidate.size() - name.size()};
    if (lengthDelta >= bestDistance) continue;
    if (const auto d{distance(name, candidate)}; d < bestDistance) {
      best = candidate;
      bestDistance = d;
    }
  }
  return best;
}

} // namespace smdl
