#include "smdl/Support/Strings.h"
#include "smdl/Support/Filesystem.h"

#include <algorithm>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iterator>
#include <vector>

namespace smdl {

void SpellQuoted::appendTo(std::string &result) const {
  result += '"';
  result += mStr;
  result += '"';
}

void SpellFilePath::appendTo(std::string &result) const {
  result += '"';
  result += bestPathForPrinting(std::string(mStr));
  result += '"';
}

void SpellLocation::appendTo(std::string &result) const {
  result += '[';
  if (mIsPath) {
    result += bestPathForPrinting(std::string(mName));
  } else {
    result += mName;
  }
  result += ':';
  result += std::to_string(mLineNo);
  if (mCharNo > 0) {
    result += ':';
    result += std::to_string(mCharNo);
  }
  result += ']';
}

// The numbers below go through `snprintf` rather than the `<charconv>`
// floating point overloads, which libstdc++ only grew in GCC 11 and
// which this build's floor does not assume.
void SpellFloat::appendTo(std::string &result) const {
  // NOLINTNEXTLINE
  char buffer[32]{};
  std::snprintf(buffer, sizeof(buffer), "%.*g", std::clamp(mDigits, 1, 17),
                mValue);
  result += buffer;
}

void SpellFixed::appendTo(std::string &result) const {
  // The decimal places that give the significant digits asked for: one
  // for every digit past the leading one, plus however many the leading
  // digit sits below the decimal point. A value of zero has no magnitude
  // to ask, and takes the digits as places.
  const int digits{std::clamp(mDigits, 1, 17)};
  int places{digits - 1};
  if (std::isfinite(mValue) && mValue != 0)
    places =
        std::max(places - int(std::floor(std::log10(std::abs(mValue)))), 0);
  // The places stay as the digits asked for, trailing zeros and all: a
  // value that happens to end in one is the reason a column lines up.
  // NOLINTNEXTLINE
  char buffer[512]{};
  std::snprintf(buffer, sizeof(buffer), "%.*f", std::min(places, 350), mValue);
  result += buffer;
}

void SpellExact::appendTo(std::string &result) const {
  // The shortest spelling that reads back as the same number. Shortest
  // by the string rather than by the digits, since `%g` turns
  // exponential once the exponent reaches the precision and `6e+02` is
  // no improvement on `600`.
  const int maxDigits{mIsFloat ? 9 : 17};
  // NOLINTNEXTLINE
  char shortest[64]{};
  for (int digits = 1; digits <= maxDigits; digits++) {
    // NOLINTNEXTLINE
    char buffer[64]{};
    std::snprintf(buffer, sizeof(buffer), "%.*g", digits, mValue);
    if (mIsFloat ? std::strtof(buffer, nullptr) != float(mValue)
                 : std::strtod(buffer, nullptr) != mValue)
      continue;
    if (!shortest[0] || std::strlen(buffer) < std::strlen(shortest))
      std::snprintf(shortest, sizeof(shortest), "%s", buffer);
  }
  // Nothing reads back for an infinity or a NaN, which have no decimal
  // spelling to be exact about; they are written as they are written.
  if (!shortest[0])
    std::snprintf(shortest, sizeof(shortest), "%.*g", maxDigits, mValue);
  result += shortest;
}

void SpellPercent::appendTo(std::string &result) const {
  // NOLINTNEXTLINE
  char buffer[64]{};
  std::snprintf(buffer, sizeof(buffer), "%.*f", std::clamp(mDecimals, 0, 9),
                100 * mValue);
  result += buffer;
  result += '%';
}

void SpellByteSize::appendTo(std::string &result) const {
  // NOLINTNEXTLINE
  constexpr const char *UNITS[]{"KiB", "MiB", "GiB", "TiB"};
  if (mCount < 1024) {
    result += std::to_string(mCount);
    result += " B";
    return;
  }
  double value{double(mCount) / 1024.0};
  size_t unit{0};
  for (; value >= 1024.0 && unit + 1 < std::size(UNITS); unit++)
    value /= 1024.0;
  // Three significant digits, except that a value that rounds to 1000 or
  // more is written whole rather than going exponential.
  // NOLINTNEXTLINE
  char buffer[32]{};
  std::snprintf(buffer, sizeof(buffer), value < 999.5 ? "%.3g" : "%.0f", value);
  result += buffer;
  result += ' ';
  result += UNITS[unit];
}

void SpellCounted::appendTo(std::string &result) const {
  result += std::to_string(mCount);
  result += ' ';
  if (mCount == 1) {
    result += mSingular;
  } else if (mPlural.empty()) {
    result += mSingular;
    result += 's';
  } else {
    result += mPlural;
  }
}

std::string_view suggestNearestName(std::string_view name,
                                    Span<const std::string_view> candidates) {
  auto tailOf{[](std::string_view str) {
    size_t i{str.rfind('_')};
    return i == std::string_view::npos ? std::string_view() : str.substr(i + 1);
  }};
  const size_t maxDistance{std::min<size_t>(1 + name.size() / 4, 4)};
  if (std::string_view tail{tailOf(name)}; !tail.empty()) {
    std::vector<std::string_view> sameKind{};
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
    std::vector<size_t> row(b.size() + 1);
    for (size_t j = 0; j <= b.size(); j++) row[j] = j;
    for (size_t i = 1; i <= a.size(); i++) {
      size_t diagonal{row[0]};
      row[0] = i;
      for (size_t j = 1; j <= b.size(); j++) {
        const size_t previous{row[j]};
        const size_t substitution{diagonal + (a[i - 1] == b[j - 1] ? 0 : 1)};
        row[j] = std::min({row[j] + 1, row[j - 1] + 1, substitution});
        diagonal = previous;
      }
    }
    return row[b.size()];
  }};
  std::string_view best{};
  size_t bestDistance{maxDistance + 1};
  for (const auto &candidate : candidates) {
    // The length difference alone bounds the distance from below.
    const size_t lengthDelta{name.size() > candidate.size()
                                 ? name.size() - candidate.size()
                                 : candidate.size() - name.size()};
    if (lengthDelta >= bestDistance) continue;
    if (const size_t d{distance(name, candidate)}; d < bestDistance) {
      best = candidate;
      bestDistance = d;
    }
  }
  return best;
}

} // namespace smdl
