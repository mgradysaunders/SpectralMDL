#pragma once

#include <cstddef>
#include <string_view>

namespace smdl {

/// The length of the block comment that `src` starts with, from its `/*`
/// through the `*/` that closes it, or `std::string_view::npos` if nothing
/// closes it.
///
/// Block comments nest, as of MDL 1.8, so the `*/` that closes one is the
/// first that balances every `/*` before it, and a `//` inside one starts
/// nothing. The parser, the formatter, and the documentation extractor all
/// step over comments, and have to agree on where one ends.
[[nodiscard]] inline size_t blockCommentLength(std::string_view src) noexcept {
  size_t depth{};
  for (size_t i{}; i + 1 < src.size();) {
    if (src[i] == '/' && src[i + 1] == '*') {
      depth++;
      i += 2;
    } else if (src[i] == '*' && src[i + 1] == '/') {
      i += 2;
      if (--depth == 0) return i;
    } else {
      i++;
    }
  }
  return std::string_view::npos;
}

} // namespace smdl
