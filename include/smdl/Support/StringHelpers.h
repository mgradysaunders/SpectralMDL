/// \file
#pragma once

#include <string>
#include <string_view>
#include <type_traits>
#include <utility>

#include "smdl/Export.h"
#include "smdl/Support/Span.h"

namespace smdl {

/// \addtogroup Support
/// \{

/// A quoted string for use with `concat`.
class SMDL_EXPORT quoted final {
public:
  constexpr quoted(std::string_view str) : str(str) {}

  void append_to(std::string &result);

public:
  std::string_view str{};
};

/// A quoted path string for use with `concat`.
class SMDL_EXPORT quoted_path final {
public:
  constexpr quoted_path(std::string_view str) : str(str) {}

  void append_to(std::string &result);

public:
  std::string_view str{};
};

namespace detail {

template <typename T, typename... Ts>
inline void do_concat(std::string &str, T &&value, Ts &&...values) {
  using DecayT = std::decay_t<T>;
  if constexpr (std::is_arithmetic_v<DecayT>) {
    str += std::to_string(value);
  } else if constexpr (std::is_same_v<DecayT, quoted> ||
                       std::is_same_v<DecayT, quoted_path>) {
    value.append_to(str);
  } else {
    str += value;
  }
  if constexpr (sizeof...(Ts) > 0)
    do_concat(str, std::forward<Ts>(values)...);
}

} // namespace detail

/// \name Functions (strings)
/// \{

/// Concatenate the given values into a string.
template <typename T, typename... Ts>
[[nodiscard]] inline auto concat(T &&value0, Ts &&...values) {
  if constexpr (sizeof...(Ts) == 0 &&
                std::is_same_v<std::decay_t<T>, std::string>) {
    return value0;
  } else if constexpr (sizeof...(Ts) == 0 &&
                       std::is_constructible_v<std::string_view,
                                               std::decay_t<T>>) {
    return std::string_view(value0);
  } else {
    std::string str{};
    str.reserve(128);
    detail::do_concat(str, std::forward<T>(value0),
                      std::forward<Ts>(values)...);
    return str;
  }
}

/// Is ASCII alphabetic character?
[[nodiscard]] constexpr bool is_alpha(char ch) noexcept {
  return (static_cast<int>('A' <= ch) & static_cast<int>(ch <= 'Z')) |
         (static_cast<int>('a' <= ch) & static_cast<int>(ch <= 'z'));
}

/// Is ASCII digit?
[[nodiscard]] constexpr bool is_digit(char ch) noexcept {
  return (static_cast<int>('0' <= ch) & static_cast<int>(ch <= '9'));
}

/// Is ASCII binary digit?
[[nodiscard]] constexpr bool is_digit_2(char ch) noexcept {
  return (static_cast<int>('0' <= ch) & static_cast<int>(ch <= '1'));
}

/// Is ASCII octal digit?
[[nodiscard]] constexpr bool is_digit_8(char ch) noexcept {
  return (static_cast<int>('0' <= ch) & static_cast<int>(ch <= '7'));
}

/// Is ASCII hexadecimal digit?
[[nodiscard]] constexpr bool is_digit_16(char ch) noexcept {
  return is_digit(ch) |
         (static_cast<int>('A' <= ch) & static_cast<int>(ch <= 'F')) |
         (static_cast<int>('a' <= ch) & static_cast<int>(ch <= 'f'));
}

/// Is ASCII alphabetic character, digit, or underscore?
[[nodiscard]] constexpr bool is_word(char ch) noexcept {
  return static_cast<int>(is_alpha(ch)) | static_cast<int>(is_digit(ch)) |
         static_cast<int>(ch == '_');
}

/// Is ASCII whitespace character?
[[nodiscard]] constexpr bool is_space(char ch) noexcept {
  return static_cast<int>(ch == ' ') | static_cast<int>(ch == '\t') |
         static_cast<int>(ch == '\n') | static_cast<int>(ch == '\r') |
         static_cast<int>(ch == '\v');
}

/// Convert ASCII octal character to runtime int.
[[nodiscard]] constexpr int oct_to_int(int ch) noexcept {
  if ('0' <= ch && ch <= '7')
    return ch - '0';
  return 0;
}

/// Convert ASCII hexadecimal character to runtime int.
[[nodiscard]] constexpr int hex_to_int(int ch) noexcept {
  if ('0' <= ch && ch <= '9')
    return ch - '0';
  if ('a' <= ch && ch <= 'f')
    return ch - 'a' + 10;
  if ('A' <= ch && ch <= 'F')
    return ch - 'A' + 10;
  return 0;
}

/// Determine if `str0` starts with `str1`.
[[nodiscard]] constexpr bool starts_with(std::string_view str0,
                                         std::string_view str1) noexcept {
  return str0.size() >= str1.size() && str0.substr(0, str1.size()) == str1;
}

/// Join the given string views by the given delimiter.
[[nodiscard]] inline std::string join(Span<std::string_view> strs,
                                      std::string_view delim) {
  std::string str{};
  str.reserve(128);
  for (size_t i = 0; i < strs.size(); i++) {
    str += strs[i];
    if (i + 1 < strs.size())
      str += delim;
  }
  return str;
}

/// \}

/// \}

} // namespace smdl
