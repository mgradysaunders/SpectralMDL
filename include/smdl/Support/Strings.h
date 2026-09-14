/// \file
#pragma once

#include <cstdint>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>

#include "smdl/Export.h"
#include "smdl/Support/Span.h"

namespace smdl {

/// \addtogroup support
/// \{

/// Is ASCII alphabetic character?
[[nodiscard]] constexpr bool isAlpha(char ch) noexcept {
  return (static_cast<int>('A' <= ch) & static_cast<int>(ch <= 'Z')) |
         (static_cast<int>('a' <= ch) & static_cast<int>(ch <= 'z'));
}

/// Is ASCII digit?
[[nodiscard]] constexpr bool isDigit(char ch) noexcept {
  return (static_cast<int>('0' <= ch) & static_cast<int>(ch <= '9'));
}

/// Is ASCII binary digit?
[[nodiscard]] constexpr bool isDigit2(char ch) noexcept {
  return (static_cast<int>('0' <= ch) & static_cast<int>(ch <= '1'));
}

/// Is ASCII octal digit?
[[nodiscard]] constexpr bool isDigit8(char ch) noexcept {
  return (static_cast<int>('0' <= ch) & static_cast<int>(ch <= '7'));
}

/// Is ASCII hexadecimal digit?
[[nodiscard]] constexpr bool isDigit16(char ch) noexcept {
  return isDigit(ch) |
         (static_cast<int>('A' <= ch) & static_cast<int>(ch <= 'F')) |
         (static_cast<int>('a' <= ch) & static_cast<int>(ch <= 'f'));
}

/// Is ASCII alphabetic character, digit, or underscore?
[[nodiscard]] constexpr bool isWord(char ch) noexcept {
  return static_cast<int>(isAlpha(ch)) | static_cast<int>(isDigit(ch)) |
         static_cast<int>(ch == '_');
}

/// Is ASCII whitespace character?
[[nodiscard]] constexpr bool isSpace(char ch) noexcept {
  return static_cast<int>(ch == ' ') | static_cast<int>(ch == '\t') |
         static_cast<int>(ch == '\n') | static_cast<int>(ch == '\r') |
         static_cast<int>(ch == '\v');
}

/// Convert ASCII octal character to runtime int.
[[nodiscard]] constexpr int octToInt(int ch) noexcept {
  if ('0' <= ch && ch <= '7') return ch - '0';
  return 0;
}

/// Convert ASCII hexadecimal character to runtime int.
[[nodiscard]] constexpr int hexToInt(int ch) noexcept {
  if ('0' <= ch && ch <= '9') return ch - '0';
  if ('a' <= ch && ch <= 'f') return ch - 'a' + 10;
  if ('A' <= ch && ch <= 'F') return ch - 'A' + 10;
  return 0;
}

/// The spellings: how one value is written into a message, a diagnostic,
/// or a text file, each a small class that `concat` recognizes by its
/// `appendTo()` and writes in one place, so that the same kind of value
/// reads the same way whoever wrote the line.
///
/// Three of them spell a number, and which one to reach for is the whole
/// question:
///
/// * `SpellFloat` is what a message wants, and is what `concat` gives a
///   bare `float` or `double` anyway, so a message rarely names it.
/// * `SpellFixed` is `SpellFloat` with the exponent refused, for a table
///   or a column that must not sprout one mid-row.
/// * `SpellExact` is for a number something will read back: a header
///   field, a re-parsable dump, a JSON number, a key compared as text.
inline namespace spelling {

/// A double-quoted string for use with `concat`. Everything a message
/// quotes goes through this or `SpellFilePath`, so that the quoting does
/// not vary with who wrote the message.
class SMDL_EXPORT SpellQuoted final {
public:
  constexpr SpellQuoted(std::string_view str) : mStr(str) {}
  void appendTo(std::string &result) const;

private:
  std::string_view mStr{};
};

/// A quoted path string for use with `concat`. This quotes like
/// `SpellQuoted` and differs only in shortening the path first, as
/// `bestPathForPrinting()` shortens it.
class SMDL_EXPORT SpellFilePath final {
public:
  constexpr SpellFilePath(std::string_view str) : mStr(str) {}
  void appendTo(std::string &result) const;

private:
  std::string_view mStr{};
};

/// A location in a source for use with `concat`, written the one way every
/// diagnostic writes one: `[file:line:col]`, or `[file:line]` when the
/// column is zero, meaning unknown. The file shortens as `SpellFilePath`
/// shortens it, unless `isPath` says the name is not a path at all, like
/// the `<builtin ::df>` of a module with no file.
class SMDL_EXPORT SpellLocation final {
public:
  constexpr SpellLocation(std::string_view name, uint32_t lineNo,
                          uint32_t charNo = 0, bool isPath = true)
      : mName(name), mLineNo(lineNo), mCharNo(charNo), mIsPath(isPath) {}
  void appendTo(std::string &result) const;

private:
  std::string_view mName{};
  uint32_t mLineNo{};
  uint32_t mCharNo{};
  bool mIsPath{true};
};

/// A number written to a few significant digits, for use with `concat`:
/// what a log line or a diagnostic wants, and what `concat` spells a
/// bare `float` or `double` as.
///
/// The digits are SIGNIFICANT digits, not decimal places, and six is the
/// default because that is where the ordinary run of scene-scale numbers
/// stops going exponential: `1000` stays `1000` and only past a million
/// does a magnitude earn an exponent. Ask for fewer where the quantity
/// is one a reader skims rather than reads, such as a duration.
class SMDL_EXPORT SpellFloat final {
public:
  constexpr SpellFloat(double value, int digits = 6)
      : mValue(value), mDigits(digits) {}
  void appendTo(std::string &result) const;

private:
  double mValue{};
  int mDigits{6}; ///< The significant digits, clamped to `[1, 17]`.
};

/// A number written to a few significant digits and never as an
/// exponent, for use with `concat`.
///
/// The digits mean what they mean in `SpellFloat`, and the decimal
/// places follow from the magnitude, so nine digits of `1.23456789e-12`
/// is the twenty places of `0.00000000000123456789` rather than an
/// exponent. Unlike `SpellFloat`, the places stay put where a value
/// ends in zeros, three digits of `2` being `2.00`: this is for a
/// column a reader scans down, where an exponential row is harder to
/// read than a long one and a ragged one is harder than either.
class SMDL_EXPORT SpellFixed final {
public:
  constexpr SpellFixed(double value, int digits = 9)
      : mValue(value), mDigits(digits) {}
  void appendTo(std::string &result) const;

private:
  double mValue{};
  int mDigits{9}; ///< The significant digits, clamped to `[1, 17]`.
};

/// A number written so that it reads back as itself, for use with
/// `concat`: a header field, a dump something parses again, a JSON
/// number, a key compared as text.
///
/// The spelling is the SHORTEST one that reads back exactly, by the
/// length of the string rather than the count of digits, so `600` wins
/// over `6e+02`. A `float` is spelled as the float it is: the same value
/// widened to `double` and written to nine digits reads `0.0120000001`
/// where the float alone reads `0.012`, which is why the two overloads
/// are separate.
class SMDL_EXPORT SpellExact final {
public:
  constexpr SpellExact(float value) : mValue(value), mIsFloat(true) {}
  constexpr SpellExact(double value) : mValue(value) {}
  void appendTo(std::string &result) const;

private:
  double mValue{};
  bool mIsFloat{};
};

/// A fraction written as a percent for use with `concat`, as in `12.3%`:
/// one takes the whole, and a ratio past one or below zero is written as
/// what it is, since a corner brighter than the middle is a real
/// measurement.
class SMDL_EXPORT SpellPercent final {
public:
  constexpr SpellPercent(double value, int decimals = 1)
      : mValue(value), mDecimals(decimals) {}
  void appendTo(std::string &result) const;

private:
  double mValue{};
  int mDecimals{1}; ///< The decimal places, clamped to `[0, 9]`.
};

/// A size in bytes, for use with `concat`: whole bytes below a KiB, and
/// otherwise the largest binary unit that keeps the number at least one,
/// to three significant digits, as in `2.67 MiB`.
class SMDL_EXPORT SpellByteSize final {
public:
  constexpr SpellByteSize(size_t count) : mCount(count) {}
  void appendTo(std::string &result) const;

private:
  size_t mCount{};
};

/// A count and the noun it counts, for use with `concat`: the noun is
/// singular for exactly one and plural otherwise, as in `1 image` and
/// `0 images`.
class SMDL_EXPORT SpellCounted final {
public:
  constexpr SpellCounted(size_t count, std::string_view singular,
                         std::string_view plural = {})
      : mCount(count), mSingular(singular), mPlural(plural) {}
  void appendTo(std::string &result) const;

private:
  size_t mCount{};
  std::string_view mSingular{};
  std::string_view mPlural{};
};

} // namespace spelling

#if !SMDL_DOXYGEN
namespace detail {

/// Does `T` spell itself, so that `concat` writes it through
/// `appendTo()` rather than appending it? Every spelling is recognized
/// this way rather than by name, so that a program may add one of its
/// own and `concat` takes it.
template <typename T, typename = void> struct HasAppendTo : std::false_type {};

template <typename T>
struct HasAppendTo<T, std::void_t<decltype(std::declval<const T &>().appendTo(
                          std::declval<std::string &>()))>> : std::true_type {};

template <typename T, typename... Ts>
inline void doConcat(std::string &str, T &&value, Ts &&...values) {
  using DecayT = std::decay_t<T>;
  if constexpr (HasAppendTo<DecayT>::value) {
    value.appendTo(str);
  } else if constexpr (std::is_floating_point_v<DecayT>) {
    // One rule for a number in a message: the significant digits
    // `SpellFloat` writes, rather than the six decimal places
    // `std::to_string` writes whatever the magnitude, which spells a
    // scene height `0.000000` and a plane extent `1000.000000`.
    SpellFloat(value).appendTo(str);
  } else if constexpr (std::is_arithmetic_v<DecayT>) {
    str += std::to_string(value);
  } else {
    str += value;
  }
  if constexpr (sizeof...(Ts) > 0) doConcat(str, std::forward<Ts>(values)...);
}

} // namespace detail
#endif // #if !SMDL_DOXYGEN

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
    detail::doConcat(str, std::forward<T>(value0), std::forward<Ts>(values)...);
    return str;
  }
}

/// Determine if `str0` starts with `str1`.
[[nodiscard]] constexpr bool startsWith(std::string_view str0,
                                        std::string_view str1) noexcept {
  return str0.size() >= str1.size() && str0.substr(0, str1.size()) == str1;
}

/// Join the given string views by the given delimiter.
[[nodiscard]] inline std::string join(Span<const std::string_view> strs,
                                      std::string_view delim) {
  std::string str{};
  str.reserve(128);
  for (size_t i = 0; i < strs.size(); i++) {
    str += strs[i];
    if (i + 1 < strs.size()) str += delim;
  }
  return str;
}

/// The did-you-mean helper: the nearest candidate to `name` within
/// `maxDistance` edits (Levenshtein), or empty if none is close enough.
/// Ties keep the earliest candidate. The threshold is the caller's policy,
/// because what counts as a plausible typo depends on how long the names
/// in the candidate list tend to be.
[[nodiscard]] SMDL_EXPORT std::string_view
suggestNearest(std::string_view name, Span<const std::string_view> candidates,
               size_t maxDistance = 2);

/// The did-you-mean helper with the compiler's own policy applied: the
/// tolerance scales with the length of what was typed, because one edit in
/// `abs` is a much bigger relative error than one edit in
/// `loadBSDFMeasurement`; and a suggestion must keep the trailing `_word`
/// of the typed name whenever any candidate has one to match, which keeps a
/// missing `diffuse_bsdf` from being answered with the unrelated
/// `diffuse_edf`.
[[nodiscard]] SMDL_EXPORT std::string_view
suggestNearestName(std::string_view name,
                   Span<const std::string_view> candidates);

/// \}

/// \}

} // namespace smdl
