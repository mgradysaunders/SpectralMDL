/// \file
#pragma once

#include <algorithm>
#include <array>
#include <cstddef>
#include <initializer_list>
#include <vector>

namespace smdl {

/// \addtogroup Support
/// \{

/// A span.
///
/// We target C++17, which does not have `std::span` yet. We also
/// do not want to include any LLVM headers from our headers, so
/// we also cannot use `llvm::ArrayRef`.
///
template <typename T> class Span final {
public:
  constexpr Span() = default;

  /// Construct from single element.
  constexpr Span(const T &elem) : first(&elem), count(1) {}

  /// Construct from pointer to first element and element count.
  constexpr Span(const T *first, size_t count) : first(first), count(count) {}

  /// Construct from `std::initializer_list`.
  constexpr Span(std::initializer_list<T> elems)
      : first(elems.begin()), count(elems.size()) {}

  /// Construct from `std::array`.
  template <size_t N>
  constexpr Span(const std::array<T, N> &elems)
      : first(elems.data()), count(elems.size()) {}

  /// Construct from `std::vector`.
  Span(const std::vector<T> &elems)
      : first(elems.data()), count(elems.size()) {}

  /// Is empty?
  [[nodiscard]] constexpr bool empty() const { return count == 0; }

  /// Get the size.
  [[nodiscard]] constexpr size_t size() const { return count; }

  /// Get the begin iterator.
  [[nodiscard]] constexpr auto begin() const { return first; }

  /// Get the end iterator.
  [[nodiscard]] constexpr auto end() const { return first + count; }

  /// Get the data pointer.
  [[nodiscard]] constexpr const T *data() const { return first; }

  /// Get the front element.
  [[nodiscard]] constexpr const T &front() const { return first[0]; }

  /// Get the back element.
  [[nodiscard]] constexpr const T &back() const { return first[count - 1]; }

  /// Drop the front element while the given predicate is true.
  template <typename Pred>
  [[nodiscard]] constexpr Span<T> drop_front_while(Pred &&pred) const {
    size_t i = 0;
    size_t n = count;
    while (i < count && pred(first[i])) {
      i++;
      n--;
    }
    return subspan(i, n);
  }

  /// Drop the front element.
  [[nodiscard]] constexpr Span<T> drop_front() const {
    return subspan(1, count - 1);
  }

  /// Drop the back element.
  [[nodiscard]] constexpr Span<T> drop_back() const {
    return subspan(0, count - 1);
  }

  /// Get subspan.
  [[nodiscard]] constexpr Span<T> subspan(size_t i,
                                          size_t n = size_t(-1)) const {
    return Span(first + i, std::min(count - i, n));
  }

  /// Contains the given value?
  [[nodiscard]] constexpr bool contains(const T &value) const {
    return std::find(begin(), end(), value) != end();
  }

  /// Starts with the given sequence of values?
  [[nodiscard]] constexpr bool starts_with(Span<T> other) const {
    if (count < other.count)
      return false;
    for (size_t i = 0; i < other.count; i++)
      if (operator[](i) != other[i])
        return false;
    return true;
  }

  /// Get element by index.
  [[nodiscard]] constexpr const T &operator[](size_t i) const {
    return first[i];
  }

  /// Implicit conversion to bool.
  [[nodiscard]] constexpr operator bool() const { return first && count > 0; }

  /// All equal?
  [[nodiscard]] constexpr bool operator==(const Span &other) const {
    if (count != other.count)
      return false;
    for (size_t i = 0; i < count; i++)
      if (first[i] != other.first[i])
        return false;
    return true;
  }

  /// Any not-equal?
  [[nodiscard]] constexpr bool operator!=(const Span &other) const {
    return !operator==(other);
  }

  /// The pointer to the first element.
  const T *first{};

  /// The element count.
  size_t count{};
};

/// \}

} // namespace smdl
