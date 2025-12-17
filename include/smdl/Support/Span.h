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
  constexpr Span(T &elem) : first(&elem), count(1) {}

  /// Construct from pointer to first element and element count.
  constexpr Span(T *first, size_t count) : first(first), count(count) {}

  /// Construct from `std::initializer_list`.
  constexpr Span(std::initializer_list<std::decay_t<T>> elems)
      : first(elems.begin()), count(elems.size()) {}

  /// Construct from `std::array`.
  template <size_t N>
  constexpr Span(const std::array<std::decay_t<T>, N> &elems)
      : first(const_cast<T *>(elems.data())), count(elems.size()) {}

  /// Construct from `std::vector`.
  template <typename Allocator>
  Span(const std::vector<std::decay_t<T>, Allocator> &elems)
      : first(const_cast<T *>(elems.data())), count(elems.size()) {}

  /// Is empty?
  [[nodiscard]] constexpr bool empty() const noexcept { return count == 0; }

  /// Get the size.
  [[nodiscard]] constexpr size_t size() const noexcept { return count; }

  /// Get the data pointer.
  [[nodiscard]] constexpr T *data() const noexcept { return first; }

  /// Get the begin iterator.
  [[nodiscard]] constexpr T *begin() const noexcept { return first; }

  /// Get the end iterator.
  [[nodiscard]] constexpr T *end() const noexcept { return first + count; }

  /// Get the front element.
  [[nodiscard]] constexpr const T &front() const noexcept { return first[0]; }

  /// Get the back element.
  [[nodiscard]] constexpr const T &back() const noexcept {
    return first[count - 1];
  }

  /// Drop the front element while the given predicate is true.
  template <typename Pred>
  [[nodiscard]] constexpr Span dropFrontWhile(Pred &&pred) const {
    size_t i{};
    size_t n{count};
    while (i < count && pred(first[i])) {
      i++;
      n--;
    }
    return subspan(i, n);
  }

  /// Drop the front element.
  [[nodiscard]] constexpr Span dropFront() const noexcept {
    return subspan(1, count - 1);
  }

  /// Drop the back element.
  [[nodiscard]] constexpr Span dropBack() const noexcept {
    return subspan(0, count - 1);
  }

  /// Get subspan.
  [[nodiscard]] constexpr Span subspan(size_t i,
                                       size_t n = size_t(-1)) const noexcept {
    return Span(first + i, std::min(count - i, n));
  }

  /// Contains the given value?
  [[nodiscard]] constexpr bool contains(const T &value) const {
    return std::find(begin(), end(), value) != end();
  }

  /// Starts with the given sequence of values?
  [[nodiscard]] constexpr bool startsWith(Span other) const {
    if (count < other.count)
      return false;
    for (size_t i = 0; i < other.count; i++)
      if (operator[](i) != other[i])
        return false;
    return true;
  }

  /// Get element by index.
  [[nodiscard]] constexpr T &operator[](size_t i) const noexcept {
    return first[i];
  }

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

  /// Implicit conversion of non-const to const.
  template <typename ConstT,
            typename = std::enable_if_t<
                std::is_same_v<ConstT, const T> && !std::is_const_v<T>, void>>
  [[nodiscard]] constexpr operator Span<ConstT>() const noexcept {
    return Span<ConstT>(first, count);
  }

private:
  /// The pointer to the first element.
  T *first{};

  /// The element count.
  size_t count{};
};

/// \}

} // namespace smdl
