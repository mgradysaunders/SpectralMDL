/// \file
#pragma once

#include <algorithm>
#include <array>
#include <cstddef>
#include <initializer_list>
#include <vector>

namespace smdl {

/// \addtogroup support
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
  constexpr Span(T &elem) : mFirst(&elem), mCount(1) {}

  /// Construct from pointer to first element and element count.
  constexpr Span(T *first, size_t count)
      : mFirst(first), mCount(first == nullptr ? 0 : count) {}

  /// Construct from `std::initializer_list`.
  constexpr Span(std::initializer_list<std::decay_t<T>> elems)
      : mFirst(elems.begin()), mCount(elems.size()) {}

  /// Construct from `std::array`.
  template <size_t N>
  constexpr Span(const std::array<std::decay_t<T>, N> &elems)
      : mFirst(const_cast<T *>(elems.data())), mCount(elems.size()) {}

  /// Construct from `std::vector`.
  template <typename Allocator>
  Span(const std::vector<std::decay_t<T>, Allocator> &elems)
      : mFirst(const_cast<T *>(elems.data())), mCount(elems.size()) {}

  /// Is empty?
  [[nodiscard]] constexpr bool empty() const noexcept { return mCount == 0; }

  /// Get the size.
  [[nodiscard]] constexpr size_t size() const noexcept { return mCount; }

  /// Get the data pointer.
  [[nodiscard]] constexpr T *data() const noexcept { return mFirst; }

  /// Get the begin iterator.
  [[nodiscard]] constexpr T *begin() const noexcept { return mFirst; }

  /// Get the end iterator.
  [[nodiscard]] constexpr T *end() const noexcept { return mFirst + mCount; }

  /// Get the front element.
  [[nodiscard]] constexpr const T &front() const noexcept { return mFirst[0]; }

  /// Get the back element.
  [[nodiscard]] constexpr const T &back() const noexcept {
    return mFirst[mCount - 1];
  }

  /// Drop the front element while the given predicate is true.
  template <typename Pred>
  [[nodiscard]] constexpr Span dropFrontWhile(Pred &&pred) const {
    size_t i{};
    size_t n{mCount};
    while (i < mCount && pred(mFirst[i])) {
      i++;
      n--;
    }
    return subspan(i, n);
  }

  /// Drop the front element.
  [[nodiscard]] constexpr Span dropFront() const noexcept {
    return subspan(1, mCount - 1);
  }

  /// Drop the back element.
  [[nodiscard]] constexpr Span dropBack() const noexcept {
    return subspan(0, mCount - 1);
  }

  /// Get subspan. The start index `i` is clamped to `size()`, so an
  /// out-of-range request yields an empty span instead of underflowing
  /// `count - i` into an enormous out-of-bounds span.
  [[nodiscard]] constexpr Span subspan(size_t i,
                                       size_t n = size_t(-1)) const noexcept {
    i = std::min(i, mCount);
    return Span(mFirst + i, std::min(mCount - i, n));
  }

  /// Contains the given value?
  [[nodiscard]] constexpr bool contains(const T &value) const {
    return std::find(begin(), end(), value) != end();
  }

  /// Starts with the given sequence of values?
  [[nodiscard]] constexpr bool startsWith(Span other) const {
    if (mCount < other.mCount) return false;
    for (size_t i = 0; i < other.mCount; i++)
      if (operator[](i) != other[i]) return false;
    return true;
  }

  /// Get element by index.
  [[nodiscard]] constexpr T &operator[](size_t i) const noexcept {
    return mFirst[i];
  }

  /// All equal?
  [[nodiscard]] constexpr bool operator==(const Span &other) const {
    if (mCount != other.mCount) return false;
    for (size_t i = 0; i < mCount; i++)
      if (mFirst[i] != other.mFirst[i]) return false;
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
    return Span<ConstT>(mFirst, mCount);
  }

private:
  /// The pointer to the first element.
  T *mFirst{};

  /// The element count.
  size_t mCount{};
};

/// \}

} // namespace smdl
