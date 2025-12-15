/// \file
#pragma once

#include <cstddef>
#include <new>
#include <type_traits>
#include <utility>

#include "smdl/Export.h"

namespace smdl {

/// \addtogroup Support
/// \{

/// A bump pointer allocated by `BumpPtrAllocator` that does not need to be
/// freed, but may need to be destructed.
///
/// This is effectively a `std::unique_ptr` with a deleter that only
/// invokes the destructor.
template <typename T> class BumpPtr final {
public:
  BumpPtr() = default;

  BumpPtr(std::nullptr_t) {}

  /// Construct from raw pointer.
  template <typename U> BumpPtr(U *ptr) : ptr(static_cast<T *>(ptr)) {
    static_assert(std::is_base_of_v<T, U>);
  }

  /// Copy constructor is disabled!
  BumpPtr(const BumpPtr &) = delete;

  /// Move constructor.
  BumpPtr(BumpPtr &&other) : ptr(std::exchange(other.ptr, nullptr)) {}

  /// Move constructor from derived type.
  template <typename U>
  BumpPtr(BumpPtr<U> &&other) : BumpPtr(std::exchange(other.ptr, nullptr)) {}

  /// Copy assignment is disabled!
  BumpPtr &operator=(const BumpPtr &) = delete;

  /// Move assignment.
  BumpPtr &operator=(BumpPtr &&other) {
    reset(std::exchange(other.ptr, nullptr));
    return *this;
  }

  /// Move assignment from derived type.
  template <typename U> BumpPtr &operator=(BumpPtr<U> &&other) {
    static_assert(std::is_base_of_v<T, U>);
    reset(std::exchange(other.ptr, nullptr));
    return *this;
  }

  ~BumpPtr() { reset(); }

  [[nodiscard]] auto *get() { return ptr; }

  [[nodiscard]] auto *get() const { return ptr; }

  [[nodiscard]] auto *operator->() { return ptr; }

  [[nodiscard]] auto *operator->() const { return ptr; }

  [[nodiscard]] auto &operator*() { return *ptr; }

  [[nodiscard]] auto &operator*() const { return *ptr; }

  [[nodiscard]] operator bool() const { return ptr != nullptr; }

  [[nodiscard]] bool operator!() const { return ptr == nullptr; }

  void reset() {
    if (ptr) {
      ptr->~T();
      ptr = nullptr;
    }
  }

  template <typename U> void reset(U *newPtr) {
    static_assert(std::is_base_of_v<T, U>);
    reset();
    ptr = static_cast<T *>(newPtr);
  }

public:
  T *ptr{};
};

/// A bump pointer allocator, opaque wrapper around
/// `llvm::BumpPtrAllocator`.
class SMDL_EXPORT BumpPtrAllocator final {
public:
  BumpPtrAllocator();

  BumpPtrAllocator(const BumpPtrAllocator &) = delete;

  ~BumpPtrAllocator();

  /// Allocate raw memory.
  ///
  /// \param[in] size   The size in bytes.
  /// \param[in] align  The alignment in bytes.
  ///
  [[nodiscard]] void *allocate(size_t size, size_t align) noexcept;

  /// Allocate and initialize type `T` by passing `Args...` to the constructor.
  template <typename T, typename... Args>
  [[nodiscard]] auto allocate(Args &&...args) {
    auto result{new (allocate(sizeof(T), alignof(T)))
                    T{std::forward<Args>(args)...}};
    if constexpr (std::is_trivially_destructible_v<T>)
      return result;
    else
      return BumpPtr<T>(result);
  }

  /// Reset the allocator, freeing all memory.
  void reset() noexcept;

  /// Get the number of bytes allocated.
  [[nodiscard]] size_t getBytesAllocated() const noexcept;

private:
  /// The pointer to the `llvm::BumpPtrAllocator`.
  void *object{};
};

/// \}

} // namespace smdl

[[nodiscard]] inline void *operator new(std::size_t sz,
                                        smdl::BumpPtrAllocator &allocator) {
  return allocator.allocate(sz, alignof(std::max_align_t));
}
