/// \file
#pragma once

#include <cstddef>
#include <cstdint>
#include <new>
#include <type_traits>
#include <utility>

#include "smdl/Export.h"
#include "smdl/Support/Macros.h"

namespace smdl {

/// \addtogroup support
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

/// A bump pointer allocator.
///
/// Memory comes from slabs obtained from the system allocator and is handed
/// out by advancing a pointer through the newest one. Nothing is freed
/// individually and no destructor ever runs, which is what `BumpPtr` is
/// for; `reset()` reclaims everything at once but keeps the first slab, so
/// a caller that resets in a loop (once per render sample, once per
/// compile) stops calling the system allocator at all after the first pass.
///
/// The fast path is inline here on purpose. The library is built with
/// hidden visibility and is consumed across a shared library boundary, so
/// an out-of-line `allocate()` costs a call per allocation on both the host
/// side and the JIT'd material side, which is the same order of work as the
/// allocation itself.
///
/// Not thread safe. Give each thread its own.
class SMDL_EXPORT BumpPtrAllocator final {
public:
  /// The size of the first slab, which `reset()` keeps.
  static constexpr size_t MIN_SLAB_SIZE{size_t(64) << 10};

  /// The size no slab grows past.
  static constexpr size_t MAX_SLAB_SIZE{size_t(16) << 20};

  BumpPtrAllocator() noexcept = default;

  BumpPtrAllocator(const BumpPtrAllocator &) = delete;

  BumpPtrAllocator &operator=(const BumpPtrAllocator &) = delete;

  ~BumpPtrAllocator();

  /// Allocate raw memory.
  ///
  /// \param[in] size   The size in bytes. Zero allocates nothing and
  ///                   returns null.
  /// \param[in] align  The alignment in bytes, which must be a power of
  ///                   two.
  ///
  [[nodiscard]] void *allocate(size_t size, size_t align) noexcept {
    if (SMDL_UNLIKELY(size == 0)) return nullptr;
    // Neither comparison can overflow: `mCur` is never past `mEnd`, and
    // the padding is only weighed against what is left once `size` is
    // known to fit.
    const uintptr_t avail{mEnd - mCur};
    const uintptr_t ptr{(mCur + (align - 1)) & ~uintptr_t(align - 1)};
    if (SMDL_UNLIKELY(size > avail || ptr - mCur > avail - size))
      return allocateSlow(size, align);
    mCur = ptr + size;
    return reinterpret_cast<void *>(ptr);
  }

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

  /// Reset the allocator, freeing every slab but the first and rewinding
  /// into it.
  ///
  /// Every pointer handed out since construction or since the last reset
  /// dangles afterward, and the addresses are reused by the allocations
  /// that follow, so a cache keyed on the identity of an allocation must
  /// not outlive the reset that invalidates it.
  void reset() noexcept {
    if (SMDL_UNLIKELY(mSlab != mFirstSlab)) freeSlabsAfterFirst();
    if (SMDL_LIKELY(mFirstSlab != nullptr)) {
      mCur = reinterpret_cast<uintptr_t>(mFirstSlab) + SLAB_HEADER_SIZE;
      mEnd = reinterpret_cast<uintptr_t>(mFirstSlab) + mFirstSlab->size;
    }
  }

private:
  /// The header at the front of every slab, so that the chain of slabs
  /// needs no bookkeeping allocation of its own.
  struct Slab final {
    Slab *prev;

    /// The size of the whole slab in bytes, header included.
    size_t size;
  };

  /// The offset of the usable bytes in a slab. The system allocator
  /// returns memory aligned for any fundamental type, so rounding the
  /// header up to that alignment is what lets the first allocation in a
  /// slab need no padding.
  static constexpr size_t SLAB_HEADER_SIZE{
      (sizeof(Slab) + alignof(std::max_align_t) - 1) &
      ~(alignof(std::max_align_t) - 1)};

  /// Allocate from a fresh slab, the current one having no room.
  [[nodiscard]] SMDL_NO_INLINE void *allocateSlow(size_t size,
                                                  size_t align) noexcept;

  /// Free every slab but the first, which `reset()` keeps.
  SMDL_NO_INLINE void freeSlabsAfterFirst() noexcept;

  /// The bump pointer, and the end of the slab it points into.
  uintptr_t mCur{};

  /// See `mCur`.
  uintptr_t mEnd{};

  /// The newest slab, which is the one `mCur` points into and the head of
  /// the chain running back to `mFirstSlab`.
  Slab *mSlab{};

  /// The oldest slab, which `reset()` keeps and rewinds into.
  Slab *mFirstSlab{};
};

/// \}

} // namespace smdl

[[nodiscard]] inline void *operator new(std::size_t sz,
                                        smdl::BumpPtrAllocator &allocator) {
  return allocator.allocate(sz, __STDCPP_DEFAULT_NEW_ALIGNMENT__);
}

/// The over-aligned form, which the compiler selects instead of the above
/// for a type whose alignment exceeds `__STDCPP_DEFAULT_NEW_ALIGNMENT__`.
[[nodiscard]] inline void *operator new(std::size_t sz, std::align_val_t al,
                                        smdl::BumpPtrAllocator &allocator) {
  return allocator.allocate(sz, static_cast<std::size_t>(al));
}
