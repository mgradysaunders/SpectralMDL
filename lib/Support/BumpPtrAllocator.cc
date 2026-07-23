#include "smdl/Support/BumpPtrAllocator.h"

#include "llvm/Support/Allocator.h"

namespace smdl {

BumpPtrAllocator::BumpPtrAllocator() { mPtr = new llvm::BumpPtrAllocator(); }

BumpPtrAllocator::~BumpPtrAllocator() {
  delete static_cast<llvm::BumpPtrAllocator *>(mPtr);
  mPtr = nullptr;
}

void *BumpPtrAllocator::allocate(size_t size, size_t align) noexcept {
  if (size == 0)
    return nullptr;
  // Serve from the fast-path window if the request fits. This must match
  // the inline fast path JIT-compiled code emits for '#bump'.
  auto aligned{(reinterpret_cast<uintptr_t>(windowPtr) + align - 1) &
               ~(align - 1)};
  if (windowPtr && aligned + size <= reinterpret_cast<uintptr_t>(windowEnd)) {
    windowPtr = reinterpret_cast<void *>(aligned + size);
    return reinterpret_cast<void *>(aligned);
  }
  auto allocator{static_cast<llvm::BumpPtrAllocator *>(mPtr)};
  // Serve large requests directly so they do not thrash the window.
  constexpr size_t windowSize{4096};
  if (size + align > windowSize / 4)
    return allocator->Allocate(size, align);
  // Refill the window, abandoning whatever slack the old window had left,
  // and carve the request from the front.
  auto window{static_cast<std::byte *>(
      allocator->Allocate(windowSize, alignof(std::max_align_t)))};
  aligned = (reinterpret_cast<uintptr_t>(window) + align - 1) & ~(align - 1);
  windowPtr = reinterpret_cast<void *>(aligned + size);
  windowEnd = window + windowSize;
  return reinterpret_cast<void *>(aligned);
}

void BumpPtrAllocator::reset() noexcept {
  windowPtr = nullptr;
  windowEnd = nullptr;
  static_cast<llvm::BumpPtrAllocator *>(mPtr)->Reset();
}

size_t BumpPtrAllocator::getBytesAllocated() const noexcept {
  return static_cast<const llvm::BumpPtrAllocator *>(mPtr)
      ->getBytesAllocated();
}

} // namespace smdl
