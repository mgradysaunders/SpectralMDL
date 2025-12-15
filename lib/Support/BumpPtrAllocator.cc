#include "smdl/Support/BumpPtrAllocator.h"

#include "llvm/Support/Allocator.h"

namespace smdl {

BumpPtrAllocator::BumpPtrAllocator() { object = new llvm::BumpPtrAllocator(); }

BumpPtrAllocator::~BumpPtrAllocator() {
  delete static_cast<llvm::BumpPtrAllocator *>(object);
  object = nullptr;
}

void *BumpPtrAllocator::allocate(size_t size, size_t align) noexcept {
  if (size == 0)
    return nullptr;
  return static_cast<llvm::BumpPtrAllocator *>(object)->Allocate(size, align);
}

void BumpPtrAllocator::reset() noexcept {
  static_cast<llvm::BumpPtrAllocator *>(object)->Reset();
}

size_t BumpPtrAllocator::getBytesAllocated() const noexcept {
  return static_cast<const llvm::BumpPtrAllocator *>(object)
      ->getBytesAllocated();
}

} // namespace smdl
