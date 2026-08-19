#include "smdl/Support/BumpPtrAllocator.h"

#include "llvm/Support/Allocator.h"

namespace smdl {

// Use 64K slabs instead of the default 4K so that typical per-sample
// render allocations fit in the first slab, which `Reset()` retains.
using LLVMAllocator = llvm::BumpPtrAllocatorImpl<llvm::MallocAllocator, 65536>;

BumpPtrAllocator::BumpPtrAllocator() { mPtr = new LLVMAllocator(); }

BumpPtrAllocator::~BumpPtrAllocator() {
  delete static_cast<LLVMAllocator *>(mPtr);
  mPtr = nullptr;
}

void *BumpPtrAllocator::allocate(size_t size, size_t align) noexcept {
  if (size == 0) return nullptr;
  return static_cast<LLVMAllocator *>(mPtr)->Allocate(size, align);
}

void BumpPtrAllocator::reset() noexcept {
  static_cast<LLVMAllocator *>(mPtr)->Reset();
}

size_t BumpPtrAllocator::getBytesAllocated() const noexcept {
  return static_cast<const LLVMAllocator *>(mPtr)->getBytesAllocated();
}

} // namespace smdl
