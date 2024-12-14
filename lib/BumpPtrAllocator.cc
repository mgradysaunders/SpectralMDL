#include "smdl/BumpPtrAllocator.h"

#include "llvm.h"

namespace smdl {

BumpPtrAllocator::BumpPtrAllocator() { allocator = new llvm::BumpPtrAllocator(); }

BumpPtrAllocator::~BumpPtrAllocator() {
  delete static_cast<llvm::BumpPtrAllocator *>(allocator);
  allocator = nullptr;
}

void *BumpPtrAllocator::allocate(size_t size, size_t align) {
  return static_cast<llvm::BumpPtrAllocator *>(allocator)->Allocate(size, align);
}

void BumpPtrAllocator::reset() { static_cast<llvm::BumpPtrAllocator *>(allocator)->Reset(); }

} // namespace smdl
