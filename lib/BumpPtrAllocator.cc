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

extern "C" {

SMDL_EXPORT void *smdl_bump_allocate(BumpPtrAllocator &allocator, int_t size, int_t align) {
  sanity_check(size >= 0);
  sanity_check(align >= 1);
  return allocator.allocate(size, align);
}

} // extern "C"

} // namespace smdl
