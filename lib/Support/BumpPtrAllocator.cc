#include "smdl/Support/BumpPtrAllocator.h"

#include <cstdlib>

namespace smdl {

BumpPtrAllocator::~BumpPtrAllocator() {
  for (Slab *slab{mSlab}; slab;) {
    Slab *prev{slab->prev};
    std::free(slab);
    slab = prev;
  }
  mCur = mEnd = 0;
  mSlab = mFirstSlab = nullptr;
}

void *BumpPtrAllocator::allocateSlow(size_t size, size_t align) noexcept {
  // Each slab doubles the last up to the cap, so the number of system
  // allocations is logarithmic in the total handed out. A request too big
  // for that gets a slab sized for it alone, which strands whatever was
  // left of the current slab; the doubling then catches up, so a run of
  // such requests strands memory only until the slabs outgrow them.
  SMDL_SANITY_CHECK_MSG(size <= SIZE_MAX - SLAB_HEADER_SIZE - align,
                        "allocation size overflows!");
  size_t slabSize{!mSlab                         ? MIN_SLAB_SIZE
                  : mSlab->size >= MAX_SLAB_SIZE ? MAX_SLAB_SIZE
                                                 : mSlab->size * 2};
  const size_t sizeNeeded{SLAB_HEADER_SIZE + (align - 1) + size};
  if (slabSize < sizeNeeded) slabSize = sizeNeeded;
  auto *slab{static_cast<Slab *>(std::malloc(slabSize))};
  SMDL_SANITY_CHECK_MSG(slab != nullptr, "out of memory!");
  slab->prev = mSlab;
  slab->size = slabSize;
  mSlab = slab;
  if (!mFirstSlab) mFirstSlab = slab;
  const uintptr_t ptr{
      (reinterpret_cast<uintptr_t>(slab) + SLAB_HEADER_SIZE + (align - 1)) &
      ~uintptr_t(align - 1)};
  mCur = ptr + size;
  mEnd = reinterpret_cast<uintptr_t>(slab) + slabSize;
  return reinterpret_cast<void *>(ptr);
}

void BumpPtrAllocator::freeSlabsAfterFirst() noexcept {
  for (Slab *slab{mSlab}; slab != mFirstSlab;) {
    Slab *prev{slab->prev};
    std::free(slab);
    slab = prev;
  }
  mSlab = mFirstSlab;
}

} // namespace smdl
