#include "smdl/Support/BumpPtrAllocator.h"

#include <cstdlib>

namespace smdl {

BumpPtrAllocator::~BumpPtrAllocator() {
  for (Slab *slab{slab_}; slab;) {
    Slab *prev{slab->prev};
    std::free(slab);
    slab = prev;
  }
  cur_ = end_ = 0;
  slab_ = firstSlab_ = nullptr;
}

void *BumpPtrAllocator::allocateSlow_(size_t size, size_t align) noexcept {
  // Each slab doubles the last up to the cap, so the number of system
  // allocations is logarithmic in the total handed out. A request too big
  // for that gets a slab sized for it alone, which strands whatever was
  // left of the current slab; the doubling then catches up, so a run of
  // such requests strands memory only until the slabs outgrow them.
  SMDL_SANITY_CHECK_MSG(size <= SIZE_MAX - SLAB_HEADER_SIZE - align,
                        "allocation size overflows!");
  size_t slabSize{!slab_                         ? MIN_SLAB_SIZE
                  : slab_->size >= MAX_SLAB_SIZE ? MAX_SLAB_SIZE
                                                 : slab_->size * 2};
  const size_t sizeNeeded{SLAB_HEADER_SIZE + (align - 1) + size};
  if (slabSize < sizeNeeded) slabSize = sizeNeeded;
  Slab *slab{static_cast<Slab *>(std::malloc(slabSize))};
  SMDL_SANITY_CHECK_MSG(slab != nullptr, "out of memory!");
  slab->prev = slab_;
  slab->size = slabSize;
  slab_ = slab;
  if (!firstSlab_) firstSlab_ = slab;
  const uintptr_t ptr{
      (reinterpret_cast<uintptr_t>(slab) + SLAB_HEADER_SIZE + (align - 1)) &
      ~uintptr_t(align - 1)};
  cur_ = ptr + size;
  end_ = reinterpret_cast<uintptr_t>(slab) + slabSize;
  return reinterpret_cast<void *>(ptr);
}

void BumpPtrAllocator::freeSlabsAfterFirst_() noexcept {
  for (Slab *slab{slab_}; slab != firstSlab_;) {
    Slab *prev{slab->prev};
    std::free(slab);
    slab = prev;
  }
  slab_ = firstSlab_;
}

} // namespace smdl
