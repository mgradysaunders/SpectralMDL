#pragma once

#include "smdl/common.h"

namespace smdl {

/// This is a wrapper around `llvm::BumpPtrAllocator`, without needing to 
/// include `llvm/Support/Allocator.h` in client code.
class SMDL_EXPORT BumpPtrAllocator final {
public:
  BumpPtrAllocator();

  BumpPtrAllocator(const BumpPtrAllocator &) = delete;

  ~BumpPtrAllocator();

  [[nodiscard]] void *allocate(size_t size, size_t align);

  void reset();

private:
  void *allocator{};
};

} // namespace smdl
