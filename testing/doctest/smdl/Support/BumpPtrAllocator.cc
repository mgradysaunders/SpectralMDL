#include "doctest.h"

#include <cstdint>
#include <vector>

#include "smdl/Support/BumpPtrAllocator.h"
#include "smdl/Support/RNG.h"

namespace {

[[nodiscard]] bool isAlignedTo(const void *ptr, size_t align) {
  return reinterpret_cast<uintptr_t>(ptr) % align == 0;
}

// One block handed out by the allocator, and the byte it was filled with,
// so that a later pass can prove nothing overlapped or moved.
struct Block final {
  unsigned char *ptr{};
  size_t size{};
  unsigned char fill{};
};

int liveCount{};

struct Destructed final {
  Destructed() { liveCount++; }
  ~Destructed() { liveCount--; }
  int value{7};
};

struct alignas(64) OverAligned final {
  float values[4];
};

} // namespace

TEST_CASE("BumpPtrAllocator") {
  SUBCASE("zero size allocates nothing") {
    auto allocator{smdl::BumpPtrAllocator()};
    CHECK(allocator.allocate(0, 1) == nullptr);
    CHECK(allocator.allocate(0, 16) == nullptr);
    // A zero-size request must not disturb the bump pointer.
    void *ptr0{allocator.allocate(8, 8)};
    CHECK(allocator.allocate(0, 8) == nullptr);
    void *ptr1{allocator.allocate(8, 8)};
    CHECK(static_cast<char *>(ptr1) - static_cast<char *>(ptr0) == 8);
  }
  SUBCASE("alignment") {
    auto allocator{smdl::BumpPtrAllocator()};
    // Interleave alignments so that each allocation has to pad past the
    // last, and include alignments past the slab header's own.
    for (size_t align = 1; align <= 256; align *= 2) {
      for (size_t size = 1; size <= 40; size += 7) {
        CHECK(isAlignedTo(allocator.allocate(size, align), align));
        CHECK(isAlignedTo(allocator.allocate(1, 1), 1));
      }
    }
  }
  SUBCASE("blocks do not overlap and survive slab growth") {
    auto allocator{smdl::BumpPtrAllocator()};
    auto rng{smdl::RNG(1234)};
    auto blocks{std::vector<Block>()};
    // Far past MIN_SLAB_SIZE, so the run spans many slabs, with the
    // occasional request too big for the slab it would land in.
    size_t total{};
    while (total < 8 * smdl::BumpPtrAllocator::MIN_SLAB_SIZE) {
      const bool isHuge{rng.generateInt(32) == 0};
      const size_t size{isHuge ? size_t(rng.generateInt(200000) + 1)
                               : size_t(rng.generateInt(300) + 1)};
      const size_t align{size_t(1) << rng.generateInt(7)};
      auto *ptr{static_cast<unsigned char *>(allocator.allocate(size, align))};
      REQUIRE(ptr != nullptr);
      CHECK(isAlignedTo(ptr, align));
      const auto fill{static_cast<unsigned char>(blocks.size() & 0xFF)};
      for (size_t i = 0; i < size; i++) ptr[i] = fill;
      blocks.push_back(Block{ptr, size, fill});
      total += size;
    }
    // Every block still reads back what it was filled with, so no two
    // overlapped and no slab was released early.
    bool isIntact{true};
    for (const auto &block : blocks)
      for (size_t i = 0; i < block.size; i++)
        isIntact &= block.ptr[i] == block.fill;
    CHECK(isIntact);
    CHECK(blocks.size() > 1);
  }
  SUBCASE("reset rewinds into the first slab") {
    auto allocator{smdl::BumpPtrAllocator()};
    void *first{allocator.allocate(64, 16)};
    allocator.reset();
    // The first slab is kept, so the same request lands at the same
    // address. The render loop resets once per sample and depends on this
    // to stop calling the system allocator after the first sample.
    CHECK(allocator.allocate(64, 16) == first);
    // Grow well past the first slab, then reset again: the extra slabs go
    // back to the system and the first one is rewound into.
    for (size_t i = 0; i < 4 * smdl::BumpPtrAllocator::MIN_SLAB_SIZE; i += 128)
      REQUIRE(allocator.allocate(128, 16) != nullptr);
    allocator.reset();
    CHECK(allocator.allocate(64, 16) == first);
    // And a reset with nothing allocated at all is harmless.
    auto empty{smdl::BumpPtrAllocator()};
    empty.reset();
    empty.reset();
    CHECK(empty.allocate(8, 8) != nullptr);
  }
  SUBCASE("an allocation larger than a slab") {
    auto allocator{smdl::BumpPtrAllocator()};
    const size_t size{4 * smdl::BumpPtrAllocator::MIN_SLAB_SIZE};
    auto *ptr{static_cast<unsigned char *>(allocator.allocate(size, 64))};
    REQUIRE(ptr != nullptr);
    CHECK(isAlignedTo(ptr, 64));
    for (size_t i = 0; i < size; i++) ptr[i] = 0xAB;
    // The allocator keeps serving small requests afterward.
    void *after{allocator.allocate(16, 16)};
    REQUIRE(after != nullptr);
    CHECK(isAlignedTo(after, 16));
    bool isIntact{true};
    for (size_t i = 0; i < size; i++) isIntact &= ptr[i] == 0xAB;
    CHECK(isIntact);
  }
  SUBCASE("typed allocation") {
    auto allocator{smdl::BumpPtrAllocator()};
    // A trivially destructible type comes back as a raw pointer, since
    // there is nothing to destruct.
    auto *values{allocator.allocate<float>(3.0f)};
    static_assert(std::is_same_v<decltype(values), float *>);
    CHECK(*values == 3.0f);
    liveCount = 0;
    {
      auto ptr{allocator.allocate<Destructed>()};
      static_assert(std::is_same_v<decltype(ptr), smdl::BumpPtr<Destructed>>);
      CHECK(ptr->value == 7);
      CHECK(liveCount == 1);
    }
    CHECK(liveCount == 0);
  }
  SUBCASE("placement new honors extended alignment") {
    auto allocator{smdl::BumpPtrAllocator()};
    // Nudge the bump pointer off the extended alignment first, so that a
    // pass through the unaligned overload would be caught.
    CHECK(allocator.allocate(1, 1) != nullptr);
    auto *ptr{new (allocator) OverAligned{}};
    CHECK(isAlignedTo(ptr, alignof(OverAligned)));
  }
}
