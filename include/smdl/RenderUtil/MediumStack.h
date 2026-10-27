/// \file
/// The stack of nested participating media a path is inside, entered and
/// left at boundary crossings, with priorities where media overlap.
#pragma once

#include <atomic>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <utility>

#include "smdl/JIT.h"
#include "smdl/Support/BumpPtrAllocator.h"

namespace smdl {

/// \addtogroup renderutil
/// \{

/// The boundary of a placed medium as the host identifies it: typically
/// one instance of a mesh, whose interior is the medium.
struct MediumBoundary final {
  /// The host's identity of the boundary, compared by address only, or
  /// null for a medium with no geometry (a scene-wide exterior medium).
  const void *id{};

  /// The priority, where overlapping media of higher priority win.
  int priority{};

  /// Does the boundary enclose a medium at all? A boundary that does not
  /// (a fiber, say) is never on a stack and is never crossed falsely.
  bool boundsMedium{true};
};

/// An entry of the stack of nested participating media a path is inside.
///
/// Where media overlap, one of them is the medium there: the dominant
/// entry (`dominantOf()`), the non-additive entry of the highest priority,
/// the most recently entered among equals. An additive entry
/// (`material_volume.additive`) adds to it where it ranks above it. A
/// crossing that changes none of that is false (`isFalseCrossing()`): it
/// scatters nothing, and a walk passes through it keeping the stack
/// current. After Schmidt and Budge, "Simple Nested Dielectrics in Ray
/// Traced Images", JGT 7(2), 2002, except that leaving an entry that is not
/// the dominant one is false even at the dominant's priority, so that the
/// medium at a point does not depend on how a path came to it.
///
/// Entries are immutable once pushed and live in a `BumpPtrAllocator`, so a
/// stack is a pointer, copying one is free, and a crossing rebuilds only
/// the entries above the one it touches.
class MediumStack final {
public:
  /// The rank of an entry with no boundary, which every placed medium
  /// outranks.
  static constexpr int LOWEST_PRIORITY{std::numeric_limits<int>::min()};

  /// The entry below this one.
  const MediumStack *prev{};

  /// The material instance evaluated at the boundary that entered this
  /// medium, which must outlive the stack.
  const jit::Material *material{};

  /// The identity of the boundary that entered this medium
  /// (`MediumBoundary::id`).
  const void *boundary{};

  /// The dominant entry of the stack below this one, so that the dominant
  /// entry of any stack is one comparison away.
  const MediumStack *dominantBelow{};

  /// The rank of the medium: the boundary's priority, or `LOWEST_PRIORITY`
  /// for an entry with no boundary.
  int priority{};

  /// An entry over `prev` of the medium `material`, entered through
  /// `boundary`.
  MediumStack(const MediumStack *prev, const jit::Material *material,
              const MediumBoundary &boundary) noexcept
      : prev(prev), material(material), boundary(boundary.id),
        dominantBelow(dominantOf(prev)),
        priority(boundary.id ? boundary.priority : LOWEST_PRIORITY) {}

  /// Does the medium add to the one it is in rather than replace it?
  [[nodiscard]] bool isAdditive() const noexcept {
    return material->hasAdditiveVolume();
  }

  /// Is `other` the same medium: the same boundary with the same material
  /// definition? A mesh that overlaps itself enters it twice, and is one
  /// medium all the same.
  [[nodiscard]] bool isSameMedium(const MediumStack &other) const noexcept {
    return boundary == other.boundary && material->def == other.material->def;
  }

  /// The dominant entry of `stack`, or null where every entry is additive
  /// or there is none.
  [[nodiscard]] static const MediumStack *
  dominantOf(const MediumStack *stack) noexcept {
    if (!stack) return nullptr;
    const MediumStack *below{stack->dominantBelow};
    if (stack->isAdditive() || (below && stack->priority < below->priority))
      return below;
    return stack;
  }

  /// The dominant entry `stack` would have without `entry`, one of its own
  /// entries.
  [[nodiscard]] static const MediumStack *
  dominantWithout(const MediumStack *stack, const MediumStack *entry) noexcept {
    // The entries above it are more recent than any below, so the first of
    // their highest priority wins a tie with the dominant below.
    const MediumStack *above{};
    for (const MediumStack *other{stack}; other != entry; other = other->prev)
      if (!other->isAdditive() && (!above || other->priority > above->priority))
        above = other;
    const MediumStack *below{entry->dominantBelow};
    if (above && (!below || above->priority >= below->priority))
      return above;
    return below;
  }

  /// Call `fn` with every active entry of `stack`, from the top down: the
  /// dominant entry, and every additive entry that ranks above it, by
  /// priority and then by recency. These are the media a segment travels
  /// through.
  template <typename Fn>
  static void forEachActive(const MediumStack *stack, Fn &&fn) {
    const MediumStack *dominant{dominantOf(stack)};
    bool isAboveDominant{true};
    for (const MediumStack *entry{stack}; entry; entry = entry->prev) {
      if (entry == dominant) {
        isAboveDominant = false;
        fn(*entry);
      } else if (entry->isAdditive() &&
                 (!dominant || entry->priority > dominant->priority ||
                  (isAboveDominant && entry->priority == dominant->priority))) {
        fn(*entry);
      }
    }
  }

  /// The entry of `stack` that leaving through `boundary` with
  /// `materialDef` leaves, or null for none: the one entered through this
  /// same boundary, not blindly the top, since overlapping volumes need not
  /// be left in the order they were entered. The material breaks ties
  /// between entries of one boundary whose shell mixes materials.
  [[nodiscard]] static const MediumStack *
  entryOf(const MediumStack *stack, const jit::MaterialDef *materialDef,
          const void *boundary) noexcept {
    const MediumStack *found{};
    for (const MediumStack *entry{stack}; entry; entry = entry->prev) {
      if (entry->boundary == boundary) {
        if (entry->material->def == materialDef) return entry;
        if (!found) found = entry;
      }
    }
    return found;
  }

  /// Is crossing `boundary` with `materialDef`, out of its interior where
  /// `isLeaving`, false: does it leave the active media of `stack` as they
  /// are? Entering is false below the dominant entry's priority, or into
  /// the dominant medium itself. Leaving is false out of an entry that is
  /// not active, or out of the dominant one where another entry of the same
  /// medium takes its place.
  [[nodiscard]] static bool
  isFalseCrossing(const MediumStack *stack, const MediumBoundary &boundary,
                  const jit::MaterialDef *materialDef,
                  bool isLeaving) noexcept {
    if (!boundary.boundsMedium) return false;
    const MediumStack *dominant{dominantOf(stack)};
    if (!isLeaving)
      return dominant && (boundary.priority < dominant->priority ||
                          (dominant->boundary == boundary.id &&
                           dominant->material->def == materialDef));
    const MediumStack *entry{entryOf(stack, materialDef, boundary.id)};
    if (!entry) return false;
    if (entry != dominant) {
      bool isActive{};
      forEachActive(stack, [&](const MediumStack &active) {
        isActive |= &active == entry;
      });
      return !isActive;
    }
    const MediumStack *next{dominantWithout(stack, entry)};
    return next && next->isSameMedium(*entry);
  }

  /// Can a segment from inside `stack` cross anything falsely, where no
  /// boundary of a medium ranks below `lowestPriority`? Not where nothing
  /// outranks what it might enter and every entry it might leave is active.
  [[nodiscard]] static bool canCrossFalsely(const MediumStack *stack,
                                            int lowestPriority) noexcept {
    const MediumStack *dominant{dominantOf(stack)};
    if (dominant && dominant->priority > lowestPriority) return true;
    size_t numActive{};
    size_t numPlaced{};
    forEachActive(stack, [&](const MediumStack &active) {
      numActive += active.boundary ? 1 : 0;
    });
    for (const MediumStack *entry{stack}; entry; entry = entry->prev)
      numPlaced += entry->boundary ? 1 : 0;
    return numPlaced != numActive;
  }

  /// The entry above `entry` of `stack` that is a different medium of the
  /// same priority, or null for none. Leaving an entry under one is the
  /// signature of two overlapping media of equal priority, whose medium in
  /// the overlap depends on the way a path came in.
  [[nodiscard]] static const MediumStack *
  tiedAbove(const MediumStack *stack, const MediumStack *entry) noexcept {
    for (const MediumStack *other{stack}; other != entry; other = other->prev)
      if (other->priority == entry->priority && !other->isSameMedium(*entry))
        return other;
    return nullptr;
  }

  /// Cross `boundary` with the `material` evaluated at it, from `wo` to
  /// `wi`: entering is `enter()`, leaving is `leave()`, and a pair on one
  /// side does nothing.
  ///
  /// \note
  /// This decides the side from `jit::Material::isInterior()`, which is
  /// only meaningful where the host evaluated the material with the true
  /// geometric orientation. A host that only knows which way a crossing
  /// went calls `enter()` and `leave()` directly.
  ///
  static void update(const MediumStack *&stack, BumpPtrAllocator &allocator,
                     const jit::Material *material,
                     const MediumBoundary &boundary, const float3 &wo,
                     const float3 &wi) {
    if (!boundary.id || !boundary.boundsMedium ||
        !material->isTransmitting(wo, wi))
      return;
    if (material->isInterior(wi))
      enter(stack, allocator, material, boundary);
    else
      leave(stack, allocator, material->def, boundary);
  }

  /// Enter the medium `material` through `boundary`.
  static void enter(const MediumStack *&stack, BumpPtrAllocator &allocator,
                    const jit::Material *material,
                    const MediumBoundary &boundary) {
    stack = new (allocator) MediumStack{stack, material, boundary};
  }

  /// Leave the medium entered through `boundary` with `materialDef`
  /// (`entryOf()`), which is all that identifies it, so a crossing known to
  /// leave need not evaluate a material. A leave with no entry removes
  /// nothing rather than desynchronizing the medium the walk is actually
  /// in.
  static void leave(const MediumStack *&stack, BumpPtrAllocator &allocator,
                    const jit::MaterialDef *materialDef,
                    const MediumBoundary &boundary) {
    if (const MediumStack *entry{entryOf(stack, materialDef, boundary.id)})
      stack = remove_(stack, entry, allocator);
  }

private:
  // Rebuild the stack without `entry`, copying the entries above it. The
  // entries are immutable once pushed, so sharing the tail below `entry` is
  // sound, and the recursion depth is the nesting depth.
  [[nodiscard]] static const MediumStack *
  remove_(const MediumStack *stack, const MediumStack *entry,
         BumpPtrAllocator &allocator) {
    if (stack == entry) return stack->prev;
    return new (allocator)
        MediumStack{remove_(stack->prev, entry, allocator), stack->material,
                    MediumBoundary{stack->boundary, stack->priority}};
  }
};

/// The medium a segment leaving a vertex toward `wi` starts in: `stack`
/// where the direction stays on the side the path arrived from along `wo`,
/// and across the vertex's boundary where it transmits. `material` is the
/// instance at the vertex, or null at a vertex on no boundary. See the note
/// on `MediumStack::update()`.
[[nodiscard]] inline const MediumStack *
mediumToward(const MediumStack *stack, BumpPtrAllocator &allocator,
             const jit::Material *material, const MediumBoundary &boundary,
             const float3 &wo, const float3 &wi) {
  if (material)
    MediumStack::update(stack, allocator, material, boundary, wo, wi);
  return stack;
}

/// The index of refraction of the medium surrounding a boundary hit from
/// `wo`, which `jit::Material::setExteriorIOR()` wants: the dominant entry
/// of the stack on the far side of the crossing. Entering, that is the
/// dominant entry the ray travels in; leaving the dominant entry, it is the
/// one that takes its place. The vacuum is 1. See the note on
/// `MediumStack::update()`.
[[nodiscard]] inline float exteriorIOR(const MediumStack *stack,
                                       const jit::Material &material,
                                       const MediumBoundary &boundary,
                                       const float3 &wo) noexcept {
  const MediumStack *outside{MediumStack::dominantOf(stack)};
  if (outside && boundary.id && boundary.boundsMedium &&
      material.isInterior(wo) &&
      MediumStack::entryOf(stack, material.def, boundary.id) == outside)
    outside = MediumStack::dominantWithout(stack, outside);
  return outside ? outside->material->getIOR() : 1.0f;
}

/// A render-wide tally of crossings that showed overlapping media of equal
/// priority (`MediumStack::tiedAbove()`), and the first pair of boundaries
/// by host index, so that the pair a host reports is the same at any
/// thread count.
class MediumTies final {
public:
  /// The pair of boundary indices `a` and `b` as `add()` takes it.
  [[nodiscard]] static uint64_t pairOf(uint32_t a, uint32_t b) noexcept {
    return a < b ? uint64_t(a) << 32 | b : uint64_t(b) << 32 | a;
  }

  [[nodiscard]] uint64_t count() const noexcept {
    return count_.load(std::memory_order_relaxed);
  }

  /// The two boundary indices of the first pair.
  [[nodiscard]] std::pair<uint32_t, uint32_t> firstPair() const noexcept {
    const uint64_t pair{firstPair_.load(std::memory_order_relaxed)};
    return {uint32_t(pair >> 32), uint32_t(pair)};
  }

  /// Add `count` crossings over the pair of boundary indices `pair`, as
  /// `pairOf()` forms it.
  void add(uint64_t count, uint64_t pair) noexcept {
    count_.fetch_add(count, std::memory_order_relaxed);
    uint64_t first{firstPair_.load(std::memory_order_relaxed)};
    while (pair < first && !firstPair_.compare_exchange_weak(
                               first, pair, std::memory_order_relaxed)) {
    }
  }

private:
  std::atomic<uint64_t> count_{};

  std::atomic<uint64_t> firstPair_{std::numeric_limits<uint64_t>::max()};
};

/// \}

} // namespace smdl
