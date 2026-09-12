#pragma once

#include "smdl/RenderUtil/Haze.h"

#include "Scene/Scene.h"

/// The stack of nested participating media the walk is currently
/// inside, entered and left through transmitting boundary crossings.
class MediumStack final {
public:
  const MediumStack *prev{};

  /// The instance evaluated at the boundary that entered this medium,
  /// which lives as long as the stack does: in the path's allocator for
  /// an entry a walk pushes, alongside the coefficients the instance
  /// already keeps there, and in the stage's for the exterior medium.
  const smdl::JIT::Material *material{};

  /// The mesh instance whose boundary was crossed to enter this medium,
  /// which carries the world-to-rigid transform that heterogeneous
  /// volume queries evaluate in. Null for a medium with no geometry,
  /// e.g., a scene-wide exterior medium.
  const MeshInstance *meshInstance{};

  /// Cross the boundary of `meshInstance` with the evaluated `material`
  /// evaluated at it, from `wo` to `wi`: entering pushes the instance,
  /// which the stack keeps by address and so must outlive it, leaving
  /// is `Leave()`, and a pair on one side does nothing.
  static void Update(const MediumStack *&stack,
                     smdl::BumpPtrAllocator &allocator,
                     const smdl::JIT::Material *material,
                     const MeshInstance *meshInstance, const float3 &wo,
                     const float3 &wi) {
    if (!material->isTransmitting(wo, wi)) return;
    if (material->isInterior(wi)) {
      stack = new (allocator) MediumStack{stack, material, meshInstance};
      return;
    }
    Leave(stack, allocator, material->def, meshInstance);
  }

  /// Leave the medium entered through the boundary of `meshInstance`
  /// with `material`, which is all that identifies it, so a crossing
  /// known to leave need not evaluate an instance at all. The entry
  /// removed is the one entered through this same boundary, not
  /// blindly the top. With overlapping volumes the boundary being
  /// exited need not be the most recently entered one (enter fog,
  /// enter cloud, exit fog), and a walk that began inside the geometry
  /// has no matching entry at all, in which case nothing is removed
  /// rather than desynchronizing whatever medium the walk is actually
  /// in. The boundary is identified by the instance and material
  /// together, falling back to the instance alone for a closed volume
  /// whose shell mixes materials.
  static void Leave(const MediumStack *&stack,
                    smdl::BumpPtrAllocator &allocator,
                    const smdl::JIT::MaterialDef *materialDef,
                    const MeshInstance *meshInstance) {
    const MediumStack *found{};
    for (const MediumStack *entry{stack}; entry; entry = entry->prev) {
      if (entry->meshInstance == meshInstance) {
        if (entry->material->def == materialDef) {
          found = entry;
          break;
        }
        if (!found) found = entry;
      }
    }
    if (found) stack = Remove(stack, found, allocator);
  }

private:
  // Rebuild the stack without `entry`, copying the entries above it.
  // The entries are immutable once pushed, so sharing the tail below
  // `entry` is sound, and the recursion depth is the nesting depth.
  [[nodiscard]] static const MediumStack *
  Remove(const MediumStack *stack, const MediumStack *entry,
         smdl::BumpPtrAllocator &allocator) {
    if (stack == entry) return stack->prev;
    return new (allocator) MediumStack{Remove(stack->prev, entry, allocator),
                                       stack->material, stack->meshInstance};
  }
};

static_assert(sizeof(MediumStack) == 3 * sizeof(void *),
              "a stack entry is three pointers");

/// The index of refraction of the medium surrounding the object being hit,
/// which the material needs to form the relative IOR across the interface.
/// On a front-face hit that is the medium the ray currently travels in (the
/// top of the stack); on a back-face hit the ray travels inside the object
/// itself, so the surrounding medium is the next stack entry below it.
[[nodiscard]] inline float ExteriorIOR(const MediumStack *stack,
                                       const smdl::JIT::Material &material,
                                       const float3 &wo) noexcept {
  if (material.isInterior(wo)) stack = stack ? stack->prev : nullptr;
  return stack ? stack->material->getIOR() : 1.0f;
}

/// The scattering interface of one path vertex: the material instance
/// that owns the BSDF at a surface or hair vertex, the VDF of a volume
/// vertex inside a medium an MDL material describes, or the exterior
/// haze, whose phase function has no material behind it. Converts
/// implicitly from each, so every vertex reads as what it scatters with.
class Scatterer final {
public:
  Scatterer(const smdl::JIT::Material &material) noexcept
      : mMaterial(&material) {}

  Scatterer(const smdl::JIT::VDF &vdf) noexcept : mVDF(vdf) {}

  Scatterer(const smdl::Haze &haze) noexcept : mHaze(&haze) {}

  /// The material instance behind a surface or hair vertex, which every
  /// one of them has; a volume vertex scatters with a VDF or the haze
  /// and has none.
  [[nodiscard]] const smdl::JIT::Material &material() const noexcept {
    return *mMaterial;
  }

  /// The VDF of a volume vertex inside a medium an MDL material
  /// describes, whose pointer names the evaluation it was taken from.
  [[nodiscard]] const smdl::JIT::VDF &vdf() const noexcept { return mVDF; }

  /// The phase function of a volume vertex, normalized over the sphere
  /// and so also the solid-angle density of `volumeScatterSample()`.
  [[nodiscard]] float volumeScatterEvaluate(const float3 &wo,
                                            const float3 &wi) const {
    return SMDL_UNLIKELY(mHaze) ? mHaze->phase().evaluate(wo, wi)
                                : mVDF.evaluate(wo, wi);
  }

  /// Sample the phase function of a volume vertex, returning its value.
  [[nodiscard]] float volumeScatterSample(const float4 &xi, const float3 &wo,
                                          float3 &wi) const {
    return SMDL_UNLIKELY(mHaze)
               ? mHaze->phase().sample(float3(xi.x, xi.y, xi.z), wo, wi)
               : mVDF.sample(xi, wo, wi);
  }

private:
  const smdl::JIT::Material *mMaterial{};

  smdl::JIT::VDF mVDF{};

  const smdl::Haze *mHaze{};
};
