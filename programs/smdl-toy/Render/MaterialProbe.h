/// \file
/// Reading what is structural about a material, before any path exists.
#pragma once

#include "smdl/JIT.h"

#include "Color.h"

/// One material evaluated against a placeholder shading state, for the
/// sake of what is structural about it.
///
/// Nothing geometric is applied, so only what does not depend on the
/// state means anything through a probe: the emission and lobe flags,
/// which are decided by whether the corresponding node is non-default,
/// and whatever those imply. A quantity that varies over a surface reads
/// here as one representative sample of it, which is enough for a
/// selection weight and nothing more.
///
/// The evaluation allocates, so a caller probing a whole scene declares
/// one allocator outside its loop and resets it per instance. The probe
/// does not reset it, because the caller is the one that knows when what
/// it read stops being needed.
class MaterialProbe final {
public:
  MaterialProbe(smdl::BumpPtrAllocator &allocator, const Color &wavelengths,
                const smdl::JIT::MaterialDef &materialDef)
      : mState{makeRenderState(wavelengths, &allocator)} {
    // One texture space, so that a material sampling a texture reads a
    // defined coordinate rather than a space the state does not have.
    mState.textureSpaceCount = 1;
    mState.finalize();
    mMaterial = smdl::JIT::Material(mState, &materialDef);
  }

  MaterialProbe(const MaterialProbe &) = delete;

  MaterialProbe &operator=(const MaterialProbe &) = delete;

  /// The evaluated material, which lives as long as the probe.
  [[nodiscard]] smdl::JIT::Material &material() noexcept { return mMaterial; }

private:
  /// The placeholder state, held because the evaluation was taken
  /// against it.
  smdl::State mState;

  smdl::JIT::Material mMaterial{};
};
