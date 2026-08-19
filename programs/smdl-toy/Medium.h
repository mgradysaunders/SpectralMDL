#pragma once

#include <vector>

#include "Scene.h"

/// The stack of nested participating media the walk is currently
/// inside, entered and left through transmitting boundary crossings.
class MediumStack final {
public:
  const MediumStack *prev{};

  smdl::JIT::MaterialInstance materialInstance{};

  /// The mesh instance whose boundary was crossed to enter this medium,
  /// which carries the world-to-rigid transform that heterogeneous
  /// volume queries evaluate in. Null for a medium with no geometry,
  /// e.g., a scene-wide exterior medium.
  const MeshInstance *meshInstance{};

  static void Update(const MediumStack *&stack,
                     smdl::BumpPtrAllocator &allocator,
                     smdl::JIT::MaterialInstance mat,
                     const MeshInstance *meshInstance, const float3 &wo,
                     const float3 &wi) {
    if (!mat.isTransmitting(wo, wi)) return;
    if (mat.isInterior(wi)) {
      stack = new (allocator) MediumStack{stack, mat, meshInstance};
      return;
    }
    // Exiting: remove the entry entered through this same boundary, not
    // blindly the top. With overlapping volumes the boundary being
    // exited need not be the most recently entered one (enter fog,
    // enter cloud, exit fog), and a walk that began inside the geometry
    // has no matching entry at all, in which case nothing is removed
    // rather than desynchronizing whatever medium the walk is actually
    // in. The boundary is identified by the instance and material
    // together, falling back to the instance alone for a closed volume
    // whose shell mixes materials.
    const MediumStack *found{};
    for (const MediumStack *entry{stack}; entry; entry = entry->prev) {
      if (entry->meshInstance == meshInstance) {
        if (entry->materialInstance.material == mat.material) {
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
    return new (allocator)
        MediumStack{Remove(stack->prev, entry, allocator),
                    stack->materialInstance, stack->meshInstance};
  }
};

/// The index of refraction of the medium surrounding the object being hit,
/// which the material needs to form the relative IOR across the interface.
/// On a front-face hit that is the medium the ray currently travels in (the
/// top of the stack); on a back-face hit the ray travels inside the object
/// itself, so the surrounding medium is the next stack entry below it.
[[nodiscard]] inline float ExteriorIOR(const MediumStack *stack,
                                       const smdl::JIT::MaterialInstance &mat,
                                       const float3 &wo) noexcept {
  if (mat.isInterior(wo)) stack = stack ? stack->prev : nullptr;
  return stack ? stack->materialInstance.getIOR() : 1.0f;
}

/// The medium of one ray segment: a view over the active media on the
/// stack that owns free-flight distance sampling and transmittance
/// estimation, resolved against the materials' static knowledge.
///
/// The active media are the run of additive entries from the top of
/// the stack (`material_volume.additive`) plus the first non-additive
/// entry, which replaces everything below it, i.e., the ordinary
/// nested-medium convention. Overlapping components sum: coefficients,
/// majorants, and emission add, distance sampling runs against the
/// summed majorant, and a real collision picks the component whose
/// phase function governs the event in proportion to its share of the
/// scattering coefficient there (decomposition tracking, Kutz et al.
/// 2017), folding the spectral share over the pick probability into
/// the throughput. The overwhelmingly common single-medium segment
/// takes exactly the paths described below with no per-component
/// bookkeeping at all.
///
/// A provably homogeneous medium (`Material::hasHomogeneousVolume()`)
/// takes the closed-form path against the coefficient spectra captured
/// by the instance. Anything else (heterogeneous or unproven) is
/// tracked with null-collision methods against the majorants the
/// material declares (`material_volume.max_*_coefficient`): delta
/// tracking for distance sampling, ratio tracking for shadow-ray
/// transmittance, per-point coefficients queried through the JIT
/// `volumeEvaluate` entry point in the rigid frame of the instance
/// whose boundary entered the medium. Evaluated coefficients are
/// clamped to the declared majorants, so a lying majorant renders a
/// clamped medium instead of accumulating negative-weight bias. A
/// heterogeneous medium missing a majorant for a coefficient it uses
/// falls back to the homogeneous treatment with a one-time warning.
///
/// Both estimators sample against one hero wavelength and weight by
/// the single-sample MIS balance heuristic over all bins, which the
/// null-collision generalization carries through the chain of null
/// interactions.
///
/// Coefficients are in inverse meters per the MDL specification and
/// are converted with `State::meters_per_scene_unit`, so distances
/// here stay in scene units.
class Medium final {
public:
  /// Resolve the medium for a segment leaving `org` toward the unit
  /// direction `dir`, both in world space. Cheap for the vacuum and
  /// homogeneous cases; the heterogeneous case builds the partial
  /// query state and transforms the segment into the rigid frame.
  explicit Medium(const MediumStack *stack, const Color &wavelengths,
                  const float3 &org, const float3 &dir) noexcept;

  /// Is there a participating medium at all?
  [[nodiscard]] bool hasMedium() const noexcept { return mHasMedium; }

  /// Sample a free-flight scattering distance over `[0, tEnd)` in scene
  /// units. Returns true on a real scattering event, setting `t` and
  /// multiplying `beta` by the spectral weight of scattering there;
  /// returns false when the segment survives to `tEnd`, multiplying
  /// `beta` by the spectral transmittance weight. Draws nothing from
  /// `sampler` when there is no medium.
  ///
  /// `emitted` accumulates the radiance the medium itself emits along
  /// the segment, an unbiased estimate of the integral of
  /// transmittance times the emission coefficient, NOT weighted by the
  /// caller's throughput: the caller adds `beta * emitted` using the
  /// throughput from before this call. Closed form for homogeneous
  /// media; for heterogeneous media the estimate accumulates at the
  /// tentative collisions of the tracking chain, so a heterogeneous
  /// medium with a zero extinction majorant contributes no emission
  /// (physical emitters absorb).
  [[nodiscard]] bool sampleDistance(Sampler &sampler, float tEnd, float &t,
                                    Color &beta, Color &emitted) const;

  /// Multiply `beta` by an unbiased estimate of the transmittance over
  /// `[0, tEnd]` in scene units, for shadow rays. Draws nothing from
  /// `sampler` for the vacuum and homogeneous cases.
  void attenuate(Sampler &sampler, float tEnd, Color &beta) const;

  /// The material whose phase function governs the scattering event
  /// the last `sampleDistance` call returned: the single medium, or
  /// with additive overlap the component picked at the collision.
  [[nodiscard]] const smdl::JIT::MaterialInstance *
  scatterInstance() const noexcept {
    return mScatterInstance;
  }

private:
  /// One component of an additive overlap, mirroring the single-medium
  /// members below; populated only when the segment is inside two or
  /// more overlapping media.
  struct Component final {
    /// The material instance of the stack entry, whose lifetime is the
    /// path's allocator.
    const smdl::JIT::MaterialInstance *materialInstance{};

    /// Is this component heterogeneous (or unproven) with usable
    /// majorants? A component missing a majorant falls back to the
    /// homogeneous snapshot with the same one-time warning as a
    /// single medium.
    bool heterogeneous{};

    /// Does the density-hint span scale apply to this component? True
    /// only for the unique component whose grid drives the majorant
    /// spans, see `mDensityGrid`.
    bool scaledByGrid{};

    /// The coefficient snapshots captured by the instance, in inverse
    /// scene units, see the single-medium members below.
    smdl::ColorVector sigmaA{};

    /// See `sigmaA`.
    smdl::ColorVector sigmaS{};

    /// See `sigmaA`.
    smdl::ColorVector emission{};

    /// The declared majorants in inverse scene units, present only on
    /// heterogeneous components.
    smdl::ColorVector maxSigmaA{};

    /// See `maxSigmaA`.
    smdl::ColorVector maxSigmaS{};

    /// The segment in the rigid frame of this component's instance.
    float3 orgR{};

    /// See `orgR`.
    float3 dirR{};

    /// The `volumeEvaluate` query state of this component, mutable for
    /// the same reason as `mState`.
    mutable smdl::State state{};

    /// The clamped scattering coefficient at the most recent
    /// `evaluateCoefficients` query, which is the collision point when
    /// a real collision picks the scattering component.
    mutable smdl::ColorVector lastSigmaS{};
  };

  /// Query the volume coefficients at distance `t` along the segment,
  /// clamped to the declared majorants scaled by `majorantScale` (the
  /// local density-hint bound, or 1 without the hint; with overlap the
  /// scale applies only to the grid component), in inverse scene
  /// units, along with the emission coefficient, which has no majorant
  /// and is only clamped nonnegative. With overlap this sums the
  /// per-component queries.
  void evaluateCoefficients(float t, float majorantScale, Color &sigmaA,
                            Color &sigmaS, Color &emission) const;

  /// At a real collision with summed scattering coefficient `sigmaS`,
  /// pick the component whose phase function governs the event with
  /// probability proportional to its share of the scattering
  /// coefficient there (averaged over bins), and fold the per-bin
  /// spectral share over that scalar probability into `beta`, so the
  /// expectation is exactly the sigma_s-weighted mixture of the
  /// component phase functions.
  void pickScatterComponent(float xi, const Color &sigmaS, Color &beta) const;

  /// The scattering coefficient of one component at the most recent
  /// query: the per-collision clamp for a heterogeneous component, the
  /// snapshot otherwise.
  [[nodiscard]] static const smdl::ColorVector &
  componentSigmaS(const Component &component) noexcept {
    return component.heterogeneous ? component.lastSigmaS : component.sigmaS;
  }

  /// Is there a medium?
  bool mHasMedium{};

  /// Is the medium heterogeneous (or unproven) with usable majorants?
  bool mHeterogeneous{};

  /// The material of the medium, for `volumeEvaluate`.
  const smdl::JIT::Material *mMaterial{};

  /// The absorption coefficient captured by the instance (summed over
  /// components with overlap), either the exact homogeneous spectrum
  /// or the surface-hit snapshot that the heterogeneous path ignores,
  /// in inverse scene units.
  ///
  /// The coefficient members are plain `ColorVector`s that stay
  /// empty until a medium sizes them, so the ubiquitous
  /// vacuum-segment construction costs nothing; every read is
  /// guarded by `mHasMedium`/`mHeterogeneous`, under which the
  /// constructor sized them.
  smdl::ColorVector mSigmaA{};

  /// The scattering coefficient captured by the instance, see
  /// `mSigmaA`.
  smdl::ColorVector mSigmaS{};

  /// Does the medium emit at all? A pure emitter with no coefficients
  /// still counts as a medium.
  bool mHasEmission{};

  /// The emission coefficient captured by the instance, in radiance
  /// per scene unit, see `mSigmaA` for the heterogeneous caveat.
  smdl::ColorVector mEmission{};

  /// The declared majorants in inverse scene units, present only on
  /// the heterogeneous path. With overlap these are per-bin sums, a
  /// homogeneous component contributing its exact spectrum.
  smdl::ColorVector mMaxSigmaA{};

  /// See `mMaxSigmaA`.
  smdl::ColorVector mMaxSigmaS{};

  /// The scalar tracking majorant: the maximum over bins of the summed
  /// extinction majorant, in inverse scene units.
  float mMajorant{};

  /// The split of `mMajorant` that the majorant spans scale: the local
  /// tracking majorant over a span is `mMajorantBase + mMajorantGrid *
  /// span.scale`. A single medium puts everything in the grid part
  /// (base 0, grid `mMajorant`), reducing to plain scaling; with
  /// overlap only the grid component's contribution scales and the
  /// rest is the constant base.
  float mMajorantBase{};

  /// See `mMajorantBase`.
  float mMajorantGrid{};

  /// The extinction majorant spectrum that `span.scaleMin` lower
  /// bounds, which residual ratio tracking uses as its analytic
  /// control: the summed majorant for a single medium, the grid
  /// component's alone with overlap (the other components' extinction
  /// is not bounded below by the grid). Sized only on the
  /// heterogeneous path.
  smdl::ColorVector mGridMaxSigma{};

  /// The segment origin in the rigid frame of the medium's instance.
  float3 mOrgR{};

  /// The segment direction in the rigid frame, still unit length
  /// because the rigid transform has no scale.
  float3 mDirR{};

  /// The density acceleration hint grid, non-null only when the
  /// material declares the complete hint (see `material_volume.density`
  /// in the builtin API) and the medium is heterogeneous. The tracking
  /// loops then traverse per-brick majorant spans instead of the global
  /// majorant, skipping empty bricks outright.
  const smdl::VoxelGrid *mDensityGrid{};

  /// With the hint, the segment mapped into the grid's brick space: the
  /// brick coordinate at distance `t` in scene units is
  /// `mBrickOrg + t * mBrickDir`.
  float3 mBrickOrg{};

  /// See `mBrickOrg`.
  float3 mBrickDir{};

  /// With the hint, one over the grid's global maximum value, which
  /// scales per-brick maxima into majorant scale factors in `[0, 1]`.
  float mInvMaxValue{};

  /// The partial state for `volumeEvaluate` queries: render-wide
  /// fields plus the rigid-frame transform; `position` is set per
  /// query. Mutable because queries write the position into it while
  /// leaving the medium logically unchanged.
  mutable smdl::State mState{};

  /// The `State::meters_per_scene_unit` conversion the coefficient
  /// clamps apply, hoisted so the per-component states need not be
  /// consulted.
  float mUnitScale{1.0f};

  /// The components of an additive overlap, empty for the common
  /// single-medium segment, which keeps to the members above and
  /// allocates nothing extra.
  std::vector<Component> mComponents{};

  /// See `scatterInstance()`. Mutable because a real collision picks
  /// the component during the const sampling call.
  mutable const smdl::JIT::MaterialInstance *mScatterInstance{};
};
