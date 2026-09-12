#pragma once

#include <cstdint>
#include <cstring>
#include <optional>
#include <vector>

#include "smdl/Resource/VoxelGrid.h"
#include "smdl/Support/RNG.h"

#include "Render/MediumStack.h"
#include "Render/Sampler.h"

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
/// the throughput. The single-medium segment, which is nearly every
/// segment, takes the paths below with no per-component bookkeeping.
///
/// A medium whose coefficients are provably point-independent
/// (`MaterialDef::hasHomogeneousCoefficients()`) takes the closed-form
/// path against the coefficient spectra captured by the instance, which
/// is why every instance is evaluated at the path's own wavelengths and
/// time. Anything else (heterogeneous or unproven) is tracked with
/// null-collision methods against the majorants the material declares
/// (`material_volume.max_*_coefficient`): delta tracking for distance
/// sampling, residual ratio tracking for shadow-ray transmittance,
/// per-point coefficients queried through the JIT `volumeEvaluate` entry
/// point in the rigid frame of the instance whose boundary entered the
/// medium. Evaluated coefficients are clamped to the declared majorants,
/// so a lying majorant renders a clamped medium instead of accumulating
/// negative-weight bias. A heterogeneous medium missing a majorant for a
/// coefficient it uses falls back to the homogeneous treatment with a
/// one-time warning.
///
/// Both estimators sample against one hero wavelength and weight by
/// the single-sample MIS balance heuristic over all bins, which the
/// null-collision generalization carries through the chain of null
/// interactions.
///
/// Coefficients are in inverse meters per the MDL specification, and
/// the renderer's scene unit is the meter (`State::metersPerSceneUnit`
/// stays 1), so distances here are in scene units with no conversion.
class Medium final {
public:
  /// Construct with nothing resolved; `reset()` targets the view at a
  /// segment.
  Medium() = default;

  /// Target the segment leaving `org` toward the unit direction `dir`,
  /// both in world space, inside the media on `stack`.
  ///
  /// Resolving the stack is the expensive half (the coefficient spectra,
  /// the majorants, the query state, the density-hint mapping), so it is
  /// skipped whenever the stack and time are the ones already resolved,
  /// and only the segment is reprojected. Within a path a repeat is
  /// found by the stack's address. Across paths the addresses mean
  /// nothing, the allocator handing them out again, so `beginPath()`
  /// drops the key and the first call after it identifies the medium by
  /// what the stack carries: the same run of materials with the same
  /// coefficients keeps the resolution, with the instance pointers
  /// refreshed, so a render inside one fog or one plume resolves it once
  /// per block rather than once per sample.
  void reset(const MediumStack *stack, const Color &wavelengths, PathTime time,
             const float3 &org, const float3 &dir) noexcept;

  /// Forget the stack the resolution is keyed on, at the head of every
  /// path; see `reset()`.
  void beginPath() noexcept { mKey.isKnown = false; }

  /// Set the scene-wide exterior haze that an empty stack resolves to,
  /// or null for a vacuum exterior. A change invalidates whatever is
  /// resolved; restating the haze already set does nothing.
  void setHaze(const smdl::Haze *haze) noexcept;

  /// Is there a scene-wide exterior haze? A caller that skips the view
  /// outright for an empty stack, the exterior vacuum being the common
  /// case, must not skip it when there is.
  [[nodiscard]] bool hasHaze() const noexcept { return mHaze.haze != nullptr; }

  /// Is there a participating medium at all?
  [[nodiscard]] bool hasMedium() const noexcept { return mHasMedium; }

  /// Does the medium emit, so that `sampleDistance()` can accumulate
  /// anything into `emitted`? A caller that carries the emission through
  /// its throughput skips that bookkeeping when this is false.
  [[nodiscard]] bool hasEmission() const noexcept { return mHasEmission; }

  /// Does `attenuate()` consume sampler draws? True exactly for the
  /// tracked path, which seeds its generator from two draws; the count
  /// is fixed, which is what lets a caller that discards the
  /// transmittance of a blocked segment consume the same two draws
  /// instead, keeping the deterministic sample sequence unchanged.
  [[nodiscard]] bool attenuationDraws() const noexcept {
    return mHasMedium && mIsHeterogeneous && mMajorant > 0.0f;
  }

  /// Sample a free-flight scattering distance over `[0, tEnd)` in scene
  /// units. Returns true on a real scattering event, setting `t` and
  /// multiplying `beta` by the spectral weight of scattering there;
  /// returns false when the segment survives to `tEnd`, multiplying
  /// `beta` by the spectral transmittance weight. Draws nothing from
  /// `sampler` when there is no medium.
  ///
  /// `emitted` accumulates the radiance the medium itself emits along
  /// the segment, an unbiased estimate of the integral of transmittance
  /// times the emission coefficient, NOT weighted by the caller's
  /// throughput: the caller adds `beta * emitted` using the throughput
  /// from before this call. Closed form for homogeneous media; for
  /// tracked media the estimate accumulates at the tentative collisions
  /// of the chain, so a tracked medium with a zero extinction majorant
  /// contributes no emission (physical emitters absorb).
  [[nodiscard]] bool sampleDistance(Sampler &sampler, float tEnd, float &t,
                                    Color &beta, Color &emitted) const;

  /// Multiply `beta` by an unbiased estimate of the transmittance over
  /// `[0, tEnd]` in scene units, for shadow rays. Draws nothing from
  /// `sampler` for the vacuum, haze and homogeneous cases.
  ///
  /// `unbounded` says the segment only ends at `tEnd` because a light
  /// infinitely far away needs a finite point to aim at, and really
  /// runs to infinity. Only the exterior haze honors it, its depth to
  /// infinity being finite and closed form.
  void attenuate(Sampler &sampler, float tEnd, Color &beta,
                 bool isUnbounded = false) const;

  /// The scattering interface of the vertex the last `sampleDistance`
  /// call returned: the haze's own phase function, or the VDF of the
  /// medium's instance, which with additive overlap is the component
  /// the collision picked.
  [[nodiscard]] Scatterer scatterer() const noexcept {
    return SMDL_UNLIKELY(mIsHaze) ? Scatterer(*mHaze.haze)
                                  : Scatterer(mScatterInstance->getVDF());
  }

private:
  //--{ One active medium
  /// One active medium of the segment, and the only representation of
  /// one: a single medium is one of these, an additive overlap is
  /// several. The flat members further down are the aggregates the
  /// sampling loops read, which are sums over these.
  struct Component final {
    /// The instance of the stack entry, whose lifetime is the path's
    /// allocator; `rebind()` points it at the entry of the path in
    /// flight.
    const smdl::JIT::Material *material{};

    /// The definition behind `material`, which outlives every path.
    const smdl::JIT::MaterialDef *materialDef{};

    /// The volume fields the instance carries, see `presenceOf()`.
    uint8_t presence{};

    /// Heterogeneous (or unproven) with usable majorants, so tracked;
    /// else the snapshot below stands for the medium.
    bool isHeterogeneous{};

    /// Does the density-hint span scale apply to this component? True
    /// only for the component whose grid drives the spans, see `Hint`.
    bool isScaledByGrid{};

    /// The instance whose rigid frame the queries evaluate in; null for
    /// a medium with no geometry, which queries in world space.
    const MeshInstance *meshInstance{};

    /// The coefficient snapshots the instance captured.
    smdl::SpectralColor sigmaA{};
    smdl::SpectralColor sigmaS{};
    smdl::SpectralColor emission{};

    /// The declared majorants, clamped nonnegative; tracked only.
    smdl::SpectralColor maxSigmaA{};
    smdl::SpectralColor maxSigmaS{};

    /// The density hint the instance declared, when usable
    /// (`hasUsableDensityGrid()`): the grid and the rigid-frame box
    /// that maps onto it.
    const smdl::VoxelGrid *grid{};
    float3 boundMin{};
    float3 boundMax{};

    /// The segment in the rigid frame of the instance.
    float3 orgR{};
    float3 dirR{};

    /// The partial state of `volumeEvaluate` queries, the render-wide
    /// fields plus the rigid frame, with `position` set per query;
    /// tracked only, a `State` being too much to build for a component
    /// that never asks a material anything.
    mutable std::optional<smdl::State> state{};

    /// The clamped scattering coefficient at the most recent query, the
    /// collision point when an overlap picks its scattering component.
    mutable smdl::SpectralColor lastSigmaS{};
  };
  //--}

  //--{ The regimes
  /// The regimes `sampleDistance()` dispatches to, each with its own
  /// frame: the closed forms are short and the tracked one is long, and
  /// one frame for all cost the tracked path its register budget.
  [[nodiscard]] SMDL_NO_INLINE bool
  sampleDistanceHaze(Sampler &sampler, float tEnd, float &t, Color &beta) const;

  [[nodiscard]] SMDL_NO_INLINE bool
  sampleDistanceHomogeneous(Sampler &sampler, float tEnd, float &t, Color &beta,
                            Color &emitted) const;

  [[nodiscard]] SMDL_NO_INLINE bool sampleDistanceTracked(Sampler &sampler,
                                                          float tEnd, float &t,
                                                          Color &beta,
                                                          Color &emitted) const;

  /// The regimes `attenuate()` dispatches to; the homogeneous form is
  /// one line and stays in the dispatcher.
  SMDL_NO_INLINE void attenuateHaze(float tEnd, Color &beta,
                                    bool isUnbounded) const;

  SMDL_NO_INLINE void attenuateTracked(Sampler &sampler, float tEnd,
                                       Color &beta) const;
  //--}

  //--{ Tracking
  /// What a tentative collision decided, and what a tracked segment
  /// came to.
  enum class Outcome { CONTINUE, SURVIVED, SCATTERED, DEAD };

  /// The null-collision march both tracked regimes share: one
  /// exponential in optical depth consumed across the majorant spans of
  /// the segment, `majorantOf(span)` giving the local tracking rate of a
  /// span and `collide(t, span, m, tau)` deciding each tentative
  /// collision, `CONTINUE` to march on with the next flight it drew
  /// into `tau` (drawn where its logarithm overlaps the collision's own
  /// reduction). The majorant is piecewise constant, so a span costs a
  /// multiply and a compare to march through, and the draw and the
  /// logarithm are paid only where a collision lands. Drawing off the
  /// canonical value itself keeps full precision where the flight is
  /// long, which is the tail that decides how far this marches. A
  /// segment past the collision cap is `DEAD`.
  template <typename MajorantOf, typename Collide>
  [[nodiscard]] Outcome track(smdl::RNG &rng, float tEnd, MajorantOf majorantOf,
                              Collide collide) const;
  //--}

  //--{ Resolution
  /// Resolve the active media on `stack` into the members that depend
  /// on the stack alone: kept by `rebind()` when they already describe
  /// this medium, built anew by `rebuild()` otherwise.
  void resolve(const MediumStack *stack, const Color &wavelengths,
               PathTime time) noexcept;

  /// Is the resolved medium the one on `stack`? Walks the active run of
  /// the stack as `rebuild()` does and compares each entry with its
  /// component; on a match the components take the entries' instances,
  /// a moving instance's frame is read again at the new time, and
  /// nothing else is touched.
  [[nodiscard]] bool rebind(const MediumStack *stack, PathTime time) noexcept;

  /// Build the resolution of `stack` from nothing.
  void rebuild(const MediumStack *stack, const Color &wavelengths,
               PathTime time) noexcept;

  /// Would `entry` resolve to `component` again? The material and the
  /// presence of its volume fields decide the path; along it, a
  /// homogeneous component is its three spectra, and a tracked one is
  /// its majorants, its instance and its density hint. The snapshot a
  /// tracked component also captured is never consulted on that path
  /// and varies with where the boundary was crossed, so it is not
  /// compared, and `rebind()` leaves it holding the first path's.
  [[nodiscard]] bool matches(const Component &component,
                             const MediumStack &entry) const noexcept;

  /// Take the density hint of the component whose grid drives the
  /// majorant spans: the hint box spans texture space `[0,1]^3`, which
  /// spans the voxel extent, and a majorant cell is
  /// `VoxelGrid::getMajorantExtent()` voxels per axis.
  void setHint(const Component &component) noexcept;

  /// Does the material declare a majorant for every coefficient it
  /// carries? A heterogeneous volume that cannot bound its own field is
  /// treated as homogeneous.
  [[nodiscard]] static bool
  hasUsableMajorants(const smdl::JIT::Material &material) noexcept {
    return (material.getAbsorptionCoefficient().empty() ||
            !material.getMaxAbsorptionCoefficient().empty()) &&
           (material.getScatteringCoefficient().empty() ||
            !material.getMaxScatteringCoefficient().empty());
  }

  /// Which of the volume fields the instance carries, as a bit set:
  /// the absorption and scattering coefficients, their majorants, and
  /// the emission intensity. Presence is what the resolution branches
  /// on, so `matches()` compares it before any value.
  [[nodiscard]] static uint8_t
  presenceOf(const smdl::JIT::Material &material) noexcept {
    uint8_t bits{};
    if (!material.getAbsorptionCoefficient().empty()) bits |= 1;
    if (!material.getScatteringCoefficient().empty()) bits |= 2;
    if (!material.getMaxAbsorptionCoefficient().empty()) bits |= 4;
    if (!material.getMaxScatteringCoefficient().empty()) bits |= 8;
    if (!material.getVolumeEmissionIntensity().empty()) bits |= 16;
    return bits;
  }

  /// Would `values` resolve to `color` again? Compared bit for bit, so
  /// two instances agree here precisely when they would resolve to the
  /// same spectrum (a signed zero against a zero rebuilds, which is
  /// harmless). An empty span agrees with the zeros it resolves to;
  /// presence is compared separately.
  [[nodiscard]] static bool
  valuesMatch(smdl::Span<const float> values,
              const smdl::SpectralColor &color) noexcept {
    const size_t n{std::min(values.size(), color.size())};
    return std::memcmp(values.data(), color.data(), n * sizeof(float)) == 0;
  }
  //--}

  //--{ Segment
  /// Project the segment into the rigid frame of every tracked
  /// component, and into majorant cell space where a density hint
  /// drives the spans. Nothing to do for a homogeneous medium.
  void setSegment(const float3 &org, const float3 &dir, float time) noexcept;

  /// The projection of `setSegment()`, `rigidOf` answering with the
  /// world-to-rigid transform of one component's instance. The static
  /// answer stays in line and the moving one out, so that the frame
  /// query stays off the per-segment path.
  template <typename RigidOf>
  void projectSegmentWith(const float3 &org, const float3 &dir,
                          const RigidOf &rigidOf) noexcept {
    for (auto &comp : mComponents) {
      if (!comp.isHeterogeneous) continue;
      if (comp.meshInstance) {
        const auto &toRigid{rigidOf(*comp.meshInstance)};
        comp.orgR = transformPoint(toRigid, org);
        comp.dirR = transformDirection(toRigid, dir);
      } else {
        comp.orgR = org;
        comp.dirR = dir;
      }
      comp.state->direction = comp.dirR;
    }
  }

  void projectSegment(const float3 &org, const float3 &dir) noexcept;

  SMDL_NO_INLINE void projectSegmentMoving(const float3 &org, const float3 &dir,
                                           float time) noexcept;
  //--}

  //--{ Queries
  /// One component's coefficients at distance `t` along the segment:
  /// the snapshot of a homogeneous component, or the clamped
  /// `volumeEvaluate` query of a tracked one, the majorant scaled by
  /// `majorantScale` for the component the hint drives. `stash` records
  /// the clamped scattering coefficient for `pickScatterComponent()`,
  /// which only an overlap runs. Inline: this is the tentative
  /// collision's work, and a leaf of its own measured slower.
  void query(const Component &comp, float t, float majorantScale,
             bool shouldStash, Color &sigmaA, Color &sigmaS,
             Color &emission) const;

  /// The volume coefficients at distance `t` along the segment, clamped
  /// to the declared majorants scaled by `majorantScale` (the local
  /// density-hint bound, or 1 without the hint), along with the
  /// emission coefficient, which has no majorant and is only clamped
  /// nonnegative. The single medium is one `query()` in line; an
  /// overlap sums the components' queries out of line.
  void evaluateCoefficients(float t, float majorantScale, Color &sigmaA,
                            Color &sigmaS, Color &emission) const;

  SMDL_NO_INLINE void evaluateOverlap(float t, float majorantScale,
                                      Color &sigmaA, Color &sigmaS,
                                      Color &emission) const;

  /// At a real collision with summed scattering coefficient `sigmaS`,
  /// pick the component whose phase function governs the event with
  /// probability proportional to its share of the scattering
  /// coefficient there (averaged over bins), and fold the per-bin
  /// spectral share over that scalar probability into `beta`, so the
  /// expectation is exactly the sigma_s-weighted mixture of the
  /// component phase functions.
  void pickScatterComponent(float xi, const Color &sigmaS, Color &beta) const;

  /// The scattering coefficient of one component at the most recent
  /// query: the per-collision clamp of a tracked component, the
  /// snapshot otherwise.
  [[nodiscard]] static const smdl::SpectralColor &
  componentSigmaS(const Component &comp) noexcept {
    return comp.isHeterogeneous ? comp.lastSigmaS : comp.sigmaS;
  }
  //--}

  //--{ State
  /// The scene-wide exterior haze, which an empty stack resolves to,
  /// and what its segment reduces to: the haze varies with world height
  /// alone, so a segment is the extinction where it starts and the
  /// shape exponent of the height along it, see `Haze::shape()`. The
  /// albedo does not vary with height and is resolved once by
  /// `setHaze()`.
  struct HazeState final {
    const smdl::Haze *haze{};
    Color sigmaC{};
    Color albedo{};
    float k{};
  };

  HazeState mHaze{};

  /// Is the resolved medium the exterior haze? Never true on a
  /// non-empty stack: the haze is the atmosphere, which a walk inside an
  /// object is not in.
  bool mIsHaze{};

  bool mHasMedium{};

  /// Heterogeneous (or unproven) with usable majorants, so tracked.
  bool mIsHeterogeneous{};

  /// Does the medium emit at all? A pure emitter with no coefficients
  /// still counts as a medium.
  bool mHasEmission{};

  /// Is the segment an additive overlap of several components? The
  /// single medium takes every path in line on this being false.
  bool mHasOverlap{};

  /// Does an instance the queries evaluate in move over the shutter, so
  /// that `setSegment()` reads its frame at the segment's time?
  bool mIsMoving{};

  /// The coefficients summed over the components: the exact spectra of
  /// a homogeneous medium, which the closed forms read, or the snapshots
  /// the tracked path ignores. Empty until a medium resolves, so a view
  /// that never resolves one costs nothing; every read is behind
  /// `mHasMedium` or `mIsHeterogeneous`.
  smdl::SpectralColor mSigmaA{};
  smdl::SpectralColor mSigmaS{};
  smdl::SpectralColor mSigmaT{};
  smdl::SpectralColor mEmission{};

  /// The declared majorants summed over the components, a homogeneous
  /// component contributing its exact spectrum; tracked only.
  smdl::SpectralColor mMaxSigmaA{};
  smdl::SpectralColor mMaxSigmaS{};

  /// The scalar tracking majorant: the maximum over bins of the summed
  /// extinction majorant.
  float mMajorant{};

  /// The split of `mMajorant` that the majorant spans scale: the local
  /// tracking majorant over a span is `mMajorantBase + mMajorantGrid *
  /// span.scale`. A single medium is all grid part (base 0), reducing
  /// to plain scaling; with overlap only the hinted component scales
  /// and the rest is the constant base.
  float mMajorantBase{};
  float mMajorantGrid{};

  /// The density acceleration hint, active when exactly one component
  /// declares a usable one (`material_volume.density` in the builtin
  /// API) and the medium is tracked: the tracking loops then walk the
  /// grid's majorant cells instead of the global majorant, skipping
  /// empty cells outright.
  struct Hint final {
    /// The grid, null without the hint.
    const smdl::VoxelGrid *grid{};

    /// The extinction majorant spectrum that a span's `scaleMin` lower
    /// bounds, the control of residual ratio tracking: the hinted
    /// component's own, the other components' extinction not being
    /// bounded below by the grid. Zero without the hint.
    smdl::SpectralColor majorant{};

    /// The affine map from the rigid frame into majorant cell space,
    /// per axis `(x - boundMin) * cellScale`.
    float3 boundMin{};
    float3 cellScale{};

    /// The segment in cell space: the cell coordinate at distance `t`
    /// is `cellOrg + t * cellDir`.
    float3 cellOrg{};
    float3 cellDir{};

    /// One over the grid's global maximum, which scales a cell's
    /// maximum into a majorant scale in `[0, 1]`.
    float invMaxValue{};

    /// The index in `mComponents` of the hinted component, or -1.
    int component{-1};
  };

  Hint mHint{};

  /// The active media of the segment, one entry for the single medium
  /// and one per overlapping medium otherwise. The capacity survives
  /// `resolve()`, so the storage is bought once per view.
  // Not an 'llvm::SmallVector': 'Component' carries 'SpectralColor', whose
  // inline buffer is over-aligned, and that container's heap growth is a
  // plain 'malloc' that only guarantees the fundamental alignment.
  std::vector<Component> mComponents{};

  /// The instance behind `scatterer()`; mutable because a real collision
  /// picks the component during the const sampling call.
  mutable const smdl::JIT::Material *mScatterInstance{};

  /// What the resolution describes: the stack and the time it was
  /// resolved at, which `reset()` compares against to keep it outright.
  struct Key final {
    const MediumStack *stack{};
    float time{};

    /// Is `stack` a stack of the path in flight? False before the first
    /// `reset()` and after `beginPath()`, when the address may since
    /// have been handed to another stack.
    bool isKnown{};

    /// Has anything been resolved at all, for `rebind()` to compare
    /// against? Resolving a null stack, or one carrying no medium, is a
    /// resolution like any other; a haze change is what unsets this.
    bool isResolved{};
  };

  Key mKey{};
  //--}
};
