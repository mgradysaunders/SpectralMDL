/// \file
#pragma once

#include "smdl/Common.h"

namespace smdl {

class Compiler;

class VoxelGrid;

/// \addtogroup compiler
/// \{

/// \name Material Flags
///
/// The builtin `api.smdl` mirrors these constants by hand to compute
/// `_MaterialEval.flags`, so a new flag that an evaluation sets must be
/// added in both places.
///
/// \{

/// Indicates that the material is transporting importance.
inline constexpr int MATERIAL_TRANSPORT_IMPORTANCE = (1 << 0);

/// Indicates that the material is thin-walled.
inline constexpr int MATERIAL_THIN_WALLED = (1 << 1);

/// Indicates that the material has a non-default `surface` initializer.
inline constexpr int MATERIAL_HAS_SURFACE = (1 << 2);

/// Indicates that the material has a non-default `backface` initializer.
inline constexpr int MATERIAL_HAS_BACKFACE = (1 << 3);

/// Indicates that the material has a non-default emission EDF in the
/// `surface` initializer.
inline constexpr int MATERIAL_HAS_SURFACE_EMISSION = (1 << 4);

/// Indicates that the material has a non-default emission EDF in the
/// `backface` initializer.
///
/// \note
/// The back side only actually emits if the material is also thin-walled,
/// which may only be knowable at runtime. See
/// `JIT::MaterialDef::emissionEvaluate`.
///
inline constexpr int MATERIAL_HAS_BACKFACE_EMISSION = (1 << 5);

/// Indicates that the material has a non-default `volume` initializer.
inline constexpr int MATERIAL_HAS_VOLUME = (1 << 6);

/// Indicates that the material has a non-default `hair` initializer.
inline constexpr int MATERIAL_HAS_HAIR = (1 << 7);

/// Indicates that the material has a cutout opacity less than one.
inline constexpr int MATERIAL_HAS_CUTOUT = (1 << 8);

/// Indicates that the material volume coefficients vary from point to
/// point inside a medium instance.
///
/// \note
/// This bit only ever appears in `JIT::MaterialDef::staticFlags`: it is
/// derived after optimization from which `State` fields the body of
/// `JIT::MaterialDef::volumeEvaluate` still reads, so it degrades to unknown
/// at `OPT_LEVEL_NONE` and `JIT::MaterialDef::Eval::flags` never sets it.
/// See `JIT::MaterialDef::hasHomogeneousCoefficients()` for the
/// conservative reading and the contract.
///
inline constexpr int MATERIAL_HAS_HETEROGENEOUS_COEFFICIENTS = (1 << 9);

/// Indicates that the material has a non-zero `geometry.displacement`.
///
/// \note
/// This bit only ever appears in `JIT::MaterialDef::staticFlags`: it is
/// derived after optimization from whether `geometry.displacement` folds to a
/// constant, so it degrades to unknown at `OPT_LEVEL_NONE` and
/// `JIT::MaterialDef::Eval::flags` never sets it. See
/// `JIT::MaterialDef::hasZeroDisplacement()` for the conservative reading.
///
inline constexpr int MATERIAL_HAS_DISPLACEMENT = (1 << 10);

/// Indicates that the material volume is declared additive (the SMDL
/// extension field `material_volume.additive`): it overlaps rather than
/// displaces the medium that encloses it, so hosts tracking nested media
/// should add its coefficients to the enclosing medium's over the shared
/// interior instead of replacing them.
inline constexpr int MATERIAL_ADDITIVE_VOLUME = (1 << 11);

/// Indicates that the material remaps `geometry.normal` away from the
/// state's shading normal.
///
/// \note
/// This bit only ever appears in `JIT::MaterialDef::staticFlags`: it is
/// derived after optimization from whether `geometry.normal - $state.normal`
/// folds to the constant zero vector, so it degrades to unknown at
/// `OPT_LEVEL_NONE` and `JIT::MaterialDef::Eval::flags` never sets it. See
/// `JIT::MaterialDef::canRemapNormal()` for the conservative reading.
///
inline constexpr int MATERIAL_REMAPS_NORMAL = (1 << 12);

/// \}

/// \name Distribution Function (DF) Lobes
///
/// \anchor DFLobes
/// One bit per **lobe**: a domain (which side of the surface the lobe
/// sends light to) paired with a kind (what structure it has to sample).
/// The six partition everything the library can build, so a lobe word is
/// the SET of lobes present and a lobe mask is a set of lobes wanted, in
/// one vocabulary. `|` unions, `&` intersects, and a lobe is live iff the
/// two sets share a bit.
///
/// A bit is a lobe and not a distribution type: one `bsdf` struct spans
/// as many bits as it has ways to scatter, and a whole tree unions into
/// one word.
///
/// The pairs rather than the two axes separately are what a material
/// reports, because `JIT::MaterialDef::Eval::surfaceLobes` unions over a
/// whole BSDF tree. Two axes OR'd together lose which domain went with which
/// kind: a Dirac reflection over a diffuse transmission and a Dirac
/// transmission over a diffuse reflection would report identical words,
/// and only the second has a Dirac transmission to refract through.
///
/// \{

/// A reflective lobe whose density is a smooth function of direction,
/// with no normal distribution behind it: diffuse, sheen, micrograin and
/// Hapke lobes among them. It can be sampled and evaluated by direction,
/// but has no half vector to constrain.
///
/// The energy-compensation lobe of a rough BSDF is one of these, so a
/// glossy BSDF that carries one reports both kinds and a mask cuts
/// between them. See `DF_GLOSSY_BRDF`.
inline constexpr int DF_SMOOTH_BRDF = (1 << 0);

/// The transmissive counterpart of `DF_SMOOTH_BRDF`.
inline constexpr int DF_SMOOTH_BTDF = (1 << 3);

/// Every smooth lobe of either domain.
inline constexpr int DF_SMOOTH = DF_SMOOTH_BRDF | DF_SMOOTH_BTDF;

/// A reflective lobe with a sampleable normal distribution, so a half
/// vector is a meaningful quantity of it and a manifold constraint can be
/// solved through it.
///
/// This holds of the whole lobe and not merely of most of it, which is
/// why the Kulla-Conty style compensation lobe that a rough BSDF adds to
/// make up its energy deficit is `DF_SMOOTH_BRDF` rather than part of
/// this: it is a cosine hemisphere with no normal distribution behind it,
/// and a caller that asks for a half vector must not be handed a lobe
/// that has none.
///
/// Having a normal distribution is necessary and not sufficient. A lobe
/// that mixes one with something else, or whose half vector nothing would
/// ever want to constrain, belongs in `DF_SMOOTH_BRDF`; the micrograin
/// layer is both and is classified there.
inline constexpr int DF_GLOSSY_BRDF = (1 << 1);

/// The transmissive counterpart of `DF_GLOSSY_BRDF`.
inline constexpr int DF_GLOSSY_BTDF = (1 << 4);

/// Every normal-distribution lobe of either domain.
inline constexpr int DF_GLOSSY = DF_GLOSSY_BRDF | DF_GLOSSY_BTDF;

/// A reflective Dirac delta lobe, which has no density and whose half
/// vector is fixed by the geometry.
inline constexpr int DF_DIRAC_BRDF = (1 << 2);

/// The transmissive counterpart of `DF_DIRAC_BRDF`.
inline constexpr int DF_DIRAC_BTDF = (1 << 5);

/// Every Dirac lobe of either domain.
inline constexpr int DF_DIRAC = DF_DIRAC_BRDF | DF_DIRAC_BTDF;

/// Every lobe with a density, which is every lobe but the Dirac ones.
/// This is the question a caller asks to find out whether a vertex can
/// scatter a direction that another strategy could also have produced.
inline constexpr int DF_FINITE = DF_SMOOTH | DF_GLOSSY;

/// Every reflective lobe.
inline constexpr int DF_BRDF = DF_SMOOTH_BRDF | DF_GLOSSY_BRDF | DF_DIRAC_BRDF;

/// Every transmissive lobe.
inline constexpr int DF_BTDF = DF_SMOOTH_BTDF | DF_GLOSSY_BTDF | DF_DIRAC_BTDF;

/// Every lobe, which is the lobe mask of a caller that wants the whole
/// distribution.
inline constexpr int DF_ALL = DF_BRDF | DF_BTDF;

/// Property bit riding above the lobes in the same word: some node in
/// the scattering tree was given a `normal` that is, at the evaluation
/// reporting the bit, actually different from the state's shading
/// normal. Such a node's lobes scatter about that normal rather than
/// `geometry.normal`, which is what a manifold estimator solves against,
/// so an estimator refuses trees reporting this bit.
///
/// A node whose `normal` is left defaulted inherits the normal already
/// active where it sits, which is `geometry.normal` unless an enclosing
/// node overrode it, so it detaches from nothing and reports neither
/// this bit nor `DF_CAN_SET_NORMAL`.
///
/// NOT a lobe and NOT in `DF_ALL`: a lobe mask never names it, so it
/// cannot select anything, and it survives only in `surfaceLobes`
/// and `backfaceLobes`.
inline constexpr int DF_SETS_NORMAL = (1 << 6);

/// Property bit like `DF_SETS_NORMAL`: some node in the tree was given a
/// `normal` at all, whether or not it currently differs from the state's
/// shading normal. A given normal that equals the state normal still
/// detaches its node from a REMAPPED `geometry.normal`, so when a
/// material also remaps the field (see `MATERIAL_REMAPS_NORMAL`) an
/// estimator refuses that combination by this bit.
inline constexpr int DF_CAN_SET_NORMAL = (1 << 7);

/// \}

/// Just-in-time interfaces.
namespace JIT {

template <typename> struct Function;

/// A just-in-time SMDL function.
template <typename Result, typename... Args>
struct Function<Result(Args...)> final {
public:
  /// The function pointer type.
  using FunctionPointer = Result (*)(Args...);

  Function() = default;

  Function(std::string name) : name(std::move(name)) {}

  /// Invoke the function.
  SMDL_ALWAYS_INLINE Result operator()(Args... args) const {
    return func(args...);
  }

  /// Is non-null?
  [[nodiscard]] SMDL_ALWAYS_INLINE operator bool() const noexcept {
    return func;
  }

public:
  /// The name used to look up the function in the JIT runtime.
  std::string name{};

  /// The function pointer.
  FunctionPointer func{};
};

/// A just-in-time compiled SMDL material definition: what the material
/// was compiled to, being its entry points and what is statically known
/// about it. The `Compiler` owns it and the next `compile()` invalidates
/// it. Evaluating one at a shading point yields a `Material`.
///
/// An entry point that takes an `Eval` takes it first, followed by exactly
/// the parameters of the `Material` function that wraps it, in the same
/// order. Every `float *` spectrum points to `Compiler::wavelengthBaseMax`
/// floats.
struct MaterialDef final {
public:
  /// The module name.
  std::string moduleName{};

  /// The module file name. This is empty if the module has no file, as
  /// is the case for builtin modules and modules supplied as source
  /// code (see `Compiler::addCode()`).
  std::string moduleFileName{};

  /// The module name to print in diagnostics, which is the file name
  /// for ordinary modules and origin markup for the others. See
  /// `Module::getDisplayName()`.
  std::string moduleDisplayName{};

  /// The line number.
  uint32_t lineNo{};

  /// The material name.
  std::string materialName{};

  /// The qualified material name formed from the module's qualified
  /// name, the enclosing `namespace` names if any, and the material
  /// name, e.g., `::vendor::metals::steel::brushed`. This is the name
  /// that `Compiler::findMaterial()` matches against.
  std::string qualifiedName{};

  /// Is the containing module shadowed by an equally named module under
  /// an earlier search root? If so, `Compiler::findMaterial()` never
  /// matches this material. See `Module::isShadowed()`.
  bool moduleIsShadowed{};

  /// The values of the flag bits that are compile-time constants for
  /// every possible evaluation of this material. This is a subset of
  /// `staticFlagsKnown`; for every evaluation,
  /// `(eval.flags & staticFlagsKnown) == staticFlags`.
  int staticFlags{};

  /// The mask of flag bits whose values are compile-time constants.
  ///
  /// The `MATERIAL_HAS_*` bits derived from `#isDefault` are always
  /// known. `MATERIAL_THIN_WALLED` and `MATERIAL_HAS_CUTOUT` are known
  /// iff their initializers constant-fold after optimization, so they
  /// degrade to unknown at `OPT_LEVEL_NONE`; an unknown bit must be
  /// treated conservatively (e.g., possibly transparent).
  /// `MATERIAL_TRANSPORT_IMPORTANCE` is never known because it mirrors
  /// the `State::transport` each evaluation is made with.
  ///
  /// \note
  /// Like the entry points themselves, static flags describe the
  /// material with its parameters bound to their defaults.
  ///
  int staticFlagsKnown{};

  /// Provably opaque: the cutout opacity is the compile-time constant 1
  /// and the material is not a null interface, so a hit on it blocks a
  /// shadow query outright, with no material work. Nothing to sample,
  /// nothing to pass through: neither `opacityEvaluate` nor an evaluation
  /// is ever needed at it. This is a statement about hits, not
  /// interiors: a shadow segment that STARTS inside this material's
  /// volume still integrates that medium, and `hasVolume()` is that
  /// question.
  [[nodiscard]] bool isAlwaysOpaque() const noexcept {
    return (staticFlagsKnown & MATERIAL_HAS_CUTOUT) != 0 &&
           (staticFlags & MATERIAL_HAS_CUTOUT) == 0 && !isNullInterface();
  }

  /// Has a non-default `volume` initializer? Always statically known.
  [[nodiscard]] bool hasVolume() const noexcept {
    return (staticFlags & MATERIAL_HAS_VOLUME) != 0;
  }

  /// Has a non-default `hair` initializer? Always statically known.
  [[nodiscard]] bool hasHair() const noexcept {
    return (staticFlags & MATERIAL_HAS_HAIR) != 0;
  }

  /// Is this a null interface? (a boundary that scatters nothing itself
  /// but encloses a participating medium)
  [[nodiscard]] bool isNullInterface() const noexcept {
    return hasVolume() &&
           (staticFlags & (MATERIAL_HAS_SURFACE | MATERIAL_HAS_BACKFACE)) == 0;
  }

  /// Provably point-independent coefficients: the volume coefficients
  /// read nothing of the `State` that varies from point to point inside a
  /// medium instance, so the coefficient spectra captured by the `Eval`
  /// at the boundary are exact everywhere in the interior and
  /// `volumeEvaluate` never needs to be called. When this returns false
  /// the coefficients are heterogeneous *or unproven*, and hosts must
  /// treat them as heterogeneous: sample the interior through
  /// `volumeEvaluate` against the majorants (see
  /// `Eval::maxScatteringCoefficient`).
  ///
  /// Point-independent is not state-independent. The coefficients may
  /// still depend on the render-wide fields the instance was evaluated
  /// with: the wavelength grid and its weights (an RGB or spectral
  /// constant is resampled onto `State::wavelengthBase`), the units, the
  /// animation time, the object transform, and the transport mode. These
  /// are the same at every point of one path inside one instance, which
  /// is what the proof promises; a host that evaluates an instance with
  /// other render fields than a path's own (a per-sample wavelength
  /// grid, say) must evaluate it again for that path.
  [[nodiscard]] bool hasHomogeneousCoefficients() const noexcept {
    return (staticFlagsKnown & MATERIAL_HAS_HETEROGENEOUS_COEFFICIENTS) != 0 &&
           (staticFlags & MATERIAL_HAS_HETEROGENEOUS_COEFFICIENTS) == 0;
  }

  /// Provably undisplaced: `geometry.displacement` is the compile-time
  /// constant zero vector, so hosts that apply displacement to geometry
  /// at load time may skip this material without evaluating anything.
  /// When this returns false the displacement is non-zero *or unproven*,
  /// and hosts must query it per point through `displacementEvaluate`.
  [[nodiscard]] bool hasZeroDisplacement() const noexcept {
    return (staticFlagsKnown & MATERIAL_HAS_DISPLACEMENT) != 0 &&
           (staticFlags & MATERIAL_HAS_DISPLACEMENT) == 0;
  }

  /// Possibly remaps `geometry.normal`: the material's shading normal is
  /// not provably the state's own. When this returns true the remap is
  /// real *or unproven*, and a host that differentiates the shading
  /// normal field must read the field itself through
  /// `geometryNormalEvaluate`; reading it for a material that turns out
  /// not to remap returns the state normal and costs only the query.
  [[nodiscard]] bool canRemapNormal() const noexcept {
    return (staticFlagsKnown & MATERIAL_REMAPS_NORMAL) == 0 ||
           (staticFlags & MATERIAL_REMAPS_NORMAL) != 0;
  }

  /// The definition evaluated at one shading point: the record the JIT
  /// writes through `evaluate`, laid out to match the builtin
  /// `_MaterialEval` struct field for field. Hosts hold a `Material`
  /// rather than reaching in here.
  ///
  /// Every `const float *` spectrum is null or points to `wavelengthCount`
  /// floats.
  struct Eval final {
  public:
    /// Is null?
    [[nodiscard]] SMDL_ALWAYS_INLINE bool operator!() const noexcept {
      return ptr == nullptr;
    }

    /// Is non-null?
    [[nodiscard]] SMDL_ALWAYS_INLINE operator bool() const noexcept {
      return ptr != nullptr;
    }

  public:
    /// The material deep copied with `#bump()`, which is opaque to the host.
    const void *ptr{};

    /// The builtin `material_geometry` struct, field for field.
    struct Geometry final {
      /// The displacement vector.
      const float3 displacement{};

      /// The cutout opacity.
      const float cutoutOpacity{};

      /// The normal.
      const float3 normal{};
    };

    /// The geometry.
    const Geometry *geometry{};

    /// The index of refraction.
    float ior{};

    /// The exterior index of refraction, being the absolute index of the
    /// medium on the front side of the geometry. Initialized to 1 by
    /// `evaluate` and meant to be overwritten by hosts that track nested
    /// dielectrics. The relative ratio the scattering calculations refract
    /// with is `exteriorIOR / ior`.
    float exteriorIOR{};

    /// The temperature in Kelvin or -1 if undefined.
    float temperature{};

    /// The volume absorption coefficient if applicable, in units of
    /// inverse meters per the MDL specification: hosts working in scene
    /// units convert distances with `State::metersPerSceneUnit`
    /// before exponentiating.
    ///
    /// \note
    /// This is the coefficient expression at the surface hit. For
    /// heterogeneous coefficients (see
    /// `MaterialDef::hasHomogeneousCoefficients()`), interior sampling
    /// must go through `MaterialDef::volumeEvaluate` instead.
    ///
    const float *absorptionCoefficient{};

    /// The volume scattering coefficient if applicable, in units of
    /// inverse meters. See `absorptionCoefficient` for the
    /// heterogeneous-volume caveat.
    const float *scatteringCoefficient{};

    /// The volume absorption coefficient majorant if declared, in units
    /// of inverse meters: an author-declared, position-independent
    /// upper bound of `absorptionCoefficient` over the whole interior
    /// (the SMDL extension field
    /// `material_volume.max_absorption_coefficient`).
    const float *maxAbsorptionCoefficient{};

    /// The volume scattering coefficient majorant if declared, in units
    /// of inverse meters, see `maxAbsorptionCoefficient`. This is
    /// what null-collision tracking through a heterogeneous interior
    /// runs against.
    const float *maxScatteringCoefficient{};

    /// The `smdl::VoxelGrid` behind the volume density acceleration
    /// hint if declared (the SMDL extension field
    /// `material_volume.density`), else null. Together with the bound
    /// box below, this promises the coefficients at any interior point
    /// are bounded by the majorants scaled by the grid's trilinear
    /// value there over its maximum, so renderers may track against
    /// per-region majorants from the grid's per-brick bounds and skip
    /// empty regions. See `Material::getVolumeDensityGrid()`.
    const void *volumeDensityResource{};

    /// The object-space lower corner of the box that the density
    /// hint's texture space spans, if declared. If non-null, points to
    /// one `float3`.
    const float3 *volumeDensityBoundMin{};

    /// The object-space upper corner, see `volumeDensityBoundMin`.
    const float3 *volumeDensityBoundMax{};

    /// The volumetric emission coefficient if declared (MDL 1.8
    /// `material_volume.emission_intensity`): the radiance the medium
    /// adds per unit length, in `W/(m^2 sr nm)` per meter, converted
    /// with `State::metersPerSceneUnit` like the scattering
    /// coefficients. Evaluated at the surface hit; heterogeneous
    /// interiors re-query per point through `MaterialDef::volumeEvaluate`.
    const float *volumeEmissionIntensity{};

    /// The `surface` emission intensity, or null if the `surface` has no
    /// non-default emission EDF.
    const float *surfaceEmissionIntensity{};

    /// The `backface` emission intensity, or null if the `backface` has no
    /// non-default emission EDF.
    const float *backfaceEmissionIntensity{};

    /// The wavelength count.
    int wavelengthCount{};

    /// The flags.
    int flags{};

    /// The set of lobes (\ref DFLobes "the `DF_` lobes") present
    /// anywhere in the material `surface` scattering tree.
    ///
    /// This is a union over the tree, so it answers "could this material
    /// do that" and never "will this query do that". It is exact about
    /// which domain goes with which kind, which is what makes
    /// `surfaceLobes & DF_DIRAC_BTDF` a sound test for an interface
    /// a manifold walk can refract through.
    ///
    /// One distribution can contribute more than one bit: a rough BSDF
    /// with an energy-compensation lobe is `DF_GLOSSY_BRDF` and
    /// `DF_SMOOTH_BRDF` together, since the two parts are different kinds
    /// on the same domain.
    ///
    /// The word also carries the normal property bits `DF_SETS_NORMAL`
    /// and `DF_CAN_SET_NORMAL`, which are not lobes; mask with `DF_ALL`
    /// where only the lobes are wanted.
    int surfaceLobes{};

    /// The `backface` counterpart of `surfaceLobes`.
    int backfaceLobes{};

    /// The emission intensity modes: bit 0 is set if the `surface` emission
    /// intensity is `intensity_power` (as opposed to the default
    /// `intensity_radiant_exitance`), and bit 1 likewise for the `backface`.
    int emissionModes{};

    /// The random seed captured from the raw state of `State::rng` at
    /// evaluation, which seeds the generator for stochastically evaluated
    /// BSDFs.
    int64_t seed{};

    /// The tangent-to-world space matrix at evaluation.
    float3x3 tangentToWorld{};
  };

  /// The evaluate function.
  ///
  /// \param[inout] state  The state.
  /// \param[out]   eval   The evaluated material.
  ///
  /// This uses the `state.allocator` to allocate an `Eval` that must be
  /// passed to all scattering calculations.
  ///
  /// \note
  /// After the host obtains an `Eval`, the `State` can be dropped.
  ///
  Function<void(State &state, Eval &eval)> evaluate{};

  /// The opacity evaluate function.
  ///
  /// \param[inout] state  The state.
  ///
  /// \return `geometry.cutout_opacity`.
  ///
  /// Evaluates only `geometry.cutout_opacity` and dead-code eliminates the
  /// rest: no evaluation is constructed and nothing is allocated, so
  /// `state.allocator` may be null. This is the cheap path for shadow and
  /// transmission rays against materials that are not `isAlwaysOpaque()`
  /// (a null interface needs no opacity either, it passes through
  /// unconditionally).
  ///
  Function<float(State &state)> opacityEvaluate{};

  /// The displacement evaluate function.
  ///
  /// \param[inout] state         The state.
  /// \param[out]   displacement  The displacement vector in internal space.
  ///
  /// Evaluates only `geometry.displacement` and dead-code eliminates the
  /// rest: no evaluation is constructed and nothing is allocated, so
  /// `state.allocator` may be null. This is the per-vertex query for hosts
  /// that apply displacement to geometry at load time; see
  /// `hasZeroDisplacement()` for skipping materials that provably never
  /// displace.
  ///
  Function<void(State &state, float3 &displacement)> displacementEvaluate{};

  /// The volume evaluate function.
  ///
  /// \param[inout] state     The state.
  /// \param[out]   sigmaA    The absorption coefficient in inverse meters.
  /// \param[out]   sigmaS    The scattering coefficient in inverse meters.
  /// \param[out]   emission  The emission coefficient in radiance per meter.
  ///
  /// Evaluates only the volume coefficients and dead-code eliminates the
  /// rest: no evaluation is constructed and nothing is allocated, so
  /// `state.allocator` may be null. A coefficient the material does not
  /// declare comes back zero. This is the per-point query that
  /// null-collision tracking makes at every tentative collision inside a
  /// heterogeneous medium; for provably point-independent coefficients
  /// (`hasHomogeneousCoefficients()`) the coefficient pointers of an
  /// `Eval` answer the same question with no call at all.
  ///
  /// \note
  /// The state is a partial state in the sense of an environment
  /// lookup: the caller fills `position` with the query point in the
  /// *object space* of the volume instance (internal space equals
  /// object space here, there being no surface frame; do NOT call
  /// `State::finalize()`), along with the render-wide fields
  /// (`wavelengthBase`, ...), and may leave the surface-geometry
  /// fields defaulted. Volume expressions read the point through
  /// `state::position()`.
  ///
  Function<void(State &state, float *sigmaA, float *sigmaS, float *emission)>
      volumeEvaluate{};

  /// The scatter evaluate function.
  ///
  /// \param[in]  eval      The evaluated material.
  /// \param[in]  wo        The outgoing direction in world space.
  /// \param[in]  wi        The incoming direction in world space.
  /// \param[out] pdfFwd    The solid-angle PDF of sampling `wi` given `wo`.
  /// \param[out] pdfRev    The solid-angle PDF of sampling `wo` given `wi`.
  /// \param[out] f         The scattering spectrum.
  /// \param[in]  lobeMask  The lobes to consider, `DF_ALL` is every lobe.
  ///
  /// \return `true` if the result is non-zero.
  ///
  Function<int(const Eval &eval, const float3 &wo, const float3 &wi,
               float &pdfFwd, float &pdfRev, float *f, int lobeMask)>
      scatterEvaluate{};

  /// The scatter sample function.
  ///
  /// \param[in]  eval        The evaluated material.
  /// \param[in]  xi          The canonical random sample.
  /// \param[in]  wo          The outgoing direction in world space.
  /// \param[out] wi          The incoming direction in world space.
  /// \param[out] pdfFwd      The solid-angle PDF of sampling `wi` given `wo`.
  /// \param[out] pdfRev      The solid-angle PDF of sampling `wo` given `wi`.
  /// \param[out] f           The scattering spectrum.
  /// \param[out] lobe        The sampled lobe, exactly one bit, or `0` if none.
  /// \param[in]  lobeMask    The lobes to consider, `DF_ALL` is every lobe.
  /// \param[out] lobeChance  The probability that an **unmasked** sample would
  ///                         have resulted in the same selection.
  ///
  /// \return `true` if the result is non-zero.
  ///
  /// `lobe & DF_DIRAC` is the Dirac test, and a Dirac lobe has no density,
  /// so for one both PDFs are 1.
  ///
  /// A masked call renormalizes its own chances over the lobes the mask
  /// keeps, so `lobeChance` is the only way back to what the whole BSDF
  /// would have done. A caller weighing a masked result against a strategy
  /// that samples the whole BSDF needs it, and for a Dirac lobe there is
  /// no other source, since `scatterEvaluate` reports zero there at
  /// every mask. Exactly 1 when nothing chose.
  ///
  Function<int(const Eval &eval, const float4 &xi, const float3 &wo, float3 &wi,
               float &pdfFwd, float &pdfRev, float *f, int &lobe, int lobeMask,
               float &lobeChance)>
      scatterSample{};

  /// The scatter normal evaluate function.
  ///
  /// \param[in]  eval        The evaluated material.
  /// \param[in]  isBackface  Whether to ask on the backface side.
  /// \param[in]  wm          The microfacet normal in world space.
  /// \param[out] pdf         The solid-angle PDF of sampling `wm`.
  /// \param[in]  lobeMask    The lobes to consider, `DF_GLOSSY` is every lobe.
  ///
  /// \return `true` if the PDF is non-zero.
  ///
  /// The density with which `scatterNormalSample` draws `wm` on the same
  /// side. See it for the contract.
  ///
  /// \note
  /// Null unless `Compiler::shouldEmitScatterNormal` was set before
  /// `compile()`.
  ///
  Function<int(const Eval &eval, int isBackface, const float3 &wm, float &pdf,
               int lobeMask)>
      scatterNormalEvaluate{};

  /// The scatter normal sample function.
  ///
  /// \param[in]  eval        The evaluated material.
  /// \param[in]  xi          The canonical random sample.
  /// \param[in]  isBackface  Whether to ask on the backface side.
  /// \param[out] wm          The microfacet normal in world space.
  /// \param[out] pdf         The solid-angle PDF of sampling `wm`.
  /// \param[out] alpha       The squared roughness of the lobe drawn from.
  /// \param[in]  lobeMask    The lobes to consider, `DF_GLOSSY` is every lobe.
  ///
  /// \return `true` if a lobe with a normal distribution was reached.
  ///
  /// Draws a microfacet normal from the normal distribution behind one
  /// GLOSSY lobe, which is what `DF_GLOSSY_BRDF` promises exists and this
  /// is how a caller reaches. A host solving a manifold constraint through
  /// a rough interface needs a half vector it can draw and weigh; this and
  /// `scatterNormalEvaluate` are that, and nothing more. What such an
  /// estimator is worth is `scatterEvaluate` at the directions the
  /// constraint resolves to.
  ///
  /// A normal query carries no outgoing direction, so the side is an
  /// input: the caller asks about one side of one crossing and already
  /// knows which, and `wm` comes back on that side. `lobeMask` is
  /// intersected with `DF_GLOSSY`, since nothing else has a normal
  /// distribution to report, and selection chances are renormalized over
  /// the lobes that survive, so a mask naming one interface's transmissive
  /// lobe draws that lobe's own distribution however the tree layers it. A
  /// manifold estimator wants exactly one kind, `DF_GLOSSY_BRDF` or
  /// `DF_GLOSSY_BTDF`: a two-domain mask reports the mixture of the
  /// distributions on both sides of the interface, which is not a
  /// distribution any single crossing scatters by, and the `Material`
  /// wrapper refuses it.
  ///
  /// This draws the normal distribution itself and not the part of it any
  /// direction can see, which is deliberate: the visible form is the
  /// better proposal for a scattering event and the wrong one for a
  /// constraint, since it would make the draw depend on a direction a
  /// solve then changes.
  ///
  /// `pdf` is mixed over every lobe the mask keeps that could have
  /// produced `wm`, and is exactly what `scatterNormalEvaluate` reports at
  /// the same directions. That identity is the one property a caller's
  /// correctness may rest on: it is what makes the pair a usable proposal.
  /// It is NOT in general the density `scatterEvaluate` divides out,
  /// though the microfacet lobes match that too. `alpha` lets a host
  /// decide whether an interface is smooth enough to be worth constraining
  /// without a second query.
  ///
  /// \note
  /// Null unless `Compiler::shouldEmitScatterNormal` was set before
  /// `compile()`. A host that never asks pays nothing for these.
  ///
  Function<int(const Eval &eval, const float4 &xi, int isBackface, float3 &wm,
               float &pdf, float2 &alpha, int lobeMask)>
      scatterNormalSample{};

  /// The geometry normal evaluate function.
  ///
  /// \param[inout] state   The state.
  /// \param[out]   normal  The normal in internal space.
  ///
  /// Evaluates only `geometry.normal` and dead-code eliminates the rest: no
  /// evaluation is constructed and nothing is allocated, so
  /// `state.allocator` may be null. This is the query for a host that must
  /// read or differentiate the shading normal field a material remaps (see
  /// `canRemapNormal()`), a manifold walk over a normal-mapped caster being
  /// the motivating case: the compiler does not differentiate materials, so
  /// such a host evaluates the field at perturbed surface parameterizations
  /// and differences it.
  ///
  /// \note
  /// Null unless `Compiler::shouldEmitScatterNormal` was set before
  /// `compile()`, like the two normal distribution hooks above.
  ///
  Function<void(State &state, float3 &normal)> geometryNormalEvaluate{};

  /// The emission evaluate function.
  ///
  /// \param[in]  eval  The evaluated material.
  /// \param[in]  we    The emission direction in world space, pointing away.
  /// \param[out] pdf   The solid-angle PDF of sampling `we`.
  /// \param[out] Le    The emission spectrum.
  ///
  /// \return `true` if the result is non-zero.
  ///
  /// \note
  /// The radiance is `material_emission.intensity` times the normalized
  /// EDF, which is the physical radiance when the intensity mode is
  /// `intensity_radiant_exitance`. When the mode is `intensity_power`
  /// (see `Eval::emissionModes`), the host must additionally divide
  /// by the total emitting surface area.
  ///
  /// \note
  /// Solid materials emit only on the exterior side of the geometry.
  /// Thin-walled materials emit `surface.emission` on the front side and
  /// `backface.emission`, if the backface is non-default, on the back
  /// side, else `surface.emission` mirrored.
  ///
  Function<int(const Eval &eval, const float3 &we, float &pdf, float *Le)>
      emissionEvaluate{};

  /// The emission sample function.
  ///
  /// \param[in]  eval  The evaluated material.
  /// \param[in]  xi    The canonical random sample.
  /// \param[out] we    The emission direction in world space, pointing away.
  /// \param[out] pdf   The solid-angle PDF of sampling `we`.
  /// \param[out] Le    The emission spectrum.
  ///
  /// \return `true` if the result is non-zero.
  ///
  Function<int(const Eval &eval, const float4 &xi, float3 &we, float &pdf,
               float *Le)>
      emissionSample{};

  /// The volume scatter evaluate function.
  ///
  /// \param[in] eval  The evaluated material.
  /// \param[in] wo    The outgoing direction in world space.
  /// \param[in] wi    The incoming direction in world space.
  ///
  /// \return The phase function, or zero if the material has no volume
  /// scattering. It is normalized over the sphere and so is also the
  /// solid-angle PDF of `volumeScatterSample`.
  ///
  Function<float(const Eval &eval, const float3 &wo, const float3 &wi)>
      volumeScatterEvaluate{};

  /// The volume scatter sample function.
  ///
  /// \param[in]  eval  The evaluated material.
  /// \param[in]  xi    The canonical random sample.
  /// \param[in]  wo    The outgoing direction in world space.
  /// \param[out] wi    The incoming direction in world space.
  ///
  /// \return The phase function at `wi`, or zero if the material has no
  /// volume scattering. It is also the solid-angle PDF of having sampled
  /// `wi`, so the implied throughput weight is always 1.
  ///
  Function<float(const Eval &eval, const float4 &xi, const float3 &wo,
                 float3 &wi)>
      volumeScatterSample{};

  /// The hair scatter evaluate function.
  ///
  /// \param[in]  eval    The evaluated material.
  /// \param[in]  wo      The outgoing direction in world space.
  /// \param[in]  wi      The incoming direction in world space.
  /// \param[out] pdfFwd  The solid-angle PDF of sampling `wi` given `wo`.
  /// \param[out] pdfRev  The solid-angle PDF of sampling `wo` given `wi`.
  /// \param[out] f       The scattering spectrum.
  ///
  /// \return `true` if the result is non-zero.
  ///
  /// This dispatches `material.hair`.
  ///
  /// \note
  /// The state contract at a hair hit: `State::normal` must be the
  /// shading normal on the fiber surface (the true normal of a tube;
  /// ribbon geometry must synthesize it) and `State::textureTangentU[0]`
  /// must be the fiber tangent pointing root to tip. The BSDF implies the
  /// cross-section offset from the normal as `h = sin(gamma_o)` and reads
  /// no texture coordinate. Per the MDL specification, the material `ior`,
  /// `thin_walled`, `volume`, and `geometry` fields do not influence hair
  /// shading, and `wo` on the far side of the normal plane is a
  /// legitimate configuration rather than a backface hit. Hosts should
  /// gate calls on `hasHair()`; calling anyway is safe because the
  /// default `hair_bsdf()` reports black.
  ///
  Function<int(const Eval &eval, const float3 &wo, const float3 &wi,
               float &pdfFwd, float &pdfRev, float *f)>
      hairScatterEvaluate{};

  /// The hair scatter sample function.
  ///
  /// \param[in]  eval    The evaluated material.
  /// \param[in]  xi      The canonical random sample.
  /// \param[in]  wo      The outgoing direction in world space.
  /// \param[out] wi      The incoming direction in world space.
  /// \param[out] pdfFwd  The solid-angle PDF of sampling `wi` given `wo`.
  /// \param[out] pdfRev  The solid-angle PDF of sampling `wo` given `wi`.
  /// \param[out] f       The scattering spectrum.
  ///
  /// \return `true` if the result is non-zero.
  ///
  /// \note
  /// See `hairScatterEvaluate` for the state contract at a hair hit. There
  /// are no Dirac hair distributions and no lobe taxonomy for hair, so this
  /// reports neither a sampled lobe nor a Dirac flag.
  ///
  Function<int(const Eval &eval, const float4 &xi, const float3 &wo, float3 &wi,
               float &pdfFwd, float &pdfRev, float *f)>
      hairScatterSample{};
};

/// A material definition together with an evaluation of it at one shading
/// point, which is what the host holds and queries for scattering, volume,
/// emission, etc.
///
/// Each scattering and emission function wraps the like-named `MaterialDef`
/// entry point with `eval` bound. Every `Span<float>` spectrum must have
/// `eval.wavelengthCount` elements.
struct Material final {
public:
  Material() = default;

  /// Allocate and initialize from the given state and definition.
  explicit Material(State &state, const MaterialDef *def) : def(def) {
    SMDL_SANITY_CHECK(def);
    SMDL_DEBUG_CHECK(state.allocator);
    def->evaluate(state, eval);
    SMDL_SANITY_CHECK(eval);
  }

  /// The cutout opacity.
  [[nodiscard]] SMDL_ALWAYS_INLINE float getCutoutOpacity() const noexcept {
    return eval.geometry->cutoutOpacity;
  }

  /// Is thin-walled?
  [[nodiscard]] SMDL_ALWAYS_INLINE bool isThinWalled() const noexcept {
    return (eval.flags & MATERIAL_THIN_WALLED) != 0;
  }

  /// Has medium properties?
  [[nodiscard]] SMDL_ALWAYS_INLINE bool hasMedium() const noexcept {
    return (eval.absorptionCoefficient != nullptr ||
            eval.scatteringCoefficient != nullptr);
  }

  /// Is the volume declared additive? See `MATERIAL_ADDITIVE_VOLUME`.
  [[nodiscard]] SMDL_ALWAYS_INLINE bool hasAdditiveVolume() const noexcept {
    return (eval.flags & MATERIAL_ADDITIVE_VOLUME) != 0;
  }

  /// Has a non-default `hair` initializer?
  [[nodiscard]] SMDL_ALWAYS_INLINE bool hasHair() const noexcept {
    return (eval.flags & MATERIAL_HAS_HAIR) != 0;
  }

  /// Has a non-default emission EDF in the `surface` initializer?
  [[nodiscard]] SMDL_ALWAYS_INLINE bool hasSurfaceEmission() const noexcept {
    return (eval.flags & MATERIAL_HAS_SURFACE_EMISSION) != 0;
  }

  /// Has a non-default emission EDF in the `backface` initializer?
  [[nodiscard]] SMDL_ALWAYS_INLINE bool hasBackfaceEmission() const noexcept {
    return (eval.flags & MATERIAL_HAS_BACKFACE_EMISSION) != 0;
  }

  /// Has a non-default emission EDF at all?
  [[nodiscard]] SMDL_ALWAYS_INLINE bool hasEmission() const noexcept {
    return hasSurfaceEmission() || hasBackfaceEmission();
  }

  /// The `surface` emission intensity, or empty if the `surface` has no
  /// non-default emission EDF.
  [[nodiscard]] SMDL_ALWAYS_INLINE Span<const float>
  getSurfaceEmissionIntensity() const noexcept {
    return {eval.surfaceEmissionIntensity, size_t(eval.wavelengthCount)};
  }

  /// The `backface` emission intensity, or empty if the `backface` has no
  /// non-default emission EDF.
  [[nodiscard]] SMDL_ALWAYS_INLINE Span<const float>
  getBackfaceEmissionIntensity() const noexcept {
    return {eval.backfaceEmissionIntensity, size_t(eval.wavelengthCount)};
  }

  /// Is the `surface` emission intensity in units of power (watts) as
  /// opposed to radiant exitance (watts per square meter)? If so, the host
  /// must divide emitted radiance by the total emitting surface area.
  [[nodiscard]] SMDL_ALWAYS_INLINE bool
  isSurfaceEmissionPower() const noexcept {
    return (eval.emissionModes & 1) != 0;
  }

  /// Is the `backface` emission intensity in units of power (watts) as
  /// opposed to radiant exitance (watts per square meter)?
  [[nodiscard]] SMDL_ALWAYS_INLINE bool
  isBackfaceEmissionPower() const noexcept {
    return (eval.emissionModes & 2) != 0;
  }

  /// The index of refraction.
  [[nodiscard]] SMDL_ALWAYS_INLINE float getIOR() const noexcept {
    return eval.ior;
  }

  /// The exterior index of refraction, i.e., of the medium surrounding
  /// the object on the front side of the geometry. Defaults to 1.
  [[nodiscard]] SMDL_ALWAYS_INLINE float getExteriorIOR() const noexcept {
    return eval.exteriorIOR;
  }

  /// Set the exterior index of refraction. Hosts that track nested
  /// dielectrics call this after construction and before the scattering
  /// functions, passing the index of the medium surrounding the object.
  SMDL_ALWAYS_INLINE void setExteriorIOR(float exteriorIOR) noexcept {
    eval.exteriorIOR = exteriorIOR;
  }

  /// The absorption coefficient of the medium, or empty if none.
  [[nodiscard]] SMDL_ALWAYS_INLINE Span<const float>
  getAbsorptionCoefficient() const noexcept {
    return {eval.absorptionCoefficient, size_t(eval.wavelengthCount)};
  }

  /// The scattering coefficient of the medium, or empty if none.
  [[nodiscard]] SMDL_ALWAYS_INLINE Span<const float>
  getScatteringCoefficient() const noexcept {
    return {eval.scatteringCoefficient, size_t(eval.wavelengthCount)};
  }

  /// The declared absorption coefficient majorant, or empty if none.
  [[nodiscard]] SMDL_ALWAYS_INLINE Span<const float>
  getMaxAbsorptionCoefficient() const noexcept {
    return {eval.maxAbsorptionCoefficient, size_t(eval.wavelengthCount)};
  }

  /// The declared scattering coefficient majorant, or empty if none.
  [[nodiscard]] SMDL_ALWAYS_INLINE Span<const float>
  getMaxScatteringCoefficient() const noexcept {
    return {eval.maxScatteringCoefficient, size_t(eval.wavelengthCount)};
  }

  /// The volume density acceleration hint grid, or null if not
  /// declared. See `Eval::volumeDensityResource`.
  [[nodiscard]]
  SMDL_ALWAYS_INLINE const VoxelGrid *getVolumeDensityGrid() const noexcept {
    return static_cast<const VoxelGrid *>(eval.volumeDensityResource);
  }

  /// The lower corner of the density hint box, or null if not declared.
  [[nodiscard]]
  SMDL_ALWAYS_INLINE const float3 *getVolumeDensityBoundMin() const noexcept {
    return eval.volumeDensityBoundMin;
  }

  /// The upper corner of the density hint box, or null if not declared.
  [[nodiscard]]
  SMDL_ALWAYS_INLINE const float3 *getVolumeDensityBoundMax() const noexcept {
    return eval.volumeDensityBoundMax;
  }

  /// The volumetric emission coefficient, or empty if none.
  [[nodiscard]] SMDL_ALWAYS_INLINE Span<const float>
  getVolumeEmissionIntensity() const noexcept {
    return {eval.volumeEmissionIntensity, size_t(eval.wavelengthCount)};
  }

  /// The geometry normal in world space.
  [[nodiscard]] SMDL_ALWAYS_INLINE float3 getGeometryNormal() const noexcept {
    return eval.tangentToWorld[2];
  }

  /// Is the given direction on the exterior side of the geometry?
  [[nodiscard]]
  SMDL_ALWAYS_INLINE bool isExterior(const float3 &w) const noexcept {
    return dot(getGeometryNormal(), w) > 0.0f;
  }

  /// Is the given direction on the interior side of the geometry?
  [[nodiscard]]
  SMDL_ALWAYS_INLINE bool isInterior(const float3 &w) const noexcept {
    return !isExterior(w);
  }

  /// Is the given pair of directions transmitting through the geometry?
  [[nodiscard]]
  SMDL_ALWAYS_INLINE bool isTransmitting(const float3 &wo,
                                         const float3 &wi) const noexcept {
    return isExterior(wo) != isExterior(wi);
  }

  /// The lobes the material can produce on either side, together with the
  /// normal property bits; mask with `DF_ALL` where only the lobes are wanted.
  [[nodiscard]] SMDL_ALWAYS_INLINE int getLobes() const noexcept {
    return eval.surfaceLobes | eval.backfaceLobes;
  }

  /// The lobes on one side of the interface, together with the normal
  /// property bits: the `backface` scattering tree's when the material
  /// declares a `backface` and the query is on that side, the `surface`
  /// tree's otherwise.
  ///
  /// This is the dispatch the scattering functions make themselves, and
  /// `isBackface` is the side they derive from their outgoing direction,
  /// which a caller spells `isInterior(wo)`, so the word reported
  /// describes the tree they will actually run. A material with no
  /// `backface` initializer scatters by its `surface` from both sides, so
  /// both sides report one word and only a two-sided material
  /// distinguishes.
  [[nodiscard]] SMDL_ALWAYS_INLINE int
  getLobes(bool isBackface) const noexcept {
    return isBackface && (eval.flags & MATERIAL_HAS_BACKFACE) != 0
               ? eval.backfaceLobes
               : eval.surfaceLobes;
  }

  /// The scatter evaluate function.
  ///
  /// \param[in]  wo        The outgoing direction in world space.
  /// \param[in]  wi        The incoming direction in world space.
  /// \param[out] pdfFwd    The solid-angle PDF of sampling `wi` given `wo`.
  /// \param[out] pdfRev    The solid-angle PDF of sampling `wo` given `wi`.
  /// \param[out] f         The scattering spectrum.
  /// \param[in]  lobeMask  The lobes to consider, `DF_ALL` is every lobe.
  ///
  /// \return `true` if the result is non-zero.
  ///
  [[nodiscard]] SMDL_ALWAYS_INLINE bool
  scatterEvaluate(const float3 &wo, const float3 &wi, float &pdfFwd,
                  float &pdfRev, Span<float> f, int lobeMask = DF_ALL) const {
    SMDL_DEBUG_CHECK(def && eval);
    SMDL_DEBUG_CHECK(f.size() == size_t(eval.wavelengthCount));
    return def->scatterEvaluate(eval, wo, wi, pdfFwd, pdfRev, f.data(),
                                lobeMask);
  }

  /// The scatter sample function.
  ///
  /// \param[in]  xi          The canonical random sample.
  /// \param[in]  wo          The outgoing direction in world space.
  /// \param[out] wi          The incoming direction in world space.
  /// \param[out] pdfFwd      The solid-angle PDF of sampling `wi` given `wo`.
  /// \param[out] pdfRev      The solid-angle PDF of sampling `wo` given `wi`.
  /// \param[out] f           The scattering spectrum.
  /// \param[out] lobe        The sampled lobe, exactly one bit, or `0` if none.
  /// \param[in]  lobeMask    The lobes to consider, `DF_ALL` is every lobe.
  /// \param[out] lobeChance  The probability that an **unmasked** sample would
  ///                         have resulted in the same selection, if non-null.
  ///
  /// \return `true` if the result is non-zero.
  ///
  /// See `MaterialDef::scatterSample` for the Dirac PDFs and for what
  /// `lobeChance` is for.
  ///
  [[nodiscard]] SMDL_ALWAYS_INLINE bool
  scatterSample(const float4 &xi, const float3 &wo, float3 &wi, float &pdfFwd,
                float &pdfRev, Span<float> f, int &lobe, int lobeMask = DF_ALL,
                float *lobeChance = nullptr) const {
    SMDL_DEBUG_CHECK(def && eval);
    SMDL_DEBUG_CHECK(f.size() == size_t(eval.wavelengthCount));
    float lobeChanceLocal{1};
    return def->scatterSample(eval, xi, wo, wi, pdfFwd, pdfRev, f.data(), lobe,
                              lobeMask,
                              lobeChance ? *lobeChance : lobeChanceLocal);
  }

  /// The scatter normal evaluate function.
  ///
  /// \param[in]  isBackface  Whether to ask on the backface side.
  /// \param[in]  wm          The microfacet normal in world space.
  /// \param[out] pdf         The solid-angle PDF of sampling `wm`.
  /// \param[in]  lobeMask    The lobes to consider, which must be exactly
  ///                         `DF_GLOSSY_BRDF` or `DF_GLOSSY_BTDF`.
  ///
  /// \return `true` if the PDF is non-zero.
  ///
  /// See `MaterialDef::scatterNormalSample` for the contract. Any other
  /// `lobeMask` reports zero, and a call aborts if the entry point was not
  /// emitted, which is the case unless `Compiler::shouldEmitScatterNormal`
  /// was set before `compile()`.
  ///
  [[nodiscard]] SMDL_ALWAYS_INLINE bool
  scatterNormalEvaluate(bool isBackface, const float3 &wm, float &pdf,
                        int lobeMask) const {
    SMDL_DEBUG_CHECK(def && eval);
    SMDL_SANITY_CHECK_MSG(bool(def->scatterNormalEvaluate),
                          "set 'Compiler::shouldEmitScatterNormal' before "
                          "'compile()' to emit the normal distribution "
                          "entry points");
    // The mask must name exactly one glossy kind: the mixture over both
    // domains is not a distribution any single crossing scatters by, so
    // a manifold estimator must never use it. The raw entry point still
    // reports the mixture for a caller that wants it.
    if (SMDL_UNLIKELY(lobeMask != DF_GLOSSY_BRDF &&
                      lobeMask != DF_GLOSSY_BTDF)) {
      pdf = 0.0f;
      return false;
    } else {
      return def->scatterNormalEvaluate(eval, int(isBackface), wm, pdf,
                                        lobeMask);
    }
  }

  /// The scatter normal sample function.
  ///
  /// \param[in]  xi          The canonical random sample.
  /// \param[in]  isBackface  Whether to ask on the backface side.
  /// \param[out] wm          The microfacet normal in world space.
  /// \param[out] pdf         The solid-angle PDF of sampling `wm`.
  /// \param[out] alpha       The squared roughness of the lobe drawn from.
  /// \param[in]  lobeMask    The lobes to consider, which must be exactly
  ///                         `DF_GLOSSY_BRDF` or `DF_GLOSSY_BTDF`.
  ///
  /// \return `true` if a lobe with a normal distribution was reached.
  ///
  /// See `MaterialDef::scatterNormalSample` for the contract. Any other
  /// `lobeMask` reports zero, and a call aborts if the entry point was not
  /// emitted, which is the case unless `Compiler::shouldEmitScatterNormal`
  /// was set before `compile()`.
  ///
  [[nodiscard]] SMDL_ALWAYS_INLINE bool
  scatterNormalSample(const float4 &xi, bool isBackface, float3 &wm, float &pdf,
                      float2 &alpha, int lobeMask) const {
    SMDL_DEBUG_CHECK(def && eval);
    SMDL_SANITY_CHECK_MSG(bool(def->scatterNormalSample),
                          "set 'Compiler::shouldEmitScatterNormal' before "
                          "'compile()' to emit the normal distribution "
                          "entry points");
    // As in `scatterNormalEvaluate`: exactly one glossy kind.
    if (SMDL_UNLIKELY(lobeMask != DF_GLOSSY_BRDF &&
                      lobeMask != DF_GLOSSY_BTDF)) {
      pdf = 0.0f;
      alpha = {};
      return false;
    } else {
      return def->scatterNormalSample(eval, xi, int(isBackface), wm, pdf, alpha,
                                      lobeMask);
    }
  }

  /// The emission evaluate function.
  ///
  /// \param[in]  we   The emission direction in world space, pointing away.
  /// \param[out] pdf  The solid-angle PDF of sampling `we`.
  /// \param[out] Le   The emission spectrum.
  ///
  /// \return `true` if the result is non-zero.
  ///
  /// See `MaterialDef::emissionEvaluate` for the unit conventions and for
  /// which side of the geometry emits.
  ///
  [[nodiscard]] SMDL_ALWAYS_INLINE bool
  emissionEvaluate(const float3 &we, float &pdf, Span<float> Le) const {
    SMDL_DEBUG_CHECK(def && eval);
    SMDL_DEBUG_CHECK(Le.size() == size_t(eval.wavelengthCount));
    return def->emissionEvaluate(eval, we, pdf, Le.data());
  }

  /// The emission sample function.
  ///
  /// \param[in]  xi   The canonical random sample.
  /// \param[out] we   The emission direction in world space, pointing away.
  /// \param[out] pdf  The solid-angle PDF of sampling `we`.
  /// \param[out] Le   The emission spectrum.
  ///
  /// \return `true` if the result is non-zero.
  ///
  [[nodiscard]] SMDL_ALWAYS_INLINE bool emissionSample(const float4 &xi,
                                                       float3 &we, float &pdf,
                                                       Span<float> Le) const {
    SMDL_DEBUG_CHECK(def && eval);
    SMDL_DEBUG_CHECK(Le.size() == size_t(eval.wavelengthCount));
    return def->emissionSample(eval, xi, we, pdf, Le.data());
  }

  /// The volume scatter evaluate function.
  ///
  /// \param[in] wo  The outgoing direction in world space.
  /// \param[in] wi  The incoming direction in world space.
  ///
  /// \return The phase function, or zero if the material has no volume
  /// scattering. It is normalized over the sphere and so is also the
  /// solid-angle PDF of `volumeScatterSample`.
  ///
  [[nodiscard]] SMDL_ALWAYS_INLINE float
  volumeScatterEvaluate(const float3 &wo, const float3 &wi) const {
    SMDL_DEBUG_CHECK(def && eval);
    return def->volumeScatterEvaluate(eval, wo, wi);
  }

  /// The volume scatter sample function.
  ///
  /// \param[in]  xi  The canonical random sample.
  /// \param[in]  wo  The outgoing direction in world space.
  /// \param[out] wi  The incoming direction in world space.
  ///
  /// \return The phase function at `wi`, or zero if the material has no
  /// volume scattering. It is also the solid-angle PDF of having sampled
  /// `wi`, so the implied throughput weight is always 1.
  ///
  [[nodiscard]] SMDL_ALWAYS_INLINE float
  volumeScatterSample(const float4 &xi, const float3 &wo, float3 &wi) const {
    SMDL_DEBUG_CHECK(def && eval);
    return def->volumeScatterSample(eval, xi, wo, wi);
  }

  /// The hair scatter evaluate function.
  ///
  /// \param[in]  wo      The outgoing direction in world space.
  /// \param[in]  wi      The incoming direction in world space.
  /// \param[out] pdfFwd  The solid-angle PDF of sampling `wi` given `wo`.
  /// \param[out] pdfRev  The solid-angle PDF of sampling `wo` given `wi`.
  /// \param[out] f       The scattering spectrum.
  ///
  /// \return `true` if the result is non-zero.
  ///
  /// See `MaterialDef::hairScatterEvaluate` for the state contract at a
  /// hair hit.
  ///
  [[nodiscard]] SMDL_ALWAYS_INLINE bool
  hairScatterEvaluate(const float3 &wo, const float3 &wi, float &pdfFwd,
                      float &pdfRev, Span<float> f) const {
    SMDL_DEBUG_CHECK(def && eval);
    SMDL_DEBUG_CHECK(f.size() == size_t(eval.wavelengthCount));
    return def->hairScatterEvaluate(eval, wo, wi, pdfFwd, pdfRev, f.data());
  }

  /// The hair scatter sample function.
  ///
  /// \param[in]  xi      The canonical random sample.
  /// \param[in]  wo      The outgoing direction in world space.
  /// \param[out] wi      The incoming direction in world space.
  /// \param[out] pdfFwd  The solid-angle PDF of sampling `wi` given `wo`.
  /// \param[out] pdfRev  The solid-angle PDF of sampling `wo` given `wi`.
  /// \param[out] f       The scattering spectrum.
  ///
  /// \return `true` if the result is non-zero.
  ///
  /// There are no Dirac hair distributions, so there is no `lobe` output.
  /// See `MaterialDef::hairScatterEvaluate` for the state contract at a
  /// hair hit.
  ///
  [[nodiscard]] SMDL_ALWAYS_INLINE bool
  hairScatterSample(const float4 &xi, const float3 &wo, float3 &wi,
                    float &pdfFwd, float &pdfRev, Span<float> f) const {
    SMDL_DEBUG_CHECK(def && eval);
    SMDL_DEBUG_CHECK(f.size() == size_t(eval.wavelengthCount));
    return def->hairScatterSample(eval, xi, wo, wi, pdfFwd, pdfRev, f.data());
  }

public:
  /// The definition.
  const MaterialDef *def{};

  /// The evaluation.
  MaterialDef::Eval eval{};
};

/// A just-in-time SMDL unit test.
struct UnitTest final {
public:
  /// The module name.
  std::string moduleName{};

  /// The module file name. This is empty if the module has no file, as
  /// is the case for builtin modules and modules supplied as source
  /// code (see `Compiler::addCode()`).
  std::string moduleFileName{};

  /// The module name to print in diagnostics, which is the file name
  /// for ordinary modules and origin markup for the others. See
  /// `Module::getDisplayName()`.
  std::string moduleDisplayName{};

  /// The line number.
  uint32_t lineNo{};

  /// The name of the test.
  std::string testName{};

  /// The test function.
  Function<void(const State &state)> test{};
};

} // namespace JIT

/// \}

} // namespace smdl
