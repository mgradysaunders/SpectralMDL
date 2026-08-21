/// \file
#pragma once

#include "smdl/Common.h"

namespace smdl {

class Compiler;

class VoxelGrid;

/// \addtogroup compiler
/// \{

/// Just-in-time interfaces.
namespace JIT {

template <typename> struct Function;

/// A just-in-time SMDL function.
template <typename Result, typename... Args>
struct Function<Result(Args...)> final {
public:
  /// The function pointer type.
  using function_pointer = Result (*)(Args...);

  Function() = default;

  Function(std::string name) : name(std::move(name)) {}

  /// Invoke the function.
  Result operator()(Args... args) const { return func(args...); }

  [[nodiscard]] operator bool() const { return func; }

public:
  /// The name used to look up the function in the JIT runtime.
  std::string name{};

  /// The function pointer.
  function_pointer func{};
};

/// \name Material Flags
/// \{

/// Indicates that the material is transporting importance.
static constexpr int MATERIAL_TRANSPORT_IMPORTANCE = (1 << 0);

/// Indicates that the material is thin-walled.
static constexpr int MATERIAL_THIN_WALLED = (1 << 1);

/// Indicates that the material has a non-default `surface` initializer.
static constexpr int MATERIAL_HAS_SURFACE = (1 << 2);

/// Indicates that the material has a non-default `backface` initializer.
static constexpr int MATERIAL_HAS_BACKFACE = (1 << 3);

/// Indicates that the material has a non-default emission EDF in the
/// `surface` initializer.
static constexpr int MATERIAL_HAS_SURFACE_EMISSION = (1 << 4);

/// Indicates that the material has a non-default emission EDF in the
/// `backface` initializer.
///
/// \note
/// The back side only actually emits if the material is also thin-walled,
/// which may only be knowable at runtime. See `Material::emissionEvaluate`.
///
static constexpr int MATERIAL_HAS_BACKFACE_EMISSION = (1 << 5);

/// Indicates that the material has a non-default `volume` initializer.
static constexpr int MATERIAL_HAS_VOLUME = (1 << 6);

/// Indicates that the material has a non-default `hair` initializer.
static constexpr int MATERIAL_HAS_HAIR = (1 << 7);

/// Indicates that the material has a cutout opacity less than one.
///
/// \note
/// These constants are mirrored by hand in the builtin `api.smdl`
/// (`_MaterialInstance.flags`); a new flag must be added in both places.
///
static constexpr int MATERIAL_HAS_CUTOUT = (1 << 8);

/// Indicates that the material volume coefficients vary with position.
///
/// \note
/// This bit only ever appears in `Material::staticFlags`: position
/// dependence is not observable at instance evaluation time, so
/// `Instance::flags` never sets it. Like `MATERIAL_HAS_CUTOUT` it is
/// derived after optimization and degrades to unknown at
/// `OPT_LEVEL_NONE`; see `Material::hasHomogeneousVolume()` for the
/// conservative reading.
///
static constexpr int MATERIAL_HAS_HETEROGENEOUS_VOLUME = (1 << 9);

/// Indicates that the material has a non-zero `geometry.displacement`.
///
/// \note
/// This bit only ever appears in `Material::staticFlags`: it is derived
/// after optimization from whether the displacement expression folds to
/// a constant, so it degrades to unknown at `OPT_LEVEL_NONE`, and
/// `Instance::flags` never sets it. See
/// `Material::hasZeroDisplacement()` for the conservative reading.
static constexpr int MATERIAL_HAS_DISPLACEMENT = (1 << 10);

/// Indicates that the material volume is declared additive (the SMDL
/// extension field `material_volume.additive`): it overlaps rather than
/// displaces the medium that encloses it, so hosts tracking nested media
/// should add its coefficients to the enclosing medium's over the shared
/// interior instead of replacing them.
static constexpr int MATERIAL_ADDITIVE_VOLUME = (1 << 11);

/// \}

/// \name Distribution Function (DF) Lobes
/// \{

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
/// reports, because `Instance::df_lobes_surface` unions over a whole
/// BSDF tree. Two axes OR'd together lose which domain went with which
/// kind: a Dirac reflection over a diffuse transmission and a Dirac
/// transmission over a diffuse reflection would report identical words,
/// and only the second has a Dirac transmission to refract through.
///
/// A reflective lobe with a finite density and no normal distribution
/// behind it: diffuse, sheen, micrograin and Hapke lobes among them. It
/// can be sampled and evaluated by direction, but has no half vector to
/// constrain.
///
/// The energy-compensation lobe of a rough BSDF is one of these, so a
/// glossy BSDF that carries one reports both kinds and a mask cuts
/// between them. See `DF_GLOSSY_BRDF`.
static constexpr int DF_GENERIC_BRDF = (1 << 0);

/// A reflective lobe with a sampleable normal distribution, so a half
/// vector is a meaningful quantity of it and a manifold constraint can be
/// solved through it.
///
/// This holds of the whole lobe and not merely of most of it, which is
/// why the Kulla-Conty style compensation lobe that a rough BSDF adds to
/// make up its energy deficit is `DF_GENERIC_BRDF` rather than part of
/// this: it is a cosine hemisphere with no normal distribution behind it,
/// and a caller that asks for a half vector must not be handed a lobe
/// that has none.
///
/// Having a normal distribution is necessary and not sufficient. A lobe
/// that mixes one with something else, or whose half vector nothing would
/// ever want to constrain, belongs in `DF_GENERIC_BRDF`; the micrograin
/// layer is both and is classified there.
static constexpr int DF_GLOSSY_BRDF = (1 << 1);

/// A reflective Dirac delta lobe, which has no density and whose half
/// vector is fixed by the geometry.
static constexpr int DF_DELTA_BRDF = (1 << 2);

/// \copydoc DF_GENERIC_BRDF
static constexpr int DF_GENERIC_BTDF = (1 << 3);

/// \copydoc DF_GLOSSY_BRDF
static constexpr int DF_GLOSSY_BTDF = (1 << 4);

/// \copydoc DF_DELTA_BRDF
static constexpr int DF_DELTA_BTDF = (1 << 5);

/// \name Lobe unions
/// The rows and columns of the table: each names one axis and leaves the
/// other unconstrained. Intersecting two of them names the single lobe
/// their names spell, so `DF_DELTA & DF_BTDF` is `DF_DELTA_BTDF`.
/// \{

/// Every reflective lobe.
static constexpr int DF_BRDF = DF_GENERIC_BRDF | DF_GLOSSY_BRDF | DF_DELTA_BRDF;

/// Every transmissive lobe.
static constexpr int DF_BTDF = DF_GENERIC_BTDF | DF_GLOSSY_BTDF | DF_DELTA_BTDF;

/// Every lobe with a density but no normal distribution, of either
/// domain.
static constexpr int DF_GENERIC = DF_GENERIC_BRDF | DF_GENERIC_BTDF;

/// Every normal-distribution lobe of either domain.
static constexpr int DF_GLOSSY = DF_GLOSSY_BRDF | DF_GLOSSY_BTDF;

/// Every Dirac lobe of either domain.
static constexpr int DF_DELTA = DF_DELTA_BRDF | DF_DELTA_BTDF;

/// Every lobe with a density, which is every lobe but the Dirac ones.
/// This is the question a caller asks to find out whether a vertex can
/// scatter a direction that another strategy could also have produced.
static constexpr int DF_FINITE = DF_GENERIC | DF_GLOSSY;

/// Every lobe, which is the lobe mask of a caller that wants the whole
/// distribution.
static constexpr int DF_ALL = DF_BRDF | DF_BTDF;

/// \}

/// \}

/// A just-in-time SMDL material.
struct Material final {
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
  /// every possible instance of this material. This is a subset of
  /// `staticFlagsKnown`; for every instance,
  /// `(instance.flags & staticFlagsKnown) == staticFlags`.
  int staticFlags{};

  /// The mask of flag bits whose values are compile-time constants.
  ///
  /// The `MATERIAL_HAS_*` bits derived from `#isDefault` are always
  /// known. `MATERIAL_THIN_WALLED` and `MATERIAL_HAS_CUTOUT` are known
  /// iff their initializers constant-fold after optimization, so they
  /// degrade to unknown at `OPT_LEVEL_NONE`; an unknown bit must be
  /// treated conservatively (e.g., possibly transparent).
  /// `MATERIAL_TRANSPORT_IMPORTANCE` is never known because it mirrors
  /// the `State::transport` the instance is evaluated with.
  ///
  /// \note
  /// Like the entry points themselves, static flags describe the
  /// material with its parameters bound to their defaults.
  ///
  int staticFlagsKnown{};

  /// Provably opaque: the cutout opacity is the compile-time constant 1.
  [[nodiscard]] bool isAlwaysOpaque() const noexcept {
    return (staticFlagsKnown & MATERIAL_HAS_CUTOUT) != 0 &&
           (staticFlags & MATERIAL_HAS_CUTOUT) == 0;
  }

  /// Has a non-default `volume` initializer? Always statically known.
  [[nodiscard]] bool hasVolume() const noexcept {
    return (staticFlags & MATERIAL_HAS_VOLUME) != 0;
  }

  /// Has a non-default `hair` initializer? Always statically known.
  [[nodiscard]] bool hasHair() const noexcept {
    return (staticFlags & MATERIAL_HAS_HAIR) != 0;
  }

  /// Do shadow rays need no material work at all? True if the material
  /// is provably opaque and has no volume, in which case an occlusion
  /// hit is fully blocking and the material never needs to be
  /// constructed for shadow or transmission rays.
  [[nodiscard]] bool isShadowTrivial() const noexcept {
    return isAlwaysOpaque() && !hasVolume();
  }

  /// Is this a null interface? (a boundary that scatters nothing itself
  /// but encloses a participating medium)
  [[nodiscard]] bool isNullInterface() const noexcept {
    return hasVolume() &&
           (staticFlags & (MATERIAL_HAS_SURFACE | MATERIAL_HAS_BACKFACE)) == 0;
  }

  /// Provably homogeneous: the volume coefficients are independent of
  /// the evaluation point, so the coefficient spectra captured by the
  /// `Instance` at the surface hit are exact everywhere in the interior
  /// and `volumeEvaluate` never needs to be called. When this returns
  /// false the volume is heterogeneous *or unproven*, and hosts must
  /// treat it as heterogeneous: sample the interior through
  /// `volumeEvaluate` against the majorants (see
  /// `Instance::max_scattering_coefficient`).
  [[nodiscard]] bool hasHomogeneousVolume() const noexcept {
    return (staticFlagsKnown & MATERIAL_HAS_HETEROGENEOUS_VOLUME) != 0 &&
           (staticFlags & MATERIAL_HAS_HETEROGENEOUS_VOLUME) == 0;
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

  /// An instance of the material.
  struct Instance final {
  public:
    /// Is null?
    [[nodiscard]] bool operator!() const noexcept { return ptr == nullptr; }

    /// Is non-null?
    [[nodiscard]] operator bool() const noexcept { return ptr != nullptr; }

  public:
    /// The JIT struct memory block.
    ///
    /// This holds the JIT material structure, which is entirely opaque to the
    /// user over in C++ land. Just ignore this!
    ///
    const void *ptr{};

    struct material_geometry final {
      /// The displacement vector.
      const float3 displacement{};

      /// The cutout opacity.
      const float cutout_opacity{};

      /// The normal.
      const float3 normal{};
    };

    /// The geometry.
    const material_geometry *geometry{};

    /// The index of refraction.
    float ior{};

    /// The exterior index of refraction, being the absolute index of the
    /// medium on the front side of the geometry. Initialized to 1 by
    /// `evaluate` and meant to be overwritten by hosts that track nested
    /// dielectrics. The relative ratio the scattering calculations refract
    /// with is `exterior_ior / ior`.
    float exterior_ior{};

    /// The temperature in Kelvin or -1 if undefined.
    float temperature{};

    /// The volume absorption coefficient if applicable, in units of
    /// inverse meters per the MDL specification: hosts working in scene
    /// units convert distances with `State::meters_per_scene_unit`
    /// before exponentiating.
    ///
    /// \note
    /// If non-null, this necessarily points to `wavelength_base_max` values.
    /// The value is whatever the coefficient expression evaluated to at
    /// instance time; for heterogeneous volumes (see
    /// `Material::hasHomogeneousVolume()`) that is merely the
    /// coefficient at the surface hit, and interior sampling must go
    /// through `Material::volumeEvaluate` instead.
    ///
    const float *absorption_coefficient{};

    /// The volume scattering coefficient if applicable, in units of
    /// inverse meters. See `absorption_coefficient` for the
    /// heterogeneous-volume caveat.
    ///
    /// \note
    /// If non-null, this necessarily points to `wavelength_base_max` values.
    ///
    const float *scattering_coefficient{};

    /// The volume absorption coefficient majorant if declared, in units
    /// of inverse meters: an author-declared, position-independent
    /// upper bound of `absorption_coefficient` over the whole interior
    /// (the SMDL extension field
    /// `material_volume.max_absorption_coefficient`).
    ///
    /// \note
    /// If non-null, this necessarily points to `wavelength_base_max` values.
    ///
    const float *max_absorption_coefficient{};

    /// The volume scattering coefficient majorant if declared, in units
    /// of inverse meters, see `max_absorption_coefficient`. This is
    /// what null-collision tracking through a heterogeneous interior
    /// runs against.
    ///
    /// \note
    /// If non-null, this necessarily points to `wavelength_base_max` values.
    ///
    const float *max_scattering_coefficient{};

    /// The `smdl::VoxelGrid` behind the volume density acceleration
    /// hint if declared (the SMDL extension field
    /// `material_volume.density`), else null. Together with the bound
    /// box below, this promises the coefficients at any interior point
    /// are bounded by the majorants scaled by the grid's trilinear
    /// value there over its maximum, so renderers may track against
    /// per-region majorants from the grid's per-brick bounds and skip
    /// empty regions. See `MaterialInstance::getVolumeDensityGrid()`.
    const void *volume_density_resource{};

    /// The object-space lower corner of the box that the density
    /// hint's texture space spans, if declared. If non-null, points to
    /// one `float3`.
    const float3 *volume_density_bound_min{};

    /// The object-space upper corner, see `volume_density_bound_min`.
    const float3 *volume_density_bound_max{};

    /// The volumetric emission coefficient if declared (MDL 1.8
    /// `material_volume.emission_intensity`): the radiance the medium
    /// adds per unit length, in `W/(m^2 sr nm)` per meter, converted
    /// with `State::meters_per_scene_unit` like the scattering
    /// coefficients. Evaluated at the surface hit; heterogeneous
    /// interiors re-query per point through `volumeEvaluate`.
    ///
    /// \note
    /// If non-null, this necessarily points to `wavelength_base_max` values.
    ///
    const float *volume_emission_intensity{};

    /// The `surface` emission intensity, or null if the `surface` has no
    /// non-default emission EDF.
    ///
    /// \note
    /// If non-null, this necessarily points to `wavelength_base_max` values.
    ///
    const float *surface_emission_intensity{};

    /// The `backface` emission intensity, or null if the `backface` has no
    /// non-default emission EDF.
    ///
    /// \note
    /// If non-null, this necessarily points to `wavelength_base_max` values.
    ///
    const float *backface_emission_intensity{};

    /// The wavelength count.
    int wavelength_base_max{};

    /// The flags.
    int flags{};

    /// The set of lobes (\ref DFLobes "the `DF_` lobes") present
    /// anywhere in the material `surface` scattering tree.
    ///
    /// This is a union over the tree, so it answers "could this material
    /// do that" and never "will this query do that". It is exact about
    /// which domain goes with which kind, which is what makes
    /// `df_lobes_surface & DF_DELTA_BTDF` a sound test for an interface
    /// a manifold walk can refract through.
    ///
    /// One distribution can contribute more than one bit: a rough BSDF
    /// with an energy-compensation lobe is `DF_GLOSSY_BRDF` and
    /// `DF_GENERIC_BRDF` together, since the two parts are different kinds
    /// on the same domain.
    int df_lobes_surface{};

    /// \copydoc df_lobes_surface
    int df_lobes_backface{};

    /// The emission intensity modes: bit 0 is set if the `surface` emission
    /// intensity is `intensity_power` (as opposed to the default
    /// `intensity_radiant_exitance`), and bit 1 likewise for the `backface`.
    int emission_modes{};

    /// The random seed captured from the raw state of `State::rng` when
    /// constructing the instance, which seeds the generator for
    /// stochastically evaluated BSDFs.
    int64_t seed{};

    /// The tangent-to-world space matrix present when constructing the
    /// instance.
    float3x3 tangent_to_world_space{};
  };

  /// The evaluate function.
  ///
  /// \param[inout] state
  /// The state.
  ///
  /// \param[out] instance
  /// The instance.
  ///
  /// This uses the `state.allocator` to allocate an `Instance`
  /// that must be passed to all other scattering calculations.
  ///
  /// \note
  /// After the user obtains an `Instance`, the `State` can be
  /// dropped.
  ///
  Function<void(State &state, Instance &instance)> evaluate{};

  /// The evaluate opacity function.
  ///
  /// \param[in] state
  /// The state.
  ///
  /// \return
  /// Returns `geometry.cutout_opacity` and evaluates nothing else: no
  /// instance is constructed and no allocation happens, so
  /// `state.allocator` may be null. This is the cheap path for shadow
  /// and transmission rays against materials that are not
  /// `isShadowTrivial()`.
  ///
  Function<float(State &state)> evaluateOpacity{};

  /// The displacement evaluate function.
  ///
  /// \param[in] state
  /// The state, which identifies the surface point being queried.
  ///
  /// \param[out] displacement
  /// The displacement vector, in the internal space the state's
  /// geometric fields were given in.
  ///
  /// Evaluates only `geometry.displacement` and nothing else: no
  /// instance is constructed and no allocation happens, so
  /// `state.allocator` may be null, and everything not feeding the
  /// displacement is dead-code eliminated, the way `evaluateOpacity`
  /// evaluates only the cutout opacity. This is the per-vertex query
  /// for hosts that apply displacement to geometry at load time; see
  /// `Material::hasZeroDisplacement()` for skipping materials that
  /// provably never displace.
  Function<void(State &state, float3 &displacement)> displacementEvaluate{};

  /// The volume evaluate function.
  ///
  /// \param[inout] state
  /// The state, which identifies the interior point being queried.
  ///
  /// \param[out] sigma_a
  /// The absorption coefficient spectrum in units of inverse meters.
  /// This must point to `wavelengthBaseMax` floats!
  ///
  /// \param[out] sigma_s
  /// The scattering coefficient spectrum in units of inverse meters.
  /// This must point to `wavelengthBaseMax` floats!
  ///
  /// \param[out] emission
  /// The volumetric emission coefficient spectrum, the radiance added
  /// per meter, resolved to zero when `emission_intensity` is not
  /// declared. This must point to `wavelengthBaseMax` floats!
  ///
  /// Evaluates only the volume coefficient expressions of the material
  /// at `state`, resolving an absent coefficient to zero: no instance
  /// is constructed and no allocation happens, so `state.allocator` may
  /// be null, and everything not feeding the coefficients is dead-code
  /// eliminated, the way `evaluateOpacity` evaluates only the cutout
  /// opacity. This is the per-point query that null-collision tracking
  /// calls at every tentative collision inside a heterogeneous medium;
  /// for provably homogeneous materials
  /// (`Material::hasHomogeneousVolume()`) the instance coefficient
  /// pointers answer the same question with no call at all.
  ///
  /// \note
  /// The state is a partial state in the sense of an environment
  /// lookup: the caller fills `position` with the query point in the
  /// *object space* of the volume instance (internal space equals
  /// object space here, there being no surface frame; do NOT call
  /// `State::finalizeAndApplyInternalSpaceConventions()`), along with
  /// the render-wide fields (`wavelength_base`, ...), and may leave the
  /// surface-geometry fields defaulted. Volume expressions read the
  /// point through `state::position()`.
  ///
  Function<void(State &state, float *sigma_a, float *sigma_s, float *emission)>
      volumeEvaluate{};

  /// The scatter evaluate function.
  ///
  /// \param[in] instance
  /// The instance obtained from the `evaluate` function.
  ///
  /// \param[in] wo
  /// The outgoing direction in world space.
  ///
  /// \param[in] wi
  /// The incoming direction in world space.
  ///
  /// \param[in] lobeMask
  /// Which lobes to evaluate: a lobe contributes when it is in this set
  /// (\ref DFLobes "the DF lobes"). `DF_ALL` is every lobe, which is the
  /// whole BSDF and what an ordinary caller wants; `0` names nothing and
  /// selects nothing.
  ///
  /// A restricted mask restricts `f` to the part of the whole BSDF the
  /// named lobes account for, weighted as they are weighted inside it
  /// and renormalized by nothing. **Disjoint masks therefore add**, so
  /// evaluating with `DF_GLOSSY`, `DF_GENERIC` and `DF_DELTA` and summing
  /// gives what `DF_ALL` gives, and so does evaluating with `DF_BRDF` and
  /// `DF_BTDF` and summing.
  ///
  /// The densities are not that. They are the densities of the sampler
  /// restricted to the same mask, which takes a lobe with certainty once
  /// the mask has removed the alternatives, so they do not add and they
  /// exceed the share the mask keeps. The pairing is the point: a masked
  /// `f` over a masked density is an estimator of the masked part, and
  /// this is the density `scatterSample` reports for a non-delta sample
  /// drawn under the same mask.
  ///
  /// The domain axis partitions even a `scatter_reflect_transmit` lobe,
  /// which is in lobes of both domains: `wo` and `wi` decide which of
  /// its branches this query is about, so masking away that domain
  /// returns zero rather than the other branch.
  ///
  /// \param[out] pdfFwd
  /// The forward PDF of sampling `wi` given `wo`.
  ///
  /// \param[out] pdfRev
  /// The reverse PDF of sampling `wo` given `wi`.
  ///
  /// \param[out] f
  /// The BSDF spectrum. This must be non-null!
  ///
  /// \return
  /// Returns `true` if the result is non-zero.
  ///
  Function<int(const Instance &instance, const float3 &wo, const float3 &wi,
               int lobeMask, float &pdfFwd, float &pdfRev, float *f)>
      scatterEvaluate{};

  /// The scatter sample function.
  ///
  /// \param[in] instance
  /// The instance obtained from the `evaluate` function.
  ///
  /// \param[in] xi
  /// The canonical random sample in \f$ [0,1]^4 \f$.
  ///
  /// \param[in] wo
  /// The outgoing direction in world space.
  ///
  /// \param[in] lobeMask
  /// Which lobes to sample among: a lobe is a candidate when it is in
  /// this set (\ref DFLobes "the DF lobes"). `DF_ALL` is every lobe.
  ///
  /// Selection chances are renormalized over the lobes the mask keeps,
  /// so this draws from the masked part of the BSDF rather than drawing
  /// from the whole and sometimes landing outside the mask. What comes
  /// back estimates that masked part: `f` and the densities describe the
  /// restricted sampler, and the estimators of disjoint masks sum in
  /// expectation to the estimator of the whole.
  ///
  /// This is how a caller reaches the Dirac lobes of a layered material:
  /// `DF_DELTA` selects them wherever they sit in the tree, and the
  /// layering above them is applied on the way out. `scatterEvaluate`
  /// cannot do this at any mask, because a Dirac lobe has no density to
  /// evaluate at a direction pair.
  ///
  /// Naming one domain **forces** it here: a mask holding only
  /// transmissive lobes makes a `scatter_reflect_transmit` lobe
  /// transmit every time, in place of the Fresnel choice it would
  /// otherwise make, and the weight it reports is the transmissive part
  /// of the lobe rather than the whole. So `DF_DELTA_BTDF` is how a
  /// caller asks for the Dirac transmission of an interface, whatever
  /// the tree above it.
  ///
  /// \param[out] wi
  /// The incoming direction in world space.
  ///
  /// \param[out] pdfFwd
  /// The forward PDF of sampling `wi` given `wo`.
  ///
  /// \param[out] pdfRev
  /// The reverse PDF of sampling `wo` given `wi`.
  ///
  /// \param[out] f
  /// The BSDF spectrum. This must be non-null!
  ///
  /// \param[out] sampledLobe
  /// The single lobe the sample was drawn from (\ref DFLobes "the DF
  /// lobes"), or `0` when nothing was sampled. Exactly one bit: the
  /// selection descends to one branch of one leaf, and that branch has one
  /// domain and one kind.
  ///
  /// This subsumes the older `isDelta`, which is `sampledLobe & DF_DELTA`,
  /// and answers what a class word over the whole tree cannot: a material
  /// reports what it *could* do, and this reports what it *did*. It is the
  /// per-sample quantity a ray-cone heuristic, a guiding bypass, or a
  /// specular-chain test wants.
  ///
  /// The bit is always one the material declared in `df_lobes_surface` or
  /// `df_lobes_backface`, and always inside the caller's `lobeMask`.
  ///
  /// \param[out] lobeChance
  /// The discrete probability that an **unmasked** sample would have
  /// made the same selections this call did.
  ///
  /// A masked call renormalizes its own chances over the lobes the mask
  /// keeps, so this is the only way back to what the whole BSDF would
  /// have done. A caller weighing a masked result against a strategy
  /// that samples the whole BSDF needs it, and for a Dirac lobe there is
  /// no other source, since `scatterEvaluate` reports zero there at
  /// every mask. Exactly 1 when nothing chose.
  ///
  /// \return
  /// Returns `true` if the result is non-zero.
  ///
  Function<int(const Instance &instance, const float4 &xi, const float3 &wo,
               int lobeMask, float3 &wi, float &pdfFwd, float &pdfRev, float *f,
               int &sampledLobe, float &lobeChance)>
      scatterSample{};

  /// The normal distribution sample function.
  ///
  /// Draws a microfacet normal from the normal distribution behind one
  /// GLOSSY lobe, which is what `DF_GLOSSY_BRDF` promises exists and this
  /// is how a caller reaches. A host solving a manifold constraint through
  /// a rough interface needs a half vector it can draw and weigh; this and
  /// `scatterNormalEvaluate` are that, and nothing more. What such an
  /// estimator is worth is `scatterEvaluate` at the directions the
  /// constraint resolves to.
  ///
  /// \note
  /// Null unless `Compiler::enableScatterNormal` was set before
  /// `compile()`. A host that never asks pays nothing for these.
  ///
  /// \param[in] instance
  /// The instance obtained from the `evaluate` function.
  ///
  /// \param[in] xi
  /// The canonical random sample in \f$ [0,1]^4 \f$.
  ///
  /// \param[in] wo
  /// The outgoing direction in world space.
  ///
  /// \param[in] lobeMask
  /// Which lobes to draw from, intersected with `DF_GLOSSY` throughout
  /// since nothing else has a normal distribution to report. Selection
  /// chances are renormalized over the lobes that survive, so a mask
  /// naming one interface's transmissive lobe draws that lobe's own
  /// distribution however the tree layers it.
  ///
  /// \param[out] wm
  /// The microfacet normal in world space, on the same side of the shading
  /// normal as `wo`.
  ///
  /// \param[out] pdf
  /// The density of `wm` per unit solid angle, mixed over every lobe the
  /// mask keeps that could have produced it. This is exactly what
  /// `scatterNormalEvaluate` reports at the same directions, and that
  /// identity is the one property a caller's correctness may rest on: it
  /// is what makes the pair a usable proposal. It is NOT in general the
  /// density `scatterEvaluate` divides out, though the microfacet lobes
  /// match that too.
  ///
  /// \param[out] alpha
  /// The squared roughness of the lobe drawn from, so a host can decide
  /// whether an interface is smooth enough to be worth constraining
  /// without a second query.
  ///
  /// \return
  /// Returns `true` if a lobe with a normal distribution was reached.
  ///
  Function<int(const Instance &instance, const float4 &xi, const float3 &wo,
               int lobeMask, float3 &wm, float &pdf, float2 &alpha)>
      scatterNormalSample{};

  /// The normal distribution evaluate function.
  ///
  /// The density with which `scatterNormalSample` draws `wm` given `wo`.
  /// See it for the contract; see `Compiler::enableScatterNormal` for why
  /// this may be null.
  ///
  /// \param[in] instance
  /// The instance obtained from the `evaluate` function.
  ///
  /// \param[in] wo
  /// The outgoing direction in world space.
  ///
  /// \param[in] wm
  /// The microfacet normal in world space.
  ///
  /// \param[in] lobeMask
  /// Which lobes to mix over, as in `scatterNormalSample`.
  ///
  /// \param[out] pdf
  /// The density of `wm` per unit solid angle.
  ///
  /// \return
  /// Returns `true` if the density is non-zero.
  ///
  Function<int(const Instance &instance, const float3 &wo, const float3 &wm,
               int lobeMask, float &pdf)>
      scatterNormalEvaluate{};

  /// The emission evaluate function.
  ///
  /// \param[in] instance
  /// The instance obtained from the `evaluate` function.
  ///
  /// \param[in] wi
  /// The emission direction in world space, pointing away from the
  /// surface.
  ///
  /// \param[out] pdf
  /// The solid-angle PDF of `emissionSample` sampling `wi`.
  ///
  /// \param[out] Le
  /// The emitted radiance spectrum. This must be non-null!
  ///
  /// \return
  /// Returns `true` if the result is non-zero.
  ///
  /// \note
  /// The radiance is `material_emission.intensity` times the normalized
  /// EDF, which is the physical radiance when the intensity mode is
  /// `intensity_radiant_exitance`. When the mode is `intensity_power`
  /// (see `Instance::emission_modes`), the host must additionally divide
  /// by the total emitting surface area.
  ///
  /// \note
  /// Solid materials emit only on the exterior side of the geometry.
  /// Thin-walled materials emit `surface.emission` on the front side and
  /// `backface.emission`, if the backface is non-default, on the back
  /// side, else `surface.emission` mirrored.
  ///
  Function<int(const Instance &instance, const float3 &wi, float &pdf,
               float *Le)>
      emissionEvaluate{};

  /// The emission sample function.
  ///
  /// \param[in] instance
  /// The instance obtained from the `evaluate` function.
  ///
  /// \param[in] xi
  /// The canonical random sample in \f$ [0,1]^4 \f$.
  ///
  /// \param[out] wi
  /// The emission direction in world space.
  ///
  /// \param[out] pdf
  /// The solid-angle PDF of sampling `wi`.
  ///
  /// \param[out] Le
  /// The emitted radiance spectrum. This must be non-null!
  ///
  /// \return
  /// Returns `true` if the result is non-zero.
  ///
  Function<int(const Instance &instance, const float4 &xi, float3 &wi,
               float &pdf, float *Le)>
      emissionSample{};

  /// The volume scatter evaluate function.
  ///
  /// \param[in] instance
  /// The instance obtained from the `evaluate` function.
  ///
  /// \param[in] wo
  /// The outgoing direction in world space.
  ///
  /// \param[in] wi
  /// The incoming direction in world space.
  ///
  /// \return
  /// Returns the phase function, which is normalized over the sphere and
  /// so is also the solid-angle PDF of `volumeScatterSample`. Returns zero
  /// if the material has no volume scattering.
  ///
  Function<float(const Instance &instance, const float3 &wo, const float3 &wi)>
      volumeScatterEvaluate{};

  /// The volume scatter sample function.
  ///
  /// \param[in] instance
  /// The instance obtained from the `evaluate` function.
  ///
  /// \param[in] xi
  /// The canonical random sample in \f$ [0,1]^4 \f$.
  ///
  /// \param[in] wo
  /// The outgoing direction in world space.
  ///
  /// \param[out] wi
  /// The incoming direction in world space.
  ///
  /// \return
  /// Returns the phase function at `wi`, which is also the solid-angle
  /// PDF of having sampled it, so the implied throughput weight is
  /// always 1. Returns zero if the material has no volume scattering.
  ///
  Function<float(const Instance &instance, const float4 &xi, const float3 &wo,
                 float3 &wi)>
      volumeScatterSample{};

  /// The hair scatter evaluate function, dispatching `material.hair`.
  ///
  /// \param[in] instance
  /// The instance obtained from the `evaluate` function.
  ///
  /// \param[in] wo
  /// The outgoing direction in world space.
  ///
  /// \param[in] wi
  /// The incoming direction in world space.
  ///
  /// \param[out] pdfFwd
  /// The forward PDF of sampling `wi` given `wo`.
  ///
  /// \param[out] pdfRev
  /// The reverse PDF of sampling `wo` given `wi`.
  ///
  /// \param[out] f
  /// The BSDF spectrum. This must be non-null!
  ///
  /// \return
  /// Returns `true` if the result is non-zero.
  ///
  /// \note
  /// The state contract at a hair hit: `State::normal` must be the
  /// shading normal on the fiber surface (the true normal of a tube;
  /// ribbon geometry must synthesize it) and `State::texture_tangent_u[0]`
  /// must be the fiber tangent pointing root to tip. The BSDF implies the
  /// cross-section offset from the normal as `h = sin(gamma_o)` and reads
  /// no texture coordinate. Per the MDL specification, the material `ior`,
  /// `thin_walled`, `volume`, and `geometry` fields do not influence hair
  /// shading, and `wo` on the far side of the normal plane is a
  /// legitimate configuration rather than a backface hit. Hosts should
  /// gate calls on `hasHair()`; calling anyway is safe because the
  /// default `hair_bsdf()` reports black.
  ///
  Function<int(const Instance &instance, const float3 &wo, const float3 &wi,
               float &pdfFwd, float &pdfRev, float *f)>
      hairScatterEvaluate{};

  /// The hair scatter sample function.
  ///
  /// \param[in] instance
  /// The instance obtained from the `evaluate` function.
  ///
  /// \param[in] xi
  /// The canonical random sample in \f$ [0,1]^4 \f$.
  ///
  /// \param[in] wo
  /// The outgoing direction in world space.
  ///
  /// \param[out] wi
  /// The incoming direction in world space.
  ///
  /// \param[out] pdfFwd
  /// The forward PDF of sampling `wi` given `wo`.
  ///
  /// \param[out] pdfRev
  /// The reverse PDF of sampling `wo` given `wi`.
  ///
  /// \param[out] f
  /// The BSDF spectrum. This must be non-null!
  ///
  /// \return
  /// Returns `true` if the result is non-zero.
  ///
  /// \note
  /// See `hairScatterEvaluate` for the state contract at a hair hit. There
  /// are no delta hair distributions and no lobe taxonomy for hair, so this
  /// reports neither a sampled lobe nor a delta flag.
  ///
  Function<int(const Instance &instance, const float4 &xi, const float3 &wo,
               float3 &wi, float &pdfFwd, float &pdfRev, float *f)>
      hairScatterSample{};
};

/// A just-in-time SMDL material pointer and an instance of the material.
struct MaterialInstance final {
public:
  MaterialInstance() = default;

  /// Allocate and initialize from the given state and material.
  explicit MaterialInstance(State &state, const Material *material)
      : material(material) {
    SMDL_SANITY_CHECK(material);
    material->evaluate(state, instance);
  }

  /// The cutout opacity.
  [[nodiscard]] float getCutoutOpacity() const noexcept {
    return instance.geometry->cutout_opacity;
  }

  /// Is thin walled?
  [[nodiscard]] bool isThinWalled() const noexcept {
    return (instance.flags & MATERIAL_THIN_WALLED) != 0;
  }

  /// Has medium properties?
  [[nodiscard]] bool hasMedium() const noexcept {
    return (instance.absorption_coefficient != nullptr ||
            instance.scattering_coefficient != nullptr);
  }

  /// Is the volume declared additive? See `MATERIAL_ADDITIVE_VOLUME`.
  [[nodiscard]] bool hasAdditiveVolume() const noexcept {
    return (instance.flags & MATERIAL_ADDITIVE_VOLUME) != 0;
  }

  /// Has a non-default `hair` initializer?
  [[nodiscard]] bool hasHair() const noexcept {
    return (instance.flags & MATERIAL_HAS_HAIR) != 0;
  }

  /// Has a non-default emission EDF in the `surface` initializer?
  [[nodiscard]] bool hasSurfaceEmission() const noexcept {
    return (instance.flags & MATERIAL_HAS_SURFACE_EMISSION) != 0;
  }

  /// Has a non-default emission EDF in the `backface` initializer?
  [[nodiscard]] bool hasBackfaceEmission() const noexcept {
    return (instance.flags & MATERIAL_HAS_BACKFACE_EMISSION) != 0;
  }

  /// Has a non-default emission EDF at all?
  [[nodiscard]] bool hasEmission() const noexcept {
    return hasSurfaceEmission() || hasBackfaceEmission();
  }

  /// The `surface` emission intensity, or empty if the `surface` has no
  /// non-default emission EDF.
  [[nodiscard]]
  Span<const float> getSurfaceEmissionIntensity() const noexcept {
    return Span<const float>(
        instance.surface_emission_intensity,
        instance.surface_emission_intensity ? instance.wavelength_base_max : 0);
  }

  /// The `backface` emission intensity, or empty if the `backface` has no
  /// non-default emission EDF.
  [[nodiscard]]
  Span<const float> getBackfaceEmissionIntensity() const noexcept {
    return Span<const float>(instance.backface_emission_intensity,
                             instance.backface_emission_intensity
                                 ? instance.wavelength_base_max
                                 : 0);
  }

  /// Is the `surface` emission intensity in units of power (watts) as
  /// opposed to radiant exitance (watts per square meter)? If so, the host
  /// must divide emitted radiance by the total emitting surface area.
  [[nodiscard]] bool isSurfaceEmissionPower() const noexcept {
    return (instance.emission_modes & 1) != 0;
  }

  /// Is the `backface` emission intensity in units of power (watts) as
  /// opposed to radiant exitance (watts per square meter)?
  [[nodiscard]] bool isBackfaceEmissionPower() const noexcept {
    return (instance.emission_modes & 2) != 0;
  }

  /// The index of refraction.
  [[nodiscard]] float getIOR() const noexcept { return instance.ior; }

  /// The exterior index of refraction, i.e., of the medium surrounding
  /// the object on the front side of the geometry. Defaults to 1.
  [[nodiscard]] float getExteriorIOR() const noexcept {
    return instance.exterior_ior;
  }

  /// Set the exterior index of refraction. Hosts that track nested
  /// dielectrics call this after construction and before the scattering
  /// functions, passing the index of the medium surrounding the object.
  void setExteriorIOR(float exteriorIOR) noexcept {
    instance.exterior_ior = exteriorIOR;
  }

  /// The absorption coefficient of the medium, or empty if none.
  [[nodiscard]] Span<const float> getAbsorptionCoefficient() const noexcept {
    return Span<const float>(
        instance.absorption_coefficient,
        instance.absorption_coefficient ? instance.wavelength_base_max : 0);
  }

  /// The scattering coefficient of the medium, or empty if none.
  [[nodiscard]] Span<const float> getScatteringCoefficient() const noexcept {
    return Span<const float>(
        instance.scattering_coefficient,
        instance.scattering_coefficient ? instance.wavelength_base_max : 0);
  }

  /// The declared absorption coefficient majorant, or empty if none.
  [[nodiscard]] Span<const float> getMaxAbsorptionCoefficient() const noexcept {
    return Span<const float>(
        instance.max_absorption_coefficient,
        instance.max_absorption_coefficient ? instance.wavelength_base_max : 0);
  }

  /// The declared scattering coefficient majorant, or empty if none.
  [[nodiscard]] Span<const float> getMaxScatteringCoefficient() const noexcept {
    return Span<const float>(
        instance.max_scattering_coefficient,
        instance.max_scattering_coefficient ? instance.wavelength_base_max : 0);
  }

  /// The volume density acceleration hint grid, or null if not
  /// declared. See `Instance::volume_density_resource`.
  [[nodiscard]] const VoxelGrid *getVolumeDensityGrid() const noexcept {
    return static_cast<const VoxelGrid *>(instance.volume_density_resource);
  }

  /// The lower corner of the density hint box, or null if not declared.
  [[nodiscard]] const float3 *getVolumeDensityBoundMin() const noexcept {
    return instance.volume_density_bound_min;
  }

  /// The upper corner of the density hint box, or null if not declared.
  [[nodiscard]] const float3 *getVolumeDensityBoundMax() const noexcept {
    return instance.volume_density_bound_max;
  }

  /// The volumetric emission coefficient, or empty if none.
  [[nodiscard]] Span<const float> getVolumeEmissionIntensity() const noexcept {
    return Span<const float>(
        instance.volume_emission_intensity,
        instance.volume_emission_intensity ? instance.wavelength_base_max : 0);
  }

  /// The geometry normal in world space.
  [[nodiscard]] float3 getGeometryNormal() const noexcept {
    return instance.tangent_to_world_space[2];
  }

  /// Is the given direction on the exterior side of the geometry?
  [[nodiscard]] bool isExterior(const float3 &w) const noexcept {
    return dot(getGeometryNormal(), w) > 0.0f;
  }

  /// Is the given direction on the interior side of the geometry?
  [[nodiscard]] bool isInterior(const float3 &w) const noexcept {
    return !isExterior(w);
  }

  /// Is the given pair of directions transmitting through the geometry?
  [[nodiscard]] bool isTransmitting(const float3 &wo,
                                    const float3 &wi) const noexcept {
    return isExterior(wo) != isExterior(wi);
  }

  /// The scatter evaluate function.
  ///
  /// \param[in] wo
  /// The outgoing direction in world space.
  ///
  /// \param[in] wi
  /// The incoming direction in world space.
  ///
  /// \param[out] pdfFwd
  /// The forward PDF of sampling `wi` given `wo`.
  ///
  /// \param[out] pdfRev
  /// The reverse PDF of sampling `wo` given `wi`.
  ///
  /// \param[out] f
  /// The BSDF spectrum. This must be non-null!
  ///
  /// \return
  /// Returns `true` if the result is non-zero.
  ///
  [[nodiscard]] bool scatterEvaluate(const float3 &wo, const float3 &wi,
                                     float &pdfFwd, float &pdfRev,
                                     Span<float> f,
                                     int lobeMask = DF_ALL) const {
    SMDL_SANITY_CHECK(material && instance);
    SMDL_SANITY_CHECK(f.size() == size_t(instance.wavelength_base_max));
    return material->scatterEvaluate(instance, wo, wi, lobeMask, pdfFwd, pdfRev,
                                     f.data());
  }

  /// The scatter sample function.
  ///
  /// \param[in] xi
  /// The canonical random sample in \f$ [0,1]^4 \f$.
  ///
  /// \param[in] wo
  /// The outgoing direction in world space.
  ///
  /// \param[out] wi
  /// The incoming direction in world space.
  ///
  /// \param[out] pdfFwd
  /// The forward PDF of sampling `wi` given `wo`.
  ///
  /// \param[out] pdfRev
  /// The reverse PDF of sampling `wo` given `wi`.
  ///
  /// \param[out] f
  /// The BSDF spectrum. This must be non-null!
  ///
  /// \param[out] sampledLobe
  /// The single lobe the sample was drawn from, `0` if none. See
  /// `Material::scatterSample`; `sampledLobe & DF_DELTA` is the Dirac test.
  ///
  /// \return
  /// Returns `true` if the result is non-zero.
  ///
  [[nodiscard]] bool scatterSample(const float4 &xi, const float3 &wo,
                                   float3 &wi, float &pdfFwd, float &pdfRev,
                                   Span<float> f, int &sampledLobe,
                                   int lobeMask = DF_ALL,
                                   float *lobeChance = nullptr) const {
    SMDL_SANITY_CHECK(material && instance);
    SMDL_SANITY_CHECK(f.size() == size_t(instance.wavelength_base_max));
    auto lobeChanceLocal{float(1)};
    return material->scatterSample(instance, xi, wo, lobeMask, wi, pdfFwd,
                                   pdfRev, f.data(), sampledLobe,
                                   lobeChance ? *lobeChance : lobeChanceLocal);
  }

  /// The normal distribution sample function.
  ///
  /// See `Material::scatterNormalSample` for the contract. Aborts if the
  /// entry point was not emitted, which is the case unless
  /// `Compiler::enableScatterNormal` was set before `compile()`.
  [[nodiscard]] bool scatterNormalSample(const float4 &xi, const float3 &wo,
                                         float3 &wm, float &pdf, float2 &alpha,
                                         int lobeMask = DF_GLOSSY) const {
    SMDL_SANITY_CHECK(material && instance);
    SMDL_SANITY_CHECK_MSG(bool(material->scatterNormalSample),
                          "set 'Compiler::enableScatterNormal' before "
                          "'compile()' to emit the normal distribution "
                          "entry points");
    return material->scatterNormalSample(instance, xi, wo, lobeMask, wm, pdf,
                                         alpha);
  }

  /// The normal distribution evaluate function.
  ///
  /// See `Material::scatterNormalEvaluate` for the contract. Aborts if the
  /// entry point was not emitted, as above.
  [[nodiscard]] bool scatterNormalEvaluate(const float3 &wo, const float3 &wm,
                                           float &pdf,
                                           int lobeMask = DF_GLOSSY) const {
    SMDL_SANITY_CHECK(material && instance);
    SMDL_SANITY_CHECK_MSG(bool(material->scatterNormalEvaluate),
                          "set 'Compiler::enableScatterNormal' before "
                          "'compile()' to emit the normal distribution "
                          "entry points");
    return material->scatterNormalEvaluate(instance, wo, wm, lobeMask, pdf);
  }

  /// The emission evaluate function.
  ///
  /// \param[in] wi
  /// The emission direction in world space, pointing away from the
  /// surface.
  ///
  /// \param[out] pdf
  /// The solid-angle PDF of `emissionSample` sampling `wi`.
  ///
  /// \param[out] Le
  /// The emitted radiance spectrum. This must be non-null!
  ///
  /// \return
  /// Returns `true` if the result is non-zero.
  ///
  /// \note
  /// See `Material::emissionEvaluate` for the unit conventions and for
  /// which side of the geometry emits.
  ///
  [[nodiscard]] bool emissionEvaluate(const float3 &wi, float &pdf,
                                      Span<float> Le) const {
    SMDL_SANITY_CHECK(material && instance);
    SMDL_SANITY_CHECK(Le.size() == size_t(instance.wavelength_base_max));
    return material->emissionEvaluate(instance, wi, pdf, Le.data());
  }

  /// The emission sample function.
  ///
  /// \param[in] xi
  /// The canonical random sample in \f$ [0,1]^4 \f$.
  ///
  /// \param[out] wi
  /// The emission direction in world space.
  ///
  /// \param[out] pdf
  /// The solid-angle PDF of sampling `wi`.
  ///
  /// \param[out] Le
  /// The emitted radiance spectrum. This must be non-null!
  ///
  /// \return
  /// Returns `true` if the result is non-zero.
  ///
  [[nodiscard]] bool emissionSample(const float4 &xi, float3 &wi, float &pdf,
                                    Span<float> Le) const {
    SMDL_SANITY_CHECK(material && instance);
    SMDL_SANITY_CHECK(Le.size() == size_t(instance.wavelength_base_max));
    return material->emissionSample(instance, xi, wi, pdf, Le.data());
  }

  /// The volume scatter evaluate function.
  ///
  /// \param[in] wo
  /// The outgoing direction in world space.
  ///
  /// \param[in] wi
  /// The incoming direction in world space.
  ///
  /// \return
  /// Returns the phase function, which is normalized over the sphere and
  /// so is also the solid-angle PDF of `volumeScatterSample`.
  ///
  [[nodiscard]] float volumeScatterEvaluate(const float3 &wo,
                                            const float3 &wi) const {
    SMDL_SANITY_CHECK(material && instance);
    return material->volumeScatterEvaluate(instance, wo, wi);
  }

  /// The volume scatter sample function.
  ///
  /// \param[in] xi
  /// The canonical random sample in \f$ [0,1]^4 \f$.
  ///
  /// \param[in] wo
  /// The outgoing direction in world space.
  ///
  /// \param[out] wi
  /// The incoming direction in world space.
  ///
  /// \return
  /// Returns the phase function at `wi`, which is also the solid-angle
  /// PDF of having sampled it, so the implied throughput weight is
  /// always 1.
  ///
  [[nodiscard]] float volumeScatterSample(const float4 &xi, const float3 &wo,
                                          float3 &wi) const {
    SMDL_SANITY_CHECK(material && instance);
    return material->volumeScatterSample(instance, xi, wo, wi);
  }

  /// The hair scatter evaluate function. See
  /// `Material::hairScatterEvaluate` for the state contract at a hair
  /// hit.
  ///
  /// \param[in] wo
  /// The outgoing direction in world space.
  ///
  /// \param[in] wi
  /// The incoming direction in world space.
  ///
  /// \param[out] pdfFwd
  /// The forward PDF of sampling `wi` given `wo`.
  ///
  /// \param[out] pdfRev
  /// The reverse PDF of sampling `wo` given `wi`.
  ///
  /// \param[out] f
  /// The BSDF spectrum. This must be non-null!
  ///
  /// \return
  /// Returns `true` if the result is non-zero.
  ///
  [[nodiscard]] bool hairScatterEvaluate(const float3 &wo, const float3 &wi,
                                         float &pdfFwd, float &pdfRev,
                                         Span<float> f) const {
    SMDL_SANITY_CHECK(material && instance);
    SMDL_SANITY_CHECK(f.size() == size_t(instance.wavelength_base_max));
    return material->hairScatterEvaluate(instance, wo, wi, pdfFwd, pdfRev,
                                         f.data());
  }

  /// The hair scatter sample function. There are no delta hair
  /// distributions, so there is no `isDelta` output.
  ///
  /// \param[in] xi
  /// The canonical random sample in \f$ [0,1]^4 \f$.
  ///
  /// \param[in] wo
  /// The outgoing direction in world space.
  ///
  /// \param[out] wi
  /// The incoming direction in world space.
  ///
  /// \param[out] pdfFwd
  /// The forward PDF of sampling `wi` given `wo`.
  ///
  /// \param[out] pdfRev
  /// The reverse PDF of sampling `wo` given `wi`.
  ///
  /// \param[out] f
  /// The BSDF spectrum. This must be non-null!
  ///
  /// \return
  /// Returns `true` if the result is non-zero.
  ///
  [[nodiscard]] bool hairScatterSample(const float4 &xi, const float3 &wo,
                                       float3 &wi, float &pdfFwd, float &pdfRev,
                                       Span<float> f) const {
    SMDL_SANITY_CHECK(material && instance);
    SMDL_SANITY_CHECK(f.size() == size_t(instance.wavelength_base_max));
    return material->hairScatterSample(instance, xi, wo, wi, pdfFwd, pdfRev,
                                       f.data());
  }

public:
  /// The material.
  const Material *material{};

  /// The instance.
  Material::Instance instance{};
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
  Function<void(const State &)> test{};
};

} // namespace JIT

/// \}

} // namespace smdl
