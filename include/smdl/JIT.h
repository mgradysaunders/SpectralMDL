/// \file
#pragma once

#include "smdl/Common.h"

namespace smdl {

class Compiler;

/// \addtogroup Main
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

// \name Material Flags
// \{

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

/// \}

/// \name Distribution Function (DF) Flags
/// \{

/// Indicates that the function has a non-zero reflection component.
static constexpr int DF_REFLECTION = (1 << 0);

/// Indicates that the function has a non-zero transmission component.
static constexpr int DF_TRANSMISSION = (1 << 1);

/// Indicates that the function has a diffuse component.
static constexpr int DF_DIFFUSE = (1 << 2);

/// Indicates that the function has a glossy component.
static constexpr int DF_GLOSSY = (1 << 3);

/// Indicates that the function has a specular (Dirac delta) component.
static constexpr int DF_SPECULAR = (1 << 4);

/// \}

/// A just-in-time SMDL material.
struct Material final {
public:
  /// The module name.
  std::string moduleName{};

  /// The module file name.
  std::string moduleFileName{};

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
  /// The `MATERIAL_HAS_*` bits derived from `#is_default` are always
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

  /// Do shadow rays need no material work at all? True if the material
  /// is provably opaque and has no volume, in which case an occlusion
  /// hit is fully blocking and the material never needs to be
  /// constructed for shadow or transmission rays.
  [[nodiscard]] bool isShadowTrivial() const noexcept {
    return isAlwaysOpaque() && !hasVolume();
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

    /// The volume absorption coefficient if applicable.
    ///
    /// \note
    /// If non-null, this necessarily points to `wavelength_base_max` values.
    ///
    const float *absorption_coefficient{};

    /// The volume scattering coefficient if applicable.
    ///
    /// \note
    /// If non-null, this necessarily points to `wavelength_base_max` values.
    ///
    const float *scattering_coefficient{};

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

    /// The df flags for the material `surface` component.
    int df_flags_surface{};

    /// The df flags for the material `backface` component.
    int df_flags_backface{};

    /// The emission intensity modes: bit 0 is set if the `surface` emission
    /// intensity is `intensity_power` (as opposed to the default
    /// `intensity_radiant_exitance`), and bit 1 likewise for the `backface`.
    int emission_modes{};

    /// The random seed captured from `State::seed` when constructing the
    /// instance, which seeds the generator for stochastically evaluated
    /// BSDFs.
    int seed{};

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
               float &pdfFwd, float &pdfRev, float *f)>
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
  /// \param[out] isDelta
  /// Set to `true` if sampling Dirac delta distribution.
  ///
  /// \return
  /// Returns `true` if the result is non-zero.
  ///
  Function<int(const Instance &instance, const float4 &xi, const float3 &wo,
               float3 &wi, float &pdfFwd, float &pdfRev, float *f,
               int &isDelta)>
      scatterSample{};

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
                                     Span<float> f) const {
    SMDL_SANITY_CHECK(material && instance);
    SMDL_SANITY_CHECK(f.size() == size_t(instance.wavelength_base_max));
    return material->scatterEvaluate(instance, wo, wi, pdfFwd, pdfRev,
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
  /// \param[out] isDelta
  /// Set to `true` if sampling Dirac delta distribution.
  ///
  /// \return
  /// Returns `true` if the result is non-zero.
  ///
  [[nodiscard]] bool scatterSample(const float4 &xi, const float3 &wo,
                                   float3 &wi, float &pdfFwd, float &pdfRev,
                                   Span<float> f, bool &isDelta) const {
    SMDL_SANITY_CHECK(material && instance);
    SMDL_SANITY_CHECK(f.size() == size_t(instance.wavelength_base_max));
    auto isDeltaInt{int(0)};
    auto isNonZero{material->scatterSample(instance, xi, wo, wi, pdfFwd, pdfRev,
                                           f.data(), isDeltaInt)};
    isDelta = isDeltaInt;
    return isNonZero;
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

  /// The module file name.
  std::string moduleFileName{};

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
