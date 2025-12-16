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

/// Indicates that the material has a non-default `volume` initializer.
static constexpr int MATERIAL_HAS_VOLUME = (1 << 6);

/// Indicates that the material has a non-default `hair` initializer.
static constexpr int MATERIAL_HAS_HAIR = (1 << 7);

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

  /// An instance of the material.
  struct Instance final {
  public:
    /// Is null?
    [[nodiscard]] bool operator!() const noexcept {
      return jit_struct == nullptr;
    }

    /// Is non-null?
    [[nodiscard]] operator bool() const noexcept {
      return jit_struct != nullptr;
    }

  public:
    /// The JIT struct memory block.
    ///
    /// This holds the JIT material structure, which is entirely opaque to the
    /// user over in C++ land. Just ignore this!
    ///
    const void *jit_struct{};

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

    /// The wavelength count.
    int wavelength_base_max{};

    /// The flags.
    int flags{};

    /// The df flags for the material `surface` component.
    int df_flags_surface{};

    /// The df flags for the material `backface` component.
    int df_flags_backface{};

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

  /// The index of refraction.
  [[nodiscard]] float getIOR() const noexcept { return instance.ior; }

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
