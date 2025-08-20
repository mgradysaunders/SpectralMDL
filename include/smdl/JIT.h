/// \file
#pragma once

#include "smdl/common.h"

namespace smdl {

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
    /// The material memory block.
    ///
    /// This holds the JIT material structure, which is entirely opaque to the
    /// user over in C++ land. Just ignore this!
    ///
    const void *mat{};

    /// The displacement vector.
    const float3 *displacement{};

    /// The cutout opacity.
    const float *cutout_opacity{};

    /// The normal.
    const float3 *normal{};

    /// The index of refraction.
    const float *ior{};

    /// The volume absorption coefficient if applicable.
    const float *absorption_coefficient{};

    /// The volume scattering coefficient if applicable.
    const float *scattering_coefficient{};

    /// The wavelength count.
    int wavelength_base_max{};

    /// The flags.
    int flags{};

    /// The df flags for the material `surface` component.
    int df_flags_surface{};

    /// The df flags for the material `backface` component.
    int df_flags_backface{};

    /// The tangent space matrix held by the `State` when constructing the
    /// instance.
    float3x3 tangent_space{};
  };

  /// The allocate function.
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
  Function<void(State &state, Instance &instance)> allocate{};

  /// The scatter evaluate function.
  ///
  /// \param[in] instance
  /// The instance obtained from the `allocate` function.
  ///
  /// \param[in] wo
  /// The outgoing direction in world space.
  ///
  /// \param[in] wi
  /// The incoming direction in world space.
  ///
  /// \param[out] pdf_fwd
  /// The forward PDF of sampling `wi` given `wo`.
  ///
  /// \param[out] pdf_rev
  /// The reverse PDF of sampling `wo` given `wi`.
  ///
  /// \param[out] f
  /// The BSDF spectrum. This must be non-null!
  ///
  /// \return
  /// Returns `true` if the result is non-zero.
  ///
  Function<int(const Instance &instance, const float3 &wo, const float3 &wi,
               float &pdf_fwd, float &pdf_rev, float *f)>
      scatter_evaluate{};

  /// The scatter sample function.
  ///
  /// \param[in] instance
  /// The instance obtained from the `allocate` function.
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
  /// \param[out] pdf_fwd
  /// The forward PDF of sampling `wi` given `wo`.
  ///
  /// \param[out] pdf_rev
  /// The reverse PDF of sampling `wo` given `wi`.
  ///
  /// \param[out] f
  /// The BSDF spectrum. This must be non-null!
  ///
  /// \param[out] is_delta
  /// Set to `true` if sampling Dirac delta distribution.
  ///
  Function<int(const Instance &instance, const float4 &xi, const float3 &wo,
               float3 &wi, float &pdf_fwd, float &pdf_rev, float *f,
               int &is_delta)>
      scatter_sample{};
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
