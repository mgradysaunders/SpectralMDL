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

  /// Is thin walled?
  static constexpr int THIN_WALLED = (1 << 0);

  /// Has a non-default surface component?
  static constexpr int HAS_SURFACE = (1 << 1);

  /// Has a non-default backface surface component?
  static constexpr int HAS_BACKFACE = (1 << 2);

  /// Has a non-default surface emission component?
  static constexpr int HAS_SURFACE_EMISSION = (1 << 3);

  /// Has a non-default backface emission component?
  static constexpr int HAS_BACKFACE_EMISSION = (1 << 4);

  /// Has a non-default volume component?
  static constexpr int HAS_VOLUME = (1 << 5);

  /// Has a non-default hair component?
  static constexpr int HAS_HAIR = (1 << 6);

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
  Function<int(const Instance &instance, TransportMode transport,
               const float3 &wo, const float3 &wi, float &pdf_fwd,
               float &pdf_rev, float *f)>
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
  Function<int(const Instance &instance, TransportMode transport,
               const float4 &xi, const float3 &wo, float3 &wi, float &pdf_fwd,
               float &pdf_rev, float *f, int &is_delta)>
      scatter_sample{};

  /// The emission evaluate function.
  //
  /// \param[in] instance
  /// The instance obtained from the `allocate` function.
  ///
  /// \param[in] we
  /// The emission direction in tangent space.
  ///
  /// \param[out] pdf
  /// The PDF.
  ///
  /// \param[out] Le
  /// The emission spectrum. This must be non-null!
  ///
  Function<int(const Instance &instance, const float3 &we, float &pdf,
               float *Le)>
      emission_evaluate{};

  /// The emission sample function.
  ///
  /// \param[in] instance
  /// The instance obtained from the `allocate` function.
  ///
  /// \param[in] xi
  /// The canonical random sample in \f$ [0,1]^4 \f$.
  ///
  /// \param[out] we
  /// The emission direction in tangent space.
  ///
  /// \param[out] pdf
  /// The PDF.
  ///
  /// \param[out] Le
  /// The emission spectrum. This must be non-null!
  ///
  Function<int(const Instance &instance, const float4 &xi, float3 &we,
               float &pdf, float *Le)>
      emission_sample{};
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
