/// \file
#pragma once

#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <functional>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "smdl/Export.h"
#include "smdl/Support/BumpPtrAllocator.h"
#include "smdl/Support/Error.h"
#include "smdl/Support/Filesystem.h"
#include "smdl/Support/MacroHelpers.h"
#include "smdl/Support/Span.h"
#include "smdl/Support/StringHelpers.h"
#include "smdl/Support/VectorMath.h"

namespace llvm {

class Constant;
class ConstantInt;
class DataLayout;
class LLVMContext;
class Module;
class TargetMachine;
class Type;
class Value;

namespace orc {

class ThreadSafeModule;
class LLJIT;

} // namespace orc

} // namespace llvm

/// The top-level SMDL namespace.
namespace smdl {

/// \defgroup Support Support
/// \{
/// \}

/// \defgroup Main Main
/// \{

/// The SMDL build information.
class SMDL_EXPORT BuildInfo final {
public:
  /// Get.
  [[nodiscard]] static BuildInfo get() noexcept;

public:
  /// The major version number.
  uint32_t major{};

  /// The minor version number.
  uint32_t minor{};

  /// The patch version number.
  uint32_t patch{};

  /// The git branch name.
  const char *gitBranch{};

  /// The git commit hash.
  const char *gitCommit{};
};

/// The LLVM native target.
class SMDL_EXPORT NativeTarget final {
public:
  /// Get.
  [[nodiscard]] static const NativeTarget &get() noexcept;

public:
  /// The CPU name.
  std::string_view name{};

  /// The CPU triple.
  std::string_view triple{};

  /// The LLVM target machine representation.
  llvm::TargetMachine *machine{};
};

/// \}

/// \addtogroup Main
/// \{

class Compiler;
class Module;
class Type;

/// A source location somewhere in an MDL module.
class SMDL_EXPORT SourceLocation final {
public:
  /// Get the module name.
  [[nodiscard]] std::string_view get_module_name() const;

  /// Get the file name.
  [[nodiscard]] std::string_view get_module_file_name() const;

  /// Log a warning.
  void log_warn(std::string_view message) const;

  /// Log an error.
  void log_error(std::string_view message) const;

  /// Throw an `Error`.
  void throw_error(std::string message) const;

  /// Throw an `Error` using `concat` to concatenate the arguments.
  template <typename T0, typename T1, typename... Ts>
  void throw_error(T0 &&value0, T1 &&value1, Ts &&...values) const {
    throw_error(concat(std::forward<T0>(value0), std::forward<T1>(value1),
                       std::forward<Ts>(values)...));
  }

  /// Is not-valid?
  [[nodiscard]] bool operator!() const { return !module_; }

  /// Is valid?
  [[nodiscard]] operator bool() const { return module_; }

  /// Convert to string.
  [[nodiscard]] operator std::string() const;

public:
  /// The associated MDL module, which contains the filename and source code.
  Module *module_{};

  /// The line number.
  uint32_t lineNo{1};

  /// The character number in the line.
  uint32_t charNo{1};

  /// The raw index in the source code string.
  uint64_t i{};
};

/// The format options.
class SMDL_EXPORT FormatOptions final {
public:
  /// Format files in-place. If false, prints formatted source code to `stdout`.
  bool inPlace{};

  /// Remove comments from formatted source code.
  bool noComments{};

  /// Remove annotations from formatted source code.
  bool noAnnotations{};

  /// Want compact?
  bool compact{};
};

/// The transport mode.
enum Transport : int {
  /// Transport radiance (tracing paths from cameras to lights).
  TRANSPORT_RADIANCE = 0,
  /// Transport importance (tracing paths from lights to cameras).
  TRANSPORT_IMPORTANCE = 1,
};

/// The MDL state passed in at runtime.
class SMDL_EXPORT State final {
public:
  /// Finalize and apply internal space conventions.
  void finalize_and_apply_internal_space_conventions() noexcept;

public:
  /// The allocator, which must point to thread-local
  /// instance of `BumpPtrAllocator`.
  void *allocator{};

  /// The wavelengths in nanometers, must be sorted in increasing order!
  const float *wavelength_base{};

  /// The minimum wavelength in nanometers.
  float wavelength_min{};

  /// The maximum wavelength in nanometers.
  float wavelength_max{};

  /// The meters per scene unit.
  float meters_per_scene_unit{1.0f};

  /// The animation time.
  float animation_time{0.0f};

  /// The object ID.
  int object_id{};

  /// If applicable, the Ptex face ID.
  int ptex_face_id{};

  /// If applicable, the Ptex face UV.
  float2 ptex_face_uv{};

  /// The position or ray intersection point in object space.
  float3 position{};

#if 0
  /// The direction in the context of an environment lookup.
  ///
  /// \note
  /// Not sure exactly how this fits yet.
  ///
  float3 direction{};
#endif

  /// The motion vector in object space.
  float3 motion{};

  /// The normal in object space.
  float3 normal{0, 0, 1};

  /// The geometry normal in object space.
  float3 geometry_normal{0, 0, 1};

  /// The max supported number of texture spaces.
  static constexpr size_t TEXTURE_SPACE_MAX = 4;

  /// The number of texture spaces.
  int texture_space_max{1};

  /// The texture coordinates.
  float3 texture_coordinate[TEXTURE_SPACE_MAX]{};

  // The texture tangent U vector(s) in object space.
  float3 texture_tangent_u[TEXTURE_SPACE_MAX] = {
      float3{1, 0, 0}, float3{1, 0, 0}, float3{1, 0, 0}, float3{1, 0, 0}};

  // The texture tangent V vector(s) in object space.
  float3 texture_tangent_v[TEXTURE_SPACE_MAX] = {
      float3{0, 1, 0}, float3{0, 1, 0}, float3{0, 1, 0}, float3{0, 1, 0}};

  // The geometry tangent U vector(s) in object space.
  float3 geometry_tangent_u[TEXTURE_SPACE_MAX] = {
      float3{1, 0, 0}, float3{1, 0, 0}, float3{1, 0, 0}, float3{1, 0, 0}};

  // The geometry tangent V vector(s) in object space.
  float3 geometry_tangent_v[TEXTURE_SPACE_MAX] = {
      float3{0, 1, 0}, float3{0, 1, 0}, float3{0, 1, 0}, float3{0, 1, 0}};

  /// The tangent-to-object matrix.
  ///
  /// The tangent space is the coordinate system where
  /// - The X axis is aligned to the geometry tangent in U.
  /// - The Y axis is aligned to the geometry tangent in V.
  /// - The Z axis is aligned to the geometry normal.
  /// - The origin is the ray intersection point.
  ///
  /// Do not populate this!
  ///
  /// Instead call `finalize_and_apply_internal_space_conventions()`
  /// to compute this from `geometry_tangent_u[0]`, `geometry_tangent_v[0]`,
  /// `geometry_normal`, and `position`.
  ///
  float4x4 tangent_to_object_matrix{float4x4(1.0f)};

  /// The object-to-world matrix.
  float4x4 object_to_world_matrix{float4x4(1.0f)};

  /// The transport mode.
  ///
  /// \note
  /// This is necessary to account for asymmetric scattering in
  /// bidirectional methods.
  /// - `TRANSPORT_RADIANCE` means tracing paths from cameras to lights,
  /// - `TRANSPORT_IMPORTANCE` means tracing paths from lights to cameras.
  ///
  Transport transport{TRANSPORT_RADIANCE};
};

/// An albedo look-up table (LUT) for energy compensation in lossy BSDFs.
class SMDL_EXPORT AlbedoLUT final {
public:
  /// The number of samples of the cosine of the viewing angle.
  const int num_cos_theta = 0;

  /// The number of samples of the roughness parameter.
  const int num_roughness = 0;

  /// The directional albedo.
  ///
  /// \note
  /// This must point to `num_cos_theta` rows by `num_roughness` values.
  ///
  const float *const directional_albedo = nullptr;

  /// The average albedo.
  ///
  /// \note
  /// This must point to `num_roughness` values.
  ///
  const float *const average_albedo = nullptr;
};

/// \}

} // namespace smdl
