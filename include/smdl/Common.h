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
#include "smdl/Support/Macros.h"
#include "smdl/Support/RNG.h"
#include "smdl/Support/Span.h"
#include "smdl/Support/Strings.h"
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

/// \defgroup compiler Compiler
///
/// Everything involved in turning MDL source code into executable native
/// code: the `Compiler` front end, the `Module` and `Parser` layers beneath
/// it, the `JIT` interface that the resulting materials are called through,
/// and the documentation extractor.
///
/// The usual entry point is `Compiler`. Add search paths and modules to it,
/// compile, then look up materials and invoke them through the types in
/// the `smdl::JIT` namespace.
///
/// \{
/// \}

/// \defgroup resource Resources
///
/// The resource and runtime types a renderer hands to, or receives from,
/// compiled MDL materials: images and textures, spectra, measured BSDFs and
/// light profiles, scene data lookups, and the shading `State` that
/// material functions read from.
///
/// \{
/// \}

/// \defgroup manifold Manifold
///
/// The renderer-agnostic core of manifold next-event estimation and
/// specular manifold sampling: the Newton solver over renderer-supplied
/// surfaces, the connection measures, the reciprocal trial loop, and the
/// eligibility questions answerable from `JIT` material instances.
///
/// \{
/// \}

/// \defgroup ast Abstract Syntax Tree
///
/// The parsed representation of an MDL module, produced by `Parser` and
/// consumed by the compiler.
///
/// Nodes derive from `Node` and are discriminated by a kind enum, so they
/// are traversed with `llvm::isa`, `llvm::dyn_cast`, and `llvm::cast`.
/// The three node categories each have their own subgroup.
///
/// \{
/// \}

/// \defgroup renderutil Render Utilities
///
/// Algorithmic rendering utilities layered on the support types and usable
/// by any renderer: color and spectral containers, Monte Carlo sampling,
/// illuminants and metal IORs, the sun-sky model, and
/// spectral render images.
///
/// \{
/// \}

/// \defgroup support Support
///
/// Standalone mechanical utility types and functions used throughout the
/// library and usable independently of it: vector and matrix math, string
/// and filesystem helpers, logging, hashing, random number generation, and
/// allocation.
///
/// \{
/// \}

/// \addtogroup compiler
/// \{

/// The SMDL build information.
class SMDL_EXPORT BuildInfo final {
public:
  /// A third-party dependency: its name and the version linked, or "off"
  /// for one the build configured out.
  struct ThirdParty final {
    std::string name{};
    std::string version{};
  };

  /// Get.
  [[nodiscard]] static BuildInfo get() noexcept;

  /// Summarize as a human-readable multi-line string, with `thirdparty`
  /// as one comma-separated list wrapped to 80 columns.
  [[nodiscard]] std::string toString() const;

public:
  /// The major version number.
  uint32_t major{};

  /// The minor version number.
  uint32_t minor{};

  /// The patch version number.
  uint32_t patch{};

  /// The git branch name, or "unknown" if it was unavailable at build time.
  const char *gitBranch{};

  /// The git commit hash, or "unknown" if it was unavailable at build time.
  const char *gitCommit{};

  /// The LLVM version linked into the library. Never null.
  const char *llvmVersion{};

  /// The compile date and time, from `__DATE__` and `__TIME__`. Never null.
  /// This tracks the translation unit that defines `get()`, so an
  /// incremental rebuild of other code does not refresh it.
  const char *buildDate{};

  /// Was the library built with RTTI?
  bool hasRTTI{};

  /// Does `parallelFor()` schedule dynamically? False is the fixed split
  /// of the range into contiguous tasks. This changes only how fast a
  /// parallel loop runs, never what it computes.
  bool hasDynamicScheduling{};

  /// The version of the vendored miniz. Never null.
  const char *withMiniz{};

  /// The version of the vendored stb_image. Never null.
  const char *withSTBImage{};

  /// The version of the vendored stb_image_write. Never null.
  const char *withSTBImageWrite{};

  /// The version of the vendored stb_image_resize2. Never null.
  const char *withSTBImageResize{};

  /// The version of the vendored tinyexr. Never null.
  const char *withTinyEXR{};

  /// The pinned Ptex release tag, or null if built without Ptex.
  const char *withPtex{};

  /// The pinned OpenVDB release tag providing NanoVDB, or null if built
  /// without NanoVDB.
  const char *withNanoVDB{};

  /// The dependencies above in the order `toString()` lists them. A
  /// program linking dependencies of its own appends them before
  /// printing, so that the list reads as one.
  std::vector<ThirdParty> thirdparty{};
};

/// The LLVM native target.
class SMDL_EXPORT NativeTarget final {
public:
  /// Get.
  [[nodiscard]] static const NativeTarget &get() noexcept;

public:
  /// The CPU name.
  std::string name{};

  /// The CPU triple.
  std::string triple{};

  /// The LLVM target machine representation.
  llvm::TargetMachine *machine{};
};

/// \}

/// \addtogroup compiler
/// \{

class Compiler;
class Module;
class Type;

/// A source location somewhere in an MDL module.
class SMDL_EXPORT SourceLocation final {
public:
  /// Get the module name.
  [[nodiscard]] std::string_view getModuleName() const;

  /// Get the file name. This is empty unless the module is file backed.
  [[nodiscard]] std::string_view getModuleFileName() const;

  /// Get the name to print in diagnostics, which is the file name for
  /// ordinary modules and origin markup for the others. See
  /// `Module::getDisplayName()`.
  [[nodiscard]] std::string_view getModuleDisplayName() const;

  /// Get the source line containing this location with a caret under the
  /// relevant column, as a block that begins with a newline so that it may
  /// be appended after a diagnostic message and whatever notes follow it.
  /// Returns the empty string if there is no source code to show.
  [[nodiscard]] std::string getSourceSnippet() const;

  /// Log a warning.
  void logWarn(std::string_view message) const;

  /// Log an error.
  void logError(std::string_view message) const;

  /// Throw an `Error`.
  [[noreturn]] void throwError(std::string message) const;

  /// Throw an `Error` using `concat` to concatenate the arguments.
  template <typename T0, typename T1, typename... Ts>
  [[noreturn]] void throwError(T0 &&value0, T1 &&value1, Ts &&...values) const {
    throwError(concat(std::forward<T0>(value0), std::forward<T1>(value1),
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

  /// Keep `///` and `///<` documentation comments even when
  /// `noComments` is true.
  bool keepDocComments{};

  /// Remove annotations from formatted source code.
  bool noAnnotations{};

  /// Want compact?
  bool compact{};
};

/// \}

/// \addtogroup resource
/// \{

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
  ///
  /// The implementation does the following:
  /// 1. Orthonormalize the normal and tangent vectors.
  /// 2. Orthonormalize the geometric normal and tangent vectors.
  /// 3. Construct the matrix pair for transforming between geometric tangent
  ///    space and object space.
  /// 4. Transform every member variable defined in object space to
  ///    geometric tangent space.
  /// 5. Orthonormalize the object-to-world matrix, unless it already is,
  ///    in which case it is left exactly as given.
  ///
  /// Afterward,
  /// - `position` is at the origin `float3(0,0,0)`
  /// - `geometry_tangent_u[0]` is the X axis `float3(1,0,0)`
  /// - `geometry_tangent_v[0]` is the Y axis `float3(0,1,0)`
  /// - `geometry_normal` is the Z axis `float3(0,0,1)`
  ///
  void finalizeAndApplyInternalSpaceConventions() noexcept;

public:
  /// The allocator, which must point to thread-local
  /// instance of `BumpPtrAllocator`.
  void *allocator{};

  /// The opaque host context, which is never interpreted by SpectralMDL.
  ///
  /// This is provided so that host applications can associate a `State` with
  /// whatever context it was constructed from, and recover that context in
  /// `@(foreign)` functions and `SceneData::Getter` callbacks, both of which
  /// receive the `State` but are otherwise unable to determine which shading
  /// point they are being asked about.
  ///
  /// \note
  /// The host is responsible for the lifetime of whatever this points to. It
  /// must remain valid for at least as long as the `State` that refers to it.
  void *user_data{};

  /// The wavelengths in nanometers, must be sorted in increasing order!
  const float *wavelength_base{};

  /// The minimum wavelength in nanometers.
  float wavelength_min{};

  /// The maximum wavelength in nanometers.
  float wavelength_max{};

  /// If non-null, this necessarily points to `wavelengthBaseMax`
  /// per-band quadrature weights in nanometers: the effective width of
  /// each band, for integrating spectral quantities over a non-uniform
  /// wavelength grid. Null means the uniform default of
  /// `(wavelength_max - wavelength_min) / wavelengthBaseMax` per band,
  /// which is what color-to-RGB conversion has always assumed.
  const float *wavelength_weight{};

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

  /// The normalized direction of propagation of the ray that produced this
  /// evaluation, pointing toward the shading point, in object space (internal
  /// space after `finalizeAndApplyInternalSpaceConventions()`). In the
  /// context of an environment lookup, the lookup direction.
  ///
  /// \note
  /// Zero if the renderer does not provide it, in which case
  /// direction-dependent material effects must be skipped. Populating this
  /// at surface hits is non-standard.
  float3 direction{};

  /// The motion vector in object space.
  float3 motion{};

  /// The normal in object space.
  float3 normal{0, 0, 1};

  /// The geometry normal in object space.
  float3 geometry_normal{0, 0, 1};

  /// The max supported number of texture spaces.
  ///
  /// \note
  /// Half of `State` scales with this, so it is set to what materials
  /// actually index: a base space and an optional second one, which is as
  /// many as any of the geometry paths fill. A constant index past it is a
  /// compile error in SMDL; `texture_space_max` gates the rest, and
  /// `finalizeAndApplyInternalSpaceConventions()` clamps it.
  static constexpr size_t TEXTURE_SPACE_MAX = 2;

  /// The number of texture spaces, clamped to `TEXTURE_SPACE_MAX`.
  int texture_space_max{1};

  /// The texture coordinates.
  float3 texture_coordinate[TEXTURE_SPACE_MAX]{};

  /// The texture tangent U vector(s) in object space.
  float3 texture_tangent_u[TEXTURE_SPACE_MAX] = {float3{1, 0, 0},
                                                 float3{1, 0, 0}};

  /// The texture tangent V vector(s) in object space.
  float3 texture_tangent_v[TEXTURE_SPACE_MAX] = {float3{0, 1, 0},
                                                 float3{0, 1, 0}};

  /// The geometry tangent U vector(s) in object space.
  float3 geometry_tangent_u[TEXTURE_SPACE_MAX] = {float3{1, 0, 0},
                                                  float3{1, 0, 0}};

  /// The geometry tangent V vector(s) in object space.
  float3 geometry_tangent_v[TEXTURE_SPACE_MAX] = {float3{0, 1, 0},
                                                  float3{0, 1, 0}};

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

  /// The random number generator for stochastic evaluation.
  ///
  /// \note
  /// The contract: the renderer initializes this randomly per evaluation,
  /// e.g., `state.rng = RNG(seed)` or `RNG(seed, stream)`, and the
  /// implementation may draw from it (advancing it) at will during
  /// evaluation. The material instance captures a draw to seed the
  /// generator used by stochastically evaluated BSDFs, e.g., the diffuse
  /// component of `df::micrograin_layer`. Such evaluations are unbiased
  /// in expectation; renderers should vary the seed per path vertex (or
  /// per pixel sample) to decorrelate them. A fixed initial generator
  /// keeps evaluations deterministic.
  RNG rng{};

  /// The transport mode.
  ///
  /// \note
  /// This is necessary to account for asymmetric scattering in
  /// bidirectional methods.
  /// - `TRANSPORT_RADIANCE` means tracing paths from cameras to lights,
  /// - `TRANSPORT_IMPORTANCE` means tracing paths from lights to cameras.
  Transport transport{TRANSPORT_RADIANCE};

  /// The number of path segments traversed to reach this shading point, so
  /// 1 at a primary hit. Zero means "not provided", which consumers must treat
  /// exactly like 1, i.e., highest fidelity.
  ///
  /// NOTE: This is non-standard!
  int scattering_order{};

  /// The accumulated distance in scene units traveled by the path to reach
  /// this shading point. Zero conventionally means "not provided" and implies
  /// highest fidelity.
  ///
  /// \note This is non-standard!
  float travel_distance{};

  /// The pixel ray cone spread angle in radians, using the small-angle
  /// convention that the cone width grows by `cone_angle` per unit
  /// distance. Zero means "no cone", i.e., level-of-detail off.
  ///
  /// \note This is non-standard!
  float cone_angle{};

  /// The pixel ray cone width in scene units at the shading point. Zero means
  /// "no footprint", i.e., level-of-detail off.
  ///
  /// \note This is non-standard!
  float cone_width{};

  /// The UV texture density of each texture space: UV area per world-space
  /// area of the underlying geometry, so `cone_width * sqrt(texture_density)`
  /// is a UV-space filter width. Zero means "unknown", i.e., no filtering.
  /// Renderers must guard the defining division against degenerate geometry:
  /// a degenerate triangle must produce 0, never infinity.
  ///
  /// \note This is non-standard!
  float texture_density[TEXTURE_SPACE_MAX]{};

  /// The max supported number of vertex color sets.
  ///
  /// \note
  /// One: a base RGBA set, which is as many as any geometry path fills. A
  /// constant index past it is a compile error in SMDL; `vertex_color_max`
  /// gates the rest, and `finalizeAndApplyInternalSpaceConventions()`
  /// clamps it.
  ///
  /// \note This is non-standard!
  static constexpr size_t VERTEX_COLOR_MAX = 1;

  /// The number of vertex color sets the geometry carries, clamped to
  /// `VERTEX_COLOR_MAX`. Zero means "not provided".
  ///
  /// \note This is non-standard!
  int vertex_color_max{};

  /// The vertex colors: RGBA as the geometry stores them, interpolated to
  /// the shading point, with no color management and no premultiplication.
  /// White where no set is present, so an ungated read still behaves.
  ///
  /// \note This is non-standard!
  float4 vertex_color[VERTEX_COLOR_MAX] = {float4{1, 1, 1, 1}};
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
