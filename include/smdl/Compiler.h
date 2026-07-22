/// \file
#pragma once

#include <map>
#include <set>

#include "smdl/BSDFMeasurement.h"
#include "smdl/FileLocator.h"
#include "smdl/Image.h"
#include "smdl/JIT.h"
#include "smdl/LightProfile.h"
#include "smdl/Module.h"
#include "smdl/SceneData.h"
#include "smdl/Spectrum.h"
#include "smdl/Support/MD5Hash.h"

namespace smdl {

/// \addtogroup Support
/// \{

/// An opaque Ptex texture.
///
/// If built without Ptex (`-DSMDL_ENABLE_PTEX=OFF`), this
/// is never populated by the compiler and is passed around as
/// the nullified default.
class SMDL_EXPORT Ptexture final {
public:
  Ptexture() = default;

  Ptexture(const Ptexture &) = delete;

  Ptexture(Ptexture &&other) noexcept
      : texture(std::exchange(other.texture, nullptr)),
        textureFilter(std::exchange(other.textureFilter, nullptr)),
        channelCount(std::exchange(other.channelCount, 0)),
        alphaIndex(std::exchange(other.alphaIndex, -1)) {}

  Ptexture &operator=(const Ptexture &) = delete;

  Ptexture &operator=(Ptexture &&other) noexcept {
    if (this != &other) {
      release();
      texture = std::exchange(other.texture, nullptr);
      textureFilter = std::exchange(other.textureFilter, nullptr);
      channelCount = std::exchange(other.channelCount, 0);
      alphaIndex = std::exchange(other.alphaIndex, -1);
    }
    return *this;
  }

  ~Ptexture() { release(); }

  /// Release the held `PtexTexture` and `PtexFilter` if present.
  void release() noexcept;

public:
  /// The pointer to the `PtexTexture`.
  void *texture{};

  /// The pointer to the `PtexFilter`. May be null: `smdlPtexEvaluate`
  /// maintains per-thread filters because `PtexFilter::eval` is not
  /// thread-safe.
  void *textureFilter{};

  /// The channel count.
  int channelCount{};

  /// The alpha channel index if present, else `-1`.
  int alphaIndex{-1};
};

/// \}

/// \addtogroup Main
/// \{

/// The optimization level.
enum OptLevel : int {
  OPT_LEVEL_NONE = 0, ///< No optimization at all.
  OPT_LEVEL_O1 = 1,   ///< Level 1 - basic optimizations.
  OPT_LEVEL_O2 = 2,   ///< Level 2 - sensible optimizations.
  OPT_LEVEL_O3 = 3    ///< Level 3 - aggressive optimizations.
};

/// The dump format for `Compiler::dump()`.
enum DumpFormat : int {
  DUMP_FORMAT_IR,  ///< LLVM-IR.
  DUMP_FORMAT_ASM, ///< Native assembly code.
  DUMP_FORMAT_OBJ  ///< Native object code.
};

/// The compiler.
///
/// \note
/// Lifetime contract:
/// - `compile()` frees the previous JIT, so every function pointer
///   previously obtained (materials, unit tests, execs, color conversion)
///   dangles. No thread may be executing JIT-compiled code during
///   `compile()` or `jitCompile()`.
/// - JIT-compiled code embeds absolute pointers to host data owned by this
///   `Compiler` (images, spectra, `sceneData`, ...). The `Compiler` must
///   outlive all execution of its JIT-compiled code, and object files
///   emitted by `dump()` are not relocatable into other processes.
class SMDL_EXPORT Compiler final {
public:
  Compiler(uint32_t wavelengthBaseMax = 16);

  Compiler(const Compiler &) = delete;

  ~Compiler();

  /// Add MDL module file or directory.
  [[nodiscard]] std::optional<Error> add(std::string fileOrDirName) noexcept;

  /// Compile to LLVM-IR.
  [[nodiscard]] std::optional<Error>
  compile(OptLevel optLevel = OPT_LEVEL_O2) noexcept;

  /// Format source code.
  [[nodiscard]] std::optional<Error>
  formatSourceCode(const FormatOptions &formatOptions) noexcept;

private:
  /// Get the LLVM context, or throw an `Error` if there is none (i.e., if
  /// `compile()` has not run yet or `jitCompile()` already consumed it).
  [[nodiscard]] llvm::LLVMContext &getLLVMContext();

  /// Get the LLVM module, or throw an `Error` if there is none (i.e., if
  /// `compile()` has not run yet or `jitCompile()` already consumed it).
  [[nodiscard]] llvm::Module &getLLVMModule();

  /// Load image.
  [[nodiscard]] const Image &loadImage(const std::string &fileName,
                                       const SourceLocation &srcLoc);

  /// Load ptex texture.
  [[nodiscard]] const Ptexture &loadPtexture(const std::string &fileName,
                                             const SourceLocation &srcLoc);

  /// Load BSDF measurement.
  [[nodiscard]]
  const BSDFMeasurement &loadBSDFMeasurement(const std::string &fileName,
                                             const SourceLocation &srcLoc);

  /// Load light profile.
  [[nodiscard]]
  const LightProfile &loadLightProfile(const std::string &fileName,
                                       const SourceLocation &srcLoc);

  /// Load spectrum from TXT file.
  [[nodiscard]]
  SpectrumView loadSpectrum(const std::string &fileName,
                            const SourceLocation &srcLoc);

  /// Load spectrum from ENVI Spectral Library file.
  [[nodiscard]]
  SpectrumView loadSpectrum(const std::string &fileName, int curveIndex,
                            const SourceLocation &srcLoc);

  /// Load spectrum from ENVI Spectral Library file.
  [[nodiscard]]
  SpectrumView loadSpectrum(const std::string &fileName,
                            const std::string &curveName,
                            const SourceLocation &srcLoc);

  /// Load ENVI Spectral Library file.
  [[nodiscard]]
  const SpectrumLibrary &loadSpectrumLibrary(const std::string &fileName,
                                             const SourceLocation &srcLoc);

public:
  /// Dump as LLVM-IR or native assembly into `out`. Must be called after
  /// `compile()` and before `jitCompile()`.
  [[nodiscard]] std::optional<Error> dump(DumpFormat dumpFormat,
                                          std::string &out) noexcept;

  /// JIT-compile to machine code.
  [[nodiscard]] std::optional<Error> jitCompile() noexcept;

private:
  /// After JIT-compiling, lookup symbol with the given name, or throw an
  /// `Error` carrying the underlying ORC failure message.
  [[nodiscard]] void *jitLookup(std::string_view name);

  /// After JIT-compiling, lookup symbol with the given name or throw an error
  /// if it is not present.
  template <typename T> void jitLookupOrThrow(JIT::Function<T> &func) {
    func.func = reinterpret_cast<typename JIT::Function<T>::function_pointer>(
        jitLookup(func.name));
    if (!func.func)
      throw Error(concat("cannot resolve JIT function ", Quoted(func.name)));
  }

public:
  /// Find JIT-compiled material named `materialName`, or return `nullptr` on
  /// failure.
  ///
  /// \note
  /// This assumes that `materialName` is unique across all MDL modules. If
  /// this is not the case, the implementation logs a warning to report the
  /// ambiguity and returns the first material found.
  ///
  [[nodiscard]]
  const JIT::Material *
  findMaterial(std::string_view materialName) const noexcept;

  /// Find JIT-compiled material named `materialName` in the MDL module named
  /// `moduleName`, or return `nullptr` on failure.
  [[nodiscard]]
  const JIT::Material *
  findMaterial(std::string_view moduleName,
               std::string_view materialName) const noexcept;

  /// Run the JIT-compiled color-to-RGB function.
  ///
  /// \param[in] state
  /// The state. Must have the wavelength parameters set:
  /// - `state.wavelength_base`
  /// - `state.wavelength_min`
  /// - `state.wavelength_max`
  ///
  /// \param[in] color
  /// The pointer to the color spectrum.
  ///
  [[nodiscard]]
  float3 convertColorToRGB(const State &state,
                           const float *color) const noexcept;

  /// Run the JIT-compiled RGB-to-color function.
  ///
  /// \param[in] state
  /// The state. Must have the wavelength parameters set:
  /// - `state.wavelength_base`
  /// - `state.wavelength_min`
  /// - `state.wavelength_max`
  ///
  /// \param[in] rgb
  /// The RGB triple.
  ///
  /// \param[out] color
  /// The pointer to the color spectrum.
  ///
  void convertRGBToColor(const State &state, const float3 &rgb,
                         float *color) const noexcept;

  /// Run JIT-compiled unit tests and print results to standard error.
  [[nodiscard]] std::optional<Error> runUnitTests(const State &state) noexcept;

  /// Run JIT-compiled execs.
  [[nodiscard]] std::optional<Error> runExecs() noexcept;

  /// Summarize all compiled materials.
  [[nodiscard]] std::string printMaterialSummary() const;

public:
  /// The file locator.
  FileLocator fileLocator{};

  /// Enable debugging?
  bool enableDebug{false};

  /// Enable unit tests?
  bool enableUnitTests{false};

  /// The number of wavelengths per MDL `color`.
  uint32_t wavelengthBaseMax{16};

  /// The scene data.
  SceneData sceneData{};

private:
  /// The allocator.
  ///
  /// \note
  /// This is used during the active compiling phase to allocate
  /// AST nodes, type representations, crumbs, etc. Once `jit_compile()`
  /// is called and everything is finalized as JIT-linked native code,
  /// intermediate representations are dropped and the allocator is
  /// reset.
  ///
  BumpPtrAllocator mAllocator{};

  /// The MD5 file hasher.
  MD5FileHasher mFileHasher{};

  /// The images used by textures.
  std::map<const MD5FileHash *, Image> mImages;

  /// The ptex textures.
  std::map<const MD5FileHash *, Ptexture> mPtextures;

  /// The BSDF measurements.
  std::map<const MD5FileHash *, BSDFMeasurement> mBSDFMeasurements;

  /// The light profiles.
  std::map<const MD5FileHash *, LightProfile> mLightProfiles;

  /// The spectrums.
  std::map<const MD5FileHash *, Spectrum> mSpectrums;

  /// The spectrum libraries.
  std::map<const MD5FileHash *, SpectrumLibrary> mSpectrumLibraries;

  /// The MDL module file names.
  std::set<std::string> mModuleFileNames;

  /// The MDL module directory names.
  std::set<std::string> mModuleDirNames;

  /// The MDL module directory search paths.
  ///
  /// \note
  /// This is maintained separately from `moduleDirNames` but should
  /// contain all of the same paths. This is necessary to preserve the
  /// order in which the paths were added.
  ///
  std::vector<std::string> mModuleDirSearchPaths;

  /// The MDL modules.
  std::vector<std::unique_ptr<Module>> mModules;

  /// Reset all JIT-derived state for a (re)compile: frees the previous
  /// JIT, clears all resource maps and JIT handle tables, and creates a
  /// fresh LLVM context, module, and JIT.
  void resetForRecompile();

  /// The LLVM context for the module being compiled. Consumed (moved into
  /// the JIT) by `jitCompile()`.
  std::unique_ptr<llvm::LLVMContext> mLLVMContext;

  /// The LLVM module being compiled. Consumed by `jitCompile()`.
  std::unique_ptr<llvm::Module> mLLVMModule;

  /// The names and host addresses of the builtin runtime callees
  /// registered during emission (see `Context::getBuiltinCallee`). These
  /// are defined as absolute symbols in the JIT so resolution does not
  /// depend on the host process exporting them (e.g. a statically linked
  /// host without `--export-dynamic`).
  std::map<std::string, const void *, std::less<>> mBuiltinCalleeAddresses;

  /// The LLVM JIT.
  std::unique_ptr<llvm::orc::LLJIT> mLLVMJit;

  /// Asynchronous errors reported by the LLVM JIT execution session,
  /// accumulated so they can be surfaced in the `Error` returned by
  /// `jitCompile()` instead of only going to standard error.
  std::string mJITSessionErrors;

  /// The JIT-compiled color-to-RGB conversion function.
  JIT::Function<void(const State &state, const float *cptr, float3 &rgb)>
      mColorToRGB{"smdlColorToRGB"};

  /// The JIT-compiled RGB-to-color conversion function.
  JIT::Function<void(const State &state, const float3 &rgb, float *cptr)>
      mRGBToColor{"smdlRGBToColor"};

  /// The JIT-compiled materials.
  std::vector<JIT::Material> mMaterials;

  /// The JIT-compiled unit tests.
  std::vector<JIT::UnitTest> mUnitTests;

  /// The JIT-compiled execs.
  std::vector<JIT::Function<void()>> mExecs;

  friend class Context;

  friend class Emitter;

  friend class FunctionType;
};

/// \}

} // namespace smdl
