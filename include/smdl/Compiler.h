#pragma once

#include <map>
#include <set>

#include "smdl/FileLocator.h"
#include "smdl/Image.h"
#include "smdl/JIT.h"
#include "smdl/Module.h"
#include "smdl/SceneData.h"

namespace smdl {

/// \addtogroup Support
/// \{

/// An opaque Ptex texture.
///
/// If built without Ptex (`-DSMDL_ENABLE_PTEX=OFF`), this
/// is never populated by the compiler and is passed around as
/// the nullified default.
class Ptexture final {
public:
  /// The pointer to the `PtexTexture`.
  void *texture{};

  /// The pointer to the `PtexFilter`.
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
enum class OptLevel : uint32_t {
  None = 0, ///< No optimization at all.
  O1 = 1,   ///< Level 1 - basic optimizations.
  O2 = 2,   ///< Level 2 - sensible optimizations.
  O3 = 3    ///< Level 3 - aggressive optimizations.
};

/// The dump format for `Compiler::dump()`.
enum class DumpFormat : uint32_t {
  IR,       ///< LLVM-IR.
  Assembly, ///< Native assembly code.
  Object    ///< Native object code.
};

/// The compiler.
class SMDL_EXPORT Compiler final {
public:
  Compiler(uint32_t wavelengthBaseMax = 16);

  Compiler(const Compiler &) = delete;

  ~Compiler();

  /// Add MDL module file or directory.
  [[nodiscard]] std::optional<Error> add(std::string fileOrDirName);

  /// Compile to LLVM-IR.
  [[nodiscard]] std::optional<Error> compile(OptLevel optLevel = OptLevel::O2);

  /// Format source code.
  [[nodiscard]] std::optional<Error>
  format_source_code(const FormatOptions &formatOptions) noexcept;

private:
  /// Get the LLVM context.
  [[nodiscard]] llvm::LLVMContext &get_llvm_context() noexcept;

  /// Get the LLVM module.
  [[nodiscard]] llvm::Module &get_llvm_module() noexcept;

  /// Load image.
  [[nodiscard]] const Image &load_image(const std::string &fileName,
                                        const SourceLocation &srcLoc);

  /// Load ptex texture.
  [[nodiscard]] Ptexture load_ptexture(const std::string &fileName,
                                       const SourceLocation &srcLoc);

public:
  /// Dump as LLVM-IR or native assembly.
  [[nodiscard]] std::string dump(DumpFormat dumpFormat);

  /// JIT-compile to machine code.
  [[nodiscard]] std::optional<Error> jit_compile() noexcept;

private:
  /// After JIT-compiling, lookup symbol with the given name.
  [[nodiscard]] void *jit_lookup(std::string_view name) noexcept;

  /// After JIT-compiling, lookup symbol with the given name or throw an error
  /// if it is not present.
  template <typename T> void jit_lookup_or_throw(JIT::Function<T> &func) {
    func.func = reinterpret_cast<typename JIT::Function<T>::function_pointer>(
        jit_lookup(func.name));
    if (!func.func)
      throw Error(concat("cannot resolve JIT function ", quoted(func.name)));
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
  find_jit_material(std::string_view materialName) const noexcept;

  /// Find JIT-compiled material named `materialName` in the MDL module named
  /// `moduleName`, or return `nullptr` on failure.
  [[nodiscard]]
  const JIT::Material *
  find_jit_material(std::string_view moduleName,
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
  float3 jit_color_to_rgb(const State &state,
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
  void jit_rgb_to_color(const State &state, const float3 &rgb,
                        float *color) const noexcept;

  /// Run JIT-compiled unit tests and print results to standard error.
  [[nodiscard]] std::optional<Error>
  run_jit_unit_tests(const State &state) noexcept;

  /// Summarize all compiled materials.
  [[nodiscard]] std::string summarize_materials() const;

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
  BumpPtrAllocator allocator{};

  /// The images used by textures.
  std::map<std::string, std::unique_ptr<Image>, std::less<>> images{};

  /// The ptex textures.
  std::map<std::string, Ptexture, std::less<>> ptextures{};

  /// The MDL module file names.
  std::set<std::string> moduleFileNames{};

  /// The MDL module directory names.
  std::set<std::string> moduleDirNames{};

  /// The MDL modules.
  std::vector<std::unique_ptr<Module>> modules{};

  /// The LLVM JIT module.
  std::unique_ptr<llvm::orc::ThreadSafeModule> llvmJitModule;

  /// The LLVM JIT.
  std::unique_ptr<llvm::orc::LLJIT> llvmJit;

  /// The JIT-compiled color-to-RGB conversion function.
  JIT::Function<void(const State &state, const float *cptr, float3 &rgb)>
      jitColorToRgb{"jit_color_to_rgb"};

  /// The JIT-compiled RGB-to-color conversion function.
  JIT::Function<void(const State &state, const float3 &rgb, float *cptr)>
      jitRgbToColor{"jit_rgb_to_color"};

  /// The JIT-compiled materials.
  std::vector<JIT::Material> jitMaterials{};

  /// The JIT-compiled unit tests.
  std::vector<JIT::UnitTest> jitUnitTests{};

  /// The JIT-compiled exec blocks.
  std::vector<JIT::Function<void()>> jitExecs{};

  friend class Context;

  friend class Emitter;

  friend class FunctionType;

  friend class Texture2DType;

  friend class TexturePtexType;
};

/// \}

} // namespace smdl
