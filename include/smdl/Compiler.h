/// \file
#pragma once

#include <map>
#include <memory>
#include <set>
#include <unordered_map>

#include "smdl/Doc.h"
#include "smdl/JIT.h"
#include "smdl/Module.h"
#include "smdl/Resource/BSDFMeasurement.h"
#include "smdl/Resource/Image.h"
#include "smdl/Resource/LightProfile.h"
#include "smdl/Resource/SceneData.h"
#include "smdl/Resource/Spectrum.h"
#include "smdl/Resource/SpectrumLibrary.h"
#include "smdl/Resource/VoxelGrid.h"
#include "smdl/Support/FileLocator.h"
#include "smdl/Support/MD5Hash.h"

namespace smdl {

/// \addtogroup resource
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

/// \addtogroup compiler
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

/// The color mode for `Compiler::runUnitTests()`.
enum ColorMode : int {
  COLOR_MODE_AUTO,   ///< Colorize only if standard error is a terminal.
  COLOR_MODE_ALWAYS, ///< Colorize even if standard error is redirected.
  COLOR_MODE_NEVER   ///< Never colorize.
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
  ///
  /// \param[in] fileOrDirName
  /// The file or directory name.
  ///
  /// \param[out] addedModuleNames
  /// If non-null, the qualified names of the modules added by this call
  /// are appended in the order they were loaded. Files skipped because
  /// they were already added are not reported.
  ///
  /// A directory is added as a *search root*: every `.mdl` and `.smdl`
  /// file beneath it is loaded as a module whose qualified name is
  /// derived from its relative path, so `<root>/vendor/metals/steel.mdl`
  /// becomes `::vendor::metals::steel`. A single file is added with its
  /// parent directory as an implicit search root, so its qualified name
  /// is just `::stem`. Re-adding the same directory is a no-op. Adding a
  /// directory nested inside, or enclosing, an existing search root is an
  /// error, because nested roots would give modules ambiguous qualified
  /// names.
  ///
  /// If two modules under different roots derive the same qualified
  /// name, the module under the earlier root wins for qualified-name
  /// lookup: the later module still loads and compiles, and relative
  /// imports within its own directory tree still resolve to it, but it
  /// is marked shadowed and a warning is logged.
  ///
  /// MDL archives (`.mdr`) at the top level of each root are also added;
  /// archives deeper in the tree are ignored with a warning. Per the
  /// MDL specification, the archive file name encodes the enclosed
  /// package prefix (`vendor.metals.mdr` provides `::vendor::metals`),
  /// every `.mdl` entry must be the enclosed module (`vendor/metals.mdl`)
  /// or live under the enclosed package directory (`vendor/metals/...`),
  /// and entries resolve as if the archive were extracted at the root. It
  /// is an error for an archive to be non-conforming, to duplicate existing
  /// loose files in the same root, or to overlap the package prefix of
  /// archive in the same root. A single `.mdr` file may also be added
  /// directly, with its parent directory as the implicit root.
  ///
  /// MDLE containers (`.mdle`) are added only by passing the `.mdle`
  /// file explicitly. Directory walks never ingest them, mirroring the
  /// MDL convention that encapsulated materials are addressed individually.
  /// The container must hold `main.mdl`, which is loaded as a module whose
  /// qualified name is the content-based `::mdle::<md5>` of the container
  /// bytes (reported through `addedModuleNames`), so the canonical material
  /// is `::mdle::<md5>::main`. Identical containers at different paths dedup
  /// to one module; distinct containers can never collide. Every other entry
  /// is extracted to a content-addressed cache directory under the system
  /// temp directory, and the module's resource lookups anchor there, so
  /// self-contained textures resolve.
  ///
  [[nodiscard]] std::optional<Error>
  add(std::string fileOrDirName,
      std::vector<std::string> *addedModuleNames = nullptr) noexcept;

  /// Add MDL module from source code supplied by the host, so that a
  /// material may be added programmatically without going through the
  /// filesystem at all.
  ///
  /// \param[in] moduleName
  /// The qualified module name, e.g., `::vendor::metals::steel`. The
  /// leading `::` is optional and every `::`-delimited component must
  /// be an identifier (a letter or underscore followed by letters,
  /// digits, and underscores), because this is the name other modules
  /// spell in an `import`. It is an error for the name to be taken
  /// already, except that adding the same name with byte-identical
  /// source code again is a no-op. Note that a name that matches a
  /// builtin module is legal but unreachable by absolute import, which
  /// prefers the builtin, so it is warned about.
  ///
  /// \param[in] sourceCode
  /// The source code, which is copied into the module, so the caller
  /// need not keep it alive. This is the module verbatim, so the SMDL
  /// dialect requires the leading `#smdl` pragma exactly as a file
  /// does.
  ///
  /// \param[in] anchorDirectory
  /// If not empty, an existing directory the module resolves relative
  /// paths against, as if its source lived there: resources, relative
  /// `#search_dir` paths, and relative imports all anchor here. With no
  /// anchor, resources resolve only through `fileLocator` and relative
  /// imports resolve nothing.
  ///
  /// The module joins the added modules as an equal: it compiles with
  /// them, imports them by qualified name and is imported by them, and
  /// its materials are found by `findMaterial()` under
  /// `<moduleName>::<materialName>`. Because it has no file, it is
  /// skipped by `formatSourceFiles()` and reports its source locations
  /// as `<string ::vendor::metals::steel>` (see
  /// `Module::getDisplayName()`). Unlike a file, it is immune to the
  /// source changing underneath the compiler: `compile()` re-parses the
  /// string it was given here.
  ///
  [[nodiscard]] std::optional<Error>
  addCode(std::string moduleName, std::string sourceCode,
          std::string anchorDirectory = {}) noexcept;

  /// Set the desired material names, which restricts the next
  /// `compile()` to the named materials.
  ///
  /// By default every material in every added module is compiled. A host
  /// that knows exactly which materials it will look up can pass their
  /// names here before `compile()`: every material whose qualified name
  /// matches none of the names (by the matching rules of
  /// `findMaterial()`) is skipped, so its JIT entry points are never
  /// emitted, optimized, or JIT-compiled, and resources only it
  /// references (textures, spectra, ...) are never loaded. This can save
  /// substantial compile time and memory when the added modules define
  /// many more materials than a render uses.
  ///
  /// A skipped material still exists as an ordinary function, so other
  /// materials that instantiate it compile unaffected, and unit tests
  /// and execs are unaffected entirely. The skipped material itself is
  /// absent from `getMaterials()` and unreachable by `findMaterial()`,
  /// which logs the exclusion when asked for it (see
  /// `getSkippedMaterialNames()`). Desired names that match no material
  /// at all are warned about during `compile()`. Note that a skipped
  /// material's body is never emitted, so errors inside it may go
  /// undiagnosed. Passing an empty vector restores the default of
  /// compiling every material.
  void setDesiredMaterials(std::vector<std::string> materialNames) noexcept {
    mDesiredMaterialNames = std::move(materialNames);
  }

  /// Compile to LLVM-IR.
  [[nodiscard]] std::optional<Error>
  compile(OptLevel optLevel = OPT_LEVEL_O2) noexcept;

  /// Format source code.
  [[nodiscard]] std::optional<Error>
  formatSourceFiles(const FormatOptions &formatOptions) noexcept;

  /// Extract documentation for all added modules into `docs`. This
  /// parses modules as necessary, but does not require `compile()`.
  [[nodiscard]] std::optional<Error> extractDocs(DocDatabase &docs) noexcept;

private:
  /// Get the LLVM context, or throw an `Error` if there is none (i.e., if
  /// `compile()` has not run yet or `jitCompile()` already consumed it).
  [[nodiscard]] llvm::LLVMContext &getLLVMContext();

  /// Get the LLVM module, or throw an `Error` if there is none (i.e., if
  /// `compile()` has not run yet or `jitCompile()` already consumed it).
  [[nodiscard]] llvm::Module &getLLVMModule();

  /// Warn about the resource `fileName`, at most once per distinct file
  /// name per `compile()`.
  ///
  /// This is for diagnostics raised while emitting a `#load_*` intrinsic,
  /// which is not once per mention in the source: a material body is
  /// emitted three times (once each for the `evaluate`, `evaluateOpacity`
  /// and `thinWalledProbe` functions that `Type.cc` generates), so a
  /// `texture_2d("missing.png")` in a material would otherwise report the
  /// same warning three times over.
  ///
  /// The `load*()` functions below need no such thing: they memoize by file
  /// hash, so a resource that is found but fails to load already reports
  /// exactly once. A file that is never found has no hash to key on, which
  /// is how it slips past that memo.
  ///
  /// Deduplication is by file name alone, deliberately. The source location
  /// cannot help: all three reports carry the same one, the builtin
  /// `texture_2d` constructor in `api.smdl`, not the call site in the
  /// user's module.
  ///
  void logResourceWarningOnce(const SourceLocation &srcLoc,
                              const std::string &fileName,
                              std::string_view message);

  /// Load image.
  ///
  /// If `withMipLevels` is true, this reference asks for a mip chain
  /// (see `Image::requestMipLevels()`). The request is shared with every
  /// other reference to the same file, so an image is mipped if anything
  /// ever asks: passing false means only that this reference does not
  /// need the chain, never that the image will not have one.
  ///
  /// Whether to *read* the chain is a property of the referencing
  /// `texture_2d`, which bakes its own level count, exactly as it bakes
  /// its own gamma. Both readings share one decoded image.
  ///
  /// An image holds one chain, so every reference that asks for one
  /// must ask for the same `filter`; a reference asking for the other
  /// kind is an error naming the file and the earlier request.
  [[nodiscard]] const Image &
  loadImage(const std::string &fileName, const SourceLocation &srcLoc,
            bool withMipLevels = false,
            Image::MipFilter filter = Image::MIP_MEAN);

  /// Load ptex texture.
  [[nodiscard]] const Ptexture &loadPtexture(const std::string &fileName,
                                             const SourceLocation &srcLoc);

  /// Load BSDF measurement.
  [[nodiscard]] const BSDFMeasurement &
  loadBSDFMeasurement(const std::string &fileName,
                      const SourceLocation &srcLoc);

  /// Load light profile.
  [[nodiscard]] const LightProfile &
  loadLightProfile(const std::string &fileName, const SourceLocation &srcLoc);

  /// Load voxel grid.
  ///
  /// `gridName` selects a named grid within the file (see
  /// `VoxelGrid::loadFromFile()`). The cache is keyed by content hash
  /// *and* the grid name, so two references to different grids of the
  /// same file are two separate `VoxelGrid`s.
  [[nodiscard]] const VoxelGrid &loadVoxelGrid(const std::string &fileName,
                                               const std::string &gridName,
                                               const SourceLocation &srcLoc);

  /// Load spectrum from TXT file.
  [[nodiscard]] SpectrumView loadSpectrum(const std::string &fileName,
                                          const SourceLocation &srcLoc);

  /// Load spectrum from ENVI Spectral Library file.
  [[nodiscard]] SpectrumView loadSpectrum(const std::string &fileName,
                                          int curveIndex,
                                          const SourceLocation &srcLoc);

  /// Load spectrum from ENVI Spectral Library file.
  [[nodiscard]] SpectrumView loadSpectrum(const std::string &fileName,
                                          const std::string &curveName,
                                          const SourceLocation &srcLoc);

  /// Load ENVI Spectral Library file.
  [[nodiscard]] const SpectrumLibrary &
  loadSpectrumLibrary(const std::string &fileName,
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
  template <typename T> void jitLookup(JIT::Function<T> &func) {
    func.func = reinterpret_cast<typename JIT::Function<T>::function_pointer>(
        jitLookup(func.name));
    if (!func.func)
      throw Error(concat("cannot resolve JIT function ", Quoted(func.name)));
  }

public:
  /// Find the unique JIT-compiled material matching `materialName`.
  ///
  /// Every material has a qualified name formed from its module's
  /// qualified name (see `add()`), the enclosing `namespace` names if
  /// any, and the material name, e.g., `::vendor::metals::steel::brushed`.
  /// If `materialName` starts with `::`, it must match a qualified name
  /// exactly. Otherwise it is matched as a suffix on `::` component
  /// boundaries, so `"brushed"` and `"steel::brushed"` both match the
  /// example above, but `"shed"` does not.
  ///
  /// Materials in shadowed modules are never matched, mirroring the
  /// rule that a shadowed module is unreachable by qualified name. See
  /// `Module::isShadowed()`.
  ///
  /// \return
  /// The unique match, or `nullptr` if nothing matches. Also
  /// returns `nullptr` if more than one material matches, in which
  /// case an error is logged that lists every candidate. Use a longer
  /// suffix to disambiguate, or use `findMaterials()` to get all
  /// candidates.
  ///
  [[nodiscard]] const JIT::Material *
  findMaterial(std::string_view materialName) const noexcept;

  /// Find all JIT-compiled materials matching `materialName`, by the
  /// same matching rules as `findMaterial()`. This is useful for
  /// tooling, and for disambiguating the candidates when
  /// `findMaterial()` reports an ambiguity.
  [[nodiscard]] std::vector<const JIT::Material *>
  findMaterials(std::string_view materialName) const;

  /// Match `materialName` against a material's qualified name by the
  /// rules of `findMaterial()`: a name starting with `::` matches the
  /// qualified name exactly, anything else matches as a suffix on `::`
  /// component boundaries. This is the one predicate shared by
  /// `findMaterial()`, `findMaterials()`, and the desired-material
  /// filter of `setDesiredMaterials()`.
  [[nodiscard]] static bool
  matchesMaterialName(std::string_view materialName,
                      std::string_view qualifiedName) noexcept;

  /// Get the qualified names of the materials the last `compile()`
  /// skipped because they matched no desired material name. Empty
  /// unless `setDesiredMaterials()` is active.
  [[nodiscard]] Span<const std::string>
  getSkippedMaterialNames() const noexcept {
    return mSkippedMaterialNames;
  }

  /// Get all JIT-compiled materials, including materials in shadowed
  /// modules.
  [[nodiscard]] Span<const JIT::Material> getMaterials() const noexcept {
    return mMaterials;
  }

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
  [[nodiscard]] float3 convertColorToRGB(const State &state,
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

  /// Run JIT-compiled unit tests and print results to standard error,
  /// colorized according to `colorMode`. Stops at the first failure,
  /// which is what the returned `Error` describes.
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

  /// Enable the `scatterNormalSample`, `scatterNormalEvaluate`, and
  /// `geometryNormalEvaluate` entry points?
  ///
  /// The first two answer for the normal distribution behind a GLOSSY
  /// lobe, and the third reads the `geometry.normal` field itself; a
  /// host needs them only to solve a manifold constraint through a rough
  /// or normal-remapped interface or to do something else with a half
  /// vector. When false they are never emitted, so they cost no codegen,
  /// no optimizer time and no JIT compilation, and
  /// `JIT::Material::scatterNormalSample` stays null;
  /// `JIT::MaterialInstance` aborts with a message naming this flag if
  /// called anyway.
  ///
  /// This is read while `compile()` lowers each material, so set it
  /// beforehand. Changing it means recompiling.
  bool enableScatterNormal{false};

  /// Enable unit tests?
  bool enableUnitTests{false};

  /// Colorize the unit test results printed by `runUnitTests()`?
  ColorMode colorMode{COLOR_MODE_AUTO};

  /// The number of wavelengths per MDL `color`.
  uint32_t wavelengthBaseMax{16};

  /// The scene data.
  SceneData sceneData{};

private:
  /// The allocator.
  ///
  /// \note
  /// This is used during the active compiling phase to allocate
  /// AST nodes, type representations, declarations, etc. Once `jit_compile()`
  /// is called and everything is finalized as JIT-linked native code,
  /// intermediate representations are dropped and the allocator is
  /// reset.
  ///
  BumpPtrAllocator mAllocator{};

  /// The MD5 file hasher.
  ///
  /// \note
  /// The resource tables below are keyed on the stable `const MD5FileHash *`
  /// pointers this hasher hands out, and hold their resources through
  /// `std::unique_ptr` so that rehashing never moves them: JIT-compiled code
  /// bakes absolute pointers into resource internals, so resource addresses
  /// must be stable for the lifetime of the compile.
  ///
  MD5FileHasher mFileHasher{};

  /// The file names already reported by `logResourceWarningOnce()`. Not
  /// keyed on `MD5FileHash` like the resource tables below, because the
  /// usual reason to warn is that the file does not exist, and a file that
  /// does not exist has nothing to hash.
  std::set<std::string, std::less<>> mWarnedResourceFileNames;

  /// The images used by textures, keyed by content hash alone: one
  /// decoded image per file, however its references differ in gamma or
  /// in whether they read mip levels.
  std::unordered_map<const MD5FileHash *, std::unique_ptr<Image>> mImages;

  /// The first reference to request a mip chain of each image, which a
  /// later request for the other kind of chain is reported against.
  std::unordered_map<const Image *, SourceLocation> mImageMipRequesters;

  /// The ptex textures.
  std::unordered_map<const MD5FileHash *, std::unique_ptr<Ptexture>> mPtextures;

  /// The BSDF measurements.
  std::unordered_map<const MD5FileHash *, std::unique_ptr<BSDFMeasurement>>
      mBSDFMeasurements;

  /// The light profiles.
  std::unordered_map<const MD5FileHash *, std::unique_ptr<LightProfile>>
      mLightProfiles;

  /// The hasher for the voxel grid key.
  struct VoxelGridKeyHash final {
    [[nodiscard]] size_t operator()(
        const std::pair<const MD5FileHash *, std::string> &key) const noexcept {
      auto hash{std::hash<const MD5FileHash *>()(key.first)};
      hash ^= std::hash<std::string>()(key.second) + 0x9E3779B97F4A7C15ULL +
              (hash << 6) + (hash >> 2);
      return hash;
    }
  };

  /// The voxel grids used by 3D textures, keyed by content hash and the
  /// grid name of `loadVoxelGrid()`.
  std::unordered_map<std::pair<const MD5FileHash *, std::string>,
                     std::unique_ptr<VoxelGrid>, VoxelGridKeyHash>
      mVoxelGrids;

  /// The spectrums.
  std::unordered_map<const MD5FileHash *, std::unique_ptr<Spectrum>> mSpectrums;

  /// The spectrum libraries.
  std::unordered_map<const MD5FileHash *, std::unique_ptr<SpectrumLibrary>>
      mSpectrumLibraries;

  /// The MDL modules by canonical file name, used to skip files that
  /// were already added.
  std::map<std::string, Module *> mModuleFileNames;

  /// The MDL modules by qualified name, e.g., `::vendor::metals::steel`.
  /// On collisions across search roots, the module under the earliest
  /// added root wins and later modules are marked shadowed.
  std::map<std::string, Module *> mModulesByQualifiedName;

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

  /// Drop every image the optimized module provably never reads, so
  /// `compile()` can skip decoding it and release its texel
  /// reservation. Returns the number of images dropped.
  size_t dropUnusedImages();

  /// Take ownership of a successfully loaded module: index it by file
  /// name and by qualified name, mark it shadowed if the qualified name
  /// is taken (the earliest added module wins), and report its
  /// qualified name through `addedModuleNames` if non-null.
  ///
  /// This runs only after the load succeeds, so a file that failed can
  /// be retried instead of being silently skipped.
  void registerModule(std::unique_ptr<Module> loadedModule,
                      std::vector<std::string> *addedModuleNames);

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

  /// The desired material names (see `setDesiredMaterials()`). When
  /// non-empty, `compile()` skips every material whose qualified name
  /// matches none of these.
  std::vector<std::string> mDesiredMaterialNames;

  /// The qualified names of the materials skipped by the last
  /// `compile()` because they matched no desired material name.
  std::vector<std::string> mSkippedMaterialNames;

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
