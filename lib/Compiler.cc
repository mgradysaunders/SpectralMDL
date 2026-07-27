#include "smdl/Compiler.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Profiler.h"
#include "smdl/Support/QualifiedName.h"

#include <chrono>
#include <filesystem>
#include <iostream>

#include "llvm/ExecutionEngine/Orc/AbsoluteSymbols.h"
#include "llvm/ExecutionEngine/Orc/LLJIT.h"
#include "llvm/ExecutionEngine/Orc/Mangling.h"
#include "llvm/ExecutionEngine/Orc/ThreadSafeModule.h"
#include "llvm/Support/Parallel.h"

#include "Archive.h"
#include "Compiler/Context.h"

#if SMDL_HAS_PTEX
#include "Ptexture.h"
#endif // #if SMDL_HAS_PTEX

namespace smdl {

Compiler::Compiler(uint32_t wavelengthBaseMax)
    : wavelengthBaseMax(wavelengthBaseMax) {}

/// Sort JIT handle records by module filename, then line number.
template <typename T> static void sortByFileAndLine(std::vector<T> &elems) {
  std::sort(elems.begin(), elems.end(), [](const auto &lhs, const auto &rhs) {
    return std::pair(std::string_view(lhs.moduleFileName), lhs.lineNo) <
           std::pair(std::string_view(rhs.moduleFileName), rhs.lineNo);
  });
}

/// Visit `[itrFirst, itrLast)` runs of records that share a module
/// filename, assuming the records are sorted by `sortByFileAndLine`.
template <typename Iterator, typename Visitor>
static void forEachFileGroup(Iterator itr, Iterator itrEnd, Visitor &&visitor) {
  while (itr != itrEnd) {
    auto itrLast{itr};
    while (itrLast != itrEnd && itrLast->moduleFileName == itr->moduleFileName)
      ++itrLast;
    visitor(itr, itrLast);
    itr = itrLast;
  }
}

Compiler::~Compiler() = default;

void Ptexture::release() noexcept {
#if SMDL_HAS_PTEX
  if (textureFilter) {
    static_cast<PtexFilter *>(textureFilter)->release();
  }
  if (texture) {
    static_cast<PtexTexture *>(texture)->release();
  }
#endif // #if SMDL_HAS_PTEX
  texture = nullptr;
  textureFilter = nullptr;
  channelCount = 0;
  alphaIndex = -1;
}

/// Parse the dot-separated package prefix encoded by an MDL archive
/// file name per the MDL specification, e.g., `vendor.metals.mdr`
/// encodes `{"vendor", "metals"}`. Throws on empty components.
[[nodiscard]] static std::vector<std::string>
parseArchivePackagePrefix(const std::string &fileName) {
  auto stem{std::filesystem::path(fileName).stem().string()};
  auto prefix{std::vector<std::string>()};
  size_t pos{0};
  while (true) {
    auto dot{stem.find('.', pos)};
    auto component{stem.substr(pos, dot == std::string::npos ? std::string::npos
                                                             : dot - pos)};
    if (component.empty())
      throw Error(concat("invalid archive name ", QuotedPath(fileName),
                         ": empty package prefix component"));
    prefix.push_back(std::move(component));
    if (dot == std::string::npos) return prefix;
    pos = dot + 1;
  }
}

/// Does the archive entry conform to the package prefix encoded by the
/// archive file name? A conforming `.mdl` entry is either the enclosed
/// module itself (`vendor/metals.mdl` for the prefix
/// `{"vendor", "metals"}`) or anywhere under the enclosed package
/// directory (`vendor/metals/...`).
[[nodiscard]] static bool
isConformingArchiveEntry(const std::vector<std::string> &prefix,
                         const std::string &entryName) {
  auto components{std::vector<std::string>()};
  size_t pos{0};
  while (true) {
    auto sep{entryName.find('/', pos)};
    components.push_back(entryName.substr(
        pos, sep == std::string::npos ? std::string::npos : sep - pos));
    if (sep == std::string::npos) break;
    pos = sep + 1;
  }
  if (components.size() == prefix.size()) {
    for (size_t i{0}; i + 1 < prefix.size(); i++) {
      if (components[i] != prefix[i]) return false;
    }
    return components.back() == prefix.back() + ".mdl";
  }
  if (components.size() > prefix.size()) {
    for (size_t i{0}; i < prefix.size(); i++) {
      if (components[i] != prefix[i]) return false;
    }
    return true;
  }
  return false;
}

/// Is `parent` a lexical ancestor directory of `child`? Assumes both
/// paths are already canonical. Equal paths do not count.
[[nodiscard]] static bool isLexicalSubPath(const std::string &parent,
                                           const std::string &child) {
  auto parentPath{std::filesystem::path(parent)};
  auto childPath{std::filesystem::path(child)};
  auto [parentItr, childItr] =
      std::mismatch(parentPath.begin(), parentPath.end(), //
                    childPath.begin(), childPath.end());
  return parentItr == parentPath.end() && childItr != childPath.end();
}

std::optional<Error>
Compiler::add(std::string fileOrDirName,
              std::vector<std::string> *addedModuleNames) noexcept {
  SMDL_PROFILER_ENTRY("Compiler::add()", fileOrDirName.c_str());
  // The filesystem iterators, 'Archive', and 'Module::loadFromFile' all
  // throw; catch everything so the 'optional<Error>' contract holds.
  return catchAndReturnError([&] {
    // Register a successfully loaded module: index it by file name and
    // by qualified name, and report it through 'addedModuleNames'.
    // NOTE: This runs only after the load succeeds, so a failed file can
    // be retried instead of being silently skipped.
    auto registerModule{[&](std::unique_ptr<Module> loadedModule) {
      auto &module_{*mModules.emplace_back(std::move(loadedModule))};
      mModuleFileNames.emplace(std::string(module_.getFileName()), &module_);
      auto qualifiedName{std::string(module_.getQualifiedName())};
      if (auto [itr, inserted] =
              mModulesByQualifiedName.try_emplace(qualifiedName, &module_);
          !inserted) {
        // The earliest search root wins for qualified-name lookup. The
        // module still loads and compiles, and relative imports within
        // its own tree still resolve to it.
        module_.mIsShadowed = true;
        SMDL_LOG_WARN("module ", Quoted(qualifiedName), " in ",
                      QuotedPath(module_.getFileName()), " is shadowed by ",
                      QuotedPath(itr->second->getFileName()));
      }
      if (addedModuleNames) {
        addedModuleNames->push_back(std::move(qualifiedName));
      }
    }};
    auto addFile{[&](const std::string &fileName,
                     const std::string &searchRoot) {
      if (llvm::StringRef(fileName).ends_with_insensitive(".mdle")) {
        SMDL_LOG_DEBUG("Adding MDLE ", QuotedPath(fileName));
        // An MDLE is a self-contained encapsulated material. Identity
        // is content-based: the qualified name is '::mdle::<md5>' of
        // the container bytes, so identical containers at different
        // paths dedupe to one module and distinct containers can never
        // collide.
        auto qualifiedName{"::mdle::" +
                           std::string(MD5Hash::hashFile(fileName))};
        if (auto itr{mModulesByQualifiedName.find(qualifiedName)};
            itr != mModulesByQualifiedName.end()) {
          if (addedModuleNames) {
            addedModuleNames->push_back(std::move(qualifiedName));
          }
          return;
        }
        // Load 'main.mdl' and extract every other entry into a
        // content-addressed cache directory that serves as the anchor
        // for the module's resource lookups.
        auto extractDir{(std::filesystem::temp_directory_path() /
                         ("smdl-mdle-" + qualifiedName.substr(8)))
                            .string()};
        auto archive{Archive{fileName}};
        auto mainSource{std::optional<std::string>()};
        for (int i = 0; i < archive.get_file_count(); i++) {
          auto entryName{archive.get_file_name(i)};
          if (entryName == "main.mdl") {
            mainSource = archive.extract_file(i);
          } else if (!entryName.empty() && entryName.back() != '/') {
            auto outPath{std::filesystem::path(extractDir) / entryName};
            std::filesystem::create_directories(outPath.parent_path());
            openOrThrow(outPath.string(), std::ios::out | std::ios::binary)
                << archive.extract_file(i);
          }
        }
        if (!mainSource) {
          throw Error(concat("MDLE ", QuotedPath(fileName),
                             " does not contain 'main.mdl'"));
        }
        registerModule(Module::loadFromMDLE(fileName, *mainSource,
                                            qualifiedName, extractDir));
      } else if (llvm::StringRef(fileName).ends_with_insensitive(".mdr")) {
        SMDL_LOG_DEBUG("Adding MDL archive ", QuotedPath(fileName));
        // Per the MDL specification, the archive file name encodes the
        // enclosed package prefix: 'vendor.metals.mdr' provides
        // '::vendor::metals', and every '.mdl' entry must be the
        // enclosed module ('vendor/metals.mdl') or live under the
        // enclosed package directory ('vendor/metals/...').
        auto prefix{parseArchivePackagePrefix(fileName)};
        {
          // Duplicating the enclosed contents as loose files in the
          // same search root is an error.
          auto loosePath{searchRoot};
          for (const auto &component : prefix) {
            loosePath = joinPaths(loosePath, component);
          }
          if (isDirectory(loosePath) || isFile(loosePath + ".mdl") ||
              isFile(loosePath + ".smdl")) {
            throw Error(concat("archive ", QuotedPath(fileName),
                               " conflicts with loose contents at ",
                               QuotedPath(loosePath),
                               " in the same search root"));
          }
        }
        auto archive{Archive{fileName}};
        for (int i = 0; i < archive.get_file_count(); i++) {
          if (auto entryName{archive.get_file_name(i)};
              hasExtension(entryName, ".mdl")) {
            if (!isConformingArchiveEntry(prefix, entryName)) {
              throw Error(
                  concat("archive ", QuotedPath(fileName), " entry ",
                         Quoted(entryName),
                         " does not conform to the package prefix encoded "
                         "by the archive file name"));
            }
            if (auto entryPath{joinPaths(fileName, entryName)};
                mModuleFileNames.count(entryPath) == 0) {
              SMDL_LOG_DEBUG("Adding MDL file from archive ",
                             QuotedPath(entryPath));
              registerModule(Module::loadFromFileExtractedFromArchive(
                  fileName, entryName, archive.extract_file(i), searchRoot));
            }
          }
        }
      } else {
        if (auto itr{mModuleFileNames.find(fileName)};
            itr == mModuleFileNames.end()) {
          SMDL_LOG_DEBUG("Adding MDL file ", QuotedPath(fileName));
          registerModule(Module::loadFromFile(fileName, searchRoot));
        } else if (itr->second->getSearchRoot() != searchRoot) {
          // Already added under a different search root: the first
          // identity wins.
          SMDL_LOG_WARN("module file ", QuotedPath(fileName),
                        " was already added as ",
                        Quoted(itr->second->getQualifiedName()),
                        "; keeping the existing identity");
        }
      }
    }};
    if (auto maybePath{fileLocator.locate(fileOrDirName, {},
                                          FileLocator::REGULAR_FILES |
                                              FileLocator::DIRS)}) {
      auto &path{*maybePath};
      if (isFile(path)) {
        addFile(path, parentPathOf(path));
        return;
      } else if (isDirectory(path)) {
        if (mModuleDirNames.count(path) != 0) {
          // Re-adding the same search root is a no-op.
          return;
        }
        for (const auto &dir : mModuleDirSearchPaths) {
          if (isLexicalSubPath(dir, path) || isLexicalSubPath(path, dir)) {
            throw Error(
                concat("cannot add search root ", QuotedPath(path),
                       ": nested inside or encloses another search root ",
                       QuotedPath(dir),
                       " (would give modules ambiguous qualified names)"));
          }
        }
        // Collect the top-level archives, sorted so registration order
        // is deterministic, and reject overlapping package prefixes:
        // per the MDL specification, 'a.b.mdr' and 'a.b.c.mdr' must not
        // coexist in the same search root (siblings like 'a.c.mdr' are
        // fine).
        auto archivePaths{std::vector<std::string>()};
        for (const auto &entry : std::filesystem::directory_iterator(path)) {
          if (auto entryPath{makePathCanonical(entry.path().string())};
              isFile(entryPath) && hasExtension(entryPath, ".mdr")) {
            archivePaths.push_back(std::move(entryPath));
          }
        }
        std::sort(archivePaths.begin(), archivePaths.end());
        for (size_t i = 0; i < archivePaths.size(); i++) {
          auto prefixI{parseArchivePackagePrefix(archivePaths[i])};
          for (size_t j = i + 1; j < archivePaths.size(); j++) {
            auto prefixJ{parseArchivePackagePrefix(archivePaths[j])};
            if (auto n{std::min(prefixI.size(), prefixJ.size())}; std::equal(
                    prefixI.begin(), prefixI.begin() + n, prefixJ.begin())) {
              throw Error(concat(
                  "archives ", //
                  QuotedPath(archivePaths[i]), " and ",
                  QuotedPath(archivePaths[j]),
                  " have overlapping package prefixes in the same search "
                  "root"));
            }
          }
        }
        SMDL_LOG_DEBUG("Adding MDL directory ", QuotedPath(path));
        mModuleDirNames.insert(path);
        mModuleDirSearchPaths.emplace_back(path);
        for (const auto &archivePath : archivePaths) {
          addFile(archivePath, path);
        }
        for (const auto &entry :
             std::filesystem::recursive_directory_iterator(path)) {
          if (auto entryPath{makePathCanonical(entry.path().string())};
              isFile(entryPath)) {
            if (hasExtension(entryPath, ".mdl") ||
                hasExtension(entryPath, ".smdl")) {
              addFile(entryPath, path);
            } else if (hasExtension(entryPath, ".mdr") &&
                       !isPathEquivalent(parentPathOf(entryPath), path)) {
              // Per the MDL specification, archives are only recognized
              // at the top level of a search root.
              SMDL_LOG_WARN("ignoring archive ", QuotedPath(entryPath),
                            " because it is not at the top level of search "
                            "root ",
                            QuotedPath(path));
            }
          }
        }
        return;
      }
    }
    throw Error(concat("cannot locate ", Quoted(fileOrDirName)));
  });
}

void Compiler::resetForRecompile() {
  // Free the previous JIT first: this invalidates every function pointer
  // previously handed out, per the lifetime contract on the class.
  mLLVMJit.reset();
  mJITSessionErrors.clear();
  mImages.clear();
  mPtextures.clear();
  mBSDFMeasurements.clear();
  mLightProfiles.clear();
  mSpectrums.clear();
  mSpectrumLibraries.clear();
  mBuiltinCalleeAddresses.clear();
  mRGBToColor.func = nullptr;
  mColorToRGB.func = nullptr;
  mMaterials.clear();
  mUnitTests.clear();
  mExecs.clear();
  mLLVMContext = std::make_unique<llvm::LLVMContext>();
  mLLVMModule = std::make_unique<llvm::Module>("MDL", *mLLVMContext);
  mLLVMModule->setTargetTriple(
      llvm::Triple(llvm::StringRef(NativeTarget::get().triple)));
  mLLVMModule->setDataLayout(NativeTarget::get().machine->createDataLayout());
  // Be explicit that the JIT links against the host process's own symbols:
  // '@(foreign)' declarations and emitted libcalls (e.g. 'strncmp')
  // resolve via 'dlsym' on the current process.
  mLLVMJit = llvmThrowIfError(
      llvm::orc::LLJITBuilder().setLinkProcessSymbolsByDefault(true).create());
  mLLVMJit->getExecutionSession().setErrorReporter([this](llvm::Error error) {
    auto message{llvm::toString(std::move(error))};
    SMDL_LOG_ERROR("JIT session error: ", message);
    if (!mJITSessionErrors.empty()) mJITSessionErrors += '\n';
    mJITSessionErrors += message;
  });
}

std::optional<Error> Compiler::compile(OptLevel optLevel) noexcept {
  SMDL_PROFILER_ENTRY("Compiler::compile()");
  // The 'Context' constructor and the emit phase can throw; catch
  // everything so the 'optional<Error>' contract holds instead of exiting
  // or terminating the host process.
  return catchAndReturnError([&] {
    resetForRecompile();
    auto initializeEntry{profilerEntryBegin("Initialize")};
    Context context{*this};
    for (auto &module_ : mModules) module_->reset();
    profilerEntryEnd(initializeEntry);
    {
      SMDL_PROFILER_ENTRY("Parse AST");
      for (auto &module_ : mModules)
        if (auto error{module_->parse(mAllocator)}) throw std::move(*error);
    }
    {
      SMDL_PROFILER_ENTRY("Emit LLVM-IR");
      for (auto &module_ : mModules)
        if (auto error{module_->compile(context)}) throw std::move(*error);
    }
    // Sort JIT materials and unit tests by filename and line number in
    // case we want to print them later.
    sortByFileAndLine(mMaterials);
    sortByFileAndLine(mUnitTests);
    // Finish loading images.
    if (!mImages.empty()) {
      SMDL_PROFILER_ENTRY("Load images in parallel");
      SMDL_LOG_INFO("Loading images ...");
      auto now{std::chrono::steady_clock::now()};
      auto imageEntries{std::vector<std::pair<const MD5FileHash *, Image *>>()};
      imageEntries.reserve(mImages.size());
      for (auto &[fileHash, image] : mImages)
        imageEntries.emplace_back(fileHash, &image);
      llvm::parallelFor(0, imageEntries.size(), [&](size_t i) {
        auto fileHash{imageEntries[i].first};
        auto image{imageEntries[i].second};
        SMDL_PROFILER_ENTRY("Load image",
                            fileHash->canonicalFileNames[0].c_str());
        SMDL_LOG_DEBUG("Loading image ",
                       QuotedPath(fileHash->canonicalFileNames[0]), " ...");
        // A decode failure must not unwind into LLVM's thread pool (LLVM
        // is built '-fno-exceptions'); warn and continue with the image's
        // pre-allocated (zeroed) texels, matching the 'loadImage' policy.
        if (auto error{catchAndReturnError([&] {
              image->finishLoad();
              // NOTE: Images flipped vertically (at least for now) because
              // it makes the implementation of the tex evaluation functions
              // more straightforward
              image->flipVertically();
            })}) {
          SMDL_LOG_WARN("cannot load ",
                        QuotedPath(fileHash->canonicalFileNames[0]), ": ",
                        error->message);
        }
      });
      auto duration{std::chrono::duration_cast<std::chrono::microseconds>(
                        std::chrono::steady_clock::now() - now)
                        .count()};
      SMDL_LOG_INFO("Loading images done. [", std::to_string(duration * 1e-6),
                    " seconds]");
    }
    if (optLevel != OPT_LEVEL_NONE) {
      SMDL_PROFILER_ENTRY("Optimize LLVM-IR");
      LLVMOptimizer llvmOptimizer{};
      llvmOptimizer.run(*mLLVMModule, optLevel == OPT_LEVEL_O1
                                          ? llvm::OptimizationLevel::O1
                                      : optLevel == OPT_LEVEL_O2
                                          ? llvm::OptimizationLevel::O2
                                          : llvm::OptimizationLevel::O3);
    }
  });
}

std::optional<Error>
Compiler::formatSourceCode(const FormatOptions &formatOptions) noexcept {
  SMDL_PROFILER_ENTRY("Compiler::formatSourceCode()");
  for (auto &module_ : mModules) {
    if (!module_->isBuiltin()) {
      if (auto error{module_->formatSourceCode(formatOptions)}) return error;
    }
  }
  return std::nullopt;
}

llvm::LLVMContext &Compiler::getLLVMContext() {
  if (!mLLVMContext)
    throw Error("no LLVM context: 'compile()' must be called first (and "
                "'jitCompile()' consumes it)");
  return *mLLVMContext;
}

llvm::Module &Compiler::getLLVMModule() {
  if (!mLLVMModule)
    throw Error("no LLVM module: 'compile()' must be called first (and "
                "'jitCompile()' consumes it)");
  return *mLLVMModule;
}

/// Look up the resource for the given file in `resources`, running `loader`
/// exactly once per distinct file. A load failure is a warning, not an
/// error: the resource stays default-constructed and rendering continues.
template <typename T, typename Loader>
static T &loadResource(std::map<const MD5FileHash *, T> &resources,
                       MD5FileHasher &fileHasher, const std::string &fileName,
                       const SourceLocation &srcLoc, Loader &&loader) {
  auto [itr, inserted] = resources.try_emplace(fileHasher[fileName]);
  auto &resource{itr->second};
  if (inserted) {
    if (auto error{std::invoke(std::forward<Loader>(loader), resource)}) {
      srcLoc.logWarn(error->message);
    }
  }
  return resource;
}

const Image &Compiler::loadImage(const std::string &fileName,
                                 const SourceLocation &srcLoc) {
  return loadResource(
      mImages, mFileHasher, fileName, srcLoc, [&](Image &image) {
        SMDL_PROFILER_ENTRY("Compiler::loadImage()", fileName.c_str());
        return image.startLoad(fileName);
      });
}

const Ptexture &Compiler::loadPtexture(const std::string &fileName,
                                       const SourceLocation &srcLoc) {
  return loadResource(
      mPtextures, mFileHasher, fileName, srcLoc,
      [&](Ptexture &ptexture) -> std::optional<Error> {
#if SMDL_HAS_PTEX
        SMDL_PROFILER_ENTRY("Compiler::loadPtexture()", fileName.c_str());
        Ptex::String message{};
        auto texture{PtexTexture::open(fileName.c_str(), message,
                                       /*premultiply=*/false)};
        if (!texture)
          return Error(concat("cannot load ", QuotedPath(fileName), ": ",
                              message.c_str()));
        ptexture.texture = texture;
        // NOTE: No shared 'PtexFilter': 'PtexFilter::eval' mutates filter
        // members, so 'smdlPtexEvaluate' maintains per-thread filters.
        ptexture.channelCount = texture->numChannels();
        ptexture.alphaIndex = texture->alphaChannel();
        return std::nullopt;
#else
        return Error(concat("cannot load ", QuotedPath(fileName),
                            ": built without ptex!"));
#endif // #if SMDL_HAS_PTEX
      });
}

const BSDFMeasurement &
Compiler::loadBSDFMeasurement(const std::string &fileName,
                              const SourceLocation &srcLoc) {
  return loadResource(mBSDFMeasurements, mFileHasher, fileName, srcLoc,
                      [&](BSDFMeasurement &bsdfMeasurement) {
                        SMDL_PROFILER_ENTRY("Compiler::loadBSDFMeasurement()",
                                            fileName.c_str());
                        return bsdfMeasurement.loadFromFile(fileName);
                      });
}

const LightProfile &Compiler::loadLightProfile(const std::string &fileName,
                                               const SourceLocation &srcLoc) {
  return loadResource(mLightProfiles, mFileHasher, fileName, srcLoc,
                      [&](LightProfile &lightProfile) {
                        SMDL_PROFILER_ENTRY("Compiler::loadLightProfile()",
                                            fileName.c_str());
                        return lightProfile.loadFromFile(fileName);
                      });
}

SpectrumView Compiler::loadSpectrum(const std::string &fileName,
                                    const SourceLocation &srcLoc) {
  return SpectrumView(loadResource(
      mSpectrums, mFileHasher, fileName, srcLoc, [&](Spectrum &spectrum) {
        SMDL_PROFILER_ENTRY("Compiler::loadSpectrum()", fileName.c_str());
        return spectrum.loadFromFile(fileName);
      }));
}

SpectrumView Compiler::loadSpectrum(const std::string &fileName, int curveIndex,
                                    const SourceLocation &srcLoc) {
  return loadSpectrumLibrary(fileName, srcLoc).getCurveByIndex(curveIndex);
}

SpectrumView Compiler::loadSpectrum(const std::string &fileName,
                                    const std::string &curveName,
                                    const SourceLocation &srcLoc) {
  return loadSpectrumLibrary(fileName, srcLoc).getCurveByName(curveName);
}

const SpectrumLibrary &
Compiler::loadSpectrumLibrary(const std::string &fileName,
                              const SourceLocation &srcLoc) {
  return loadResource(mSpectrumLibraries, mFileHasher, fileName, srcLoc,
                      [&](SpectrumLibrary &spectrumLibrary) {
                        SMDL_PROFILER_ENTRY("Compiler::loadSpectrum()",
                                            fileName.c_str());
                        return spectrumLibrary.loadFromFile(fileName);
                      });
}

std::optional<Error> Compiler::dump(DumpFormat dumpFormat,
                                    std::string &out) noexcept {
  return catchAndReturnError([&] {
    if (dumpFormat == DUMP_FORMAT_IR) {
      llvm::raw_string_ostream os{out};
      os << getLLVMModule();
    } else {
      llvm::SmallVector<char> str{};
      llvm::raw_svector_ostream os{str};
      llvm::legacy::PassManager passManager{};
      if (NativeTarget::get().machine->addPassesToEmitFile(
              passManager, os, nullptr,
              dumpFormat == DUMP_FORMAT_ASM
                  ? llvm::CodeGenFileType::AssemblyFile
                  : llvm::CodeGenFileType::ObjectFile))
        throw Error("cannot emit assembly or object code for the native "
                    "target");
      // The codegen passes mutate the IR, so run them on a clone to keep
      // the module later handed to the JIT pristine.
      auto clonedModule{llvm::CloneModule(getLLVMModule())};
      passManager.run(*clonedModule);
      out = std::string(os.str());
    }
  });
}

std::optional<Error> Compiler::jitCompile() noexcept {
  SMDL_PROFILER_ENTRY("Compiler::jit_compile()");
  auto error{catchAndReturnError([&] {
    if (!mLLVMJit || !mLLVMModule || !mLLVMContext)
      throw Error("nothing to JIT-compile: 'compile()' must be called first");
    // Define the builtin runtime callees ('smdlPanic', 'smdlBumpAllocate',
    // ...) as absolute symbols so they resolve even when the host process
    // does not export its own symbols (e.g. static link without
    // '--export-dynamic').
    if (!mBuiltinCalleeAddresses.empty()) {
      auto mangle{llvm::orc::MangleAndInterner(mLLVMJit->getExecutionSession(),
                                               mLLVMJit->getDataLayout())};
      auto symbolMap{llvm::orc::SymbolMap{}};
      for (const auto &[calleeName, calleeAddr] : mBuiltinCalleeAddresses)
        symbolMap[mangle(calleeName)] = llvm::orc::ExecutorSymbolDef(
            llvm::orc::ExecutorAddr::fromPtr(calleeAddr),
            llvm::JITSymbolFlags::Exported | llvm::JITSymbolFlags::Callable);
      llvmThrowIfError(mLLVMJit->getMainJITDylib().define(
          llvm::orc::absoluteSymbols(std::move(symbolMap))));
    }
    // Hand the module to the JIT, dropping our handles up front — a failed
    // call must not leave moved-from state behind for 'dump()' or
    // 'getLLVMModule()' to trip over.
    auto llvmJitModule{llvm::orc::ThreadSafeModule(std::move(mLLVMModule),
                                                   std::move(mLLVMContext))};
    llvmThrowIfError(mLLVMJit->addIRModule(std::move(llvmJitModule)));
    jitLookupOrThrow(mColorToRGB);
    jitLookupOrThrow(mRGBToColor);
    for (auto &jitMaterial : mMaterials) {
      jitLookupOrThrow(jitMaterial.evaluate);
      jitLookupOrThrow(jitMaterial.scatterEvaluate);
      jitLookupOrThrow(jitMaterial.scatterSample);
      jitLookupOrThrow(jitMaterial.emissionEvaluate);
      jitLookupOrThrow(jitMaterial.emissionSample);
    }
    for (auto &jitUnitTest : mUnitTests) {
      jitLookupOrThrow(jitUnitTest.test);
    }
    for (auto &jitExec : mExecs) {
      jitLookupOrThrow(jitExec);
    }
    // Deallocate everything we no longer need!
    for (auto &mod : mModules) {
      mod->reset();
    }
    mAllocator.reset();
  })};
  if (error && !mJITSessionErrors.empty()) {
    error->message += "\nJIT session errors:\n";
    error->message += mJITSessionErrors;
  }
  return error;
}

void *Compiler::jitLookup(std::string_view name) {
  llvm::Expected<llvm::orc::ExecutorAddr> symbol{mLLVMJit->lookup(name)};
  if (!symbol)
    throw Error(concat("cannot resolve JIT symbol ", Quoted(name), ": ",
                       llvm::toString(symbol.takeError())));
  return symbol->toPtr<void *>();
}

const JIT::Material *
Compiler::findMaterial(std::string_view materialName) const noexcept try {
  auto results{findMaterials(materialName)};
  if (results.empty()) return nullptr;
  if (results.size() > 1) {
    auto message{concat("Material ", Quoted(materialName),
                        " is ambiguous with ", results.size(), " matches:")};
    for (const auto *jitMaterial : results) {
      message += "\n  ";
      message +=
          concat(jitMaterial->qualifiedName, " (",
                 jitMaterial->moduleFileName, ":", jitMaterial->lineNo, ")");
    }
    SMDL_LOG_ERROR(message);
    return nullptr;
  }
  return results.front();
} catch (...) {
  return nullptr;
}

std::vector<const JIT::Material *>
Compiler::findMaterials(std::string_view materialName) const {
  auto results{std::vector<const JIT::Material *>()};
  const bool isAbsolute{materialName.substr(0, 2) == "::"};
  for (const auto &jitMaterial : mMaterials) {
    if (jitMaterial.moduleIsShadowed) {
      continue;
    }
    if (isAbsolute
            ? jitMaterial.qualifiedName == materialName
            : isQualifiedNameSuffix(materialName, jitMaterial.qualifiedName)) {
      results.push_back(&jitMaterial);
    }
  }
  return results;
}

float3 Compiler::convertColorToRGB(const State &state,
                                   const float *color) const noexcept {
  SMDL_SANITY_CHECK(mColorToRGB && color);
  SMDL_SANITY_CHECK(state.wavelength_base != nullptr);
  float3 rgb{};
  mColorToRGB(state, color, rgb);
  return rgb;
}

void Compiler::convertRGBToColor(const State &state, const float3 &rgb,
                                 float *color) const noexcept {
  SMDL_SANITY_CHECK(mRGBToColor && color);
  SMDL_SANITY_CHECK(state.wavelength_base != nullptr);
  mRGBToColor(state, rgb, color);
}

std::optional<Error> Compiler::runUnitTests(const State &state) noexcept {
  return catchAndReturnError([&] {
    forEachFileGroup(
        mUnitTests.begin(), mUnitTests.end(), [&](auto itr0, auto itr1) {
          std::cerr << concat("Running tests in ",
                              QuotedPath(itr0->moduleFileName), ":\n");
          for (; itr0 != itr1; ++itr0) {
            std::cerr << concat("  ", Quoted(itr0->testName), " (line ",
                                itr0->lineNo, ") ... ");
            try {
              if (!itr0->test)
                throw Error(concat("unit test ", Quoted(itr0->testName),
                                   " has no JIT-compiled function"));
              itr0->test(state);
              std::cerr << "success\n";
            } catch (const Error &error) {
              std::cerr << "failure\n";
              throw;
            }
          }
          std::cerr << '\n';
        });
  });
}

std::optional<Error> Compiler::runExecs() noexcept {
  return catchAndReturnError([&] {
    for (auto &jitExec : mExecs) {
      if (!jitExec.func)
        throw Error(concat("exec ", Quoted(jitExec.name),
                           " has no JIT-compiled function: 'jitCompile()' "
                           "must be called first"));
      jitExec();
    }
  });
}

std::string Compiler::printMaterialSummary() const {
  std::string message{};
  forEachFileGroup(
      mMaterials.begin(), mMaterials.end(), [&](auto itr0, auto itr1) {
        message += concat(QuotedPath(itr0->moduleFileName), " contains ",
                          itr1 - itr0, " materials:\n");
        for (; itr0 != itr1; ++itr0) {
          message += "  ";
          message += concat(Quoted(itr0->materialName), " (line ", itr0->lineNo,
                            ")\n");
        }
      });
  return message;
}

} // namespace smdl

#if SMDL_HAS_PTEX
namespace {

/// Per-thread Ptex filters: 'PtexFilter::eval' mutates filter members, so
/// render threads must not share one filter instance. Each filter holds a
/// reference on its 'PtexTexture', so a cached filter stays memory-safe
/// even after the compiler releases the texture on recompile.
class ThreadLocalPtexFilters final {
public:
  ~ThreadLocalPtexFilters() {
    for (auto &[texture, filter] : mFilters) filter->release();
  }

  [[nodiscard]] PtexFilter *get(const smdl::Ptexture &ptex) {
    auto &filter{mFilters[ptex.texture]};
    if (!filter)
      filter =
          PtexFilter::getFilter(static_cast<PtexTexture *>(ptex.texture),
                                PtexFilter::Options(PtexFilter::f_bilinear));
    return filter;
  }

private:
  std::map<const void *, PtexFilter *> mFilters{};
};

} // namespace
#endif // #if SMDL_HAS_PTEX

extern "C" {

SMDL_EXPORT void smdlPtexEvaluate(const void *state,
                                  const ::smdl::Ptexture *ptex, int gamma,
                                  int first, int num, float *out) {
  SMDL_SANITY_CHECK(state != nullptr);
  SMDL_SANITY_CHECK(out != nullptr);
  for (int i = 0; i < num; i++) {
    out[i] = 0.0f;
  }
#if SMDL_HAS_PTEX
  if (ptex && ptex->texture && first < ptex->channelCount) {
    num = std::min(num, int(ptex->channelCount - first));
    thread_local ThreadLocalPtexFilters filters{};
    filters.get(*ptex)->eval(
        out, first, num, static_cast<const smdl::State *>(state)->ptex_face_id,
        static_cast<const smdl::State *>(state)->ptex_face_uv.x,
        static_cast<const smdl::State *>(state)->ptex_face_uv.y,
        /*uw1=*/0.0f, /*vw1=*/0.0f,
        /*uw2=*/0.0f, /*vw2=*/0.0f,
        /*width=*/1.0f, /*blur=*/0.0f);
    if (gamma == 1) { // sRGB?
      for (int i = 0; i < num; i++) {
        int channel{first + i};
        if (channel != ptex->alphaIndex) {
          // NOTE: This is the crudest approximation possible to a true sRGB
          // decoding, worth replacing with the proper equation at some point
          out[i] *= out[i];
        }
      }
    }
  }
#endif // #if SMDL_HAS_PTEX
}

} // extern "C"
