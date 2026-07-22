#include "smdl/Compiler.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Profiler.h"

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
  std::sort(elems.begin(), elems.end(),
            [](const auto &lhs, const auto &rhs) {
              return std::pair(std::string_view(lhs.moduleFileName),
                               lhs.lineNo) <
                     std::pair(std::string_view(rhs.moduleFileName),
                               rhs.lineNo);
            });
}

/// Visit `[itrFirst, itrLast)` runs of records that share a module
/// filename, assuming the records are sorted by `sortByFileAndLine`.
template <typename Iterator, typename Visitor>
static void forEachFileGroup(Iterator itr, Iterator itrEnd,
                             Visitor &&visitor) {
  while (itr != itrEnd) {
    auto itrLast{itr};
    while (itrLast != itrEnd &&
           itrLast->moduleFileName == itr->moduleFileName)
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

std::optional<Error> Compiler::add(std::string fileOrDirName) noexcept {
  SMDL_PROFILER_ENTRY("Compiler::add()", fileOrDirName.c_str());
  // The filesystem iterators, 'Archive', and 'Module::loadFromFile' all
  // throw; catch everything so the 'optional<Error>' contract holds.
  return catchAndReturnError([&] {
    auto addFile{[&](std::string fileName) {
      if (llvm::StringRef(fileName).ends_with_insensitive(".mdr")) {
        SMDL_LOG_DEBUG("Adding MDL archive ", QuotedPath(fileName));
        auto archive{Archive{fileName}};
        for (int i = 0; i < archive.get_file_count(); i++) {
          if (auto entryPath{joinPaths(fileName, archive.get_file_name(i))};
              hasExtension(entryPath, ".mdl")) {
            if (mModuleFileNames.count(entryPath) == 0) {
              SMDL_LOG_DEBUG("Adding MDL file from archive ",
                             QuotedPath(entryPath));
              mModules.emplace_back(Module::loadFromFileExtractedFromArchive(
                  entryPath, archive.extract_file(i)));
              // Only register the name once the load succeeds, so a failed
              // file can be retried instead of being silently skipped.
              mModuleFileNames.insert(entryPath);
            }
          }
        }
      } else {
        if (mModuleFileNames.count(fileName) == 0) {
          SMDL_LOG_DEBUG("Adding MDL file ", QuotedPath(fileName));
          mModules.emplace_back(Module::loadFromFile(fileName));
          mModuleFileNames.insert(fileName);
        }
      }
    }};
    if (auto maybePath{fileLocator.locate(
            fileOrDirName, {},
            FileLocator::REGULAR_FILES | FileLocator::DIRS)}) {
      auto &path{*maybePath};
      if (isFile(path)) {
        addFile(path);
        return;
      } else if (isDirectory(path) && mModuleDirNames.insert(path).second) {
        SMDL_LOG_DEBUG("Adding MDL directory ", QuotedPath(path));
        mModuleDirSearchPaths.emplace_back(path);
        for (const auto &entry : std::filesystem::directory_iterator(path)) {
          if (auto entryPath{makePathCanonical(entry.path().string())};
              isFile(entryPath) && hasExtension(entryPath, ".mdr")) {
            addFile(entryPath);
          }
        }
        for (const auto &entry :
             std::filesystem::recursive_directory_iterator(path)) {
          if (auto entryPath{makePathCanonical(entry.path().string())};
              isFile(entryPath) && (hasExtension(entryPath, ".mdl") ||
                                    hasExtension(entryPath, ".smdl"))) {
            addFile(entryPath);
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
  mLLVMJit = llvmThrowIfError(llvm::orc::LLJITBuilder()
                                  .setLinkProcessSymbolsByDefault(true)
                                  .create());
  mLLVMJit->getExecutionSession().setErrorReporter([this](llvm::Error error) {
    auto message{llvm::toString(std::move(error))};
    SMDL_LOG_ERROR("JIT session error: ", message);
    if (!mJITSessionErrors.empty())
      mJITSessionErrors += '\n';
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
    for (auto &module_ : mModules)
      module_->reset();
    profilerEntryEnd(initializeEntry);
    {
      SMDL_PROFILER_ENTRY("Parse AST");
      for (auto &module_ : mModules)
        if (auto error{module_->parse(mAllocator)})
          throw std::move(*error);
    }
    {
      SMDL_PROFILER_ENTRY("Emit LLVM-IR");
      for (auto &module_ : mModules)
        if (auto error{module_->compile(context)})
          throw std::move(*error);
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
      auto imageEntries{
          std::vector<std::pair<const MD5FileHash *, Image *>>()};
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
      llvmOptimizer.run(
          *mLLVMModule, optLevel == OPT_LEVEL_O1 ? llvm::OptimizationLevel::O1
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
      if (auto error{module_->formatSourceCode(formatOptions)})
        return error;
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
  return loadResource(mImages, mFileHasher, fileName, srcLoc,
                      [&](Image &image) {
                        SMDL_PROFILER_ENTRY("Compiler::loadImage()",
                                            fileName.c_str());
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
        // NOTE: No shared 'PtexFilter' — 'PtexFilter::eval' mutates filter
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
  return SpectrumView(loadResource(mSpectrums, mFileHasher, fileName, srcLoc,
                                   [&](Spectrum &spectrum) {
                                     SMDL_PROFILER_ENTRY(
                                         "Compiler::loadSpectrum()",
                                         fileName.c_str());
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
    }
    for (auto &jitUnitTest : mUnitTests) {
      jitLookupOrThrow(jitUnitTest.test);
    }
    for (auto &jitExec : mExecs) {
      jitLookupOrThrow(jitExec);
    }
    // Deallocate everything we no longer need!
    for (auto &module_ : mModules) {
      module_->reset();
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
  auto results{llvm::SmallVector<const JIT::Material *>()};
  for (const auto &jitMaterial : mMaterials) {
    if (jitMaterial.materialName == materialName) {
      results.push_back(&jitMaterial);
    }
  }
  if (results.empty()) {
    return nullptr;
  }
  if (results.size() > 1) {
    auto message{concat("Material ", Quoted(materialName),
                        " requested by name is ambiguous with ", results.size(),
                        " definitions:\n")};
    for (size_t i = 0; i < results.size(); i++) {
      message += "  ";
      message += results[i]->moduleFileName, message += ':';
      message += std::to_string(results[i]->lineNo), message += '\n';
    }
    SMDL_LOG_WARN(message);
  }
  return results.front();
} catch (...) {
  return nullptr;
}

const JIT::Material *
Compiler::findMaterial(std::string_view moduleName,
                       std::string_view materialName) const noexcept {
  for (const auto &jitMaterial : mMaterials) {
    if (jitMaterial.moduleName == moduleName &&
        jitMaterial.materialName == materialName) {
      return &jitMaterial;
    }
  }
  return nullptr;
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
          message += concat(Quoted(itr0->materialName), " (line ",
                            itr0->lineNo, ")\n");
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
    for (auto &[texture, filter] : mFilters)
      filter->release();
  }

  [[nodiscard]] PtexFilter *get(const smdl::Ptexture &ptex) {
    auto &filter{mFilters[ptex.texture]};
    if (!filter)
      filter = PtexFilter::getFilter(
          static_cast<PtexTexture *>(ptex.texture),
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
    filters.get(*ptex)
        ->eval(out, first, num,
               static_cast<const smdl::State *>(state)->ptex_face_id,
               static_cast<const smdl::State *>(state)->ptex_face_uv.x,
               static_cast<const smdl::State *>(state)->ptex_face_uv.y,
               /*uw1=*/0.0f, /*vw1=*/0.0f,
               /*uw2=*/0.0f, /*vw2=*/0.0f,
               /*width=*/1.0f, /*blur=*/0.0f);
    if (gamma == 1) { // sRGB?
      for (int i = 0; i < num; i++) {
        int channel{first + i};
        if (channel != ptex->alphaIndex) {
          out[i] *= out[i];
        }
      }
    }
  }
#endif // #if SMDL_HAS_PTEX
}

} // extern "C"
