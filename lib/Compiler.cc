#include "smdl/Compiler.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Profiler.h"

#include <chrono>
#include <filesystem>
#include <iostream>

#include "llvm/ExecutionEngine/Orc/LLJIT.h"
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

Compiler::~Compiler() {
#if SMDL_HAS_PTEX
  for (auto &[fileHash, ptexture] : ptextures) {
    if (ptexture.textureFilter) {
      static_cast<PtexFilter *>(ptexture.textureFilter)->release();
      ptexture.textureFilter = nullptr;
    }
    if (ptexture.texture) {
      static_cast<PtexTexture *>(ptexture.texture)->release();
      ptexture.texture = nullptr;
    }
  }
#endif // #if SMDL_HAS_PTEX
}

std::optional<Error> Compiler::add(std::string fileOrDirName) {
  SMDL_PROFILER_ENTRY("Compiler::add()", fileOrDirName.c_str());
  auto addFile{[&](std::string fileName) {
    if (llvm::StringRef(fileName).ends_with_insensitive(".mdr")) {
      SMDL_LOG_DEBUG("Adding MDL archive ", QuotedPath(fileName));
      auto archive{Archive{fileName}};
      for (int i = 0; i < archive.get_file_count(); i++) {
        if (auto entryPath{joinPaths(fileName, archive.get_file_name(i))};
            hasExtension(entryPath, ".mdl")) {
          if (moduleFileNames.insert(entryPath).second) {
            SMDL_LOG_DEBUG("Adding MDL file from archive ",
                           QuotedPath(entryPath));
            modules.emplace_back(Module::loadFromFileExtractedFromArchive(
                entryPath, archive.extract_file(i)));
          }
        }
      }
    } else {
      if (moduleFileNames.insert(fileName).second) {
        SMDL_LOG_DEBUG("Adding MDL file ", QuotedPath(fileName));
        modules.emplace_back(Module::loadFromFile(fileName));
      }
    }
  }};
  if (auto maybePath{fileLocator.locate(
          fileOrDirName, {}, FileLocator::REGULAR_FILES | FileLocator::DIRS)}) {
    auto &path{*maybePath};
    if (isFile(path)) {
      addFile(path);
      return std::nullopt;
    } else if (isDirectory(path) && moduleDirNames.insert(path).second) {
      SMDL_LOG_DEBUG("Adding MDL directory ", QuotedPath(path));
      moduleDirSearchPaths.emplace_back(path);
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
      return std::nullopt;
    }
  }
  return Error(concat("cannot locate ", Quoted(fileOrDirName)));
}

std::optional<Error> Compiler::compile(OptLevel optLevel) {
  SMDL_PROFILER_ENTRY("Compiler::compile()");
  {
    images.clear();
    ptextures.clear();
    bsdfMeasurements.clear();
    lightProfiles.clear();
    llvmJit.reset();
    llvm::ExitOnError exitOnError;
    auto llvmContext{std::make_unique<llvm::LLVMContext>()};
    auto llvmModule{std::make_unique<llvm::Module>("MDL", *llvmContext)};
    llvmModule->setTargetTriple(
        llvm::Triple(llvm::StringRef(NativeTarget::get().triple)));
    llvmModule->setDataLayout(NativeTarget::get().machine->createDataLayout());
    llvmJitModule = std::make_unique<llvm::orc::ThreadSafeModule>(
        std::move(llvmModule), std::move(llvmContext));
    llvmJit = exitOnError(llvm::orc::LLJITBuilder().create());
    jitRgbToColor.func = nullptr;
    jitColorToRgb.func = nullptr;
    jitMaterials.clear();
    jitUnitTests.clear();
  }
  auto initializeEntry{profilerEntryBegin("Initialize")};
  Context context{*this};
  for (auto &module_ : modules)
    module_->reset();
  profilerEntryEnd(initializeEntry);
  {
    SMDL_PROFILER_ENTRY("Parse AST");
    for (auto &module_ : modules)
      if (auto error{module_->parse(allocator)})
        return error;
  }
  {
    SMDL_PROFILER_ENTRY("Emit LLVM-IR");
    for (auto &module_ : modules)
      if (auto error{module_->compile(context)})
        return error;
  }
  // Sort JIT materials by filename and line number in case we
  // want to print them later
  std::sort(
      jitMaterials.begin(), jitMaterials.end(),
      [](const auto &lhs, const auto &rhs) {
        return std::pair(std::string_view(lhs.moduleFileName), lhs.lineNo) <
               std::pair(std::string_view(rhs.moduleFileName), rhs.lineNo);
      });
  // Sort JIT unit tests by filename and line number for similar reasons
  std::sort(
      jitUnitTests.begin(), jitUnitTests.end(),
      [](const auto &lhs, const auto &rhs) {
        return std::pair(std::string_view(lhs.moduleFileName), lhs.lineNo) <
               std::pair(std::string_view(rhs.moduleFileName), rhs.lineNo);
      });
  // Finish loading images.
  if (!images.empty()) {
    SMDL_PROFILER_ENTRY("Load images in parallel");
    SMDL_LOG_INFO("Loading images ...");
    auto now{std::chrono::steady_clock::now()};
    llvm::parallelFor(0, images.size(), [&](size_t i) {
      auto &[fileHash, image] = *std::next(images.begin(), ptrdiff_t(i));
      SMDL_PROFILER_ENTRY("Load image",
                          fileHash->canonicalFileNames[0].c_str());
      SMDL_LOG_DEBUG("Loading image ",
                     QuotedPath(fileHash->canonicalFileNames[0]), " ...");
      image.finishLoad();
      // NOTE: Images flipped vertically (at least for now) because it
      // makes the implementation of the tex evaluation functions more
      // straightforward
      image.flipVertically();
    });
    auto duration{std::chrono::duration_cast<std::chrono::microseconds>(
                      std::chrono::steady_clock::now() - now)
                      .count()};
    SMDL_LOG_INFO("Loading images done. [", std::to_string(duration * 1e-6),
                  " seconds]");
  }
  if (optLevel != OPT_LEVEL_NONE) {
    SMDL_PROFILER_ENTRY("Optimize LLVM-IR");
    llvmJitModule->withModuleDo([&](llvm::Module &llvmModule) {
      LLVMOptimizer llvmOptimizer{};
      llvmOptimizer.run(
          llvmModule, optLevel == OPT_LEVEL_O1   ? llvm::OptimizationLevel::O1
                      : optLevel == OPT_LEVEL_O2 ? llvm::OptimizationLevel::O2
                                                 : llvm::OptimizationLevel::O3);
    });
  }
  return std::nullopt;
}

std::optional<Error>
Compiler::formatSourceCode(const FormatOptions &formatOptions) noexcept {
  SMDL_PROFILER_ENTRY("Compiler::formatSourceCode()");
  for (auto &module_ : modules) {
    if (!module_->isBuiltin()) {
      if (auto error{module_->formatSourceCode(formatOptions)})
        return error;
    }
  }
  return std::nullopt;
}

llvm::LLVMContext &Compiler::getLLVMContext() noexcept {
  SMDL_SANITY_CHECK(llvmJitModule.get() != nullptr);
  // TODO Unsafe?
  llvm::LLVMContext *context{};
  llvmJitModule.get()->getContext().withContextDo(
      [&](auto *C) { context = C; });
  return *context;
}

llvm::Module &Compiler::getLLVMModule() noexcept {
  SMDL_SANITY_CHECK(llvmJitModule.get() != nullptr);
  return *llvmJitModule.get()->getModuleUnlocked();
}

const Image &Compiler::loadImage(const std::string &fileName,
                                 const SourceLocation &srcLoc) {
  auto [itr, inserted] = images.try_emplace(fileHasher[fileName]);
  auto &image{itr->second};
  if (inserted) {
    SMDL_PROFILER_ENTRY("Compiler::loadImage()", fileName.c_str());
    if (auto error{image.startLoad(fileName)}) {
      srcLoc.logWarn(error->message);
    }
  }
  return image;
}

const Ptexture &Compiler::loadPtexture(const std::string &fileName,
                                       const SourceLocation &srcLoc) {
  auto [itr, inserted] = ptextures.try_emplace(fileHasher[fileName]);
  auto &ptexture{itr->second};
  if (inserted) {
#if SMDL_HAS_PTEX
    SMDL_PROFILER_ENTRY("Compiler::loadPtexture()", fileName.c_str());
    Ptex::String message{};
    auto texture{PtexTexture::open(fileName.c_str(), message,
                                   /*premultiply=*/false)};
    if (!texture) {
      srcLoc.logWarn(
          concat("cannot load ", QuotedPath(fileName), ": ", message.c_str()));
    } else {
      ptexture.texture = texture;
      ptexture.textureFilter = PtexFilter::getFilter(
          texture, PtexFilter::Options(PtexFilter::f_bilinear));
      ptexture.channelCount = texture->numChannels();
      ptexture.alphaIndex = texture->alphaChannel();
    }
#else
    srcLoc.logWarn(
        concat("cannot load ", quoted_path(fileName), ": built without ptex!"));
#endif // #if SMDL_HAS_PTEX
  }
  return ptexture;
}

const BSDFMeasurement &
Compiler::loadBSDFMeasurement(const std::string &fileName,
                              const SourceLocation &srcLoc) {
  auto [itr, inserted] = bsdfMeasurements.try_emplace(fileHasher[fileName]);
  auto &bsdfMeasurement{itr->second};
  if (inserted) {
    SMDL_PROFILER_ENTRY("Compiler::loadBSDFMeasurement()", fileName.c_str());
    if (auto error{bsdfMeasurement.loadFromFile(fileName)}) {
      srcLoc.logWarn(error->message);
    }
  }
  return bsdfMeasurement;
}

const LightProfile &Compiler::loadLightProfile(const std::string &fileName,
                                               const SourceLocation &srcLoc) {
  auto [itr, inserted] = lightProfiles.try_emplace(fileHasher[fileName]);
  auto &lightProfile{itr->second};
  if (inserted) {
    SMDL_PROFILER_ENTRY("Compiler::loadLightProfile()", fileName.c_str());
    if (auto error{lightProfile.loadFromFile(fileName)}) {
      srcLoc.logWarn(error->message);
    }
  }
  return lightProfile;
}

SpectrumView Compiler::loadSpectrum(const std::string &fileName,
                                    const SourceLocation &srcLoc) {
  auto [itr, inserted] = spectrums.try_emplace(fileHasher[fileName]);
  auto &spectrum{itr->second};
  if (inserted) {
    if (auto error{spectrum.loadFromFile(fileName)}) {
      srcLoc.logWarn(error->message);
    }
  }
  return SpectrumView(spectrum);
}

SpectrumView Compiler::loadSpectrum(const std::string &fileName, int curveIndex,
                                    const SourceLocation &srcLoc) {
  auto [itr, inserted] = spectrumLibraries.try_emplace(fileHasher[fileName]);
  auto &spectrumLibrary{itr->second};
  if (inserted) {
    SMDL_PROFILER_ENTRY("Compiler::loadSpectrum()", fileName.c_str());
    if (auto error{spectrumLibrary.loadFromFile(fileName)}) {
      srcLoc.logWarn(error->message);
    }
  }
  return spectrumLibrary.getCurveByIndex(curveIndex);
}

SpectrumView Compiler::loadSpectrum(const std::string &fileName,
                                    const std::string &curveName,
                                    const SourceLocation &srcLoc) {
  auto [itr, inserted] = spectrumLibraries.try_emplace(fileHasher[fileName]);
  auto &spectrumLibrary{itr->second};
  if (inserted) {
    SMDL_PROFILER_ENTRY("Compiler::loadSpectrum()", fileName.c_str());
    if (auto error{spectrumLibrary.loadFromFile(fileName)}) {
      srcLoc.logWarn(error->message);
    }
  }
  return spectrumLibrary.getCurveByName(curveName);
}

std::string Compiler::dump(DumpFormat dumpFormat) {
  if (dumpFormat == DUMP_FORMAT_IR) {
    std::string str{};
    llvm::raw_string_ostream os{str};
    os << getLLVMModule();
    return str;
  } else {
    llvm::SmallVector<char> str{};
    llvm::raw_svector_ostream os{str};
    llvm::legacy::PassManager passManager{};
    if (NativeTarget::get().machine->addPassesToEmitFile(
            passManager, os, nullptr,
            dumpFormat == DUMP_FORMAT_ASM ? llvm::CodeGenFileType::AssemblyFile
                                          : llvm::CodeGenFileType::ObjectFile))
      return "cannot dump";
    passManager.run(getLLVMModule());
    return std::string(os.str());
  }
}

std::optional<Error> Compiler::jitCompile() noexcept {
  SMDL_PROFILER_ENTRY("Compiler::jit_compile()");
  return catchAndReturnError([&] {
    llvmThrowIfError(llvmJit->addIRModule(std::move(*llvmJitModule)));
    llvmJitModule.reset();
    jitLookupOrThrow(jitColorToRgb);
    jitLookupOrThrow(jitRgbToColor);
    for (auto &jitMaterial : jitMaterials) {
      jitLookupOrThrow(jitMaterial.evaluate);
      jitLookupOrThrow(jitMaterial.scatterEvaluate);
      jitLookupOrThrow(jitMaterial.scatterSample);
    }
    for (auto &jitUnitTest : jitUnitTests) {
      jitLookupOrThrow(jitUnitTest.test);
    }
    for (auto &jitExec : jitExecs) {
      jitLookupOrThrow(jitExec);
    }
    // Deallocate everything we no longer need!
    for (auto &module_ : modules) {
      module_->reset();
    }
    allocator.reset();
  });
}

void *Compiler::jitLookup(std::string_view name) noexcept {
  llvm::Expected<llvm::orc::ExecutorAddr> symbol{llvmJit->lookup(name)};
  if (!symbol)
    return nullptr;
  return symbol->toPtr<void *>();
}

const JIT::Material *
Compiler::findMaterial(std::string_view materialName) const noexcept try {
  auto results{llvm::SmallVector<const JIT::Material *>()};
  for (const auto &jitMaterial : jitMaterials) {
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
  for (const auto &jitMaterial : jitMaterials) {
    if (jitMaterial.moduleName == moduleName &&
        jitMaterial.materialName == materialName) {
      return &jitMaterial;
    }
  }
  return nullptr;
}

float3 Compiler::convertColorToRGB(const State &state,
                                   const float *color) const noexcept {
  SMDL_SANITY_CHECK(jitColorToRgb && color);
  SMDL_SANITY_CHECK(state.wavelength_base != nullptr);
  float3 rgb{};
  jitColorToRgb(state, color, rgb);
  return rgb;
}

void Compiler::convertRGBToColor(const State &state, const float3 &rgb,
                                 float *color) const noexcept {
  SMDL_SANITY_CHECK(jitRgbToColor && color);
  SMDL_SANITY_CHECK(state.wavelength_base != nullptr);
  jitRgbToColor(state, rgb, color);
}

std::optional<Error> Compiler::runUnitTests(const State &state) noexcept {
  return catchAndReturnError([&] {
    for (auto itr0 = jitUnitTests.begin(); itr0 != jitUnitTests.end();) {
      auto itr1{itr0};
      while (itr1 != jitUnitTests.end() &&
             itr1->moduleFileName == itr0->moduleFileName) {
        ++itr1;
      }
      std::cerr << concat("Running tests in ",
                          QuotedPath(itr0->moduleFileName), ":\n");
      for (; itr0 != itr1; ++itr0) {
        std::cerr << concat("  ", Quoted(itr0->testName), " (line ",
                            itr0->lineNo, ") ... ");
        try {
          SMDL_SANITY_CHECK(itr0->test);
          itr0->test(state);
          std::cerr << "success\n";
        } catch (const Error &error) {
          std::cerr << "failure\n";
          throw;
        }
      }
      std::cerr << '\n';
    }
  });
}

std::optional<Error> Compiler::runExecs() noexcept {
  return catchAndReturnError([&] {
    for (auto &jitExec : jitExecs)
      jitExec();
  });
}

std::string Compiler::printMaterialSummary() const {
  std::string message{};
  for (auto itr0{jitMaterials.begin()}; itr0 != jitMaterials.end();) {
    auto numMaterials{0};
    auto itr1{itr0};
    while (itr1 != jitMaterials.end() &&
           itr1->moduleFileName == itr0->moduleFileName) {
      ++itr1;
      ++numMaterials;
    }
    message += concat(QuotedPath(itr0->moduleFileName), " contains ",
                      numMaterials, " materials:\n");
    for (; itr0 != itr1; ++itr0) {
      message += "  ";
      message +=
          concat(Quoted(itr0->materialName), " (line ", itr0->lineNo, ")\n");
    }
  }
  return message;
}

extern "C" {

SMDL_EXPORT int smdl_data_exists(void *sceneData, const char *name) {
  return static_cast<SceneData *>(sceneData)->get(name) != nullptr;
}

SMDL_EXPORT void smdl_data_lookup(void *state, void *sceneData, // NOLINT
                                  const char *name, int kind, int size,
                                  void *ptr) {
  if (auto getter{static_cast<SceneData *>(sceneData)->get(name)})
    (*getter)(static_cast<State *>(state), SceneData::Kind(kind), size, ptr);
}

SMDL_EXPORT void smdl_ptex_evaluate(const void *state,
                                    const ::smdl::Ptexture *ptex, int gamma,
                                    int first, int num, float *out) {
  SMDL_SANITY_CHECK(state != nullptr);
  SMDL_SANITY_CHECK(out != nullptr);
  for (int i = 0; i < num; i++) {
    out[i] = 0.0f;
  }
#if SMDL_HAS_PTEX
  if (ptex && first < ptex->channelCount) {
    num = std::min(num, int(ptex->channelCount - first));
    static_cast<PtexFilter *>(ptex->textureFilter)
        ->eval(out, first, num, static_cast<const State *>(state)->ptex_face_id,
               static_cast<const State *>(state)->ptex_face_uv.x,
               static_cast<const State *>(state)->ptex_face_uv.y,
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

} // namespace smdl
