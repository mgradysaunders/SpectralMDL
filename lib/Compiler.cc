#include "smdl/Compiler.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Parallel.h"
#include "smdl/Support/Profiler.h"

#include <chrono>
#include <iostream>

#include "llvm/ExecutionEngine/Orc/LLJIT.h"
#include "llvm/ExecutionEngine/Orc/ThreadSafeModule.h"

#include "Archive.h"
#include "filesystem.h"

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
      SMDL_LOG_DEBUG("Adding MDL archive ", quoted_path(fileName));
      auto archive{Archive{fileName}};
      for (int i = 0; i < archive.get_file_count(); i++) {
        if (auto entryPath{join_paths(fileName, archive.get_file_name(i))};
            has_extension(entryPath, ".mdl")) {
          if (moduleFileNames.insert(entryPath).second) {
            SMDL_LOG_DEBUG("Adding MDL file from archive ",
                           quoted_path(entryPath));
            modules.emplace_back(Module::load_from_file_extracted_from_archive(
                entryPath, archive.extract_file(i)));
          }
        }
      }
    } else {
      if (moduleFileNames.insert(fileName).second) {
        SMDL_LOG_DEBUG("Adding MDL file ", quoted_path(fileName));
        modules.emplace_back(Module::load_from_file(fileName));
      }
    }
  }};
  if (auto maybePath{fileLocator.locate(
          fileOrDirName, {}, FileLocator::REGULAR_FILES | FileLocator::DIRS)}) {
    auto &path{*maybePath};
    if (is_file(path)) {
      addFile(path);
      return std::nullopt;
    } else if (is_directory(path) && moduleDirNames.insert(path).second) {
      SMDL_LOG_DEBUG("Adding MDL directory ", quoted_path(path));
      moduleDirSearchPaths.emplace_back(path);
      for (const auto &entry : fs::directory_iterator(path)) {
        if (auto entryPath{canonical(entry.path().string())};
            is_file(entryPath) && has_extension(entryPath, ".mdr")) {
          addFile(entryPath);
        }
      }
      for (const auto &entry : fs::recursive_directory_iterator(path)) {
        if (auto entryPath{canonical(entry.path().string())};
            is_file(entryPath) && (has_extension(entryPath, ".mdl") ||
                                   has_extension(entryPath, ".smdl"))) {
          addFile(entryPath);
        }
      }
      return std::nullopt;
    }
  }
  return Error(concat("cannot locate ", quoted(fileOrDirName)));
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
    llvmModule->setTargetTriple(NativeTarget::get().triple);
    llvmModule->setDataLayout(NativeTarget::get().machine->createDataLayout());
    llvmJitModule = std::make_unique<llvm::orc::ThreadSafeModule>(
        std::move(llvmModule), std::move(llvmContext));
    llvmJit = exitOnError(llvm::orc::LLJITBuilder().create());
    jitRgbToColor.func = nullptr;
    jitColorToRgb.func = nullptr;
    jitMaterials.clear();
    jitUnitTests.clear();
  }
  auto initializeEntry{profiler_entry_begin("Initialize")};
  Context context{*this};
  for (auto &module_ : modules)
    module_->reset();
  profiler_entry_end(initializeEntry);
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
    parallel_for(0, images.size(), [&](size_t i) {
      auto &[fileHash, image] = *std::next(images.begin(), i);
      SMDL_PROFILER_ENTRY("Load image",
                          fileHash->canonicalFileNames[0].c_str());
      SMDL_LOG_DEBUG("Loading image ",
                     quoted_path(fileHash->canonicalFileNames[0]), " ...");
      image.finish_load();
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
Compiler::format_source_code(const FormatOptions &formatOptions) noexcept {
  SMDL_PROFILER_ENTRY("Compiler::format_source_code()");
  for (auto &module_ : modules) {
    if (!module_->is_builtin()) {
      if (auto error{module_->format_source_code(formatOptions)})
        return error;
    }
  }
  return std::nullopt;
}

llvm::LLVMContext &Compiler::get_llvm_context() noexcept {
  SMDL_SANITY_CHECK(llvmJitModule.get() != nullptr);
  return *llvmJitModule.get()->getContext().getContext();
}

llvm::Module &Compiler::get_llvm_module() noexcept {
  SMDL_SANITY_CHECK(llvmJitModule.get() != nullptr);
  return *llvmJitModule.get()->getModuleUnlocked();
}

const Image &Compiler::load_image(const std::string &fileName,
                                  const SourceLocation &srcLoc) {
  auto [itr, inserted] = images.try_emplace(fileHasher[fileName]);
  auto &image{itr->second};
  if (inserted) {
    SMDL_PROFILER_ENTRY("Compiler::load_image()", fileName.c_str());
    if (auto error{image.start_load(fileName)}) {
      srcLoc.log_warn(error->message);
    }
  }
  return image;
}

const Ptexture &Compiler::load_ptexture(const std::string &fileName,
                                        const SourceLocation &srcLoc) {
  auto [itr, inserted] = ptextures.try_emplace(fileHasher[fileName]);
  auto &ptexture{itr->second};
  if (inserted) {
#if SMDL_HAS_PTEX
    SMDL_PROFILER_ENTRY("Compiler::load_ptexture()", fileName.c_str());
    Ptex::String message{};
    auto texture{PtexTexture::open(fileName.c_str(), message,
                                   /*premultiply=*/false)};
    if (!texture) {
      srcLoc.log_warn(
          concat("cannot load ", quoted_path(fileName), ": ", message.c_str()));
    } else {
      ptexture.texture = texture;
      ptexture.textureFilter = PtexFilter::getFilter(
          texture, PtexFilter::Options(PtexFilter::f_bilinear));
      ptexture.channelCount = texture->numChannels();
      ptexture.alphaIndex = texture->alphaChannel();
    }
#else
    srcLoc.log_warn(
        concat("cannot load ", quoted_path(fileName), ": built without ptex!"));
#endif // #if SMDL_HAS_PTEX
  }
  return ptexture;
}

const BSDFMeasurement &
Compiler::load_bsdf_measurement(const std::string &fileName,
                                const SourceLocation &srcLoc) {
  auto [itr, inserted] = bsdfMeasurements.try_emplace(fileHasher[fileName]);
  auto &bsdfMeasurement{itr->second};
  if (inserted) {
    SMDL_PROFILER_ENTRY("Compiler::load_bsdf_measurement()", fileName.c_str());
    if (auto error{bsdfMeasurement.load_from_file(fileName)}) {
      srcLoc.log_warn(error->message);
    }
  }
  return bsdfMeasurement;
}

const LightProfile &Compiler::load_light_profile(const std::string &fileName,
                                                 const SourceLocation &srcLoc) {
  auto [itr, inserted] = lightProfiles.try_emplace(fileHasher[fileName]);
  auto &lightProfile{itr->second};
  if (inserted) {
    SMDL_PROFILER_ENTRY("Compiler::load_light_profile()", fileName.c_str());
    if (auto error{lightProfile.load_from_file(fileName)}) {
      srcLoc.log_warn(error->message);
    }
  }
  return lightProfile;
}

std::string Compiler::dump(DumpFormat dumpFormat) {
  if (dumpFormat == DUMP_FORMAT_IR) {
    std::string str{};
    llvm::raw_string_ostream os{str};
    os << get_llvm_module();
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
    passManager.run(get_llvm_module());
    return std::string(os.str());
  }
}

std::optional<Error> Compiler::jit_compile() noexcept {
  SMDL_PROFILER_ENTRY("Compiler::jit_compile()");
  return catch_and_return_error([&] {
    llvm_throw_if_error(llvmJit->addIRModule(std::move(*llvmJitModule)));
    llvmJitModule.reset();
    jit_lookup_or_throw(jitColorToRgb);
    jit_lookup_or_throw(jitRgbToColor);
    for (auto &jitMaterial : jitMaterials) {
      jit_lookup_or_throw(jitMaterial.allocate);
      jit_lookup_or_throw(jitMaterial.scatter_evaluate);
      jit_lookup_or_throw(jitMaterial.scatter_sample);
      // jit_lookup_or_throw(jitMaterial.emission_evaluate);
      // jit_lookup_or_throw(jitMaterial.emission_sample);
    }
    for (auto &jitUnitTest : jitUnitTests) {
      jit_lookup_or_throw(jitUnitTest.test);
    }
    for (auto &jitExec : jitExecs) {
      jit_lookup_or_throw(jitExec);
    }
    // Deallocate everything we no longer need!
    for (auto &module_ : modules) {
      module_->reset();
    }
    allocator.reset();
  });
}

void *Compiler::jit_lookup(std::string_view name) noexcept {
  llvm::Expected<llvm::orc::ExecutorAddr> symbol{llvmJit->lookup(name)};
  if (!symbol)
    return nullptr;
  return symbol->toPtr<void *>();
}

const JIT::Material *
Compiler::find_jit_material(std::string_view materialName) const noexcept try {
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
    auto message{concat("Material ", quoted(materialName),
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
Compiler::find_jit_material(std::string_view moduleName,
                            std::string_view materialName) const noexcept {
  for (const auto &jitMaterial : jitMaterials) {
    if (jitMaterial.moduleName == moduleName &&
        jitMaterial.materialName == materialName) {
      return &jitMaterial;
    }
  }
  return nullptr;
}

float3 Compiler::jit_color_to_rgb(const State &state,
                                  const float *color) const noexcept {
  SMDL_SANITY_CHECK(jitColorToRgb && color);
  SMDL_SANITY_CHECK(state.wavelength_base != nullptr);
  SMDL_PROFILER_ENTRY("Compiler::jit_color_to_rgb()");
  float3 rgb{};
  jitColorToRgb(state, color, rgb);
  return rgb;
}

void Compiler::jit_rgb_to_color(const State &state, const float3 &rgb,
                                float *color) const noexcept {
  SMDL_SANITY_CHECK(jitRgbToColor && color);
  SMDL_SANITY_CHECK(state.wavelength_base != nullptr);
  SMDL_PROFILER_ENTRY("Compiler::jit_rgb_to_color()");
  jitRgbToColor(state, rgb, color);
}

std::optional<Error> Compiler::jit_unit_tests(const State &state) noexcept {
  return catch_and_return_error([&] {
    for (auto itr0 = jitUnitTests.begin(); itr0 != jitUnitTests.end();) {
      auto itr1{itr0};
      while (itr1 != jitUnitTests.end() &&
             itr1->moduleFileName == itr0->moduleFileName) {
        ++itr1;
      }
      std::cerr << concat("Running tests in ",
                          quoted_path(itr0->moduleFileName), ":\n");
      for (; itr0 != itr1; ++itr0) {
        std::cerr << concat("  ", quoted(itr0->testName), " (line ",
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

std::optional<Error> Compiler::jit_execs() noexcept {
  return catch_and_return_error([&] {
    for (auto &jitExec : jitExecs)
      jitExec();
  });
}

std::string Compiler::summarize_materials() const {
  std::string message{};
  for (auto itr0{jitMaterials.begin()}; itr0 != jitMaterials.end();) {
    auto numMaterials{0};
    auto itr1{itr0};
    while (itr1 != jitMaterials.end() &&
           itr1->moduleFileName == itr0->moduleFileName) {
      ++itr1;
      ++numMaterials;
    }
    message += concat(quoted_path(itr0->moduleFileName), " contains ",
                      numMaterials, " materials:\n");
    for (; itr0 != itr1; ++itr0) {
      message += "  ";
      message +=
          concat(quoted(itr0->materialName), " (line ", itr0->lineNo, ")\n");
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
