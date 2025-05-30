#include "smdl/Compiler.h"
#include "smdl/Logger.h"

#include <chrono>
#include <future>
#include <iostream>

#include "llvm/ExecutionEngine/Orc/LLJIT.h"
#include "llvm/ExecutionEngine/Orc/ThreadSafeModule.h"

#include "filesystem.h"

#include "Compiler/Context.h"

#if SMDL_HAS_PTEX
#include "Ptexture.h"
#endif // #if SMDL_HAS_PTEX

namespace smdl {

Compiler::Compiler(uint32_t wavelengthBaseMax)
    : wavelengthBaseMax(wavelengthBaseMax) {
  init_or_exit();
}

Compiler::~Compiler() {
#if SMDL_HAS_PTEX
  for (auto &[fileName, ptexture] : ptextures) {
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
  auto addFile{[&](std::string fileName) {
    if (moduleFileNames.insert(fileName).second)
      modules.emplace_back(Module::load_from_file(fileName));
  }};
  if (auto maybePathStr{fileLocator.locate(
          fileOrDirName, {}, FileLocator::REGULAR_FILES | FileLocator::DIRS)}) {
    auto &pathStr{*maybePathStr};
    auto path{fs_make_path(pathStr)};
    auto ec{fs_error_code{}};
    if (fs::is_regular_file(path, ec)) {
      SMDL_LOG_DEBUG("Adding MDL file ", quoted(fs_abbreviate(path)));
      addFile(pathStr);
      return std::nullopt;
    }
    if (fs::is_directory(path, ec) && moduleDirNames.insert(pathStr).second) {
      SMDL_LOG_DEBUG("Adding MDL directory ", quoted(fs_abbreviate(path)));
      for (const auto &entry : fs::recursive_directory_iterator(path)) {
        if (fs::is_regular_file(entry.path(), ec)) {
          if (auto extension{fs_extension(entry.path())};
              extension == ".mdl" || //
              extension == ".smdl") {
            if (auto canonPath{fs::canonical(entry.path(), ec)}; !ec) {
              SMDL_LOG_DEBUG("  Adding MDL file ",
                             quoted(fs_abbreviate(canonPath)));
              addFile(canonPath.string());
            }
          }
        }
      }
      return std::nullopt;
    }
  }
  return Error(concat("cannot locate ", quoted(fileOrDirName)));
}

std::optional<Error> Compiler::compile(OptLevel optLevel) {
  {
    images.clear();
    ptextures.clear();
    llvmJit.reset();
    llvm::ExitOnError exitOnError;
    auto llvmContext{std::make_unique<llvm::LLVMContext>()};
    auto llvmModule{std::make_unique<llvm::Module>("MDL", *llvmContext)};
    llvmModule->setTargetTriple(get_native_target().triple);
    llvmModule->setDataLayout(get_native_target().machine->createDataLayout());
    llvmJitModule = std::make_unique<llvm::orc::ThreadSafeModule>(
        std::move(llvmModule), std::move(llvmContext));
    llvmJit = exitOnError(llvm::orc::LLJITBuilder().create());
    jitRgbToColor.func = nullptr;
    jitColorToRgb.func = nullptr;
    jitMaterials.clear();
    jitUnitTests.clear();
  }
  Context context{*this};
  for (auto &mod : modules)
    mod->reset();
  for (auto &mod : modules)
    if (auto error{mod->parse(allocator)})
      return error;
  for (auto &mod : modules)
    if (auto error{mod->compile(context)})
      return error;
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
    SMDL_LOG_INFO("Loading images ...");
    auto now{std::chrono::steady_clock::now()};
    std::vector<std::future<void>> imageLoads{};
    imageLoads.reserve(images.size());
    for (auto &fileNameAndImage : images)
      imageLoads.push_back(std::async(std::launch::async, [&]() {
        auto &[fileName, image] = fileNameAndImage;
        SMDL_LOG_DEBUG("Loading image ", quoted(fs_abbreviate(fileName)),
                       " ...");
        image->finish_load();
      }));
    for (auto &imageLoad : imageLoads)
      imageLoad.wait();
    auto duration{std::chrono::duration_cast<std::chrono::microseconds>(
                      std::chrono::steady_clock::now() - now)
                      .count()};
    SMDL_LOG_INFO("Loading images done. [", std::to_string(duration * 1e-6),
                  " seconds]");
  }
  if (optLevel != OptLevel::None) {
    llvmJitModule->withModuleDo([&](llvm::Module &llvmModule) {
      LLVMOptimizer llvmOptimizer{};
      llvmOptimizer.run(
          llvmModule, optLevel == OptLevel::O1   ? llvm::OptimizationLevel::O1
                      : optLevel == OptLevel::O2 ? llvm::OptimizationLevel::O2
                                                 : llvm::OptimizationLevel::O3);
    });
  }
  return std::nullopt;
}

std::optional<Error>
Compiler::format_source_code(const FormatOptions &formatOptions) noexcept {
  for (auto &mod : modules) {
    if (!mod->is_builtin()) {
      if (auto error{mod->format_source_code(formatOptions)})
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
  auto &image{images[fileName]};
  if (!image) {
    image.reset(new Image());
    try {
      image->start_load(fileName);
    } catch (const std::exception &err) {
      image->clear();
      srcLoc.log_warn(
          concat("cannot load ", quoted(fileName), ": ", err.what()));
    }
  }
  return *image;
}

Ptexture Compiler::load_ptexture(const std::string &fileName,
                                 const SourceLocation &srcLoc) {
  auto resolvedFileName{
      fileLocator.locate(fileName, srcLoc.get_module_file_name())};
  if (!resolvedFileName) {
    srcLoc.log_warn(
        concat("cannot load ", quoted(fileName), ": file not found"));
    return Ptexture{};
  }
  auto [itr, inserted] = ptextures.try_emplace(*resolvedFileName, Ptexture{});
  auto &ptex{itr->second};
  if (inserted) {
#if SMDL_HAS_PTEX
    Ptex::String message{};
    auto texture{PtexTexture::open(resolvedFileName->c_str(), message,
                                   /*premultiply=*/false)};
    if (!texture) {
      srcLoc.log_warn(
          concat("cannot load ", quoted(fileName), ": ", message.c_str()));
    } else {
      ptex.texture = texture;
      ptex.textureFilter = PtexFilter::getFilter(
          texture, PtexFilter::Options(PtexFilter::f_bilinear));
      ptex.channelCount = texture->numChannels();
      ptex.alphaIndex = texture->alphaChannel();
    }
#else
    srcLoc.log_warn(
        concat("cannot load ", quoted(fileName), ": built without ptex!"));
#endif // #if SMDL_HAS_PTEX
  }
  return ptex;
}

std::string Compiler::dump(DumpFormat dumpFormat) {
  if (dumpFormat == DumpFormat::IR) {
    std::string str{};
    llvm::raw_string_ostream os{str};
    os << get_llvm_module();
    return str;
  } else {
    llvm::SmallVector<char> str{};
    llvm::raw_svector_ostream os{str};
    llvm::legacy::PassManager passManager{};
    if (get_native_target().machine->addPassesToEmitFile(
            passManager, os, nullptr,
            dumpFormat == DumpFormat::Assembly
                ? llvm::CodeGenFileType::AssemblyFile
                : llvm::CodeGenFileType::ObjectFile))
      return "cannot dump";
    passManager.run(get_llvm_module());
    return std::string(os.str());
  }
}

std::optional<Error> Compiler::jit_compile() noexcept {
  return catch_and_return_error([&] {
    llvm_throw_if_error(llvmJit->addIRModule(std::move(*llvmJitModule)));
    llvmJitModule.reset();
    jit_lookup_or_throw(jitColorToRgb);
    jit_lookup_or_throw(jitRgbToColor);
    for (auto &jitMaterial : jitMaterials) {
      jit_lookup_or_throw(jitMaterial.allocate);
      jit_lookup_or_throw(jitMaterial.scatter_evaluate);
      jit_lookup_or_throw(jitMaterial.scatter_sample);
    }
    for (auto &jitUnitTest : jitUnitTests) {
      jit_lookup_or_throw(jitUnitTest.test);
    }
    // Deallocate everything we no longer need!
    for (auto &mod : modules) {
      mod->reset();
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
  float3 rgb{};
  jitColorToRgb(state, color, rgb);
  return rgb;
}

void Compiler::jit_rgb_to_color(const State &state, const float3 &rgb,
                                float *color) const noexcept {
  SMDL_SANITY_CHECK(jitRgbToColor && color);
  SMDL_SANITY_CHECK(state.wavelength_base != nullptr);
  jitRgbToColor(state, rgb, color);
}

std::optional<Error> Compiler::run_jit_unit_tests(const State &state) noexcept {
  return catch_and_return_error([&] {
    for (auto itr0 = jitUnitTests.begin(); itr0 != jitUnitTests.end();) {
      auto itr1{itr0};
      while (itr1 != jitUnitTests.end() &&
             itr1->moduleFileName == itr0->moduleFileName) {
        ++itr1;
      }
      std::cerr << concat("Running tests in ",
                          quoted(fs_abbreviate(itr0->moduleFileName)), ":\n");
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
    message += concat(quoted(fs_abbreviate(itr0->moduleFileName)), " contains ",
                      numMaterials, " materials:\n");
    for (; itr0 != itr1; ++itr0) {
      message += "  ";
      message +=
          concat(quoted(itr0->materialName), " (line ", itr0->lineNo, ")\n");
    }
  }
  return message;
}

} // namespace smdl
