#include "smdl/MDLInstance.h"

#include "Compiler/Context.h"
#include "Compiler/Emitter.h"
#include "Compiler/Module.h"
#include "DataLookup.h"

#include "llvm.h"

#if WITH_PTEX
#include "Ptexture.h"
#endif // #if WITH_PTEX

namespace smdl {

MDLInstance::MDLInstance(uint32_t wavelengthBaseMax) : wavelengthBaseMax(wavelengthBaseMax) {
  llvm::ExitOnError exitOnError;
  auto llvmContext{std::make_unique<llvm::LLVMContext>()};
  auto llvmModule{std::make_unique<llvm::Module>("MDL", *llvmContext)};
  llvmModule->setTargetTriple(llvm_get_native_target_triple());
  llvmModule->setDataLayout(llvm_get_native_target_machine()->createDataLayout());
  llvmJitModule = std::make_unique<llvm::orc::ThreadSafeModule>(std::move(llvmModule), std::move(llvmContext));
  llvmJit = exitOnError(llvm::orc::LLJITBuilder().create());
  dataLookup = std::make_unique<DataLookup>();
}

MDLInstance::~MDLInstance() {
#if WITH_PTEX
  for (auto &[path, ptex] : ptextures) {
    if (ptex.filter) {
      static_cast<PtexFilter *>(ptex.filter)->release();
      ptex.filter = nullptr;
    }
    if (ptex.texture) {
      static_cast<PtexTexture *>(ptex.texture)->release();
      ptex.texture = nullptr;
    }
  }
#endif // #if WITH_PTEX
}

llvm::LLVMContext &MDLInstance::get_llvm_context() noexcept {
  return *sanity_check_nonnull(llvmJitModule.get())->getContext().getContext();
}

llvm::Module &MDLInstance::get_llvm_module() noexcept {
  return *sanity_check_nonnull(llvmJitModule.get())->getModuleUnlocked();
}

std::optional<Error> MDLInstance::add(std::filesystem::path path) noexcept {
  return catch_and_return_error([&] {
    path = std::filesystem::canonical(path);
    if (std::filesystem::is_regular_file(path)) {
      moduleFilenames.insert(std::move(path));
    } else if (std::filesystem::is_directory(path)) {
      if (moduleDirnames.insert(path).second) {
        for (const auto &entry : std::filesystem::recursive_directory_iterator(path)) {
          if (entry.is_regular_file()) {
            if (auto entryExt{to_lower(entry.path().extension().string())}; entryExt == ".mdl" || entryExt == ".smdl") {
              moduleFilenames.insert(std::filesystem::canonical(entry.path()));
            }
          }
        }
      }
    }
  });
}

std::optional<Error> MDLInstance::format_source() noexcept {
  return catch_and_return_error([&] {
    llvm::BumpPtrAllocator bumpAllocator{};
    Compiler::Context context{*this, bumpAllocator};
    for (auto &moduleFilename : moduleFilenames)
      modules.emplace_back(new Compiler::Module(moduleFilename));
    // Parse everything before formatting anything so that, if an 
    // error occurs, we do not write anything to disk.
    for (auto &mod : modules) 
      mod->parse(context);
    for (auto &mod : modules)
      mod->format_source();
    modules.clear();
  });
}

std::optional<Error> MDLInstance::compile(OptLevel optLevel) noexcept {
  return catch_and_return_error([&] {
    {
      llvm::BumpPtrAllocator bumpAllocator{};
      Compiler::Context context{*this, bumpAllocator};
      for (auto &moduleFilename : moduleFilenames)
        modules.emplace_back(new Compiler::Module(moduleFilename));
      for (auto &mod : modules)
        mod->parse(context);
      for (auto &mod : modules)
        mod->emit(context);
      modules.clear();
    }
    if (optLevel != OptLevel::None)
      llvmJitModule->withModuleDo([&](llvm::Module &llvmModule) {
        LLVMOptimizer llvmOptimizer{};
        llvmOptimizer.run(
            llvmModule, optLevel == OptLevel::None ? llvm::OptimizationLevel::O0
                        : optLevel == OptLevel::O1 ? llvm::OptimizationLevel::O1
                        : optLevel == OptLevel::O2 ? llvm::OptimizationLevel::O2
                                                   : llvm::OptimizationLevel::O3);
      });
  });
}

std::string MDLInstance::dump(OutputFormat outputFormat) {
  if (outputFormat == OutputFormat::IR) {
    std::string str{};
    llvm::raw_string_ostream os{str};
    os << get_llvm_module();
    return str;
  } else {
    llvm::SmallVector<char> str{};
    llvm::raw_svector_ostream os{str};
    llvm::legacy::PassManager passManager{};
    if (llvm_get_native_target_machine()->addPassesToEmitFile(
            passManager, os, nullptr,
            outputFormat == OutputFormat::Assembly ? llvm::CodeGenFileType::AssemblyFile : llvm::CodeGenFileType::ObjectFile))
      return "can't dump";
    passManager.run(get_llvm_module());
    return std::string(os.str());
  }
}

std::optional<Error> MDLInstance::compile_jit() noexcept {
  return catch_and_return_error([&] {
    llvm_throw_if_error(llvmJit->addIRModule(std::move(*llvmJitModule)));
    llvmJitModule.reset();
    lookup_jit_or_throw(colorToRgb);
    lookup_jit_or_throw(rgbToColor);
    for (auto &material : materials) {
      lookup_jit_or_throw(material.evalGeometry);
      lookup_jit_or_throw(material.evalBsdf);
      lookup_jit_or_throw(material.evalBsdfSample);
    }
    for (auto &unitTest : unitTests) {
      lookup_jit_or_throw(unitTest.exec);
    }
  });
}

std::optional<Error> MDLInstance::execute_jit_unit_tests(const state_t &state) noexcept {
  return catch_and_return_error([&] {
    for (auto &unitTest : unitTests) {
      llvm::errs() << "Running test ";
      llvm::WithColor(llvm::errs(), llvm::HighlightColor::String) << "'" << unitTest.name << "'";
      llvm::errs() << " ... ";
      try {
        sanity_check(unitTest.exec);
        unitTest.exec(state);
        llvm::WithColor(llvm::errs(), llvm::HighlightColor::Remark) << "passed\n";
      } catch (const Error &error) {
        llvm::WithColor(llvm::errs(), llvm::HighlightColor::Error) << "failed\n";
        throw;
      }
    }
  });
}

void *MDLInstance::lookup_jit(std::string_view name) noexcept {
  llvm::Expected<llvm::orc::ExecutorAddr> symbol{llvmJit->lookup(name)};
  if (!symbol)
    return nullptr;
  return symbol->toPtr<void *>();
}

void MDLInstance::set_data_int(std::string_view name, int_t value) { dataLookup->values[name] = value; }

void MDLInstance::set_data_int2(std::string_view name, int2_t value) { dataLookup->values[name] = value; }

void MDLInstance::set_data_int3(std::string_view name, int3_t value) { dataLookup->values[name] = value; }

void MDLInstance::set_data_int4(std::string_view name, int4_t value) { dataLookup->values[name] = value; }

void MDLInstance::set_data_float(std::string_view name, float_t value) { dataLookup->values[name] = value; }

void MDLInstance::set_data_float2(std::string_view name, float2_t value) { dataLookup->values[name] = value; }

void MDLInstance::set_data_float3(std::string_view name, float3_t value) { dataLookup->values[name] = value; }

void MDLInstance::set_data_float4(std::string_view name, float4_t value) { dataLookup->values[name] = value; }

void MDLInstance::set_data_color(std::string_view name, std::span<const float_t> value) {
  sanity_check(wavelengthBaseMax == value.size());
  dataLookup->values[name] = DataLookup::color_t(value.begin(), value.end());
}

const jit::Material *MDLInstance::find_material(std::string_view name) const noexcept {
  if (auto itr{std::find_if(materials.begin(), materials.end(), [&](auto &material) { return material.name == name; })};
      itr != materials.end())
    return &*itr;
  return nullptr;
}

const jit::Material *MDLInstance::find_material(std::string_view moduleName, std::string_view name) const noexcept {
  if (auto itr{std::find_if(
          materials.begin(), materials.end(),
          [&](auto &material) { return material.moduleName == moduleName && material.name == name; })};
      itr != materials.end())
    return &*itr;
  return nullptr;
}

float3_t MDLInstance::color_to_rgb(const state_t &state, std::span<const float_t> color) const noexcept {
  float3_t rgb{};
  sanity_check(color.data() != nullptr);
  sanity_check(color.size() == wavelengthBaseMax);
  colorToRgb(state, color.data(), rgb);
  return rgb;
}

void MDLInstance::rgb_to_color(const state_t &state, const float3_t &rgb, std::span<float_t> color) const noexcept {
  sanity_check(color.data() != nullptr);
  sanity_check(color.size() == wavelengthBaseMax);
  rgbToColor(state, rgb, color.data());
}

} // namespace smdl
