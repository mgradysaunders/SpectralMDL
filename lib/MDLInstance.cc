#include "smdl/MDLInstance.h"

#include "Compiler/Context.h"
#include "Compiler/Emitter.h"
#include "Compiler/Module.h"

#include "llvm.h"

#if WITH_PTEX
#include "Ptexture.h"
#endif // #if WITH_PTEX

#include <iostream>

namespace smdl {

static std::filesystem::path canonicalize(const std::filesystem::path &path) {
  std::error_code errorCode{};
  auto canonicalPath{std::filesystem::canonical(path, errorCode)};
  if (errorCode)
    throw Error(std::format("can't canonicalize path: {} ({})", path.string(), errorCode.message()));
  return canonicalPath;
}

MDLInstance::MDLInstance(unsigned numWavelens) : numWavelens(numWavelens) {
  llvm::ExitOnError exitOnError;
  auto llvmContext{std::make_unique<llvm::LLVMContext>()};
  auto llvmModule{std::make_unique<llvm::Module>("MDL", *llvmContext)};
  llvmModule->setTargetTriple(llvm_get_native_target_triple());
  llvmModule->setDataLayout(llvm_get_native_target_machine()->createDataLayout());
  llvmJITModule = std::make_unique<llvm::orc::ThreadSafeModule>(std::move(llvmModule), std::move(llvmContext));
  llvmJIT = exitOnError(llvm::orc::LLJITBuilder().create());
}

MDLInstance::~MDLInstance() {
#if WITH_PTEX
  for (auto &[pathStr, ptex] : ptexs) {
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

llvm::LLVMContext &MDLInstance::get_llvm_context() {
  sanity_check(llvmJITModule != nullptr);
  return *llvmJITModule->getContext().getContext();
}

llvm::Module &MDLInstance::get_llvm_module() {
  sanity_check(llvmJITModule != nullptr);
  return *llvmJITModule->getModuleUnlocked();
}

std::optional<Error> MDLInstance::load_all_modules(std::filesystem::path path) {
  for (const auto &dirEntry : std::filesystem::recursive_directory_iterator(path)) {
    if (dirEntry.is_regular_file() && dirEntry.path().extension() == ".mdl") {
      if (auto error{load_module(dirEntry.path())})
        return error;
    }
  }
  return std::nullopt;
}

std::optional<Error> MDLInstance::load_module(std::filesystem::path path) {
  return catch_and_return_error([&] {
    // Canonicalize the file path and only load if not already loaded.
    path = canonicalize(path);
    std::string pathStr{path.string()};
    if (!modulePaths.contains(pathStr)) {
      modules.emplace_back(new Compiler::Module(std::move(path)));
      modulePaths.insert(std::move(pathStr));
    }
  });
}

std::optional<Error> MDLInstance::compile(OptLevel optLevel) {
  return catch_and_return_error([&] {
    llvm::BumpPtrAllocator bumpAllocator{};
    Compiler::Context context{*this, bumpAllocator};
    for (auto &module : modules)
      module->parse(context);
    for (auto &module : modules)
      module->emit(context);
    if (optLevel != OptLevel::None)
      llvmJITModule->withModuleDo([&](llvm::Module &M) {
        LLVMOptimizer llvmOptimizer{};
        llvmOptimizer.run(
            M, optLevel == OptLevel::None ? llvm::OptimizationLevel::O0
               : optLevel == OptLevel::O1 ? llvm::OptimizationLevel::O1
               : optLevel == OptLevel::O2 ? llvm::OptimizationLevel::O2
                                          : llvm::OptimizationLevel::O3);
      });
    modules.clear();
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

std::optional<Error> MDLInstance::compile_jit() {
  return catch_and_return_error([&] {
    llvm_throw_if_error(llvmJIT->addIRModule(std::move(*llvmJITModule)));
    llvmJITModule.reset();
    if (enableUnitTests) {
      for (auto &unitTest : unitTests) {
        auto unitTestFn{lookup_jit_symbol<void(const state_t &)>(unitTest.llvmName)};
        if (!unitTestFn)
          throw Error(std::format("can't find JIT symbol for unit test '{}'", unitTest.name));
        unitTest.test = unitTestFn;
      }
    }
  });
}

std::optional<Error> MDLInstance::execute_jit_unit_tests(const state_t &state) {
  return catch_and_return_error([&] {
    for (auto &unitTest : unitTests) {
      llvm::errs() << "Running test ";
      llvm::WithColor(llvm::errs(), llvm::HighlightColor::String) << "'" << unitTest.name << "'";
      llvm::errs() << " ... ";
      try {
        sanity_check(unitTest.test);
        unitTest.test(state);
        llvm::WithColor(llvm::errs(), llvm::HighlightColor::Remark) << "passed\n";
      } catch (const Error &error) {
        llvm::WithColor(llvm::errs(), llvm::HighlightColor::Error) << "failed\n";
        throw;
      }
    }
  });
}

void *MDLInstance::lookup_jit_symbol(std::string_view name) {
  llvm::Expected<llvm::orc::ExecutorAddr> symbol{llvmJIT->lookup(name)};
  if (!symbol)
    return nullptr;
  return symbol->toPtr<void *>();
}

} // namespace smdl
