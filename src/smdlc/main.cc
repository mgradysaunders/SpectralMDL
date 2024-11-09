#include "llvm/Support/CommandLine.h"
#include "llvm/Support/InitLLVM.h"
#include "smdl/MDLInstance.h"
#include <iostream>

namespace cl = llvm::cl;

static cl::OptionCategory category("Compiler Options");
static cl::list<std::string> inputFiles{cl::Positional, cl::desc("<input>"), cl::OneOrMore};
static cl::opt<smdl::OutputFormat> outputFormat{
    "format", cl::desc("Output format:"),
    cl::values(
        cl::OptionEnumValue("llvm-ir", int(smdl::OutputFormat::IR), "LLVM-IR"),
        cl::OptionEnumValue("asm", int(smdl::OutputFormat::Assembly), "Native assembly"),
        cl::OptionEnumValue("obj", int(smdl::OutputFormat::Object), "Native object file")),
    cl::cat(category)};
static cl::opt<unsigned> optLevel{"O", cl::desc("Optimization level (default 2)"), cl::Prefix, cl::init(2U), cl::cat(category)};
static cl::opt<bool> enableDebug{"g", cl::desc("Enable debugging"), cl::cat(category)};
static cl::opt<unsigned> numWavelens{
    "num-wavelengths", cl::desc("Number of wavelengths (default 16)"), cl::init(16U), cl::cat(category)};
static cl::opt<bool> enableUnitTests{"test", cl::desc("Run unit tests"), cl::cat(category)};

int main(int argc, char **argv) {
  llvm::InitLLVM X(argc, argv);
  smdl::init_or_exit();
  llvm::cl::ParseCommandLineOptions(argc, argv, "SpectralMDL Compiler");
  smdl::MDLInstance mdl{};
  try {
    mdl.enableDebug = enableDebug;
    mdl.enableUnitTests = true;
    mdl.numWavelens = numWavelens;
    for (auto &inputFile : inputFiles) {
      if (auto error = mdl.load_module(std::string(inputFile))) {
        std::cerr << "load_module: " << std::string(*error) << '\n';
        std::exit(EXIT_FAILURE);
      }
    }
    if (auto error = mdl.compile(smdl::OptLevel(unsigned(optLevel)))) {
      std::cerr << "compile: " << std::string(*error) << '\n';
      std::exit(EXIT_FAILURE);
    }
    if (enableUnitTests) {
      if (auto error = mdl.compile_jit()) {
        std::cerr << "compile_jit: " << std::string(*error) << '\n';
        std::exit(EXIT_FAILURE);
      }
      smdl::state_t state{};
      if (auto error = mdl.execute_jit_unit_tests(state)) {
        std::cerr << "execute_jit_unit_tests: " << std::string(*error) << '\n';
        std::exit(EXIT_FAILURE);
      }
      std::cerr << "All tests passed!\n";
    } else {
      std::cout << mdl.dump(outputFormat);
      std::cout.flush();
    }
  } catch (const smdl::Error &error) {
    std::cerr << std::string(error) << '\n';
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}
