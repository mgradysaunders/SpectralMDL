#include "llvm/Support/CommandLine.h"
#include "llvm/Support/InitLLVM.h"
#include "smdl/ImageLoader.h"
#include "smdl/MDLInstance.h"
#include <iostream>
#include <fstream>

namespace cl = llvm::cl;

static cl::SubCommand subDump{"dump", "Dump as LLVM-IR or native assembly"};
static cl::SubCommand subTest{"test", "Execute unit tests"};
static cl::SubCommandGroup allSubs{&subDump, &subTest};

static cl::list<std::string> inputFiles{cl::Positional, cl::desc("<input>"), cl::OneOrMore, cl::sub(allSubs)};
static cl::opt<unsigned> optLevel{"O", cl::desc("Optimization level (default 2)"), cl::Prefix, cl::init(2U), cl::sub(allSubs)};
static cl::opt<bool> enableDebug{"g", cl::desc("Enable debugging"), cl::sub(allSubs)};
static cl::opt<smdl::OutputFormat> outputFormat{
    "format", cl::desc("Output format:"),
    cl::values(
        cl::OptionEnumValue("llvm-ir", int(smdl::OutputFormat::IR), "LLVM-IR"),
        cl::OptionEnumValue("asm", int(smdl::OutputFormat::Assembly), "Native assembly"),
        cl::OptionEnumValue("obj", int(smdl::OutputFormat::Object), "Native object file")),
    cl::sub(subDump)};
static cl::opt<std::string> outputFilename{"output", cl::desc("Output filename (default stdout)"), cl::Optional, cl::sub(subDump)};
static cl::alias outputFormatAlias{"f", cl::aliasopt(outputFormat)};
static cl::alias outputFilenameAlias{"o", cl::aliasopt(outputFilename)};
static cl::opt<unsigned> numWavelens{
    "num-wavelens", cl::desc("Number of wavelengths (default 16)"), cl::init(16U), cl::sub(allSubs)};

int main(int argc, char **argv) {
  llvm::InitLLVM X(argc, argv);
  smdl::init_or_exit();
  llvm::cl::ParseCommandLineOptions(argc, argv, "SpectralMDL compiler");
  smdl::MDLInstance mdl{};
  try {
    mdl.enableDebug = enableDebug;
    mdl.enableUnitTests = true;
    mdl.imageLoader = smdl::ImageLoader;
    mdl.numWavelens = numWavelens;
    for (auto &inputFile : inputFiles) {
      if (auto error = mdl.load_module(std::string(inputFile))) {
        error->print();
        std::exit(EXIT_FAILURE);
      }
    }
    if (auto error = mdl.compile(smdl::OptLevel(unsigned(optLevel)))) {
      error->print();
      std::exit(EXIT_FAILURE);
    }
    if (subDump) {
      if (outputFilename.getNumOccurrences()) {
        auto ofs{std::ofstream(outputFilename.getValue())};
        if (!ofs.is_open())
          std::exit(EXIT_FAILURE);
        ofs << mdl.dump(outputFormat);
      } else {
        std::cout << mdl.dump(outputFormat);
        std::cout.flush();
      }
    } else if (subTest) {
      if (auto error = mdl.compile_jit()) {
        error->print();
        std::exit(EXIT_FAILURE);
      }
      smdl::state_t state{};
      if (auto error = mdl.execute_jit_unit_tests(state)) {
        error->print();
        std::exit(EXIT_FAILURE);
      }
      std::cerr << "All tests passed!\n";
    }
  } catch (const smdl::Error &error) {
    error.print();
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}
