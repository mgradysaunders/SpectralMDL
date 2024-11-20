#include "llvm/Support/CommandLine.h"
#include "llvm/Support/InitLLVM.h"
#include "smdl/MDLInstance.h"
#include <fstream>
#include <iostream>

namespace cl = llvm::cl;

static cl::OptionCategory cat{"Options"};
static cl::SubCommand subDump{"dump", "Dump as LLVM-IR or native assembly"};
static cl::SubCommand subTest{"test", "Execute unit tests"};
static cl::SubCommandGroup allSubs{&subDump, &subTest};

static cl::list<std::string> inputFiles{cl::Positional, cl::desc("<input>"), cl::OneOrMore, cl::sub(allSubs), cl::cat(cat)};
static cl::opt<unsigned> optLevel{
    "O", cl::desc("Optimization level (default 2)"), cl::Prefix, cl::init(2U), cl::sub(allSubs), cl::cat(cat)};
static cl::opt<bool> enableDebug{"g", cl::desc("Enable debugging"), cl::sub(allSubs), cl::cat(cat)};
static cl::opt<smdl::OutputFormat> outputFormat{
    "format", cl::desc("Output format:"),
    cl::values(
        cl::OptionEnumValue("llvm-ir", int(smdl::OutputFormat::IR), "LLVM-IR"),
        cl::OptionEnumValue("asm", int(smdl::OutputFormat::Assembly), "Native assembly"),
        cl::OptionEnumValue("obj", int(smdl::OutputFormat::Object), "Native object file")),
    cl::sub(subDump), cl::cat(cat)};
static cl::opt<std::string> outputFilename{
    "output", cl::desc("Output filename (default stdout)"), cl::Optional, cl::sub(subDump), cl::cat(cat)};

static cl::OptionCategory catState{"Options for state"};
static cl::opt<unsigned> wavelengthBaseMax{
    "wavelength_base_max", cl::desc("Number of wavelengths (default 16)"), cl::init(16U), cl::sub(allSubs), cl::cat(catState)};
static cl::opt<float> minWavelen{
    "wavelength_min", cl::desc("Wavelength minimum in nanometers (default 380)"), cl::init(380.0f), cl::sub(subTest),
    cl::cat(catState)};
static cl::opt<float> maxWavelen{
    "wavelength_max", cl::desc("Wavelength maximum in nanometers (default 720)"), cl::init(720.0f), cl::sub(subTest),
    cl::cat(catState)};

int main(int argc, char **argv) {
  llvm::InitLLVM X(argc, argv);
  smdl::init_or_exit();
  cl::HideUnrelatedOptions({&cat, &catState});
  cl::ParseCommandLineOptions(argc, argv, "SpectralMDL compiler");
  smdl::MDLInstance mdl{};
  try {
    mdl.enableDebug = enableDebug;
    mdl.enableUnitTests = true;
    mdl.wavelengthBaseMax = wavelengthBaseMax;
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
      state.wavelength_min = minWavelen;
      state.wavelength_max = maxWavelen;
      std::vector<float> wavelength_base{};
      wavelength_base.resize(mdl.wavelengthBaseMax);
      for (size_t i = 0; i < mdl.wavelengthBaseMax; i++) {
        float t = float(i) / float(mdl.wavelengthBaseMax - 1);
        wavelength_base[i] = (1 - t) * state.wavelength_min + t * state.wavelength_max;
      }
      state.wavelength_base = wavelength_base.data();
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
