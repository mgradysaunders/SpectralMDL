#include "smdl/Compiler.h"
#include "smdl/Logger.h"
#include <fstream>
#include <iostream>

#include "llvm/Support/CommandLine.h"
#include "llvm/Support/InitLLVM.h"

namespace cl = llvm::cl;
static cl::OptionCategory optionsCat{"Options"};
static cl::SubCommand subDump{"dump", "Dump as LLVM-IR or native assembly"};
static cl::SubCommand subTest{"test", "Execute unit tests"};
static cl::SubCommand subFormat{"format", "Format source code"};
static cl::SubCommandGroup subsWithCompileOptions{&subDump, &subTest};
static cl::SubCommandGroup allSubs{&subDump, &subTest, &subFormat};

static cl::list<std::string> inputFiles{cl::Positional, cl::desc("<input>"),
                                        cl::OneOrMore, cl::sub(allSubs),
                                        cl::cat(optionsCat)};

static cl::opt<unsigned> optLevel{"O",
                                  cl::desc("Optimization level (default 2)"),
                                  cl::Prefix,
                                  cl::init(2U),
                                  cl::sub(subsWithCompileOptions),
                                  cl::cat(optionsCat)};
static cl::opt<bool> enableDebug{"g", cl::desc("Enable debugging"),
                                 cl::sub(subsWithCompileOptions),
                                 cl::cat(optionsCat)};
static cl::opt<smdl::DumpFormat> dumpFormat{
    "f", cl::desc("Dump format:"),
    cl::values(
        cl::OptionEnumValue{"llvm-ir", int(smdl::DumpFormat::IR), "LLVM-IR"},
        cl::OptionEnumValue{"asm", int(smdl::DumpFormat::Assembly),
                            "Native assembly"},
        cl::OptionEnumValue{"obj", int(smdl::DumpFormat::Object),
                            "Native object file"}),
    cl::sub(subDump), cl::cat(optionsCat)};
static cl::opt<std::string> outputFilename{
    "output", cl::desc("Output filename (default stdout)"), cl::Optional,
    cl::sub(subDump), cl::cat(optionsCat)};

static cl::opt<bool> formatInPlace{"i", cl::desc("Format in place"),
                                   cl::sub(subFormat), cl::cat(optionsCat)};
static cl::opt<bool> formatNoComments{"no-comments",
                                      cl::desc("Remove comments"),
                                      cl::sub(subFormat), cl::cat(optionsCat)};
static cl::opt<bool> formatNoAnnotations{
    "no-annotations", cl::desc("Remove annotations"), cl::sub(subFormat),
    cl::cat(optionsCat)};
static cl::opt<bool> formatCompact{"c",
                                   cl::desc("Format output more compactly"),
                                   cl::sub(subFormat), cl::cat(optionsCat)};

int main(int argc, char **argv) {
  llvm::InitLLVM X(argc, argv);
  smdl::Logger::get().add_sink<smdl::LogSinks::print_to_cerr>();
  cl::HideUnrelatedOptions({&optionsCat});
  cl::ParseCommandLineOptions(argc, argv, "SpectralMDL compiler");

  auto compiler{smdl::Compiler{}};
  compiler.enableDebug = enableDebug;
  compiler.enableUnitTests = true;
  compiler.wavelengthBaseMax = 16;

  for (auto &inputFile : inputFiles) {
    if (auto error{compiler.add(std::string(inputFile))}) {
      error->print_and_exit();
    }
  }
  if (subFormat) {
    smdl::FormatOptions options{};
    options.inPlace = formatInPlace;
    options.noComments = formatNoComments;
    options.noAnnotations = formatNoAnnotations;
    options.compact = formatCompact;
    if (auto error{compiler.format_source_code(options)}) {
      error->print_and_exit();
    }
  } else {
    if (auto error{compiler.compile(smdl::OptLevel(unsigned(optLevel)))}) {
      error->print_and_exit();
    }
    if (subDump) {
      if (outputFilename.getNumOccurrences()) {
        auto ofs{std::ofstream(outputFilename.getValue())};
        if (!ofs.is_open())
          std::exit(EXIT_FAILURE);
        ofs << compiler.dump(dumpFormat);
      } else {
        std::cout << compiler.dump(dumpFormat);
        std::cout.flush();
      }
    } else if (subTest) {
      if (auto error{compiler.jit_compile()})
        error->print_and_exit();
      std::array<float, 16> wavelengths{};
      smdl::BumpPtrAllocator allocator{};
      smdl::State state{};
      state.allocator = &allocator;
      state.wavelength_base = &wavelengths[0];
      for (unsigned i = 0; i < 16; i++) {
        float fac = float(i) / 15.0f;
        wavelengths[i] =
            (1 - fac) * state.wavelength_min + fac * state.wavelength_max;
      }
      if (auto error{compiler.run_jit_unit_tests(state)}) {
        std::cerr << '\n';
        error->print_and_exit();
      }
    }
  }
  return 0;
}
