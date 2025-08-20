#include "smdl/Compiler.h"
#include "smdl/Support/Logger.h"
#include <fstream>
#include <iostream>

#include "llvm/Support/CommandLine.h"
#include "llvm/Support/InitLLVM.h"

namespace cl = llvm::cl;
static cl::OptionCategory optionsCat{"Options"};
static cl::SubCommand subDump{"dump", "Dump as LLVM-IR or native assembly"};
static cl::SubCommand subList{"list", "List all materials"};
static cl::SubCommand subRun{"run", "Run execs"};
static cl::SubCommand subTest{"test", "Run execs and unit tests"};
static cl::SubCommand subFormat{"format", "Format source code"};
static cl::SubCommandGroup subsWithCompileOptions{&subDump, &subList, &subRun,
                                                  &subTest};
static cl::SubCommandGroup allSubs{&subDump, &subList, &subRun, &subTest,
                                   &subFormat};

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
        cl::OptionEnumValue{"llvm-ir", int(smdl::DUMP_FORMAT_IR), "LLVM-IR"},
        cl::OptionEnumValue{"asm", int(smdl::DUMP_FORMAT_ASM),
                            "Native assembly"},
        cl::OptionEnumValue{"obj", int(smdl::DUMP_FORMAT_OBJ),
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

static cl::OptionCategory catState{"State Options"};
static cl::opt<unsigned> wavelengthBaseMax{
    "wavelength_base_max", cl::desc("Number of wavelengths (default 16)"),
    cl::init(16U), cl::sub(subsWithCompileOptions), cl::cat(catState)};
static cl::opt<float> minWavelen{
    "wavelength_min",
    cl::desc("Wavelength minimum in nanometers (default 380)"),
    cl::init(380.0f), cl::sub(subTest), cl::cat(catState)};
static cl::opt<float> maxWavelen{
    "wavelength_max",
    cl::desc("Wavelength maximum in nanometers (default 720)"),
    cl::init(720.0f), cl::sub(subTest), cl::cat(catState)};
static cl::opt<float> animationTime{
    "animation_time", cl::desc("Animation time (default 0)"), cl::init(0.0f),
    cl::sub(subTest), cl::cat(catState)};
static cl::opt<int> objectID{"object_id", cl::desc("Object ID (default 0)"),
                             cl::init(0), cl::sub(subTest), cl::cat(catState)};
static cl::opt<float> texCoordU{
    "texcoord_u", cl::desc("Texture coordinate U (default 0)"), cl::init(0.0f),
    cl::sub(subTest), cl::cat(catState)};
static cl::opt<float> texCoordV{
    "texcoord_v", cl::desc("Texture coordinate V (default 0)"), cl::init(0.0f),
    cl::sub(subTest), cl::cat(catState)};
static cl::opt<float> texCoordW{
    "texcoord_w", cl::desc("Texture coordinate W (default 0)"), cl::init(0.0f),
    cl::sub(subTest), cl::cat(catState)};
static cl::opt<int> ptexFaceID{
    "ptex_face_id", cl::desc("Ptex face ID (default 0)"), cl::init(0),
    cl::sub(subTest), cl::cat(catState)};
static cl::opt<float> ptexFaceU{
    "ptex_face_u", cl::desc("Ptex face coordinate U (default 0)"),
    cl::init(0.0f), cl::sub(subTest), cl::cat(catState)};
static cl::opt<float> ptexFaceV{
    "ptex_face_v", cl::desc("Ptex face coordinate V (default 0)"),
    cl::init(0.0f), cl::sub(subTest), cl::cat(catState)};

int main(int argc, char **argv) {
  llvm::InitLLVM X(argc, argv);
  smdl::Logger::get().add_sink<smdl::LogSinks::print_to_cerr>();
  cl::HideUnrelatedOptions({&optionsCat});
  cl::ParseCommandLineOptions(argc, argv, "SpectralMDL compiler");

  auto compiler{smdl::Compiler{}};
  compiler.enableDebug = enableDebug;
  compiler.enableUnitTests = true;
  compiler.wavelengthBaseMax = wavelengthBaseMax;
  for (const auto &inputFile : inputFiles) {
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
    } else if (subList) {
      std::cout << compiler.summarize_materials();
      std::cout.flush();
    } else if (subRun || subTest) {
      if (auto error{compiler.jit_compile()})
        error->print_and_exit();
      if (auto error{compiler.jit_execs()})
        error->print_and_exit();
      if (subTest) {
        std::array<float, 16> wavelengths{};
        smdl::BumpPtrAllocator allocator{};
        smdl::State state{};
        state.allocator = &allocator;
        state.texture_coordinate[0][0] = texCoordU;
        state.texture_coordinate[0][1] = texCoordV;
        state.texture_coordinate[0][2] = texCoordW;
        state.animation_time = animationTime;
        state.object_id = objectID;
        state.ptex_face_id = ptexFaceID;
        state.ptex_face_uv[0] = ptexFaceU;
        state.ptex_face_uv[1] = ptexFaceV;
        state.wavelength_min = minWavelen;
        state.wavelength_max = maxWavelen;
        state.wavelength_base = &wavelengths[0];
        for (unsigned i = 0; i < compiler.wavelengthBaseMax; i++) {
          float fac = float(i) / float(compiler.wavelengthBaseMax - 1);
          wavelengths[i] =
              (1 - fac) * state.wavelength_min + fac * state.wavelength_max;
        }
        if (auto error{compiler.jit_unit_tests(state)}) {
          std::cerr << '\n';
          error->print_and_exit();
        }
      }
    }
  }
  return 0;
}
