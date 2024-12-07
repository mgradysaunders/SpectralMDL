#include "llvm/Support/CommandLine.h"
#include "llvm/Support/InitLLVM.h"
#include "smdl/MDLInstance.h"
#include <fstream>
#include <iostream>

#if WITH_MATERIALX
#include "MaterialXCore/Document.h"
#include "MaterialXFormat/Util.h"
#include "MaterialXFormat/XmlIo.h"
#include "MaterialXGenMdl/MdlShaderGenerator.h"
#include "MaterialXGenGlsl/GlslShaderGenerator.h"
#include "MaterialXGenShader/Shader.h"
#include "MaterialXGenShader/Util.h"
#endif // #if WITH_MATERIALX

namespace cl = llvm::cl;

static cl::OptionCategory cat{"Options"};
static cl::SubCommand subDump{"dump", "Dump as LLVM-IR or native assembly"};
static cl::SubCommand subTest{"test", "Execute unit tests"};
static cl::SubCommand subFormat{"format", "Format MDL source code"};
static cl::SubCommandGroup subCompileOptions{&subDump, &subTest};
//static cl::SubCommand subMtlx{"mtlx", "Convert MaterialX to MDL"};
//static cl::SubCommandGroup allSubs{&subDump, &subTest, &subMtlx};
static cl::SubCommandGroup allSubs{&subDump, &subTest, &subFormat};

static cl::list<std::string> inputFiles{cl::Positional, cl::desc("<input>"), cl::OneOrMore, cl::sub(allSubs), cl::cat(cat)};
static cl::opt<unsigned> optLevel{
    "O", cl::desc("Optimization level (default 2)"), cl::Prefix, cl::init(2U), cl::sub(subCompileOptions), cl::cat(cat)};
static cl::opt<bool> enableDebug{"g", cl::desc("Enable debugging"), cl::sub(subCompileOptions), cl::cat(cat)};
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
    "wavelength_base_max", cl::desc("Number of wavelengths (default 16)"), cl::init(16U), cl::sub(subCompileOptions),
    cl::cat(catState)};
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
#if 0
  if (subMtlx) {
#if WITH_MATERIALX
    auto shaderGen = MaterialX::MdlShaderGenerator::create();
    auto shaderGenContext = MaterialX::GenContext(shaderGen);
    auto lib = MaterialX::createDocument();
    MaterialX::loadLibraries({"libraries"}, std::string("./build/_deps/materialx-src/"), lib);
    // shaderGenContext.registerSourceCodeSearchPath(MaterialX::FilePath("deps/install/"));
    for (auto &inputFile : inputFiles) {
      auto doc{MaterialX::createDocument()};
      MaterialX::readFromXmlFile(doc, inputFile.c_str());
      doc->importLibrary(lib);
      auto elems{MaterialX::findRenderableElements(doc)};
      for (auto elem : elems) {
        if (auto node = elem->asA<MaterialX::Node>(); node && node->getType() == MaterialX::MATERIAL_TYPE_STRING) {
          // This is a material!
          auto shader{shaderGen->generate("Test", elem, shaderGenContext)};
          auto shaderSource{shader->getStage(MaterialX::Stage::PIXEL).getSourceCode()};
          std::cout << shaderSource << std::endl;
        }
      }
    }
    return EXIT_SUCCESS;
#else
    return EXIT_FAILURE;
#endif // #if WITH_MATERIALX
  }
#endif
  smdl::MDLInstance mdl{};
  try {
    mdl.enableDebug = enableDebug;
    mdl.enableUnitTests = true;
    mdl.wavelengthBaseMax = wavelengthBaseMax;
    for (auto &inputFile : inputFiles) {
      if (auto error = mdl.add(std::string(inputFile))) {
        error->print();
        std::exit(EXIT_FAILURE);
      }
    }
    if (subFormat) {
      if (auto error = mdl.format_source()) {
        error->print();
        return EXIT_FAILURE;
      }
      return EXIT_SUCCESS;
    }
    if (auto error = mdl.compile(smdl::OptLevel(unsigned(optLevel)))) {
      error->print();
      return EXIT_FAILURE;
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
