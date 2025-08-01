#include "llvm/Support/InitLLVM.h"

#include "MLTIntegrator.h"
#include "PathIntegrator.h"
#include "BDPTIntegrator.h"
#include "Scene.h"
#include "command_line.h"
#include "common.h"

#include "smdl/Support/Profiler.h"

#include <fstream>
#include <iostream>

static cl::opt<std::string> inputSceneFile{
    cl::Positional, cl::desc("<input scene>"), cl::Required};
static cl::list<std::string> inputMDLFiles{
    cl::Positional, cl::desc("<input mdl>"), cl::OneOrMore};

static cl::OptionCategory catCamera{"Camera Options"};
static cl::opt<smdl::int2> cameraImageExtent{
    "dims", cl::desc("The image dimensions in pixels (default: 1280,720)"),
    cl::init(smdl::int2{1280, 720}), cl::cat(catCamera)};
static cl::opt<smdl::float3> cameraFrom{
    "look-from", cl::desc("The position to look from (default: -6,0,2)"),
    cl::init(smdl::float3{-6, 0, 2}), cl::cat(catCamera)};
static cl::opt<smdl::float3> cameraTo{
    "look-to", cl::desc("The position to look to (default: 0,0,0.5)"),
    cl::init(smdl::float3{0, 0, 0.5}), cl::cat(catCamera)};
static cl::opt<smdl::float3> cameraUp{
    "up", cl::desc("The up vector (default: 0,0,1)"),
    cl::init(smdl::float3{0, 0, 1}), cl::cat(catCamera)};
static cl::opt<float> cameraFov{"fov",
                                cl::desc("The FOV in degrees (default: 60)"),
                                cl::init(60.0f), cl::cat(catCamera)};
static cl::opt<unsigned> samplesPerPixel{
    "spp", cl::desc("The number of samples per pixel (default: 8)"),
    cl::init(8U), cl::cat(catCamera)};

static cl::OptionCategory catIBL{"Image-Based Light (IBL) Options"};
static cl::opt<std::string> imageLightFile{"ibl", cl::desc("The IBL filename"),
                                           cl::cat(catIBL)};
static cl::opt<float> imageLightScale{"ibl-scale",
                                      cl::desc("The IBL scale factor"),
                                      cl::init(1.0f), cl::cat(catIBL)};

int main(int argc, char **argv) try {
  llvm::InitLLVM X(argc, argv);
  smdl::Logger::get().add_sink<smdl::LogSinks::print_to_cerr>();
  smdl::profiler_initialize();
  SMDL_DEFER([]() { smdl::profiler_finalize(); });
  cl::HideUnrelatedOptions({&catCamera});
  cl::ParseCommandLineOptions(argc, argv, "SpectralMDL toy renderer");

  smdl::Compiler compiler{};
  compiler.wavelengthBaseMax = WAVELENGTH_BASE_MAX;
  compiler.enableDebug = false;
  compiler.enableUnitTests = false;
  for (auto &inputMDLFile : inputMDLFiles)
    if (auto error{compiler.add(std::string(inputMDLFile))})
      error->print_and_exit();
  if (auto error{compiler.compile(smdl::OptLevel::O2)})
    error->print_and_exit();
  if (auto error{compiler.jit_compile()})
    error->print_and_exit();

  const Camera camera{smdl::look_at(cameraFrom, cameraTo, cameraUp),
                      smdl::int2(cameraImageExtent),
                      float(cameraFov) * PI / 180.0f};
  Scene scene{compiler, camera, inputSceneFile};
  scene.lights.emplace_back(
#if 0
      SpotLight{.intensity = Color(20.0f),
                .origin = smdl::float3(0, 0, 5),
                .direction = smdl::float3(0, 0, -1),
                .cosThetaInner = 0.866f,
                .cosThetaOuter = 0.707f}
#else
      DiskLight{.intensity = Color(20.0f),
                .origin = smdl::float3(1, 2, 3),
                .direction = smdl::normalize(smdl::float3(-1, -2, -3)),
                .radius = 0.5f}
#endif
  );
  scene.initialize_light_distribution();
#if 0
  if (imageLightFile.getNumOccurrences() > 0) {
    scene.imageLight = std::make_unique<smdl::Image>();
    if (auto error{scene.imageLight->start_load(std::string(imageLightFile))}) {
      error->print_and_exit();
    }
    scene.imageLight->finish_load();
    scene.imageLightScale = float(imageLightScale);
  }
#endif
  auto integrator{
      std::make_unique<PathIntegrator>(0x12345, samplesPerPixel, 2, 5)};
  integrator->integrate_and_write_file(scene, 2.0f, "output-path.png");
} catch (const smdl::Error &error) {
  error.print();
  return EXIT_FAILURE;
} catch (const std::exception &error) {
  std::cerr << error.what() << '\n';
  return EXIT_FAILURE;
}
