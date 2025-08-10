#include "llvm/Support/InitLLVM.h"

#include "Scene.h"
#include "command_line.h"
#include "common.h"
#include "integrators/BDPTIntegrator.h"
#include "integrators/PSMLTIntegrator.h"
#include "integrators/PTIntegrator.h"
#include "integrators/PTLightIntegrator.h"

#include "smdl/Support/Profiler.h"

#include <fstream>
#include <iostream>

static cl::opt<std::string> inputSceneFile{
    cl::Positional, cl::desc("<input scene>"), cl::Required};
static cl::list<std::string> inputMDLFiles{
    cl::Positional, cl::desc("<input mdl>"), cl::OneOrMore};

static cl::OptionCategory catCamera{"Render Options (Camera)"};
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

static cl::OptionCategory catIntegrator{"Render Options (Integrator)"};
enum IntegratorKind : int {
  INTEGRATOR_KIND_PT,
  INTEGRATOR_KIND_PT_LIGHT,
  INTEGRATOR_KIND_BDPT,
  INTEGRATOR_KIND_PSMLT,
  INTEGRATOR_KIND_PSMLT_INDIRECT
};
static cl::opt<IntegratorKind> integratorKind{
    "integrator", cl::desc("Integrator (default: pt)"),
    cl::values(
        cl::OptionEnumValue{"pt", int(INTEGRATOR_KIND_PT),
                            "Path tracing (PT) from camera"},
        cl::OptionEnumValue{"pt-light", int(INTEGRATOR_KIND_PT_LIGHT),
                            "Path tracing (PT) from lights"},
        cl::OptionEnumValue{
            "bdpt", int(INTEGRATOR_KIND_BDPT),
            "Bidirectional path tracing (BDPT) from camera and lights"},
        cl::OptionEnumValue{"psmlt", int(INTEGRATOR_KIND_PSMLT),
                            "Primary-space metropolis light transport (PSMLT)"},
        cl::OptionEnumValue{
            "psmlt-indirect", int(INTEGRATOR_KIND_PSMLT_INDIRECT),
            "PT for direct light and PSMLT for indirect light"}),
    cl::init(INTEGRATOR_KIND_PT), cl::cat(catIntegrator)};
static cl::opt<size_t> minOrder{
    "min-order", cl::desc("The minimum scattering order (default: 0)"),
    cl::init(0u), cl::cat(catIntegrator)};
static cl::opt<size_t> maxOrder{
    "max-order", cl::desc("The maximum scattering order (default: 5)"),
    cl::init(5u), cl::cat(catIntegrator)};
static cl::opt<size_t> samplesPerPixel{
    "spp", cl::desc("The number of samples per pixel (default: 8)"),
    cl::init(8U), cl::cat(catIntegrator)};
static cl::opt<size_t> seed{"seed", cl::desc("The random seed (default: 0)"),
                            cl::init(0u), cl::cat(catIntegrator)};
static cl::opt<float> imageGain{
    "image-gain", cl::desc("The final image gain multiplier (default: 1)"),
    cl::init(1.0f), cl::cat(catIntegrator)};

/*
static cl::opt<std::string> imageLightFile{"ibl", cl::desc("The IBL filename"),
                                           cl::cat(catCamera)};
static cl::opt<float> imageLightScale{"ibl-scale",
                                      cl::desc("The IBL scale factor"),
                                      cl::init(1.0f), cl::cat(catCamera)};
 */

int main(int argc, char **argv) try {
  llvm::InitLLVM X(argc, argv);
  smdl::Logger::get().add_sink<smdl::LogSinks::print_to_cerr>();
  smdl::profiler_initialize();
  SMDL_DEFER([]() { smdl::profiler_finalize(); });
  cl::HideUnrelatedOptions({&catCamera, &catIntegrator});
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
      DiskLight{.intensity = Color(40.0f),
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
  auto integrator{[&]() -> std::unique_ptr<Integrator> {
    switch (IntegratorKind(integratorKind)) {
    case INTEGRATOR_KIND_PT:
      return std::make_unique<PTIntegrator>(seed, samplesPerPixel, minOrder,
                                            maxOrder);
    case INTEGRATOR_KIND_PT_LIGHT:
      return std::make_unique<PTLightIntegrator>(seed, samplesPerPixel,
                                                 minOrder, maxOrder);
    case INTEGRATOR_KIND_BDPT:
      return std::make_unique<BDPTIntegrator>(seed, samplesPerPixel, minOrder,
                                              maxOrder);
    case INTEGRATOR_KIND_PSMLT:
      return std::make_unique<PSMLTIntegrator>(seed, samplesPerPixel, minOrder,
                                               maxOrder,
                                               /*onlyIndirectPSMLT=*/false);
    case INTEGRATOR_KIND_PSMLT_INDIRECT:
      return std::make_unique<PSMLTIntegrator>(seed, samplesPerPixel, minOrder,
                                               maxOrder,
                                               /*onlyIndirectPSMLT=*/true);
    default:
      SMDL_SANITY_CHECK(false, "Invalid integrator kind");
      break;
    }
    return nullptr;
  }()};
  integrator->integrate_and_write_file(scene, imageGain, "output.png");
} catch (const smdl::Error &error) {
  error.print();
  return EXIT_FAILURE;
} catch (const std::exception &error) {
  std::cerr << error.what() << '\n';
  return EXIT_FAILURE;
}
