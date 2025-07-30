#include "llvm/Support/InitLLVM.h"

#include "MLTIntegrator.h"
#include "Scene.h"
#include "command_line.h"
#include "common.h"

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

  const smdl::int2 imageExtent{cameraImageExtent};
  const Camera camera{smdl::look_at(cameraFrom, cameraTo, cameraUp),
                      imageExtent, float(cameraFov) * PI / 180.0f};
  std::cerr << "Focal plane area = "
            << camera.focalLen * camera.focalLen / camera.imageAspect
            << std::endl;
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
  Color wavelengthBase{};
  for (size_t i = 0; i < WAVELENGTH_BASE_MAX; i++) {
    float t = i / float(WAVELENGTH_BASE_MAX - 1);
    wavelengthBase[i] = (1 - t) * WAVELENGTH_MIN + t * WAVELENGTH_MAX;
  }
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
  auto renderImage{smdl::SpectralRenderImage(WAVELENGTH_BASE_MAX, imageExtent.x,
                                             imageExtent.y)};
#if 1
  auto options{MLTIntegrator::Options{}};
  options.smallStepSigma = 0.01f;
  options.minOrder = 2;
  options.maxOrder = 5;
  options.nMutationsPerPixel = size_t(samplesPerPixel);
  options.nChains = 1000;
  auto integrator{MLTIntegrator{options}};
  integrator.integrate(scene, wavelengthBase, renderImage);
#else
#if 1
  smdl::parallel_for(imageExtent.x * imageExtent.y, [&](size_t i) {
    auto allocator{smdl::BumpPtrAllocator()};
    auto rng{RNG{i}};
    auto rngf{[&rng]() { return generate_canonical(rng); }};
    auto cameraPath{std::array<Vertex, 5>{}};
    auto lightPath{std::array<Vertex, 5>{}};
    for (size_t sample = 0; sample != size_t(samplesPerPixel); ++sample) {
      auto cameraPathLen{scene.trace_path_from_camera(
          allocator, rngf, wavelengthBase,
          smdl::float2(imageExtent.x * rngf(), imageExtent.y * rngf()),
          cameraPath.size(), cameraPath.data())};
      auto lightPathLen{scene.trace_path_from_light(
          allocator, rngf, wavelengthBase, lightPath.size(), lightPath.data())};
#if 1
      for (int s = 0; s < cameraPathLen; s++) {
        for (int t = 0; t < lightPathLen; t++) {
          // Limit path length to 6 (effective max path length in
          // unidirectional light or camera mode)
          if (s + t + 2 > 5)
            continue;
          Color beta{};
          float misWeight{};
          smdl::float2 imageCoord{cameraPath[0].imageCoord};
          if (connect_bidirectional(scene, allocator, rngf, wavelengthBase,
                                    &cameraPath[s], &lightPath[t], beta,
                                    misWeight, imageCoord)) {
            renderImage
                .pixel_reference(size_t(imageCoord.x), size_t(imageCoord.y))
                .add_sample(misWeight,
                            smdl::Span<float>(beta.data(), beta.size()));
          }
        }
      }
#else
      for (int t = 1; t < lightPathLen; t++) {
        if (auto &v{lightPath[t]}; !v.isAtInfinity) {
          Vertex vLast{};
          if (Camera_last_vertex_sample(camera, smdl::float2(rngf(), rngf()), v,
                                        vLast)) {
            if (scene.test_visibility(allocator, rngf, wavelengthBase, v, vLast,
                                      vLast.beta)) {
              renderImage
                  .pixel_reference(size_t(vLast.imageCoord.x),
                             size_t(vLast.imageCoord.y))
                  .add_sample(1.0, smdl::Span<float>(vLast.beta.data(),
                                                     vLast.beta.size()));
            }
          }
        }
      }
#endif
      allocator.reset();
    }
  });
#else
  smdl::parallel_for(imageExtent.x * imageExtent.y, [&](size_t i) {
    auto allocator{smdl::BumpPtrAllocator()};
    size_t y{i / size_t(imageExtent.x)};
    size_t x{i % size_t(imageExtent.x)};
    auto rng{RNG{i}};
    auto rngf{[&rng]() { return generate_canonical(rng); }};
    Color Lsum{};
    for (size_t sample = 0; sample < size_t(samplesPerPixel); sample++) {
      auto imageCoord{smdl::float2(float(x) + rngf(), float(y) + rngf())};
      auto path{std::array<Vertex, 5>{}};
      auto pathLen{scene.trace_path_from_camera(allocator, rngf, wavelengthBase,
                                                imageCoord, path.size(),
                                                path.data())};
      for (int k = 1; k < pathLen; k++) {
        if (auto &v = path[k]; !v.isAtInfinity) {
          Vertex vLast{};
          if (Light_last_vertex_sample(scene, scene.lights[0],
                                       smdl::float2(rngf(), rngf()), v,
                                       vLast)) {
            if (scene.test_visibility(allocator, rngf, wavelengthBase, v, vLast,
                                      vLast.beta)) {
              Lsum += vLast.beta;
            }
          }
        }
      }
      allocator.reset();
    }
    renderImage.pixel_reference(x, y).add_sample(
        1.0, smdl::Span<float>(Lsum.data(), Lsum.size()));
  });
#endif
#endif

  std::ofstream ofs("output.pgm", std::ios::binary | std::ios::out);
  ofs << "P6 ";
  ofs << imageExtent.x << ' ';
  ofs << imageExtent.y << " 255\n";
  for (int y = 0; y < imageExtent.y; y++)
    for (int x = 0; x < imageExtent.x; x++) {
      auto color{Color()};
      auto pixelRef{renderImage.pixel_reference(x, y)};
      for (int k = 0; k < WAVELENGTH_BASE_MAX; k++) {
        color[k] =
            double(pixelRef.totalValues[k]) / double(size_t(samplesPerPixel));
      }
      smdl::State state{};
      state.wavelength_base = wavelengthBase.data();
      state.wavelength_min = WAVELENGTH_MIN;
      state.wavelength_max = WAVELENGTH_MAX;
      auto rgb{compiler.jit_color_to_rgb(state, color.data())};
      rgb[0] = std::pow(std::min(2 * rgb[0], 1.0f), 1.0f / 2.2f);
      rgb[1] = std::pow(std::min(2 * rgb[1], 1.0f), 1.0f / 2.2f);
      rgb[2] = std::pow(std::min(2 * rgb[2], 1.0f), 1.0f / 2.2f);
      auto pixel{std::array<uint8_t, 3>{}};
      pixel[0] = std::round(255 * rgb[0]);
      pixel[1] = std::round(255 * rgb[1]);
      pixel[2] = std::round(255 * rgb[2]);
      ofs.write(reinterpret_cast<const char *>(&pixel[0]), 3);
    }
} catch (const smdl::Error &error) {
  error.print();
  return EXIT_FAILURE;
} catch (const std::exception &error) {
  std::cerr << error.what() << '\n';
  return EXIT_FAILURE;
}
