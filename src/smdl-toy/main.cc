#include "llvm/Support/InitLLVM.h"

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
  Scene scene{compiler, camera, inputSceneFile};
  scene.lights.emplace_back(
      PointLight{.intensity = Color(20.0f), .origin = smdl::float3(0, 0, 5)});
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
  std::vector<std::array<uint8_t, 4>> imagePixels{};
  imagePixels.resize(imageExtent.x * imageExtent.y);
  std::cerr << camera.focalLen << std::endl;
  std::cerr << camera.imageAspect << std::endl;
#define TRACE_FROM_LIGHT 1
#if TRACE_FROM_LIGHT
  std::mutex imageColorsMutex{};
  std::vector<Color> imageColors{};
  imageColors.resize(imageExtent.x * imageExtent.y);
  size_t nPaths = imageExtent.x * imageExtent.y * size_t(samplesPerPixel);
  oneapi::tbb::parallel_for(
      oneapi::tbb::blocked_range<size_t>(0, nPaths, 128),
      [&](const auto &indexes) {
        auto allocator{smdl::BumpPtrAllocator()};
        for (size_t i = indexes.begin(); i != indexes.end(); ++i) {
          auto rng{RNG{i}};
          auto rngFunc{[&]() { return generate_canonical(rng); }};
          auto path{std::array<Vertex, 5>{}};
          auto pathLen{scene.trace_path_from_light(
              allocator, rngFunc, wavelengthBase, path.size(), path.data())};
          for (int k = 1; k < pathLen; k++) {
            Vertex vLast{};
            if (auto &v = path[k]; !v.isAtInfinity) {
              if (Camera_last_vertex_sample(camera,
                                            smdl::float2(rngFunc(), rngFunc()),
                                            &path[k - 1], path[k], vLast)) {
#if 1
                if (scene.test_visibility(allocator, rngFunc, wavelengthBase, v,
                                          vLast, vLast.beta)) {
                  int x = int(vLast.imageCoord.x);
                  int y = int(vLast.imageCoord.y);
                  if (0 <= x && x < camera.imageExtent.x && //
                      0 <= y && y < camera.imageExtent.y) {
                    std::lock_guard guard{imageColorsMutex};
                    auto &color{imageColors[y * camera.imageExtent.x + x]};
                    color += vLast.beta ;
                  }
                }
#endif
              }
            }
          }
          allocator.reset();
        }
      });
  for (size_t i = 0; i < imageColors.size(); i++) {
    imageColors[i] /= float(size_t(samplesPerPixel));
    smdl::State state{};
    state.wavelength_base = wavelengthBase.data();
    state.wavelength_min = WAVELENGTH_MIN;
    state.wavelength_max = WAVELENGTH_MAX;
    smdl::float3 rgb{compiler.jit_color_to_rgb(state, imageColors[i].data())};
    rgb[0] = std::pow(std::min(2 * rgb[0], 1.0f), 1.0f / 2.2f);
    rgb[1] = std::pow(std::min(2 * rgb[1], 1.0f), 1.0f / 2.2f);
    rgb[2] = std::pow(std::min(2 * rgb[2], 1.0f), 1.0f / 2.2f);
    auto &pixel{imagePixels[i]};
    pixel[0] = std::round(255 * rgb[0]);
    pixel[1] = std::round(255 * rgb[1]);
    pixel[2] = std::round(255 * rgb[2]);
    pixel[3] = 255;
  }
#else
  oneapi::tbb::parallel_for(
      oneapi::tbb::blocked_range<size_t>(0, imageExtent.x * imageExtent.y, 8),
      [&](const auto &indexes) {
        auto allocator{smdl::BumpPtrAllocator()};
        auto N{size_t(samplesPerPixel)};
        for (size_t i = indexes.begin(); i != indexes.end(); ++i) {
          size_t y{i / size_t(imageExtent.x)};
          size_t x{i % size_t(imageExtent.x)};
          auto rng{RNG{i}};
          auto rngFunc{[&]() { return generate_canonical(rng); }};
          Color Lsum{};
          for (size_t j = 0; j < N; j++) {
            auto imageCoord{smdl::float2(static_cast<float>(x) + rngFunc(),
                                         static_cast<float>(y) + rngFunc())};
            auto path{std::array<Vertex, 5>{}};
            auto pathLen{scene.trace_path_from_camera(
                allocator, rngFunc, wavelengthBase, imageCoord, path.size(),
                path.data())};
            for (int k = 1; k < pathLen; k++) {
              auto preserve{
                  smdl::Preserve(path[k].pdfAdjoint, path[k - 1].pdfAdjoint)};
              if (auto &v = path[k]; !v.isAtInfinity) {
                Vertex vLast{};
                if (Light_last_vertex_sample(scene, scene.lights[0],
                                             smdl::float2(rngFunc(), rngFunc()),
                                             smdl::float2(rngFunc(), rngFunc()),
                                             &path[k - 1], v, vLast)) {
                  if (scene.test_visibility(allocator, rngFunc, wavelengthBase,
                                            v, vLast, vLast.beta)) {
                    Lsum += vLast.beta;
                  }
                }
              }
            }
            allocator.reset();
          }
          Lsum /= float(N);
          smdl::State state{};
          state.wavelength_base = wavelengthBase.data();
          state.wavelength_min = WAVELENGTH_MIN;
          state.wavelength_max = WAVELENGTH_MAX;
          smdl::float3 rgb{compiler.jit_color_to_rgb(state, Lsum.data())};
          rgb[0] = std::pow(std::min(2 * rgb[0], 1.0f), 1.0f / 2.2f);
          rgb[1] = std::pow(std::min(2 * rgb[1], 1.0f), 1.0f / 2.2f);
          rgb[2] = std::pow(std::min(2 * rgb[2], 1.0f), 1.0f / 2.2f);
          auto &pixel{imagePixels[i]};
          pixel[0] = std::round(255 * rgb[0]);
          pixel[1] = std::round(255 * rgb[1]);
          pixel[2] = std::round(255 * rgb[2]);
          pixel[3] = 255;
        }
      });
#endif
  std::ofstream ofs("output.pgm", std::ios::binary | std::ios::out);
  ofs << "P6 ";
  ofs << imageExtent.x << ' ';
  ofs << imageExtent.y << " 255\n";
  for (auto &imagePixel : imagePixels)
    ofs.write(reinterpret_cast<const char *>(&imagePixel[0]), 3);
} catch (const smdl::Error &error) {
  error.print();
  return EXIT_FAILURE;
} catch (const std::exception &error) {
  std::cerr << error.what() << '\n';
  return EXIT_FAILURE;
}
