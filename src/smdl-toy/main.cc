#include "command_line.h"
#include "rt.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Parallel.h"
#include "smdl/Support/Sampling.h"
#include "smdl/Support/SpectralRenderImage.h"
#include <fstream>
#include <iostream>

static cl::opt<std::string> inputSceneFile{
    cl::Positional, cl::desc("<input scene>"), cl::Required};
static cl::list<std::string> inputMDLFiles{
    cl::Positional, cl::desc("<input mdl>"), cl::OneOrMore};

static cl::OptionCategory catCamera{"Camera Options"};
static cl::opt<int2> cameraDims{
    "dims", cl::desc("The image dimensions in pixels (default: 1280,720)"),
    cl::init(int2{1280, 720}), cl::cat(catCamera)};
static cl::opt<float3> cameraFrom{
    "look-from", cl::desc("The position to look from (default: -6,0,2)"),
    cl::init(float3{-6, 0, 2}), cl::cat(catCamera)};
static cl::opt<float3> cameraTo{
    "look-to", cl::desc("The position to look to (default: 0,0,0.5)"),
    cl::init(float3{0, 0, 0.5}), cl::cat(catCamera)};
static cl::opt<float3> cameraUp{"up",
                                cl::desc("The up vector (default: 0,0,1)"),
                                cl::init(float3{0, 0, 1}), cl::cat(catCamera)};
static cl::opt<float> cameraFOV{"fov",
                                cl::desc("The FOV in degrees (default: 60)"),
                                cl::init(60.0f), cl::cat(catCamera)};
static cl::opt<unsigned> samplesPerPixel{
    "spp", cl::desc("The number of samples per pixel (default: 8)"),
    cl::init(8U), cl::cat(catCamera)};

int main(int argc, char **argv) try {
  llvm::InitLLVM X(argc, argv);
  smdl::Logger::get().add_sink<smdl::LogSinks::print_to_cerr>();
  cl::HideUnrelatedOptions({&catCamera});
  cl::ParseCommandLineOptions(argc, argv, "SpectralMDL toy renderer");

  auto compiler{smdl::Compiler{}};
  compiler.wavelengthBaseMax = WAVELENGTH_BASE_MAX;
  compiler.enableDebug = false;
  compiler.enableUnitTests = false;
  for (auto &inputMDLFile : inputMDLFiles)
    if (auto error{compiler.add(std::string(inputMDLFile))})
      error->print_and_exit();
  if (auto error{compiler.compile(smdl::OPT_LEVEL_O2)})
    error->print_and_exit();
  if (auto error{compiler.jit_compile()})
    error->print_and_exit();
  const auto scene{Scene(compiler, inputSceneFile)};

  auto wavelengthBase{Color()};
  for (size_t i = 0; i < WAVELENGTH_BASE_MAX; i++) {
    float t = i / float(WAVELENGTH_BASE_MAX - 1);
    wavelengthBase[i] = (1 - t) * WAVELENGTH_MIN + t * WAVELENGTH_MAX;
  }
  const auto dims{int2(cameraDims)};
  const auto numPixelsX{size_t(dims.x)};
  const auto numPixelsY{size_t(dims.y)};
  const auto aspectRatio{float(numPixelsX) / float(numPixelsY)};
  const auto focalLength{0.5f / std::tan(float(cameraFOV) / 2 * PI / 180)};
  const auto cameraToWorld{smdl::look_at(cameraFrom, cameraTo, cameraUp)};
  auto renderImage{
      smdl::SpectralRenderImage(WAVELENGTH_BASE_MAX, numPixelsX, numPixelsY)};
  smdl::parallel_for(0, numPixelsX * numPixelsY, [&](size_t i) {
    auto allocator{smdl::BumpPtrAllocator()};
    auto state{smdl::State{}};
    state.allocator = &allocator;
    state.wavelength_base = wavelengthBase.data();
    state.wavelength_min = WAVELENGTH_MIN;
    state.wavelength_max = WAVELENGTH_MAX;
    auto rng{make_RNG(0x8f54190b ^ i, 0xbb7c1003 + i)};
    auto spp{size_t(samplesPerPixel)};
    auto y{i / numPixelsX};
    auto x{i % numPixelsX};
    Color Lsum{};
    for (size_t s = 0; s < spp; s++) {
      Ray ray{};
      float u{(x + smdl::generate_canonical(rng)) / float(numPixelsX)};
      float v{(y + smdl::generate_canonical(rng)) / float(numPixelsY)};
      ray.dir.x = (u - 0.5f) * aspectRatio;
      ray.dir.y = -(v - 0.5f);
      ray.dir.z = -focalLength;
      ray.dir = normalize(ray.dir);
      ray.tmin = EPS;
      ray.tmax = INF;
      ray.transform(cameraToWorld);
      Hit hit{};
      if (scene.intersect(ray, hit)) {
        hit.apply_geometry_to_state(state);
        smdl::JIT::MaterialInstance material{hit.material, state};
        float pdfFwd{};
        float pdfRev{};
        Color f{};
        auto lightDir{smdl::normalize(float3(1.0f, -1.0f, 1.0f))};
        if (material.scatter_evaluate(-ray.dir, lightDir, pdfFwd, pdfRev, f.data())) {
          Lsum += f; 
        }
      }
      allocator.reset();
    }
    Lsum /= spp;
    renderImage(x, y).add(Lsum.data());
  });
  auto imageScale{2.0f};
  auto rgbImage{std::vector<uint8_t>(numPixelsX * numPixelsY * 3)};
  auto rgbImageIndex{0};
  for (size_t y{}; y < numPixelsY; y++) {
    for (size_t x{}; x < numPixelsX; x++) {
      auto color{Color()};
      auto pixel{renderImage(x, y)};
      for (size_t i = 0; i < WAVELENGTH_BASE_MAX; i++)
        color[i] = float(double(pixel[i]));
      smdl::State state{};
      state.wavelength_base = wavelengthBase.data();
      state.wavelength_min = WAVELENGTH_MIN;
      state.wavelength_max = WAVELENGTH_MAX;
      auto rgb{compiler.jit_color_to_rgb(state, color.data())};
      rgb[0] *= imageScale;
      rgb[1] *= imageScale;
      rgb[2] *= imageScale;
      rgb[0] = std::pow(std::fmin(std::fmax(0.0f, rgb[0]), 1.0f), 1.0f / 2.2f);
      rgb[1] = std::pow(std::fmin(std::fmax(0.0f, rgb[1]), 1.0f), 1.0f / 2.2f);
      rgb[2] = std::pow(std::fmin(std::fmax(0.0f, rgb[2]), 1.0f), 1.0f / 2.2f);
      rgbImage[rgbImageIndex++] = std::round(255.0f * rgb[0]);
      rgbImage[rgbImageIndex++] = std::round(255.0f * rgb[1]);
      rgbImage[rgbImageIndex++] = std::round(255.0f * rgb[2]);
    }
  }
  if (auto error{smdl::write_8_bit_image("output.png", numPixelsX, numPixelsY,
                                         3, rgbImage.data())}) {
    error->print();
  }
  return EXIT_SUCCESS;
} catch (const smdl::Error &error) {
  error.print();
  return EXIT_FAILURE;
} catch (const std::exception &error) {
  std::cerr << error.what() << '\n';
  return EXIT_FAILURE;
}
