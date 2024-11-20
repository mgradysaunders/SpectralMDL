#include "llvm/Support/InitLLVM.h"

#include "Scene.h"
#include "cli.h"

static cl::opt<std::string> inputSceneFile{cl::Positional, cl::desc("<input scene>"), cl::Required};
static cl::list<std::string> inputMDLFiles{cl::Positional, cl::desc("<input mdl>"), cl::OneOrMore};

static cl::OptionCategory catCamera{"Camera Options"};
static cl::opt<smdl::int2_t> cameraDims{
    "dims", cl::desc("The image dimensions in pixels (default: 1280,720)"), cl::init(smdl::int2_t{1280, 720}),
    cl::cat(catCamera)};
static cl::opt<smdl::float3_t> cameraFrom{
    "look-from", cl::desc("The position to look from (default: -6,0,2)"), cl::init(smdl::float3_t{-6, 0, 2}),
    cl::cat(catCamera)};
static cl::opt<smdl::float3_t> cameraTo{
    "look-to", cl::desc("The position to look to (default: 0,0,0.5)"), cl::init(smdl::float3_t{0, 0, 0.5}), cl::cat(catCamera)};
static cl::opt<smdl::float3_t> cameraUp{
    "up", cl::desc("The up vector (default: 0,0,1)"), cl::init(smdl::float3_t{0, 0, 1}), cl::cat(catCamera)};
static cl::opt<float> cameraFov{"fov", cl::desc("The FOV in degrees (default: 60)"), cl::init(60.0f), cl::cat(catCamera)};
static cl::opt<unsigned> samplesPerPixel{
    "spp", cl::desc("The number of samples per pixel (default: 8)"), cl::init(8U), cl::cat(catCamera)};

int main(int argc, char **argv) try {
  llvm::InitLLVM X(argc, argv);
  smdl::init_or_exit();
  cl::HideUnrelatedOptions({&catCamera});
  cl::ParseCommandLineOptions(argc, argv, "SpectralMDL toy renderer");

  smdl::MDLInstance mdl{};
  mdl.wavelengthBaseMax = WAVELENGTH_BASE_MAX;
  mdl.enableDebug = false;
  mdl.enableUnitTests = false;
  for (auto &inputMDLFile : inputMDLFiles) {
    if (auto error{mdl.load_module(std::string(inputMDLFile))}) {
      error->print();
      std::exit(EXIT_FAILURE);
    }
  }
  if (auto error{mdl.compile(smdl::OptLevel::O2)}) {
    error->print();
    std::exit(EXIT_FAILURE);
  }
  if (auto error{mdl.compile_jit()}) {
    error->print();
    std::exit(EXIT_FAILURE);
  }

  Scene scene{};
  scene.load(inputSceneFile);

  std::vector<const smdl::jit::Material *> materials{};
  for (auto &materialName : scene.materialNames)
    materials.push_back(mdl.find_material_jit(materialName));

  Color wavelengthBase{};
  for (size_t i = 0; i < WAVELENGTH_BASE_MAX; i++) {
    float t = i / float(WAVELENGTH_BASE_MAX - 1);
    wavelengthBase[i] = (1 - t) * WAVELENGTH_MIN + t * WAVELENGTH_MAX;
  }

  smdl::Image image{};
  image.extent = smdl::int2_t(cameraDims);
  image.texels.resize(image.extent.x * image.extent.y);
  const float aspectRatio{float(image.extent.x) / float(image.extent.y)};
  const float focalLen{0.5f / std::tan(0.5f * float(cameraFov) * 3.14159f / 180.0f)};
  const smdl::float4x4_t cameraTransform{smdl::look_at(cameraFrom, cameraTo, cameraUp)};
  oneapi::tbb::parallel_for(
      oneapi::tbb::blocked_range<size_t>(0, image.extent.x * image.extent.y, 8), [&](const auto &indexes) {
        size_t N{size_t(samplesPerPixel)};
        for (size_t i = indexes.begin(); i != indexes.end(); ++i) {
          size_t y{i / image.extent.x};
          size_t x{i % image.extent.x};
          RNG rng{i};
          alignas(64) Color Lsum{};
          for (size_t j = 0; j < N; j++) {
            Ray ray{};
            ray.tmin = 0;
            ray.tmax = std::numeric_limits<float>::infinity();
            float s = (x + std::generate_canonical<float, 32>(rng)) / float(image.extent.x);
            float t = (y + std::generate_canonical<float, 32>(rng)) / float(image.extent.y);
            ray.dir.x = (s - 0.5f) * aspectRatio;
            ray.dir.y = (t - 0.5f);
            ray.dir.z = -focalLen;
            ray.dir = smdl::normalize(ray.dir);
            ray.transform(cameraTransform);
            auto L = scene.trace_path(wavelengthBase, materials, rng, ray);
            for (size_t k = 0; k < WAVELENGTH_BASE_MAX; k++)
              Lsum[k] += L[k] / float(N);
          }
          smdl::state_t state{};
          state.wavelength_base = wavelengthBase.data();
          state.wavelength_min = WAVELENGTH_MIN;
          state.wavelength_max = WAVELENGTH_MAX;
          smdl::float3_t rgb{mdl.color_to_rgb(state, Lsum)};
          image.texels[i] = {10 * rgb.x, 10 * rgb.y, 10 * rgb.z, 1.0f};
          image.texels[i].x = std::pow(smdl::saturate(image.texels[i].x), 1.0f / 2.2f);
          image.texels[i].y = std::pow(smdl::saturate(image.texels[i].y), 1.0f / 2.2f);
          image.texels[i].z = std::pow(smdl::saturate(image.texels[i].z), 1.0f / 2.2f);
        }
      });
  image.flip_vertical();
  smdl::save_image("toy.png", image);
  return EXIT_SUCCESS;
} catch (const smdl::Error &error) {
  error.print();
  return EXIT_FAILURE;
}
