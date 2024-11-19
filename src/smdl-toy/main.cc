#include "llvm/Support/InitLLVM.h"
#include "smdl/MDLInstance.h"

#include "Scene.h"
#include "cli.h"

static cl::opt<std::string> inputSceneFile{cl::Positional, cl::desc("<input scene>"), cl::Required};
static cl::list<std::string> inputMDLFiles{cl::Positional, cl::desc("<input mdl>"), cl::OneOrMore};

static cl::OptionCategory catCamera{"Camera Options"};
static cl::opt<smdl::int2_t> cameraDims{
    "dims", cl::desc("The image dimensions in pixels (default: 1280,720)"), cl::init(smdl::int2_t{1280, 720}),
    cl::cat(catCamera)};
static cl::opt<smdl::float3_t> cameraFrom{
    "look-from", cl::desc("The position to look from (default: 3,4,5)"), cl::init(smdl::float3_t{3, 4, 5}), cl::cat(catCamera)};
static cl::opt<smdl::float3_t> cameraTo{
    "look-to", cl::desc("The position to look to (default: 0,0,0)"), cl::init(smdl::float3_t{0, 0, 0}), cl::cat(catCamera)};
static cl::opt<smdl::float3_t> cameraUp{
    "up", cl::desc("The up vector (default: 0,0,1)"), cl::init(smdl::float3_t{0, 0, 1}), cl::cat(catCamera)};
static cl::opt<float> cameraFov{"fov", cl::desc("The FOV in degrees (default: 60)"), cl::init(60.0f), cl::cat(catCamera)};
static cl::opt<unsigned> samplesPerPixel{"spp", cl::desc("The number of samples per pixel (default: 8)"), cl::init(8U), cl::cat(catCamera)};

int main(int argc, char **argv) try {
  llvm::InitLLVM X(argc, argv);
  smdl::init_or_exit();
  cl::HideUnrelatedOptions({&catCamera});
  cl::ParseCommandLineOptions(argc, argv, "SpectralMDL toy renderer");

  smdl::MDLInstance mdl{};
  mdl.numWavelens = NUM_WAVELENS;
  mdl.enableDebug = false;
  mdl.enableUnitTests = false;
  for (auto &inputMDLFile : inputMDLFiles)
    if (auto error{mdl.load_module(std::string(inputMDLFile))})
      throw *error;
  if (auto error{mdl.compile(smdl::OptLevel::O2)})
    throw *error;
  if (auto error{mdl.compile_jit()})
    throw *error;

  Scene scene{};
  scene.load(inputSceneFile);

  smdl::Image image{};
  image.extent = smdl::int2_t(cameraDims);
  image.texels.resize(image.extent.x * image.extent.y);
  const float aspectRatio{float(image.extent.x) / float(image.extent.y)};
  const float focalLen{0.5f / std::tan(0.5f * float(cameraFov) * 3.14159f / 180.0f)};
  const smdl::float4x4_t cameraTransform{smdl::look_at(cameraFrom, cameraTo, cameraUp)};
  oneapi::tbb::parallel_for(
      oneapi::tbb::blocked_range<size_t>(0, image.extent.x * image.extent.y, 8), [&](const auto &indexes) {
        for (size_t i = indexes.begin(); i != indexes.end(); ++i) {
          size_t N{size_t(samplesPerPixel)};
          size_t y{i / image.extent.x};
          size_t x{i % image.extent.x};
          RNG rng{i};
          smdl::float4_t Lsum{};
          for (size_t j = 0; j < N; j++) {
            Ray ray{};
            ray.tmin = 0;
            ray.tmax = 1000;
            float s = (x + std::generate_canonical<float, 32>(rng)) / float(image.extent.x);
            float t = (y + std::generate_canonical<float, 32>(rng)) / float(image.extent.y);
            ray.dir.x = (s - 0.5f) * aspectRatio;
            ray.dir.y = (t - 0.5f);
            ray.dir.z = -focalLen;
            ray.dir = smdl::normalize(ray.dir);
            ray.transform(cameraTransform);
            Hit hit{};
            if (scene.intersect(ray, hit)) {
              Lsum = Lsum +smdl::float4_t{0.5f * hit.normal.x + 0.5f, 0.5f * hit.normal.y + 0.5f, 0.5f * hit.normal.z + 0.5f, 1};
            } else {
              Lsum = Lsum +smdl::float4_t{0, 0, 0, 1};
            }
          }
          image.texels[i] = Lsum / float(N);
        }
      });
  image.flip_vertical();
  smdl::save_image("toy.png", image);
  return EXIT_SUCCESS;
} catch (const smdl::Error &error) {
  error.print();
  return EXIT_FAILURE;
}
