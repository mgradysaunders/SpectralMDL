#include "cl.h"
#include "light.h"
#include "raytracing.h"
#include "vertex.h"

#include "smdl/Support/Logger.h"
#include "smdl/Support/MonteCarlo.h"
#include "smdl/Support/SpectralRenderImage.h"

#include "llvm/Support/Parallel.h"
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

static cl::OptionCategory catOutput{"Output Options"};
static cl::opt<std::string> outputFile{
    "output", cl::desc("The tone mapped image filename (default: output.png)"),
    cl::init(std::string("output.png")), cl::cat(catOutput)};
static cl::opt<std::string> outputFloatFile{
    "output-float",
    cl::desc("Also write the linear radiance to this '.exr' or '.hdr' file, "
             "with no exposure or gamma applied"),
    cl::cat(catOutput)};
static cl::opt<std::string> outputSpectralFile{
    "output-spectral",
    cl::desc("Also write every wavelength band to this ENVI file, alongside "
             "which a '.hdr' header is written"),
    cl::cat(catOutput)};
static cl::opt<float> imageExposure{
    "exposure",
    cl::desc("The linear exposure applied before tone mapping "
             "(default: 1)"),
    cl::init(1.0f), cl::cat(catOutput)};

static cl::opt<std::string> envLightFile{
    "ibl-filename", cl::desc("The IBL filename"), cl::cat(catCamera)};
static cl::opt<float> envLightScale{"ibl-scale",
                                    cl::desc("The IBL scale factor"),
                                    cl::init(1.0f), cl::cat(catCamera)};

int main(int argc, char **argv) try {
  llvm::InitLLVM X(argc, argv);
  smdl::Logger::get().addSink<smdl::LogSinks::print_to_cerr>();
  cl::HideUnrelatedOptions({&catCamera, &catOutput});
  cl::ParseCommandLineOptions(argc, argv, "SpectralMDL toy renderer");

  auto compiler{smdl::Compiler{}};
  compiler.wavelengthBaseMax = WAVELENGTH_BASE_MAX;
  compiler.enableDebug = false;
  compiler.enableUnitTests = false;
  for (auto &inputMDLFile : inputMDLFiles)
    if (auto error{compiler.add(std::string(inputMDLFile))})
      error->printAndExit();
  if (auto error{compiler.compile(smdl::OPT_LEVEL_O2)}) error->printAndExit();
  if (auto error{compiler.jitCompile()}) error->printAndExit();
  const auto scene{Scene(compiler, inputSceneFile)};

  std::unique_ptr<EnvLight> envLight{};
  if (envLightFile.getNumOccurrences() > 0) {
    envLight.reset(
        new EnvLight(std::string(envLightFile), float(envLightScale)));
  }

  auto wavelengths{Color()};
  for (size_t i = 0; i < WAVELENGTH_BASE_MAX; i++) {
    float t = i / float(WAVELENGTH_BASE_MAX - 1);
    wavelengths[i] = (1 - t) * WAVELENGTH_MIN + t * WAVELENGTH_MAX;
  }
  // Every light in one selection path: each emissive mesh instance plus
  // the environment, weighted by power.
  const auto lights{LightSampler(compiler, scene, envLight.get(), wavelengths)};
  const auto dims{int2(cameraDims)};
  const auto numPixelsX{size_t(dims.x)};
  const auto numPixelsY{size_t(dims.y)};
  const auto spp{size_t(samplesPerPixel)};
  const auto aspectRatio{float(numPixelsX) / float(numPixelsY)};
  const auto focalLength{0.5f / std::tan(float(cameraFOV) / 2 * PI / 180)};
  const auto cameraToWorld{smdl::lookAt(cameraFrom, cameraTo, cameraUp)};
  auto renderImage{
      smdl::SpectralRenderImage(WAVELENGTH_BASE_MAX, numPixelsX, numPixelsY)};
  // An upper bound on the path buffer only. Paths are terminated by Russian
  // roulette in `random_walk` long before this, so it is set high enough that
  // clipping it is negligible even for high-albedo transport.
  constexpr int MAX_PATH_LEN = 64;
  std::atomic<size_t> progress{};
  llvm::parallelFor(0, numPixelsX * numPixelsY, [&](size_t i) {
    {
      size_t p = ++progress;
      if (p % 100 == 0) {
        std::cerr << smdl::concat(
            "\r", p / double(numPixelsX * numPixelsY) * 100.0, "%");
      }
    }
    auto allocator{smdl::BumpPtrAllocator()};
    auto state{smdl::State{}};
    state.allocator = &allocator;
    state.wavelength_base = wavelengths.data();
    state.wavelength_min = WAVELENGTH_MIN;
    state.wavelength_max = WAVELENGTH_MAX;
    auto sampler{Sampler()};
    Vertex path[MAX_PATH_LEN]{};
    auto y{i / numPixelsX};
    auto x{i % numPixelsX};
    Color Lsum{};
    for (size_t s = 0; s < spp; s++) {
      sampler.startPixelSample(uint32_t(i), uint32_t(s));
      // The first two dimensions are the pixel jitter, so the camera rays
      // of a pixel are the best-stratified dimensions of the sequence.
      auto xi{float2(sampler)};
      float u{(x + xi.x) / float(numPixelsX)};
      float v{(y + xi.y) / float(numPixelsY)};
      Ray ray{float3(0.0f),
              float3(+(u - 0.5f) * aspectRatio, -(v - 0.5f), -focalLength), EPS,
              INF};
      ray.transform(cameraToWorld);
      ray.dir = normalize(ray.dir);
      Vertex path0{};
      path0.point = ray.org;
      path0.beta = Color(1.0f);
      path0.wNext = ray.dir;
      path0.pdfFwd = 0;
      path0.pdfRev = 0;
      size_t pathLen{random_walk(compiler, scene, sampler, wavelengths,
                                 allocator, smdl::TRANSPORT_RADIANCE, path0, 1,
                                 MAX_PATH_LEN, &path[0], &lights)};
      for (size_t depth = 1; depth < pathLen; ++depth) {
        auto &vertex{path[depth]};
        if (vertex.isInfiniteLight) {
          // The walk escaped the scene. Only the camera ray adds the
          // environment here: at deeper vertices the gather below has
          // already estimated direct lighting with its own MIS pair, so
          // adding the escape radiance again would double count.
          if (envLight && depth == 1) {
            float Lipdf{};
            Color Li{envLight->Li(compiler, state, vertex.wNext, Lipdf)};
            auto L{vertex.beta * Li / float(spp)};
            if (!L.isAnyNonFinite()) Lsum += L;
          }
          continue;
        }
        const float3 wo{normalize(path[depth - 1].point - vertex.point)};
        // A directly visible emitter. The same reasoning as the
        // environment above: emissive hits at deeper vertices are already
        // covered by the BSDF-sampling half of the gather at the previous
        // vertex.
        if (depth == 1 && !vertex.isVolume &&
            vertex.materialInstance.hasEmission()) {
          Color Le{};
          if (lights.emittedRadiance(vertex.materialInstance,
                                     vertex.meshInstanceIndex, wo, Le)) {
            auto L{vertex.beta * Le / float(spp)};
            if (!L.isAnyNonFinite()) Lsum += L;
          }
        }
        if (lights.empty()) continue;
        // Direct lighting, strategy 1: sample a light, evaluate the BSDF,
        // and test visibility against the sampled point.
        {
          LightSampler::LightSample lightSample{};
          if (lights.sample(state, sampler, vertex.point, lightSample)) {
            float fpdfFwd{};
            float fpdfRev{};
            Color f{};
            if (vertex.scatter_evaluate(wo, lightSample.wi, fpdfFwd, fpdfRev,
                                        f)) {
              if (test_visibility(scene, sampler, wavelengths, allocator,
                                  vertex.medium, vertex.point,
                                  lightSample.target, f)) {
                auto L{vertex.beta * f * lightSample.Li /
                       (lightSample.pdf * float(spp))};
                if (!L.isAnyNonFinite()) {
                  L *= powerHeuristic(lightSample.pdf, fpdfFwd);
                  Lsum += L;
                }
              }
            }
          }
        }
        // Direct lighting, strategy 2: sample the BSDF and trace to
        // whatever light the sample lands on — an emissive surface or, if
        // the ray escapes, the environment.
        {
          float3 wi{};
          float fpdfFwd{};
          float fpdfRev{};
          Color f{};
          bool isDeltaBounce{};
          if (vertex.scatter_sample(float4(sampler), wo, wi, fpdfFwd, fpdfRev,
                                    f, isDeltaBounce)) {
            Hit hit{};
            auto hitInstance{smdl::JIT::MaterialInstance()};
            Color transmittance{1.0f};
            if (trace_nearest(scene, sampler, wavelengths, allocator,
                              vertex.medium, Ray{vertex.point, wi, EPS, INF},
                              hit, hitInstance, transmittance)) {
              Color Le{};
              if (hitInstance.hasEmission() &&
                  lights.emittedRadiance(hitInstance, hit.meshInstanceIndex,
                                         -wi, Le)) {
                auto L{vertex.beta * f * transmittance * Le /
                       (fpdfFwd * float(spp))};
                if (!L.isAnyNonFinite()) {
                  // Light sampling can never generate a Dirac direction, so
                  // there is no competing strategy to weigh against.
                  L *=
                      isDeltaBounce
                          ? 1.0f
                          : powerHeuristic(
                                fpdfFwd, lights.solidAnglePDF(
                                             hit.meshInstanceIndex, hit.point,
                                             hit.geometryNormal, vertex.point));
                  Lsum += L;
                }
              }
            } else if (envLight) {
              // No pdf gate here: with MIS compensation the environment
              // sampling density is zero below the mean radiance, but the
              // radiance is not — those directions are exactly the ones
              // this strategy alone must cover, at weight 1.
              float Lipdf{};
              Color Li{envLight->Li(compiler, state, wi, Lipdf)};
              auto L{vertex.beta * f * transmittance * Li /
                     (fpdfFwd * float(spp))};
              if (!L.isAnyNonFinite()) {
                L *= isDeltaBounce
                         ? 1.0f
                         : powerHeuristic(fpdfFwd,
                                          lights.envSelectionPMF() * Lipdf);
                Lsum += L;
              }
            }
          }
        }
      }
      allocator.reset();
    }
    renderImage(x, y).add(Lsum.data());
  });
  // Resolve the spectral buffer to linear RGB once. This is the radiance the
  // renderer actually estimated, so it is what gets written to the floating
  // point file. The exposure and gamma below are display transforms applied
  // only on the way to an 8-bit file.
  auto rgbImage{std::vector<float>(numPixelsX * numPixelsY * 3)};
  for (size_t y{}; y < numPixelsY; y++) {
    for (size_t x{}; x < numPixelsX; x++) {
      auto color{Color()};
      auto pixel{renderImage(x, y)};
      for (size_t i = 0; i < WAVELENGTH_BASE_MAX; i++)
        color[i] = float(double(pixel[i]));
      smdl::State state{};
      state.wavelength_base = wavelengths.data();
      state.wavelength_min = WAVELENGTH_MIN;
      state.wavelength_max = WAVELENGTH_MAX;
      auto rgb{compiler.convertColorToRGB(state, color.data())};
      auto texel{&rgbImage[3 * (x + numPixelsX * y)]};
      texel[0] = rgb[0];
      texel[1] = rgb[1];
      texel[2] = rgb[2];
    }
  }
  if (!std::string(outputFloatFile).empty()) {
    if (auto error{smdl::writeFloatImage(std::string(outputFloatFile),
                                         numPixelsX, numPixelsY, 3,
                                         rgbImage.data())}) {
      error->print();
    }
  }
  if (!std::string(outputSpectralFile).empty()) {
    renderImage.writeENVIFile(
        smdl::Span<const float>(wavelengths.data(), WAVELENGTH_BASE_MAX),
        std::string(outputSpectralFile));
  }
  {
    auto imageScale{float(imageExposure)};
    auto ldrImage{std::vector<uint8_t>(rgbImage.size())};
    for (size_t i{}; i < rgbImage.size(); i++) {
      auto value{std::fmin(std::fmax(0.0f, imageScale * rgbImage[i]), 1.0f)};
      ldrImage[i] = uint8_t(std::round(255.0f * std::pow(value, 1.0f / 2.2f)));
    }
    if (auto error{smdl::write8bitImage(std::string(outputFile), numPixelsX,
                                        numPixelsY, 3, ldrImage.data())}) {
      error->print();
    }
  }
  return EXIT_SUCCESS;
} catch (const smdl::Error &error) {
  error.print();
  return EXIT_FAILURE;
} catch (const std::exception &error) {
  std::cerr << error.what() << '\n';
  return EXIT_FAILURE;
}
