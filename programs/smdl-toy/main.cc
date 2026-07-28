#include "cl.h"
#include "guiding.h"
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
static cl::opt<std::string> toneMap{
    "tonemap",
    cl::desc("Tone mapping for the 8-bit output: 'linear' clamps and "
             "gamma-encodes; 'log' maps the decades below the "
             "exposure-scaled white point onto the display range, for "
             "inspecting scenes whose radiance spans several orders of "
             "magnitude (default: linear)"),
    cl::init(std::string("linear")), cl::cat(catOutput)};
static cl::opt<float> toneMapDecades{
    "tonemap-decades",
    cl::desc("With -tonemap log, how many decades below white reach "
             "black (default: 4)"),
    cl::init(4.0f), cl::cat(catOutput)};

static cl::opt<std::string> envLightFile{
    "ibl-filename", cl::desc("The IBL filename"), cl::cat(catCamera)};
static cl::opt<float> envLightScale{"ibl-scale",
                                    cl::desc("The IBL scale factor"),
                                    cl::init(1.0f), cl::cat(catCamera)};
static cl::opt<bool> enableGuiding{
    "guide",
    cl::desc("Enable SD-tree path guiding (Müller et al. 2017) with "
             "adjoint-driven Russian roulette. Renders in geometrically "
             "growing passes combined by inverse variance"),
    cl::init(false), cl::cat(catCamera)};
static cl::opt<bool> enableADRRS{
    "guide-adrrs",
    cl::desc("With -guide, drive Russian roulette by expected pixel "
             "contribution instead of throughput (default: true)"),
    cl::init(true), cl::cat(catCamera)};

/// In-place 3x3 box blur over a single-channel image.
static void boxBlur3(std::vector<float> &image, size_t numPixelsX,
                     size_t numPixelsY) {
  auto source{image};
  for (size_t y = 0; y < numPixelsY; y++) {
    for (size_t x = 0; x < numPixelsX; x++) {
      float sum{};
      int count{};
      for (int dy = -1; dy <= 1; dy++) {
        for (int dx = -1; dx <= 1; dx++) {
          int xx = int(x) + dx, yy = int(y) + dy;
          if (xx < 0 || yy < 0 || xx >= int(numPixelsX) ||
              yy >= int(numPixelsY))
            continue;
          sum += source[xx + numPixelsX * yy];
          count++;
        }
      }
      image[x + numPixelsX * y] = sum / count;
    }
  }
}

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
  auto sdtree{std::unique_ptr<STree>()};
  if (enableGuiding) {
    auto r{scene.boundRadius};
    sdtree.reset(new STree(scene.boundCenter - float3(r, r, r),
                           scene.boundCenter + float3(r, r, r)));
  }
  // The per-pixel value estimate from the passes so far (spectral mean,
  // box-blurred), which drives adjoint-driven Russian roulette. Empty
  // until the first pass finishes.
  auto imageEstimate{std::vector<float>()};
  // Inverse-variance combination of the passes (Mueller, "Practical Path
  // Guiding in Production", SIGGRAPH 2019 course): every pass's image
  // enters the output weighted by the reciprocal of its estimated
  // per-pixel variance, so the poorly guided early passes fade out
  // instead of being discarded outright.
  auto halfImageA{std::vector<float>()};
  auto halfImageB{std::vector<float>()};
  auto halfSquaresA{std::vector<float>()};
  auto halfSquaresB{std::vector<float>()};
  auto comboNumerator{std::vector<double>()};
  auto comboDenominator{std::vector<double>()};
  if (enableGuiding) {
    halfImageA.resize(numPixelsX * numPixelsY * WAVELENGTH_BASE_MAX);
    halfImageB.resize(numPixelsX * numPixelsY * WAVELENGTH_BASE_MAX);
    halfSquaresA.resize(numPixelsX * numPixelsY);
    halfSquaresB.resize(numPixelsX * numPixelsY);
    comboNumerator.assign(numPixelsX * numPixelsY * WAVELENGTH_BASE_MAX, 0.0);
    comboDenominator.assign(numPixelsX * numPixelsY, 0.0);
  }
  size_t sppDone{0};
  size_t passIndex{0};
  while (sppDone < spp) {
    // Without guiding there is a single pass of the whole budget. With
    // guiding, passes grow geometrically (1, 2, 4, ... spp) and the
    // remainder is dumped into the final pass, so it always holds at
    // least half the budget.
    size_t thisPass{
        enableGuiding ? std::min(size_t(1) << passIndex, spp - sppDone) : spp};
    if (enableGuiding && (spp - sppDone) < 2 * thisPass)
      thisPass = spp - sppDone;
    const bool isFinal{sppDone + thisPass == spp};
    // Pre-final passes train the SD-tree; every pass contributes to the
    // output through the inverse-variance combination below.
    const bool recordPass{enableGuiding && !isFinal};
    const float invPass{1.0f / float(thisPass)};
    renderImage.resize(WAVELENGTH_BASE_MAX, numPixelsX, numPixelsY);
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
      Color direct[MAX_PATH_LEN]{};
      auto y{i / numPixelsX};
      auto x{i % numPixelsX};
      Color Lsum{};
      Color halfA{};
      Color halfB{};
      float squaresA{};
      float squaresB{};
      Guiding guiding{};
      guiding.tree = sdtree.get();
      guiding.pixelEstimate =
          !enableADRRS || imageEstimate.empty() ? 0.0f : imageEstimate[i];
      for (size_t s = 0; s < thisPass; s++) {
        sampler.startPixelSample(uint32_t(i), uint32_t(sppDone + s));
        Color Lsample{};
        // The first two dimensions are the pixel jitter, so the camera rays
        // of a pixel are the best-stratified dimensions of the sequence.
        auto xi{float2(sampler)};
        float u{(x + xi.x) / float(numPixelsX)};
        float v{(y + xi.y) / float(numPixelsY)};
        Ray ray{float3(0.0f),
                float3(+(u - 0.5f) * aspectRatio, -(v - 0.5f), -focalLength),
                EPS, INF};
        ray.transform(cameraToWorld);
        ray.dir = normalize(ray.dir);
        Vertex path0{};
        path0.point = ray.org;
        path0.beta = Color(1.0f);
        path0.wNext = ray.dir;
        path0.pdfFwd = 0;
        path0.pdfRev = 0;
        size_t pathLen{random_walk(compiler, scene, sampler, wavelengths,
                                   allocator, smdl::TRANSPORT_RADIANCE, path0,
                                   1, MAX_PATH_LEN, &path[0], &lights,
                                   &guiding)};
        for (size_t depth = 0; depth < pathLen; ++depth)
          direct[depth] = Color();
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
              auto L{vertex.beta * Li};
              if (!L.isAnyNonFinite()) Lsample += L;
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
              auto L{vertex.beta * Le};
              if (!L.isAnyNonFinite()) Lsample += L;
            }
          }
          if (lights.empty()) continue;
          // Direct lighting, strategy 1: sample a light, evaluate the BSDF,
          // and test visibility against the sampled point. The estimates
          // accumulate into `direct` without the path throughput so the
          // guiding pass below can reuse them as radiance estimates.
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
                  auto D{f * lightSample.Li / lightSample.pdf};
                  if (!D.isAnyNonFinite()) {
                    D *= powerHeuristic(lightSample.pdf, fpdfFwd);
                    direct[depth] += D;
                    Lsample += vertex.beta * D;
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
            if (vertex.scatter_sample(float4(sampler), wo, wi, fpdfFwd,
                                      fpdfRev, f, isDeltaBounce)) {
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
                  auto D{f * transmittance * Le / fpdfFwd};
                  if (!D.isAnyNonFinite()) {
                    // Light sampling can never generate a Dirac direction, so
                    // there is no competing strategy to weigh against.
                    D *= isDeltaBounce
                             ? 1.0f
                             : powerHeuristic(
                                   fpdfFwd,
                                   lights.solidAnglePDF(
                                       hit.meshInstanceIndex, hit.point,
                                       hit.geometryNormal, vertex.point));
                    direct[depth] += D;
                    Lsample += vertex.beta * D;
                  }
                }
              } else if (envLight) {
                // No pdf gate here: with MIS compensation the environment
                // sampling density is zero below the mean radiance, but the
                // radiance is not — those directions are exactly the ones
                // this strategy alone must cover, at weight 1.
                float Lipdf{};
                Color Li{envLight->Li(compiler, state, wi, Lipdf)};
                auto D{f * transmittance * Li / fpdfFwd};
                if (!D.isAnyNonFinite()) {
                  D *= isDeltaBounce
                           ? 1.0f
                           : powerHeuristic(fpdfFwd,
                                            lights.envSelectionPMF() * Lipdf);
                  direct[depth] += D;
                  Lsample += vertex.beta * D;
                }
              }
            }
          }
        }
        // Train the SD-tree: walk the path backward, reconstructing the
        // radiance estimate along every sampled continuation direction,
        // and splat it in. `R` carries the reflected-radiance estimate
        // leaving the next vertex, and is deliberately also the RECORDED
        // target: in this integrator every vertex gathers its own direct
        // light with NEE-MIS and walk-hit emission adds nothing past
        // depth 1, so the continuation's only job is to harvest reflected
        // light. Training on total incident radiance would aim
        // continuations at emitters, where they die without contributing.
        if (recordPass && pathLen > 1) {
          Color R{};
          for (size_t depth = pathLen; depth-- > 1;) {
            auto &vertex{path[depth]};
            if (vertex.isInfiniteLight) {
              R = Color();
              continue;
            }
            if (depth + 1 < pathLen && !path[depth + 1].isInfiniteLight) {
              auto &next{path[depth + 1]};
              if (!vertex.isDeltaBounce && vertex.pdfWNext > 0) {
                float value{R.average() / vertex.pdfWNext};
                if (std::isfinite(value) && value > 0)
                  sdtree->record(sampler, vertex.point, vertex.wNext, value);
              }
              // R(d) = D_d + w_d * R(d+1), where the bounce weight w_d is
              // recovered from the stored throughputs.
              Color w{};
              for (size_t b = 0; b < WAVELENGTH_BASE_MAX; b++)
                w[b] =
                    vertex.beta[b] > 0 ? next.beta[b] / vertex.beta[b] : 0.0f;
              R = direct[depth] + w * R;
            } else {
              R = direct[depth];
            }
            if (R.isAnyNonFinite()) R = Color();
          }
        }
        Lsum += Lsample * invPass;
        if (enableGuiding) {
          // Split the samples into two half images so the combination can
          // cross-weight each half by the other's variance estimate.
          float value{Lsample.average()};
          if (s % 2 == 0) {
            halfA += Lsample;
            squaresA += value * value;
          } else {
            halfB += Lsample;
            squaresB += value * value;
          }
        }
        allocator.reset();
      }
      if (enableGuiding) {
        for (size_t b = 0; b < WAVELENGTH_BASE_MAX; b++) {
          halfImageA[i * WAVELENGTH_BASE_MAX + b] = halfA[b];
          halfImageB[i * WAVELENGTH_BASE_MAX + b] = halfB[b];
        }
        halfSquaresA[i] = squaresA;
        halfSquaresB[i] = squaresB;
      }
      renderImage(x, y).add(Lsum.data());
    });
    if (enableGuiding) {
      // Fold this pass into the inverse-variance combination. Each half
      // image is weighted by the per-sample variance estimated from the
      // OTHER half, so the weights are statistically independent of the
      // data they weight and the combination stays exactly unbiased no
      // matter how noisy the variance estimates are. (Weighting an image
      // by its own variance estimate measurably biases the result dark:
      // a half that happened to sample low also looks low-variance.)
      // Passes too small to split (1 and 2 spp) contribute nothing.
      const size_t numPixels{numPixelsX * numPixelsY};
      const size_t countA{(thisPass + 1) / 2};
      const size_t countB{thisPass / 2};
      if (countB >= 2) {
        double meanSum{};
        double varianceSumA{};
        double varianceSumB{};
        for (size_t p = 0; p < numPixels; p++) {
          double meanA{};
          double meanB{};
          for (size_t b = 0; b < WAVELENGTH_BASE_MAX; b++) {
            meanA += double(halfImageA[p * WAVELENGTH_BASE_MAX + b]);
            meanB += double(halfImageB[p * WAVELENGTH_BASE_MAX + b]);
          }
          meanA /= double(WAVELENGTH_BASE_MAX * countA);
          meanB /= double(WAVELENGTH_BASE_MAX * countB);
          meanSum += (meanA * countA + meanB * countB) / thisPass;
          // Per-sample variance within each half.
          double ex2A{double(halfSquaresA[p]) / countA};
          double ex2B{double(halfSquaresB[p]) / countB};
          varianceSumA += std::max(0.0, ex2A - meanA * meanA) *
                          (double(countA) / double(countA - 1));
          varianceSumB += std::max(0.0, ex2B - meanB * meanB) *
                          (double(countB) / double(countB - 1));
        }
        double imageMean{meanSum / double(numPixels)};
        double epsilon{(0.01 * imageMean) * (0.01 * imageMean) + 1e-20};
        // Inverse-variance weight per kept sample, cross-applied.
        double weightA{double(countA) /
                       (varianceSumB / double(numPixels) + epsilon)};
        double weightB{double(countB) /
                       (varianceSumA / double(numPixels) + epsilon)};
        for (size_t p = 0; p < numPixels; p++) {
          comboDenominator[p] += weightA + weightB;
          for (size_t b = 0; b < WAVELENGTH_BASE_MAX; b++) {
            comboNumerator[p * WAVELENGTH_BASE_MAX + b] +=
                weightA * double(halfImageA[p * WAVELENGTH_BASE_MAX + b]) /
                    double(countA) +
                weightB * double(halfImageB[p * WAVELENGTH_BASE_MAX + b]) /
                    double(countB);
          }
        }
      }
    }
    if (recordPass) {
      // The next pass's ADRRS pixel estimates: the spectral mean of the
      // combined image so far, box-blurred so single-pixel noise does
      // not drive the roulette.
      imageEstimate.assign(numPixelsX * numPixelsY, 0.0f);
      for (size_t p = 0; p < numPixelsX * numPixelsY; p++) {
        if (comboDenominator[p] > 0) {
          double sum{};
          for (size_t b = 0; b < WAVELENGTH_BASE_MAX; b++)
            sum += comboNumerator[p * WAVELENGTH_BASE_MAX + b];
          imageEstimate[p] =
              float(sum / (WAVELENGTH_BASE_MAX * comboDenominator[p]));
        }
      }
      boxBlur3(imageEstimate, numPixelsX, numPixelsY);
      // Refine: split spatial leaves past c*sqrt(2^k) records (c = 12000,
      // k this pass's index), rebuild the directional quadtrees with the
      // 1% flux threshold.
      sdtree->refine(uint32_t(12000.0 * std::sqrt(double(thisPass))), 0.01f,
                     20);
      std::cerr << smdl::concat("\rguide pass done: ", thisPass, " spp, ",
                                sdtree->leafCount(), " spatial leaves\n");
    }
    sppDone += thisPass;
    passIndex++;
  }
  if (enableGuiding) {
    // Resolve the inverse-variance combination back into the image every
    // downstream output reads from.
    renderImage.resize(WAVELENGTH_BASE_MAX, numPixelsX, numPixelsY);
    for (size_t p = 0; p < numPixelsX * numPixelsY; p++) {
      if (!(comboDenominator[p] > 0)) continue;
      auto combined{Color()};
      for (size_t b = 0; b < WAVELENGTH_BASE_MAX; b++)
        combined[b] = float(comboNumerator[p * WAVELENGTH_BASE_MAX + b] /
                            comboDenominator[p]);
      renderImage(p % numPixelsX, p / numPixelsX).add(combined.data());
    }
  }
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
    if (std::string(toneMap) == "log") {
      // Luminance-based log mapping: white is where the exposure-scaled
      // luminance reaches 1, and `-tonemap-decades` below that reaches
      // black. Channels scale with the luminance so hue is preserved. The
      // log curve is itself a display transform, so no extra gamma.
      const float decades{std::max(0.1f, float(toneMapDecades))};
      for (size_t p = 0; p < rgbImage.size() / 3; p++) {
        float r{std::fmax(0.0f, imageScale * rgbImage[3 * p + 0])};
        float g{std::fmax(0.0f, imageScale * rgbImage[3 * p + 1])};
        float b{std::fmax(0.0f, imageScale * rgbImage[3 * p + 2])};
        float lum{(r + g + b) / 3.0f};
        float scale{};
        if (lum > 0) {
          float display{1.0f + std::log10(lum) / decades};
          display = std::fmin(std::fmax(display, 0.0f), 1.0f);
          scale = display / lum;
        }
        ldrImage[3 * p + 0] =
            uint8_t(std::round(255.0f * std::fmin(r * scale, 1.0f)));
        ldrImage[3 * p + 1] =
            uint8_t(std::round(255.0f * std::fmin(g * scale, 1.0f)));
        ldrImage[3 * p + 2] =
            uint8_t(std::round(255.0f * std::fmin(b * scale, 1.0f)));
      }
    } else if (std::string(toneMap) == "linear") {
      for (size_t i{}; i < rgbImage.size(); i++) {
        auto value{std::fmin(std::fmax(0.0f, imageScale * rgbImage[i]), 1.0f)};
        ldrImage[i] =
            uint8_t(std::round(255.0f * std::pow(value, 1.0f / 2.2f)));
      }
    } else {
      throw smdl::Error(smdl::concat("unknown -tonemap mode ",
                                     std::string(toneMap),
                                     " (expected 'linear' or 'log')"));
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
