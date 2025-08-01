#include "MLTIntegrator.h"

#include "smdl/Support/Profiler.h"

#include <numeric>

void MLTIntegrator::Sampler::next_iteration() noexcept {
  iteration++;
  isLargeStep = generate_canonical(rng) < largeStepProbability;
  sampleCount = sequenceCount = 0;
}

void MLTIntegrator::Sampler::next_sequence() {
  sampleCount = 0;
  sequenceCount++;
  if (sequences.size() < sequenceCount) {
    sequences.resize(sequenceCount);
    sequences.back().reserve(32);
  }
}

/// The inverse of the error function.
[[nodiscard]] inline float erf_inverse(float y) {
  float w = -std::log(
      std::max(std::numeric_limits<float>::denorm_min(), (1 - y) * (1 + y)));
  float x = 0;
  using Float = float;
  if (w < Float(5)) {
    w = w - Float(2.5);
    x = w * Float(+2.81022636e-08) + Float(+3.43273939e-7);
    x = w * x + Float(-3.52338770e-6);
    x = w * x + Float(-4.39150654e-6);
    x = w * x + Float(+2.18580870e-4);
    x = w * x + Float(-1.25372503e-3);
    x = w * x + Float(-4.17768164e-3);
    x = w * x + Float(+2.46640727e-1);
    x = w * x + Float(+1.50140941);
  } else {
    w = std::sqrt(w) - 3;
    x = x * Float(-2.00214257e-4) + Float(+1.00950558e-4);
    x = w * x + Float(+1.34934322e-3);
    x = w * x + Float(-3.67342844e-3);
    x = w * x + Float(+5.73950773e-3);
    x = w * x + Float(-7.62246130e-3);
    x = w * x + Float(+9.43887047e-3);
    x = w * x + Float(+1.00167406);
    x = w * x + Float(+2.83297682);
  }
  x *= y;
  return x;
}

float MLTIntegrator::Sampler::next_sample() {
  SMDL_SANITY_CHECK(sequenceCount > 0);
  auto &sequence{sequences[sequenceCount - 1]};
  sampleCount++;
  if (sequence.size() < sampleCount)
    sequence.resize(sampleCount);
  auto &sample{sequence[sampleCount - 1]};

  // If large-step happened since the last time this sample was accessed,
  // then fast forward by overwriting the sample value with a canonical
  // random number.
  if (sample.iteration < iterationOfLastLargeStep) {
    sample.iteration = iterationOfLastLargeStep;
    sample.value = generate_canonical(rng);
  }

  // Save.
  sample.backup();
  if (isLargeStep) {
    sample.value = generate_canonical(rng);
  } else {
    sample.value += smallStepSigma *
                    std::sqrt(2.0f * (iteration - sample.iteration)) *
                    erf_inverse(2.0f * generate_canonical(rng) - 1.0f);
    if (!std::isfinite(sample.value)) {
      sample.value = generate_canonical(rng);
    }
  }
  sample.value -= std::floor(sample.value);
  sample.value =
      std::fmax(sample.value, std::numeric_limits<float>::denorm_min());
  sample.value =
      std::fmin(sample.value, 1 - std::numeric_limits<float>::epsilon() / 2);
  sample.iteration = iteration;
  return sample.value;
}

void MLTIntegrator::integrate(const Scene &scene, const Color &wavelengthBase,
                              smdl::SpectralRenderImage &renderImage) const {
  SMDL_PROFILER_ENTRY("MLTIntegrator::integrate()");
  auto scalarMeasurement{
      [](const Color &beta) { return beta.average_value(); }};

  const auto numTotalMutations{size_t(scene.camera.imageExtent.x) *
                               size_t(scene.camera.imageExtent.y) *
                               size_t(samplesPerPixel)};
  struct Contribution final {
    explicit Contribution(size_t maxDepth)
        : cameraPath(maxDepth), lightPath(maxDepth) {}
    size_t cameraPathLen{};
    size_t lightPathLen{};
    std::vector<Vertex> cameraPath{};
    std::vector<Vertex> lightPath{};
    Color beta{};
    float betaMeasurement{};
    float misWeight{};
    smdl::float2 imageCoord{};
  };
  auto contributionSample{[&](smdl::BumpPtrAllocator &allocator,
                              Sampler &sampler, size_t order,
                              Contribution &contribution) -> bool {
    sampler.next_sequence();
    auto rngf{[&]() { return sampler.next_sample(); }};
    if (order == 0)
      return false;
    size_t numStrategies{order + 1};
    size_t depthFromCamera = size_t(numStrategies * rngf()) + 1;
    size_t depthFromLight = numStrategies + 1 - depthFromCamera;
    contribution.imageCoord =
        smdl::float2(float(scene.camera.imageExtent.x) * rngf(),
                     float(scene.camera.imageExtent.y) * rngf());
    contribution.cameraPathLen = scene.trace_path_from_camera(
        allocator, rngf, wavelengthBase, contribution.imageCoord,
        depthFromCamera, &contribution.cameraPath[0]);
    if (depthFromCamera != contribution.cameraPathLen)
      return false;
    sampler.next_sequence();
    contribution.lightPathLen =
        scene.trace_path_from_light(allocator, rngf, wavelengthBase,
                                    depthFromLight, &contribution.lightPath[0]);
    if (depthFromLight != contribution.lightPathLen)
      return false;
    sampler.next_sequence();
    if (!connect_bidirectional(
            scene, allocator, rngf, wavelengthBase,
            contribution.cameraPathLen == 0
                ? nullptr
                : &contribution.cameraPath[contribution.cameraPathLen - 1],
            contribution.lightPathLen == 0
                ? nullptr
                : &contribution.lightPath[contribution.lightPathLen - 1],
            contribution.beta, contribution.misWeight, contribution.imageCoord))
      return false;
    contribution.beta *= contribution.misWeight * numStrategies;
    contribution.betaMeasurement = scalarMeasurement(contribution.beta);
    return true;
  }};

  auto bootstraps{std::vector<double>(
      size_t(numBootstrap * (maxOrder - minOrder + 1)), 0.0)};
  auto bootstrapSampler{[&](unsigned long bootstrapIndex) {
    return Sampler(smallStepSigma, largeStepProbability, seed, 0xA7CBE565UL,
                   0x6AF93C73UL, bootstrapIndex, 0xE5C6FB2CUL, 0x24718FB5UL);
  }};
  {
    SMDL_PROFILER_ENTRY("Bootstrap");
    smdl::parallel_for(0, numBootstrap, [&](size_t i) {
      auto allocator{smdl::BumpPtrAllocator{}};
      auto contribution{Contribution(maxOrder + 2)};
      for (size_t order{minOrder}; order <= maxOrder; order++) {
        auto bootstrapIndex{i * (maxOrder - minOrder + 1) + order - minOrder};
        auto sampler{bootstrapSampler(bootstrapIndex)};
        if (contributionSample(allocator, sampler, order, contribution)) {
          bootstraps[bootstrapIndex] = contribution.betaMeasurement;
        }
        allocator.reset();
      }
    });
  }
  auto bootstrapIntegral{
      std::accumulate(bootstraps.begin(), bootstraps.end(), 0.0) /
      bootstraps.size() * (maxOrder - minOrder + 1)};
  auto bootstrap{smdl::DiscreteDistribution(bootstraps)};

  {
    SMDL_PROFILER_ENTRY("Markov chains");
    smdl::parallel_for(0, numChains, [&](size_t i) {
      auto allocator{smdl::BumpPtrAllocator{}};
      auto rng{RNG{std::seed_seq{static_cast<unsigned long>(i), 0x3D6411FFUL,
                                 0xDE44B7D2UL, static_cast<unsigned long>(seed),
                                 0xE9F523E9UL, 0xD64CFEEEUL}}};
      const auto bootstrapIndex{bootstrap(rng)};
      const auto order{bootstrapIndex % (maxOrder - minOrder + 1) + minOrder};
      const auto numChainMutations{
          std::min(size_t((i + 1) * numTotalMutations / numChains),
                   size_t(numTotalMutations)) -
          size_t(i * numTotalMutations / numChains)};
      auto sampler{bootstrapSampler(bootstrapIndex)};
      auto prevContribution{Contribution(maxOrder + 2)};
      auto nextContribution{Contribution(maxOrder + 2)};
      if (!contributionSample(allocator, sampler, order, prevContribution))
        SMDL_SANITY_CHECK(false,
                          "Bootstrap contribution should have been non-zero!");
      for (size_t j{}; j < numChainMutations; j++) {
        sampler.next_iteration();
        float acceptChance{0.0f};
        if (contributionSample(allocator, sampler, order, nextContribution)) {
          acceptChance = std::fmin(1.0f, nextContribution.betaMeasurement /
                                             prevContribution.betaMeasurement);
        }
        if (acceptChance > 0.0f) {
          renderImage
              .pixel_reference(size_t(nextContribution.imageCoord.x),
                               size_t(nextContribution.imageCoord.y))
              .add_sample(bootstrapIntegral / nextContribution.betaMeasurement *
                              acceptChance / float(samplesPerPixel),
                          smdl::Span<float>(nextContribution.beta.data(),
                                            nextContribution.beta.size()));
        }
        if (acceptChance < 1.0f) {
          renderImage
              .pixel_reference(size_t(prevContribution.imageCoord.x),
                               size_t(prevContribution.imageCoord.y))
              .add_sample(bootstrapIntegral / prevContribution.betaMeasurement *
                              (1.0 - acceptChance)/ float(samplesPerPixel),
                          smdl::Span<float>(prevContribution.beta.data(),
                                            prevContribution.beta.size()));
        }
        if (generate_canonical(rng) < acceptChance) {
          sampler.accept();
          std::swap(prevContribution, nextContribution);
        } else {
          sampler.reject();
        }
        allocator.reset();
      }
    });
  }
}
