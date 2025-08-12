#include "PSMLTIntegrator.h"
#include "PTIntegrator.h"

#include "smdl/Support/Profiler.h"
#include "smdl/Support/Sampling.h"

#include <numeric>

void PSMLTIntegrator::Sampler::next_iteration() noexcept {
  iteration++;
  isLargeStep = generate_canonical(rng) < largeStepProbability;
  sampleCount = sequenceCount = 0;
}

void PSMLTIntegrator::Sampler::next_sequence() {
  sampleCount = 0;
  sequenceCount++;
  if (sequences.size() < sequenceCount) {
    sequences.resize(sequenceCount);
    sequences.back().reserve(32);
  }
}

float PSMLTIntegrator::Sampler::next_sample() {
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
                    std::sqrt(float(iteration - sample.iteration)) *
                    smdl::standard_normal_sample(generate_canonical(rng));
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

void PSMLTIntegrator::integrate(const Scene &scene, const Color &wavelengthBase,
                                smdl::SpectralRenderImage &renderImage) const {
  SMDL_PROFILER_ENTRY("PSMLTIntegrator::integrate()");
  const auto minOrderPSMLT{onlyIndirectPSMLT ? std::max(minOrder, size_t(2))
                                             : minOrder};
  if (minOrderPSMLT <= maxOrder) {
    const auto scalarMeasurement{
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
      smdl::float2 pixelCoord{};
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
      contribution.pixelCoord =
          smdl::float2(float(scene.camera.imageExtent.x) * rngf(),
                       float(scene.camera.imageExtent.y) * rngf());
      contribution.cameraPathLen = scene.trace_path_from_camera(
          allocator, rngf, wavelengthBase, contribution.pixelCoord,
          depthFromCamera, &contribution.cameraPath[0]);
      if (depthFromCamera != contribution.cameraPathLen)
        return false;
      sampler.next_sequence();
      contribution.lightPathLen = scene.trace_path_from_light(
          allocator, rngf, wavelengthBase, depthFromLight,
          &contribution.lightPath[0]);
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
              contribution.beta, contribution.misWeight,
              contribution.pixelCoord))
        return false;
      contribution.beta *= contribution.misWeight * numStrategies;
      contribution.betaMeasurement = scalarMeasurement(contribution.beta);
      return true;
    }};

    auto bootstraps{std::vector<double>(
        size_t(numBootstrap * (maxOrder - minOrderPSMLT + 1)), 0.0)};
    auto bootstrapSampler{[&](size_t bootstrapIndex) {
      return Sampler(smallStepSigma, largeStepProbability, seed, 0xA7CBE565UL,
                     0x6AF93C73UL, bootstrapIndex, 0xE5C6FB2CUL, 0x24718FB5UL);
    }};
    {
      SMDL_PROFILER_ENTRY("Bootstrap");
      smdl::parallel_for(0, numBootstrap, [&](size_t i) {
        auto allocator{smdl::BumpPtrAllocator{}};
        auto contribution{Contribution(maxOrder + 2)};
        for (size_t order{minOrderPSMLT}; order <= maxOrder; order++) {
          auto bootstrapIndex{i * (maxOrder - minOrderPSMLT + 1) + order -
                              minOrderPSMLT};
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
        bootstraps.size() * (maxOrder - minOrderPSMLT + 1)};
    auto bootstrap{smdl::DiscreteDistribution(bootstraps)};

    {
      SMDL_PROFILER_ENTRY("Markov chains");
      smdl::parallel_for(0, numChains, [&](size_t i) {
        auto allocator{smdl::BumpPtrAllocator{}};
        auto rng{make_RNG(i, 0x3D6411FFUL, 0xDE44B7D2UL, seed, 0xE9F523E9UL,
                          0xD64CFEEEUL)};
        const auto bootstrapIndex{bootstrap(rng)};
        const auto order{bootstrapIndex % (maxOrder - minOrderPSMLT + 1) +
                         minOrderPSMLT};
        const auto numChainMutations{
            std::min(size_t((i + 1) * numTotalMutations / numChains),
                     size_t(numTotalMutations)) -
            size_t(i * numTotalMutations / numChains)};
        auto sampler{bootstrapSampler(bootstrapIndex)};
        auto prevContribution{Contribution(maxOrder + 2)};
        auto nextContribution{Contribution(maxOrder + 2)};
        if (!contributionSample(allocator, sampler, order, prevContribution))
          SMDL_SANITY_CHECK(
              false, "Bootstrap contribution should have been non-zero!");
        for (size_t j{}; j < numChainMutations; j++) {
          sampler.next_iteration();
          float acceptChance{0.0f};
          if (contributionSample(allocator, sampler, order, nextContribution)) {
            acceptChance =
                std::fmin(1.0f, nextContribution.betaMeasurement /
                                    prevContribution.betaMeasurement);
          }
          if (acceptChance > 0.0f) {
            renderImage
                .pixel_reference(size_t(nextContribution.pixelCoord.x),
                                 size_t(nextContribution.pixelCoord.y))
                .add(bootstrapIntegral / nextContribution.betaMeasurement *
                         acceptChance / double(samplesPerPixel),
                     nextContribution.beta.data());
          }
          if (acceptChance < 1.0f) {
            renderImage
                .pixel_reference(size_t(prevContribution.pixelCoord.x),
                                 size_t(prevContribution.pixelCoord.y))
                .add(bootstrapIntegral / prevContribution.betaMeasurement *
                         (1.0 - acceptChance) / double(samplesPerPixel),
                     prevContribution.beta.data());
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
  if (onlyIndirectPSMLT) {
    auto directIntegrator{PTIntegrator(seed, 4 * samplesPerPixel, minOrder,
                                       std::min(maxOrder, size_t(1)))};
    auto directRenderImage{smdl::SpectralRenderImage(
        WAVELENGTH_BASE_MAX, scene.camera.imageExtent.x,
        scene.camera.imageExtent.y)};
    directIntegrator.integrate(scene, wavelengthBase, directRenderImage);
    renderImage.add(directRenderImage);
  }
}
