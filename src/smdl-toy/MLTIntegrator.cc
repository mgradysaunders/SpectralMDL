#include "MLTIntegrator.h"

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
  sample.checkpoint();
  if (isLargeStep) {
    sample.value = generate_canonical(rng);
    sample.iteration = iteration;
  } else {
    auto sigma{smallStepSigma * float(std::sqrt(iteration - sample.iteration))};
    sample.value += std::normal_distribution<float>(0.0f, sigma)(rng);
    sample.value -= std::floor(sample.value);
    sample.iteration = iteration;
  }
  sample.value = std::fmax(sample.value, 0.0f);
  sample.value = std::fmin(sample.value, 0.999999f);
  return sample.value;
}

void MLTIntegrator::Sampler::finish_and_accept_iteration() noexcept {
  if (isLargeStep) {
    iterationOfLastLargeStep = iteration;
  }
}

void MLTIntegrator::Sampler::finish_and_reject_iteration() noexcept {
  for (auto &sequence : sequences) {
    for (auto &sample : sequence) {
      if (sample.iteration == iteration) {
        sample.rewind();
      }
    }
  }
  --iteration;
}

void MLTIntegrator::integrate(const Scene &scene, const Color &wavelengthBase,
                              smdl::SpectralRenderImage &renderImage) const {
  auto scalarMeasurement{[](const Color &beta) { return beta.average(); }};

  const auto seed{options.seed};
  const auto maxBounces{options.maxBounces};
  const auto numBootstrapPaths{options.numBootstrapPaths};
  const auto numMutationsPerPixel{options.numMutationsPerPixel};
  const auto numTotalMutations{scene.camera.imageExtent.x *
                               scene.camera.imageExtent.y *
                               numMutationsPerPixel};
  const auto numChains{options.numChains};

  auto contributionSample{[&](smdl::BumpPtrAllocator &allocator,
                              Sampler &sampler, size_t numBounces,
                              Contribution &contribution) -> bool {
    sampler.next_sequence();
    auto rng{[&]() { return sampler.next_sample(); }};
    if (numBounces == 0)
      return false;
    float u = sampler.next_sample();
    size_t numStrategies{numBounces + 1};
    size_t depthFromCamera = size_t(numStrategies * u) + 1;
    size_t depthFromLight = numStrategies + 1 - depthFromCamera;
    contribution.imageCoord =
        smdl::float2(float(scene.camera.imageExtent.x) * rng(),
                     float(scene.camera.imageExtent.y) * rng());
    contribution.cameraPathLen = scene.trace_path_from_camera(
        allocator, rng, wavelengthBase, contribution.imageCoord,
        depthFromCamera, &contribution.cameraPath[0]);
    if (depthFromCamera != contribution.cameraPathLen)
      return false;
    sampler.next_sequence();
    contribution.lightPathLen =
        scene.trace_path_from_light(allocator, rng, wavelengthBase,
                                    depthFromLight, &contribution.lightPath[0]);
    if (depthFromLight != contribution.lightPathLen)
      return false;
    sampler.next_sequence();
    if (!connect_bidirectional(
            scene, allocator, rng, wavelengthBase,
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
      size_t(numBootstrapPaths * (maxBounces + 1)), 0.0)};
  auto bootstrapSampler{[&](unsigned long bootstrapIndex) {
    return Sampler(options, 0xA7CBE565UL, 0x6AF93C73UL, bootstrapIndex,
                   0xE5C6FB2CUL, 0x24718FB5UL);
  }};
  smdl::parallel_for(numBootstrapPaths, [&](size_t pathIndex) {
    auto allocator{smdl::BumpPtrAllocator{}};
    auto contribution{Contribution(maxBounces + 2)};
    for (size_t numBounces = 0; numBounces <= maxBounces; numBounces++) {
      auto bootstrapIndex{pathIndex * (maxBounces + 1)+ numBounces};
      auto sampler{bootstrapSampler(bootstrapIndex)};
      if (contributionSample(allocator, sampler, numBounces, contribution)) {
        bootstraps[bootstrapIndex] = contribution.betaMeasurement;
      }
      allocator.reset();
    }
  });
  auto bootstrapIntegral{ 4 * 
      std::accumulate(bootstraps.begin(), bootstraps.end(), 0.0) /
      bootstraps.size() * (maxBounces + 1)};
  auto bootstrap{smdl::DiscreteDistribution(bootstraps)};

  smdl::parallel_for(numChains, [&](size_t chainIndex) {
    auto allocator{smdl::BumpPtrAllocator{}};
    auto rng{RNG{std::seed_seq{
        static_cast<unsigned long>(chainIndex), 0x3D6411FFUL, 0xDE44B7D2UL,
        static_cast<unsigned long>(seed), 0xE9F523E9UL, 0xD64CFEEEUL}}};
    const auto bootstrapIndex{bootstrap(rng)};
    const auto numBounces{bootstrapIndex % (maxBounces + 1)};
    const auto numChainMutations{
        std::min((chainIndex + 1) * numTotalMutations / numChains,
                 numTotalMutations) -
        chainIndex * numTotalMutations / numChains};
    auto sampler{bootstrapSampler(bootstrapIndex)};
    auto prevContribution{Contribution(maxBounces + 2)};
    auto nextContribution{Contribution(maxBounces + 2)};
    if (!contributionSample(allocator, sampler, numBounces, prevContribution))
      SMDL_SANITY_CHECK(false,
                        "Bootstrap contribution should have been non-zero!");
    for (size_t mutationIndex{}; mutationIndex < numChainMutations;
         mutationIndex++) {
      sampler.next_iteration();
      float acceptChance{0.0f};
      if (contributionSample(allocator, sampler, numBounces,
                             nextContribution)) {
        acceptChance = std::fmin(1.0f, nextContribution.betaMeasurement /
                                           prevContribution.betaMeasurement);
      }
      if (acceptChance > 0.0f) {
        renderImage
            .pixel_ref(size_t(nextContribution.imageCoord.x),
                       size_t(nextContribution.imageCoord.y))
            .add_sample(bootstrapIntegral / nextContribution.betaMeasurement *
                            acceptChance,
                        smdl::Span<float>(nextContribution.beta.data(),
                                          nextContribution.beta.size()));
      }
      if (acceptChance < 1.0f) {
        renderImage
            .pixel_ref(size_t(prevContribution.imageCoord.x),
                       size_t(prevContribution.imageCoord.y))
            .add_sample(bootstrapIntegral / prevContribution.betaMeasurement *
                            (1.0 - acceptChance),
                        smdl::Span<float>(prevContribution.beta.data(),
                                          prevContribution.beta.size()));
      }
      if (generate_canonical(rng) < acceptChance) {
        sampler.finish_and_accept_iteration();
        std::swap(prevContribution, nextContribution);
      } else {
        sampler.finish_and_reject_iteration();
      }
      allocator.reset();
    }
  });
}
