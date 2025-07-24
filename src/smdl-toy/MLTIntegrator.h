#pragma once

#include "Scene.h"

class MLTIntegrator final {
public:
  class Options final {
  public:
    /// The random seed.
    uint32_t seed{0};

    /// The small-step standard deviation.
    float smallStepSigma{0.01f};

    /// The large-step probability.
    float largeStepProbability{0.3f};

    /// The minimum depth.
    uint64_t minBounces{0};

    /// The maximum depth.
    uint64_t maxBounces{5};

    /// The number of bootstrap paths to initialize the algorithm.
    uint64_t numBootstrapPaths{50'000};

    /// The number of mutations.
    ///
    /// Typically this is the number of desired mutations per pixel times the
    /// number of pixels in the image, though it is important to note that the
    /// mutations will not be perfectly distributed across pixels in general,
    /// and that is by design. The nature of metropolis is to focus more effort
    /// in the areas of highest contribution.
    ///
    uint64_t numMutations{5'000'000};

    /// The number of Markov chains.
    uint64_t numChains{1000};
  };

  MLTIntegrator() = default;

  explicit MLTIntegrator(const Options &options) : options(options) {}

private:
  class Sampler final {
  public:
    template <typename... Seeds>
    explicit Sampler(const Options &options, const Seeds &...seeds)
        : rng(std::seed_seq{options.seed, uint32_t(seeds)...}),
          smallStepSigma(options.smallStepSigma),
          largeStepProbability(options.largeStepProbability) {}

    void next_iteration() noexcept;

    void next_sequence();

    [[nodiscard]] double next_sample();

    void finish_and_accept_iteration() noexcept;

    void finish_and_reject_iteration() noexcept;

  private:
    struct Sample final {
      void checkpoint() noexcept {
        valueBackup = value, iterationBackup = iteration;
      }

      void rewind() noexcept {
        value = valueBackup, iteration = iterationBackup;
      }

      float value{};

      float valueBackup{};

      uint64_t iteration{};

      uint64_t iterationBackup{};
    };

    /// The random number generator.
    pcg32_k1024 rng{};

    /// The iteration.
    uint64_t iteration{};

    /// The iteration of the last large step.
    uint64_t iterationOfLastLargeStep{};

    /// The small-step standard deviation.
    const float smallStepSigma{0.01};

    /// The large-step probability.
    const float largeStepProbability{0.3};

    /// Is large-step currently?
    bool isLargeStep{true};

    /// The total sample count.
    uint64_t sampleCount{0};

    /// The total sequence count.
    uint64_t sequenceCount{0};

    /// The sequences.
    std::vector<std::vector<Sample>> sequences{};
  };

  class Contribution final {
  public:
    /// The subpath traced from the camera.
    std::vector<Vertex> cameraPath{};

    /// The subpath traced from the light.
    std::vector<Vertex> lightPath{};

    /// The path contribution.
    Color L{};

    /// The path contribution intensity measure which guides the acceptance
    /// probability. This is typically luminance, but can be calculated in any
    /// way that is linear with respect to the path contribution itself.
    float I{};

    /// The pixel coordinate.
    smdl::float2 pixelCoord{};
  };

#if 0
  using RandomSampler = std::function<std::optional<Contribution>(
      Random &random, uint64_t depthFromCamera, uint64_t depthFromLight)>;

  using Recorder =
      std::function<void(const Contribution &contribution, double multiplier)>;

  void operator()(const RandomSampler &randomSampler,
                  const Recorder &recorder) const;
#endif

private:
  Options options{};
};
