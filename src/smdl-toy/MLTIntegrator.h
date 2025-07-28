#pragma once

#include "Scene.h"

class MLTIntegrator final {
public:
  class Options final {
  public:
    /// The random seed.
    uint32_t seed{0};

    /// The small-step standard deviation.
    float smallStepSigma{0.008f};

    /// The large-step probability.
    float largeStepProbability{0.3f};

    /// The maximum depth.
    uint64_t maxBounces{4};

    /// The number of bootstrap paths to initialize the algorithm.
    uint64_t numBootstrapPaths{100'000};

    /// The number of mutations.
    ///
    /// Typically this is the number of desired mutations per pixel times the
    /// number of pixels in the image, though it is important to note that the
    /// mutations will not be perfectly distributed across pixels in general,
    /// and that is by design. The nature of metropolis is to focus more effort
    /// in the areas of highest contribution.
    ///
    uint64_t numMutationsPerPixel{100};

    /// The number of Markov chains.
    uint64_t numChains{5000};
  };

  MLTIntegrator() = default;

  explicit MLTIntegrator(const Options &options) : options(options) {}

  void integrate(const Scene &scene, const Color &wavelengthBase,
                 smdl::SpectralRenderImage &renderImage) const;

private:
  class Sampler final {
  public:
    template <typename... Seeds>
    explicit Sampler(const Options &options, const Seeds &...seeds)
        : rng(std::seed_seq{options.seed, uint32_t(seeds)...}),
          smallStepSigma(options.smallStepSigma),
          largeStepProbability(options.largeStepProbability) {
      next_iteration();
    }

    void next_iteration() noexcept;

    void next_sequence();

    [[nodiscard]] float next_sample();

    [[nodiscard]] size_t next_sample_as_index(size_t n) {
      return std::min(size_t(std::floor(float(n) * next_sample())), n - 1);
    }

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

private:
  Options options{};

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
};
