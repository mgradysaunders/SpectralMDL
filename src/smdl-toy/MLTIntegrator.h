#pragma once

#include "Scene.h"

class MLTIntegrator final {
public:
  class Options final {
  public:
    /// The random seed.
    uint32_t seed{0};

    /// The small-step standard deviation.
    float smallStepSigma{0.2f};

    /// The large-step probability.
    float largeStepProbability{0.1f};

    /// The minimum scattering order (number of bounces).
    uint64_t minOrder{0};

    /// The maximum scattering order (number of bounces).
    uint64_t maxOrder{4};

    /// The number of bootstrap paths to initialize the algorithm.
    uint64_t nBootstrap{100'000};

    /// The number of mutations per pixel.
    uint64_t nMutationsPerPixel{100};

    /// The number of Markov chains.
    uint64_t nChains{1000};
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
    }

    void next_iteration() noexcept;

    void next_sequence();

    [[nodiscard]] float next_sample();

    void finish_and_accept_iteration() noexcept;

    void finish_and_reject_iteration() noexcept;

  private:
    struct Sample final {
      void backup() noexcept {
        valueBackup = value, iterationBackup = iteration;
      }

      void restore() noexcept {
        value = valueBackup, iteration = iterationBackup;
      }

      float value{};

      float valueBackup{};

      int64_t iteration{};

      int64_t iterationBackup{};
    };

    /// The random number generator.
    pcg32_k1024 rng{};

    /// The iteration.
    int64_t iteration{};

    /// The iteration of the last large step.
    int64_t iterationOfLastLargeStep{};

    /// The small-step standard deviation.
    const float smallStepSigma{0.01f};

    /// The large-step probability.
    const float largeStepProbability{0.3f};

    /// Is large-step currently?
    bool isLargeStep{true};

    /// The total sample count.
    size_t sampleCount{0};

    /// The total sequence count.
    size_t sequenceCount{0};

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
