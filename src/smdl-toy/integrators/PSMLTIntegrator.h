#pragma once

#include "../Integrator.h"

class PSMLTIntegrator final : public Integrator {
public:
  explicit PSMLTIntegrator(size_t seed, size_t samplesPerPixel, size_t minOrder,
                           size_t maxOrder, bool onlyIndirectPSMLT = false,
                           float smallStepSigma = 0.01f,
                           float largeStepProbability = 0.1f,
                           size_t numBootstrap = 100'000,
                           size_t numChains = 1000)
      : Integrator(seed, samplesPerPixel, minOrder, maxOrder),
        onlyIndirectPSMLT(onlyIndirectPSMLT && minOrder < 2),
        smallStepSigma(smallStepSigma),
        largeStepProbability(largeStepProbability), numBootstrap(numBootstrap),
        numChains(numChains) {}

  void integrate(const Scene &scene, const Color &wavelengthBase,
                 smdl::SpectralRenderImage &renderImage) const final;

private:
  class Sampler final {
  public:
    template <typename... Seeds>
    explicit Sampler(float smallStepSigma, float largeStepProbability,
                     const Seeds &...seeds)
        : rng(make_RNG(seeds...)), smallStepSigma(smallStepSigma),
          largeStepProbability(largeStepProbability) {}

    void next_iteration() noexcept;

    void next_sequence();

    [[nodiscard]] float next_sample();

    void accept() noexcept {
      if (isLargeStep) {
        iterationOfLastLargeStep = iteration;
      }
    }

    void reject() noexcept {
      for (auto &sequence : sequences) {
        for (auto &sample : sequence) {
          if (sample.iteration == iteration) {
            sample.restore();
          }
        }
      }
      --iteration;
    }

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
  const bool onlyIndirectPSMLT{false};

  const float smallStepSigma{0.01f};

  const float largeStepProbability{0.1f};

  const size_t numBootstrap{100'000};

  const size_t numChains{1000};
};
