#pragma once

#include "raytracing.h"

#include "smdl/Compiler.h"
#include "smdl/Image.h"
#include "smdl/Support/Sampling.h"

class EnvLight final {
public:
  EnvLight() = default;

  EnvLight(const std::string &filename, float scaleFactor = 1.0f);

  [[nodiscard]] Color Li(smdl::Compiler &compiler, const smdl::State &state,
                         float3 wi, float &pdf) const;

  [[nodiscard]] float3 Li_sample(smdl::Compiler &compiler,
                                 const smdl::State &state, float2 xi,
                                 float &pdf, Color &Li) const;

  [[nodiscard]] bool Le_sample(smdl::Compiler &compiler,
                               const smdl::State &state, const Scene &scene,
                               float4 xi, Ray &ray, float &ppdf, float &wpdf,
                               Color &Le) const;

private:
  float scaleFactor{1.0f};

  smdl::Image image{};

  smdl::Distribution2D imageDistr{};
};
