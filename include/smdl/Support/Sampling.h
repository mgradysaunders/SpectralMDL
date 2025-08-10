/// \file
#pragma once

#include <limits>

#include "smdl/Support/VectorMath.h"

namespace smdl {

/// \addtogroup Support
/// \{

/// The constant `PI`.
constexpr float PI = 3.141592653589793f;

/// The inverse of the error function.
[[nodiscard]] inline float erf_inverse(float y) {
  float w = -std::log(
      std::max(std::numeric_limits<float>::denorm_min(), (1 - y) * (1 + y)));
  float x = 0;
  if (w < 5) {
    w = w - 2.5f;
    x = w * 2.81022636e-08f + 3.43273939e-7f;
    x = w * x - 3.52338770e-6f;
    x = w * x - 4.39150654e-6f;
    x = w * x + 2.18580870e-4f;
    x = w * x - 1.25372503e-3f;
    x = w * x - 4.17768164e-3f;
    x = w * x + 2.46640727e-1f;
    x = w * x + 1.50140941f;
  } else {
    w = std::sqrt(w) - 3;
    x = x * -2.00214257e-4f + 1.00950558e-4f;
    x = w * x + 1.34934322e-3f;
    x = w * x - 3.67342844e-3f;
    x = w * x + 5.73950773e-3f;
    x = w * x - 7.62246130e-3f;
    x = w * x + 9.43887047e-3f;
    x = w * x + 1.00167406f;
    x = w * x + 2.83297682f;
  }
  x *= y;
  return x;
}

[[nodiscard]] inline float standard_normal_pdf(float x) {
  return /*1/sqrt(2pi)=*/0.398942280401f * std::exp(-0.5f * x * x);
}

[[nodiscard]] inline float standard_normal_cdf(float x) {
  return 0.5f + 0.5f * std::erf(/*1/sqrt(2)=*/0.707106781187f * x);
}

[[nodiscard]] inline float standard_normal_sample(float u) {
  return /*sqrt(2)=*/1.41421356237f * erf_inverse(2 * u - 1);
}

[[nodiscard]] inline float uniform_disk_pdf(float r = 1) {
  return 1.0f / (PI * r * r);
}

[[nodiscard]] inline float2 uniform_disk_sample(float2 xi) {
  xi = xi * 2.0f - float2(1.0f);
  xi.x = (xi.x == 0.0f) ? std::numeric_limits<float>::epsilon() : xi.x;
  xi.y = (xi.y == 0.0f) ? std::numeric_limits<float>::epsilon() : xi.y;
  bool cond = std::abs(xi.x) > std::abs(xi.y);
  float rad = cond ? xi.x : xi.y;
  float phi = cond ? (PI / 4.0f) * xi.y / xi.x
                   : (PI / 2.0f) - (PI / 4.0f) * xi.x / xi.y;
  return float2(rad * std::cos(phi), rad * std::sin(phi));
}

[[nodiscard]] inline float cosine_hemisphere_pdf(float cosTheta) {
  return std::max(cosTheta, 0.0f) / PI;
}

[[nodiscard]] inline float3 cosine_hemisphere_sample(float2 xi,
                                                     float *pdf = {}) {
  auto sinTheta{uniform_disk_sample(xi)};
  auto cosTheta{std::sqrt(std::max(0.0f, 1.0f - sinTheta.x * sinTheta.x -
                                             sinTheta.y * sinTheta.y))};
  if (pdf)
    *pdf = cosTheta / PI;
  return float3(sinTheta.x, sinTheta.y, cosTheta);
}

[[nodiscard]] inline float uniform_sphere_pdf(float r = 1) {
  return 1.0f / (4.0f * PI * r * r);
}

[[nodiscard]] inline float3 uniform_sphere_sample(float2 xi, float *pdf = {}) {
  float cosTheta{std::max(-1.0f, std::min(2.0f * xi.x - 1.0f, 1.0f))};
  float sinTheta{std::sqrt(1.0f - cosTheta * cosTheta)};
  float phi{2.0f * PI * xi.y};
  if (pdf)
    *pdf = uniform_sphere_pdf();
  return float3(sinTheta * std::cos(phi), sinTheta * std::sin(phi), cosTheta);
}

[[nodiscard]] inline float uniform_cone_pdf(float zMin) {
  return 1.0f / (2.0f * PI * (1.0f - zMin));
}

[[nodiscard]] inline float3 uniform_cone_sample(float2 xi,  //
                                                float zMin, //
                                                float *pdf = {}) {
  float cosTheta{std::max(zMin, std::min((1.0f - xi.x) * zMin + xi.x, 1.0f))};
  float sinTheta{std::sqrt(1.0f - cosTheta * cosTheta)};
  float phi{2.0f * PI * xi.y};
  if (pdf)
    *pdf = uniform_cone_pdf(zMin);
  return float3(sinTheta * std::cos(phi), sinTheta * std::sin(phi), cosTheta);
}

/// \}

} // namespace smdl
