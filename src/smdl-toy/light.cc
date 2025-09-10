#include "light.h"

EnvLight::EnvLight(const std::string &filename, float scaleFactor)
    : scaleFactor(scaleFactor) {
  if (auto error{image.start_load(filename)})
    error->print_and_exit();
  image.finish_load();
  auto weights{std::vector<float>{}};
  const int numTexelsX{image.get_num_texels_x()};
  const int numTexelsY{image.get_num_texels_y()};
  weights.reserve(numTexelsX * numTexelsY);
  for (int iY = 0; iY < numTexelsY; iY++) {
    auto theta{PI * (iY + 0.5f) / float(numTexelsY)};
    auto sinTheta{std::sin(theta)};
    for (int iX = 0; iX < numTexelsX; iX++) {
      auto value{image.fetch(iX, iY)};
      weights.push_back(sinTheta * (value.x + value.y + value.z) / 3.0f);
    }
  }
  imageDistr = smdl::Distribution2D(numTexelsX, numTexelsY, weights);
}

Color EnvLight::Li(smdl::Compiler &compiler, const smdl::State &state,
                   float3 wi, float &pdf) const {
  float theta = std::atan2(std::hypot(wi.x, wi.y), wi.z);
  theta = std::max(theta, 0.0f);
  theta = std::min(theta, PI);
  float sinTheta{std::sin(theta)};
  if (!(sinTheta > 0)) {
    pdf = 0;
    return {};
  }
  float phi = std::atan2(wi.y, wi.x);
  if (phi < 0.0f)
    phi += 2.0f * PI;
  phi = std::max(phi, 0.0f);
  phi = std::min(phi, 2.0f * PI);

  int nX = image.get_num_texels_x(), iX = int(nX * phi / (2.0f * PI));
  int nY = image.get_num_texels_y(), iY = int(nY * theta / PI);
  iX = std::max(0, std::min(iX, nX - 1));
  iY = std::max(0, std::min(iY, nY - 1));
  pdf = imageDistr.pixel_pmf(int2(iX, iY));
  pdf *= image.get_num_texels_x() * image.get_num_texels_y();
  pdf /= 2.0f * PI * PI * sinTheta;
  Color Li{};
  compiler.jit_rgb_to_color(state, image.fetch(iX, iY), Li.data());
  return Li * scaleFactor;
}

float3 EnvLight::Li_sample(smdl::Compiler &compiler, const smdl::State &state,
                           float2 xi, float &pdf, Color &Li) const {
  auto i{imageDistr.pixel_sample(xi, &xi, &pdf)};
  auto phi{2.0f * PI * (i.x + xi.x) / float(image.get_num_texels_x())};
  auto theta{PI * (i.y + xi.y) / float(image.get_num_texels_y())};
  auto cosTheta{std::cos(theta)};
  auto sinTheta{std::sin(theta)};
  if (sinTheta == 0.0f) {
    pdf = 0.0f;
    Li = Color(0.0f);
    return {};
  } else {
    pdf *= image.get_num_texels_x() * image.get_num_texels_y();
    pdf /= 2.0f * PI * PI * sinTheta;
    compiler.jit_rgb_to_color(state, image.fetch(i.x, i.y), Li.data());
    Li *= scaleFactor;
    return normalize(float3(sinTheta * std::cos(phi), //
                            sinTheta * std::sin(phi), cosTheta));
  }
}

bool EnvLight::Le_sample(smdl::Compiler &compiler, const smdl::State &state,
                         const Scene &scene, float4 xi, Ray &ray, float &ppdf,
                         float &wpdf, Color &Le) const {
  ray.dir = -Li_sample(compiler, state, float2(xi.x, xi.y), wpdf, Le);
  if (wpdf == 0.0f) {
    return false;
  }
  ray.tmin = EPS;
  ray.tmax = INF;
  auto coords{smdl::coordinate_system(ray.dir)};
  auto disk{smdl::uniform_disk_sample(float2(xi.z, xi.w))};
  ray.org = scene.boundCenter +
            scene.boundRadius * (coords * float3(disk.x, disk.y, -1.0f));
  ppdf = 1.0f / (PI * scene.boundRadius * scene.boundRadius);
  return true;
}
