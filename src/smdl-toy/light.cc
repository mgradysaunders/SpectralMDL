#include "light.h"

EnvLight::EnvLight(const std::string &filename, float scaleFactor)
    : scaleFactor(scaleFactor) {
  if (auto error{image.startLoad(filename)})
    error->printAndExit();
  image.finishLoad();
  auto weights{std::vector<float>{}};
  const int numTexelsX{image.getNumTexelsX()};
  const int numTexelsY{image.getNumTexelsY()};
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
  Color Li{};
  int2 iPixel{};
  pdf = imageDistr.directionPDF(wi, &iPixel);
  if (pdf > 0)
    compiler.convertRGBToColor(state, image.fetch(iPixel.x, iPixel.y),
                              Li.data());
  return Li * scaleFactor;
}

float3 EnvLight::Li_sample(smdl::Compiler &compiler, const smdl::State &state,
                           float2 xi, float &pdf, Color &Li) const {
  int2 iPixel{};
  float3 wi{imageDistr.directionSample(xi, &iPixel, &pdf)};
  if (pdf > 0.0f) {
    compiler.convertRGBToColor(state, image.fetch(iPixel.x, iPixel.y),
                              Li.data());
    Li *= scaleFactor;
  } else {
    Li = Color(0.0f);
  }
  return wi;
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
  auto coords{smdl::coordinateSystem(ray.dir)};
  auto disk{smdl::uniformDiskSample(float2(xi.z, xi.w))};
  ray.org = scene.boundCenter +
            scene.boundRadius * (coords * float3(disk.x, disk.y, -1.0f));
  ppdf = 1.0f / (PI * scene.boundRadius * scene.boundRadius);
  return true;
}
