#include "smdl/RenderUtil/Colorimetry.h"

#include <cmath>

namespace smdl {

namespace {

constexpr double PI_DOUBLE{3.14159265358979323846};

// In double: `radians()` and `degrees()` take a float.
[[nodiscard]] double toRadians(double deg) noexcept {
  return deg * PI_DOUBLE / 180.0;
}

[[nodiscard]] double toDegrees(double rad) noexcept {
  return rad * 180.0 / PI_DOUBLE;
}

// Lam's cone response matrix, the rows as published stored by columns.
[[nodiscard]] double3x3 bradfordCones() noexcept {
  return {double3(0.8951, -0.7502, 0.0389), double3(0.2664, 1.7135, -0.0685),
          double3(-0.1614, 0.0367, 1.0296)};
}

// The CIELAB companding function, linear below the knee.
[[nodiscard]] double labCompand(double t) noexcept {
  constexpr double DELTA{6.0 / 29.0};
  return t > DELTA * DELTA * DELTA ? std::cbrt(t)
                                   : t / (3.0 * DELTA * DELTA) + 4.0 / 29.0;
}

} // namespace

double3x3 xyzToLinearSRGB() noexcept {
  return {double3(3.240450, -0.969266, 0.0556434),
          double3(-1.537140, 1.876010, -0.2040260),
          double3(-0.498532, 0.041556, 1.0572300)};
}

double3 linearSRGBWhite() noexcept {
  double3x3 inverse{xyzToLinearSRGB()};
  (void)tryInvert(inverse);
  return inverse * double3(1.0);
}

double3x3 bradfordAdaptation(const double3 &from, const double3 &to) noexcept {
  const double3x3 cones{bradfordCones()};
  double3x3 conesInverse{cones};
  (void)tryInvert(conesInverse);
  const double3 toCones{cones * to};
  const double3 fromCones{cones * from};
  double3x3 scale{1.0};
  for (size_t i = 0; i < 3; i++) scale[i][i] = toCones[i] / fromCones[i];
  return conesInverse * (scale * cones);
}

double mccamyKelvin(const double2 &xy) noexcept {
  const double n{(xy.x - 0.3320) / (0.1858 - xy.y)};
  return ((449.0 * n + 3525.0) * n + 6823.3) * n + 5520.33;
}

double3 xyzToLab(const double3 &xyz, const double3 &white) noexcept {
  const double fx{labCompand(xyz.x / white.x)};
  const double fy{labCompand(xyz.y / white.y)};
  const double fz{labCompand(xyz.z / white.z)};
  return {116.0 * fy - 16.0, 500.0 * (fx - fy), 200.0 * (fy - fz)};
}

double deltaEab(const double3 &lab0, const double3 &lab1) noexcept {
  return length(lab1 - lab0);
}

double deltaE00(const double3 &lab0, const double3 &lab1) noexcept {
  constexpr double POW25_7{6103515625.0};
  const double chroma0{std::hypot(lab0.y, lab0.z)};
  const double chroma1{std::hypot(lab1.y, lab1.z)};
  const double chromaMean7{std::pow(0.5 * (chroma0 + chroma1), 7.0)};
  const double g{0.5 *
                 (1.0 - std::sqrt(chromaMean7 / (chromaMean7 + POW25_7)))};
  // The primed chroma and hue, with a' stretched to even out the blues.
  const double a0{(1.0 + g) * lab0.y};
  const double a1{(1.0 + g) * lab1.y};
  const double c0{std::hypot(a0, lab0.z)};
  const double c1{std::hypot(a1, lab1.z)};
  const auto hueOf{[](double a, double b) {
    if (a == 0.0 && b == 0.0) return 0.0;
    const double hue{toDegrees(std::atan2(b, a))};
    return hue < 0.0 ? hue + 360.0 : hue;
  }};
  const double h0{hueOf(a0, lab0.z)};
  const double h1{hueOf(a1, lab1.z)};
  const bool isAchromatic{c0 * c1 == 0.0};
  double dh{h1 - h0};
  if (isAchromatic)
    dh = 0.0;
  else if (dh > 180.0)
    dh -= 360.0;
  else if (dh < -180.0)
    dh += 360.0;
  const double dL{lab1.x - lab0.x};
  const double dC{c1 - c0};
  const double dH{2.0 * std::sqrt(c0 * c1) * std::sin(toRadians(0.5 * dh))};
  // The means the weights are taken at, the hue's around the circle.
  const double lMean{0.5 * (lab0.x + lab1.x)};
  const double cMean{0.5 * (c0 + c1)};
  double hMean{h0 + h1};
  if (!isAchromatic) {
    if (std::abs(h0 - h1) <= 180.0)
      hMean *= 0.5;
    else if (hMean < 360.0)
      hMean = 0.5 * (hMean + 360.0);
    else
      hMean = 0.5 * (hMean - 360.0);
  }
  const double t{1.0 - 0.17 * std::cos(toRadians(hMean - 30.0)) +
                 0.24 * std::cos(toRadians(2.0 * hMean)) +
                 0.32 * std::cos(toRadians(3.0 * hMean + 6.0)) -
                 0.20 * std::cos(toRadians(4.0 * hMean - 63.0))};
  const double dTheta{
      30.0 * std::exp(-((hMean - 275.0) / 25.0) * ((hMean - 275.0) / 25.0))};
  const double cMean7{std::pow(cMean, 7.0)};
  const double rC{2.0 * std::sqrt(cMean7 / (cMean7 + POW25_7))};
  const double lOff{(lMean - 50.0) * (lMean - 50.0)};
  const double sL{1.0 + 0.015 * lOff / std::sqrt(20.0 + lOff)};
  const double sC{1.0 + 0.045 * cMean};
  const double sH{1.0 + 0.015 * cMean * t};
  const double rT{-std::sin(toRadians(2.0 * dTheta)) * rC};
  const double l{dL / sL};
  const double c{dC / sC};
  const double h{dH / sH};
  return std::sqrt(l * l + c * c + h * h + rT * c * h);
}

} // namespace smdl
