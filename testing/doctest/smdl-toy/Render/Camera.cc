#include "Fixtures.h"

#include <cmath>

#include "Render/Camera.h"
#include "Render/Sampler.h"

// The camera over the shutter: a still camera places its ray the same
// way at every shutter fraction, and a moving camera reproduces its
// keys at the two ends and the interpolation of them between.

namespace {
CameraOptions openOptions() {
  auto options{CameraOptions{}};
  options.resolution = int2(64, 48);
  options.lookFrom = float3(-6.0f, 0.0f, 2.0f);
  options.lookTo = float3(0.0f, 0.0f, 0.5f);
  options.lookUp = float3(0.0f, 0.0f, 1.0f);
  return options;
}

const float3 LOOK_FROM_SHUT{-5.0f, 1.0f, 2.5f};
const float3 LOOK_TO_SHUT{0.5f, 0.2f, 0.4f};
const float3 LOOK_UP_SHUT{0.1f, 0.0f, 1.0f};

CameraOptions movingOptions() {
  auto options{openOptions()};
  options.hasMotion = true;
  options.lookFromShut = LOOK_FROM_SHUT;
  options.lookToShut = LOOK_TO_SHUT;
  options.lookUpShut = LOOK_UP_SHUT;
  return options;
}

// The same pixel and the same sampler state every time, so that two
// cameras differ only by what they do with the draw.
CameraSample rayAt(const Camera &camera, float u) {
  Sampler sampler{};
  sampler.startPixelSample(1234, 5);
  auto sample{camera.sample(17, 9, sampler)};
  camera.toWorld(sample, u);
  return sample;
}

bool isSameRay(const Ray &a, const Ray &b) {
  return isSame(a.org, b.org) && isSame(a.dir, b.dir) && a.tmin == b.tmin &&
         a.tmax == b.tmax;
}
} // namespace

TEST_CASE("Camera: a still camera places its ray the same at every fraction") {
  auto options{openOptions()};
  SUBCASE("With a pinhole") {}
  SUBCASE("With a thin lens") { options.fStop = 2.8f; }
  const Camera camera{options};
  const auto r0{rayAt(camera, 0.0f)};
  const auto r1{rayAt(camera, 0.3f)};
  const auto r2{rayAt(camera, 1.0f)};
  CHECK(isSameRay(r0.ray, r1.ray));
  CHECK(isSameRay(r0.ray, r2.ray));
  CHECK(r0.ray.time == 0.0f);
  CHECK(r1.ray.time == 0.3f);
  CHECK(r2.ray.time == 1.0f);
  CHECK(length(r0.ray.dir) == doctest::Approx(1.0f));
}

TEST_CASE("Camera: a moving camera reproduces its keys at the shutter ends") {
  const Camera moving{movingOptions()};
  const Camera stillOpen{openOptions()};
  auto shutOptions{openOptions()};
  shutOptions.lookFrom = LOOK_FROM_SHUT;
  shutOptions.lookTo = LOOK_TO_SHUT;
  shutOptions.lookUp = LOOK_UP_SHUT;
  const Camera stillShut{shutOptions};
  CHECK(isSameRay(rayAt(moving, 0.0f).ray, rayAt(stillOpen, 0.0f).ray));
  CHECK(isSameRay(rayAt(moving, 1.0f).ray, rayAt(stillShut, 1.0f).ray));
  // The keys differ, so the two ends do.
  CHECK(!isSameRay(rayAt(moving, 0.0f).ray, rayAt(moving, 1.0f).ray));
}

TEST_CASE("Camera: halfway through the shutter is the camera of the mid keys") {
  const Camera moving{movingOptions()};
  auto midOptions{openOptions()};
  midOptions.lookFrom = 0.5f * (midOptions.lookFrom + LOOK_FROM_SHUT);
  midOptions.lookTo = 0.5f * (midOptions.lookTo + LOOK_TO_SHUT);
  midOptions.lookUp = 0.5f * (midOptions.lookUp + LOOK_UP_SHUT);
  const Camera stillMid{midOptions};
  const auto mid{rayAt(moving, 0.5f)};
  const auto expected{rayAt(stillMid, 0.5f)};
  CHECK(length(mid.ray.org - expected.ray.org) < 1e-6f);
  CHECK(length(mid.ray.dir - expected.ray.dir) < 1e-6f);
  // A pinhole's origin is the position itself, so the origin halfway is
  // the midpoint of the origins at the ends.
  const auto r0{rayAt(moving, 0.0f)};
  const auto r1{rayAt(moving, 1.0f)};
  CHECK(length(mid.ray.org - 0.5f * (r0.ray.org + r1.ray.org)) < 1e-6f);
  CHECK(mid.ray.time == 0.5f);
}

TEST_CASE("Camera: a motion equal to the open keys is still") {
  auto options{openOptions()};
  options.hasMotion = true;
  options.lookFromShut = options.lookFrom;
  options.lookToShut = options.lookTo;
  options.lookUpShut = options.lookUp;
  const Camera still{openOptions()};
  const Camera notMoving{options};
  const auto a{rayAt(still, 0.3f)};
  const auto b{rayAt(notMoving, 0.3f)};
  CHECK(isSameRay(a.ray, b.ray));
  CHECK(b.ray.time == 0.3f);
}

// The camera with a lens in it. The seam is one branch in `sample()`, so
// what these pin is that the branch keeps the frame the same way up,
// keeps the sampler consuming what it consumed, and hands the walk a ray
// that starts at the glass rather than at the camera's origin.

namespace {
// A biconvex singlet with the stop against its back, thick enough to be
// a solid out to its rim. What it does optically is pinned in the `Lens`
// suite; here it is only a lens that is present.
LensPrescription singlet() {
  const auto surface{[](float radius, float thickness, float ior, bool isStop) {
    auto value{LensSurface{}};
    value.radius = radius;
    value.thickness = thickness;
    value.ior = ior;
    value.diameter = 20.0f;
    value.isStop = isStop;
    return value;
  }};
  auto lens{LensPrescription{}};
  lens.name = "singlet";
  lens.surfaces.push_back(surface(50, 4, 1.5f, false));
  lens.surfaces.push_back(surface(-50, 0, 1, false));
  lens.surfaces.push_back(surface(0, 0, 1, true));
  return lens;
}

CameraOptions lensOptions() {
  auto options{openOptions()};
  options.lens = singlet();
  options.sensorMM = float2(36.0f, 27.0f);
  options.focus = 8.0f;
  return options;
}

// One pixel's ray in camera space, before the world transform puts it
// where the camera stands, from a fixed sampler state.
CameraSample cameraSpaceSample(const Camera &camera, size_t x, size_t y,
                               uint32_t sampleIndex) {
  Sampler sampler{};
  sampler.startPixelSample(uint32_t(y * 64 + x), sampleIndex);
  return camera.sample(x, y, sampler);
}

// The first sample of this pixel that gets through the glass. Which draw
// that is depends on where the pupil point lands, so a test that wants a
// ray rather than a blocked one has to ask for one.
CameraSample passingSample(const Camera &camera, size_t x, size_t y) {
  for (uint32_t sampleIndex = 0; sampleIndex < 32; sampleIndex++)
    if (auto sample{cameraSpaceSample(camera, x, y, sampleIndex)};
        sample.weight > 0)
      return sample;
  return CameraSample{};
}

// What the pixel weighs on average, which is the vignetting: the cos^4
// of the pupil integral times the fraction of the aperture that is not
// blocked on the way out.
float meanWeight(const Camera &camera, size_t x, size_t y) {
  constexpr uint32_t NUM_SAMPLES = 256;
  auto total{0.0f};
  for (uint32_t sampleIndex = 0; sampleIndex < NUM_SAMPLES; sampleIndex++)
    total += cameraSpaceSample(camera, x, y, sampleIndex).weight;
  return total / float(NUM_SAMPLES);
}
} // namespace

TEST_CASE("Camera: a lens replaces the thin lens and nothing else") {
  const Camera lensed{lensOptions()};
  const Camera pinhole{openOptions()};
  SUBCASE("The frame comes out the same way up") {
    // A film point is the sensor coordinate with the image inverted on
    // it, and the pinhole's is the ideal image point already the right
    // way up, so the two arrive at the same signs by opposite routes.
    // Corner pixels, where the field angle swamps the pupil point.
    for (const auto pixel : {int2(60, 4), int2(4, 44)}) {
      const auto a{passingSample(lensed, size_t(pixel.x), size_t(pixel.y))};
      const auto b{
          cameraSpaceSample(pinhole, size_t(pixel.x), size_t(pixel.y), 3)};
      REQUIRE(a.weight > 0);
      CHECK(std::signbit(a.ray.dir.x) == std::signbit(b.ray.dir.x));
      CHECK(std::signbit(a.ray.dir.y) == std::signbit(b.ray.dir.y));
      CHECK(a.ray.dir.z < 0);
      CHECK(b.ray.dir.z < 0);
    }
  }
  SUBCASE("The ray leaves the front of the glass, not the camera origin") {
    const auto sample{passingSample(lensed, 32, 24)};
    REQUIRE(sample.weight > 0);
    CHECK(sample.ray.org.z < 0);
    CHECK(length(sample.ray.org) > 0);
  }
  SUBCASE("It consumes the sampler dimensions the thin lens with a lens "
          "point consumes, in the same place") {
    auto withLens{Sampler{}};
    auto withDOF{Sampler{}};
    withLens.startPixelSample(77, 2);
    withDOF.startPixelSample(77, 2);
    auto dofOptions{openOptions()};
    dofOptions.aperture = 0.1f;
    const Camera defocused{dofOptions};
    (void)lensed.sample(11, 7, withLens);
    (void)defocused.sample(11, 7, withDOF);
    // Whatever the next draw is, both have to be looking at it: a lens
    // that consumed a different count would move every sequence after
    // it and `SAMPLER_VERSION` would owe a bump.
    CHECK(float(withLens) == float(withDOF));
  }
  SUBCASE("A blocked ray weighs nothing, so the walk skips it and the pixel "
          "average still counts it") {
    // Stopped far down, most of the rear aperture is shut off, so some
    // pixel of a frame this size draws a pupil point the stop blocks.
    auto options{lensOptions()};
    options.fStop = 22.0f;
    const Camera stopped{options};
    auto numBlocked{0};
    for (size_t y = 0; y < 48; y++)
      for (size_t x = 0; x < 64; x++)
        if (cameraSpaceSample(stopped, x, y, 3).weight == 0) numBlocked++;
    CHECK(numBlocked > 0);
  }
  SUBCASE("The cos^4 falloff is there without anything asking for it") {
    // The thin lens leaves `vignetting` off by default; the pupil
    // integral has no such switch, so a corner weighs less than the
    // middle does.
    const auto middle{meanWeight(lensed, 32, 24)};
    const auto corner{meanWeight(lensed, 1, 1)};
    REQUIRE(middle > 0);
    CHECK(corner < middle);
    CHECK(middle <= 1.0f);
  }
}

namespace {
// The same singlet with a wide flat window behind it. The window bends
// nothing and blocks nothing; all it does is make the rear aperture four
// times the area and move the plane the pupil point is drawn on, neither
// of which a lens's exposure may depend on. It stands 2 mm back, clear
// of the rim of the curved surface in front of it, which bulges a
// millimeter toward the film and would otherwise cross it.
LensPrescription singletBehindWindow() {
  auto lens{singlet()};
  lens.surfaces.back().thickness = 2.0f;
  auto window{LensSurface{}};
  window.diameter = 40.0f;
  lens.surfaces.push_back(window);
  return lens;
}

float fNumberOf(const LensPrescription &prescription) {
  return Lens{prescription, LensOptions{8.0f, 0, 0, 0}}.fNumberWideOpen();
}
} // namespace

TEST_CASE("Camera: a lens is exposed by its f-number and nothing else") {
  const auto fNumber{fNumberOf(singlet())};
  const auto ideal{1 / (fNumber * fNumber)};
  SUBCASE("An ideal lens reads one over the f-number squared, and a real "
          "one a little under") {
    const auto middle{meanWeight(Camera{lensOptions()}, 32, 24)};
    CHECK(middle < 1.02f * ideal);
    CHECK(middle > 0.8f * ideal);
  }
  SUBCASE("A wider rear element changes nothing, the exposure being an "
          "integral over the pupil and not over the glass") {
    auto options{lensOptions()};
    options.lens = singletBehindWindow();
    const auto behindWindow{meanWeight(Camera{options}, 32, 24)};
    const auto bare{meanWeight(Camera{lensOptions()}, 32, 24)};
    CHECK(behindWindow == doctest::Approx(bare).epsilon(0.02));
  }
  SUBCASE("Stopping down darkens by the square of the f-number") {
    auto options{lensOptions()};
    options.fStop = 2 * fNumber;
    const auto stopped{meanWeight(Camera{options}, 32, 24)};
    const auto wideOpen{meanWeight(Camera{lensOptions()}, 32, 24)};
    CHECK(stopped == doctest::Approx(wideOpen / 4).epsilon(0.1));
  }
  SUBCASE("Normalizing takes the f-number back out, so the frame holds its "
          "brightness") {
    auto options{lensOptions()};
    options.shouldNormalizeLensExposure = true;
    const auto wideOpen{meanWeight(Camera{options}, 32, 24)};
    options.fStop = 2 * fNumber;
    const auto stopped{meanWeight(Camera{options}, 32, 24)};
    CHECK(wideOpen == doctest::Approx(1.0).epsilon(0.2));
    CHECK(stopped == doctest::Approx(wideOpen).epsilon(0.1));
  }
}
