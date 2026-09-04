#include "doctest.h"

#include "Render/Camera.h"
#include "Render/Sampler.h"

// The camera over the shutter: a still camera places its ray the same
// way at every shutter fraction, and a moving camera reproduces its
// keys at the two ends and the interpolation of them between.

static CameraOptions openOptions() {
  auto options{CameraOptions{}};
  options.resolution = int2(64, 48);
  options.lookFrom = float3(-6.0f, 0.0f, 2.0f);
  options.lookTo = float3(0.0f, 0.0f, 0.5f);
  options.lookUp = float3(0.0f, 0.0f, 1.0f);
  return options;
}

static const float3 LOOK_FROM_CLOSE{-5.0f, 1.0f, 2.5f};
static const float3 LOOK_TO_CLOSE{0.5f, 0.2f, 0.4f};
static const float3 LOOK_UP_CLOSE{0.1f, 0.0f, 1.0f};

static CameraOptions movingOptions() {
  auto options{openOptions()};
  options.motion = true;
  options.lookFromClose = LOOK_FROM_CLOSE;
  options.lookToClose = LOOK_TO_CLOSE;
  options.lookUpClose = LOOK_UP_CLOSE;
  return options;
}

// The same pixel and the same sampler state every time, so that two
// cameras differ only by what they do with the draw.
static CameraSample rayAt(const Camera &camera, float u) {
  Sampler sampler{};
  sampler.startPixelSample(1234, 5);
  auto sample{camera.sample(17, 9, sampler)};
  camera.toWorld(sample, u);
  return sample;
}

static bool sameVector(const float3 &a, const float3 &b) {
  return a.x == b.x && a.y == b.y && a.z == b.z;
}

static bool sameRay(const Ray &a, const Ray &b) {
  return sameVector(a.org, b.org) && sameVector(a.dir, b.dir) &&
         a.tmin == b.tmin && a.tmax == b.tmax;
}

TEST_CASE("Camera: a still camera places its ray the same at every fraction") {
  auto options{openOptions()};
  SUBCASE("Pinhole") {}
  SUBCASE("Thin lens") { options.fStop = 2.8f; }
  const Camera camera{options};
  const auto r0{rayAt(camera, 0.0f)};
  const auto r1{rayAt(camera, 0.3f)};
  const auto r2{rayAt(camera, 1.0f)};
  CHECK(sameRay(r0.ray, r1.ray));
  CHECK(sameRay(r0.ray, r2.ray));
  CHECK(r0.ray.time == 0.0f);
  CHECK(r1.ray.time == 0.3f);
  CHECK(r2.ray.time == 1.0f);
  CHECK(length(r0.ray.dir) == doctest::Approx(1.0f));
}

TEST_CASE("Camera: a moving camera reproduces its keys at the shutter ends") {
  const Camera moving{movingOptions()};
  const Camera stillOpen{openOptions()};
  auto closeOptions{openOptions()};
  closeOptions.lookFrom = LOOK_FROM_CLOSE;
  closeOptions.lookTo = LOOK_TO_CLOSE;
  closeOptions.lookUp = LOOK_UP_CLOSE;
  const Camera stillClose{closeOptions};
  CHECK(sameRay(rayAt(moving, 0.0f).ray, rayAt(stillOpen, 0.0f).ray));
  CHECK(sameRay(rayAt(moving, 1.0f).ray, rayAt(stillClose, 1.0f).ray));
  // The keys differ, so the two ends do.
  CHECK(!sameRay(rayAt(moving, 0.0f).ray, rayAt(moving, 1.0f).ray));
}

TEST_CASE("Camera: halfway through the shutter is the camera of the mid keys") {
  const Camera moving{movingOptions()};
  auto midOptions{openOptions()};
  midOptions.lookFrom = 0.5f * (midOptions.lookFrom + LOOK_FROM_CLOSE);
  midOptions.lookTo = 0.5f * (midOptions.lookTo + LOOK_TO_CLOSE);
  midOptions.lookUp = 0.5f * (midOptions.lookUp + LOOK_UP_CLOSE);
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
  options.motion = true;
  options.lookFromClose = options.lookFrom;
  options.lookToClose = options.lookTo;
  options.lookUpClose = options.lookUp;
  const Camera still{openOptions()};
  const Camera notMoving{options};
  const auto a{rayAt(still, 0.3f)};
  const auto b{rayAt(notMoving, 0.3f)};
  CHECK(sameRay(a.ray, b.ray));
  CHECK(b.ray.time == 0.3f);
}
