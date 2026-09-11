#include "Fixtures.h"

#include "LensFixtures.h"

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
    value.medium = smdl::OpticalGlass::constant(ior);
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
  options.frameSize = float2(0.036f, 0.027f);
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

TEST_CASE("Camera: what a lens's film holds is the sensor's decision") {
  constexpr double PI_DOUBLE{3.14159265358979323846};
  const auto fNumber{fNumberOf(singlet())};
  SUBCASE("On the observer's film the lens is normalized to its f-number, so "
          "the frame holds its brightness however far it is stopped down") {
    auto options{lensOptions()};
    const auto wideOpen{meanWeight(Camera{options}, 32, 24)};
    options.fStop = 2 * fNumber;
    const auto stopped{meanWeight(Camera{options}, 32, 24)};
    CHECK(wideOpen == doctest::Approx(1.0).epsilon(0.2));
    CHECK(stopped == doctest::Approx(wideOpen).epsilon(0.1));
  }
  SUBCASE("On a physical sensor's film an ideal lens reads pi over four "
          "f-numbers squared, and a real one a little under") {
    auto options{lensOptions()};
    options.filmQuantity = FilmQuantity::IRRADIANCE;
    const auto ideal{PI_DOUBLE / (4 * fNumber * fNumber)};
    const auto middle{meanWeight(Camera{options}, 32, 24)};
    CHECK(middle < 1.02f * ideal);
    CHECK(middle > 0.8f * ideal);
  }
  SUBCASE("There, stopping down darkens by the square of the f-number") {
    auto options{lensOptions()};
    options.filmQuantity = FilmQuantity::IRRADIANCE;
    const auto wideOpen{meanWeight(Camera{options}, 32, 24)};
    options.fStop = 2 * fNumber;
    const auto stopped{meanWeight(Camera{options}, 32, 24)};
    CHECK(stopped == doctest::Approx(wideOpen / 4).epsilon(0.1));
  }
  SUBCASE("And a wider rear element changes nothing, the exposure being an "
          "integral over the pupil and not over the glass") {
    auto options{lensOptions()};
    options.filmQuantity = FilmQuantity::IRRADIANCE;
    const auto bare{meanWeight(Camera{options}, 32, 24)};
    options.lens = singletBehindWindow();
    const auto behindWindow{meanWeight(Camera{options}, 32, 24)};
    CHECK(behindWindow == doctest::Approx(bare).epsilon(0.02));
  }
  SUBCASE("The two films differ by exactly four f-numbers squared over pi, "
          "sample for sample") {
    auto options{lensOptions()};
    options.fStop = 2 * fNumber;
    const Camera observer{options};
    options.filmQuantity = FilmQuantity::IRRADIANCE;
    const Camera sensor{options};
    const double N{sensor.fNumber()};
    for (const auto pixel : {int2(32, 24), int2(60, 4), int2(4, 44)}) {
      const auto a{
          cameraSpaceSample(observer, size_t(pixel.x), size_t(pixel.y), 3)};
      const auto b{
          cameraSpaceSample(sensor, size_t(pixel.x), size_t(pixel.y), 3)};
      CHECK(a.weight ==
            doctest::Approx(b.weight * 4 * N * N / PI_DOUBLE).epsilon(1e-5));
    }
  }
}

TEST_CASE("Camera: the frame's size and the f-number") {
  SUBCASE("A lens reports its own f-number, and the frame it was given") {
    const Camera camera{lensOptions()};
    CHECK(camera.fNumber() == doctest::Approx(fNumberOf(singlet())));
    CHECK(camera.frameSize().x == doctest::Approx(0.036f));
    CHECK(camera.frameSize().y == doctest::Approx(0.027f));
  }
  SUBCASE("The thin lens spans the frame it was given, full frame by "
          "default, and its f-number is what set the aperture") {
    auto options{openOptions()};
    options.fStop = 2.8f;
    const Camera fullFrame{options};
    CHECK(fullFrame.frameSize().x == 1e-3f * 36.0f);
    CHECK(fullFrame.frameSize().y == 1e-3f * 24.0f);
    CHECK(fullFrame.fNumber() == doctest::Approx(2.8f));
    options.frameSize = float2(7.68e-3f, 5.76e-3f);
    const Camera phone{options};
    CHECK(phone.frameSize().x == doctest::Approx(0.00768f));
    CHECK(phone.fNumber() == doctest::Approx(2.8f));
    // The same f-number on a frame a quarter the height is a lens a
    // quarter the radius: the lens point of one draw scales with it.
    const auto onFullFrame{rayAt(fullFrame, 0.0f)};
    const auto onPhone{rayAt(phone, 0.0f)};
    CHECK(length(onPhone.ray.org - openOptions().lookFrom) ==
          doctest::Approx(
              0.24 * length(onFullFrame.ray.org - openOptions().lookFrom)));
  }
  SUBCASE("An aperture radius has an f-number too, and a pinhole none") {
    auto options{openOptions()};
    const float focalLength{0.5f /
                            std::tan(smdl::radians(options.fovYDeg / 2))};
    options.aperture = 0.5f * 0.024f * focalLength / 4.0f;
    const Camera stated{options};
    CHECK(stated.fNumber() == doctest::Approx(4.0f));
    const Camera pinhole{openOptions()};
    CHECK(pinhole.fNumber() == 0.0f);
  }
  SUBCASE("On the default frame the thin lens's aperture is what it always "
          "was") {
    // The frame height the f-number is a fraction of is spelled as the
    // same float the old constant was, so a render that names no body
    // draws the same lens point bit for bit.
    CHECK(1e-3f * 24.0f == 0.024f);
    auto byFStop{openOptions()};
    byFStop.fStop = 2.8f;
    auto byRadius{openOptions()};
    const float focalLength{0.5f /
                            std::tan(smdl::radians(byRadius.fovYDeg / 2))};
    byRadius.aperture = 0.5f * 0.024f * focalLength / 2.8f;
    CHECK(isSameRay(rayAt(Camera{byFStop}, 0.0f).ray,
                    rayAt(Camera{byRadius}, 0.0f).ray));
  }
}

TEST_CASE("Camera: the thin lens on a physical sensor's film") {
  constexpr double PI_DOUBLE{3.14159265358979323846};
  auto options{openOptions()};
  options.filmQuantity = FilmQuantity::IRRADIANCE;
  SUBCASE("A pinhole has no pupil to integrate over, and is refused") {
    CHECK_ERROR(smdl::catchAndReturnError(
                    [&] { [[maybe_unused]] const Camera camera{options}; }),
                "a pinhole has none");
  }
  SUBCASE("Focused far, the middle of the frame reads the exact disk "
          "integral, pi over four f-numbers squared plus one") {
    options.fStop = 2.8f;
    options.focus = 1000.0f;
    const Camera camera{options};
    // Each sample carries the cos^4 of its own pupil point, so one draw
    // reads under the paraxial constant and the mean over the pupil
    // reads the exact integral, which at f/2.8 is 3% under it.
    const auto middle{meanWeight(camera, 32, 24)};
    CHECK(middle ==
          doctest::Approx(PI_DOUBLE / (4 * 2.8 * 2.8 + 1)).epsilon(0.01));
    CHECK(cameraSpaceSample(camera, 32, 24, 3).weight <
          PI_DOUBLE / (4 * 2.8 * 2.8));
  }
  SUBCASE("Focused near, the bellows factor darkens the whole frame") {
    options.fStop = 2.8f;
    options.focus = 1000.0f;
    const auto far{cameraSpaceSample(Camera{options}, 32, 24, 3).weight};
    // A focal length of about 35 mm focused at 0.35 m puts the film a
    // tenth further back, which costs a fifth of the light.
    options.focus = 0.35f;
    const auto near{cameraSpaceSample(Camera{options}, 32, 24, 3).weight};
    CHECK(near < far);
    CHECK(near == doctest::Approx(far / 1.21).epsilon(0.05));
  }
  SUBCASE("The corner reads less than the middle, by the cos^4 of its own "
          "segment, with 'vignetting' not applied on top") {
    options.fStop = 2.8f;
    options.focus = 1000.0f;
    const Camera plain{options};
    options.vignetting = 1.0f;
    const Camera vignetted{options};
    const auto middle{cameraSpaceSample(plain, 32, 24, 3).weight};
    const auto corner{cameraSpaceSample(plain, 1, 1, 3).weight};
    CHECK(corner < middle);
    CHECK(corner > 0.5f * middle);
    CHECK(cameraSpaceSample(vignetted, 1, 1, 3).weight == corner);
  }
  SUBCASE("A focus inside the focal length cannot be imaged") {
    options.fStop = 2.8f;
    options.focus = 0.01f;
    CHECK_ERROR(smdl::catchAndReturnError(
                    [&] { [[maybe_unused]] const Camera camera{options}; }),
                "inside its focal length");
  }
}

// The focus and the depth of field: the closed forms against hand
// values, infinity as a focus, and the natural vignetting in the units
// the lens point is drawn in.

TEST_CASE("Camera: the depth of field") {
  const float2 fullFrame{0.036f, 0.024f};
  SUBCASE("50 mm at f/8 focused at 5 m, against the tables") {
    const auto dof{depthOfField(0.05f, 8.0f, 5.0f, fullFrame)};
    CHECK(dof.hasLimits());
    CHECK(dof.circleOfConfusion == doctest::Approx(2.8844e-5f).epsilon(1e-3));
    CHECK(dof.hyperfocal == doctest::Approx(10.884f).epsilon(1e-3));
    CHECK(dof.nearLimit == doctest::Approx(3.432f).epsilon(1e-3));
    CHECK(dof.farLimit == doctest::Approx(9.206f).epsilon(1e-3));
  }
  SUBCASE("Focused at infinity, sharp from the hyperfocal less one focal "
          "length") {
    const auto dof{depthOfField(0.05f, 8.0f, INF, fullFrame)};
    CHECK(dof.nearLimit == doctest::Approx(10.834f).epsilon(1e-3));
    CHECK(dof.farLimit == INF);
  }
  SUBCASE("Focused past the hyperfocal, the far limit is infinity") {
    const auto dof{depthOfField(0.05f, 8.0f, 20.0f, fullFrame)};
    CHECK(dof.nearLimit == doctest::Approx(7.039f).epsilon(1e-3));
    CHECK(dof.farLimit == INF);
    const auto atHyperfocal{
        depthOfField(0.05f, 8.0f, dof.hyperfocal, fullFrame)};
    CHECK(atHyperfocal.farLimit == INF);
  }
  SUBCASE("A pinhole has none") {
    const auto dof{depthOfField(0.05f, 0.0f, 5.0f, fullFrame)};
    CHECK(!dof.hasLimits());
    CHECK(dof.nearLimit == INF);
  }
  SUBCASE("The camera reports its own, from what it resolved") {
    auto options{openOptions()};
    options.frameSize = fullFrame;
    options.fStop = 8.0f;
    options.focus = 5.0f;
    const Camera thin{options};
    CHECK(thin.focalLength() ==
          doctest::Approx(0.024f * 0.5f /
                          std::tan(smdl::radians(options.fovYDeg / 2))));
    const auto dof{thin.depthOfField()};
    const auto byHand{depthOfField(thin.focalLength(), 8.0f, 5.0f, fullFrame)};
    CHECK(dof.nearLimit == byHand.nearLimit);
    CHECK(dof.farLimit == byHand.farLimit);
    auto lensed{lensOptions()};
    lensed.focus = 5.0f;
    const Camera traced{lensed};
    CHECK(traced.depthOfField().hyperfocal ==
          depthOfField(traced.focalLength(), traced.fNumber(), 5.0f,
                       lensed.frameSize)
              .hyperfocal);
  }
}

TEST_CASE("Camera: the share of a frame an image circle leaves dark") {
  SUBCASE("None once the circle reaches the corners, and all without one") {
    CHECK(darkShareOfFrame(float2(0.036f, 0.024f), 0.0217f) == 0.0f);
    CHECK(darkShareOfFrame(float2(0.036f, 0.024f), 0.0f) == 1.0f);
  }
  SUBCASE("A circle inside the frame lights its own area and no more") {
    CHECK(darkShareOfFrame(float2(4.0f, 2.0f), 0.5f) ==
          doctest::Approx(1.0 - PI * 0.25 / 8.0).epsilon(1e-5));
  }
  SUBCASE("The circle inscribed in a square leaves the corners, 1 - pi / 4") {
    CHECK(darkShareOfFrame(float2(2.0f, 2.0f), 1.0f) ==
          doctest::Approx(1.0 - PI / 4.0).epsilon(1e-5));
  }
  SUBCASE("A circle past the short sides and short of the corners leaves "
          "what a count of the frame's points does") {
    const float2 frame{3.0f, 2.0f};
    const float radius{1.3f};
    constexpr int N{1000};
    int numDark{};
    for (int i = 0; i < N; i++) {
      for (int j = 0; j < N; j++) {
        const float x{frame.x * ((float(i) + 0.5f) / N - 0.5f)};
        const float y{frame.y * ((float(j) + 0.5f) / N - 0.5f)};
        numDark += x * x + y * y > radius * radius;
      }
    }
    CHECK(darkShareOfFrame(frame, radius) ==
          doctest::Approx(double(numDark) / (N * N)).epsilon(2e-3));
  }
}

TEST_CASE("Camera: focus at infinity") {
  auto options{openOptions()};
  options.fStop = 2.0f;
  options.focus = INF;
  SUBCASE("The thin lens sends the rays through every lens point out "
          "parallel, along the pinhole's") {
    const Camera camera{options};
    const Camera pinhole{openOptions()};
    CHECK(std::isinf(camera.focusDistance()));
    // The same draw jitters the pixel the same way, so each lens point's
    // ray runs along the pinhole's for that draw.
    for (uint32_t sampleIndex = 0; sampleIndex < 4; sampleIndex++) {
      const auto a{cameraSpaceSample(camera, 17, 9, sampleIndex)};
      const auto b{cameraSpaceSample(pinhole, 17, 9, sampleIndex)};
      CHECK(length(a.ray.org) > 0);
      CHECK(isSame(normalize(a.ray.dir), normalize(b.ray.dir)));
    }
    CHECK(!isSame(cameraSpaceSample(camera, 17, 9, 0).ray.org,
                  cameraSpaceSample(camera, 17, 9, 1).ray.org));
  }
  SUBCASE("Focused far, the rays converge on the focus plane instead") {
    options.focus = 1000.0f;
    const Camera camera{options};
    const auto a{cameraSpaceSample(camera, 17, 9, 0)};
    const auto b{cameraSpaceSample(camera, 17, 9, 1)};
    CHECK(!isSame(normalize(a.ray.dir), normalize(b.ray.dir)));
  }
  SUBCASE("On a physical sensor's film the image distance is the focal "
          "length, the limit of the far focus") {
    options.filmQuantity = FilmQuantity::IRRADIANCE;
    const auto atInfinity{cameraSpaceSample(Camera{options}, 32, 24, 3).weight};
    options.focus = 100000.0f;
    const auto far{cameraSpaceSample(Camera{options}, 32, 24, 3).weight};
    CHECK(atInfinity == doctest::Approx(far).epsilon(1e-4));
    CHECK(atInfinity > far);
  }
  SUBCASE("A lens focuses at infinity too") {
    auto lensed{lensOptions()};
    lensed.focus = INF;
    const Camera camera{lensed};
    CHECK(std::isinf(camera.focusDistance()));
    CHECK(passingSample(camera, 32, 24).weight > 0);
  }
}

TEST_CASE("Camera: the natural vignetting is taken in image heights") {
  constexpr double PI_DOUBLE{3.14159265358979323846};
  auto options{openOptions()};
  options.vignetting = 1.0f;
  SUBCASE("With a pinhole the lens point is nothing, and the corner reads "
          "the chief ray's cos^4") {
    const Camera camera{options};
    const float focalLength{0.5f /
                            std::tan(smdl::radians(options.fovYDeg / 2))};
    const auto middle{cameraSpaceSample(camera, 32, 24, 3)};
    const auto corner{cameraSpaceSample(camera, 0, 0, 3)};
    // Within the jitter of the middle pixel of 1.
    CHECK(middle.weight == doctest::Approx(1.0).epsilon(1e-3));
    const float2 image{-(corner.ray.dir.x), -(corner.ray.dir.y)};
    const float cosSquared{focalLength * focalLength /
                           (lengthSquared(image) + focalLength * focalLength)};
    CHECK(corner.weight == doctest::Approx(cosSquared * cosSquared));
  }
  SUBCASE("With an aperture the middle of the frame reads the disk mean of "
          "cos^4, which is not 1") {
    // Over a disk of radius R in image heights at a focal length f, the
    // mean of cos^4 is f^2 / (R^2 + f^2); at f/2.8 that is 4 N^2 over
    // 4 N^2 + 1, three percent under 1. A lens point taken in meters
    // would have read within a part in ten thousand of 1.
    options.fStop = 2.8f;
    options.focus = INF;
    const Camera camera{options};
    const auto middle{meanWeight(camera, 32, 24)};
    CHECK(middle ==
          doctest::Approx(4 * 2.8 * 2.8 / (4 * 2.8 * 2.8 + 1)).epsilon(0.005));
    CHECK(middle < 0.98f);
  }
  SUBCASE("It is the physical sensor's weight over the pupil constant, "
          "sample for sample, at infinity focus") {
    options.fStop = 2.8f;
    options.focus = INF;
    const Camera observer{options};
    options.filmQuantity = FilmQuantity::IRRADIANCE;
    const Camera sensor{options};
    const double R{0.5 * 0.024 * observer.focalLength() / 0.024 / 2.8};
    const double pupil{
        PI_DOUBLE * R * R /
        (double(observer.focalLength()) * double(observer.focalLength()))};
    for (const auto pixel : {int2(32, 24), int2(60, 4), int2(4, 44)}) {
      const auto a{
          cameraSpaceSample(observer, size_t(pixel.x), size_t(pixel.y), 3)};
      const auto b{
          cameraSpaceSample(sensor, size_t(pixel.x), size_t(pixel.y), 3)};
      CHECK(a.weight == doctest::Approx(b.weight / pupil).epsilon(1e-4));
    }
  }
}

TEST_CASE("Camera: the thin lens fitted to a lens") {
  auto options{openOptions()};
  options.lens = dgauss50mm();
  options.frameSize = float2(0.036f, 0.024f);
  options.focus = INF;
  const Lens lens{*options.lens, LensOptions{}};
  SUBCASE("On full frame its chief rays land within a sixth of a 6 um "
          "pixel, and the edge of its field is the lens's") {
    const auto fit{approximateLens(options)};
    CHECK(!fit.doesFold);
    CHECK(fit.numFittedRadii == 32);
    CHECK(fit.numDroppedRadii == 0);
    CHECK(fit.maxChiefRayError < 1e-6f);
    // The middle of the top edge through the thin lens's own map: the
    // half height over the focal length, stretched by the distortion at
    // that radius.
    const float focalLength{0.5f /
                            std::tan(smdl::radians(fit.options.fovYDeg / 2))};
    const float s{0.012f / (0.5f * std::hypot(0.036f, 0.024f))};
    const float stretch{
        1 +
        s * s * (fit.options.distortionK1 + s * s * fit.options.distortionK2)};
    CHECK(0.5f * stretch / focalLength ==
          doctest::Approx(std::tan(lens.fieldAngleAt(0.012f))).epsilon(1e-4));
  }
  SUBCASE("It takes the lens's entrance pupil and none of its vignetting") {
    const auto fit{approximateLens(options)};
    CHECK(!fit.options.lens);
    CHECK(fit.options.aperture == lens.entrancePupilRadius());
    CHECK(fit.options.fStop == 0);
    CHECK(fit.options.vignetting == 0);
    CHECK(fit.options.catEye == 0);
    CHECK(!fit.options.shouldFitDistortion);
    // So its f-number is the lens's, over the fitted focal length.
    CHECK(Camera{fit.options}.fNumber() ==
          doctest::Approx(lens.fNumber()).epsilon(0.005));
  }
  SUBCASE("Radii past the image circle are dropped") {
    options.frameSize = float2(0.06f, 0.04f);
    const auto fit{approximateLens(options)};
    CHECK(fit.numDroppedRadii > 0);
    CHECK(fit.numFittedRadii + fit.numDroppedRadii == 32);
  }
  SUBCASE("Too few radii to fit fall back to the paraxial pinhole") {
    // Two of the radii land inside the 55 mm circle of a frame this size.
    options.frameSize = float2(0.7f, 0.5f);
    const auto fit{approximateLens(options)};
    CHECK(fit.doesFold);
    CHECK(fit.options.distortionK1 == 0);
    CHECK(fit.options.distortionK2 == 0);
    CHECK(0.5f * 0.5f / std::tan(smdl::radians(fit.options.fovYDeg / 2)) ==
          doctest::Approx(lens.focalLength()).epsilon(1e-4));
  }
}
