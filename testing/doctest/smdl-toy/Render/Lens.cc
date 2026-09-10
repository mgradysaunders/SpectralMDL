#include "Fixtures.h"

#include <cmath>
#include <optional>

#include "smdl/Support/Error.h"

#include "Render/Lens.h"

namespace {
// A surface, spelled the way the file spells one: millimeters, radius
// signed toward the film, index of the space behind it.
LensSurface surfaceOf(float radius, float thickness, float ior,
                      float diameter) {
  auto surface{LensSurface{}};
  surface.radius = radius;
  surface.thickness = thickness;
  surface.ior = ior;
  surface.diameter = diameter;
  return surface;
}

LensSurface stopOf(float thickness, float diameter) {
  auto surface{surfaceOf(0, thickness, 1, diameter)};
  surface.isStop = true;
  return surface;
}

// A biconvex lens of equal radii in air, with the stop against its back.
//
// The thin limit is the one case with a closed form for every paraxial
// quantity, so it is what that arithmetic is checked against before any
// real design. It is not a solid, though: at zero thickness the two
// surfaces cross everywhere off the axis, so the ray trace needs a
// thickness greater than twice the sag at the rim.
LensPrescription equiconvex(float radius, float thickness, float ior,
                            float diameter) {
  auto lens{LensPrescription{}};
  lens.name = "equiconvex";
  lens.surfaces.push_back(surfaceOf(radius, thickness, ior, diameter));
  lens.surfaces.push_back(surfaceOf(-radius, 0, 1, diameter));
  lens.surfaces.push_back(stopOf(0, diameter));
  return lens;
}

// The double Gauss pbrt distributes, from US 2,673,491 (Tronnier) by way
// of Modern Lens Design p.312, scaled to 50 mm. A real design whose
// focal length and f-number are printed on it, which is what makes it
// the transcription check.
LensPrescription dgauss50mm() {
  auto lens{LensPrescription{}};
  lens.name = "Double Gauss 50mm f/2";
  lens.surfaces = {
      surfaceOf(29.475f, 3.76f, 1.67f, 25.2f),
      surfaceOf(84.83f, 0.12f, 1, 25.2f),
      surfaceOf(19.275f, 4.025f, 1.67f, 23.0f),
      surfaceOf(40.77f, 3.275f, 1.699f, 23.0f),
      surfaceOf(12.75f, 5.705f, 1, 18.0f),
      stopOf(4.5f, 17.1f),
      surfaceOf(-14.495f, 1.18f, 1.603f, 17.0f),
      surfaceOf(40.77f, 6.065f, 1.658f, 20.0f),
      surfaceOf(-20.385f, 0.19f, 1, 20.0f),
      surfaceOf(437.065f, 3.22f, 1.717f, 20.0f),
      surfaceOf(-39.73f, 0, 1, 20.0f),
  };
  return lens;
}

// Building a lens throws, and the shared assertion macros speak in
// `std::optional<Error>`; this is the adapter between the two.
std::optional<smdl::Error> buildLens(const LensPrescription &prescription,
                                     const LensOptions &options) {
  return smdl::catchAndReturnError(
      [&] { [[maybe_unused]] const Lens lens{prescription, options}; });
}

// Millimeters, since that is what every published number is in.
constexpr float MM = 1e-3f;

// A focus distance of zero is focus at infinity, which is where most of
// these start from because it is the one film position with a closed
// form: the rear focal point.
constexpr float AT_INFINITY = 0.0f;
} // namespace

TEST_CASE("Lens: the paraxial solve against closed forms") {
  SUBCASE("A thin equiconvex lens has the focal length the makers' equation "
          "gives it") {
    // 1/f = (n - 1) (1/R1 - 1/R2) = 2 (n - 1) / R, exactly, at zero
    // thickness.
    const Lens lens{equiconvex(50, 0, 1.5f, 20), {AT_INFINITY, 0}};
    CHECK(lens.focalLength() == doctest::Approx(50 * MM).epsilon(1e-5));
  }
  SUBCASE("Its principal planes are at the lens, so the film lands one focal "
          "length behind it") {
    const Lens lens{equiconvex(50, 0, 1.5f, 20), {AT_INFINITY, 0}};
    CHECK(lens.backFocalDistance() ==
          doctest::Approx(lens.focalLength()).epsilon(1e-5));
  }
  SUBCASE("A thick one follows the thick lens equation") {
    constexpr float R = 50, T = 6, N = 1.5f;
    const auto expected{1 / ((N - 1) * (2 / R - (N - 1) * T / (N * R * R)))};
    const Lens lens{equiconvex(R, T, N, 20), {AT_INFINITY, 0}};
    CHECK(lens.focalLength() == doctest::Approx(expected * MM).epsilon(1e-4));
    // The rear principal plane moves forward into the glass, so the back
    // focus falls short of the focal length by exactly that much.
    const auto shift{expected * (N - 1) * T / (N * R)};
    CHECK(lens.backFocalDistance() ==
          doctest::Approx((expected - shift) * MM).epsilon(1e-4));
  }
  SUBCASE("A stop against a thin lens is its own entrance pupil") {
    const Lens lens{equiconvex(50, 0, 1.5f, 20), {AT_INFINITY, 0}};
    CHECK(lens.entrancePupilRadius() == doctest::Approx(10 * MM));
    CHECK(lens.fNumberWideOpen() == doctest::Approx(2.5f).epsilon(1e-4));
  }
}

TEST_CASE("Lens: focus places the film and nothing else") {
  SUBCASE("At infinity the film sits on the rear focal point") {
    const Lens lens{dgauss50mm(), {AT_INFINITY, 0}};
    CHECK(lens.filmZ() - lens.rearZ() ==
          doctest::Approx(lens.backFocalDistance()).epsilon(1e-5));
  }
  SUBCASE("A thin lens racks out by exactly the Newton extension") {
    // Its principal planes and its pupil all sit at the lens, so the
    // object distance needs no correction and the extension beyond the
    // rear focal point is f^2 / (distance - f) with nothing left over.
    const auto atInfinity{Lens{equiconvex(50, 0, 1.5f, 20), {AT_INFINITY, 0}}};
    const auto close{Lens{equiconvex(50, 0, 1.5f, 20), {1.0f, 0}}};
    const auto f{atInfinity.focalLength()};
    CHECK(close.filmZ() - atInfinity.filmZ() ==
          doctest::Approx(f * f / (1.0f - f)).epsilon(1e-4));
  }
  SUBCASE("A thick one holds the Newton invariant, which fixes its focal "
          "points") {
    // x x' = f^2 on both sides of the lens, so two focus distances give
    // two readings of where the front focal point is, and a solve that
    // is only approximately conjugate makes them disagree.
    const auto atInfinity{Lens{dgauss50mm(), {AT_INFINITY, 0}}};
    const auto near{Lens{dgauss50mm(), {2.0f, 0}}};
    const auto far{Lens{dgauss50mm(), {5.0f, 0}}};
    const auto f{atInfinity.focalLength()};
    const auto rearFocalZ{atInfinity.filmZ()};
    const auto frontFocalFrom{[&](const Lens &lens, float distance) {
      return f * f / (lens.filmZ() - rearFocalZ) - distance;
    }};
    CHECK(near.filmZ() > rearFocalZ);
    CHECK(frontFocalFrom(near, 2.0f) ==
          doctest::Approx(frontFocalFrom(far, 5.0f)).epsilon(1e-3));
  }
  SUBCASE("A focus distance inside the front focal point is an error") {
    CHECK_ERROR(buildLens(dgauss50mm(), {0.01f, 0}), "cannot focus at");
  }
  SUBCASE("A focus distance of zero is focus at infinity, not an error") {
    const Lens lens{dgauss50mm(), {0, 0}};
    CHECK(lens.filmZ() - lens.rearZ() ==
          doctest::Approx(lens.backFocalDistance()).epsilon(1e-6));
  }
  SUBCASE("A negative focus distance is an error") {
    CHECK_ERROR(buildLens(dgauss50mm(), {-1, 0}), "nonnegative focus distance");
  }
}

TEST_CASE("Lens: the f-number is a statement about the entrance pupil") {
  SUBCASE("Wide open it is the focal length over the pupil the stop makes") {
    const Lens lens{dgauss50mm(), {AT_INFINITY, 0}};
    CHECK(lens.fNumber() == doctest::Approx(lens.fNumberWideOpen()));
    CHECK(lens.fNumber() == doctest::Approx(lens.focalLength() /
                                            (2 * lens.entrancePupilRadius())));
  }
  SUBCASE("Stopping down narrows the pupil to exactly what was asked") {
    const Lens lens{dgauss50mm(), {AT_INFINITY, 5.6f}};
    CHECK(lens.fNumber() == doctest::Approx(5.6f).epsilon(1e-4));
    CHECK(2 * lens.entrancePupilRadius() ==
          doctest::Approx(lens.focalLength() / 5.6f).epsilon(1e-4));
  }
  SUBCASE("The physical stop narrows with it, by the pupil magnification") {
    const auto open{Lens{dgauss50mm(), {AT_INFINITY, 0}}};
    const auto shut{Lens{dgauss50mm(), {AT_INFINITY, 4.0f}}};
    const auto ratio{open.fNumberWideOpen() / 4.0f};
    CHECK(
        shut.elements()[shut.stopIndex()].semiDiameter ==
        doctest::Approx(open.elements()[open.stopIndex()].semiDiameter * ratio)
            .epsilon(1e-4));
  }
  SUBCASE("Nothing else moves when the lens is stopped down") {
    const auto open{Lens{dgauss50mm(), {AT_INFINITY, 0}}};
    const auto shut{Lens{dgauss50mm(), {AT_INFINITY, 8.0f}}};
    CHECK(shut.focalLength() == doctest::Approx(open.focalLength()));
    CHECK(shut.filmZ() == doctest::Approx(open.filmZ()));
    CHECK(shut.exitPupilZ() == doctest::Approx(open.exitPupilZ()));
  }
  SUBCASE("Asking for more light than the stop passes is an error") {
    CHECK_ERROR(buildLens(dgauss50mm(), {AT_INFINITY, 1.4f}),
                "cannot open the lens");
  }
}

TEST_CASE("Lens: the camera-space origin is the entrance pupil") {
  const Lens lens{dgauss50mm(), {AT_INFINITY, 0}};
  SUBCASE("The glass stands in front of it and the film behind it") {
    CHECK(lens.frontZ() < 0);
    CHECK(lens.rearZ() > 0);
    CHECK(lens.filmZ() > lens.rearZ());
  }
  SUBCASE("Surfaces run front to film in order, and the stop is among them") {
    const auto elements{lens.elements()};
    REQUIRE(elements.size() == 11);
    for (size_t i = 1; i < elements.size(); i++)
      CHECK(elements[i].z >= elements[i - 1].z);
    CHECK(lens.stopIndex() == 5);
    CHECK(elements[5].isStop);
  }
  SUBCASE("The index carries across the stop, which ends no space") {
    const auto elements{lens.elements()};
    CHECK(elements[4].iorAfter == doctest::Approx(1.0f));
    CHECK(elements[5].iorBefore == doctest::Approx(1.0f));
    CHECK(elements[5].iorAfter == doctest::Approx(1.0f));
    CHECK(elements[6].iorBefore == doctest::Approx(1.0f));
    CHECK(elements[0].iorAfter == doctest::Approx(1.67f));
    CHECK(elements[1].iorBefore == doctest::Approx(1.67f));
  }
}

TEST_CASE("Lens: a transcribed design reproduces what is printed on it") {
  SUBCASE("The double Gauss is a 50mm f/2") {
    const Lens lens{dgauss50mm(), {AT_INFINITY, 0}};
    CHECK(lens.focalLength() / MM == doctest::Approx(50.0f).epsilon(0.01));
    CHECK(lens.fNumberWideOpen() == doctest::Approx(2.0f).epsilon(0.02));
    // Its own summary is the line a user reads to see this; run it once
    // so that a format that stopped compiling is caught here.
    lens.logSummary();
  }
  SUBCASE("A design that states its back focus is checked against the solve") {
    auto lens{dgauss50mm()};
    lens.surfaces.back().thickness = 36.106f;
    const Lens built{lens, {AT_INFINITY, 0}};
    CHECK(built.designBackFocus() / MM == doctest::Approx(36.106f));
    CHECK(built.backFocalDistance() ==
          doctest::Approx(built.designBackFocus()).epsilon(1e-3));
  }
}

namespace {
// A ray leaving `film` aimed at the point `(x, y)` of the plane the
// camera draws its pupil points on, which is how the camera builds one.
Ray rayToPupil(const Lens &lens, float3 film, float x, float y) {
  return Ray{film, float3(x, y, lens.rearZ()) - film, EPS, INF};
}
} // namespace

TEST_CASE("Lens: tracing a ray from the film") {
  // 4 mm of glass across a 20 mm face of 50 mm radii leaves about 2 mm of
  // edge, so this one is a solid the whole way out to its rim.
  const Lens lens{equiconvex(50, 4, 1.5f, 20), {1.0f, 0}};
  const float3 film{1 * MM, 2 * MM, lens.filmZ()};
  SUBCASE("It leaves as a unit vector, from the front element, going the way "
          "the camera looks") {
    auto ray{rayToPupil(lens, film, 2 * MM, 0)};
    REQUIRE(lens.traceFromFilm(ray));
    CHECK(length(ray.dir) == doctest::Approx(1.0f).epsilon(1e-5));
    CHECK(ray.dir.z < 0);
    CHECK(ray.org.z == doctest::Approx(lens.frontZ()).epsilon(1e-4));
  }
  SUBCASE("Rays through different pupil points meet again on the plane the "
          "lens is focused at, which is what focus means") {
    const auto imageOf{[&](float x, float y) {
      auto ray{rayToPupil(lens, film, x, y)};
      REQUIRE(lens.traceFromFilm(ray));
      return ray((-1.0f - ray.org.z) / ray.dir.z);
    }};
    // The chief ray, through the middle of the pupil, says where; the
    // others have to agree with it, which is the whole of what a lens
    // does and needs no formula to state. They agree to the aberrations
    // and no further: this singlet is fast and the film point is off
    // axis, so a rim ray lands a few hundredths of a millimeter from the
    // chief ray on an image 40 mm across. That is the lens being a poor
    // one; a trace with a root or a sign wrong misses by millimeters.
    const auto center{imageOf(0, 0)};
    CHECK_NEAR(imageOf(0.5f * MM, 0), center, 1e-4f);
    CHECK_NEAR(imageOf(0, -0.5f * MM), center, 1e-4f);
    // The image on the film is inverted, so the two run opposite.
    CHECK(center.x < 0);
    CHECK(center.y < 0);
  }
  SUBCASE("A ray outside a clear aperture is blocked") {
    auto ray{rayToPupil(lens, film, 20 * MM, 0)};
    CHECK_FALSE(lens.traceFromFilm(ray));
  }
}

TEST_CASE("Lens: the stop is what the trace tests, not the prescription") {
  const auto stopped{Lens{equiconvex(50, 4, 1.5f, 20), {1.0f, 25.0f}}};
  const float3 film{0, 0, stopped.filmZ()};
  SUBCASE("Stopping down blocks the rays the wider stop passed") {
    const auto radius{stopped.elements()[stopped.stopIndex()].semiDiameter};
    auto inside{rayToPupil(stopped, film, 0.5f * radius, 0)};
    auto outside{rayToPupil(stopped, film, 2.0f * radius, 0)};
    CHECK(stopped.traceFromFilm(inside));
    CHECK_FALSE(stopped.traceFromFilm(outside));
  }
  SUBCASE("Blades cut the corners off the circle they carry the area of") {
    auto options{LensOptions{1.0f, 25.0f}};
    options.numBlades = 4;
    const Lens bladed{equiconvex(50, 4, 1.5f, 20), options};
    const auto radius{bladed.elements()[bladed.stopIndex()].semiDiameter};
    // A square of the circle's area reaches past it toward its vertices
    // and falls short of it toward the middle of an edge, so the same
    // radius passes one way and not the other.
    const auto reach{0.95f * radius};
    auto towardVertex{rayToPupil(bladed, film, reach, 0)};
    auto towardEdge{
        rayToPupil(bladed, film, reach * 0.70710678f, reach * 0.70710678f)};
    CHECK(bladed.traceFromFilm(towardVertex));
    CHECK_FALSE(bladed.traceFromFilm(towardEdge));
  }
}

TEST_CASE("Lens: prescriptions that cannot be a camera lens") {
  SUBCASE("One with no stop is refused") {
    auto lens{LensPrescription{}};
    lens.surfaces.push_back(surfaceOf(50, 0, 1.5f, 20));
    lens.surfaces.push_back(surfaceOf(-50, 0, 1, 20));
    CHECK_ERROR(buildLens(lens, {AT_INFINITY, 0}), "aperture stop");
  }
  SUBCASE("An empty one is refused") {
    CHECK_ERROR(buildLens(LensPrescription{}, {AT_INFINITY, 0}),
                "at least one surface");
  }
  SUBCASE("One that leaves the film in glass is refused") {
    // The last surface refracts into glass and nothing brings the light
    // back out, so the film would be immersed in it.
    auto lens{LensPrescription{}};
    lens.surfaces.push_back(stopOf(2, 20));
    lens.surfaces.push_back(surfaceOf(50, 0, 1.5f, 20));
    CHECK_ERROR(buildLens(lens, {AT_INFINITY, 0}),
                "expected air behind the last surface");
  }
  SUBCASE("One with no net power has no focal length") {
    auto lens{LensPrescription{}};
    // A plane parallel plate bends nothing, so it images nothing.
    lens.surfaces.push_back(surfaceOf(0, 4, 1.5f, 20));
    lens.surfaces.push_back(surfaceOf(0, 0, 1, 20));
    lens.surfaces.push_back(stopOf(0, 20));
    CHECK_ERROR(buildLens(lens, {AT_INFINITY, 0}), "no net power");
  }
}

namespace {
// The share of the rear aperture that a film point `filmRadius` off axis
// can see out through, by brute force over the whole of it.
double transmission(const Lens &lens, float filmRadius) {
  constexpr int NUM_STEPS = 512;
  const auto radius{lens.rearApertureRadius()};
  const float3 film{filmRadius, 0, lens.filmZ()};
  auto numPassed{0.0}, numInside{0.0};
  for (int i = 0; i < NUM_STEPS; i++) {
    const auto x{radius * (2 * (i + 0.5f) / NUM_STEPS - 1)};
    for (int j = 0; j < NUM_STEPS; j++) {
      const auto y{radius * (2 * (j + 0.5f) / NUM_STEPS - 1)};
      if (x * x + y * y > radius * radius) continue;
      numInside += 1;
      auto ray{rayToPupil(lens, film, x, y)};
      if (lens.traceFromFilm(ray)) numPassed += 1;
    }
  }
  return numPassed / numInside;
}

// The same share as the camera arrives at it: draw through the table and
// average the weight it reports over every draw, the blocked ones
// included. The two agreeing is the statement that narrowing the domain
// left the estimator alone, which holds exactly when the bound contains
// everything that gets out.
double transmissionThroughBound(const Lens &lens, const ExitPupil &pupil,
                                float filmRadius) {
  constexpr int NUM_STEPS = 512;
  const float3 film{filmRadius, 0, lens.filmZ()};
  auto total{0.0};
  for (int i = 0; i < NUM_STEPS; i++) {
    for (int j = 0; j < NUM_STEPS; j++) {
      auto weight{0.0f};
      const auto point{pupil.sample(
          float2(filmRadius, 0),
          float2((i + 0.5f) / NUM_STEPS, (j + 0.5f) / NUM_STEPS), weight)};
      if (!(weight > 0)) continue;
      auto ray{rayToPupil(lens, film, point.x, point.y)};
      if (lens.traceFromFilm(ray)) total += weight;
    }
  }
  return total / (NUM_STEPS * NUM_STEPS);
}

// Half the diagonal of a 36 by 24 mm frame, which is how far off axis a
// film point on a full-frame sensor can be.
const float FULL_FRAME_CORNER = 0.5f * std::hypot(36.0f, 24.0f) * MM;
} // namespace

TEST_CASE("ExitPupil: the domain the pupil point is drawn from") {
  const Lens lens{dgauss50mm(), {5.0f, 0}};
  const ExitPupil pupil{lens, FULL_FRAME_CORNER};
  SUBCASE("It carries the transmission of the whole aperture, which is what "
          "leaves the estimator alone") {
    for (const auto fraction : {0.0f, 0.25f, 0.5f, 0.75f, 1.0f}) {
      const auto filmRadius{fraction * FULL_FRAME_CORNER};
      CHECK(transmissionThroughBound(lens, pupil, filmRadius) ==
            doctest::Approx(transmission(lens, filmRadius)).epsilon(0.02));
    }
  }
  SUBCASE("It is smaller than the whole aperture, and smallest at the corner") {
    CHECK(pupil.areaFraction(0) < 1);
    CHECK(pupil.areaFraction(FULL_FRAME_CORNER) < pupil.areaFraction(0));
  }
  SUBCASE("A film point past the corner it was built for reads the last "
          "entry") {
    CHECK(pupil.areaFraction(100 * FULL_FRAME_CORNER) ==
          pupil.areaFraction(FULL_FRAME_CORNER));
  }
  SUBCASE("Every draw is inside the aperture or weighs nothing") {
    const auto radius{lens.rearApertureRadius()};
    auto numOutside{0};
    for (int i = 0; i < 64; i++) {
      for (int j = 0; j < 64; j++) {
        auto weight{0.0f};
        const auto point{pupil.sample(float2(FULL_FRAME_CORNER, 0),
                                      float2((i + 0.5f) / 64, (j + 0.5f) / 64),
                                      weight)};
        if (weight > 0 && lengthSquared(point) > radius * radius) numOutside++;
      }
    }
    CHECK(numOutside == 0);
  }
  SUBCASE("It turns with the film point's azimuth, the system being one of "
          "revolution") {
    auto weightOnX{0.0f}, weightOnY{0.0f};
    const auto xi{float2(0.3f, 0.7f)};
    const auto onX{pupil.sample(float2(FULL_FRAME_CORNER, 0), xi, weightOnX)};
    const auto onY{pupil.sample(float2(0, FULL_FRAME_CORNER), xi, weightOnY)};
    CHECK(weightOnY == doctest::Approx(weightOnX));
    CHECK(onY.x == doctest::Approx(-onX.y));
    CHECK(onY.y == doctest::Approx(onX.x));
  }
  SUBCASE("Stopping down narrows it, which is where the wasted draws went") {
    const Lens stopped{dgauss50mm(), {5.0f, 11.0f}};
    const ExitPupil stoppedPupil{stopped, FULL_FRAME_CORNER};
    CHECK(stoppedPupil.areaFraction(0) < 0.05f * pupil.areaFraction(0));
    CHECK(transmissionThroughBound(stopped, stoppedPupil, 0) ==
          doctest::Approx(transmission(stopped, 0)).epsilon(0.02));
  }
}
