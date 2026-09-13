#include "RenderFixtures.h"

#include <algorithm>
#include <memory>
#include <vector>

#include "embree4/rtcore_device.h"

#include "IO/CurvesFile.h"
#include "Scene/Curves.h"

// The Embree build of a groom, over the one thing the file format alone
// cannot check: that a moving groom reaches Embree as two time steps
// and that the axis the hit path reads agrees with the keys the file
// states. `CurvesFile.cc`'s own doctest covers the format and the basis
// math, which need no device.

namespace {

// One straight strand of four points along +y, at `keyCount` keys, each
// key a further metre along +x, so that the two keys are far apart and
// a swapped or dropped one shows up as a whole metre of error. Four
// points is the B-spline floor, and linear is the basis whose window is
// the two points either side of the segment.
[[nodiscard]] CurvesFile makeStrand(uint32_t keyCount) {
  CurvesFile groom{};
  groom.basis = CurvesFile::Basis::LINEAR;
  groom.keyTimes.clear();
  for (uint32_t key = 0; key < keyCount; key++)
    groom.keyTimes.push_back(float(key));
  groom.strandOffsets = {0, 4};
  for (uint32_t point = 0; point < 4; point++)
    for (uint32_t key = 0; key < keyCount; key++)
      groom.points.push_back(float4(float(key), float(point), 0.0f,
                                    0.01f + 0.001f * float(point)));
  return groom;
}

/// An Embree device for the duration of a scope.
class ScopedDevice final {
public:
  ScopedDevice() = default;
  ScopedDevice(const ScopedDevice &) = delete;
  ScopedDevice &operator=(const ScopedDevice &) = delete;
  ~ScopedDevice() { rtcReleaseDevice(device); }

  RTCDevice device{rtcNewDevice(nullptr)};
};

} // namespace

TEST_CASE("Curves: a groom at the two instants of the shutter") {
  ScopedDevice embree{};
  const CurvesSpec spec{};
  SUBCASE("A still groom builds one key") {
    const std::unique_ptr<Curves> groom{makeCurves(embree.device, makeStrand(1),
                                                   spec, 0, false,
                                                   MotionSampling(0.0f, 1.0f))};
    CHECK(!groom->moves());
    CHECK(groom->pointsShut.empty());
    CHECK(groom->segCount() == 3);
    // The time is then not consulted: every instant reports one axis.
    CHECK(groom->axisAt(0, 0.0f, 0.0f).point.x == 0.0f);
    CHECK(groom->axisAt(0, 0.0f, 1.0f).point.x == 0.0f);
  }
  SUBCASE("A moving groom builds both keys") {
    const std::unique_ptr<Curves> groom{makeCurves(embree.device, makeStrand(2),
                                                   spec, 0, false,
                                                   MotionSampling(0.0f, 1.0f))};
    REQUIRE(groom->moves());
    REQUIRE(groom->pointsShut.size() == groom->points.size());
    // The ends of the shutter are the file's own keys, exactly.
    CHECK(groom->axisAt(0, 0.0f, 0.0f).point.x == 0.0f);
    CHECK(groom->axisAt(0, 0.0f, 1.0f).point.x == 1.0f);
    // And halfway is halfway, which is what Embree's per-vertex lerp
    // does to the window before it evaluates the basis.
    CHECK(groom->axisAt(0, 0.0f, 0.5f).point.x == doctest::Approx(0.5f));
    // The position along the strand is the key's, not the shutter's.
    CHECK(groom->axisAt(1, 0.5f, 0.0f).point.y == doctest::Approx(1.5f));
    CHECK(groom->axisAt(1, 0.5f, 1.0f).point.y == doctest::Approx(1.5f));
  }
  SUBCASE("A shut shutter holds the groom at the open key") {
    const std::unique_ptr<Curves> groom{makeCurves(embree.device, makeStrand(2),
                                                   spec, 0, false,
                                                   MotionSampling(0.0f, 0.0f))};
    CHECK(!groom->moves());
    CHECK(groom->axisAt(0, 0.0f, 1.0f).point.x == 0.0f);
  }
  SUBCASE("A shut key that restates the open one is no key") {
    CurvesFile file{makeStrand(2)};
    for (uint32_t point = 0; point < 4; point++)
      file.points[2 * point + 1] = file.points[2 * point];
    const std::unique_ptr<Curves> groom{
        makeCurves(embree.device, std::move(file), spec, 0, false,
                   MotionSampling(0.0f, 1.0f))};
    CHECK(!groom->moves());
  }
  SUBCASE("The shutter samples the track, not the keys") {
    // Three keys a second apart, with the shutter open over the middle
    // one: the two samples are the first and last, and the key between
    // them is the one the loader warns about rather than renders.
    CurvesFile file{makeStrand(3)};
    const std::unique_ptr<Curves> groom{
        makeCurves(embree.device, std::move(file), spec, 0, false,
                   MotionSampling(0.0f, 2.0f))};
    REQUIRE(groom->moves());
    CHECK(groom->axisAt(0, 0.0f, 0.0f).point.x == 0.0f);
    CHECK(groom->axisAt(0, 0.0f, 1.0f).point.x == 2.0f);
  }
  SUBCASE("Both keys frame the proxy points") {
    const std::unique_ptr<Curves> groom{makeCurves(embree.device, makeStrand(2),
                                                   spec, 0, false,
                                                   MotionSampling(0.0f, 1.0f))};
    // The corners are inflated by the fattest radius, which is the
    // tip's 0.013; what matters is that the span reaches the shut key's
    // metre rather than stopping at the open key.
    float minX{+INF}, maxX{-INF};
    for (const auto &point : groom->proxyPoints) {
      minX = std::min(minX, point.x);
      maxX = std::max(maxX, point.x);
    }
    CHECK(minX == doctest::Approx(-0.013f));
    CHECK(maxX == doctest::Approx(1.013f));
  }
  SUBCASE("The radius scale reaches both keys") {
    CurvesSpec scaled{spec};
    scaled.radiusScale = 10.0f;
    const std::unique_ptr<Curves> groom{makeCurves(embree.device, makeStrand(2),
                                                   scaled, 0, false,
                                                   MotionSampling(0.0f, 1.0f))};
    REQUIRE(groom->moves());
    CHECK(groom->points[0].w == doctest::Approx(0.1f));
    CHECK(groom->pointsShut[0].w == doctest::Approx(0.1f));
  }
}
