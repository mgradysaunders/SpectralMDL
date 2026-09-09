#include "Fixtures.h"

#include <cmath>
#include <string>
#include <utility>

#include "Layout/Motion.h"

namespace {

[[nodiscard]] float4x4 rotationZ(float degrees) {
  const float radians{smdl::radians(degrees)};
  const float c{std::cos(radians)}, s{std::sin(radians)};
  return float4x4{float4(c, s, 0, 0), float4(-s, c, 0, 0), float4(0, 0, 1, 0),
                  float4(0, 0, 0, 1)};
}

[[nodiscard]] float4x4 translation(float x, float y, float z) {
  return float4x4{float4(1, 0, 0, 0), float4(0, 1, 0, 0), float4(0, 0, 1, 0),
                  float4(x, y, z, 1)};
}

const float4x4 SHEAR{
    float4(1.0f, 0.2f, 0.0f, 0.0f), float4(0.3f, 1.5f, 0.1f, 0.0f),
    float4(0.0f, -0.4f, 0.8f, 0.0f), float4(2.0f, -1.0f, 3.0f, 1.0f)};

} // namespace

TEST_CASE("Motion: the decomposition reassembles the transform") {
  auto mirrored{SHEAR};
  mirrored[2] = -mirrored[2];
  const std::pair<const char *, float4x4> keys[]{{"sheared", SHEAR},
                                                 {"mirrored", mirrored}};
  for (const auto &entry : keys) {
    const char *name{entry.first};
    CAPTURE(name);
    const auto parts{decomposeTransform(entry.second)};
    CHECK_NEAR(composeTransform(parts), entry.second, 2.0e-6f);
    CHECK(parts.translation.x == entry.second[3].x);
    CHECK(parts.scale.x > 0.0f);
    CHECK(parts.scale.y > 0.0f);
    // A mirrored key folds its reflection into the last scale rather
    // than into an improper rotation.
    CHECK((parts.scale.z < 0.0f) == (name == std::string("mirrored")));
    CHECK(smdl::length(parts.quaternion) == doctest::Approx(1.0f));
  }
}

TEST_CASE("Motion: the ends of an interpolation are the keys themselves") {
  const auto a{translation(1, 2, 3)};
  const auto b{rotationZ(90.0f)};
  CHECK_SAME(interpolateTransform(a, b, 0.0f), a);
  CHECK_SAME(interpolateTransform(a, b, 1.0f), b);
  CHECK_SAME(interpolateTransform(a, b, -1.0f), a);
  CHECK_SAME(interpolateTransform(a, b, 2.0f), b);
}

TEST_CASE("Motion: a turn slerps, so halfway through 90 degrees is 45") {
  const auto half{
      interpolateTransform(rotationZ(0.0f), rotationZ(90.0f), 0.5f)};
  CHECK_NEAR(half, rotationZ(45.0f), 1.0e-5f);
  // The chord a componentwise lerp would take is shorter than the arc,
  // so the object it places is smaller than the one the keys state.
  const float3 axis{float3(half[0])};
  CHECK(smdl::length(axis) == doctest::Approx(1.0f).epsilon(1.0e-5));
}

TEST_CASE("MotionTrack: an empty track says nothing") {
  const MotionTrack track{};
  CHECK(track.empty());
  CHECK_SAME(track.at(0.0f), float4x4(1.0f));
  CHECK(!track.hasKeyBetween(0.0f, 1.0f));
}

TEST_CASE("MotionTrack: a key's own time reproduces the key") {
  MotionTrack track{};
  track.keys.push_back({0.5f, translation(1, 0, 0)});
  track.keys.push_back({0.75f, SHEAR});
  track.keys.push_back({2.0f, rotationZ(30.0f)});
  // Bit for bit, not merely close: this is what keeps a two-key track
  // sampled at the shutter's own instants rendering what the keys say.
  for (const auto &key : track.keys)
    CHECK_SAME(track.at(key.time), key.transform);
}

TEST_CASE("MotionTrack: outside the keys the track clamps") {
  MotionTrack track{};
  track.keys.push_back({1.0f, translation(1, 0, 0)});
  track.keys.push_back({2.0f, translation(2, 0, 0)});
  CHECK_SAME(track.at(-5.0f), track.keys.front().transform);
  CHECK_SAME(track.at(0.999f), track.keys.front().transform);
  CHECK_SAME(track.at(2.001f), track.keys.back().transform);
  CHECK_SAME(track.at(100.0f), track.keys.back().transform);
}

TEST_CASE("MotionTrack: between keys the track interpolates") {
  MotionTrack track{};
  track.keys.push_back({0.0f, translation(0, 0, 0)});
  track.keys.push_back({1.0f, translation(10, 0, 0)});
  track.keys.push_back({3.0f, translation(10, 20, 0)});
  CHECK(track.at(0.5f)[3].x == doctest::Approx(5.0f));
  CHECK(track.at(0.5f)[3].y == doctest::Approx(0.0f));
  // The second span is twice as long, so a quarter of the way along it
  // is 2.0 s, not 1.5 s.
  CHECK(track.at(2.0f)[3].x == doctest::Approx(10.0f));
  CHECK(track.at(2.0f)[3].y == doctest::Approx(10.0f));
}

TEST_CASE("MotionTrack: one key is a constant") {
  MotionTrack track{};
  track.keys.push_back({7.0f, translation(1, 2, 3)});
  CHECK_SAME(track.at(-1.0f), track.keys[0].transform);
  CHECK_SAME(track.at(7.0f), track.keys[0].transform);
  CHECK_SAME(track.at(99.0f), track.keys[0].transform);
}

TEST_CASE("MotionTrack: a key inside the shutter is reported") {
  MotionTrack track{};
  track.keys.push_back({0.0f, float4x4(1.0f)});
  track.keys.push_back({0.5f, float4x4(1.0f)});
  track.keys.push_back({1.0f, float4x4(1.0f)});
  CHECK(track.hasKeyBetween(0.0f, 1.0f));
  CHECK(!track.hasKeyBetween(0.5f, 1.0f));
  CHECK(!track.hasKeyBetween(0.0f, 0.5f));
  CHECK(!track.hasKeyBetween(1.0f, 2.0f));
}

TEST_CASE("MotionSampling: a shut shutter lands both samples on one instant") {
  CHECK(MotionSampling{}.isStill());
  CHECK(MotionSampling(2.5f, 2.5f).isStill());
  CHECK(!MotionSampling(2.5f, 2.52f).isStill());
  const auto shifted{MotionSampling(1.0f, 2.0f).shiftedBy(0.25f)};
  CHECK(shifted.open == doctest::Approx(1.25f));
  CHECK(shifted.shut == doctest::Approx(2.25f));
}
