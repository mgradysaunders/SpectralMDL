#include "Fixtures.h"
#include "TransformFixtures.h"

#include "Layout/Motion.h"

using xf::rotationZ;
using xf::SHEAR;
using xf::translation;

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
  const MotionSampling shifted{MotionSampling(1.0f, 2.0f).shiftedBy(0.25f)};
  CHECK(shifted.open == doctest::Approx(1.25f));
  CHECK(shifted.shut == doctest::Approx(2.25f));
}
