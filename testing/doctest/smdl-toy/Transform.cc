#include "Fixtures.h"
#include "TransformFixtures.h"

#include <string>
#include <utility>

#include "Transform.h"

using xf::rotationZ;
using xf::SHEAR;
using xf::translation;

TEST_CASE("Transform: the decomposition reassembles the transform") {
  float4x4 mirrored{SHEAR};
  mirrored[2] = -mirrored[2];
  const std::pair<const char *, float4x4> keys[]{{"sheared", SHEAR},
                                                 {"mirrored", mirrored}};
  for (const auto &entry : keys) {
    const char *name{entry.first};
    CAPTURE(name);
    const TransformDecomposition parts{decomposeTransform(entry.second)};
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

TEST_CASE("Transform: the ends of an interpolation are the keys themselves") {
  const float4x4 a{translation(1, 2, 3)};
  const float4x4 b{rotationZ(90.0f)};
  CHECK_SAME(interpolateTransform(a, b, 0.0f), a);
  CHECK_SAME(interpolateTransform(a, b, 1.0f), b);
  CHECK_SAME(interpolateTransform(a, b, -1.0f), a);
  CHECK_SAME(interpolateTransform(a, b, 2.0f), b);
}

TEST_CASE("Transform: a turn slerps, so halfway through 90 degrees is 45") {
  const float4x4 half{
      interpolateTransform(rotationZ(0.0f), rotationZ(90.0f), 0.5f)};
  CHECK_NEAR(half, rotationZ(45.0f), 1.0e-5f);
  // The chord a componentwise lerp would take is shorter than the arc,
  // so the object it places is smaller than the one the keys state.
  const float3 axis{half[0]};
  CHECK(smdl::length(axis) == doctest::Approx(1.0f).epsilon(1.0e-5));
}
