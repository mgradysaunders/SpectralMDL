#include "Fixtures.h"

#include <cstring>
#include <string>

#include "smdl/Common.h"

TEST_CASE("BuildInfo: what the banner reports about this build") {
  smdl::BuildInfo info{smdl::BuildInfo::get()};
  SUBCASE("Fields documented as never null are never null") {
    CHECK(info.gitBranch != nullptr);
    CHECK(info.gitCommit != nullptr);
    CHECK(info.llvmVersion != nullptr);
    CHECK(info.buildDate != nullptr);
    CHECK(info.withMiniz != nullptr);
    CHECK(info.withSTBImage != nullptr);
    CHECK(info.withSTBImageWrite != nullptr);
    CHECK(info.withSTBImageResize != nullptr);
    CHECK(info.withTinyEXR != nullptr);
  }
  SUBCASE("RTTI report agrees with this test binary") {
    // Valid because the test harness compiles with the same RTTI flag as
    // the library (see the CMakeLists.txt here).
#if defined(__cpp_rtti) || defined(__GXX_RTTI) || defined(_CPPRTTI)
    CHECK(info.hasRTTI);
#else
    CHECK(!info.hasRTTI);
#endif
  }
  SUBCASE("String summary mentions the version and commit") {
    std::string str{info.toString()};
    std::string version{std::to_string(info.major) + "." +
                        std::to_string(info.minor) + "." +
                        std::to_string(info.patch)};
    CHECK_CONTAINS(str, version);
    CHECK_CONTAINS(str, info.gitCommit);
    CHECK_CONTAINS(str, info.llvmVersion);
  }
  SUBCASE("String summary lists every third-party dependency") {
    CHECK(!info.thirdparty.empty());
    std::string str{info.toString()};
    for (const auto &dep : info.thirdparty) {
      CHECK(!dep.version.empty());
      CHECK_CONTAINS(str, dep.name + " " + dep.version);
    }
  }
}

TEST_CASE("State: what finalize establishes") {
  SUBCASE("Finalize clamps the texture space count") {
    // A host that asks for more spaces than there are must not send the
    // loops here, or the generated code that reads the same arrays, off
    // the end of them.
    smdl::State state{};
    state.textureSpaceCount = 16;
    state.finalize();
    CHECK(state.textureSpaceCount == int(smdl::State::TEXTURE_SPACE_MAX));

    state = smdl::State();
    state.textureSpaceCount = -1;
    state.finalize();
    CHECK(state.textureSpaceCount == 0);

    state = smdl::State();
    state.finalize();
    CHECK(state.textureSpaceCount == 1);
  }
  SUBCASE("Finalize establishes the internal space conventions") {
    smdl::State state{};
    state.position = smdl::float3(3, -1, 2);
    state.normal = smdl::float3(0, 1, 1);
    state.geometryNormal = smdl::float3(0, 1, 1);
    state.geometryTangentU[0] = smdl::float3(2, 0, 0);
    state.geometryTangentV[0] = smdl::float3(0, 1, 0);
    state.finalize();
    CHECK(state.position.x == doctest::Approx(0.0f));
    CHECK(state.position.y == doctest::Approx(0.0f));
    CHECK(state.position.z == doctest::Approx(0.0f));
    // Space 0's frame lands on the axes exactly, not merely close: the
    // transform is skipped for the vectors the frame was built from.
    CHECK(smdl::isAllTrue(state.geometryNormal == smdl::float3(0, 0, 1)));
    CHECK(smdl::isAllTrue(state.geometryTangentU[0] == smdl::float3(1, 0, 0)));
    CHECK(smdl::isAllTrue(state.geometryTangentV[0] == smdl::float3(0, 1, 0)));
  }
  SUBCASE("Finalize unchecked agrees with finalize on an orthonormal frame") {
    using smdl::float3;
    const auto near{[](const float3 &a, const float3 &b) {
      return smdl::length(a - b) < 1e-6f;
    }};
    // A frame no repair step would touch: unit normals, each tangent
    // pair orthonormal with its normal, and an orthonormal placement.
    const smdl::float3 w{smdl::normalize(float3(1, 2, 3))};
    const smdl::float3 u{smdl::perpendicularTo(w)};
    const smdl::float3 v{smdl::cross(w, u)};
    const smdl::float3 n{smdl::normalize(w + 0.25f * u - 0.125f * v)};
    smdl::float3 tu{u - smdl::dot(u, n) * n};
    CHECK(smdl::tryNormalize(tu));
    const smdl::float3 tv{smdl::cross(n, tu)};
    const smdl::float3x3 placement{smdl::orthonormalize(
        smdl::float3x3(float3(2, 1, 0), float3(-1, 3, 1), float3(0, -1, 2)))};
    smdl::State state{};
    state.position = float3(3, -1, 2);
    state.direction = smdl::normalize(float3(-1, 0.5f, -2));
    state.motion = float3(0.1f, 0.2f, 0.3f);
    state.normal = n;
    state.textureTangentU[0] = tu;
    state.textureTangentV[0] = tv;
    state.geometryNormal = w;
    state.geometryTangentU[0] = u;
    state.geometryTangentV[0] = v;
    state.objectToWorld = smdl::float4x4(
        smdl::float4(placement[0], 0), smdl::float4(placement[1], 0),
        smdl::float4(placement[2], 0), smdl::float4(4, 5, 6, 1));
    smdl::State checked{state};
    checked.finalize();
    smdl::State unchecked{state};
    unchecked.finalizeUnchecked();
    CHECK(near(checked.position, unchecked.position));
    CHECK(near(checked.direction, unchecked.direction));
    CHECK(near(checked.motion, unchecked.motion));
    CHECK(near(checked.normal, unchecked.normal));
    CHECK(near(checked.textureTangentU[0], unchecked.textureTangentU[0]));
    CHECK(near(checked.textureTangentV[0], unchecked.textureTangentV[0]));
    CHECK(smdl::isAllTrue(unchecked.geometryNormal == float3(0, 0, 1)));
    CHECK(smdl::isAllTrue(unchecked.geometryTangentU[0] == float3(1, 0, 0)));
    CHECK(smdl::isAllTrue(unchecked.geometryTangentV[0] == float3(0, 1, 0)));
    for (int j = 0; j < 4; j++) {
      CHECK(near(float3(checked.tangentToObject[j]),
                 float3(unchecked.tangentToObject[j])));
      CHECK(near(float3(checked.objectToWorld[j]),
                 float3(unchecked.objectToWorld[j])));
    }
  }
}
