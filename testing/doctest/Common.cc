#include "doctest.h"

#include <cstring>
#include <string>

#include "smdl/Common.h"

TEST_CASE("BuildInfo") {
  auto info{smdl::BuildInfo::get()};
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
    auto str{info.toString()};
    auto version{std::to_string(info.major) + "." + std::to_string(info.minor) +
                 "." + std::to_string(info.patch)};
    CHECK(str.find(version) != std::string::npos);
    CHECK(str.find(info.gitCommit) != std::string::npos);
    CHECK(str.find(info.llvmVersion) != std::string::npos);
  }
  SUBCASE("String summary lists every third-party dependency") {
    CHECK(!info.thirdparty.empty());
    auto str{info.toString()};
    for (const auto &dep : info.thirdparty) {
      CHECK(!dep.version.empty());
      CHECK(str.find(dep.name + " " + dep.version) != std::string::npos);
    }
  }
}

TEST_CASE("State") {
  SUBCASE("Finalize clamps the texture space count") {
    // A host that asks for more spaces than there are must not send the
    // loops here, or the generated code that reads the same arrays, off
    // the end of them.
    auto state{smdl::State()};
    state.texture_space_max = 16;
    state.finalizeAndApplyInternalSpaceConventions();
    CHECK(state.texture_space_max == int(smdl::State::TEXTURE_SPACE_MAX));

    state = smdl::State();
    state.texture_space_max = -1;
    state.finalizeAndApplyInternalSpaceConventions();
    CHECK(state.texture_space_max == 0);

    state = smdl::State();
    state.finalizeAndApplyInternalSpaceConventions();
    CHECK(state.texture_space_max == 1);
  }
  SUBCASE("Finalize establishes the internal space conventions") {
    auto state{smdl::State()};
    state.position = smdl::float3(3, -1, 2);
    state.normal = smdl::float3(0, 1, 1);
    state.geometry_normal = smdl::float3(0, 1, 1);
    state.geometry_tangent_u[0] = smdl::float3(2, 0, 0);
    state.geometry_tangent_v[0] = smdl::float3(0, 1, 0);
    state.finalizeAndApplyInternalSpaceConventions();
    CHECK(state.position.x == doctest::Approx(0.0f));
    CHECK(state.position.y == doctest::Approx(0.0f));
    CHECK(state.position.z == doctest::Approx(0.0f));
    // Space 0's frame lands on the axes exactly, not merely close: the
    // transform is skipped for the vectors the frame was built from.
    CHECK(smdl::isAllTrue(state.geometry_normal == smdl::float3(0, 0, 1)));
    CHECK(
        smdl::isAllTrue(state.geometry_tangent_u[0] == smdl::float3(1, 0, 0)));
    CHECK(
        smdl::isAllTrue(state.geometry_tangent_v[0] == smdl::float3(0, 1, 0)));
  }
}
