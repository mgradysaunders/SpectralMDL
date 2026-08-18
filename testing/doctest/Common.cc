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
    auto version{std::to_string(info.major) + "." +
                 std::to_string(info.minor) + "." +
                 std::to_string(info.patch)};
    CHECK(str.find(version) != std::string::npos);
    CHECK(str.find(info.gitCommit) != std::string::npos);
    CHECK(str.find(info.llvmVersion) != std::string::npos);
  }
}
