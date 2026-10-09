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
    CHECK(info.withSTBSprintf != nullptr);
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
