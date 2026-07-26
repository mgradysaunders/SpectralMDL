#include "doctest.h"

#include "smdl/Support/QualifiedName.h"

using Components = std::vector<std::string_view>;

TEST_CASE("QualifiedName") {
  SUBCASE("splitQualifiedName") {
    CHECK(smdl::splitQualifiedName("").empty());
    CHECK(smdl::splitQualifiedName("::").empty());
    CHECK(smdl::splitQualifiedName("a") == Components{"a"});
    CHECK(smdl::splitQualifiedName("::a") == Components{"a"});
    CHECK(smdl::splitQualifiedName("a::b") == Components{"a", "b"});
    CHECK(smdl::splitQualifiedName("::a::b::c") == Components{"a", "b", "c"});
    // Malformed inputs split verbatim into empty components.
    CHECK(smdl::splitQualifiedName("a::") == Components{"a", ""});
    CHECK(smdl::splitQualifiedName("a::::b") == Components{"a", "", "b"});
  }
  SUBCASE("joinQualifiedName") {
    CHECK(smdl::joinQualifiedName({}) == "");
    CHECK(smdl::joinQualifiedName({"a"}) == "::a");
    CHECK(smdl::joinQualifiedName({"a", "b"}) == "::a::b");
    // Round trip.
    auto components{smdl::splitQualifiedName("::vendor::metals::steel")};
    CHECK(smdl::joinQualifiedName(components) == "::vendor::metals::steel");
  }
  SUBCASE("isQualifiedNameSuffix") {
    const auto name{"::vendor::metals::steel::brushed"};
    CHECK(smdl::isQualifiedNameSuffix("brushed", name));
    CHECK(smdl::isQualifiedNameSuffix("steel::brushed", name));
    CHECK(smdl::isQualifiedNameSuffix("metals::steel::brushed", name));
    CHECK(smdl::isQualifiedNameSuffix("vendor::metals::steel::brushed", name));
    // A leading '::' is ignored, so the full name is its own suffix.
    CHECK(smdl::isQualifiedNameSuffix(name, name));
    // Not on a component boundary.
    CHECK(!smdl::isQualifiedNameSuffix("shed", name));
    CHECK(!smdl::isQualifiedNameSuffix("s::brushed", name));
    // Not a suffix: interior, disjoint, too long, or empty.
    CHECK(!smdl::isQualifiedNameSuffix("metals::steel", name));
    CHECK(!smdl::isQualifiedNameSuffix("vendor", name));
    CHECK(!smdl::isQualifiedNameSuffix("other::brushed", name));
    CHECK(!smdl::isQualifiedNameSuffix("x::vendor::metals::steel::brushed",
                                       name));
    CHECK(!smdl::isQualifiedNameSuffix("", name));
    CHECK(!smdl::isQualifiedNameSuffix("a", ""));
    CHECK(smdl::isQualifiedNameSuffix("a", "::a"));
    CHECK(!smdl::isQualifiedNameSuffix("a", "::b"));
  }
}
