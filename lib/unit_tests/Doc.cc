#include "doctest.h"

#include <algorithm>
#include <cstdlib>

#include "smdl/Doc.h"
#include "smdl/Module.h"

using namespace smdl;

namespace {

[[nodiscard]] DocModule extractFromSource(std::string sourceCode) {
  auto allocator{BumpPtrAllocator{}};
  auto module_{Module("test", std::move(sourceCode))};
  auto error{module_.parse(allocator)};
  REQUIRE(!error);
  return extractDocModule(module_);
}

[[nodiscard]] const DocEntry &entryNamed(const DocModule &mod,
                                         std::string_view name) {
  for (const auto &entry : mod.entries)
    if (entry.name == name) return entry;
  FAIL("no entry named " << name);
  std::abort(); // Unreachable
}

} // namespace

TEST_CASE("Doc") {
  SUBCASE("extraction") {
    auto mod{extractFromSource(R"(/// The module documentation.
#smdl

/// The epsilon.
const float EPSILON = 1e-6, OTHER = 2.0; ///< The other.

/// The scatter mode.
export enum scatter_mode {
  scatter_none = 0,    ///< None.
  scatter_reflect = 1, ///< Reflect.
};

/// The result.
export struct result {
  /// The value.
  float value = 0.0;
  int flags = 0; ///< The flags.
};

/// The helper function,
/// on two lines.
@(pure macro)
export float helper(
  /// The x coordinate.
  float x,
  float weight = 1.0) {
  return x * weight;
}

export tag my_tag;

export typedef float3 vector3;

namespace inner {
  /// The nested function.
  export float nested(float y) = y;
}
)")};
    CHECK(mod.name == "test");
    CHECK(mod.qualifiedName == "::test");
    CHECK(mod.docText == "The module documentation.");

    const auto &epsilon{entryNamed(mod, "EPSILON")};
    CHECK(epsilon.kind == "variable");
    CHECK(epsilon.qualifiedName == "::test::EPSILON");
    CHECK(!epsilon.isExported);
    CHECK(epsilon.signature == "const float EPSILON = 1e-6");
    CHECK(epsilon.docText == "The epsilon.");
    const auto &other{entryNamed(mod, "OTHER")};
    CHECK(other.signature == "const float OTHER = 2.0");
    CHECK(other.docText == "The other.");

    const auto &scatterMode{entryNamed(mod, "scatter_mode")};
    CHECK(scatterMode.kind == "enum");
    CHECK(scatterMode.isExported);
    CHECK(scatterMode.signature == "export enum scatter_mode");
    CHECK(scatterMode.docText == "The scatter mode.");
    REQUIRE(scatterMode.members.size() == 2);
    CHECK(scatterMode.members[0].kind == "enumerator");
    // NOTE: Enum values inject into the enclosing scope.
    CHECK(scatterMode.members[0].qualifiedName == "::test::scatter_none");
    CHECK(scatterMode.members[0].signature == "scatter_none = 0");
    CHECK(scatterMode.members[0].docText == "None.");
    CHECK(scatterMode.members[1].docText == "Reflect.");

    const auto &result{entryNamed(mod, "result")};
    CHECK(result.kind == "struct");
    CHECK(result.signature == "export struct result");
    REQUIRE(result.members.size() == 2);
    CHECK(result.members[0].kind == "field");
    CHECK(result.members[0].qualifiedName == "::test::result::value");
    CHECK(result.members[0].signature == "float value = 0.0");
    CHECK(result.members[0].docText == "The value.");
    CHECK(result.members[1].docText == "The flags.");

    const auto &helper{entryNamed(mod, "helper")};
    CHECK(helper.kind == "function");
    CHECK(helper.signature ==
          "@(pure macro) export float helper(float x, float weight = 1.0)");
    CHECK(helper.docText == "The helper function,\non two lines.");
    REQUIRE(helper.params.size() == 2);
    CHECK(helper.params[0].name == "x");
    CHECK(helper.params[0].signature == "float x");
    CHECK(helper.params[0].docText == "The x coordinate.");
    CHECK(helper.params[1].signature == "float weight = 1.0");
    CHECK(helper.params[1].docText.empty());

    CHECK(entryNamed(mod, "my_tag").kind == "tag");
    CHECK(entryNamed(mod, "my_tag").signature == "export tag my_tag");
    CHECK(entryNamed(mod, "vector3").kind == "typedef");
    CHECK(entryNamed(mod, "vector3").signature ==
          "export typedef float3 vector3");

    const auto &inner{entryNamed(mod, "inner")};
    CHECK(inner.kind == "namespace");
    REQUIRE(inner.members.size() == 1);
    CHECK(inner.members[0].qualifiedName == "::test::inner::nested");
    CHECK(inner.members[0].docText == "The nested function.");
  }
  SUBCASE("anno::description fallback") {
    auto mod{extractFromSource(R"(mdl 1.7;
export const int X = 0 [[ anno::description("The described constant.") ]];
)")};
    CHECK(entryNamed(mod, "X").docText == "The described constant.");
  }
  SUBCASE("findSymbol and removeHidden") {
    auto docs{DocDatabase{}};
    docs.modules.push_back(extractFromSource(R"(#smdl
/// The exported function.
export int f(int a) = a;

/// The internal function.
int g(int a) = a;

/// The exported-but-underscored function.
export int _hideMe(int a) = a;

/// The exported struct with an underscored field.
export struct s {
  int visible = 0;
  int _invisible = 0;
};

namespace ns {
  export int h(int a) = a;
}
)"));
    auto foundF{docs.findSymbol("f")};
    REQUIRE(foundF.size() == 1);
    CHECK(foundF[0]->qualifiedName == "::test::f");
    CHECK(docs.findSymbol("::test::f").size() == 1);
    CHECK(docs.findSymbol("test::f").size() == 1);
    CHECK(docs.findSymbol("::other::f").empty());
    CHECK(docs.findSymbol("ns::h").size() == 1);
    CHECK(docs.findSymbol("_hideMe").size() == 1);
    CHECK(docs.findSymbol("nope").empty());
    CHECK(entryNamed(docs.modules[0], "_hideMe").isHidden());
    CHECK(!entryNamed(docs.modules[0], "f").isHidden());
    CHECK(entryNamed(docs.modules[0], "g").isHidden());

    docs.removeHidden();
    CHECK(docs.findSymbol("f").size() == 1);
    CHECK(docs.findSymbol("g").empty());
    CHECK(docs.findSymbol("_hideMe").empty());
    CHECK(docs.findSymbol("ns::h").size() == 1);
    CHECK(docs.findSymbol("s::visible").size() == 1);
    CHECK(docs.findSymbol("s::_invisible").empty());
  }
  SUBCASE("removeHidden drops empty namespaces") {
    auto docs{DocDatabase{}};
    docs.modules.push_back(extractFromSource(R"(#smdl
namespace ns {
  int internal(int a) = a;
}
namespace _detail {
  export int hidden(int a) = a;
}
)"));
    docs.removeHidden();
    CHECK(docs.findSymbol("ns").empty());
    CHECK(docs.findSymbol("_detail").empty());
    CHECK(docs.findSymbol("hidden").empty());
  }
  SUBCASE("printJSON escaping") {
    auto docs{DocDatabase{}};
    docs.modules.push_back(extractFromSource(R"(#smdl
/// A "quoted" doc with a backslash \ and
/// a second line.
export const int X = 0;
)"));
    auto json{docs.printJSON()};
    CHECK(json.find("\"modules\"") != std::string::npos);
    CHECK(json.find("A \\\"quoted\\\" doc with a backslash \\\\ and\\n"
                    "a second line.") != std::string::npos);
  }
  SUBCASE("printMarkdown") {
    auto docs{DocDatabase{}};
    docs.modules.push_back(extractFromSource(R"(#smdl
/// The function.
export int f(int a) = a;
)"));
    auto md{docs.printMarkdown()};
    CHECK(md.find("# Module `::test`") != std::string::npos);
    CHECK(md.find("## `::test::f`") != std::string::npos);
    CHECK(md.find("The function.") != std::string::npos);
  }
  SUBCASE("builtin modules") {
    auto names{getBuiltinModuleNames()};
    CHECK(!names.empty());
    CHECK(std::find(names.begin(), names.end(), "df") != names.end());
    auto mod{extractBuiltinDocModule("df")};
    REQUIRE(mod.has_value());
    CHECK(mod->qualifiedName == "::df");
    CHECK(!mod->entries.empty());
    // The builtin modules are embedded with `--keep-doc-comments`, so
    // documentation must survive minification. If this fails, the
    // `Builtin.h.rb` generator or the formatter is dropping doc
    // comments!
    CHECK(std::any_of(
        mod->entries.begin(), mod->entries.end(),
        [](const DocEntry &entry) { return !entry.docText.empty(); }));
    auto docs{DocDatabase{}};
    docs.modules.push_back(std::move(*mod));
    CHECK(!docs.findSymbol("::df::diffuse_reflection_bsdf").empty());
    CHECK(!extractBuiltinDocModule("not_a_builtin").has_value());
  }
}
