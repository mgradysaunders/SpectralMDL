#include "Fixtures.h"

#include <filesystem>
#include <memory>
#include <string>

#include "smdl/Module.h"

TEST_CASE("Formatter: the comments it keeps") {
  TempDir tmpDir{"formatter-comments"};
  SUBCASE("A nested block comment is kept whole") {
    // Block comments nest, so the formatter has to find the end of one
    // where the parser does, or it splits the comment and formats the
    // rest of it as code.
    const std::filesystem::path path{
        tmpDir.write("mod.mdl", "mdl 1.8;\n"
                                "/* outer /* inner */ still the outer */\n"
                                "export const int x=1;\n")};
    std::unique_ptr<smdl::Module> module{
        smdl::Module::loadFromFile(path.string())};
    smdl::FormatOptions formatOptions{};
    formatOptions.isInPlace = true;
    CHECK_OK(module->formatSourceFiles(formatOptions));
    std::string formatted{tmpDir.read("mod.mdl")};
    CHECK_CONTAINS(formatted, "/* outer /* inner */ still the outer */");
    CHECK_CONTAINS(formatted, "export const int x = 1;");
  }
}

TEST_CASE("Formatter: the syntax it writes back as written") {
  TempDir tmpDir{"formatter-syntax"};
  auto format{[&](std::string_view text) {
    const std::filesystem::path path{tmpDir.write("mod.mdl", text)};
    std::unique_ptr<smdl::Module> module{
        smdl::Module::loadFromFile(path.string())};
    smdl::FormatOptions formatOptions{};
    formatOptions.isInPlace = true;
    CHECK_OK(module->formatSourceFiles(formatOptions));
    return tmpDir.read("mod.mdl");
  }};
  SUBCASE("An operator called by name") {
    const std::string text{"mdl 1.9;\n\n"
                           "export int f(int[2] a) = operator+(y: a[0], x: 1) "
                           "+ operator[](a, 1);\n"};
    CHECK(format(text) == text);
  }
  SUBCASE("A unicode identifier") {
    const std::string text{"mdl 1.9;\n\n"
                           "import .::'\\u00fcn\\u00ef'::*;\n\n"
                           "export float f() = '\xc3\xbcn\xc3\xaf'::g(1.0);\n"};
    CHECK(format(text) == text);
  }
}
