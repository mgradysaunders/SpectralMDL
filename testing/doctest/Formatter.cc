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
