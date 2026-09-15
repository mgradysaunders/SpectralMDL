#include "Fixtures.h"

#include <filesystem>
#include <string>

#include "smdl/Module.h"

namespace fs = std::filesystem;

namespace {
// Write, load, and parse a module. Returns the parse error message, or
// the empty string on success, with the parsed module in 'module_'.
std::string parseModule(const TempDir &tmpDir, std::string_view name,
                        std::string_view text,
                        std::unique_ptr<smdl::Module> &module_,
                        smdl::BumpPtrAllocator &allocator) {
  const std::filesystem::path path{tmpDir.write(name, text)};
  module_ = smdl::Module::loadFromFile(path.string());
  if (std::optional<smdl::Error> error{module_->parse(allocator)})
    return error->message;
  return {};
}
} // namespace

TEST_CASE("Module: the search directories a module declares") {
  TempDir tmpDir{"module"};
  fs::create_directories(tmpDir / "data");
  // The allocator owns the AST, so it must outlive the module it is
  // parsed into: declare it first so it is destroyed last.
  smdl::BumpPtrAllocator allocator{};
  std::unique_ptr<smdl::Module> module_{};
  SUBCASE("Relative and absolute paths expand and canonicalize in order") {
    CHECK(parseModule(tmpDir, "mod.smdl",
                      "#smdl\n"
                      "#search_dir \"./data/\"\n"
                      "#search_dir \"" +
                          (tmpDir / "data").string() + "\"\n",
                      module_, allocator) == "");
    const std::vector<std::string> &searchDirs{module_->getSearchDirs()};
    REQUIRE(searchDirs.size() == 2);
    CHECK(fs::path(searchDirs[0]) == fs::weakly_canonical(tmpDir / "data"));
    CHECK(fs::path(searchDirs[1]) == fs::weakly_canonical(tmpDir / "data"));
  }
  SUBCASE("Environment variables expand") {
    const ScopedEnv searchDir{"SMDL_TEST_SEARCH_DIR",
                              (tmpDir / "data").string()};
    CHECK(parseModule(tmpDir, "mod.smdl",
                      "#smdl\n"
                      "#search_dir \"${SMDL_TEST_SEARCH_DIR}\"\n"
                      "#search_dir \"$SMDL_TEST_SEARCH_DIR\"\n",
                      module_, allocator) == "");
    const std::vector<std::string> &searchDirs{module_->getSearchDirs()};
    REQUIRE(searchDirs.size() == 2);
    CHECK(fs::path(searchDirs[0]) == fs::weakly_canonical(tmpDir / "data"));
    CHECK(fs::path(searchDirs[1]) == fs::weakly_canonical(tmpDir / "data"));
  }
  SUBCASE("Undefined environment variable is an error") {
    std::string message{
        parseModule(tmpDir, "mod.smdl",
                    "#smdl\n"
                    "#search_dir \"${SMDL_TEST_SEARCH_DIR_UNDEF}\"\n",
                    module_, allocator)};
    CHECK_CONTAINS(message, "Undefined environment variable");
  }
  SUBCASE("Empty path is an error") {
    std::string message{parseModule(tmpDir, "mod.smdl",
                                    "#smdl\n#search_dir \"\"\n", //
                                    module_, allocator)};
    CHECK_CONTAINS(message, "must not be empty");
  }
  SUBCASE("Missing literal string path is an error") {
    std::string message{parseModule(tmpDir, "mod.smdl",
                                    "#smdl\n#search_dir 42\n", //
                                    module_, allocator)};
    CHECK_CONTAINS(message, "Expected literal string path");
  }
  SUBCASE("'#search_dir' requires the '#smdl' dialect") {
    std::string message{parseModule(tmpDir, "mod.mdl",
                                    "#search_dir \"./data/\"\nmdl 1.7;\n", //
                                    module_, allocator)};
    CHECK_CONTAINS(message, "requires the file to begin with '#smdl'");
  }
  SUBCASE("Misplaced after an import is an error") {
    std::string message{parseModule(tmpDir, "mod.smdl",
                                    "#smdl\n"
                                    "import ::df::*;\n"
                                    "#search_dir \"./data/\"\n",
                                    module_, allocator)};
    CHECK_CONTAINS(message, "only allowed at the top");
  }
  SUBCASE("Misplaced inside a function is an error") {
    std::string message{parseModule(tmpDir, "mod.smdl",
                                    "#smdl\n"
                                    "int bad() {\n"
                                    "  return #search_dir \"./data/\";\n"
                                    "}\n",
                                    module_, allocator)};
    CHECK_CONTAINS(message, "only allowed at the top");
  }
  SUBCASE("Formatter preserves '#search_dir'") {
    const std::filesystem::path path{tmpDir.write("mod.smdl",
                                                  "#smdl\n"
                                                  "#search_dir    \"./data/\"\n"
                                                  "#search_dir\t\"$HOME\"\n")};
    module_ = smdl::Module::loadFromFile(path.string());
    smdl::FormatOptions formatOptions{};
    formatOptions.isInPlace = true;
    CHECK_OK(module_->formatSourceFiles(formatOptions));
    std::string formatted{tmpDir.read("mod.smdl")};
    CHECK_CONTAINS(formatted, "#search_dir \"./data/\"");
    CHECK_CONTAINS(formatted, "#search_dir \"$HOME\"");
  }
}
