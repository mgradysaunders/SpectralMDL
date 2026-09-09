#include "Fixtures.h"

#include <cstdlib>
#include <filesystem>
#include <string>

#include "smdl/Module.h"

namespace fs = std::filesystem;

namespace {
// An environment variable for the duration of a scope. The suite shares
// one process, so a variable left behind changes what a later test
// resolves, and the order tests run in is not fixed.
class ScopedEnv final {
public:
  ScopedEnv(const char *name, const std::string &value) : mName(name) {
#if defined(_WIN32)
    _putenv_s(name, value.c_str());
#else
    setenv(name, value.c_str(), 1);
#endif
  }

  ScopedEnv(const ScopedEnv &) = delete;

  ScopedEnv &operator=(const ScopedEnv &) = delete;

  ~ScopedEnv() {
#if defined(_WIN32)
    _putenv_s(mName, "");
#else
    unsetenv(mName);
#endif
  }

private:
  const char *mName{};
};

// Write, load, and parse a module. Returns the parse error message, or
// the empty string on success, with the parsed module in 'module_'.
std::string parseModule(const TempDir &tmpDir, std::string_view name,
                        std::string_view text,
                        std::unique_ptr<smdl::Module> &module_,
                        smdl::BumpPtrAllocator &allocator) {
  const auto path{tmpDir.write(name, text)};
  module_ = smdl::Module::loadFromFile(path.string());
  if (auto error{module_->parse(allocator)}) return error->message;
  return {};
}
} // namespace

TEST_CASE("Module: the search directories a module declares") {
  TempDir tmpDir{"module"};
  fs::create_directories(tmpDir / "data");
  // The allocator owns the AST, so it must outlive the module it is
  // parsed into: declare it first so it is destroyed last.
  auto allocator{smdl::BumpPtrAllocator{}};
  auto module_{std::unique_ptr<smdl::Module>()};
  SUBCASE("Relative and absolute paths expand and canonicalize in order") {
    CHECK(parseModule(tmpDir, "mod.smdl",
                      "#smdl\n"
                      "#search_dir \"./data/\"\n"
                      "#search_dir \"" +
                          (tmpDir / "data").string() + "\"\n",
                      module_, allocator) == "");
    const auto &searchDirs{module_->getSearchDirs()};
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
    const auto &searchDirs{module_->getSearchDirs()};
    REQUIRE(searchDirs.size() == 2);
    CHECK(fs::path(searchDirs[0]) == fs::weakly_canonical(tmpDir / "data"));
    CHECK(fs::path(searchDirs[1]) == fs::weakly_canonical(tmpDir / "data"));
  }
  SUBCASE("Undefined environment variable is an error") {
    auto message{parseModule(tmpDir, "mod.smdl",
                             "#smdl\n"
                             "#search_dir \"${SMDL_TEST_SEARCH_DIR_UNDEF}\"\n",
                             module_, allocator)};
    CHECK_CONTAINS(message, "undefined environment variable");
  }
  SUBCASE("Empty path is an error") {
    auto message{parseModule(tmpDir, "mod.smdl",
                             "#smdl\n#search_dir \"\"\n", //
                             module_, allocator)};
    CHECK_CONTAINS(message, "must not be empty");
  }
  SUBCASE("Missing literal string path is an error") {
    auto message{parseModule(tmpDir, "mod.smdl",
                             "#smdl\n#search_dir 42\n", //
                             module_, allocator)};
    CHECK_CONTAINS(message, "expected literal string path");
  }
  SUBCASE("'#search_dir' requires the '#smdl' dialect") {
    auto message{parseModule(tmpDir, "mod.mdl",
                             "#search_dir \"./data/\"\nmdl 1.7;\n", //
                             module_, allocator)};
    CHECK_CONTAINS(message, "requires the file to begin with '#smdl'");
  }
  SUBCASE("Misplaced after an import is an error") {
    auto message{parseModule(tmpDir, "mod.smdl",
                             "#smdl\n"
                             "import ::df::*;\n"
                             "#search_dir \"./data/\"\n",
                             module_, allocator)};
    CHECK_CONTAINS(message, "only allowed at the top");
  }
  SUBCASE("Misplaced inside a function is an error") {
    auto message{parseModule(tmpDir, "mod.smdl",
                             "#smdl\n"
                             "int bad() {\n"
                             "  return #search_dir \"./data/\";\n"
                             "}\n",
                             module_, allocator)};
    CHECK_CONTAINS(message, "only allowed at the top");
  }
  SUBCASE("Formatter preserves '#search_dir'") {
    const auto path{tmpDir.write("mod.smdl", "#smdl\n"
                                             "#search_dir    \"./data/\"\n"
                                             "#search_dir\t\"$HOME\"\n")};
    module_ = smdl::Module::loadFromFile(path.string());
    auto formatOptions{smdl::FormatOptions{}};
    formatOptions.isInPlace = true;
    CHECK_OK(module_->formatSourceFiles(formatOptions));
    auto formatted{tmpDir.read("mod.smdl")};
    CHECK_CONTAINS(formatted, "#search_dir \"./data/\"");
    CHECK_CONTAINS(formatted, "#search_dir \"$HOME\"");
  }
}
