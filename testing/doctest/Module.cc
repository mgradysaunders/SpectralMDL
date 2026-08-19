#include "doctest.h"

#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <string>

#include "smdl/Module.h"

namespace fs = std::filesystem;

static void writeFile(const fs::path &path, std::string_view text) {
  fs::create_directories(path.parent_path());
  std::ofstream(path) << text;
}

static std::string readFile(const fs::path &path) {
  auto stream{std::ifstream(path)};
  return std::string((std::istreambuf_iterator<char>(stream)),
                     std::istreambuf_iterator<char>());
}

// Write, load, and parse a module. Returns the parse error message, or
// the empty string on success, with the parsed module in 'module_'.
static std::string parseModule(const fs::path &path, std::string_view text,
                               std::unique_ptr<smdl::Module> &module_,
                               smdl::BumpPtrAllocator &allocator) {
  writeFile(path, text);
  module_ = smdl::Module::loadFromFile(path.string());
  if (auto error{module_->parse(allocator)}) return error->message;
  return {};
}

TEST_CASE("Module search dirs") {
  auto tmpDir{fs::temp_directory_path() / "smdl-module-test"};
  fs::remove_all(tmpDir);
  fs::create_directories(tmpDir / "data");
  // The allocator owns the AST, so it must outlive the module it is
  // parsed into: declare it first so it is destroyed last.
  auto allocator{smdl::BumpPtrAllocator{}};
  auto module_{std::unique_ptr<smdl::Module>()};
  SUBCASE("Relative and absolute paths expand and canonicalize in order") {
    CHECK(parseModule(tmpDir / "mod.smdl",
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
#if defined(_WIN32)
    _putenv_s("SMDL_TEST_SEARCH_DIR", (tmpDir / "data").string().c_str());
#else
    setenv("SMDL_TEST_SEARCH_DIR", (tmpDir / "data").string().c_str(), 1);
#endif
    CHECK(parseModule(tmpDir / "mod.smdl",
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
    auto message{parseModule(tmpDir / "mod.smdl",
                             "#smdl\n"
                             "#search_dir \"${SMDL_TEST_SEARCH_DIR_UNDEF}\"\n",
                             module_, allocator)};
    CHECK(message.find("undefined environment variable") != std::string::npos);
  }
  SUBCASE("Empty path is an error") {
    auto message{parseModule(tmpDir / "mod.smdl",
                             "#smdl\n#search_dir \"\"\n", //
                             module_, allocator)};
    CHECK(message.find("must not be empty") != std::string::npos);
  }
  SUBCASE("Missing literal string path is an error") {
    auto message{parseModule(tmpDir / "mod.smdl",
                             "#smdl\n#search_dir 42\n", //
                             module_, allocator)};
    CHECK(message.find("expected literal string path") != std::string::npos);
  }
  SUBCASE("Requires '#smdl'") {
    auto message{parseModule(tmpDir / "mod.mdl",
                             "#search_dir \"./data/\"\nmdl 1.7;\n", //
                             module_, allocator)};
    CHECK(message.find("requires the file to begin with '#smdl'") !=
          std::string::npos);
  }
  SUBCASE("Misplaced after an import is an error") {
    auto message{parseModule(tmpDir / "mod.smdl",
                             "#smdl\n"
                             "import ::df::*;\n"
                             "#search_dir \"./data/\"\n",
                             module_, allocator)};
    CHECK(message.find("only allowed at the top") != std::string::npos);
  }
  SUBCASE("Misplaced inside a function is an error") {
    auto message{parseModule(tmpDir / "mod.smdl",
                             "#smdl\n"
                             "int bad() {\n"
                             "  return #search_dir \"./data/\";\n"
                             "}\n",
                             module_, allocator)};
    CHECK(message.find("only allowed at the top") != std::string::npos);
  }
  SUBCASE("Formatter preserves '#search_dir'") {
    writeFile(tmpDir / "mod.smdl", "#smdl\n"
                                   "#search_dir    \"./data/\"\n"
                                   "#search_dir\t\"$HOME\"\n");
    module_ = smdl::Module::loadFromFile((tmpDir / "mod.smdl").string());
    auto formatOptions{smdl::FormatOptions{}};
    formatOptions.inPlace = true;
    CHECK(!module_->formatSourceFiles(formatOptions));
    auto formatted{readFile(tmpDir / "mod.smdl")};
    CHECK(formatted.find("#search_dir \"./data/\"") != std::string::npos);
    CHECK(formatted.find("#search_dir \"$HOME\"") != std::string::npos);
  }
  fs::remove_all(tmpDir);
}
