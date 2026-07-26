#include "doctest.h"

#include <algorithm>
#include <filesystem>
#include <fstream>
#include <string>
#include <vector>

#include "smdl/Compiler.h"

namespace fs = std::filesystem;

// NOTE: These are characterization tests (see 'material-resolution-plan.md'
// milestone M1): they pin the CURRENT behavior of module and material
// resolution as the baseline for the qualified-name redesign. Cases marked
// 'NOTE: revised in M3' pin behavior that later milestones intentionally
// change.

static void writeFile(const fs::path &path, std::string_view text) {
  fs::create_directories(path.parent_path());
  std::ofstream(path) << text;
}

// Add everything, compile, and JIT-compile. Returns the first error
// message, or the empty string on success.
static std::string buildAll(smdl::Compiler &compiler,
                            const std::vector<fs::path> &paths) {
  for (const auto &path : paths)
    if (auto error{compiler.add(path.string())})
      return error->message;
  if (auto error{compiler.compile(smdl::OPT_LEVEL_NONE)})
    return error->message;
  if (auto error{compiler.jitCompile()})
    return error->message;
  return {};
}

// A minimal named material definition.
static std::string materialDef(std::string_view name) {
  auto text{std::string()};
  text += "material ";
  text += name;
  text += "() = material(\n"
          "  surface: material_surface(\n"
          "    scattering: df::diffuse_reflection_bsdf(tint: 0.5)),\n"
          ");\n";
  return text;
}

TEST_CASE("Compiler module resolution") {
  auto tmpDir{fs::temp_directory_path() / "smdl-compiler-test"};
  fs::remove_all(tmpDir);
  SUBCASE("Weak-relative import prefers the importing module's directory") {
    writeFile(tmpDir / "root" / "util.mdl",
              "#smdl\nexport const int marker_top = 1;\n");
    writeFile(tmpDir / "root" / "sub" / "util.mdl",
              "#smdl\nexport const int marker_sub = 1;\n");
    writeFile(tmpDir / "root" / "sub" / "main.mdl",
              "#smdl\nimport ::df::*;\nimport util::marker_sub;\n" +
                  materialDef("main_ok"));
    smdl::Compiler compiler{};
    CHECK(buildAll(compiler, {tmpDir / "root"}) == "");
    CHECK(compiler.findMaterial("main_ok") != nullptr);
  }
  SUBCASE("Import binds the resolved module even if the name is absent") {
    // 'sub/util.mdl' shadows 'util.mdl' for 'sub/main.mdl', and there is
    // no fallback re-resolution when the imported name is missing.
    writeFile(tmpDir / "root" / "util.mdl",
              "#smdl\nexport const int marker_top = 1;\n");
    writeFile(tmpDir / "root" / "sub" / "util.mdl",
              "#smdl\nexport const int marker_sub = 1;\n");
    writeFile(tmpDir / "root" / "sub" / "main.mdl",
              "#smdl\nimport ::df::*;\nimport util::marker_top;\n" +
                  materialDef("main_bad"));
    smdl::Compiler compiler{};
    auto message{buildAll(compiler, {tmpDir / "root"})};
    CHECK(message != "");
    CHECK(message.find("cannot resolve import") != std::string::npos);
  }
  SUBCASE("Weak-relative import falls back to search roots in add order") {
    writeFile(tmpDir / "rootA" / "util.mdl",
              "#smdl\nexport const int marker_a = 1;\n");
    writeFile(tmpDir / "rootB" / "util.mdl",
              "#smdl\nexport const int marker_b = 1;\n");
    writeFile(tmpDir / "rootC" / "main.mdl",
              "#smdl\nimport ::df::*;\nimport util::marker_a;\n" +
                  materialDef("main_ok"));
    {
      // 'rootA' added first: its 'util' wins, so 'marker_a' resolves.
      smdl::Compiler compiler{};
      CHECK(buildAll(compiler, {tmpDir / "rootA", tmpDir / "rootB",
                                tmpDir / "rootC"}) == "");
    }
    {
      // 'rootB' added first: its 'util' wins, so 'marker_a' does not.
      smdl::Compiler compiler{};
      auto message{buildAll(compiler, {tmpDir / "rootB", tmpDir / "rootA",
                                       tmpDir / "rootC"})};
      CHECK(message.find("cannot resolve import") != std::string::npos);
    }
  }
  SUBCASE("Strict-relative '.' import never falls back to search roots") {
    writeFile(tmpDir / "root" / "util.mdl",
              "#smdl\nexport const int marker_top = 1;\n");
    writeFile(tmpDir / "root" / "pkg" / "strict.mdl",
              "#smdl\nimport .::util::marker_top;\n");
    {
      smdl::Compiler compiler{};
      auto message{buildAll(compiler, {tmpDir / "root"})};
      CHECK(message.find("cannot resolve import") != std::string::npos);
    }
    // The same import spelled weakly succeeds via the search root.
    writeFile(tmpDir / "root" / "pkg" / "strict.mdl",
              "#smdl\nimport util::marker_top;\n");
    {
      smdl::Compiler compiler{};
      CHECK(buildAll(compiler, {tmpDir / "root"}) == "");
    }
  }
  SUBCASE("Strict-relative '..' traversal") {
    writeFile(tmpDir / "root" / "p2" / "helper.mdl",
              "#smdl\nexport const int marker = 1;\n");
    writeFile(tmpDir / "root" / "p1" / "main.mdl",
              "#smdl\nimport ::df::*;\nimport ..::p2::helper::marker;\n" +
                  materialDef("main_ok"));
    smdl::Compiler compiler{};
    CHECK(buildAll(compiler, {tmpDir / "root"}) == "");
    CHECK(compiler.findMaterial("main_ok") != nullptr);
  }
  SUBCASE("Absolute '::' prefers builtins over search roots") {
    // A module named 'df' on disk cannot shadow the builtin '::df' ...
    writeFile(tmpDir / "root" / "df.mdl",
              "#smdl\nexport const int fake_fn = 1;\n");
    writeFile(tmpDir / "root" / "main.mdl", "#smdl\nimport ::df::fake_fn;\n");
    {
      smdl::Compiler compiler{};
      auto message{buildAll(compiler, {tmpDir / "root"})};
      CHECK(message.find("cannot resolve import") != std::string::npos);
    }
    // ... but a weak-relative 'df' import binds the disk module.
    writeFile(tmpDir / "root" / "main.mdl", "#smdl\nimport df::fake_fn;\n");
    {
      smdl::Compiler compiler{};
      CHECK(buildAll(compiler, {tmpDir / "root"}) == "");
    }
  }
  SUBCASE("Nested builtin absolute import") {
    writeFile(tmpDir / "root" / "main.mdl",
              "#smdl\nimport ::df::*;\nimport ::models::prospect::*;\n"
              "import ::models::marmit::*;\n" +
                  materialDef("main_ok"));
    smdl::Compiler compiler{};
    CHECK(buildAll(compiler, {tmpDir / "root"}) == "");
    CHECK(compiler.findMaterial("main_ok") != nullptr);
  }
  SUBCASE("Nested builtin priority mirrors single-component rules") {
    // A disk module at 'models/prospect.mdl' cannot shadow the builtin
    // '::models::prospect' on an absolute import ...
    writeFile(tmpDir / "root" / "models" / "prospect.mdl",
              "#smdl\nexport const int fake_fn = 1;\n");
    writeFile(tmpDir / "root" / "main.mdl",
              "#smdl\nimport ::models::prospect::fake_fn;\n");
    {
      smdl::Compiler compiler{};
      auto message{buildAll(compiler, {tmpDir / "root"})};
      CHECK(message.find("cannot resolve import") != std::string::npos);
    }
    // ... but a weak import binds the disk module.
    writeFile(tmpDir / "root" / "main.mdl",
              "#smdl\nimport models::prospect::fake_fn;\n");
    {
      smdl::Compiler compiler{};
      CHECK(buildAll(compiler, {tmpDir / "root"}) == "");
    }
  }
  SUBCASE("Weak import falls back to nested builtins") {
    // No disk 'models/prospect' anywhere, so the weak path reaches the
    // builtin after the relative and search-root strategies miss.
    writeFile(tmpDir / "root" / "main.mdl",
              "#smdl\nimport models::prospect::*;\n");
    smdl::Compiler compiler{};
    CHECK(buildAll(compiler, {tmpDir / "root"}) == "");
  }
  SUBCASE("Using aliases resolve through the same machinery") {
    writeFile(tmpDir / "root" / "target.mdl",
              "#smdl\nexport const int marker = 1;\n");
    writeFile(tmpDir / "root" / "sub" / "helper.mdl",
              "#smdl\nexport const int marker = 2;\n");
    writeFile(tmpDir / "root" / "main.mdl",
              "#smdl\n"
              "import ::df::*;\n"
              "using u = \"target\";\n"
              "using v = .::sub::helper;\n"
              "import u::marker;\n"
              "import v::*;\n" +
                  materialDef("main_ok"));
    smdl::Compiler compiler{};
    CHECK(buildAll(compiler, {tmpDir / "root"}) == "");
    CHECK(compiler.findMaterial("main_ok") != nullptr);
  }
  fs::remove_all(tmpDir);
}

TEST_CASE("Compiler module identity") {
  auto tmpDir{fs::temp_directory_path() / "smdl-compiler-test"};
  fs::remove_all(tmpDir);
  SUBCASE("Qualified names derive from search roots") {
    writeFile(tmpDir / "root" / "top.mdl", "#smdl\nexport const int x = 1;\n");
    writeFile(tmpDir / "root" / "vendor" / "metals" / "steel.mdl",
              "#smdl\nexport const int x = 1;\n");
    smdl::Compiler compiler{};
    auto names{std::vector<std::string>()};
    REQUIRE(!compiler.add((tmpDir / "root").string(), &names));
    std::sort(names.begin(), names.end());
    CHECK(names == std::vector<std::string>{"::top", "::vendor::metals::steel"});
  }
  SUBCASE("Single-file add uses the parent directory as implicit root") {
    writeFile(tmpDir / "dir" / "pkg" / "mod.mdl",
              "#smdl\nexport const int x = 1;\n");
    smdl::Compiler compiler{};
    auto names{std::vector<std::string>()};
    REQUIRE(!compiler.add((tmpDir / "dir" / "pkg" / "mod.mdl").string(),
                          &names));
    CHECK(names == std::vector<std::string>{"::mod"});
  }
  SUBCASE("Re-adding the same search root is a no-op") {
    writeFile(tmpDir / "root" / "mod.mdl", "#smdl\nexport const int x = 1;\n");
    smdl::Compiler compiler{};
    REQUIRE(!compiler.add((tmpDir / "root").string()));
    auto names{std::vector<std::string>()};
    CHECK(!compiler.add((tmpDir / "root").string(), &names));
    CHECK(names.empty());
  }
  SUBCASE("Nested search roots are rejected") {
    writeFile(tmpDir / "root" / "sub" / "mod.mdl",
              "#smdl\nexport const int x = 1;\n");
    {
      smdl::Compiler compiler{};
      REQUIRE(!compiler.add((tmpDir / "root").string()));
      auto error{compiler.add((tmpDir / "root" / "sub").string())};
      REQUIRE(error.has_value());
      CHECK(error->message.find("nested") != std::string::npos);
    }
    {
      smdl::Compiler compiler{};
      REQUIRE(!compiler.add((tmpDir / "root" / "sub").string()));
      auto error{compiler.add((tmpDir / "root").string())};
      REQUIRE(error.has_value());
      CHECK(error->message.find("nested") != std::string::npos);
    }
  }
  SUBCASE("Same qualified name across roots loads both, later is shadowed") {
    writeFile(tmpDir / "root1" / "util.mdl",
              "#smdl\nexport const int marker_a = 1;\n");
    writeFile(tmpDir / "root2" / "util.mdl",
              "#smdl\nexport const int marker_b = 1;\n");
    writeFile(tmpDir / "root2" / "main.mdl",
              "#smdl\nimport ::df::*;\nimport util::marker_b;\n" +
                  materialDef("main_ok"));
    smdl::Compiler compiler{};
    auto names1{std::vector<std::string>()};
    auto names2{std::vector<std::string>()};
    REQUIRE(!compiler.add((tmpDir / "root1").string(), &names1));
    REQUIRE(!compiler.add((tmpDir / "root2").string(), &names2));
    CHECK(names1 == std::vector<std::string>{"::util"});
    // The shadowed module is still loaded and reported (a warning is
    // logged), and the weak-relative import inside 'root2' still binds
    // 'root2/util.mdl', so the build succeeds.
    std::sort(names2.begin(), names2.end());
    CHECK(names2 == std::vector<std::string>{"::main", "::util"});
    REQUIRE(!compiler.compile(smdl::OPT_LEVEL_NONE));
    REQUIRE(!compiler.jitCompile());
    CHECK(compiler.findMaterial("main_ok") != nullptr);
  }
  fs::remove_all(tmpDir);
}

TEST_CASE("Compiler findMaterial") {
  auto tmpDir{fs::temp_directory_path() / "smdl-compiler-test"};
  fs::remove_all(tmpDir);
  SUBCASE("Qualified and suffix lookup") {
    writeFile(tmpDir / "root" / "alpha.mdl",
              "#smdl\nimport ::df::*;\n" + materialDef("unique_mat") +
                  materialDef("dup"));
    writeFile(tmpDir / "root" / "beta.mdl",
              "#smdl\nimport ::df::*;\n" + materialDef("dup") +
                  materialDef("beta_only"));
    smdl::Compiler compiler{};
    REQUIRE(buildAll(compiler, {tmpDir / "root"}) == "");
    // Unique bare name resolves and carries the qualified identity.
    auto material{compiler.findMaterial("unique_mat")};
    REQUIRE(material != nullptr);
    CHECK(material->moduleName == "alpha");
    CHECK(material->materialName == "unique_mat");
    CHECK(material->qualifiedName == "::alpha::unique_mat");
    CHECK(fs::path(material->moduleFileName).filename() == "alpha.mdl");
    CHECK(material->lineNo > 0);
    // Absent name is null.
    CHECK(compiler.findMaterial("no_such_material") == nullptr);
    // An ambiguous name is null and logs an error listing the
    // candidates; 'findMaterials' enumerates them.
    CHECK(compiler.findMaterial("dup") == nullptr);
    CHECK(compiler.findMaterials("dup").size() == 2);
    // Module-qualified suffixes disambiguate.
    auto dupAlpha{compiler.findMaterial("alpha::dup")};
    auto dupBeta{compiler.findMaterial("beta::dup")};
    REQUIRE(dupAlpha != nullptr);
    REQUIRE(dupBeta != nullptr);
    CHECK(dupAlpha != dupBeta);
    CHECK(dupAlpha->qualifiedName == "::alpha::dup");
    CHECK(dupBeta->qualifiedName == "::beta::dup");
    CHECK(compiler.findMaterial("alpha::beta_only") == nullptr);
    CHECK(compiler.findMaterial("gamma::dup") == nullptr);
    // A leading '::' requires an exact match of the full name.
    CHECK(compiler.findMaterial("::alpha::dup") == dupAlpha);
    CHECK(compiler.findMaterial("::dup") == nullptr);
    // JIT symbols are deterministic dotted qualified names.
    CHECK(dupAlpha->evaluate.name == "alpha.dup.evaluate");
    CHECK(dupBeta->evaluate.name == "beta.dup.evaluate");
    // Enumeration exposes everything.
    CHECK(compiler.getMaterials().size() == 4);
  }
  SUBCASE("Suffix matching at multiple depths") {
    writeFile(tmpDir / "root" / "vendor" / "metals" / "steel.mdl",
              "#smdl\nimport ::df::*;\n" + materialDef("brushed"));
    smdl::Compiler compiler{};
    REQUIRE(buildAll(compiler, {tmpDir / "root"}) == "");
    auto material{compiler.findMaterial("brushed")};
    REQUIRE(material != nullptr);
    CHECK(material->qualifiedName == "::vendor::metals::steel::brushed");
    CHECK(material->evaluate.name == "vendor.metals.steel.brushed.evaluate");
    CHECK(compiler.findMaterial("steel::brushed") == material);
    CHECK(compiler.findMaterial("metals::steel::brushed") == material);
    CHECK(compiler.findMaterial("vendor::metals::steel::brushed") == material);
    CHECK(compiler.findMaterial("::vendor::metals::steel::brushed") ==
          material);
    // Not suffixes: absolute mismatch, interior components, non-boundary.
    CHECK(compiler.findMaterial("::steel::brushed") == nullptr);
    CHECK(compiler.findMaterial("metals::brushed") == nullptr);
    CHECK(compiler.findMaterial("shed") == nullptr);
  }
  SUBCASE("Namespace-nested materials") {
    writeFile(tmpDir / "root" / "nsmod.mdl",
              "#smdl\nimport ::df::*;\n"
              "namespace outer {\n"
              "namespace inner {\n" +
                  materialDef("nested") +
                  "}\n"
                  "}\n");
    smdl::Compiler compiler{};
    REQUIRE(buildAll(compiler, {tmpDir / "root"}) == "");
    auto material{compiler.findMaterial("nested")};
    REQUIRE(material != nullptr);
    CHECK(material->qualifiedName == "::nsmod::outer::inner::nested");
    CHECK(material->evaluate.name == "nsmod.outer.inner.nested.evaluate");
    CHECK(compiler.findMaterial("inner::nested") == material);
    CHECK(compiler.findMaterial("outer::inner::nested") == material);
    CHECK(compiler.findMaterial("nsmod::outer::inner::nested") == material);
    // Skipping interior components is not a suffix.
    CHECK(compiler.findMaterial("nsmod::nested") == nullptr);
  }
  SUBCASE("Same module name in different search roots") {
    writeFile(tmpDir / "root1" / "mat.mdl",
              "#smdl\nimport ::df::*;\n" + materialDef("shared_name") +
                  materialDef("only_r1"));
    writeFile(tmpDir / "root2" / "mat.mdl",
              "#smdl\nimport ::df::*;\n" + materialDef("shared_name") +
                  materialDef("only_r2"));
    smdl::Compiler compiler{};
    REQUIRE(buildAll(compiler, {tmpDir / "root1", tmpDir / "root2"}) == "");
    // 'root2/mat.mdl' is shadowed by 'root1/mat.mdl', so its materials
    // are unreachable by name, mirroring the unreachability of the
    // module itself by qualified name.
    auto material{compiler.findMaterial("mat::shared_name")};
    REQUIRE(material != nullptr);
    CHECK(material->moduleFileName.find("root1") != std::string::npos);
    CHECK(compiler.findMaterials("shared_name").size() == 1);
    CHECK(compiler.findMaterial("mat::only_r1") != nullptr);
    CHECK(compiler.findMaterial("mat::only_r2") == nullptr);
    CHECK(compiler.findMaterial("only_r2") == nullptr);
    // Enumeration still exposes the shadowed materials, flagged.
    auto materials{compiler.getMaterials()};
    REQUIRE(materials.size() == 4);
    auto numShadowed{size_t(0)};
    for (const auto &each : materials)
      numShadowed += each.moduleIsShadowed ? 1 : 0;
    CHECK(numShadowed == 2);
    // Shadow duplicates get deterministic ordinal symbol suffixes.
    for (const auto &each : materials) {
      if (each.qualifiedName == "::mat::shared_name") {
        CHECK(each.evaluate.name == (each.moduleIsShadowed
                                         ? "mat.shared_name.1.evaluate"
                                         : "mat.shared_name.evaluate"));
      }
    }
  }
  SUBCASE("Symbols are deterministic across identical compiles") {
    writeFile(tmpDir / "root1" / "mat.mdl",
              "#smdl\nimport ::df::*;\n" + materialDef("shared_name"));
    writeFile(tmpDir / "root2" / "mat.mdl",
              "#smdl\nimport ::df::*;\n" + materialDef("shared_name"));
    auto symbolNames{[&]() {
      smdl::Compiler compiler{};
      REQUIRE(buildAll(compiler, {tmpDir / "root1", tmpDir / "root2"}) == "");
      auto names{std::vector<std::string>()};
      for (const auto &each : compiler.getMaterials())
        names.push_back(each.evaluate.name);
      std::sort(names.begin(), names.end());
      return names;
    }};
    CHECK(symbolNames() == symbolNames());
  }
  fs::remove_all(tmpDir);
}
