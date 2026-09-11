#include "CompileFixtures.h"

#include <algorithm>
#include <cmath>
#include <filesystem>
#include <string>
#include <vector>

#include "smdl/Compiler.h"
#include "smdl/Support/MD5Hash.h"

namespace fs = std::filesystem;

TEST_CASE("Compiler: the import resolution order") {
  TempDir tmpDir{"compiler-modules"};
  SUBCASE("Weak-relative import prefers the importing module's directory") {
    tmpDir.write("root/util.mdl", "#smdl\nexport const int marker_top = 1;\n");
    tmpDir.write("root/sub/util.mdl",
                 "#smdl\nexport const int marker_sub = 1;\n");
    tmpDir.write("root/sub/main.mdl",
                 "#smdl\nimport ::df::*;\nimport util::marker_sub;\n" +
                     minimalMaterial("main_ok"));
    smdl::Compiler compiler{};
    CHECK(buildAll(compiler, {tmpDir / "root"}) == "");
    CHECK(compiler.findMaterial("main_ok") != nullptr);
  }
  SUBCASE("Import binds the resolved module even if the name is absent") {
    // 'sub/util.mdl' shadows 'util.mdl' for 'sub/main.mdl', and there is
    // no fallback re-resolution when the imported name is missing.
    tmpDir.write("root/util.mdl", "#smdl\nexport const int marker_top = 1;\n");
    tmpDir.write("root/sub/util.mdl",
                 "#smdl\nexport const int marker_sub = 1;\n");
    tmpDir.write("root/sub/main.mdl",
                 "#smdl\nimport ::df::*;\nimport util::marker_top;\n" +
                     minimalMaterial("main_bad"));
    smdl::Compiler compiler{};
    auto message{buildAll(compiler, {tmpDir / "root"})};
    CHECK(message != "");
    CHECK_CONTAINS(message, "cannot resolve import");
  }
  SUBCASE("Weak-relative import falls back to search roots in add order") {
    tmpDir.write("rootA/util.mdl", "#smdl\nexport const int marker_a = 1;\n");
    tmpDir.write("rootB/util.mdl", "#smdl\nexport const int marker_b = 1;\n");
    tmpDir.write("rootC/main.mdl",
                 "#smdl\nimport ::df::*;\nimport util::marker_a;\n" +
                     minimalMaterial("main_ok"));
    {
      // 'rootA' added first: its 'util' wins, so 'marker_a' resolves.
      smdl::Compiler compiler{};
      CHECK(buildAll(compiler, {tmpDir / "rootA", tmpDir / "rootB",
                                tmpDir / "rootC"}) == "");
    }
    {
      // 'rootB' added first: its 'util' wins, so 'marker_a' does not.
      smdl::Compiler compiler{};
      auto message{buildAll(
          compiler, {tmpDir / "rootB", tmpDir / "rootA", tmpDir / "rootC"})};
      CHECK_CONTAINS(message, "cannot resolve import");
    }
  }
  SUBCASE("Strict-relative '.' import never falls back to search roots") {
    tmpDir.write("root/util.mdl", "#smdl\nexport const int marker_top = 1;\n");
    tmpDir.write("root/pkg/strict.mdl", "#smdl\nimport .::util::marker_top;\n");
    {
      smdl::Compiler compiler{};
      auto message{buildAll(compiler, {tmpDir / "root"})};
      CHECK_CONTAINS(message, "cannot resolve import");
    }
    // The same import spelled weakly succeeds via the search root.
    tmpDir.write("root/pkg/strict.mdl", "#smdl\nimport util::marker_top;\n");
    {
      smdl::Compiler compiler{};
      CHECK(buildAll(compiler, {tmpDir / "root"}) == "");
    }
  }
  SUBCASE("A strict-relative '..' traverses to the parent") {
    tmpDir.write("root/p2/helper.mdl", "#smdl\nexport const int marker = 1;\n");
    tmpDir.write("root/p1/main.mdl",
                 "#smdl\nimport ::df::*;\nimport ..::p2::helper::marker;\n" +
                     minimalMaterial("main_ok"));
    smdl::Compiler compiler{};
    CHECK(buildAll(compiler, {tmpDir / "root"}) == "");
    CHECK(compiler.findMaterial("main_ok") != nullptr);
  }
  SUBCASE("Absolute '::' prefers builtins over search roots") {
    // A module named 'df' on disk cannot shadow the builtin '::df' ...
    tmpDir.write("root/df.mdl", "#smdl\nexport const int fake_fn = 1;\n");
    tmpDir.write("root/main.mdl", "#smdl\nimport ::df::fake_fn;\n");
    {
      smdl::Compiler compiler{};
      auto message{buildAll(compiler, {tmpDir / "root"})};
      CHECK_CONTAINS(message, "cannot resolve import");
    }
    // ... but a weak-relative 'df' import binds the disk module.
    tmpDir.write("root/main.mdl", "#smdl\nimport df::fake_fn;\n");
    {
      smdl::Compiler compiler{};
      CHECK(buildAll(compiler, {tmpDir / "root"}) == "");
    }
  }
  SUBCASE("A nested builtin resolves absolutely") {
    tmpDir.write("root/main.mdl",
                 "#smdl\nimport ::df::*;\nimport ::models::prospect::*;\n"
                 "import ::models::marmit::*;\n" +
                     minimalMaterial("main_ok"));
    smdl::Compiler compiler{};
    CHECK(buildAll(compiler, {tmpDir / "root"}) == "");
    CHECK(compiler.findMaterial("main_ok") != nullptr);
  }
  SUBCASE("Nested builtin priority mirrors single-component rules") {
    // A disk module at 'models/prospect.mdl' cannot shadow the builtin
    // '::models::prospect' on an absolute import ...
    tmpDir.write("root/models/prospect.mdl",
                 "#smdl\nexport const int fake_fn = 1;\n");
    tmpDir.write("root/main.mdl",
                 "#smdl\nimport ::models::prospect::fake_fn;\n");
    {
      smdl::Compiler compiler{};
      auto message{buildAll(compiler, {tmpDir / "root"})};
      CHECK_CONTAINS(message, "cannot resolve import");
    }
    // ... but a weak import binds the disk module.
    tmpDir.write("root/main.mdl", "#smdl\nimport models::prospect::fake_fn;\n");
    {
      smdl::Compiler compiler{};
      CHECK(buildAll(compiler, {tmpDir / "root"}) == "");
    }
  }
  SUBCASE("Weak import falls back to nested builtins") {
    // No disk 'models/prospect' anywhere, so the weak path reaches the
    // builtin after the relative and search-root strategies miss.
    tmpDir.write("root/main.mdl", "#smdl\nimport models::prospect::*;\n");
    smdl::Compiler compiler{};
    CHECK(buildAll(compiler, {tmpDir / "root"}) == "");
  }
  SUBCASE("A cyclic import is reported at the import that closes it") {
    // Host-supplied modules compile in the order they were added, so the
    // cycle is always entered through '::a'.
    smdl::Compiler compiler{};
    REQUIRE_OK(compiler.addCode("::a", "#smdl\nimport ::b::*;\n"));
    REQUIRE_OK(compiler.addCode("::b", "#smdl\nimport ::c::*;\n"));
    REQUIRE_OK(compiler.addCode("::c", "#smdl\nimport ::a::*;\n"));
    const auto error{compiler.compile(smdl::OPT_LEVEL_NONE)};
    REQUIRE(error.has_value());
    CHECK_CONTAINS(error->message,
                   "[<string ::c>:2:1] cyclic import: '::a' imports '::b', "
                   "which imports '::c', which imports '::a'");
    CHECK_CONTAINS(error->snippet, "import ::a::*;");
  }
  SUBCASE("Using aliases resolve through the same machinery") {
    tmpDir.write("root/target.mdl", "#smdl\nexport const int marker = 1;\n");
    tmpDir.write("root/sub/helper.mdl",
                 "#smdl\nexport const int marker = 2;\n");
    tmpDir.write("root/main.mdl", "#smdl\n"
                                  "import ::df::*;\n"
                                  "using u = \"target\";\n"
                                  "using v = .::sub::helper;\n"
                                  "import u::marker;\n"
                                  "import v::*;\n" +
                                      minimalMaterial("main_ok"));
    smdl::Compiler compiler{};
    CHECK(buildAll(compiler, {tmpDir / "root"}) == "");
    CHECK(compiler.findMaterial("main_ok") != nullptr);
  }
}

TEST_CASE("Compiler: the qualified name a search root derives") {
  TempDir tmpDir{"compiler-identity"};
  SUBCASE("Qualified names derive from search roots") {
    tmpDir.write("root/top.mdl", "#smdl\nexport const int x = 1;\n");
    tmpDir.write("root/vendor/metals/steel.mdl",
                 "#smdl\nexport const int x = 1;\n");
    smdl::Compiler compiler{};
    auto names{std::vector<std::string>()};
    REQUIRE_OK(compiler.add((tmpDir / "root").string(), &names));
    std::sort(names.begin(), names.end());
    CHECK(names ==
          std::vector<std::string>{"::top", "::vendor::metals::steel"});
  }
  SUBCASE("Single-file add uses the parent directory as implicit root") {
    tmpDir.write("dir/pkg/mod.mdl", "#smdl\nexport const int x = 1;\n");
    smdl::Compiler compiler{};
    auto names{std::vector<std::string>()};
    REQUIRE(
        !compiler.add((tmpDir / "dir" / "pkg" / "mod.mdl").string(), &names));
    CHECK(names == std::vector<std::string>{"::mod"});
  }
  SUBCASE("Re-adding the same search root is a no-op") {
    tmpDir.write("root/mod.mdl", "#smdl\nexport const int x = 1;\n");
    smdl::Compiler compiler{};
    REQUIRE_OK(compiler.add((tmpDir / "root").string()));
    auto names{std::vector<std::string>()};
    CHECK_OK(compiler.add((tmpDir / "root").string(), &names));
    CHECK(names.empty());
  }
  SUBCASE("Nested search roots are rejected") {
    tmpDir.write("root/sub/mod.mdl", "#smdl\nexport const int x = 1;\n");
    {
      smdl::Compiler compiler{};
      REQUIRE_OK(compiler.add((tmpDir / "root").string()));
      CHECK_ERROR(compiler.add((tmpDir / "root" / "sub").string()), "nested");
    }
    {
      smdl::Compiler compiler{};
      REQUIRE_OK(compiler.add((tmpDir / "root" / "sub").string()));
      CHECK_ERROR(compiler.add((tmpDir / "root").string()), "nested");
    }
  }
  SUBCASE("Same qualified name across roots loads both, later is shadowed") {
    tmpDir.write("root1/util.mdl", "#smdl\nexport const int marker_a = 1;\n");
    tmpDir.write("root2/util.mdl", "#smdl\nexport const int marker_b = 1;\n");
    tmpDir.write("root2/main.mdl",
                 "#smdl\nimport ::df::*;\nimport util::marker_b;\n" +
                     minimalMaterial("main_ok"));
    smdl::Compiler compiler{};
    auto names1{std::vector<std::string>()};
    auto names2{std::vector<std::string>()};
    REQUIRE_OK(compiler.add((tmpDir / "root1").string(), &names1));
    REQUIRE_OK(compiler.add((tmpDir / "root2").string(), &names2));
    CHECK(names1 == std::vector<std::string>{"::util"});
    // The shadowed module is still loaded and reported (a warning is
    // logged), and the weak-relative import inside 'root2' still binds
    // 'root2/util.mdl', so the build succeeds.
    std::sort(names2.begin(), names2.end());
    CHECK(names2 == std::vector<std::string>{"::main", "::util"});
    REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_NONE));
    REQUIRE_OK(compiler.jitCompile());
    CHECK(compiler.findMaterial("main_ok") != nullptr);
  }
}

TEST_CASE("Compiler: an MDR archive") {
  TempDir tmpDir{"compiler-archives"};
  SUBCASE("Archive names encode the package prefix") {
    writeZip(
        tmpDir / "root" / "vendor.metals.mdr",
        {{"vendor/metals.mdl", "#smdl\nexport const int metals_marker = 1;\n"},
         {"vendor/metals/steel.mdl", "#smdl\nimport ::df::*;\n"
                                     "import ..::metals::metals_marker;\n" +
                                         minimalMaterial("brushed")}});
    // A loose module importing through the archive: absolutely and
    // weakly.
    tmpDir.write("root/main.mdl", "#smdl\nimport ::df::*;\n"
                                  "import ::vendor::metals::steel::*;\n"
                                  "import vendor::metals::metals_marker;\n" +
                                      minimalMaterial("main_ok"));
    smdl::Compiler compiler{};
    auto names{std::vector<std::string>()};
    REQUIRE_OK(compiler.add((tmpDir / "root").string(), &names));
    std::sort(names.begin(), names.end());
    CHECK(names == std::vector<std::string>{"::main", "::vendor::metals",
                                            "::vendor::metals::steel"});
    REQUIRE(buildAll(compiler, {tmpDir / "root"}) == "");
    CHECK(compiler.findMaterial("main_ok") != nullptr);
    auto materialDef{compiler.findMaterial("brushed")};
    REQUIRE(materialDef != nullptr);
    CHECK(materialDef->qualifiedName == "::vendor::metals::steel::brushed");
    CHECK_CONTAINS(materialDef->moduleFileName, "vendor.metals.mdr");
  }
  SUBCASE("Non-conforming archives are rejected") {
    writeZip(tmpDir / "root" / "vendor.metals.mdr",
             {{"other/thing.mdl", "#smdl\nexport const int x = 1;\n"}});
    smdl::Compiler compiler{};
    CHECK_ERROR(compiler.add((tmpDir / "root").string()), "conform");
  }
  SUBCASE("Empty package prefix components are rejected") {
    writeZip(tmpDir / "root" / "vendor..metals.mdr",
             {{"vendor/metals.mdl", "#smdl\n"}});
    smdl::Compiler compiler{};
    CHECK_ERROR(compiler.add((tmpDir / "root").string()),
                "empty package prefix");
  }
  SUBCASE("Loose duplicates of archive contents are errors") {
    writeZip(tmpDir / "root" / "vendor.metals.mdr",
             {{"vendor/metals.mdl", "#smdl\nexport const int x = 1;\n"}});
    tmpDir.write("root/vendor/metals/extra.mdl",
                 "#smdl\nexport const int y = 1;\n");
    {
      smdl::Compiler compiler{};
      CHECK_ERROR(compiler.add((tmpDir / "root").string()),
                  "conflicts with loose");
    }
    // Loose siblings outside the enclosed package are fine.
    fs::remove_all(tmpDir / "root" / "vendor" / "metals");
    tmpDir.write("root/vendor/other.mdl", "#smdl\nexport const int y = 1;\n");
    {
      smdl::Compiler compiler{};
      auto names{std::vector<std::string>()};
      REQUIRE_OK(compiler.add((tmpDir / "root").string(), &names));
      std::sort(names.begin(), names.end());
      CHECK(names ==
            std::vector<std::string>{"::vendor::metals", "::vendor::other"});
    }
  }
  SUBCASE("Overlapping archive prefixes are errors") {
    writeZip(tmpDir / "root" / "a.b.mdr", {{"a/b.mdl", "#smdl\n"}});
    writeZip(tmpDir / "root" / "a.b.c.mdr", {{"a/b/c.mdl", "#smdl\n"}});
    {
      smdl::Compiler compiler{};
      CHECK_ERROR(compiler.add((tmpDir / "root").string()), "overlapping");
    }
    // Sibling prefixes are fine.
    fs::remove(tmpDir / "root" / "a.b.c.mdr");
    writeZip(tmpDir / "root" / "a.c.mdr", {{"a/c.mdl", "#smdl\n"}});
    {
      smdl::Compiler compiler{};
      auto names{std::vector<std::string>()};
      REQUIRE_OK(compiler.add((tmpDir / "root").string(), &names));
      std::sort(names.begin(), names.end());
      CHECK(names == std::vector<std::string>{"::a::b", "::a::c"});
    }
  }
  SUBCASE("An archive under a later root is shadowed") {
    auto archiveEntry{"#smdl\nimport ::df::*;\n" +
                      minimalMaterial("shared_arch")};
    writeZip(tmpDir / "root1" / "vendor.metals.mdr",
             {{"vendor/metals.mdl", archiveEntry}});
    writeZip(tmpDir / "root2" / "vendor.metals.mdr",
             {{"vendor/metals.mdl", archiveEntry}});
    smdl::Compiler compiler{};
    REQUIRE(buildAll(compiler, {tmpDir / "root1", tmpDir / "root2"}) == "");
    auto materialDef{compiler.findMaterial("shared_arch")};
    REQUIRE(materialDef != nullptr);
    CHECK_CONTAINS(materialDef->moduleFileName, "root1");
    CHECK(compiler.findMaterials("shared_arch").size() == 1);
  }
  SUBCASE("Archives below the top level are ignored") {
    writeZip(tmpDir / "root" / "sub" / "x.y.mdr", {{"x/y.mdl", "#smdl\n"}});
    tmpDir.write("root/mod.mdl", "#smdl\nexport const int x = 1;\n");
    smdl::Compiler compiler{};
    auto names{std::vector<std::string>()};
    REQUIRE_OK(compiler.add((tmpDir / "root").string(), &names));
    CHECK(names == std::vector<std::string>{"::mod"});
  }
}

TEST_CASE("Compiler: an MDLE container") {
  TempDir tmpDir{"compiler-mdle"};
  const auto mainModule{"#smdl\nimport ::df::*;\nexport const int m = 1;\n" +
                        minimalMaterial("main")};
  SUBCASE("Content-based identity and the 'main' convention") {
    writeZip(tmpDir / "CoolSteel.mdle", {{"main.mdl", mainModule}});
    auto expectedName{"::mdle::" + std::string(smdl::MD5Hash::hashFile(
                                       (tmpDir / "CoolSteel.mdle").string()))};
    smdl::Compiler compiler{};
    auto names{std::vector<std::string>()};
    REQUIRE(buildAll(compiler, {tmpDir / "CoolSteel.mdle"}, &names) == "");
    REQUIRE(names == std::vector<std::string>{expectedName});
    auto materialDef{compiler.findMaterial(expectedName + "::main")};
    REQUIRE(materialDef != nullptr);
    CHECK(materialDef->qualifiedName == expectedName + "::main");
    CHECK(materialDef->moduleName == "CoolSteel");
    CHECK_CONTAINS(materialDef->moduleFileName, "CoolSteel.mdle");
    // Unique here, so the bare suffix also resolves.
    CHECK(compiler.findMaterial("main") == materialDef);
  }
  SUBCASE("Identical containers dedupe, distinct containers cannot collide") {
    writeZip(tmpDir / "a" / "one.mdle", {{"main.mdl", mainModule}});
    writeZip(tmpDir / "b" / "two.mdle", {{"main.mdl", mainModule}});
    auto otherModule{"#smdl\nimport ::df::*;\nexport const int m = 2;\n" +
                     minimalMaterial("main")};
    writeZip(tmpDir / "c" / "three.mdle", {{"main.mdl", otherModule}});
    smdl::Compiler compiler{};
    auto names{std::vector<std::string>()};
    REQUIRE(buildAll(compiler,
                     {tmpDir / "a" / "one.mdle", tmpDir / "b" / "two.mdle",
                      tmpDir / "c" / "three.mdle"},
                     &names) == "");
    REQUIRE(names.size() == 3);
    // Identical bytes at different paths report the same handle and
    // load once; different bytes get a different handle.
    CHECK(names[0] == names[1]);
    CHECK(names[0] != names[2]);
    CHECK(compiler.getMaterials().size() == 2);
    // The ambiguous bare name is refused, the handles disambiguate.
    CHECK(compiler.findMaterial("main") == nullptr);
    CHECK(compiler.findMaterial(names[0] + "::main") != nullptr);
    CHECK(compiler.findMaterial(names[2] + "::main") != nullptr);
    // No shadow warnings: nothing is marked shadowed.
    for (const auto &each : compiler.getMaterials())
      CHECK(!each.moduleIsShadowed);
  }
  SUBCASE("Missing 'main.mdl' is an error") {
    writeZip(tmpDir / "bad.mdle", {{"other.mdl", "#smdl\n"}});
    smdl::Compiler compiler{};
    CHECK_ERROR(compiler.add((tmpDir / "bad.mdle").string()), "main.mdl");
  }
  SUBCASE("Directory walks do not ingest MDLEs") {
    writeZip(tmpDir / "root" / "loose.mdle", {{"main.mdl", mainModule}});
    tmpDir.write("root/mod.mdl", "#smdl\nexport const int x = 1;\n");
    smdl::Compiler compiler{};
    auto names{std::vector<std::string>()};
    REQUIRE_OK(compiler.add((tmpDir / "root").string(), &names));
    CHECK(names == std::vector<std::string>{"::mod"});
  }
  SUBCASE("Container resources extract and anchor resource lookups") {
    // Generate a tiny PNG with the library's own writer and pack it
    // beside a 'main.mdl' that references it.
    const uint8_t texels[12] = {255, 0, 0, 0, 255, 0, 0, 0, 255, 255, 255, 255};
    REQUIRE_OK(
        smdl::write8bitImage((tmpDir / "wood.png").string(), 2, 2, 3, texels));
    const auto pngBytes{tmpDir.read("wood.png")};
    REQUIRE(!pngBytes.empty());
    writeZip(tmpDir / "Textured.mdle",
             {{"main.mdl", "#smdl\nimport ::df::*;\nimport ::tex::*;\n" +
                               minimalMaterial("main") +
                               "unit_test \"MDLE texture\" {\n"
                               "  const auto t = texture_2d(\"wood.png\", "
                               "tex::gamma_linear);\n"
                               "  #assert(tex::texture_isvalid(t));\n"
                               "  #assert(tex::width(t) == 2);\n"
                               "  #assert(tex::height(t) == 2);\n"
                               "}\n"},
              {"wood.png", pngBytes}});
    auto hash{std::string(
        smdl::MD5Hash::hashFile((tmpDir / "Textured.mdle").string()))};
    smdl::Compiler compiler{};
    compiler.shouldEmitUnitTests = true;
    REQUIRE(buildAll(compiler, {tmpDir / "Textured.mdle"}) == "");
    CHECK(compiler.findMaterial("main") != nullptr);
    // The resource was extracted to the content-addressed cache.
    CHECK(fs::is_regular_file(fs::temp_directory_path() /
                              ("smdl-mdle-" + hash) / "wood.png"));
    // Run the in-container unit test: it asserts the texture actually
    // loaded (a resource that failed to resolve would only have
    // produced a warning and a default texture).
    StateStorage storage{compiler};
    auto state{storage.makeState()};
    REQUIRE_OK(compiler.runUnitTests(state));
  }
}

TEST_CASE("findMaterial: looking a material up by name") {
  TempDir tmpDir{"compiler-materials"};
  SUBCASE("A material is found by qualified name and by suffix") {
    tmpDir.write("root/alpha.mdl", "#smdl\nimport ::df::*;\n" +
                                       minimalMaterial("unique_mat") +
                                       minimalMaterial("dup"));
    tmpDir.write("root/beta.mdl", "#smdl\nimport ::df::*;\n" +
                                      minimalMaterial("dup") +
                                      minimalMaterial("beta_only"));
    smdl::Compiler compiler{};
    REQUIRE(buildAll(compiler, {tmpDir / "root"}) == "");
    // Unique bare name resolves and carries the qualified identity.
    auto materialDef{compiler.findMaterial("unique_mat")};
    REQUIRE(materialDef != nullptr);
    CHECK(materialDef->moduleName == "alpha");
    CHECK(materialDef->materialName == "unique_mat");
    CHECK(materialDef->qualifiedName == "::alpha::unique_mat");
    CHECK(fs::path(materialDef->moduleFileName).filename() == "alpha.mdl");
    CHECK(materialDef->lineNo > 0);
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
    tmpDir.write("root/vendor/metals/steel.mdl",
                 "#smdl\nimport ::df::*;\n" + minimalMaterial("brushed"));
    smdl::Compiler compiler{};
    REQUIRE(buildAll(compiler, {tmpDir / "root"}) == "");
    auto materialDef{compiler.findMaterial("brushed")};
    REQUIRE(materialDef != nullptr);
    CHECK(materialDef->qualifiedName == "::vendor::metals::steel::brushed");
    CHECK(materialDef->evaluate.name == "vendor.metals.steel.brushed.evaluate");
    CHECK(compiler.findMaterial("steel::brushed") == materialDef);
    CHECK(compiler.findMaterial("metals::steel::brushed") == materialDef);
    CHECK(compiler.findMaterial("vendor::metals::steel::brushed") ==
          materialDef);
    CHECK(compiler.findMaterial("::vendor::metals::steel::brushed") ==
          materialDef);
    // Not suffixes: absolute mismatch, interior components, non-boundary.
    CHECK(compiler.findMaterial("::steel::brushed") == nullptr);
    CHECK(compiler.findMaterial("metals::brushed") == nullptr);
    CHECK(compiler.findMaterial("shed") == nullptr);
  }
  SUBCASE("A material inside a namespace is found by its path") {
    tmpDir.write("root/nsmod.mdl", "#smdl\nimport ::df::*;\n"
                                   "namespace outer {\n"
                                   "namespace inner {\n" +
                                       minimalMaterial("nested") +
                                       "}\n"
                                       "}\n");
    smdl::Compiler compiler{};
    REQUIRE(buildAll(compiler, {tmpDir / "root"}) == "");
    auto materialDef{compiler.findMaterial("nested")};
    REQUIRE(materialDef != nullptr);
    CHECK(materialDef->qualifiedName == "::nsmod::outer::inner::nested");
    CHECK(materialDef->evaluate.name == "nsmod.outer.inner.nested.evaluate");
    CHECK(compiler.findMaterial("inner::nested") == materialDef);
    CHECK(compiler.findMaterial("outer::inner::nested") == materialDef);
    CHECK(compiler.findMaterial("nsmod::outer::inner::nested") == materialDef);
    // Skipping interior components is not a suffix.
    CHECK(compiler.findMaterial("nsmod::nested") == nullptr);
  }
  SUBCASE("Same module name in different search roots") {
    tmpDir.write("root1/mat.mdl", "#smdl\nimport ::df::*;\n" +
                                      minimalMaterial("shared_name") +
                                      minimalMaterial("only_r1"));
    tmpDir.write("root2/mat.mdl", "#smdl\nimport ::df::*;\n" +
                                      minimalMaterial("shared_name") +
                                      minimalMaterial("only_r2"));
    smdl::Compiler compiler{};
    REQUIRE(buildAll(compiler, {tmpDir / "root1", tmpDir / "root2"}) == "");
    // 'root2/mat.mdl' is shadowed by 'root1/mat.mdl', so its materials
    // are unreachable by name, mirroring the unreachability of the
    // module itself by qualified name.
    auto materialDef{compiler.findMaterial("mat::shared_name")};
    REQUIRE(materialDef != nullptr);
    CHECK_CONTAINS(materialDef->moduleFileName, "root1");
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
    tmpDir.write("root1/mat.mdl",
                 "#smdl\nimport ::df::*;\n" + minimalMaterial("shared_name"));
    tmpDir.write("root2/mat.mdl",
                 "#smdl\nimport ::df::*;\n" + minimalMaterial("shared_name"));
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
}

TEST_CASE("printMaterialSummary: the materials of each module") {
  smdl::Compiler compiler{};
  REQUIRE_OK(compiler.addCode("::one", "#smdl\nimport ::df::*;\n" +
                                           minimalMaterial("m")));
  REQUIRE_OK(compiler.addCode("::two", "#smdl\nimport ::df::*;\n" +
                                           minimalMaterial("m1") +
                                           minimalMaterial("m2")));
  REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_NONE));
  const auto summary{compiler.printMaterialSummary()};
  CHECK_CONTAINS(summary, " contains 1 material:\n");
  CHECK_CONTAINS(summary, " contains 2 materials:\n");
}

TEST_CASE("jitCompile: a '@(foreign)' function the host does not define") {
  const CollectedLog logged{"smdlNoSuchSymbolAnywhere"};
  smdl::Compiler compiler{};
  REQUIRE_OK(compiler.addCode("::host::foreign",
                              "#smdl\n@(pure foreign)\n"
                              "int smdlNoSuchSymbolAnywhere(int x);\n"
                              "exec {\n"
                              "  #println(smdlNoSuchSymbolAnywhere(3));\n"
                              "}\n"));
  REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_NONE));
  const auto error{compiler.jitCompile()};
  REQUIRE(error.has_value());
  // The declaration is what there is to fix, so it leads, rather than the
  // entry point whose lookup happened to fail first.
  CHECK(smdl::startsWith(error->message, "[<string ::host::foreign>:3:1] "
                                         "'@(foreign)' function "
                                         "'smdlNoSuchSymbolAnywhere' is not "
                                         "defined in the host process"));
  CHECK_CONTAINS(error->snippet, "int smdlNoSuchSymbolAnywhere(int x);");
  // Said once, in the error, and not logged on the way as well.
  CHECK(logged.messages().empty());
}

TEST_CASE("setDesiredMaterials: compiling only what the host asked for") {
  TempDir tmpDir{"desired-materials"};
  tmpDir.write("root/mats.mdl", "#smdl\nimport ::df::*;\n" +
                                    minimalMaterial("wanted") +
                                    minimalMaterial("unwanted"));
  SUBCASE("Filter compiles only the desired materials") {
    smdl::Compiler compiler{};
    compiler.setDesiredMaterials({"wanted"});
    REQUIRE(buildAll(compiler, {tmpDir / "root"}) == "");
    REQUIRE(compiler.getMaterials().size() == 1);
    CHECK(compiler.getMaterials()[0].qualifiedName == "::mats::wanted");
    CHECK(compiler.findMaterial("wanted") != nullptr);
    // The skipped material is unreachable, and remembered by qualified
    // name so 'findMaterial' can log the exclusion.
    CHECK(compiler.findMaterial("unwanted") == nullptr);
    REQUIRE(compiler.getSkippedMaterialNames().size() == 1);
    CHECK(compiler.getSkippedMaterialNames()[0] == "::mats::unwanted");
    // Names that match nothing anywhere only warn; the build succeeds.
  }
  SUBCASE("Skipped materials emit no entry points at all") {
    smdl::Compiler compiler{};
    compiler.setDesiredMaterials({"wanted"});
    REQUIRE_OK(compiler.add((tmpDir / "root").string()));
    REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_NONE));
    auto ir{std::string()};
    REQUIRE_OK(compiler.dump(smdl::DUMP_FORMAT_IR, ir));
    CHECK_CONTAINS(ir, "mats.wanted.evaluate");
    CHECK_NOT_CONTAINS(ir, "mats.unwanted");
  }
  SUBCASE("Matching rules mirror findMaterial") {
    // Absolute names must match exactly; '::wanted' matches nothing, so
    // everything is skipped.
    smdl::Compiler compiler{};
    compiler.setDesiredMaterials({"::mats::wanted", "::unwanted"});
    REQUIRE(buildAll(compiler, {tmpDir / "root"}) == "");
    REQUIRE(compiler.getMaterials().size() == 1);
    CHECK(compiler.getMaterials()[0].qualifiedName == "::mats::wanted");
    CHECK(compiler.getSkippedMaterialNames().size() == 1);
  }
  SUBCASE("A kept material may instantiate a skipped one") {
    tmpDir.write("root2/variants.mdl", "#smdl\nimport ::df::*;\n" +
                                           minimalMaterial("base") +
                                           "material derived() = base();\n");
    smdl::Compiler compiler{};
    compiler.setDesiredMaterials({"derived"});
    REQUIRE(buildAll(compiler, {tmpDir / "root2"}) == "");
    REQUIRE(compiler.getMaterials().size() == 1);
    CHECK(compiler.findMaterial("derived") != nullptr);
    CHECK(compiler.getSkippedMaterialNames().size() == 1);
  }
  SUBCASE("Clearing the filter restores everything on recompile") {
    smdl::Compiler compiler{};
    compiler.setDesiredMaterials({"wanted"});
    REQUIRE(buildAll(compiler, {tmpDir / "root"}) == "");
    REQUIRE(compiler.getMaterials().size() == 1);
    compiler.setDesiredMaterials({});
    REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_NONE));
    REQUIRE_OK(compiler.jitCompile());
    CHECK(compiler.getMaterials().size() == 2);
    CHECK(compiler.getSkippedMaterialNames().empty());
  }
}

TEST_CASE(
    "Compiler: the spectral machinery at one, four, and sixty-four bands") {
  TempDir tmpDir{"compiler-wavelength"};
  // One module whose in-JIT unit test exercises the spectral machinery
  // that is easiest to get wrong away from the default 16-band grid:
  // comparisons over 'color' (at 1 band the compare result is a scalar
  // bool rather than a bool vector), '#any'/'#all' reductions of those
  // results, and RGB-to-color construction.
  tmpDir.write("spectral.smdl",
               "#smdl\nimport ::df::*;\n" + minimalMaterial("main") +
                   "unit_test \"Spectral basics\" {\n"
                   "  const color c = color(float3(0.8, 0.5, 0.2));\n"
                   "  #assert(#all(c >= 0.0));\n"
                   "  #assert(#any(color(0.5) > 0.0));\n"
                   "  #assert(#all(color(0.5) == 0.5));\n"
                   "  #assert(!#any(color(0.0) > 0.0));\n"
                   "}\n");
  for (uint32_t numBands : {1u, 4u, 64u}) {
    CAPTURE(numBands);
    smdl::Compiler compiler{numBands};
    compiler.shouldEmitUnitTests = true;
    REQUIRE(compiler.wavelengthBaseMax == numBands);
    REQUIRE(buildAll(compiler, {tmpDir / "spectral.smdl"}) == "");
    REQUIRE(compiler.findMaterial("main") != nullptr);
    // An endpoint-inclusive uniform grid over the visible; a single band
    // sits at the midpoint.
    StateStorage storage{compiler};
    auto state{storage.makeState()};
    REQUIRE_OK(compiler.runUnitTests(state));
    // The gray fast path of RGB-to-color is exact at any band count.
    auto colorBuf{std::vector<float>(size_t(numBands), -1.0f)};
    compiler.convertRGBToColor(state, smdl::float3(0.5f, 0.5f, 0.5f),
                               colorBuf.data());
    for (float band : colorBuf) CHECK(band == 0.5f);
    // A colored value upsamples to a finite non-negative spectrum.
    compiler.convertRGBToColor(state, smdl::float3(0.8f, 0.5f, 0.2f),
                               colorBuf.data());
    for (float band : colorBuf) {
      CHECK(band >= 0.0f);
      CHECK(std::isfinite(band));
    }
    // Black converts to black exactly, at any band count.
    std::fill(colorBuf.begin(), colorBuf.end(), 0.0f);
    auto rgbOfBlack{compiler.convertColorToRGB(state, colorBuf.data())};
    CHECK(rgbOfBlack[0] == 0.0f);
    CHECK(rgbOfBlack[1] == 0.0f);
    CHECK(rgbOfBlack[2] == 0.0f);
    // A positive spectrum converts to finite RGB at any band count; the
    // CIE quadrature only resolves an approximately-gray round trip once
    // the grid samples the visible densely (see the 64-band check below).
    std::fill(colorBuf.begin(), colorBuf.end(), 0.5f);
    auto rgbOfGray{compiler.convertColorToRGB(state, colorBuf.data())};
    for (int i = 0; i < 3; i++) CHECK(std::isfinite(rgbOfGray[i]));
    if (numBands == 64) {
      for (int i = 0; i < 3; i++) {
        CHECK(rgbOfGray[i] > 0.3f);
        CHECK(rgbOfGray[i] < 0.8f);
      }
    }
    // Explicit per-band quadrature weights. Uniform weights of
    // `(max - min) / N` reproduce the null-weight result up to
    // summation order, and scaling every weight scales the result
    // linearly.
    auto weights{std::vector<float>(
        size_t(numBands),
        (state.wavelengthMax - state.wavelengthMin) / float(numBands))};
    state.wavelengthWeight = weights.data();
    auto rgbWeighted{compiler.convertColorToRGB(state, colorBuf.data())};
    for (int i = 0; i < 3; i++)
      CHECK(rgbWeighted[i] ==
            doctest::Approx(rgbOfGray[i]).epsilon(1e-4).scale(1.0));
    for (auto &weight : weights) weight *= 2.0f;
    auto rgbDoubled{compiler.convertColorToRGB(state, colorBuf.data())};
    for (int i = 0; i < 3; i++)
      CHECK(rgbDoubled[i] ==
            doctest::Approx(2.0f * rgbWeighted[i]).epsilon(1e-5).scale(1.0));
    state.wavelengthWeight = nullptr;
  }
}

TEST_CASE("addCode: a module the host supplies as source") {
  TempDir tmpDir{"source-code"};
  // The render state the unit tests below run against.
  auto runUnitTests{[](smdl::Compiler &compiler) {
    StateStorage storage{compiler};
    auto state{storage.makeState()};
    if (auto error{compiler.runUnitTests(state)}) return error->message;
    return std::string();
  }};
  SUBCASE("Source code compiles as a module with no file") {
    smdl::Compiler compiler{};
    REQUIRE_OK(compiler.addCode("::host::mats", "#smdl\nimport ::df::*;\n" +
                                                    minimalMaterial("mat_ok")));
    REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_NONE));
    REQUIRE_OK(compiler.jitCompile());
    auto materialDef{compiler.findMaterial("mat_ok")};
    REQUIRE(materialDef != nullptr);
    CHECK(materialDef->qualifiedName == "::host::mats::mat_ok");
    CHECK(materialDef->moduleName == "mats");
    CHECK(materialDef->moduleFileName.empty());
    CHECK(materialDef->moduleDisplayName == "<string ::host::mats>");
  }
  SUBCASE("The leading '::' is optional") {
    smdl::Compiler compiler{};
    auto source{"#smdl\nimport ::df::*;\n" + minimalMaterial("mat_ok")};
    REQUIRE_OK(compiler.addCode("host::mats", source));
    // The same name and the same source code again is a no-op, so a host
    // may register its defaults defensively.
    CHECK_OK(compiler.addCode("::host::mats", source));
    REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_NONE));
    REQUIRE_OK(compiler.jitCompile());
    CHECK(compiler.findMaterials("mat_ok").size() == 1);
  }
  SUBCASE("A different body under a taken name is an error") {
    smdl::Compiler compiler{};
    REQUIRE_OK(compiler.addCode("::host", "#smdl\nexport const int x = 1;\n"));
    const auto error{
        compiler.addCode("::host", "#smdl\nexport const int x = 2;\n")};
    REQUIRE(error.has_value());
    CHECK_CONTAINS(error->message, "already taken");
    CHECK_CONTAINS(error->message, "<string ::host>");
  }
  SUBCASE("A name taken by a file module is an error") {
    tmpDir.write("root/util.mdl", "#smdl\nexport const int x = 1;\n");
    smdl::Compiler compiler{};
    REQUIRE_OK(compiler.add((tmpDir / "root").string()));
    CHECK_ERROR(compiler.addCode("::util", "#smdl\nexport const int x = 2;\n"),
                "already taken");
  }
  SUBCASE("A file module added later is shadowed, not an error") {
    tmpDir.write("root/host.mdl",
                 "#smdl\nimport ::df::*;\n" + minimalMaterial("from_file"));
    smdl::Compiler compiler{};
    REQUIRE_OK(compiler.addCode("::host", "#smdl\nimport ::df::*;\n" +
                                              minimalMaterial("from_string")));
    REQUIRE_OK(compiler.add((tmpDir / "root").string()));
    REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_NONE));
    REQUIRE_OK(compiler.jitCompile());
    // The earliest added module wins the qualified name, exactly as it
    // does across search roots, so the file is the one shadowed here.
    CHECK(compiler.findMaterial("from_string") != nullptr);
    CHECK(compiler.findMaterial("from_file") == nullptr);
  }
  SUBCASE("Invalid module names are rejected") {
    smdl::Compiler compiler{};
    for (const char *moduleName :
         {"", "::", "a::", "::a::::b", "1bad", "a b", "a-b"}) {
      auto error{compiler.addCode(moduleName, "#smdl\n")};
      CAPTURE(moduleName);
      CHECK(error.has_value());
    }
  }
  SUBCASE("Modules import each other by qualified name") {
    tmpDir.write("root/util.mdl", "#smdl\nexport const int marker_file = 1;\n");
    tmpDir.write("root/main.mdl",
                 "#smdl\nimport ::df::*;\nimport ::host::consts::*;\n"
                 "export const int echo = host::consts::marker_host;\n" +
                     minimalMaterial("main_ok"));
    smdl::Compiler compiler{};
    REQUIRE_OK(compiler.addCode(
        "::host::consts",
        "#smdl\nimport ::util::*;\n"
        "export const int marker_host = util::marker_file + 1;\n"));
    REQUIRE_OK(compiler.add((tmpDir / "root").string()));
    REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_NONE));
    REQUIRE_OK(compiler.jitCompile());
    CHECK(compiler.findMaterial("main_ok") != nullptr);
  }
  SUBCASE("A compile error names the module it came from") {
    smdl::Compiler compiler{};
    REQUIRE(
        !compiler.addCode("::host::bad", "#smdl\nimport ::nonexistent::*;\n"));
    CHECK_ERROR(compiler.compile(smdl::OPT_LEVEL_NONE),
                "[<string ::host::bad>:2:");
  }
  SUBCASE("The unit tests in the module run") {
    smdl::Compiler compiler{};
    compiler.shouldEmitUnitTests = true;
    REQUIRE_OK(compiler.addCode("::host::tests",
                                "#smdl\nunit_test \"Arithmetic\" {\n"
                                "  int i = 2;\n"
                                "  #assert(i + i == 4);\n}\n"));
    REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_NONE));
    REQUIRE_OK(compiler.jitCompile());
    CHECK(runUnitTests(compiler) == "");
  }
  SUBCASE("The source code outlives the string it was given") {
    smdl::Compiler compiler{};
    {
      auto source{"#smdl\nimport ::df::*;\n" + minimalMaterial("mat_ok")};
      REQUIRE_OK(compiler.addCode("::host::mats", source));
    }
    // Twice, because 'compile()' resets and re-parses every module: a
    // module of source code has no file to read back.
    for (int i = 0; i < 2; i++) {
      REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_NONE));
      REQUIRE_OK(compiler.jitCompile());
      CHECK(compiler.findMaterial("mat_ok") != nullptr);
    }
  }
  SUBCASE("An anchor directory resolves relative paths") {
    const uint8_t texels[4] = {16, 32, 48, 64};
    fs::create_directories(tmpDir / "anchor");
    REQUIRE(!smdl::write8bitImage((tmpDir / "anchor" / "wood.png").string(), 2,
                                  2, 1, texels));
    tmpDir.write("anchor/helper.mdl", "#smdl\nexport const int marker = 3;\n");
    // Explicitly relative, so this resolves against the module's own
    // directory and nothing else.
    auto source{std::string("#smdl\nimport ::tex::*;\n"
                            "import .::helper::marker;\n"
                            "unit_test \"Anchored\" {\n"
                            "  #assert(helper::marker == 3);\n"
                            "  const auto t = texture_2d(\"wood.png\", "
                            "tex::gamma_linear);\n"
                            "  #assert(tex::texture_isvalid(t));\n}\n")};
    auto build{[&](const std::string &anchorDirectory) {
      smdl::Compiler compiler{};
      compiler.shouldEmitUnitTests = true;
      if (auto error{compiler.add((tmpDir / "anchor").string())})
        return error->message;
      if (auto error{compiler.addCode("::host::mats", source, anchorDirectory)})
        return error->message;
      if (auto error{compiler.compile(smdl::OPT_LEVEL_NONE)})
        return error->message;
      if (auto error{compiler.jitCompile()}) return error->message;
      return runUnitTests(compiler);
    }};
    CHECK(build((tmpDir / "anchor").string()) == "");
    // Without the anchor the relative import has nothing to resolve
    // against.
    CHECK(build("") != "");
    // An anchor that is not a directory is refused outright.
    smdl::Compiler compiler{};
    CHECK_ERROR(
        compiler.addCode("::host::mats", source, (tmpDir / "nowhere").string()),
        "not an existing directory");
  }
  SUBCASE("A name that matches a builtin module is warned about") {
    const CollectedLog warned{"same name as a builtin"};
    smdl::Compiler compiler{};
    CHECK_OK(compiler.addCode("::df", "#smdl\nexport const int x = 1;\n"));
    CHECK(warned.messages().size() == 1);
  }
}
