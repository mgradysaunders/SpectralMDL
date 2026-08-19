#include "doctest.h"

#include <algorithm>
#include <cmath>
#include <filesystem>
#include <fstream>
#include <iterator>
#include <string>
#include <vector>

#include "smdl/Compiler.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/MD5Hash.h"

namespace fs = std::filesystem;

static void writeFile(const fs::path &path, std::string_view text) {
  fs::create_directories(path.parent_path());
  std::ofstream(path) << text;
}

// Add everything, compile, and JIT-compile. Returns the first error
// message, or the empty string on success.
static std::string buildAll(smdl::Compiler &compiler,
                            const std::vector<fs::path> &paths,
                            std::vector<std::string> *names = nullptr) {
  for (const auto &path : paths)
    if (auto error{compiler.add(path.string(), names)}) return error->message;
  if (auto error{compiler.compile(smdl::OPT_LEVEL_NONE)}) return error->message;
  if (auto error{compiler.jitCompile()}) return error->message;
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

// CRC-32 (polynomial 0xEDB88320) as required by the ZIP format.
static uint32_t zipCrc32(std::string_view data) {
  uint32_t crc{0xFFFFFFFFu};
  for (auto ch : data) {
    crc ^= uint8_t(ch);
    for (int i = 0; i < 8; i++)
      crc = (crc >> 1) ^ (0xEDB88320u & (0u - (crc & 1u)));
  }
  return ~crc;
}

// Write a minimal ZIP with stored (uncompressed) entries, sufficient
// for the miniz-based 'Archive' reader to load as an '.mdr'.
static void
writeZip(const fs::path &path,
         const std::vector<std::pair<std::string, std::string>> &entries) {
  auto out{std::string()};
  auto putU16{[&](uint32_t value) {
    out += char(value & 0xFF);
    out += char((value >> 8) & 0xFF);
  }};
  auto putU32{[&](uint32_t value) {
    putU16(value & 0xFFFF);
    putU16(value >> 16);
  }};
  auto offsets{std::vector<uint32_t>()};
  for (const auto &[name, data] : entries) {
    offsets.push_back(uint32_t(out.size()));
    putU32(0x04034B50u); // Local file header
    putU16(20), putU16(0), putU16(0), putU16(0), putU16(0);
    putU32(zipCrc32(data));
    putU32(uint32_t(data.size())), putU32(uint32_t(data.size()));
    putU16(uint32_t(name.size())), putU16(0);
    out += name, out += data;
  }
  auto centralOffset{uint32_t(out.size())};
  for (size_t i = 0; i < entries.size(); i++) {
    const auto &[name, data]{entries[i]};
    putU32(0x02014B50u); // Central directory header
    putU16(20), putU16(20), putU16(0), putU16(0), putU16(0), putU16(0);
    putU32(zipCrc32(data));
    putU32(uint32_t(data.size())), putU32(uint32_t(data.size()));
    putU16(uint32_t(name.size())), putU16(0), putU16(0), putU16(0), putU16(0);
    putU32(0);
    putU32(offsets[i]);
    out += name;
  }
  auto centralSize{uint32_t(out.size()) - centralOffset};
  putU32(0x06054B50u); // End of central directory
  putU16(0), putU16(0);
  putU16(uint32_t(entries.size())), putU16(uint32_t(entries.size()));
  putU32(centralSize), putU32(centralOffset);
  putU16(0);
  fs::create_directories(path.parent_path());
  std::ofstream(path, std::ios::binary) << out;
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
      auto message{buildAll(
          compiler, {tmpDir / "rootB", tmpDir / "rootA", tmpDir / "rootC"})};
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
    writeFile(tmpDir / "root" / "main.mdl", "#smdl\n"
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
    CHECK(names ==
          std::vector<std::string>{"::top", "::vendor::metals::steel"});
  }
  SUBCASE("Single-file add uses the parent directory as implicit root") {
    writeFile(tmpDir / "dir" / "pkg" / "mod.mdl",
              "#smdl\nexport const int x = 1;\n");
    smdl::Compiler compiler{};
    auto names{std::vector<std::string>()};
    REQUIRE(
        !compiler.add((tmpDir / "dir" / "pkg" / "mod.mdl").string(), &names));
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

TEST_CASE("Compiler MDR archives") {
  auto tmpDir{fs::temp_directory_path() / "smdl-compiler-test"};
  fs::remove_all(tmpDir);
  SUBCASE("Archive names encode the package prefix") {
    writeZip(
        tmpDir / "root" / "vendor.metals.mdr",
        {{"vendor/metals.mdl", "#smdl\nexport const int metals_marker = 1;\n"},
         {"vendor/metals/steel.mdl", "#smdl\nimport ::df::*;\n"
                                     "import ..::metals::metals_marker;\n" +
                                         materialDef("brushed")}});
    // A loose module importing through the archive: absolutely and
    // weakly.
    writeFile(tmpDir / "root" / "main.mdl",
              "#smdl\nimport ::df::*;\n"
              "import ::vendor::metals::steel::*;\n"
              "import vendor::metals::metals_marker;\n" +
                  materialDef("main_ok"));
    smdl::Compiler compiler{};
    auto names{std::vector<std::string>()};
    REQUIRE(!compiler.add((tmpDir / "root").string(), &names));
    std::sort(names.begin(), names.end());
    CHECK(names == std::vector<std::string>{"::main", "::vendor::metals",
                                            "::vendor::metals::steel"});
    REQUIRE(buildAll(compiler, {tmpDir / "root"}) == "");
    CHECK(compiler.findMaterial("main_ok") != nullptr);
    auto material{compiler.findMaterial("brushed")};
    REQUIRE(material != nullptr);
    CHECK(material->qualifiedName == "::vendor::metals::steel::brushed");
    CHECK(material->moduleFileName.find("vendor.metals.mdr") !=
          std::string::npos);
  }
  SUBCASE("Non-conforming archives are rejected") {
    writeZip(tmpDir / "root" / "vendor.metals.mdr",
             {{"other/thing.mdl", "#smdl\nexport const int x = 1;\n"}});
    smdl::Compiler compiler{};
    auto error{compiler.add((tmpDir / "root").string())};
    REQUIRE(error.has_value());
    CHECK(error->message.find("conform") != std::string::npos);
  }
  SUBCASE("Empty package prefix components are rejected") {
    writeZip(tmpDir / "root" / "vendor..metals.mdr",
             {{"vendor/metals.mdl", "#smdl\n"}});
    smdl::Compiler compiler{};
    auto error{compiler.add((tmpDir / "root").string())};
    REQUIRE(error.has_value());
    CHECK(error->message.find("empty package prefix") != std::string::npos);
  }
  SUBCASE("Loose duplicates of archive contents are errors") {
    writeZip(tmpDir / "root" / "vendor.metals.mdr",
             {{"vendor/metals.mdl", "#smdl\nexport const int x = 1;\n"}});
    writeFile(tmpDir / "root" / "vendor" / "metals" / "extra.mdl",
              "#smdl\nexport const int y = 1;\n");
    {
      smdl::Compiler compiler{};
      auto error{compiler.add((tmpDir / "root").string())};
      REQUIRE(error.has_value());
      CHECK(error->message.find("conflicts with loose") != std::string::npos);
    }
    // Loose siblings outside the enclosed package are fine.
    fs::remove_all(tmpDir / "root" / "vendor" / "metals");
    writeFile(tmpDir / "root" / "vendor" / "other.mdl",
              "#smdl\nexport const int y = 1;\n");
    {
      smdl::Compiler compiler{};
      auto names{std::vector<std::string>()};
      REQUIRE(!compiler.add((tmpDir / "root").string(), &names));
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
      auto error{compiler.add((tmpDir / "root").string())};
      REQUIRE(error.has_value());
      CHECK(error->message.find("overlapping") != std::string::npos);
    }
    // Sibling prefixes are fine.
    fs::remove(tmpDir / "root" / "a.b.c.mdr");
    writeZip(tmpDir / "root" / "a.c.mdr", {{"a/c.mdl", "#smdl\n"}});
    {
      smdl::Compiler compiler{};
      auto names{std::vector<std::string>()};
      REQUIRE(!compiler.add((tmpDir / "root").string(), &names));
      std::sort(names.begin(), names.end());
      CHECK(names == std::vector<std::string>{"::a::b", "::a::c"});
    }
  }
  SUBCASE("Cross-root archive shadowing") {
    auto archiveEntry{"#smdl\nimport ::df::*;\n" + materialDef("shared_arch")};
    writeZip(tmpDir / "root1" / "vendor.metals.mdr",
             {{"vendor/metals.mdl", archiveEntry}});
    writeZip(tmpDir / "root2" / "vendor.metals.mdr",
             {{"vendor/metals.mdl", archiveEntry}});
    smdl::Compiler compiler{};
    REQUIRE(buildAll(compiler, {tmpDir / "root1", tmpDir / "root2"}) == "");
    auto material{compiler.findMaterial("shared_arch")};
    REQUIRE(material != nullptr);
    CHECK(material->moduleFileName.find("root1") != std::string::npos);
    CHECK(compiler.findMaterials("shared_arch").size() == 1);
  }
  SUBCASE("Archives below the top level are ignored") {
    writeZip(tmpDir / "root" / "sub" / "x.y.mdr", {{"x/y.mdl", "#smdl\n"}});
    writeFile(tmpDir / "root" / "mod.mdl", "#smdl\nexport const int x = 1;\n");
    smdl::Compiler compiler{};
    auto names{std::vector<std::string>()};
    REQUIRE(!compiler.add((tmpDir / "root").string(), &names));
    CHECK(names == std::vector<std::string>{"::mod"});
  }
  fs::remove_all(tmpDir);
}

TEST_CASE("Compiler MDLE") {
  auto tmpDir{fs::temp_directory_path() / "smdl-compiler-test"};
  fs::remove_all(tmpDir);
  const auto mainModule{"#smdl\nimport ::df::*;\nexport const int m = 1;\n" +
                        materialDef("main")};
  SUBCASE("Content-based identity and the 'main' convention") {
    writeZip(tmpDir / "CoolSteel.mdle", {{"main.mdl", mainModule}});
    auto expectedName{"::mdle::" + std::string(smdl::MD5Hash::hashFile(
                                       (tmpDir / "CoolSteel.mdle").string()))};
    smdl::Compiler compiler{};
    auto names{std::vector<std::string>()};
    REQUIRE(buildAll(compiler, {tmpDir / "CoolSteel.mdle"}, &names) == "");
    REQUIRE(names == std::vector<std::string>{expectedName});
    auto material{compiler.findMaterial(expectedName + "::main")};
    REQUIRE(material != nullptr);
    CHECK(material->qualifiedName == expectedName + "::main");
    CHECK(material->moduleName == "CoolSteel");
    CHECK(material->moduleFileName.find("CoolSteel.mdle") != std::string::npos);
    // Unique here, so the bare suffix also resolves.
    CHECK(compiler.findMaterial("main") == material);
  }
  SUBCASE("Identical containers dedupe, distinct containers cannot collide") {
    writeZip(tmpDir / "a" / "one.mdle", {{"main.mdl", mainModule}});
    writeZip(tmpDir / "b" / "two.mdle", {{"main.mdl", mainModule}});
    auto otherModule{"#smdl\nimport ::df::*;\nexport const int m = 2;\n" +
                     materialDef("main")};
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
    auto error{compiler.add((tmpDir / "bad.mdle").string())};
    REQUIRE(error.has_value());
    CHECK(error->message.find("main.mdl") != std::string::npos);
  }
  SUBCASE("Directory walks do not ingest MDLEs") {
    writeZip(tmpDir / "root" / "loose.mdle", {{"main.mdl", mainModule}});
    writeFile(tmpDir / "root" / "mod.mdl", "#smdl\nexport const int x = 1;\n");
    smdl::Compiler compiler{};
    auto names{std::vector<std::string>()};
    REQUIRE(!compiler.add((tmpDir / "root").string(), &names));
    CHECK(names == std::vector<std::string>{"::mod"});
  }
  SUBCASE("Container resources extract and anchor resource lookups") {
    // Generate a tiny PNG with the library's own writer and pack it
    // beside a 'main.mdl' that references it.
    const uint8_t texels[12] = {255, 0, 0, 0, 255, 0, 0, 0, 255, 255, 255, 255};
    fs::create_directories(tmpDir);
    auto pngPath{(tmpDir / "wood.png").string()};
    REQUIRE(!smdl::write8bitImage(pngPath, 2, 2, 3, texels));
    auto pngBytes{std::string()};
    {
      auto stream{std::ifstream(pngPath, std::ios::binary)};
      pngBytes.assign(std::istreambuf_iterator<char>(stream),
                      std::istreambuf_iterator<char>());
    }
    REQUIRE(!pngBytes.empty());
    writeZip(tmpDir / "Textured.mdle",
             {{"main.mdl", "#smdl\nimport ::df::*;\nimport ::tex::*;\n" +
                               materialDef("main") +
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
    compiler.enableUnitTests = true;
    REQUIRE(buildAll(compiler, {tmpDir / "Textured.mdle"}) == "");
    CHECK(compiler.findMaterial("main") != nullptr);
    // The resource was extracted to the content-addressed cache.
    CHECK(fs::is_regular_file(fs::temp_directory_path() /
                              ("smdl-mdle-" + hash) / "wood.png"));
    // Run the in-container unit test: it asserts the texture actually
    // loaded (a resource that failed to resolve would only have
    // produced a warning and a default texture).
    auto allocator{smdl::BumpPtrAllocator()};
    auto wavelengths{std::vector<float>(size_t(compiler.wavelengthBaseMax))};
    auto state{smdl::State()};
    state.allocator = &allocator;
    state.wavelength_min = 380.0f;
    state.wavelength_max = 720.0f;
    state.wavelength_base = wavelengths.data();
    for (uint32_t i = 0; i < compiler.wavelengthBaseMax; i++) {
      float fac{float(i) / float(compiler.wavelengthBaseMax - 1)};
      wavelengths[i] =
          (1 - fac) * state.wavelength_min + fac * state.wavelength_max;
    }
    REQUIRE(!compiler.runUnitTests(state));
  }
  fs::remove_all(tmpDir);
}

TEST_CASE("Compiler findMaterial") {
  auto tmpDir{fs::temp_directory_path() / "smdl-compiler-test"};
  fs::remove_all(tmpDir);
  SUBCASE("Qualified and suffix lookup") {
    writeFile(tmpDir / "root" / "alpha.mdl", "#smdl\nimport ::df::*;\n" +
                                                 materialDef("unique_mat") +
                                                 materialDef("dup"));
    writeFile(tmpDir / "root" / "beta.mdl", "#smdl\nimport ::df::*;\n" +
                                                materialDef("dup") +
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
    writeFile(tmpDir / "root" / "nsmod.mdl", "#smdl\nimport ::df::*;\n"
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
    writeFile(tmpDir / "root1" / "mat.mdl", "#smdl\nimport ::df::*;\n" +
                                                materialDef("shared_name") +
                                                materialDef("only_r1"));
    writeFile(tmpDir / "root2" / "mat.mdl", "#smdl\nimport ::df::*;\n" +
                                                materialDef("shared_name") +
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

TEST_CASE("Compiler desired materials") {
  auto tmpDir{fs::temp_directory_path() / "smdl-desired-materials-test"};
  fs::remove_all(tmpDir);
  writeFile(tmpDir / "root" / "mats.mdl", "#smdl\nimport ::df::*;\n" +
                                              materialDef("wanted") +
                                              materialDef("unwanted"));
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
    REQUIRE(!compiler.add((tmpDir / "root").string()));
    REQUIRE(!compiler.compile(smdl::OPT_LEVEL_NONE));
    auto ir{std::string()};
    REQUIRE(!compiler.dump(smdl::DUMP_FORMAT_IR, ir));
    CHECK(ir.find("mats.wanted.evaluate") != std::string::npos);
    CHECK(ir.find("mats.unwanted") == std::string::npos);
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
    writeFile(tmpDir / "root2" / "variants.mdl",
              "#smdl\nimport ::df::*;\n" + materialDef("base") +
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
    REQUIRE(!compiler.compile(smdl::OPT_LEVEL_NONE));
    REQUIRE(!compiler.jitCompile());
    CHECK(compiler.getMaterials().size() == 2);
    CHECK(compiler.getSkippedMaterialNames().empty());
  }
  fs::remove_all(tmpDir);
}

TEST_CASE("Compiler static material flags") {
  using namespace smdl::JIT;
  auto tmpDir{fs::temp_directory_path() / "smdl-compiler-test"};
  fs::remove_all(tmpDir);
  writeFile(
      tmpDir / "root" / "mats.mdl",
      "#smdl\n"
      "import ::df::*;\n"
      "import ::state::*;\n"
      "import ::scene::*;\n"
      "export material mat_default() = material();\n"
      "export material mat_plastic() = material(\n"
      "  surface: material_surface(\n"
      "    scattering: df::diffuse_reflection_bsdf(tint: 0.8)));\n"
      "export material mat_cutout_const() = material(\n"
      "  geometry: material_geometry(cutout_opacity: 0.5));\n"
      "export material mat_cutout_folds() = material(\n"
      "  geometry: material_geometry(cutout_opacity: 0.25 + 0.75));\n"
      "export material mat_cutout_runtime() = material(\n"
      "  geometry: material_geometry(\n"
      "    cutout_opacity: scene::data_lookup_float(\"opacity\", 1.0)));\n"
      "export material mat_thin() = material(thin_walled: true);\n"
      "export material mat_thin_runtime() = material(\n"
      "  thin_walled: state::position().x > 0.0);\n"
      "export material mat_volume() = material(\n"
      "  volume: material_volume(absorption_coefficient: color(0.5)));\n"
      "export material mat_volume_additive() = material(\n"
      "  ior: 1.0,\n"
      "  volume: material_volume(\n"
      "    scattering_coefficient: color(0.5),\n"
      "    additive: true));\n"
      "export material mat_emissive() = material(\n"
      "  surface: material_surface(\n"
      "    scattering: df::diffuse_reflection_bsdf(),\n"
      "    emission: material_emission(emission: df::diffuse_edf())));\n");
  smdl::Compiler compiler{};
  REQUIRE(!compiler.add((tmpDir / "root").string()));
  REQUIRE(!compiler.compile(smdl::OPT_LEVEL_O2));
  REQUIRE(!compiler.jitCompile());
  auto get{[&](std::string_view name) {
    auto material{compiler.findMaterial(name)};
    REQUIRE(material != nullptr);
    return material;
  }};
  // The six '#isDefault'-derived structural bits are always known, and
  // at -O2 the constant-foldable value bits are too, including the
  // heterogeneous-volume bit (every material here has a constant, or
  // no, volume, so the '.volumeEvaluate' body folds away from the
  // state and homogeneity is proven) and the displacement bit (every
  // material here has a constant, in fact default, displacement, so
  // the '.displacementProbe' body folds to the zero vector).
  constexpr int structuralBits{MATERIAL_HAS_SURFACE | MATERIAL_HAS_BACKFACE |
                               MATERIAL_HAS_SURFACE_EMISSION |
                               MATERIAL_HAS_BACKFACE_EMISSION |
                               MATERIAL_HAS_VOLUME | MATERIAL_HAS_HAIR};
  constexpr int allBits{
      structuralBits | MATERIAL_THIN_WALLED | MATERIAL_HAS_CUTOUT |
      MATERIAL_HAS_HETEROGENEOUS_VOLUME | MATERIAL_HAS_DISPLACEMENT};
  SUBCASE("Structural and constant-foldable bits are known") {
    auto matDefault{get("mat_default")};
    CHECK(matDefault->staticFlagsKnown == allBits);
    CHECK(matDefault->staticFlags == 0);
    CHECK(matDefault->isAlwaysOpaque());
    CHECK(matDefault->isShadowTrivial());
    auto matPlastic{get("mat_plastic")};
    CHECK(matPlastic->staticFlagsKnown == allBits);
    CHECK(matPlastic->staticFlags == MATERIAL_HAS_SURFACE);
    CHECK(matPlastic->isShadowTrivial());
    auto matCutoutConst{get("mat_cutout_const")};
    CHECK((matCutoutConst->staticFlagsKnown & MATERIAL_HAS_CUTOUT) != 0);
    CHECK((matCutoutConst->staticFlags & MATERIAL_HAS_CUTOUT) != 0);
    CHECK(!matCutoutConst->isAlwaysOpaque());
    CHECK(!matCutoutConst->isShadowTrivial());
    auto matCutoutFolds{get("mat_cutout_folds")};
    CHECK(matCutoutFolds->isAlwaysOpaque());
    CHECK(matCutoutFolds->isShadowTrivial());
    auto matThin{get("mat_thin")};
    CHECK((matThin->staticFlagsKnown & MATERIAL_THIN_WALLED) != 0);
    CHECK((matThin->staticFlags & MATERIAL_THIN_WALLED) != 0);
    auto matVolume{get("mat_volume")};
    CHECK(matVolume->hasVolume());
    CHECK(matVolume->hasHomogeneousVolume());
    CHECK(matVolume->isAlwaysOpaque());
    CHECK(!matVolume->isShadowTrivial());
    auto matEmissive{get("mat_emissive")};
    CHECK((matEmissive->staticFlags & MATERIAL_HAS_SURFACE_EMISSION) != 0);
    CHECK(matEmissive->isShadowTrivial());
  }
  SUBCASE("Runtime-dependent bits degrade to unknown") {
    auto matCutoutRuntime{get("mat_cutout_runtime")};
    CHECK(matCutoutRuntime->staticFlagsKnown ==
          (allBits & ~MATERIAL_HAS_CUTOUT));
    CHECK(!matCutoutRuntime->isAlwaysOpaque());
    CHECK(!matCutoutRuntime->isShadowTrivial());
    auto matThinRuntime{get("mat_thin_runtime")};
    CHECK(matThinRuntime->staticFlagsKnown ==
          (allBits & ~MATERIAL_THIN_WALLED));
  }
  SUBCASE("Instances satisfy the static-flags invariant") {
    auto allocator{smdl::BumpPtrAllocator()};
    auto wavelengths{std::vector<float>(size_t(compiler.wavelengthBaseMax))};
    auto state{smdl::State()};
    state.allocator = &allocator;
    state.wavelength_min = 380.0f;
    state.wavelength_max = 720.0f;
    state.wavelength_base = wavelengths.data();
    for (uint32_t i = 0; i < compiler.wavelengthBaseMax; i++) {
      float fac{float(i) / float(compiler.wavelengthBaseMax - 1)};
      wavelengths[i] =
          (1 - fac) * state.wavelength_min + fac * state.wavelength_max;
    }
    for (const auto &material : compiler.getMaterials()) {
      auto materialInstance{smdl::JIT::MaterialInstance(state, &material)};
      CHECK((materialInstance.instance.flags & material.staticFlagsKnown) ==
            material.staticFlags);
    }
    // 'evaluateOpacity' agrees with the full evaluation and requires no
    // allocator.
    auto stateNoAlloc{state};
    stateNoAlloc.allocator = nullptr;
    for (const auto &material : compiler.getMaterials()) {
      auto materialInstance{smdl::JIT::MaterialInstance(state, &material)};
      CHECK(material.evaluateOpacity(stateNoAlloc) ==
            materialInstance.getCutoutOpacity());
    }
    CHECK(get("mat_default")->evaluateOpacity(stateNoAlloc) == 1.0f);
    CHECK(get("mat_cutout_const")->evaluateOpacity(stateNoAlloc) == 0.5f);
    // The additive-volume declaration reaches the instance flags.
    auto instAdditive{
        smdl::JIT::MaterialInstance(state, get("mat_volume_additive"))};
    CHECK(instAdditive.hasAdditiveVolume());
    auto instReplacing{smdl::JIT::MaterialInstance(state, get("mat_volume"))};
    CHECK(!instReplacing.hasAdditiveVolume());
  }
  fs::remove_all(tmpDir);
}

TEST_CASE("Compiler volume evaluate") {
  using namespace smdl::JIT;
  auto tmpDir{fs::temp_directory_path() / "smdl-volume-evaluate-test"};
  fs::remove_all(tmpDir);
  // A 32x8x4 Mitsuba volume holding the linear field
  // 'value = x + 10*y + 100*z', so trilinear filtering reproduces it
  // exactly and the maximum is 31 + 70 + 300 = 401.
  {
    fs::create_directories(tmpDir / "root");
    std::ofstream file((tmpDir / "root" / "linear.vol").string(),
                       std::ios::binary);
    file.write("VOL", 3);
    const char version{3};
    file.write(&version, 1);
    const int32_t header[5] = {1, 32, 8, 4, 1};
    file.write(reinterpret_cast<const char *>(header), sizeof(header));
    const float bound[6] = {0.0f, 0.0f, 0.0f, 1.0f, 1.0f, 1.0f};
    file.write(reinterpret_cast<const char *>(bound), sizeof(bound));
    for (int z = 0; z < 4; z++)
      for (int y = 0; y < 8; y++)
        for (int x = 0; x < 32; x++) {
          const float value{float(x) + 10.0f * float(y) + 100.0f * float(z)};
          file.write(reinterpret_cast<const char *>(&value), sizeof(value));
        }
  }
  writeFile(tmpDir / "root" / "vols.mdl",
            "#smdl\n"
            "import ::df::*;\n"
            "import ::state::*;\n"
            "import ::tex::*;\n"
            "export material vol_homog() = material(\n"
            "  ior: 1.0,\n"
            "  volume: material_volume(\n"
            "    scattering: df::anisotropic_vdf(),\n"
            "    absorption_coefficient: color(0.5),\n"
            "    scattering_coefficient: color(2.0),\n"
            "    max_scattering_coefficient: color(2.0)));\n"
            "export material vol_hetero() = material(\n"
            "  ior: 1.0,\n"
            "  volume: material_volume(\n"
            "    scattering: df::anisotropic_vdf(),\n"
            "    scattering_coefficient: 4.0 *\n"
            "      tex::lookup_float(texture_3d(\"linear.vol\"),\n"
            "                        state::position()) * color(1.0),\n"
            "    max_scattering_coefficient: 4.0 *\n"
            "      tex::max_value(texture_3d(\"linear.vol\")) * color(1.0)));\n"
            "export material vol_none() = material();\n"
            "export material vol_fire() = material(\n"
            "  ior: 1.0,\n"
            "  volume: material_volume(\n"
            "    scattering: df::anisotropic_vdf(),\n"
            "    absorption_coefficient: 2.0 *\n"
            "      tex::lookup_float(texture_3d(\"linear.vol\"),\n"
            "                        state::position()) * color(1.0),\n"
            "    emission_intensity: 0.25 *\n"
            "      tex::lookup_float(texture_3d(\"linear.vol\"),\n"
            "                        state::position()) * color(1.0),\n"
            "    max_absorption_coefficient: 2.0 *\n"
            "      tex::max_value(texture_3d(\"linear.vol\")) * color(1.0)));\n"
            "export material vol_hinted() = material(\n"
            "  ior: 1.0,\n"
            "  volume: material_volume(\n"
            "    scattering: df::anisotropic_vdf(),\n"
            "    scattering_coefficient: 4.0 *\n"
            "      tex::lookup_float(texture_3d(\"linear.vol\"),\n"
            "                        state::position()) * color(1.0),\n"
            "    max_scattering_coefficient: 4.0 *\n"
            "      tex::max_value(texture_3d(\"linear.vol\")) * color(1.0),\n"
            "    density: texture_3d(\"linear.vol\"),\n"
            "    density_bound_min: float3(0.0),\n"
            "    density_bound_max: float3(1.0)));\n");
  smdl::Compiler compiler{};
  REQUIRE(!compiler.add((tmpDir / "root").string()));
  REQUIRE(!compiler.compile(smdl::OPT_LEVEL_O2));
  REQUIRE(!compiler.jitCompile());
  auto get{[&](std::string_view name) {
    auto material{compiler.findMaterial(name)};
    REQUIRE(material != nullptr);
    return material;
  }};
  const auto N{size_t(compiler.wavelengthBaseMax)};
  auto sigmaA{std::vector<float>(N)};
  auto sigmaS{std::vector<float>(N)};
  auto emission{std::vector<float>(N)};
  // 'volumeEvaluate' is allocation-free, so the partial state carries
  // no allocator: only the object-space position identifies the query.
  auto state{smdl::State()};
  SUBCASE("Homogeneous coefficients are position-independent") {
    auto material{get("vol_homog")};
    CHECK(material->hasVolume());
    CHECK(material->hasHomogeneousVolume());
    material->volumeEvaluate(state, sigmaA.data(), sigmaS.data(),
                             emission.data());
    for (size_t i = 0; i < N; i++) {
      CHECK(sigmaA[i] == 0.5f);
      CHECK(sigmaS[i] == 2.0f);
    }
  }
  SUBCASE("Heterogeneous coefficients follow the position") {
    auto material{get("vol_hetero")};
    CHECK(material->hasVolume());
    CHECK(!material->hasHomogeneousVolume());
    // The center of voxel (3, 4, 2) has the exactly representable
    // texture coordinate below, where the field is 243.
    state.position = smdl::float3(3.5f / 32.0f, 4.5f / 8.0f, 2.5f / 4.0f);
    material->volumeEvaluate(state, sigmaA.data(), sigmaS.data(),
                             emission.data());
    for (size_t i = 0; i < N; i++) {
      CHECK(sigmaA[i] == 0.0f);
      CHECK(sigmaS[i] == 4.0f * 243.0f);
    }
    // The center of voxel (0, 0, 0), where the field is 0.
    state.position = smdl::float3(0.5f / 32.0f, 0.5f / 8.0f, 0.5f / 4.0f);
    material->volumeEvaluate(state, sigmaA.data(), sigmaS.data(),
                             emission.data());
    for (size_t i = 0; i < N; i++) CHECK(sigmaS[i] == 0.0f);
  }
  SUBCASE("Emission follows the position and absent emission is zero") {
    auto material{get("vol_fire")};
    CHECK(material->hasVolume());
    // The center of voxel (3, 4, 2), where the linear field is 243:
    // emission is 0.25 times the field, absorption 2 times it.
    state.position = smdl::float3(3.5f / 32.0f, 4.5f / 8.0f, 2.5f / 4.0f);
    material->volumeEvaluate(state, sigmaA.data(), sigmaS.data(),
                             emission.data());
    for (size_t i = 0; i < N; i++) {
      CHECK(sigmaA[i] == 2.0f * 243.0f);
      CHECK(sigmaS[i] == 0.0f);
      CHECK(emission[i] == 0.25f * 243.0f);
    }
    // A material that declares no emission resolves it to zero.
    auto hetero{get("vol_hetero")};
    state.position = smdl::float3(3.5f / 32.0f, 4.5f / 8.0f, 2.5f / 4.0f);
    hetero->volumeEvaluate(state, sigmaA.data(), sigmaS.data(),
                           emission.data());
    for (size_t i = 0; i < N; i++) CHECK(emission[i] == 0.0f);
  }
  SUBCASE("No volume evaluates to zero and proves homogeneous") {
    auto material{get("vol_none")};
    CHECK(!material->hasVolume());
    CHECK(material->hasHomogeneousVolume());
    material->volumeEvaluate(state, sigmaA.data(), sigmaS.data(),
                             emission.data());
    for (size_t i = 0; i < N; i++) {
      CHECK(sigmaA[i] == 0.0f);
      CHECK(sigmaS[i] == 0.0f);
    }
  }
  SUBCASE("Instances expose the density acceleration hint") {
    auto allocator{smdl::BumpPtrAllocator()};
    auto wavelengths{std::vector<float>(N)};
    auto fullState{smdl::State()};
    fullState.allocator = &allocator;
    fullState.wavelength_min = 380.0f;
    fullState.wavelength_max = 720.0f;
    fullState.wavelength_base = wavelengths.data();
    for (size_t i = 0; i < N; i++) {
      float fac{float(i) / float(N - 1)};
      wavelengths[i] =
          (1 - fac) * fullState.wavelength_min + fac * fullState.wavelength_max;
    }
    // A material with the complete hint exposes the grid resource and
    // both corners of the bound box through the instance.
    auto hinted{smdl::JIT::MaterialInstance(fullState, get("vol_hinted"))};
    const auto *grid{hinted.getVolumeDensityGrid()};
    REQUIRE(grid != nullptr);
    CHECK(grid->isValid());
    CHECK(grid->getExtent().x == 32);
    CHECK(grid->getExtent().y == 8);
    CHECK(grid->getExtent().z == 4);
    CHECK(grid->getMaxValue() == 401.0f);
    REQUIRE(hinted.getVolumeDensityBoundMin() != nullptr);
    REQUIRE(hinted.getVolumeDensityBoundMax() != nullptr);
    CHECK(hinted.getVolumeDensityBoundMin()->x == 0.0f);
    CHECK(hinted.getVolumeDensityBoundMax()->x == 1.0f);
    CHECK(hinted.getVolumeDensityBoundMax()->z == 1.0f);
    // The per-brick bounds behind the hint: the grid is 32x8x4, so two
    // bricks in x, and the linear field peaks in the upper brick.
    CHECK(grid->getBrickCount().x == 2);
    CHECK(grid->getBrickMaxValue(1, 0, 0) == 401.0f);
    // A material without the hint reports null pointers.
    auto unhinted{smdl::JIT::MaterialInstance(fullState, get("vol_hetero"))};
    CHECK(unhinted.getVolumeDensityGrid() == nullptr);
    CHECK(unhinted.getVolumeDensityBoundMin() == nullptr);
    CHECK(unhinted.getVolumeDensityBoundMax() == nullptr);
  }
  SUBCASE("Instances expose the declared majorants") {
    auto allocator{smdl::BumpPtrAllocator()};
    auto wavelengths{std::vector<float>(N)};
    auto fullState{smdl::State()};
    fullState.allocator = &allocator;
    fullState.wavelength_min = 380.0f;
    fullState.wavelength_max = 720.0f;
    fullState.wavelength_base = wavelengths.data();
    for (size_t i = 0; i < N; i++) {
      float fac{float(i) / float(N - 1)};
      wavelengths[i] =
          (1 - fac) * fullState.wavelength_min + fac * fullState.wavelength_max;
    }
    auto homog{smdl::JIT::MaterialInstance(fullState, get("vol_homog"))};
    REQUIRE(homog.getMaxScatteringCoefficient().size() == N);
    CHECK(homog.getMaxAbsorptionCoefficient().empty());
    for (size_t i = 0; i < N; i++)
      CHECK(homog.getMaxScatteringCoefficient()[i] == 2.0f);
    // The heterogeneous majorant is exact through 'tex::max_value'.
    auto hetero{smdl::JIT::MaterialInstance(fullState, get("vol_hetero"))};
    REQUIRE(hetero.getMaxScatteringCoefficient().size() == N);
    for (size_t i = 0; i < N; i++)
      CHECK(hetero.getMaxScatteringCoefficient()[i] == 4.0f * 401.0f);
  }
  fs::remove_all(tmpDir);
}

TEST_CASE("Compiler displacement evaluate") {
  using namespace smdl::JIT;
  auto tmpDir{fs::temp_directory_path() / "smdl-displacement-test"};
  fs::remove_all(tmpDir);
  writeFile(tmpDir / "root" / "disp.mdl",
            "#smdl\n"
            "import ::state::*;\n"
            "export material disp_none() = material();\n"
            "export material disp_const() = material(\n"
            "  geometry: material_geometry(\n"
            "    displacement: float3(0.0, 0.0, 0.25)));\n"
            "export material disp_state() = material(\n"
            "  geometry: material_geometry(\n"
            "    displacement: state::texture_coordinate(0).x *\n"
            "      float3(0.0, 0.0, 1.0)));\n");
  smdl::Compiler compiler{};
  REQUIRE(!compiler.add((tmpDir / "root").string()));
  REQUIRE(!compiler.compile(smdl::OPT_LEVEL_O2));
  REQUIRE(!compiler.jitCompile());
  auto get{[&](std::string_view name) {
    auto material{compiler.findMaterial(name)};
    REQUIRE(material != nullptr);
    return material;
  }};
  // 'displacementEvaluate' is allocation-free, so the partial state
  // carries no allocator, exactly as with 'volumeEvaluate'.
  auto state{smdl::State()};
  auto displacement{smdl::float3()};
  SUBCASE("The default material is provably undisplaced") {
    auto material{get("disp_none")};
    CHECK(material->hasZeroDisplacement());
    CHECK((material->staticFlagsKnown & MATERIAL_HAS_DISPLACEMENT) != 0);
    CHECK((material->staticFlags & MATERIAL_HAS_DISPLACEMENT) == 0);
    material->displacementEvaluate(state, displacement);
    CHECK(displacement.x == 0.0f);
    CHECK(displacement.y == 0.0f);
    CHECK(displacement.z == 0.0f);
  }
  SUBCASE("A constant displacement is provably non-zero") {
    auto material{get("disp_const")};
    CHECK(!material->hasZeroDisplacement());
    CHECK((material->staticFlagsKnown & MATERIAL_HAS_DISPLACEMENT) != 0);
    CHECK((material->staticFlags & MATERIAL_HAS_DISPLACEMENT) != 0);
    material->displacementEvaluate(state, displacement);
    CHECK(displacement.x == 0.0f);
    CHECK(displacement.y == 0.0f);
    CHECK(displacement.z == 0.25f);
  }
  SUBCASE("A state-dependent displacement is unknown, not proven zero") {
    auto material{get("disp_state")};
    CHECK(!material->hasZeroDisplacement());
    CHECK((material->staticFlagsKnown & MATERIAL_HAS_DISPLACEMENT) == 0);
    state.texture_coordinate[0] = smdl::float3(2.5f, 0.0f, 0.0f);
    material->displacementEvaluate(state, displacement);
    CHECK(displacement.x == 0.0f);
    CHECK(displacement.y == 0.0f);
    CHECK(displacement.z == 2.5f);
  }
  fs::remove_all(tmpDir);
}

TEST_CASE("Compiler hair scattering") {
  using namespace smdl::JIT;
  auto tmpDir{fs::temp_directory_path() / "smdl-hair-test"};
  fs::remove_all(tmpDir);
  writeFile(tmpDir / "root" / "hair.mdl",
            "#smdl\n"
            "import ::df::*;\n"
            "export material hair_brown() = material(\n"
            "  hair: df::chiang_hair_bsdf(\n"
            "    roughness_R: float2(0.3, 0.4),\n"
            "    absorption_coefficient: color(0.4)));\n"
            "export material hair_none() = material();\n");
  smdl::Compiler compiler{};
  REQUIRE(!compiler.add((tmpDir / "root").string()));
  REQUIRE(!compiler.compile(smdl::OPT_LEVEL_O2));
  REQUIRE(!compiler.jitCompile());
  auto get{[&](std::string_view name) {
    auto material{compiler.findMaterial(name)};
    REQUIRE(material != nullptr);
    return material;
  }};
  auto allocator{smdl::BumpPtrAllocator()};
  auto wavelengths{std::vector<float>(size_t(compiler.wavelengthBaseMax))};
  auto state{smdl::State()};
  state.allocator = &allocator;
  state.wavelength_min = 380.0f;
  state.wavelength_max = 720.0f;
  state.wavelength_base = wavelengths.data();
  for (uint32_t i = 0; i < compiler.wavelengthBaseMax; i++) {
    float fac{float(i) / float(compiler.wavelengthBaseMax - 1)};
    wavelengths[i] =
        (1 - fac) * state.wavelength_min + fac * state.wavelength_max;
  }
  // The default state's tangent-to-world is identity, so the directions
  // below are written directly in the hair frame: X is the fiber tangent
  // and Z is the cross-section normal.
  auto wo{smdl::float3(0.0f, 0.6f, 0.8f)};
  auto wi{smdl::float3(0.6f, -0.64f, -0.48f)};
  auto f{std::vector<float>(size_t(compiler.wavelengthBaseMax))};
  auto fSpan{smdl::Span<float>(f.data(), f.size())};
  float pdfFwd{};
  float pdfRev{};
  SUBCASE("A hair material evaluates and samples through the entry points") {
    auto material{get("hair_brown")};
    CHECK(material->hasHair());
    CHECK((material->staticFlagsKnown & MATERIAL_HAS_HAIR) != 0);
    CHECK((material->staticFlags & MATERIAL_HAS_HAIR) != 0);
    auto materialInstance{smdl::JIT::MaterialInstance(state, material)};
    CHECK(materialInstance.hasHair());
    CHECK(materialInstance.hairScatterEvaluate(wo, wi, pdfFwd, pdfRev, fSpan));
    CHECK(pdfFwd > 0.0f);
    CHECK(pdfRev > 0.0f);
    for (float fValue : f) {
      CHECK(fValue > 0.0f);
      CHECK(std::isfinite(fValue));
    }
    auto xi{smdl::float4(0.3f, 0.4f, 0.5f, 0.6f)};
    auto wiSampled{smdl::float3()};
    CHECK(materialInstance.hairScatterSample(xi, wo, wiSampled, pdfFwd, pdfRev,
                                             fSpan));
    CHECK(pdfFwd > 0.0f);
    float lengthSquared{wiSampled.x * wiSampled.x + wiSampled.y * wiSampled.y +
                        wiSampled.z * wiSampled.z};
    CHECK(lengthSquared == doctest::Approx(1.0f).epsilon(1e-3));
  }
  SUBCASE("The default hair BSDF is safe to call and reports black") {
    auto material{get("hair_none")};
    CHECK(!material->hasHair());
    CHECK((material->staticFlagsKnown & MATERIAL_HAS_HAIR) != 0);
    CHECK((material->staticFlags & MATERIAL_HAS_HAIR) == 0);
    auto materialInstance{smdl::JIT::MaterialInstance(state, material)};
    CHECK(!materialInstance.hasHair());
    CHECK(!materialInstance.hairScatterEvaluate(wo, wi, pdfFwd, pdfRev, fSpan));
    CHECK(pdfFwd == 0.0f);
    CHECK(pdfRev == 0.0f);
    for (float fValue : f) {
      CHECK(fValue == 0.0f);
    }
  }
  fs::remove_all(tmpDir);
}

TEST_CASE("Compiler inferred-size array deduction") {
  auto tmpDir{fs::temp_directory_path() / "smdl-inferred-size-test"};
  fs::remove_all(tmpDir);
  // A macro whose two parameters share the size name 'N'.
  static const char *sharedN{
      "#smdl\n"
      "@(pure macro)\n"
      "int sharedN(const float[<N>] a, const float[<N>] b) = N;\n"};
  SUBCASE("Consistent sizes across a shared size name compile") {
    writeFile(tmpDir / "root" / "main.mdl",
              std::string(sharedN) +
                  "export const int ok = sharedN(float[2](1.0, 2.0), "
                  "float[2](3.0, 4.0));\n");
    smdl::Compiler compiler{};
    CHECK(buildAll(compiler, {tmpDir / "root"}) == "");
  }
  SUBCASE("Mismatched sizes are rejected at overload resolution") {
    writeFile(tmpDir / "root" / "main.mdl",
              std::string(sharedN) +
                  "export const int bad = sharedN(float[2](1.0, 2.0), "
                  "float[3](3.0, 4.0, 5.0));\n");
    smdl::Compiler compiler{};
    auto message{buildAll(compiler, {tmpDir / "root"})};
    CHECK(message.find("deduces array size") != std::string::npos);
  }
  SUBCASE("A local size name must not silently rebind") {
    writeFile(tmpDir / "root" / "main.mdl",
              "#smdl\n"
              "@(pure macro)\n"
              "int localRebind() {\n"
              "  const float[<N>] a(1.0, 2.0);\n"
              "  const float[<N>] b(1.0, 2.0, 3.0);\n"
              "  return N + int(a[0] + b[0]);\n"
              "}\n"
              "export const int bad = localRebind();\n");
    smdl::Compiler compiler{};
    auto message{buildAll(compiler, {tmpDir / "root"})};
    CHECK(message.find("conflicts with") != std::string::npos);
  }
  SUBCASE("A size name must not silently shadow a same-scope parameter") {
    writeFile(tmpDir / "root" / "main.mdl",
              "#smdl\n"
              "@(pure macro)\n"
              "int collide(const int N, const float[<N>] w) = N + int(w[0]);\n"
              "export const int bad = collide(7, float[3](0.0, 0.0, 0.0));\n");
    smdl::Compiler compiler{};
    auto message{buildAll(compiler, {tmpDir / "root"})};
    CHECK(message.find("conflicts with") != std::string::npos);
  }
  fs::remove_all(tmpDir);
}

TEST_CASE("Compiler lambda expressions") {
  auto tmpDir{fs::temp_directory_path() / "smdl-lambda-test"};
  fs::remove_all(tmpDir);
  // Compile a single module and return the first error message, or the
  // empty string on success. The positive behavior of lambdas is covered
  // by 'testing/smdl/lambda.smdl'; these subcases pin the error paths.
  auto build{[&](std::string_view text) {
    writeFile(tmpDir / "root" / "main.mdl", std::string("#smdl\n") += text);
    smdl::Compiler compiler{};
    // Some subcases place the erroring code inside a 'unit_test' body,
    // which is only compiled when unit tests are enabled.
    compiler.enableUnitTests = true;
    return buildAll(compiler, {tmpDir / "root"});
  }};
  SUBCASE("Function values must not pass through non-macro parameters") {
    auto message{build("@(pure)\n"
                       "float apply(const auto f, const float x) = f(x);\n"
                       "export const float bad = "
                       "apply(\\(const float x) = x, 1.0);\n")};
    CHECK(message.find("compile-time only") != std::string::npos);
  }
  SUBCASE("A variable holding a function must be 'const'") {
    auto message{build("unit_test \"t\" {\n"
                       "  auto f = \\(const float x) = x;\n"
                       "  #assert(f(1.0) == 1.0);\n"
                       "}\n")};
    CHECK(message.find("must be declared 'const'") != std::string::npos);
  }
  SUBCASE("A variable holding a function must not be 'static'") {
    auto message{build("static const auto f = \\(const float x) = x;\n"
                       "unit_test \"t\" { #assert(f(1.0) == 1.0); }\n")};
    CHECK(message.find("without 'static'") != std::string::npos);
  }
  SUBCASE("Lambda must not be a function variant") {
    auto message{build("const auto f = \\(*) = 1.0;\n")};
    CHECK(message.find("must not be a function variant") != std::string::npos);
  }
  SUBCASE("Lambda must not be variadic") {
    auto message{build("const auto f = \\(const float x,...) = x;\n")};
    CHECK(message.find("must not be variadic") != std::string::npos);
  }
  SUBCASE("Lambda requires a parameter list") {
    auto message{build("const auto f = \\;\n")};
    CHECK(message.find("expected parameter list") != std::string::npos);
  }
  SUBCASE("Lambda requires a body") {
    auto message{build("const auto f = \\(const float x);\n")};
    CHECK(message.find("expected '=' or compound statement") !=
          std::string::npos);
  }
  SUBCASE("Lambda parameter names must be unique") {
    auto message{
        build("const auto f = \\(const float x, const float x) = x;\n")};
    CHECK(message.find("duplicate parameter name") != std::string::npos);
  }
  SUBCASE("Mutual recursion through a lambda hits the recursion limit") {
    auto message{
        build("@(pure macro)\n"
              "float rec(const auto f, const float x) = f(f, x);\n"
              "export const float bad = "
              "rec(\\(const auto g, const float x) = rec(g, x), 1.0);\n")};
    CHECK(message.find("recursion limit") != std::string::npos);
  }
  fs::remove_all(tmpDir);
}

TEST_CASE("Compiler indirect aggregate parameters") {
  auto tmpDir{fs::temp_directory_path() / "smdl-aggregate-abi-test"};
  fs::remove_all(tmpDir);
  // Dump LLVM-IR for a module. Returns the empty string on failure.
  auto dumpIR{[&](std::string_view text) {
    writeFile(tmpDir / "root" / "main.mdl", text);
    smdl::Compiler compiler{};
    if (compiler.add((tmpDir / "root").string())) return std::string();
    if (compiler.compile(smdl::OPT_LEVEL_NONE)) return std::string();
    auto out{std::string()};
    if (compiler.dump(smdl::DUMP_FORMAT_IR, out)) return std::string();
    return out;
  }};
  SUBCASE("Large aggregates pass as 'byval' pointers") {
    // 'float[24]' is 96 bytes, over the 64-byte threshold.
    auto ir{dumpIR("#smdl\n"
                   "@(pure noinline)\n"
                   "float f(const float[24] w) = w[0];\n"
                   "@(pure visible)\n"
                   "export float use(const float x) = f(float[24]());\n")};
    CHECK(ir.find("byval([24 x float])") != std::string::npos);
  }
  SUBCASE("Aggregates at the threshold still pass by value") {
    // 'float[16]' is exactly 64 bytes. The threshold is deliberately
    // 'greater than': a 'color' is the same size and lives in the hot path.
    auto ir{dumpIR("#smdl\n"
                   "@(pure noinline)\n"
                   "float f(const float[16] w) = w[0];\n"
                   "@(pure visible)\n"
                   "export float use(const float x) = f(float[16]());\n")};
    CHECK(ir.find("byval") == std::string::npos);
    CHECK(ir.find("[16 x float] %w") != std::string::npos);
  }
  SUBCASE("'@(visible)' keeps the by-value convention") {
    // External linkage: the host matches this signature by hand, so it must
    // not silently change.
    auto ir{dumpIR("#smdl\n"
                   "@(pure visible noinline)\n"
                   "export float f(const float[24] w) = w[0];\n")};
    CHECK(ir.find("byval") == std::string::npos);
  }
  fs::remove_all(tmpDir);
}

TEST_CASE("Compiler inline call-site arguments") {
  auto tmpDir{fs::temp_directory_path() / "smdl-inline-args-test"};
  fs::remove_all(tmpDir);
  // Compile a single module and return the first error message, or the
  // empty string on success. The positive behavior of call-site 'inline'
  // is covered by 'testing/smdl/inline_args.smdl'; these subcases pin the
  // error paths.
  auto build{[&](std::string_view text) {
    writeFile(tmpDir / "root" / "main.mdl", std::string("#smdl\n") += text);
    smdl::Compiler compiler{};
    compiler.enableUnitTests = true;
    return buildAll(compiler, {tmpDir / "root"});
  }};
  static const char *sum2{
      "float sum2(const float a, const float b) = a + b;\n"};
  SUBCASE("A scalar does not expand") {
    auto message{build(std::string(sum2) +
                       "export const float bad = sum2(inline 1.0, 2.0);\n")};
    CHECK(message.find("cannot expand 'inline' argument") != std::string::npos);
  }
  SUBCASE("A color does not expand") {
    auto message{build(std::string(sum2) + "unit_test \"t\" {\n"
                                           "  const color c = color(0.5);\n"
                                           "  #assert(sum2(inline c) == 1.0);\n"
                                           "}\n")};
    CHECK(message.find("cannot expand 'inline' argument") != std::string::npos);
  }
  SUBCASE("A pointer does not expand, with a dereference hint") {
    auto message{build("struct P { float a = 1.0; };\n"
                       "float f(const float a) = a;\n"
                       "unit_test \"t\" {\n"
                       "  auto p = P();\n"
                       "  auto q = &p;\n"
                       "  #assert(f(inline q) == 1.0);\n"
                       "}\n")};
    CHECK(message.find("dereference it first") != std::string::npos);
  }
  SUBCASE("'visit inline' is rejected at parse") {
    auto message{build(std::string(sum2) +
                       "export const float bad = "
                       "sum2(visit inline auto(1.0, 2.0));\n")};
    CHECK(message.find("cannot combine 'visit' and 'inline'") !=
          std::string::npos);
  }
  SUBCASE("An inlined argument must not be named") {
    auto message{build(std::string(sum2) +
                       "export const float bad = "
                       "sum2(inline a: auto(1.0, 2.0));\n")};
    CHECK(message.find("must not be named") != std::string::npos);
  }
  SUBCASE("A struct field colliding with a named argument is ambiguous") {
    auto message{build(std::string(sum2) +
                       "struct S { float a = 1.0; float b = 2.0; };\n"
                       "export const float bad = sum2(a: 3.0, inline S());\n")};
    CHECK(message.find("ambiguous name") != std::string::npos);
  }
  SUBCASE("A positional argument after an inlined struct is rejected") {
    auto message{build(std::string(sum2) +
                       "struct S { float a = 1.0; };\n"
                       "export const float bad = sum2(inline S(), 2.0);\n")};
    CHECK(message.find("unnamed arguments must appear before named") !=
          std::string::npos);
  }
  fs::remove_all(tmpDir);
}

// Counts warnings mentioning a substring, so a test can assert how many
// times a diagnostic was raised rather than merely that it was.
class WarningCounter final : public smdl::LogSink {
public:
  WarningCounter(std::string needle) : needle(std::move(needle)) {}

  void logMessage(smdl::LogLevel level, std::string_view message) override {
    if (level == smdl::LOG_LEVEL_WARN &&
        message.find(needle) != std::string_view::npos)
      count++;
  }

  std::string needle{};
  int count{};
};

TEST_CASE("Compiler missing resource warnings") {
  auto tmpDir{fs::temp_directory_path() / "smdl-missing-resource-test"};
  fs::remove_all(tmpDir);
  // A missing texture is a warning, not an error, and the texture reads
  // black -- so the interesting question is how many times it is reported.
  // A material body is emitted three times ('evaluate', 'evaluateOpacity'
  // and 'thinWalledProbe' in 'Type.cc'), and the not-found path cannot be
  // memoized by file hash the way an actual load failure is, so without
  // 'Compiler::logResourceWarningOnce' it would be reported three times.
  auto materialUsing{[](std::string_view fileName) {
    auto text{std::string("#smdl\nimport ::df::*;\nimport ::tex::*;\n"
                          "export material M() = let {\n  auto t = "
                          "texture_2d(\"")};
    text += fileName;
    text += "\", tex::gamma_srgb);\n"
            "  auto c = ::tex::lookup_float3(t, float2(0.5));\n"
            "} in material(surface: material_surface(\n"
            "  scattering: df::diffuse_reflection_bsdf(tint: color(c))));\n";
    return text;
  }};
  SUBCASE("A missing texture is reported exactly once per file") {
    writeFile(tmpDir / "main.mdl", materialUsing("nowhere.png"));
    auto &sink{smdl::Logger::get().addSink<WarningCounter>("nowhere.png")};
    smdl::Compiler compiler{};
    CHECK(buildAll(compiler, {tmpDir / "main.mdl"}).empty());
    CHECK(sink.count == 1);
    // The memo is per compile, not per Compiler: recompiling has to report
    // the same missing file again rather than swallow it.
    REQUIRE(!compiler.compile(smdl::OPT_LEVEL_NONE));
    CHECK(sink.count == 2);
    smdl::Logger::get().reset();
  }
  SUBCASE("Distinct missing textures are each reported") {
    auto text{materialUsing("gone_a.png")};
    text += "export material N() = let {\n"
            "  auto t = texture_2d(\"gone_b.png\", tex::gamma_srgb);\n"
            "  auto c = ::tex::lookup_float3(t, float2(0.5));\n"
            "} in material(surface: material_surface(\n"
            "  scattering: df::diffuse_reflection_bsdf(tint: color(c))));\n";
    writeFile(tmpDir / "main.mdl", text);
    auto &sinkA{smdl::Logger::get().addSink<WarningCounter>("gone_a.png")};
    auto &sinkB{smdl::Logger::get().addSink<WarningCounter>("gone_b.png")};
    smdl::Compiler compiler{};
    CHECK(buildAll(compiler, {tmpDir / "main.mdl"}).empty());
    CHECK(sinkA.count == 1);
    CHECK(sinkB.count == 1);
    smdl::Logger::get().reset();
  }
  fs::remove_all(tmpDir);
}

TEST_CASE("Compiler enableMipMaps") {
  auto tmpDir{fs::temp_directory_path() / "smdl-compiler-mipmap-test"};
  fs::remove_all(tmpDir);
  // A 4x4 image, whose chain is 4x4 -> 2x2 -> 1x1, so a texture that
  // reads the chain bakes 3 levels and one that does not bakes 1. The
  // level count is the thing to pin: it is what keeps JIT code from
  // walking mip levels that were never generated.
  const uint8_t texels[16] = {0,   16,  32,  48,  //
                              64,  80,  96,  112, //
                              128, 144, 160, 176, //
                              192, 208, 224, 240};
  fs::create_directories(tmpDir);
  REQUIRE(
      !smdl::write8bitImage((tmpDir / "mip.png").string(), 4, 4, 1, texels));
  // Build and run a module asserting the baked level count of a texture
  // that asked for mip filtering. Whether the '#assert' folds at compile
  // time or runs in the JIT, a mismatch comes back as a message here.
  auto checkNumLevels{[&](bool enableMipMaps, int numLevels) {
    writeFile(tmpDir / "mips.smdl",
              "#smdl\nimport ::tex::*;\n"
              "unit_test \"Baked level count\" {\n"
              "  const auto t = texture_2d(\"mip.png\", tex::gamma_linear, "
              "use_mipmap: true);\n"
              "  #assert(t.num_levels == " +
                  std::to_string(numLevels) + ");\n}\n");
    smdl::Compiler compiler{};
    compiler.enableMipMaps = enableMipMaps;
    compiler.enableUnitTests = true;
    if (auto message{buildAll(compiler, {tmpDir / "mips.smdl"})};
        !message.empty())
      return message;
    auto allocator{smdl::BumpPtrAllocator()};
    auto wavelengths{std::vector<float>(size_t(compiler.wavelengthBaseMax))};
    auto state{smdl::State()};
    state.allocator = &allocator;
    state.wavelength_min = 380.0f;
    state.wavelength_max = 720.0f;
    state.wavelength_base = wavelengths.data();
    for (uint32_t i = 0; i < compiler.wavelengthBaseMax; i++) {
      float fac{float(i) / float(compiler.wavelengthBaseMax - 1)};
      wavelengths[i] =
          (1 - fac) * state.wavelength_min + fac * state.wavelength_max;
    }
    if (auto error{compiler.runUnitTests(state)}) return error->message;
    return std::string();
  }};
  SUBCASE("The default honors 'use_mipmap: true'") {
    CHECK(checkNumLevels(true, 3) == "");
  }
  SUBCASE("Disabling bakes one level despite 'use_mipmap: true'") {
    CHECK(checkNumLevels(false, 1) == "");
    // And the request really was refused, rather than the check above
    // passing for some unrelated reason.
    CHECK(checkNumLevels(false, 3) != "");
  }
  fs::remove_all(tmpDir);
}

// Collects log messages mentioning a substring at any level, for
// asserting on diagnostics that are not warnings.
class MessageCollector final : public smdl::LogSink {
public:
  MessageCollector(std::string needle) : needle(std::move(needle)) {}

  void logMessage(smdl::LogLevel, std::string_view message) override {
    if (message.find(needle) != std::string_view::npos)
      messages.emplace_back(message);
  }

  std::string needle{};
  std::vector<std::string> messages{};
};

TEST_CASE("Compiler unused image dropping") {
  auto tmpDir{fs::temp_directory_path() / "smdl-unused-image-test"};
  fs::remove_all(tmpDir);
  fs::create_directories(tmpDir);
  // Distinct texels, or the two files would content-hash to one image.
  const uint8_t texelsLive[4] = {32, 64, 96, 128};
  const uint8_t texelsDead[4] = {1, 2, 3, 4};
  REQUIRE(!smdl::write8bitImage((tmpDir / "live.png").string(), 2, 2, 1,
                                texelsLive));
  REQUIRE(!smdl::write8bitImage((tmpDir / "dead.png").string(), 2, 2, 1,
                                texelsDead));
  // 'live.png' is sampled, so its texel pointer must survive into the
  // optimized module; 'dead.png' contributes only its extent, which is
  // baked by the probe, so optimization erases every read and the
  // decode can be skipped. The unit test pins the extent staying valid
  // after the drop: a comptime-false '#assert' is a compile error.
  writeFile(tmpDir / "main.smdl",
            "#smdl\nimport ::df::*;\nimport ::tex::*;\n"
            "export material M() = let {\n"
            "  auto tLive = texture_2d(\"live.png\", tex::gamma_srgb);\n"
            "  auto tDead = texture_2d(\"dead.png\", tex::gamma_srgb);\n"
            "  auto c = ::tex::lookup_float3(tLive, float2(0.5));\n"
            "  auto s = float(::tex::width(tDead)) / 4.0;\n"
            "} in material(surface: material_surface(\n"
            "  scattering: df::diffuse_reflection_bsdf(tint: color(s * c))));"
            "\n"
            "unit_test \"Extent survives the drop\" {\n"
            "  #assert(tex::width(texture_2d(\"dead.png\")) == 2);\n"
            "}\n");
  auto build{[&](smdl::OptLevel optLevel) {
    smdl::Compiler compiler{};
    compiler.enableUnitTests = true;
    if (auto error{compiler.add((tmpDir / "main.smdl").string())})
      return error->message;
    if (auto error{compiler.compile(optLevel)}) return error->message;
    if (auto error{compiler.jitCompile()}) return error->message;
    return std::string();
  }};
  SUBCASE("An unread image is dropped at O2 and a sampled one is kept") {
    auto &dropped{
        smdl::Logger::get().addSink<MessageCollector>("Dropping image")};
    CHECK(build(smdl::OPT_LEVEL_O2) == "");
    REQUIRE(dropped.messages.size() == 1);
    CHECK(dropped.messages[0].find("dead.png") != std::string::npos);
    smdl::Logger::get().reset();
  }
  SUBCASE("The unread image is dropped even at OPT_LEVEL_NONE") {
    // Constant-field elimination bakes the 'texture_2d' struct into the
    // type, so the dead image's texel pointers never enter the IR at
    // all: the image is provably unused with no optimization running.
    // The size requirement doubles as the guard that the sampled image
    // is never dropped.
    auto &dropped{
        smdl::Logger::get().addSink<MessageCollector>("Dropping image")};
    CHECK(build(smdl::OPT_LEVEL_NONE) == "");
    REQUIRE(dropped.messages.size() == 1);
    CHECK(dropped.messages[0].find("dead.png") != std::string::npos);
    smdl::Logger::get().reset();
  }
  fs::remove_all(tmpDir);
}

TEST_CASE("Compiler wavelengthBaseMax") {
  auto tmpDir{fs::temp_directory_path() / "smdl-compiler-wavelength-test"};
  fs::remove_all(tmpDir);
  // One module whose in-JIT unit test exercises the spectral machinery
  // that is easiest to get wrong away from the default 16-band grid:
  // comparisons over 'color' (at 1 band the compare result is a scalar
  // bool rather than a bool vector), '#any'/'#all' reductions of those
  // results, and RGB-to-color construction.
  writeFile(tmpDir / "spectral.smdl",
            "#smdl\nimport ::df::*;\n" + materialDef("main") +
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
    compiler.enableUnitTests = true;
    REQUIRE(compiler.wavelengthBaseMax == numBands);
    REQUIRE(buildAll(compiler, {tmpDir / "spectral.smdl"}) == "");
    REQUIRE(compiler.findMaterial("main") != nullptr);
    // An endpoint-inclusive uniform grid over the visible; a single band
    // sits at the midpoint.
    auto allocator{smdl::BumpPtrAllocator()};
    auto wavelengths{std::vector<float>(size_t(numBands))};
    auto state{smdl::State()};
    state.allocator = &allocator;
    state.wavelength_min = 380.0f;
    state.wavelength_max = 720.0f;
    state.wavelength_base = wavelengths.data();
    for (uint32_t i = 0; i < numBands; i++) {
      float fac{numBands > 1 ? float(i) / float(numBands - 1) : 0.5f};
      wavelengths[i] =
          (1 - fac) * state.wavelength_min + fac * state.wavelength_max;
    }
    REQUIRE(!compiler.runUnitTests(state));
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
        (state.wavelength_max - state.wavelength_min) / float(numBands))};
    state.wavelength_weight = weights.data();
    auto rgbWeighted{compiler.convertColorToRGB(state, colorBuf.data())};
    for (int i = 0; i < 3; i++)
      CHECK(rgbWeighted[i] ==
            doctest::Approx(rgbOfGray[i]).epsilon(1e-4).scale(1.0));
    for (auto &weight : weights) weight *= 2.0f;
    auto rgbDoubled{compiler.convertColorToRGB(state, colorBuf.data())};
    for (int i = 0; i < 3; i++)
      CHECK(rgbDoubled[i] ==
            doctest::Approx(2.0f * rgbWeighted[i]).epsilon(1e-5).scale(1.0));
    state.wavelength_weight = nullptr;
  }
  fs::remove_all(tmpDir);
}

TEST_CASE("Compiler addCode") {
  auto tmpDir{fs::temp_directory_path() / "smdl-source-code-test"};
  fs::remove_all(tmpDir);
  // The render state the unit tests below run against.
  auto runUnitTests{[](smdl::Compiler &compiler) {
    auto allocator{smdl::BumpPtrAllocator()};
    auto wavelengths{std::vector<float>(size_t(compiler.wavelengthBaseMax))};
    auto state{smdl::State()};
    state.allocator = &allocator;
    state.wavelength_min = 380.0f;
    state.wavelength_max = 720.0f;
    state.wavelength_base = wavelengths.data();
    for (uint32_t i = 0; i < compiler.wavelengthBaseMax; i++) {
      float fac{float(i) / float(compiler.wavelengthBaseMax - 1)};
      wavelengths[i] =
          (1 - fac) * state.wavelength_min + fac * state.wavelength_max;
    }
    if (auto error{compiler.runUnitTests(state)}) return error->message;
    return std::string();
  }};
  SUBCASE("Source code compiles as a module with no file") {
    smdl::Compiler compiler{};
    REQUIRE(!compiler.addCode("::host::mats", "#smdl\nimport ::df::*;\n" +
                                                  materialDef("mat_ok")));
    REQUIRE(!compiler.compile(smdl::OPT_LEVEL_NONE));
    REQUIRE(!compiler.jitCompile());
    auto material{compiler.findMaterial("mat_ok")};
    REQUIRE(material != nullptr);
    CHECK(material->qualifiedName == "::host::mats::mat_ok");
    CHECK(material->moduleName == "mats");
    CHECK(material->moduleFileName.empty());
    CHECK(material->moduleDisplayName == "<string ::host::mats>");
  }
  SUBCASE("The leading '::' is optional") {
    smdl::Compiler compiler{};
    auto source{"#smdl\nimport ::df::*;\n" + materialDef("mat_ok")};
    REQUIRE(!compiler.addCode("host::mats", source));
    // The same name and the same source code again is a no-op, so a host
    // may register its defaults defensively.
    CHECK(!compiler.addCode("::host::mats", source));
    REQUIRE(!compiler.compile(smdl::OPT_LEVEL_NONE));
    REQUIRE(!compiler.jitCompile());
    CHECK(compiler.findMaterials("mat_ok").size() == 1);
  }
  SUBCASE("A different body under a taken name is an error") {
    smdl::Compiler compiler{};
    REQUIRE(!compiler.addCode("::host", "#smdl\nexport const int x = 1;\n"));
    auto error{compiler.addCode("::host", "#smdl\nexport const int x = 2;\n")};
    REQUIRE(error.has_value());
    CHECK(error->message.find("already taken") != std::string::npos);
    CHECK(error->message.find("<string ::host>") != std::string::npos);
  }
  SUBCASE("A name taken by a file module is an error") {
    writeFile(tmpDir / "root" / "util.mdl", "#smdl\nexport const int x = 1;\n");
    smdl::Compiler compiler{};
    REQUIRE(!compiler.add((tmpDir / "root").string()));
    auto error{compiler.addCode("::util", "#smdl\nexport const int x = 2;\n")};
    REQUIRE(error.has_value());
    CHECK(error->message.find("already taken") != std::string::npos);
  }
  SUBCASE("A file module added later is shadowed, not an error") {
    writeFile(tmpDir / "root" / "host.mdl",
              "#smdl\nimport ::df::*;\n" + materialDef("from_file"));
    smdl::Compiler compiler{};
    REQUIRE(!compiler.addCode("::host", "#smdl\nimport ::df::*;\n" +
                                            materialDef("from_string")));
    REQUIRE(!compiler.add((tmpDir / "root").string()));
    REQUIRE(!compiler.compile(smdl::OPT_LEVEL_NONE));
    REQUIRE(!compiler.jitCompile());
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
    writeFile(tmpDir / "root" / "util.mdl",
              "#smdl\nexport const int marker_file = 1;\n");
    writeFile(tmpDir / "root" / "main.mdl",
              "#smdl\nimport ::df::*;\nimport ::host::consts::*;\n"
              "export const int echo = host::consts::marker_host;\n" +
                  materialDef("main_ok"));
    smdl::Compiler compiler{};
    REQUIRE(!compiler.addCode(
        "::host::consts",
        "#smdl\nimport ::util::*;\n"
        "export const int marker_host = util::marker_file + 1;\n"));
    REQUIRE(!compiler.add((tmpDir / "root").string()));
    REQUIRE(!compiler.compile(smdl::OPT_LEVEL_NONE));
    REQUIRE(!compiler.jitCompile());
    CHECK(compiler.findMaterial("main_ok") != nullptr);
  }
  SUBCASE("A compile error names the module it came from") {
    smdl::Compiler compiler{};
    REQUIRE(
        !compiler.addCode("::host::bad", "#smdl\nimport ::nonexistent::*;\n"));
    auto error{compiler.compile(smdl::OPT_LEVEL_NONE)};
    REQUIRE(error.has_value());
    CHECK(error->message.find("[<string ::host::bad>:2]") != std::string::npos);
  }
  SUBCASE("Unit tests run") {
    smdl::Compiler compiler{};
    compiler.enableUnitTests = true;
    REQUIRE(!compiler.addCode("::host::tests",
                              "#smdl\nunit_test \"Arithmetic\" {\n"
                              "  int i = 2;\n"
                              "  #assert(i + i == 4);\n}\n"));
    REQUIRE(!compiler.compile(smdl::OPT_LEVEL_NONE));
    REQUIRE(!compiler.jitCompile());
    CHECK(runUnitTests(compiler) == "");
  }
  SUBCASE("The source code outlives the string it was given") {
    smdl::Compiler compiler{};
    {
      auto source{"#smdl\nimport ::df::*;\n" + materialDef("mat_ok")};
      REQUIRE(!compiler.addCode("::host::mats", source));
    }
    // Twice, because 'compile()' resets and re-parses every module: a
    // module of source code has no file to read back.
    for (int i = 0; i < 2; i++) {
      REQUIRE(!compiler.compile(smdl::OPT_LEVEL_NONE));
      REQUIRE(!compiler.jitCompile());
      CHECK(compiler.findMaterial("mat_ok") != nullptr);
    }
  }
  SUBCASE("An anchor directory resolves relative paths") {
    const uint8_t texels[4] = {16, 32, 48, 64};
    fs::create_directories(tmpDir / "anchor");
    REQUIRE(!smdl::write8bitImage((tmpDir / "anchor" / "wood.png").string(), 2,
                                  2, 1, texels));
    writeFile(tmpDir / "anchor" / "helper.mdl",
              "#smdl\nexport const int marker = 3;\n");
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
      compiler.enableUnitTests = true;
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
    auto error{compiler.addCode("::host::mats", source,
                                (tmpDir / "nowhere").string())};
    REQUIRE(error.has_value());
    CHECK(error->message.find("not an existing directory") !=
          std::string::npos);
  }
  SUBCASE("A name that matches a builtin module is warned about") {
    auto &warned{smdl::Logger::get().addSink<MessageCollector>(
        "same name as a builtin")};
    smdl::Compiler compiler{};
    CHECK(!compiler.addCode("::df", "#smdl\nexport const int x = 1;\n"));
    CHECK(warned.messages.size() == 1);
    smdl::Logger::get().reset();
  }
  fs::remove_all(tmpDir);
}
