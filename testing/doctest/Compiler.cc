#include "doctest.h"

#include <algorithm>
#include <filesystem>
#include <fstream>
#include <iterator>
#include <string>
#include <vector>

#include "smdl/Compiler.h"
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
  // The six '#is_default'-derived structural bits are always known, and
  // at -O2 the constant-foldable value bits are too.
  constexpr int structuralBits{MATERIAL_HAS_SURFACE | MATERIAL_HAS_BACKFACE |
                               MATERIAL_HAS_SURFACE_EMISSION |
                               MATERIAL_HAS_BACKFACE_EMISSION |
                               MATERIAL_HAS_VOLUME | MATERIAL_HAS_HAIR};
  constexpr int allBits{structuralBits | MATERIAL_THIN_WALLED |
                        MATERIAL_HAS_CUTOUT};
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
