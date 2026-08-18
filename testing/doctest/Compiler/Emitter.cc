#include "doctest.h"

#include <filesystem>
#include <fstream>
#include <string>
#include <string_view>

#include "smdl/Compiler.h"

namespace fs = std::filesystem;

// Compile one module of source and return the error message, or the
// empty string on success. Compile-time diagnostics are what this file
// tests, so it deliberately stops before 'jitCompile()'.
static std::string compileSource(const fs::path &tmpDir,
                                 std::string_view sourceCode) {
  auto path{tmpDir / "main.smdl"};
  fs::create_directories(tmpDir);
  std::ofstream(path) << sourceCode;
  smdl::Compiler compiler{};
  // Without this, 'unit_test' bodies are never emitted, so nothing below
  // would be diagnosed at all.
  compiler.enableUnitTests = true;
  if (auto error{compiler.add(path.string())}) return error->message;
  if (auto error{compiler.compile(smdl::OPT_LEVEL_NONE)}) return error->message;
  return {};
}

// Compile one module of source and return its unoptimized LLVM-IR.
static std::string compileToIR(const fs::path &tmpDir,
                               std::string_view sourceCode) {
  auto path{tmpDir / "main.smdl"};
  fs::create_directories(tmpDir);
  std::ofstream(path) << sourceCode;
  smdl::Compiler compiler{};
  REQUIRE(!compiler.add(path.string()));
  REQUIRE(!compiler.compile(smdl::OPT_LEVEL_NONE));
  auto ir{std::string()};
  REQUIRE(!compiler.dump(smdl::DUMP_FORMAT_IR, ir));
  return ir;
}

// The parameter list of the LLVM definition of 'name', i.e. the text
// between the parentheses of its 'define' line.
static std::string llvmParamsOf(const std::string &ir, std::string_view name) {
  auto marker{std::string("@") + std::string(name) + "("};
  auto i{ir.find("define")};
  while (i != std::string::npos) {
    auto lineEnd{ir.find('\n', i)};
    auto line{ir.substr(i, lineEnd - i)};
    if (auto j{line.find(marker)}; j != std::string::npos) {
      auto open{j + marker.size()};
      auto close{line.rfind(')')};
      REQUIRE(close != std::string::npos);
      REQUIRE(close >= open);
      return line.substr(open, close - open);
    }
    i = ir.find("define", i + 1);
  }
  FAIL("no LLVM definition of " << name << " in:\n" << ir);
  return {};
}

TEST_CASE("Emitter voided fields") {
  auto tmpDir{fs::temp_directory_path() / "smdl-emitter-test"};
  fs::remove_all(tmpDir);
  // A struct with a voided field between two live ones, so that the
  // cases below exercise the field-index-to-element-index mapping too.
  const auto structDef{std::string("#smdl\n"
                                   "struct Mixed {\n"
                                   "  int a = 1;\n"
                                   "  auto v = void();\n"
                                   "  float b = 2.0;\n"
                                   "};\n")};
  SUBCASE("Taking the address of a voided field is an error") {
    auto error{compileSource(tmpDir, structDef + "unit_test \"t\" {\n"
                                                 "  auto m = Mixed();\n"
                                                 "  auto p = &m.v;\n"
                                                 "}\n")};
    CHECK(error.find("cannot take address") != std::string::npos);
  }
  SUBCASE("Taking the address of a void value is the same error") {
    auto error{compileSource(tmpDir, "#smdl\n"
                                     "unit_test \"t\" {\n"
                                     "  auto x = void();\n"
                                     "  auto p = &x;\n"
                                     "}\n")};
    CHECK(error.find("cannot take address") != std::string::npos);
  }
  // A voided field has no storage to write through, so both of these
  // report cleanly instead of aborting the compiler, which is what they
  // did while voided fields still occupied a placeholder byte.
  SUBCASE("Assigning to a voided field is an error") {
    auto error{compileSource(tmpDir, structDef + "unit_test \"t\" {\n"
                                                 "  auto m = Mixed();\n"
                                                 "  m.v = void();\n"
                                                 "}\n")};
    CHECK(error.find("rvalue") != std::string::npos);
  }
  SUBCASE("Preserving a voided field is an error") {
    auto error{compileSource(tmpDir, structDef + "unit_test \"t\" {\n"
                                                 "  auto m = Mixed();\n"
                                                 "  preserve m.v;\n"
                                                 "}\n")};
    CHECK(error.find("rvalue") != std::string::npos);
  }
  // The address-of error must not reach the meta-type branch of
  // 'emitOp()', where '&' builds a pointer type rather than taking an
  // address. '&void' is a legal type, and 'api.smdl' uses it.
  SUBCASE("The '&void' pointer type still compiles") {
    auto error{compileSource(tmpDir, "#smdl\n"
                                     "unit_test \"t\" {\n"
                                     "  &void p = none;\n"
                                     "  #assert(!p);\n"
                                     "}\n")};
    CHECK(error.empty());
  }
  // The way to write generic code over a field that may or may not be
  // voided. The dead branch of a compile-time '?:' is never emitted, so
  // the address-of never runs into the error above.
  SUBCASE("A '#hasField' guard makes the address-of legal") {
    auto error{compileSource(
        tmpDir, structDef + "unit_test \"t\" {\n"
                            "  auto m = Mixed();\n"
                            "  const auto pv = #hasField(m, \"v\") ? "
                            "&m.v : none;\n"
                            "  const auto pb = #hasField(m, \"b\") ? "
                            "&m.b : none;\n"
                            "  #assert(#isVoid(pv));\n"
                            "  #assert(*pb == 2.0);\n"
                            "}\n")};
    CHECK(error.empty());
  }
  fs::remove_all(tmpDir);
}

TEST_CASE("Emitter voided parameters") {
  auto tmpDir{fs::temp_directory_path() / "smdl-emitter-param-test"};
  fs::remove_all(tmpDir);
  // Behavior is covered end to end by 'testing/smdl/language_features.smdl';
  // what only the IR can show is that the voided parameter is *absent*
  // from the signature rather than passed as an undefined placeholder,
  // which is the whole point of the change and which no '#assert' could
  // tell apart.
  SUBCASE("A voided parameter is absent from the signature and the call") {
    auto ir{compileToIR(tmpDir, "#smdl\n"
                                "@(pure)\n"
                                "int voided(int a, auto x, int b) = a + b;\n"
                                "@(pure)\n"
                                "int plain(int a, int b) = a + b;\n"
                                "exec {\n"
                                "  #print(voided(3, void(), 4), plain(3, 4));\n"
                                "}\n")};
    // The voided parameter leaves no trace: the two functions have the
    // same signature, and neither mentions a placeholder.
    CHECK(llvmParamsOf(ir, "voided") == llvmParamsOf(ir, "plain"));
    CHECK(llvmParamsOf(ir, "voided").find("void") == std::string::npos);
    // And the call site passes exactly two arguments, not three.
    CHECK(ir.find("call i32 @voided(i32 3, i32 4)") != std::string::npos);
  }
  fs::remove_all(tmpDir);
}

TEST_CASE("Emitter pbr_maps load flags") {
  auto tmpDir{fs::temp_directory_path() / "smdl-emitter-pbr-test"};
  fs::remove_all(tmpDir);
  // The flags decide which images load and which fields the resulting
  // type has, so a runtime value can never reach one. The pack itself
  // need not exist: the argument check runs before the pack is located.
  auto materialWith{[](std::string_view args) {
    auto text{std::string("#smdl\nimport ::df::*;\n"
                          "using ::extras::pbr import *;\n"
                          "export material M(bool flag = false) = let {\n"
                          "  auto p = pbr_maps(\"nowhere\"")};
    text += args;
    text += ");\n"
            "} in material(surface: material_surface(\n"
            "  scattering: df::diffuse_reflection_bsdf()));\n";
    return text;
  }};
  SUBCASE("A runtime flag is an error naming the parameter") {
    auto error{compileSource(tmpDir, materialWith(", use_mipmap: flag"))};
    CHECK(error.find("use_mipmap") != std::string::npos);
    CHECK(error.find("compile-time") != std::string::npos);
  }
  SUBCASE("Each flag is blamed by its own name") {
    auto error{compileSource(tmpDir, materialWith(", no_parallax: flag"))};
    CHECK(error.find("no_parallax") != std::string::npos);
    CHECK(error.find("use_mipmap") == std::string::npos);
  }
  SUBCASE("Compile-time flags are accepted") {
    // A pack that does not exist is a warning, not an error, so this
    // isolates the argument check from the resource lookup.
    CHECK(compileSource(tmpDir, materialWith(", no_height: true, "
                                             "use_mipmap: true"))
              .empty());
  }
  SUBCASE("Class weights on a set with no class map blame the flag") {
    // A set with no class map is a compile-time error to ask for class
    // weights, and since opting out is one way to get there, the
    // diagnostic has to mention it.
    auto error{compileSource(
        tmpDir, "#smdl\nimport ::df::*;\n"
                "using ::extras::pbr import *;\n"
                "export material M() = let {\n"
                "  auto p = pbr_maps(\"nowhere\", no_class_map: true);\n"
                "  auto w = class_weights(p);\n"
                "} in material(surface: material_surface(\n"
                "  scattering: df::diffuse_reflection_bsdf()));\n")};
    CHECK(error.find("class_weights") != std::string::npos);
    CHECK(error.find("no_class_map") != std::string::npos);
  }
  fs::remove_all(tmpDir);
}
