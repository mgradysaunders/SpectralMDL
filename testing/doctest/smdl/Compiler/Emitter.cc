#include "CompileFixtures.h"

#include <string>
#include <string_view>

#include "smdl/Compiler.h"
#include "smdl/Resource/Image.h"

TEST_CASE("Emitter: a voided field") {
  TempDir tmpDir{"emitter"};
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
    CHECK_CONTAINS(error, "cannot take address");
  }
  SUBCASE("Taking the address of a void value is the same error") {
    auto error{compileSource(tmpDir, "#smdl\n"
                                     "unit_test \"t\" {\n"
                                     "  auto x = void();\n"
                                     "  auto p = &x;\n"
                                     "}\n")};
    CHECK_CONTAINS(error, "cannot take address");
  }
  // A voided field has no storage to write through, so both of these
  // report cleanly instead of aborting the compiler, which is what they
  // did while voided fields still occupied a placeholder byte.
  SUBCASE("Assigning to a voided field is an error") {
    auto error{compileSource(tmpDir, structDef + "unit_test \"t\" {\n"
                                                 "  auto m = Mixed();\n"
                                                 "  m.v = void();\n"
                                                 "}\n")};
    CHECK_CONTAINS(error, "rvalue");
  }
  SUBCASE("Preserving a voided field is an error") {
    auto error{compileSource(tmpDir, structDef + "unit_test \"t\" {\n"
                                                 "  auto m = Mixed();\n"
                                                 "  preserve m.v;\n"
                                                 "}\n")};
    CHECK_CONTAINS(error, "rvalue");
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
}

TEST_CASE("Emitter: a voided parameter") {
  TempDir tmpDir{"emitter-param"};
  // Behavior is covered end to end by 'testing/language/lang/functions.smdl';
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
    CHECK_NOT_CONTAINS(llvmParamsOf(ir, "voided"), "void");
    // And the call site passes exactly two arguments, not three.
    CHECK_CONTAINS(ir, "call i32 @voided(i32 3, i32 4)");
  }
}

TEST_CASE("Emitter: one mip chain per image") {
  TempDir tmpDir{"emitter-mip"};
  const uint8_t texels[4] = {0, 85, 170, 255};
  REQUIRE(
      !smdl::write8bitImage((tmpDir / "height.png").string(), 2, 2, 1, texels));
  SUBCASE("One image, one chain") {
    // The same file wanted with a mean chain and a maximum chain is an
    // error at the second request, naming the first.
    auto error{compileSource(tmpDir,
                             "#smdl\n"
                             "import ::tex::*;\n"
                             "const auto mean = texture_2d(\"height.png\", "
                             "tex::gamma_linear, use_mipmap: true);\n"
                             "const auto peak = texture_2d(\"height.png\", "
                             "tex::gamma_linear, max_mipmap: true);\n")};
    CHECK_CONTAINS(error, "maximum mip chain");
    CHECK_CONTAINS(error, "mean mip chain was requested at");
    CHECK_CONTAINS(error, "main.smdl:3");
  }
  SUBCASE("Agreeing requests share the image") {
    CHECK(compileSource(tmpDir,
                        "#smdl\n"
                        "import ::tex::*;\n"
                        "const auto a = texture_2d(\"height.png\", "
                        "tex::gamma_linear, max_mipmap: true);\n"
                        "const auto b = texture_2d(\"height.png\", "
                        "tex::gamma_linear, use_mipmap: true, max_mipmap: "
                        "true);\n"
                        "const auto c = texture_2d(\"height.png\", "
                        "tex::gamma_linear);\n") == "");
  }
  SUBCASE("The requests must be compile-time") {
    auto error{compileSource(
        tmpDir, "#smdl\n"
                "import ::tex::*;\n"
                "unit_test \"t\" { bool b = $state.wavelengthMin > 0.0; "
                "auto t = texture_2d(\"height.png\", tex::gamma_linear, "
                "max_mipmap: b); }\n")};
    CHECK_CONTAINS(error, "compile-time");
  }
}

TEST_CASE("Emitter: an inferred array size") {
  TempDir tmpDir{"inferred-size"};
  // A macro whose two parameters share the size name 'N'.
  static const char *sharedN{
      "#smdl\n"
      "@(pure macro)\n"
      "int sharedN(const float[<N>] a, const float[<N>] b) = N;\n"};
  SUBCASE("Consistent sizes across a shared size name compile") {
    tmpDir.write("root/main.mdl",
                 std::string(sharedN) +
                     "export const int ok = sharedN(float[2](1.0, 2.0), "
                     "float[2](3.0, 4.0));\n");
    smdl::Compiler compiler{};
    CHECK(buildAll(compiler, {tmpDir / "root"}) == "");
  }
  SUBCASE("Mismatched sizes are rejected at overload resolution") {
    tmpDir.write("root/main.mdl",
                 std::string(sharedN) +
                     "export const int bad = sharedN(float[2](1.0, 2.0), "
                     "float[3](3.0, 4.0, 5.0));\n");
    smdl::Compiler compiler{};
    auto message{buildAll(compiler, {tmpDir / "root"})};
    CHECK_CONTAINS(message, "deduces array size");
  }
  SUBCASE("A local size name must not silently rebind") {
    tmpDir.write("root/main.mdl", "#smdl\n"
                                  "@(pure macro)\n"
                                  "int localRebind() {\n"
                                  "  const float[<N>] a(1.0, 2.0);\n"
                                  "  const float[<N>] b(1.0, 2.0, 3.0);\n"
                                  "  return N + int(a[0] + b[0]);\n"
                                  "}\n"
                                  "export const int bad = localRebind();\n");
    smdl::Compiler compiler{};
    auto message{buildAll(compiler, {tmpDir / "root"})};
    CHECK_CONTAINS(message, "conflicts with");
  }
  SUBCASE("A size name must not silently shadow a same-scope parameter") {
    tmpDir.write(
        "root/main.mdl",
        "#smdl\n"
        "@(pure macro)\n"
        "int collide(const int N, const float[<N>] w) = N + int(w[0]);\n"
        "export const int bad = collide(7, float[3](0.0, 0.0, 0.0));\n");
    smdl::Compiler compiler{};
    auto message{buildAll(compiler, {tmpDir / "root"})};
    CHECK_CONTAINS(message, "conflicts with");
  }
}

TEST_CASE("Emitter: the error paths of a lambda") {
  TempDir tmpDir{"lambda"};
  // Compile a single module and return the first error message, or the
  // empty string on success. The positive behavior of lambdas is covered
  // by 'testing/language/lang/lambdas.smdl'; these subcases pin the error
  // paths.
  auto build{[&](std::string_view text) {
    tmpDir.write("root/main.mdl", std::string("#smdl\n") += text);
    smdl::Compiler compiler{};
    // Some subcases place the erroring code inside a 'unit_test' body,
    // which is only compiled when unit tests are enabled.
    compiler.shouldEmitUnitTests = true;
    return buildAll(compiler, {tmpDir / "root"});
  }};
  SUBCASE("Function values must not pass through non-macro parameters") {
    auto message{build("@(pure)\n"
                       "float apply(const auto f, const float x) = f(x);\n"
                       "export const float bad = "
                       "apply(\\(const float x) = x, 1.0);\n")};
    CHECK_CONTAINS(message, "compile-time only");
  }
  SUBCASE("A variable holding a function must be 'const'") {
    auto message{build("unit_test \"t\" {\n"
                       "  auto f = \\(const float x) = x;\n"
                       "  #assert(f(1.0) == 1.0);\n"
                       "}\n")};
    CHECK_CONTAINS(message, "must be declared 'const'");
  }
  SUBCASE("A variable holding a function must not be 'static'") {
    auto message{build("static const auto f = \\(const float x) = x;\n"
                       "unit_test \"t\" { #assert(f(1.0) == 1.0); }\n")};
    CHECK_CONTAINS(message, "without 'static'");
  }
  SUBCASE("Lambda must not be a function variant") {
    auto message{build("const auto f = \\(*) = 1.0;\n")};
    CHECK_CONTAINS(message, "must not be a function variant");
  }
  SUBCASE("Lambda must not be variadic") {
    auto message{build("const auto f = \\(const float x,...) = x;\n")};
    CHECK_CONTAINS(message, "must not be variadic");
  }
  SUBCASE("Lambda requires a parameter list") {
    auto message{build("const auto f = \\;\n")};
    CHECK_CONTAINS(message, "expected parameter list");
  }
  SUBCASE("Lambda requires a body") {
    auto message{build("const auto f = \\(const float x);\n")};
    CHECK_CONTAINS(message, "expected '=' or compound statement");
  }
  SUBCASE("Lambda parameter names must be unique") {
    auto message{
        build("const auto f = \\(const float x, const float x) = x;\n")};
    CHECK_CONTAINS(message, "duplicate parameter name");
  }
  SUBCASE("Mutual recursion through a lambda hits the recursion limit") {
    auto message{
        build("@(pure macro)\n"
              "float rec(const auto f, const float x) = f(f, x);\n"
              "export const float bad = "
              "rec(\\(const auto g, const float x) = rec(g, x), 1.0);\n")};
    CHECK_CONTAINS(message, "recursion limit");
  }
}

TEST_CASE("Emitter: the byval threshold for an aggregate parameter") {
  TempDir tmpDir{"aggregate-abi"};
  // Dump LLVM-IR for a module. Returns the empty string on failure.
  auto dumpIR{[&](std::string_view text) {
    tmpDir.write("root/main.mdl", text);
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
    CHECK_CONTAINS(ir, "byval([24 x float])");
  }
  SUBCASE("Aggregates at the threshold still pass by value") {
    // 'float[16]' is exactly 64 bytes. The threshold is deliberately
    // 'greater than': a 'color' is the same size and lives in the hot path.
    auto ir{dumpIR("#smdl\n"
                   "@(pure noinline)\n"
                   "float f(const float[16] w) = w[0];\n"
                   "@(pure visible)\n"
                   "export float use(const float x) = f(float[16]());\n")};
    CHECK_NOT_CONTAINS(ir, "byval");
    CHECK_CONTAINS(ir, "[16 x float] %w");
  }
  SUBCASE("'@(visible)' keeps the by-value convention") {
    // External linkage: the host matches this signature by hand, so it must
    // not silently change.
    auto ir{dumpIR("#smdl\n"
                   "@(pure visible noinline)\n"
                   "export float f(const float[24] w) = w[0];\n")};
    CHECK_NOT_CONTAINS(ir, "byval");
  }
}

TEST_CASE("Emitter: the error paths of an inline argument") {
  TempDir tmpDir{"inline-args"};
  // Compile a single module and return the first error message, or the
  // empty string on success. The positive behavior of call-site 'inline'
  // is covered by 'testing/language/lang/structs.smdl'; these subcases pin the
  // error paths.
  auto build{[&](std::string_view text) {
    tmpDir.write("root/main.mdl", std::string("#smdl\n") += text);
    smdl::Compiler compiler{};
    compiler.shouldEmitUnitTests = true;
    return buildAll(compiler, {tmpDir / "root"});
  }};
  static const char *sum2{
      "float sum2(const float a, const float b) = a + b;\n"};
  SUBCASE("A scalar does not expand") {
    auto message{build(std::string(sum2) +
                       "export const float bad = sum2(inline 1.0, 2.0);\n")};
    CHECK_CONTAINS(message, "cannot expand 'inline' argument");
  }
  SUBCASE("A color does not expand") {
    auto message{build(std::string(sum2) + "unit_test \"t\" {\n"
                                           "  const color c = color(0.5);\n"
                                           "  #assert(sum2(inline c) == 1.0);\n"
                                           "}\n")};
    CHECK_CONTAINS(message, "cannot expand 'inline' argument");
  }
  SUBCASE("A pointer does not expand, with a dereference hint") {
    auto message{build("struct P { float a = 1.0; };\n"
                       "float f(const float a) = a;\n"
                       "unit_test \"t\" {\n"
                       "  auto p = P();\n"
                       "  auto q = &p;\n"
                       "  #assert(f(inline q) == 1.0);\n"
                       "}\n")};
    CHECK_CONTAINS(message, "dereference it first");
  }
  SUBCASE("'visit inline' is rejected at parse") {
    auto message{build(std::string(sum2) +
                       "export const float bad = "
                       "sum2(visit inline auto(1.0, 2.0));\n")};
    CHECK_CONTAINS(message, "cannot combine 'visit' and 'inline'");
  }
  SUBCASE("An inlined argument must not be named") {
    auto message{build(std::string(sum2) +
                       "export const float bad = "
                       "sum2(inline a: auto(1.0, 2.0));\n")};
    CHECK_CONTAINS(message, "must not be named");
  }
  SUBCASE("A struct field colliding with a named argument is ambiguous") {
    auto message{build(std::string(sum2) +
                       "struct S { float a = 1.0; float b = 2.0; };\n"
                       "export const float bad = sum2(a: 3.0, inline S());\n")};
    CHECK_CONTAINS(message, "ambiguous name");
  }
  SUBCASE("A positional argument after an inlined struct is rejected") {
    auto message{build(std::string(sum2) +
                       "struct S { float a = 1.0; };\n"
                       "export const float bad = sum2(inline S(), 2.0);\n")};
    CHECK_CONTAINS(message, "unnamed arguments must appear before named");
  }
}
