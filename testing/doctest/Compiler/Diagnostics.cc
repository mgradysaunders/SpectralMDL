/// \file
/// The words a failing compile fails with: where a message points, what
/// it suggests when a name is close to a real one, which overloads it
/// names as candidates, and how it phrases the refusal.
///
/// A compile that must fail is the one thing the SMDL-language suite
/// cannot express, since a failing compile aborts its run, so every
/// negative case in the language lives here. The machinery is spread
/// across the parser, the emitter, and the type system; the contract is
/// only observable from outside, which is why this file is named for the
/// contract rather than for a source file.

#include "CompileFixtures.h"

#include <string>

#include "smdl/Compiler.h"

TEST_CASE("Compiler: where a message points and what it quotes") {
  // Compile source that is expected to fail, returning the whole error so
  // that the message and its source snippet can both be checked.
  SUBCASE("A message carries the line, the column, and the source line") {
    smdl::Error error{compileError("#smdl\nexec { int i = nope; }\n")};
    CHECK_CONTAINS(error.message, "[<string ::diag>:2:16]");
    CHECK_CONTAINS(error.snippet, "exec { int i = nope; }");
    CHECK_CONTAINS(error.snippet, '^');
  }
  SUBCASE("A quoted candidate note does not repeat the caret") {
    smdl::Error error{compileError("#smdl\nstruct S { int alpha = 1; };\n"
                                   "exec { auto s = S(alhpa: 2); }\n")};
    CHECK_CONTAINS(error.message, "No parameter named \"alhpa\"; did you mean "
                                  "\"alpha\"?");
    // The caret belongs to the primary error, not to the note quoting the
    // rejected candidate.
    CHECK_NOT_CONTAINS(error.message, '^');
    CHECK_CONTAINS(error.snippet, '^');
  }
  SUBCASE("Too many arguments reports how many were expected") {
    smdl::Error error{compileError("#smdl\nint f(int a) { return a; }\n"
                                   "exec { #assert(f(1, 2) == 1); }\n")};
    CHECK_CONTAINS(error.message, "expected at most 1, got 2");
  }
  SUBCASE("Integer division by a constant zero is rejected") {
    CHECK_CONTAINS(
        compileError("#smdl\nexec { const int i = 1 / 0; }\n").message,
        "integer division by zero");
    CHECK_CONTAINS(
        compileError("#smdl\nexec { const int i = 1 % 0; }\n").message,
        "integer remainder by zero");
    // A compound assignment lowers to the same operator.
    CHECK_CONTAINS(compileError("#smdl\nexec { int i = 4; i /= 0; }\n").message,
                   "integer division by zero");
    // One zero lane is enough to poison a vector divide.
    CHECK_CONTAINS(
        compileError(
            "#smdl\nexec { const auto v = int2(1, 2) / int2(1, 0); }\n")
            .message,
        "integer division by zero");
    // Floating point division by zero is well defined and stays legal.
    CHECK(
        compileError("#smdl\nexec { const float f = 1.0 / 0.0; }\n").message ==
        "compiled without error");
  }
  SUBCASE("A debug line leads with the same location markup") {
    const CollectedLog logged{"New material", /*shouldCollectDebug=*/true};
    smdl::Compiler compiler{};
    REQUIRE_OK(compiler.addCode("::diag", "#smdl\nimport ::df::*;\n" +
                                              minimalMaterial("m")));
    REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_NONE));
    REQUIRE(logged.messages().size() == 1);
    CHECK(smdl::startsWith(logged.messages()[0], "[<string ::diag>:3:"));
    CHECK_CONTAINS(logged.messages()[0], "] New material \"::diag::m\"");
  }
  SUBCASE("A run-time assertion failure reports where it failed") {
    smdl::Compiler compiler{};
    compiler.shouldEmitUnitTests = true;
    REQUIRE_OK(compiler.addCode("::diag", "#smdl\nunit_test \"t\" {\n"
                                          "  int i = 1;\n"
                                          "  #assert(i == 2);\n}\n"));
    REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_NONE));
    REQUIRE_OK(compiler.jitCompile());
    StateStorage storage{compiler};
    smdl::State state{storage.makeState()};
    const std::optional<smdl::Error> error{compiler.runUnitTests(state)};
    REQUIRE(error.has_value());
    CHECK_CONTAINS(error->message, "[<string ::diag>:4:");
    CHECK_CONTAINS(error->message, "Assertion failed: i == 2");
    CHECK_CONTAINS(error->snippet, "#assert(i == 2);");
  }
}

TEST_CASE("Compiler: the name a misspelling is corrected to") {
  auto errorFor{[&](std::string body) {
    return compileError("#smdl\nexec {\n" + std::move(body) + "\n}\n").message;
  }};
  SUBCASE("The C spelling of an intrinsic suggests the intrinsic") {
    CHECK_CONTAINS(errorFor(" assert(1 == 1);"), "did you mean \"#assert\"?");
    CHECK_CONTAINS(errorFor(" printf(\"hi\");"), "did you mean \"#print\"?");
    // Wins over 'size_t', which is a keyword two edits away, because the
    // candidates are weighed together instead of list by list.
    CHECK_CONTAINS(errorFor(" #print(sizeof(int));"),
                   "did you mean \"#sizeOf\"?");
  }
  SUBCASE("A word-shaped literal is suggested") {
    CHECK_CONTAINS(errorFor(" bool b = True; #print(b);"),
                   "did you mean \"true\"?");
  }
  SUBCASE("A bare 'state' suggests '$state'") {
    CHECK_CONTAINS(errorFor(" #print(state);"), "did you mean \"$state\"?");
  }
  SUBCASE("A qualified name suggests within its module") {
    smdl::Error error{compileError("#smdl\nimport ::math::*;\n"
                                   "exec { #print(math::sqr(4.0)); }\n")};
    CHECK_CONTAINS(error.message, "did you mean \"math::sqrt\"?");
  }
  SUBCASE("A suggestion keeps the kind of what was typed") {
    // 'diffuse_edf' is nearer to 'diffuse_bsdf' by edit distance, and is
    // the wrong kind of distribution function, so nothing is suggested.
    smdl::Error error{compileError("#smdl\nimport ::df::*;\n"
                                   "exec { auto b = df::diffuse_bsdf(); }\n")};
    CHECK_CONTAINS(error.message, "Cannot resolve identifier");
    CHECK_NOT_CONTAINS(error.message, "did you mean");
  }
  SUBCASE("An imported module that is not opened says so") {
    smdl::Error error{compileError("#smdl\nimport ::math::*;\n"
                                   "exec { #print(sqrt(4.0)); }\n")};
    CHECK_CONTAINS(error.message, "\"math\" is imported but not opened");
    CHECK_CONTAINS(error.message, "\"math::sqrt\"");
    CHECK_CONTAINS(error.message, "using ::math import sqrt;");
  }
  SUBCASE("A misspelled module suggests the builtin") {
    smdl::Error error{
        compileError("#smdl\nimport ::maths::*;\nexec { #print(1); }\n")};
    CHECK_CONTAINS(error.message, "did you mean \"::math\"?");
  }
  SUBCASE("A misspelled imported name suggests within the module") {
    smdl::Error error{compileError("#smdl\nusing ::math import sqrtt;\n"
                                   "exec { #print(1); }\n")};
    CHECK_CONTAINS(error.message, "did you mean \"sqrt\"?");
  }
  SUBCASE("A misspelled field suggests the field") {
    smdl::Error error{compileError("#smdl\nstruct S { int alpha = 1; };\n"
                                   "exec { #print(S().alhpa); }\n")};
    CHECK_CONTAINS(error.message, "did you mean \"alpha\"?");
  }
  SUBCASE("A swizzle off the end lists the components") {
    smdl::Error error{compileError("#smdl\nexec { float2 v; #print(v.z); }\n")};
    CHECK_CONTAINS(error.message, "the components are x, y (or r, g)");
  }
  SUBCASE("'=' where ':' was meant names the mistake") {
    smdl::Error error{
        compileError("#smdl\nexport material m() = material(ior = 1.5);\n")};
    CHECK_CONTAINS(error.message,
                   "a named argument is written \"ior: ...\", not "
                   "\"ior = ...\"");
  }
  SUBCASE("An empty 'exec' is skipped with a warning") {
    const CollectedLog warned{"empty body does nothing"};
    smdl::Compiler compiler{};
    REQUIRE_OK(compiler.addCode("::diag", "#smdl\nexec {}\n"));
    REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_NONE));
    REQUIRE_OK(compiler.jitCompile());
    CHECK(warned.messages().size() == 1);
    // Nothing was emitted, so there is nothing to run.
    CHECK_OK(compiler.runExecs());
  }
  SUBCASE("An assignment to something real stays an assignment") {
    CHECK(compileError("#smdl\nint f(int a) { return a; }\n"
                       "export int g() { int y = 0; return f(y = 3); }\n")
              .message == "compiled without error");
  }
}

TEST_CASE("Compiler: the overloads a rejected call names") {
  SUBCASE("A rejected overload names itself and where it is declared") {
    smdl::Error error{compileError("#smdl\nint f(int a) { return a; }\n"
                                   "exec { #assert(f(1, 2) == 1); }\n")};
    // The signature identifies which candidate the note is about, and the
    // location is the declaration, not the call site every candidate was
    // probed against.
    CHECK_CONTAINS(error.message, "candidate rejected: f(int a) declared at "
                                  "[<string ::diag>:2:");
    CHECK_CONTAINS(error.message, "expected at most 1, got 2");
  }
  SUBCASE("Every overload of a builtin is told apart") {
    smdl::Error error{compileError(
        "#smdl\nimport ::tex::*;\n"
        "export material m(uniform texture_2d t = texture_2d()) = material(\n"
        "  surface: material_surface(emission: material_emission(\n"
        "    intensity: tex::lookup_color(t, 0.5))));\n")};
    CHECK_CONTAINS(error.message, "lookup_color(texture_2d tex");
    CHECK_CONTAINS(error.message, "lookup_color(texture_3d tex");
    CHECK_CONTAINS(error.message, "<builtin ::tex>");
  }
  SUBCASE("A forwarded call leads with the name that was written") {
    smdl::Error error{compileError(
        "#smdl\nimport ::df::*;\nexport material m() = material(surface: "
        "material_surface(scattering: "
        "df::microfacet_ggx_smith_bsdf(roughness: 0.1)));\n")};
    // A '(*)' forwarder has to lead with the name and the arguments the
    // caller wrote, not with the internal callee and the two arguments the
    // forwarder injects.
    CHECK_CONTAINS(error.message,
                   "Cannot call \"microfacet_ggx_smith_bsdf\" with "
                   "arguments \"(roughness: float)\"");
    CHECK_CONTAINS(error.message, "forwards to:");
    CHECK_CONTAINS(error.message, "did you mean \"roughness_u\"?");
  }
  SUBCASE("A builtin type says what it accepts") {
    CHECK_CONTAINS(
        compileError("#smdl\nexec { float3 v = float3(1, 2, 3, 4); }\n")
            .message,
        "takes one scalar, or scalars and shorter vectors "
        "totalling 3 components");
    CHECK_CONTAINS(
        compileError("#smdl\nexec { float4x3 m = float4x3(1, 2, 3); }\n")
            .message,
        "4 column vectors of 3 components, or 12 scalars");
    CHECK_CONTAINS(
        compileError("#smdl\nexec { color c = color(1.0, 0.5); }\n").message,
        "'(wavelengths, amplitudes)'");
    CHECK_CONTAINS(compileError("#smdl\nexec { int[3] a(1, 2); }\n").message,
                   "takes 3 elements or one array of that size");
  }
  SUBCASE("Narrowing to every overload decides nothing") {
    // A widening conversion beats a narrowing one, and the specification's
    // order decides between two that widen, but nothing decides between two
    // that narrow, least of all the narrower type.
    smdl::Error error{compileError("#smdl\n@(pure) int f(int x) = x;\n"
                                   "@(pure) int f(float x) = int(x);\n"
                                   "exec { #assert(f(1.5d) == 1); }\n")};
    CHECK_CONTAINS(error.message,
                   "Function \"f\" is ambiguous for arguments \"(double)\"");
  }
  SUBCASE("Overloads the arguments rank in opposite orders are ambiguous") {
    // The specification's own example: each overload is exact for one
    // argument and widens the other.
    smdl::Error error{compileError(
        "mdl 1.9;\nint bar(float a, double b) = 5;\n"
        "int bar(double a, float b) = 6;\n"
        "export material m() = material(surface: material_surface(emission:\n"
        "  material_emission(intensity: color(float(bar(1.0f, 1.0f))))));\n")};
    CHECK_CONTAINS(error.message, "Function \"bar\" is ambiguous for "
                                  "arguments \"(float, float)\"");
  }
}

TEST_CASE("Compiler: a function declared ahead of its definition") {
  // MDL declares a function ahead of the definition that follows it, and
  // the two are one function only if they declare the same one.
  SUBCASE("The definition returns what the declaration does") {
    CHECK_CONTAINS(compileError("mdl 1.9;\nfloat f(float x);\n"
                                "int f(float x) { return int(x); }\n")
                       .message,
                   "Function \"f\" is declared again returning \"int\", "
                   "where its declaration at [<string ::diag>:2:1] returns "
                   "\"float\"");
  }
  SUBCASE("The definition names each parameter as the declaration does") {
    CHECK_CONTAINS(compileError("mdl 1.9;\nfloat f(float x);\n"
                                "float f(float y) { return y; }\n")
                       .message,
                   "Function \"f\" is declared again with parameter \"y\", "
                   "which its declaration at [<string ::diag>:2:1] names "
                   "\"x\"");
  }
  SUBCASE("Only the first declaration has default initializers") {
    CHECK_CONTAINS(
        compileError("mdl 1.9;\nfloat f(float x, float y = 1.0);\n"
                     "float f(float x, float y = 2.0) { return x + y; }\n")
            .message,
        "Function \"f\" is declared again with a default initializer for "
        "\"y\", which only its first declaration, at "
        "[<string ::diag>:2:1], may have");
  }
  SUBCASE("A call compiled before the definition says so") {
    // A material compiles where it is declared, so a definition below it
    // is one it cannot reach.
    CHECK_CONTAINS(
        compileError(
            "mdl 1.9;\nfloat f(float x);\n"
            "export material m() = material(surface: material_surface(\n"
            "  emission: material_emission(intensity: color(f(1.0)))));\n"
            "float f(float x) { return x; }\n")
            .message,
        "Function \"f\" declared at [<string ::diag>:2:1] is not defined "
        "before this call");
  }
}

TEST_CASE("Compiler: what an open function refuses") {
  SUBCASE("An open function has a parameter to dispatch on") {
    CHECK_CONTAINS(compileError("#smdl\n@(open) int f();\n").message,
                   "Function \"f\" declared '@(open)' must have a parameter "
                   "to dispatch on");
  }
  SUBCASE("An open function is neither foreign nor visible") {
    CHECK_CONTAINS(
        compileError("#smdl\n@(pure open foreign) int f(int x);\n").message,
        "declared '@(open)' must not be '@(foreign)'");
    CHECK_CONTAINS(
        compileError("#smdl\n@(open visible) int f(int x) = x;\n").message,
        "declared '@(open)' must not be '@(visible)'");
  }
  SUBCASE("A type that implements nothing is named with its declaration") {
    smdl::Error error{
        compileError("#smdl\ntag shape;\n"
                     "@(pure open) float area(const &shape this);\n"
                     "struct Triangle: shape { float base; };\n"
                     "exec { auto t = Triangle(base: 1.0); #print(area(&t)); "
                     "}\n")};
    CHECK_CONTAINS(error.message,
                   "Open function \"area\" has no implementation for "
                   "\"Triangle\" declared at [<string ::diag>:4:");
  }
  SUBCASE("Only the candidates beside the type are named") {
    // The library's own overloads are rejected for the same arguments, but
    // the type's author can do nothing about them, so they are noise.
    smdl::Compiler compiler{};
    REQUIRE_OK(compiler.addCode(
        "::lib", "#smdl\nexport tag shape;\n"
                 "@(pure open) export float area(const &shape this);\n"
                 "export struct Square: shape { float side; };\n"
                 "@(pure) float area(const &Square this) = this.side;\n"
                 "@(pure macro) export float areaOf(const auto ptr) = "
                 "area(ptr);\n"));
    REQUIRE_OK(compiler.addCode(
        "::user", "#smdl\nimport ::lib::*;\n"
                  "export struct Triangle: lib::shape { float base; };\n"
                  "@(pure) float area(const &Triangle this, int scale) = "
                  "this.base * float(scale);\n"
                  "exec { auto t = Triangle(base: 1.0); "
                  "#print(lib::areaOf(&t)); }\n"));
    const std::optional<smdl::Error> error{
        compiler.compile(smdl::OPT_LEVEL_NONE)};
    REQUIRE_MESSAGE(error.has_value(), "compiled without error");
    CHECK_CONTAINS(error->message,
                   "Open function \"area\" has no implementation for "
                   "\"Triangle\"");
    CHECK_CONTAINS(error->message,
                   "candidate rejected: area(&Triangle this, int scale) "
                   "declared at [<string ::user>:4:");
    CHECK_NOT_CONTAINS(error->message, "Square");
  }
}

TEST_CASE("Compiler: what the author of a distribution is told") {
  // Every source starts with the same imports and a BSDF struct, followed by
  // whatever the subcase is about.
  auto withBSDF{[](std::string_view lobes, std::string_view rest) {
    return smdl::concat("#smdl\nimport ::df::*;\nexport struct MyBSDF: bsdf { "
                        "static const int _lobes = ",
                        lobes, "; };\n", rest);
  }};
  const std::string_view eval{
      "@(pure) auto scatterEval(const &MyBSDF this [[anno::unused()]], const "
      "&df::ScatterEvalParams params [[anno::unused()]]) = "
      "df::ScatterEvalResult(isBlack: true);\n"};
  const std::string_view draw{
      "@(pure) auto scatterDraw(const &MyBSDF this [[anno::unused()]], const "
      "&df::ScatterDrawParams params [[anno::unused()]]) = "
      "df::ScatterDrawResult();\n"};
  const std::string_view material{"export material mine() = material(surface: "
                                  "material_surface(scattering: MyBSDF()));\n"};
  SUBCASE("A missing implementation says what to declare") {
    smdl::Error error{compileError(withBSDF(
        "df::DF_MATTE_BRDF",
        smdl::concat(eval,
                     "@(pure) auto scatterSample(const &MyBSDF this "
                     "[[anno::unused()]], const &df::ScatterDrawParams params "
                     "[[anno::unused()]]) = df::ScatterDrawResult();\n",
                     "@(pure) auto scatterDraw(const &MyBSDF this "
                     "[[anno::unused()]], const df::ScatterDrawParams params "
                     "[[anno::unused()]]) = df::ScatterDrawResult();\n")))};
    CHECK_CONTAINS(error.message, "[<string ::diag>:3:8] Struct \"MyBSDF\" has "
                                  "no implementation of \"scatterDraw\", which "
                                  "its tag \"bsdf\" requires");
    CHECK_CONTAINS(error.message,
                   "to implement it: @(pure) auto scatterDraw(const &MyBSDF "
                   "this, inline const &df::ScatterDrawParams params)");
    // The one with the right name and the wrong signature, and the one with
    // the right signature and the wrong name.
    CHECK_CONTAINS(error.message,
                   "candidate rejected: scatterDraw(&MyBSDF this, "
                   "ScatterDrawParams params)");
    CHECK_CONTAINS(error.message,
                   "similar: scatterSample(&MyBSDF this, &ScatterDrawParams "
                   "params) declared at [<string ::diag>:5:");
  }
  SUBCASE("A distribution that declares no lobes is told how") {
    CHECK_CONTAINS(
        compileError("#smdl\nimport ::df::*;\nstruct MyBSDF: bsdf {};\n")
            .message,
        "Struct \"MyBSDF\" with tag \"bsdf\" declares no \"_lobes\"");
    CHECK_CONTAINS(
        compileError("#smdl\nimport ::df::*;\nstruct MyEDF: edf {};\n").message,
        "to declare it: static const int _lobes = 0;");
  }
  SUBCASE("An implementation of a pure open function must be pure") {
    smdl::Error error{compileError(withBSDF(
        "df::DF_MATTE_BRDF",
        smdl::concat(eval,
                     "auto scatterDraw(const &MyBSDF this [[anno::unused()]], "
                     "const &df::ScatterDrawParams params [[anno::unused()]]) "
                     "= df::ScatterDrawResult();\n")))};
    CHECK_CONTAINS(error.message,
                   "[<string ::diag>:5:1] Function \"scatterDraw\" implements "
                   "the '@(pure)' open function \"scatterDraw\" for "
                   "\"MyBSDF\", so it must be declared '@(pure)' or "
                   "'@(macro)'");
  }
  SUBCASE("A wrong return type is reported at the implementation") {
    smdl::Error error{compileError(withBSDF(
        "df::DF_MATTE_BRDF",
        smdl::concat("@(pure) int scatterEval(const &MyBSDF this "
                     "[[anno::unused()]], const &df::ScatterEvalParams params "
                     "[[anno::unused()]]) = 0;\n",
                     draw, material)))};
    CHECK_CONTAINS(error.message,
                   "[<string ::diag>:4:9] Function \"scatterEval\" returns "
                   "\"int\", where the open function it implements returns "
                   "\"ScatterEvalResult\"");
    CHECK_CONTAINS(error.message,
                   "while generating the \"scatterEval\" entry point of "
                   "material \"::diag::mine\" declared at [<string ::diag>:6:");
  }
  SUBCASE("A material before the implementations it needs names them") {
    smdl::Error error{compileError(
        withBSDF("df::DF_MATTE_BRDF", smdl::concat(material, eval)))};
    CHECK_CONTAINS(error.message, "[<string ::diag>:3:8] Open function "
                                  "\"scatterEval\" has no implementation for "
                                  "\"MyBSDF\"");
    CHECK_CONTAINS(error.message, "declared after the code that needs it: "
                                  "\"scatterEval\" at [<string ::diag>:5:");
    // A struct carrying several tags reaches the open declarations of all of
    // them, which together are no more an implementation than one is.
    smdl::Error errorTwoTags{compileError(smdl::concat(
        "#smdl\nimport ::df::*;\nexport struct MyBSDF: bsdf, vdf { "
        "static const int _lobes = df::DF_MATTE_BRDF; };\n",
        material, eval))};
    CHECK_CONTAINS(errorTwoTags.message, "has no implementation for");
    CHECK_NOT_CONTAINS(errorTwoTags.message, "ambiguous");
  }
  SUBCASE("An error inside an implementation names its material") {
    smdl::Error error{compileError(withBSDF(
        "df::DF_MATTE_BRDF",
        smdl::concat(
            "@(pure) auto scatterEval(const &MyBSDF this "
            "[[anno::unused()]], inline const &df::ScatterEvalParams "
            "params) = df::ScatterEvalResult(f: wo.q, pdf: float2(0));\n",
            draw, material)))};
    CHECK_CONTAINS(error.message, "[<string ::diag>:4:");
    CHECK_CONTAINS(error.message, "No field \"q\"");
    CHECK_CONTAINS(error.message,
                   "while generating the \"scatterEval\" entry point of "
                   "material \"::diag::mine\"");
  }
  SUBCASE("A promise the struct does not keep is warned about") {
    const CollectedLog warned{"Struct \"MyBSDF\""};
    smdl::Compiler compiler{};
    REQUIRE_OK(
        compiler.addCode("::zero", withBSDF("0", smdl::concat(eval, draw))));
    REQUIRE_OK(compiler.addCode(
        "::gloss", withBSDF("df::DF_GLOSS_BRDF", smdl::concat(eval, draw))));
    REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_NONE));
    CHECK(warned.count("declares \"_lobes\" as 0") == 1);
    CHECK(warned.count("declares a GLOSS lobe but implements no "
                       "\"normalDraw\"") == 1);
  }
  SUBCASE("An implementation no dispatch reaches is warned about") {
    const CollectedLog warned{"Function \"scatterEval\" takes"};
    smdl::Compiler compiler{};
    REQUIRE_OK(compiler.addCode(
        "::untagged",
        smdl::concat("#smdl\nimport ::df::*;\nexport struct MyBSDF {};\n",
                     eval)));
    REQUIRE_OK(compiler.addCode(
        "::elsewhere",
        smdl::concat("#smdl\nimport ::df::*;\nimport ::untagged::*;\n",
                     "@(pure) auto scatterEval(const &untagged::MyBSDF this "
                     "[[anno::unused()]], const &df::ScatterEvalParams params "
                     "[[anno::unused()]]) = df::ScatterEvalResult(isBlack: "
                     "true);\n")));
    REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_NONE));
    CHECK(warned.count("which does not carry the tag \"bsdf\"") == 1);
    CHECK(warned.count("did you mean \"struct MyBSDF: bsdf\"?") == 1);
    CHECK(warned.count("which is declared in another module") == 1);
  }
  SUBCASE("Under debug, a draw outside the declared lobes is caught") {
    // A reflection BSDF that draws a transmission, which the host would
    // otherwise be told came from no lobe at all.
    smdl::Compiler compiler{};
    compiler.isDebugEnabled = true;
    compiler.shouldEmitUnitTests = true;
    REQUIRE_OK(compiler.addCode(
        "::diag",
        withBSDF(
            "df::DF_MATTE_BRDF",
            smdl::concat(eval,
                         "@(pure) auto scatterDraw(const &MyBSDF this "
                         "[[anno::unused()]], const &df::ScatterDrawParams "
                         "params [[anno::unused()]]) = df::ScatterDrawResult("
                         "wi: float3(0, 0, -1), mode: df::scatter_transmit);\n",
                         "unit_test \"t\" {\n"
                         "  auto mat = material(surface: "
                         "material_surface(scattering: MyBSDF()));\n"
                         "  auto eval = _MaterialEval(&mat);\n"
                         "  float4 xi = float4(0.5);\n"
                         "  float3 wo = float3(0, 0, 1);\n"
                         "  float3 wi = float3(0);\n"
                         "  float pdf = 0;\n"
                         "  color f = color(0);\n"
                         "  int lobe = 0;\n"
                         "  float lobeChance = 1;\n"
                         "  #print(df::_scatterDraw(&eval, &xi, &wo, &wi, "
                         "&pdf, cast<&float>(&f), &lobe, "
                         "df::DF_ALL, &lobeChance));\n"
                         "}\n"))));
    REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_NONE));
    REQUIRE_OK(compiler.jitCompile());
    StateStorage storage{compiler};
    smdl::State state{storage.makeState()};
    const std::optional<smdl::Error> error{compiler.runUnitTests(state)};
    REQUIRE(error.has_value());
    CHECK_CONTAINS(error->message, "'scatterDraw' drew in a domain the "
                                   "'_lobes' of what it drew from does not "
                                   "declare");
  }
}

TEST_CASE("Compiler: how a refusal is phrased") {
  SUBCASE("An ill-formed operator is not called unimplemented") {
    smdl::Error error{
        compileError("#smdl\nexec { float4x3 a; float4x3 b; auto c = a "
                     "* b; #print(c); }\n")};
    CHECK_CONTAINS(error.message, "No binary operator \"*\"");
    CHECK_CONTAINS(error.message,
                   "columns of the left (4) to match the rows of "
                   "the right (3)");
    CHECK_NOT_CONTAINS(error.message, "unimplemented");
  }
  SUBCASE("A returned value of the wrong type says so") {
    // A function body is only emitted where something reaches it, so the
    // call has to be somewhere that is compiled.
    smdl::Error error{
        compileError("#smdl\n@(pure) int f() { return \"hello\"; }\n"
                     "exec { #print(f()); }\n")};
    CHECK_CONTAINS(error.message,
                   "Cannot convert return value of type \"string\" "
                   "to \"int\"");
  }
  SUBCASE("Assigning to a 'const' says 'const', not 'rvalue'") {
    smdl::Error error{
        compileError("#smdl\nexec { const int i = 1; i = 2; }\n")};
    CHECK_CONTAINS(error.message,
                   "Cannot assign to \"i\" because it is declared "
                   "'const'");
    CHECK_CONTAINS(error.message, "declared at [<string ::diag>:2:");
    CHECK_NOT_CONTAINS(error.message, "rvalue");
  }
  SUBCASE(
      "Assigning to something that is not a variable does not claim const") {
    smdl::Error error{
        compileError("#smdl\nenum E { A, B };\nexec { A = B; }\n")};
    CHECK_CONTAINS(error.message, "because it is not a variable");
  }
  SUBCASE("A missing resource blames the line that asked for it") {
    const CollectedLog warned{"load \"nope.png\": file not found"};
    smdl::Compiler compiler{};
    REQUIRE_OK(compiler.addCode(
        "::diag", "#smdl\nexport material m(uniform texture_2d t = "
                  "texture_2d(\"nope.png\")) = material();\n"));
    REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_NONE));
    REQUIRE(warned.messages().size() >= 1);
    // The load happens inside the builtin 'texture_2d' constructor, whose
    // location the user cannot act on.
    CHECK_NOT_CONTAINS(warned.messages()[0], "<builtin");
    CHECK_CONTAINS(warned.messages()[0], "<string ::diag>:2:");
  }
  SUBCASE("A malformed resource blames the line that asked for it") {
    TempDir tmpDir{"diagnostics-malformed"};
    tmpDir.write("bad.ies", "This is not an IES file!\n");
    const CollectedLog warned{"bad.ies"};
    smdl::Compiler compiler{};
    REQUIRE_OK(
        compiler.addCode("::diag",
                         "#smdl\nexport material m(uniform light_profile p = "
                         "light_profile(\"bad.ies\")) = material();\n",
                         tmpDir.path().string()));
    REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_NONE));
    REQUIRE(warned.messages().size() == 1);
    CHECK_CONTAINS(warned.messages()[0], "Not an IES file");
    CHECK_NOT_CONTAINS(warned.messages()[0], "<builtin");
    CHECK_CONTAINS(warned.messages()[0], "<string ::diag>:2:");
  }
  SUBCASE("A missing resource named through a wrapper blames the caller") {
    // The name is looked for beside the module being compiled, not beside
    // the wrapper, so the caller's line is the one to blame.
    TempDir tmpDir{"diagnostics-wrapper"};
    tmpDir.write("lib/wrap.mdl", "#smdl\n@(macro)\n"
                                 "export bsdf_measurement load(const string "
                                 "name) =\n  bsdf_measurement(name);\n");
    tmpDir.write("user/main.mdl",
                 "#smdl\nimport ::lib::wrap::*;\n"
                 "exec {\n"
                 "  const auto m = ::lib::wrap::load(\"nope.mbsdf\");\n"
                 "  #assert(!bool(m.ptr));\n"
                 "}\n");
    const CollectedLog warned{"nope.mbsdf"};
    smdl::Compiler compiler{};
    REQUIRE_OK(compiler.add(tmpDir.path().string()));
    REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_NONE));
    REQUIRE(warned.messages().size() == 1);
    CHECK_CONTAINS(warned.messages()[0], "main.mdl:4:");
    CHECK_NOT_CONTAINS(warned.messages()[0], "wrap.mdl");
  }
  SUBCASE("A resource name that is not compile-time blames where it went in") {
    // A function parameter is a runtime value, so the name stops being
    // compile-time on the line that passes it to the constructor.
    smdl::Error error{compileError(
        "#smdl\n@(pure)\n"
        "bsdf_measurement load(const string name) = bsdf_measurement(name);\n"
        "exec { const auto m = load(\"x.mbsdf\"); }\n")};
    CHECK_CONTAINS(error.message, "[<string ::diag>:3:");
    CHECK_CONTAINS(error.message,
                   "expects 1 compile-time string argument; a name passed "
                   "through a function is compile-time only if the function "
                   "is '@(macro)' or a struct constructor");
  }
  SUBCASE("An unused value is reported in user code, never in builtin code") {
    const CollectedLog warned{"unused"};
    smdl::Compiler compiler{};
    // The measurement fails to load, and the null pointer it leaves folds
    // away the only use of a variable inside the builtin 'measured_bsdf'.
    REQUIRE_OK(compiler.addCode(
        "::diag", "#smdl\nimport ::df::*;\n"
                  "export material m() = material(surface: material_surface(\n"
                  "  scattering: df::measured_bsdf(\n"
                  "    measurement: bsdf_measurement(\"nope.mbsdf\"))));\n"
                  "exec { int unusedLocal = 1; }\n"));
    REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_NONE));
    REQUIRE(warned.messages().size() == 1);
    CHECK_CONTAINS(warned.messages()[0], "variable \"unusedLocal\"");
  }
}

TEST_CASE("Compiler: where a color and a float3 may convert") {
  SUBCASE("'color' to 'float3' is refused in a pure context") {
    smdl::Error error{compileError(
        "#smdl\nexec { color c = color(1.0); float3 v = c; #print(v); }\n")};
    CHECK_CONTAINS(error.message, "Cannot convert 'color' to \"float3\" in a "
                                  "'@(pure)' context");
    // Naming the internal function the conversion reaches is what this
    // replaced.
    CHECK_NOT_CONTAINS(error.message, "_colorToRGB");
  }
  SUBCASE("'float3' to 'color' is refused in a pure context") {
    smdl::Error error{
        compileError("#smdl\nexec { color c = float3(1.0, 0.5, 0.25); "
                     "#print(c[0]); }\n")};
    CHECK_CONTAINS(error.message, "Cannot convert 'float3' to 'color' in a "
                                  "'@(pure)' context");
    CHECK_NOT_CONTAINS(error.message, "nontrivialRGBToColor");
  }
  SUBCASE("A compile-time grey needs no wavelengths") {
    // It folds to a flat spectrum, so it stays legal where the
    // colorimetric path is not.
    CHECK(compileError("#smdl\nexec { color c = float3(0.5); #print(c[0]); }\n")
              .message == "compiled without error");
  }
  SUBCASE("The conversion is colorimetric where there is state") {
    // A flat spectrum is not RGB white, which is the whole point of the
    // conversion being an integration against the observer.
    CHECK(compileError("#smdl\nexport material m() = material(\n"
                       "  geometry: material_geometry(normal: "
                       "float3(color(1.0))));\n")
              .message == "compiled without error");
  }
}
