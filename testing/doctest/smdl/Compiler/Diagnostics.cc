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
    CHECK_CONTAINS(error.message, "no parameter named 'alhpa'; did you mean "
                                  "'alpha'?");
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
    CHECK_CONTAINS(logged.messages()[0], "] New material '::diag::m'");
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
    CHECK_CONTAINS(error->message, "assertion failed: i == 2");
    CHECK_CONTAINS(error->snippet, "#assert(i == 2);");
  }
}

TEST_CASE("Compiler: the name a misspelling is corrected to") {
  auto errorFor{[&](std::string body) {
    return compileError("#smdl\nexec {\n" + std::move(body) + "\n}\n").message;
  }};
  SUBCASE("The C spelling of an intrinsic suggests the intrinsic") {
    CHECK_CONTAINS(errorFor(" assert(1 == 1);"), "did you mean '#assert'?");
    CHECK_CONTAINS(errorFor(" printf(\"hi\");"), "did you mean '#print'?");
    // Wins over 'size_t', which is a keyword two edits away, because the
    // candidates are weighed together instead of list by list.
    CHECK_CONTAINS(errorFor(" #print(sizeof(int));"),
                   "did you mean '#sizeOf'?");
  }
  SUBCASE("A word-shaped literal is suggested") {
    CHECK_CONTAINS(errorFor(" bool b = True; #print(b);"),
                   "did you mean 'true'?");
  }
  SUBCASE("A bare 'state' suggests '$state'") {
    CHECK_CONTAINS(errorFor(" #print(state);"), "did you mean '$state'?");
  }
  SUBCASE("A qualified name suggests within its module") {
    smdl::Error error{compileError("#smdl\nimport ::math::*;\n"
                                   "exec { #print(math::sqr(4.0)); }\n")};
    CHECK_CONTAINS(error.message, "did you mean 'math::sqrt'?");
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
    CHECK_CONTAINS(error.message, "'math' is imported but not opened");
    CHECK_CONTAINS(error.message, "'math::sqrt'");
    CHECK_CONTAINS(error.message, "using ::math import sqrt;");
  }
  SUBCASE("A misspelled module suggests the builtin") {
    smdl::Error error{
        compileError("#smdl\nimport ::maths::*;\nexec { #print(1); }\n")};
    CHECK_CONTAINS(error.message, "did you mean '::math'?");
  }
  SUBCASE("A misspelled imported name suggests within the module") {
    smdl::Error error{compileError("#smdl\nusing ::math import sqrtt;\n"
                                   "exec { #print(1); }\n")};
    CHECK_CONTAINS(error.message, "did you mean 'sqrt'?");
  }
  SUBCASE("A misspelled field suggests the field") {
    smdl::Error error{compileError("#smdl\nstruct S { int alpha = 1; };\n"
                                   "exec { #print(S().alhpa); }\n")};
    CHECK_CONTAINS(error.message, "did you mean 'alpha'?");
  }
  SUBCASE("A swizzle off the end lists the components") {
    smdl::Error error{compileError("#smdl\nexec { float2 v; #print(v.z); }\n")};
    CHECK_CONTAINS(error.message, "the components are x, y (or r, g)");
  }
  SUBCASE("'=' where ':' was meant names the mistake") {
    smdl::Error error{
        compileError("#smdl\nexport material m() = material(ior = 1.5);\n")};
    CHECK_CONTAINS(error.message, "a named argument is written 'ior: ...', not "
                                  "'ior = ...'");
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
                   "Cannot call 'microfacet_ggx_smith_bsdf' with "
                   "arguments '(roughness: float)'");
    CHECK_CONTAINS(error.message, "forwards to:");
    CHECK_CONTAINS(error.message, "did you mean 'roughness_u'?");
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
}

TEST_CASE("Compiler: how a refusal is phrased") {
  SUBCASE("An ill-formed operator is not called unimplemented") {
    smdl::Error error{
        compileError("#smdl\nexec { float4x3 a; float4x3 b; auto c = a "
                     "* b; #print(c); }\n")};
    CHECK_CONTAINS(error.message, "No binary operator '*'");
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
                   "Cannot convert return value of type 'string' "
                   "to 'int'");
  }
  SUBCASE("Assigning to a 'const' says 'const', not 'rvalue'") {
    smdl::Error error{
        compileError("#smdl\nexec { const int i = 1; i = 2; }\n")};
    CHECK_CONTAINS(error.message, "cannot assign to 'i' because it is declared "
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
    const CollectedLog warned{"load 'nope.png': file not found"};
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
    CHECK_CONTAINS(warned.messages()[0], "not an IES file");
    CHECK_NOT_CONTAINS(warned.messages()[0], "<builtin");
    CHECK_CONTAINS(warned.messages()[0], "<string ::diag>:2:");
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
    CHECK_CONTAINS(warned.messages()[0], "variable 'unusedLocal'");
  }
}

TEST_CASE("Compiler: where a color and a float3 may convert") {
  SUBCASE("'color' to 'float3' is refused in a pure context") {
    smdl::Error error{compileError(
        "#smdl\nexec { color c = color(1.0); float3 v = c; #print(v); }\n")};
    CHECK_CONTAINS(error.message, "Cannot convert 'color' to 'float3' in a "
                                  "'@(pure)' context");
    // Naming the internal function the conversion reaches is what this
    // replaced.
    CHECK_NOT_CONTAINS(error.message, "_colorToRgb");
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
