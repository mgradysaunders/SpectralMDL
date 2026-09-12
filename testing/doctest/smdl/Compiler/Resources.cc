/// \file
/// The resources a compile loads and caches: the warning a missing one
/// raises and how often, the curve a spectrum library does not have, what
/// each says in the log and in what order, the mip chain a texture bakes,
/// the images an optimized module drops, and the one symbol per file the
/// JIT defines.
///
/// This is `Compiler`'s own behavior rather than the readers' (those are
/// tested under `Resource/`), so it sits beside the compiler it is about.

#include "CompileFixtures.h"

#include <cstdint>
#include <cstring>
#include <fstream>
#include <string>

#include "smdl/Compiler.h"
#include "smdl/Resource/Image.h"

TEST_CASE("Compiler: how often a missing resource is reported") {
  TempDir tmpDir{"missing-resource"};
  // A missing texture is a warning, not an error, and the texture reads
  // black -- so the interesting question is how many times it is reported.
  // A material body is emitted once for each function 'Type.cc' generates
  // from it, and the not-found path cannot be memoized by file hash the way
  // an actual load failure is, so without 'Compiler::logResourceWarningOnce'
  // it would be reported once per emission.
  auto materialUsing{[](std::string_view fileName) {
    std::string text{std::string("#smdl\nimport ::df::*;\nimport ::tex::*;\n"
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
    tmpDir.write("main.mdl", materialUsing("nowhere.png"));
    const CollectedLog warnings{"nowhere.png"};
    smdl::Compiler compiler{};
    CHECK(buildAll(compiler, {tmpDir / "main.mdl"}).empty());
    CHECK(warnings.warningCount() == 1);
    // The memo is per compile, not per Compiler: recompiling has to report
    // the same missing file again rather than swallow it.
    REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_NONE));
    CHECK(warnings.warningCount() == 2);
  }
  SUBCASE("Distinct missing textures are each reported") {
    std::string text{materialUsing("gone_a.png")};
    text += "export material N() = let {\n"
            "  auto t = texture_2d(\"gone_b.png\", tex::gamma_srgb);\n"
            "  auto c = ::tex::lookup_float3(t, float2(0.5));\n"
            "} in material(surface: material_surface(\n"
            "  scattering: df::diffuse_reflection_bsdf(tint: color(c))));\n";
    tmpDir.write("main.mdl", text);
    // One needle over both, so that this also says nothing else warned.
    const CollectedLog warnings{"gone_"};
    smdl::Compiler compiler{};
    CHECK(buildAll(compiler, {tmpDir / "main.mdl"}).empty());
    CHECK(warnings.count("gone_a.png") == 1);
    CHECK(warnings.count("gone_b.png") == 1);
    CHECK(warnings.messages().size() == 2);
  }
}

TEST_CASE("Compiler: the curve a spectrum library does not have") {
  TempDir tmpDir{"spectrum-library-curve"};
  // Two named curves over two wavelengths, as little-endian floats.
  tmpDir.write("lib.sli.hdr", "ENVI\n"
                              "file type = ENVI Spectral Library\n"
                              "samples = 2\nlines = 2\nbands = 1\n"
                              "header offset = 0\ndata type = 4\n"
                              "byte order = 0\n"
                              "wavelength units = Nanometers\n"
                              "wavelength = {400, 700}\n"
                              "spectra names = {grass, sand}\n");
  std::string data{};
  for (float value : {0.1f, 0.2f, 0.3f, 0.4f}) {
    uint32_t bits{};
    std::memcpy(&bits, &value, sizeof(bits));
    for (int k = 0; k < 4; k++) data += char((bits >> (8 * k)) & 0xFF);
  }
  tmpDir.write("lib.sli", data);
  auto materialUsing{[](std::string_view name, std::string_view curve) {
    return smdl::concat("export material ", name,
                        "() = material(surface: material_surface(\n"
                        "  scattering: df::diffuse_reflection_bsdf(\n"
                        "    tint: color(spectral_curve(\"lib.sli\", ",
                        curve, ")))));\n");
  }};
  tmpDir.write("main.mdl", smdl::concat("#smdl\nimport ::df::*;\n",
                                        materialUsing("byName", "\"grasss\""),
                                        materialUsing("byIndex", "7"),
                                        materialUsing("found", "\"grass\"")));
  const CollectedLog warnings{"has no curve"};
  smdl::Compiler compiler{};
  CHECK(buildAll(compiler, {tmpDir / "main.mdl"}).empty());
  // Once each, however many times the material bodies were emitted, and
  // at the line in the user's module rather than in the builtin that did
  // the lookup.
  REQUIRE(warnings.messages().size() == 2);
  CHECK(warnings.count("main.mdl:5:") == 1);
  CHECK(warnings.count("main.mdl:8:") == 1);
  CHECK(warnings.count("<builtin") == 0);
  CHECK(warnings.count("has no curve named \"grasss\"; did you mean "
                       "\"grass\"?") == 1);
  CHECK(warnings.count("has no curve at index 7 (it has 2 curves)") == 1);
}

TEST_CASE("Compiler: the mip levels a texture bakes") {
  TempDir tmpDir{"compiler-mipmap"};
  // A 4x4 image, whose chain is 4x4 -> 2x2 -> 1x1, so a texture that
  // reads the chain bakes 3 levels and one that does not bakes 1. The
  // level count is the thing to pin: it is what keeps JIT code from
  // walking mip levels that were never generated.
  const uint8_t texels[16] = {0,   16,  32,  48,  //
                              64,  80,  96,  112, //
                              128, 144, 160, 176, //
                              192, 208, 224, 240};
  REQUIRE(
      !smdl::write8bitImage((tmpDir / "mip.png").string(), 4, 4, 1, texels));
  // Build and run a module asserting the baked level count of a texture
  // constructed with the given extra arguments. Whether the '#assert'
  // folds at compile time or runs in the JIT, a mismatch comes back as a
  // message here.
  auto checkNumLevels{[&](std::string_view args, int numLevels) {
    tmpDir.write("mips.smdl", "#smdl\nimport ::tex::*;\n"
                              "unit_test \"Baked level count\" {\n"
                              "  const auto t = texture_2d(\"mip.png\"" +
                                  std::string(args) +
                                  ");\n"
                                  "  #assert(t.num_levels == " +
                                  std::to_string(numLevels) + ");\n}\n");
    smdl::Compiler compiler{};
    compiler.shouldEmitUnitTests = true;
    if (std::string message{buildAll(compiler, {tmpDir / "mips.smdl"})};
        !message.empty())
      return message;
    StateStorage storage{compiler};
    smdl::State state{storage.makeState()};
    if (std::optional<smdl::Error> error{compiler.runUnitTests(state)})
      return error->message;
    return std::string();
  }};
  SUBCASE("'use_mipmap: true' bakes the whole chain") {
    CHECK(checkNumLevels(", tex::gamma_linear, use_mipmap: true", 3) == "");
  }
  SUBCASE("Mip filtering is opt in") {
    CHECK(checkNumLevels(", tex::gamma_linear", 1) == "");
    // And the level count really is what is asserted, rather than the
    // check above passing for some unrelated reason.
    CHECK(checkNumLevels(", tex::gamma_linear", 3) != "");
  }
}

TEST_CASE("Compiler: what a resource says at debug level") {
  TempDir tmpDir{"resource-debug"};
  const uint8_t gray[4] = {0, 64, 128, 255};
  const uint8_t rgba[16] = {};
  REQUIRE(!smdl::write8bitImage((tmpDir / "gray.png").string(), 2, 2, 1, gray));
  tmpDir.write("curve.txt", "nanometers\n400 0.1\n500 0.5\n600 0.9\n");
  // A material that samples `fileName`, so that the image survives to be
  // decoded, tinted by the curve in 'curve.txt' on line 9.
  auto materialUsing{[](std::string_view fileName) {
    return smdl::concat(
        "#smdl\nimport ::df::*;\nimport ::tex::*;\n"
        "export material M() = let {\n"
        "  auto t = texture_2d(\"",
        fileName,
        "\", tex::gamma_linear);\n"
        "  auto c = ::tex::lookup_float3(t, float2(0.5));\n"
        "} in material(surface: material_surface(\n"
        "  scattering: df::diffuse_reflection_bsdf(\n"
        "    tint: color(c) * color(spectral_curve(\"curve.txt\")))));\n");
  }};
  SUBCASE("A loaded resource says what it is, and an image once decoded") {
    tmpDir.write("main.smdl", materialUsing("gray.png"));
    const CollectedLog logged{"Loaded", /*shouldCollectDebug=*/true};
    smdl::Compiler compiler{};
    CHECK(buildAll(compiler, {tmpDir / "main.smdl"}).empty());
    // Once each, however many times the material body was emitted, and
    // then the summary of the images decoded.
    REQUIRE(logged.messages().size() == 3);
    CHECK(logged.count("main.smdl:9:") == 1);
    CHECK(logged.count("curve.txt\": 3 samples from 400 to 600 nm") == 1);
    CHECK(logged.count("Loaded image \"") == 1);
    CHECK(logged.count("gray.png\": 2 x 2, 1-channel uint8, 4 B") == 1);
    CHECK_CONTAINS(logged.messages()[2], "Loaded 1 image (4 B) in ");
  }
  SUBCASE("Decoded images are logged in file name order, then summed up") {
    // Enough images that the order of the cache, which is keyed by
    // pointer, is almost never the sorted one by chance.
    std::string declarations{};
    std::string sum{"float3(0)"};
    for (char letter = 'a'; letter <= 'h'; letter++) {
      const std::string name(1, letter);
      const uint8_t texels[4] = {uint8_t(letter), 0, 0, 0};
      REQUIRE(!smdl::write8bitImage((tmpDir / (name + ".png")).string(), 2, 2,
                                    1, texels));
      declarations +=
          smdl::concat("  auto ", name, " = ::tex::lookup_float3(texture_2d(\"",
                       name, ".png\"), float2(0.5));\n");
      sum += smdl::concat(" + ", name);
    }
    tmpDir.write("main.smdl",
                 smdl::concat("#smdl\nimport ::df::*;\nimport ::tex::*;\n"
                              "export material M() = let {\n",
                              declarations,
                              "} in material(surface: material_surface(\n"
                              "  scattering: df::diffuse_reflection_bsdf(\n"
                              "    tint: color(",
                              sum, "))));\n"));
    const CollectedLog logged{"Loaded", /*shouldCollectDebug=*/true};
    smdl::Compiler compiler{};
    CHECK(buildAll(compiler, {tmpDir / "main.smdl"}).empty());
    REQUIRE(logged.messages().size() == 9);
    for (size_t i = 0; i < 8; i++)
      CHECK_CONTAINS(logged.messages()[i],
                     std::string(1, char('a' + i)) + ".png\": 2 x 2");
    CHECK_CONTAINS(logged.messages()[8], "Loaded 8 images (32 B) in ");
  }
  SUBCASE("A file not found is followed by where it was looked for") {
    tmpDir.write("main.smdl", materialUsing("nowhere.png"));
    const CollectedLog logged{"nowhere.png", /*shouldCollectDebug=*/true};
    smdl::Compiler compiler{};
    compiler.fileLocator.setSearchPwd(false);
    compiler.fileLocator.setSearchDefaultDirs(false);
    CHECK(buildAll(compiler, {tmpDir / "main.smdl"}).empty());
    // The warning, then the one directory searched, once each.
    REQUIRE(logged.messages().size() == 2);
    CHECK(logged.warningCount() == 1);
    CHECK_CONTAINS(logged.messages()[1],
                   "Searched 1 directory for \"nowhere.png\":\n  \"");
    CHECK_CONTAINS(logged.messages()[1], "smdl-test-resource-debug\"");
  }
  SUBCASE("Tiles that disagree on their format say which and how") {
    REQUIRE(!smdl::write8bitImage((tmpDir / "tile_1001.png").string(), 2, 2, 1,
                                  gray));
    REQUIRE(!smdl::write8bitImage((tmpDir / "tile_1002.png").string(), 2, 2, 4,
                                  rgba));
    tmpDir.write("main.smdl", materialUsing("tile_<UDIM>.png"));
    const CollectedLog logged{"image formats for"};
    smdl::Compiler compiler{};
    CHECK(buildAll(compiler, {tmpDir / "main.smdl"}).empty());
    REQUIRE(logged.messages().size() == 1);
    CHECK_CONTAINS(
        logged.messages()[0],
        "Inconsistent image formats for \"tile_<UDIM>.png\": "
        "\"tile_1001.png\" is 1-channel uint8, but \"tile_1002.png\" "
        "is 4-channel uint8");
  }
}

TEST_CASE("Compiler: dropping an image nothing reads") {
  TempDir tmpDir{"unused-image"};
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
  tmpDir.write(
      "main.smdl",
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
  std::string ir{};
  auto build{[&](smdl::OptLevel optLevel) {
    smdl::Compiler compiler{};
    compiler.shouldEmitUnitTests = true;
    if (std::optional<smdl::Error> error{
            compiler.add((tmpDir / "main.smdl").string())})
      return error->message;
    if (std::optional<smdl::Error> error{compiler.compile(optLevel)})
      return error->message;
    if (std::optional<smdl::Error> error{
            compiler.dump(smdl::DUMP_FORMAT_IR, ir)})
      return error->message;
    if (std::optional<smdl::Error> error{compiler.jitCompile()})
      return error->message;
    return std::string();
  }};
  SUBCASE("An unread image is dropped at O2 and a sampled one is kept") {
    const CollectedLog logged{"image", true};
    CHECK(build(smdl::OPT_LEVEL_O2) == "");
    REQUIRE(logged.count("Dropping image") == 1);
    CHECK(logged.count("dead.png\": never read by the compiled code") == 1);
    // The summary of the one image decoded counts the drop too.
    CHECK(logged.count("Loaded 1 image (") == 1);
    CHECK(logged.count(", skipping 1 that the compiled code never reads") == 1);
    // The declaration goes with the image, so the JIT is never asked to
    // define a symbol for texels that were never loaded.
    CHECK(countImageSymbols(ir) == 1);
  }
  SUBCASE("The unread image is dropped even at OPT_LEVEL_NONE") {
    // Constant-field elimination bakes the 'texture_2d' struct into the
    // type, so the dead image's texel pointers never enter the IR at
    // all: the image is provably unused with no optimization running.
    // The size requirement doubles as the guard that the sampled image
    // is never dropped.
    const CollectedLog dropped{"Dropping image", true};
    CHECK(build(smdl::OPT_LEVEL_NONE) == "");
    REQUIRE(dropped.messages().size() == 1);
    CHECK_CONTAINS(dropped.messages()[0], "dead.png");
    CHECK(countImageSymbols(ir) == 1);
  }
}

TEST_CASE("Compiler: one image symbol per file") {
  TempDir tmpDir{"image-symbol"};
  // 4x4 so the chain is 4x4 -> 2x2 -> 1x1, and distinct content so the
  // two files cannot content-hash to one image.
  const uint8_t texelsA[16] = {0,   16,  32,  48,  //
                               64,  80,  96,  112, //
                               128, 144, 160, 176, //
                               192, 208, 224, 240};
  const uint8_t texelsB[16] = {1, 2,  3,  4,  5,  6,  7,  8,
                               9, 10, 11, 12, 13, 14, 15, 16};
  REQUIRE(!smdl::write8bitImage((tmpDir / "a.png").string(), 4, 4, 1, texelsA));
  REQUIRE(!smdl::write8bitImage((tmpDir / "b.png").string(), 4, 4, 1, texelsB));
  SUBCASE("One symbol per file, however many textures read it") {
    // Two textures over 'a.png' differing in exactly the thing that is
    // per-texture rather than per-image: whether they read the chain.
    tmpDir.write(
        "main.smdl",
        "#smdl\nimport ::tex::*;\n"
        "export const auto a0 = texture_2d(\"a.png\", tex::gamma_linear);\n"
        "export const auto a1 = texture_2d(\"a.png\", tex::gamma_linear, "
        "use_mipmap: true);\n"
        "export const auto b = texture_2d(\"b.png\", tex::gamma_linear);\n"
        "export exec {\n"
        "  #assert(tex::texel_float(a0, int2(1, 2)) == "
        "tex::texel_float(a1, int2(1, 2)));\n"
        "  #assert(a0.num_levels == 1);\n"
        "  #assert(a1.num_levels == 3);\n"
        "  #assert(tex::texel_float(b, int2(0, 0)) != "
        "tex::texel_float(a0, int2(0, 0)));\n"
        "}\n");
    smdl::Compiler compiler{};
    REQUIRE_OK(compiler.add((tmpDir / "main.smdl").string()));
    REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_O2));
    std::string ir{};
    REQUIRE_OK(compiler.dump(smdl::DUMP_FORMAT_IR, ir));
    // Two files, so two symbols: the level count is baked per texture,
    // but the texels are shared and named once.
    CHECK(countImageSymbols(ir) == 2);
    // And they resolve, which is the whole point of naming them.
    REQUIRE_OK(compiler.jitCompile());
    CHECK_OK(compiler.runExecs());
  }
  SUBCASE("The chain reads correctly through the symbol") {
    // Level 0 is named; the higher levels are found by offsetting from
    // it, so a wrong base or a short allocation shows up here. The mean
    // of each 2x2 block of 'texelsA' gives level 1 as 40, 72, 168, 200
    // in file order, and level 2 as their mean, 120. Texture space is
    // v-up, so 'int2(0, 0)' of level 1 is the last row in file order.
    tmpDir.write(
        "mips.smdl",
        "#smdl\nimport ::tex::*;\n"
        "export const auto t = texture_2d(\"a.png\", tex::gamma_linear, "
        "use_mipmap: true);\n"
        "export exec {\n"
        "  #assert(#abs(tex::level_texel_float4(t, 1, int2(0, 0)).x - "
        "168.0 / 255.0) < 1e-6);\n"
        "  #assert(#abs(tex::level_texel_float4(t, 1, int2(0, 1)).x - "
        "40.0 / 255.0) < 1e-6);\n"
        "  #assert(#abs(tex::level_texel_float4(t, 2, int2(0, 0)).x - "
        "120.0 / 255.0) < 1e-6);\n"
        "}\n");
    smdl::Compiler compiler{};
    REQUIRE_OK(compiler.add((tmpDir / "mips.smdl").string()));
    REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_O2));
    REQUIRE_OK(compiler.jitCompile());
    CHECK_OK(compiler.runExecs());
  }
  SUBCASE("An unloadable image still links") {
    // 'startLoad()' fails, so there are no texels and the symbol
    // resolves to a null address. The extent is zero, which is what
    // keeps the lookups away from it, but the link must still succeed.
    std::ofstream(tmpDir / "bad.png") << "This is not an image!\n";
    tmpDir.write("bad.smdl",
                 "#smdl\nimport ::tex::*;\n"
                 "export const auto t = texture_2d(\"bad.png\", "
                 "tex::gamma_linear);\n"
                 "export exec {\n"
                 "  #assert(tex::width(t) == 0);\n"
                 "  #assert(tex::texel_float(t, int2(0, 0)) == 0.0);\n"
                 "}\n");
    smdl::Compiler compiler{};
    REQUIRE_OK(compiler.add((tmpDir / "bad.smdl").string()));
    // 'OPT_LEVEL_NONE' so the reads survive to the JIT rather than being
    // folded away along with the image.
    REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_NONE));
    REQUIRE_OK(compiler.jitCompile());
    CHECK_OK(compiler.runExecs());
  }
}
