/// \file
/// The resources a compile loads and caches: the warning a missing one
/// raises and how often, the mip chain a texture bakes, the images an
/// optimized module drops, and the one symbol per file the JIT defines.
///
/// This is `Compiler`'s own behavior rather than the readers' (those are
/// tested under `Resource/`), so it sits beside the compiler it is about.

#include "CompileFixtures.h"

#include <fstream>
#include <string>

#include "smdl/Compiler.h"
#include "smdl/Resource/Image.h"

TEST_CASE("Compiler: how often a missing resource is reported") {
  TempDir tmpDir{"missing-resource"};
  // A missing texture is a warning, not an error, and the texture reads
  // black -- so the interesting question is how many times it is reported.
  // A material body is emitted three times ('evaluate', 'opacityEvaluate'
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
    auto text{materialUsing("gone_a.png")};
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
    if (auto message{buildAll(compiler, {tmpDir / "mips.smdl"})};
        !message.empty())
      return message;
    StateStorage storage{compiler};
    auto state{storage.makeState()};
    if (auto error{compiler.runUnitTests(state)}) return error->message;
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
  auto ir{std::string()};
  auto build{[&](smdl::OptLevel optLevel) {
    smdl::Compiler compiler{};
    compiler.shouldEmitUnitTests = true;
    if (auto error{compiler.add((tmpDir / "main.smdl").string())})
      return error->message;
    if (auto error{compiler.compile(optLevel)}) return error->message;
    if (auto error{compiler.dump(smdl::DUMP_FORMAT_IR, ir)})
      return error->message;
    if (auto error{compiler.jitCompile()}) return error->message;
    return std::string();
  }};
  SUBCASE("An unread image is dropped at O2 and a sampled one is kept") {
    const CollectedLog dropped{"Dropping image", true};
    CHECK(build(smdl::OPT_LEVEL_O2) == "");
    REQUIRE(dropped.messages().size() == 1);
    CHECK_CONTAINS(dropped.messages()[0], "dead.png");
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
    auto ir{std::string()};
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
