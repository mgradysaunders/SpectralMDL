#include "doctest.h"

#include <filesystem>
#include <fstream>
#include <string>

#include "smdl/RenderUtil/PBRMaps.h"

namespace fs = std::filesystem;

using Manifest = smdl::PBRMaps;

// Parse and require success.
static Manifest parseOK(const std::string &sourceCode) {
  Manifest manifest{};
  auto error{manifest.parse(sourceCode)};
  if (error) MESSAGE(error->message);
  REQUIRE(!error);
  return manifest;
}

// Parse, require failure, and require the message to name the line and
// contain the expected fragment.
static void parseFail(const std::string &sourceCode, int lineNo,
                      const std::string &fragment) {
  Manifest manifest{};
  auto error{manifest.parse(sourceCode)};
  REQUIRE(error);
  CAPTURE(error->message);
  CHECK(error->message.find(fragment) != std::string::npos);
  CHECK(error->message.find("line " + std::to_string(lineNo) + ":") !=
        std::string::npos);
}

TEST_CASE("PBRMaps") {
  SUBCASE("Complete manifest") {
    auto manifest{parseOK(R"(# forest_ground_06.pbr
pbr: 1
name: Forest ground 06
description: "Photoscanned forest floor, PolyHaven."

physical_extent: [1.5, 2.5]  # Meters per repeat
physical_relief: 0.02        # Meters
hex_rotation: 0.5

basecolor: forest_ground_06_diff_4k.jpg
normal:
  file: forest_ground_06_nor_gl_4k.png
  convention: directx
roughness:
  file: forest_ground_06_arm_4k.png
  channel: g
metallic:
  file: forest_ground_06_arm_4k.png
  channel: b
height: forest_ground_06_disp_4k.png
horizon:
  file: forest_ground_06_hrz_4k.png
  max_slope: 4
class: forest_ground_06_cls_4k.png
)")};
    CHECK(manifest.version == 1);
    CHECK(manifest.name == "Forest ground 06");
    CHECK(manifest.description == "Photoscanned forest floor, PolyHaven.");
    REQUIRE(manifest.physicalExtent.has_value());
    CHECK((*manifest.physicalExtent)[0] == doctest::Approx(1.5f));
    CHECK((*manifest.physicalExtent)[1] == doctest::Approx(2.5f));
    REQUIRE(manifest.physicalRelief.has_value());
    CHECK(*manifest.physicalRelief == doctest::Approx(0.02f));
    CHECK(!manifest.reliefScale);
    REQUIRE(manifest.effectiveReliefScale().has_value());
    CHECK(*manifest.effectiveReliefScale() == doctest::Approx(0.02f / 1.5f));
    CHECK(manifest.warnings.empty());
    CHECK(manifest.hexRotation == doctest::Approx(0.5f));
    CHECK(bool(manifest[Manifest::ROLE_BASECOLOR]));
    CHECK(manifest[Manifest::ROLE_BASECOLOR].file ==
          "forest_ground_06_diff_4k.jpg");
    CHECK(manifest[Manifest::ROLE_NORMAL].normalConvention ==
          Manifest::NORMAL_CONVENTION_DIRECTX);
    CHECK(manifest[Manifest::ROLE_ROUGHNESS].channel == Manifest::CHANNEL_G);
    CHECK(manifest[Manifest::ROLE_METALLIC].channel == Manifest::CHANNEL_B);
    CHECK(manifest[Manifest::ROLE_ROUGHNESS].file ==
          manifest[Manifest::ROLE_METALLIC].file);
    CHECK(bool(manifest[Manifest::ROLE_HEIGHT]));
    CHECK(!bool(manifest[Manifest::ROLE_EMISSION]));
    CHECK(bool(manifest[Manifest::ROLE_HORIZON]));
    CHECK(manifest[Manifest::ROLE_HORIZON].maxSlope == doctest::Approx(4.0f));
    CHECK(manifest[Manifest::ROLE_HORIZON].effectiveColorSpace() ==
          Manifest::COLOR_SPACE_LINEAR);
    CHECK(bool(manifest[Manifest::ROLE_CLASS]));
    CHECK(manifest[Manifest::ROLE_CLASS].effectiveColorSpace() ==
          Manifest::COLOR_SPACE_LINEAR);
  }
  SUBCASE("Normal conventions") {
    auto conventionOf{[](const std::string &value) {
      auto manifest{
          parseOK("normal:\n  file: n.png\n  convention: " + value + "\n")};
      return manifest[Manifest::ROLE_NORMAL].normalConvention;
    }};
    CHECK(conventionOf("opengl") == Manifest::NORMAL_CONVENTION_OPENGL);
    CHECK(conventionOf("directx") == Manifest::NORMAL_CONVENTION_DIRECTX);
    CHECK(conventionOf("opengl_xy") == Manifest::NORMAL_CONVENTION_OPENGL_XY);
    CHECK(conventionOf("directx_xy") == Manifest::NORMAL_CONVENTION_DIRECTX_XY);
  }
  SUBCASE("Minimal manifest") {
    auto manifest{parseOK("basecolor: a.png\n")};
    CHECK(manifest[Manifest::ROLE_BASECOLOR].file == "a.png");
    CHECK(!manifest.physicalExtent);
    CHECK(!manifest.physicalRelief);
    CHECK(!manifest.reliefScale);
    CHECK(!manifest.effectiveReliefScale());
    CHECK(manifest.hexRotation == 0.0f);
    for (int i = 0; i < Manifest::ROLE_COUNT; i++)
      if (i != Manifest::ROLE_BASECOLOR)
        CHECK(!bool(manifest[Manifest::Role(i)]));
  }
  SUBCASE("Relief scale") {
    // Declared directly: the bespoke case, with no physical metadata at
    // all because a one-off asset has no meaningful extent to measure.
    {
      auto manifest{parseOK("relief_scale: 0.031\nheight: h.png\n")};
      REQUIRE(manifest.reliefScale.has_value());
      CHECK(*manifest.reliefScale == doctest::Approx(0.031f));
      CHECK(!manifest.physicalExtent);
      CHECK(!manifest.physicalRelief);
      REQUIRE(manifest.effectiveReliefScale().has_value());
      CHECK(*manifest.effectiveReliefScale() == doctest::Approx(0.031f));
      CHECK(manifest.warnings.empty());
    }
    // Derived from the physical pair, against the U extent.
    {
      auto manifest{
          parseOK("physical_extent: [1.5, 2.5]\nphysical_relief: 0.03\n")};
      CHECK(!manifest.reliefScale);
      REQUIRE(manifest.effectiveReliefScale().has_value());
      CHECK(*manifest.effectiveReliefScale() == doctest::Approx(0.03f / 1.5f));
    }
    // Both spellings: the direct one wins, and the manifest says so
    // rather than failing, blaming the overriding line and naming the
    // overridden one.
    {
      auto manifest{parseOK("physical_extent: [2, 2]\n"
                            "physical_relief: 0.5\n"
                            "relief_scale: 0.1\n")};
      CHECK(*manifest.effectiveReliefScale() == doctest::Approx(0.1f));
      REQUIRE(manifest.warnings.size() == 1);
      CAPTURE(manifest.warnings[0]);
      CHECK(manifest.warnings[0].find("line 3:") != std::string::npos);
      CHECK(manifest.warnings[0].find(
                "'relief_scale' overrides 'physical_relief' (line 2)") !=
            std::string::npos);
    }
    // An extent without a relief overrides nothing, so it is silent.
    {
      auto manifest{parseOK("physical_extent: [2, 2]\nrelief_scale: 0.1\n")};
      CHECK(*manifest.effectiveReliefScale() == doctest::Approx(0.1f));
      CHECK(manifest.warnings.empty());
    }
    // A horizon map needs a relief scale, from either spelling.
    {
      auto manifest{parseOK("relief_scale: 0.25\nhorizon: h.png\n")};
      CHECK(bool(manifest[Manifest::ROLE_HORIZON]));
      CHECK(*manifest.effectiveReliefScale() == doctest::Approx(0.25f));
    }
    // Reparsing must not inherit the previous parse's warnings.
    {
      Manifest manifest{};
      REQUIRE(!manifest.parse("physical_relief: 0.5\nrelief_scale: 0.1\n"));
      REQUIRE(manifest.warnings.size() == 1);
      REQUIRE(!manifest.parse("relief_scale: 0.1\n"));
      CHECK(manifest.warnings.empty());
      CHECK(!manifest.physicalRelief);
    }
  }
  SUBCASE("Scalar and block forms agree") {
    auto scalarForm{parseOK("roughness: r.png\n")};
    auto blockForm{parseOK("roughness:\n  file: r.png\n")};
    CHECK(scalarForm[Manifest::ROLE_ROUGHNESS].file ==
          blockForm[Manifest::ROLE_ROUGHNESS].file);
  }
  SUBCASE("Role defaults and overrides") {
    auto manifest{parseOK(R"(basecolor: c.png
emission: e.png
roughness:
  file: r.png
height:
  file: h.png
  color_space: srgb
)")};
    // Color spaces resolve sRGB for color/emission, linear otherwise,
    // unless overridden.
    CHECK(manifest[Manifest::ROLE_BASECOLOR].effectiveColorSpace() ==
          Manifest::COLOR_SPACE_SRGB);
    CHECK(manifest[Manifest::ROLE_EMISSION].effectiveColorSpace() ==
          Manifest::COLOR_SPACE_SRGB);
    CHECK(manifest[Manifest::ROLE_ROUGHNESS].effectiveColorSpace() ==
          Manifest::COLOR_SPACE_LINEAR);
    CHECK(manifest[Manifest::ROLE_HEIGHT].effectiveColorSpace() ==
          Manifest::COLOR_SPACE_SRGB);
  }
  SUBCASE("Emission factor and roughness range") {
    auto manifest{parseOK(R"(emission:
  file: e.png
  factor: 2.5
roughness:
  file: r.png
  range: [0.1, 0.2]
metallic:
  file: m.png
  range: [1, 0]
basecolor: c.png
)")};
    CHECK(manifest[Manifest::ROLE_EMISSION].factor == doctest::Approx(2.5f));
    CHECK(manifest[Manifest::ROLE_ROUGHNESS].range[0] == doctest::Approx(0.1f));
    CHECK(manifest[Manifest::ROLE_ROUGHNESS].range[1] == doctest::Approx(0.2f));
    // Inverted, e.g. a gloss map read as roughness.
    CHECK(manifest[Manifest::ROLE_METALLIC].range[0] == doctest::Approx(1.0f));
    CHECK(manifest[Manifest::ROLE_METALLIC].range[1] == doctest::Approx(0.0f));
    // Undeclared maps keep the identity defaults.
    CHECK(manifest[Manifest::ROLE_BASECOLOR].factor == doctest::Approx(1.0f));
    CHECK(manifest[Manifest::ROLE_BASECOLOR].range[0] == doctest::Approx(0.0f));
    CHECK(manifest[Manifest::ROLE_BASECOLOR].range[1] == doctest::Approx(1.0f));
    // Zero is allowed: emission present but switched off, which
    // tool-generated manifests express without dropping the map.
    auto zeroFactor{parseOK("emission:\n  file: e.png\n  factor: 0\n")};
    CHECK(zeroFactor[Manifest::ROLE_EMISSION].factor == doctest::Approx(0.0f));
  }
  SUBCASE("Comments, blank lines, CRLF, and quotes") {
    auto manifest{parseOK("# leading comment\r\n"
                          "\r\n"
                          "name: \"He said \\\"hi\\\" # not a comment\"\r\n"
                          "basecolor: a.png # trailing comment\r\n"
                          "normal:\r\n"
                          "  # comment inside a block\r\n"
                          "  file: n.png\r\n")};
    CHECK(manifest.name == "He said \"hi\" # not a comment");
    CHECK(manifest[Manifest::ROLE_BASECOLOR].file == "a.png");
    CHECK(manifest[Manifest::ROLE_NORMAL].file == "n.png");
  }
  SUBCASE("Subdirectory filenames are allowed") {
    auto manifest{parseOK("basecolor: maps/albedo.png\n")};
    CHECK(manifest[Manifest::ROLE_BASECOLOR].file == "maps/albedo.png");
  }
  SUBCASE("Map line numbers are recorded") {
    auto manifest{
        parseOK("name: X\nbasecolor: a.png\nnormal:\n  file: n.png\n")};
    CHECK(manifest[Manifest::ROLE_BASECOLOR].lineNo == 2);
    CHECK(manifest[Manifest::ROLE_NORMAL].lineNo == 3);
  }
  SUBCASE("Clear on reparse") {
    Manifest manifest{};
    REQUIRE(!manifest.parse("basecolor: a.png\nhex_rotation: 1.0\n"));
    REQUIRE(!manifest.parse("normal: n.png\n"));
    CHECK(!bool(manifest[Manifest::ROLE_BASECOLOR]));
    CHECK(manifest.hexRotation == 0.0f);
  }
  SUBCASE("Errors") {
    // Structure
    parseFail("colour: a.png\n", 1, "unknown key 'colour'");
    parseFail("basecolor: a.png\nbasecolor: b.png\n", 2, "duplicate key");
    parseFail("just some text\n", 1, "expected 'key: value'");
    parseFail("basecolor:a.png\n", 1, "expected a space after ':'");
    parseFail("\tbasecolor: a.png\n", 1, "tab in indentation");
    parseFail("name: x\n  stray: y\n", 2, "unexpected indentation");
    parseFail("name:\n", 1, "expected a value for 'name'");
    parseFail("basecolor:\n", 1, "expected a filename or an indented block");
    parseFail("normal:\n  file: n.png\n   over: x\n", 3,
              "inconsistent indentation");
    parseFail("normal:\n  convention: opengl_xy\n", 1, "missing 'file'");
    parseFail("normal:\n  file: n.png\n  file: m.png\n", 3, "duplicate key");
    // Strings
    parseFail("name: \"unterminated\n", 1, "unterminated string");
    parseFail("name: \"x\" y\n", 1, "unexpected text after string");
    parseFail("name: \"bad \\n escape\"\n", 1, "invalid escape");
    // Numbers and booleans
    parseFail("pbr: 2\n", 1, "unsupported manifest version 2");
    parseFail("pbr: x\n", 1, "expected an integer");
    parseFail("physical_relief: soon\n", 1, "expected a real number");
    parseFail("physical_relief: 0\n", 1, "expected a positive real");
    parseFail("physical_relief: -0.5\n", 1, "expected a positive real");
    parseFail("relief_scale: nope\n", 1, "expected a real number");
    parseFail("relief_scale: 0\n", 1,
              "expected a positive real for 'relief_scale'");
    parseFail("relief_scale: -0.5\n", 1,
              "expected a positive real for 'relief_scale'");
    parseFail("hex_rotation: 1.5\n", 1, "expected a real in [0, 1]");
    // 'invert', roughness 'gloss', and the 'opacity' role were removed
    // from the schema on purpose; all must now be rejected outright.
    parseFail("opacity: o.png\n", 1, "unknown key 'opacity'");
    parseFail("roughness:\n  file: r.png\n  invert: true\n", 3,
              "unknown key 'invert'");
    parseFail("roughness:\n  file: r.png\n  convention: gloss\n", 3,
              "'convention' is not allowed for 'roughness'");
    // Lists
    parseFail("physical_extent: 1.5\n", 1, "expected an inline list");
    parseFail("physical_extent: [1.5]\n", 1, "expected two positive reals");
    parseFail("physical_extent: [1.5, 2.5, 3.5]\n", 1,
              "expected two positive reals");
    parseFail("physical_extent: [1.5, -2.5]\n", 1,
              "expected two positive reals");
    parseFail("physical_extent: [1.5, , 2.5]\n", 1, "empty list item");
    parseFail("physical_extent: [[1.5], 2.5]\n", 1,
              "nested lists are not supported");
    // Filenames
    parseFail("basecolor: /abs/a.png\n", 1, "must be relative");
    parseFail("basecolor: ~/a.png\n", 1, "must be relative");
    parseFail("basecolor: C:/a.png\n", 1, "must be relative");
    parseFail("basecolor: maps\\a.png\n", 1, "use forward slashes");
    parseFail("basecolor: ../a.png\n", 1, "must not contain '..'");
    parseFail("basecolor: maps/../a.png\n", 1, "must not contain '..'");
    // Role gating
    parseFail("normal:\n  file: n.png\n  channel: r\n", 3,
              "'channel' is not allowed for 'normal'");
    parseFail("metallic:\n  file: m.png\n  convention: opengl\n", 3,
              "'convention' is not allowed for 'metallic'");
    parseFail("roughness:\n  file: r.png\n  channel: x\n", 3,
              "expected 'r', 'g', 'b', or 'a'");
    parseFail("normal:\n  file: n.png\n  convention: mikkt\n", 3,
              "expected 'opengl', 'directx', 'opengl_xy', or 'directx_xy'");
    // Bare 'xy' was split into 'opengl_xy' and 'directx_xy'; refuse to
    // guess which one was meant.
    parseFail("normal:\n  file: n.png\n  convention: xy\n", 3,
              "'xy' is ambiguous");
    parseFail("basecolor:\n  file: c.png\n  color_space: rec709\n", 3,
              "expected 'srgb' or 'linear'");
    parseFail("basecolor:\n  file: c.png\n  wrap: repeat\n", 3,
              "unknown key 'wrap'");
    // Horizon
    {
      auto manifest{parseOK("physical_extent: [2, 2]\n"
                            "physical_relief: 0.5\n"
                            "horizon: h.png\n")};
      CHECK(manifest[Manifest::ROLE_HORIZON].maxSlope == doctest::Approx(8.0f));
    }
    parseFail("horizon: h.png\n", 1,
              "requires 'relief_scale', or 'physical_extent' and "
              "'physical_relief'");
    parseFail("physical_extent: [2, 2]\nhorizon: h.png\n", 2,
              "requires 'relief_scale', or 'physical_extent' and "
              "'physical_relief'");
    parseFail("physical_relief: 0.5\nhorizon: h.png\n", 2,
              "requires 'relief_scale', or 'physical_extent' and "
              "'physical_relief'");
    parseFail("roughness:\n  file: r.png\n  max_slope: 4\n", 3,
              "'max_slope' is not allowed for 'roughness'");
    // Emission factor and roughness/metallic range
    parseFail("roughness:\n  file: r.png\n  factor: 2\n", 3,
              "'factor' is not allowed for 'roughness'");
    parseFail("basecolor:\n  file: c.png\n  factor: 0.5\n", 3,
              "'factor' is not allowed for 'basecolor'");
    parseFail("emission:\n  file: e.png\n  factor: -1\n", 3,
              "expected a non-negative real for 'factor'");
    parseFail("emission:\n  file: e.png\n  range: [0, 1]\n", 3,
              "'range' is not allowed for 'emission'");
    parseFail("height:\n  file: h.png\n  range: [0, 1]\n", 3,
              "'range' is not allowed for 'height'");
    parseFail("roughness:\n  file: r.png\n  range: 0.5\n", 3,
              "expected an inline list");
    parseFail("roughness:\n  file: r.png\n  range: [0.5]\n", 3,
              "expected two reals in [0, 1] for 'range'");
    parseFail("roughness:\n  file: r.png\n  range: [0, 0.5, 1]\n", 3,
              "expected two reals in [0, 1] for 'range'");
    parseFail("roughness:\n  file: r.png\n  range: [0, 1.5]\n", 3,
              "expected two reals in [0, 1] for 'range'");
    parseFail("roughness:\n  file: r.png\n  range: [-0.1, 1]\n", 3,
              "expected two reals in [0, 1] for 'range'");
    parseFail("physical_extent: [2, 2]\nphysical_relief: 0.5\n"
              "horizon:\n  file: h.png\n  max_slope: 0\n",
              5, "expected a positive real for 'max_slope'");
    parseFail("physical_extent: [2, 2]\nphysical_relief: 0.5\n"
              "horizon:\n  file: h.png\n  channel: r\n",
              5, "'channel' is not allowed for 'horizon'");
    // Classes are no longer named, and the retired key says so itself
    // rather than reporting an unknown key.
    parseFail("class:\n  file: c.png\n  classes: [a, b]\n", 3,
              "'classes' is retired");
    parseFail("roughness:\n  file: r.png\n  classes: [a]\n", 3,
              "'classes' is retired");
  }
  SUBCASE("Resolve file name") {
    auto tmpDir{fs::temp_directory_path() / "smdl-texturepack-test"};
    fs::remove_all(tmpDir);
    fs::create_directories(tmpDir / "empty");
    fs::create_directories(tmpDir / "unique");
    fs::create_directories(tmpDir / "ambiguous");
    std::ofstream((tmpDir / "unique" / "material.pbr").string())
        << "basecolor: a.png\n";
    std::ofstream((tmpDir / "unique" / "notes.txt").string()) << "x\n";
    std::ofstream((tmpDir / "ambiguous" / "material_1k.pbr").string())
        << "basecolor: a.png\n";
    std::ofstream((tmpDir / "ambiguous" / "material_4k.pbr").string())
        << "basecolor: a.png\n";
    // A non-directory path passes through unchanged.
    CHECK(Manifest::resolveFileName("whatever.pbr") == "whatever.pbr");
    // A directory must contain exactly one '.pbr'.
    auto resolved{Manifest::resolveFileName((tmpDir / "unique").string())};
    CHECK(fs::path(resolved).filename() == "material.pbr");
    CHECK_THROWS_WITH_AS(
        (void)Manifest::resolveFileName((tmpDir / "empty").string()),
        doctest::Contains("no '.pbr' manifest"), smdl::Error);
    CHECK_THROWS_WITH_AS(
        (void)Manifest::resolveFileName((tmpDir / "ambiguous").string()),
        doctest::Contains("more than one '.pbr' manifest"), smdl::Error);
    // And 'loadFromFile' composes with it.
    Manifest manifest{};
    CHECK(!manifest.loadFromFile(resolved));
    CHECK(manifest[Manifest::ROLE_BASECOLOR].file == "a.png");
    CHECK(manifest.loadFromFile((tmpDir / "missing.pbr").string()));
    fs::remove_all(tmpDir);
  }
}
