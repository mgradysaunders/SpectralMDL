#include "RenderFixtures.h"

#include <cmath>
#include <string>

#include "smdl/Compiler.h"

#include "Color.h"
#include "Layout/Layout.h"
#include "Render/Autolook.h"
#include "Scene/Scene.h"

// The autofocus: a probe of the committed scene at the center of the
// frame. What matters is that it finds the surface the center sees at
// its distance along the view axis, that the median keeps a sliver at
// the very center from pulling the focus off what fills the patch, and
// that a frame with nothing at its center focuses at infinity.

namespace {

const char *MATERIALS{"#smdl\n"
                      "import ::df::*;\n"
                      "export material gray() = material(\n"
                      "  surface: material_surface(\n"
                      "    scattering: df::diffuse_reflection_bsdf()));\n"};

// A quad in the XY plane facing +Z, two units across.
const char *QUAD{"o quad\n"
                 "v -1 -1 0\n"
                 "v 1 -1 0\n"
                 "v 1 1 0\n"
                 "v -1 1 0\n"
                 "f 1 2 3 4\n"};

// The same quad tilted about the X axis: its far edge a unit higher
// than its near edge, so that what the probe's rays hit lies at
// distances on both sides of the center's.
const char *TILTED_QUAD{"o quad\n"
                        "v -1 -1 -0.5\n"
                        "v 1 -1 -0.5\n"
                        "v 1 1 0.5\n"
                        "v -1 1 0.5\n"
                        "f 1 2 3 4\n"};

// A sliver a hundredth of a unit across, which the center ray alone
// hits from where the cases look.
const char *SLIVER{"o sliver\n"
                   "v -0.005 -0.005 0\n"
                   "v 0.005 -0.005 0\n"
                   "v 0.005 0.005 0\n"
                   "v -0.005 0.005 0\n"
                   "f 1 2 3 4\n"};

// The thin lens's focal length in image heights at the default field.
constexpr float FOCAL_LENGTH{1.4605f};

class QuadScene final {
public:
  // The quads named, each placed at its own height along Z.
  QuadScene(const char *stem,
            std::initializer_list<std::pair<const char *, float>> quads)
      : dir(stem) {
    REQUIRE_OK(compiler.addCode("::autofocustest", MATERIALS));
    registerSceneData(compiler);
    int i{};
    for (const auto &[text, z] : quads) {
      LayoutItem item{};
      item.fileName =
          dir.write("quad" + std::to_string(i++) + ".obj", text).string();
      item.materials.all = "gray";
      item.objectToWorld[3] = float4(0.0f, 0.0f, z, 1.0f);
      scene.add(item);
    }
    REQUIRE_OK(compiler.compile(smdl::OPT_LEVEL_O2));
    REQUIRE_OK(compiler.jitCompile());
    scene.commit(wavelengths);
  }

  // The probe from `from` looking at `to`, up the Y axis.
  [[nodiscard]] AutofocusResult solve(float3 from, float3 to) const {
    auto options{AutofocusOptions{}};
    options.lookFrom = from;
    options.lookTo = to;
    options.lookUp = float3(0.0f, 1.0f, 0.0f);
    options.focalLengthOverHeight = FOCAL_LENGTH;
    return solveAutofocus(scene, options);
  }

  TempDir dir;
  smdl::Compiler compiler{};
  Scene scene{compiler};
  ScopedGrid grid{};
  Color wavelengths{grid.wavelengths()};
};

} // namespace

TEST_CASE("Autofocus: a quad five units down the view axis focuses at five") {
  QuadScene fixture{"toy-autofocus-quad", {{QUAD, 0.0f}}};
  const auto result{fixture.solve(float3(0.0f, 0.0f, 5.0f), float3(0.0f))};
  CHECK(result.rayCount == 25);
  CHECK(result.hitCount == 25);
  CHECK(result.distance == doctest::Approx(5.0f).epsilon(1e-4));
  CHECK(result.instIndex == 0);
  CHECK(result.matIndex < fixture.scene.materialNames.size());
  CHECK(fixture.scene.materialNames[result.matIndex] == "gray");
}

TEST_CASE("Autofocus: the distance is along the view axis, not the ray") {
  QuadScene fixture{"toy-autofocus-axis", {{QUAD, 0.0f}}};
  // From off to the side, the center ray runs the diagonal and the
  // projection on the axis is the diagonal's length.
  const auto from{float3(3.0f, 0.0f, 4.0f)};
  const auto result{fixture.solve(from, float3(0.0f))};
  CHECK(result.hitCount == 25);
  CHECK(result.distance == doctest::Approx(5.0f).epsilon(1e-3));
}

TEST_CASE("Autofocus: a tilted plane focuses at what the center sees") {
  QuadScene fixture{"toy-autofocus-tilt", {{TILTED_QUAD, 0.0f}}};
  const auto result{fixture.solve(float3(0.0f, 0.0f, 5.0f), float3(0.0f))};
  CHECK(result.hitCount == 25);
  // The patch's rays land on both sides of the center at distances that
  // differ from it by a few hundredths; the median is the center's.
  CHECK(result.distance == doctest::Approx(5.0f).epsilon(1e-3));
}

TEST_CASE("Autofocus: a sliver at the very center does not pull the focus") {
  QuadScene fixture{"toy-autofocus-sliver", {{QUAD, 0.0f}, {SLIVER, 4.0f}}};
  const auto result{fixture.solve(float3(0.0f, 0.0f, 5.0f), float3(0.0f))};
  CHECK(result.hitCount == 25);
  // One ray of the twenty-five hits the sliver a unit away; the median
  // is what the other twenty-four see.
  CHECK(result.distance == doctest::Approx(5.0f).epsilon(1e-3));
  CHECK(result.instIndex == 0);
}

TEST_CASE("Autofocus: a frame with nothing at its center focuses at infinity") {
  QuadScene fixture{"toy-autofocus-miss", {{QUAD, 0.0f}}};
  const auto result{
      fixture.solve(float3(0.0f, 0.0f, 5.0f), float3(0.0f, 0.0f, 10.0f))};
  CHECK(result.rayCount == 25);
  CHECK(result.hitCount == 0);
  CHECK(std::isinf(result.distance));
  CHECK(result.instIndex == INVALID_INDEX);
}
