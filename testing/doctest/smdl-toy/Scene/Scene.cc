#include "doctest.h"

#include <filesystem>
#include <fstream>
#include <string>
#include <vector>

#include "smdl/Compiler.h"
#include "smdl/Support/Span.h"

#include "Color.h"
#include "IO/MeshImport.h"
#include "Layout/Layout.h"
#include "Scene/Scene.h"

namespace fs = std::filesystem;

// Vertex colors from file to material: an OBJ quad with a color per
// corner (assimp reads the six-component `v` line), placed under three
// materials that read the color through the three spellings, beside a
// colorless quad under two of them. What a hit interpolates, what the
// state receives, and what the material instance carries are checked
// against each other and against the corner values.

static const char *MATERIALS{
    "#smdl\n"
    "import ::df::*;\n"
    "import ::scene::*;\n"
    "import ::state::*;\n"
    "export material vc_red() = material(\n"
    "  surface: material_surface(emission: material_emission(\n"
    "    emission: df::diffuse_edf(), intensity: state::vertex_color().x)));\n"
    "export material vc_alias() = material(\n"
    "  surface: material_surface(emission: material_emission(\n"
    "    emission: df::diffuse_edf(),\n"
    "    intensity: scene::data_lookup_float4(\"vertex_color\",\n"
    "                 float4(0.0, 0.0, 0.0, 0.0)).y)));\n"
    "export material vc_valid() = material(\n"
    "  surface: material_surface(emission: material_emission(\n"
    "    emission: df::diffuse_edf(),\n"
    "    intensity: scene::data_isvalid(\"vertex_color\") ? 1.0 : 0.0)));\n"};

// A unit quad in the XY plane facing +Z, with or without a color per
// corner: red, green, blue, white counterclockwise from (-1, -1).
static const char *QUAD_COLORS{"o quad\n"
                               "v -1 -1 0 1 0 0\n"
                               "v 1 -1 0 0 1 0\n"
                               "v 1 1 0 0 0 1\n"
                               "v -1 1 0 1 1 1\n"
                               "f 1 2 3 4\n"};
static const char *QUAD_PLAIN{"o quad\n"
                              "v -1 -1 0\n"
                              "v 1 -1 0\n"
                              "v 1 1 0\n"
                              "v -1 1 0\n"
                              "f 1 2 3 4\n"};

namespace {

class Fixture final {
public:
  Fixture() {
    fs::remove_all(dir);
    fs::create_directories(dir);
    colored = write("quad_colors.obj", QUAD_COLORS);
    plain = write("quad.obj", QUAD_PLAIN);
    if (auto error{compiler.addCode("::vertexcolortest", MATERIALS)}) {
      MESSAGE(error->message);
      REQUIRE(false);
    }
    registerSceneData(compiler);
    // Seven placements four units apart along +X: the colored quad under
    // each spelling, the plain quad under the presence test and under the
    // extension spelling, then the colored quad refined linearly one
    // level and smoothly two levels.
    const char *materials[]{"vc_red", "vc_alias", "vc_valid", "vc_valid",
                            "vc_red", "vc_red",   "vc_red"};
    for (int i = 0; i < 7; i++) {
      LayoutItem item{};
      item.fileName = i == 3 || i == 4 ? plain : colored;
      item.materials.all = materials[i];
      item.objectToWorld[3] = float4(4.0f * float(i), 0.0f, 0.0f, 1.0f);
      if (i == 5) {
        item.subdiv.levels = 1;
        item.subdiv.smooth = false;
      } else if (i == 6) {
        item.subdiv.levels = 2;
        item.subdiv.smooth = true;
      }
      scene.add(item);
    }
    if (auto error{compiler.compile(smdl::OPT_LEVEL_O2)}) {
      MESSAGE(error->message);
      REQUIRE(false);
    }
    if (auto error{compiler.jitCompile()}) {
      MESSAGE(error->message);
      REQUIRE(false);
    }
    auto gridSpec{std::vector<float>(16)};
    for (size_t i = 0; i < gridSpec.size(); i++)
      gridSpec[i] = 400.0f + 300.0f * float(i) / float(gridSpec.size() - 1);
    wavelengths =
        Color(smdl::Span<const float>(gridSpec.data(), gridSpec.size()));
    renderWavelengths() = wavelengths;
    scene.commit(wavelengths);
  }
  ~Fixture() { fs::remove_all(dir); }

  std::string write(const std::string &name, const char *text) const {
    const auto path{(dir / name).string()};
    std::ofstream file(path, std::ios::binary | std::ios::trunc);
    file << text;
    return path;
  }

  /// The hit on placement `i` at its local (x, y), cast straight down.
  [[nodiscard]] Hit hitOn(int i, float x, float y) const {
    Ray ray{float3(4.0f * float(i) + x, y, 5.0f), float3(0.0f, 0.0f, -1.0f),
            EPS, INF};
    Hit hit{};
    REQUIRE(scene.intersect(ray, hit));
    REQUIRE(hit.instIndex == uint32_t(i));
    return hit;
  }

  /// The emission intensity a material instance carries at `hit`.
  [[nodiscard]] float intensityAt(const Hit &hit) const {
    auto allocator{smdl::BumpPtrAllocator()};
    auto state{makeRenderState(wavelengths, &allocator)};
    hit.applyGeometryToState(state, float3(0.0f, 0.0f, -1.0f));
    const auto mat{smdl::JIT::MaterialInstance(state, hit.material)};
    const auto values{mat.getSurfaceEmissionIntensity()};
    REQUIRE(!values.empty());
    float sum{};
    for (float value : values) sum += value;
    return sum / float(values.size());
  }

  fs::path dir{fs::temp_directory_path() / "smdl-toy-scene-test"};
  std::string colored{};
  std::string plain{};
  smdl::Compiler compiler{};
  Scene scene{compiler};
  Color wavelengths{};
};

} // namespace

TEST_CASE("Scene: vertex colors from file to state") {
  Fixture fixture{};
  SUBCASE("The listing reports them") {
    ObjectFileInfo info{};
    (void)importObjectUsage(fixture.colored, &info);
    CHECK(info.hasColors);
    ObjectFileInfo plainInfo{};
    (void)importObjectUsage(fixture.plain, &plainInfo);
    CHECK(!plainInfo.hasColors);
  }
  SUBCASE("A hit interpolates the corners") {
    // The center sits on the diagonal between the red and blue corners,
    // whichever triangle the ray lands in.
    const auto center{fixture.hitOn(0, 0.0f, 0.0f)};
    CHECK(center.vertexColorSets == 1);
    CHECK(center.vertexColor.x == doctest::Approx(0.5f).epsilon(1e-4));
    CHECK(center.vertexColor.y == doctest::Approx(0.0f).epsilon(1e-4));
    CHECK(center.vertexColor.z == doctest::Approx(0.5f).epsilon(1e-4));
    CHECK(center.vertexColor.w == doctest::Approx(1.0f).epsilon(1e-4));
    // Next to the green corner.
    const auto corner{fixture.hitOn(0, 0.999f, -0.998f)};
    CHECK(corner.vertexColor.x == doctest::Approx(0.0f).epsilon(3e-3));
    CHECK(corner.vertexColor.y == doctest::Approx(1.0f).epsilon(3e-3));
    CHECK(corner.vertexColor.z == doctest::Approx(0.0f).epsilon(3e-3));
    // Anywhere: the barycentric sum of the face's stored colors.
    const auto hit{fixture.hitOn(0, 0.3f, -0.6f)};
    const auto &mesh{*fixture.scene.meshes[hit.meshIndex]};
    REQUIRE(!mesh.colors.empty());
    const auto &face{mesh.faces[hit.faceIndex]};
    const auto expected{hit.bary[0] * mesh.colors[face[0]] +
                        hit.bary[1] * mesh.colors[face[1]] +
                        hit.bary[2] * mesh.colors[face[2]]};
    for (int k = 0; k < 4; k++) {
      CAPTURE(k);
      CHECK(hit.vertexColor[k] == doctest::Approx(expected[k]).epsilon(1e-6));
    }
    // And the state receives exactly that.
    auto state{makeRenderState(fixture.wavelengths)};
    hit.applyGeometryToState(state);
    CHECK(state.vertex_color_max == 1);
    for (int k = 0; k < 4; k++) {
      CAPTURE(k);
      CHECK(state.vertex_color[0][k] == hit.vertexColor[k]);
    }
  }
  SUBCASE("A colorless mesh carries no set") {
    const auto hit{fixture.hitOn(3, 0.3f, -0.6f)};
    CHECK(hit.vertexColorSets == 0);
    CHECK(hit.vertexColor.x == 1.0f);
    CHECK(hit.vertexColor.w == 1.0f);
    auto state{makeRenderState(fixture.wavelengths)};
    hit.applyGeometryToState(state);
    CHECK(state.vertex_color_max == 0);
    CHECK(state.vertex_color[0].x == 1.0f);
  }
  SUBCASE("Subdivision refines the colors") {
    for (int i : {5, 6}) {
      CAPTURE(i);
      // The refined quad's center vertex carries the mean of the four
      // corners, under linear refinement by construction and under
      // smooth refinement by the symmetry of the smoothing rule; the
      // corners keep their values under the linear boundary rule.
      const auto center{fixture.hitOn(i, 0.0f, 0.0f)};
      CHECK(center.vertexColorSets == 1);
      CHECK(center.vertexColor.x == doctest::Approx(0.5f).epsilon(1e-3));
      CHECK(center.vertexColor.y == doctest::Approx(0.5f).epsilon(1e-3));
      CHECK(center.vertexColor.z == doctest::Approx(0.5f).epsilon(1e-3));
      CHECK(center.vertexColor.w == doctest::Approx(1.0f).epsilon(1e-3));
      const auto corner{fixture.hitOn(i, 0.999f, -0.998f)};
      CHECK(corner.vertexColor.x == doctest::Approx(0.0f).epsilon(5e-3));
      CHECK(corner.vertexColor.y == doctest::Approx(1.0f).epsilon(5e-3));
      CHECK(corner.vertexColor.z == doctest::Approx(0.0f).epsilon(5e-3));
      // The refined colors are parallel to the refined vertices.
      const auto &mesh{*fixture.scene.meshes[center.meshIndex]};
      CHECK(mesh.colors.size() == mesh.verts.size());
      CHECK(fixture.intensityAt(center) ==
            doctest::Approx(center.vertexColor.x).epsilon(1e-5));
    }
  }
  SUBCASE("The material sees it through every spelling") {
    const auto red{fixture.hitOn(0, 0.3f, -0.6f)};
    CHECK(fixture.intensityAt(red) ==
          doctest::Approx(red.vertexColor.x).epsilon(1e-5));
    const auto alias{fixture.hitOn(1, 0.3f, -0.6f)};
    CHECK(fixture.intensityAt(alias) ==
          doctest::Approx(alias.vertexColor.y).epsilon(1e-5));
    CHECK(fixture.intensityAt(fixture.hitOn(2, 0.3f, -0.6f)) ==
          doctest::Approx(1.0f));
    // The plain quad: not valid, and the extension reads white.
    CHECK(fixture.intensityAt(fixture.hitOn(3, 0.3f, -0.6f)) ==
          doctest::Approx(0.0f));
    CHECK(fixture.intensityAt(fixture.hitOn(4, 0.3f, -0.6f)) ==
          doctest::Approx(1.0f));
  }
}

// The frame of an instance at a time: a static instance answers with its
// own frame by reference, and the transform Embree holds behind the
// retained handle, read back at both ends of the shutter, is the frame's
// own matrix, for a regular instance and for the elements of an instance
// array alike.

namespace {

class FrameFixture final {
public:
  FrameFixture() {
    if (auto error{compiler.addCode("::frametest", MATERIALS)}) {
      MESSAGE(error->message);
      REQUIRE(false);
    }
    // A sheared, non-uniformly scaled placement, so that the read-back is
    // a real matrix rather than a pattern of ones and zeros: a sphere
    // placed once, then a box placed as a batch of three, each element
    // sheared a little differently.
    const float4x4 shear{
        float4(1.0f, 0.2f, 0.0f, 0.0f), float4(0.3f, 1.5f, 0.1f, 0.0f),
        float4(0.0f, -0.4f, 0.8f, 0.0f), float4(2.0f, -1.0f, 3.0f, 1.0f)};
    LayoutItem single{};
    single.primitive.shape = PrimitiveSpec::Shape::SPHERE;
    single.materials.all = "vc_red";
    single.objectToWorld = shear;
    scene.add(single);
    LayoutItem batch{};
    batch.primitive.shape = PrimitiveSpec::Shape::BOX;
    batch.materials.all = "vc_red";
    for (int i = 0; i < 3; i++) {
      auto xf{shear};
      xf[0][1] += 0.05f * float(i);
      xf[3] = float4(10.0f * float(i), 0.0f, 0.0f, 1.0f);
      batch.batchXfs.push_back(xf);
    }
    scene.add(batch);
    if (auto error{compiler.compile(smdl::OPT_LEVEL_O2)}) {
      MESSAGE(error->message);
      REQUIRE(false);
    }
    if (auto error{compiler.jitCompile()}) {
      MESSAGE(error->message);
      REQUIRE(false);
    }
    auto gridSpec{std::vector<float>(16)};
    for (size_t i = 0; i < gridSpec.size(); i++)
      gridSpec[i] = 400.0f + 300.0f * float(i) / float(gridSpec.size() - 1);
    wavelengths =
        Color(smdl::Span<const float>(gridSpec.data(), gridSpec.size()));
    renderWavelengths() = wavelengths;
    scene.commit(wavelengths);
  }

  smdl::Compiler compiler{};
  Scene scene{compiler};
  Color wavelengths{};
};

[[nodiscard]] float4x4 readBack(const MeshInstance &instance, float time) {
  float4x4 xf{};
  rtcGetGeometryTransformEx(instance.geometry, instance.instPrimID, time,
                            RTC_FORMAT_FLOAT4X4_COLUMN_MAJOR, &xf[0][0]);
  return xf;
}

[[nodiscard]] bool sameMatrix(const float4x4 &a, const float4x4 &b) {
  for (int j = 0; j < 4; j++)
    for (int i = 0; i < 4; i++)
      if (a[j][i] != b[j][i]) return false;
  return true;
}

} // namespace

TEST_CASE("Scene: the frame at a time through the retained handle") {
  FrameFixture fixture{};
  const auto &instances{fixture.scene.meshInstances};
  REQUIRE(instances.size() == 4);
  for (const auto &instance : instances) {
    CHECK(!instance.isMoving);
    std::optional<InstanceFrame> scratch{};
    CHECK(&instance.frameAt(0.3f, scratch) == &instance.frame);
    CHECK(sameMatrix(readBack(instance, 0.0f), instance.frame.objectToWorld));
    CHECK(sameMatrix(readBack(instance, 1.0f), instance.frame.objectToWorld));
  }
  // One handle for the regular instance, one shared by the array's
  // elements, addressed by their index.
  CHECK(instances[0].instPrimID == 0);
  CHECK(instances[1].instPrimID == 0);
  CHECK(instances[2].instPrimID == 1);
  CHECK(instances[3].instPrimID == 2);
  CHECK(instances[0].geometry != instances[1].geometry);
  CHECK(instances[1].geometry == instances[3].geometry);
  CHECK(fixture.scene.instanceGeometries.size() == 2);
  // The elements really do differ, so the array read is per element.
  CHECK(!sameMatrix(instances[1].frame.objectToWorld,
                    instances[3].frame.objectToWorld));
}
