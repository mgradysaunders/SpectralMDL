#include "doctest.h"

#include <cmath>
#include <filesystem>
#include <fstream>
#include <optional>
#include <string>
#include <vector>

#include "smdl/Compiler.h"
#include "smdl/Support/Span.h"

#include "Color.h"
#include "IO/MeshImport.h"
#include "Layout/Layout.h"
#include "RigFixtures.h"
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
        item.subdiv.isSmooth = false;
      } else if (i == 6) {
        item.subdiv.levels = 2;
        item.subdiv.isSmooth = true;
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
    renderGrid().wavelengths = wavelengths;
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
    // A 90-degree turn about z: the x axis lands on y.
    const float4x4 turn{
        float4(0.0f, 1.0f, 0.0f, 0.0f), float4(-1.0f, 0.0f, 0.0f, 0.0f),
        float4(0.0f, 0.0f, 1.0f, 0.0f), float4(0.0f, 0.0f, 0.0f, 1.0f)};
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
    // Then the moving instances: the sphere again under a shut key that
    // turns and translates it; the batch again under one rigid shut
    // key over every element; a sphere turning 90 degrees about z from
    // identity, for the slerp check; and a sphere whose shut key
    // mirrors its open key, which cannot be interpolated and stays
    // still.
    LayoutItem moving{single};
    moving.objectToWorldShut = turn * shear;
    (*moving.objectToWorldShut)[3] = float4(3.0f, 4.0f, 5.0f, 1.0f);
    scene.add(moving);
    LayoutItem movingBatch{batch};
    for (const auto &xf : batch.batchXfs) {
      auto shut{xf};
      shut[3].z += 5.0f;
      movingBatch.batchXfsShut.push_back(shut);
    }
    scene.add(movingBatch);
    LayoutItem turning{single};
    turning.objectToWorld = float4x4(1.0f);
    turning.objectToWorldShut = turn;
    scene.add(turning);
    LayoutItem insideOut{single};
    insideOut.objectToWorld = float4x4(1.0f);
    insideOut.objectToWorldShut = float4x4(1.0f);
    (*insideOut.objectToWorldShut)[0].x = -1.0f;
    scene.add(insideOut);
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
    renderGrid().wavelengths = wavelengths;
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

// Every component of `a` within `tolerance` of `b`'s, absolutely, which
// is what the read-back of a decomposed key can promise: the
// quaternion round trip rounds at the last bit, so a bitwise match is
// not on offer.
[[nodiscard]] bool nearMatrix(const float4x4 &a, const float4x4 &b,
                              float tolerance) {
  for (int j = 0; j < 4; j++)
    for (int i = 0; i < 4; i++)
      if (!(std::fabs(a[j][i] - b[j][i]) <= tolerance)) return false;
  return true;
}

// The affine transform a decomposition stands for, `T R S` with the
// shift inside `S`, from the fields alone: the check's own reassembly,
// independent of Embree's.
[[nodiscard]] float4x4 reassemble(const RTCQuaternionDecomposition &qd) {
  const float r{qd.quaternion_r};
  const float i{qd.quaternion_i};
  const float j{qd.quaternion_j};
  const float k{qd.quaternion_k};
  const float3x3 R{float3(r * r + i * i - j * j - k * k, 2 * (i * j + r * k),
                          2 * (i * k - r * j)),
                   float3(2 * (i * j - r * k), r * r - i * i + j * j - k * k,
                          2 * (j * k + r * i)),
                   float3(2 * (i * k + r * j), 2 * (j * k - r * i),
                          r * r - i * i - j * j + k * k)};
  const float3x3 S{float3(qd.scale_x, 0.0f, 0.0f),
                   float3(qd.skew_xy, qd.scale_y, 0.0f),
                   float3(qd.skew_xz, qd.skew_yz, qd.scale_z)};
  const float3x3 RS{R * S};
  const float3 t{float3(qd.translation_x, qd.translation_y, qd.translation_z) +
                 R * float3(qd.shift_x, qd.shift_y, qd.shift_z)};
  return float4x4{float4(RS[0], 0.0f), float4(RS[1], 0.0f), float4(RS[2], 0.0f),
                  float4(t, 1.0f)};
}

} // namespace

TEST_CASE("Scene: the quaternion decomposition reassembles the key") {
  const float4x4 shear{
      float4(1.0f, 0.2f, 0.0f, 0.0f), float4(0.3f, 1.5f, 0.1f, 0.0f),
      float4(0.0f, -0.4f, 0.8f, 0.0f), float4(2.0f, -1.0f, 3.0f, 1.0f)};
  auto mirrored{shear};
  mirrored[2] = -mirrored[2];
  const std::pair<const char *, float4x4> keys[]{{"sheared", shear},
                                                 {"mirrored", mirrored}};
  for (const auto &entry : keys) {
    const char *name{entry.first};
    const auto &key{entry.second};
    CAPTURE(name);
    const auto qd{quaternionDecompositionOf(key)};
    CHECK(nearMatrix(reassemble(qd), key, 2.0e-6f));
    CHECK(qd.shift_x == 0.0f);
    CHECK(qd.shift_y == 0.0f);
    CHECK(qd.shift_z == 0.0f);
    CHECK(qd.translation_x == key[3].x);
    CHECK(qd.translation_y == key[3].y);
    CHECK(qd.translation_z == key[3].z);
    CHECK(qd.scale_x > 0.0f);
    CHECK(qd.scale_y > 0.0f);
    // The quaternion is a rotation, so its norm is 1 and the rotation
    // it stands for is proper; a mirrored key folds into its scale.
    const float norm{
        qd.quaternion_r * qd.quaternion_r + qd.quaternion_i * qd.quaternion_i +
        qd.quaternion_j * qd.quaternion_j + qd.quaternion_k * qd.quaternion_k};
    CHECK(norm == doctest::Approx(1.0f).epsilon(1.0e-5));
    CHECK((qd.scale_z < 0.0f) == (name == std::string("mirrored")));
  }
}

TEST_CASE("Scene: the frame at a time through the retained handle") {
  FrameFixture fixture{};
  const auto &instances{fixture.scene.meshInstances};
  REQUIRE(instances.size() == 10);
  // The four static instances: no query, the authored matrix exactly.
  for (size_t i = 0; i < 4; i++) {
    const auto &instance{instances[i]};
    CHECK(!instance.isMoving);
    std::optional<InstanceFrame> scratch{};
    CHECK(&instance.frameAt(0.3f, scratch) == &instance.frame);
    CHECK(!scratch);
    CHECK(sameMatrix(readBack(instance, 0.0f), instance.frame.objectToWorld));
    CHECK(sameMatrix(readBack(instance, 1.0f), instance.frame.objectToWorld));
  }
  // One handle for the regular instance, one shared by the array's
  // elements, addressed by their index; then one per moving item.
  CHECK(instances[0].instPrimID == 0);
  CHECK(instances[1].instPrimID == 0);
  CHECK(instances[2].instPrimID == 1);
  CHECK(instances[3].instPrimID == 2);
  CHECK(instances[0].geometry != instances[1].geometry);
  CHECK(instances[1].geometry == instances[3].geometry);
  CHECK(fixture.scene.instanceGeometries.size() == 6);
  // The elements really do differ, so the array read is per element.
  CHECK(!sameMatrix(instances[1].frame.objectToWorld,
                    instances[3].frame.objectToWorld));
}

TEST_CASE("Scene: a moving instance reads back its keys") {
  FrameFixture fixture{};
  const auto &instances{fixture.scene.meshInstances};
  REQUIRE(instances.size() == 10);
  constexpr float TOLERANCE = 1.0e-5f;
  SUBCASE("A regular instance: the frame is queried, the keys come back") {
    const auto &sphere{instances[4]};
    CHECK(sphere.isMoving);
    // The stored frame is the authored open key; the query fills the
    // scratch and answers with it.
    CHECK(sameMatrix(sphere.frame.objectToWorld,
                     instances[0].frame.objectToWorld));
    std::optional<InstanceFrame> scratch{};
    const auto &frame{sphere.frameAt(0.3f, scratch)};
    REQUIRE(scratch);
    CHECK(&frame == &*scratch);
    CHECK(nearMatrix(readBack(sphere, 0.0f), sphere.frame.objectToWorld,
                     TOLERANCE));
    auto shut{instances[0].frame.objectToWorld};
    shut = float4x4{float4(0.0f, 1.0f, 0.0f, 0.0f),
                    float4(-1.0f, 0.0f, 0.0f, 0.0f),
                    float4(0.0f, 0.0f, 1.0f, 0.0f),
                    float4(0.0f, 0.0f, 0.0f, 1.0f)} *
           shut;
    shut[3] = float4(3.0f, 4.0f, 5.0f, 1.0f);
    CHECK(nearMatrix(readBack(sphere, 1.0f), shut, TOLERANCE));
    CHECK(nearMatrix(sphere.frameAt(1.0f, scratch).objectToWorld, shut,
                     TOLERANCE));
  }
  SUBCASE("An array: every element reads back its own pair") {
    for (size_t i = 5; i < 8; i++) {
      CAPTURE(i);
      const auto &element{instances[i]};
      CHECK(element.isMoving);
      CHECK(element.instPrimID == unsigned(i - 5));
      CHECK(element.geometry == instances[5].geometry);
      const auto &open{instances[i - 4].frame.objectToWorld};
      auto shut{open};
      shut[3].z += 5.0f;
      CHECK(nearMatrix(readBack(element, 0.0f), open, TOLERANCE));
      CHECK(nearMatrix(readBack(element, 1.0f), shut, TOLERANCE));
    }
  }
  SUBCASE("A turn slerps: halfway through 90 degrees is 45, not the lerp") {
    const auto &turning{instances[8]};
    CHECK(turning.isMoving);
    const auto halfway{readBack(turning, 0.5f)};
    const float c{std::cos(PI / 4)};
    CHECK(halfway[0].x == doctest::Approx(c).epsilon(TOLERANCE));
    CHECK(halfway[0].y == doctest::Approx(c).epsilon(TOLERANCE));
    CHECK(halfway[0].z == doctest::Approx(0.0f).epsilon(TOLERANCE));
    CHECK(halfway[1].x == doctest::Approx(-c).epsilon(TOLERANCE));
    CHECK(halfway[1].y == doctest::Approx(c).epsilon(TOLERANCE));
    // The componentwise lerp of the two keys has x axis (0.5, 0.5, 0).
    CHECK(std::fabs(halfway[0].x - 0.5f) > 1.0e-2f);
  }
  SUBCASE("A pair that turns inside out holds its open key") {
    const auto &insideOut{instances[9]};
    CHECK(!insideOut.isMoving);
    std::optional<InstanceFrame> scratch{};
    CHECK(&insideOut.frameAt(1.0f, scratch) == &insideOut.frame);
    CHECK(sameMatrix(readBack(insideOut, 1.0f), float4x4(1.0f)));
  }
}

// The animated read: the rigged glTF files of `RigFixtures.h` placed
// under a clock, checked for what the read bakes, welds, and hands the
// instances, before any hit is built.

static const char *RIG_MATERIALS{
    "#smdl\n"
    "import ::df::*;\n"
    "export material paint() = material(\n"
    "  surface: material_surface(emission: material_emission(\n"
    "    emission: df::diffuse_edf(), intensity: 1.0)));\n"
    "export material bump() = material(\n"
    "  geometry: material_geometry(displacement: float3(0.0, 0.0, 0.1)));\n"};

namespace {

class RigFixture final {
public:
  RigFixture(float base, float shutter) {
    fs::remove_all(dir);
    fs::create_directories(dir);
    files = rig::writeFiles(dir);
    if (auto error{compiler.addCode("::rigtest", RIG_MATERIALS)}) {
      MESSAGE(error->message);
      REQUIRE(false);
    }
    renderShutter().time = base;
    renderShutter().length = shutter;
  }
  ~RigFixture() {
    renderShutter().time = 0.0f;
    renderShutter().length = 0.0f;
    fs::remove_all(dir);
  }

  /// Place `fileName` under `spec`; returns its first instance index.
  uint32_t add(const std::string &fileName, const AnimationSpec &spec,
               const SubdivSpec &subdiv = {}, const char *material = "paint") {
    LayoutItem item{};
    item.fileName = fileName;
    item.animation = spec;
    item.subdiv = subdiv;
    item.materials.all = material;
    const auto first{uint32_t(scene.meshInstances.size())};
    scene.add(item);
    return first;
  }

  void commit() {
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
    renderGrid().wavelengths = wavelengths;
    scene.commit(wavelengths);
  }

  [[nodiscard]] const Mesh &meshOf(uint32_t instIndex) const {
    return *scene.meshes[scene.meshInstances[instIndex].meshIndex];
  }

  fs::path dir{fs::temp_directory_path() / "smdl-toy-rig-scene-test"};
  rig::Files files{};
  smdl::Compiler compiler{};
  Scene scene{compiler};
  Color wavelengths{};
};

[[nodiscard]] bool near3(const float3 &a, const float3 &b, float tol = 1e-4f) {
  return std::fabs(a.x - b.x) < tol && std::fabs(a.y - b.y) < tol &&
         std::fabs(a.z - b.z) < tol;
}

// The vertex whose open point is nearest `point`: the reader renumbers
// vertices and the weld renumbers them again, so nothing else is stable.
[[nodiscard]] uint32_t nearestVert(const Mesh &mesh, const float3 &point) {
  uint32_t best{};
  float bestDist{INF};
  for (uint32_t i = 0; i < mesh.verts.size(); i++) {
    const float dist{smdl::length(mesh.verts[i].point - point)};
    if (dist < bestDist) bestDist = dist, best = i;
  }
  return best;
}

} // namespace

TEST_CASE("Scene: the animated read bakes both keys of the shutter") {
  // Open at 0.25 s and shut at 0.75 s: inside the one-second clip, since
  // a looping clip at exactly its duration wraps to its start.
  RigFixture fixture{0.25f, 0.5f};
  const float open{0.25f * PI / 2}, shut{0.75f * PI / 2};
  const float3 tipOpen{1 + std::cos(open), std::sin(open), 0};
  const float3 tipShut{1 + std::cos(shut), std::sin(shut), 0};
  AnimationSpec off{};
  off.off = true;
  AnimationSpec swing{};
  swing.clipName = "swing";
  AnimationSpec phase{};
  phase.offset = 0.25f;
  const auto waveInst{fixture.add(fixture.files.wave, {})};
  const auto stillInst{fixture.add(fixture.files.wave, off)};
  const auto phaseInst{fixture.add(fixture.files.wave, phase)};
  const auto armInst{fixture.add(fixture.files.pendulum, swing)};
  // The same two as a batch of two records each, ten units apart.
  const auto batchOf{
      [&](const std::string &fileName, const AnimationSpec &spec) {
        LayoutItem item{};
        item.fileName = fileName;
        item.animation = spec;
        item.materials.all = "paint";
        for (int i = 0; i < 2; i++) {
          auto xf{float4x4(1.0f)};
          xf[3] = float4(10.0f * float(i), 0.0f, 0.0f, 1.0f);
          item.batchXfs.push_back(xf);
        }
        const auto first{uint32_t(fixture.scene.meshInstances.size())};
        fixture.scene.add(item);
        return first;
      }};
  const auto waveBatch{batchOf(fixture.files.wave, {})};
  const auto armBatch{batchOf(fixture.files.pendulum, swing)};
  fixture.commit();
  SUBCASE("A skinned mesh carries a parallel shut key in file space") {
    const auto &mesh{fixture.meshOf(waveInst)};
    const auto &inst{fixture.scene.meshInstances[waveInst]};
    CHECK(mesh.isSkinned);
    CHECK(mesh.deforms());
    CHECK(inst.isDeforming);
    CHECK(!inst.isMoving);
    REQUIRE(mesh.verts.size() == 7);
    REQUIRE(mesh.vertsShut.size() == 7);
    // The tip has turned 22.5 degrees at open and 67.5 at shut.
    const auto tip{nearestVert(mesh, tipOpen)};
    CHECK(near3(mesh.verts[tip].point, tipOpen));
    CHECK(near3(mesh.vertsShut[tip].point, tipShut));
    CHECK(near3(mesh.vertsShut[tip].normal, float3(0, 0, 1)));
    const auto base{nearestVert(mesh, float3(0, 0, 0))};
    CHECK(near3(mesh.vertsShut[base].point, float3(0, 0, 0)));
    const auto loose{nearestVert(mesh, float3(5, 5, 0))};
    CHECK(near3(mesh.verts[loose].point, float3(5, 5, 0)));
    CHECK(near3(mesh.vertsShut[loose].point, float3(5, 5, 0)));
    // The placing node's five units up never reach a skinned mesh.
    for (const auto &vert : mesh.verts)
      CHECK(vert.point.z == doctest::Approx(0));
    CHECK(fixture.scene.meshInstances[waveInst].frame.objectToWorld[3].z ==
          doctest::Approx(0));
  }
  SUBCASE("'off' reads the bind pose as a still mesh, welded alike") {
    const auto &mesh{fixture.meshOf(stillInst)};
    CHECK(&mesh != &fixture.meshOf(waveInst));
    CHECK(!mesh.isSkinned);
    CHECK(!mesh.deforms());
    CHECK(!fixture.scene.meshInstances[stillInst].isDeforming);
    CHECK(mesh.verts.size() == 7);
    CHECK(near3(mesh.verts[nearestVert(mesh, float3(2, 0, 0))].point,
                float3(2, 0, 0)));
    // The bind pose sits under the node's transform, as a still file does.
    CHECK(fixture.scene.meshInstances[stillInst].frame.objectToWorld[3].z ==
          doctest::Approx(5));
  }
  SUBCASE("A phase is another mesh set") {
    const auto &mesh{fixture.meshOf(phaseInst)};
    CHECK(&mesh != &fixture.meshOf(waveInst));
    // 0.5 s into the clip at open: forty-five degrees.
    const float3 expected{1 + rig::SIN45, rig::SIN45, 0};
    CHECK(near3(mesh.verts[nearestVert(mesh, expected)].point, expected));
  }
  SUBCASE("A clip that moves a node moves the instance") {
    const auto &inst{fixture.scene.meshInstances[armInst]};
    CHECK(inst.isMoving);
    CHECK(!inst.isDeforming);
    CHECK(!fixture.meshOf(armInst).deforms());
    std::optional<InstanceFrame> scratch{};
    CHECK(near3(float3(inst.frame.objectToWorld[0]),
                float3(std::cos(open), std::sin(open), 0)));
    CHECK(near3(float3(inst.frameAt(1.0f, scratch).objectToWorld[0]),
                float3(std::cos(shut), std::sin(shut), 0), 1e-3f));
  }
  SUBCASE("A batch carries the same keys per element") {
    for (uint32_t i = 0; i < 2; i++) {
      const auto &skinned{fixture.scene.meshInstances[waveBatch + i]};
      CHECK(skinned.isDeforming);
      CHECK(!skinned.isMoving);
      CHECK(&fixture.meshOf(waveBatch + i) == &fixture.meshOf(waveInst));
      CHECK(skinned.frame.objectToWorld[3].x == doctest::Approx(10.0f * i));
      const auto &arm{fixture.scene.meshInstances[armBatch + i]};
      CHECK(arm.isMoving);
      CHECK(!arm.isDeforming);
      std::optional<InstanceFrame> scratch{};
      CHECK(near3(float3(arm.frame.objectToWorld[0]),
                  float3(std::cos(open), std::sin(open), 0)));
      const auto &shutFrame{arm.frameAt(1.0f, scratch)};
      CHECK(near3(float3(shutFrame.objectToWorld[0]),
                  float3(std::cos(shut), std::sin(shut), 0), 1e-3f));
      CHECK(shutFrame.objectToWorld[3].x == doctest::Approx(10.0f * i));
    }
  }
}

TEST_CASE("Scene: a shut shutter holds the pose at the base time") {
  RigFixture fixture{0.5f, 0.0f};
  AnimationSpec swing{};
  swing.clipName = "swing";
  const auto waveInst{fixture.add(fixture.files.wave, {})};
  const auto armInst{fixture.add(fixture.files.pendulum, swing)};
  fixture.commit();
  const auto &mesh{fixture.meshOf(waveInst)};
  CHECK(mesh.isSkinned);
  CHECK(!mesh.deforms());
  CHECK(mesh.vertsShut.empty());
  CHECK(!fixture.scene.meshInstances[waveInst].isDeforming);
  const float3 tip{1 + rig::SIN45, rig::SIN45, 0};
  CHECK(near3(mesh.verts[nearestVert(mesh, tip)].point, tip));
  const auto &arm{fixture.scene.meshInstances[armInst]};
  CHECK(!arm.isMoving);
  CHECK(near3(float3(arm.frame.objectToWorld[0]),
              float3(rig::SIN45, rig::SIN45, 0)));
}

TEST_CASE("Scene: subdivision and displacement carry the shut key") {
  RigFixture fixture{0.25f, 0.5f};
  SubdivSpec linear{};
  linear.levels = 1;
  linear.isSmooth = false;
  SubdivSpec displaced{};
  displaced.isDisplaced = true;
  const auto morphInst{fixture.add(fixture.files.morph, {}, linear)};
  const auto bumpInst{fixture.add(fixture.files.wave, {}, displaced, "bump")};
  const auto flatInst{fixture.add(fixture.files.morph, {})};
  fixture.commit();
  SUBCASE("The animated read welds a duplicated corner at both keys") {
    // Five authored corners, four after the weld, at both keys.
    const auto &mesh{fixture.meshOf(flatInst)};
    CHECK(mesh.verts.size() == 4);
    CHECK(mesh.vertsShut.size() == 4);
    CHECK(mesh.faces.size() == 2);
    for (const auto &face : mesh.faces)
      for (const auto index : face) CHECK(index < 4);
  }
  SUBCASE("A linearly subdivided morph lifts by its weights at each key") {
    const auto &mesh{fixture.meshOf(morphInst)};
    CHECK(mesh.deforms());
    CHECK(fixture.scene.meshInstances[morphInst].isDeforming);
    // The file's quad is two triangles, which the bilinear split turns
    // into six quads over eleven vertices.
    REQUIRE(mesh.verts.size() == 11);
    REQUIRE(mesh.vertsShut.size() == 11);
    // Weights (0.25, 0.125) at 0.25 s and (0.75, 0.375) at 0.75 s: the
    // lift is the first weight, the stretch of the x = 1 edge the second.
    float maxOpenX{}, maxShutX{};
    for (size_t i = 0; i < mesh.verts.size(); i++) {
      CHECK(mesh.verts[i].point.z == doctest::Approx(0.25f));
      CHECK(mesh.vertsShut[i].point.z == doctest::Approx(0.75f));
      maxOpenX = std::max(maxOpenX, mesh.verts[i].point.x);
      maxShutX = std::max(maxShutX, mesh.vertsShut[i].point.x);
    }
    CHECK(maxOpenX == doctest::Approx(1.125f));
    CHECK(maxShutX == doctest::Approx(1.375f));
  }
  SUBCASE("A displaced skin moves both keys along their normals") {
    const auto &mesh{fixture.meshOf(bumpInst)};
    REQUIRE(mesh.vertsShut.size() == mesh.verts.size());
    for (size_t i = 0; i < mesh.verts.size(); i++) {
      CHECK(mesh.verts[i].point.z == doctest::Approx(0.1f));
      CHECK(mesh.vertsShut[i].point.z == doctest::Approx(0.1f));
    }
    const float open{0.25f * PI / 2}, shut{0.75f * PI / 2};
    const float3 tip{1 + std::cos(open), std::sin(open), 0.1f};
    const auto index{nearestVert(mesh, tip)};
    CHECK(near3(mesh.verts[index].point, tip));
    CHECK(near3(mesh.vertsShut[index].point,
                float3(1 + std::cos(shut), std::sin(shut), 0.1f)));
  }
}

TEST_CASE("Scene: a hit on a deforming mesh lerps its triangle to the time") {
  RigFixture fixture{0.25f, 0.5f};
  const auto flatInst{fixture.add(fixture.files.morph, {})};
  // The same quad three units over, under a placement that also rises one
  // unit over the shutter.
  LayoutItem rising{};
  rising.fileName = fixture.files.morph;
  rising.materials.all = "paint";
  rising.objectToWorld[3] = float4(3.0f, 0.0f, 0.0f, 1.0f);
  rising.objectToWorldShut = rising.objectToWorld;
  (*rising.objectToWorldShut)[3].z = 1.0f;
  const auto risingInst{uint32_t(fixture.scene.meshInstances.size())};
  fixture.scene.add(rising);
  fixture.commit();
  const auto castDown{[&](float x, float y, float time) {
    Ray ray{float3(x, y, 5.0f), float3(0.0f, 0.0f, -1.0f), EPS, INF};
    ray.time = time;
    Hit hit{};
    REQUIRE(fixture.scene.intersect(ray, hit));
    return hit;
  }};
  // The morph quad's lift is 0.25 at open and 0.75 at shut, and the
  // stretch target tilts every normal toward +X by its weight, 0.125 at
  // open and 0.375 at shut.
  const auto normalAt{[](float weight) {
    return smdl::normalize(float3(weight, 0.0f, 1.0f - weight));
  }};
  SUBCASE("The point and the normal land between the keys") {
    for (const auto &key : {std::pair{0.0f, 0.25f}, std::pair{0.5f, 0.5f},
                            std::pair{1.0f, 0.75f}}) {
      const float u{key.first}, z{key.second};
      CAPTURE(u);
      const auto hit{castDown(0.5f, 0.5f, u)};
      CHECK(hit.instIndex == flatInst);
      CHECK(hit.time == u);
      CHECK(hit.point.z == doctest::Approx(z).epsilon(1e-4));
      CHECK(near3(hit.Ng, float3(0, 0, 1)));
      const auto expected{smdl::normalize((1.0f - u) * normalAt(0.125f) +
                                          u * normalAt(0.375f))};
      CHECK(near3(hit.normal, expected, 1e-4f));
    }
  }
  SUBCASE("The manifold geometry and the projection hit reproduce it") {
    const auto hit{castDown(0.5f, 0.5f, 0.5f)};
    const auto geometry{fixture.scene.manifoldGeometry(hit)};
    for (int i = 0; i < 3; i++) {
      CHECK(geometry.point[i] == hit.point[i]);
      CHECK(geometry.normal[i] == hit.normal[i]);
      CHECK(geometry.Ng[i] == hit.Ng[i]);
    }
    Ray ray{float3(0.5f, 0.5f, 5.0f), float3(0.0f, 0.0f, -1.0f), EPS, INF};
    ray.time = 0.5f;
    ManifoldHit projected{};
    REQUIRE(fixture.scene.intersect(ray, projected));
    CHECK(projected.vertex.face == hit.faceIndex);
    for (int i = 0; i < 3; i++)
      CHECK(projected.vertex.point[i] == hit.point[i]);
  }
  SUBCASE("Deformation composes with the placement's motion") {
    const auto &inst{fixture.scene.meshInstances[risingInst]};
    CHECK(inst.isMoving);
    CHECK(inst.isDeforming);
    for (const auto &key : {std::pair{0.0f, 0.25f}, std::pair{0.5f, 1.0f},
                            std::pair{1.0f, 1.75f}}) {
      const float u{key.first}, z{key.second};
      CAPTURE(u);
      const auto hit{castDown(3.5f, 0.5f, u)};
      CHECK(hit.instIndex == risingInst);
      CHECK(hit.point.z == doctest::Approx(z).epsilon(1e-3));
      const auto geometry{fixture.scene.manifoldGeometry(hit)};
      for (int i = 0; i < 3; i++) CHECK(geometry.point[i] == hit.point[i]);
    }
  }
}
