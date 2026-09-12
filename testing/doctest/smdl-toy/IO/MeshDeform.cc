#include "Fixtures.h"

#include <string>

#include "assimp/Importer.hpp"
#include "assimp/anim.h"
#include "assimp/postprocess.h"
#include "assimp/scene.h"

#include "smdl/Support/Error.h"
#include "smdl/Support/Strings.h"

#include "IO/MeshDeform.h"
#include "IO/MeshImport.h"
#include "RigFixtures.h"

namespace {

// The fixture files, written once per test case and read with the least
// post-processing that yields triangles and nothing that joins vertices:
// the reader's own renumbering by first use is the only reordering the
// checks face, which is why they find vertices by authored position.
class Fixture final {
public:
  Fixture() {
    const rig::Files files{rig::writeFiles(dir.path())};
    wave = files.wave;
    morph = files.morph;
    pendulum = files.pendulum;
    plain = files.plain;
  }
  [[nodiscard]] const aiScene *read(Assimp::Importer &importer,
                                    const std::string &fileName) const {
    const aiScene *assScene{importer.ReadFile(
        fileName.c_str(), aiProcess_Triangulate | aiProcess_SortByPType)};
    if (!assScene) MESSAGE(importer.GetErrorString());
    REQUIRE(assScene);
    return assScene;
  }

  TempDir dir{"toy-deform"};
  std::string wave{};
  std::string morph{};
  std::string pendulum{};
  std::string plain{};
};

[[nodiscard]] float3 translationOf(const float4x4 &xf) { return float3(xf[3]); }

/// The index of the vertex the file authored at `point`, since the reader
/// renumbers vertices by first use.
[[nodiscard]] unsigned vertexAt(const aiMesh &assMesh, const float3 &point) {
  for (unsigned i = 0; i < assMesh.mNumVertices; i++)
    if (isNear(float3(assMesh.mVertices[i].x, assMesh.mVertices[i].y,
                      assMesh.mVertices[i].z),
               point))
      return i;
  REQUIRE(false);
  return 0;
}

} // namespace

TEST_CASE("MeshDeform: the spec's key") {
  AnimationSpec spec{};
  CHECK(spec.key() == "");
  CHECK(!spec.hasClip());
  spec.isOff = true;
  CHECK(spec.key() == "off");
  spec = {};
  spec.clipName = "walk";
  spec.offset = 0.25f;
  CHECK(spec.hasClip());
  CHECK(spec.key() == "clip 'walk' offset 0.25");
  spec = {};
  spec.clipIndex = 2;
  spec.speed = 2.0f;
  spec.shouldPlayOnce = true;
  CHECK(spec.key() == "clip 2 speed 2 once");
}

TEST_CASE("MeshDeform: clip time and the tick rate") {
  // A clip built by hand, since every reader in the build sets a rate.
  aiAnimation clip{};
  clip.mDuration = 50.0;
  clip.mTicksPerSecond = 0.0;
  CHECK(ticksPerSecond(clip) == 25.0);
  AnimationSpec spec{};
  CHECK(clipTime(clip, spec, 1.0) == doctest::Approx(25.0));
  SUBCASE("Looping wraps, including backwards") {
    CHECK(clipTime(clip, spec, 3.0) == doctest::Approx(25.0));
    CHECK(clipTime(clip, spec, -0.5) == doctest::Approx(37.5));
  }
  SUBCASE("Once clamps") {
    spec.shouldPlayOnce = true;
    CHECK(clipTime(clip, spec, 3.0) == doctest::Approx(50.0));
    CHECK(clipTime(clip, spec, -0.5) == doctest::Approx(0.0));
  }
  SUBCASE("An offset and a speed shift and scale the clock") {
    spec.offset = 0.5f;
    CHECK(clipTime(clip, spec, 0.25) == doctest::Approx(18.75));
    spec.speed = 2.0f;
    CHECK(clipTime(clip, spec, 0.25) == doctest::Approx(25.0));
    spec.speed = -1.0f;
    spec.offset = 0.0f;
    CHECK(clipTime(clip, spec, 0.5) == doctest::Approx(37.5));
  }
  SUBCASE("A clip of no duration is at zero") {
    clip.mDuration = 0.0;
    CHECK(clipTime(clip, spec, 3.0) == 0.0);
  }
}

TEST_CASE("MeshDeform: resolving a clip") {
  Fixture fixture{};
  SUBCASE("A file with one clip plays it unasked") {
    Assimp::Importer importer{};
    const aiScene *assScene{fixture.read(importer, fixture.wave)};
    const std::vector<ClipInfo> clips{listClips(*assScene)};
    REQUIRE(clips.size() == 1);
    CHECK(clips[0].name == "wave");
    CHECK(clips[0].duration == doctest::Approx(1.0f));
    CHECK(clips[0].nodeChannelCount == 1);
    CHECK(clips[0].morphChannelCount == 0);
    AnimationSpec spec{};
    const aiAnimation *clip{resolveClip(*assScene, spec, fixture.wave)};
    REQUIRE(clip);
    CHECK(clip->mName == aiString("wave"));
    spec.isOff = true;
    CHECK(resolveClip(*assScene, spec, fixture.wave) == nullptr);
    spec = {};
    spec.clipName = "nope";
    CHECK_THROWS_AS((void)resolveClip(*assScene, spec, fixture.wave),
                    smdl::Error);
    spec = {};
    spec.clipIndex = 3;
    CHECK_THROWS_AS((void)resolveClip(*assScene, spec, fixture.wave),
                    smdl::Error);
  }
  SUBCASE("A file with several clips needs a choice") {
    Assimp::Importer importer{};
    const aiScene *assScene{fixture.read(importer, fixture.pendulum)};
    const std::vector<ClipInfo> clips{listClips(*assScene)};
    REQUIRE(clips.size() == 3);
    CHECK(clips[0].name == "swing");
    CHECK(clips[1].name == "hop");
    CHECK(clips[2].name == "ease");
    AnimationSpec spec{};
    CHECK_THROWS_AS((void)resolveClip(*assScene, spec, fixture.pendulum),
                    smdl::Error);
    spec.clipName = "hop";
    REQUIRE(resolveClip(*assScene, spec, fixture.pendulum) ==
            assScene->mAnimations[1]);
    spec = {};
    spec.clipIndex = 2;
    REQUIRE(resolveClip(*assScene, spec, fixture.pendulum) ==
            assScene->mAnimations[2]);
    spec.clipIndex = 5;
    CHECK_THROWS_AS((void)resolveClip(*assScene, spec, fixture.pendulum),
                    smdl::Error);
  }
  SUBCASE("A file with no clip") {
    Assimp::Importer importer{};
    const aiScene *assScene{fixture.read(importer, fixture.plain)};
    CHECK(listClips(*assScene).empty());
    AnimationSpec spec{};
    CHECK(resolveClip(*assScene, spec, fixture.plain) == nullptr);
    spec.clipName = "anything";
    CHECK_THROWS_AS((void)resolveClip(*assScene, spec, fixture.plain),
                    smdl::Error);
    CHECK(!meshDeforms(*assScene, 0, nullptr));
  }
}

TEST_CASE("MeshDeform: the pose of the node graph") {
  Fixture fixture{};
  Assimp::Importer importer{};
  const aiScene *assScene{fixture.read(importer, fixture.pendulum)};
  SUBCASE("A linear rotation channel, and the nearest key outside") {
    const aiAnimation *swing{assScene->mAnimations[0]};
    const uint32_t arm{evaluatePose(*assScene, swing, 0.0).find("arm")};
    REQUIRE(arm != INVALID_INDEX);
    const auto at{[&](double ticks) {
      return evaluatePose(*assScene, swing, ticks).nodeToFile[arm];
    }};
    CHECK_NEAR(float3(at(0.0)[0]), float3(1, 0, 0), 1e-5f);
    CHECK_NEAR(float3(at(500.0)[0]), float3(rig::SIN45, rig::SIN45, 0), 1e-5f);
    CHECK_NEAR(float3(at(1000.0)[0]), float3(0, 1, 0), 1e-5f);
    CHECK_NEAR(float3(at(-100.0)[0]), float3(1, 0, 0), 1e-5f);
    CHECK_NEAR(float3(at(2000.0)[0]), float3(0, 1, 0), 1e-5f);
    // A component the channel has no keys for stays authored.
    CHECK_NEAR(translationOf(at(500.0)), float3(0, 0, 0), 1e-5f);
  }
  SUBCASE("A step channel holds its key") {
    const aiAnimation *hop{assScene->mAnimations[1]};
    const uint32_t arm{evaluatePose(*assScene, hop, 0.0).find("arm")};
    const auto at{[&](double ticks) {
      return translationOf(evaluatePose(*assScene, hop, ticks).nodeToFile[arm]);
    }};
    CHECK_NEAR(at(0.0), float3(0, 0, 0), 1e-5f);
    CHECK_NEAR(at(500.0), float3(0, 0, 0), 1e-5f);
    CHECK_NEAR(at(999.0), float3(0, 0, 0), 1e-5f);
    CHECK_NEAR(at(1000.0), float3(0, 0, 1), 1e-5f);
  }
  SUBCASE("A cubic spline channel with zero tangents eases") {
    const aiAnimation *ease{assScene->mAnimations[2]};
    const uint32_t arm{evaluatePose(*assScene, ease, 0.0).find("arm")};
    const auto at{[&](double ticks) {
      return translationOf(
          evaluatePose(*assScene, ease, ticks).nodeToFile[arm]);
    }};
    CHECK_NEAR(at(0.0), float3(0, 0, 0), 1e-5f);
    CHECK_NEAR(at(250.0), float3(0, 0, 0.15625f), 1e-5f);
    CHECK_NEAR(at(500.0), float3(0, 0, 0.5f), 1e-5f);
    CHECK_NEAR(at(1000.0), float3(0, 0, 1), 1e-5f);
  }
  SUBCASE("No clip is the authored pose") {
    const NodePose pose{evaluatePose(*assScene, nullptr, 0.0)};
    REQUIRE(pose.nodeToFile.size() == 2);
    CHECK(pose.find("arm") == 1);
    CHECK(pose.find("elsewhere") == INVALID_INDEX);
    CHECK_NEAR(float3(pose.nodeToFile[1][0]), float3(1, 0, 0), 1e-5f);
  }
}

TEST_CASE("MeshDeform: a skinned bake") {
  Fixture fixture{};
  Assimp::Importer importer{};
  const aiScene *assScene{fixture.read(importer, fixture.wave)};
  const aiAnimation *clip{resolveClip(*assScene, {}, fixture.wave)};
  REQUIRE(clip);
  REQUIRE(assScene->mNumMeshes == 1);
  REQUIRE(assScene->mMeshes[0]->mNumVertices == 7);
  CHECK(meshDeforms(*assScene, 0, clip));
  CHECK(!meshDeforms(*assScene, 0, nullptr));
  const aiMesh &assMesh{*assScene->mMeshes[0]};
  const auto bakeAt{[&](double ticks) {
    const NodePose pose{evaluatePose(*assScene, clip, ticks)};
    return bakeMesh(*assScene, 0, pose, clip, ticks, fixture.wave);
  }};
  // Where the vertex authored at `point` lands in `bake`.
  const auto landed{[&](const MeshBake &bake, const float3 &point) {
    return bake.points[vertexAt(assMesh, point)];
  }};
  SUBCASE("The bind pose, in file space, without the node's translation") {
    const NodePose pose{evaluatePose(*assScene, clip, 0.0)};
    const uint32_t strip{pose.find("strip")};
    REQUIRE(strip != INVALID_INDEX);
    CHECK_NEAR(translationOf(pose.nodeToFile[strip]), float3(0, 0, 5), 1e-5f);
    const MeshBake bake{bakeAt(0.0)};
    CHECK(bake.isSkinned);
    REQUIRE(bake.points.size() == 7);
    REQUIRE(bake.normals.size() == 7);
    CHECK_NEAR(landed(bake, float3(0, 0, 0)), float3(0, 0, 0), 1e-5f);
    CHECK_NEAR(landed(bake, float3(2, 0, 0)), float3(2, 0, 0), 1e-5f);
    CHECK_NEAR(landed(bake, float3(5, 5, 0)), float3(5, 5, 0), 1e-5f);
    CHECK_NEAR(bake.normals[vertexAt(assMesh, float3(2, 0, 0))],
               float3(0, 0, 1), 1e-5f);
  }
  SUBCASE("At a quarter turn") {
    const MeshBake bake{bakeAt(1000.0)};
    CHECK_NEAR(landed(bake, float3(0, 0, 0)), float3(0, 0, 0), 1e-5f);
    CHECK_NEAR(landed(bake, float3(0, 1, 0)), float3(0, 1, 0), 1e-5f);
    CHECK_NEAR(landed(bake, float3(1, 0, 0)), float3(1, 0, 0), 1e-5f);
    CHECK_NEAR(landed(bake, float3(1, 1, 0)), float3(0.5f, 0.5f, 0), 1e-5f);
    CHECK_NEAR(landed(bake, float3(2, 0, 0)), float3(1, 1, 0), 1e-5f);
    CHECK_NEAR(landed(bake, float3(2, 1, 0)), float3(0, 1, 0), 1e-5f);
    CHECK_NEAR(landed(bake, float3(5, 5, 0)), float3(5, 5, 0), 1e-5f);
    for (const auto &normal : bake.normals)
      CHECK_NEAR(normal, float3(0, 0, 1), 1e-5f);
  }
  SUBCASE("Halfway, the tip is at forty-five degrees") {
    const MeshBake bake{bakeAt(500.0)};
    CHECK_NEAR(landed(bake, float3(2, 0, 0)),
               float3(1 + rig::SIN45, rig::SIN45, 0), 1e-5f);
    CHECK_NEAR(landed(bake, float3(2, 1, 0)), float3(1, rig::SIN45 * 2, 0),
               1e-5f);
  }
}

TEST_CASE("MeshDeform: a morphed bake") {
  Fixture fixture{};
  Assimp::Importer importer{};
  const aiScene *assScene{fixture.read(importer, fixture.morph)};
  REQUIRE(assScene->mNumMeshes == 1);
  REQUIRE(assScene->mMeshes[0]->mNumAnimMeshes == 2);
  const aiAnimation *clip{resolveClip(*assScene, {}, fixture.morph)};
  REQUIRE(clip);
  const aiMesh &assMesh{*assScene->mMeshes[0]};
  const unsigned corner{vertexAt(assMesh, float3(1, 0, 0))};
  const unsigned origin{vertexAt(assMesh, float3(0, 0, 0))};
  SUBCASE("The default weights apply with no clip") {
    CHECK(meshDeforms(*assScene, 0, nullptr));
    const NodePose pose{evaluatePose(*assScene, nullptr, 0.0)};
    const MeshBake bake{
        bakeMesh(*assScene, 0, pose, nullptr, 0.0, fixture.morph)};
    CHECK(!bake.isSkinned);
    // The duplicated corner is still two vertices here: the evaluator
    // bakes what the reader gives it, and the scene welds afterwards.
    REQUIRE(bake.points.size() == 5);
    CHECK_NEAR(bake.points[corner], float3(1, 0, 0.25f), 1e-5f);
    CHECK_NEAR(bake.normals[corner], float3(0, 0, 1), 1e-5f);
  }
  SUBCASE("The channel drives both targets") {
    CHECK(meshDeforms(*assScene, 0, clip));
    const auto bakeAt{[&](double ticks) {
      const NodePose pose{evaluatePose(*assScene, clip, ticks)};
      return bakeMesh(*assScene, 0, pose, clip, ticks, fixture.morph);
    }};
    const MeshBake start{bakeAt(0.0)};
    CHECK_NEAR(start.points[corner], float3(1, 0, 0), 1e-5f);
    const MeshBake half{bakeAt(500.0)};
    CHECK_NEAR(half.points[origin], float3(0, 0, 0.5f), 1e-5f);
    CHECK_NEAR(half.points[corner], float3(1.25f, 0, 0.5f), 1e-5f);
    CHECK(
        isNear(half.normals[corner], smdl::normalize(float3(0.25f, 0, 0.75f))));
    const MeshBake end{bakeAt(1000.0)};
    CHECK_NEAR(end.points[corner], float3(1.5f, 0, 1), 1e-5f);
    CHECK_NEAR(end.normals[corner], float3(rig::SIN45, 0, rig::SIN45), 1e-5f);
  }
}

TEST_CASE("MeshDeform: the listing reports clips and deforming objects") {
  Fixture fixture{};
  SUBCASE("A skinned file") {
    ObjectFileInfo info{};
    const std::vector<ObjectUsage> usage{
        importObjectUsage(fixture.wave, &info)};
    REQUIRE(info.animations.size() == 1);
    CHECK(info.animations[0].name == "wave");
    CHECK(info.animations[0].duration == doctest::Approx(1.0f));
    CHECK(info.deforms);
    REQUIRE(usage.size() == 1);
    CHECK(usage[0].path == "strip");
    CHECK(usage[0].deforms);
  }
  SUBCASE("A morphed file") {
    ObjectFileInfo info{};
    const std::vector<ObjectUsage> usage{
        importObjectUsage(fixture.morph, &info)};
    REQUIRE(info.animations.size() == 1);
    CHECK(info.animations[0].morphChannelCount == 1);
    REQUIRE(usage.size() == 1);
    CHECK(usage[0].deforms);
  }
  SUBCASE("A plain file") {
    ObjectFileInfo info{};
    const std::vector<ObjectUsage> usage{
        importObjectUsage(fixture.plain, &info)};
    CHECK(info.animations.empty());
    CHECK(!info.deforms);
    REQUIRE(usage.size() == 1);
    CHECK(!usage[0].deforms);
  }
}
