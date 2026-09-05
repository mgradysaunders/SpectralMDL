#include "IO/MeshDeform.h"
#include "IO/Assimp.h"

#include "assimp/anim.h"
#include "assimp/commonMetaData.h"
#include "assimp/mesh.h"
#include "assimp/scene.h"

#include "smdl/Support/Error.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Strings.h"

#include <algorithm>
#include <cctype>
#include <cmath>

namespace {

[[nodiscard]] float4 normalizeQuat(float4 q) noexcept {
  if (!smdl::tryNormalize(q)) return float4(0.0f, 0.0f, 0.0f, 1.0f);
  return q;
}

// Spherical linear interpolation along the shorter arc.
[[nodiscard]] float4 slerp(const float4 &a, float4 b, float t) noexcept {
  float cosTheta{dot(a, b)};
  if (cosTheta < 0) b = -b, cosTheta = -cosTheta;
  if (cosTheta > 0.9995f) return normalizeQuat((1 - t) * a + t * b);
  const float theta{std::acos(std::min(cosTheta, 1.0f))};
  const float sinTheta{std::sin(theta)};
  return (std::sin((1 - t) * theta) / sinTheta) * a +
         (std::sin(t * theta) / sinTheta) * b;
}

// The rotation matrix of a unit quaternion (x, y, z, w), by columns.
[[nodiscard]] float3x3 rotationOf(const float4 &q) noexcept {
  const float x{q.x}, y{q.y}, z{q.z}, w{q.w};
  return float3x3(
      float3(1 - 2 * (y * y + z * z), 2 * (x * y + z * w), 2 * (x * z - y * w)),
      float3(2 * (x * y - z * w), 1 - 2 * (x * x + z * z), 2 * (y * z + x * w)),
      float3(2 * (x * z + y * w), 2 * (y * z - x * w),
             1 - 2 * (x * x + y * y)));
}

// Scale, then rotate, then translate: the order assimp's channels compose in.
[[nodiscard]] float4x4 composeTRS(const float3 &t, const float4 &r,
                                  const float3 &s) noexcept {
  const auto R{rotationOf(r)};
  return float4x4(float4(s.x * R[0], 0.0f), float4(s.y * R[1], 0.0f),
                  float4(s.z * R[2], 0.0f), float4(t, 1.0f));
}

// glTF's cubic Hermite segment between two keys, `td` the segment's length in
// seconds and the tangents per second.
template <typename T>
[[nodiscard]] T hermite(const T &p0, const T &m0, const T &p1, const T &m1,
                        float td, float s) noexcept {
  const float s2{s * s}, s3{s2 * s};
  return (2 * s3 - 3 * s2 + 1) * p0 + (td * (s3 - 2 * s2 + s)) * m0 +
         (-2 * s3 + 3 * s2) * p1 + (td * (s3 - s2)) * m1;
}

// The keys of one channel as an evaluator sees them. assimp stores a
// glTF cubic spline as triplets sharing one time, in-tangent, value,
// out-tangent, so the logical keys of such a track are every third entry.
template <typename Key> class KeyTrack final {
public:
  KeyTrack(const Key *keys, unsigned count) noexcept
      : keys(keys), count(count),
        isCubic(count >= 3 && count % 3 == 0 &&
                keys[0].mInterpolation == aiAnimInterpolation_Cubic_Spline) {}

  [[nodiscard]] unsigned size() const noexcept {
    return isCubic ? count / 3 : count;
  }

  [[nodiscard]] double timeAt(unsigned i) const noexcept {
    return keys[isCubic ? 3 * i : i].mTime;
  }

  [[nodiscard]] const Key &valueAt(unsigned i) const noexcept {
    return keys[isCubic ? 3 * i + 1 : i];
  }

  [[nodiscard]] const Key &inTangentAt(unsigned i) const noexcept {
    return keys[3 * i];
  }

  [[nodiscard]] const Key &outTangentAt(unsigned i) const noexcept {
    return keys[3 * i + 2];
  }

  const Key *keys{};
  unsigned count{};
  bool isCubic{};
};

// The logical keys around `ticks`: the last at or before it and the first
// after, with the fraction between them; the same key twice at or beyond
// either end, which is the nearest-key rule outside the track's range.
class Bracket final {
public:
  unsigned i0{};
  unsigned i1{};
  float s{};
};

template <typename Key>
[[nodiscard]] Bracket bracket(const KeyTrack<Key> &track,
                              double ticks) noexcept {
  const auto n{track.size()};
  if (ticks <= track.timeAt(0)) return {0, 0, 0.0f};
  if (ticks >= track.timeAt(n - 1)) return {n - 1, n - 1, 0.0f};
  unsigned lo{0}, hi{n - 1};
  while (hi - lo > 1) {
    const unsigned mid{(lo + hi) / 2};
    if (track.timeAt(mid) <= ticks)
      lo = mid;
    else
      hi = mid;
  }
  const double t0{track.timeAt(lo)}, t1{track.timeAt(hi)};
  return {lo, hi, t1 > t0 ? float((ticks - t0) / (t1 - t0)) : 0.0f};
}

[[nodiscard]] float3 sampleVector(const aiVectorKey *keys, unsigned count,
                                  double ticks, double tps) noexcept {
  const KeyTrack<aiVectorKey> track{keys, count};
  const auto [i0, i1, s]{bracket(track, ticks)};
  const auto v0{fromAssimp(track.valueAt(i0).mValue)};
  if (i0 == i1) return v0;
  const auto v1{fromAssimp(track.valueAt(i1).mValue)};
  if (track.isCubic) {
    const float td{float((track.timeAt(i1) - track.timeAt(i0)) / tps)};
    return hermite(v0, fromAssimp(track.outTangentAt(i0).mValue), v1,
                   fromAssimp(track.inTangentAt(i1).mValue), td, s);
  }
  if (track.valueAt(i0).mInterpolation == aiAnimInterpolation_Step) return v0;
  return (1 - s) * v0 + s * v1;
}

[[nodiscard]] float4 sampleRotation(const aiQuatKey *keys, unsigned count,
                                    double ticks, double tps) noexcept {
  const KeyTrack<aiQuatKey> track{keys, count};
  const auto [i0, i1, s]{bracket(track, ticks)};
  const auto q0{normalizeQuat(fromAssimp(track.valueAt(i0).mValue))};
  if (i0 == i1) return q0;
  const auto q1{normalizeQuat(fromAssimp(track.valueAt(i1).mValue))};
  if (track.isCubic) {
    const float td{float((track.timeAt(i1) - track.timeAt(i0)) / tps)};
    return normalizeQuat(hermite(q0, fromAssimp(track.outTangentAt(i0).mValue),
                                 q1, fromAssimp(track.inTangentAt(i1).mValue),
                                 td, s));
  }
  if (track.valueAt(i0).mInterpolation == aiAnimInterpolation_Step) return q0;
  return slerp(q0, q1, s);
}

// The first node that places mesh `meshIndex`, or null.
[[nodiscard]] const aiNode *findPlacingNode(const aiNode &node,
                                            unsigned meshIndex) noexcept {
  for (unsigned i = 0; i < node.mNumMeshes; i++)
    if (node.mMeshes[i] == meshIndex) return &node;
  for (unsigned i = 0; i < node.mNumChildren; i++)
    if (auto found{findPlacingNode(*node.mChildren[i], meshIndex)})
      return found;
  return nullptr;
}

// glTF's default morph weights are the file's resting shape; every other
// format assimp reads reports 1.0 for a shape whose channel value it does
// not carry, so only glTF's static weights mean anything. The importer
// records the source extension when the reader does not name the format.
[[nodiscard]] bool isGLTF(const aiScene &assScene) {
  if (!assScene.mMetaData) return false;
  aiString format{};
  if (!assScene.mMetaData->Get(AI_METADATA_SOURCE_FORMAT, format)) return false;
  auto text{std::string(format.C_Str())};
  for (auto &ch : text) ch = char(std::tolower(uint8_t(ch)));
  return text.find("gltf") != std::string::npos ||
         text.find("glb") != std::string::npos;
}

// The clip's morph channel for a mesh: by the placing node's name, which
// is how the glTF reader names channels, then by the mesh's own name,
// which is how the FBX reader does.
[[nodiscard]] const aiMeshMorphAnim *
findMorphChannel(const aiAnimation &clip, const aiMesh &assMesh,
                 const aiNode *placingNode) noexcept {
  const aiMeshMorphAnim *byMeshName{};
  for (unsigned j = 0; j < clip.mNumMorphMeshChannels; j++) {
    const auto &channel{*clip.mMorphMeshChannels[j]};
    if (placingNode && channel.mName == placingNode->mName) return &channel;
    if (!byMeshName && channel.mName == assMesh.mName) byMeshName = &channel;
  }
  return byMeshName;
}

// The weight of every morph target of `assMesh` at `ticks`.
[[nodiscard]] std::vector<float> morphWeights(const aiScene &assScene,
                                              const aiMesh &assMesh,
                                              const aiMeshMorphAnim *channel,
                                              double ticks,
                                              std::string_view fileName) {
  auto weights{std::vector<float>(assMesh.mNumAnimMeshes, 0.0f)};
  if (!channel) {
    if (isGLTF(assScene))
      for (unsigned i = 0; i < assMesh.mNumAnimMeshes; i++)
        weights[i] = assMesh.mAnimMeshes[i]->mWeight;
    return weights;
  }
  if (channel->mNumKeys == 0) return weights;
  const auto weightsOf{[&](const aiMeshMorphKey &key) {
    auto result{std::vector<float>(assMesh.mNumAnimMeshes, 0.0f)};
    for (unsigned k = 0; k < key.mNumValuesAndWeights; k++) {
      if (key.mValues[k] >= assMesh.mNumAnimMeshes)
        throw smdl::Error(smdl::concat(
            "morph channel ", smdl::Quoted(channel->mName.C_Str()), " in ",
            smdl::QuotedPath(fileName), " keys target ", key.mValues[k],
            " of a mesh with ", assMesh.mNumAnimMeshes));
      result[key.mValues[k]] = float(key.mWeights[k]);
    }
    return result;
  }};
  const auto *keys{channel->mKeys};
  const auto n{channel->mNumKeys};
  if (ticks <= keys[0].mTime) return weightsOf(keys[0]);
  if (ticks >= keys[n - 1].mTime) return weightsOf(keys[n - 1]);
  unsigned lo{0}, hi{n - 1};
  while (hi - lo > 1) {
    const unsigned mid{(lo + hi) / 2};
    if (keys[mid].mTime <= ticks)
      lo = mid;
    else
      hi = mid;
  }
  const auto w0{weightsOf(keys[lo])};
  const auto w1{weightsOf(keys[hi])};
  const double t0{keys[lo].mTime}, t1{keys[hi].mTime};
  const float s{t1 > t0 ? float((ticks - t0) / (t1 - t0)) : 0.0f};
  for (size_t i = 0; i < weights.size(); i++)
    weights[i] = (1 - s) * w0[i] + s * w1[i];
  return weights;
}

// Every clip on its own line, for the errors that have to name them.
[[nodiscard]] std::string clipListing(const aiScene &assScene) {
  auto listing{std::string()};
  for (unsigned i = 0; i < assScene.mNumAnimations; i++) {
    const auto &clip{*assScene.mAnimations[i]};
    listing += smdl::concat(
        "\n  ", i, ": ", smdl::Quoted(clip.mName.C_Str()), " (",
        smdl::Brief(clip.mDuration / ticksPerSecond(clip), 3), " s)");
  }
  return listing;
}

} // namespace

std::string AnimationSpec::key() const {
  if (off) return "off";
  auto parts{std::vector<std::string>()};
  if (!clipName.empty())
    parts.push_back(smdl::concat("clip ", smdl::Quoted(clipName)));
  else if (clipIndex != INVALID_INDEX)
    parts.push_back(smdl::concat("clip ", clipIndex));
  if (offset != 0)
    parts.push_back(smdl::concat("offset ", smdl::Precise(offset)));
  if (speed != 1) parts.push_back(smdl::concat("speed ", smdl::Precise(speed)));
  if (once) parts.push_back("once");
  auto result{std::string()};
  for (const auto &part : parts) {
    if (!result.empty()) result += ' ';
    result += part;
  }
  return result;
}

std::vector<ClipInfo> listClips(const aiScene &assScene) {
  auto clips{std::vector<ClipInfo>()};
  clips.reserve(assScene.mNumAnimations);
  for (unsigned i = 0; i < assScene.mNumAnimations; i++) {
    const auto &clip{*assScene.mAnimations[i]};
    auto &info{clips.emplace_back()};
    info.name = clip.mName.C_Str();
    info.duration = float(clip.mDuration / ticksPerSecond(clip));
    info.nodeChannelCount = clip.mNumChannels;
    info.morphChannelCount = clip.mNumMorphMeshChannels;
  }
  return clips;
}

const aiAnimation *resolveClip(const aiScene &assScene,
                               const AnimationSpec &spec,
                               std::string_view fileName) {
  if (spec.off) return nullptr;
  if (assScene.mNumAnimations == 0) {
    if (spec.hasClip())
      throw smdl::Error(smdl::concat("'animation' names a clip, but ",
                                     smdl::QuotedPath(fileName),
                                     " carries none"));
    return nullptr;
  }
  const aiAnimation *clip{};
  if (!spec.clipName.empty()) {
    for (unsigned i = 0; i < assScene.mNumAnimations && !clip; i++)
      if (std::string_view(assScene.mAnimations[i]->mName.C_Str()) ==
          spec.clipName)
        clip = assScene.mAnimations[i];
    if (!clip)
      throw smdl::Error(
          smdl::concat("no clip named ", smdl::Quoted(spec.clipName), " in ",
                       smdl::QuotedPath(fileName),
                       ", which carries:", clipListing(assScene)));
  } else if (spec.clipIndex != INVALID_INDEX) {
    if (spec.clipIndex >= assScene.mNumAnimations)
      throw smdl::Error(smdl::concat(
          "no clip ", spec.clipIndex, " in ", smdl::QuotedPath(fileName),
          ", which carries:", clipListing(assScene)));
    clip = assScene.mAnimations[spec.clipIndex];
  } else if (assScene.mNumAnimations == 1) {
    clip = assScene.mAnimations[0];
  } else {
    throw smdl::Error(smdl::concat(
        smdl::QuotedPath(fileName), " carries ", assScene.mNumAnimations,
        " clips; choose one with 'animation \"<name>\"' or 'animation "
        "<index>':",
        clipListing(assScene)));
  }
  if (!(clip->mTicksPerSecond > 0))
    SMDL_LOG_WARN("Clip ", smdl::Quoted(clip->mName.C_Str()), " in ",
                  smdl::QuotedPath(fileName),
                  " has no tick rate; assuming 25 ticks per second.");
  return clip;
}

double ticksPerSecond(const aiAnimation &clip) noexcept {
  return clip.mTicksPerSecond > 0 ? clip.mTicksPerSecond : 25.0;
}

double clipTime(const aiAnimation &clip, const AnimationSpec &spec,
                double seconds) noexcept {
  const double tps{ticksPerSecond(clip)};
  const double duration{clip.mDuration / tps};
  if (!(duration > 0)) return 0.0;
  double tau{double(spec.offset) + double(spec.speed) * seconds};
  if (spec.once) {
    tau = std::clamp(tau, 0.0, duration);
  } else {
    tau = std::fmod(tau, duration);
    if (tau < 0) tau += duration;
  }
  return tau * tps;
}

uint32_t NodePose::find(std::string_view name) const {
  const auto entry{indexByName.find(std::string(name))};
  return entry == indexByName.end() ? INVALID_INDEX : entry->second;
}

namespace {

void walkPose(
    const aiNode &node, const float4x4 &parentXf,
    const std::unordered_map<std::string_view, const aiNodeAnim *> &channels,
    double ticks, double tps, NodePose &pose) {
  auto local{float4x4()};
  if (const auto entry{channels.find(node.mName.C_Str())};
      entry != channels.end()) {
    const auto &channel{*entry->second};
    // A channel replaces the whole local transform, so a component it has
    // no keys for comes from the authored transform, decomposed.
    aiVector3D scaling{}, position{};
    aiQuaternion rotation{};
    node.mTransformation.Decompose(scaling, rotation, position);
    const auto t{channel.mNumPositionKeys > 0
                     ? sampleVector(channel.mPositionKeys,
                                    channel.mNumPositionKeys, ticks, tps)
                     : fromAssimp(position)};
    const auto r{channel.mNumRotationKeys > 0
                     ? sampleRotation(channel.mRotationKeys,
                                      channel.mNumRotationKeys, ticks, tps)
                     : normalizeQuat(fromAssimp(rotation))};
    const auto s{channel.mNumScalingKeys > 0
                     ? sampleVector(channel.mScalingKeys,
                                    channel.mNumScalingKeys, ticks, tps)
                     : fromAssimp(scaling)};
    local = composeTRS(t, r, s);
  } else {
    local = fromAssimp(node.mTransformation);
  }
  const auto xf{parentXf * local};
  const auto index{uint32_t(pose.nodeToFile.size())};
  pose.nodeToFile.push_back(xf);
  pose.indexByName.try_emplace(node.mName.C_Str(), index);
  for (unsigned i = 0; i < node.mNumChildren; i++)
    walkPose(*node.mChildren[i], xf, channels, ticks, tps, pose);
}

} // namespace

NodePose evaluatePose(const aiScene &assScene, const aiAnimation *clip,
                      double ticks) {
  auto channels{std::unordered_map<std::string_view, const aiNodeAnim *>()};
  if (clip)
    for (unsigned i = 0; i < clip->mNumChannels; i++)
      channels.try_emplace(clip->mChannels[i]->mNodeName.C_Str(),
                           clip->mChannels[i]);
  auto pose{NodePose()};
  if (assScene.mRootNode)
    walkPose(*assScene.mRootNode, float4x4(1.0f), channels, ticks,
             clip ? ticksPerSecond(*clip) : 1.0, pose);
  return pose;
}

bool meshDeforms(const aiScene &assScene, uint32_t meshIndex,
                 const aiAnimation *clip) {
  const auto &assMesh{*assScene.mMeshes[meshIndex]};
  if (clip && assMesh.HasBones()) return true;
  if (assMesh.mNumAnimMeshes == 0) return false;
  if (clip && findMorphChannel(*clip, assMesh,
                               findPlacingNode(*assScene.mRootNode, meshIndex)))
    return true;
  if (isGLTF(assScene))
    for (unsigned i = 0; i < assMesh.mNumAnimMeshes; i++)
      if (assMesh.mAnimMeshes[i]->mWeight != 0) return true;
  return false;
}

MeshBake bakeMesh(const aiScene &assScene, uint32_t meshIndex,
                  const NodePose &pose, const aiAnimation *clip, double ticks,
                  std::string_view fileName) {
  const auto &assMesh{*assScene.mMeshes[meshIndex]};
  const auto numVerts{assMesh.mNumVertices};
  auto bake{MeshBake()};
  bake.points.resize(numVerts);
  for (unsigned i = 0; i < numVerts; i++)
    bake.points[i] = fromAssimp(assMesh.mVertices[i]);
  if (assMesh.mNormals) {
    bake.normals.resize(numVerts);
    for (unsigned i = 0; i < numVerts; i++)
      bake.normals[i] = fromAssimp(assMesh.mNormals[i]);
  }
  if (assMesh.mTangents) {
    bake.tangents.resize(numVerts);
    for (unsigned i = 0; i < numVerts; i++)
      bake.tangents[i] = fromAssimp(assMesh.mTangents[i]);
  }
  if (assMesh.mNumAnimMeshes > 0) {
    const auto *channel{
        clip ? findMorphChannel(*clip, assMesh,
                                findPlacingNode(*assScene.mRootNode, meshIndex))
             : nullptr};
    const auto weights{
        morphWeights(assScene, assMesh, channel, ticks, fileName)};
    auto anyMoved{false};
    for (unsigned t = 0; t < assMesh.mNumAnimMeshes; t++) {
      const float w{weights[t]};
      if (w == 0) continue;
      const auto &target{*assMesh.mAnimMeshes[t]};
      if (target.mNumVertices != numVerts)
        throw smdl::Error(smdl::concat("morph target ", t, " of mesh ",
                                       smdl::Quoted(assMesh.mName.C_Str()),
                                       " in ", smdl::QuotedPath(fileName),
                                       " has ", target.mNumVertices,
                                       " vertices, the mesh ", numVerts));
      anyMoved = true;
      // Relative to the base the file stores, whatever this bake has
      // already accumulated from the other targets.
      if (target.mVertices)
        for (unsigned i = 0; i < numVerts; i++)
          bake.points[i] += w * (fromAssimp(target.mVertices[i]) -
                                 fromAssimp(assMesh.mVertices[i]));
      if (target.mNormals && !bake.normals.empty())
        for (unsigned i = 0; i < numVerts; i++)
          bake.normals[i] += w * (fromAssimp(target.mNormals[i]) -
                                  fromAssimp(assMesh.mNormals[i]));
      if (target.mTangents && !bake.tangents.empty())
        for (unsigned i = 0; i < numVerts; i++)
          bake.tangents[i] += w * (fromAssimp(target.mTangents[i]) -
                                   fromAssimp(assMesh.mTangents[i]));
    }
    if (anyMoved) {
      for (unsigned i = 0; i < numVerts; i++) {
        if (!bake.normals.empty() && !smdl::tryNormalize(bake.normals[i]))
          bake.normals[i] = fromAssimp(assMesh.mNormals[i]);
        if (!bake.tangents.empty() && !smdl::tryNormalize(bake.tangents[i]))
          bake.tangents[i] = fromAssimp(assMesh.mTangents[i]);
      }
    }
  }
  if (clip && assMesh.HasBones()) {
    // The weights vertex-major, since assimp stores them bone-major.
    auto offsets{std::vector<uint32_t>(numVerts + 1, 0)};
    for (unsigned b = 0; b < assMesh.mNumBones; b++) {
      const auto &bone{*assMesh.mBones[b]};
      for (unsigned k = 0; k < bone.mNumWeights; k++) {
        if (bone.mWeights[k].mVertexId >= numVerts)
          throw smdl::Error(smdl::concat(
              "bone ", smdl::Quoted(bone.mName.C_Str()), " of mesh ",
              smdl::Quoted(assMesh.mName.C_Str()), " in ",
              smdl::QuotedPath(fileName), " weights vertex ",
              bone.mWeights[k].mVertexId, " of ", numVerts));
        offsets[bone.mWeights[k].mVertexId + 1]++;
      }
    }
    for (unsigned i = 0; i < numVerts; i++) offsets[i + 1] += offsets[i];
    class Influence final {
    public:
      uint32_t bone{};
      float weight{};
    };
    auto influences{std::vector<Influence>(offsets[numVerts])};
    auto cursor{std::vector<uint32_t>(offsets.begin(), offsets.end() - 1)};
    auto boneXfs{std::vector<float4x4>(assMesh.mNumBones)};
    for (unsigned b = 0; b < assMesh.mNumBones; b++) {
      const auto &bone{*assMesh.mBones[b]};
      const auto nodeIndex{pose.find(bone.mName.C_Str())};
      if (nodeIndex == INVALID_INDEX)
        throw smdl::Error(smdl::concat(
            "bone ", smdl::Quoted(bone.mName.C_Str()), " of mesh ",
            smdl::Quoted(assMesh.mName.C_Str()), " in ",
            smdl::QuotedPath(fileName), " names no node in the file"));
      boneXfs[b] = pose.nodeToFile[nodeIndex] * fromAssimp(bone.mOffsetMatrix);
      for (unsigned k = 0; k < bone.mNumWeights; k++)
        influences[cursor[bone.mWeights[k].mVertexId]++] = {
            b, bone.mWeights[k].mWeight};
    }
    for (unsigned i = 0; i < numVerts; i++) {
      float weightSum{};
      for (auto k = offsets[i]; k < offsets[i + 1]; k++)
        weightSum += influences[k].weight;
      if (!(weightSum > 0)) continue;
      auto xf{float4x4()};
      for (auto k = offsets[i]; k < offsets[i + 1]; k++) {
        const float w{influences[k].weight / weightSum};
        const auto &boneXf{boneXfs[influences[k].bone]};
        for (size_t j = 0; j < 4; j++) xf[j] += w * boneXf[j];
      }
      bake.points[i] = transformPoint(xf, bake.points[i]);
      const auto axis0{float3(xf[0])}, axis1{float3(xf[1])},
          axis2{float3(xf[2])};
      if (!bake.normals.empty()) {
        // The cofactor matrix, `det(A) A^-T` without forming either
        // factor, as `InstanceFrame` builds it.
        auto normal{float3x3(cross(axis1, axis2), cross(axis2, axis0),
                             cross(axis0, axis1)) *
                    bake.normals[i]};
        if (smdl::tryNormalize(normal)) bake.normals[i] = normal;
      }
      if (!bake.tangents.empty()) {
        auto tangent{float3x3(axis0, axis1, axis2) * bake.tangents[i]};
        if (smdl::tryNormalize(tangent)) bake.tangents[i] = tangent;
      }
    }
    bake.isSkinned = true;
  }
  return bake;
}
