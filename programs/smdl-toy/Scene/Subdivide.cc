#include "Scene/Subdivide.h"

#include <memory>
#include <unordered_map>
#include <utility>

#include "opensubdiv/far/primvarRefiner.h"
#include "opensubdiv/far/topologyDescriptor.h"
#include "opensubdiv/far/topologyRefiner.h"
#include "opensubdiv/far/topologyRefinerFactory.h"

#include "smdl/Support/Logger.h"

namespace {

// OpenSubdiv interpolates through value types exposing this protocol.
// `float3` has no compound assignment, hence the spelled-out sums.
struct OsdPoint final {
  void Clear(void * = nullptr) { value = {}; }
  void AddWithWeight(const OsdPoint &other, float weight) {
    value = value + weight * other.value;
  }
  float3 value{};
};

struct OsdUV final {
  void Clear(void * = nullptr) { value = {}; }
  void AddWithWeight(const OsdUV &other, float weight) {
    value = value + weight * other.value;
  }
  float2 value{};
};

// The fallback when OpenSubdiv rejects the topology: fan triangulate the
// base polygons as they are, so a hostile mesh renders unsubdivided
// rather than not at all. Normals and tangents are left for the caller.
void fanTriangulate(Mesh &mesh) {
  mesh.verts.resize(mesh.basePoints.size());
  for (size_t i = 0; i < mesh.basePoints.size(); i++) {
    mesh.verts[i].point = mesh.basePoints[i];
    if (i < mesh.baseTexcoords.size())
      mesh.verts[i].texcoord = mesh.baseTexcoords[i];
  }
  size_t corner{};
  for (const auto count : mesh.baseFaceCounts) {
    for (size_t j = 2; j < count; j++)
      mesh.faces.push_back({mesh.baseIndices[corner],
                            mesh.baseIndices[corner + j - 1],
                            mesh.baseIndices[corner + j]});
    corner += count;
  }
}

} // namespace

bool subdivideMesh(Mesh &mesh) {
  using namespace OpenSubdiv;
  const auto &spec{mesh.subdiv};
  SMDL_SANITY_CHECK(spec.levels > 0);
  const bool isLoop{spec.scheme == SubdivSpec::Scheme::LOOP};
  // Loop refines triangles into triangles and OpenSubdiv rejects
  // anything else outright. The import triangulates for it, so this
  // should be unreachable; check anyway, because the alternative is
  // OpenSubdiv's own error handler printing to stderr from inside
  // `Factory::Create` with no mention of which mesh it meant.
  if (isLoop) {
    for (const auto count : mesh.baseFaceCounts) {
      if (count != 3) {
        SMDL_LOG_WARN("'subdivide ", spec.levels,
                      " loop' needs triangles, but a mesh has a face with ",
                      count, " vertices; rendering it unsubdivided instead.");
        fanTriangulate(mesh);
        return false;
      }
    }
  }
  // Weld the imported vertices by exact position bits into the
  // topological vertices subdivision refines, so that copies split to
  // carry different UVs (texture seams) subdivide as one surface. See
  // `positionKey()` for why exactness is the point.
  auto weldOf{std::vector<int>(mesh.basePoints.size())};
  auto weldedPoints{std::vector<float3>()};
  {
    auto groups{std::unordered_map<std::array<uint32_t, 3>, int, WeldHash>()};
    groups.reserve(mesh.basePoints.size());
    for (size_t i = 0; i < mesh.basePoints.size(); i++) {
      auto [entry, isNew]{groups.try_emplace(positionKey(mesh.basePoints[i]),
                                             int(weldedPoints.size()))};
      if (isNew) weldedPoints.push_back(mesh.basePoints[i]);
      weldOf[i] = entry->second;
    }
  }
  const bool hasUVs{!mesh.baseTexcoords.empty()};
  auto faceCounts{
      std::vector<int>(mesh.baseFaceCounts.begin(), mesh.baseFaceCounts.end())};
  auto vertIndices{std::vector<int>()};
  vertIndices.reserve(mesh.baseIndices.size());
  for (const auto index : mesh.baseIndices)
    vertIndices.push_back(weldOf[index]);
  // The UV channel indexes the imported vertices directly: the importer
  // welded identical (position, UV) pairs, so distinct imported vertices
  // at one position differ in UV, which is exactly the face-varying
  // seam structure.
  auto uvIndices{
      std::vector<int>(mesh.baseIndices.begin(), mesh.baseIndices.end())};
  auto desc{Far::TopologyDescriptor{}};
  desc.numVertices = int(weldedPoints.size());
  desc.numFaces = int(faceCounts.size());
  desc.numVertsPerFace = faceCounts.data();
  desc.vertIndicesPerFace = vertIndices.data();
  auto uvChannel{Far::TopologyDescriptor::FVarChannel{}};
  if (hasUVs) {
    uvChannel.numValues = int(mesh.baseTexcoords.size());
    uvChannel.valueIndices = uvIndices.data();
    desc.numFVarChannels = 1;
    desc.fvarChannels = &uvChannel;
  }
  // Corners are kept whatever the scheme (an open ground plane must not
  // round away). Smooth refinement locks UV seams and boundaries but
  // refines UVs smoothly in the interior; linear refinement leaves
  // everything, UVs included, exactly where linear interpolation puts
  // it.
  auto sdcOptions{Sdc::Options{}};
  sdcOptions.SetVtxBoundaryInterpolation(
      Sdc::Options::VTX_BOUNDARY_EDGE_AND_CORNER);
  sdcOptions.SetFVarLinearInterpolation(
      spec.smooth ? Sdc::Options::FVAR_LINEAR_BOUNDARIES
                  : Sdc::Options::FVAR_LINEAR_ALL);
  // The 2x2 of split and smoothing, which OpenSubdiv spells with three
  // schemes rather than four: Catmull-Clark and bilinear split faces
  // into quads identically and differ only in where they put the
  // vertices, so bilinear *is* the unsmoothed quad scheme. Loop has no
  // such counterpart, and stays Loop for the unsmoothed triangle split;
  // `varyingPoints` below is what takes the smoothing back out of it.
  const auto sdcScheme{isLoop        ? Sdc::SCHEME_LOOP
                       : spec.smooth ? Sdc::SCHEME_CATMARK
                                     : Sdc::SCHEME_BILINEAR};
  using Factory = Far::TopologyRefinerFactory<Far::TopologyDescriptor>;
  auto refiner{std::unique_ptr<Far::TopologyRefiner>(
      Factory::Create(desc, Factory::Options(sdcScheme, sdcOptions)))};
  if (!refiner) {
    SMDL_LOG_WARN("OpenSubdiv rejected the topology of a mesh with ",
                  faceCounts.size(),
                  " faces; rendering it unsubdivided instead.");
    fanTriangulate(mesh);
    return false;
  }
  auto refineOptions{Far::TopologyRefiner::UniformOptions(int(spec.levels))};
  refineOptions.fullTopologyInLastLevel = true;
  refiner->RefineUniform(refineOptions);
  const auto primvar{Far::PrimvarRefiner(*refiner)};
  // Unsmoothed Loop refinement asks for something no scheme computes:
  // Loop's topological split with the vertices left alone. Positions
  // ride the varying channel to get it, which is linear by definition
  // (the midpoint of the parent edge, a copy of the parent vertex) on
  // whatever topology the scheme refined. The quad half of the 2x2 does
  // not need this, because SCHEME_BILINEAR already applies those very
  // masks as its own.
  const bool varyingPoints{isLoop && !spec.smooth};
  // Interpolate positions level by level through one flat buffer, the
  // last level's block last.
  auto points{std::vector<OsdPoint>(size_t(refiner->GetNumVerticesTotal()))};
  for (size_t i = 0; i < weldedPoints.size(); i++)
    points[i].value = weldedPoints[i];
  auto *srcPoint{points.data()};
  for (int level = 1; level <= int(spec.levels); level++) {
    auto *dstPoint{srcPoint + refiner->GetLevel(level - 1).GetNumVertices()};
    if (varyingPoints) {
      primvar.InterpolateVarying(level, srcPoint, dstPoint);
    } else {
      primvar.Interpolate(level, srcPoint, dstPoint);
    }
    srcPoint = dstPoint;
  }
  // And likewise the face-varying UVs.
  auto uvs{std::vector<OsdUV>()};
  auto *srcUV{static_cast<OsdUV *>(nullptr)};
  if (hasUVs) {
    uvs.resize(size_t(refiner->GetNumFVarValuesTotal(0)));
    for (size_t i = 0; i < mesh.baseTexcoords.size(); i++)
      uvs[i].value = mesh.baseTexcoords[i];
    srcUV = uvs.data();
    for (int level = 1; level <= int(spec.levels); level++) {
      auto *dstUV{srcUV + refiner->GetLevel(level - 1).GetNumFVarValues(0)};
      primvar.InterpolateFaceVarying(level, srcUV, dstUV, 0);
      srcUV = dstUV;
    }
  }
  const auto &lastLevel{refiner->GetLevel(int(spec.levels))};
  const auto numFineVerts{size_t(lastLevel.GetNumVertices())};
  // Smooth refinement additionally snaps the last level to the limit
  // surface, whose first derivatives are exact normals; that is what
  // "smooth" means, and it is what every DCC displays. The limit UVs
  // pair with the limit positions for the same reason.
  auto limitPoints{std::vector<OsdPoint>()};
  auto limitDu{std::vector<OsdPoint>()};
  auto limitDv{std::vector<OsdPoint>()};
  auto limitUVs{std::vector<OsdUV>()};
  auto haveLimitNormals{false};
  if (spec.smooth) {
    limitPoints.resize(numFineVerts);
    limitDu.resize(numFineVerts);
    limitDv.resize(numFineVerts);
    auto *dstPoints{limitPoints.data()};
    auto *dstDu{limitDu.data()};
    auto *dstDv{limitDv.data()};
    primvar.Limit(srcPoint, dstPoints, dstDu, dstDv);
    haveLimitNormals = true;
    if (hasUVs) {
      limitUVs.resize(size_t(lastLevel.GetNumFVarValues(0)));
      auto *dstUVs{limitUVs.data()};
      primvar.LimitFaceVarying(srcUV, dstUVs, 0);
      srcUV = limitUVs.data();
    }
    srcPoint = limitPoints.data();
  }
  // Emit the unique (vertex, UV) pairs of the last level as the final
  // vertices, and split its quads into triangles. A vertex on a texture
  // seam appears once per distinct UV, exactly as it did coming in.
  // Output indices number in first-encounter order, so the result is
  // deterministic regardless of the hashing underneath.
  auto outIndex{std::unordered_map<std::pair<int, int>, uint32_t, WeldHash>()};
  outIndex.reserve(numFineVerts);
  auto emit{[&](int vertex, int uv) {
    auto [entry, isNew]{outIndex.try_emplace(std::make_pair(vertex, uv),
                                             uint32_t(mesh.verts.size()))};
    if (isNew) {
      auto &vert{mesh.verts.emplace_back()};
      vert.point = srcPoint[vertex].value;
      if (uv >= 0) vert.texcoord = srcUV[uv].value;
      if (haveLimitNormals) {
        const auto normal{smdl::cross(limitDu[size_t(vertex)].value,
                                      limitDv[size_t(vertex)].value)};
        const auto len{smdl::length(normal)};
        // A degenerate limit derivative (an unlucky extraordinary
        // point) poisons the whole mesh back to geometric normals
        // rather than shading one vertex with a lie.
        if (len > 0) {
          vert.normal = normal / len;
        } else {
          haveLimitNormals = false;
        }
      }
    }
    return entry->second;
  }};
  mesh.verts.reserve(numFineVerts);
  mesh.faces.reserve(size_t(lastLevel.GetNumFaces()) * (isLoop ? 1 : 2));
  for (int face = 0; face < lastLevel.GetNumFaces(); face++) {
    const auto fVerts{lastLevel.GetFaceVertices(face)};
    const auto fUVs{hasUVs ? lastLevel.GetFaceFVarValues(face, 0)
                           : Far::ConstIndexArray()};
    auto corner{[&](int j) { return emit(fVerts[j], hasUVs ? fUVs[j] : -1); }};
    // Every refined face is a quad under Catmull-Clark and bilinear and
    // a triangle under Loop, but fan out generically so that an
    // unexpected size cannot misindex.
    for (int j = 2; j < fVerts.size(); j++)
      mesh.faces.push_back({corner(0), corner(j - 1), corner(j)});
  }
  return haveLimitNormals;
}
