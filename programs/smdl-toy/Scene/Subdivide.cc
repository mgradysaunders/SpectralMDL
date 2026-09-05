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

// The face-varying corner attributes, UV and vertex color, refined
// together through one channel: both index the imported vertices, which
// the importer welded on position, UV, and color alike, so they share the
// seam structure and one set of indices refines both. An absent half
// rides along as zero, so a mesh with neither UVs nor colors has no
// channel and one with either has exactly this one.
struct OsdCorner final {
  void Clear(void * = nullptr) {
    uv = {};
    color = {};
  }
  void AddWithWeight(const OsdCorner &other, float weight) {
    uv = uv + weight * other.uv;
    color = color + weight * other.color;
  }
  float2 uv{};
  float4 color{};
};

// The fallback when OpenSubdiv rejects the topology: fan triangulate the
// base polygons as they are, so a hostile mesh renders unsubdivided
// rather than not at all. Normals and tangents are left for the caller.
void fanTriangulate(Mesh &mesh) {
  const auto fill{
      [&](std::vector<Mesh::Vert> &verts, const std::vector<float3> &points) {
        verts.resize(points.size());
        for (size_t i = 0; i < points.size(); i++) {
          verts[i].point = points[i];
          if (i < mesh.baseTexcoords.size())
            verts[i].texcoord = mesh.baseTexcoords[i];
        }
      }};
  fill(mesh.verts, mesh.basePoints);
  if (!mesh.basePointsShut.empty()) fill(mesh.vertsShut, mesh.basePointsShut);
  if (!mesh.baseColors.empty()) mesh.colors = mesh.baseColors;
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
  // The shut key of a deforming mesh rides the same weld, by the open
  // key's positions: a group takes the shut point of its first member.
  const bool hasShut{!mesh.basePointsShut.empty()};
  auto weldOf{std::vector<int>(mesh.basePoints.size())};
  auto weldedPoints{std::vector<float3>()};
  auto weldedPointsShut{std::vector<float3>()};
  {
    auto groups{std::unordered_map<std::array<uint32_t, 3>, int, WeldHash>()};
    groups.reserve(mesh.basePoints.size());
    for (size_t i = 0; i < mesh.basePoints.size(); i++) {
      auto [entry, isNew]{groups.try_emplace(positionKey(mesh.basePoints[i]),
                                             int(weldedPoints.size()))};
      if (isNew) {
        weldedPoints.push_back(mesh.basePoints[i]);
        if (hasShut) weldedPointsShut.push_back(mesh.basePointsShut[i]);
      }
      weldOf[i] = entry->second;
    }
  }
  const bool hasUVs{!mesh.baseTexcoords.empty()};
  const bool hasColors{!mesh.baseColors.empty()};
  const bool hasCorners{hasUVs || hasColors};
  auto faceCounts{
      std::vector<int>(mesh.baseFaceCounts.begin(), mesh.baseFaceCounts.end())};
  auto vertIndices{std::vector<int>()};
  vertIndices.reserve(mesh.baseIndices.size());
  for (const auto index : mesh.baseIndices)
    vertIndices.push_back(weldOf[index]);
  // The corner channel indexes the imported vertices directly: the
  // importer welded identical (position, UV, color) tuples, so distinct
  // imported vertices at one position differ in UV or color, which is
  // exactly the face-varying seam structure.
  auto cornerIndices{
      std::vector<int>(mesh.baseIndices.begin(), mesh.baseIndices.end())};
  auto desc{Far::TopologyDescriptor{}};
  desc.numVertices = int(weldedPoints.size());
  desc.numFaces = int(faceCounts.size());
  desc.numVertsPerFace = faceCounts.data();
  desc.vertIndicesPerFace = vertIndices.data();
  auto cornerChannel{Far::TopologyDescriptor::FVarChannel{}};
  if (hasCorners) {
    cornerChannel.numValues = int(mesh.basePoints.size());
    cornerChannel.valueIndices = cornerIndices.data();
    desc.numFVarChannels = 1;
    desc.fvarChannels = &cornerChannel;
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
      spec.isSmooth ? Sdc::Options::FVAR_LINEAR_BOUNDARIES
                    : Sdc::Options::FVAR_LINEAR_ALL);
  // The 2x2 of split and smoothing, which OpenSubdiv spells with three
  // schemes rather than four: Catmull-Clark and bilinear split faces
  // into quads identically and differ only in where they put the
  // vertices, so bilinear *is* the unsmoothed quad scheme. Loop has no
  // such counterpart, and stays Loop for the unsmoothed triangle split;
  // `varyingPoints` below is what takes the smoothing back out of it.
  const auto sdcScheme{isLoop          ? Sdc::SCHEME_LOOP
                       : spec.isSmooth ? Sdc::SCHEME_CATMARK
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
  const bool varyingPoints{isLoop && !spec.isSmooth};
  // Interpolate positions level by level through one flat buffer, the
  // last level's block last; the shut key of a deforming mesh goes
  // through the same refiner, so the two keys share every index.
  const auto refinePoints{[&](const std::vector<float3> &welded,
                              std::vector<OsdPoint> &buffer) -> OsdPoint * {
    buffer.resize(size_t(refiner->GetNumVerticesTotal()));
    for (size_t i = 0; i < welded.size(); i++) buffer[i].value = welded[i];
    auto *src{buffer.data()};
    for (int level = 1; level <= int(spec.levels); level++) {
      auto *dst{src + refiner->GetLevel(level - 1).GetNumVertices()};
      if (varyingPoints) {
        primvar.InterpolateVarying(level, src, dst);
      } else {
        primvar.Interpolate(level, src, dst);
      }
      src = dst;
    }
    return src;
  }};
  auto points{std::vector<OsdPoint>()};
  auto *srcPoint{refinePoints(weldedPoints, points)};
  auto pointsShut{std::vector<OsdPoint>()};
  auto *srcPointShut{hasShut ? refinePoints(weldedPointsShut, pointsShut)
                             : nullptr};
  // And likewise the face-varying corner attributes.
  auto corners{std::vector<OsdCorner>()};
  auto *srcCorner{static_cast<OsdCorner *>(nullptr)};
  if (hasCorners) {
    corners.resize(size_t(refiner->GetNumFVarValuesTotal(0)));
    for (size_t i = 0; i < mesh.basePoints.size(); i++) {
      if (hasUVs) corners[i].uv = mesh.baseTexcoords[i];
      if (hasColors) corners[i].color = mesh.baseColors[i];
    }
    srcCorner = corners.data();
    for (int level = 1; level <= int(spec.levels); level++) {
      auto *dstCorner{srcCorner +
                      refiner->GetLevel(level - 1).GetNumFVarValues(0)};
      primvar.InterpolateFaceVarying(level, srcCorner, dstCorner, 0);
      srcCorner = dstCorner;
    }
  }
  const auto &lastLevel{refiner->GetLevel(int(spec.levels))};
  const auto numFineVerts{size_t(lastLevel.GetNumVertices())};
  // Smooth refinement additionally snaps the last level to the limit
  // surface, whose first derivatives are exact normals; that is what
  // "smooth" means, and it is what every DCC displays. The limit corner
  // attributes pair with the limit positions for the same reason.
  class LimitKey final {
  public:
    std::vector<OsdPoint> points{};
    std::vector<OsdPoint> du{};
    std::vector<OsdPoint> dv{};
  };
  auto limitOpen{LimitKey()};
  auto limitShut{LimitKey()};
  auto limitCorners{std::vector<OsdCorner>()};
  auto haveLimitNormals{false};
  if (spec.isSmooth) {
    const auto limitKey{[&](OsdPoint *&src, LimitKey &limit) {
      limit.points.resize(numFineVerts);
      limit.du.resize(numFineVerts);
      limit.dv.resize(numFineVerts);
      auto *dstPoints{limit.points.data()};
      auto *dstDu{limit.du.data()};
      auto *dstDv{limit.dv.data()};
      primvar.Limit(src, dstPoints, dstDu, dstDv);
      src = limit.points.data();
    }};
    limitKey(srcPoint, limitOpen);
    if (hasShut) limitKey(srcPointShut, limitShut);
    haveLimitNormals = true;
    if (hasCorners) {
      limitCorners.resize(size_t(lastLevel.GetNumFVarValues(0)));
      auto *dstCorners{limitCorners.data()};
      primvar.LimitFaceVarying(srcCorner, dstCorners, 0);
      srcCorner = limitCorners.data();
    }
  }
  // Emit the unique (vertex, corner) pairs of the last level as the final
  // vertices, and split its quads into triangles. A vertex on a texture
  // or color seam appears once per distinct corner value, exactly as it
  // did coming in. Output indices number in first-encounter order, so the
  // result is deterministic regardless of the hashing underneath.
  auto outIndex{std::unordered_map<std::pair<int, int>, uint32_t, WeldHash>()};
  outIndex.reserve(numFineVerts);
  // One key's vertex from its refined (or limit) points.
  const auto fillVert{[&](Mesh::Vert &vert, const OsdPoint *src,
                          const LimitKey &limit, int vertex, int corner) {
    vert.point = src[vertex].value;
    if (corner >= 0) vert.texcoord = srcCorner[corner].uv;
    if (haveLimitNormals) {
      const auto normal{smdl::cross(limit.du[size_t(vertex)].value,
                                    limit.dv[size_t(vertex)].value)};
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
  }};
  auto emit{[&](int vertex, int corner) {
    auto [entry, isNew]{outIndex.try_emplace(std::make_pair(vertex, corner),
                                             uint32_t(mesh.verts.size()))};
    if (isNew) {
      fillVert(mesh.verts.emplace_back(), srcPoint, limitOpen, vertex, corner);
      if (hasShut)
        fillVert(mesh.vertsShut.emplace_back(), srcPointShut, limitShut, vertex,
                 corner);
      if (corner >= 0 && hasColors)
        mesh.colors.push_back(srcCorner[corner].color);
    }
    return entry->second;
  }};
  mesh.verts.reserve(numFineVerts);
  if (hasShut) mesh.vertsShut.reserve(numFineVerts);
  if (hasColors) mesh.colors.reserve(numFineVerts);
  mesh.faces.reserve(size_t(lastLevel.GetNumFaces()) * (isLoop ? 1 : 2));
  for (int face = 0; face < lastLevel.GetNumFaces(); face++) {
    const auto fVerts{lastLevel.GetFaceVertices(face)};
    const auto fCorners{hasCorners ? lastLevel.GetFaceFVarValues(face, 0)
                                   : Far::ConstIndexArray()};
    auto vertexOf{
        [&](int j) { return emit(fVerts[j], hasCorners ? fCorners[j] : -1); }};
    // Every refined face is a quad under Catmull-Clark and bilinear and
    // a triangle under Loop, but fan out generically so that an
    // unexpected size cannot misindex.
    for (int j = 2; j < fVerts.size(); j++)
      mesh.faces.push_back({vertexOf(0), vertexOf(j - 1), vertexOf(j)});
  }
  return haveLimitNormals;
}
