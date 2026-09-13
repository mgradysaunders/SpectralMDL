#include "Scene/Curves.h"

#include <algorithm>
#include <cstring>

// The Embree build of a loaded groom: where the file's basis and the
// spec's cross-section mode meet Embree's geometry type table. The
// `.curves` I/O and the basis math live in `CurvesFile.cc`, which
// stays Embree-free for the doctest binary.

namespace {

// One Embree vertex buffer: the file's control points sampled at one
// instant, strand by strand, with the radius scale applied. Catmull-Rom
// additionally duplicates each strand's end points, so the four-point
// window of every segment exists and the curve runs through every
// authored point (see `CurvesFile`).
[[nodiscard]] std::vector<float4> sampleVerts(const CurvesFile &file,
                                              const CurvesSpec &spec,
                                              const CurvesKeyBlend &blend) {
  const bool isCatmullRom{file.basis == CurvesFile::Basis::CATMULL_ROM};
  const uint32_t strandCount{file.strandCount()};
  std::vector<float4> verts{};
  verts.reserve(size_t(file.pointCount()) +
                (isCatmullRom ? 2 * size_t(strandCount) : 0));
  for (uint32_t strand = 0; strand < strandCount; strand++) {
    const uint32_t first{file.strandOffsets[strand]};
    const uint32_t last{file.strandOffsets[strand + 1]};
    const size_t base{verts.size()};
    if (isCatmullRom) verts.push_back(file.pointAt(first, blend));
    for (uint32_t i = first; i < last; i++)
      verts.push_back(file.pointAt(i, blend));
    if (isCatmullRom) verts.push_back(file.pointAt(last - 1, blend));
    if (spec.radiusScale != 1.0f)
      for (size_t i = base; i < verts.size(); i++)
        verts[i].w *= spec.radiusScale;
  }
  return verts;
}

} // namespace

std::unique_ptr<Curves> makeCurves(RTCDevice device, CurvesFile file,
                                   const CurvesSpec &spec, uint32_t matIndex,
                                   bool useRobustIntersection,
                                   const MotionSampling &sampling) {
  auto curves{std::make_unique<Curves>()};
  curves->spec = spec;
  curves->basis = file.basis;
  curves->matIndex = matIndex;
  curves->rootUVs = std::move(file.rootUVs);
  const uint32_t strandCount{file.strandCount()};
  // The two vertex buffers Embree lerps between, sampled from the
  // file's keys at the instants the shutter opens and shuts. The shut
  // buffer is dropped when it says nothing the open one does not, so a
  // still groom or a shut shutter builds exactly the one-step geometry
  // it always did.
  const CurvesKeyBlend openBlend{file.blendAt(sampling.open)};
  curves->points = sampleVerts(file, spec, openBlend);
  if (file.isMoving() && !sampling.isStill()) {
    const CurvesKeyBlend shutBlend{file.blendAt(sampling.shut)};
    curves->pointsShut = sampleVerts(file, spec, shutBlend);
    // A shut key that restates the open one bit for bit is no key: the
    // groom builds through the static path, as a mesh does.
    if (std::memcmp(curves->pointsShut.data(), curves->points.data(),
                    curves->points.size() * sizeof(float4)) == 0)
      curves->pointsShut.clear();
  }
  // Segment counts per basis: a strand of N usable points has N - 1
  // linear segments, N - 3 B-spline windows, and (after padding) N - 1
  // Catmull-Rom windows.
  const bool isCatmullRom{file.basis == CurvesFile::Basis::CATMULL_ROM};
  const uint32_t windowSize{file.basis == CurvesFile::Basis::LINEAR ? 2U : 4U};
  curves->strandFirstSeg.reserve(size_t(strandCount) + 1);
  curves->strandFirstSeg.push_back(0);
  uint32_t base{0};
  for (uint32_t strand = 0; strand < strandCount; strand++) {
    const uint32_t numPoints{file.strandOffsets[strand + 1] -
                             file.strandOffsets[strand] +
                             (isCatmullRom ? 2U : 0U)};
    const uint32_t numSegs{numPoints - (windowSize - 1)};
    for (uint32_t segment = 0; segment < numSegs; segment++) {
      curves->segIndices.push_back(base + segment);
      curves->segStrand.push_back(strand);
    }
    curves->strandFirstSeg.push_back(uint32_t(curves->segIndices.size()));
    base += numPoints;
  }
  // The proxy points: enough of the control points to frame and bound
  // by, plus the radius-inflated bounding corners so the fattest fiber
  // still fits. The autolook solver walks these per instance, so they
  // are capped rather than complete.
  // Both keys fold in, so a moving groom is framed and bounded by the
  // extent it sweeps rather than by one instant of it.
  BoundBox3 bound{};
  float maxRadius{};
  const auto foldIn{[&](const std::vector<float4> &verts) {
    for (const auto &point : verts) {
      bound.extend(float3(point.x, point.y, point.z));
      maxRadius = std::max(maxRadius, point.w);
    }
  }};
  foldIn(curves->points);
  foldIn(curves->pointsShut);
  constexpr size_t PROXY_POINT_CAP = 4096;
  const size_t stride{
      std::max(size_t(1), curves->points.size() / PROXY_POINT_CAP)};
  for (size_t i = 0; i < curves->points.size(); i += stride) {
    curves->proxyPoints.push_back(float3(curves->points[i]));
    if (curves->moves())
      curves->proxyPoints.push_back(float3(curves->pointsShut[i]));
  }
  if (!curves->points.empty())
    for (int corner = 0; corner < 8; corner++)
      curves->proxyPoints.push_back(
          float3(((corner & 1) ? bound.upper.x + maxRadius
                               : bound.lower.x - maxRadius),
                 ((corner & 2) ? bound.upper.y + maxRadius
                               : bound.lower.y - maxRadius),
                 ((corner & 4) ? bound.upper.z + maxRadius
                               : bound.lower.z - maxRadius)));
  // The geometry type is the file's basis crossed with the spec's
  // cross-section mode; see `RTC_GEOMETRY_TYPE_CURVE(3)`.
  RTCGeometryType geometryType{RTC_GEOMETRY_TYPE_ROUND_BSPLINE_CURVE};
  switch (file.basis) {
  case CurvesFile::Basis::LINEAR:
    geometryType = spec.mode == CurvesSpec::Mode::RIBBON
                       ? RTC_GEOMETRY_TYPE_FLAT_LINEAR_CURVE
                       : RTC_GEOMETRY_TYPE_ROUND_LINEAR_CURVE;
    break;
  case CurvesFile::Basis::BSPLINE:
    geometryType = spec.mode == CurvesSpec::Mode::RIBBON
                       ? RTC_GEOMETRY_TYPE_FLAT_BSPLINE_CURVE
                       : RTC_GEOMETRY_TYPE_ROUND_BSPLINE_CURVE;
    break;
  case CurvesFile::Basis::CATMULL_ROM:
    geometryType = spec.mode == CurvesSpec::Mode::RIBBON
                       ? RTC_GEOMETRY_TYPE_FLAT_CATMULL_ROM_CURVE
                       : RTC_GEOMETRY_TYPE_ROUND_CATMULL_ROM_CURVE;
    break;
  }
  curves->scene = rtcNewScene(device);
  rtcSetSceneFlags(curves->scene, useRobustIntersection ? RTC_SCENE_FLAG_ROBUST
                                                        : RTC_SCENE_FLAG_NONE);
  rtcSetSceneBuildQuality(curves->scene, RTC_BUILD_QUALITY_HIGH);
  RTCGeometry geometry{rtcNewGeometry(device, geometryType)};
  // A moving groom gives Embree its shut key as a second time step,
  // which it lerps per control point; a still one keeps the one step,
  // so its input is exactly what it always was.
  rtcSetGeometryTimeStepCount(geometry, curves->moves() ? 2 : 1);
  // The point vector is exactly Embree's FLOAT4 vertex layout, and a
  // 16-byte element satisfies Embree's read-past-the-end padding rule
  // by construction, so the buffer is shared rather than copied. This
  // is why the vectors above must never reallocate again.
  rtcSetSharedGeometryBuffer(geometry, RTC_BUFFER_TYPE_VERTEX, 0,
                             RTC_FORMAT_FLOAT4, curves->points.data(), 0,
                             sizeof(float4), curves->points.size());
  if (curves->moves())
    rtcSetSharedGeometryBuffer(geometry, RTC_BUFFER_TYPE_VERTEX, 1,
                               RTC_FORMAT_FLOAT4, curves->pointsShut.data(), 0,
                               sizeof(float4), curves->pointsShut.size());
  uint32_t *indices{static_cast<uint32_t *>(rtcSetNewGeometryBuffer(
      geometry, RTC_BUFFER_TYPE_INDEX, 0, RTC_FORMAT_UINT, sizeof(uint32_t),
      curves->segIndices.size()))};
  std::copy(curves->segIndices.begin(), curves->segIndices.end(), indices);
  rtcCommitGeometry(geometry);
  rtcAttachGeometry(curves->scene, geometry);
  rtcReleaseGeometry(geometry);
  rtcCommitScene(curves->scene);
  return curves;
}
