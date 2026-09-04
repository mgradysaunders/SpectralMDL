#include "Scene/Curves.h"

#include <algorithm>

// The Embree build of a loaded groom: where the file's basis and the
// spec's cross-section mode meet Embree's geometry type table. The
// `.curves` I/O and the basis math live in `CurvesFile.cc`, which
// stays Embree-free for the doctest binary.

std::unique_ptr<Curves> makeCurves(RTCDevice device, CurvesFile file,
                                   const CurvesSpec &spec, uint32_t matIndex) {
  auto curves{std::make_unique<Curves>()};
  curves->spec = spec;
  curves->basis = file.basis;
  curves->matIndex = matIndex;
  curves->rootUVs = std::move(file.rootUVs);
  const auto strandCount{file.strandCount()};
  // Build the Embree-ready buffers strand by strand. The points pass
  // through with the radius scale applied; Catmull-Rom additionally
  // duplicates each strand's end points, so the four-point window of
  // every segment exists and the curve runs through every authored
  // point (see `CurvesFile`). Segment counts per basis: a strand of N
  // usable points has N - 1 linear segments, N - 3 B-spline windows,
  // and (after padding) N - 1 Catmull-Rom windows.
  const auto isCatmullRom{file.basis == CurvesFile::Basis::CATMULL_ROM};
  const auto windowSize{file.basis == CurvesFile::Basis::LINEAR ? 2U : 4U};
  curves->points.reserve(file.points.size() +
                         (isCatmullRom ? 2 * size_t(strandCount) : 0));
  curves->strandFirstSeg.reserve(size_t(strandCount) + 1);
  curves->strandFirstSeg.push_back(0);
  for (uint32_t strand = 0; strand < strandCount; strand++) {
    const auto first{file.strandOffsets[strand]};
    const auto last{file.strandOffsets[strand + 1]};
    const auto base{uint32_t(curves->points.size())};
    if (isCatmullRom) curves->points.push_back(file.points[first]);
    for (auto i = first; i < last; i++)
      curves->points.push_back(file.points[i]);
    if (isCatmullRom) curves->points.push_back(file.points[last - 1]);
    if (spec.radiusScale != 1.0f)
      for (auto i = size_t(base); i < curves->points.size(); i++)
        curves->points[i].w *= spec.radiusScale;
    const auto numPoints{uint32_t(curves->points.size()) - base};
    const auto numSegs{numPoints - (windowSize - 1)};
    for (uint32_t segment = 0; segment < numSegs; segment++) {
      curves->segIndices.push_back(base + segment);
      curves->segStrand.push_back(strand);
    }
    curves->strandFirstSeg.push_back(uint32_t(curves->segIndices.size()));
  }
  // The proxy points: enough of the control points to frame and bound
  // by, plus the radius-inflated bounding corners so the fattest fiber
  // still fits. The autolook solver walks these per instance, so they
  // are capped rather than complete.
  BoundBox3 bound{};
  float maxRadius{};
  for (const auto &point : curves->points) {
    bound.extend(float3(point.x, point.y, point.z));
    maxRadius = std::max(maxRadius, point.w);
  }
  constexpr size_t PROXY_POINT_CAP = 4096;
  const auto stride{
      std::max(size_t(1), curves->points.size() / PROXY_POINT_CAP)};
  for (size_t i = 0; i < curves->points.size(); i += stride)
    curves->proxyPoints.push_back(float3(curves->points[i]));
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
  auto geometryType{RTC_GEOMETRY_TYPE_ROUND_BSPLINE_CURVE};
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
  rtcSetSceneFlags(curves->scene, RTC_SCENE_FLAG_ROBUST);
  rtcSetSceneBuildQuality(curves->scene, RTC_BUILD_QUALITY_HIGH);
  auto geometry{rtcNewGeometry(device, geometryType)};
  // The point vector is exactly Embree's FLOAT4 vertex layout, and a
  // 16-byte element satisfies Embree's read-past-the-end padding rule
  // by construction, so the buffer is shared rather than copied. This
  // is why the vectors above must never reallocate again.
  rtcSetSharedGeometryBuffer(geometry, RTC_BUFFER_TYPE_VERTEX, 0,
                             RTC_FORMAT_FLOAT4, curves->points.data(), 0,
                             sizeof(float4), curves->points.size());
  auto *indices{static_cast<uint32_t *>(rtcSetNewGeometryBuffer(
      geometry, RTC_BUFFER_TYPE_INDEX, 0, RTC_FORMAT_UINT, sizeof(uint32_t),
      curves->segIndices.size()))};
  std::copy(curves->segIndices.begin(), curves->segIndices.end(), indices);
  rtcCommitGeometry(geometry);
  rtcAttachGeometry(curves->scene, geometry);
  rtcReleaseGeometry(geometry);
  rtcCommitScene(curves->scene);
  return curves;
}
