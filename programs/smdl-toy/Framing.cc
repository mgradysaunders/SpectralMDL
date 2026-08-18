#include "Framing.h"

#include "llvm/Support/Parallel.h"

#include "smdl/Support/Logger.h"

namespace {

/// The camera basis for one candidate direction. The zenith is validated
/// away from the poles by the command line, so the world-up cross product
/// never degenerates.
class FramingBasis final {
public:
  FramingBasis(float zenithInDegrees, float azimuthInDegrees) {
    const float zenith{zenithInDegrees * PI / 180.0f};
    const float azimuth{azimuthInDegrees * PI / 180.0f};
    const auto toCamera{float3(std::sin(zenith) * std::cos(azimuth),
                               std::sin(zenith) * std::sin(azimuth),
                               std::cos(zenith))};
    forward = -toCamera;
    right = smdl::normalize(smdl::cross(forward, float3(0, 0, 1)));
    up = smdl::cross(right, forward);
  }

  float3 right{};
  float3 up{};
  float3 forward{};
};

/// One candidate azimuth, solved and scored.
class Candidate final {
public:
  float3 position{};
  float fill{};
  float visibleArea{};
  float backfaceFraction{};
};

} // namespace

/// Every framed vertex in world space, gathered once so the per-candidate
/// passes are pure arithmetic, plus the axis-aligned bounds that `lookTo`
/// centers on. Runs after `commit()`, so refinement and displacement are
/// already in the vertices.
[[nodiscard]] static std::vector<float3>
gatherWorldPoints(const Scene &scene, uint32_t skipInstance, float3 &lower,
                  float3 &upper) {
  auto points{std::vector<float3>()};
  for (size_t i = 0; i < scene.meshInstances.size(); i++) {
    if (uint32_t(i) == skipInstance) continue;
    const auto &instance{scene.meshInstances[i]};
    auto fold{[&](const float3 &objectPoint) {
      const auto point{
          float3(instance.objectToWorld * float4(objectPoint, 1.0f))};
      points.push_back(point);
      for (int axis = 0; axis < 3; axis++) {
        lower[axis] = std::min(lower[axis], point[axis]);
        upper[axis] = std::max(upper[axis], point[axis]);
      }
    }};
    // A primitive or a groom stands in with its coarse proxy points.
    if (instance.isPrimitive()) {
      for (const auto &point :
           scene.primitives[instance.primitiveIndex]->proxyPoints)
        fold(point);
    } else if (instance.isCurves()) {
      for (const auto &point : scene.curves[instance.curvesIndex]->proxyPoints)
        fold(point);
    } else {
      for (const auto &vert : scene.meshes[instance.meshIndex]->verts)
        fold(vert.point);
    }
  }
  return points;
}

/// The closed-form tightest containing camera for a fixed direction: each
/// frustum plane is a linear constraint on the position, so four min
/// reductions decide it. The binding axis ends tangent to the geometry;
/// the slack axis is centered independently of the distance.
[[nodiscard]] static float3 solvePosition(const std::vector<float3> &points,
                                          const FramingBasis &basis, float tanX,
                                          float tanY) {
  float A{+INF}, B{+INF}, C{+INF}, D{+INF};
  for (const auto &q : points) {
    const float x{smdl::dot(q, basis.right)};
    const float y{smdl::dot(q, basis.up)};
    const float z{smdl::dot(q, basis.forward)};
    A = std::min(A, tanX * z - x);
    B = std::min(B, tanX * z + x);
    C = std::min(C, tanY * z - y);
    D = std::min(D, tanY * z + y);
  }
  const float pz{std::min((A + B) / (2 * tanX), (C + D) / (2 * tanY))};
  return 0.5f * (B - A) * basis.right + 0.5f * (D - C) * basis.up +
         pz * basis.forward;
}

namespace {

/// What one low-resolution ray grid through the frame measures.
///
/// The ranking metric is `visibleArea`, the classic viewpoint-selection
/// criterion: it peaks on the three-face 3/4 diagonals and puts both
/// degenerate views (end-on and dead broadside) below them. Frame
/// coverage is deliberately not the ranking: the exact fit lets an
/// end-on camera sit much closer, so coverage prefers degenerate end-on
/// views.
class ProbeResult final {
public:
  /// The fraction of the frame the framed geometry covers. Reported as
  /// the fill diagnostic; deliberately not the ranking (see above).
  float coverage{};

  /// The visible surface area in scene units squared: per hit pixel,
  /// distance squared times pixel solid angle over the incidence cosine
  /// (clamped, so silhouette-grazing pixels stay bounded). Distance
  /// invariant, occlusion aware, and what the sweep maximizes.
  float visibleArea{};

  /// The fraction of covered pixels showing non-exempt backfaces, which
  /// recognizes an open mesh seen from the side never meant to be seen.
  float backfaceFraction{};
};

} // namespace

[[nodiscard]] static ProbeResult probeFrame(const Scene &scene,
                                            const FramingBasis &basis,
                                            const float3 &position, float tanX,
                                            float tanY, uint32_t skipInstance) {
  constexpr int RESOLUTION = 96;
  // The solid angle one probe pixel stands for, taken as uniform across
  // the frame: the off-axis error is identical across candidates, and
  // the areas are only ever compared, never trusted absolutely.
  const float pixelSolidAngle{(2 * tanX / RESOLUTION) *
                              (2 * tanY / RESOLUTION)};
  size_t numHits{};
  size_t numBackfacing{};
  float visibleArea{};
  for (int j = 0; j < RESOLUTION; j++) {
    for (int i = 0; i < RESOLUTION; i++) {
      auto ray{Ray()};
      ray.org = position;
      ray.dir = smdl::normalize(
          basis.forward +
          (2.0f * (float(i) + 0.5f) / RESOLUTION - 1.0f) * tanX * basis.right +
          (2.0f * (float(j) + 0.5f) / RESOLUTION - 1.0f) * tanY * basis.up);
      auto hit{Hit()};
      if (!scene.intersect(ray, hit)) continue;
      if (hit.meshInstanceIndex == skipInstance) continue;
      numHits++;
      visibleArea +=
          ray.tmax * ray.tmax * pixelSolidAngle /
          std::max(std::abs(smdl::dot(hit.geometryNormal, ray.dir)), 0.05f);
      if (hit.material) {
        // A statically thin-walled material, or one declaring a backface
        // surface, legitimately shows its back (foliage cards); an
        // unknown thin-walled bit stays conservative and counts.
        const bool thinWalled{
            (hit.material->staticFlagsKnown &
             smdl::JIT::MATERIAL_THIN_WALLED) &&
            (hit.material->staticFlags & smdl::JIT::MATERIAL_THIN_WALLED)};
        if (thinWalled ||
            (hit.material->staticFlags & smdl::JIT::MATERIAL_HAS_BACKFACE))
          continue;
      }
      if (smdl::dot(hit.geometryNormal, ray.dir) > 0) numBackfacing++;
    }
  }
  auto result{ProbeResult{}};
  result.coverage = float(numHits) / float(RESOLUTION * RESOLUTION);
  result.visibleArea = visibleArea;
  result.backfaceFraction =
      numHits > 0 ? float(numBackfacing) / float(numHits) : 0.0f;
  return result;
}

FramingResult solveFraming(const Scene &scene, const FramingOptions &options) {
  auto lower{float3(+INF, +INF, +INF)};
  auto upper{float3(-INF, -INF, -INF)};
  const auto points{
      gatherWorldPoints(scene, options.skipInstance, lower, upper)};
  if (points.empty())
    throw smdl::Error("cannot -frame: the scene has no geometry");
  const float usable{1.0f - options.margin};
  const float tanY{std::tan(0.5f * options.fovYInDegrees * PI / 180.0f) *
                   usable};
  const float tanX{tanY * options.aspectRatio};
  auto azimuths{std::vector<float>()};
  if (options.azimuthInDegrees) {
    azimuths.push_back(*options.azimuthInDegrees);
  } else {
    constexpr int STEPS = 72;
    for (int i = 0; i < STEPS; i++)
      azimuths.push_back(float(i) * (360.0f / STEPS));
  }
  auto candidates{std::vector<Candidate>(azimuths.size())};
  llvm::parallelFor(0, azimuths.size(), [&](size_t i) {
    const auto basis{FramingBasis(options.zenithInDegrees, azimuths[i])};
    auto &candidate{candidates[i]};
    candidate.position = solvePosition(points, basis, tanX, tanY);
    const auto probe{probeFrame(scene, basis, candidate.position, tanX, tanY,
                                options.skipInstance)};
    candidate.fill = probe.coverage;
    candidate.visibleArea = probe.visibleArea;
    candidate.backfaceFraction = probe.backfaceFraction;
  });
  size_t best{0};
  if (candidates.size() > 1) {
    // Views dominated by backfaces are culled first, then visible
    // surface area decides; the tolerance keeps probe noise from
    // vetoing a meaningfully better view. The veto only means something
    // when some view is nearly clean (the signature of an actual
    // front); when even the best view shows backfaces everywhere they
    // *are* the geometry (unshaded two-sided foliage), so the veto
    // stands down and area alone decides.
    constexpr float BACKFACE_TOLERANCE = 0.02f;
    float minFraction{+INF};
    for (const auto &candidate : candidates)
      minFraction = std::min(minFraction, candidate.backfaceFraction);
    const bool cull{!options.ignoreBackfaces && minFraction <= 0.1f};
    if (!options.ignoreBackfaces && !cull)
      SMDL_LOG_INFO("Framing: every view shows at least ", 100.0f * minFraction,
                    "% backfaces, so they are treated as two-sided "
                    "geometry rather than as a wrong side");
    float bestArea{-INF};
    for (size_t i = 0; i < candidates.size(); i++) {
      if (cull &&
          candidates[i].backfaceFraction > minFraction + BACKFACE_TOLERANCE)
        continue;
      if (candidates[i].visibleArea > bestArea) {
        bestArea = candidates[i].visibleArea;
        best = i;
      }
    }
  } else if (!options.ignoreBackfaces &&
             candidates[0].backfaceFraction > 0.1f) {
    SMDL_LOG_WARN(
        "-frame-azimuth ", azimuths[0], ": ",
        100.0f * candidates[0].backfaceFraction,
        "% of the visible surface is backfacing, which looks like the "
        "side of an open mesh that is never meant to be seen");
  }
  const auto &chosen{candidates[best]};
  const auto basis{FramingBasis(options.zenithInDegrees, azimuths[best])};
  const auto center{0.5f * (lower + upper)};
  auto result{FramingResult{}};
  result.lookFrom = chosen.position;
  result.lookTo =
      chosen.position +
      smdl::dot(center - chosen.position, basis.forward) * basis.forward;
  result.azimuthInDegrees = azimuths[best];
  result.fill = chosen.fill;
  result.visibleArea = chosen.visibleArea;
  result.backfaceFraction = chosen.backfaceFraction;
  SMDL_LOG_INFO("Framing: azimuth ", result.azimuthInDegrees,
                " deg, look from (", result.lookFrom.x, ", ", result.lookFrom.y,
                ", ", result.lookFrom.z, "), visible area ", result.visibleArea,
                ", fill ", 100.0f * result.fill, "%, backface ",
                100.0f * result.backfaceFraction, "%");
  return result;
}
