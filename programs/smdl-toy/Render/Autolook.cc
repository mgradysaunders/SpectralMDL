#include "Render/Autolook.h"

#include "smdl/Support/Logger.h"
#include "smdl/Support/Parallel.h"

namespace {

// The camera basis for one candidate direction. The zenith is validated
// away from the poles by the command line, so the world-up cross product
// never degenerates.
class AutolookBasis final {
public:
  AutolookBasis(float zenithDeg, float azimuthDeg) {
    const float zenith{smdl::radians(zenithDeg)};
    const float azimuth{smdl::radians(azimuthDeg)};
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

// One candidate azimuth, solved and scored.
class Candidate final {
public:
  float3 position{};
  float fill{};
  float visibleArea{};
  float backfaceFraction{};
};

} // namespace

// Every framed vertex in world space, gathered once so the per-candidate
// passes are pure arithmetic, plus the axis-aligned bounds that `lookTo`
// centers on. Runs after `commit()`, so refinement and displacement are
// already in the vertices.
[[nodiscard]] static std::vector<float3>
gatherWorldPoints(const Scene &scene, uint32_t skipInstance, BoundBox3 &bound) {
  auto points{std::vector<float3>()};
  for (size_t i = 0; i < scene.meshInstances.size(); i++) {
    if (uint32_t(i) == skipInstance) continue;
    const auto &instance{scene.meshInstances[i]};
    auto fold{[&](const float3 &objectPoint) {
      const auto point{
          float3(instance.frame.objectToWorld * float4(objectPoint, 1.0f))};
      points.push_back(point);
      bound.extend(point);
    }};
    // A primitive or a groom stands in with its coarse proxy points.
    if (instance.isPrimitive()) {
      for (const auto &point :
           scene.primitives[instance.primIndex]->proxyPoints)
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

// The closed-form tightest containing camera for a fixed direction: each
// frustum plane is a linear constraint on the position, so four min
// reductions decide it. The binding axis ends tangent to the geometry;
// the slack axis is centered independently of the distance.
[[nodiscard]] static float3 solvePosition(const std::vector<float3> &points,
                                          const AutolookBasis &basis,
                                          float tanX, float tanY) {
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

// What one low-resolution ray grid through the frame measures.
//
// The ranking metric is `visibleArea`, the classic viewpoint-selection
// criterion: it peaks on the three-face 3/4 diagonals and puts both
// degenerate views (end-on and dead broadside) below them. Frame
// coverage is deliberately not the ranking: the exact fit lets an
// end-on camera sit much closer, so coverage prefers degenerate end-on
// views.
class ProbeResult final {
public:
  // The fraction of the frame the framed geometry covers. Reported as
  // the fill diagnostic; deliberately not the ranking (see above).
  float coverage{};

  // The visible surface area in scene units squared: per hit pixel,
  // distance squared times pixel solid angle over the incidence cosine
  // (clamped, so silhouette-grazing pixels stay bounded). Distance
  // invariant, occlusion aware, and what the sweep maximizes.
  float visibleArea{};

  // The fraction of covered pixels showing non-exempt backfaces, which
  // recognizes an open mesh seen from the side never meant to be seen.
  float backfaceFraction{};
};

} // namespace

[[nodiscard]] static ProbeResult probeFrame(const Scene &scene,
                                            const AutolookBasis &basis,
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
      if (hit.instIndex == skipInstance) continue;
      numHits++;
      visibleArea += ray.tmax * ray.tmax * pixelSolidAngle /
                     std::max(std::abs(smdl::dot(hit.Ng, ray.dir)), 0.05f);
      if (hit.material) {
        // A statically thin-walled material, or one declaring a backface
        // surface, legitimately shows its back (foliage cards); an
        // unknown thin-walled bit stays conservative and counts.
        const bool thinWalled{
            (hit.material->staticFlagsKnown & smdl::MATERIAL_THIN_WALLED) &&
            (hit.material->staticFlags & smdl::MATERIAL_THIN_WALLED)};
        if (thinWalled ||
            (hit.material->staticFlags & smdl::MATERIAL_HAS_BACKFACE))
          continue;
      }
      if (smdl::dot(hit.Ng, ray.dir) > 0) numBackfacing++;
    }
  }
  auto result{ProbeResult{}};
  result.coverage = float(numHits) / float(RESOLUTION * RESOLUTION);
  result.visibleArea = visibleArea;
  result.backfaceFraction =
      numHits > 0 ? float(numBackfacing) / float(numHits) : 0.0f;
  return result;
}

AutolookResult solveAutolook(const Scene &scene,
                             const AutolookOptions &options) {
  BoundBox3 bound{};
  const auto points{gatherWorldPoints(scene, options.skipInstance, bound)};
  if (points.empty())
    throw smdl::Error("cannot -autolook: the scene has no geometry");
  const float usable{1.0f - options.margin};
  const float tanY{std::tan(smdl::radians(0.5f * options.fovYDeg)) * usable};
  const float tanX{tanY * options.aspectRatio};
  auto azimuths{std::vector<float>()};
  if (options.azimuthDeg) {
    azimuths.push_back(*options.azimuthDeg);
  } else {
    constexpr int STEPS = 72;
    for (int i = 0; i < STEPS; i++)
      azimuths.push_back(float(i) * (360.0f / STEPS));
  }
  auto candidates{std::vector<Candidate>(azimuths.size())};
  smdl::parallelFor(0, azimuths.size(), [&](size_t i) {
    const auto basis{AutolookBasis(options.zenithDeg, azimuths[i])};
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
      SMDL_LOG_INFO("Autolook: every view shows at least ",
                    100.0f * minFraction,
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
        "-autolook-azimuth ", azimuths[0], ": ",
        100.0f * candidates[0].backfaceFraction,
        "% of the visible surface is backfacing, which looks like the "
        "side of an open mesh that is never meant to be seen");
  }
  const auto &chosen{candidates[best]};
  const auto basis{AutolookBasis(options.zenithDeg, azimuths[best])};
  const auto center{bound.center()};
  auto result{AutolookResult{}};
  result.lookFrom = chosen.position;
  result.lookTo =
      chosen.position +
      smdl::dot(center - chosen.position, basis.forward) * basis.forward;
  result.azimuthDeg = azimuths[best];
  result.fill = chosen.fill;
  result.visibleArea = chosen.visibleArea;
  result.backfaceFraction = chosen.backfaceFraction;
  SMDL_LOG_INFO("Autolook: azimuth ", result.azimuthDeg, " deg, look from (",
                result.lookFrom.x, ", ", result.lookFrom.y, ", ",
                result.lookFrom.z, "), visible area ", result.visibleArea,
                ", fill ", 100.0f * result.fill, "%, backface ",
                100.0f * result.backfaceFraction, "%");
  return result;
}
