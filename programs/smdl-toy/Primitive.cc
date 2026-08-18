#include "Primitive.h"

#include "embree4/rtcore_ray.h"

#include <algorithm>
#include <cmath>
#include <limits>

// The analytic geometry of the primitive shapes, in one place: the
// closed-form surface evaluation, the Embree user-geometry callbacks
// that intersect it, and the uniform-area sampling that lights it. The
// intersect callbacks and `evalPrimitiveSurface()` speak the same
// (piece, u, v) parameterization, so the point a ray hits and the point
// the renderer shades are the same construction.

namespace {

constexpr float PI = 3.14159265358979323846f;
constexpr float TWO_PI = 6.28318530717958647692f;

// The azimuth parameter of a point in the XY plane, wrapped to [0, 1).
[[nodiscard]] float azimuthOf(float x, float y) noexcept {
  float u{std::atan2(y, x) * (1.0f / TWO_PI)};
  return u < 0.0f ? u + 1.0f : u;
}

// The piece indices, fixed per shape: the sphere and disk are piece 0;
// the cylinder is side, bottom cap, top cap; the cone is side, base cap.
enum : uint32_t {
  CYLINDER_SIDE = 0,
  CYLINDER_BOTTOM = 1,
  CYLINDER_TOP = 2,
  CONE_SIDE = 0,
  CONE_BASE = 1,
};

// A cap: a disk of radius `r` at height `z`, facing `sign` along Z. The
// disk primitive is the cap that faces up from the origin.
[[nodiscard]] PrimitiveSurface capSurface(float r, float z, float sign,
                                          float2 uv) {
  const float phi{TWO_PI * uv.x};
  const float cosPhi{std::cos(phi)};
  const float sinPhi{std::sin(phi)};
  const float rho{r * uv.y};
  PrimitiveSurface surface{};
  surface.point = float3(rho * cosPhi, rho * sinPhi, z);
  surface.normal = float3(0.0f, 0.0f, sign);
  surface.dPdu = TWO_PI * float3(-surface.point.y, surface.point.x, 0.0f);
  surface.dPdv = float3(r * cosPhi, r * sinPhi, 0.0f);
  return surface;
}

// One candidate root acceptor for the quadratic shapes: the smallest
// `t` inside the ray interval whose hit also lies inside the shape's
// finite extent wins, and testing both roots is what lets a ray that
// starts inside (a transmissive interior) find its way out.
class RootPair final {
public:
  float t0{-1.0f};
  float t1{-1.0f};
  int count{};
};

[[nodiscard]] RootPair solveQuadratic(float a, float b, float c) noexcept {
  RootPair roots{};
  if (std::fabs(a) < 1e-12f) {
    if (std::fabs(b) < 1e-12f) return roots;
    roots.t0 = -c / b;
    roots.count = 1;
    return roots;
  }
  const float disc{b * b - 4.0f * a * c};
  if (disc < 0.0f) return roots;
  const float sqrtDisc{std::sqrt(disc)};
  const float q{-0.5f * (b + std::copysign(sqrtDisc, b))};
  roots.t0 = q / a;
  roots.t1 = std::fabs(q) > 1e-20f ? c / q : roots.t0;
  if (roots.t0 > roots.t1) std::swap(roots.t0, roots.t1);
  roots.count = 2;
  return roots;
}

// What one accepted intersection reports back to Embree.
class PieceHit final {
public:
  float t{};
  float2 uv{};
  float3 ng{};
};

[[nodiscard]] bool intersectCap(float r, float z, float sign, const float3 &org,
                                const float3 &dir, float tnear, float tfar,
                                PieceHit &hit) {
  if (std::fabs(dir.z) < 1e-12f) return false;
  const float t{(z - org.z) / dir.z};
  if (!(t > tnear && t < tfar)) return false;
  const float px{org.x + t * dir.x};
  const float py{org.y + t * dir.y};
  const float rhoSq{px * px + py * py};
  if (rhoSq > r * r) return false;
  hit.t = t;
  hit.uv = float2(azimuthOf(px, py), std::sqrt(rhoSq) / r);
  hit.ng = float3(0.0f, 0.0f, sign);
  return true;
}

[[nodiscard]] bool intersectPiece(const PrimitiveSpec &spec, uint32_t primID,
                                  const float3 &org, const float3 &dir,
                                  float tnear, float tfar, PieceHit &hit) {
  const float r{spec.radius};
  const float h{spec.height};
  switch (spec.shape) {
  case PrimitiveSpec::Shape::SPHERE: {
    const auto roots{solveQuadratic(smdl::dot(dir, dir),
                                    2.0f * smdl::dot(org, dir),
                                    smdl::dot(org, org) - r * r)};
    for (int i = 0; i < roots.count; i++) {
      const float t{i == 0 ? roots.t0 : roots.t1};
      if (!(t > tnear && t < tfar)) continue;
      const auto point{org + t * dir};
      const auto normal{smdl::normalize(point)};
      hit.t = t;
      hit.uv = float2(azimuthOf(normal.x, normal.y),
                      std::acos(std::max(-1.0f, std::min(1.0f, normal.z))) *
                          (1.0f / PI));
      hit.ng = normal;
      return true;
    }
    return false;
  }
  case PrimitiveSpec::Shape::DISK:
    return intersectCap(r, 0.0f, 1.0f, org, dir, tnear, tfar, hit);
  case PrimitiveSpec::Shape::CYLINDER: {
    if (primID == CYLINDER_BOTTOM)
      return intersectCap(r, 0.0f, -1.0f, org, dir, tnear, tfar, hit);
    if (primID == CYLINDER_TOP)
      return intersectCap(r, h, 1.0f, org, dir, tnear, tfar, hit);
    const auto roots{solveQuadratic(dir.x * dir.x + dir.y * dir.y,
                                    2.0f * (org.x * dir.x + org.y * dir.y),
                                    org.x * org.x + org.y * org.y - r * r)};
    for (int i = 0; i < roots.count; i++) {
      const float t{i == 0 ? roots.t0 : roots.t1};
      if (!(t > tnear && t < tfar)) continue;
      const float z{org.z + t * dir.z};
      if (!(z >= 0.0f && z <= h)) continue;
      const float px{org.x + t * dir.x};
      const float py{org.y + t * dir.y};
      hit.t = t;
      hit.uv = float2(azimuthOf(px, py), z / h);
      hit.ng = float3(px, py, 0.0f);
      return true;
    }
    return false;
  }
  case PrimitiveSpec::Shape::CONE: {
    if (primID == CONE_BASE)
      return intersectCap(r, 0.0f, -1.0f, org, dir, tnear, tfar, hit);
    // The surface is sqrt(x^2 + y^2) = r (1 - z/h). Squaring admits the
    // mirror cone above the apex, which the z range rejects.
    const float k{r / h};
    const float m{r - k * org.z};
    const auto roots{
        solveQuadratic(dir.x * dir.x + dir.y * dir.y - k * k * dir.z * dir.z,
                       2.0f * (org.x * dir.x + org.y * dir.y + k * m * dir.z),
                       org.x * org.x + org.y * org.y - m * m)};
    for (int i = 0; i < roots.count; i++) {
      const float t{i == 0 ? roots.t0 : roots.t1};
      if (!(t > tnear && t < tfar)) continue;
      const float z{org.z + t * dir.z};
      if (!(z >= 0.0f && z <= h)) continue;
      const float px{org.x + t * dir.x};
      const float py{org.y + t * dir.y};
      // The apex has no azimuth and a zero gradient; treat a hit within
      // float noise of it as a miss rather than manufacture a frame.
      if (px * px + py * py < 1e-12f * r * r) continue;
      hit.t = t;
      hit.uv = float2(azimuthOf(px, py), z / h);
      hit.ng = float3(px, py, k * (r - k * z));
      return true;
    }
    return false;
  }
  default:
    return false;
  }
}

void primitiveBounds(const RTCBoundsFunctionArguments *args) {
  const auto *primitive{static_cast<const Primitive *>(args->geometryUserPtr)};
  const auto &spec{primitive->spec};
  const float r{spec.radius};
  const float h{spec.height};
  auto &bounds{*args->bounds_o};
  bounds.lower_x = -r, bounds.upper_x = r;
  bounds.lower_y = -r, bounds.upper_y = r;
  switch (spec.shape) {
  case PrimitiveSpec::Shape::SPHERE:
    bounds.lower_z = -r, bounds.upper_z = r;
    break;
  case PrimitiveSpec::Shape::DISK:
    bounds.lower_z = 0.0f, bounds.upper_z = 0.0f;
    break;
  case PrimitiveSpec::Shape::CYLINDER:
    bounds.lower_z = args->primID == CYLINDER_TOP ? h : 0.0f;
    bounds.upper_z = args->primID == CYLINDER_BOTTOM ? 0.0f : h;
    break;
  case PrimitiveSpec::Shape::CONE:
    bounds.lower_z = 0.0f;
    bounds.upper_z = args->primID == CONE_BASE ? 0.0f : h;
    break;
  default:
    break;
  }
}

void primitiveIntersect(const RTCIntersectFunctionNArguments *args) {
  // Single-ray queries only, which is all this renderer ever issues.
  if (args->N != 1 || !args->valid[0]) return;
  const auto *primitive{static_cast<const Primitive *>(args->geometryUserPtr)};
  auto *rayHit{reinterpret_cast<RTCRayHit *>(args->rayhit)};
  auto &ray{rayHit->ray};
  const auto org{float3(ray.org_x, ray.org_y, ray.org_z)};
  const auto dir{float3(ray.dir_x, ray.dir_y, ray.dir_z)};
  PieceHit hit{};
  if (!intersectPiece(primitive->spec, args->primID, org, dir, ray.tnear,
                      ray.tfar, hit))
    return;
  ray.tfar = hit.t;
  rayHit->hit.geomID = args->geomID;
  rayHit->hit.primID = args->primID;
  rayHit->hit.instID[0] = args->context->instID[0];
  // A shape scattered through an instance ARRAY is identified by which
  // element of the array the traversal came through; regular instances
  // report element 0.
  rayHit->hit.instPrimID[0] = args->context->instPrimID[0];
  rayHit->hit.u = hit.uv.x;
  rayHit->hit.v = hit.uv.y;
  rayHit->hit.Ng_x = hit.ng.x;
  rayHit->hit.Ng_y = hit.ng.y;
  rayHit->hit.Ng_z = hit.ng.z;
}

void primitiveOccluded(const RTCOccludedFunctionNArguments *args) {
  if (args->N != 1 || !args->valid[0]) return;
  const auto *primitive{static_cast<const Primitive *>(args->geometryUserPtr)};
  auto *ray{reinterpret_cast<RTCRay *>(args->ray)};
  const auto org{float3(ray->org_x, ray->org_y, ray->org_z)};
  const auto dir{float3(ray->dir_x, ray->dir_y, ray->dir_z)};
  PieceHit hit{};
  if (intersectPiece(primitive->spec, args->primID, org, dir, ray->tnear,
                     ray->tfar, hit))
    ray->tfar = -std::numeric_limits<float>::infinity();
}

// The object-space area of one piece.
[[nodiscard]] float pieceArea(const PrimitiveSpec &spec, uint32_t primID) {
  const float r{spec.radius};
  const float h{spec.height};
  switch (spec.shape) {
  case PrimitiveSpec::Shape::SPHERE:
    return 4.0f * PI * r * r;
  case PrimitiveSpec::Shape::DISK:
    return PI * r * r;
  case PrimitiveSpec::Shape::CYLINDER:
    return primID == CYLINDER_SIDE ? TWO_PI * r * h : PI * r * r;
  case PrimitiveSpec::Shape::CONE:
    return primID == CONE_SIDE ? PI * r * std::sqrt(r * r + h * h) : PI * r * r;
  default:
    return 0.0f;
  }
}

} // namespace

uint32_t primitivePieceCount(const PrimitiveSpec &spec) {
  switch (spec.shape) {
  case PrimitiveSpec::Shape::SPHERE:
  case PrimitiveSpec::Shape::DISK:
    return 1;
  case PrimitiveSpec::Shape::CYLINDER:
    return 3;
  case PrimitiveSpec::Shape::CONE:
    return 2;
  default:
    return 0;
  }
}

float primitiveObjectArea(const PrimitiveSpec &spec) {
  float area{};
  for (uint32_t primID = 0; primID < primitivePieceCount(spec); primID++)
    area += pieceArea(spec, primID);
  return area;
}

PrimitiveSurface evalPrimitiveSurface(const PrimitiveSpec &spec,
                                      uint32_t primID, float2 uv) {
  const float r{spec.radius};
  const float h{spec.height};
  const float phi{TWO_PI * uv.x};
  const float cosPhi{std::cos(phi)};
  const float sinPhi{std::sin(phi)};
  switch (spec.shape) {
  case PrimitiveSpec::Shape::SPHERE: {
    const float theta{PI * uv.y};
    const float sinTheta{std::sin(theta)};
    const float cosTheta{std::cos(theta)};
    PrimitiveSurface surface{};
    surface.normal = float3(sinTheta * cosPhi, sinTheta * sinPhi, cosTheta);
    surface.point = r * surface.normal;
    surface.dPdu = TWO_PI * float3(-surface.point.y, surface.point.x, 0.0f);
    surface.dPdv =
        PI * r * float3(cosTheta * cosPhi, cosTheta * sinPhi, -sinTheta);
    return surface;
  }
  case PrimitiveSpec::Shape::DISK:
    return capSurface(r, 0.0f, 1.0f, uv);
  case PrimitiveSpec::Shape::CYLINDER: {
    if (primID == CYLINDER_BOTTOM) return capSurface(r, 0.0f, -1.0f, uv);
    if (primID == CYLINDER_TOP) return capSurface(r, h, 1.0f, uv);
    PrimitiveSurface surface{};
    surface.point = float3(r * cosPhi, r * sinPhi, h * uv.y);
    surface.normal = float3(cosPhi, sinPhi, 0.0f);
    surface.dPdu = TWO_PI * float3(-surface.point.y, surface.point.x, 0.0f);
    surface.dPdv = float3(0.0f, 0.0f, h);
    return surface;
  }
  case PrimitiveSpec::Shape::CONE: {
    if (primID == CONE_BASE) return capSurface(r, 0.0f, -1.0f, uv);
    const float rho{r * (1.0f - uv.y)};
    PrimitiveSurface surface{};
    surface.point = float3(rho * cosPhi, rho * sinPhi, h * uv.y);
    // The gradient is rho (cos, sin, r/h), so the unit normal is
    // constant along each generator, apex included.
    surface.normal = smdl::normalize(float3(h * cosPhi, h * sinPhi, r));
    surface.dPdu = TWO_PI * float3(-surface.point.y, surface.point.x, 0.0f);
    surface.dPdv = float3(-r * cosPhi, -r * sinPhi, h);
    return surface;
  }
  default:
    return {};
  }
}

PrimitiveAreaSample samplePrimitiveArea(const PrimitiveSpec &spec, float2 xi) {
  // Pick the piece proportionally to its area, rescaling the used
  // coordinate so the pick costs no stratification within the piece.
  const auto pieceCount{primitivePieceCount(spec)};
  const float totalArea{primitiveObjectArea(spec)};
  uint32_t primID{0};
  float accum{};
  for (uint32_t i = 0; i < pieceCount; i++) {
    const float fraction{pieceArea(spec, i) / totalArea};
    if (xi.x < accum + fraction || i + 1 == pieceCount) {
      primID = i;
      xi.x = std::min(std::max((xi.x - accum) / fraction, 0.0f), 0.999999f);
      break;
    }
    accum += fraction;
  }
  // Uniform area within the piece: azimuth is always uniform; the other
  // parameter warps by the piece's area element.
  auto uv{float2(xi.x, xi.y)};
  const bool isCap{
      spec.shape == PrimitiveSpec::Shape::DISK ||
      (spec.shape == PrimitiveSpec::Shape::CYLINDER &&
       primID != CYLINDER_SIDE) ||
      (spec.shape == PrimitiveSpec::Shape::CONE && primID == CONE_BASE)};
  if (isCap) {
    uv.y = std::sqrt(xi.y);
  } else if (spec.shape == PrimitiveSpec::Shape::SPHERE) {
    uv.y = std::acos(std::max(-1.0f, std::min(1.0f, 1.0f - 2.0f * xi.y))) *
           (1.0f / PI);
  } else if (spec.shape == PrimitiveSpec::Shape::CONE) {
    // Lateral area density is proportional to the distance from the
    // apex, i.e. to (1 - v).
    uv.y = 1.0f - std::sqrt(1.0f - xi.y);
  }
  const auto surface{evalPrimitiveSurface(spec, primID, uv)};
  PrimitiveAreaSample sample{};
  sample.primID = primID;
  sample.uv = uv;
  sample.point = surface.point;
  sample.normal = surface.normal;
  return sample;
}

std::unique_ptr<Primitive> makePrimitive(RTCDevice device,
                                         const PrimitiveSpec &spec,
                                         uint32_t materialIndex) {
  auto primitive{std::make_unique<Primitive>()};
  primitive->spec = spec;
  primitive->materialIndex = materialIndex;
  primitive->objectArea = primitiveObjectArea(spec);
  // The proxy points: a coarse parameter grid per piece, endpoints
  // included so poles, rims, and caps all contribute to bounds.
  for (uint32_t primID = 0; primID < primitivePieceCount(spec); primID++)
    for (int iu = 0; iu < 8; iu++)
      for (int iv = 0; iv < 4; iv++)
        primitive->proxyPoints.push_back(
            evalPrimitiveSurface(spec, primID,
                                 float2(float(iu) / 8.0f, float(iv) / 3.0f))
                .point);
  primitive->scene = rtcNewScene(device);
  rtcSetSceneFlags(primitive->scene, RTC_SCENE_FLAG_ROBUST);
  rtcSetSceneBuildQuality(primitive->scene, RTC_BUILD_QUALITY_HIGH);
  auto geometry{rtcNewGeometry(device, RTC_GEOMETRY_TYPE_USER)};
  rtcSetGeometryUserPrimitiveCount(geometry, primitivePieceCount(spec));
  rtcSetGeometryUserData(geometry, primitive.get());
  rtcSetGeometryBoundsFunction(geometry, primitiveBounds, nullptr);
  rtcSetGeometryIntersectFunction(geometry, primitiveIntersect);
  rtcSetGeometryOccludedFunction(geometry, primitiveOccluded);
  rtcCommitGeometry(geometry);
  rtcAttachGeometry(primitive->scene, geometry);
  rtcReleaseGeometry(geometry);
  rtcCommitScene(primitive->scene);
  return primitive;
}
