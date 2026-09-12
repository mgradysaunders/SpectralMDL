#include "Render/Lens.h"

#include <cmath>
#include <limits>
#include <optional>
#include <string>

#include "smdl/RenderUtil/MonteCarlo.h"
#include "smdl/Support/Error.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Parallel.h"

namespace {

// Prescriptions are in millimeters and the renderer's scene unit is the
// meter (`State::metersPerSceneUnit` stays 1 here), so this is the only
// conversion in the whole path from the file to the trace.
constexpr float MM_TO_SCENE = 1e-3f;
constexpr float SCENE_TO_MM = 1e3f;

// How many passes the aspheric solve may take, and how close to the
// surface it has to land as a fraction of the clear aperture radius. The
// solve keeps a bracket around the root and bisects whenever the
// derivative would step outside it, so the cap is a floor under the
// answer rather than a hope: bisection alone closes a bracket onto
// neighboring floats well inside the count, and a pass costs nothing on
// a ray that has converged, since the loop leaves on the residual.
constexpr int MAX_SOLVE_STEPS = 32;
constexpr float SOLVE_TOLERANCE = 1e-6f;

// How many radii the sag of a surface is read at to find the band its
// cap covers.
constexpr int NUM_SAG_SAMPLES = 64;

// How many pieces the span of a ray over a surface is walked in to reach
// the crossing the ray meets first. A lens surface is met once by a ray
// that is not grazing it, so the walk is there for the oblate rim that
// lifts back over an oblique one and is crossed twice; what it cannot
// see is a pair of crossings inside one piece.
constexpr int NUM_SPAN_STEPS = 8;

// The two passes of a scan across the plane of the rear vertex: a coarse
// one over the whole aperture, then a fine one over the span it found.
// The rear aperture can be twenty times the width of the cone that
// actually reaches a film point, and ten times that again once the lens
// is stopped down, a phone lens being the case that sets these. So the
// coarse pass is what decides whether the cone is found at all and the
// fine pass is what resolves it.
constexpr int NUM_SCAN_COARSE_STEPS = 400;
constexpr int NUM_SCAN_FINE_STEPS = 200;

// How many times a search over the film radius halves its bracket, which
// closes one the width of an image circle onto neighboring floats.
constexpr int NUM_RADIUS_BISECTIONS = 30;

// How many rays a bundle carries while the film plane is being solved,
// and how far either way the search starts out from, as a fraction of
// the focal length. A published design sits within a percent of its
// paraxial plane; the bracket only seeds the search, which walks out of
// it if it has to.
constexpr int NUM_FOCUS_FAN_RAYS = 64;
constexpr int NUM_FOCUS_SWEEP_STEPS = 64;
constexpr float FOCUS_SEARCH_SPAN = 0.02f;

// How many film radii the exit pupil is tabulated at, how many radii
// within one entry's span are traced (the ends included, so the entry
// bounds the whole span and not just its middle), and how finely the
// region one of them sees is gridded once the scans have found it.
constexpr size_t NUM_PUPIL_RADII = 64;
constexpr int NUM_PUPIL_FILM_STEPS = 4;
constexpr int NUM_PUPIL_FINE_STEPS = 64;

// How finely the window a film point sees is gridded to measure how much
// of the plane of the rear vertex it covers.
constexpr int NUM_TRANSMITTED_AREA_STEPS = 32;

// Where the surface stops being one, which is what `radialLimit` holds:
// the clear aperture, or the radius the base conic turns back on itself
// at if that comes first. The turn is held well clear rather than barely
// clear. The sag stands vertical there, and the last sliver before it
// takes rays that reach the wall at near-tangency, letting them through
// a piece of the pupil a hundredth of a micron wide that no scan can
// resolve; the exit pupil bound then misses what the trace passes.
// Stopping short keeps the two answering the same question, and costs a
// few rays at the very rim, which a mounted element would have taken.
[[nodiscard]] float radialLimitOf(const LensElement &elem) noexcept {
  float uLimit{elem.semiDiameter * elem.semiDiameter};
  const float kappa{elem.curvature()};
  if (const float turn{(1 + elem.conic) * kappa * kappa}; turn > 0)
    uLimit = std::min(uLimit, (1 - 1e-2f) / turn);
  return std::sqrt(uLimit);
}

// The sag of a surface at squared radius `u`, and its derivative with
// respect to `u`, both in scene units. False is a radius past where the
// base conic turns back on itself, which no point of the surface is.
[[nodiscard]] bool sagOf(const LensElement &elem, float u, float &sag,
                         float &dSagDu) noexcept {
  const float kappa{elem.curvature()};
  const float wSq{1 - (1 + elem.conic) * kappa * kappa * u};
  if (SMDL_UNLIKELY(!(wSq > 0))) return false;
  const float w{std::sqrt(wSq)};
  sag = kappa * u / (1 + w);
  dSagDu = 0.5f * kappa / w;
  if (SMDL_UNLIKELY(elem.numAsphericTerms > 0)) {
    // The polynomial is evaluated in the millimeters its coefficients are
    // written in and converted once, here. Scaling the coefficients into
    // scene units instead would multiply the `r^18` one by 1e51 and divide
    // its argument by the same, which is off the end of a float at both
    // ends of the product.
    u *= SCENE_TO_MM * SCENE_TO_MM;
    float poly{0.0f};
    float dPoly{0.0f};
    float power{u};
    for (int i = 0; i < elem.numAsphericTerms; i++) {
      poly += elem.aspheric[i] * power * u;
      dPoly += elem.aspheric[i] * (float(i) + 2) * power;
      power *= u;
    }
    sag += poly * MM_TO_SCENE;
    dSagDu += dPoly * SCENE_TO_MM;
  }
  return true;
}

// The band of sag the cap covers inside its radial limit. A polynomial
// puts its extremes where no closed form reaches them, so the curve is
// sampled instead, and the band is then opened by the tolerance the
// solve works to, below which nothing here is resolved anyway.
void sagRangeOf(const LensElement &elem, float &sagMin,
                float &sagMax) noexcept {
  sagMin = sagMax = 0;
  for (int i = 1; i <= NUM_SAG_SAMPLES; i++) {
    const float radius{elem.radialLimit * (float(i) / NUM_SAG_SAMPLES)};
    float sag{0.0f};
    float dSagDu{0.0f};
    if (sagOf(elem, radius * radius, sag, dSagDu)) {
      sagMin = std::min(sagMin, sag);
      sagMax = std::max(sagMax, sag);
    }
  }
  const float pad{0.01f * (sagMax - sagMin) +
                  SOLVE_TOLERANCE * elem.semiDiameter};
  sagMin -= pad;
  sagMax += pad;
}

// The cardinal points of a system: where it brings light to a focus, and
// the two planes the focal length is measured from.
struct CardinalPoints final {
  // The same points, with the origin moved to `originZ`.
  [[nodiscard]] CardinalPoints movedTo(float originZ) const noexcept {
    CardinalPoints points{*this};
    points.frontPrincipalZ -= originZ;
    points.rearPrincipalZ -= originZ;
    return points;
  }
  // The paraxial image plane of an object `focusDistance` ahead of the
  // origin, zero being an object at infinity, which lands on the rear
  // focal point. The thick lens images an object `s` ahead of the front
  // principal plane at `f s / (s - f)` behind the rear one. Nothing is an
  // object inside the front focal point, which no film position images.
  [[nodiscard]]
  std::optional<float> imagePlane(float focusDist) const noexcept {
    if (focusDist == 0) return rearPrincipalZ + focalLength;
    const float objectDist{frontPrincipalZ + focusDist};
    if (!(objectDist > focalLength)) return std::nullopt;
    return rearPrincipalZ +
           focalLength * objectDist / (objectDist - focalLength);
  }

  float focalLength{};
  float backFocalDistance{};
  float frontPrincipalZ{};
  float rearPrincipalZ{};
};

// The paraxial ray transfer matrix, in reduced coordinates: it acts on
// the pair (height, index times slope) rather than (height, slope), so
// every factor has unit determinant and a surface's matrix says the same
// thing whatever glass surrounds it. That is what makes the pupil solves
// below one division each.
struct ABCD final {
  [[nodiscard]] static ABCD refraction(float power) noexcept {
    return {1.0f, 0.0f, -power, 1.0f};
  }
  [[nodiscard]] static ABCD transfer(float dist, float ior) noexcept {
    return {1.0f, dist / ior, 0.0f, 1.0f};
  }
  // `next` acts after this, so it multiplies from the left.
  [[nodiscard]] ABCD then(const ABCD &next) const noexcept {
    return {next.a * a + next.b * c, next.a * b + next.b * d,
            next.c * a + next.d * c, next.c * b + next.d * d};
  }
  // The cardinal points of a system whose vertices stand at `frontZ` and
  // `rearZ`. Air stands at both ends, so the determinant is 1 and every
  // point is one of the matrix's elements over another.
  [[nodiscard]] CardinalPoints cardinalPoints(float frontZ,
                                              float rearZ) const noexcept {
    CardinalPoints points{};
    points.focalLength = -1 / c;
    points.backFocalDistance = -a / c;
    points.frontPrincipalZ = frontZ + (d - 1) / c;
    points.rearPrincipalZ = rearZ + (a - 1) * points.focalLength;
    return points;
  }

  float a{1.0f};
  float b{0.0f};
  float c{0.0f};
  float d{1.0f};
};

// The first-order model of a laid-out prescription, with every medium at
// one set of indices. It runs in the frame the prescription was laid out
// in, the front vertex at zero, so that the constructor's solve at the d
// line and a later solve at any wavelength are the same arithmetic.
class Paraxial final {
public:
  [[nodiscard]] ABCD refractionAt(size_t i) const noexcept {
    const LensElement &elem{elements[i]};
    return ABCD::refraction(elem.radius == 0 ? 0.0f
                                             : (indices[elem.mediumAfter] -
                                                indices[elem.mediumBefore]) /
                                                   elem.radius);
  }
  [[nodiscard]] ABCD transferAfter(size_t i) const noexcept {
    return ABCD::transfer(layoutZ[i + 1] - layoutZ[i],
                          indices[elements[i].mediumAfter]);
  }
  [[nodiscard]] ABCD system() const noexcept {
    ABCD system{};
    for (size_t i = 0; i < elements.size(); i++) {
      system = system.then(refractionAt(i));
      if (i + 1 < elements.size()) system = system.then(transferAfter(i));
    }
    return system;
  }
  smdl::Span<const LensElement> elements{};
  smdl::Span<const float> layoutZ{};
  const float *indices{};
};

// The span of `t` the ray is inside the surface's own extent over, which
// is the only stretch a root can mean anything on. Both limits are
// needed to close it: a ray up the axis stays inside the radius forever
// and one square across the axis stays inside the sag band forever, but
// a ray is parallel to at most one of them.
[[nodiscard]] bool spanOverElement(const LensElement &elem, const Ray &ray,
                                   float &tLo, float &tHi) noexcept {
  tLo = 0, tHi = FLOAT_MAX;
  const float ox{ray.org.x}, oy{ray.org.y}, oz{ray.org.z};
  const float dx{ray.dir.x}, dy{ray.dir.y}, dz{ray.dir.z};
  const float a{dx * dx + dy * dy};
  const float b{2 * (ox * dx + oy * dy)};
  const float c{ox * ox + oy * oy - elem.radialLimit * elem.radialLimit};
  if (a > 0) {
    const float discrim{b * b - 4 * a * c};
    if (!(discrim > 0)) return false;
    // The stable pairing of the roots, `q / a` and `c / q`, which takes
    // the root the subtraction would cancel off the product of the roots
    // instead. A strictly positive discriminant leaves `q` nonzero.
    const float q{-0.5f * (b + std::copysign(std::sqrt(discrim), b))};
    tLo = std::max(tLo, std::min(q / a, c / q));
    tHi = std::min(tHi, std::max(q / a, c / q));
  } else if (c > 0) {
    return false;
  }
  const float zLo{elem.z + elem.sagMin};
  const float zHi{elem.z + elem.sagMax};
  if (dz > 0) {
    tLo = std::max(tLo, (zLo - oz) / dz);
    tHi = std::min(tHi, (zHi - oz) / dz);
  } else if (dz < 0) {
    tLo = std::max(tLo, (zHi - oz) / dz);
    tHi = std::min(tHi, (zLo - oz) / dz);
  } else if (!(zLo <= oz && oz <= zHi)) {
    return false;
  }
  return tLo <= tHi;
}

// Intersect a ray with a surface whose sag carries an even polynomial on
// top of its base conic, which has no closed form. The search is held to
// the span the ray crosses the surface's own extent over, and inside it
// runs on a bracket that only ever shrinks: the ends say which side of
// the surface the ray is on, and each pass takes the step the derivative
// asks for when it lands inside the bracket and bisects when it does
// not, so the bracket closes whatever the surface does under it.
//
// Holding the search inside the extent is the point of it. An aspheric
// polynomial is a fit over the clear aperture, and read past there its
// high-order terms diverge and grow roots that no light ever meets; an
// iteration free to wander out can come back with one of them and report
// a hit at the wrong place on the surface.
[[nodiscard]] bool intersectAspheric(const LensElement &elem, const Ray &ray,
                                     float3 &point, float3 &normal) noexcept {
  float tLo{}, tHi{};
  if (!spanOverElement(elem, ray, tLo, tHi)) return false;
  float3 p{};
  float sag{0.0f};
  float dSagDu{0.0f};
  float height{0.0f};
  const float tolerance{SOLVE_TOLERANCE * elem.semiDiameter};
  // How far the ray stands above the surface at `t`, along the axis.
  const auto probe{[&](float t) {
    p = ray(t);
    if (!sagOf(elem, p.x * p.x + p.y * p.y, sag, dSagDu)) return false;
    height = (p.z - elem.z) - sag;
    return true;
  }};
  const auto accept{[&]() {
    point = p;
    // The surface is `z - vertex - sag(x^2 + y^2) = 0`, so its gradient
    // is the sag's slope in the two radial directions against a unit
    // rise in z.
    normal = normalize(float3(-2 * dSagDu * p.x, //
                              -2 * dSagDu * p.y, 1.0f));
    if (dot(normal, ray.dir) > 0) normal = -normal;
    return true;
  }};
  if (!probe(tLo)) {
    return false;
  } else if (std::abs(height) <= tolerance) {
    return accept();
  }
  // Walk the span from the near end to the first piece of it the ray
  // changes sides over, so that what the solve closes on is the crossing
  // the ray reaches first rather than whichever one an iteration happens
  // to land in. Reading the two ends alone would not do: they agree in
  // sign whenever the surface lifts back over an oblique ray, and they
  // disagree without saying which of three crossings came first. What
  // the walk does not see is a pair of crossings inside one piece, which
  // is a surface finer than the span divided this far.
  const bool isBelowAtLo{height < 0};
  float tA{tLo};
  float tB{tLo};
  bool isBracketed{false};
  for (int i = 1; i <= NUM_SPAN_STEPS && !isBracketed; i++) {
    if (const float t{tLo + (tHi - tLo) * (float(i) / NUM_SPAN_STEPS)};
        !probe(t)) {
      return false;
    } else if (std::abs(height) <= tolerance) {
      return accept();
    } else if ((height < 0) != isBelowAtLo) {
      tB = t, isBracketed = true;
    } else {
      tA = t;
    }
  }
  if (!isBracketed) return false;
  float t{0.5f * (tA + tB)};
  for (int step = 0; step < MAX_SOLVE_STEPS; step++) {
    if (!probe(t)) {
      return false;
    } else if (std::abs(height) <= tolerance) {
      return accept();
    } else if ((height < 0) == isBelowAtLo) {
      tA = t;
    } else {
      tB = t;
    }
    // How fast the height closes: the ray climbing in z against the
    // surface receding under it as the radius grows.
    const float slope{ray.dir.z -
                      2 * dSagDu * (p.x * ray.dir.x + p.y * ray.dir.y)};
    t = slope == 0 ? FLOAT_MAX : t - height / slope;
    if (!(tA < t && t < tB)) t = 0.5f * (tA + tB);
  }
  return false;
}

// Intersect a ray with one surface of revolution about the z axis, and
// return the hit point and the surface normal there, the normal turned to
// face the ray.
//
// Sphere, conic and plane are all one quadric here:
//
//     c (x^2 + y^2 + (1 + k) s^2) - 2 s = 0,   s = z - vertex,  c = 1/R
//
// which is the sag formula cleared of its square root, so a conic costs
// exactly what a sphere costs and a flat surface degenerates to a linear
// solve. The root to take is the one nearest the vertex: the quadric has
// two sheets, and the far one stands about `2 R` away, well outside any
// clear aperture a real design states.
[[nodiscard]] bool intersectSurface(const LensElement &elem, const Ray &ray,
                                    float3 &point, float3 &normal) noexcept {
  // A polynomial on top of the conic is not a quadric and has no closed
  // form, so it has a solve of its own and does not start from this one.
  if (SMDL_UNLIKELY(elem.numAsphericTerms > 0))
    return intersectAspheric(elem, ray, point, normal);
  const float kappa{elem.curvature()};
  const float kPlus1{1 + elem.conic};
  const float ox{ray.org.x}, oy{ray.org.y}, os{ray.org.z - elem.z};
  const float dx{ray.dir.x}, dy{ray.dir.y}, dz{ray.dir.z};
  const float a{kappa * (dx * dx + dy * dy + kPlus1 * dz * dz)};
  const float b{2 * (kappa * (ox * dx + oy * dy + kPlus1 * os * dz) - dz)};
  const float c{kappa * (ox * ox + oy * oy + kPlus1 * os * os) - 2 * os};
  float sBest{FLOAT_MAX};
  float tBest{0.0f};
  const auto consider{[&](float t) {
    // Zero counts: two surfaces of a cemented pair may share a vertex,
    // and an axial ray then meets the second one at no distance at all.
    if (!(t >= 0)) return;
    if (const float s{std::abs(os + t * dz)}; s < sBest) {
      sBest = s;
      tBest = t;
    }
  }};
  if (SMDL_UNLIKELY(a == 0)) {
    if (SMDL_UNLIKELY(b == 0)) return false;
    consider(-c / b);
  } else {
    const float discrim{b * b - 4 * a * c};
    if (SMDL_UNLIKELY(discrim < 0)) return false;
    // The stable pairing of the roots, `q / a` and `c / q`, which takes
    // the root the subtraction would cancel off the product of the roots
    // instead. Here `q` is zero where `b` and `c` both are, which leaves
    // `q / a` the whole answer.
    const float q{-0.5f * (b + std::copysign(std::sqrt(discrim), b))};
    consider(q / a);
    if (SMDL_LIKELY(q != 0)) consider(c / q);
  }
  if (SMDL_UNLIKELY(sBest == FLOAT_MAX)) return false;
  point = ray(tBest);
  normal = normalize(float3(kappa * point.x, kappa * point.y,
                            kappa * kPlus1 * (point.z - elem.z) - 1));
  if (dot(normal, ray.dir) > 0) normal = -normal;
  return true;
}

// Snell's law. `normal` faces the incident side and `eta` is the index
// the ray leaves over the index it enters. False is total internal
// reflection, which a lens with no mirror in it has no answer to.
[[nodiscard]] bool refract(float3 &dir, const float3 &normal,
                           float ior) noexcept {
  const float cosThetaI{-dot(dir, normal)};
  const float sin2ThetaT{ior * ior * (1 - cosThetaI * cosThetaI)};
  if (SMDL_UNLIKELY(sin2ThetaT >= 1)) return false;
  dir = ior * dir + (ior * cosThetaI - std::sqrt(1 - sin2ThetaT)) * normal;
  return true;
}

// Is the point inside the regular polygon of `numBlades` sides with
// circumradius `circumRadius` and a vertex at `angle`? The boundary in
// polar form is the apothem over the cosine of the angle off the nearest
// edge, which is one modulo and one cosine rather than a loop over the
// edges.
[[nodiscard]] bool isInsideBlades(float x, float y, int numBlades,
                                  float circumRadius, float angle) noexcept {
  const float sector{TWO_PI / float(numBlades)};
  float phi{std::atan2(y, x) - angle};
  phi -= sector * std::floor(phi / sector);
  const float apothem{circumRadius * std::cos(0.5f * sector)};
  const float boundary{apothem / std::cos(phi - 0.5f * sector)};
  return x * x + y * y <= boundary * boundary;
}

// An axis-aligned rectangle on the plane of the rear vertex, grown by
// folding in the points that got out. The default is empty.
struct PupilRect final {
  [[nodiscard]] bool isEmpty() const noexcept { return !(loX <= hiX); }
  void extend(float x, float y) noexcept {
    loX = std::min(loX, x), hiX = std::max(hiX, x);
    loY = std::min(loY, y), hiY = std::max(hiY, y);
  }
  void extend(const PupilRect &other) noexcept {
    if (!other.isEmpty())
      extend(other.loX, other.loY), extend(other.hiX, other.hiY);
  }
  void expand(float x, float y) noexcept {
    loX -= x, hiX += x, loY -= y, hiY += y;
  }
  void clampTo(const PupilRect &other) noexcept {
    loX = std::max(loX, other.loX), hiX = std::min(hiX, other.hiX);
    loY = std::max(loY, other.loY), hiY = std::min(hiY, other.hiY);
  }
  [[nodiscard]] float2 center() const noexcept {
    return {0.5f * (loX + hiX), 0.5f * (loY + hiY)};
  }
  [[nodiscard]] float2 halfExtent() const noexcept {
    return {0.5f * (hiX - loX), 0.5f * (hiY - loY)};
  }

  float loX{+FLOAT_MAX}, hiX{-FLOAT_MAX};
  float loY{+FLOAT_MAX}, hiY{-FLOAT_MAX};
};

// The ellipse a pupil point is drawn inside, on the plane of the rear
// vertex, in the frame where the film point lies on the +x axis.
struct PupilEllipse final {
  float2 center{};
  float2 semiAxes{};
};

// The whole rear aperture as a rectangle, which is the domain a pupil
// point was drawn from before there was a table and is still the domain
// the weights are relative to.
[[nodiscard]] PupilRect apertureRect(const Lens &lens) noexcept {
  PupilRect rect{};
  rect.extend(-lens.rearApertureRadius(), -lens.rearApertureRadius());
  rect.extend(+lens.rearApertureRadius(), +lens.rearApertureRadius());
  return rect;
}

// A lens with one set of medium indices, which is what every scan and
// grid here asks its questions of: the reference indices, for the
// lens's own probes, and for the exit pupil's table the two ends of a
// range of wavelengths besides. The pair travels together because every
// one of those questions is about a lens at a wavelength, and neither
// half means anything without the other.
class Probe final {
public:
  Probe(const Lens &lens, smdl::Span<const float> indices) noexcept
      : lens(lens), indices(indices) {}

  // Refract `ray` out through every surface; see `Lens::traceFromFilm()`.
  [[nodiscard]] bool trace(Ray &ray) const noexcept {
    return lens.traceFromFilm(ray, indices);
  }
  // The ray from the film point `filmRadius` off the axis, on the +x
  // side, toward the point `(x, y)` of the plane of the rear vertex,
  // which is the one ray every scan here is made of.
  [[nodiscard]] Ray rayTo(float filmRadius, float x, float y) const noexcept {
    const float3 film{filmRadius, 0, lens.filmZ()};
    const float3 rear{x, y, lens.rearZ()};
    return {film, rear - film, EPS, INF};
  }
  // Does light reaching that film point come through that point?
  [[nodiscard]] bool passes(float filmRadius, float x, float y) const noexcept {
    Ray ray{rayTo(filmRadius, x, y)};
    return trace(ray);
  }
  // The window along x on the plane of the rear vertex that the light
  // reaching a film point `filmRadius` off the axis comes through. A scan
  // along x finds the whole of it, the system being one of revolution and
  // the film point lying on the +x axis.
  [[nodiscard]]
  bool pupilWindowAt(float filmRadius, float &lo, float &hi) const noexcept {
    const float radius{lens.rearApertureRadius()};
    if (scanPupil(
            filmRadius, [&](float t) { return float2(radius * t, 0); }, //
            lo, hi)) {
      lo *= radius;
      hi *= radius;
      return true;
    } else {
      return false;
    }
  }
  // How far that same region reaches in y at `x`, which at the middle of
  // the window bounds the whole of it within a factor of two: what a film
  // point sees is the clear apertures projected onto this plane and
  // intersected, so its half-height is a concave function of x vanishing
  // at both ends of the window, and a concave function is at least half
  // its largest value at the midpoint of an interval it vanishes on.
  [[nodiscard]]
  float pupilHalfHeightAt(float filmRadius, float x) const noexcept {
    const float radius{lens.rearApertureRadius()};
    if (float lo{0.0f}, hi{0.0f}; scanPupil(
            filmRadius, [&](float t) { return float2(x, radius * t); }, //
            lo, hi)) {
      return radius * std::max(std::abs(lo), std::abs(hi));
    } else {
      return 0;
    }
  }
  // The middle of the window, which is the chief ray where the lens does
  // not vignette and the middle of what survives where it does.
  [[nodiscard]] bool chiefPointAt(float filmRadius, float &x) const noexcept {
    if (float lo{0.0f}, hi{0.0f}; pupilWindowAt(filmRadius, lo, hi)) {
      x = 0.5f * (lo + hi);
      return true;
    } else {
      return false;
    }
  }
  // The angle off the axis that light reaching a film point `filmRadius`
  // off the axis comes in at, the chief ray traced out: see
  // `Lens::fieldAngleAt()`. Empty when nothing reaches that far.
  [[nodiscard]]
  std::optional<float> fieldAngleAt(float filmRadius) const noexcept {
    if (float x{0.0f}; chiefPointAt(filmRadius, x)) {
      if (Ray ray{rayTo(filmRadius, x, 0)}; trace(ray)) {
        return std::atan2(std::hypot(ray.dir.x, ray.dir.y), -ray.dir.z);
      }
    }
    return std::nullopt;
  }
  // The largest film radius anything reaches: see
  // `Lens::imageCircleRadius()`. Grow until nothing gets out, then halve
  // the gap. A film point further off the axis needs its light to clear
  // more of the glass and never less, so where the light stops is one
  // place and not several.
  [[nodiscard]] float imageCircleRadius() const noexcept {
    const float cap{16 * lens.focalLength()};
    float x{0.0f};
    float inside{0.0f}, outside{0.25f * lens.focalLength()};
    while (outside < cap && chiefPointAt(outside, x)) {
      inside = outside;
      outside *= 2;
    }
    if (!(outside < cap)) return inside;
    for (int i = 0; i < NUM_RADIUS_BISECTIONS; i++) {
      if (const float middle{0.5f * (inside + outside)};
          chiefPointAt(middle, x)) {
        inside = middle;
      } else {
        outside = middle;
      }
    }
    return inside;
  }
  // The film radius that looks out at `angle` radians off the axis: see
  // `Lens::filmRadiusForFieldAngle()`.
  [[nodiscard]]
  std::optional<float> filmRadiusForFieldAngle(float angle) const noexcept {
    if (!(angle > 0)) return 0.0f;
    const float largest{imageCircleRadius()};
    const std::optional<float> widest{fieldAngleAt(largest)};
    if (!(widest && *widest >= angle)) return std::nullopt;
    float lo{0.0f}, hi{largest};
    for (int i = 0; i < NUM_RADIUS_BISECTIONS; i++) {
      const float middle{0.5f * (lo + hi)};
      // A radius nothing gets out of counts as short of the angle, which
      // walks the search back in toward where light does get out.
      const std::optional<float> angleAt{fieldAngleAt(middle)};
      if (!angleAt || *angleAt < angle) {
        lo = middle;
      } else {
        hi = middle;
      }
    }
    return 0.5f * (lo + hi);
  }
  // Scan a line across the plane of the rear vertex and report the span
  // of it that light reaching a film point `filmRadius` off the axis, on
  // the +x side, comes through. `pointAt` places a point on the line for
  // a parameter running over [-1, 1], and the span comes back in that
  // same parameter. False is a line nothing gets out through.
  template <typename F>
  [[nodiscard]]
  SMDL_ALWAYS_INLINE bool scanPupil(float filmRadius, F &&pointAt, //
                                    float &lo, float &hi) const noexcept {
    const auto passesAt{[&](float t) {
      const auto point{pointAt(t)};
      return passes(filmRadius, point.x, point.y);
    }};
    lo = FLOAT_MAX, hi = -FLOAT_MAX;
    for (int i = 0; i <= NUM_SCAN_COARSE_STEPS; i++) {
      if (const float t{2.0f * float(i) / NUM_SCAN_COARSE_STEPS - 1};
          passesAt(t)) {
        lo = std::min(lo, t);
        hi = std::max(hi, t);
      }
    }
    if (!(lo <= hi)) return false;
    const float cell{2.0f / NUM_SCAN_COARSE_STEPS};
    const float from{lo - cell}, to{hi + cell};
    for (int i = 0; i <= NUM_SCAN_FINE_STEPS; i++) {
      if (const float t{from + (to - from) * float(i) / NUM_SCAN_FINE_STEPS};
          passesAt(t)) {
        lo = std::min(lo, t);
        hi = std::max(hi, t);
      }
    }
    return true;
  }
  // Grid `over` and hand `visit` every point that a ray from a film
  // point `filmRadius` off axis, on the +x side, gets out through. This
  // is the whole cost of building the table.
  template <typename F>
  SMDL_ALWAYS_INLINE void eachPupilPoint(float filmRadius,
                                         const PupilRect &over, int numSteps,
                                         F &&visit) const noexcept {
    const float radius{lens.rearApertureRadius()};
    for (int i = 0; i < numSteps; i++) {
      const float x{
          smdl::lerp(over.loX, over.hiX, (float(i) + 0.5f) / float(numSteps))};
      for (int j = 0; j < numSteps; j++) {
        const float y{smdl::lerp(over.loY, over.hiY,
                                 (float(j) + 0.5f) / float(numSteps))};
        // The aperture is a disk and the grid is a rectangle, so the
        // corners of the grid lie outside the domain and stay out of the
        // bound: a draw landing there is blocked either way, and letting
        // it widen the bound would only make every other draw likelier to
        // land there too.
        if (x * x + y * y <= radius * radius && passes(filmRadius, x, y))
          visit(x, y);
      }
    }
  }
  // The box on this plane that everything a film point `filmRadius` off
  // the axis can see lies within, found by the two scans rather than by
  // gridding the whole aperture: the rear aperture can be hundreds of
  // times the area of the cone, tens of thousands once the lens is
  // stopped down, and a grid coarse enough to run over the one at every
  // film radius of the table misses the other outright.
  //
  // The x range is the scan's own and is exact. The y range is twice the
  // half-height at the middle of it, which contains the rest. Both are
  // then widened by a quarter, and never by less than the coarse scan's
  // own step, so that a grid over the box reaches past the region on
  // every side.
  [[nodiscard]] PupilRect seedPupilRect(float filmRadius) const noexcept {
    PupilRect rect{};
    float lo{0.0f}, hi{0.0f};
    if (!pupilWindowAt(filmRadius, lo, hi)) return rect;
    const float height{2 * pupilHalfHeightAt(filmRadius, 0.5f * (lo + hi))};
    rect.extend(lo, -height), rect.extend(hi, +height);
    const float least{2 * lens.rearApertureRadius() / NUM_SCAN_COARSE_STEPS};
    rect.expand(std::max(0.25f * (rect.hiX - rect.loX), least),
                std::max(0.25f * (rect.hiY - rect.loY), least));
    rect.clampTo(apertureRect(lens));
    return rect;
  }

  const Lens &lens;
  smdl::Span<const float> indices{};
};

// The RMS radius of the axial spot for a film at `filmZ`, measured at
// the plane the lens is focused on, or as a slope for a lens focused at
// infinity, which is the limit of the same quantity. The point is on the
// axis and the system is one of revolution, so a fan along x carries the
// whole pupil once each ray is weighted by the annulus it stands for,
// and the spot is centered on the axis by symmetry. Negative is a film
// position nothing reaches.
[[nodiscard]] float axialSpotOf(const Lens &lens, float filmZ,
                                float pupilRadius, float focusDist) noexcept {
  const float3 film{0, 0, filmZ};
  float sumWeight{0.0f};
  float sumSq{0.0f};
  for (int i = 0; i < NUM_FOCUS_FAN_RAYS; i++) {
    const float height{pupilRadius * (float(i) + 0.5f) / NUM_FOCUS_FAN_RAYS};
    Ray ray{film, float3(height, 0, lens.rearZ()) - film, EPS, INF};
    if (!lens.traceFromFilm(ray) || !(ray.dir.z < 0)) continue;
    const float slope{ray.dir.x / ray.dir.z};
    const float at{focusDist > 0 ? ray.org.x + (-focusDist - ray.org.z) * slope
                                 : -slope};
    sumWeight += height;
    sumSq += height * at * at;
  }
  return sumWeight > 0 ? std::sqrt(sumSq / sumWeight) : -1.0f;
}

// Move the film to where that spot is smallest, from the paraxial plane
// the caller has already put it on. The Gaussian solve is only where the
// rays cross in the limit of zero aperture: a design left with spherical
// aberration in it, which is every fast design, brings its own zones to
// a head a little off that plane and states the plane it chose as its
// back focus.
//
// The pupil the fan is drawn on is taken once, at the seed, the window
// moving by a fraction of a percent over a search this narrow. A sweep
// brackets the minimum and a compass search walks into it.
[[nodiscard]] float bestFilmPlane(const Lens &lens, float focusDist) noexcept {
  float lo{0.0f}, hi{0.0f};
  if (!Probe{lens, lens.referenceIndices()}.pupilWindowAt(0, lo, hi))
    return lens.filmZ();
  const float pupilRadius{std::max(hi, -lo)};
  const float span{FOCUS_SEARCH_SPAN * lens.focalLength()};
  float best{lens.filmZ()};
  float bestSpot{FLOAT_MAX};
  const auto consider{[&](float z) {
    const float spot{axialSpotOf(lens, z, pupilRadius, focusDist)};
    if (!(spot >= 0) || !(spot < bestSpot)) return false;
    bestSpot = spot, best = z;
    return true;
  }};
  for (int i = 0; i <= NUM_FOCUS_SWEEP_STEPS; i++)
    consider(lens.filmZ() +
             span * (2.0f * float(i) / NUM_FOCUS_SWEEP_STEPS - 1));
  if (!(bestSpot < FLOAT_MAX)) return lens.filmZ();
  float step{span / NUM_FOCUS_SWEEP_STEPS};
  for (int i = 0; i < 64 && step > 1e-4f * span; i++)
    if (!consider(best - step) && !consider(best + step)) step *= 0.5f;
  return best;
}

// Bound what every film point between `filmRadius0` and `filmRadius1` can
// see, at each of `probes`. Nothing getting out anywhere in the span gives
// back the whole aperture, which costs the draws it always cost and leaves
// the estimator exactly what it was.
//
// The bound is an ellipse rather than the rectangle it is found as. What
// a film point sees is the aperture stop and the clear apertures in
// front of it, all disks, all projected onto this plane and intersected:
// a convex region bounded by circular arcs, which one disk alone often
// limits. An ellipse on that region's own bounding box contains it and
// wastes a fifth less area than the box does, and exactly none when one
// disk is doing the limiting.
//
// Over a range of wavelengths the probes are the reference and the two
// ends. What a film point sees through moves with the index,
// continuously and, for normal dispersion, monotonically, so at a
// wavelength between the ends each point of it lies near the chord
// between where it lies at the two, inside the hull of their regions.
// The ellipse is convex and holds every point found at either end, so
// it holds that hull. That is an argument rather than a proof, since a
// point's path bows off the chord.
[[nodiscard]] PupilEllipse boundPupil(smdl::Span<const Probe> probes,
                                      float filmRadius0,
                                      float filmRadius1) noexcept {
  const Lens &lens{probes.front().lens};
  const PupilRect whole{apertureRect(lens)};
  const float radius{lens.rearApertureRadius()};
  const PupilEllipse wholeEllipse{whole.center(), float2(radius, radius)};
  // Every pass below asks the same question of every probe at every film
  // radius of the span, and differs only in what it does with the
  // answer.
  const auto eachProbeAndRadius{[&](auto &&ask) {
    for (const auto &probe : probes)
      for (int k = 0; k < NUM_PUPIL_FILM_STEPS; k++)
        ask(probe, filmRadius0 + (filmRadius1 - filmRadius0) * float(k) /
                                     (NUM_PUPIL_FILM_STEPS - 1));
  }};
  PupilRect seed{};
  eachProbeAndRadius([&](const Probe &probe, float filmRadius) {
    seed.extend(probe.seedPupilRect(filmRadius));
  });
  if (seed.isEmpty()) return wholeEllipse;
  PupilRect fine{};
  eachProbeAndRadius([&](const Probe &probe, float filmRadius) {
    probe.eachPupilPoint(filmRadius, seed, NUM_PUPIL_FINE_STEPS,
                         [&](float x, float y) { fine.extend(x, y); });
  });
  if (fine.isEmpty()) return wholeEllipse;
  const float2 cell{(seed.hiX - seed.loX) / NUM_PUPIL_FINE_STEPS,
                    (seed.hiY - seed.loY) / NUM_PUPIL_FINE_STEPS};
  const float2 center{fine.center()};
  const float2 extent{std::max(fine.halfExtent().x, cell.x),
                      std::max(fine.halfExtent().y, cell.y)};
  // How far the region reaches in units of that box, which decides how
  // much the ellipse on the box has to grow to contain it: 1 whenever it
  // already does, which is every case where one disk is the limit. The
  // grid is walked again rather than remembered, the second walk being
  // cheaper than the tens of thousands of points it would hold.
  float scale{1.0f};
  eachProbeAndRadius([&](const Probe &probe, float filmRadius) {
    probe.eachPupilPoint(filmRadius, seed, NUM_PUPIL_FINE_STEPS,
                         [&](float x, float y) {
                           const float u{(x - center.x) / extent.x};
                           const float v{(y - center.y) / extent.y};
                           scale = std::max(scale, std::sqrt(u * u + v * v));
                         });
  });
  return PupilEllipse{center, scale * extent + cell};
}

} // namespace

Lens::Lens(const LensPrescription &prescription, const LensOptions &options) {
  const std::vector<LensSurface> &surfaces{prescription.surfaces};
  if (surfaces.empty())
    throw smdl::Error("expected a lens with at least one surface");
  if (surfaces.size() > LENS_MAX_SURFACES)
    throw smdl::Error(smdl::concat("expected at most ", LENS_MAX_SURFACES,
                                   " surfaces in a lens, got ",
                                   surfaces.size()));
  mStopIndex = prescription.stopIndex();
  if (mStopIndex == surfaces.size())
    throw smdl::Error("expected a lens with an aperture stop, which is what "
                      "decides how much light it gathers");
  mName = prescription.name;
  // Lay the surfaces out on the axis from the front vertex, one medium to
  // a space: the air in front, then the space after each surface. The stop
  // stands in a space rather than ending one, so it names that space's
  // medium on both sides.
  mElements.resize(surfaces.size());
  mLayoutZ.resize(surfaces.size());
  mMedia.emplace_back();
  std::string_view mediumName{};
  float z{0.0f};
  for (size_t i = 0; i < surfaces.size(); i++) {
    const LensSurface &surface{surfaces[i]};
    LensElement &elem{mElements[i]};
    elem.z = mLayoutZ[i] = z * MM_TO_SCENE;
    elem.radius = surface.radius * MM_TO_SCENE;
    elem.conic = surface.conic;
    if (surface.aspheric.size() > LENS_MAX_ASPHERIC_TERMS)
      throw smdl::Error(
          smdl::concat("expected at most ", LENS_MAX_ASPHERIC_TERMS,
                       " aspheric coefficients on a surface, got ",
                       surface.aspheric.size()));
    // A table that prints its unused terms leaves trailing zeros, and a
    // polynomial that is all zeros is a conic. Dropping them is what
    // keeps such a surface on the closed-form path.
    size_t numTerms{surface.aspheric.size()};
    while (numTerms > 0 && surface.aspheric[numTerms - 1] == 0) numTerms--;
    elem.numAsphericTerms = int(numTerms);
    for (size_t j = 0; j < numTerms; j++)
      elem.aspheric[j] = surface.aspheric[j];
    elem.semiDiameter = 0.5f * surface.diameter * MM_TO_SCENE;
    elem.mediumBefore = int(mMedia.size() - 1);
    if (!surface.isStop) {
      mMedia.push_back(surface.medium);
      mediumName = surface.mediumName;
    }
    elem.mediumAfter = int(mMedia.size() - 1);
    elem.isStop = surface.isStop;
    z += surface.thickness;
  }
  // Everything below is solved at the d line, and every trace that names
  // no other wavelength refracts there. A medium that does not disperse
  // has that index at every wavelength.
  for (const auto &medium : mMedia) {
    mReferenceIndices.push_back(medium.indexAt(smdl::FRAUNHOFER_D_LINE));
    mIsDispersive = mIsDispersive || medium.isDispersive();
  }
  if (mReferenceIndices.back() != 1 || !mediumName.empty())
    throw smdl::Error(smdl::concat(
        "expected air behind the last surface, got ",
        mediumName.empty()
            ? smdl::concat("an index of ", mReferenceIndices.back())
            : smdl::concat("the medium ", smdl::Quoted(mediumName)),
        ": the film is not immersed, so the prescription is missing the "
        "surface that brings the light back out"));
  mDesignBackFocus = surfaces.back().thickness * MM_TO_SCENE;

  // The system, from the plane of the front vertex to the plane of the
  // rear one, in the frame the surfaces were just laid out in.
  const Paraxial paraxial{mElements, mLayoutZ, mReferenceIndices.data()};
  const ABCD system{paraxial.system()};
  if (system.c == 0)
    throw smdl::Error("the surfaces have no net power between them, so the "
                      "prescription forms no image and has no focal length");
  CardinalPoints points{
      system.cardinalPoints(mLayoutZ.front(), mLayoutZ.back())};
  mFocalLength = points.focalLength;
  mBackFocalDistance = points.backFocalDistance;

  // The entrance pupil is the image of the stop through whatever stands
  // in front of it, so the matrix carrying a ray from the front vertex to
  // the stop plane says both where the pupil lies (the plane the stop is
  // conjugate to, where the matrix loses its `b`) and how much larger
  // than the stop it looks (the magnification, which is that matrix's
  // `a`).
  ABCD front{};
  for (size_t i = 0; i < mStopIndex; i++) {
    front = front.then(paraxial.refractionAt(i));
    front = front.then(paraxial.transferAfter(i));
  }
  if (front.a == 0)
    throw smdl::Error("the stop sits at the front focal point of the "
                      "surfaces before it, which puts the entrance pupil at "
                      "infinity; the renderer cannot sample a telecentric "
                      "lens");
  mLayoutEntrancePupilZ = mLayoutZ.front() + front.b / front.a;

  // The exit pupil, the same solve through whatever stands behind the
  // stop, read from the other end of the matrix.
  ABCD rear{};
  for (size_t i = mStopIndex; i + 1 < mElements.size(); i++) {
    rear = rear.then(paraxial.transferAfter(i));
    rear = rear.then(paraxial.refractionAt(i + 1));
  }
  if (rear.d == 0)
    throw smdl::Error("the stop sits at the rear focal point of the surfaces "
                      "behind it, which puts the exit pupil at infinity");
  mExitPupilZ = mLayoutZ.back() - rear.b / rear.d;

  // The entrance pupil is the origin, which is what `look_from` names and
  // what `focus` is measured from, so everything shifts onto it now that
  // it is known.
  for (auto &element : mElements) element.z -= mLayoutEntrancePupilZ;
  mExitPupilZ -= mLayoutEntrancePupilZ;
  points = points.movedTo(mLayoutEntrancePupilZ);

  const float pupilMagnification{std::abs(front.a)};
  const float stopRadius{mElements[mStopIndex].semiDiameter};
  mFNumberWideOpen = mFocalLength * pupilMagnification / (2 * stopRadius);
  float workingStopRadius{stopRadius};
  if (options.fStop > 0) {
    if (options.fStop < mFNumberWideOpen)
      throw smdl::Error(smdl::concat(
          "cannot open the lens to f/", options.fStop, ": its stop is ",
          2 * stopRadius * SCENE_TO_MM, " mm across, which is f/",
          mFNumberWideOpen, " wide open"));
    // An f-number is a statement about the entrance pupil, so that is
    // what it sets; the physical stop follows by the magnification the
    // surfaces in front of it apply.
    workingStopRadius =
        0.5f * (mFocalLength / options.fStop) * pupilMagnification;
  }
  // Captured before the stop is narrowed, since a lens whose rear
  // element is the stop would otherwise shrink the disk its pupil points
  // are drawn on along with it, and stopping down would cost no light.
  mRearApertureRadius = mElements.back().semiDiameter;
  mElements[mStopIndex].semiDiameter = workingStopRadius;
  mEntrancePupilRadius = workingStopRadius / pupilMagnification;
  mExitPupilRadius = workingStopRadius / std::abs(rear.d);
  // Taken from what was asked rather than recomputed from the pupil the
  // asking produced: the two agree to the last digit only by luck, and a
  // lens left wide open has to report the wide-open number exactly, or
  // everything downstream that compares the two sees it stopped down.
  mFNumber = options.fStop > 0 ? options.fStop : mFNumberWideOpen;
  if (options.numBlades >= 3) {
    mNumBlades = options.numBlades;
    mBladeAngle = options.bladeAngle;
    // The polygon carries the area of the round stop it replaces, so
    // that blades change the shape of a highlight and not the exposure,
    // and `fstop` goes on meaning what it says. This is the same
    // scaling `smdl::uniformApertureSample()` samples the thin lens
    // aperture with.
    const float n{float(mNumBlades)};
    mBladeCircumRadius =
        workingStopRadius * std::sqrt(TWO_PI / (n * std::sin(TWO_PI / n)));
  }

  // The extent every intersection searches inside, settled now that the
  // stop carries its working radius and before the film solve below,
  // which is the first thing here to trace.
  for (auto &elem : mElements) {
    elem.radialLimit = radialLimitOf(elem);
    sagRangeOf(elem, elem.sagMin, elem.sagMax);
  }

  // Focus by moving the film, which is what a lens whose elements do not
  // move does.
  mFocusDistance = options.focusDistance;
  if (!(mFocusDistance >= 0))
    throw smdl::Error("expected a nonnegative focus distance");
  const std::optional<float> imageZ{points.imagePlane(mFocusDistance)};
  if (!imageZ)
    throw smdl::Error(smdl::concat(
        "cannot focus at ", mFocusDistance,
        " scene units: that is inside the lens's front focal point, ",
        (mFocalLength - points.frontPrincipalZ),
        " scene units out, and no film position images it"));
  mParaxialFilmZ = *imageZ;
  // The paraxial plane is where the film starts and not where it ends:
  // the trace moves it onto the focus the whole cone comes to, which is
  // the film position the design itself was drawn around. It is set
  // first because the scan that finds the pupil traces from it.
  mFilmZ = mParaxialFilmZ;
  mFilmZ = bestFilmPlane(*this, mFocusDistance);
}

bool Lens::traceFromFilm(Ray &ray) const noexcept {
  return traceThrough(ray, mReferenceIndices.data());
}

bool Lens::traceFromFilm(Ray &ray, float wavelength) const noexcept {
  const Indices indices{indicesAt(wavelength)};
  return traceThrough(ray, indices.data());
}

bool Lens::traceFromFilm(Ray &ray,
                         smdl::Span<const float> indices) const noexcept {
  SMDL_SANITY_CHECK(indices.size() >= mMedia.size());
  return traceThrough(ray, indices.data());
}

Lens::Indices Lens::indicesAt(float wavelength) const noexcept {
  Indices indices{};
  for (size_t i = 0; i < mMedia.size(); i++)
    indices[i] = mMedia[i].indexAt(wavelength);
  return indices;
}

float Lens::focalLengthAt(float wavelength) const noexcept {
  const Indices indices{indicesAt(wavelength)};
  const ABCD system{Paraxial{mElements, mLayoutZ, indices.data()}.system()};
  return system.cardinalPoints(mLayoutZ.front(), mLayoutZ.back()).focalLength;
}

float Lens::paraxialFilmZAt(float wavelength) const noexcept {
  const Indices indices{indicesAt(wavelength)};
  const ABCD system{Paraxial{mElements, mLayoutZ, indices.data()}.system()};
  return system.cardinalPoints(mLayoutZ.front(), mLayoutZ.back())
      .movedTo(mLayoutEntrancePupilZ)
      .imagePlane(mFocusDistance)
      .value_or(std::numeric_limits<float>::quiet_NaN());
}

bool Lens::traceThrough(Ray &ray, const float *indices) const noexcept {
  ray.dir = normalize(ray.dir);
  // Backward through the list, since the file runs front to film and the
  // light here runs the other way: at every surface the ray leaves the
  // space on the film side and enters the one on the scene side.
  for (size_t i = mElements.size(); i-- > 0;) {
    const LensElement &elem{mElements[i]};
    float3 point{}, normal{};
    if (!intersectSurface(elem, ray, point, normal)) return false;
    if (point.x * point.x + point.y * point.y >
        elem.semiDiameter * elem.semiDiameter)
      return false;
    if (elem.isStop) {
      if (mNumBlades >= 3 && !isInsideBlades(point.x, point.y, mNumBlades,
                                             mBladeCircumRadius, mBladeAngle))
        return false;
      // The stop is a plane with a hole in it: it bends nothing and has
      // no thickness, so the ray goes on from where it was. Moving the
      // origin onto it would be exact only in exact arithmetic, and a
      // stop written hard against the next surface would then start the
      // next intersection a rounding error onto the wrong side of that
      // surface's vertex, where the near root reads as behind the ray
      // and the far sheet answers instead.
      continue;
    }
    ray.org = point;
    if (!refract(ray.dir, normal,
                 indices[elem.mediumAfter] / indices[elem.mediumBefore]))
      return false;
  }
  return true;
}

void Lens::logSummary() const {
  SMDL_LOG_INFO("Lens ", smdl::Quoted(mName.empty() ? "(unnamed)" : mName),
                ": ", mElements.size(), " surfaces, focal length ",
                mFocalLength * SCENE_TO_MM, " mm, f/", mFNumberWideOpen,
                " wide open");
  if (mFNumber != mFNumberWideOpen)
    SMDL_LOG_INFO("Lens aperture: stopped down to f/", mFNumber, ", a stop ",
                  2 * mElements[mStopIndex].semiDiameter * SCENE_TO_MM,
                  " mm across");
  if (mNumBlades >= 3)
    SMDL_LOG_INFO("Lens aperture: ", mNumBlades,
                  " blades, of the same area as the round stop");
  // The exit pupil's z is signed and often negative: on a double Gauss it
  // stands in front of the entrance pupil, which is the origin.
  SMDL_LOG_INFO("Lens pupils: entrance ",
                2 * mEntrancePupilRadius * SCENE_TO_MM,
                " mm across at the camera origin, exit ",
                2 * mExitPupilRadius * SCENE_TO_MM,
                " mm across at z = ", mExitPupilZ * SCENE_TO_MM, " mm");
  SMDL_LOG_INFO("Lens focus: at ",
                mFocusDistance > 0
                    ? smdl::concat(mFocusDistance, " scene units")
                    : std::string("infinity"),
                ", film ", (mFilmZ - rearZ()) * SCENE_TO_MM,
                " mm behind the rear vertex (back focal distance ",
                mBackFocalDistance * SCENE_TO_MM, " mm)");
  if (const float correction{(mFilmZ - mParaxialFilmZ) * SCENE_TO_MM};
      std::abs(correction) > 1e-4f)
    SMDL_LOG_INFO("Lens focus: the traced axial spot is smallest ",
                  std::abs(correction), " mm ",
                  correction > 0 ? "behind" : "in front of",
                  " the paraxial image plane, which is where the film sits");
  // Longitudinal color: how far apart the paraxial images of the F and C
  // lines stand. Like the back focus below, it is a check on a
  // transcription that costs nothing: an achromat brings the two
  // together, so a design meant as one that comes out with millimeters
  // between them has a medium typed wrong.
  if (mIsDispersive) {
    const float imageF{paraxialFilmZAt(smdl::FRAUNHOFER_F_LINE)};
    const float imageC{paraxialFilmZAt(smdl::FRAUNHOFER_C_LINE)};
    if (std::isfinite(imageF) && std::isfinite(imageC)) {
      const float apart{(imageF - imageC) * SCENE_TO_MM};
      SMDL_LOG_INFO("Lens color: the F line (486 nm) focuses ", std::abs(apart),
                    " mm ", apart > 0 ? "behind" : "in front of",
                    " the C line (656 nm), paraxially");
    }
  }
  // A design that states its own back focus is stating where it puts the
  // film, which is the one check on a transcription that costs nothing.
  // It is not the paraxial point, though: a design left with spherical
  // aberration in it sits a percent or so of its focal length behind
  // that on purpose, at the focus its own zones agree on. A transcription
  // error is a much larger number, and that is what this stands between.
  if (mDesignBackFocus > 0) {
    if (std::abs(mDesignBackFocus - mBackFocalDistance) > 0.02f * mFocalLength)
      SMDL_LOG_WARN("Lens: the design states a back focus of ",
                    mDesignBackFocus * SCENE_TO_MM,
                    " mm and the surfaces solve to ",
                    mBackFocalDistance * SCENE_TO_MM,
                    " mm, which is too far apart to be where the design "
                    "chose to put its film; check the transcription");
  }
}

ExitPupil::ExitPupil(const Lens &lens, float maxFilmRadius,
                     const std::optional<float2> &wavelengthRange) {
  mRearRadius = lens.rearApertureRadius();
  mMaxFilmRadius = maxFilmRadius;
  mBoundsPerRadius = maxFilmRadius > 0 ? NUM_PUPIL_RADII / maxFilmRadius : 0.0f;
  mInvApertureArea = 1 / (PI * mRearRadius * mRearRadius);
  mBounds.resize(NUM_PUPIL_RADII);
  // The lens every entry is bounded at: the reference indices, and the
  // two ends of the range when the glasses disperse over it.
  std::array<Lens::Indices, 2> ends{};
  std::vector<Probe> probes{Probe{lens, lens.referenceIndices()}};
  if (wavelengthRange && lens.isDispersive()) {
    mWavelengthRange = wavelengthRange;
    ends[0] = lens.indicesAt(wavelengthRange->x);
    ends[1] = lens.indicesAt(wavelengthRange->y);
    for (const auto &end : ends)
      probes.emplace_back(
          lens, smdl::Span<const float>(end.data(), lens.media().size()));
  }
  // Every entry is independent, and the whole table is the one part of
  // building a camera slow enough to notice.
  smdl::parallelFor(size_t(0), mBounds.size(), [&](size_t i) {
    const float span{maxFilmRadius / NUM_PUPIL_RADII};
    const PupilEllipse ellipse{
        boundPupil(probes, span * float(i), span * (float(i) + 1))};
    mBounds[i].center = ellipse.center;
    mBounds[i].semiAxes = ellipse.semiAxes;
  });
}

bool ExitPupil::contains(float2 film, float2 point) const noexcept {
  const float filmRadius{length(film)};
  const Bound &bound{boundAt(filmRadius)};
  // Turn the point back into the bound frame, which undoes the turn
  // `sample()` ends on.
  const float cosPhi{filmRadius > 0 ? film.x / filmRadius : 1.0f};
  const float sinPhi{filmRadius > 0 ? film.y / filmRadius : 0.0f};
  const float x{cosPhi * point.x + sinPhi * point.y};
  const float y{cosPhi * point.y - sinPhi * point.x};
  const float u{(x - bound.center.x) / bound.semiAxes.x};
  const float v{(y - bound.center.y) / bound.semiAxes.y};
  return u * u + v * v <= 1;
}

float2 ExitPupil::sample(float2 film, float2 xi, float &area) const noexcept {
  const float filmRadius{length(film)};
  const Bound &bound{boundAt(filmRadius)};
  const float2 disk{smdl::uniformDiskSample(xi)};
  const float x{bound.center.x + bound.semiAxes.x * disk.x};
  const float y{bound.center.y + bound.semiAxes.y * disk.y};
  // The bound covers a span of film radii and is padded on top of that,
  // so it reaches outside the aperture near the rim. A draw landing
  // there is the zero it would have been drawn on the whole aperture.
  area = x * x + y * y <= mRearRadius * mRearRadius
             ? PI * bound.semiAxes.x * bound.semiAxes.y
             : 0.0f;
  // Turn the bound frame, whose +x axis is the film point's azimuth,
  // onto the film point. The turn preserves length, so the aperture test
  // above reads the same on either side of it.
  const float cosPhi{filmRadius > 0 ? film.x / filmRadius : 1.0f};
  const float sinPhi{filmRadius > 0 ? film.y / filmRadius : 0.0f};
  return {cosPhi * x - sinPhi * y, sinPhi * x + cosPhi * y};
}

float ExitPupil::areaFraction(float filmRadius) const noexcept {
  const Bound &bound{boundAt(filmRadius)};
  return PI * bound.semiAxes.x * bound.semiAxes.y * mInvApertureArea;
}

void ExitPupil::logSummary() const {
  SMDL_LOG_INFO("Lens pupil bounds: ", mBounds.size(), " film radii",
                mWavelengthRange
                    ? smdl::concat(" over ",
                                   smdl::Brief(mWavelengthRange->x, 5), "-",
                                   smdl::Brief(mWavelengthRange->y, 5), " nm")
                    : std::string(),
                ", drawing the pupil point from ", 100 * areaFraction(0.0f),
                "% of the rear aperture in the middle of the frame and ",
                100 * areaFraction(mMaxFilmRadius), "% at the corner");
}

std::optional<float> Lens::fieldAngleAt(float filmRadius) const noexcept {
  return Probe{*this, mReferenceIndices}.fieldAngleAt(filmRadius);
}

float Lens::imageCircleRadius() const noexcept {
  return Probe{*this, mReferenceIndices}.imageCircleRadius();
}

float Lens::transmittedArea(float filmRadius) const noexcept {
  const Probe probe{*this, mReferenceIndices};
  const PupilRect seed{probe.seedPupilRect(filmRadius)};
  if (seed.isEmpty()) return 0;
  int numPassed{0};
  probe.eachPupilPoint(filmRadius, seed, NUM_TRANSMITTED_AREA_STEPS,
                       [&](float, float) { numPassed++; });
  const float2 extent{seed.halfExtent()};
  return 4 * extent.x * extent.y * float(numPassed) /
         float(NUM_TRANSMITTED_AREA_STEPS * NUM_TRANSMITTED_AREA_STEPS);
}

std::optional<float> Lens::filmRadiusForFieldAngle(float angle) const noexcept {
  return Probe{*this, mReferenceIndices}.filmRadiusForFieldAngle(angle);
}

float Lens::lateralColorAt(float filmRadius) const noexcept {
  const std::optional<float> angle{fieldAngleAt(filmRadius)};
  if (!angle) return std::numeric_limits<float>::quiet_NaN();
  const Indices indicesF{indicesAt(smdl::FRAUNHOFER_F_LINE)};
  const Indices indicesC{indicesAt(smdl::FRAUNHOFER_C_LINE)};
  const std::optional<float> radiusF{
      Probe{*this, indicesF}.filmRadiusForFieldAngle(*angle)};
  const std::optional<float> radiusC{
      Probe{*this, indicesC}.filmRadiusForFieldAngle(*angle)};
  if (!radiusF || !radiusC) return std::numeric_limits<float>::quiet_NaN();
  return *radiusF - *radiusC;
}
