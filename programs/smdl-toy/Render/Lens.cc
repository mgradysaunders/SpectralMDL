#include "Render/Lens.h"

#include <cmath>
#include <limits>
#include <optional>
#include <string>

#include "smdl/RenderUtil/MonteCarlo.h"
#include "smdl/Support/Error.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Parallel.h"
#include "smdl/Support/SIMD.h"

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

// The sag of a surface at squared radius `u`, and, when the caller
// wants it, its derivative with respect to `u`, both in scene units.
// False is a radius past where the base conic turns back on itself,
// which no point of the surface is.
//
// The bracketing walk reads the sign of the residual and nothing else,
// and the derivative is a divide plus a term of polynomial apiece, so it
// asks for the sag alone. The sag itself is computed the same either
// way, which keeps the walk's decisions identical.
template <bool WantDerivative = true>
[[nodiscard]] bool sagOf(const LensElement &elem, float u, float &sag,
                         float &dSagDu) noexcept {
  const float kappa{elem.curvature()};
  const float wSq{1 - (1 + elem.conic) * kappa * kappa * u};
  if (SMDL_UNLIKELY(!(wSq > 0))) return false;
  const float w{std::sqrt(wSq)};
  sag = kappa * u / (1 + w);
  if constexpr (WantDerivative) dSagDu = 0.5f * kappa / w;
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
      if constexpr (WantDerivative)
        dPoly += elem.aspheric[i] * (float(i) + 2) * power;
      power *= u;
    }
    sag += poly * MM_TO_SCENE;
    if constexpr (WantDerivative) dSagDu += dPoly * SCENE_TO_MM;
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

// Is the point inside the regular polygon the blades cut, whose inward
// edge normals and apothem `Lens` laid out? A convex polygon is the
// intersection of its edge half-planes, so the point is inside when it
// clears every one of them.
//
// The polar form this replaces read the angle off the nearest edge,
// which is one `atan2` and one cosine however many blades there are.
// That is fewer operations but a longer dependency chain, and the trace
// is latency bound: these dot products do not depend on each other, so
// the processor runs them together.
[[nodiscard]] bool isInsideBlades(float x, float y,
                                  smdl::Span<const float2> edgeNormals,
                                  float apothem) noexcept {
  for (const float2 &normal : edgeNormals)
    if (x * normal.x + y * normal.y > apothem) return false;
  return true;
}

// The trace, written once over a pack of `W` rays and instantiated at
// the two widths the lens is traced at: `Lens::TRACE_WIDTH` for a camera
// sample, and one for the probes and the diagnostics, which ask about a
// single ray at a time. Each early return is a lane dropped from a mask
// and each branch is both sides and a select, which is what lets the two
// widths be the same code.
//
// It is written wide because the trace is latency bound. One ray is a
// serial chain of divides and square roots too thin to fill the machine,
// and the surfaces of one ray cannot overlap, so the only way to go
// faster is to work on rays that do not depend on each other. A width of
// one compiles to the scalar instruction for each operation, so nothing
// is paid for the generality where it is not wanted.
namespace simd = smdl::simd;

template <size_t W> using Pack = simd::Pack<float, W>;
template <size_t W> using Mask = simd::Mask<float, W>;

// A direction or a point, one component to a pack.
template <size_t W> struct Vector3Pack final {
  Pack<W> x{}, y{}, z{};
};

template <size_t W>
[[nodiscard]] Pack<W> dot(const Vector3Pack<W> &a,
                          const Vector3Pack<W> &b) noexcept {
  return a.x * b.x + a.y * b.y + a.z * b.z;
}

template <size_t W> void normalize(Vector3Pack<W> &v) noexcept {
  const Pack<W> length{simd::sqrt(dot<W>(v, v))};
  const Pack<W> inverse{
      simd::select(length > Pack<W>(0.0f), Pack<W>(1.0f) / length, Pack<W>(0.0f))};
  v.x = v.x * inverse, v.y = v.y * inverse, v.z = v.z * inverse;
}

// `sagOf` over a pack. Lanes past where the base conic turns back on
// itself leave `ok`, and are held at a finite value so that the
// arithmetic the live lanes need cannot trip over them.
template <size_t W>
void sagOf(const LensElement &elem, const Pack<W> &u, Pack<W> &sag,
           Pack<W> &dSagDu, Mask<W> &ok) noexcept {
  const Pack<W> kappa{elem.curvature()};
  const Pack<W> wSq{Pack<W>(1.0f) - Pack<W>(1 + elem.conic) * kappa * kappa * u};
  const Mask<W> live{wSq > Pack<W>(0.0f)};
  ok = ok & live;
  const Pack<W> w{simd::sqrt(simd::select(live, wSq, Pack<W>(1.0f)))};
  sag = kappa * u / (Pack<W>(1.0f) + w);
  dSagDu = Pack<W>(0.5f) * kappa / w;
  if (SMDL_UNLIKELY(elem.numAsphericTerms > 0)) {
    const Pack<W> uMM{u * Pack<W>(SCENE_TO_MM) * Pack<W>(SCENE_TO_MM)};
    Pack<W> poly{0.0f}, dPoly{0.0f}, power{uMM};
    for (int i = 0; i < elem.numAsphericTerms; i++) {
      const Pack<W> coeff{elem.aspheric[i]};
      poly = poly + coeff * power * uMM;
      dPoly = dPoly + coeff * Pack<W>(float(i) + 2) * power;
      power = power * uMM;
    }
    sag = sag + poly * Pack<W>(MM_TO_SCENE);
    dSagDu = dSagDu + dPoly * Pack<W>(SCENE_TO_MM);
  }
}

// `spanOverElement` over a pack.
template <size_t W>
void spanOverElement(const LensElement &elem, const Vector3Pack<W> &org,
                     const Vector3Pack<W> &dir, Pack<W> &tLo, Pack<W> &tHi,
                     Mask<W> &ok) noexcept {
  const Pack<W> zero{0.0f};
  tLo = zero;
  tHi = Pack<W>(FLOAT_MAX);
  const Pack<W> a{dir.x * dir.x + dir.y * dir.y};
  const Pack<W> b{Pack<W>(2.0f) * (org.x * dir.x + org.y * dir.y)};
  const Pack<W> c{org.x * org.x + org.y * org.y -
                  Pack<W>(elem.radialLimit * elem.radialLimit)};
  const Pack<W> discrim{b * b - Pack<W>(4.0f) * a * c};
  // A ray that runs along the axis never leaves the radius, so the
  // cylinder bounds nothing and all that matters is whether it started
  // inside. Only a ray that actually crosses the wall is bounded by it.
  const Mask<W> crossing{a > zero};
  ok = ok & ((crossing & (discrim > zero)) | (~crossing & (c <= zero)));
  const Pack<W> root{simd::sqrt(simd::select(discrim > zero, discrim, zero))};
  // The stable pairing of the roots, `q / a` and `c / q`, which takes
  // the root the subtraction would cancel off the product instead.
  const Pack<W> q{Pack<W>(-0.5f) *
                  (b + simd::select(b >= zero, root, -root))};
  const Pack<W> t1{q / simd::select(crossing, a, Pack<W>(1.0f))};
  const Pack<W> t2{c / simd::select(q != zero, q, Pack<W>(1.0f))};
  tLo = simd::select(crossing, simd::max(tLo, simd::min(t1, t2)), tLo);
  tHi = simd::select(crossing, simd::min(tHi, simd::max(t1, t2)), tHi);
  // The band of sag the cap covers, which is a pair of planes on z. A
  // ray square across the axis stays inside it forever, and is held only
  // by whether it started inside.
  const Pack<W> zLo{elem.z + elem.sagMin}, zHi{elem.z + elem.sagMax};
  const Mask<W> rising{dir.z > zero}, falling{dir.z < zero};
  const Mask<W> level{~rising & ~falling};
  const Pack<W> dz{simd::select(level, Pack<W>(1.0f), dir.z)};
  const Pack<W> s1{(zLo - org.z) / dz}, s2{(zHi - org.z) / dz};
  tLo = simd::select(rising, simd::max(tLo, s1), tLo);
  tHi = simd::select(rising, simd::min(tHi, s2), tHi);
  tLo = simd::select(falling, simd::max(tLo, s2), tLo);
  tHi = simd::select(falling, simd::min(tHi, s1), tHi);
  ok = ok & (~level | ((zLo <= org.z) & (org.z <= zHi)));
  ok = ok & (tLo <= tHi);
}

// `intersectAspheric` over a pack: bracket, walk the span to the first
// crossing, then a safeguarded Newton. Every lane runs every step and
// the finished ones are held by their mask, so the batch costs the
// deepest lane rather than the sum of them.
template <size_t W>
void intersectAspheric(const LensElement &elem, const Vector3Pack<W> &org,
                       const Vector3Pack<W> &dir, Vector3Pack<W> &point,
                       Vector3Pack<W> &normal, Mask<W> &alive) noexcept {
  Pack<W> tLo{}, tHi{};
  Mask<W> ok{alive};
  spanOverElement<W>(elem, org, dir, tLo, tHi, ok);
  const Pack<W> tolerance{SOLVE_TOLERANCE * elem.semiDiameter};
  Pack<W> sag{}, dSagDu{};
  // The scalar solve returns the moment it lands, so a lane that has
  // landed must stop taking part: its slope is what the normal is built
  // from, and a later step would overwrite it, and a later step's radius
  // could drop it from `ok` after it had already succeeded.
  Mask<W> done{false};
  Pack<W> tBest{}, dSagDuBest{};
  const auto probe{[&](const Pack<W> &t) {
    const Pack<W> px{org.x + t * dir.x}, py{org.y + t * dir.y};
    Mask<W> valid{true};
    sagOf<W>(elem, px * px + py * py, sag, dSagDu, valid);
    ok = ok & (valid | done);
    return (org.z + t * dir.z) - Pack<W>(elem.z) - sag;
  }};
  const auto land{[&](const Mask<W> &accepts, const Pack<W> &t) {
    tBest = simd::select(accepts, t, tBest);
    dSagDuBest = simd::select(accepts, dSagDu, dSagDuBest);
    done = done | accepts;
  }};
  Pack<W> height{probe(tLo)};
  land(simd::abs(height) <= tolerance, tLo);
  const Mask<W> isBelowAtLo{height < Pack<W>(0.0f)};
  Pack<W> tA{tLo}, tB{tLo};
  Mask<W> isBracketed{done};
  // Once every lane has landed or been bracketed the walk has nothing
  // left to find, which at width one is the first bracket.
  for (int i = 1; i <= NUM_SPAN_STEPS && !simd::allTrue(done | isBracketed);
       i++) {
    const Pack<W> t{tLo + (tHi - tLo) * Pack<W>(float(i) / NUM_SPAN_STEPS)};
    const Pack<W> h{probe(t)};
    const Mask<W> pending{~done & ~isBracketed};
    const Mask<W> accepts{pending & (simd::abs(h) <= tolerance)};
    land(accepts, t);
    const Mask<W> crosses{pending & ~accepts & ((h < Pack<W>(0.0f)) ^ isBelowAtLo)};
    tB = simd::select(crosses, t, tB);
    isBracketed = isBracketed | crosses;
    tA = simd::select(pending & ~accepts & ~crosses, t, tA);
  }
  Mask<W> solving{ok & isBracketed & ~done};
  Pack<W> t{(tA + tB) * Pack<W>(0.5f)};
  for (int step = 0; step < MAX_SOLVE_STEPS && simd::anyTrue(solving); step++) {
    const Pack<W> h{probe(t)};
    const Mask<W> accepts{solving & (simd::abs(h) <= tolerance)};
    land(accepts, t);
    solving = solving & ~accepts;
    const Mask<W> below{~((h < Pack<W>(0.0f)) ^ isBelowAtLo)};
    tA = simd::select(solving & below, t, tA);
    tB = simd::select(solving & ~below, t, tB);
    // How fast the height closes: the ray climbing in z against the
    // surface receding under it as the radius grows.
    const Pack<W> px{org.x + t * dir.x}, py{org.y + t * dir.y};
    const Pack<W> slope{dir.z - Pack<W>(2.0f) * dSagDu * (px * dir.x + py * dir.y)};
    const Mask<W> sloped{slope != Pack<W>(0.0f)};
    const Pack<W> step2{t - h / simd::select(sloped, slope, Pack<W>(1.0f))};
    const Pack<W> next{simd::select(sloped, step2, Pack<W>(FLOAT_MAX))};
    t = simd::select((tA < next) & (next < tB), next, (tA + tB) * Pack<W>(0.5f));
  }
  alive = alive & ok & done;
  point.x = org.x + tBest * dir.x;
  point.y = org.y + tBest * dir.y;
  point.z = org.z + tBest * dir.z;
  normal.x = -Pack<W>(2.0f) * dSagDuBest * point.x;
  normal.y = -Pack<W>(2.0f) * dSagDuBest * point.y;
  normal.z = Pack<W>(1.0f);
  normalize<W>(normal);
}

// `intersectSurface` over a pack. The flat and curved cases are chosen
// by the surface rather than by the ray, so they stay a scalar branch.
template <size_t W>
void intersectSurface(const LensElement &elem, const Vector3Pack<W> &org,
                      const Vector3Pack<W> &dir, Vector3Pack<W> &point,
                      Vector3Pack<W> &normal, Mask<W> &alive) noexcept {
  const float curvature{elem.curvature()};
  const Pack<W> kappa{curvature};
  const Pack<W> kPlus1{1 + elem.conic};
  const Pack<W> os{org.z - Pack<W>(elem.z)};
  if (SMDL_UNLIKELY(elem.numAsphericTerms > 0)) {
    intersectAspheric<W>(elem, org, dir, point, normal, alive);
  } else if (curvature == 0) {
    const Mask<W> crossing{dir.z != Pack<W>(0.0f)};
    const Pack<W> t{-os / simd::select(crossing, dir.z, Pack<W>(1.0f))};
    alive = alive & crossing & (t >= Pack<W>(0.0f));
    point.x = org.x + t * dir.x;
    point.y = org.y + t * dir.y;
    point.z = org.z + t * dir.z;
    normal.x = Pack<W>(0.0f), normal.y = Pack<W>(0.0f), normal.z = Pack<W>(-1.0f);
  } else {
    const Pack<W> a{kappa *
                 (dir.x * dir.x + dir.y * dir.y + kPlus1 * dir.z * dir.z)};
    const Pack<W> b{Pack<W>(2.0f) *
                 (kappa * (org.x * dir.x + org.y * dir.y + kPlus1 * os * dir.z) -
                  dir.z)};
    const Pack<W> c{kappa * (org.x * org.x + org.y * org.y + kPlus1 * os * os) -
                 Pack<W>(2.0f) * os};
    const Pack<W> discrim{b * b - Pack<W>(4.0f) * a * c};
    const Mask<W> real{discrim >= Pack<W>(0.0f)};
    alive = alive & real;
    const Pack<W> root{simd::sqrt(simd::select(real, discrim, Pack<W>(0.0f)))};
    // The stable pairing: `q` takes the sign of `b`.
    const Pack<W> q{Pack<W>(-0.5f) *
                 (b + simd::select(b >= Pack<W>(0.0f), root, -root))};
    const Mask<W> hasA{a != Pack<W>(0.0f)}, hasQ{q != Pack<W>(0.0f)};
    const Pack<W> t1{q / simd::select(hasA, a, Pack<W>(1.0f))};
    const Pack<W> t2{c / simd::select(hasQ, q, Pack<W>(1.0f))};
    // Of the roots that lie ahead, the one nearest the vertex.
    const Pack<W> s1{simd::abs(os + t1 * dir.z)};
    const Pack<W> s2{simd::abs(os + t2 * dir.z)};
    const Mask<W> ahead1{hasA & (t1 >= Pack<W>(0.0f))};
    const Mask<W> ahead2{hasQ & (t2 >= Pack<W>(0.0f))};
    const Pack<W> t{simd::select(ahead1 & (~ahead2 | (s1 < s2)), t1, t2)};
    alive = alive & (ahead1 | ahead2);
    point.x = org.x + t * dir.x;
    point.y = org.y + t * dir.y;
    point.z = org.z + t * dir.z;
    normal.x = kappa * point.x;
    normal.y = kappa * point.y;
    normal.z = kappa * kPlus1 * (point.z - Pack<W>(elem.z)) - Pack<W>(1.0f);
    normalize<W>(normal);
  }
  const Mask<W> flipped{dot<W>(normal, dir) > Pack<W>(0.0f)};
  normal.x = simd::select(flipped, -normal.x, normal.x);
  normal.y = simd::select(flipped, -normal.y, normal.y);
  normal.z = simd::select(flipped, -normal.z, normal.z);
}

// `refract` over a pack, with `eta` per lane so that a batch traced at
// several wavelengths refracts each ray at its own indices.
template <size_t W>
void refract(Vector3Pack<W> &dir, const Vector3Pack<W> &normal,
             const Pack<W> &eta, Mask<W> &alive) noexcept {
  const Pack<W> cosThetaI{-dot<W>(dir, normal)};
  const Pack<W> sin2ThetaT{eta * eta * (Pack<W>(1.0f) - cosThetaI * cosThetaI)};
  const Mask<W> transmits{sin2ThetaT < Pack<W>(1.0f)};
  alive = alive & transmits;
  const Pack<W> scale{eta * cosThetaI -
                   simd::sqrt(Pack<W>(1.0f) -
                              simd::select(transmits, sin2ThetaT, Pack<W>(0.0f)))};
  dir.x = eta * dir.x + scale * normal.x;
  dir.y = eta * dir.y + scale * normal.y;
  dir.z = eta * dir.z + scale * normal.z;
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
    throw smdl::Error("Expected a lens with at least one surface");
  if (surfaces.size() > LENS_MAX_SURFACES)
    throw smdl::Error(smdl::concat("Expected at most ", LENS_MAX_SURFACES,
                                   " surfaces in a lens, got ",
                                   surfaces.size()));
  mStopIndex = prescription.stopIndex();
  if (mStopIndex == surfaces.size())
    throw smdl::Error("Expected a lens with an aperture stop, which is what "
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
          smdl::concat("Expected at most ", LENS_MAX_ASPHERIC_TERMS,
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
        "Expected air behind the last surface, got ",
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
    throw smdl::Error("The surfaces have no net power between them, so the "
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
    throw smdl::Error("The stop sits at the front focal point of the "
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
    throw smdl::Error("The stop sits at the rear focal point of the surfaces "
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
          "Cannot open the lens to f/", options.fStop, ": its stop is ",
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
    const float circumRadius{
        workingStopRadius * std::sqrt(TWO_PI / (n * std::sin(TWO_PI / n)))};
    // The half planes the trace tests against. `bladeAngle` is measured
    // so that zero puts a vertex at screen right, so the edge between
    // two vertices faces half a sector off that.
    const float sector{TWO_PI / n};
    mBladeApothem = circumRadius * std::cos(0.5f * sector);
    mBladeEdgeNormals.resize(mNumBlades);
    for (int i = 0; i < mNumBlades; i++) {
      const float phi{mBladeAngle + (float(i) + 0.5f) * sector};
      mBladeEdgeNormals[i] = float2(std::cos(phi), std::sin(phi));
    }
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
    throw smdl::Error("Expected a nonnegative focus distance");
  const std::optional<float> imageZ{points.imagePlane(mFocusDistance)};
  if (!imageZ)
    throw smdl::Error(smdl::concat(
        "Cannot focus at ", mFocusDistance,
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

template <size_t W>
void Lens::traceBatch(smdl::Span<Ray> rays, smdl::Span<bool> passes,
                      smdl::Span<const smdl::Span<const float>> indices)
    const noexcept {
  SMDL_SANITY_CHECK(rays.size() == passes.size());
  SMDL_SANITY_CHECK(rays.size() == indices.size());
  SMDL_SANITY_CHECK(rays.size() <= W);
  // A short batch runs at full width with the absent lanes dead, which
  // costs the same and keeps one code path.
  const size_t count{rays.size()};
  alignas(32) std::array<float, W> ox{}, oy{}, oz{};
  alignas(32) std::array<float, W> dx{}, dy{}, dz{};
  Mask<W> alive{false};
  for (size_t k = 0; k < count; k++) {
    ox[k] = rays[k].org.x, oy[k] = rays[k].org.y, oz[k] = rays[k].org.z;
    dx[k] = rays[k].dir.x, dy[k] = rays[k].dir.y, dz[k] = rays[k].dir.z;
    alive.values[k] = -1;
  }
  // A dead lane still divides and takes square roots alongside the live
  // ones, so give it a direction that stays finite through all of it.
  for (size_t k = count; k < W; k++) dz[k] = 1.0f;

  Vector3Pack<W> org{Pack<W>::load(ox.data()), Pack<W>::load(oy.data()),
                     Pack<W>::load(oz.data())};
  Vector3Pack<W> dir{Pack<W>::load(dx.data()), Pack<W>::load(dy.data()),
                     Pack<W>::load(dz.data())};
  normalize<W>(dir);
  for (size_t i = mElements.size(); i-- > 0;) {
    if (!simd::anyTrue(alive)) break;
    const LensElement &elem{mElements[i]};
    Vector3Pack<W> point{}, normal{};
    intersectSurface<W>(elem, org, dir, point, normal, alive);
    alive = alive & (point.x * point.x + point.y * point.y <=
                     Pack<W>(elem.semiDiameter * elem.semiDiameter));
    if (elem.isStop) {
      if (mNumBlades >= 3) {
        // The blade test is a handful of dot products against the edge
        // normals, which is cheaper scalar than the gather a packed
        // version would need over a count known only at run time.
        alignas(32) std::array<float, W> px{}, py{};
        point.x.store(px.data()), point.y.store(py.data());
        for (size_t k = 0; k < count; k++)
          if (alive.values[k] &&
              !isInsideBlades(px[k], py[k], mBladeEdgeNormals, mBladeApothem))
            alive.values[k] = 0;
      }
      // The stop bends nothing and has no thickness, so the ray goes on
      // from where it was.
      continue;
    }
    org = point;
    alignas(32) std::array<float, W> etas{};
    for (size_t k = 0; k < W; k++) {
      const smdl::Span<const float> &index{indices[k < count ? k : 0]};
      etas[k] = index[elem.mediumAfter] / index[elem.mediumBefore];
    }
    refract<W>(dir, normal, Pack<W>::load(etas.data()), alive);
  }

  org.x.store(ox.data()), org.y.store(oy.data()), org.z.store(oz.data());
  dir.x.store(dx.data()), dir.y.store(dy.data()), dir.z.store(dz.data());
  for (size_t k = 0; k < count; k++) {
    passes[k] = alive.values[k] != 0;
    if (!passes[k]) continue;
    rays[k].org = float3(ox[k], oy[k], oz[k]);
    rays[k].dir = float3(dx[k], dy[k], dz[k]);
  }
}

void Lens::traceFromFilm(smdl::Span<Ray> rays, smdl::Span<bool> passes,
                         smdl::Span<const smdl::Span<const float>> indices)
    const noexcept {
  traceBatch<TRACE_WIDTH>(rays, passes, indices);
}

void Lens::traceFromFilm(smdl::Span<Ray> rays,
                         smdl::Span<bool> passes) const noexcept {
  const smdl::Span<const float> reference{mReferenceIndices.data(),
                                          mReferenceIndices.size()};
  std::array<smdl::Span<const float>, TRACE_WIDTH> indices{};
  indices.fill(reference);
  traceFromFilm(rays, passes,
                smdl::Span<const smdl::Span<const float>>(indices.data(),
                                                          rays.size()));
}

bool Lens::traceThrough(Ray &ray, const float *indices) const noexcept {
  bool passes{};
  const smdl::Span<const float> span{indices, mMedia.size()};
  traceBatch<1>(smdl::Span<Ray>(&ray, 1), smdl::Span<bool>(&passes, 1),
                smdl::Span<const smdl::Span<const float>>(&span, 1));
  return passes;
}


void Lens::logSummary() const {
  // TODO Having all of the log info messages emitted separately, all prefixed
  // by a different form "Lens (something):", is ugly. We should prepare the
  // entire info message in an intermediate, nicely formatted multiline string
  // (perhaps with indentation or bullets) and emit the entire info message once
  // at the end
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
