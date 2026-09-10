#include "Render/Lens.h"

#include <cmath>
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

// The paraxial ray transfer matrix, in reduced coordinates: it acts on
// the pair (height, index times slope) rather than (height, slope), so
// every factor has unit determinant and a surface's matrix says the same
// thing whatever glass surrounds it. That is what makes the pupil solves
// below one division each.
class ABCD final {
public:
  float a{1}, b{}, c{}, d{1};

  // `next` acts after this, so it multiplies from the left.
  [[nodiscard]] ABCD then(const ABCD &next) const noexcept {
    return ABCD{next.a * a + next.b * c, next.a * b + next.b * d,
                next.c * a + next.d * c, next.c * b + next.d * d};
  }

  [[nodiscard]] static ABCD refraction(float power) noexcept {
    return ABCD{1, 0, -power, 1};
  }

  [[nodiscard]] static ABCD transfer(float distance, float ior) noexcept {
    return ABCD{1, distance / ior, 0, 1};
  }
};

[[nodiscard]] float powerOf(const LensElement &element) noexcept {
  return element.radius == 0
             ? 0.0f
             : (element.iorAfter - element.iorBefore) / element.radius;
}

// The transfer from one vertex to the next, through the space between
// them, which is the space the earlier surface names.
[[nodiscard]] ABCD transferBetween(const LensElement &from,
                                   const LensElement &to) noexcept {
  return ABCD::transfer(to.z - from.z, from.iorAfter);
}

// How many passes the aspheric intersection may take, and how close to
// the surface it has to land as a fraction of the clear aperture radius.
// The conic root is a good enough seed that one correction and the pass
// that confirms it are all any surface tested here needs; eight is a
// runaway.
constexpr int MAX_NEWTON_STEPS = 8;
constexpr float NEWTON_TOLERANCE = 1e-6f;

// The sag of a surface at squared radius `u`, and its derivative with
// respect to `u`, both in scene units. False is a radius past where the
// base conic turns back on itself, which no point of the surface is.
[[nodiscard]] bool sagOf(const LensElement &element, float u, float &sag,
                         float &dSagDu) noexcept {
  const auto curvature{element.radius == 0 ? 0.0f : 1 / element.radius};
  const auto wSquared{1 - (1 + element.conic) * curvature * curvature * u};
  if (!(wSquared > 0)) return false;
  const auto w{std::sqrt(wSquared)};
  sag = curvature * u / (1 + w);
  dSagDu = 0.5f * curvature / w;
  if (element.numAsphericTerms == 0) return true;
  // The polynomial is evaluated in the millimeters its coefficients are
  // written in and converted once, here. Scaling the coefficients into
  // scene units instead would multiply the `r^18` one by 1e51 and divide
  // its argument by the same, which is off the end of a float at both
  // ends of the product.
  const auto uMM{u * (SCENE_TO_MM * SCENE_TO_MM)};
  auto poly{0.0f}, dPoly{0.0f};
  auto power{uMM};
  for (int i = 0; i < element.numAsphericTerms; i++) {
    poly += element.aspheric[i] * power * uMM;
    dPoly += element.aspheric[i] * (i + 2) * power;
    power *= uMM;
  }
  sag += poly * MM_TO_SCENE;
  dSagDu += dPoly * SCENE_TO_MM;
  return true;
}

// Refine a conic root into a root of the whole sag, the polynomial
// included, by Newton on the axial distance from the ray to the surface.
// The polynomial is a perturbation of the conic the seed came from,
// microns against a sag of millimeters on any real design, which is why
// so few passes settle it.
[[nodiscard]] bool intersectAspheric(const LensElement &element, const Ray &ray,
                                     float t, float3 &point,
                                     float3 &normal) noexcept {
  const auto tolerance{NEWTON_TOLERANCE * element.semiDiameter};
  for (int step = 0; step < MAX_NEWTON_STEPS; step++) {
    const auto p{ray(t)};
    const auto u{p.x * p.x + p.y * p.y};
    auto sag{0.0f}, dSagDu{0.0f};
    if (!sagOf(element, u, sag, dSagDu)) return false;
    const auto residual{(p.z - element.z) - sag};
    if (std::abs(residual) <= tolerance) {
      point = p;
      // The surface is `z - vertex - sag(x^2 + y^2) = 0`, so its
      // gradient is the sag's slope in the two radial directions against
      // a unit rise in z.
      normal = normalize(float3(-2 * dSagDu * p.x, -2 * dSagDu * p.y, 1.0f));
      if (dot(normal, ray.dir) > 0) normal = -normal;
      return true;
    }
    // How fast the residual closes: the ray climbing in z against the
    // surface receding under it as the radius grows.
    const auto slope{ray.dir.z -
                     2 * dSagDu * (p.x * ray.dir.x + p.y * ray.dir.y)};
    if (slope == 0) return false;
    t -= residual / slope;
    if (!(t >= 0)) return false;
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
[[nodiscard]] bool intersectSurface(const LensElement &element, const Ray &ray,
                                    float3 &point, float3 &normal) noexcept {
  const auto curvature{element.radius == 0 ? 0.0f : 1 / element.radius};
  const auto kPlusOne{1 + element.conic};
  const auto ox{ray.org.x}, oy{ray.org.y}, os{ray.org.z - element.z};
  const auto &d{ray.dir};
  const auto a{curvature * (d.x * d.x + d.y * d.y + kPlusOne * d.z * d.z)};
  const auto b{2 *
               (curvature * (ox * d.x + oy * d.y + kPlusOne * os * d.z) - d.z)};
  const auto c{curvature * (ox * ox + oy * oy + kPlusOne * os * os) - 2 * os};
  auto tBest{0.0f};
  auto sBest{FLOAT_MAX};
  const auto consider{[&](float t) {
    // Zero counts: two surfaces of a cemented pair may share a vertex,
    // and an axial ray then meets the second one at no distance at all.
    if (!(t >= 0)) return;
    if (const auto s{std::abs(os + t * d.z)}; s < sBest) sBest = s, tBest = t;
  }};
  if (a == 0) {
    if (b == 0) return false;
    consider(-c / b);
  } else {
    const auto discriminant{b * b - 4 * a * c};
    if (discriminant < 0) return false;
    // The stable pairing, so that the root the subtraction would cancel
    // comes off the product of the roots instead.
    const auto root{std::sqrt(discriminant)};
    const auto q{-0.5f * (b + (b < 0 ? -root : root))};
    consider(q / a);
    if (q != 0) consider(c / q);
  }
  if (sBest == FLOAT_MAX) return false;
  // A polynomial on top of the conic is not a quadric and has no closed
  // form; the root just found is where its solve starts.
  if (element.numAsphericTerms > 0)
    return intersectAspheric(element, ray, tBest, point, normal);
  point = ray(tBest);
  normal = normalize(float3(curvature * point.x, curvature * point.y,
                            curvature * kPlusOne * (point.z - element.z) - 1));
  if (dot(normal, ray.dir) > 0) normal = -normal;
  return true;
}

// Snell's law. `normal` faces the incident side and `eta` is the index
// the ray leaves over the index it enters. False is total internal
// reflection, which a lens with no mirror in it has no answer to.
[[nodiscard]] bool refract(float3 &dir, const float3 &normal,
                           float eta) noexcept {
  const auto cosThetaI{-dot(dir, normal)};
  const auto sinThetaTSquared{eta * eta * (1 - cosThetaI * cosThetaI)};
  if (sinThetaTSquared >= 1) return false;
  dir =
      eta * dir + (eta * cosThetaI - std::sqrt(1 - sinThetaTSquared)) * normal;
  return true;
}

// Is the point inside the regular polygon of `numBlades` sides with
// circumradius `circumRadius` and a vertex at `angle`? The boundary in
// polar form is the apothem over the cosine of the angle off the nearest
// edge, which is one modulo and one cosine rather than a loop over the
// edges.
[[nodiscard]] bool isInsideBlades(float x, float y, int numBlades,
                                  float circumRadius, float angle) noexcept {
  const auto sector{TWO_PI / float(numBlades)};
  auto phi{std::atan2(y, x) - angle};
  phi -= sector * std::floor(phi / sector);
  const auto apothem{circumRadius * std::cos(PI / float(numBlades))};
  const auto boundary{apothem / std::cos(phi - 0.5f * sector)};
  return x * x + y * y <= boundary * boundary;
}

// How many film radii the exit pupil is tabulated at, how many radii
// within one entry's span are traced (the ends included, so the entry
// bounds the whole span and not just its middle), and the two grids over
// the aperture that find the region: a coarse one over the whole of it,
// then a fine one over what the coarse one found.
constexpr size_t NUM_PUPIL_RADII = 64;
constexpr int NUM_PUPIL_FILM_STEPS = 4;
constexpr int NUM_PUPIL_COARSE_STEPS = 64;
constexpr int NUM_PUPIL_FINE_STEPS = 64;

// An axis-aligned rectangle on the plane of the rear vertex, grown by
// folding in the points that got out. The default is empty.
struct PupilRect final {
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

  [[nodiscard]] bool isEmpty() const noexcept { return !(loX <= hiX); }

  [[nodiscard]] float2 center() const noexcept {
    return float2(0.5f * (loX + hiX), 0.5f * (loY + hiY));
  }

  [[nodiscard]] float2 halfExtent() const noexcept {
    return float2(0.5f * (hiX - loX), 0.5f * (hiY - loY));
  }

  float loX{+FLOAT_MAX}, hiX{-FLOAT_MAX};
  float loY{+FLOAT_MAX}, hiY{-FLOAT_MAX};
};

// The ellipse a pupil point is drawn inside, on the plane of the rear
// vertex, in the frame where the film point lies on the +x axis.
struct PupilEllipse final {
  float2 center{}, semiAxes{};
};

// The whole rear aperture as a rectangle, which is the domain a pupil
// point was drawn from before there was a table and is still the domain
// the weights are relative to.
[[nodiscard]] PupilRect apertureRect(const Lens &lens) noexcept {
  auto rect{PupilRect{}};
  rect.extend(-lens.rearApertureRadius(), -lens.rearApertureRadius());
  rect.extend(+lens.rearApertureRadius(), +lens.rearApertureRadius());
  return rect;
}

// Grid `over` and hand `visit` every point that a ray from a film point
// `filmRadius` off axis, on the +x side, gets out through. This is the
// whole cost of building the table.
template <typename F>
void eachPupilPoint(const Lens &lens, float filmRadius, const PupilRect &over,
                    int numSteps, F &&visit) noexcept {
  const auto radius{lens.rearApertureRadius()};
  const float3 org{filmRadius, 0.0f, lens.filmZ()};
  for (int i = 0; i < numSteps; i++) {
    const auto x{over.loX + (over.hiX - over.loX) * (i + 0.5f) / numSteps};
    for (int j = 0; j < numSteps; j++) {
      const auto y{over.loY + (over.hiY - over.loY) * (j + 0.5f) / numSteps};
      // The aperture is a disk and the grid is a rectangle, so the
      // corners of the grid lie outside the domain and stay out of the
      // bound: a draw landing there is blocked either way, and letting
      // it widen the bound would only make every other draw likelier to
      // land there too.
      if (x * x + y * y > radius * radius) continue;
      auto ray{Ray{org, float3(x, y, lens.rearZ()) - org, EPS, INF}};
      if (lens.traceFromFilm(ray)) visit(x, y);
    }
  }
}

[[nodiscard]] PupilRect gridPupil(const Lens &lens, float filmRadius,
                                  const PupilRect &over,
                                  int numSteps) noexcept {
  auto rect{PupilRect{}};
  eachPupilPoint(lens, filmRadius, over, numSteps,
                 [&](float x, float y) { rect.extend(x, y); });
  return rect;
}

// Bound what every film point between `filmRadius0` and `filmRadius1` can
// see. Nothing getting out anywhere in the span gives back the whole
// aperture, which costs the draws it always cost and leaves the estimator
// exactly what it was.
//
// The bound is an ellipse rather than the rectangle it is found as. What
// a film point sees is the aperture stop and the clear apertures in
// front of it, all disks, all projected onto this plane and intersected:
// a convex region bounded by circular arcs, which one disk alone often
// limits. An ellipse on that region's own bounding box contains it and
// wastes a fifth less area than the box does, and exactly none when one
// disk is doing the limiting.
[[nodiscard]] PupilEllipse boundPupil(const Lens &lens, float filmRadius0,
                                      float filmRadius1) noexcept {
  const auto whole{apertureRect(lens)};
  const auto radius{lens.rearApertureRadius()};
  const auto wholeEllipse{PupilEllipse{whole.center(), float2(radius, radius)}};
  const auto filmRadiusAt{[&](int k) {
    return filmRadius0 +
           (filmRadius1 - filmRadius0) * float(k) / (NUM_PUPIL_FILM_STEPS - 1);
  }};
  auto coarse{PupilRect{}};
  for (int k = 0; k < NUM_PUPIL_FILM_STEPS; k++)
    coarse.extend(
        gridPupil(lens, filmRadiusAt(k), whole, NUM_PUPIL_COARSE_STEPS));
  if (coarse.isEmpty()) return wholeEllipse;
  // Widen by two cells before gridding again, since the region reaches
  // past the outermost sample that landed in it, and then grid a second
  // time over that. The second pass is the whole point: it makes the
  // padding below a fraction of the region rather than a fraction of the
  // aperture, which at a small aperture is the difference between a
  // useful bound and no bound at all.
  const auto coarseCell{2 * radius / NUM_PUPIL_COARSE_STEPS};
  coarse.expand(2 * coarseCell, 2 * coarseCell);
  coarse.clampTo(whole);
  auto fine{PupilRect{}};
  for (int k = 0; k < NUM_PUPIL_FILM_STEPS; k++)
    fine.extend(gridPupil(lens, filmRadiusAt(k), coarse, NUM_PUPIL_FINE_STEPS));
  if (fine.isEmpty()) return wholeEllipse;
  const auto cell{float2((coarse.hiX - coarse.loX) / NUM_PUPIL_FINE_STEPS,
                         (coarse.hiY - coarse.loY) / NUM_PUPIL_FINE_STEPS)};
  const auto center{fine.center()};
  const auto extent{float2(std::max(fine.halfExtent().x, cell.x),
                           std::max(fine.halfExtent().y, cell.y))};
  // How far the region reaches in units of that box, which decides how
  // much the ellipse on the box has to grow to contain it: 1 whenever it
  // already does, which is every case where one disk is the limit. The
  // grid is walked again rather than remembered, the second walk being
  // cheaper than the tens of thousands of points it would hold.
  auto scale{1.0f};
  for (int k = 0; k < NUM_PUPIL_FILM_STEPS; k++)
    eachPupilPoint(lens, filmRadiusAt(k), coarse, NUM_PUPIL_FINE_STEPS,
                   [&](float x, float y) {
                     const auto u{(x - center.x) / extent.x};
                     const auto v{(y - center.y) / extent.y};
                     scale = std::max(scale, std::sqrt(u * u + v * v));
                   });
  return PupilEllipse{center, scale * extent + cell};
}

} // namespace

Lens::Lens(const LensPrescription &prescription, const LensOptions &options) {
  const auto &surfaces{prescription.surfaces};
  if (surfaces.empty())
    throw smdl::Error("expected a lens with at least one surface");
  mStopIndex = prescription.stopIndex();
  if (mStopIndex == surfaces.size())
    throw smdl::Error("expected a lens with an aperture stop, which is what "
                      "decides how much light it gathers");
  mName = prescription.name;
  // Lay the surfaces out on the axis from the front vertex, carrying the
  // index of a space across the stop, which stands in a space rather than
  // ending one.
  mElements.resize(surfaces.size());
  auto ior{1.0f};
  auto z{0.0f};
  for (size_t i = 0; i < surfaces.size(); i++) {
    const auto &surface{surfaces[i]};
    auto &element{mElements[i]};
    element.z = z * MM_TO_SCENE;
    element.radius = surface.radius * MM_TO_SCENE;
    element.conic = surface.conic;
    if (surface.aspheric.size() > LENS_MAX_ASPHERIC_TERMS)
      throw smdl::Error(
          smdl::concat("expected at most ", LENS_MAX_ASPHERIC_TERMS,
                       " aspheric coefficients on a surface, got ",
                       surface.aspheric.size()));
    // A table that prints its unused terms leaves trailing zeros, and a
    // polynomial that is all zeros is a conic. Dropping them is what
    // keeps such a surface on the closed-form path.
    auto numTerms{surface.aspheric.size()};
    while (numTerms > 0 && surface.aspheric[numTerms - 1] == 0) numTerms--;
    element.numAsphericTerms = int(numTerms);
    for (size_t j = 0; j < numTerms; j++)
      element.aspheric[j] = surface.aspheric[j];
    element.semiDiameter = 0.5f * surface.diameter * MM_TO_SCENE;
    element.iorBefore = ior;
    element.iorAfter = surface.isStop ? ior : surface.ior;
    element.isStop = surface.isStop;
    ior = element.iorAfter;
    z += surface.thickness;
  }
  if (mElements.back().iorAfter != 1)
    throw smdl::Error(smdl::concat(
        "expected air behind the last surface, got an index of ",
        mElements.back().iorAfter,
        ": the film is not immersed, so the prescription is missing the "
        "surface that brings the light back out"));
  mDesignBackFocus = surfaces.back().thickness * MM_TO_SCENE;

  // The system, from the plane of the front vertex to the plane of the
  // rear one. Air stands at both ends, so its determinant is 1 and every
  // cardinal point below is one of its elements over another.
  auto system{ABCD{}};
  for (size_t i = 0; i < mElements.size(); i++) {
    system = system.then(ABCD::refraction(powerOf(mElements[i])));
    if (i + 1 < mElements.size())
      system = system.then(transferBetween(mElements[i], mElements[i + 1]));
  }
  if (system.c == 0)
    throw smdl::Error("the surfaces have no net power between them, so the "
                      "prescription forms no image and has no focal length");
  mFocalLength = -1 / system.c;
  mBackFocalDistance = -system.a / system.c;
  auto frontPrincipalZ{mElements.front().z + (system.d - 1) / system.c};
  auto rearPrincipalZ{mElements.back().z + (system.a - 1) * mFocalLength};

  // The entrance pupil is the image of the stop through whatever stands
  // in front of it, so the matrix carrying a ray from the front vertex to
  // the stop plane says both where the pupil lies (the plane the stop is
  // conjugate to, where the matrix loses its `b`) and how much larger
  // than the stop it looks (the magnification, which is that matrix's
  // `a`).
  auto front{ABCD{}};
  for (size_t i = 0; i < mStopIndex; i++) {
    front = front.then(ABCD::refraction(powerOf(mElements[i])));
    front = front.then(transferBetween(mElements[i], mElements[i + 1]));
  }
  if (front.a == 0)
    throw smdl::Error("the stop sits at the front focal point of the "
                      "surfaces before it, which puts the entrance pupil at "
                      "infinity; the renderer cannot sample a telecentric "
                      "lens");
  const auto entrancePupilZ{mElements.front().z + front.b / front.a};

  // The exit pupil, the same solve through whatever stands behind the
  // stop, read from the other end of the matrix.
  auto rear{ABCD{}};
  for (size_t i = mStopIndex; i + 1 < mElements.size(); i++) {
    rear = rear.then(transferBetween(mElements[i], mElements[i + 1]));
    rear = rear.then(ABCD::refraction(powerOf(mElements[i + 1])));
  }
  if (rear.d == 0)
    throw smdl::Error("the stop sits at the rear focal point of the surfaces "
                      "behind it, which puts the exit pupil at infinity");
  mExitPupilZ = mElements.back().z - rear.b / rear.d;

  // The entrance pupil is the origin, which is what `look_from` names and
  // what `focus` is measured from, so everything shifts onto it now that
  // it is known.
  for (auto &element : mElements) element.z -= entrancePupilZ;
  mExitPupilZ -= entrancePupilZ;
  frontPrincipalZ -= entrancePupilZ;
  rearPrincipalZ -= entrancePupilZ;

  const auto pupilMagnification{std::abs(front.a)};
  const auto stopRadius{mElements[mStopIndex].semiDiameter};
  mFNumberWideOpen = mFocalLength * pupilMagnification / (2 * stopRadius);
  auto workingStopRadius{stopRadius};
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
    const auto n{float(mNumBlades)};
    mBladeCircumRadius =
        workingStopRadius * std::sqrt(TWO_PI / (n * std::sin(TWO_PI / n)));
  }

  // Focus by moving the film, which is what a lens whose elements do not
  // move does. The thick lens images an object `objectDistance` ahead of
  // the front principal plane at `f s / (s - f)` behind the rear one, and
  // an object at infinity lands exactly on the rear focal point.
  mFocusDistance = options.focusDistance;
  if (!(mFocusDistance >= 0))
    throw smdl::Error("expected a nonnegative focus distance");
  if (mFocusDistance == 0) {
    // Focus at infinity, which is the limit of the solve below and is
    // what the film sitting on the rear focal point means.
    mFilmZ = rearPrincipalZ + mFocalLength;
  } else {
    const auto objectDistance{frontPrincipalZ + mFocusDistance};
    if (!(objectDistance > mFocalLength))
      throw smdl::Error(smdl::concat(
          "cannot focus at ", mFocusDistance,
          " scene units: that is inside the lens's front focal point, ",
          (mFocalLength - frontPrincipalZ),
          " scene units out, and no film position images it"));
    mFilmZ = rearPrincipalZ +
             mFocalLength * objectDistance / (objectDistance - mFocalLength);
  }
}

bool Lens::traceFromFilm(Ray &ray) const noexcept {
  ray.dir = normalize(ray.dir);
  // Backward through the list, since the file runs front to film and the
  // light here runs the other way: at every surface the ray leaves the
  // space on the film side and enters the one on the scene side.
  for (size_t i = mElements.size(); i-- > 0;) {
    const auto &element{mElements[i]};
    float3 point{}, normal{};
    if (!intersectSurface(element, ray, point, normal)) return false;
    if (point.x * point.x + point.y * point.y >
        element.semiDiameter * element.semiDiameter)
      return false;
    if (element.isStop) {
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
    if (!refract(ray.dir, normal, element.iorAfter / element.iorBefore))
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
  // A design that states its own back focus is stating what the solve
  // above should produce at infinity focus, which is the one check on a
  // transcription that costs nothing.
  if (mDesignBackFocus > 0) {
    const auto error{std::abs(mDesignBackFocus - mBackFocalDistance)};
    if (error > 0.01f * mDesignBackFocus)
      SMDL_LOG_WARN("Lens: the design states a back focus of ",
                    mDesignBackFocus * SCENE_TO_MM,
                    " mm and the surfaces solve to ",
                    mBackFocalDistance * SCENE_TO_MM,
                    " mm; one of the two is transcribed wrong");
  }
}

ExitPupil::ExitPupil(const Lens &lens, float maxFilmRadius) {
  mRearRadius = lens.rearApertureRadius();
  mMaxFilmRadius = maxFilmRadius;
  mBoundsPerRadius = maxFilmRadius > 0 ? NUM_PUPIL_RADII / maxFilmRadius : 0.0f;
  mInvApertureArea = 1 / (PI * mRearRadius * mRearRadius);
  mBounds.resize(NUM_PUPIL_RADII);
  // Every entry is independent, and the whole table is the one part of
  // building a camera slow enough to notice.
  smdl::parallelFor(size_t(0), mBounds.size(), [&](size_t i) {
    const auto span{maxFilmRadius / NUM_PUPIL_RADII};
    const auto ellipse{boundPupil(lens, span * i, span * (i + 1))};
    mBounds[i].center = ellipse.center;
    mBounds[i].semiAxes = ellipse.semiAxes;
  });
}

float2 ExitPupil::sample(float2 film, float2 xi, float &weight) const noexcept {
  const auto filmRadius{length(film)};
  const auto &bound{boundAt(filmRadius)};
  const auto disk{smdl::uniformDiskSample(xi)};
  const auto x{bound.center.x + bound.semiAxes.x * disk.x};
  const auto y{bound.center.y + bound.semiAxes.y * disk.y};
  // The bound covers a span of film radii and is padded on top of that,
  // so it reaches outside the aperture near the rim. A draw landing
  // there is the zero it would have been drawn on the whole aperture.
  weight = x * x + y * y <= mRearRadius * mRearRadius
               ? PI * bound.semiAxes.x * bound.semiAxes.y * mInvApertureArea
               : 0.0f;
  // Turn the bound frame, whose +x axis is the film point's azimuth,
  // onto the film point. The turn preserves length, so the aperture test
  // above reads the same on either side of it.
  const auto cosPhi{filmRadius > 0 ? film.x / filmRadius : 1.0f};
  const auto sinPhi{filmRadius > 0 ? film.y / filmRadius : 0.0f};
  return float2(cosPhi * x - sinPhi * y, sinPhi * x + cosPhi * y);
}

float ExitPupil::areaFraction(float filmRadius) const noexcept {
  const auto &bound{boundAt(filmRadius)};
  return PI * bound.semiAxes.x * bound.semiAxes.y * mInvApertureArea;
}

void ExitPupil::logSummary() const {
  SMDL_LOG_INFO("Lens pupil bounds: ", mBounds.size(),
                " film radii, drawing the pupil point from ",
                100 * areaFraction(0.0f),
                "% of the rear aperture in the middle of the frame and ",
                100 * areaFraction(mMaxFilmRadius), "% at the corner");
}
