/// \file
/// The traced lens: a prescription laid out on the camera's own axis in
/// scene units, with the paraxial quantities every camera setting is
/// derived from.
///
/// The camera-space origin is the entrance pupil. That is already where a
/// pinhole camera's origin sits, it is the point a pan turns about
/// without parallax, and it makes `look_from` and `focus` mean with a
/// lens what they mean without one. The film is at positive z behind it,
/// the surfaces run toward negative z in front of it, and a ray leaving
/// the front element is already travelling the way the camera looks.
#pragma once

#include <array>
#include <optional>
#include <string>
#include <vector>

#include "smdl/RenderUtil/OpticalGlass.h"
#include "smdl/Support/Span.h"

#include "Common.h"

#include "Layout/LensFile.h"

/// What this render asks of the prescription: where the lens is focused
/// and how far it is stopped down.
struct LensOptions final {
  /// The focus distance in scene units, from the entrance pupil, or 0
  /// to focus at infinity, which puts the film on the rear focal point.
  float focusDistance{};

  /// The f-number to stop down to, or 0 to leave the lens wide open at
  /// the stop diameter the prescription states.
  float fStop{};

  /// The number of aperture blades, or 0 for a round stop. The polygon
  /// has the same area as the round stop it replaces, so the f-number
  /// keeps meaning what it says.
  int numBlades{};

  /// With `numBlades`, the rotation of the polygon in radians, measured
  /// so that zero puts a vertex at screen right, as the thin lens does.
  float bladeAngle{};
};

/// One surface as the trace sees it: on the camera's z axis, in scene
/// units, with the medium on each side named.
class LensElement final {
public:
  /// The camera-space z of the vertex. Surfaces run from the front
  /// element at the smallest z to the rear element at the largest, and
  /// the film sits behind them all.
  float z{};

  /// The signed curvature radius, positive when the center of curvature
  /// lies on the film side of the vertex. Zero is flat.
  float radius{};

  /// The conic constant. Zero is a sphere.
  float conic{};

  /// The even aspheric coefficients added to the sag, the `r^4` term
  /// first, and how many of them there are. Trailing zeros are dropped
  /// when the element is laid out, so a count of zero is a surface the
  /// closed-form intersection answers on its own, which is every surface
  /// of every design published before roughly 1990.
  ///
  /// These alone are in the millimeters the file states, everything else
  /// here being in scene units. Scaling them would multiply the `r^18`
  /// coefficient by 1e51 and divide its argument by the same, which is
  /// off the end of a float in both directions.
  std::array<float, LENS_MAX_ASPHERIC_TERMS> aspheric{};
  int numAsphericTerms{};

  /// The clear aperture radius, which is half of what the file states.
  /// On the stop it is the working radius, so stopping down narrows it.
  float semiDiameter{};

  /// Where the surface stops being one: the clear aperture, or the
  /// radius at which the base conic turns back on itself if that comes
  /// first, held a hair inside the turn because the sag stands vertical
  /// there. The intersection searches inside this and nowhere else. Out
  /// past it an aspheric polynomial is a fit read beyond the data it was
  /// made from, where a term worth microns at the rim is worth meters,
  /// and the roots it grows there are the formula's rather than the
  /// lens's.
  float radialLimit{};

  /// The band of sag the surface covers inside `radialLimit`, which is
  /// the z its cap occupies measured from the vertex. Together with the
  /// radial limit it closes the span of a ray that the surface can be
  /// met over: a ray parallel to the axis never leaves the radius, and
  /// one square across it never leaves the band, but nothing is parallel
  /// to both.
  float sagMin{}, sagMax{};

  /// The medium on the scene side and on the film side, as indices into
  /// `Lens::media()`. Both are named because a refraction needs both. The
  /// film side of one surface and the scene side of the next are one
  /// space, and so one index, by construction.
  int mediumBefore{}, mediumAfter{};

  /// Is this the aperture stop? It refracts nothing. What it does is
  /// block whatever falls outside `semiDiameter`.
  bool isStop{};
};

/// A lens the camera can look through: the surfaces in order, and the
/// paraxial solve that says what the prescription amounts to.
///
/// Everything here is scale-invariant arithmetic on a first-order model
/// of the system, so it costs nothing and is exact for the quantities it
/// reports, which are the ones lens designs are published with. It is
/// also the check on a transcription: a prescription typed wrong almost
/// never reproduces its own focal length.
///
/// Every space is a medium, and a glass's medium disperses. Everything the
/// constructor derives is taken at the d line, and so is every trace that
/// names no other wavelength. The paraxial solve can be asked again at any
/// wavelength, which is where the lens's color is read from.
///
class Lens final {
public:
  /// Lay the prescription out on the axis, solve the cardinal points and
  /// the pupils, stop the lens down, and place the film.
  ///
  /// \throws smdl::Error  If the prescription cannot be a camera lens:
  ///                      no stop, more than `LENS_MAX_SURFACES` surfaces,
  ///                      no net power, glass against the film, a pupil at
  ///                      infinity, an aperture wider than the stop, or a
  ///                      focus distance the lens cannot reach.
  ///
  Lens(const LensPrescription &prescription, const LensOptions &options);

  /// Trace a ray from the film out into the scene: refract at every
  /// surface, and block whatever falls outside a clear aperture or
  /// outside the stop.
  ///
  /// `ray` arrives as the film point and a direction toward the lens,
  /// and leaves as the point on the front element and the unit direction
  /// departing it, both in camera space. False means the ray was
  /// blocked, which the caller weights zero rather than redrawing, so
  /// that the trace consumes no sampler dimensions of its own.
  ///
  /// Every medium refracts at its index at the d line, unless
  /// `wavelength` names another in nanometers, or `indices` holds every
  /// medium's index at one as `indicesAt()` evaluates them, which is how a
  /// caller tracing many rays at one wavelength evaluates the glasses
  /// once. A medium that does not disperse has one index at every
  /// wavelength, so a lens with no dispersion data traces the same ray
  /// either way, bit for bit.
  ///
  /// \{
  [[nodiscard]] bool traceFromFilm(Ray &ray) const noexcept;
  [[nodiscard]] bool traceFromFilm(Ray &ray, float wavelength) const noexcept;
  [[nodiscard]] bool
  traceFromFilm(Ray &ray, smdl::Span<const float> indices) const noexcept;
  /// \}

  /// Room for every medium a prescription can have: the air in front,
  /// and one space per surface. A trace at a wavelength holds its indices
  /// in one of these on the stack, so that a camera sample never
  /// allocates.
  using Indices = std::array<float, LENS_MAX_SURFACES + 1>;

  /// Each medium's index at `wavelength` nanometers, in the order of
  /// `media()`.
  [[nodiscard]] Indices indicesAt(float wavelength) const noexcept;

  /// Log what the prescription turned out to be: the line that says a
  /// transcription is right, and the only place the numbers appear in
  /// the millimeters they were written in.
  void logSummary() const;

  /// The disk a camera draws its pupil point on: the rear element's
  /// clear aperture as the prescription states it, in the plane
  /// `rearZ()`, which bounds as much of the exit pupil as any film point
  /// can possibly see.
  ///
  /// Stopping down never narrows this, even on a lens whose rear element
  /// is the stop. The sampling domain has to stay put for the fraction
  /// of it that gets through to mean what it means, which is how a
  /// smaller aperture comes to darken the picture.
  [[nodiscard]] float rearApertureRadius() const noexcept {
    return mRearApertureRadius;
  }

  /// The surfaces, front first.
  [[nodiscard]] smdl::Span<const LensElement> elements() const noexcept {
    return mElements;
  }

  /// The index of the aperture stop within `elements()`.
  [[nodiscard]] size_t stopIndex() const noexcept { return mStopIndex; }

  /// The media, one per space: the air in front of the first surface,
  /// then the space after each surface but the stop, which stands in a
  /// space rather than ending one. `LensElement::mediumBefore` and
  /// `mediumAfter` index into this.
  [[nodiscard]] smdl::Span<const smdl::OpticalGlass> media() const noexcept {
    return mMedia;
  }

  /// Each medium's index at the d line, which the lens is laid out and
  /// solved at, and traced at unless a trace names another wavelength.
  [[nodiscard]] smdl::Span<const float> referenceIndices() const noexcept {
    return mReferenceIndices;
  }

  /// Does any medium disperse?
  [[nodiscard]] bool isDispersive() const noexcept { return mIsDispersive; }

  /// The effective focal length.
  [[nodiscard]] float focalLength() const noexcept { return mFocalLength; }

  /// The distance from the rear vertex to the rear focal point, which is
  /// where the film sits when the lens is focused at infinity.
  [[nodiscard]] float backFocalDistance() const noexcept {
    return mBackFocalDistance;
  }

  /// The design's own back focus, or 0 when the file did not state one.
  /// A design that states one and does not agree with
  /// `backFocalDistance()` was transcribed wrong.
  [[nodiscard]] float designBackFocus() const noexcept {
    return mDesignBackFocus;
  }

  /// The working f-number, which is the wide-open one unless `fStop`
  /// stopped the lens down.
  [[nodiscard]] float fNumber() const noexcept { return mFNumber; }

  /// The f-number at the stop diameter the prescription states.
  [[nodiscard]] float fNumberWideOpen() const noexcept {
    return mFNumberWideOpen;
  }

  /// The working entrance pupil radius. The pupil is at the camera-space
  /// origin by construction, so it has no position to report.
  [[nodiscard]] float entrancePupilRadius() const noexcept {
    return mEntrancePupilRadius;
  }

  /// The working exit pupil radius, and the camera-space z of the plane
  /// it lies in, which is negative whenever the pupil stands in front of
  /// the entrance pupil, as it does on a double Gauss.
  [[nodiscard]] float exitPupilRadius() const noexcept {
    return mExitPupilRadius;
  }
  [[nodiscard]] float exitPupilZ() const noexcept { return mExitPupilZ; }

  /// The film plane: the paraxial solve of the focus distance, moved to
  /// where the traced axial spot is smallest. A design left with
  /// spherical aberration in it, which is every fast design, images its
  /// own zones a little off the plane the Gaussian arithmetic gives, and
  /// a short lens over a small sensor has no depth of focus to hide that
  /// in.
  [[nodiscard]] float filmZ() const noexcept { return mFilmZ; }

  /// The paraxial image plane the focus distance solves to, which is
  /// where `filmZ()` starts from before the trace moves it. The two are
  /// reported apart so that the arithmetic and the trace that refines it
  /// can be read separately.
  [[nodiscard]] float paraxialFilmZ() const noexcept { return mParaxialFilmZ; }

  /// The effective focal length, and the paraxial image plane of the
  /// focus distance, at `wavelength` nanometers. They come from the solve
  /// the constructor runs, so at the d line they are `focalLength()` and
  /// `paraxialFilmZ()` to the last bit. Elsewhere they differ from those by
  /// the lens's longitudinal color.
  ///
  /// The image plane is NaN at a wavelength whose front focal point the
  /// focus distance falls inside.
  ///
  /// \{
  [[nodiscard]] float focalLengthAt(float wavelength) const noexcept;
  [[nodiscard]] float paraxialFilmZAt(float wavelength) const noexcept;
  /// \}

  /// The angle off the axis, in radians, that light reaching a film
  /// point `filmRadius` off the axis comes in at. That is what a field
  /// of view is made of, and it is traced rather than taken from the
  /// focal length, so whatever distortion the surfaces have is in it.
  /// Empty when nothing reaches that far.
  ///
  /// The ray it measures is the middle of what actually gets out, which
  /// is the chief ray where the lens does not vignette and the middle of
  /// what survives where it does.
  [[nodiscard]] std::optional<float>
  fieldAngleAt(float filmRadius) const noexcept;

  /// The film radius that looks out at `angle` radians off the axis,
  /// which is `fieldAngleAt()` inverted and is how a stated field of
  /// view becomes a sensor size. Empty when the lens does not reach that
  /// far.
  [[nodiscard]] std::optional<float>
  filmRadiusForFieldAngle(float angle) const noexcept;

  /// How far apart the F and C lines land on the film, in scene units,
  /// from what a film point `filmRadius` off the axis sees at the d line:
  /// the film radius the F line's chief ray from there reaches, less the
  /// C line's. That is the lens's lateral color at that radius, zero for a
  /// lens with no dispersion data, and NaN where the field runs past the
  /// image circle at either line.
  ///
  /// Traced, as `filmRadiusForFieldAngle()` is at each line, so it costs
  /// about a hundred thousand rays.
  [[nodiscard]] float lateralColorAt(float filmRadius) const noexcept;

  /// The radius of the image circle: the largest film radius anything
  /// reaches at all. A sensor larger than this is dark in the corners
  /// however long the exposure.
  ///
  /// Traced, so it costs tens of thousands of rays; nothing caches it.
  [[nodiscard]] float imageCircleRadius() const noexcept;

  /// The area on the plane of the rear vertex that a film point
  /// `filmRadius` off the axis sees the scene through. The ratio of two
  /// of these is the mechanical vignette: what the corner of the frame
  /// gets against what its middle gets. Zero is a film point nothing
  /// reaches at all.
  ///
  /// Measured over the window the light comes through rather than over
  /// the whole rear aperture, which on a lens like a phone camera's is
  /// hundreds of times the area and leaves a handful of rays to count.
  [[nodiscard]] float transmittedArea(float filmRadius) const noexcept;

  /// The front and rear vertices, which bound the glass.
  [[nodiscard]] float frontZ() const noexcept { return mElements.front().z; }
  [[nodiscard]] float rearZ() const noexcept { return mElements.back().z; }

private:
  /// The trace itself, with every medium at `indices`.
  [[nodiscard]] bool traceThrough(Ray &ray,
                                  const float *indices) const noexcept;

  std::string mName{};
  std::vector<LensElement> mElements{};
  std::vector<smdl::OpticalGlass> mMedia{};
  std::vector<float> mReferenceIndices{};
  bool mIsDispersive{};

  /// Each vertex's z in the frame the prescription was laid out in, the
  /// front vertex at zero, and the entrance pupil's z in that frame,
  /// which is how far the origin moved onto it. The paraxial solve runs
  /// in this frame, the constructor's and every later one alike, so that
  /// a solve at the d line repeats the constructor's arithmetic exactly.
  std::vector<float> mLayoutZ{};
  float mLayoutEntrancePupilZ{};

  size_t mStopIndex{};
  float mFocalLength{};
  float mBackFocalDistance{};
  float mDesignBackFocus{};
  float mFNumber{};
  float mFNumberWideOpen{};
  float mEntrancePupilRadius{};
  float mExitPupilRadius{};
  float mExitPupilZ{};
  float mRearApertureRadius{};
  float mParaxialFilmZ{};
  float mFilmZ{};

  /// The focus distance the film was placed for, kept for the log.
  float mFocusDistance{};

  /// The aperture polygon, or zero blades for a round stop. The radius
  /// is the circumradius of the polygon of the stop's own area, so
  /// blades change the shape of the bokeh and not the exposure.
  int mNumBlades{};
  float mBladeAngle{};
  float mBladeCircumRadius{};
};

/// The exit pupil as a film point sees it, tabulated by film radius.
///
/// A film point sees only part of the rear element, and through a
/// stopped-down aperture or from a corner of the frame it sees very
/// little of it, so drawing the pupil point on the whole rear aperture
/// throws most of the draws away. The lens is a surface of revolution,
/// so what a film point can see out through depends only on how far off
/// axis it is, up to a rotation: this traces that region out for a
/// ladder of film radii, keeps an ellipse bounding each, and at render
/// time draws inside the ellipse and turns it to the film point's
/// azimuth.
///
/// The estimator does not change. The pupil point is still distributed
/// over the rear aperture, the weight carries the fraction of the
/// aperture the ellipse covers, which is exactly the factor by which the
/// domain shrank, and a draw landing outside the aperture is the same
/// zero it always was.
class ExitPupil final {
public:
  /// Trace the table out. `maxFilmRadius` is how far off axis the film
  /// point can be, which is half the sensor diagonal.
  ///
  /// `wavelengthRange` is the shortest and the longest wavelength in
  /// nanometers the lens is to be traced at. When its glasses disperse,
  /// what a film point sees through moves with the wavelength, and each
  /// entry bounds it at the reference and at both ends of the range
  /// together, which holds it at every wavelength between. A lens whose
  /// glasses do not disperse sees through one region at every wavelength
  /// and is bounded at the reference alone.
  ///
  /// This is the one part of building a camera that is not instant: it
  /// is a few million rays through the prescription, run in parallel,
  /// and three times that over a range.
  ExitPupil(const Lens &lens, float maxFilmRadius,
            const std::optional<float2> &wavelengthRange = {});

  /// Draw a point on the plane of the rear vertex for the film point
  /// `film`, out of the two numbers `xi`, and report in `area` the area
  /// of the ellipse it came from, which is the domain the camera's
  /// response has to carry. Zero there is a draw that landed outside the
  /// aperture, which is blocked and need not be traced.
  [[nodiscard]] float2 sample(float2 film, float2 xi,
                              float &area) const noexcept;

  /// Does the ellipse `sample()` draws from for the film point `film`
  /// hold the point `point` of the plane of the rear vertex? It is built
  /// to hold every point a ray from `film` gets out through, at every
  /// wavelength it was bounded over, and that is what leaves the
  /// estimator alone.
  [[nodiscard]] bool contains(float2 film, float2 point) const noexcept;

  /// Log what the table bought: the share of the rear aperture the
  /// middle of the frame and the corner of it draw from.
  void logSummary() const;

  /// The share of the rear aperture the film point at `filmRadius`
  /// draws from, which is the factor by which the table cut the wasted
  /// draws at that radius. For the log; nothing samples through it.
  [[nodiscard]] float areaFraction(float filmRadius) const noexcept;

private:
  /// One entry: the ellipse bounding what a film point at this radius
  /// can see, on the plane of the rear vertex, in the frame where the
  /// film point lies on the +x axis.
  struct Bound final {
    float2 center{};
    float2 semiAxes{};
  };

  /// The bounds, indexed by film radius over `[0, maxFilmRadius]`. Each
  /// entry bounds a span of that range rather than a point of it, so the
  /// table is conservative between its entries as well as at them.
  std::vector<Bound> mBounds{};

  /// The entry covering a film point `filmRadius` off axis, which is the
  /// last one for anything past the corner the table was built for.
  [[nodiscard]] const Bound &boundAt(float filmRadius) const noexcept {
    const auto index{size_t(filmRadius * mBoundsPerRadius)};
    return mBounds[index < mBounds.size() ? index : mBounds.size() - 1];
  }

  /// The aperture the drawn points lie within, copied out of the lens so
  /// that sampling touches nothing else.
  float mRearRadius{};

  /// Entries per unit of film radius, and one over the area of the rear
  /// aperture, which are the two constants `sample()` runs on.
  float mBoundsPerRadius{};
  float mInvApertureArea{};

  /// The largest film radius the table covers, and the range of
  /// wavelengths it was bounded over, or none for the reference alone,
  /// kept for the log.
  float mMaxFilmRadius{};
  std::optional<float2> mWavelengthRange{};
};
