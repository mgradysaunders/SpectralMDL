/// \file
/// The lens format: the surfaces light passes through between the scene
/// and the film, as a published prescription states them.
///
/// A `.lens` file is the third format of the layout family and is parsed
/// by the same syntax core (`TextParser.h`). The split from `.camera` is
/// the separation the family is built on: a `.camera` says where the
/// picture is taken from and on what sensor, and a `.lens` says what the
/// light came through to get there. One lens then serves as many cameras
/// as name it, and neither file restates the other.
///
/// Everything here is in millimeters, which is what every published
/// prescription is written in, so a patent table transcribes without
/// arithmetic. The conversion to scene units happens once, downstream,
/// where the traced system is built.
#pragma once

#include <cstddef>
#include <string>
#include <string_view>
#include <vector>

#include "Layout/LayoutDiagnostics.h"

/// The extension that marks a lens file.
constexpr std::string_view LENS_EXTENSION = ".lens";

/// The most even aspheric coefficients one surface may carry. Published
/// designs rarely go past the `r^16` term; the cap is here so that a
/// runaway number list is a diagnostic rather than a resize.
constexpr size_t LENS_MAX_ASPHERIC_TERMS = 8;

/// One surface of a prescription, in millimeters.
///
/// Surfaces are written front (scene side) first and film last, which is
/// the order light travels and the order every prescription prints in.
///
class LensSurface final {
public:
  /// Is this the aperture stop? The stop is flat, states no index of its
  /// own, and there is exactly one of them in a parsed prescription.
  bool isStop{};

  /// The signed curvature radius, positive when the center of curvature
  /// lies on the film side of the vertex. That is the sign the Zemax and
  /// pbrt conventions use, so a published row copies across unchanged.
  /// Zero is flat.
  float radius{};

  /// The axial distance from this vertex to the next. On the last
  /// surface it is the design's back focus, which is a check on the
  /// focus solve rather than an input to it, and may be omitted.
  float thickness{};

  /// The refractive index of the space following this surface in file
  /// order. One is air, which is why it is the default: an air gap is
  /// one key shorter to write than a glass.
  float ior{1};

  /// The clear aperture diameter. Prescriptions state a diameter and a
  /// trace wants a radius, so the halving happens once, downstream.
  float diameter{};

  /// The conic constant `k` of the surface of revolution
  /// `z = c r^2 / (1 + sqrt(1 - (1 + k) c^2 r^2))`, with `c = 1/radius`.
  /// Zero is a sphere, -1 a paraboloid, less than -1 a hyperboloid.
  float conic{};

  /// The even aspheric coefficients added to the sag, the `r^4` term
  /// first: `sum a[i] * r^(2 i + 4)`. Empty for most designs, and for
  /// every design before roughly 1990.
  ///
  /// A table that prints its zero terms is common, so all-zero
  /// coefficients are accepted; a nonzero one is refused at parse time
  /// while the trace has no iterative intersection to solve it with.
  std::vector<float> aspheric{};
};

/// The lens a file's `lens` directive describes.
class LensPrescription final {
public:
  /// The name the file gave it, or empty. Free text, since it names a
  /// real lens rather than anything the language looks up.
  std::string name{};

  /// The surfaces, front first.
  std::vector<LensSurface> surfaces{};

  /// The index of the aperture stop, or `surfaces.size()` if there is
  /// none, which a prescription that parsed never has.
  [[nodiscard]] size_t stopIndex() const noexcept;
};

/// A parsed lens file, as written.
class LensDocument final {
public:
  /// The source the document was parsed from, owned by the
  /// `LayoutDiagnostics` that loaded it.
  const LayoutSource *source{};

  /// The prescription the file's `lens` directive described.
  LensPrescription lens{};
  LayoutLocation lensLoc{};
};

/// Parse one lens source into a document.
///
/// Pure: no filesystem access. Errors and warnings accumulate in
/// `diags`, and the returned document is the best effort regardless.
[[nodiscard]] LensDocument parseLens(LayoutDiagnostics &diags,
                                     const LayoutSource &source);

/// Read a lens file: parse, print every diagnostic to standard error
/// (colored when stderr is a terminal), and throw if any were errors.
///
/// \throws smdl::Error  If the file cannot be read, or on any parse
///                      error after printing the diagnostics.
///
[[nodiscard]] LensDocument readLens(const std::string &fileName);

/// The lens file a camera should look through, or empty for none:
/// `given` if the command line named one, else `stated` if the camera
/// file did, resolved relative to `cameraFileName` so that a scene
/// directory stays self-contained.
///
/// \throws smdl::Error  If either names a file that does not exist,
///                      since neither may be quietly ignored.
///
[[nodiscard]] std::string resolveLensFileName(const std::string &given,
                                              const std::string &cameraFileName,
                                              const std::string &stated);
