#pragma once

#include <optional>

#include "Scene/Scene.h"

/// How `-autolook` wants the camera solved. See `solveAutolook()`.
struct AutolookOptions final {
  /// The vertical field of view in degrees, as merged from every source.
  float fovYDeg{37.8f};

  /// The aspect ratio, X over Y, from the merged image dimensions.
  float aspectRatio{16.0f / 9.0f};

  /// The zenith angle of the scene-to-camera direction in degrees, in the
  /// same convention as `-sun-zenith`: 0 is straight overhead. Fixed on
  /// purpose: solving elevation degenerates to a straight-down view for
  /// flat assets.
  float zenithDeg{65.0f};

  /// The azimuth of the scene-to-camera direction in degrees CCW from +X,
  /// like `-sun-azimuth`. Unset means solve it (see `solveAutolook()`).
  std::optional<float> azimuthDeg{};

  /// The padding between the scene and the frame edge, as a fraction of
  /// the frame.
  float margin{0.05f};

  /// Neither avoid nor warn about views of backfacing geometry. Even
  /// when false, the veto stands down on its own when every candidate
  /// shows backfaces, which is what unshaded two-sided geometry looks
  /// like.
  bool ignoreBackfaces{};

  /// A mesh instance to leave out of the fit and the probe, or
  /// `INVALID_INDEX`; used for the ground plane, which is scenery rather
  /// than geometry to be framed.
  uint32_t skipInstance{INVALID_INDEX};
};

/// What `solveAutolook()` decided, plus the diagnostics that make a batch
/// run's choices auditable.
struct AutolookResult final {
  /// The solved camera position.
  float3 lookFrom{};

  /// The point on the view axis nearest the scene's bound center, so the
  /// default depth-of-field focus lands somewhere sensible.
  float3 lookTo{};

  /// The chosen (or locked) azimuth in degrees, which is what the sun
  /// default follows under `-autolook`.
  float azimuthDeg{};

  /// The fraction of the frame the framed geometry covers, from the
  /// probe: true projected area, occlusion and perspective included.
  float fill{};

  /// The visible surface area of the chosen view in scene units squared,
  /// which is what the azimuth sweep maximizes; see `solveAutolook()`.
  float visibleArea{};

  /// The fraction of visible non-exempt geometry that is backfacing, from
  /// the probe. 0 when nothing objectionable is in view.
  float backfaceFraction{};
};

/// Solve the camera position that frames the committed scene.
///
/// For a **fixed** view direction the tightest containing camera is
/// closed form: each frustum plane is a linear constraint on the
/// position, so four min-reductions over every world-space vertex decide
/// it, and nothing ever clips.
///
/// With `azimuthDeg` unset, candidate azimuths are swept and scored
/// by one low-resolution ray probe each: views dominated by backfaces are
/// rejected (statically thin-walled materials and declared backface
/// surfaces exempt), and the survivors are ranked by visible surface
/// area; see `ProbeResult` in `Autolook.cc`. With it set, the direction is
/// locked and the one probe only warns.
///
/// Runs after `Scene::commit()` on final vertices, displacement included.
///
/// \throws smdl::Error  If the scene has no geometry to frame.
///
[[nodiscard]] AutolookResult solveAutolook(const Scene &scene,
                                           const AutolookOptions &options);

/// How `focus auto` wants the focus measured. See `solveAutofocus()`.
struct AutofocusOptions final {
  /// The framing at shutter open, as the camera is about to be built
  /// with it: after `-autolook` has had its say, when both are asked
  /// for.
  ///
  /// \{
  float3 lookFrom{};
  float3 lookTo{};
  float3 lookUp{};
  /// \}

  /// The focal length in units of the frame height, `0.5 / tan(fovy /
  /// 2)` for the thin lens and `f / H` for a lens, which turns a frame
  /// offset into a direction near the axis. Both optics image the
  /// center of the frame through a pinhole to first order, which is as
  /// far as the probe goes.
  float focalLengthOverHeight{};
};

/// What `solveAutofocus()` measured.
struct AutofocusResult final {
  /// The focus distance along the view axis in scene units: the median
  /// hit of the probe, or `INF` when every ray missed.
  float distance{INF};

  /// How many rays hit anything, out of how many were cast.
  ///
  /// \{
  size_t hitCount{};
  size_t rayCount{};
  /// \}

  /// The instance the median ray hit, or `INVALID_INDEX` for none, and
  /// the material it hit it on, which is how the log names what the
  /// center of the frame sees.
  ///
  /// \{
  uint32_t instIndex{INVALID_INDEX};
  uint32_t matIndex{INVALID_INDEX};
  /// \}
};

/// Measure the focus distance for `focus auto`: the median distance,
/// projected on the view axis, of the hits of a 5 by 5 grid of pinhole
/// rays over the central 2 percent of the frame height, from the
/// framing at shutter open. Infinity when every ray misses. The median
/// rather than the mean, so that a thin foreground post or a gap onto
/// the sky does not pull the focus off what fills the center.
///
/// Runs after `Scene::commit()`, and after `solveAutolook()` when both
/// are asked for, since the framing is its input.
[[nodiscard]] AutofocusResult solveAutofocus(const Scene &scene,
                                             const AutofocusOptions &options);
