#pragma once

#include <optional>

#include "Scene.h"

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
