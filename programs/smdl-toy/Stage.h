/// \file
/// Getting from a command line to something that can be rendered.
///
/// The two stages here are what turns flags into a scene. `Frame`
/// resolves what is being looked at and how many pixels and samples make
/// up the picture, from the layout and the command line alone, before
/// anything slow loads. `StagedScene` then builds the thing itself: the
/// wavelength grid, the compiled materials, the acceleration structures,
/// the environment, the media, and the light sampler.
#pragma once

#include <cstddef>
#include <memory>
#include <optional>

#include "smdl/Compiler.h"

#include "smdl/RenderUtil/Haze.h"

#include "Color.h"
#include "Common.h"
#include "Layout/Layout.h"
#include "Render/Camera.h"
#include "Render/Light.h"
#include "Render/Medium.h"
#include "Scene/Scene.h"

struct Options;
struct ResumedSequence;

/// The material a scene falls back to when it has none of its own: a
/// plain 20 percent Lambertian, so a layout can be looked at before any
/// material has been written for it. `-fallback-material` names it to
/// get it, and the darker `_GROUND_` twin is what `-ground` defaults to,
/// for contrast against it.
///
/// \{
constexpr const char *DEFAULT_OBJECT_MATERIAL_NAME{"default_object"};
constexpr const char *DEFAULT_GROUND_MATERIAL_NAME{"default_ground"};
/// \}

/// What is being framed, before any of it is loaded.
struct Frame final {
  /// The layout, with every `-mesh` argument merged into the positional
  /// one. Either may name a mesh file or a `.layout`.
  Layout layout{};

  /// The camera settings, merged from three sources in increasing order
  /// of priority: the defaults, the layout's `camera` directive, and
  /// whatever the command line explicitly gave.
  CameraOptions cameraOptions{};

  /// The camera itself.
  ///
  /// Empty under `-autolook`, whose position comes from measuring the
  /// committed scene: `StagedScene` fills it in after the solve. Every
  /// other path constructs it here, before anything slow loads, so that
  /// the lens validation in the constructor still fails fast.
  std::optional<Camera> camera{};

  int2 resolution{};

  size_t numPixelsX{};

  size_t numPixelsY{};

  /// The samples per pixel this session draws.
  size_t spp{};

  /// The pixel window to render, the whole frame unless `-crop-window`
  /// narrows it.
  int4 window{};

  size_t numWindowPixels{};
};

/// Resolve the frame from the command line and the layout it names.
///
/// \throws smdl::Error  If the layout cannot be read, or the merged
///                      camera or window is malformed.
///
[[nodiscard]] Frame resolveFrame(const Options &opts);

/// The wavelength grid the render runs on.
struct ResolvedGrid final {
  /// The grid in nanometers, which sizes every `Color`.
  Color wavelengths{};

  /// Does the grid reach outside the visible? Everything RGB-sourced
  /// degrades there, so several later stages say so once rather than
  /// rendering a mysteriously dark image.
  bool beyondVisible{};
};

/// Resolve the grid, in priority order: explicit `-wavelengths`,
/// `-wavelength-range` uniform bands, or, when resuming with no grid
/// flags at all, the grid recorded in the file being resumed, so that a
/// resumed render needs no grid retyping.
///
/// This also installs the render-wide `renderGrid()`, whose band count
/// sizes every `Color` built from here on, so nothing may construct one
/// before this runs.
///
/// \throws smdl::Error  If the grid is malformed, or does not match the
///                      one the resumed file was rendered on.
///
[[nodiscard]] ResolvedGrid
resolveWavelengthGrid(const Options &opts, const Frame &frame,
                      const ResumedSequence &resumed);

/// Configure `compiler` and load every MDL module the command line
/// names, plus the built-in stand-in.
///
/// Split from the staging below because `-list-materials` reports how
/// names resolve and so needs exactly this much and no more.
void setUpCompiler(const Options &opts, const Frame &frame,
                   const ResolvedGrid &grid, smdl::Compiler &compiler);

/// The scene, built and ready to render.
///
/// Constructed in place and never moved. The JIT'd material code embeds
/// absolute pointers into the compiler's data and into this object's own
/// allocator, so both have to stay where they were built for as long as
/// anything renders.
class StagedScene final {
public:
  /// Build the scene the compiled materials shade: import the layout,
  /// add the ground plane, commit the acceleration structures, solve the
  /// autolook, and construct the environment, the media and the light
  /// sampler.
  ///
  /// `compiler` is compiled and JIT-compiled here, and is borrowed for
  /// the object's whole lifetime. `frame.camera` is filled in under
  /// `-autolook`, whose position is a measurement of the committed
  /// scene.
  ///
  /// \throws smdl::Error  If a name the layout uses does not resolve, or
  ///                      the scene asks for two mutually exclusive
  ///                      exterior media.
  ///
  StagedScene(const Options &opts, Frame &frame, const ResolvedGrid &grid,
              smdl::Compiler &compiler);

  StagedScene(const StagedScene &) = delete;

  StagedScene &operator=(const StagedScene &) = delete;

public:
  /// The geometry, committed and ready to trace.
  std::optional<Scene> scene{};

  /// Every light in one selection path: each emissive mesh instance plus
  /// the environment, weighted by power.
  std::optional<LightSampler> lights{};

  /// The environment, null when the scene has none.
  std::unique_ptr<EnvLight> envLight{};

  /// The exterior haze, null when the scene has none. It is the medium
  /// of everything outside all geometry, which is where the `medium`
  /// directive puts its material too, so the two cannot both be asked
  /// for.
  std::unique_ptr<smdl::Haze> haze{};

  /// The material the layout's `medium` directive names, evaluated once
  /// and seeding every camera path's medium stack. Null when the layout
  /// names none.
  const MediumStack *exteriorMedium{};

  /// The geometry bounds as they were before the ground plane went in,
  /// which is what the SD-tree is built over: an unbounded plane would
  /// otherwise swamp the tree with empty space.
  ///
  /// \{
  BoundBox3 guideBound{};
  bool guideBoundsValid{};
  /// \}

private:
  /// Where `exteriorMedium` lives. Declared last so that it outlives
  /// nothing that points into it.
  smdl::BumpPtrAllocator mMediumAllocator{};
};
