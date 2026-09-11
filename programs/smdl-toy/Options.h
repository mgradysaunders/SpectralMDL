/// \file
/// Everything the command line asks for, lowered into plain values.
///
/// The `cl::opt` objects themselves live in `Options.cc` and go no
/// further: every other translation unit takes an `Options` and never
/// knows a command line was involved. That is what lets the render
/// stages be read, and called, without one.
///
/// The groups below are the `cl::OptionCategory` groups the help text
/// prints, one struct apiece, so that where a setting lives here and
/// where a user finds it are the same question. Anything finer is a
/// struct nested inside its category.
#pragma once

#include <optional>
#include <string>
#include <vector>

#include "smdl/Compiler.h"
#include "smdl/Support/Logger.h"

#include "../CommandLine.h"
#include "Color.h"
#include "Layout/CameraFile.h"
#include "MedianFilter.h"
#include "Progress.h"
#include "Render/PathTracing.h"
#include "Sensor/Detector.h"
#include "Sensor/Develop.h"
#include "Tonemap.h"

//--{ Compile Options
/// What the MDL compiler bakes into the material code. No scene file has
/// a say in any of it.
struct CompileOptions final {
  smdl::OptLevel optLevel{smdl::OPT_LEVEL_O2};

  bool isDebugEnabled{};
};
//--}

//--{ Camera Options
/// Framing the camera from the scene bounds instead of stating it.
struct AutolookFlags final {
  bool isEnabled{};

  /// The azimuth, whose default an asset manifest's front azimuth gets
  /// to supply when the flag is silent.
  Flag<float> azimuthDeg{};

  float zenithDeg{};

  float margin{};

  bool ignoreBackfaces{};
};

/// The camera: which file describes it, and the few things the command
/// line may say about it.
///
/// The instrument and the shot are the files' alone: the lens, the body,
/// the stop, the focus, and the shutter are keys of the `.camera` file
/// and of the `.sensor` and `.lens` files it names. What stays here
/// frames the picture, previews the camera, or acts after the last
/// sample. The picture's size is not here either: that is a fact about
/// this render rather than about the camera, and no camera file carries
/// it.
struct CameraFlags final {
  /// The '.camera' file, or empty to take the one beside the layout.
  std::string file{};

  /// The framing, over the camera file's.
  ///
  /// \{
  Flag<float3> lookFrom{};
  Flag<float3> lookTo{};
  Flag<float3> lookUp{};
  /// \}

  /// `-ideal`: preview the camera. The model resolves from the files as
  /// it would without it, and is then projected: a traced lens becomes
  /// the thin lens fitted to it, a body becomes the observer on the
  /// body's frame and pixels, and the picture is exposed as the body
  /// would expose it. See `CameraModel::isPreview`.
  bool isIdeal{};

  /// `-iso N|auto`: the ISO a physical sensor is read out at, over the
  /// camera file's, and the meter flagged apart, since it is a
  /// measurement of the film rather than a value. One flag, so at most
  /// one of the two is set. It acts after the last sample, so a readout
  /// of a saved film turns it without editing the file. See
  /// `CameraSettings::iso`.
  ///
  /// \{
  Flag<float> iso{};
  bool shouldMeterISO{};
  /// \}

  /// `-white-balance`: the white a physical sensor's develop balances
  /// to, over the camera file's; like the ISO, it acts after the last
  /// sample. See `CameraSettings::whiteBalance`.
  Flag<WhiteBalance> whiteBalance{};

  AutolookFlags autolook{};
};
//--}

//--{ Image Options
/// The picture: how big it is, how it is tone mapped, and where it goes.
/// No scene file has a say in any of it.
struct ImageOptions final {
  /// The image dimensions in pixels. Whether it was given matters: a
  /// physical sensor renders exactly its own pixels, and a `-resolution`
  /// that disagrees is refused rather than stretched.
  Flag<int2> resolution{};

  /// The fraction of the frame's pixels to render: a smaller picture of
  /// the same frame, the body's or the observer's. A body renders exactly
  /// its own pixels, so it is refused in realistic mode.
  Flag<float> resolutionScale{};

  /// The sub-rectangle to render, `x0,y0,x1,y1`. The default is not a
  /// window at all, so `given` is what says whether to narrow the frame.
  Flag<int4> cropWindow{};

  /// How a spectrum becomes RGB.
  RGBPolicy rgbPolicy{};

  /// The tone map applied to the 8-bit output.
  TonemapOptions tonemap{};

  /// The firefly filter applied to both RGB outputs, and to neither the
  /// spectral output nor anything a later session resumes from.
  MedianFilterOptions medianFilter{};

  std::string outputRGB{};

  std::string outputRGBFloat{};

  /// The spectral output. Empty means none; `-resume` implies it back
  /// to the file being resumed.
  std::string outputSpectrum{};

  bool wasOutputSpectrumGiven{};

  /// Write the spectral output, and the band film beside it, as 64-bit
  /// floats rather than 32-bit: for a byte-for-byte comparison that must
  /// see every bit the accumulation holds.
  bool shouldWriteDouble{};

  std::string resume{};

  /// The detector readout, empty for none: a 16-bit ENVI pair of digital
  /// numbers through the camera's body.
  std::string outputDN{};

  /// The readout's realization and which noise it draws.
  DetectorReadoutOptions readout{};
};
//--}

//--{ Light Options
/// The environment, which the layout's `sky` directive also sets.
struct SkyFlags final {
  /// No environment at all.
  Flag<bool> none{};

  Flag<float> sunZenithDeg{};

  /// The sun azimuth, which follows the autolook solve when neither
  /// source states it, so a batch of thumbnails is consistently lit.
  Flag<float> sunAzimuthDeg{};

  Flag<float> visibility{};

  Flag<float> waterVapor{};

  Flag<float> scale{};

  /// The moon phase. Either source giving it, or the distance, asks for
  /// moonlight.
  Flag<float> moonPhase{};

  Flag<float> moonDistance{};

  /// An image-based environment, which displaces the sun and sky.
  Flag<std::string> iblFileName{};

  Flag<float> iblScale{};
};

/// The exterior haze, which the layout's `haze` directive also sets.
struct HazeFlags final {
  bool isOn{};

  /// No haze, whichever source asked for it.
  Flag<bool> none{};

  Flag<float> visibility{};

  Flag<float> scaleHeight{};
};

/// What lights the scene.
struct LightFlags final {
  SkyFlags sky{};

  HazeFlags haze{};
};
//--}

//--{ Rendering Options
/// The sample budget and how it is spent.
struct SamplingOptions final {
  unsigned spp{};

  unsigned sampleOffset{};

  bool noLOD{};
};

/// SD-tree path guiding.
struct GuideOptions final {
  bool isEnabled{};

  bool useADRRS{};

  /// The fraction of samples drawn from the BSDF rather than the guide.
  Flag<float> bsdfFraction{};

  float split{};
};

/// The wavelength grid the command line asks for, already parsed.
struct GridOptions final {
  /// The uniform grid `-wavelength-range` spells, or the default range
  /// when it was not given.
  WavelengthRange range{};

  /// The explicit grid `-wavelengths` spells, empty when it was not
  /// given.
  std::vector<float> explicitWavelengths{};

  /// Did either grid flag speak? A resumed render with neither adopts
  /// the grid recorded in the file it resumes from.
  bool wasGiven{};

  /// Draw each sample's own grid from within the bands. Whether it was
  /// given matters: a physical sensor turns it on unless told not to.
  Flag<bool> shouldJitter{};
};

/// How the picture is computed: the budget, the estimators, and the
/// spectral grid they work on.
struct RenderFlags final {
  SamplingOptions sampling{};

  PathOptions path{};

  GuideOptions guide{};

  GridOptions grid{};

  /// The manifold estimator, filled with everything the command line
  /// decides; the caster set and the sun cone need the scene and are
  /// filled by the staging.
  MNEEOptions mnee{};

  bool useMNEE{};

  /// Print the path and contribution statistics after the render; see
  /// `PathStats`.
  bool shouldReportStats{};

  bool useMNEESunOnly{};

  bool shouldTestMNEENormalHook{};

  /// Aim light selection at every emitter, whatever the layout marks.
  bool allLights{};

  /// Select lights from a flat power-weighted distribution rather than
  /// from the spatial tree.
  bool noLightTree{};

  /// Build the acceleration structures without Embree's watertight ray
  /// intersection. See the flag's description.
  bool noRobustIntersection{};
};
//--}

//--{ Scene Options
/// What to build the scene out of, and which instant of it to render.
struct SceneOptions final {
  std::string inputSceneFile{};

  std::vector<std::string> inputMDLFiles{};

  std::vector<std::string> inputMeshFiles{};

  std::vector<std::string> assetDirs{};

  /// `State::animationTime` at shutter open, in seconds: which instant
  /// of the scene's own timeline this render photographs. The command
  /// line is its only source, so one `.camera` file renders every frame
  /// of a shot and a layout that spans seconds is a sequence.
  float time{};

  bool hasGround{};

  /// The ground plane's height. Giving it implies `ground`.
  Flag<float> groundZ{};

  std::string groundMaterial{};

  std::string fallbackMaterial{};
};
//--}

//--{ Utility Options
/// The tools and the machinery around a render: the flags that do their
/// whole job and bow out before a scene is loaded, and the ones that say
/// how the work is scheduled and reported.
struct UtilityOptions final {
  std::string dumpPlaces{};

  std::string dumpCurves{};

  std::string packPlaces{};

  std::string outputPlaces{};

  bool shouldListMaterials{};

  bool shouldListObjects{};

  /// Print what the camera resolves to and exit, before any layout is
  /// read.
  bool shouldDescribeCamera{};

  bool useJSON{};

  /// Compile every material the MDL files declare, not only the ones
  /// the scene asks for.
  bool allMaterials{};

  unsigned threads{};

  /// The lowest level of log message to print, which `main()` hands the
  /// logger before anything else can say anything.
  smdl::LogLevel logLevel{smdl::LOG_LEVEL_INFO};

  /// Whether log messages are labeled, and the progress bar drawn, with
  /// Unicode symbols, which the log sink and the bar decide for
  /// themselves when this is `UNICODE_MODE_AUTO`.
  smdl::UnicodeMode unicodeMode{smdl::UNICODE_MODE_AUTO};

  /// The progress bar, filled with everything the command line decides;
  /// the totals and the summary need the resolved window and budget.
  ProgressOptions progress{};

  double previewEvery{};

  /// The time-trace file, and whether `-profile` was given at all,
  /// since it takes an optional value.
  std::string profile{};

  bool isProfiling{};
};
//--}

/// Everything the command line asked for, grouped as the help text
/// groups it.
struct Options final {
  CompileOptions compile{};

  CameraFlags camera{};

  ImageOptions image{};

  LightFlags light{};

  RenderFlags render{};

  SceneOptions scene{};

  UtilityOptions utility{};

  /// The command line as it was given, joined, for the spectral
  /// output's `render args` field.
  std::string argsEcho{};
};

/// Parse and validate the command line.
///
/// Everything knowable without a scene is checked here, so that a typo
/// or an out-of-range value fails before anything loads or compiles
/// rather than at the end of an hour-long render.
///
/// \throws smdl::Error  If any flag is malformed or out of range.
///
[[nodiscard]] Options parseCommandLine(int argc, char **argv);
