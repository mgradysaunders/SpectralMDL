/// \file
/// Everything the command line asks for, lowered into plain values.
///
/// The `cl::opt` objects themselves live in `Options.cc` and go no
/// further: every other translation unit takes an `Options` and never
/// knows a command line was involved. That is what lets the render
/// stages be read, and called, without one.
#pragma once

#include <optional>
#include <string>
#include <vector>

#include "Color.h"
#include "Progress.h"
#include "Render/PathTracing.h"
#include "Tonemap.h"

/// A command line value the scene file also has a say in.
///
/// A flag left at its default must not override a `.layout` that spoke,
/// so `given` is the whole point: the value alone cannot distinguish
/// "the user asked for 50" from "50 is what it defaults to".
template <typename T> struct Flag final {
  /// The value, which is the flag's own default when `given` is false.
  T value{};

  /// Did the command line actually give it?
  bool given{};
};

/// Resolve one of the merged settings: the command line if it spoke,
/// else the scene file if it did, else the flag's own default.
template <typename T, typename U>
[[nodiscard]] T pick(const Flag<T> &cli, const std::optional<U> &file) {
  return !cli.given && file ? T(*file) : cli.value;
}

/// A uniform wavelength grid, as `-wavelength-range` spells one.
struct WavelengthRange final {
  /// The endpoints in nanometers, inclusive.
  float2 range{};

  /// The number of bands spanning them.
  unsigned bandCount{};
};

/// The utility flags. Each one does its whole job and bows out before a
/// scene is loaded, which is why they are grouped apart from the rest.
struct UtilityOptions final {
  std::string dumpPlaces{};

  std::string dumpCurves{};

  std::string packPlaces{};

  std::string outputPlaces{};

  bool listMaterials{};

  bool listObjects{};

  bool json{};
};

/// What to build the scene out of.
struct SceneOptions final {
  std::string inputSceneFile{};

  std::vector<std::string> inputMDLFiles{};

  std::vector<std::string> inputMeshFiles{};

  std::vector<std::string> assetDirs{};

  bool allMaterials{};

  bool ground{};

  /// The ground plane's height. Giving it implies `ground`.
  Flag<float> groundZ{};

  std::string groundMaterial{};

  std::string fallbackMaterial{};
};

/// The shutter, which the layout's `time` directive also sets.
struct ShutterFlags final {
  Flag<float> time{};

  Flag<float> speed{};
};

/// The sample budget and how it is spent.
struct SamplingOptions final {
  unsigned spp{};

  unsigned sampleOffset{};

  unsigned threads{};

  bool noLOD{};
};

/// SD-tree path guiding.
struct GuideOptions final {
  bool enabled{};

  bool adrrs{};

  /// The fraction of samples drawn from the BSDF rather than the guide.
  Flag<float> bsdfFraction{};

  float split{};
};

/// The camera, every setting of which the layout's `camera` directive
/// may also give.
struct CameraFlags final {
  Flag<int2> resolution{};

  Flag<int4> cropWindow{};

  Flag<float3> lookFrom{};

  Flag<float3> lookTo{};

  Flag<float3> lookUp{};

  Flag<float> fovYDeg{};

  Flag<float> fStop{};

  Flag<float> aperture{};

  Flag<float> focus{};

  Flag<int> blades{};

  Flag<float> bladeAngleDeg{};

  Flag<float> distortionK1{};

  Flag<float> distortionK2{};

  Flag<bool> distortionFit{};

  Flag<float> vignetting{};

  Flag<float> catEye{};

  Flag<float> catEyeRadius{};
};

/// Framing the camera from the scene bounds instead of stating it.
struct AutolookFlags final {
  bool enabled{};

  /// The azimuth, whose default an asset manifest's front azimuth gets
  /// to supply when the flag is silent.
  Flag<float> azimuthDeg{};

  float zenithDeg{};

  float margin{};

  bool ignoreBackfaces{};
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
  bool given{};

  /// Draw each sample's own grid from within the bands.
  bool jitter{};
};

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

  bool allLights{};

  bool noLightTree{};
};

/// The exterior haze, which the layout's `haze` directive also sets.
struct HazeFlags final {
  bool on{};

  /// No haze, whichever source asked for it.
  Flag<bool> none{};

  Flag<float> visibility{};

  Flag<float> scaleHeight{};
};

/// Where the render goes.
struct OutputOptions final {
  std::string rgb{};

  std::string rgbFloat{};

  /// The spectral output. Empty means none; `-resume` implies it back
  /// to the file being resumed.
  std::string spectrum{};

  bool spectrumGiven{};

  std::string resume{};

  double previewEvery{};

  /// The time-trace file, and whether `-profile` was given at all,
  /// since it takes an optional value.
  std::string profile{};

  bool profiling{};
};

/// Everything the command line asked for.
///
/// The structs that only the command line fills are held resolved; the
/// ones a `.layout` also has a say in are held as `Flag`s and merged
/// once the layout has been read.
struct Options final {
  UtilityOptions utility{};

  SceneOptions scene{};

  ShutterFlags shutter{};

  SamplingOptions sampling{};

  GuideOptions guide{};

  CameraFlags camera{};

  AutolookFlags autolook{};

  GridOptions grid{};

  SkyFlags sky{};

  HazeFlags haze{};

  /// The manifold estimator, filled with everything the command line
  /// decides; the caster set and the sun cone need the scene and are
  /// filled by the staging.
  MNEEOptions mnee{};

  bool mneeEnabled{};

  bool mneeReport{};

  bool mneeSunOnly{};

  bool mneeTestNormalHook{};

  PathOptions path{};

  TonemapOptions tonemap{};

  RGBPolicy rgbPolicy{};

  /// The progress bar, filled with everything the command line decides;
  /// the totals and the summary need the resolved window and budget.
  ProgressOptions progress{};

  OutputOptions output{};

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
