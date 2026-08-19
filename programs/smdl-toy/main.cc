// vim:foldmethod=marker:foldlevel=0:fmr=--{,--}
#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <iterator>
#include <optional>

#include "assimp/version.h"
#include "embree4/rtcore_config.h"
#include "opensubdiv/version.h"

#include "cl.h"
#include "llvm/Support/Parallel.h"
#include "llvm/Support/WithColor.h"

#include "Camera.h"
#include "Framing.h"
#include "Guiding.h"
#include "Layout.h"
#include "Light.h"
#include "PathTracing.h"
#include "Progress.h"
#include "Scene.h"
#include "Tonemap.h"

#include "smdl/Common.h"
#include "smdl/Support/Filesystem.h"
#include "smdl/Support/Logger.h"
#include "smdl/Support/Profiler.h"
#include "smdl/Support/SpectralRenderImage.h"

// Optional rather than required only because the '.places' utility
// flags below run without a scene; everything else checks it by hand
// right after they bow out.
static cl::opt<std::string> optInputSceneFile{cl::Positional,
                                              cl::desc("<input scene>")};
static cl::list<std::string> optInputMDLFiles{
    cl::Positional, cl::desc("<input mdl>"), cl::ZeroOrMore};
//--{ CLI: Scene Options
static cl::OptionCategory catScene{"Scene Options"};
static cl::list<std::string> optInputMeshFiles{
    "mesh",
    cl::desc("Add another mesh, repeatable (use a '.layout' file if you need "
             "transforms)"),
    cl::cat(catScene)};
static cl::opt<std::string> optDumpPlaces{
    "dump-places",
    cl::desc("Print a '.places' buffer as one-line place text, then exit"),
    cl::cat(catScene)};
static cl::opt<std::string> optDumpCurves{
    "dump-curves", cl::desc("Print a '.curves' file summary, then exit"),
    cl::cat(catScene)};
static cl::opt<std::string> optPackPlaces{
    "pack-places",
    cl::desc("Pack a layout's 'place' statements into a '.places' buffer, "
             "then exit"),
    cl::cat(catScene)};
static cl::opt<std::string> optOutputPlaces{
    "output-places",
    cl::desc("The output of -pack-places (default: the layout's name with "
             "'.places')"),
    cl::cat(catScene)};
static cl::list<std::string> optAssetDirs{
    "asset-dir",
    cl::desc("Add a directory to search for assets and meshes, repeatable\n"
             "* a relative path in a scene file is looked for beside that "
             "file first, then in each of these"),
    cl::cat(catScene)};
static cl::opt<bool> optListMaterials{
    "list-materials",
    cl::desc("List the material names the scene needs and exit"),
    cl::init(false), cl::cat(catScene)};
static cl::opt<bool> optListObjects{
    "list-objects",
    cl::desc("List the objects present in each scene file and exit"),
    cl::init(false), cl::cat(catScene)};
static cl::opt<bool> optJSON{
    "json",
    cl::desc("With -list-objects or -list-materials, print JSON for tooling "
             "instead of a table for a person"),
    cl::init(false), cl::cat(catScene)};
static cl::opt<std::string> optMaterialFallback{
    "material-fallback",
    cl::desc("The MDL material to use by default, without this an unresolved "
             "name is an error\n"
             "* 'default_material' is built in: a plain 20 percent "
             "Lambertian, and what a\n  scene given no <input mdl> at all "
             "falls back to"),
    cl::cat(catScene)};
static cl::opt<bool> optAllMaterials{
    "all-materials",
    cl::desc("Compile every material in the given MDL modules, not just the "
             "ones the scene\nbinds\n"
             "* a skipped material's body is never emitted, so this is how "
             "errors inside an\n  unused material get diagnosed"),
    cl::init(false), cl::cat(catScene)};
//--}
//--{ CLI: Sampling Options
static cl::OptionCategory catSampling{"Sampling Options"};
static cl::opt<bool> optGuide{"guide", cl::desc("Enable SD-tree path guiding"),
                              cl::init(false), cl::cat(catSampling)};
static cl::opt<bool> optGuideADRRS{
    "guide-adrrs",
    cl::desc("With -guide, drive Russian roulette by expected pixel "
             "contribution instead of throughput (default: true)"),
    cl::init(true), cl::cat(catSampling)};
static cl::opt<float> optGuideBSDFFraction{
    "guide-bsdf-fraction",
    cl::desc("With -guide, the probability of sampling the BSDF instead of "
             "the SD-tree at guided vertices (default: 0.5)"),
    cl::init(0.5f), cl::cat(catSampling)};
static cl::opt<bool> optNoLOD{
    "no-lod", cl::desc("Disable LOD by zeroing the camera ray cone spread"),
    cl::init(false), cl::cat(catSampling)};
static cl::opt<bool> optNoMipMaps{
    "no-mipmaps",
    cl::desc("Disable mip maps, so that 'use_mipmap: true' is ignored and no "
             "mip chains are allocated or generated\n"
             "* the natural partner of -no-lod, which leaves the chains "
             "allocated but never\n  selects one"),
    cl::init(false), cl::cat(catSampling)};
static cl::opt<unsigned> optSPP{
    "spp", cl::desc("The number of samples per pixel (default: 8)"),
    cl::init(8U), cl::cat(catSampling)};
//--}
//--{ CLI: Wavelength Options
static cl::OptionCategory catWavelength{"Wavelength Options"};
static cl::opt<unsigned> optBands{
    "bands",
    cl::desc("The number of wavelength bands (default: 16)\n"
             "* the JIT compiles materials for exactly this many bands; "
             "compile time and\n  per-sample cost both grow with it"),
    cl::init(16U), cl::cat(catWavelength)};
static cl::opt<float2> optWaveRange{
    "wave-range",
    cl::desc("The wavelength range in nm spanned by -bands uniform bands, "
             "endpoints\ninclusive (default: 380,720)\n"
             "* a single band sits at the midpoint\n"
             "* the built-in sun-sky covers 400-2500nm natively"),
    cl::init(float2{WAVELENGTH_MIN, WAVELENGTH_MAX}), cl::cat(catWavelength)};
static cl::opt<std::string> optWavelengths{
    "wavelengths",
    cl::desc("Explicit wavelengths in nm, comma-separated and strictly "
             "increasing\n(mutually exclusive with -bands/-wave-range)\n"
             "* a filename reads whitespace-separated values from that text "
             "file, which is\n  how a sensor's hundreds of band centers "
             "arrive"),
    cl::cat(catWavelength)};
//--}
//--{ CLI: Camera Options
static cl::OptionCategory catCamera{"Camera Options"};
static cl::opt<int2> optDims{
    "dims", cl::desc("The image dimensions in pixels (default: 1280,720)"),
    cl::init(int2{1280, 720}), cl::cat(catCamera)};
static cl::opt<float3> optLookFrom{
    "look-from", cl::desc("The position to look from (default: -6,0,2)"),
    cl::init(float3{-6, 0, 2}), cl::cat(catCamera)};
static cl::opt<float3> optLookTo{
    "look-to", cl::desc("The position to look to (default: 0,0,0.5)"),
    cl::init(float3{0, 0, 0.5}), cl::cat(catCamera)};
static cl::opt<float3> optUp{"up", cl::desc("The up vector (default: 0,0,1)"),
                             cl::init(float3{0, 0, 1}), cl::cat(catCamera)};
static cl::opt<float> optFOV{
    "fovy", cl::desc("The vertical FOV in degrees (default: 37.8)"),
    cl::init(37.8f), cl::cat(catCamera)};
static cl::opt<bool> optFrame{
    "frame",
    cl::desc("Solve -look-from/-look-to to fit the scene at the given FOV\n"
             "* the fit is exact per vertex (nothing clips), and runs after "
             "the scene\n"
             "  is committed, so it sees subdivided and displaced geometry"),
    cl::init(false), cl::cat(catCamera)};
static cl::opt<float> optFrameZenith{
    "frame-zenith",
    cl::desc("With -frame, the zenith angle of the scene-to-camera "
             "direction in\n"
             "degrees, like -sun-zenith (default: 65, the camera 25 degrees "
             "above the\n"
             "horizon, which is the standardized 3/4 view)"),
    cl::init(65.0f), cl::cat(catCamera)};
static cl::opt<float> optFrameAzimuth{
    "frame-azimuth",
    cl::desc("With -frame, the azimuth of the scene-to-camera direction in "
             "degrees\n"
             "CCW from +X, like -sun-azimuth (default: solved to maximize "
             "frame fill\n"
             "while avoiding views dominated by backfaces)"),
    cl::init(0.0f), cl::cat(catCamera)};
static cl::opt<float> optFrameMargin{
    "frame-margin",
    cl::desc("With -frame, the padding between the scene and the frame "
             "edge as a\n"
             "fraction of the frame (default: 0.05)"),
    cl::init(0.05f), cl::cat(catCamera)};
static cl::opt<bool> optFrameIgnoreBackfaces{
    "frame-ignore-backfaces",
    cl::desc("With -frame, neither avoid nor warn about views of "
             "backfacing geometry\n"
             "* the veto also stands down on its own when every view shows "
             "backfaces,\n"
             "  which is what unshaded two-sided geometry (foliage cards) "
             "looks like"),
    cl::init(false), cl::cat(catCamera)};
//--}
//--{ CLI: Lens Options
static cl::OptionCategory catLens{"Lens Options"};
static cl::opt<float> optFStop{
    "fstop", cl::desc("Enable DOF by f-number assuming 35mm-format frame"),
    cl::init(0.0f), cl::cat(catLens)};
static cl::opt<float> optAperture{
    "aperture",
    cl::desc("Enable DOF by aperture radius in scene units (mutually exclusive "
             "with -fstop)"),
    cl::init(0.0f), cl::cat(catLens)};
static cl::opt<float> optFocus{
    "focus",
    cl::desc("The focus distance along the view axis in scene units (default: "
             "distance between -look-from and -look-to)"),
    cl::init(0.0f), cl::cat(catLens)};
static cl::opt<int> optBlades{
    "blades",
    cl::desc("The number of aperture blades (default: 0, a round lens)"),
    cl::init(0), cl::cat(catLens)};
static cl::opt<float> optBladeAngle{
    "blade-angle",
    cl::desc("With -blades, the rotation of the aperture polygon in "
             "degrees (default: 0, vertex at screen right)"),
    cl::init(0.0f), cl::cat(catLens)};
static cl::opt<float> optDistortionK1{
    "distortion-k1",
    cl::desc("The radial distortion (barrel >0, pincushion <0, default: 0)\n"
             "* given in units of relative corner displacement, e.g., 0.1 "
             "pushes the corners out by 10 percent"),
    cl::init(0.0f), cl::cat(catLens)};
static cl::opt<float> optDistortionK2{
    "distortion-k2",
    cl::desc("The quartic term of radial distortion (default: 0)\n"
             "* same corner-fraction units as -distortion-k1\n"
             "* real lenses need this to pull the corners back without\n"
             "  over-bending the middle of the frame"),
    cl::init(0.0f), cl::cat(catLens)};
static cl::opt<bool> optDistortionFit{
    "distortion-fit",
    cl::desc("Refit so frame corner directions hold constant under distortion\n"
             "* convenience for comparing renders, not a physical effect"),
    cl::init(false), cl::cat(catLens)};
static cl::opt<float> optVignetting{
    "vignetting",
    cl::desc("The strength of cos^4 falloff (default: 0 is off, 1 is the "
             "physical law)"),
    cl::init(0.0f), cl::cat(catLens)};
static cl::opt<float> optCatEye{
    "cat-eye",
    cl::desc(
        "With -fstop or -aperture, mechanical vignette from the lens barrel\n"
        "* relative displacement at the frame corner in units of rim radius\n"
        "  (0 is off, 0.5 costs 1.35 stops in the corners, 1 is fully dark)"),
    cl::init(0.0f), cl::cat(catLens)};
static cl::opt<float> optCatEyeRadius{
    "cat-eye-radius",
    cl::desc("With -cat-eye, barrel rim radius in scene units (default: "
             "aperture radius, i.e., wide-open)\n"
             "* fixing at the wide-open radius and stopping down with -fstop\n"
             "  weakens the cat's eye on its own, as a real lens does"),
    cl::init(0.0f), cl::cat(catLens)};
//--}
//--{ CLI: Staging Options
static cl::OptionCategory catStaging{"Staging Options"};
static cl::opt<bool> optGround{"ground",
                               cl::desc("Add a ground plane under the scene"),
                               cl::init(false), cl::cat(catStaging)};
static cl::opt<float> optGroundZ{
    "ground-z",
    cl::desc("Place the ground plane at this height instead (implies "
             "-ground)"),
    cl::init(0.0f), cl::cat(catStaging)};
static cl::opt<std::string> optGroundMaterial{
    "ground-material",
    cl::desc("With -ground, the MDL material for the ground plane "
             "(default: 10\% gray)"),
    cl::cat(catStaging)};
static cl::opt<float> optTime{
    "animation-time", cl::desc("The animation time in seconds (default: 0)"),
    cl::init(0.0f), cl::cat(catStaging)};
//--}
//--{ CLI: Sun-Sky Options
static cl::OptionCategory catSunSky{"Sun-Sky Options"};
static cl::opt<bool> optNoSunSky{
    "no-sky",
    cl::desc("Disable the default sun-sky, restoring the black "
             "environment"),
    cl::init(false), cl::cat(catSunSky)};
static cl::opt<float> optSunZenith{
    "sun-zenith",
    cl::desc("The solar zenith angle in degrees, 5-88 (default: 42)"),
    cl::init(42.0f), cl::cat(catSunSky)};
static cl::opt<float> optSunAzimuth{
    "sun-azimuth",
    cl::desc("The solar azimuth angle in degrees CCW from +X (default: 135)"),
    cl::init(135.0f), cl::cat(catSunSky)};
static cl::opt<float> optSkyVisibility{
    "visibility", cl::desc("The aerosol visibility in km, 5-100 (default: 23)"),
    cl::init(23.0f), cl::cat(catSunSky)};
static cl::opt<float> optSkyWaterVapor{
    "water-vapor",
    cl::desc("The water-vapor column scale factor, 0.3-3 (default: 1)"),
    cl::init(1.0f), cl::cat(catSunSky)};
static cl::opt<float> optSkyScale{
    "sky-scale", cl::desc("The sky radiance scale factor (default: 1)"),
    cl::init(1.0f), cl::cat(catSunSky)};
static cl::opt<float> optMoonPhase{
    "moon",
    cl::desc("Enable moonlight mode\n"
             "* pass signed phase angle in degrees (0 is full moon, +/-180 is\n"
             "  new moon, sign picks waxing vs waning)\n"
             "* radiance is ~1e-6 of daylight, use with '-tonemap night'\n"),
    cl::init(0.0f), cl::cat(catSunSky)};
static cl::opt<float> optMoonDistance{
    "moon-distance",
    cl::desc("With -moon, the lunar distance factor (default: 1)\n"
             "* realistic range is ~0.86-1.14 over the month"),
    cl::init(1.0f), cl::cat(catSunSky)};
//--}
//--{ CLI: Image-Based Light Options
static cl::OptionCategory catIBL{"Image-Based Light Options"};
static cl::opt<std::string> optIBLFilename{
    "ibl",
    cl::desc("The IBL filename (any supported format, likely '.hdr', '.exr')"),
    cl::cat(catIBL)};
static cl::opt<float> optIBLScale{
    "ibl-scale", cl::desc("With -ibl, the IBL scale factor (default: 1)"),
    cl::init(1.0f), cl::cat(catIBL)};
//--}
//--{ CLI: Tonemapping Options
static cl::OptionCategory catTonemap{"Tonemapping Options"};
static cl::opt<float> optImageExposure{
    "exposure",
    cl::desc("The exposure applied before tone mapping (default: 1)"),
    cl::init(1.0f), cl::cat(catTonemap)};
static cl::opt<std::string> optTonemap{
    "tonemap", cl::desc(R"(Tone mapping for 8-bit output (default: linear)
* 'linear' passes the radiance through to the display curve
* 'log' is shorthand for '-tonemap linear -curve log'
* 'night' models human vision at absolute luminance (rods dominate below ~3
  cd/m^2, desaturated, blue-shifted, colorless by ~0.005) and auto-exposes
  the display brightness, so physically dim scenes like moonlight are both
  visible and perceptually right. In bright scenes 'night' behaves like
  auto-exposed 'linear')"),
    cl::init(std::string("linear")), cl::cat(catTonemap)};
static cl::opt<float> optTonemapDecades{
    "tonemap-decades",
    cl::desc("With -curve log, how many decades below white reach "
             "black (default: 4)"),
    cl::init(4.0f), cl::cat(catTonemap)};
static cl::opt<std::string> optCurve{
    "curve", cl::desc(R"(The display curve for 8-bit output (default: gamma)
* 'gamma' straightforwardly clamps and gamma-encodes
* 'log' maps decades below the exposure-scaled white point for scenes
  where the radiance spans several orders of magnitude
* 'filmic' rolls highlights off toward white instead of clipping them
  per channel, so saturated colors bleach the way film does; reproduces 
  color exactly below the rolloff and never clips, good to use with
  '-local fusion')"),
    cl::init(std::string("gamma")), cl::cat(catTonemap)};
static cl::opt<std::string> optLocal{
    "local", cl::desc(R"(Local tone mapping for 8-bit output (default: off)
* 'fusion' brackets the render into several synthetic exposures and
  blends them by Laplacian pyramid, the bracket is taken from the image 
  histogram, so this needs no setup and handles any dynamic range; it also 
  auto-exposes, leaving -exposure a relative adjustment)"),
    cl::init(std::string("off")), cl::cat(catTonemap)};
static cl::opt<float> optLocalStrength{
    "local-strength",
    cl::desc("With -local, how much local exposure to keep (default: 0.75)\n"
             "* 0 is the globally auto-exposed image, 1 is the full "
             "local result"),
    cl::init(0.75f), cl::cat(catTonemap)};
static cl::opt<float> optLocalRange{
    "local-range",
    cl::desc("With -local, the total bracket in EV (default: 0 means infer)"),
    cl::init(0.0f), cl::cat(catTonemap)};
static cl::opt<float> optLocalClamp{
    "local-clamp",
    cl::desc("With -local, the largest local exposure deviation in EV "
             "(default: 3)"),
    cl::init(3.0f), cl::cat(catTonemap)};
static cl::opt<bool> optFalseColor{
    "false-color",
    cl::desc("Force the false-color band mapping for the RGB outputs\n"
             "* false color engages by itself when the wavelength grid "
             "cannot cover the\n  visible; this forces it for a grid that "
             "could"),
    cl::init(false), cl::cat(catTonemap)};
static cl::opt<float3> optRGBWaves{
    "rgb-waves",
    cl::desc("With false color, the wavelengths in nm mapped to R,G,B\n"
             "(default: the bands at 5/6, 1/2, and 1/6 of the grid span, "
             "long to red)\n"
             "* giving -rgb-waves also forces false color"),
    cl::init(float3{}), cl::cat(catTonemap)};
//--}
//--{ CLI: Output Options
static cl::OptionCategory catOutput{"Output Options"};
static cl::opt<std::string> optOutput{
    "output", cl::desc("The tone mapped image filename (default: output.png)"),
    cl::init(std::string("output.png")), cl::cat(catOutput)};
static cl::opt<std::string> optOutputFloat{
    "output-float",
    cl::desc("Also write the linear radiance to this '.exr' or '.hdr' file, "
             "with no exposure or gamma applied"),
    cl::cat(catOutput)};
static cl::opt<std::string> optOutputSpectral{
    "output-spectral",
    cl::desc("Also write every wavelength band to this ENVI file, alongside "
             "which a '.hdr' header is written\n"
             "* the header records the samples per pixel, so a later run can "
             "pick up where\n  this one left off with -resume\n"
             "* implied by -resume, so it only needs to be given to redirect "
             "the write away\n  from the file being resumed"),
    cl::cat(catOutput)};
static cl::opt<std::string> optResume{
    "resume",
    cl::desc(
        "Resume accumulating from this ENVI file written by a previous run's "
        "-output-spectral\n"
        "* implies '-output-spectral' back to the same file, so one command "
        "line can be\n  re-run to keep accumulating (an explicit "
        "-output-spectral still wins)\n"
        "* the file not existing yet is fine: this run is then the first "
        "session of the\n  sequence and renders from scratch\n"
        "* continues the sample sequence where the file left off and merges "
        "both\n  sessions, so scene and camera flags must match for the merge "
        "to mean anything\n"
        "* reads before rendering, so writing back to the same file is safe\n"
        "* with '-spp 0', re-runs the output stage without rendering\n"
        "* with no wavelength flags at all, the file's grid is adopted\n"
        "* works with -guide, though the SD-tree is not saved between "
        "sessions, so a\n  resumed session retrains it from scratch"),
    cl::cat(catOutput)};
static cl::opt<std::string> optProgressFile{
    "progress-file",
    cl::desc("Write the progress as one line into this file, rewritten about "
             "ten times a second\n"
             "* 'done=N total=M elapsed=S eta=S note=...', for a tool "
             "watching the render\n"
             "* the bar on stderr is for a person and draws nothing into a "
             "pipe, so this is\n  how the Blender add-on follows a render"),
    cl::cat(catOutput)};
static cl::opt<double> optPreviewEvery{
    "preview-every",
    cl::desc("Rewrite '-output' about this often (in seconds) while "
             "rendering, so that\nsomething watching the file sees the image "
             "converge instead of nothing\n"
             "* the samples are unchanged, so the finished image is exactly "
             "what it would\n  have been; only the writing is extra\n"
             "* with '-guide' the checkpoints land on the pass boundaries "
             "instead, which\n  already grow geometrically"),
    cl::init(0.0), cl::cat(catOutput)};
static cl::opt<std::string> optProgress{
    "progress",
    cl::desc("Draw a progress bar while rendering: 'auto', 'plain', or 'none' "
             "(default: auto)\n"
             "* auto uses block characters when the locale says UTF-8 and "
             "falls back to ASCII\n  when it does not; plain always draws the "
             "ASCII bar\n"
             "* a captured or redirected stderr never gets a bar, only a "
             "line recording how\n  long the render took, so a scripted run "
             "stays greppable"),
    cl::init(std::string("auto")), cl::cat(catOutput)};
static cl::opt<std::string> optProfile{
    "profile",
    cl::desc(
        "Write a time-trace JSON of everything before rendering starts: MDL "
        "parse and JIT\ncompile, scene import, and acceleration structures "
        "(default: smdl-toy.trace.json)\n"
        "* open in chrome://tracing or https://ui.perfetto.dev\n"
        "* the render loop is deliberately not traced; the format suits a "
        "sequential\n  breakdown, not per-sample shader timing"),
    cl::ValueOptional, cl::init(std::string{}), cl::cat(catOutput)};
//--}

// The command line, joined for the `smdl args` metadata field, with the
// session-only flags stripped: outputs, display transforms, the sample
// budget, the guiding strategy, and -resume itself legitimately change
// between the sessions of one render, while anything else that differs
// likely changes the radiance being estimated and earns a warning. The
// wavelength flags are stripped too: a genuine grid mismatch already
// has its own hard error, so warning here would double-report.
// Tokenizes on whitespace, so a path containing spaces can misalign the
// comparison; the result only feeds a warning, never behavior.
[[nodiscard]] static std::vector<std::string>
stripSessionOnlyArgs(const std::string &args) {
  // Split by whether the flag's value arrives as a separate token, so
  // that token is stripped with it; the boolean guiding flags carry no
  // value and must not eat the token after them.
  static constexpr const char *SESSION_ONLY_VALUE[]{"resume",
                                                    "spp",
                                                    "output",
                                                    "output-float",
                                                    "output-spectral",
                                                    "exposure",
                                                    "tonemap",
                                                    "tonemap-decades",
                                                    "curve",
                                                    "local",
                                                    "local-strength",
                                                    "local-range",
                                                    "local-clamp",
                                                    "bands",
                                                    "wave-range",
                                                    "wavelengths",
                                                    "guide-bsdf-fraction"};
  static constexpr const char *SESSION_ONLY_FLAG[]{"guide", "guide-adrrs"};
  auto tokens{std::vector<std::string>()};
  for (size_t pos{}; pos < args.size();) {
    size_t end{args.find_first_of(" \t", pos)};
    if (end == std::string::npos) end = args.size();
    if (end > pos) tokens.push_back(args.substr(pos, end - pos));
    pos = end + 1;
  }
  auto result{std::vector<std::string>()};
  for (size_t i = 0; i < tokens.size(); i++) {
    const auto &token{tokens[i]};
    bool isSessionOnly{false};
    bool takesValue{false};
    bool hasAttachedValue{false};
    if (!token.empty() && token[0] == '-') {
      auto name{token.substr(token.find_first_not_of('-'))};
      auto equals{name.find('=')};
      hasAttachedValue = equals != std::string::npos;
      name = name.substr(0, equals);
      for (const auto *sessionOnlyName : SESSION_ONLY_VALUE)
        if (name == sessionOnlyName) {
          isSessionOnly = true;
          takesValue = true;
          break;
        }
      if (!isSessionOnly)
        for (const auto *sessionOnlyName : SESSION_ONLY_FLAG)
          if (name == sessionOnlyName) {
            isSessionOnly = true;
            break;
          }
    }
    if (isSessionOnly) {
      if (takesValue && !hasAttachedValue && i + 1 < tokens.size()) i++;
      continue;
    }
    result.push_back(token);
  }
  return result;
}

// Parse the '-wavelengths' flag: wavelengths in nanometers separated by
// commas or whitespace, or the name of a text file of the same, which
// wins whenever the value opens as a file. NOT '@file': LLVM's command
// line expands '@'-prefixed argv tokens as response files before any
// option sees them. Returns empty when the flag was not given; anything
// else must be a finite, positive, strictly increasing list.
[[nodiscard]] static std::vector<float>
parseWavelengthsFlag(const std::string &flagValue) {
  auto values{std::vector<float>()};
  if (flagValue.empty()) return values;
  auto text{flagValue};
  if (std::ifstream file{flagValue}; file) {
    text.assign(std::istreambuf_iterator<char>(file), {});
    if (text.empty())
      throw smdl::Error(smdl::concat("-wavelengths file ",
                                     smdl::Quoted(flagValue), " is empty"));
  }
  const char *ptr{text.c_str()};
  while (*ptr) {
    if (*ptr == ',' || std::isspace(static_cast<unsigned char>(*ptr))) {
      ptr++;
      continue;
    }
    char *numEnd{};
    const float value{std::strtof(ptr, &numEnd)};
    if (numEnd == ptr)
      throw smdl::Error(smdl::concat("cannot parse -wavelengths near ",
                                     smdl::Quoted(std::string(ptr, 0, 12))));
    ptr = numEnd;
    values.push_back(value);
  }
  if (values.empty())
    throw smdl::Error("expected -wavelengths to name at least 1 wavelength");
  for (size_t i = 0; i < values.size(); i++) {
    if (!(std::isfinite(values[i]) && values[i] > 0))
      throw smdl::Error(
          "expected every -wavelengths value to be positive and finite");
    if (i > 0 && !(values[i] > values[i - 1]))
      throw smdl::Error("expected -wavelengths to be strictly increasing");
  }
  return values;
}

// The material a scene falls back to when it has none of its own: a
// plain 20 percent Lambertian, so a layout can be looked at before any
// material has been written for it. Deliberately the dullest thing that
// can be shaded, so nothing about it can be mistaken for a shading
// decision the scene has not made yet.
constexpr const char *DEFAULT_MATERIAL_SOURCE = R"(#smdl
import ::df::*;

export material default_material() = material(
  surface: material_surface(
    scattering: df::diffuse_reflection_bsdf(tint: color(0.2))));

// Half the default gray, so that an unshaded object on the -ground plane
// reads against it instead of dissolving into it.
export material default_ground() = material(
  surface: material_surface(
    scattering: df::diffuse_reflection_bsdf(tint: color(0.1))));
)";

// What the compiler calls the module above, which is what qualifies the
// names of the two materials in it.
constexpr const char *DEFAULT_MATERIAL_MODULE = "::smdl_toy_default";

// The material `-material-fallback` names to get it.
constexpr const char *DEFAULT_MATERIAL_NAME = "default_material";

// The darker gray `-ground` defaults to, for contrast against the
// default material above.
constexpr const char *DEFAULT_GROUND_MATERIAL_NAME = "default_ground";

// How the sample budget is split into passes.
//
// Without guiding there is a single pass of the whole budget. With guiding,
// passes grow geometrically (1, 2, 4, ... spp) and the remainder is dumped
// into the final pass, so it always holds at least half the budget. Solved
// up front rather than as the loop runs so that the progress bar can say
// which pass of how many.
[[nodiscard]] static std::vector<size_t> solveSamplePasses(size_t spp,
                                                           bool guide) {
  auto passes{std::vector<size_t>()};
  for (size_t sppDone{0}; sppDone < spp;) {
    size_t thisPass{guide ? std::min(size_t(1) << passes.size(), spp - sppDone)
                          : spp};
    if (guide && (spp - sppDone) < 2 * thisPass) thisPass = spp - sppDone;
    passes.push_back(thisPass);
    sppDone += thisPass;
  }
  return passes;
}

int main(int argc, char **argv) try {
  llvm::InitLLVM X(argc, argv);
  // Prints exactly like 'print_to_cerr', except that it knows to step
  // around a progress bar while one is on screen.
  smdl::Logger::get().addSink<ProgressLogSink>();
  cl::SetVersionPrinter([](llvm::raw_ostream &os) {
    os << smdl::BuildInfo::get().toString();
    os << "smdl-toy:\n";
    os << "  Embree:     " << RTC_VERSION_STRING << '\n';
    os << "  Assimp:     " << aiGetVersionMajor() << '.' << aiGetVersionMinor()
       << '.' << aiGetVersionPatch() << '\n';
    os << "  OpenSubdiv: " << OPENSUBDIV_VERSION_MAJOR << '.'
       << OPENSUBDIV_VERSION_MINOR << '.' << OPENSUBDIV_VERSION_PATCH << '\n';
  });
  cl::HideUnrelatedOptions({&catScene, &catSampling, &catWavelength, &catCamera,
                            &catLens, &catSunSky, &catIBL, &catStaging,
                            &catTonemap, &catOutput});
  cl::ParseCommandLineOptions(argc, argv, "SpectralMDL toy renderer");
  // Validate the occurrence-dependent lens flags here at the CLI, where
  // "was this given at all" is knowable; in the `CameraOptions` built
  // below zero means unset, so an explicit value has to be positive to
  // stay distinguishable.
  if (optFStop.getNumOccurrences() > 0 && optAperture.getNumOccurrences() > 0)
    throw smdl::Error("expected at most one of -fstop and -aperture "
                      "(they are two spellings of the same quantity)");
  if (optFStop.getNumOccurrences() > 0 && !(float(optFStop) > 0))
    throw smdl::Error("expected -fstop to be positive");
  if (optAperture.getNumOccurrences() > 0 && !(float(optAperture) > 0))
    throw smdl::Error("expected -aperture to be positive");
  if (optFocus.getNumOccurrences() > 0 && !(float(optFocus) > 0))
    throw smdl::Error("expected -focus to be positive");
  if (optCatEyeRadius.getNumOccurrences() > 0 && !(float(optCatEyeRadius) > 0))
    throw smdl::Error("expected -cat-eye-radius to be positive");
  if (optFrame && (optLookFrom.getNumOccurrences() > 0 ||
                   optLookTo.getNumOccurrences() > 0))
    throw smdl::Error("expected at most one of -frame and "
                      "-look-from/-look-to (framing solves the camera "
                      "position)");
  if (!(float(optFrameZenith) >= 1 && float(optFrameZenith) <= 179))
    throw smdl::Error("expected -frame-zenith between 1 and 179");
  if (!(float(optFrameMargin) >= 0 && float(optFrameMargin) <= 0.5f))
    throw smdl::Error("expected -frame-margin between 0 and 0.5");
  // '-tonemap log' is kept as shorthand for '-tonemap linear -curve
  // log'; allow it, but not while the curve says otherwise.
  if (std::string(optTonemap) == "log" && optCurve.getNumOccurrences() > 0 &&
      std::string(optCurve) != "log")
    throw smdl::Error("expected -curve log with -tonemap log ('-tonemap log' "
                      "is shorthand for '-tonemap linear -curve log')");
  if (!(float(optLocalStrength) >= 0) || !(float(optLocalStrength) <= 1))
    throw smdl::Error("expected -local-strength between 0 and 1");
  if (!(float(optLocalClamp) > 0))
    throw smdl::Error("expected -local-clamp to be positive");
  if (optLocalRange.getNumOccurrences() > 0 && !(float(optLocalRange) > 0))
    throw smdl::Error("expected -local-range to be positive");
  if (optWavelengths.getNumOccurrences() > 0 &&
      (optBands.getNumOccurrences() > 0 ||
       optWaveRange.getNumOccurrences() > 0))
    throw smdl::Error("expected at most one of -wavelengths and "
                      "-bands/-wave-range (they are two spellings of the "
                      "wavelength grid)");
  if (optBands.getNumOccurrences() > 0 && unsigned(optBands) == 0)
    throw smdl::Error("expected -bands to be at least 1");
  if (const auto range{float2(optWaveRange)};
      !(range.x > 0 && range.y > range.x))
    throw smdl::Error("expected -wave-range to be positive and increasing");
  if (optRGBWaves.getNumOccurrences() > 0) {
    const auto waves{float3(optRGBWaves)};
    if (!(waves.x > 0 && waves.y > 0 && waves.z > 0))
      throw smdl::Error("expected -rgb-waves to be three positive "
                        "wavelengths in nm");
  }
  renderTime() = float(optTime);
  // Parsed and validated now so a typo fails before anything loads.
  const auto explicitWavelengths{
      parseWavelengthsFlag(std::string(optWavelengths))};
  // The display transform only runs after the last sample, so check its
  // names now rather than at the end of an hour-long render.
  auto tonemapOptions{TonemapOptions{}};
  tonemapOptions.mode = std::string(optTonemap);
  tonemapOptions.curve = std::string(optCurve);
  tonemapOptions.local = std::string(optLocal);
  tonemapOptions.exposure = float(optImageExposure);
  tonemapOptions.logDecades = float(optTonemapDecades);
  tonemapOptions.localStrength = float(optLocalStrength);
  tonemapOptions.localRange = float(optLocalRange);
  tonemapOptions.localClamp = float(optLocalClamp);
  validateTonemapOptions(tonemapOptions);
  // The '.places' utilities bow out before anything else: they touch
  // nothing but the named files.
  if (!std::string(optDumpPlaces).empty()) {
    dumpPlaces(std::string(optDumpPlaces));
    return EXIT_SUCCESS;
  }
  if (!std::string(optDumpCurves).empty()) {
    dumpCurves(std::string(optDumpCurves));
    return EXIT_SUCCESS;
  }
  if (!std::string(optPackPlaces).empty()) {
    packPlaces(std::string(optPackPlaces), std::string(optOutputPlaces));
    return EXIT_SUCCESS;
  }
  // The positional scene argument is required for everything that
  // remains; see the note on its declaration.
  if (std::string(optInputSceneFile).empty())
    throw smdl::Error("expected an <input scene> argument");
  // Validated now, though not read until the render starts.
  auto progressOptions{ProgressOptions{}};
  progressOptions.label = "Rendering";
  progressOptions.units = "px";
  progressOptions.style = std::string(optProgress);
  progressOptions.filePath = std::string(optProgressFile);
  validateProgressOptions(progressOptions);
  // The profiler covers everything from here to just before the render
  // loop: layout parsing, MDL compilation, scene import, and the
  // acceleration structures. The library's own entries (module parse, IR
  // emission, LLVM optimization, image loads) only record once this is
  // initialized. NOTE: The LLVM time-trace instance is thread-local, so
  // entries are only ever begun on this thread; parallel work is timed by
  // hand and reported through logging instead.
  const bool profiling{optProfile.getNumOccurrences() > 0};
  const auto profileFileName{std::string(optProfile).empty()
                                 ? std::string("smdl-toy.trace.json")
                                 : std::string(optProfile)};
  if (profiling) smdl::profilerInitialize();
  // Everything the command line asks for, lowered into one layout: the
  // positional argument first, then each -mesh. Either may name a mesh
  // file or a '.layout'. Read before the camera because a layout may
  // describe one; it is only a text parse, so fail-fast ordering costs
  // nothing. Tilde is expanded here because a search path is typed by a
  // person.
  auto assetSearchPath{AssetSearchPath()};
  for (const auto &directory : optAssetDirs)
    assetSearchPath.push_back(smdl::makePathCanonical(directory));
  auto profReadLayout{smdl::profilerEntryBegin("Read layout")};
  auto layout{
      resolveLayoutArgument(std::string(optInputSceneFile), assetSearchPath)};
  for (const auto &fileName : optInputMeshFiles) {
    auto more{resolveLayoutArgument(fileName, assetSearchPath)};
    layout.items.insert(layout.items.end(), more.items.begin(),
                        more.items.end());
    layout.entryMaterialAliases.insert(more.entryMaterialAliases.begin(),
                                       more.entryMaterialAliases.end());
  }
  smdl::profilerEntryEnd(profReadLayout);
  // The camera, merged from three sources in increasing order of priority:
  // the defaults in `CameraOptions`, whatever the layout's 'camera'
  // directive named, and whatever the command line explicitly gave. A flag
  // that was not given must not override the file, so what decides is the
  // occurrence count rather than the value. Constructed before anything
  // slow loads so that the value-dependent validation in the constructor
  // also fails fast.
  auto pick{[](auto cliValue, unsigned cliOccurrences, const auto &fileValue) {
    return cliOccurrences == 0 && fileValue ? decltype(cliValue)(*fileValue)
                                            : cliValue;
  }};
  const auto &fileCamera{layout.camera};
  auto cameraOptions{CameraOptions{}};
  cameraOptions.dims =
      pick(int2(optDims), optDims.getNumOccurrences(), fileCamera.dims);
  cameraOptions.lookFrom =
      pick(float3(optLookFrom), optLookFrom.getNumOccurrences(),
           fileCamera.lookFrom);
  cameraOptions.lookTo =
      pick(float3(optLookTo), optLookTo.getNumOccurrences(), fileCamera.lookTo);
  cameraOptions.up =
      pick(float3(optUp), optUp.getNumOccurrences(), fileCamera.up);
  cameraOptions.fovYInDegrees =
      pick(float(optFOV), optFOV.getNumOccurrences(), fileCamera.fovYInDegrees);
  cameraOptions.fStop =
      pick(float(optFStop), optFStop.getNumOccurrences(), fileCamera.fStop);
  cameraOptions.aperture = pick(
      float(optAperture), optAperture.getNumOccurrences(), fileCamera.aperture);
  cameraOptions.focus =
      pick(float(optFocus), optFocus.getNumOccurrences(), fileCamera.focus);
  cameraOptions.blades =
      pick(int(optBlades), optBlades.getNumOccurrences(), fileCamera.blades);
  cameraOptions.bladeAngleInDegrees =
      pick(float(optBladeAngle), optBladeAngle.getNumOccurrences(),
           fileCamera.bladeAngleInDegrees);
  cameraOptions.distortionK1 =
      pick(float(optDistortionK1), optDistortionK1.getNumOccurrences(),
           fileCamera.distortionK1);
  cameraOptions.distortionK2 =
      pick(float(optDistortionK2), optDistortionK2.getNumOccurrences(),
           fileCamera.distortionK2);
  cameraOptions.distortionFit =
      pick(bool(optDistortionFit), optDistortionFit.getNumOccurrences(),
           fileCamera.distortionFit);
  cameraOptions.vignetting =
      pick(float(optVignetting), optVignetting.getNumOccurrences(),
           fileCamera.vignetting);
  cameraOptions.catEye =
      pick(float(optCatEye), optCatEye.getNumOccurrences(), fileCamera.catEye);
  cameraOptions.catEyeRadius =
      pick(float(optCatEyeRadius), optCatEyeRadius.getNumOccurrences(),
           fileCamera.catEyeRadius);
  cameraOptions.noLOD = bool(optNoLOD);
  // The same exclusivity the command line checks above, now that the file
  // has had its say: either source can supply either spelling, so only the
  // merged pair can be checked for naming both.
  if (cameraOptions.fStop > 0 && cameraOptions.aperture > 0)
    throw smdl::Error("expected at most one of -fstop and -aperture between "
                      "the command line and the scene file's 'camera' "
                      "directive (they are two spellings of the same "
                      "quantity)");
  // Under -frame the position comes from measuring the committed scene,
  // so construction (with the lens validation and the summary it logs)
  // waits until the solve below. Every other path keeps constructing
  // here, before anything slow loads, so a lens typo still fails fast.
  auto camera{std::optional<Camera>()};
  if (!optFrame) camera.emplace(cameraOptions);
  const auto dims{cameraOptions.dims};
  const auto numPixelsX{size_t(dims.x)};
  const auto numPixelsY{size_t(dims.y)};
  const auto spp{size_t(optSPP)};
  // The command line, echoed into the spectral output's metadata so a
  // resumed session can warn when it is being driven differently.
  auto argsEcho{std::string()};
  for (int i = 1; i < argc; i++) {
    if (i > 1) argsEcho += ' ';
    argsEcho += argv[i];
  }
  // Resume: load the prior session's accumulation now, before anything
  // slow happens, so a mismatched file fails fast. The sampler is
  // deterministic in (pixel, sample index) with no seed, so continuing
  // the sample index where the file left off and merging afterward
  // yields the same estimator as one longer uninterrupted run. The
  // flag also implies -output-spectral back to the same file; see the
  // output stage at the bottom.
  auto resumed{smdl::SpectralRenderImage::ENVIFile{}};
  size_t sampleIndexBase{0};
  const bool resumeRequested{!std::string(optResume).empty()};
  bool resuming{false};
  if (resumeRequested) {
    // A wholly missing data-plus-header pair is not an error: it makes
    // this run the first session of an intended sequence, rendering
    // from scratch and writing the file for the next -resume. Half a
    // pair is a damaged prior session, and starting fresh over it
    // would clobber what is left, so that stays fatal.
    const auto resumeName{std::string(optResume)};
    const bool haveData{smdl::exists(resumeName)};
    const bool haveHeader{smdl::exists(resumeName + ".hdr")};
    if (haveData != haveHeader)
      throw smdl::Error(smdl::concat(
          "cannot resume: ",
          smdl::Quoted(haveData ? resumeName : resumeName + ".hdr"),
          " exists but ",
          smdl::Quoted(haveData ? resumeName + ".hdr" : resumeName),
          " does not; refusing to start fresh over a damaged session"));
    if (!haveData) {
      // -spp 0 re-runs the output stage, which is meaningless with
      // nothing to load; worse, the 0-sample file it would write has
      // no 'samples per pixel' field and could not itself be resumed.
      if (spp == 0)
        throw smdl::Error(smdl::concat(
            "cannot resume with '-spp 0': ", smdl::Quoted(resumeName),
            " does not exist, so there is no output stage to re-run"));
      SMDL_LOG_INFO(
          "Starting a new render sequence: ", smdl::Quoted(resumeName),
          " does not exist yet, this session writes it");
    }
    resuming = haveData;
  }
  if (resuming) {
    resumed = smdl::SpectralRenderImage::readENVIFile(std::string(optResume));
    if (resumed.image.getNumPixelsX() != numPixelsX ||
        resumed.image.getNumPixelsY() != numPixelsY)
      throw smdl::Error(smdl::concat(
          "cannot resume: the file is ", resumed.image.getNumPixelsX(), "x",
          resumed.image.getNumPixelsY(), " against -dims ", numPixelsX, ",",
          numPixelsY));
    if (resumed.samplesPerPixel == 0)
      throw smdl::Error(
          "cannot resume: the header has no 'samples per pixel' count "
          "(the file was not written by -output-spectral)");
    if (auto itr{resumed.fields.find("smdl sampler")};
        itr == resumed.fields.end() || itr->second != SAMPLER_VERSION)
      SMDL_LOG_WARN(
          "resuming a file from a different sampler: the continuation "
          "samples are independent of the first session's rather than "
          "jointly stratified (still unbiased, noise just improves more "
          "slowly)");
    if (auto itr{resumed.fields.find("smdl args")};
        itr != resumed.fields.end() &&
        stripSessionOnlyArgs(itr->second) != stripSessionOnlyArgs(argsEcho))
      SMDL_LOG_WARN("resuming with different flags: the file records ",
                    smdl::Quoted(itr->second),
                    "; if the scene or camera changed, the merged image "
                    "mixes two different renders");
    sampleIndexBase = resumed.samplesPerPixel;
    SMDL_LOG_INFO("Resuming: ", sampleIndexBase, " samples per pixel from ",
                  smdl::Quoted(std::string(optResume)));
  }
  // The wavelength grid, in priority order: explicit '-wavelengths',
  // '-bands' uniform over '-wave-range' (endpoint-inclusive, a single
  // band at the midpoint), or, when resuming with no grid flags at all,
  // the grid recorded in the resumed file, so a resumed render needs no
  // grid retyping. The band count seeds every 'Color' constructed from
  // here on.
  const bool haveGridFlags{optBands.getNumOccurrences() > 0 ||
                           optWaveRange.getNumOccurrences() > 0 ||
                           optWavelengths.getNumOccurrences() > 0};
  const bool adoptResumedGrid{!haveGridFlags && resuming};
  auto gridSpec{explicitWavelengths};
  if (adoptResumedGrid) {
    if (resumed.wavelengths.empty())
      throw smdl::Error(
          "cannot resume: the file carries no wavelengths to adopt, give "
          "the grid explicitly with -bands/-wave-range or -wavelengths");
    gridSpec = resumed.wavelengths;
  }
  if (gridSpec.empty()) {
    const auto range{float2(optWaveRange)};
    gridSpec.resize(size_t(unsigned(optBands)));
    for (size_t i = 0; i < gridSpec.size(); i++) {
      const float t{gridSpec.size() > 1 ? float(i) / float(gridSpec.size() - 1)
                                        : 0.5f};
      gridSpec[i] = (1 - t) * range.x + t * range.y;
    }
  }
  renderNumBands() = gridSpec.size();
  // Trapezoid band widths for a non-uniform grid. A uniform grid keeps
  // the weights empty and `State::wavelength_weight` null, which the
  // library treats as uniform quadrature, so the default render is
  // unchanged to the bit.
  {
    auto &weights{renderWavelengthWeights()};
    weights.clear();
    bool uniform{true};
    for (size_t i = 2; i < gridSpec.size(); i++)
      if (std::abs((gridSpec[i] - gridSpec[i - 1]) -
                   (gridSpec[1] - gridSpec[0])) >
          1e-3f * (gridSpec[1] - gridSpec[0]))
        uniform = false;
    if (!uniform) {
      weights.resize(gridSpec.size());
      for (size_t i = 0; i < gridSpec.size(); i++) {
        const float lo{i > 0 ? gridSpec[i - 1] : gridSpec[0]};
        const float hi{i + 1 < gridSpec.size() ? gridSpec[i + 1]
                                               : gridSpec[gridSpec.size() - 1]};
        weights[i] = 0.5f * (hi - lo);
      }
    }
  }
  const auto wavelengths{
      Color(smdl::Span<const float>(gridSpec.data(), gridSpec.size()))};
  if (resuming) {
    if (resumed.image.getNumBands() != wavelengths.size())
      throw smdl::Error(smdl::concat(
          "cannot resume: the file has ", resumed.image.getNumBands(),
          " bands against the renderer's ", wavelengths.size()));
    for (size_t i = 0; i < wavelengths.size(); i++)
      if (i >= resumed.wavelengths.size() ||
          !(std::abs(resumed.wavelengths[i] - wavelengths[i]) < 0.5f))
        throw smdl::Error(
            "cannot resume: the wavelength grid does not match the "
            "renderer's");
  }
  if (haveGridFlags || adoptResumedGrid)
    SMDL_LOG_INFO("Wavelength grid: ", wavelengths.size(),
                  adoptResumedGrid ? " bands adopted from the resumed file, "
                                   : " bands, ",
                  wavelengths[0], "-", wavelengths[wavelengths.size() - 1],
                  " nm");
  if (wavelengths.size() > 256)
    SMDL_LOG_WARN(wavelengths.size(),
                  " bands: JIT compile time and per-sample cost both grow "
                  "with the band count, expect a slow start and a slow "
                  "render");
  // Everything RGB-sourced degrades outside the visible; say so once
  // rather than rendering a mysteriously dark image.
  const bool gridBeyondVisible{wavelengths[0] < 379.0f ||
                               wavelengths[wavelengths.size() - 1] > 781.0f};
  if (gridBeyondVisible)
    SMDL_LOG_WARN(
        "the wavelength grid leaves the visible (380-780nm): colored RGB "
        "textures and images contribute nothing outside it (gray extends "
        "flat), metal IOR tables clamp to their measured ranges, and the "
        "RGB outputs project through CIE color matching, so they darken "
        "wherever the grid misses the visible; the ENVI output is the "
        "radiometric record");
  // The accumulation buffers scale as bands times pixels; say so before
  // allocating gigabytes.
  if (const double gib{
          double(numPixelsX * numPixelsY) *
          (8.0 + 8.0 * double(wavelengths.size()) +
           (optGuide ? 16.0 * double(wavelengths.size()) + 24.0 : 0.0)) /
          (1024.0 * 1024.0 * 1024.0)};
      gib > 1.0)
    SMDL_LOG_INFO("Accumulation buffers: ", gib, " GiB");

  auto compiler{smdl::Compiler{}};
  compiler.wavelengthBaseMax = uint32_t(wavelengths.size());
  compiler.enableDebug = false;
  compiler.enableMipMaps = !optNoMipMaps;
  compiler.enableUnitTests = false;
  // The built-in stand-in, always available: a scene whose materials
  // have not been written yet still renders, and a name that does not
  // resolve has somewhere to fall back to. It is added even when MDL
  // modules are given, so that '-material-fallback default' works
  // alongside them.
  if (auto error{
          compiler.addCode(DEFAULT_MATERIAL_MODULE, DEFAULT_MATERIAL_SOURCE)})
    error->printAndExit();
  for (auto &inputMDLFile : optInputMDLFiles)
    if (auto error{compiler.add(std::string(inputMDLFile))})
      error->printAndExit();
  if (optListObjects) {
    if (optJSON) {
      printObjectTableJSON(layout);
    } else {
      printObjectTable(layout);
    }
    if (profiling) smdl::profilerFinalize(profileFileName.c_str());
    return EXIT_SUCCESS;
  } else if (optListMaterials) {
    // The table reports how every name resolves, so it must see the
    // unfiltered material list; and it never calls JIT'd code, so
    // compile() alone is enough.
    const smdl::Compiler *compilerOrNull{};
    if (!optInputMDLFiles.empty()) {
      if (auto error{compiler.compile(smdl::OPT_LEVEL_O2)})
        error->printAndExit();
      compilerOrNull = &compiler;
    }
    if (optJSON) {
      printMaterialTableJSON(compilerOrNull, layout);
    } else {
      printMaterialTable(compilerOrNull, layout);
    }
    if (profiling) smdl::profilerFinalize(profileFileName.c_str());
    return EXIT_SUCCESS;
  }
  // A scene given no MDL at all is a layout that has not been shaded yet,
  // so it falls back to the built-in material rather than refusing to
  // render. Given MDL, an unresolved name stays an error, since there it
  // means a name that was meant to resolve and did not.
  auto fallbackMaterial{std::string(optMaterialFallback)};
  if (fallbackMaterial.empty() && optInputMDLFiles.empty())
    fallbackMaterial = DEFAULT_MATERIAL_NAME;
  // The lowering folds every alias and override into the items
  // themselves, which is what keeps an imported layout's names closed;
  // see `MaterialAssignment::renames`.
  auto scene{Scene(compiler, fallbackMaterial)};
  for (const auto &item : layout.items) {
    SMDL_PROFILER_ENTRY("Scene::add()", item.fileName.c_str());
    scene.add(item);
  }
  // The ground plane goes in before commit() because commit() builds the
  // acceleration structure, and is sized from the pre-commit bounds
  // because the Embree bounds do not exist yet. Displacement can still
  // push geometry slightly below the pre-displacement minimum; -ground-z
  // is the override, and thumbnails do not care.
  auto groundInstance{INVALID_INDEX};
  // The pre-ground geometry bounds, remembered because the SD-tree below
  // must NOT be sized by the ground plane: the plane is a backdrop three
  // orders of magnitude wider than the subject, and cubifying over it
  // spends every spatial refinement level zooming back in.
  auto guideBoundsValid{false};
  float3 guideLower{}, guideUpper{};
  if (optGround || optGroundZ.getNumOccurrences() > 0) {
    float3 lower{}, upper{};
    scene.preCommitBounds(lower, upper);
    guideBoundsValid = true;
    guideLower = lower;
    guideUpper = upper;
    if (!(lower.x <= upper.x))
      throw smdl::Error("cannot -ground: the scene has no geometry to "
                        "put a plane under");
    const float z{optGroundZ.getNumOccurrences() > 0 ? float(optGroundZ)
                                                     : lower.z};
    // Large enough that at framing elevations the plane's edge lands at
    // the visual horizon, small enough to stay in float precision.
    const float halfExtent{
        std::min(std::max(1000.0f * 0.5f * smdl::length(upper - lower), 100.0f),
                 20000.0f)};
    auto groundMaterial{std::string(optGroundMaterial)};
    if (groundMaterial.empty()) groundMaterial = DEFAULT_GROUND_MATERIAL_NAME;
    // The one command-line-facing name the entry file's aliases still
    // reach, now that the aliases themselves are folded into the items.
    if (auto alias{layout.entryMaterialAliases.find(groundMaterial)};
        alias != layout.entryMaterialAliases.end())
      groundMaterial = alias->second;
    groundInstance = scene.addGroundPlane(z, halfExtent, groundMaterial);
    SMDL_LOG_INFO("Ground plane: z = ", z, ", half extent ", halfExtent,
                  ", material ", smdl::Quoted(groundMaterial));
  }
  // The imports above interned every name the scene can shade with, so
  // narrow the compile to those materials; the fallback and the exterior
  // medium are looked up by name later, so they join the list. With no
  // MDL modules there is only the built-in default module, nothing worth
  // filtering, and the unshaded-scene workflow would warn for every name.
  if (!optAllMaterials && !optInputMDLFiles.empty()) {
    auto desiredMaterials{scene.usedMaterialNames()};
    if (!fallbackMaterial.empty()) desiredMaterials.push_back(fallbackMaterial);
    if (!layout.exteriorMediumName.empty())
      desiredMaterials.push_back(layout.exteriorMediumName);
    // The empty name (an unnamed primitive or groom) can only ever
    // resolve through the fallback.
    desiredMaterials.erase(std::remove(desiredMaterials.begin(),
                                       desiredMaterials.end(), std::string()),
                           desiredMaterials.end());
    compiler.setDesiredMaterials(std::move(desiredMaterials));
  }
  if (auto error{compiler.compile(smdl::OPT_LEVEL_O2)}) error->printAndExit();
  if (auto error{compiler.jitCompile()}) error->printAndExit();
  {
    SMDL_PROFILER_ENTRY("Scene::commit()");
    scene.commit(wavelengths);
  }
  // The framing solve, and the deferred camera construction it exists
  // for. The solved azimuth also becomes the default sun azimuth below,
  // so a batch of thumbnails is consistently lit however each one is
  // framed.
  auto framedSunAzimuth{std::optional<float>()};
  if (optFrame) {
    auto framingOptions{FramingOptions{}};
    framingOptions.fovYInDegrees = cameraOptions.fovYInDegrees;
    framingOptions.aspectRatio = float(dims.x) / float(dims.y);
    framingOptions.zenithInDegrees = float(optFrameZenith);
    if (optFrameAzimuth.getNumOccurrences() > 0) {
      framingOptions.azimuthInDegrees = float(optFrameAzimuth);
    } else if (layout.frontAzimuth) {
      framingOptions.azimuthInDegrees = layout.frontAzimuth;
      SMDL_LOG_INFO("Framing: locked to the manifest's front azimuth ",
                    *layout.frontAzimuth, " degrees");
    }
    framingOptions.margin = float(optFrameMargin);
    framingOptions.ignoreBackfaces = bool(optFrameIgnoreBackfaces);
    framingOptions.skipInstance = groundInstance;
    const auto framed{solveFraming(scene, framingOptions)};
    cameraOptions.lookFrom = framed.lookFrom;
    cameraOptions.lookTo = framed.lookTo;
    camera.emplace(cameraOptions);
    // The key light over the camera's right shoulder.
    framedSunAzimuth = framed.azimuthInDegrees - 35.0f;
  }

  // The environment, merged from the same three sources as the camera and
  // in the same order: the defaults, the layout's 'sky' directive, and
  // whatever the command line explicitly gave.
  const auto &fileSky{layout.sky};
  const auto iblFileName{pick(std::string(optIBLFilename),
                              optIBLFilename.getNumOccurrences(),
                              fileSky.iblFileName)};
  const auto moonGiven{optMoonPhase.getNumOccurrences() > 0 ||
                       bool(fileSky.moonPhase)};
  std::unique_ptr<EnvLight> envLight{};
  if (!iblFileName.empty()) {
    envLight = std::make_unique<EnvLight>(
        iblFileName, pick(float(optIBLScale), optIBLScale.getNumOccurrences(),
                          fileSky.iblScale));
    if (gridBeyondVisible)
      SMDL_LOG_WARN("-ibl is an RGB image: on this wavelength grid it "
                    "contributes only inside the visible");
  } else if (!pick(bool(optNoSunSky), optNoSunSky.getNumOccurrences(),
                   fileSky.none)) {
    auto options{smdl::SunSkyOptions{}};
    const float zenith{pick(float(optSunZenith),
                            optSunZenith.getNumOccurrences(),
                            fileSky.sunZenith) *
                       PI / 180.0f};
    float azimuthInDegrees{pick(float(optSunAzimuth),
                                optSunAzimuth.getNumOccurrences(),
                                fileSky.sunAzimuth)};
    // Under -frame with no stated sun azimuth, the key light follows the
    // solved camera: a perfectly framed thumbnail lit from behind is as
    // unreadable as one framed end-on, and this keeps a whole library
    // consistently lit however each asset was framed.
    if (framedSunAzimuth && optSunAzimuth.getNumOccurrences() == 0 &&
        !fileSky.sunAzimuth) {
      azimuthInDegrees = *framedSunAzimuth;
      SMDL_LOG_INFO("Sun azimuth follows the framed camera: ", azimuthInDegrees,
                    " degrees");
    }
    const float azimuth{azimuthInDegrees * PI / 180.0f};
    options.sunDirection =
        float3(std::sin(zenith) * std::cos(azimuth),
               std::sin(zenith) * std::sin(azimuth), std::cos(zenith));
    options.visibility =
        pick(float(optSkyVisibility), optSkyVisibility.getNumOccurrences(),
             fileSky.visibility);
    options.waterVaporScale =
        pick(float(optSkyWaterVapor), optSkyWaterVapor.getNumOccurrences(),
             fileSky.waterVapor);
    options.scaleFactor = pick(float(optSkyScale),
                               optSkyScale.getNumOccurrences(), fileSky.scale);
    if (moonGiven) {
      options.moon = true;
      options.moonPhase =
          pick(float(optMoonPhase), optMoonPhase.getNumOccurrences(),
               fileSky.moonPhase);
      options.moonDistanceScale =
          pick(float(optMoonDistance), optMoonDistance.getNumOccurrences(),
               fileSky.moonDistance);
    }
    envLight = std::make_unique<EnvLight>(options);
  }

  // The exterior medium the layout's 'medium' directive names, if
  // any: one material instance evaluated up front in an allocator that
  // outlives the render, seeding every camera path's medium stack. The
  // instance has no geometry, so heterogeneous coefficient queries run
  // in world space directly.
  auto exteriorMediumAllocator{smdl::BumpPtrAllocator()};
  const MediumStack *exteriorMedium{};
  if (!layout.exteriorMediumName.empty()) {
    const auto *material{compiler.findMaterial(layout.exteriorMediumName)};
    if (!material)
      throw smdl::Error(smdl::concat(
          "cannot resolve 'medium' directive material ",
          smdl::Quoted(layout.exteriorMediumName),
          optInputMDLFiles.empty() ? " (no MDL modules were given)" : ""));
    if (!material->hasVolume())
      throw smdl::Error(smdl::concat("'medium' directive material ",
                                     smdl::Quoted(layout.exteriorMediumName),
                                     " has no 'volume'"));
    auto state{makeRenderState(wavelengths, &exteriorMediumAllocator)};
    state.finalizeAndApplyInternalSpaceConventions();
    exteriorMedium = new (exteriorMediumAllocator) MediumStack{
        nullptr, smdl::JIT::MaterialInstance(state, material), nullptr};
    SMDL_LOG_INFO("Exterior medium: ", smdl::Quoted(layout.exteriorMediumName));
  }
  // Every light in one selection path: each emissive mesh instance plus
  // the environment, weighted by power.
  auto profLightSampler{smdl::profilerEntryBegin("Build light sampler")};
  const auto lights{LightSampler(compiler, scene, envLight.get(), wavelengths)};
  smdl::profilerEntryEnd(profLightSampler);
  // The render loop is deliberately outside the trace; see -profile.
  if (profiling) smdl::profilerFinalize(profileFileName.c_str());
  auto renderImage{
      smdl::SpectralRenderImage(wavelengths.size(), numPixelsX, numPixelsY)};
  // An upper bound on the walk depth, which also sizes the training-record
  // buffer. Paths are terminated by Russian roulette in `tracePath` long
  // before this, so it is set high enough that clipping it is negligible
  // even for high-albedo transport.
  constexpr int MAX_PATH_LEN = 64;
  auto sdtree{std::unique_ptr<STree>()};
  if (optGuide) {
    // With a ground plane, guide over the actual geometry padded by half
    // its own size, so the plane's enormous backdrop extent does not
    // dilute the spatial resolution; vertices on the far plane clamp
    // into the border cells, where there is nothing worth guiding
    // anyway. Without one, the scene bounds are the geometry bounds.
    auto center{scene.boundCenter};
    auto r{scene.boundRadius};
    if (guideBoundsValid) {
      center = 0.5f * (guideLower + guideUpper);
      r = 0.75f * smdl::length(guideUpper - guideLower);
    }
    sdtree = std::make_unique<STree>(center - float3(r, r, r),
                                     center + float3(r, r, r));
    SMDL_LOG_INFO("Guide bounds: center (", center.x, ", ", center.y, ", ",
                  center.z, "), radius ", r);
  }
  // The combination of the guided passes, which also maintains the
  // ADRRS pixel estimates between passes. Null without guiding, where
  // the single pass accumulates straight into `renderImage`.
  auto combiner{std::unique_ptr<PassCombiner>()};
  if (optGuide) {
    combiner = std::make_unique<PassCombiner>(numPixelsX, numPixelsY);
    // Seed with the prior session's accumulation, so resolve() below
    // reproduces the full merged image (the unguided path merges with
    // an image-level add instead) and the first pass's ADRRS starts
    // from the resumed estimates rather than zero.
    if (resuming) {
      combiner->seed(resumed.image, resumed.samplesPerPixel);
      combiner->rebuildPixelEstimates();
    }
  }
  // Progress is counted in samples rather than pixels, so that the
  // geometrically growing passes below read as one bar that only ever
  // moves forward. The counters still show pixels, which is the number a
  // person pictures. Nothing is drawn unless stderr is a terminal, where
  // the summary below takes the bar's place.
  // The radiance the renderer estimates, as linear RGB: what the floating
  // point output holds, and what every tonemap displays. Resolved here
  // rather than at the outputs because a checkpoint image runs the same
  // path mid-render.
  auto rgbPolicy{RGBPolicy{}};
  rgbPolicy.forceFalseColor =
      bool(optFalseColor) || optRGBWaves.getNumOccurrences() > 0;
  if (optRGBWaves.getNumOccurrences() > 0) {
    const auto waves{float3(optRGBWaves)};
    rgbPolicy.falseColorWaves = {waves.x, waves.y, waves.z};
  }
  // Rewriting the tone mapped output while the render runs, so that a tool
  // watching the file sees the image converge. The sums-plus-counts image
  // is a valid mean at every moment, so a checkpoint is the finished write
  // with fewer samples behind it, and nothing about the estimator changes.
  // Written beside the output and renamed into place: a watcher polling
  // the path never opens a half-written PNG.
  const double previewEvery{std::max(double(optPreviewEvery), 0.0)};
  const bool checkpointing{previewEvery > 0.0 &&
                           !std::string(optOutput).empty()};
  const auto writeDisplayImage{[&] {
    const auto path{std::filesystem::path(std::string(optOutput))};
    auto partPath{path};
    partPath.replace_extension("part" + path.extension().string());
    const auto rgb{resolveRGB(compiler, renderImage, wavelengths, rgbPolicy)};
    const auto ldr{tonemap(tonemapOptions, rgb, renderImage, wavelengths)};
    if (auto error{smdl::write8bitImage(partPath.string(), numPixelsX,
                                        numPixelsY, 3, ldr.data())}) {
      error->print();
      return;
    }
    std::error_code ignored{};
    std::filesystem::rename(partPath, path, ignored);
  }};
  auto lastCheckpoint{std::chrono::steady_clock::now()};
  const auto checkpoint{[&] {
    if (!checkpointing) return;
    const auto now{std::chrono::steady_clock::now()};
    if (std::chrono::duration<double>(now - lastCheckpoint).count() <
        previewEvery)
      return;
    writeDisplayImage();
    // Timed from the end of the write, so that an image expensive to tone
    // map spaces its checkpoints out instead of running back to back.
    lastCheckpoint = std::chrono::steady_clock::now();
  }};

  const auto passes{solveSamplePasses(spp, bool(optGuide))};
  progressOptions.total = numPixelsX * numPixelsY * spp;
  progressOptions.displayScale = std::max<size_t>(spp, 1);
  progressOptions.summary = smdl::concat("Rendered ", numPixelsX, "x",
                                         numPixelsY, " at ", spp, " spp");
  auto progress{ProgressBar(progressOptions)};
  size_t sppDone{0};
  size_t chunkSpp{1};
  for (size_t passIndex = 0; passIndex < passes.size(); passIndex++) {
    const size_t thisPass{passes[passIndex]};
    const bool isFinal{passIndex + 1 == passes.size()};
    if (optGuide)
      progress.setNote(
          smdl::concat("pass ", passIndex + 1, "/", passes.size()));
    // Pre-final passes train the SD-tree; every pass contributes to the
    // output through the pass combination below.
    const bool recordPass{optGuide && !isFinal};
    renderImage.resize(wavelengths.size(), numPixelsX, numPixelsY);
    // Without guiding the whole budget is one pass, so checkpointing has
    // to split it; the chunk starts at one sample, so the first image
    // lands almost immediately, and then grows toward the interval asked
    // for. With guiding the passes are the chunks: they already grow
    // geometrically, and splitting one would change what the combiner
    // weights.
    const bool chunked{checkpointing && !combiner};
    for (size_t passDone{0}; passDone < thisPass;) {
      const size_t chunk{chunked ? std::min(chunkSpp, thisPass - passDone)
                                 : thisPass - passDone};
      const size_t chunkBase{passDone};
      const auto chunkStart{std::chrono::steady_clock::now()};
      llvm::parallelFor(0, numPixelsX * numPixelsY, [&](size_t i) {
        // Constructed per pixel deliberately: hoisting this to a
        // thread_local measures as pure noise (the few malloc/free pairs
        // per pixel amortize across worker threads and malloc's own thread
        // cache), so the simpler lifetime wins.
        auto allocator{smdl::BumpPtrAllocator()};
        auto sampler{Sampler()};
        // Training records for `trainGuiding()`, constructed only on
        // the pre-final guiding passes that fill them: at a runtime
        // band count this is 128 sized vectors rather than one flat
        // memset, too much to pay per pixel of a non-guiding render.
        std::optional<std::array<GuideRecord, MAX_PATH_LEN>> guideRecords{};
        if (recordPass) guideRecords.emplace();
        auto y{i / numPixelsX};
        auto x{i % numPixelsX};
        Color Lsum{};
        PassCombiner::PixelHalves halves{};
        Guiding guiding{};
        guiding.tree = sdtree.get();
        guiding.pixelEstimate =
            combiner && optGuideADRRS ? combiner->pixelEstimate(i) : 0.0f;
        guiding.bsdfFraction =
            std::min(std::max(float(optGuideBSDFFraction), 0.0f), 1.0f);
        guiding.bsdfFractionFixed =
            optGuideBSDFFraction.getNumOccurrences() > 0;
        for (size_t s = 0; s < chunk; s++) {
          sampler.startPixelSample(
              uint32_t(i), uint32_t(sampleIndexBase + sppDone + chunkBase + s));
          Color Lsample{};
          const auto cameraSample{camera->sample(x, y, sampler)};
          // A fully vignetted sample contributes nothing, so skip the
          // walk but let it still count in the average below, keeping the
          // darkening unbiased.
          uint64_t numRecords{0};
          if (cameraSample.weight > 0) {
            Lsample = tracePath(
                compiler, scene, sampler, wavelengths, allocator,
                cameraSample.ray, cameraSample.weight, cameraSample.coneAngle,
                exteriorMedium, MAX_PATH_LEN, lights, &guiding,
                recordPass ? guideRecords->data() : nullptr, numRecords);
          }
          // Train the SD-tree on the records the walk retained.
          if (recordPass && numRecords > 0)
            trainGuiding(*sdtree, sampler, guideRecords->data(), numRecords);
          Lsum += Lsample;
          if (combiner) {
            // Split the samples into two half images so the combination can
            // cross-weight each half by the other's variance estimate.
            float value{Lsample.average()};
            if ((chunkBase + s) % 2 == 0) {
              halves.halfA += Lsample;
              halves.squaresA += value * value;
            } else {
              halves.halfB += Lsample;
              halves.squaresB += value * value;
            }
          }
          allocator.reset();
        }
        if (combiner) combiner->deposit(i, halves);
        renderImage(x, y).addSamples(chunk, Lsum.data());
        // Counted where the work is finished rather than where it starts,
        // which at thumbnail sizes is a whole pool's worth of pixels.
        progress.advance(chunk);
      });
      passDone += chunk;
      if (chunked) {
        // Aim the next chunk at the interval from what this one cost,
        // and never more than quadruple it at once: the first chunk is
        // one sample, and a scene that is cheap at one sample and dear at
        // sixty-four should not overshoot the whole way there.
        const double seconds{std::chrono::duration<double>(
                                 std::chrono::steady_clock::now() - chunkStart)
                                 .count()};
        const double perSample{seconds / double(chunk)};
        const size_t wanted{
            perSample > 0.0 ? size_t(std::max(previewEvery / perSample, 1.0))
                            : thisPass};
        chunkSpp = std::max<size_t>(1, std::min(wanted, chunk * 4));
        checkpoint();
      }
    }
    if (combiner) combiner->foldPass(thisPass);
    if (recordPass) {
      combiner->rebuildPixelEstimates();
      // Refine: split spatial leaves past c*sqrt(2^k) records (c = 12000,
      // k this pass's index), rebuild the directional quadtrees with the
      // 1% flux threshold.
      sdtree->refine(uint32_t(12000.0 * std::sqrt(thisPass)), 0.01f, 20);
      float minAlpha{}, meanAlpha{};
      sdtree->alphaStats(minAlpha, meanAlpha);
      SMDL_LOG_INFO("Guide pass ", passIndex + 1, "/", passes.size(),
                    " done: ", thisPass, " spp, ", sdtree->leafCount(),
                    " spatial leaves, alpha min ", minAlpha, " mean ",
                    meanAlpha);
    }
    sppDone += thisPass;
    // A guided render checkpoints on its own pass boundaries, which is
    // where its image changes anyway.
    if (!isFinal) checkpoint();
  }
  progress.finish();
  // Resolve the pass combination back into the image every downstream
  // output reads from.
  if (combiner) combiner->resolve(renderImage);
  // Fold the prior session's samples back in: one image-level add,
  // which is exactly the merge the sums-plus-counts invariant makes
  // safe. A guided render already merged them through the seeded pass
  // combination. Every output below divides by the combined count.
  if (sampleIndexBase > 0 && !combiner) renderImage.add(resumed.image);
  const auto rgbImage{
      resolveRGB(compiler, renderImage, wavelengths, rgbPolicy)};
  if (!std::string(optOutputFloat).empty()) {
    if (auto error{smdl::writeFloatImage(std::string(optOutputFloat),
                                         numPixelsX, numPixelsY, 3,
                                         rgbImage.data())}) {
      error->print();
    }
  }
  // -resume implies writing back to the file being resumed, so one
  // command line re-runs to keep accumulating; an explicitly given
  // -output-spectral wins verbatim, redirecting or (when empty)
  // suppressing the write.
  const auto outputSpectral{optOutputSpectral.getNumOccurrences() > 0 ||
                                    !resumeRequested
                                ? std::string(optOutputSpectral)
                                : std::string(optResume)};
  if (!outputSpectral.empty()) {
    // Write through a temporary and rename, so an interrupted write
    // cannot destroy the file a resumed session reads from, which may
    // be this very path.
    const auto partName{outputSpectral + ".part"};
    const auto extraHeaderLines{std::vector<std::string>{
        smdl::concat("smdl sampler = ", SAMPLER_VERSION),
        smdl::concat("smdl args = ", argsEcho)}};
    renderImage.writeENVIFile(
        smdl::Span<const float>(wavelengths.data(), wavelengths.size()),
        partName, extraHeaderLines);
    if (std::rename(partName.c_str(), outputSpectral.c_str()) != 0 ||
        std::rename((partName + ".hdr").c_str(),
                    (outputSpectral + ".hdr").c_str()) != 0)
      throw smdl::Error(smdl::concat("cannot rename ", smdl::Quoted(partName),
                                     " into place"));
  }
  {
    const auto ldrImage{
        tonemap(tonemapOptions, rgbImage, renderImage, wavelengths)};
    if (auto error{smdl::write8bitImage(std::string(optOutput), numPixelsX,
                                        numPixelsY, 3, ldrImage.data())}) {
      error->print();
    }
  }
  return EXIT_SUCCESS;
} catch (const smdl::Error &error) {
  error.print();
  return EXIT_FAILURE;
} catch (const std::exception &error) {
  std::cerr << error.what() << '\n';
  return EXIT_FAILURE;
}
